﻿#region --[Copyright]-----------------------------------------------------
//
// Author:
//   Anton Tykhyy (atykhyy@gmail.com)
//
// Copyright (c) 2017 Anton Tykhyy
//
// Licensed under the MIT/X11 license.
//
#endregion

#region --[Namespaces]----------------------------------------------------
using System ;
using System.Linq        ;
using System.Linq.Expressions ;
using System.Collections.Generic   ;
using System.Reflection  ;
using Mono.Cecil         ;
using Mono.Cecil.Cil     ;
using Mono.Cecil.Rocks   ;

using SR = System.Reflection ;
using MC = Mono.Cecil.Cil ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    /// <summary>
    /// Represents a single template application to a single target type.
    /// </summary>
    public sealed partial class TemplateContext : TemplateContextBase<FieldDefinition, MethodDefinition>
    {
        #region --[Fields: Private]---------------------------------------
        private readonly TypeDefinition     m_target   ;
        private readonly TypeReference      m_tVoid    ;
        private readonly TypeReference      m_tObject  ;
        private readonly TypeReference      m_tIntPtr  ;
        private readonly TypeReference      m_tString  ;
        private readonly TypeReference      m_tValueType ;

        private readonly Dictionary<MC.MethodBody, ILBranchManager> m_splices = new Dictionary<MC.MethodBody, ILBranchManager> () ;

        private readonly static MethodReference NullMethod = new MethodReference (null, new TypeReference (null, null, null, null)) ;

        private readonly static OpCode[] OneByteOpCode  = (OpCode[]) typeof (OpCodes).GetField (nameof (OneByteOpCode),  BindingFlags.Static | BindingFlags.NonPublic).GetValue (null) ;
        private readonly static OpCode[] TwoBytesOpCode = (OpCode[]) typeof (OpCodes).GetField (nameof (TwoBytesOpCode), BindingFlags.Static | BindingFlags.NonPublic).GetValue (null) ;

        // since I'm working with Assemblies, and the lifetime of Assembly objects
        // is coterminous with the lifetime of the AppDomain they are loaded into,
        // there seems to be no harm in having symbol reader instances live as long
        private static System.Collections.Immutable.ImmutableDictionary<Module, ModuleDefinition> s_symbols =
                       System.Collections.Immutable.ImmutableDictionary<Module, ModuleDefinition>.Empty ;
        #endregion

        /// <summary>
        /// Gets the target type definition that was supplied
        /// to the constructor.
        /// </summary>
        public TypeDefinition Target
        {
            get { return m_target ; }
        }

        #region --[Constructors]------------------------------------------
        /// <summary>
        /// Creates a new <see cref="TemplateContext"/>.
        /// </summary>
        public TemplateContext (Type template, TypeDefinition target) : base (template)
        {
            if (template.IsGenericType || target.HasGenericParameters)
            {
                // TODO: implementing generic templates (i.e. one creating generic
                // methods, not gratuitously generic as I had at first) will require
                // a thorough understanding of what, where and why is GenericInstance etc.
                // not just poking and prodding the code until it "works"
                // i.e. with generics GetType (Type type) does not return anything usable,
                // but GetType (CloseTemplateType (GetTemplateType (type)) does
                //
                // note that the members of a generic type are not themselves generic
                // but instead are "closed over" the type's generic parameters
                throw new NotSupportedException () ;
            }

            m_target     = target ;
            m_tVoid      = target.Module.ImportReference (typeof (void))   ;
            m_tObject    = target.Module.ImportReference (typeof (object)) ;
            m_tIntPtr    = target.Module.ImportReference (typeof (IntPtr)) ;
            m_tString    = target.Module.ImportReference (typeof (String)) ;
            m_tValueType = target.Module.ImportReference (typeof (ValueType)) ;

            m_dictionary = m_dictionary.Add (m_template, m_target) ;

            m_vars.Add (m_template) ;
        }
        #endregion

        #region --[Methods: context manipulation]-------------------------
        /// <summary>
        /// Binds the templated type <paramref name="template"/> to <paramref name="type"/>.
        /// </summary>
        /// <remarks>
        /// <paramref name="type"/> may be a template type, another templated type
        /// or a plain .NET type.
        /// </remarks>
        public void Bind (Type template, Type type)
        {
            Add (GetTemplatedType (template), GetType (type)) ;
        }

        /// <summary>
        /// Binds the templated type <paramref name="template"/> to <paramref name="type"/>.
        /// </summary>
        public void Bind (Type template, TypeReference type)
        {
            if (type != null && type.Module != m_target.Module)
                type  = m_target.Module.ImportReference (type) ;

            Add (GetTemplatedType (template), type) ;
        }

        /// <summary>
        /// Binds the templated field identified by the lambda expression <paramref name="func"/>
        /// to <paramref name="field"/>.
        /// </summary>
        /// <remarks>
        /// <paramref name="field"/> must be an instance field if the templated field
        /// is an instance field, and a static field if the templated field is static.
        /// </remarks>
        public void BindField<T> (Expression<Func<T>> func, FieldReference field)
        {
            if (field.Module != m_target.Module)
                field         = m_target.Module.ImportReference (field) ;

            var templatedField = GetTemplatedField (func, enforceIsStatic: field.Resolve ().IsStatic) ;
            var declaringType  = templatedField.DeclaringType ;

            // automatically bind the nested type to the field's declaring type
            if (declaringType != m_template)
                SetIdempotent (declaringType, field.DeclaringType) ;

            Add (templatedField, field) ;
        }

        /// <summary>
        /// Binds the templated field identified by the lambda expression <paramref name="func"/>
        /// to the special <paramref name="action"/>.
        /// </summary>
        /// <remarks>
        /// <paramref name="action"/> can manipulate the instruction referring
        /// to the templated field and emit additional instructions.
        /// </remarks>
        public void BindField<T> (Expression<Func<T>> func, Action<ILProcessor, Instruction> action)
        {
            Add (GetTemplatedField (func), action) ;
        }

        /// <summary>
        /// Binds the templated static field identified by the lambda expression <paramref name="func"/>
        /// to the local variable <paramref name="local"/>.
        /// </summary>
        /// <remarks>
        /// The bound local variable can be used only in the method where it is declared.
        /// </remarks>
        public void BindField<T> (Expression<Func<T>> func, VariableDefinition local)
        {
            Add (GetTemplatedField (func, enforceIsStatic: true), local) ;
        }

        /// <summary>
        /// Binds the templated static delegate-typed field
        /// identified by the lambda expression <paramref name="func"/>
        /// to the static method <paramref name="method"/>.
        /// </summary>
        /// <remarks>
        /// Loads of the templated field will be replaced by instructions
        /// creating a delegate from the supplied method.
        /// The type of the delegate is the bound type of the templated field.
        /// </remarks>
        public void BindField<T> (Expression<Func<T>> func, MethodReference method)
        {
            if (method.Module != m_target.Module)
                method         = m_target.Module.ImportReference (method) ;

            if (method != null && method.HasThis)
                throw new ArgumentOutOfRangeException (nameof (method)) ;

            var template = GetTemplatedField (func, enforceIsStatic: true) ;
            if (!typeof (Delegate).IsAssignableFrom (template.FieldType))
                throw new ArgumentOutOfRangeException (nameof (func)) ;

            Add (template, method ?? NullMethod) ;
        }

        /// <summary>
        /// Binds the templated field identified by the lambda expression
        /// <paramref name="func"/> to the compile-time constant <paramref name="value"/>.
        /// </summary>
        public override void BindField<T> (Expression<Func<T>> func, T value)
        {
            Instruction insn ;
            switch (Type.GetTypeCode (typeof (T)))
            {
            case TypeCode.Boolean: insn = Instruction.Create (OpCodes.Ldc_I4,        (bool)(object) value ? 1 : 0) ; break ;
            case TypeCode.Char:    insn = Instruction.Create (OpCodes.Ldc_I4,        (char)(object) value) ; break ;
            case TypeCode.SByte:   insn = Instruction.Create (OpCodes.Ldc_I4,       (sbyte)(object) value) ; break ;
            case TypeCode.Byte:    insn = Instruction.Create (OpCodes.Ldc_I4,        (byte)(object) value) ; break ;
            case TypeCode.Int16:   insn = Instruction.Create (OpCodes.Ldc_I4,       (short)(object) value) ; break ;
            case TypeCode.UInt16:  insn = Instruction.Create (OpCodes.Ldc_I4,      (ushort)(object) value) ; break ;
            case TypeCode.Int32:   insn = Instruction.Create (OpCodes.Ldc_I4, (int)        (object) value) ; break ;
            case TypeCode.UInt32:  insn = Instruction.Create (OpCodes.Ldc_I4, (int)  (uint)(object) value) ; break ;
            case TypeCode.Int64:   insn = Instruction.Create (OpCodes.Ldc_I8, (long)       (object) value) ; break ;
            case TypeCode.UInt64:  insn = Instruction.Create (OpCodes.Ldc_I8, (long)(ulong)(object) value) ; break ;
            case TypeCode.Single:  insn = Instruction.Create (OpCodes.Ldc_R4,       (float)(object) value) ; break ;
            case TypeCode.Double:  insn = Instruction.Create (OpCodes.Ldc_R8,      (double)(object) value) ; break ;
            case TypeCode.String:
                if (value == null) insn = Instruction.Create (OpCodes.Ldnull) ;
                else               insn = Instruction.Create (OpCodes.Ldstr, value as string) ;
                break ;
            default:
                throw new ArgumentOutOfRangeException () ;
            }

            if (typeof (T).IsEnum)
                insn.Operand = value ;

            Add (GetTemplatedField (func, enforceIsStatic: true), insn) ;
        }

        /// <summary>
        /// Binds the templated method identified by the lambda expression <paramref name="expr"/>
        /// to <paramref name="method"/>.
        /// </summary>
        /// <remarks>
        /// The methods may be instance or static, but their lists of parameters
        /// including the implicit <c>this</c> parameter must be compatible
        /// or the resulting IL will be illegal or unverifiable.
        /// </remarks>
        public void Bind_ (Expression<Action> expr, MethodReference method)
        {
            if (method.Module != m_target.Module)
                method         = m_target.Module.ImportReference (method) ;

            Add (GetTemplatedMethod (expr), method) ;
        }

        /// <summary>
        /// Binds the templated method that is the target of the supplied delegate
        /// to <paramref name="method"/>.
        /// </summary>
        /// <remarks>
        /// The methods may be instance or static, but their lists of parameters
        /// including the implicit <c>this</c> parameter must be compatible
        /// or the resulting IL will be illegal or unverifiable.
        /// </remarks>
        public void Bind<T> (Func<T> func, MethodReference method)
        {
            if (method.Module != m_target.Module)
                method         = m_target.Module.ImportReference (method) ;

            Add (GetTemplatedMethod (func.Method, null), method) ;
        }
        #endregion

        #region --[Methods: template application]-------------------------
        /// <summary>
        /// Finalizes methods generated or modified by the template.
        /// </summary>
        public void Commit ()
        {
            foreach (var body in m_splices.Keys)
            {
                // NB: this method computes instruction offsets
                body.OptimizeMacros () ;

                // sort sequence points by offset,
                // otherwise Cecil will silently write a corrupt portable PDB
                var mdi = body.Method.DebugInformation ;
                if (mdi.HasSequencePoints)
                {
                    var spc = mdi.SequencePoints ;
                    for (var i = 1 ; i < spc.Count ; ++i)
                        if (spc[i - 1].Offset >= spc[i].Offset)
                        {
                            var spa = new SequencePoint[spc.Count] ;
                            spc.CopyTo (spa, 0) ;
                            Array.Sort (spa, (a, b) => a.Offset.CompareTo (b.Offset)) ;
                            for (i = 0 ; i < spa.Length ; ++i)
                                spc[i] = spa[i] ;
                            break ;
                        }
                }
            }
        }

        /// <inherit/>
        public override void CopyInterfaceImpl (Type template, Type type)
        {
            var prefix  = type.GetPseudoSourceFullName () ;
            var target  = (TypeDefinition) m_dictionary[template] ;

            var mapping = GetTemplateInterfaceMap (template, type) ;
            var fixups  = new Dictionary<MethodBase, MethodDefinition> () ;

            for (int i  = 0 ; i < mapping.TargetMethods.Length ; ++i)
            {
                MethodDefinition copy ;

                // already copied?
                object value ;
                if (!m_dictionary.TryGetValue (mapping.TargetMethods[i], out value))
                {
                    // TODO: I used to add a suppression attribute for CA1033 for these methods,
                    // but that attribute is [Conditional]; I am not sure how this all works
                    // TODO: what if an implementing method is picked up from a base class?
                    copy = CopyMethodInternal (mapping.TargetMethods[i]) ;
                }
                else
                {
                    copy = (MethodDefinition) value ;

                    // if a [TemplateParameter] method was copied as override,
                    // add appropriate method attributes
                    if(!copy.IsVirtual)
                        copy.Attributes |= Mono.Cecil.MethodAttributes.Virtual | Mono.Cecil.MethodAttributes.NewSlot | Mono.Cecil.MethodAttributes.Final ;
                }

                var imethod = mapping.InterfaceMethods[i] ;
                copy.Overrides.Add (ImportMethod (imethod)) ;
                fixups.Add                       (imethod, copy) ;
            }

            foreach (var property in type.GetProperties ())
            {
                var prop = new PropertyDefinition (prefix + "." + property.Name,
                    Mono.Cecil.PropertyAttributes.SpecialName, GetType (property.PropertyType)) ;

                var indexerParams  = property.GetIndexParameters () ;
                if (indexerParams != null && indexerParams.Length != 0)
                {
                    foreach (var indexerParam in indexerParams)
                        prop.Parameters.Add (new ParameterDefinition (indexerParam.Name,
                            (Mono.Cecil.ParameterAttributes) indexerParam.Attributes, GetType (indexerParam.ParameterType))) ;
                }

                target.Properties.Add (prop) ;

                if (property.GetMethod != null) prop.GetMethod = fixups[property.GetMethod] ;
                if (property.SetMethod != null) prop.SetMethod = fixups[property.SetMethod] ;
            }

            foreach (var event_ in type.GetEvents ())
            {
                var evt = new EventDefinition (prefix + "." + event_.Name,
                    Mono.Cecil.EventAttributes.SpecialName, GetType (event_.EventHandlerType)) ;

                target.Events.Add (evt) ;

                if (event_.AddMethod    != null) evt.AddMethod    = fixups[event_.AddMethod]    ;
                if (event_.RemoveMethod != null) evt.RemoveMethod = fixups[event_.RemoveMethod] ;
            }

            target.Interfaces.Add (new InterfaceImplementation (GetType (mapping.InterfaceType))) ;
        }

        /// <inherit/>
        public override void CopyNested (Type type, SR.TypeAttributes set = 0, SR.TypeAttributes clear = 0)
        {
            // TODO: multi-level nesting
            if (!type.IsNested || !type.DeclaringType.IsAssignableFrom (m_template))
                throw new ArgumentOutOfRangeException (nameof (type)) ;

            Type template ;
            var isInstance = type.IsGenericInstance () ;
            if (isInstance)
            {
                // NB: generic types are always copied from the generic type
                // definition, but from outside the type reflection references
                // the generic instance type. to support such references, I
                // (a) check whether the corresponding generic type definition
                // has already been copied, and (b) add a dictionary entry
                // binding the generic instance type to the type copied from
                // the generic type definition template.
                // This approach precludes copying a generic type definition
                // multiple times with its generic parameters bound to different
                // types - that would require passing around the generic context
                // and using it to 'instantiate' all keys created when copying
                // the generic type definition in that context. This seems like
                // a lot of work, and I am not sure how useful it would be.
                //
                // NB: I don't check here that the template's generic parameters
                // are bound to something matching the generic instance type's
                // generic arguments, but if it doesn't peverify will flag it.
                template = type.GetGenericTypeDefinition () ;

                object temp ;
                if (m_dictionary.TryGetValue (template, out temp))
                {
                    m_dictionary = m_dictionary.Add  (type, temp) ;
                    return ;
                }
            }
            else
                template = type ;

            var newtype  = new TypeDefinition (null, GetEmitName (type), (Mono.Cecil.TypeAttributes)((template.Attributes & ~clear) | set)) ;

            var emitAttr  = type.GetCustomAttribute<EmitNameAttribute> () ;
            if (emitAttr != null)
            {
                if (emitAttr.Private)
                    newtype.Attributes = (newtype.Attributes & ~Mono.Cecil.TypeAttributes.VisibilityMask) | Mono.Cecil.TypeAttributes.NestedPrivate ;

                if (emitAttr.Protected)
                    newtype.Attributes = (newtype.Attributes & ~Mono.Cecil.TypeAttributes.VisibilityMask) | Mono.Cecil.TypeAttributes.NestedFamily ;
            }

            // set the base type if it's templated
            if (template.BaseType.Assembly == typeof (object).Assembly ||
                template.BaseType.IsDefined  (typeof (TemplatedMemberAttribute), false))
            {
                newtype.BaseType = GetType (template.BaseType) ;
            }

            CopyCAs        (template, newtype) ;
            m_target.NestedTypes.Add (newtype) ;

            m_dictionary = m_dictionary.Add (template, newtype) ;

            if (isInstance)
                m_dictionary = m_dictionary.Add (type, newtype) ;
        }

        /// <inherit/>
        protected override FieldDefinition CopyField (FieldInfo field)
        {
            var template = field ;
            var newfield = new FieldDefinition (GetEmitName (field), (Mono.Cecil.FieldAttributes) template.Attributes, GetType (template.FieldType)) ;

            CopyCAs   (template, newfield) ;
            GetTarget (template).Fields.Add (newfield) ;

            var emitAttr  = field.GetCustomAttribute<EmitNameAttribute> () ;
            if (emitAttr != null)
            {
                if (emitAttr.ReadOnly)
                    newfield.IsInitOnly = true ;

                if (emitAttr.Private)
                    newfield.Attributes = (newfield.Attributes & ~Mono.Cecil.FieldAttributes.FieldAccessMask) | Mono.Cecil.FieldAttributes.Private ;

                if (emitAttr.Protected)
                    newfield.Attributes = (newfield.Attributes & ~Mono.Cecil.FieldAttributes.FieldAccessMask) | Mono.Cecil.FieldAttributes.Family ;
            }

            m_dictionary = m_dictionary.Add (template, newfield) ;
            return newfield ;
        }

        /// <inherit/>
        protected override MethodDefinition CopyMethod (MethodBase method, SR.MethodAttributes set = 0, SR.MethodAttributes clear = 0)
        {
            return CopyMethodInternal (method, set, clear) ;
        }

        #region public ILProcessor CreateMethodBuilder (...)
        /// <summary>
        /// Creates a new, empty method from the template method
        /// that is the target of the supplied delegate
        /// and returns a <see cref="ILProcessor"/> for it.
        /// </summary>
        public ILProcessor CreateMethodBuilder<T> (Action<T> d, SR.MethodAttributes set = 0, SR.MethodAttributes clear = 0)
        {
            return CreateMethodBuilder (d.Method, set, clear).Item1.Body.GetILProcessor () ;
        }

        /// <summary>
        /// Creates a new, empty method from the template method
        /// that is the target of the supplied delegate
        /// and returns a <see cref="ILProcessor"/> for it.
        /// </summary>
        public ILProcessor CreateMethodBuilder (Action d, SR.MethodAttributes set = 0, SR.MethodAttributes clear = 0)
        {
            return CreateMethodBuilder (d.Method, set, clear).Item1.Body.GetILProcessor () ;
        }

        /// <summary>
        /// Creates a new, empty method from the template method
        /// identified by the lambda expression <paramref name="expr"/>
        /// and returns a <see cref="ILProcessor"/> for it.
        /// </summary>
        public ILProcessor CreateMethodBuilder_ (Expression<Action> expr, SR.MethodAttributes set = 0, SR.MethodAttributes clear = 0)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex != null)
                return CreateMethodBuilder (GetMethodInType (fex.Method, fex.Object?.Type), set, clear).Item1.Body.GetILProcessor () ;

            var nex  = expr.Body as NewExpression ;
            if (nex != null)
                return CreateMethodBuilder (nex.Constructor, set, clear).Item1.Body.GetILProcessor () ;

            throw new ArgumentOutOfRangeException (nameof (expr)) ;
        }
        #endregion

        #region public void Splice (ILProcessor il, Instruction before, ...)
        /// <summary>
        /// Splices the template method that is the target of the supplied delegate
        /// into <paramref name="il"/> at the indicated location.
        /// <para/>
        /// Templated method parameters that are not marked <see cref="FromILStackAttribute"/>,
        /// including the implicit <c>this</c> parameter
        /// if the template method has the <see cref="TemplatedThisAttribute"/>,
        /// are bound to the elements of <paramref name="templatedArguments"/> in order.
        /// </summary>
        public void Splice<P1, T> (ILProcessor il, SpliceLocation location, Func<P1, T> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        /// <summary>
        /// Splices the template method that is the target of the supplied delegate
        /// into <paramref name="il"/> at the indicated location.
        /// <para/>
        /// Templated method parameters that are not marked <see cref="FromILStackAttribute"/>,
        /// including the implicit <c>this</c> parameter
        /// if the template method has the <see cref="TemplatedThisAttribute"/>,
        /// are bound to the elements of <paramref name="templatedArguments"/> in order.
        /// </summary>
        public void Splice<T> (ILProcessor il, SpliceLocation location, Func<T> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        /// <summary>
        /// Splices the template method that is the target of the supplied delegate
        /// into <paramref name="il"/> at the indicated location.
        /// <para/>
        /// Templated method parameters that are not marked <see cref="FromILStackAttribute"/>,
        /// including the implicit <c>this</c> parameter
        /// if the template method has the <see cref="TemplatedThisAttribute"/>,
        /// are bound to the elements of <paramref name="templatedArguments"/> in order.
        /// </summary>
        public void Splice<P1, P2> (ILProcessor il, SpliceLocation location, Action<P1, P2> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        /// <summary>
        /// Splices the template method that is the target of the supplied delegate
        /// into <paramref name="il"/> at the indicated location.
        /// <para/>
        /// Templated method parameters that are not marked <see cref="FromILStackAttribute"/>,
        /// including the implicit <c>this</c> parameter
        /// if the template method has the <see cref="TemplatedThisAttribute"/>,
        /// are bound to the elements of <paramref name="templatedArguments"/> in order.
        /// </summary>
        public void Splice<T> (ILProcessor il, SpliceLocation location, Action<T> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        /// <summary>
        /// Splices the template method that is the target of the supplied delegate
        /// into <paramref name="il"/> at the indicated location.
        /// <para/>
        /// Templated method parameters that are not marked <see cref="FromILStackAttribute"/>,
        /// including the implicit <c>this</c> parameter
        /// if the template method has the <see cref="TemplatedThisAttribute"/>,
        /// are bound to the elements of <paramref name="templatedArguments"/> in order.
        /// </summary>
        public void Splice (ILProcessor il, SpliceLocation location, Action d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        /// <summary>
        /// Splices the template method identified by the lambda expression <paramref name="expr"/>
        /// into <paramref name="il"/> at the indicated location.
        /// <para/>
        /// Templated method parameters that are not marked <see cref="FromILStackAttribute"/>,
        /// including the implicit <c>this</c> parameter
        /// if the template method has the <see cref="TemplatedThisAttribute"/>,
        /// are bound to the elements of <paramref name="templatedArguments"/> in order.
        /// </summary>
        public void Splice_ (ILProcessor il, SpliceLocation location, Expression<Action> expr, params object[] templatedArguments)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex == null)
                throw new ArgumentOutOfRangeException (nameof (expr)) ;

            SpliceMethod (GetMethodInType (fex.Method, fex.Object?.Type), il, location, templatedArguments) ;
        }
        #endregion
        #endregion

        #region --[Methods: Private]--------------------------------------
        private void SpliceMethod (MethodBase source, ILProcessor il, SpliceLocation location, object[] templatedArguments)
        {
            if (il       == null) throw new ArgumentNullException (nameof (il)) ;
            if (location == null) throw new ArgumentNullException (nameof (location)) ;

            var srcbody = source.GetMethodBody () ;
            if (srcbody.InitLocals)
                il.Body.InitLocals = true ;

            var dictionary = new Dictionary<object, object> () ;

            var p = 0 ;
            var a = 0 ;

            // analyze implicit this parameter
            var thisFromILStack = false ;
            if (source.IsDefined (typeof (TemplatedThisAttribute)))
            {
                if (source.IsStatic)
                    throw new InvalidOperationException () ;

                if (source.IsDefined (typeof (FromILStackAttribute)))
                {
                    thisFromILStack = true ;
                }
                else
                {
                    if (templatedArguments.Length == 0)
                        throw new InvalidOperationException () ; // too few templated arguments

                    // verifiable instance methods cannot assign 'this'
                    dictionary.Add (s_this, templatedArguments[0]) ;
                    a++ ;
                }
            }
            else
                if (source.IsDefined (typeof (FromILStackAttribute)))
                    throw new InvalidOperationException () ; // must be [TemplatedThis]

            // analyze splice source parameters
            var parameters  = source.GetParameters () ;
            var fromILStack = parameters.Length ;

            IList<ParameterDefinition> targetParameters ;
            if (source.IsStatic && !il.Body.Method.IsStatic)
            {
                targetParameters = new List<ParameterDefinition> (il.Body.Method.Parameters.Count + 1) ;
                targetParameters.Add (il.Body.ThisParameter) ;

                foreach (var parameter in il.Body.Method.Parameters)
                    targetParameters.Add (parameter) ;
            }
            else
                targetParameters = il.Body.Method.Parameters ;

            for ( ; p < parameters.Length ; ++p)
            {
                if (parameters[p].IsDefined (typeof (TemplatedParameterAttribute)))
                    break ;

                if (parameters[p].IsDefined (typeof (FromILStackAttribute)))
                    throw new InvalidOperationException () ; // must be templated

                if (targetParameters.Count <= p)
                    throw new InvalidOperationException () ; // not enough parameters in target method to match splice's non-templated parameters

                var spt = parameters[p].ParameterType ;
                var tpt = targetParameters[p].ParameterType ;

                // allow matching struct& `this` in target to a by-value
                // reference parameter in source as a kludge until I work out
                // how to support this properly
                // verifiable usage of `this` in structs is quite restricted,
                // and for member access it's identical to a reference-type's
                // `this`, so after matching parameters like this emitted code
                // does the right thing in simple cases
                if (p  == 0 && il.Body.Method.HasThis &&
                    tpt.IsByReference && tpt.GetElementType ().IsValueType && !spt.HasElementType && spt.IsClass)
                    tpt = tpt.GetElementType () ;

                if(!tpt.SameAs (GetType (spt)))
                    throw new InvalidOperationException () ; // splice's non-templated parameter type does not match target method parameter type

                dictionary.Add (parameters[p], targetParameters[p]) ;
            }

            for ( ; p < parameters.Length ; ++p, ++a)
            {
                if(!parameters[p].IsDefined (typeof (TemplatedParameterAttribute)))
                    throw new InvalidOperationException () ; // templated parameters must follow non-templated parameters

                if (parameters[p].IsDefined (typeof (FromILStackAttribute)))
                {
                    if (fromILStack == parameters.Length)
                        fromILStack  = p ;

                    continue ;
                }

                if (fromILStack != parameters.Length)
                    throw new InvalidOperationException () ; // FromILStack parameters must be trailing

                if (a >= templatedArguments.Length)
                    throw new InvalidOperationException () ; // too few templated arguments

                var value  = templatedArguments[a] ;
                if (value == null && typeof (Delegate).IsAssignableFrom (parameters[p].ParameterType))
                    value  = NullMethod ;

                dictionary.Add (parameters[p], value) ;
            }

            ILBranchManager branchManager ;
            if (!m_splices.TryGetValue (il.Body, out branchManager))
            {
                branchManager = new ILBranchManager (il) ;
                m_splices.Add (il.Body, branchManager) ;
            }

            // decode splice location
            Instruction insnLeft, insnRight ;

            var insns     = il.Body.Instructions ;
            if (location == SpliceLocation.AtEnd)
            {
                insnRight = null ;
                insnLeft  = insns.Count == 0 ? null : insns[insns.Count - 1] ;
            }
            else
            if (location == SpliceLocation.AtStart)
            {
                insnRight = insns.Count == 0 ? null : insns[0] ;
                insnLeft  = null ;
            }
            else
            switch (location.Relation)
            {
            default:
            case SpliceRelation.Before:
                insnRight = null ;
                insnLeft  = location.Insn.Previous ;
                break ;

            case SpliceRelation.BeforeAsPart:
                insnRight = location.Insn ;
                insnLeft  = location.Insn.Previous ;
                break ;

            case SpliceRelation.Replace:
                insnRight = null ;
                insnLeft  = location.Insn ;

                insnLeft.NopOut () ;
                break ;

            case SpliceRelation.AfterAsPart:
                insnRight = null ;
                insnLeft  = location.Insn ;
                break ;
            }

            // prepare to splice at indicated location
            // prepare instruction to branch to instead of normal splice returns
            Instruction[] insnSaved ;
            var insnMark  = insnLeft ;
            var iidxRet   = insns.IndexOf (insnLeft) + 1 ;
            if (iidxRet  == insns.Count)
            {
                insnSaved = new Instruction[insns.Count + 1] ;
                insnSaved[iidxRet] = Instruction.Create (OpCodes.Nop) ;
            }
            else
            {
                // NB: can't use insns.RemoveAt because it removes sequence points
                // and breaks the linked list, potentially making variable scope
                // ranges unusable
                insnSaved = new Instruction[insns.Count] ;
                insns.CopyTo (insnSaved, 0) ;
                insns.Clear  () ;

                for (int i = 0 ; i < iidxRet ; ++i)
                    insns.Add (insnSaved[i]) ;
            }

            // handle templated parameters taken directly from IL stack
            if (thisFromILStack || fromILStack != parameters.Length)
            {
                if (location.Relation == SpliceRelation.Before)
                    throw new InvalidOperationException () ; // can't use this with FromILStack

                // prepare common IL stack parameter array
                var pars = new object[parameters.Length - fromILStack + (thisFromILStack ? 1 : 0)] ;

                // expect (parameters.Length - fromILStack) values on the IL stack
                for (p = parameters.Length - 1 ; p >= fromILStack ; --p)
                    pars[parameters.Length - 1 - p] = parameters[p] ;

                // expect this reference below the other values
                if (thisFromILStack)
                    pars[pars.Length - 1] = s_this ;

                var simple = true ;
                for (int i = 0 ; i < pars.Length ; ++i)
                {
                    var stack = 1 ;
                    var spill = false ;

                    // remove simple loads, skip nops, spill calculated values
                    if (simple)
                    {
                    repeat:
                        if (insnRight != null && branchManager.IsTarget (insnRight))
                        {
                            // the right thing to do here is analyze basic blocks
                            // and coalesce values along different control paths,
                            // but that's not always reliable in the general case
                            // and a huge overkill - just spill everything
                            simple = false ;
                            goto spill ;
                        }

                        // move one instruction left
                        insnRight = insnLeft ;
                        insnLeft  = insnLeft.Previous ;

                        // compute IL stack balance going backwards
                        if (!insnRight.TryBackOutStackBehavior (ref stack))
                        {
                            // only `leave` has this behavior
                            simple = false ;
                            goto spill ;
                        }

                        if (!spill)
                        {
                            if (insnRight.IsSimpleLoad ())
                            {
                                dictionary.Add (pars[i], insnRight.Clone ()) ;

                                // can't use RemoveAt (see note above)
                                insnRight.NopOut () ;
                                continue ;
                            }

                            if (insnRight.OpCode == OpCodes.Nop)
                                goto repeat ;

                            // prepare to spill one value into a new local
                            spill = true ;
                        }

                        // skip instructions until we have zero extra on stack
                        if (stack  > 0)
                            goto repeat ;

                        // this can happen if `dup` is used to produce two values - don't bother for now
                        if (stack  < 0)
                            simple = false ;
                    }

                spill:
                    // spill remaining values into new locals
                    var param = pars[i] as ParameterInfo ;
                    var local = il.CreateLocal (GetType (param != null ? param.ParameterType :
                        source.DeclaringType.IsValueType ? source.DeclaringType.MakeByRefType () : source.DeclaringType)) ;

                    il.Emit  (OpCodes.Stloc, local) ;
                    dictionary.Add (pars[i], local) ;
                }
            }

            var spliceScope = CopyMethodBody (source, srcbody, il, insnSaved[iidxRet], dictionary) ;

            // restore instructions after splice
            for (int i = iidxRet ; i < insnSaved.Length ; ++i)
                insns.Add (insnSaved[i]) ;

            if (location.Relation == SpliceRelation.BeforeAsPart && location.Insn != null)
                branchManager.Retarget (location.Insn, insnMark.Next) ;

            // TODO: how to handle EmitName'd members in debugger?
            // splice variable scopes
            // NB: spliceScope always has both Start and End non-null
            if (spliceScope != null)
                SpliceScope (il.Body, spliceScope, location) ;
        }

        private Tuple<MethodDefinition, MethodBase, MethodBase, Dictionary<object, object>> CreateMethodBuilder (
            MethodBase from, SR.MethodAttributes set, SR.MethodAttributes clear)
        {
            // TODO: return "parameter" CAs?
            var to = new MethodDefinition (GetEmitName (from), (Mono.Cecil.MethodAttributes)((from.Attributes & ~clear) | set), GetType (from.GetReturnType ()))
            {
                HasThis           = (from.CallingConvention & CallingConventions.HasThis)      != 0,
                ExplicitThis      = (from.CallingConvention & CallingConventions.ExplicitThis) != 0,
                // TODO: CallingConvention?
            } ;

            CopyCAs (from, to) ;

            // TODO: generic _methods_

            // TODO: add overrides for implicitly implemented interfaces
            // TODO: templated-generic overrides

            var dictionary = new Dictionary<object, object> () ;

            var fromTemplate    = from  ;
            var seenPseudoParam = false ;
            var templatedParams = 0 ;

            var parameters = from.GetParameters () ;

            for (var p = 0 ; p < parameters.Length ; ++p)
            {
                // remove templated parameters
                if (parameters[p].IsDefined (typeof (TemplatedParameterAttribute)))
                {
                    templatedParams++ ;
                }
                else
                if (seenPseudoParam)
                {
                    throw new InvalidOperationException () ; // non-pseudo parameters cannot follow pseudo parameters
                }
                else
                {
                    var par    = parameters[p] ;
                    var newpar = new ParameterDefinition (par.Name, (Mono.Cecil.ParameterAttributes) par.Attributes, GetType (par.ParameterType)) ;

                    CopyCAs        (par, newpar) ;
                    to.Parameters.Add   (newpar) ;
                    dictionary.Add (par, newpar) ;

                    // assignment works around Cecil deficiency
                    if (newpar.HasConstant = par.HasDefaultValue)
                        newpar.Constant    = par.RawDefaultValue ;

                    continue ;
                }

                if (!seenPseudoParam)
                {
                    // remove templated parameters from the binding key
                    // and bind to the "bare" version of the method
                    fromTemplate    = GetMethodInType (from, null, p) ;
                    seenPseudoParam = true ;

                    if (to.Name == from.Name)
                        to.Name  = GetEmitName (fromTemplate) ;
                }
            }

            // TODO: throw on unbound templated parameters (but what about emit label parameter?)
            GetTarget (fromTemplate).Methods.Add (to) ;

            return Tuple.Create (to, from, fromTemplate, dictionary) ;
        }

        private MethodDefinition CopyMethodInternal (MethodBase source, SR.MethodAttributes set = 0, SR.MethodAttributes clear = 0)
        {
            var tuple        = CreateMethodBuilder (source, set, clear) ;
            var to           = tuple.Item1 ;
            var from         = tuple.Item2 ;
            var fromTemplate = tuple.Item3 ;
            var dictionary   = tuple.Item4 ;

            m_dictionary = m_dictionary.Add (fromTemplate, to) ;

            var fromBody  = from.GetMethodBody () ;
            if (fromBody == null || to.IsAbstract)
                return to ;

            to.Body.MaxStackSize = fromBody.MaxStackSize ;
            to.Body.InitLocals   = fromBody.InitLocals ;

            // can't be abstract since we're adding a body
            // this relieves the user from having to track abstractness in some cases
            to.Attributes &= ~Mono.Cecil.MethodAttributes.Abstract ;

            to.DebugInformation.Scope = CopyMethodBody (from, fromBody, to.Body.GetILProcessor (), null, dictionary) ;
            to.Body.OptimizeMacros () ;
            return to ;
        }

        private ScopeDebugInformation CopyMethodBody (MethodBase method, System.Reflection.MethodBody from, ILProcessor il, Instruction ret,
            Dictionary<object, object> dictionary)
        {
            var insns  = new Dictionary<int, Instruction> () ;
            var bytes  = from.GetILAsByteArray () ;
            var paramz = method.GetParameters  () ;
            var localz = new VariableDefinition[from.LocalVariables.Count] ;
            for (int i = 0 ; i < localz.Length ; ++i)
            {
                var newloc = new VariableDefinition (GetType (from.LocalVariables[i].LocalType)) ;
                il.Body.Variables.Add (newloc) ;
                localz[i]            = newloc  ;
            }

            // support splicing generic methods
            var gmta = method.IsGenericMethod ? method.GetGenericArguments () : null ;
            var gtta = method.DeclaringType.IsGenericType ? method.DeclaringType.GetGenericArguments () : null ;

            // add guard value for end-of-method offset
            insns.Add (bytes.Length, ret) ;

            object[] args ;
            if ((method.CallingConvention & CallingConventions.ExplicitThis) != 0)
            {
                throw new NotImplementedException () ; // TODO
            }
            else
            if ((method.CallingConvention & CallingConventions.HasThis) != 0)
            {
                args    = new object[paramz.Length + 1] ;
                args[0] = s_this ;

                if(!dictionary.ContainsKey (s_this))
                    dictionary.Add         (s_this, il.Body.ThisParameter) ;

                Array.Copy (paramz, 0, args, 1, paramz.Length) ;
            }
            else
                args = paramz ;

            // current interface for generated constrained. prefix
            TypeReference constrain = null ;

            for (var offset = 0 ; offset < bytes.Length ; )
            {
                // remember instruction offset for branches etc.
                var newinsn = GetOrAdd (insns, offset) ;
                il.Append (newinsn) ;

                // decode opcode
                OpCode opcode ;
                if (bytes[offset] == 0xFE)
                {
                    offset++ ;
                    opcode = TwoBytesOpCode[bytes[offset++]] ;
                }
                else
                    opcode = OneByteOpCode[bytes[offset++]] ;

                if (ret != null && opcode == OpCodes.Ret)
                {
                    // TODO: this might not work with non-void methods,
                    // in case the compile-time types on the CLR stack
                    // are different; introduce a local in that case
                    newinsn.OpCode  = OpCodes.Br ;
                    newinsn.Operand = ret ;
                }
                else
                switch (opcode.Code)
                {
                // expand macro opcodes
                case Code.Ldarg_0:   newinsn.OpCode = OpCodes.Ldarg   ; newinsn.Operand = args[0]   ; break ;
                case Code.Ldarg_1:   newinsn.OpCode = OpCodes.Ldarg   ; newinsn.Operand = args[1]   ; break ;
                case Code.Ldarg_2:   newinsn.OpCode = OpCodes.Ldarg   ; newinsn.Operand = args[2]   ; break ;
                case Code.Ldarg_3:   newinsn.OpCode = OpCodes.Ldarg   ; newinsn.Operand = args[3]   ; break ;
                case Code.Ldloc_0:   newinsn.OpCode = OpCodes.Ldloc   ; newinsn.Operand = localz[0] ; break ;
                case Code.Ldloc_1:   newinsn.OpCode = OpCodes.Ldloc   ; newinsn.Operand = localz[1] ; break ;
                case Code.Ldloc_2:   newinsn.OpCode = OpCodes.Ldloc   ; newinsn.Operand = localz[2] ; break ;
                case Code.Ldloc_3:   newinsn.OpCode = OpCodes.Ldloc   ; newinsn.Operand = localz[3] ; break ;
                case Code.Stloc_0:   newinsn.OpCode = OpCodes.Stloc   ; newinsn.Operand = localz[0] ; break ;
                case Code.Stloc_1:   newinsn.OpCode = OpCodes.Stloc   ; newinsn.Operand = localz[1] ; break ;
                case Code.Stloc_2:   newinsn.OpCode = OpCodes.Stloc   ; newinsn.Operand = localz[2] ; break ;
                case Code.Stloc_3:   newinsn.OpCode = OpCodes.Stloc   ; newinsn.Operand = localz[3] ; break ;
                case Code.Leave_S:   newinsn.OpCode = OpCodes.Leave   ; break ;
                case Code.Ldarg_S:   newinsn.OpCode = OpCodes.Ldarg   ; break ;
                case Code.Ldarga_S:  newinsn.OpCode = OpCodes.Ldarga  ; break ;
                case Code.Starg_S:   newinsn.OpCode = OpCodes.Starg   ; break ;
                case Code.Ldloc_S:   newinsn.OpCode = OpCodes.Ldloc   ; break ;
                case Code.Ldloca_S:  newinsn.OpCode = OpCodes.Ldloca  ; break ;
                case Code.Stloc_S:   newinsn.OpCode = OpCodes.Stloc   ; break ;
                case Code.Br_S:      newinsn.OpCode = OpCodes.Br      ; break ;
                case Code.Brfalse_S: newinsn.OpCode = OpCodes.Brfalse ; break ;
                case Code.Brtrue_S:  newinsn.OpCode = OpCodes.Brtrue  ; break ;
                case Code.Bge_S:     newinsn.OpCode = OpCodes.Bge     ; break ;
                case Code.Bgt_S:     newinsn.OpCode = OpCodes.Bgt     ; break ;
                case Code.Ble_S:     newinsn.OpCode = OpCodes.Ble     ; break ;
                case Code.Beq_S:     newinsn.OpCode = OpCodes.Beq     ; break ;
                case Code.Blt_S:     newinsn.OpCode = OpCodes.Blt     ; break ;
                case Code.Bne_Un_S:  newinsn.OpCode = OpCodes.Bne_Un  ; break ;
                case Code.Bge_Un_S:  newinsn.OpCode = OpCodes.Bge_Un  ; break ;
                case Code.Bgt_Un_S:  newinsn.OpCode = OpCodes.Bgt_Un  ; break ;
                case Code.Ble_Un_S:  newinsn.OpCode = OpCodes.Ble_Un  ; break ;
                case Code.Blt_Un_S:  newinsn.OpCode = OpCodes.Blt_Un  ; break ;
                default:             newinsn.OpCode = opcode          ; break ;
                }

                // decode operand and loop if the operand is uninteresting
                switch (opcode.OperandType)
                {
                case OperandType.InlineBrTarget:
                    newinsn.Operand = GetOrAdd (insns, offset + 4 + BitConverter.ToInt32 (bytes, offset)) ;
                    offset += 4 ;
                    continue ;
                case OperandType.InlineField:
                    newinsn.Operand = method.Module.ResolveField (BitConverter.ToInt32 (bytes, offset), gtta, gmta) ;
                    offset += 4 ;
                    break ;
                case OperandType.InlineI:
                    newinsn.Operand = BitConverter.ToInt32 (bytes, offset) ;
                    offset += 4 ;
                    continue ;
                case OperandType.InlineI8:
                    newinsn.Operand = BitConverter.ToInt64 (bytes, offset) ;
                    offset += 8 ;
                    continue ;
                case OperandType.InlineMethod:
                    newinsn.Operand = method.Module.ResolveMethod (BitConverter.ToInt32 (bytes, offset), gtta, gmta) ;
                    offset += 4 ;
                    break ;
                case OperandType.InlineNone:
                    if (newinsn.OpCode == OpCodes.Ldarg)
                        break ; // method parameters are interesting

                    continue ;
                case OperandType.InlineR:
                    newinsn.Operand = BitConverter.ToDouble (bytes, offset) ;
                    offset += 8 ;
                    continue ;
                case OperandType.InlineString:
                    newinsn.Operand = method.Module.ResolveString (BitConverter.ToInt32 (bytes, offset)) ;
                    offset += 4 ;
                    continue ;
                case OperandType.InlineSwitch:
                    var array = new Instruction[BitConverter.ToInt32 (bytes, offset)] ;
                    var ninsn = offset + 4 + array.Length * 4 ;
                    offset   += 4 ;

                    for (int i = 0 ; i < array.Length ; ++i)
                    {
                        array[i] = GetOrAdd (insns, ninsn + BitConverter.ToInt32 (bytes, offset)) ;
                        offset  += 4 ;
                    }

                    newinsn.Operand = array ;
                    continue ;
                case OperandType.InlineTok:
                    newinsn.Operand = method.Module.ResolveMember (BitConverter.ToInt32 (bytes, offset), gtta, gmta) ;
                    offset += 4 ;
                    break ;
                case OperandType.InlineType:
                    newinsn.Operand = method.Module.ResolveType (BitConverter.ToInt32 (bytes, offset), gtta, gmta) ;
                    offset += 4 ;
                    break ;
                case OperandType.InlineVar:
                    newinsn.Operand = localz[BitConverter.ToUInt16 (bytes, offset)] ;
                    offset += 2 ;
                    continue ;
                case OperandType.InlineArg:
                    newinsn.Operand = args[BitConverter.ToUInt16 (bytes, offset)] ;
                    offset += 2 ;
                    break ;
                case OperandType.ShortInlineBrTarget:
                    newinsn.Operand = GetOrAdd (insns, offset + 1 + (sbyte)bytes[offset]) ;
                    offset += 1 ;
                    continue ;
                case OperandType.ShortInlineI:
                    newinsn.Operand = opcode == OpCodes.Ldc_I4_S ? (sbyte)bytes[offset] : (object)bytes[offset] ;
                    offset += 1 ;
                    continue ;
                case OperandType.ShortInlineR:
                    newinsn.Operand = BitConverter.ToSingle (bytes, offset) ;
                    offset += 4 ;
                    continue ;
                case OperandType.ShortInlineVar:
                    newinsn.Operand = localz[bytes[offset]] ;
                    offset += 1 ;
                    continue ;
                case OperandType.ShortInlineArg:
                    newinsn.Operand = args[bytes[offset]] ;
                    offset += 1 ;
                    break ;
                default:
                    throw new NotSupportedException () ;
                }

                // at this point, newinsn.Operand is an "interesting" object:
                // MemberInfo, ParameterInfo or s_this, and is not null

                // process template bindings
                object newop ;
                if (dictionary.TryGetValue (newinsn.Operand, out newop) ||
                  m_dictionary.TryGetValue (newinsn.Operand, out newop))
                {
                    var action  = newop as Action<ILProcessor, Instruction> ;
                    if (action != null)
                    {
                        action (il, newinsn) ;
                        continue ;
                    }

                    FieldReference      field ;
                    MethodReference     meth  ;
                    TypeReference       tref  ;
                    ParameterDefinition pd ;
                    VariableDefinition  vd ;
                    MethodDefinition    md ;
                    Instruction         li ;
                    if ((tref = newop as TypeReference) != null)
                    {
                        if (tref.Module  != m_target.Module)
                            newop         = m_target.Module.ImportReference (tref) ;
                    }
                    else
                    if ((field = newop as FieldReference) != null)
                    {
                        if (field.Module != m_target.Module)
                            newop         = m_target.Module.ImportReference (field) ;

                        switch (newinsn.OpCode.Code)
                        {
                        case Code.Ldarga:  newinsn.OpCode = OpCodes.Ldsflda ; break ;
                        case Code.Ldarg:   newinsn.OpCode = OpCodes.Ldsfld  ; break ;
                        case Code.Starg:   newinsn.OpCode = OpCodes.Stsfld  ; break ;
                        case Code.Ldflda:  break ;
                        case Code.Ldfld:   break ;
                        case Code.Stfld:   break ;
                        case Code.Ldsflda: break ;
                        case Code.Ldsfld:  break ;
                        case Code.Stsfld:  break ;
                        default:           throw new InvalidOperationException () ;
                        }

                        if (newinsn.Operand is FieldInfo finfo && !finfo.IsStatic && !finfo.DeclaringType.IsValueType && field.DeclaringType.IsValueType)
                        {
                            FixUpValueTypeReceiver (newinsn, newinsn.OpCode.Code == Code.Stfld ? 2 : 1) ;
                        }
                    }
                    else
                    if ((vd = newop as VariableDefinition) != null) // is this ever useful?
                    {
                        switch (newinsn.OpCode.Code)
                        {
                        case Code.Ldarga:  newinsn.OpCode = OpCodes.Ldloca ; break ;
                        case Code.Ldarg:   newinsn.OpCode = OpCodes.Ldloc  ; break ;
                        case Code.Starg:   newinsn.OpCode = OpCodes.Stloc  ; break ;
                        case Code.Ldsflda: newinsn.OpCode = OpCodes.Ldloca ; break ;
                        case Code.Ldsfld:  newinsn.OpCode = OpCodes.Ldloc  ; break ;
                        case Code.Stsfld:  newinsn.OpCode = OpCodes.Stloc  ; break ;
                        default:           throw new InvalidOperationException ()  ;
                        }
                    }
                    else
                    if ((pd = newop as ParameterDefinition) != null)
                    {
                        switch (newinsn.OpCode.Code)
                        {
                        case Code.Ldarga:  break ;
                        case Code.Ldarg:   break ;
                        case Code.Starg:   break ;
                        case Code.Ldsflda: newinsn.OpCode = OpCodes.Ldarga ; break ;
                        case Code.Ldsfld:  newinsn.OpCode = OpCodes.Ldarg  ; break ;
                        case Code.Stsfld:  newinsn.OpCode = OpCodes.Starg  ; break ;
                        default:           throw new InvalidOperationException ()  ;
                        }
                    }
                    else
                    if ((li = newop as Instruction) != null)
                    {
                        newinsn.OpCode  = li.OpCode  ;
                        newinsn.Operand = li.Operand ;
                        continue ;
                    }
                    else
                    if ((md = newop as MethodDefinition) != null && md.IsConstructor)
                    {
                        // convert bindings of static templated methods to constructors
                        newinsn.OpCode  = OpCodes.Newobj ;
                    }
                    else
                    if ((meth = newop as MethodReference) != null && !(newinsn.Operand is MethodBase))
                    {
                        if (meth.Module  != m_target.Module)
                            meth          = m_target.Module.ImportReference (meth) ;

                        // convert bindings of storage locations to methods as delegate creation
                        Type type ;
                        switch (newinsn.OpCode.Code)
                        {
                        default:           throw new InvalidOperationException () ;
                        case Code.Ldarg:   type = ((ParameterInfo) newinsn.Operand).ParameterType ; break ;
                        case Code.Ldsfld:  type = ((FieldInfo)     newinsn.Operand).FieldType     ; break ;
                        }

                        // TODO: instance methods bind to local 'this', whatever it is
                        newinsn.OpCode  = meth.HasThis ? OpCodes.Ldarg_0 : OpCodes.Ldnull ;
                        newinsn.Operand = null ;

                        if (meth != NullMethod)
                        {
                            il.Emit (OpCodes.Ldftn,  meth) ;
                            il.Emit (OpCodes.Newobj, MakeDelegateCtorReference (type)) ;
                        }

                        continue ;
                    }
                    else
                    if ((meth = newop as MethodReference) != null)
                    {
                        if (meth.Module  != m_target.Module)
                            newop         = m_target.Module.ImportReference (meth) ;

                        if (newinsn.Operand is MethodBase minfo && !minfo.IsStatic && !minfo.DeclaringType.IsValueType && meth.HasThis && meth.DeclaringType.IsValueType)
                        {
                            if (newinsn.OpCode.Code == Code.Callvirt)
                                newinsn.OpCode = OpCodes.Call ;

                            FixUpValueTypeReceiver (newinsn, meth.Parameters.Count + 1) ;
                        }
                    }

                    newinsn.Operand = newop ;
                }
                else
                {
                    Type       type  ;
                    MethodBase meth  ;
                    FieldInfo  field ;
                    if ((type = newinsn.Operand as Type) != null)
                    {
                        newinsn.Operand = GetType (type) ;
                    }
                    else
                    if ((meth = newinsn.Operand as MethodBase) != null)
                    {
                        // process template helper methods
                        if (meth.DeclaringType == typeof (TemplateHelpers))
                        {
                            if (meth.IsGenericMethod && GetType (meth.GetGenericArguments ()[0]).IsValueType)
                            {
                                if (meth.Name == nameof (TemplateHelpers.IsNull) ||
                                    meth.Name == nameof (TemplateHelpers.Null))
                                {
                                    throw new InvalidOperationException () ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.IsNullReference))
                                {
                                    newinsn.OpCode  = OpCodes.Pop ;
                                    newinsn.Operand = null ;

                                    il.Emit (OpCodes.Ldc_I4_0) ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.Default))
                                {
                                    var dummy = il.CreateLocal (GetType (meth.GetGenericArguments ()[0])) ;

                                    newinsn.OpCode  = OpCodes.Ldloca ;
                                    newinsn.Operand = dummy ;

                                    il.Emit (OpCodes.Initobj, dummy.VariableType) ;
                                    il.Emit (OpCodes.Ldloc,   dummy) ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.InObject))
                                {
                                    newinsn.NopOut () ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.FromObject))
                                {
                                    newinsn.OpCode  = OpCodes.Unbox_Any ;
                                    newinsn.Operand = GetType (meth.GetGenericArguments ()[0]) ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.ToObject))
                                {
                                    newinsn.OpCode  = OpCodes.Box ;
                                    newinsn.Operand = GetType (meth.GetGenericArguments ()[0]) ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.Constrain))
                                {
                                    switch (newinsn.Previous.OpCode.Code)
                                    {
                                    case Code.Ldnull:
                                    case Code.Ldarg:
                                    case Code.Ldarg_0:
                                    case Code.Ldarg_1:
                                    case Code.Ldarg_2:
                                    case Code.Ldarg_3:
                                    case Code.Ldarg_S:
                                    case Code.Ldloc:
                                    case Code.Ldloc_0:
                                    case Code.Ldloc_1:
                                    case Code.Ldloc_2:
                                    case Code.Ldloc_3:
                                    case Code.Ldloc_S:
                                        newinsn.Previous.NopOut () ;
                                        newinsn.NopOut          () ;
                                        break ;
                                    default:
                                        newinsn.OpCode  = OpCodes.Pop ;
                                        newinsn.Operand = null ;
                                        break ;
                                    }

                                    constrain = GetType (meth.GetGenericArguments ()[0]) ;
                                    continue ;
                                }
                            }
                            else
                            {
                                if (meth.Name == nameof (TemplateHelpers.IsNull) ||
                                    meth.Name == nameof (TemplateHelpers.IsNullReference))
                                {
                                    newinsn.OpCode  = OpCodes.Ldnull ;
                                    newinsn.Operand = null ;

                                    il.Emit (OpCodes.Ceq) ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.Null) ||
                                    meth.Name == nameof (TemplateHelpers.Default))
                                {
                                    newinsn.OpCode  = OpCodes.Ldnull ;
                                    newinsn.Operand = null ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.InObject))
                                {
                                    switch (newinsn.Previous.OpCode.Code)
                                    {
                                    case Code.Ldarga:   newinsn.Previous.OpCode = OpCodes.Ldarg   ; break ;
                                    case Code.Ldarga_S: newinsn.Previous.OpCode = OpCodes.Ldarg_S ; break ;
                                    case Code.Ldloca:   newinsn.Previous.OpCode = OpCodes.Ldloc   ; break ;
                                    case Code.Ldloca_S: newinsn.Previous.OpCode = OpCodes.Ldloc_S ; break ;
                                    case Code.Ldflda:   newinsn.Previous.OpCode = OpCodes.Ldfld   ; break ;
                                    default:
                                        throw new InvalidOperationException () ;
                                    }

                                    newinsn.NopOut () ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.FromObject))
                                {
                                    newinsn.OpCode  = OpCodes.Castclass ;
                                    newinsn.Operand = GetType (meth.GetGenericArguments ()[0]) ;
                                    continue ;
                                }

                                if (meth.Name == nameof (TemplateHelpers.ToObject))
                                {
                                    newinsn.NopOut () ;
                                    continue ;
                                }
                            }

                            if (meth.Name == nameof (TemplateHelpers.CtorCall))
                            {
                                if (newinsn.Previous.OpCode != OpCodes.Newobj)
                                    throw new InvalidOperationException () ;

                                newinsn.Previous.OpCode = OpCodes.Call ;
                                newinsn.NopOut () ;
                                continue ;
                            }

                            if (meth.Name == nameof (TemplateHelpers.New))
                            {
                                var ctor = MakeCtorReference (meth.GetReturnType ()) ;
                                var argz = meth.GetGenericArguments () ;

                                for (var i = 1 ; i < argz.Length ; ++i)
                                    ctor.Parameters.Add (new ParameterDefinition (GetType (argz[i]))) ;

                                newinsn.OpCode  = OpCodes.Newobj ;
                                newinsn.Operand = ctor ;
                                continue ;
                            }

                            if (meth.Name == nameof (TemplateHelpers.Return))
                            {
                                newinsn.OpCode  = OpCodes.Ret ;
                                newinsn.Operand = null ;
                                continue ;
                            }
                        }

                        // TODO: support copying reference-type templates to value-type targets
                        var importedMethod = ImportMethod (meth) ;
                        if (importedMethod.Name == ".ctor" &&
                            importedMethod.DeclaringType.SameAs (m_tValueType))
                        {
                            newinsn.Previous.NopOut () ;
                            newinsn.NopOut          () ;
                            continue ;
                        }

                        newinsn.Operand = importedMethod ;
                    }
                    else
                    if ((field = newinsn.Operand as FieldInfo) != null)
                    {
                        if (field.DeclaringType.DeclaringType == typeof (TemplateHelpers))
                        {
                            if (field.DeclaringType.Name.StartsWith (nameof (TemplateHelpers.RefThis<int>), StringComparison.Ordinal))
                            {
                                if (il.Body.Method.IsStatic ||
                                   !il.Body.Method.DeclaringType.IsValueType)
                                {
                                    throw new InvalidOperationException () ;
                                }

                                newinsn.OpCode  = OpCodes.Ldarg_0 ;
                                newinsn.Operand = null ;
                                continue ;
                            }
                        }

                        newinsn.Operand = ImportField (field) ;
                    }
                    else
                        throw new InvalidOperationException () ;
                }

                // TODO: flow `constrain` along the execution flow
                if (constrain != null && newinsn.OpCode.Code == Code.Callvirt)
                {
                    il.InsertBefore (newinsn, Instruction.Create (OpCodes.Constrained, constrain)) ;
                    constrain  = null ;
                }
            }

            if (constrain != null)
                throw new InvalidOperationException () ;

            foreach (var eh in from.ExceptionHandlingClauses)
            {
                il.Body.ExceptionHandlers.Add (new ExceptionHandler ((ExceptionHandlerType) eh.Flags)
                {
                    CatchType    = GetType (eh.CatchType),
                    TryStart     = insns[eh.TryOffset],
                    TryEnd       = insns[eh.TryOffset + eh.TryLength],
                    FilterStart  = eh.Flags.HasFlag (ExceptionHandlingClauseOptions.Filter) ?
                                   insns[eh.FilterOffset] : null,
                    HandlerStart = insns[eh.HandlerOffset],
                    HandlerEnd   = insns[eh.HandlerOffset + eh.HandlerLength],
                }) ;
            }

            // add debugging information if available
            var methodSymbols  = GetMethodSymbols (method) ;
            if (methodSymbols == null)
                return null ;

            // copy sequence points
            // TODO: copy source links if any
            var dinfo = il.Body.Method.DebugInformation ;

            if (methodSymbols.HasSequencePoints) foreach (var sp in methodSymbols.SequencePoints)
                dinfo.SequencePoints.Add (sp.WithInstruction (insns[sp.Offset])) ;

            // copy local variable scopes
            var scope = CopyScope (methodSymbols.Scope, localz, insns) ;

            // add information on current bindings
            // for now, I just add the mappings as constant strings
            // TODO: add templated parameter bindings
            //
            // adding ImportDebugInformation with type aliases does not do
            // anything visible in the debugger
            // it is possible to deal with templated fields by emitting the templated
            // field with a special DebuggerTypeProxy type and having template code
            // assign a state variable in this field indicating the "current" binding,
            // so that the debugger proxy can return the appropriate value
            foreach (var typevar in m_vars)
            {
                object value ;
                if (m_dictionary.TryGetValue (typevar, out value) && value != null)
                {
                    if (value is Instruction insn)
                        value = insn.Operand ;

                    TypeReference type ;
                    switch (Type.GetTypeCode (value.GetType ()))
                    {
                    case TypeCode.Boolean:
                    case TypeCode.Byte:
                    case TypeCode.Char:
                    case TypeCode.Double:
                    case TypeCode.Int16:
                    case TypeCode.Int32:
                    case TypeCode.Int64:
                    case TypeCode.SByte:
                    case TypeCode.Single:
                    case TypeCode.UInt16:
                    case TypeCode.UInt32:
                    case TypeCode.UInt64:
                        type  = m_target.Module.ImportReference (value.GetType ()) ;
                        break ;
                    default:
                        type  = m_tString ;
                        value = value.ToString () ;
                        break ;
                    }

                    scope.Constants.Add (new ConstantDebugInformation (typevar.Name, type, value)) ;
                }
            }

            if (gmta != null) AddQuasiGenericParameters (method,               gmta, scope) ;
            if (gtta != null) AddQuasiGenericParameters (method.DeclaringType, gtta, scope) ;
            return scope ;
        }

        private void FixUpValueTypeReceiver (Instruction newinsn, int receiverDepth)
        {
            // extremely limited support of emitting method calls and field accesses
            // with reference-type template receivers bound to value-type targets
            for (var insn = newinsn.Previous ; insn.TryBackOutStackBehavior (ref receiverDepth) ; insn = insn.Previous)
            {
                if (receiverDepth == 0)
                {
                    if (insn.OpCode.Code == Code.Ldarg && !((ParameterDefinition) insn.Operand).ParameterType.IsByReference)
                        insn.OpCode = OpCodes.Ldarga ;

                    break ;
                }
            }
        }

        private void AddQuasiGenericParameters (MemberInfo context, Type[] types, ScopeDebugInformation scope)
        {
            Type[] parameters = null ;

            for (var i = 0 ; i < types.Length ; ++i)
            {
                var typevar = types[i] ;
                var bound   = GetType (typevar, throwIfNotBound: false) ;
                if (bound  == null || bound.IsGenericParameter)
                    continue ;

                string name ;
                if (!typevar.IsGenericParameter)
                {
                    if (parameters == null)
                        parameters  = context.MemberType == MemberTypes.Method
                            ? ((MethodInfo) context).GetGenericMethodDefinition ().GetGenericArguments ()
                            : ((Type)       context).GetGenericTypeDefinition   ().GetGenericArguments () ;

                    name = parameters[i].Name ;
                }
                else
                    name = typevar.Name ;

                scope.Constants.Add (new ConstantDebugInformation (name, m_tString, bound.ToString ())) ;
            }
        }

        private Instruction GetOrAdd (Dictionary<int, Instruction> insns, int offset)
        {
            Instruction insn ;
            if (insns.TryGetValue (offset, out insn))
                return insn ;

            insn = Instruction.Create (OpCodes.Nop) ;
            insns.Add (offset, insn) ;
            return insn ;
        }

        private MethodReference MakeCtorReference (Type type)
        {
            var ctor     = new MethodReference (".ctor", m_tVoid) ;
            ctor.HasThis = true ;
            ctor.SetDeclaringType (GetType (type)) ;
            return ctor ;
        }

        private MethodReference MakeDelegateCtorReference (Type type)
        {
            var ctor = MakeCtorReference (type) ;
            ctor.Parameters.Add (new ParameterDefinition (m_tObject)) ;
            ctor.Parameters.Add (new ParameterDefinition (m_tIntPtr)) ;
            return ctor ;
        }

        private TypeDefinition GetTarget (MemberInfo member)
        {
            if (member.DeclaringType == m_template)
            {
                return m_target ;
            }
            else
                return (TypeDefinition) m_dictionary[member.DeclaringType] ;
        }

        private TypeReference GetType (Type type, bool throwIfNotBound = true)
        {
            return (TypeReference) base.GetType (type, null, true, throwIfNotBound) ;
        }

        /// <inherit/>
        protected override object GetTypeInternal (Type type, object genericContext, bool asOpen)
        {
            return ImportTypeInternal (type, new ImportGenericContext (genericContext), asOpen) ;
        }

        private void CopyCAs (MemberInfo from, Mono.Cecil.ICustomAttributeProvider to)
        {
            CopyCAs (from.CustomAttributes, to) ;
        }

        private void CopyCAs (ParameterInfo from, Mono.Cecil.ICustomAttributeProvider to)
        {
            CopyCAs (from.CustomAttributes, to) ;
        }

        private void CopyCAs (IEnumerable<CustomAttributeData> attributes, Mono.Cecil.ICustomAttributeProvider to)
        {
            foreach (var ca in attributes)
                if (ca.AttributeType.Namespace != typeof (TemplateHelpers).Namespace)
                {
                    var newca = new CustomAttribute (m_target.Module.ImportReference (ca.Constructor)) ;

                    foreach (var arg in ca.ConstructorArguments)
                        newca.ConstructorArguments.Add (
                            new CustomAttributeArgument (m_target.Module.ImportReference (arg.ArgumentType), arg.Value)) ;

                    foreach (var arg in ca.NamedArguments)
                        newca.Fields.Add (new Mono.Cecil.CustomAttributeNamedArgument (arg.MemberName,
                            new CustomAttributeArgument (m_target.Module.ImportReference (arg.TypedValue.ArgumentType), arg.TypedValue.Value))) ;

                    to.CustomAttributes.Add (newca) ; // TODO: templated items in custom attributes?
                }
        }

        private MethodDebugInformation GetMethodSymbols (MethodBase method)
        {
            return System.Collections.Immutable.ImmutableInterlocked.GetOrAdd (ref s_symbols, method.Module,
                module => ModuleDefinition.ReadModule (module.Assembly.Location, new ReaderParameters
                {
                    ReadingMode          = ReadingMode.Deferred,
                    AssemblyResolver     = null,
                    SymbolReaderProvider = new DefaultSymbolReaderProvider (false),
                })).SymbolReader?.Read (new MethodDefinition (null, 0, new TypeDefinition (null, null, 0))
                {
                    MetadataToken = new MetadataToken ((uint) method.MetadataToken),
                }) ;
        }
        #endregion

        #region --[Methods: Templated reflection importer]----------------
        TypeReference ImportType (Type type, ImportGenericContext context, bool asOpen = true)
        {
            return (TypeReference) base.GetType (type, context.Value, asOpen, true) ;
        }

        TypeReference ImportTypeInternal (Type type, ImportGenericContext context, bool asOpen)
        {
            if (type.IsByRef)
                return new ByReferenceType (ImportType (type.GetElementType (), context)) ;

            if (type.IsPointer)
                return new PointerType (ImportType (type.GetElementType (), context)) ;

            if (type.IsArray)
                return new ArrayType (ImportType (type.GetElementType (), context), type.GetArrayRank ()) ;

            if (type.IsGenericParameter)
            {
                if (type.DeclaringMethod != null)
                    return context.MethodParameter  (NormalizeMethodName   (type.DeclaringMethod), type.GenericParameterPosition) ;

                if (type.DeclaringType != null)
                    return context.TypeParameter    (NormalizeTypeFullName (type.DeclaringType), type.GenericParameterPosition) ;

                throw new InvalidOperationException () ;
            }

            if (type.IsGenericType && (!type.IsGenericTypeDefinition || asOpen))
                return ImportGenericInstance (type, context);

            switch (Type.GetTypeCode (type))
            {
            case TypeCode.Boolean:
            case TypeCode.Byte:
            case TypeCode.Char:
            case TypeCode.Decimal:
            case TypeCode.Double:
            case TypeCode.Int16:
            case TypeCode.Int32:
            case TypeCode.Int64:
            case TypeCode.SByte:
            case TypeCode.Single:
            case TypeCode.String:
            case TypeCode.UInt16:
            case TypeCode.UInt32:
            case TypeCode.UInt64:
                return m_target.Module.ImportReference (type) ;
            }

            if (type == typeof (void)      ||
                type == typeof (Object)    ||
                type == typeof (ValueType) ||
                type == typeof (IntPtr)    ||
                type == typeof (UIntPtr)   ||
                type == typeof (TypedReference))
            {
                return m_target.Module.ImportReference (type) ;
            }

            // allow limited "forward declarations"
            var reference = new TypeReference (string.Empty, GetEmitName (type),
                m_target.Module, null, type.IsValueType) ;

            if (type.IsNested)
                reference.SetDeclaringType (ImportType (type.DeclaringType, context, asOpen)) ;
            else
            {
                // invoke module's reflection importer to obtain the correct scope
                reference.Scope     = m_target.Module.ImportReference (type).Scope ;
                reference.Namespace = type.Namespace ?? string.Empty ;
            }

            if (type.IsGenericType)
                ImportGenericParameters (reference, type.GetGenericArguments ()) ;

            return reference ;
        }

        TypeReference ImportGenericInstance (Type type, ImportGenericContext context)
        {
            var elementType = ImportType (type.GetGenericTypeDefinition (), context, false) ;

            // NB: this check prevents adding unnecessary generic arguments
            // to the imported reference when `type` is a quasi-generic
            // template type that emits a non-generic type definition
            // I have no idea what will happen if emitting a genuine generic
            // type definition, and in particular mixing bound (quasi)
            // and unbound generic parameters will be difficult and may require
            // keeping a mapping between template and emitted generic parameters
            // in ImportGenericContext
            if(!elementType.HasGenericParameters)
                return elementType ;

            var instance = new GenericInstanceType (elementType) ;

            using (context.Push (elementType))
            {
                var arguments = type.GetGenericArguments () ;
                var instArgs  = instance.GenericArguments ;

                for (int i = 0 ; i < arguments.Length ; ++i)
                    instArgs.Add (ImportType (arguments[i], context)) ;
            }

            return instance ;
        }

        FieldReference ImportField (FieldInfo field, ImportGenericContext context = default (ImportGenericContext))
        {
            var declaringType = ImportType (field.DeclaringType, context) ;

            // NB: only go via the context route if I'm going to import the field into a generic
            if (field.DeclaringType.IsGenericInstance () && declaringType.IsGenericInstance)
                field = field.Module.ResolveField (field.MetadataToken) ;

            using (context.Push (declaringType))
            {
                // allow limited "forward declarations"
                var reference = new FieldReference (GetEmitName (field), ImportType (field.FieldType, context)) ;
                reference.SetDeclaringType (declaringType) ;
                return reference ;
            }
        }

        MethodReference ImportMethod (MethodBase method, ImportGenericContext context = default (ImportGenericContext), bool asOpen = true)
        {
            if (method.IsGenericMethod && (!method.IsGenericMethodDefinition || asOpen))
                return ImportMethodSpecification (method, context) ;

            var declaringType = ImportType (method.DeclaringType, context) ;

            // NB: only go via the context route if I'm going to import the field into a generic
            if (method.DeclaringType.IsGenericInstance () && declaringType.IsGenericInstance)
                method = method.Module.ResolveMethod (method.MetadataToken) ;

            // allow limited "forward declarations"
            var reference = new MethodReference (GetEmitName (method), m_tVoid)
            {
                HasThis       = method.CallingConvention.HasFlag (CallingConventions.HasThis),
                ExplicitThis  = method.CallingConvention.HasFlag (CallingConventions.ExplicitThis),
                DeclaringType = ImportType (method.DeclaringType, context, false), // for generic context
            } ;

            if (method.CallingConvention.HasFlag (CallingConventions.VarArgs))
                reference.CallingConvention &= MethodCallingConvention.VarArg ;

            if (method.IsGenericMethod)
                ImportGenericParameters (reference, method.GetGenericArguments ()) ;

            using (context.Push (reference))
            {
                var methodInfo  = method as MethodInfo ;
                if (methodInfo != null)
                    reference.ReturnType = ImportType (methodInfo.ReturnType, context) ;

                var parameters = method.GetParameters () ;
                var refParams  = reference.Parameters ;

                for (int i = 0 ; i < parameters.Length ; ++i)
                    refParams.Add (new ParameterDefinition (ImportType (parameters[i].ParameterType, context))) ;
            }

            reference.SetDeclaringType (declaringType) ;
            return reference ;
        }

        MethodReference ImportMethodSpecification (MethodBase method, ImportGenericContext context)
        {
            var methodInfo  = method as MethodInfo ;
            if (methodInfo == null)
                throw new InvalidOperationException () ;

            var elementMethod = ImportMethod (methodInfo.GetGenericMethodDefinition (), context, false) ;
            var instance      = new GenericInstanceMethod (elementMethod) ;

            using (context.Push (elementMethod))
            {
                var arguments = method.GetGenericArguments () ;
                var instArgs  = instance.GenericArguments ;

                for (int i = 0 ; i < arguments.Length ; ++i)
                    instArgs.Add (ImportType (arguments[i], context)) ;
            }

            return instance ;
        }

        static string NormalizeMethodName (MethodBase method)
        {
            return NormalizeTypeFullName (method.DeclaringType) + "." + method.Name ;
        }

        static string NormalizeTypeFullName (Type type)
        {
            return type.IsNested ? NormalizeTypeFullName (type.DeclaringType) + "/" + type.Name : type.FullName ;
        }

        static void ImportGenericParameters (IGenericParameterProvider provider, Type[] arguments)
        {
            var parameters = provider.GenericParameters ;

            foreach (var argument in arguments)
                parameters.Add (new GenericParameter (argument.Name, provider)) ;
        }
        #endregion
    }

    struct ImportGenericContext : IDisposable
    {
        #region --[Fields: Private]---------------------------------------
        private List<IGenericParameterProvider> m_stack ;
        #endregion

        #region --[Constructors]------------------------------------------
        public ImportGenericContext (IGenericParameterProvider provider)
        {
            if (provider == null)
                throw new ArgumentNullException (nameof (provider)) ;

            m_stack = null  ;
            Push (provider) ;
        }

        public ImportGenericContext (object value)
        {
            m_stack = (List<IGenericParameterProvider>) value ;
        }
        #endregion

        #region --[Properties: Public]------------------------------------
        public object Value { get { return m_stack ; }}
        #endregion

        #region --[Methods]-----------------------------------------------
        public ImportGenericContext Push (IGenericParameterProvider provider)
        {
            if (m_stack == null)
                m_stack  = new List<IGenericParameterProvider> (1) ;

            m_stack.Add (provider) ;
            return this ;
        }

        void IDisposable.Dispose ()
        {
            m_stack.RemoveAt (m_stack.Count - 1) ;
        }

        public TypeReference TypeParameter (string type, int position)
        {
            if  (null != m_stack)
            for (int i = m_stack.Count - 1 ; i >= 0 ; --i)
            {
                var candidate = GenericTypeFor (m_stack[i]) ;
                if (candidate.FullName != type)
                    continue ;

                return candidate.GenericParameters[position] ;
            }

            throw new InvalidOperationException () ;
        }

        public TypeReference MethodParameter (string method, int position)
        {
            if  (null != m_stack)
            for (int i = m_stack.Count - 1 ; i >= 0 ; --i)
            {
                var candidate  = m_stack[i] as MethodReference ;
                if (candidate == null)
                    continue ;

                if (method != NormalizeMethodName (candidate))
                    continue ;

                return candidate.GenericParameters[position] ;
            }

            throw new InvalidOperationException () ;
        }

        static string NormalizeMethodName (MethodReference method)
        {
            return method.DeclaringType.GetElementType ().FullName + "." + method.Name ;
        }

        static TypeReference GenericTypeFor (IGenericParameterProvider context)
        {
            var type  = context as TypeReference ;
            if (type != null)
                return type.GetElementType () ;

            var method  = context as MethodReference ;
            if (method != null)
                return method.DeclaringType.GetElementType () ;

            throw new InvalidOperationException () ;
        }
        #endregion
    }
}
