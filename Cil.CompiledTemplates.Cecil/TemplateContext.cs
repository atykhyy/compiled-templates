#region --[Copyright]-----------------------------------------------------
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
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    /// <summary>
    /// Represents a single template application to a single target type.
    /// </summary>
    public sealed class TemplateContext : TemplateContextBase
    {
        #region --[Fields: Private]---------------------------------------
        private readonly TypeDefinition     m_target   ;
        private readonly TypeReference      m_tVoid    ;
        private readonly TypeReference      m_tObject  ;
        private readonly TypeReference      m_tIntPtr  ;
        private readonly TypeReference      m_tString  ;

        private readonly Dictionary<ILProcessor, ILBranchManager> m_splices = new Dictionary<ILProcessor, ILBranchManager> () ;
        private readonly Dictionary<ILProcessor, Scope>           m_scopes  = new Dictionary<ILProcessor, Scope> () ;

        private readonly static MethodReference NullMethod = new MethodReference (null, new TypeReference (null, null, null, null)) ;

        private readonly static OpCode[] OneByteOpCode  = (OpCode[]) typeof (OpCodes).GetField (nameof (OneByteOpCode),  BindingFlags.Static | BindingFlags.NonPublic).GetValue (null) ;
        private readonly static OpCode[] TwoBytesOpCode = (OpCode[]) typeof (OpCodes).GetField (nameof (TwoBytesOpCode), BindingFlags.Static | BindingFlags.NonPublic).GetValue (null) ;
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

            m_target   = target ;
            m_tVoid    = target.Module.ImportReference (typeof (void))   ;
            m_tObject  = target.Module.ImportReference (typeof (object)) ;
            m_tIntPtr  = target.Module.ImportReference (typeof (IntPtr)) ;
            m_tString  = target.Module.ImportReference (typeof (String)) ;

            m_dictionary = m_dictionary.Add (m_template, m_target) ;
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
        /// <remarks>
        /// The type referred to by <paramref name="type"/> can be defined anywhere,
        /// but the reference must belong to the target module.
        /// </remarks>
        public void Bind (Type template, TypeReference type)
        {
            Add (GetTemplatedType (template), type) ;
        }

        /// <summary>
        /// Binds the templated field identified by the lambda expression <paramref name="func"/>
        /// to <paramref name="field"/>.
        /// </summary>
        /// <remarks>
        /// <paramref name="field"/> must be an instance field if the templated field
        /// is an instance field, and a static field if the templated field is static.
        /// <para/>
        /// The field referred to by <paramref name="field"/> can be defined anywhere,
        /// but the reference must belong to the target module.
        /// </remarks>
        public void BindField<T> (Expression<Func<T>> func, FieldReference field)
        {
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
            if (method != null && method.HasThis)
                throw new ArgumentOutOfRangeException (nameof (method)) ;

            var template = GetTemplatedField (func, enforceIsStatic: true) ;
            if (!typeof (Delegate).IsAssignableFrom (template.FieldType))
                throw new ArgumentOutOfRangeException (nameof (func)) ;

            Add (template, method ?? NullMethod) ;
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
            Add (GetTemplatedMethod (func.Method, null), method) ;
        }
        #endregion

        #region --[Methods: template application]-------------------------
        /// <summary>
        /// Finalizes methods generated or modified by the template.
        /// </summary>
        public void Commit ()
        {
            foreach (var il in m_splices.Keys)
                il.Body.OptimizeMacros () ;

            foreach (var kv in m_scopes)
                kv.Key.Body.Method.DebugInformation.Scope = kv.Value.ToCecil () ;
        }

        /// <inherit/>
        public override void CopyExplicitInterfaceImpl (Type type)
        {
            var fixups = new Dictionary<MethodBase, Action<MethodDefinition>> () ;
            var prefix = type.GetPseudoSourceFullName () ;

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

                m_target.Properties.Add (prop) ;

                if (property.GetMethod != null) fixups.Add (property.GetMethod, _ => prop.GetMethod = _) ;
                if (property.SetMethod != null) fixups.Add (property.GetMethod, _ => prop.SetMethod = _) ;
            }

            foreach (var event_ in type.GetEvents ())
            {
                var evt = new EventDefinition (prefix + "." + event_.Name,
                    Mono.Cecil.EventAttributes.SpecialName, GetType (event_.EventHandlerType)) ;

                m_target.Events.Add (evt) ;

                if (event_.AddMethod    != null) fixups.Add (event_.AddMethod,    _ => evt.AddMethod    = _) ;
                if (event_.RemoveMethod != null) fixups.Add (event_.RemoveMethod, _ => evt.RemoveMethod = _) ;
            }

            var mapping = m_template.GetInterfaceMap (type) ;

            for (int i  = 0 ; i < mapping.TargetMethods.Length ; ++i)
            {
                // TODO: I used to add a suppression attribute for CA1033 for these methods,
                // but that attribute is [Conditional]; I am not sure how this all works
                var copy = CopyMethodInternal (mapping.TargetMethods[i]) ;
                if(!copy.IsPrivate)
                    throw new InvalidOperationException () ;

                // TODO: template-dependent generic interfaces etc.
                copy.Overrides.Add (m_target.Module.ImportReference (mapping.InterfaceMethods[i])) ;

                Action<MethodDefinition> fixup ;
                if (fixups.TryGetValue (mapping.InterfaceMethods[i], out fixup))
                    fixup (copy) ;
            }

            m_target.Interfaces.Add (new InterfaceImplementation (
                m_target.Module.ImportReference (type))) ;
        }

        /// <inherit/>
        public override void CopyNested (Type type, SR.TypeAttributes set = 0, SR.TypeAttributes clear = 0)
        {
            // TODO: multi-level nesting
            if (!type.IsNested || !type.DeclaringType.IsAssignableFrom (m_template))
                throw new ArgumentOutOfRangeException (nameof (type)) ;

            var template = type ;
            var newtype  = new TypeDefinition (null, template.Name, (Mono.Cecil.TypeAttributes)((template.Attributes & ~clear) | set)) ;

            var emitAttr  = type.GetCustomAttribute<EmitNameAttribute> () ;
            if (emitAttr != null)
            {
                if (emitAttr.Name != null)
                    newtype.Name   = emitAttr.Name ;

                if (emitAttr.Private)
                    newtype.Attributes = (newtype.Attributes & ~Mono.Cecil.TypeAttributes.VisibilityMask) | Mono.Cecil.TypeAttributes.NestedPrivate ;

                if (emitAttr.Protected)
                    newtype.Attributes = (newtype.Attributes & ~Mono.Cecil.TypeAttributes.VisibilityMask) | Mono.Cecil.TypeAttributes.NestedFamily ;
            }

            // set the base type if it's templated
            if (type.BaseType.IsDefined (typeof (TemplatedMemberAttribute), false))
            {
                newtype.BaseType = GetType (type.BaseType) ;
            }

            CopyCAs        (template, newtype) ;
            m_target.NestedTypes.Add (newtype) ;

            m_dictionary = m_dictionary.Add (template, newtype) ;
        }

        /// <inherit/>
        protected override void CopyField (FieldInfo field)
        {
            var template = field ;
            var newfield = new FieldDefinition (template.Name, (Mono.Cecil.FieldAttributes) template.Attributes, GetType (template.FieldType)) ;

            // TODO: copy fields to nested types
            // TODO: copy methods from "base" classes
            CopyCAs   (template, newfield) ;
            m_target.Fields.Add (newfield) ;

            var emitAttr  = field.GetCustomAttribute<EmitNameAttribute> () ;
            if (emitAttr != null)
            {
                if (emitAttr.Name != null)
                    newfield.Name  = emitAttr.Name ;

                if (emitAttr.ReadOnly)
                    newfield.IsInitOnly = true ;

                if (emitAttr.Private)
                    newfield.Attributes = (newfield.Attributes & ~Mono.Cecil.FieldAttributes.FieldAccessMask) | Mono.Cecil.FieldAttributes.Private ;

                if (emitAttr.Protected)
                    newfield.Attributes = (newfield.Attributes & ~Mono.Cecil.FieldAttributes.FieldAccessMask) | Mono.Cecil.FieldAttributes.Family ;
            }

            m_dictionary = m_dictionary.Add (template, newfield) ;
        }

        /// <inherit/>
        protected override void CopyMethod (MethodBase method, SR.MethodAttributes set = 0, SR.MethodAttributes clear = 0)
        {
            CopyMethodInternal (method, set, clear) ;
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

            for ( ; p < parameters.Length ; ++p)
            {
                if (parameters[p].IsDefined (typeof (TemplatedParameterAttribute)))
                    break ;

                if (parameters[p].IsDefined (typeof (FromILStackAttribute)))
                    throw new InvalidOperationException () ; // must be templated

                if (il.Body.Method.Parameters.Count <= p)
                    throw new InvalidOperationException () ; // not enough parameters in target method to match splice's non-templated parameters

                if(!il.Body.Method.Parameters[p].ParameterType.SameAs (GetType (parameters[p].ParameterType)))
                    throw new InvalidOperationException () ; // splice's non-templated parameter type does not match target method parameter type

                dictionary.Add (parameters[p], il.Body.Method.Parameters[p]) ;
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
            if (!m_splices.TryGetValue (il, out branchManager))
            {
                branchManager = new ILBranchManager (il) ;
                m_splices.Add (il, branchManager) ;
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

                insnLeft.OpCode  = OpCodes.Nop ;
                insnLeft.Operand = null ;
                break ;

            case SpliceRelation.AfterAsPart:
                insnRight = null ;
                insnLeft  = location.Insn ;
                break ;
            }

            // NB: an intermediate scope structure is necessary
            // because ScopeDebugInformation does not expose Instruction references
            var rootScope = PrepareVariableScopes (il) ;

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
                    // remove simple loads, skip nops
                    if (simple)
                    {
                    repeat:
                        var isTarget  = insnRight != null && branchManager.IsTarget (insnRight) ;
                        if(!isTarget)
                        {
                            // move one instruction left
                            insnRight = insnLeft ;
                            insnLeft  = insnLeft.Previous ;

                            if (insnRight.IsSimpleLoad ())
                            {
                                dictionary.Add (pars[i], insnRight.Clone ()) ;

                                // can't use RemoveAt (see note above)
                                insnRight.OpCode  = OpCodes.Nop ;
                                insnRight.Operand = null ;
                                continue ;
                            }

                            if (insnRight.OpCode == OpCodes.Nop)
                                goto repeat ;
                        }

                        simple = false ;
                    }

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
                rootScope.Splice (spliceScope, location) ;
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

            var scope  = CopyMethodBody (from, fromBody, to.Body.GetILProcessor (), null, dictionary) ;
            if (scope != null)
                to.DebugInformation.Scope = scope.ToCecil () ;

            to.Body.OptimizeMacros () ;
            return to ;
        }

        private Scope CopyMethodBody (MethodBase method, System.Reflection.MethodBody from, ILProcessor il, Instruction ret,
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
                    newinsn.Operand = method.Module.ResolveField (BitConverter.ToInt32 (bytes, offset)) ;
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
                    newinsn.Operand = method.Module.ResolveMethod (BitConverter.ToInt32 (bytes, offset)) ;
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
                    newinsn.Operand = method.Module.ResolveMember (BitConverter.ToInt32 (bytes, offset)) ;
                    offset += 4 ;
                    break ;
                case OperandType.InlineType:
                    newinsn.Operand = method.Module.ResolveType (BitConverter.ToInt32 (bytes, offset)) ;
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
                    ParameterDefinition pd ;
                    VariableDefinition  vd ;
                    MethodDefinition    md ;
                    Instruction         li ;
                    if ((field = newop as FieldReference) != null)
                    {
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
                        // convert bindings of storage locations to methods as delegate creation
                        Type type ;
                        switch (newinsn.OpCode.Code)
                        {
                        default:           throw new InvalidOperationException () ;
                        case Code.Ldarg:   type = ((ParameterInfo) newinsn.Operand).ParameterType ; break ;
                        case Code.Ldsfld:  type = ((FieldInfo)     newinsn.Operand).FieldType     ; break ;
                        }

                        // TODO: only static methods for now
                        newinsn.OpCode  = OpCodes.Ldnull ;
                        newinsn.Operand = null ;

                        if (meth != NullMethod)
                        {
                            il.Emit (OpCodes.Ldftn,  meth) ;
                            il.Emit (OpCodes.Newobj, MakeDelegateCtorReference (type)) ;
                        }

                        continue ;
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
                            if (meth.Name == nameof (TemplateHelpers.IsNull))
                            {
                                if (GetType (meth.GetGenericArguments ()[0]).IsValueType)
                                    throw new InvalidOperationException () ;

                                newinsn.OpCode  = OpCodes.Ldnull ;
                                newinsn.Operand = null ;

                                il.Emit (OpCodes.Ceq) ;
                                continue ;
                            }

                            if (meth.Name == nameof (TemplateHelpers.Null))
                            {
                                if (GetType (meth.GetGenericArguments ()[0]).IsValueType)
                                    throw new InvalidOperationException () ;

                                newinsn.OpCode  = OpCodes.Ldnull ;
                                newinsn.Operand = null ;
                                continue ;
                            }

                            if (meth.Name == nameof (TemplateHelpers.Return))
                            {
                                newinsn.OpCode  = OpCodes.Ret ;
                                newinsn.Operand = null ;
                                continue ;
                            }
                        }

                        // TODO: what if a method is both?
                        // to deal with all the potential complexities, I'll have
                        // to re-implement the import code in Mono.Cecil/Import.cs
                        // inserting template bindings and GetEmitName's where appropriate
                        if (meth.IsGenericMethod)
                        {
                            var newmeth = new GenericInstanceMethod (m_target.Module.ImportReference (
                                ((MethodInfo)meth).GetGenericMethodDefinition ())) ;

                            foreach (var typeArgument in meth.GetGenericArguments ())
                                newmeth.GenericArguments.Add (GetType (typeArgument)) ;

                            newinsn.Operand = newmeth ;
                        }
                        else
                        if (meth.DeclaringType.IsGenericType && !meth.DeclaringType.IsGenericTypeDefinition)
                        {
                            var genmeth = m_target.Module.ImportReference (meth.Module.ResolveMethod (meth.MetadataToken)) ;

                            newinsn.Operand = genmeth.CloseDeclaringType (Array.ConvertAll (meth.DeclaringType.GetGenericArguments (), GetType)) ;
                        }
                        else
                        {
                            // allow limited "forward declarations"
                            var newmeth = new MethodReference (GetEmitName (meth),
                                GetType (meth.GetReturnType ()),
                                GetType (meth.DeclaringType)) ;

                            newmeth.HasThis      = (meth.CallingConvention & CallingConventions.HasThis)      != 0 ;
                            newmeth.ExplicitThis = (meth.CallingConvention & CallingConventions.ExplicitThis) != 0 ;

                            foreach (var param in meth.GetParameters ())
                                newmeth.Parameters.Add (new ParameterDefinition (param.Name, (Mono.Cecil.ParameterAttributes) param.Attributes,
                                    GetType (param.ParameterType))) ;

                            newinsn.Operand = newmeth ;
                        }
                    }
                    else
                    if ((field = newinsn.Operand as FieldInfo) != null)
                    {
                        if (field.DeclaringType.IsGenericType && !field.DeclaringType.IsGenericTypeDefinition)
                        {
                            var genfield = m_target.Module.ImportReference (meth.Module.ResolveField (field.MetadataToken)) ;

                            newinsn.Operand = genfield.CloseDeclaringType (Array.ConvertAll (field.DeclaringType.GetGenericArguments (), GetType)) ;
                        }
                        else
                        {
                            // allow limited "forward declarations"
                            newinsn.Operand = new FieldReference (GetEmitName (field),
                                GetType (field.FieldType),
                                GetType (field.DeclaringType)) ;
                        }
                    }
                    else
                        throw new InvalidOperationException () ;
                }
            }

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
            var dinfo   = il.Body.Method.DebugInformation  ;
            var spoints = methodSymbols.SequencePointCount ;
            var offsets = new int[spoints] ;
            var slines  = new int[spoints] ;
            var elines  = new int[spoints] ;
            var scols   = new int[spoints] ;
            var ecols   = new int[spoints] ;
            var symdocs = new System.Diagnostics.SymbolStore.ISymbolDocument[spoints] ;
            methodSymbols.GetSequencePoints (offsets, symdocs, slines, scols, elines, ecols) ;

            for (int i  = 0 ; i < spoints ; ++i)
            {
                var sd = new Document (symdocs[i].URL) ;

                // TODO: not implemented in unmanaged symbol reader
                //sd.Hash           = symdocs[i].GetCheckSum () ;
                //sd.HashAlgorithm  = PdbGuidMapping.ToHashAlgorithm (symdocs[i].CheckSumAlgorithmId) ;
                sd.LanguageVendor = PdbGuidMapping.ToVendor        (symdocs[i].LanguageVendor) ;
                sd.Language       = PdbGuidMapping.ToLanguage      (symdocs[i].Language) ;
                sd.Type           = PdbGuidMapping.ToType          (symdocs[i].DocumentType) ;

                var sp = new SequencePoint (insns[offsets[i]], sd) ;
                dinfo.SequencePoints.Add   (sp) ;

                sp.StartLine   = slines[i] ;
                sp.EndLine     = elines[i] ;
                sp.StartColumn = scols[i]  ;
                sp.EndColumn   = ecols[i]  ;
            }

            // copy local variable scopes
            var scope = new Scope (methodSymbols.RootScope, localz, insns) ;

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
                    scope.AddConstant (typevar.Name, m_tString, value.ToString ()) ;
            }

            return scope ;
        }

        private Scope PrepareVariableScopes (ILProcessor il)
        {
            Scope scope ;
            if (!m_scopes.TryGetValue (il, out scope))
            {
                scope = Scope.FromMethodBody (il.Body) ;
                m_scopes.Add (il, scope) ;
            }

            return scope ;
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

        private MethodReference MakeDelegateCtorReference (Type type)
        {
            var ctor     = new MethodReference (".ctor", m_tVoid, GetType (type)) ;
            ctor.HasThis = true ;

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

        private new TypeReference GetType (Type type)
        {
            return (TypeReference) base.GetType (type) ;
        }

        /// <inherit/>
        protected override object GetTypeInternal (Type type)
        {
            object value ;
            if (type.DeclaringType != null && m_dictionary.TryGetValue (type.DeclaringType, out value))
            {
                // allow limited "forward declarations"
                var emitName = GetEmitName (type) ;
                var emitType = ((TypeDefinition) value).NestedTypes.Single (_ => _.Name == emitName) ;
                return emitType ;
            }

            if (type.IsGenericType && !type.IsGenericTypeDefinition)
            {
                var git  = new GenericInstanceType (GetType (type.GetGenericTypeDefinition ())) ;

                foreach (var param in type.GetGenericArguments ())
                    git.GenericArguments.Add (GetType (param)) ;

                return git ;
            }
            else
                return m_target.Module.ImportReference (type) ;
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
        #endregion
    }
}
