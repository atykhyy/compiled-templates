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
using System.Collections.Immutable ;
using System.Reflection  ;
using Mono.Cecil         ;
using Mono.Cecil.Cil     ;
using Mono.Cecil.Rocks   ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    public sealed class TemplateContext
    {
        #region --[Fields: Private]---------------------------------------
        private readonly TypeDefinition     m_target   ;
        private readonly TypeReference      m_tVoid    ;
        private readonly TypeReference      m_tObject  ;
        private readonly TypeReference      m_tIntPtr  ;
        private readonly TypeDefinition     m_template ;
        private readonly TypeReference      m_templateClosedOverSelf ;
        private readonly Type               m_templateClrType        ;
        private readonly GenericParameter[] m_templateSelfArguments  ;

        private ImmutableDictionary<object, object> m_dictionary ;

        private readonly Dictionary<ILProcessor, ILBranchManager> m_splices = new Dictionary<ILProcessor, ILBranchManager> () ;

        private readonly static IEqualityComparer<object>              s_comparer = new MemberReferenceComparer () ;
        private readonly static Dictionary<Assembly, ModuleDefinition> s_modules  = new Dictionary<Assembly, ModuleDefinition> () ;

        private readonly static object s_dummy = new object () ;
        private readonly static object s_scope = new object () ;

        private sealed class MemberReferenceComparer : IEqualityComparer<object>
        {
            public new bool Equals (object x, object y)
            {
                var mrx = x as MemberReference ;
                var mry = y as MemberReference ;

                if (mrx == null || mry == null)
                    return Object.Equals (x, y) ;

                // XXX
                return mrx.FullName == mry.FullName ;
            }

            public int GetHashCode (object obj)
            {
                if (obj is MemberReference)
                    return ((MemberReference) obj).FullName.GetHashCode () ;

                return obj.GetHashCode () ;
            }
        }

        private sealed class DictionaryState : IDisposable
        {
            private readonly TemplateContext                     m_context ;
            private readonly ImmutableDictionary<object, object> m_state ;

            public DictionaryState (TemplateContext context)
            {
                m_context = context ;
                m_state   = context.m_dictionary ;
            }

            public void Dispose ()
            {
                m_context.m_dictionary = m_state ;
            }
        }
        #endregion

        public readonly static MethodReference NullMethod = new MethodReference (null, new TypeReference (null, null, null, null)) ;

        public TypeDefinition Target
        {
            get { return m_target ; }
        }

        #region --[Constructors]------------------------------------------
        public TemplateContext (Type template, TypeDefinition target)
        {
            if (template.IsGenericType)
            {
                // TODO: implementing generic templates (i.e. one creating generic
                // methods, not gratuitously generic as I had at first) will require
                // a thorough understanding of what, where and why is GenericInstance etc.
                // not just poking and prodding the code until it "works"
                // i.e. with generics GetType (Type type) does not return anything usable,
                // but GetType (CloseTemplateType (GetTemplateType (type)) does
                // I'm leaving what code there is around generic templates for future reference
                throw new NotSupportedException () ;
            }

            m_dictionary = ImmutableDictionary.Create<object, object> (s_comparer) ;

            ModuleDefinition module ;
            if(!s_modules.TryGetValue (template.Assembly, out module))
            {
                // TODO: is this the right way to handle assembly resolution?
                var ar = new DefaultAssemblyResolver () ;
                ar.AddSearchDirectory (System.IO.Path.GetDirectoryName (template.Assembly.Location)) ;

                module = ModuleDefinition.ReadModule (template.Assembly.Location, new ReaderParameters
                {
                    ReadingMode      = ReadingMode.Deferred,
                    ReadSymbols      = true,
                    AssemblyResolver = ar,
                }) ;

                s_modules.Add (template.Assembly, module) ;
            }

            m_templateClrType = template ;
            m_template        = module.GetType (template.Namespace, template.Name) ;

            m_target  = target ;
            m_tVoid   = target.Module.Import (typeof (void))   ;
            m_tObject = target.Module.Import (typeof (object)) ;
            m_tIntPtr = target.Module.Import (typeof (IntPtr)) ;

            if (m_template.HasGenericParameters)
            {
                m_templateSelfArguments  = m_template.GenericParameters.ToArray () ;
                m_templateClosedOverSelf = m_template.MakeGenericInstanceType   (m_templateSelfArguments) ;
            }
            else
            {
                m_templateSelfArguments  = null ;
                m_templateClosedOverSelf = m_template ;
            }

            m_dictionary = m_dictionary.Add (m_templateClosedOverSelf, m_target) ;
        }
        #endregion

        #region --[Methods: context manipulation]-------------------------
        public IDisposable Push (string scope)
        {
            var disposable = new DictionaryState  (this) ;
            m_dictionary   = m_dictionary.SetItem (s_scope, scope) ;
            return disposable ;
        }

        public object Get (Type template)
        {
            return m_dictionary[GetTemplatedType (template)] ;
        }

        public object GetField<T> (Expression<Func<T>> func)
        {
            return m_dictionary[GetTemplatedField (func)] ;
        }

        public object Get_ (Expression<Action> expr)
        {
            // TODO: GetXxx for non-templated members?
            return m_dictionary[GetTemplatedMethod (expr)] ;
        }

        public void Bind (Type template, Type type)
        {
            Add (GetTemplatedType (template), GetType (type)) ;
        }

        public void Bind (Type template, TypeReference type)
        {
            Add (GetTemplatedType (template), type) ;
        }

        public void BindField<T> (Expression<Func<T>> func, FieldReference field)
        {
            var templatedField = GetTemplatedField (func, enforceIsStatic: field.Resolve ().IsStatic) ;
            var declaringType  = templatedField.DeclaringType ;

            // automatically bind the nested type to the field's declaring type
            if (declaringType != m_templateClosedOverSelf)
                Set (declaringType, field.DeclaringType) ;

            Add (templatedField, field) ;
        }

        public void BindField<T> (Expression<Func<T>> func, Action<ILProcessor, Instruction> action)
        {
            Add (GetTemplatedField (func), action) ;
        }

        public void BindField<T> (Expression<Func<T>> func, VariableDefinition local)
        {
            Add (GetTemplatedField (func, enforceIsStatic: true), local) ;
        }

        public void BindField<T> (Expression<Func<T>> func, MethodReference method)
        {
            if (method != null && method.HasThis)
                throw new ArgumentOutOfRangeException ("method") ;

            Add (GetTemplatedField (func, enforceIsStatic: true), method ?? NullMethod) ;
        }

        public void Bind_ (Expression<Action> expr, MethodReference method)
        {
            Add (GetTemplatedMethod (expr), method) ;
        }

        public void Bind<T> (Func<T> func, MethodReference method)
        {
            Add (GetTemplatedMethod (func.Method, null), method) ;
        }

        private TypeReference GetTemplatedType (Type template)
        {
            return CloseTemplateType (VerifyTemplatedMember (GetTemplateType (template))) ;
        }

        private FieldReference GetTemplatedField (LambdaExpression func, bool? enforceIsStatic = null)
        {
            var mex  = func.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Field)
                throw new ArgumentOutOfRangeException ("func") ;

            if (enforceIsStatic.HasValue && enforceIsStatic != ((FieldInfo) mex.Member).IsStatic)
                throw new ArgumentOutOfRangeException ("func") ;

            return GetTemplatedField (GetTemplateType (mex.Member.DeclaringType), mex.Member.Name) ;
        }

        private FieldReference GetTemplatedField (TypeDefinition source, string name)
        {
            return CloseTemplateField (VerifyTemplatedMember (source.Fields.Single (_ => _.Name == name))) ;
        }

        private MethodReference GetTemplatedMethod (LambdaExpression func)
        {
            var fex  = func.Body as MethodCallExpression ;
            if (fex != null)
                return GetTemplatedMethod (fex.Method, fex.Object?.Type) ;

            var nex  = func.Body as NewExpression ;
            if (nex != null)
                return GetTemplatedMethod (nex.Constructor, null) ;

            throw new ArgumentOutOfRangeException ("expr") ;
        }

        private MethodReference GetTemplatedMethod (MethodBase method, Type source)
        {
            return CloseTemplateMethod (VerifyTemplatedMember (GetTemplate (GetMethodInType (method, source)))) ;
        }

        private void Add (object key, object value)
        {
            m_dictionary = m_dictionary.Add (key, value) ;
        }

        private void Set (object key, object value)
        {
            m_dictionary = m_dictionary.SetItem (key, value) ;
        }
        #endregion

        #region --[Methods: template application]-------------------------
        public void OptimizeSplices ()
        {
            foreach (var il in m_splices.Keys)
                il.Body.OptimizeMacros () ;
        }

        public void CopyLabel (Type label)
        {
            var labels = new HashSet<Type> () ;
            GatherLabels (labels, label) ;
            CopyLabels   (m_templateClrType, labels) ;
        }

        private void GatherLabels (HashSet<Type> labels, Type label)
        {
            if (!labels.Add (label))
                return ;

            foreach (var ca in label.GetCustomAttributes<EmitLabelAttribute> ())
            {
                if (ca.Labels == null)
                    throw new InvalidOperationException () ;

                foreach (var la in ca.Labels)
                    GatherLabels (labels, la) ;
            }
        }

        private void CopyLabels (Type template, HashSet<Type> labels)
        {
            if (template.BaseType.Assembly != typeof (object).Assembly)
                CopyLabels (template.BaseType, labels) ;

            var all = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly ;

            // TODO: copy the nested types themselves? ordering becomes tricky!
            foreach (var type in template.GetNestedTypes (all))
                    CopyLabels   (type, labels) ;

            foreach (var ctor in template.GetConstructors (all))
                if (MatchesLabel (ctor, labels) || MatchesLabelInParams (ctor, labels))
                    CopyMethod   (ctor) ;

            foreach (var meth in template.GetMethods (all | BindingFlags.Static))
                if (MatchesLabel (meth, labels) || MatchesLabelInParams (meth, labels))
                    CopyMethod   (meth) ;

            foreach (var field in template.GetFields (all | BindingFlags.Static))
                if (MatchesLabel (field, labels))
                    CopyField    (field) ;

            foreach (var ca in template.GetCustomAttributes<EmitExplicitInterfaceImplAttribute> ())
                if (MatchesLabel (ca, labels))
                    CopyExplicitInterfaceImpl (ca.Interface) ;
        }

        private bool MatchesLabel (MemberInfo member, HashSet<Type> labels)
        {
            foreach (var ca in member.GetCustomAttributes<EmitLabelAttribute> ())
                if (MatchesLabel (ca, labels))
                    return true ;

            return false ;
        }

        private bool MatchesLabel (IEmitLabelAttribute ca, HashSet<Type> labels)
        {
            if (ca.Labels == null)
                throw new InvalidOperationException () ;

            foreach (var la in ca.Labels)
                if (labels.Contains (la))
                    return true ;

            return false ;
        }

        private bool MatchesLabelInParams (MethodBase method, HashSet<Type> labels)
        {
            foreach (var pa in method.GetParameters ())
                if (pa.IsDefined (typeof (EmitLabelAttribute)) && labels.Contains (pa.ParameterType))
                    return true ;

            return false ;
        }

        public void CopyExplicitInterfaceImpl (Type type)
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

            var mapping = m_templateClrType.GetInterfaceMap (type) ;

            for (int i  = 0 ; i < mapping.TargetMethods.Length ; ++i)
            {
                // TODO: I used to add a suppression attribute for CA1033 for these methods,
                // but that attribute is [Conditional]; I am not sure how this all works
                var copy = CopyMethod (mapping.TargetMethods[i]) ;
                if(!copy.IsPrivate)
                    throw new InvalidOperationException () ;

                Action<MethodDefinition> fixup ;
                if (fixups.TryGetValue (mapping.InterfaceMethods[i], out fixup))
                    fixup (copy) ;
            }

            m_target.Interfaces.Add (m_target.Module.Import (type)) ;
        }

        public TypeDefinition CopyNestedType (Type type, Mono.Cecil.TypeAttributes attribs)
        {
            if (!type.IsNested)
                throw new ArgumentOutOfRangeException ("type") ;

            var template = GetTemplateType (type.DeclaringType).NestedTypes.Single (_ => _.Name == type.Name) ;
            var newtype  = new TypeDefinition (null, template.Name, template.Attributes | attribs) ;

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

            m_dictionary = m_dictionary.Add (CloseTemplateType (template), newtype) ;
            return newtype ;
        }

        public void CopyField<T> (Expression<Func<T>> func)
        {
            var mex  = func.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Field)
                throw new ArgumentOutOfRangeException ("func") ;

            CopyField ((FieldInfo) mex.Member) ;
        }

        private void CopyField (FieldInfo field)
        {
            var template = GetTemplateType (field.DeclaringType).Fields.Single (_ => _.Name == field.Name) ;
            var newfield = new FieldDefinition (template.Name, template.Attributes, GetType (template.FieldType)) ;

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

            m_dictionary = m_dictionary.Add (CloseTemplateField (template), newfield) ;
        }

        #region public MethodDefinition CopyMethod (..., Mono.Cecil.MethodAttributes? attribs = null)
        public void CopyMethod<P1, P2, P3, T> (Func<P1, P2, P3, T> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, P2, T> (Func<P1, P2, T> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, T> (Func<P1, T> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<T> (Func<T> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, P2, P3> (Action<P1, P2, P3> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, P2> (Action<P1, P2> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1> (Action<P1> d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod (Action d, Mono.Cecil.MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public MethodDefinition CopyMethod_ (Expression<Action> expr, Mono.Cecil.MethodAttributes? attribs = null)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex != null)
                return CopyMethod (fex.Method, attribs, fex.Object?.Type) ;

            var nex  = expr.Body as NewExpression ;
            if (nex != null)
                return CopyMethod (nex.Constructor, attribs) ;

            throw new ArgumentOutOfRangeException ("expr") ;
        }

        public void CopyGetter_<T> (Expression<Func<T>> expr, Mono.Cecil.MethodAttributes? attribs = null)
        {
            var mex  = expr.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Property)
                throw new ArgumentOutOfRangeException ("expr") ;

            CopyMethod (((PropertyInfo) mex.Member).GetGetMethod (), attribs, mex.Expression?.Type) ;
        }

        public void CopySetter_<T> (Expression<Func<T>> expr, Mono.Cecil.MethodAttributes? attribs = null)
        {
            // limitation: property must have a getter, or expression will not compile
            var mex  = expr.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Property)
                throw new ArgumentOutOfRangeException ("expr") ;

            CopyMethod (((PropertyInfo) mex.Member).GetSetMethod (), attribs, mex.Expression?.Type) ;
        }

        private MethodDefinition CopyMethod (MethodBase method, Mono.Cecil.MethodAttributes? attribs, Type source = null)
        {
            return CopyMethod (GetMethodInType (method, source), attribs) ;
        }
        #endregion

        #region public ILProcessor CreateMethodBuilder (...)
        public ILProcessor CreateMethodBuilder<T> (Action<T> d)
        {
            return CreateMethodBuilder (d.Method, null).Item1.Body.GetILProcessor () ;
        }

        public ILProcessor CreateMethodBuilder (Action d)
        {
            return CreateMethodBuilder (d.Method, null).Item1.Body.GetILProcessor () ;
        }

        public ILProcessor CreateMethodBuilder_ (Expression<Action> expr, Mono.Cecil.MethodAttributes? attribs = null)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex != null)
                return CreateMethodBuilder (GetMethodInType (fex.Method, fex.Object?.Type), attribs).Item1.Body.GetILProcessor () ;

            var nex  = expr.Body as NewExpression ;
            if (nex != null)
                return CreateMethodBuilder (nex.Constructor, attribs).Item1.Body.GetILProcessor () ;

            throw new ArgumentOutOfRangeException ("expr") ;
        }
        #endregion

        #region public void Splice (ILProcessor il, Instruction before, ...)
        public void Splice<P1, T> (ILProcessor il, SpliceLocation location, Func<P1, T> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        public void Splice<T> (ILProcessor il, SpliceLocation location, Func<T> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        public void Splice<T> (ILProcessor il, SpliceLocation location, Action<T> d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        public void Splice (ILProcessor il, SpliceLocation location, Action d, params object[] templatedArguments)
        {
            SpliceMethod (d.Method, il, location, templatedArguments) ;
        }

        public void Splice_ (ILProcessor il, SpliceLocation location, Expression<Action> expr, params object[] templatedArguments)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex == null)
                throw new ArgumentOutOfRangeException ("expr") ;

            SpliceMethod (GetMethodInType (fex.Method, fex.Object?.Type), il, location, templatedArguments) ;
        }
        #endregion

        private MethodBase GetMethodInType (MethodBase method, Type source)
        {
            if (source == null || source == method.DeclaringType)
                return method ;

            return source.GetMethod (method.Name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static,
                null, Array.ConvertAll (method.GetParameters (), _ => _.ParameterType), null) ;
        }

        private MethodDefinition GetTemplate (MethodBase method)
        {
            var templates = GetTemplateType (method.DeclaringType).Methods.Where (_ => _.Name == method.Name).ToList () ;

            if (templates.Count == 0)
                throw new KeyNotFoundException () ;

            if (templates.Count == 1)
                return templates[0] ;

            var methodParams = method.GetParameters () ;

            foreach (var template in templates)
            {
                if (template.Parameters.Count != methodParams.Length)
                    continue ;

                var p = 0 ;
                for ( ; p < methodParams.Length ; ++p)
                    if (!template.Parameters[p].ParameterType.SameAs (methodParams[p].ParameterType))
                        break ;

                if (p < methodParams.Length)
                    continue ;

                return template ;
            }

            throw new KeyNotFoundException () ;
        }
        #endregion

        #region --[Methods: Private]--------------------------------------
        private void SpliceMethod (MethodBase source, ILProcessor il, SpliceLocation location, object[] templatedArguments)
        {
            if (il       == null) throw new ArgumentNullException (nameof (il)) ;
            if (location == null) throw new ArgumentNullException (nameof (location)) ;

            var template = GetTemplate (source) ;
            if (template.Body.InitLocals)
                il.Body.InitLocals = true ;

            var dictionary = new Dictionary<object, object> () ;
            dictionary.Add (s_dummy, null) ;

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
                    dictionary.Add (template.Body.ThisParameter, templatedArguments[0]) ;
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
                if (parameters[p].IsDefined (typeof (TemplatedMemberAttribute)))
                    break ;

                if (parameters[p].IsDefined (typeof (FromILStackAttribute)))
                    throw new InvalidOperationException () ; // must be templated

                if (il.Body.Method.Parameters.Count <= p)
                    throw new InvalidOperationException () ; // not enough parameters in target method to match splice's non-templated parameters

                if(!il.Body.Method.Parameters[p].ParameterType.SameAs (GetType (parameters[p].ParameterType)))
                    throw new InvalidOperationException () ; // splice's non-templated parameter type does not match target method parameter type

                dictionary.Add (template.Parameters[p], il.Body.Method.Parameters[p]) ;
            }

            for ( ; p < parameters.Length ; ++p, ++a)
            {
                if(!parameters[p].IsDefined (typeof (TemplatedMemberAttribute)))
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

                dictionary.Add (template.Parameters[p], value) ;
            }

            // prepare to splice parameters and local variables
            MethodBodyRocks.SimplifyMacros (template.Body) ;

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

            // prepare to splice at indicated location
            var insnSaved = new List<Instruction> () ;
            var insnMark  = insnLeft ;
            while (insns.Count != 0)
            {
                var insn  = insns[insns.Count - 1] ;
                if (insn == insnMark) break ;

                insns.RemoveAt   (insns.Count - 1) ;
                insnSaved.Add    (insn) ;
            }

            // prepare instruction to branch to instead of normal splice returns
            Instruction insnRet ;
            if (insnMark == null || insnMark.Next == null)
            {
                insnRet   = il.Create (OpCodes.Nop) ;
                insnSaved.Add         (insnRet) ;
            }
            else
                insnRet   = insnMark.Next ;

            // handle templated parameters taken directly from IL stack
            if (thisFromILStack || fromILStack != parameters.Length)
            {
                if (location.Relation == SpliceRelation.Before)
                    throw new InvalidOperationException () ; // can't use this with FromILStack

                // prepare common IL stack parameter array
                var types = new Type[parameters.Length - fromILStack + (thisFromILStack ? 1 : 0)] ;
                var pars  = new ParameterDefinition[types.Length] ;

                // expect (parameters.Length - fromILStack) values on the IL stack
                for (p = parameters.Length - 1 ; p >= fromILStack ; --p)
                {
                    types[parameters.Length - 1 - p] = parameters[p].ParameterType ;
                    pars [parameters.Length - 1 - p] = template.Parameters[p] ;
                }

                // expect this reference below the other values
                if (thisFromILStack)
                {
                    types[types.Length - 1] = source.DeclaringType.IsValueType ? source.DeclaringType.MakeByRefType () : source.DeclaringType ;
                    pars [types.Length - 1] = template.Body.ThisParameter ;
                }

                var simple = 1 ; // counts skipped nops
                for (int i = 0 ; i < types.Length ; ++i)
                {
                    // remove simple loads, skip nops
                    if (simple != 0)
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
                                dictionary.Add (pars[i], insnRight) ;
                                insns.RemoveAt (insns.Count - simple) ;
                                continue ;
                            }

                            if (insnRight.OpCode == OpCodes.Nop)
                            {
                                simple++ ;
                                goto repeat ;
                            }
                        }

                        simple = 0 ;
                    }

                    // spill remaining values into new locals
                    var local = il.CreateLocal (GetType (types[i])) ;

                    il.Emit  (OpCodes.Stloc, local) ;
                    dictionary.Add (pars[i], local) ;
                }
            }

            CopyMethodBody (template.Body, il, insnRet, dictionary) ;

            // restore instructions after splice
            for (int i = insnSaved.Count - 1 ; i >= 0 ; --i)
                il.Append (insnSaved[i]) ;

            if (location.Relation == SpliceRelation.BeforeAsPart && location.Insn != null)
                branchManager.Retarget (location.Insn, insnMark.Next) ;

            // TODO: copy variable scopes? the scopes I observed are flat,
            // so it's basically just a list of locals in the pdb
            // maybe they are meaningful if there are iterators or async methods,
            // but I can't yet handle these
        }

        private Tuple<MethodDefinition, MethodDefinition, MethodReference, Dictionary<object, object>> CreateMethodBuilder (
            MethodBase source, Mono.Cecil.MethodAttributes? attribs = null)
        {
            var from = GetTemplate (source) ;

            // TODO: take [EmitName] from non-templated version, if it exists
            var to = new MethodDefinition (GetEmitName (from), attribs ?? from.Attributes, GetType (from.ReturnType))
            {
                HasThis           = from.HasThis,
                ExplicitThis      = from.ExplicitThis,
                CallingConvention = from.CallingConvention,
            } ;

            CopyCAs (from, to) ;

            // TODO: generic _methods_

            foreach (var ovr in from.Overrides)
                to.Overrides.Add (ovr) ; // TODO: templated-generic overrides

            var dictionary = new Dictionary<object, object> () ;
            dictionary.Add (s_dummy, null) ;

            var fromTemplate    = CloseTemplateMethod (from) ;
            var seenPseudoParam = false ;
            var templatedParams = 0 ;

            var parameters = source.GetParameters () ;

            for (var p = 0 ; p < from.Parameters.Count ; ++p)
            {
                // remove templated parameters
                if (parameters[p].IsDefined (typeof (TemplatedMemberAttribute)))
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
                    var par    = from.Parameters[p] ;
                    var newpar = new ParameterDefinition (par.Name, par.Attributes, GetType (par.ParameterType)) ;

                    CopyCAs        (par, newpar) ;
                    to.Parameters.Add   (newpar) ;
                    dictionary.Add (par, newpar) ;

                    // assignment works around Cecil deficiency
                    if (newpar.HasConstant = par.HasConstant)
                        newpar.Constant    = par.Constant ;

                    continue ;
                }

                if (!seenPseudoParam)
                {
                    // create a fresh method reference
                    if (fromTemplate == from)
                        fromTemplate  = from.MakeReferenceIn (from.DeclaringType) ;

                    // remove templated parameters from the binding key
                    // and bind to the "bare" version of the method
                    for (var i = fromTemplate.Parameters.Count - 1 ; i >= p ; --i)
                        fromTemplate.Parameters.RemoveAt (i) ;

                    seenPseudoParam = true ;
                }
            }

            // TODO: throw on unbound templated parameters (but what about emit label parameter?)
            GetTarget (fromTemplate).Methods.Add (to) ;

            return Tuple.Create (to, from, fromTemplate, dictionary) ;
        }

        private MethodDefinition CopyMethod (MethodBase source, Mono.Cecil.MethodAttributes? attribs = null)
        {
            var tuple        = CreateMethodBuilder (source, attribs) ;
            var to           = tuple.Item1 ;
            var from         = tuple.Item2 ;
            var fromTemplate = tuple.Item3 ;
            var dictionary   = tuple.Item4 ;

            m_dictionary = m_dictionary.Add (fromTemplate, to) ;

            if (!from.HasBody)
                return to ;

            to.Body.MaxStackSize = from.Body.MaxStackSize ;
            to.Body.InitLocals   = from.Body.InitLocals ;

            // can't be abstract since we're adding a body
            // this relieves the user from having to track abstractness in some cases
            to.Attributes &= ~Mono.Cecil.MethodAttributes.Abstract ;

            CopyMethodBody (from.Body, to.Body.GetILProcessor (), null, dictionary) ;

            // copy variable scopes
            var deb  = from.Body.DebugInformation ;
            if (deb != null)
            {
                var newdeb = new MethodDebugInformation () ;

                if (deb.Scope   != null)
                    newdeb.Scope = CopyScope (deb.Scope, dictionary) ;

                // TODO: iterators, async methods
                to.Body.DebugInformation = newdeb ;
            }

            to.Body.OptimizeMacros () ;
            return to ;
        }

        private void CopyMethodBody (Mono.Cecil.Cil.MethodBody from, ILProcessor il, Instruction ret,
            Dictionary<object, object> dictionary)
        {
            foreach (var loc in from.Variables)
            {
                var newloc = new VariableDefinition (loc.Name, GetType (loc.VariableType)) ;
                il.Body.Variables.Add (newloc) ;
                dictionary.Add   (loc, newloc) ;
            }

            var fixups = new List<Instruction> () ;

            foreach (var insn in from.Instructions)
            {
                var newinsn           = il.Clone (insn) ;
                newinsn.SequencePoint = insn.SequencePoint ;

                if (ret != null && newinsn.OpCode == OpCodes.Ret)
                {
                    // TODO: this might not work with non-void methods,
                    // in case the compile-time types on the CLR stack
                    // are different; introduce a local in that case
                    newinsn.OpCode  = OpCodes.Br ;
                    newinsn.Operand = ret ;
                }

                il.Append            (newinsn) ;
                dictionary.Add (insn, newinsn) ;

                FieldReference  field ;
                MethodReference meth  ;
                TypeReference   type  ;
                object newop ;
                if (insn.Operand == null)
                {
                }
                else
                if (dictionary.TryGetValue (insn.Operand, out newop) ||
                  m_dictionary.TryGetValue (insn.Operand, out newop))
                {
                    var action  = newop as Action<ILProcessor, Instruction> ;
                    if (action != null)
                    {
                        action (il, newinsn) ;
                        continue ;
                    }

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
                    if ((vd = newop as VariableDefinition) != null && !(insn.Operand is VariableDefinition))
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
                    if ((meth = newop as MethodReference) != null && !(insn.Operand is MethodReference))
                    {
                        // convert bindings of storage locations to methods as delegate creation
                        switch (newinsn.OpCode.Code)
                        {
                        default:           throw new InvalidOperationException () ;
                        case Code.Ldarg:   type = ((ParameterReference) insn.Operand).ParameterType ; break ;
                        case Code.Ldsfld:  type = ((FieldReference)     insn.Operand).FieldType     ; break ;
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
                if ((type = insn.Operand as TypeReference) != null)
                {
                    newinsn.Operand = GetType (type) ;
                }
                else
                if ((meth = insn.Operand as MethodReference) != null)
                {
                    // TODO: what if a method is both?
                    if (meth.IsGenericInstance)
                    {
                        var i = (GenericInstanceMethod) meth ;

                        // process template helper methods
                        if (i.DeclaringType.SameAs (typeof (TemplateHelpers)))
                        {
                            if (i.Name == nameof (TemplateHelpers.IsNull))
                            {
                                if (GetType (i.GenericArguments[0]).IsValueType)
                                    throw new InvalidOperationException () ;

                                newinsn.OpCode  = OpCodes.Ldnull ;
                                newinsn.Operand = null ;

                                il.Emit (OpCodes.Ceq) ;
                                continue ;
                            }

                            if (i.Name == nameof (TemplateHelpers.Null))
                            {
                                if (GetType (i.GenericArguments[0]).IsValueType)
                                    throw new InvalidOperationException () ;

                                newinsn.OpCode  = OpCodes.Ldnull ;
                                newinsn.Operand = null ;
                                continue ;
                            }

                            if (i.Name == nameof (TemplateHelpers.Return))
                            {
                                newinsn.OpCode  = OpCodes.Ret ;
                                newinsn.Operand = null ;
                                continue ;
                            }
                        }

                        // TODO: do I need to funnel the result through GetType? probably yes...
                        newinsn.Operand = m_target.Module.Import (i.ElementMethod).MakeGenericInstance (
                            Array.ConvertAll (i.GenericArguments.ToArray (), GetType)) ;
                    }
                    else
                    if (meth.DeclaringType.IsGenericInstance || m_dictionary.ContainsKey (meth.DeclaringType))
                    {
                        // allow limited "forward declarations"
                        var newmeth        = meth.MakeReferenceIn (GetType (meth.DeclaringType)) ;
                        newmeth.Name       = GetEmitName (meth.Resolve ()) ;
                        newmeth.ReturnType = GetType  (newmeth.ReturnType) ;

                        foreach (var param in newmeth.Parameters)
                            param.ParameterType = GetType (param.ParameterType) ;

                        newinsn.Operand = newmeth ;
                    }
                }
                else
                if ((field = insn.Operand as FieldReference) != null)
                {
                    if (field.DeclaringType.IsGenericInstance || m_dictionary.ContainsKey (field.DeclaringType))
                    {
                        // allow limited "forward declarations"
                        newinsn.Operand = new FieldReference (GetEmitName (field.Resolve ()),
                            GetType (field.FieldType),
                            GetType (field.DeclaringType)) ;
                    }
                }
                else
                if (insn.Operand is Instruction || insn.Operand is Instruction[])
                    fixups.Add (newinsn) ;
            }

            // fix up forward branches and switches
            foreach (var newinsn in fixups)
            {
                if (newinsn.Operand is Instruction)
                {
                    object newop ;
                    if (dictionary.TryGetValue (newinsn.Operand, out newop))
                        newinsn.Operand = newop ;

                    continue ;
                }

                var insns  = newinsn.Operand as Instruction[] ;
                if (insns == null)
                    continue ;

                var clone  = new Instruction[insns.Length] ;
                for (int i = 0 ; i < insns.Length ; ++i)
                    clone[i] = (Instruction) dictionary[insns[i]] ;

                newinsn.Operand = clone ;
            }

            if (from.HasExceptionHandlers)
            foreach (var eh in from.ExceptionHandlers)
            {
                il.Body.ExceptionHandlers.Add (new ExceptionHandler (eh.HandlerType)
                {
                    CatchType    = GetType (eh.CatchType),
                    TryStart     = (Instruction) dictionary[eh.TryStart     ?? s_dummy],
                    TryEnd       = (Instruction) dictionary[eh.TryEnd       ?? s_dummy],
                    FilterStart  = (Instruction) dictionary[eh.FilterStart  ?? s_dummy],
                    HandlerStart = (Instruction) dictionary[eh.HandlerStart ?? s_dummy],
                    HandlerEnd   = (Instruction) dictionary[eh.HandlerEnd   ?? s_dummy],
                }) ;
            }
        }

        private Scope CopyScope (Scope scope, Dictionary<object, object> dictionary)
        {
            var newscope   = new Scope  () ;
            newscope.Start = (Instruction) dictionary[scope.Start ?? s_dummy] ;
            newscope.End   = (Instruction) dictionary[scope.End   ?? s_dummy] ;

            if (scope.HasVariables)
                foreach (var v in scope.Variables)
                    newscope.Variables.Add ((VariableDefinition) dictionary[v]) ;

            if (scope.HasScopes)
                foreach (var s in scope.Scopes)
                    newscope.Scopes.Add (CopyScope (s, dictionary)) ;

            return newscope ;
        }

        private MethodReference MakeDelegateCtorReference (TypeReference type)
        {
            var ctor     = new MethodReference (".ctor", m_tVoid, GetType (type)) ;
            ctor.HasThis = true ;

            ctor.Parameters.Add (new ParameterDefinition (m_tObject)) ;
            ctor.Parameters.Add (new ParameterDefinition (m_tIntPtr)) ;
            return ctor ;
        }

        private T VerifyTemplatedMember<T> (T member) where T : Mono.Cecil.ICustomAttributeProvider
        {
            if (!member.CustomAttributes.Any (_ => _.AttributeType.SameAs (typeof (TemplatedMemberAttribute))))
                throw new InvalidOperationException () ;

            return member ;
        }

        private TypeDefinition GetTemplateType (Type type)
        {
            if ((type.IsGenericType ? type.GetGenericTypeDefinition () : type).FullName == m_template.FullName)
            {
                return m_template ;
            }
            else
            if (type.IsNested)
            {
                return GetTemplateType (type.DeclaringType).NestedTypes.Single (_ => _.Name == type.Name) ;
            }
            else
                return m_template.Module.Types.Single (_ => _.FullName == type.FullName) ;
        }

        private TypeReference CloseTemplateType (TypeDefinition type)
        {
            if (m_templateSelfArguments == null)
                return type ;

            if (m_template != type.DeclaringType)
                return type ;

            return type.MakeGenericInstanceType (m_templateSelfArguments) ;
        }

        private FieldReference CloseTemplateField (FieldDefinition field)
        {
            if (m_templateSelfArguments == null)
                return field ;

            if (m_template == field.DeclaringType)
                return new FieldReference (field.Name, field.FieldType, m_templateClosedOverSelf) ;

            return field.CloseDeclaringType (m_templateSelfArguments) ;
        }

        private MethodReference CloseTemplateMethod (MethodDefinition method)
        {
            if (m_templateSelfArguments == null)
                return method ;

            if (m_template == method.DeclaringType)
                return method.MakeReferenceIn (m_templateClosedOverSelf) ;

            return method.CloseDeclaringType (m_templateSelfArguments) ;
        }

        private TypeDefinition GetTarget (MemberReference fromTemplate)
        {
            if (fromTemplate.DeclaringType.GetElementType () == m_template)
            {
                return m_target ;
            }
            else
                return (TypeDefinition) m_dictionary[fromTemplate.DeclaringType] ;
        }

        private TypeReference GetType (Type type)
        {
            return GetType (m_template.Module.Import (type)) ;
        }

        private TypeReference GetType (TypeReference type)
        {
            object value ;
            if (m_dictionary.TryGetValue (type, out value))
                return (TypeReference) value ;

            if (type.DeclaringType != null && m_dictionary.TryGetValue (type.DeclaringType, out value))
            {
                // allow limited "forward declarations"
                var emitName = GetEmitName (type.Resolve ()) ;
                var emitType = ((TypeDefinition) value).NestedTypes.Single (_ => _.Name == emitName) ;

                m_dictionary = m_dictionary.Add (type, emitType) ;
                return emitType ;
            }

            if (!type.IsGenericInstance)
                return type ;

            // NB: Import() here prevents Cecil auto-import from becoming confused
            // and creating TypeRefs for TypeDefs that already exist in the module
            var git    = (GenericInstanceType) type ;
            var newgit = new GenericInstanceType (m_target.Module.Import (GetType (git.ElementType))) ;

            foreach (var ga in git.GenericArguments)
                newgit.GenericArguments.Add (GetType (ga)) ;

            m_dictionary = m_dictionary.Add (type, newgit) ;
            return newgit ;
        }

        private string GetEmitName (IMemberDefinition member)
        {
            foreach (var ca in member.CustomAttributes)
                if (ca.AttributeType.SameAs (typeof (EmitNameAttribute)))
                    return String.Format ((string) ca.ConstructorArguments[0].Value,
                        m_dictionary.GetValueOrDefault (s_scope)) ;

            // XXX: do this for methods and/or move into CLR-based code
            var type  = member as TypeDefinition ;
            if (type != null && type.BaseType != null)
                return GetEmitName (type.BaseType.Resolve ()) ;

            return member.Name ;
        }

        private void CopyCAs (Mono.Cecil.ICustomAttributeProvider from, Mono.Cecil.ICustomAttributeProvider to)
        {
            foreach (var ca in from.CustomAttributes)
                if (ca.AttributeType.Namespace != typeof (TemplateHelpers).Namespace)
                    to.CustomAttributes.Add (ca) ; // TODO: templated items in custom attributes?
        }
        #endregion
    }
}
