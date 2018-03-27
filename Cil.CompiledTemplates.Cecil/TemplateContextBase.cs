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
using System.Linq.Expressions ;
using System.Collections.Generic   ;
using System.Collections.Immutable ;
using System.Reflection  ;

using SDS = System.Diagnostics.SymbolStore ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    /// <summary>
    /// The abstract base class for template contexts.
    /// Contains target-agnostic declarations and functionality.
    /// </summary>
    public abstract class TemplateContextBase
    {
        #region --[Fields: Private]---------------------------------------
        protected readonly Type m_template ;

        // dictionary keys are System.Reflection.MemberInfos and special objects
        // values are target-scoped
        protected ImmutableDictionary<object, object> m_dictionary ;

        protected readonly static object s_label = new object () ;
        protected readonly static object s_scope = new object () ;
        protected readonly static object s_this  = new object () ;

        protected readonly HashSet<MemberInfo> m_vars = new HashSet<MemberInfo> () ;

        private sealed class DictionaryState : IDisposable
        {
            private readonly TemplateContextBase                 m_context ;
            private readonly ImmutableDictionary<object, object> m_state ;

            public DictionaryState (TemplateContextBase context)
            {
                m_context = context ;
                m_state   = context.m_dictionary ;
            }

            public void Dispose ()
            {
                m_context.m_dictionary = m_state ;
            }
        }

        // since I'm working with Assemblies, and the lifetime of Assembly objects
        // is coterminous with the lifetime of the AppDomain they are loaded into,
        // there seems to be no harm in having symbol reader instances live as long
        private static ImmutableDictionary<Assembly, SDS.ISymbolReader> s_symbolReaders =
                       ImmutableDictionary<Assembly, SDS.ISymbolReader>.Empty ;
        #endregion

        #region --[Constructors]------------------------------------------
        protected TemplateContextBase (Type template)
        {
            m_dictionary = ImmutableDictionary.Create<object, object> () ;
            m_template   = template ;
        }
        #endregion

        #region --[Methods: context manipulation]-------------------------
        /// <summary>
        /// Pushes the context state, setting a new scope name,
        /// and returns an object that restores the previous state when disposed.
        /// </summary>
        /// <param name="scope">
        /// The new scope name. Used as the first parameter in formatting [EmitName] strings.
        /// </param>
        public IDisposable Push (string scope)
        {
            var disposable = new DictionaryState  (this) ;
            m_dictionary   = m_dictionary.SetItem (s_scope, scope) ;
            return disposable ;
        }

        /// <summary>
        /// Gets the current binding for <paramref name="template"/>.
        /// </summary>
        public object Get (Type template)
        {
            return m_dictionary[template] ;
        }

        /// <summary>
        /// Gets the current binding for the templated field
        /// identified by the lambda expression <paramref name="func"/>.
        /// </summary>
        public object GetField<T> (Expression<Func<T>> func)
        {
            return m_dictionary[GetTemplatedField (func)] ;
        }

        /// <summary>
        /// Binds the templated field identified by the lambda expression
        /// <paramref name="func"/> to the compile-time constant <paramref name="value"/>.
        /// </summary>
        public abstract void BindField<T> (Expression<Func<T>> func, T value) ;

        /// <summary>
        /// Gets the current binding for the templated method
        /// identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        public object Get_ (Expression<Action> expr)
        {
            // TODO: GetXxx for non-templated members?
            return m_dictionary[GetTemplatedMethod (expr)] ;
        }

        protected Type GetTemplatedType (Type template)
        {
            return VerifyTemplatedMember (template) ;
        }

        protected FieldInfo GetTemplatedField (LambdaExpression func, bool? enforceIsStatic = null)
        {
            var mex  = func.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Field)
                throw new ArgumentOutOfRangeException (nameof (func)) ;

            if (enforceIsStatic.HasValue && enforceIsStatic != ((FieldInfo) mex.Member).IsStatic)
                throw new ArgumentOutOfRangeException (nameof (func)) ;

            return (FieldInfo) VerifyTemplatedMember (mex.Member) ;
        }

        protected MethodBase GetTemplatedMethod (LambdaExpression expr)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex != null)
                return GetTemplatedMethod (fex.Method, fex.Object?.Type) ;

            var nex  = expr.Body as NewExpression ;
            if (nex != null)
                return GetTemplatedMethod (nex.Constructor, null) ;

            throw new ArgumentOutOfRangeException (nameof (expr)) ;
        }

        protected MethodBase GetTemplatedMethod (MethodBase method, Type source)
        {
            if (source == method.DeclaringType)
                source  = null ;

            // support templated methods in quasi-generic nested types
            if (method.DeclaringType.IsGenericInstance ())
                method = method.Module.ResolveMethod   (method.MetadataToken) ;

            return VerifyTemplatedMember (GetMethodInType (method, source)) ;
        }

        protected void Add (object key, object value)
        {
            m_dictionary = m_dictionary.Add (key, value) ;
        }

        protected void SetIdempotent (object key, object value)
        {
            object existing ;
            if(!m_dictionary.TryGetValue (key, out existing))
            {
                m_dictionary = m_dictionary.Add (key, value) ;
            }
            else
            if(!m_dictionary.ValueComparer.Equals (value, existing))
                throw new InvalidOperationException () ; // would change bound value
        }
        #endregion

        #region --[Methods: Template application - Labels]----------------
        /// <summary>
        /// Copies all template members matching <paramref name="label"/>.
        /// </summary>
        public void CopyLabel (Type label)
        {
            CopyLabels (label) ;
        }

        /// <summary>
        /// Copies all template members matching any of the <paramref name="_labels"/>.
        /// </summary>
        public void CopyLabels (params Type[] _labels)
        {
            var labels = new HashSet<Type> () ;

            foreach (var label in _labels)
                GatherLabels (labels, label) ;

            /**/m_dictionary = m_dictionary.Add (s_label, labels) ;
            try
            {
                CopyLabels (m_template, labels, new HashSet<Type> ()) ;
            }
            finally
            {
                m_dictionary = m_dictionary.Remove (s_label) ;
            }
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

        private void CopyLabels (Type template, HashSet<Type> labels, HashSet<Type> templates)
        {
            if (!templates.Add (template))
                return ;

            if (template.BaseType.Assembly != typeof (object).Assembly)
                CopyLabels (template.BaseType, labels, templates) ;

            var all = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly ;

            foreach (var type in template.GetNestedTypes (all))
            {
                // check the dictionary because the type might have already
                // been copied via GetType
                if(!m_dictionary.ContainsKey (type))
                    MatchCopyNested (type, labels) ;

                /**/CopyLabels   (type, labels, templates) ;
            }

            foreach (var ctor in template.GetConstructors (all))
                MatchCopyMethod  (ctor, labels) ;

            foreach (var meth in template.GetMethods (all | BindingFlags.Static))
                MatchCopyMethod  (meth, labels) ;

            foreach (var field in template.GetFields (all | BindingFlags.Static))
                if (MatchesLabel (field, labels))
                    CopyField    (field) ;

            foreach (var ca in template.GetCustomAttributes<EmitInterfaceImplAttribute> ())
                if (MatchesLabel (ca, labels))
                    CopyInterfaceImpl (template, ca.Interface) ;
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
            if (ca.Labels == null || ca.Labels.Length == 0)
                throw new InvalidOperationException () ;

            foreach (var la in ca.Labels)
                if (!labels.Contains (la))
                    return false ;

            return true ;
        }

        private bool MatchCopyMethod (MethodBase method, HashSet<Type> labels)
        {
            var copy  = false ;
            var set   = default (MethodAttributes) ;
            var clear = default (MethodAttributes) ;

            foreach (var pa in method.GetParameters ())
                if (pa.IsDefined (typeof (EmitLabelAttribute)) && labels.Contains (pa.ParameterType))
                {
                    copy = true ;
                    break ;
                }

            foreach (var ca in method.GetCustomAttributes<EmitLabelAttribute> ())
                if (MatchesLabel (ca, labels))
                {
                    copy = true ;

                    if (ca.Virtual)
                    {
                        set   |= MethodAttributes.Virtual | MethodAttributes.NewSlot ;
                        clear |= MethodAttributes.ReuseSlot ;
                    }
                    else
                    if (ca.Override)
                    {
                        set   |= MethodAttributes.Virtual | MethodAttributes.ReuseSlot ;
                        clear |= MethodAttributes.NewSlot ;
                    }
                    else
                    if (ca.NonVirtual)
                    {
                        clear |= MethodAttributes.Virtual | MethodAttributes.Abstract  |
                                 MethodAttributes.NewSlot | MethodAttributes.ReuseSlot | MethodAttributes.Final ;
                    }

                    if (ca.Final)
                    {
                        set   |= MethodAttributes.Final    ;
                        clear |= MethodAttributes.Abstract ;
                    }
                    else
                    if (ca.Abstract)
                    {
                        set   |= MethodAttributes.Abstract ;
                        clear |= MethodAttributes.Final    ;
                    }
                }

            if ((set & clear) != 0)
                throw new InvalidOperationException () ; // inconsistent method attributes

            if ((set & MethodAttributes.Virtual) != 0 && method.IsStatic)
                throw new InvalidOperationException () ; // virtual attributes on static method

            if (copy)
            {
                CopyMethod (method, set, clear) ;
                return true ;
            }
            else
                return false ;
        }

        private bool MatchCopyNested (Type type, HashSet<Type> labels)
        {
            var copy  = false ;
            var set   = default (TypeAttributes) ;
            var clear = default (TypeAttributes) ;

            foreach (var ca in type.GetCustomAttributes<EmitLabelAttribute> ())
                if (MatchesLabel (ca, labels))
                {
                    copy = true ;

                    if (ca.Sealed)
                    {
                        set   |= TypeAttributes.Sealed   ;
                        clear |= TypeAttributes.Abstract ;
                    }
                    else
                    if (ca.Abstract)
                    {
                        set   |= TypeAttributes.Abstract ;
                        clear |= TypeAttributes.Sealed   ;
                    }
                }

            if ((set & clear) != 0)
                throw new InvalidOperationException () ; // inconsistent type attributes

            if (copy)
            {
                CopyNested (type, set, clear) ;
                return true ;
            }
            else
                return false ;
        }
        #endregion

        #region --[Methods: Template application - Manual]----------------
        /// <summary>
        /// Copies the nested template type <paramref name="type"/>.
        /// </summary>
        public abstract void CopyNested (Type type, TypeAttributes set = 0, TypeAttributes clear = 0) ;

        /// <summary>
        /// Copies all members comprising the implementation
        /// of the specified interface in <paramref name="template"/>.
        /// </summary>
        public abstract void CopyInterfaceImpl (Type template, Type iface) ;

        /// <summary>
        /// Copies the template field
        /// identified by the lambda expression <paramref name="func"/>.
        /// </summary>
        public void CopyField<T> (Expression<Func<T>> func)
        {
            var mex  = func.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Field)
                throw new ArgumentOutOfRangeException (nameof (func)) ;

            CopyField ((FieldInfo) mex.Member) ;
        }

        protected abstract void CopyField (FieldInfo field) ;

        #region public void CopyMethod (..., MethodAttributes set = 0, MethodAttributes clear = 0)
        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<P1, P2, P3, T> (Func<P1, P2, P3, T> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<P1, P2, T> (Func<P1, P2, T> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<P1, T> (Func<P1, T> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<T> (Func<T> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<P1, P2, P3> (Action<P1, P2, P3> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<P1, P2> (Action<P1, P2> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod<P1> (Action<P1> d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }

        /// <summary>
        /// Copies the template method that is the target of the supplied delegate.
        /// </summary>
        public void CopyMethod (Action d, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            CopyMethod (d.Method, set, clear) ;
        }
        #endregion

        /// <summary>
        /// Copies the template method
        /// identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        public void CopyMethod_ (Expression<Action> expr, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex != null)
            {
                CopyMethod (GetMethodInType (fex.Method, fex.Object?.Type), set, clear) ;
                return ;
            }

            var nex  = expr.Body as NewExpression ;
            if (nex != null)
            {
                CopyMethod (nex.Constructor, set, clear) ;
                return ;
            }

            throw new ArgumentOutOfRangeException (nameof (expr)) ;
        }

        /// <summary>
        /// Copies the template property getter
        /// identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        public void CopyGetter_<T> (Expression<Func<T>> expr, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            var mex  = expr.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Property)
                throw new ArgumentOutOfRangeException (nameof (expr)) ;

            CopyMethod (GetMethodInType (((PropertyInfo) mex.Member).GetGetMethod (), mex.Expression?.Type), set, clear) ;
        }

        /// <summary>
        /// Copies the template property setter
        /// identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        public void CopySetter_<T> (Expression<Func<T>> expr, MethodAttributes set = 0, MethodAttributes clear = 0)
        {
            // limitation: property must have a getter, or expression will not compile
            var mex  = expr.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Property)
                throw new ArgumentOutOfRangeException (nameof (expr)) ;

            CopyMethod (GetMethodInType (((PropertyInfo) mex.Member).GetSetMethod (), mex.Expression?.Type), set, clear) ;
        }

        protected abstract void CopyMethod (MethodBase method, MethodAttributes set, MethodAttributes clear) ;
        #endregion

        #region --[Methods: Protected]------------------------------------
        protected MethodBase GetMethodInType (MethodBase method, Type source, int? retainParameters = null)
        {
            if ((source == null || source == method.DeclaringType) && retainParameters == null)
                return method ;

            if (source == null)
                source  = method.DeclaringType ;

            var parameters     = method.GetParameters () ;
            var parameterTypes = new Type[retainParameters ?? parameters.Length] ;
            for (int i = 0 ; i < parameterTypes.Length ; ++i)
                parameterTypes[i] = parameters[i].ParameterType ;

            if (method is ConstructorInfo)
            {
                return source.GetConstructor (BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static,
                    null, parameterTypes, null) ;
            }
            else
                return source.GetMethod (method.Name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static,
                    null, parameterTypes, null) ;
        }

        protected T VerifyTemplatedMember<T> (T member) where T : MemberInfo
        {
            if (!member.IsDefined (typeof (TemplatedMemberAttribute), false))
                throw new InvalidOperationException () ;

            m_vars.Add (member) ;
            return member ;
        }

        protected object GetType (Type type, object genericContext, bool asOpen)
        {
            object value ;
            if (m_dictionary.TryGetValue (type, out value))
                return value ;

            if (type.IsDefined (typeof (EmitLabelAttribute)))
            {
                object labels ;
                if (m_dictionary.TryGetValue (s_label, out labels))
                    if (MatchCopyNested (type, (HashSet<Type>)labels))
                        return GetType  (type, genericContext, asOpen) ;

                // fall through: may be bound with [BindLabel]
            }

            if (type.IsDefined (typeof (TemplatedMemberAttribute), false))
            {
                object labels ;
                if (m_dictionary.TryGetValue (s_label, out labels))
                    foreach (var bl in type.GetCustomAttributes<BindLabelAttribute> (false))
                        if (MatchesLabel (bl, (HashSet<Type>)labels))
                            return GetType (bl.Type, genericContext, asOpen) ;

                if(!type.IsDefined (typeof (EmitNameAttribute)))
                    throw new InvalidOperationException () ; // templated type not bound

                // fall through: forward reference
            }

            value        = GetTypeInternal  (type, genericContext, asOpen) ;
            m_dictionary = m_dictionary.Add (type, value) ;
            return value ;
        }

        protected abstract object GetTypeInternal (Type type, object genericContext, bool asOpen) ;

        protected string GetEmitName (MemberInfo member)
        {
            var ca  = member.GetCustomAttribute<EmitNameAttribute> () ;
            if (ca != null)
                return String.Format (ca.Name, m_dictionary.GetValueOrDefault (s_scope)) ;

            return member.Name ;
        }

        [System.Diagnostics.DebuggerNonUserCode]
        protected SDS.ISymbolMethod GetMethodSymbols (MethodBase method)
        {
            try
            {
                return ImmutableInterlocked.GetOrAdd (ref s_symbolReaders, method.Module.Assembly,
                    _ => SymbolHelpers.GetSymbolReader (_.Location)).GetMethod (new SDS.SymbolToken (method.MetadataToken)) ;
            }
            catch
            {
                return null ;
            }
        }
        #endregion
    }
}
