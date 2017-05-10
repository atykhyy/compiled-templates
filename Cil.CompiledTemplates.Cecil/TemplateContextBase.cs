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

using SDS = System.Diagnostics.SymbolStore ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    public abstract class TemplateContextBase
    {
        #region --[Fields: Private]---------------------------------------
        protected readonly Type              m_template ;
        protected readonly SDS.ISymbolReader m_symbols  ;

        // dictionary keys are System.Reflection.MemberInfos and special objects
        // values are target-scoped
        protected ImmutableDictionary<object, object> m_dictionary ;

        protected readonly static object s_scope = new object () ;
        protected readonly static object s_this  = new object () ;

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
        private readonly static System.Runtime.CompilerServices.ConditionalWeakTable<Assembly, SDS.ISymbolReader> s_symbolReaders =
                            new System.Runtime.CompilerServices.ConditionalWeakTable<Assembly, SDS.ISymbolReader> () ;
        #endregion

        #region --[Constructors]------------------------------------------
        protected TemplateContextBase (Type template)
        {
            m_dictionary = ImmutableDictionary.Create<object, object> () ;
            m_template   = template ;
            m_symbols    = s_symbolReaders.GetValue (template.Assembly,
                _ => SymbolHelpers.GetSymbolReader (_.Location)) ;
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

        #region --[Methods: template application]-------------------------
        public void CopyLabel (Type label)
        {
            var labels = new HashSet<Type> () ;
            GatherLabels (labels, label) ;
            CopyLabels   (m_template, labels) ;
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

        public abstract void CopyExplicitInterfaceImpl (Type type) ;

        public void CopyField<T> (Expression<Func<T>> func)
        {
            var mex  = func.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Field)
                throw new ArgumentOutOfRangeException (nameof (func)) ;

            CopyField ((FieldInfo) mex.Member) ;
        }

        protected abstract void CopyField (FieldInfo field) ;

        #region public void CopyMethod (..., MethodAttributes? attribs = null)
        public void CopyMethod<P1, P2, P3, T> (Func<P1, P2, P3, T> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, P2, T> (Func<P1, P2, T> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, T> (Func<P1, T> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<T> (Func<T> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, P2, P3> (Action<P1, P2, P3> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1, P2> (Action<P1, P2> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod<P1> (Action<P1> d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }

        public void CopyMethod (Action d, MethodAttributes? attribs = null)
        {
            CopyMethod (d.Method, attribs) ;
        }
        #endregion

        public void CopyMethod_ (Expression<Action> expr, MethodAttributes? attribs = null)
        {
            var fex  = expr.Body as MethodCallExpression ;
            if (fex != null)
            {
                CopyMethod (GetMethodInType (fex.Method, fex.Object?.Type), attribs) ;
                return ;
            }

            var nex  = expr.Body as NewExpression ;
            if (nex != null)
            {
                CopyMethod (nex.Constructor, attribs) ;
                return ;
            }

            throw new ArgumentOutOfRangeException (nameof (expr)) ;
        }

        public void CopyGetter_<T> (Expression<Func<T>> expr, MethodAttributes? attribs = null)
        {
            var mex  = expr.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Property)
                throw new ArgumentOutOfRangeException (nameof (expr)) ;

            CopyMethod (GetMethodInType (((PropertyInfo) mex.Member).GetGetMethod (), mex.Expression?.Type), attribs) ;
        }

        public void CopySetter_<T> (Expression<Func<T>> expr, MethodAttributes? attribs = null)
        {
            // limitation: property must have a getter, or expression will not compile
            var mex  = expr.Body as MemberExpression ;
            if (mex == null || mex.Member.MemberType != MemberTypes.Property)
                throw new ArgumentOutOfRangeException (nameof (expr)) ;

            CopyMethod (GetMethodInType (((PropertyInfo) mex.Member).GetSetMethod (), mex.Expression?.Type), attribs) ;
        }

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

        protected abstract void CopyMethod (MethodBase method, MethodAttributes? attribs = null) ;
        #endregion

        #region --[Methods: Private]--------------------------------------
        protected T VerifyTemplatedMember<T> (T member) where T : ICustomAttributeProvider
        {
            if (!member.IsDefined (typeof (TemplatedMemberAttribute), false))
                throw new InvalidOperationException () ;

            return member ;
        }

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
                return m_symbols.GetMethod (new SDS.SymbolToken (method.MetadataToken)) ;
            }
            catch
            {
                return null ;
            }
        }
        #endregion
    }
}
