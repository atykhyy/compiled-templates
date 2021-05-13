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
using System           ;
using System.Text      ;
using System.Linq      ;
using System.Linq.Expressions ;
using System.Collections.Generic ;
using System.Reflection ;
using Mono.Cecil       ;
using Mono.Cecil.Cil   ;
using Mono.Cecil.Rocks ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    /// <summary>
    /// Provides miscellaneous extensions.
    /// </summary>
    static class MiscExtensions
    {
        #region --[Methods: Extensions]-----------------------------------
        /// <summary>
        /// Gets the value corresponding to the key if one exists;
        /// otherwise creates default value for the type,
        /// adds it to the dictionary and returns it.
        /// </summary>
        public static T GetOrAdd<T, K> (
            this IDictionary<K, T> dictionary, K key) where T : new ()
        {
            T result ;
            if (!dictionary.TryGetValue (key, out result))
            {
                result = new T () ;
                dictionary.Add (key, result) ;
            }
            return result ;
        }

        public static bool IsGenericInstance (this Type type)
        {
            return type.IsGenericType && !type.IsGenericTypeDefinition ;
        }

        public static Type GetReturnType (this MethodBase member)
        {
            var method  = member as MethodInfo ;
            if (method != null)
                return method.ReturnType ;

            // for constructors
            return typeof (void) ;
        }

        /// <summary>
        /// Gets the type's full name in pseudo-C# syntax.
        /// </summary>
        public static string GetPseudoSourceFullName (this Type type)
        {
            if (!type.IsGenericType)
                return type.FullName ;

            var builder = new StringBuilder () ;
            BuildPseudoSourceFullName (builder, type) ;
            return builder.ToString   () ;
        }

        private static void BuildPseudoSourceFullName (StringBuilder builder, Type type)
        {
            if (!type.IsGenericType)
            {
                builder.Append (type.FullName) ;
                return ;
            }

            var g  = type.GetGenericTypeDefinition ().FullName ;
            var i  = g.LastIndexOf ('`') ;

            if (i >= 0) builder.Append (g, 0, i) ;
            else        builder.Append (g) ;

            var sep = '<' ;

            foreach (var t in type.GetGenericArguments ())
            {
                builder.Append (sep) ;
                BuildPseudoSourceFullName (builder, t) ;

                sep = ',' ;
            }

            builder.Append ('>') ;
        }
        #endregion
    }

    /// <summary>
    /// Instructs the Import extensions to import an open generic type
    /// or method when a type marked with this attribute is used
    /// as a generic parameter.
    /// </summary>
    public sealed class GenericizeAttribute : Attribute
    {
    }

    /// <summary>
    /// Provides Cecil-related extension methods.
    /// </summary>
    public static class CecilExtensions
    {
        #region --[Methods: Extensions]-----------------------------------
        /// <summary>
        /// Creates a .NET <see cref="AssemblyName"/> from the assembly name reference.
        /// </summary>
        public static AssemblyName ToAssemblyName (this AssemblyNameReference name)
        {
            var result = new AssemblyName
            {
                Name          = name.Name,
                Version       = name.Version,
                CultureInfo   = System.Globalization.CultureInfo.GetCultureInfo (name.Culture),
                HashAlgorithm = (System.Configuration.Assemblies.AssemblyHashAlgorithm) name.HashAlgorithm,
            } ;

            if (name.HasPublicKey)
                result.SetPublicKeyToken (name.PublicKeyToken) ;

            return result ;
        }

        /// <summary>
        /// Imports the field identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        /// <remarks>
        /// The lambda expression must have the form (...) => ...m_field.
        /// </remarks>
        public static FieldReference ImportField<T> (this ModuleDefinition module, Expression<Func<T>> expr)
        {
            var field = (FieldInfo)((MemberExpression)expr.Body).Member ;

            var type  = field.DeclaringType ;
            if (type.IsGenericType && ShouldGenericize (type.GetGenericArguments ()))
            {
                // NB: the GetFromRuntimeHandle trick does not work for fields, but fields
                // don't have overloads or other complications and a simple name search is OK
                field = type.GetGenericTypeDefinition  ().GetField (field.Name, BindingFlags.Public | BindingFlags.NonPublic |
                                      (field.IsStatic ? BindingFlags.Static : BindingFlags.Instance | BindingFlags.DeclaredOnly)) ;
            }

            return module.ImportReference (field) ;
        }

        /// <summary>
        /// Imports the constructor for the supplied delegate type.
        /// </summary>
        public static MethodReference ImportDelegateCtor (this ModuleDefinition module, Type delegateType)
        {
            if (!typeof (Delegate).IsAssignableFrom (delegateType))
                throw new ArgumentException () ;

            return module.ImportReference (delegateType.GetConstructors ().Single ()) ;
        }

        /// <summary>
        /// Imports the method identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        /// <remarks>
        /// The lambda expression must have the form (...) => ....Method (...).
        /// </remarks>
        public static MethodReference Import<T> (this ModuleDefinition module, Expression<Func<T>> expr)
        {
            return ImportMethodGenericize (module, ((NewExpression)expr.Body).Constructor) ;
        }

        /// <summary>
        /// Imports the method identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        /// <remarks>
        /// The lambda expression must have the form (...) => ....Method (...).
        /// </remarks>
        public static MethodReference Import<T> (this ModuleDefinition module, Expression<Action<T>> expr)
        {
            return ImportMethodGenericize (module, expr.Body) ;
        }

        /// <summary>
        /// Imports the method identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        /// <remarks>
        /// The lambda expression must have the form (...) => ....Method (...).
        /// </remarks>
        public static MethodReference Import<U, V> (this ModuleDefinition module, Expression<Action<U, V>> expr)
        {
            return ImportMethodGenericize (module, expr.Body) ;
        }

        /// <summary>
        /// Imports the method identified by the lambda expression <paramref name="expr"/>.
        /// </summary>
        /// <remarks>
        /// The lambda expression must have the form (...) => ....Method (...).
        /// </remarks>
        public static MethodReference Import<T, U> (this ModuleDefinition module, Expression<Func<T, U>> expr)
        {
            return ImportMethodGenericize (module, expr.Body) ;
        }

        private static MethodReference ImportMethodGenericize (ModuleDefinition module, Expression expr)
        {
            if (expr is MemberExpression)
                return ImportMethodGenericize (module, ((PropertyInfo)((MemberExpression)expr).Member).GetGetMethod ()) ;
            else
                return ImportMethodGenericize (module, ((MethodCallExpression)expr).Method) ;
        }

        private static MethodReference ImportMethodGenericize (ModuleDefinition module, MethodBase method)
        {
            if (method.IsGenericMethod && ShouldGenericize (method.GetGenericArguments ()))
                method = ((MethodInfo) method).GetGenericMethodDefinition () ;

            var type   = method.DeclaringType ;
            if (type.IsGenericType && ShouldGenericize (type.GetGenericArguments ()))
                method = MethodBase.GetMethodFromHandle (method.MethodHandle, type.GetGenericTypeDefinition ().TypeHandle) ;

            return module.ImportReference (method) ;
        }

        private static bool ShouldGenericize (params Type[] types)
        {
            foreach (var type in types)
                if (type.IsDefined (typeof (GenericizeAttribute), false))
                    return true ;

            return false ;
        }

        /// <summary>
        /// Compares two type references for equality.
        /// </summary>
        /// <remarks>
        /// Returns incorrect results in some corner cases like [TypeForwardedTo].
        /// </remarks>
        public static bool SameAs (this TypeReference a, TypeReference b)
        {
            return TypeReferenceEqualityComparer.Instance.Equals (a, b) ;
        }

        /// <summary>
        /// Compares a type reference and a type for equality.
        /// </summary>
        /// <remarks>
        /// Returns incorrect results in some corner cases like [TypeForwardedTo].
        /// </remarks>
        public static bool SameAs (this TypeReference a, Type b)
        {
            return a == null ? b == null : a.Module.ImportReference (b).SameAs (a) ;
        }

        /// <summary>
        /// Compares two metadata scopes for equality.
        /// </summary>
        public static bool SameAs (this IMetadataScope a, IMetadataScope b)
        {
            return a.GetFullName () == b.GetFullName () ;
        }

        /// <summary>
        /// Indicates whether <paramref name="method"/> returns <c>void</c>.
        /// </summary>
        public static bool IsVoidReturn (this MethodReference method)
        {
            return method.ReturnType.SameAs (method.Module.TypeSystem.Void) ;
        }

        /// <summary>
        /// Returns the full assembly name corresponding to <paramref name="scope"/>.
        /// </summary>
        public static string GetFullName (this IMetadataScope scope)
        {
            var md  = scope as ModuleDefinition ;
            if (md != null)
                return md.Assembly.Name.FullName ;

            var nr  = scope as AssemblyNameReference ;
            if (nr != null)
                return nr.FullName ;

            throw new NotSupportedException (String.Format ("Unknown metadata scope type {0}", 
                scope.GetType ().FullName)) ;
        }

        /// <summary>
        /// Checks whether <paramref name="type"/> is a generic instance of <paramref name="generic"/>.
        /// </summary>
        public static bool IsGenericInstanceOf (this TypeReference type, TypeReference generic)
        {
            var typeg  = type as GenericInstanceType ;
            if (typeg == null)
                return false ;
            
            return SameAs (typeg.ElementType, generic) ;
        }

        /// <summary>
        /// Gets the <paramref name="i"/>-th generic argument of <paramref name="type"/>.
        /// </summary>
        public static TypeReference GetGenericArgument (this TypeReference type, int i)
        {
            return ((GenericInstanceType) type).GenericArguments[i] ;
        }

        /// <summary>
        /// Recursively substitutes the specified type arguments 
        /// for the corresponding type parameters in <paramref name="type"/>.
        /// </summary>
        public static TypeReference SubstituteGenericParameters (this TypeReference type, 
            IList<GenericParameter> parameters,
            IList<TypeReference>    arguments)
        {
            var typeg  = type as GenericInstanceType ;
            if (typeg != null)
            {
                var result = new GenericInstanceType (typeg.ElementType) ;
            
                foreach (var arg in typeg.GenericArguments)
                    result.GenericArguments.Add (arg.SubstituteGenericParameters (parameters, arguments)) ;

                return result ;
            }

            var element  = type.GetElementType () ;
            if (element == type)
            {
                for (int i = 0 ; i < parameters.Count ; ++i)
                    if (type.SameAs (parameters[i]))
                        return arguments[i] ;

                return type ;
            }

            // TODO: other type specifications
            throw new NotImplementedException () ;
        }

        /// <summary>
        /// Makes a generic method instance based on the generic method (NOT merely
        /// a method of a generic type!) and an array of type arguments.
        /// </summary>
        public static MethodReference MakeGenericInstance (this MethodReference generic,
            params TypeReference[] typeArguments)
        {
            var result = new GenericInstanceMethod (generic) ;

            foreach (var typeArgument in typeArguments)
                result.GenericArguments.Add (typeArgument) ;

            return result ;
        }

        /// <summary>
        /// Makes a reference to a field in a closed generic type.
        /// </summary>
        public static FieldReference FromDeclaringType (this FieldReference field, 
            TypeReference closedType)
        {
            return field.CloseDeclaringType (
                ((GenericInstanceType) closedType).GenericArguments.ToArray ()) ;
        }

        /// <summary>
        /// Makes a reference to a field in a closed generic type.
        /// </summary>
        public static FieldReference CloseDeclaringType (this FieldReference field, 
            params TypeReference[] typeArguments)
        {
            var source = field.DeclaringType ;
            if (source.IsGenericInstance)
                source = source.GetElementType () ;

            var result = new FieldReference (field.Name, field.FieldType)
            {
                DeclaringType = source.MakeGenericInstanceType (typeArguments),
            } ;

            return result ;
        }

        /// <summary>
        /// Makes a reference to a method in a closed generic type.
        /// </summary>
        public static MethodReference FromDeclaringType (this MethodReference method, 
            TypeReference closedType)
        {
            return method.CloseDeclaringType (
                ((GenericInstanceType) closedType).GenericArguments.ToArray ()) ;
        }

        /// <summary>
        /// Makes a reference to a method in a closed generic type.
        /// </summary>
        public static MethodReference CloseDeclaringType (this MethodReference method, 
            params TypeReference[] typeArguments)
        {
            var source = method.DeclaringType ;
            if (source.IsGenericInstance)
                source = source.GetElementType () ;

            return method.MakeReferenceIn (source.MakeGenericInstanceType (typeArguments)) ;
        }

        /// <summary>
        /// Makes a reference to a method in a related (closed/open) type.
        /// </summary>
        public static MethodReference MakeReferenceIn (this MethodReference method, TypeReference type)
        {
            var result = new MethodReference (method.Name, method.ReturnType)
            {
                DeclaringType     = type,
                HasThis           = method.HasThis,
                ExplicitThis      = method.ExplicitThis,
                CallingConvention = method.CallingConvention,
            } ;

            foreach (var parameter in method.Parameters)
                result.Parameters.Add (new ParameterDefinition (parameter.ParameterType)) ;

            foreach (var parameter in method.GenericParameters)
                result.GenericParameters.Add (new GenericParameter (parameter.Name, result)) ;

            return result ;
        }

        /// <summary>
        /// If <paramref name="method"/> is declared on a generic instance type,
        /// substitutes generic parameters for generic arguments in <paramref name="type"/>.
        /// Otherwise, returns <paramref name="type"/> as is.
        /// </summary>
        public static TypeReference GetActualType (this MethodReference method, TypeReference type)
        {
            var git  = method.DeclaringType as GenericInstanceType ;
            if (git != null)
                return type.SubstituteGenericParameters (
                    git.ElementType.GenericParameters,
                    git.GenericArguments) ;
            else
                return type ;
        }

        /// <summary>
        /// Returns the instruction that calls the base type or self constructor.
        /// </summary>
        /// <remarks>
        /// Verifiable code can only call a constructor with a <see cref="Code.Call"/>
        /// instruction if it is the base type or self instance constructor being
        /// invoked from an instance constructor, or if it is initializing a value
        /// type location. Verifiable reference type instance constructors must
        /// invoke a self or base type constructor (ECMA-335 I.8.9.6.6 and III.3.19).
        /// </remarks>
        public static Instruction FindThisOrBaseCtorInvocation (this MethodDefinition ctor)
        {
            if (ctor.DeclaringType.IsValueType)
                throw new ArgumentException () ;

            for (var i1 = ctor.Body.Instructions[0] ; i1 != null ; i1 = i1.Next)
            {
                if (i1.OpCode.Code != Code.Call)
                    continue ;

                var method = (MethodReference) i1.Operand ;
                if (method.HasThis         &&
                    method.Name == ".ctor" &&
                    method.IsVoidReturn () &&
                   !method.DeclaringType.IsValueType)
                {
                    // technically I should resolve the method
                    // and check for SpecialName and RTSpecialName flags,
                    // but it might not be resolvable at this point
                    return i1 ;
                }
            }

            // should never occur in verifiable code
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// Creates a new unnamed local variable of the specified type
        /// and adds it to the method.
        /// </summary>
        public static VariableDefinition CreateLocal (
            this ILProcessor il, TypeReference type)
        {
            var obj = new VariableDefinition (type) ;
            il.Body.Variables.Add (obj) ;
            il.Body.InitLocals = true ;
            return obj ;
        }

        /// <summary>
        /// Indicates whether <paramref name="insn"/> is a simple load instruction.
        /// </summary>
        /// <remarks>
        /// A sequence of simple load instructions can be reordered without changing
        /// the values they load.
        /// </remarks>
        public static bool IsSimpleLoad (this Instruction insn)
        {
            switch (insn.OpCode.Code)
            {
            case Code.Ldloc:
            case Code.Ldloc_0:
            case Code.Ldloc_1:
            case Code.Ldloc_2:
            case Code.Ldloc_3:
            case Code.Ldloc_S:
            case Code.Ldarg:
            case Code.Ldarg_0:
            case Code.Ldarg_1:
            case Code.Ldarg_2:
            case Code.Ldarg_3:
            case Code.Ldarg_S:
            case Code.Ldsfld:
            case Code.Ldstr:
            case Code.Ldnull:
            case Code.Ldc_I4_M1:
            case Code.Ldc_I4_0:
            case Code.Ldc_I4_1:
            case Code.Ldc_I4_2:
            case Code.Ldc_I4_3:
            case Code.Ldc_I4_4:
            case Code.Ldc_I4_5:
            case Code.Ldc_I4_6:
            case Code.Ldc_I4_7:
            case Code.Ldc_I4_8:
            case Code.Ldc_I4_S:
            case Code.Ldc_I4:
            case Code.Ldc_I8:
            case Code.Ldc_R4:
            case Code.Ldc_R8:
                return true ;

            default:
                return false ;
            }
        }

        /// <summary>
        /// Simplifies branch instruction macros (Brxxx_S) in <paramref name="il"/> to full forms.
        /// </summary>
        public static void SimplifyBranchMacros (this ILProcessor il)
        {
            foreach (var br in il.Body.Instructions)
            switch  (br.OpCode.Code)
            {
            case Code.Br_S:      br.OpCode = OpCodes.Br      ; break ;
            case Code.Brfalse_S: br.OpCode = OpCodes.Brfalse ; break ;
            case Code.Brtrue_S:  br.OpCode = OpCodes.Brtrue  ; break ;
            case Code.Beq_S:     br.OpCode = OpCodes.Beq     ; break ;
            case Code.Bge_S:     br.OpCode = OpCodes.Bge     ; break ;
            case Code.Bge_Un_S:  br.OpCode = OpCodes.Bge_Un  ; break ;
            case Code.Bgt_S:     br.OpCode = OpCodes.Bgt     ; break ;
            case Code.Bgt_Un_S:  br.OpCode = OpCodes.Bgt_Un  ; break ;
            case Code.Ble_S:     br.OpCode = OpCodes.Ble     ; break ;
            case Code.Ble_Un_S:  br.OpCode = OpCodes.Ble_Un  ; break ;
            case Code.Blt_S:     br.OpCode = OpCodes.Blt     ; break ;
            case Code.Blt_Un_S:  br.OpCode = OpCodes.Blt_Un  ; break ;
            case Code.Bne_Un_S:  br.OpCode = OpCodes.Bne_Un  ; break ;
            case Code.Leave_S:   br.OpCode = OpCodes.Leave   ; break ;
            }
        }

        /// <exclude/>
        public static void Emit (this ILProcessor il, OpCode opcode, ILProcessor method)
        {
            il.Emit (opcode, method.Body.Method) ;
        }

        /// <exclude/>
        public static Instruction Create (this ILProcessor il, OpCode opcode, ILProcessor method)
        {
            return il.Create (opcode, method.Body.Method) ;
        }

        /// <summary>
        /// Creates a copy of <paramref name="insn"/>.
        /// </summary>
        public static Instruction Clone (this Instruction insn)
        {
            var newinsn     = Instruction.Create (OpCodes.Nop) ;
            newinsn.OpCode  = insn.OpCode  ;
            newinsn.Operand = insn.Operand ;
            return newinsn ;
        }

        /// <summary>
        /// Nops out <paramref name="insn"/>.
        /// </summary>
        /// <remarks>
        /// It is often a bad idea to remove instructions with RemoveAt
        /// because it removes sequence points, breaks the linked list,
        /// potentially making variable scope ranges unusable, and creates
        /// weird corner cases with entirely empty methods.
        /// </remarks>
        public static void NopOut (this Instruction insn)
        {
            insn.OpCode  = OpCodes.Nop ;
            insn.Operand = null ;
        }
        #endregion
    }

    /// <summary>
    /// Provides the operation of comparing two type references for equality.
    /// </summary>
    /// <remarks>
    /// Returns incorrect results in some corner cases like [TypeForwardedTo].
    /// </remarks>
    public sealed class TypeReferenceEqualityComparer : IEqualityComparer<TypeReference>
    {
        /// <summary>
        /// Gets the default instance of <see cref="TypeReferenceEqualityComparer"/>.
        /// </summary>
        public static readonly TypeReferenceEqualityComparer Instance = new TypeReferenceEqualityComparer () ;

        #region --[Fields: Private]---------------------------------------
        private static readonly byte[] CoreLibraryToken = typeof (void).Assembly.GetName ().GetPublicKeyToken () ;
        private static readonly byte[] NullToken        = new byte[0] ;
        #endregion

        #region --[Interface: IEqualityComparer<>]------------------------
        /// <summary>
        /// Indicates whether the two type references refer to the same type.
        /// </summary>
        public bool Equals (TypeReference a, TypeReference b)
        {
            if (a == null)
                return b == null ;

            if (b == null)
                return false ;

            if (a.IsGenericInstance)
            {
                if (b.IsGenericInstance)
                {
                    var ag = (GenericInstanceType) a ;
                    var bg = (GenericInstanceType) b ;
                    
                    if (ag.GenericArguments.Count != bg.GenericArguments.Count)
                        return false ;

                    if (!Equals (ag.ElementType, bg.ElementType))
                        return false ;

                    for (int i = 0 ; i < ag.GenericArguments.Count ; ++i)
                        if (!Equals (ag.GenericArguments[i], bg.GenericArguments[i]))
                            return false ;

                    return true ;
                }
                else
                    return false ;
            }
            else
                if (b.IsGenericInstance)
                    return false ;

            return a.FullName == b.FullName && a.DeclaringType.SameAs (b.DeclaringType) && a.Scope.SameAs (b.Scope) ;
        }

        /// <summary>
        /// Gets a hash code for <paramref name="obj"/> that is compatible
        /// with the equality relation defined by this comparer.
        /// </summary>
        public int GetHashCode (TypeReference obj)
        {
            if (obj == null) return 0 ;
            return obj.FullName.GetHashCode () ;
        }
        #endregion
    }
}
