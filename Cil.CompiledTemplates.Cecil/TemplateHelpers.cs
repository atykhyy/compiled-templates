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
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    /// <summary>
    /// Provides helper methods for template authors.
    /// </summary>
    public static class TemplateHelpers
    {
        /// <summary>
        /// A call to this method is converted to a null reference comparison
        /// in emitted code, regardless of the template type of <c>T</c>,
        /// if the bound type of <c>T</c> is a reference type.
        /// If the bound type of <c>T</c> is a value type,
        /// an exception is thrown at code generation time.
        /// </summary>
        /// <remarks>
        /// This method helps to avoid needless duplication of templated types
        /// when it is not known beforehand whether the bound type
        /// is a value or a reference type.
        /// </remarks>
        public static bool IsNull<T> (this T value)
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// A call to this method is converted to a null reference comparison
        /// in emitted code, regardless of the template type of <c>T</c>,
        /// if the bound type of <c>T</c> is a reference type.
        /// If the bound type of <c>T</c> is a value type,
        /// the call is converted to instructions returning <c>false</c>.
        /// </summary>
        /// <remarks>
        /// This method helps to avoid needless duplication of templated types
        /// when it is not known beforehand whether the bound type
        /// is a value or a reference type.
        /// </remarks>
        public static bool IsNullReference<T> (this T value)
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// A call to this method is converted to a null constant
        /// in emitted code, regardless of the template type of <c>T</c>,
        /// if the bound type of <c>T</c> is a reference type.
        /// If the bound type of <c>T</c> is a value type,
        /// an exception is thrown at code generation time.
        /// </summary>
        /// <remarks>
        /// This method helps to avoid needless duplication of templated types
        /// when it is not known beforehand whether the bound type
        /// is a value or a reference type.
        /// </remarks>
        public static T Null<T> ()
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// A call to this method is converted to instructions
        /// creating a new <typeparamref name="T"/> using a constructor
        /// with the signature that *exactly* matches the generic type
        /// arguments of this method.
        /// </summary>
        public static T New<T, P1> (P1 p1)
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// A call to this method is converted to instructions
        /// casting <paramref name="value"/> to <c>object</c>,
        /// boxing it if <typeparamref name="T"/> is a value type.
        /// </summary>
        /// <remarks>
        /// This method helps to avoid needless duplication of templated types
        /// when it is not known beforehand whether the bound type
        /// is a value or a reference type.
        /// </remarks>
        public static object ToObject<T> (this T value)
        {
            return value ;
        }

        /// <summary>
        /// A call to this method is converted to instructions
        /// casting <c>object</c> to <paramref name="value"/>,
        /// unboxing it if <typeparamref name="T"/> is a value type.
        /// </summary>
        /// <remarks>
        /// This method helps to avoid needless duplication of templated types
        /// when it is not known beforehand whether the bound type
        /// is a value or a reference type.
        /// </remarks>
        public static T FromObject<T> (object value)
        {
            return (T) value ;
        }

        /// <summary>
        /// A call to this method is converted to a `constrained.`
        /// instruction prefix. This method can be used to avoid boxing
        /// when calling interface methods on a value type.
        /// </summary>
        public static I Constrain<T, I> (ref T self)
            where I : class
            where T : struct, I
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// Valid usage patterns: <c>this.CtorCall (new ThisType (...))</c>
        /// and <c>this.CtorCall (new BaseType (...))</c>. A call to this
        /// method with <paramref name="other"/> being an object creation
        /// expression is converted to a <c>this</c>/<c>base</c> constructor
        /// call.
        /// </summary>
        public static void CtorCall<U, V> (this U self, V other) where U : V
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// Container for the pseudo-field <see cref="Lvalue"/>.
        /// </summary>
        public struct RefThis<T> where T : struct
        {
            /// <summary>
            /// A reference to this field is converted to a load of the implicit
            /// `this` argument, regardless of the template type of <c>T</c>.
            /// </summary>
            /// <remarks>
            /// This field helps to work around the limitations of C# compiler
            /// and pass a reference to `this` to members outside of a generic
            /// nested template type that take it via generic instance types.
            /// The references' generic arguments must match the template's
            /// bound type parameters.
            /// </remarks>
            #pragma warning disable 0649
            public static T Lvalue ;
            #pragma warning restore 0649
        }

        /// <summary>
        /// A call to this method is converted to a return instruction
        /// in emitted code, returning the value passed to the method.
        /// </summary>
        /// <remarks>
        /// In spliced methods, normal returns are converted to jumps
        /// to the end of the splice block. Use this method if you need
        /// to return out of the method you are splicing into.
        /// </remarks>
        public static void Return<T> (T value)
        {
            throw new InvalidProgramException () ;
        }

        /// <summary>
        /// A call to this method is converted to a return instruction
        /// in emitted code.
        /// </summary>
        /// <remarks>
        /// In spliced methods, normal returns are converted to jumps
        /// to the end of the splice block. Use this method if you need
        /// to return out of the method you are splicing into.
        /// </remarks>
        public static void Return ()
        {
            throw new InvalidProgramException () ;
        }
    }
}
