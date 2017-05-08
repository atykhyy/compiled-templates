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
