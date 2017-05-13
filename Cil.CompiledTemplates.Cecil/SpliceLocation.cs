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
using Mono.Cecil.Cil ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    /// <summary>
    /// Enumerates the ways the splice location
    /// relates to the indicated instruction.
    /// </summary>
    public enum SpliceRelation
    {
        /// <summary>
        /// The splice is inserted before the instruction.
        /// </summary>
        Before,

        /// <summary>
        /// The splice is inserted before the instruction
        /// as if it was a part of that instruction,
        /// i.e. branches to the instruction will go
        /// to the first instruction of the splice.
        /// </summary>
        BeforeAsPart,

        /// <summary>
        /// The splice replaces the instruction.
        /// </summary>
        Replace,

        /// <summary>
        /// The splice is inserted after the instruction
        /// as if it was a part of that instruction,
        /// i.e. branches to the instruction following
        /// the indicated instruction will NOT be retargeted
        /// to the first instruction of the splice.
        /// </summary>
        AfterAsPart,
    }

    /// <summary>
    /// Describes the target of a method splice operation.
    /// </summary>
    public sealed class SpliceLocation
    {
        private readonly Instruction    m_insn ;
        private readonly SpliceRelation m_relation ;

        private SpliceLocation (Instruction insn, SpliceRelation relation)
        {
            m_insn     = insn ;
            m_relation = relation ;
        }

        /// <summary>
        /// Gets the instruction identifying the splice location.
        /// </summary>
        public Instruction    Insn     { get { return m_insn ;     }}

        /// <summary>
        /// Indicates how the splice location relates to <see cref="Insn"/>.
        /// </summary>
        public SpliceRelation Relation { get { return m_relation ; }}

        /// <summary>
        /// [1]*[insn] [2]
        /// becomes
        /// [1] [splice]*[insn] [2]
        /// 
        /// (* marks branch(es) to insn)
        /// </summary>
        /// <remarks>
        /// FromILStack parameters cannot be used with this location.
        /// </remarks>
        public static SpliceLocation Before (Instruction insn)
        {
            if (insn == null)
                throw new ArgumentNullException () ;

            return new SpliceLocation (insn, SpliceRelation.Before) ;
        }

        /// <summary>
        /// [1]*[insn] [2]
        /// becomes
        /// [1]*[splice] [insn] [2]
        /// 
        /// (* marks branch(es) to insn)
        /// </summary>
        public static SpliceLocation BeforeAsPart (Instruction insn)
        {
            if (insn == null)
                throw new ArgumentNullException () ;

            return new SpliceLocation (insn, SpliceRelation.BeforeAsPart) ;
        }

        /// <summary>
        /// [1]*[insn] [2]
        /// becomes
        /// [1]*[splice] [2]
        /// 
        /// (* marks branch(es) to insn)
        /// </summary>
        /// <remarks>
        /// insn may be converted into a nop instead of being removed.
        /// </remarks>
        public static SpliceLocation Replace (Instruction insn)
        {
            if (insn == null)
                throw new ArgumentNullException () ;

            return new SpliceLocation (insn, SpliceRelation.Replace) ;
        }

        /// <summary>
        /// [1]*[insn] [2]
        /// becomes
        /// [1]*[insn] [splice] [2]
        /// 
        /// (* marks branch(es) to insn)
        /// </summary>
        public static SpliceLocation AfterAsPart (Instruction insn)
        {
            if (insn == null)
                throw new ArgumentNullException () ;

            return new SpliceLocation (insn, SpliceRelation.AfterAsPart) ;
        }

        /// <summary>
        /// The splice is inserted before the first instruction
        /// of the target method.
        /// </summary>
        public static readonly SpliceLocation AtStart = new SpliceLocation (null, SpliceRelation.Before) ;

        /// <summary>
        /// The splice is inserted after the last instruction
        /// of the target method.
        /// </summary>
        public static readonly SpliceLocation AtEnd   = new SpliceLocation (null, SpliceRelation.BeforeAsPart) ;
    }
}
