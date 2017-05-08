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
using System.Collections.Generic ;
using Mono.Cecil ;
using Mono.Cecil.Cil ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    public class ILBranchManager
    {
        #region --[Fields: Private]---------------------------------------
        private readonly ILProcessor m_il ;

        private readonly Dictionary<Instruction, List<KeyValuePair<Instruction, int>>> m_targets =
                     new Dictionary<Instruction, List<KeyValuePair<Instruction, int>>> () ;
        #endregion

        #region --[Methods: Public]---------------------------------------
        /// <summary>
        /// Constructs a new <see cref="ILBranchManager"/>.
        /// </summary>
        public ILBranchManager (ILProcessor il)
        {
            m_il = il ;

            // calculate branch targets
            foreach (var br in il.Body.Instructions)
            switch  (br.OpCode.OperandType)
            {
            case OperandType.InlineSwitch:
                var labels = (IList<Instruction>) br.Operand ;
                for (int i = 0 ; i < labels.Count ; ++i)
                    m_targets.GetOrAdd (labels[i]).Add (
                        new KeyValuePair<Instruction, int> (br, i)) ;

                break ;

            case OperandType.ShortInlineBrTarget:
            case OperandType.InlineBrTarget:
                m_targets.GetOrAdd ((Instruction) br.Operand).Add (
                        new KeyValuePair<Instruction, int> (br, -1)) ;
                break ;
            }

            // KLUDGE: make all branches long to avoid out-of-range after modification
            il.SimplifyBranches () ;
        }

        /// <summary>
        /// Checks whether <paramref name="insn"/> is a branch target.
        /// </summary>
        public bool IsTarget (Instruction insn)
        {
            return m_targets.ContainsKey (insn) ;
        }

        /// <summary>
        /// Retargets all branches to <paramref name="from"/> to instruction <paramref name="to"/>.
        /// </summary>
        public void Retarget (Instruction from, Instruction to)
        {
            List<KeyValuePair<Instruction, int>> list ;
            if(!m_targets.TryGetValue (from, out list))
                return ;

            foreach (var kv in list)
            switch  (kv.Key.OpCode.OperandType)
            {
            case OperandType.InlineSwitch:
                ((IList<Instruction>) kv.Key.Operand)[kv.Value] = to ;
                break ;

            case OperandType.ShortInlineBrTarget:
            case OperandType.InlineBrTarget:
                kv.Key.Operand = to ;
                break ;
            }
        }
        #endregion
    }
}
