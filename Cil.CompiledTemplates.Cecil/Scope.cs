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
using System.Linq ;
using Mono.Cecil.Cil ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    partial class TemplateContext : TemplateContextBase
    {
        private ScopeDebugInformation CopyScope (ScopeDebugInformation source,
            VariableDefinition[] locals, Dictionary<int, Instruction> insns)
        {
            var target = new ScopeDebugInformation (insns[source.Start.Offset], insns[source.End.Offset]) ;

            if (source.HasVariables) foreach (var v in source.Variables)
                target.Variables.Add (new VariableDebugInformation (locals[v.Index], v.Name)) ;

            if (source.HasScopes) foreach (var s in source.Scopes)
                target.Scopes.Add (CopyScope (s, locals, insns)) ;

            if (source.HasConstants) foreach (var c in source.Constants)
                target.Constants.Add (new ConstantDebugInformation (c.Name, m_target.Module.ImportReference (c.ConstantType), c.Value)) ;

            return target ;
        }

        private static void SpliceScope (MethodBody body, ScopeDebugInformation scope, SpliceLocation location)
        {
            var target  = body.Method.DebugInformation.Scope ;
            if (target == null)
            {
                target  = new ScopeDebugInformation (body.Instructions.FirstOrDefault (), null) ;
                body.Method.DebugInformation.Scope = target ;
            }

            if (location == SpliceLocation.AtEnd)
            {
                if (target.Start.IsEndOfMethod)
                    target.Start = scope.Start ;

                target.Scopes.Add (scope) ;
            }
            else
            if (location == SpliceLocation.AtStart)
            {
                target.Scopes.Insert (0, scope) ;
                target.Start = scope.Start ;
            }
            else
                InsertScope (target, scope) ;
        }

        private static bool InsertScope (ScopeDebugInformation target, ScopeDebugInformation scope)
        {
            if(!target.HasScopes)
            {
                target.Scopes.Add (scope) ;
                return true ;
            }

            var list = target.Scopes ;
            var insn = target.Start.Instruction ;
            var end  = target.End.Instruction   ;
            var pos  = 0 ;

            while (true)
            {
                if (insn == end)
                    return false ;

                if (insn == null)
                    throw new InvalidOperationException () ;

                if (insn == scope.Start.Instruction)
                {
                    list.Insert (pos, scope) ;
                    return true ;
                }

                if (pos  != list.Count &&
                    insn == list[pos].Start.Instruction)
                {
                    if (InsertScope (list[pos], scope))
                        return true ;

                    insn  = list[pos].End.Instruction ;
                    pos  += 1 ;
                }
                else
                    insn  = insn.Next ;
            }
        }
    }
}
