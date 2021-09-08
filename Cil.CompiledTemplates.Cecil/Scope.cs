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
            {
                var stack = default (Stack<(ScopeDebugInformation, int)>) ;
                var index = 0 ;

                for (var insn = body.Instructions.FirstOrDefault () ; insn != null ; )
                {
                    if(!target.HasScopes)
                    {
                        target.Scopes.Add (scope) ;
                        break ;
                    }

                    var o = new InstructionOffset (insn) ;
                    if (o.Equals (scope.Start))
                    {
                        target.Scopes.Insert (index, scope) ;
                        break ;
                    }

                    if (o.Equals (target.End))
                    {
                        if (stack == null || stack.Count == 0)
                            break ;

                        (target, index) = stack.Pop () ;
                    }
                    else
                    if (index  != target.Scopes.Count &&
                        o.Equals (target.Scopes[index].Start))
                    {
                        if (stack == null)
                            stack  = new Stack<(ScopeDebugInformation, int)> () ;

                        stack.Push ((target, index + 1)) ;
                        target = target.Scopes[index] ;
                        index  = 0 ;
                    }
                    else insn  = insn.Next ;
                }
            }
        }
    }
}
