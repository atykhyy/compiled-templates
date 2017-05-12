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
using System.Linq ;
using System.Collections.Generic ;
using System.Diagnostics.SymbolStore ;
using Mono.Cecil ;
using Mono.Cecil.Cil ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    sealed class Scope
    {
        public Instruction Start  ;
        public Instruction End    ;
        public List<Scope> Scopes ;

        public List<VariableDebugInformation> Variables ;

        public Scope (Instruction start, Instruction end)
        {
            Start = start ;
            End   = end   ;
        }

        public Scope (ScopeDebugInformation scope, Dictionary<int, Instruction> insns)
        {
            insns.TryGetValue (scope.Start.Offset, out Start) ;
            insns.TryGetValue (scope.End.Offset,   out End)   ;

            Variables = new List<VariableDebugInformation> (scope.Variables) ;

            foreach (var s in scope.Scopes)
            {
                if (Scopes  == null)
                    Scopes   = new List<Scope> () ;

                Scopes.Add (new Scope (s, insns)) ;
            }
        }

        public Scope (ISymbolScope scope, VariableDefinition[] locals, Dictionary<int, Instruction> insns)
        {
            Start = insns[scope.StartOffset] ;
            End   = insns[scope.EndOffset]   ;

            foreach (var v in scope.GetLocals ())
                if (v.AddressKind == SymAddressKind.ILOffset)
                {
                    var local = locals[v.AddressField1] ;

                    if (Variables == null)
                        Variables  = new List<VariableDebugInformation> () ;

                    Variables.Add (new VariableDebugInformation (local, v.Name)) ;
                }

            foreach (var s in scope.GetChildren ())
            {
                if (Scopes  == null)
                    Scopes   = new List<Scope> () ;

                Scopes.Add (new Scope (s, locals, insns)) ;
            }
        }

        public ScopeDebugInformation ToCecil ()
        {
            var result = new ScopeDebugInformation (Start, End) ;

            if (Scopes != null)
                foreach (var s in Scopes)
                    result.Scopes.Add (s.ToCecil ()) ;

            if (Variables != null)
                foreach (var v in Variables)
                    result.Variables.Add (v) ;

            return result ;
        }

        public static Scope FromMethodBody (MethodBody body)
        {
            if (body.Method.DebugInformation.Scope != null)
            {
                return new Scope (body.Method.DebugInformation.Scope,
                    body.Instructions.ToDictionary (_ => _.Offset)) ;
            }
            else
                return new Scope (body.Instructions.FirstOrDefault (), null) ;
        }

        public void Splice (Scope scope, SpliceLocation location)
        {
            if (Scopes == null)
                Scopes  = new List<Scope> () ;

            if (location == SpliceLocation.AtEnd)
            {
                if (Start == null)
                    Start  = scope.Start ;

                Scopes.Add (scope) ;
            }
            else
            if (location == SpliceLocation.AtStart)
            {
                Scopes.Insert (0, scope) ;
                Start = scope.Start ;
            }
            else
                // if it's neither at start nor at end,
                // the method can't have been empty
                Insert (scope) ;
        }

        private bool Insert (Scope scope)
        {
            if (Scopes == null)
            {
                Scopes  = new List<Scope> () ;
                Scopes.Add (scope) ;
                return true ;
            }

            var insn = Start ;
            var pos  = 0 ;

            while (true)
            {
                if (insn == End)
                    return false ;

                if (insn == null)
                    throw new InvalidOperationException () ;

                if (insn == scope.Start)
                {
                    Scopes.Insert (pos, scope) ;
                    return true ;
                }

                if (pos  != Scopes.Count &&
                    insn == Scopes[pos].Start)
                {
                    if (Scopes[pos].Insert (scope))
                        return true ;

                    insn  = Scopes[pos].End ;
                    pos  += 1 ;
                }
                else
                    insn  = insn.Next ;
            }
        }
    }
}
