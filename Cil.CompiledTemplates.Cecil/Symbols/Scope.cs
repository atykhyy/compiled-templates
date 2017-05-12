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
        #region --[Fields: Private]---------------------------------------
        private Instruction m_start  ;
        private Instruction m_end    ;
        private List<Scope> m_scopes ;

        private List<VariableDebugInformation> m_variables ;
        #endregion

        public Scope (Instruction start, Instruction end)
        {
            m_start = start ;
            m_end   = end   ;
        }

        public Scope (ScopeDebugInformation scope, Dictionary<int, Instruction> insns)
        {
            insns.TryGetValue (scope.Start.Offset, out m_start) ;
            insns.TryGetValue (scope.End.Offset,   out m_end)   ;

            m_variables = new List<VariableDebugInformation> (scope.Variables) ;

            foreach (var s in scope.Scopes)
            {
                if (m_scopes  == null)
                    m_scopes   = new List<Scope> () ;

                m_scopes.Add (new Scope (s, insns)) ;
            }
        }

        public Scope (ISymbolScope scope, VariableDefinition[] locals, Dictionary<int, Instruction> insns)
        {
            m_start = insns[scope.StartOffset] ;
            m_end   = insns[scope.EndOffset]   ;

            foreach (var v in scope.GetLocals ())
                if (v.AddressKind == SymAddressKind.ILOffset)
                {
                    var local = locals[v.AddressField1] ;

                    if (m_variables == null)
                        m_variables  = new List<VariableDebugInformation> () ;

                    m_variables.Add (new VariableDebugInformation (local, v.Name)) ;
                }

            foreach (var s in scope.GetChildren ())
            {
                if (m_scopes  == null)
                    m_scopes   = new List<Scope> () ;

                m_scopes.Add (new Scope (s, locals, insns)) ;
            }
        }

        public ScopeDebugInformation ToCecil ()
        {
            var result = new ScopeDebugInformation (m_start, m_end) ;

            if (m_scopes != null)
                foreach (var s in m_scopes)
                    result.Scopes.Add (s.ToCecil ()) ;

            if (m_variables != null)
                foreach (var v in m_variables)
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
            if (m_scopes == null)
                m_scopes  = new List<Scope> () ;

            if (location == SpliceLocation.AtEnd)
            {
                if (m_start == null)
                    m_start  = scope.m_start ;

                m_scopes.Add (scope) ;
            }
            else
            if (location == SpliceLocation.AtStart)
            {
                m_scopes.Insert (0, scope) ;
                m_start = scope.m_start ;
            }
            else
                // if it's neither at start nor at end,
                // the method can't have been empty
                Insert (scope) ;
        }

        private bool Insert (Scope scope)
        {
            if (m_scopes == null)
            {
                m_scopes  = new List<Scope> () ;
                m_scopes.Add (scope) ;
                return true ;
            }

            var insn = m_start ;
            var pos  = 0 ;

            while (true)
            {
                if (insn == m_end)
                    return false ;

                if (insn == null)
                    throw new InvalidOperationException () ;

                if (insn == scope.m_start)
                {
                    m_scopes.Insert (pos, scope) ;
                    return true ;
                }

                if (pos  != m_scopes.Count &&
                    insn == m_scopes[pos].m_start)
                {
                    if (m_scopes[pos].Insert (scope))
                        return true ;

                    insn  = m_scopes[pos].m_end ;
                    pos  += 1 ;
                }
                else
                    insn  = insn.Next ;
            }
        }
    }
}
