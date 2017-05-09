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
using System.Reflection ;
using Mono.Cecil ;
#endregion

namespace Cil.CompiledTemplates.Cecil
{
    public sealed class AppDomainAssemblyResolver : IAssemblyResolver
    {
        public static readonly AppDomainAssemblyResolver Instance = new AppDomainAssemblyResolver () ;

        public AssemblyDefinition Resolve (string fullName)
        {
            return Resolve (AppDomain.CurrentDomain.Load (fullName), new ReaderParameters ()) ;
        }

        public AssemblyDefinition Resolve (AssemblyNameReference name)
        {
            return Resolve (AppDomain.CurrentDomain.Load (name.ToAssemblyName ()), new ReaderParameters ()) ;
        }

        public AssemblyDefinition Resolve (string fullName, ReaderParameters parameters)
        {
            return Resolve (AppDomain.CurrentDomain.Load (fullName), parameters) ;
        }

        public AssemblyDefinition Resolve (AssemblyNameReference name, ReaderParameters parameters)
        {
            return Resolve (AppDomain.CurrentDomain.Load (name.ToAssemblyName ()), parameters) ;
        }

        private AssemblyDefinition Resolve (Assembly assembly, ReaderParameters parameters)
        {
            return AssemblyDefinition.ReadAssembly (assembly.ManifestModule.FullyQualifiedName, parameters) ;
        }
    }
}