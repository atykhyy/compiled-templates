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
    [AttributeUsage (AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Field | AttributeTargets.Method)]
    public sealed class EmitNameAttribute : Attribute
    {
        public EmitNameAttribute (string name)
        {
            if (name == null)
                throw new ArgumentNullException () ;

            Name = name ;
        }

        public string Name { get ; private set ; }

        public bool ReadOnly  { get ; set ; }
        public bool Private   { get ; set ; }
        public bool Protected { get ; set ; }
    }

    interface IEmitLabelAttribute
    {
        Type[] Labels { get ; }
    }

    public sealed class EmitLabelAttribute : Attribute, IEmitLabelAttribute
    {
        /// <summary>
        /// This form of the attribute is used for method parameters.
        /// </summary>
        public EmitLabelAttribute ()
        {
        }

        public EmitLabelAttribute (params Type[] labels)
        {
            if (labels == null)
                throw new ArgumentNullException () ;

            Labels = labels ;
        }

        public Type[] Labels { get ; private set ; }
    }

    [AttributeUsage (AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true)]
    public sealed class BindLabelAttribute : Attribute, IEmitLabelAttribute
    {
        public BindLabelAttribute (Type type, params Type[] labels)
        {
            if (labels == null)
                throw new ArgumentNullException () ;

            if (type == null)
                throw new ArgumentNullException () ;

            Type   = type   ;
            Labels = labels ;
        }

        public Type   Type   { get ; private set ; }
        public Type[] Labels { get ; private set ; }
    }

    [AttributeUsage (AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true)]
    public sealed class EmitExplicitInterfaceImplAttribute : Attribute, IEmitLabelAttribute
    {
        public EmitExplicitInterfaceImplAttribute (Type @interface, params Type[] labels)
        {
            if (labels == null)
                throw new ArgumentNullException () ;

            if (@interface == null)
                throw new ArgumentNullException () ;

            if(!@interface.IsInterface)
                throw new ArgumentException () ;

            Interface = @interface ;
            Labels    = labels ;
        }

        public Type   Interface { get ; private set ; }
        public Type[] Labels    { get ; private set ; }
    }

    [AttributeUsage (AttributeTargets.All & ~(AttributeTargets.Parameter | AttributeTargets.ReturnValue))]
    public sealed class TemplatedMemberAttribute : Attribute
    {
    }

    [AttributeUsage (AttributeTargets.Parameter)]
    public sealed class TemplatedParameterAttribute : Attribute
    {
    }

    [AttributeUsage (AttributeTargets.Method)]
    public sealed class TemplatedThisAttribute : Attribute
    {
    }

    [AttributeUsage (AttributeTargets.Method | AttributeTargets.Parameter)]
    public sealed class FromILStackAttribute : Attribute
    {
    }
}
