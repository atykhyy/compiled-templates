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
    /// Supplies the name and optional modifications of visibility
    /// for members copied from the template member.
    /// </summary>
    /// <remarks>
    /// The name is treated as a format string
    /// and the name of the current context scope is supplied
    /// as the first format argument.
    /// </remarks>
    [AttributeUsage (AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Field | AttributeTargets.Method)]
    public sealed class EmitNameAttribute : Attribute
    {
        /// <exclude/>
        public EmitNameAttribute (string name)
        {
            if (name == null)
                throw new ArgumentNullException () ;

            Name = name ;
        }

        /// <exclude/>
        public string Name { get ; private set ; }

        /// <summary>
        /// If <c>true</c>, the copied field will be made read-only.
        /// </summary>
        public bool ReadOnly  { get ; set ; }

        /// <summary>
        /// If <c>true</c>, the copied member will be made private.
        /// </summary>
        public bool Private   { get ; set ; }

        /// <summary>
        /// If <c>true</c>, the copied member will be made protected.
        /// </summary>
        public bool Protected { get ; set ; }
    }

    interface IEmitLabelAttribute
    {
        Type[] Labels { get ; }
    }

    /// <summary>
    /// Indicates that the member is to be copied with the supplied labels.
    /// </summary>
    public sealed class EmitLabelAttribute : Attribute, IEmitLabelAttribute
    {
        /// <summary>
        /// This form of the attribute is used for method parameters.
        /// </summary>
        public EmitLabelAttribute ()
        {
        }

        /// <exclude/>
        public EmitLabelAttribute (params Type[] labels)
        {
            if (labels == null)
                throw new ArgumentNullException () ;

            Labels = labels ;
        }

        /// <exclude/>
        public Type[] Labels { get ; private set ; }
    }

    /// <summary>
    /// Indicates that the templated type is to be bound
    /// to the supplied type when a matching label is copied.
    /// <para/>
    /// This attribute works as an implicit <see cref="TemplateContext.Bind(Type, Type)"/>.
    /// </summary>
    [AttributeUsage (AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true)]
    public sealed class BindLabelAttribute : Attribute, IEmitLabelAttribute
    {
        /// <exclude/>
        public BindLabelAttribute (Type type, params Type[] labels)
        {
            if (labels == null)
                throw new ArgumentNullException () ;

            if (type == null)
                throw new ArgumentNullException () ;

            Type   = type   ;
            Labels = labels ;
        }

        /// <exclude/>
        public Type   Type   { get ; private set ; }

        /// <exclude/>
        public Type[] Labels { get ; private set ; }
    }

    /// <summary>
    /// Indicates that the explicit implementation of the interface
    /// is to be copied with the supplied labels.
    /// </summary>
    [AttributeUsage (AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true)]
    public sealed class EmitExplicitInterfaceImplAttribute : Attribute, IEmitLabelAttribute
    {
        /// <exclude/>
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

        /// <exclude/>
        public Type   Interface { get ; private set ; }

        /// <exclude/>
        public Type[] Labels    { get ; private set ; }
    }

    /// <summary>
    /// Indicates that the type, field or method is a template variable
    /// and can be bound to something else. If the member is not bound,
    /// the library forms a member reference by substituting any bound
    /// types in the template declaration and applying <see cref="EmitNameAttribute"/>
    /// if it exists.
    /// </summary>
    [AttributeUsage (AttributeTargets.All & ~(AttributeTargets.Parameter | AttributeTargets.ReturnValue))]
    public sealed class TemplatedMemberAttribute : Attribute
    {
    }

    /// <summary>
    /// Indicates that the method parameter is templated
    /// and must be bound or marked with <see cref="FromILStackAttribute"/>.
    /// The parameter is removed from the copied method's parameter list.
    /// </summary>
    [AttributeUsage (AttributeTargets.Parameter)]
    public sealed class TemplatedParameterAttribute : Attribute
    {
    }

    /// <summary>
    /// Indicates that the implicit <c>this</c> parameter
    /// of the decorated method is templated
    /// and must be bound or marked with <see cref="FromILStackAttribute"/>.
    /// </summary>
    [AttributeUsage (AttributeTargets.Method)]
    public sealed class TemplatedThisAttribute : Attribute
    {
    }

    /// <summary>
    /// Indicates that the templated parameter must be taken
    /// directly from the IL stack at the splice location.
    /// </summary>
    [AttributeUsage (AttributeTargets.Method | AttributeTargets.Parameter)]
    public sealed class FromILStackAttribute : Attribute
    {
    }
}
