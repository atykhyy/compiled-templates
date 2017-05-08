using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[assembly: AssemblyProduct("CIL compiled templates library")]
[assembly: AssemblyCopyright("Copyright © Anton Tykhyy 2017")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyConfiguration("Release")]
#endif

// Setting ComVisible to false makes the types in this assembly not visible 
// to COM components.  If you need to access a type in this assembly from 
// COM, set the ComVisible attribute to true on that type.
[assembly: ComVisible(false)]

// I use only the major version for the .NET assembly version.
// Binaries with different major versions may not be backwards binary-compatible.
// Minor version is informational and is specified in the .nuspec file.
// Minor versions may add features or APIs but are binary backward-compatible.
// Assembly(File/Informational)Version contain both major and minor version,
// as well as the repository revision number and the commit hash.
// AssemblyFileVersion identifies the exact binary and is displayed in Windows Explorer file details.
// AssemblyInformationalVersion is intended solely as a human-readable version string.
// NuGet package version is taken from AssemblyFileVersion.
#if !NOASSEMBLYVERSION
[assembly: AssemblyVersion("1.0")]
#endif