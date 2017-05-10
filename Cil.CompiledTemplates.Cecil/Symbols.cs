using System ;
using System.IO ;
using System.Runtime.InteropServices ;
using System.Diagnostics.SymbolStore ;

namespace Cil.CompiledTemplates
{
    static class SymbolHelpers
    {
        public static ISymbolReader GetSymbolReader (string filename)
        {
            var importer = default (IntPtr) ;
            try
            {
                s_dispenser.OpenScope (filename, 0, ref IID_IMetaDataImport, out importer) ;
                return new SymBinder ().GetReader (importer, filename, Path.GetDirectoryName (filename)) ;
            }
            finally
            {
                if (importer != default (IntPtr))
                    Marshal.Release (importer) ;
            }
        }

        private static IMetaDataDispenser s_dispenser = new CorMetaDataDispenserExClass () as IMetaDataDispenser ;
        private static Guid IID_IMetaDataImport       = new Guid ("7dac8207-d3ae-4c75-9b67-92801a497d44") ;
    }

    [ComImport]
    [Guid ("e5cb7a31-7512-11d2-89ce-0080c792e5d8")]
    class CorMetaDataDispenserExClass
    {
    }

    [Guid ("809c652e-7396-11d2-9771-00a0c9b4d50c")]
    [ComVisible (true), InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    interface IMetaDataDispenser
    {
        void DefineScope_Placeholder () ;

        void OpenScope (
            [In, MarshalAs (UnmanagedType.LPWStr)] string szScope,
            [In] int dwOpenFlags,
            [In] ref Guid riid, 
            [Out] out IntPtr punk) ;

        // Don't need any other methods.
    }
}
