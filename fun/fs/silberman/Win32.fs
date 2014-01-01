namespace silberman

open System
open System.Runtime.InteropServices

module internal Win32 =

    type Interop() = 
                
        [<DllImport("user32.dll", SetLastError = true)>]
        static extern [<MarshalAs(UnmanagedType.Bool)>] bool PostMessage(IntPtr hwnd, uint32 msg, IntPtr wParam , IntPtr lParam)

        static member Post hwnd msg wParam lParam = PostMessage(hwnd, msg, wParam, lParam)

