using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Windows;

// ReSharper disable FieldCanBeMadeReadOnly.Local
// ReSharper disable InconsistentNaming
// ReSharper disable MemberCanBePrivate.Local
// ReSharper disable UnusedField.Compiler
// ReSharper disable UnusedMember.Local

namespace mst.lowlevel
{
    static class Input
    {
        [StructLayout(LayoutKind.Sequential)]
        struct MOUSEINPUT 
        {
            public Int32       dx          ;
            public Int32       dy          ;
            public UInt32      mouseData   ;
            public UInt32      dwFlags     ;
            public UInt32      time        ;
            public IntPtr      dwExtraInfo ;
        }

        [StructLayout(LayoutKind.Sequential)]
        struct KEYBDINPUT 
        {
          public UInt16        wVk         ;
          public UInt16        wScan       ;
          public UInt32        dwFlags     ;
          public UInt32        time        ;
          public IntPtr        dwExtraInfo ;
        }

        [StructLayout(LayoutKind.Sequential)]
        struct HARDWAREINPUT 
        {
          public UInt32        uMsg        ;
          public UInt16        wParamL     ;
          public UInt16        wParamH     ;
        }

        [StructLayout(LayoutKind.Explicit)]
        struct INPUT 
        {
            [FieldOffset(0)]
            public UInt32       type    ;

            [FieldOffset(sizeof(UInt32))]
            public MOUSEINPUT    mi     ;

            [FieldOffset(sizeof(UInt32))]
            public KEYBDINPUT    ki     ;

            [FieldOffset(sizeof(UInt32))]
            public HARDWAREINPUT hi     ;
        }

        const UInt32    INPUT_MOUSE                     = 0         ;
        const UInt32    INPUT_KEYBOARD                  = 1         ;
        const UInt32    INPUT_HARDWARE                  = 2         ;

        const UInt32    KEYEVENTF_EXTENDEDKEY           = 0x0001    ;
        const UInt32    KEYEVENTF_KEYUP                 = 0x0002    ;
        const UInt32    KEYEVENTF_UNICODE               = 0x0004    ;
        const UInt32    KEYEVENTF_SCANCODE              = 0x0008    ;
 
        const UInt32    MOUSEEVENTF_MOVE                = 0x0001                    ;
        const UInt32    MOUSEEVENTF_LEFTDOWN            = 0x0002                    ;
        const UInt32    MOUSEEVENTF_LEFTUP              = 0x0004                    ;
        const UInt32    MOUSEEVENTF_RIGHTDOWN           = 0x0008                    ;
        const UInt32    MOUSEEVENTF_RIGHTUP             = 0x0010                    ;
        const UInt32    MOUSEEVENTF_MIDDLEDOWN          = 0x0020                    ;
        const UInt32    MOUSEEVENTF_MIDDLEUP            = 0x0040                    ;
        const UInt32    MOUSEEVENTF_XDOWN               = 0x0080                    ;
        const UInt32    MOUSEEVENTF_XUP                 = 0x0100                    ;
        const UInt32    MOUSEEVENTF_WHEEL               = 0x0800                    ;
        const UInt32    MOUSEEVENTF_HWHEEL              = 0x01000                   ;
        const UInt32    MOUSEEVENTF_MOVE_NOCOALESCE     = 0x2000                    ;
        const UInt32    MOUSEEVENTF_VIRTUALDESK         = 0x4000                    ;
        const UInt32    MOUSEEVENTF_ABSOLUTE            = 0x8000                    ;
                                                                                    
        const Int32     SM_CXSCREEN                     = 0                         ;
        const Int32     SM_CYSCREEN                     = 1                         ;
        const Int32     SM_CXVSCROLL                    = 2                         ;
        const Int32     SM_CYHSCROLL                    = 3                         ;
        const Int32     SM_CYCAPTION                    = 4                         ;
        const Int32     SM_CXBORDER                     = 5                         ;
        const Int32     SM_CYBORDER                     = 6                         ;
        const Int32     SM_CXDLGFRAME                   = 7                         ;
        const Int32     SM_CYDLGFRAME                   = 8                         ;
        const Int32     SM_CYVTHUMB                     = 9                         ;
        const Int32     SM_CXHTHUMB                     = 10                        ;
        const Int32     SM_CXICON                       = 11                        ;
        const Int32     SM_CYICON                       = 12                        ;
        const Int32     SM_CXCURSOR                     = 13                        ;
        const Int32     SM_CYCURSOR                     = 14                        ;
        const Int32     SM_CYMENU                       = 15                        ;
        const Int32     SM_CXFULLSCREEN                 = 16                        ;
        const Int32     SM_CYFULLSCREEN                 = 17                        ;
        const Int32     SM_CYKANJIWINDOW                = 18                        ;
        const Int32     SM_MOUSEPRESENT                 = 19                        ;
        const Int32     SM_CYVSCROLL                    = 20                        ;
        const Int32     SM_CXHSCROLL                    = 21                        ;
        const Int32     SM_DEBUG                        = 22                        ;
        const Int32     SM_SWAPBUTTON                   = 23                        ;
        const Int32     SM_RESERVED1                    = 24                        ;
        const Int32     SM_RESERVED2                    = 25                        ;
        const Int32     SM_RESERVED3                    = 26                        ;
        const Int32     SM_RESERVED4                    = 27                        ;
        const Int32     SM_CXMIN                        = 28                        ;
        const Int32     SM_CYMIN                        = 29                        ;
        const Int32     SM_CXSIZE                       = 30                        ;
        const Int32     SM_CYSIZE                       = 31                        ;
        const Int32     SM_CXFRAME                      = 32                        ;
        const Int32     SM_CYFRAME                      = 33                        ;
        const Int32     SM_CXMINTRACK                   = 34                        ;
        const Int32     SM_CYMINTRACK                   = 35                        ;
        const Int32     SM_CXDOUBLECLK                  = 36                        ;
        const Int32     SM_CYDOUBLECLK                  = 37                        ;
        const Int32     SM_CXICONSPACING                = 38                        ;
        const Int32     SM_CYICONSPACING                = 39                        ;
        const Int32     SM_MENUDROPALIGNMENT            = 40                        ;
        const Int32     SM_PENWINDOWS                   = 41                        ;
        const Int32     SM_DBCSENABLED                  = 42                        ;
        const Int32     SM_CMOUSEBUTTONS                = 43                        ;
        const Int32     SM_CXFIXEDFRAME                 = SM_CXDLGFRAME             ;
        const Int32     SM_CYFIXEDFRAME                 = SM_CYDLGFRAME             ;
        const Int32     SM_CXSIZEFRAME                  = SM_CXFRAME                ;
        const Int32     SM_CYSIZEFRAME                  = SM_CYFRAME                ;
        const Int32     SM_SECURE                       = 44                        ;                        
        const Int32     SM_CXEDGE                       = 45                        ;
        const Int32     SM_CYEDGE                       = 46                        ;
        const Int32     SM_CXMINSPACING                 = 47                        ;
        const Int32     SM_CYMINSPACING                 = 48                        ;
        const Int32     SM_CXSMICON                     = 49                        ;
        const Int32     SM_CYSMICON                     = 50                        ;
        const Int32     SM_CYSMCAPTION                  = 51                        ;
        const Int32     SM_CXSMSIZE                     = 52                        ;
        const Int32     SM_CYSMSIZE                     = 53                        ;
        const Int32     SM_CXMENUSIZE                   = 54                        ;
        const Int32     SM_CYMENUSIZE                   = 55                        ;
        const Int32     SM_ARRANGE                      = 56                        ;
        const Int32     SM_CXMINIMIZED                  = 57                        ;
        const Int32     SM_CYMINIMIZED                  = 58                        ;
        const Int32     SM_CXMAXTRACK                   = 59                        ;
        const Int32     SM_CYMAXTRACK                   = 60                        ;
        const Int32     SM_CXMAXIMIZED                  = 61                        ;
        const Int32     SM_CYMAXIMIZED                  = 62                        ;
        const Int32     SM_NETWORK                      = 63                        ;
        const Int32     SM_CLEANBOOT                    = 67                        ;
        const Int32     SM_CXDRAG                       = 68                        ;
        const Int32     SM_CYDRAG                       = 69                        ;
        const Int32     SM_SHOWSOUNDS                   = 70                        ;
        const Int32     SM_CXMENUCHECK                  = 71                        ;
        const Int32     SM_CYMENUCHECK                  = 72                        ;
        const Int32     SM_SLOWMACHINE                  = 73                        ;
        const Int32     SM_MIDEASTENABLED               = 74                        ;
        const Int32     SM_MOUSEWHEELPRESENT            = 75                        ;
        const Int32     SM_XVIRTUALSCREEN               = 76                        ;
        const Int32     SM_YVIRTUALSCREEN               = 77                        ;
        const Int32     SM_CXVIRTUALSCREEN              = 78                        ;
        const Int32     SM_CYVIRTUALSCREEN              = 79                        ;
        const Int32     SM_CMONITORS                    = 80                        ;
        const Int32     SM_SAMEDISPLAYFORMAT            = 81                        ;
        const Int32     SM_IMMENABLED                   = 82                        ;
        const Int32     SM_CXFOCUSBORDER                = 83                        ;
        const Int32     SM_CYFOCUSBORDER                = 84                        ;
        const Int32     SM_TABLETPC                     = 86                        ;
        const Int32     SM_MEDIACENTER                  = 87                        ;
        const Int32     SM_STARTER                      = 88                        ;
        const Int32     SM_SERVERR2                     = 89                        ;
        const Int32     SM_MOUSEHORIZONTALWHEELPRESENT  = 91                        ;
        const Int32     SM_CXPADDEDBORDER               = 92                        ;
        const Int32     SM_DIGITIZER                    = 94                        ;
        const Int32     SM_MAXIMUMTOUCHES               = 95                        ;
        //const Int32     SM_CMETRICS                     = 76                        ;
        //const Int32     SM_CMETRICS                     = 83                        ;
        //const Int32     SM_CMETRICS                     = 91                        ;
        //const Int32     SM_CMETRICS                     = 93                        ;
        //const Int32     SM_CMETRICS                     = 97                        ;
        const Int32     SM_REMOTESESSION                = 0x1000                    ;
        const Int32     SM_SHUTTINGDOWN                 = 0x2000                    ;
        const Int32     SM_REMOTECONTROL                = 0x2001                    ;
        const Int32     SM_CARETBLINKINGENABLED         = 0x2002                    ;
        const Int32     SM_CONVERTIBLESLATEMODE         = 0x2003                    ;
        const Int32     SM_SYSTEMDOCKED                 = 0x2004                    ;


        [DllImport("user32.dll")]
        static extern Int32 GetSystemMetrics(Int32 nIndex);

        [DllImport("user32.dll")]
        static extern UInt32 SendInput(
            int nInputs, 
            [MarshalAs(UnmanagedType.LPArray), In] INPUT[] pInputs, 
            int cbSize);


        static IEnumerable<KEYBDINPUT> CharInput (char ch)
        {
            yield return new KEYBDINPUT 
            {
                wVk     = 0                                 ,
                wScan   = ch                                ,
                dwFlags = KEYEVENTF_UNICODE                 ,
                time    = 0                                 ,
            };
            yield return new KEYBDINPUT 
            {
                wVk     = 0                                 ,
                wScan   = ch                                ,
                dwFlags = KEYEVENTF_UNICODE|KEYEVENTF_KEYUP ,
                time    = 0                                 ,
            };
        }

        static IEnumerable<MOUSEINPUT> MouseLeftClick (int x, int y)
        {
            yield return LeftDown(x, y);
            yield return LeftUp (x, y);
        }

        static IEnumerable<MOUSEINPUT> MouseLeftClickAndHold (int x, int y)
        {
            yield return LeftDown(x, y);
        }

        static IEnumerable<MOUSEINPUT> MouseLeftRelease (int x, int y)
        {
            yield return LeftUp(x, y);
        }

        static IEnumerable<MOUSEINPUT> MouseMoveTo (int x, int y)
        {
            yield return MoveTo(x, y);
        }

        const uint DefaultMouseFlags = MOUSEEVENTF_ABSOLUTE|MOUSEEVENTF_VIRTUALDESK|MOUSEEVENTF_MOVE_NOCOALESCE|MOUSEEVENTF_MOVE;

        static MOUSEINPUT MoveTo(int x, int y)
        {
            return new MOUSEINPUT
            {
                dx          = x                                         ,
                dy          = y                                         ,
                mouseData   = 0                                         ,
                dwFlags     = DefaultMouseFlags                         ,
                time        = 0                                         ,
            };
        }

        static MOUSEINPUT LeftDown(int x, int y)
        {
            return new MOUSEINPUT
            {
                dx          = x                                         ,
                dy          = y                                         ,
                mouseData   = 0                                         ,
                dwFlags     = DefaultMouseFlags|MOUSEEVENTF_LEFTDOWN    ,
                time        = 0                                         ,
            };
        }

        static MOUSEINPUT LeftUp(int x, int y)
        {
            return new MOUSEINPUT
            {
                dx          = x                                         ,
                dy          = y                                         ,
                mouseData   = 0                                         ,
                dwFlags     = DefaultMouseFlags|MOUSEEVENTF_LEFTUP      ,
                time        = 0                                         ,
            };
        }

        static INPUT ToInput (KEYBDINPUT input)
        {
            return new INPUT 
            {
                type    = INPUT_KEYBOARD                    ,
                ki      = input                             ,
            };
        }

        static INPUT ToInput (MOUSEINPUT input)
        {
            return new INPUT 
            {
                type    = INPUT_MOUSE                       ,
                mi      = input                             ,
            };
        }

        public static bool SendString (string s)
        {
            if (string.IsNullOrEmpty (s))
            {
                return true;
            }

            var inputs = s.SelectMany(CharInput).Select(ToInput).ToArray();

            var received = SendInput(inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));

            return received == inputs.Length;
        }

        public static bool SendClick (int x, int y)
        {
            var inputs = MouseLeftClick(x,y).Select(ToInput).ToArray();

            var received = SendInput(inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));

            return received == inputs.Length;
        }

        public static bool SendClickAndHold(int x, int y)
        {
            var inputs = MouseLeftClickAndHold(x,y).Select(ToInput).ToArray();

            var received = SendInput(inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));

            return received == inputs.Length;
        }

        public static bool SendMoveTo(int x, int y)
        {
            var inputs = MouseMoveTo(x,y).Select(ToInput).ToArray();

            var received = SendInput(inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));

            return received == inputs.Length;
        }

        public static bool SendReleaseLeft(int x, int y)
        {
            var inputs = MouseLeftRelease(x,y).Select(ToInput).ToArray();

            var received = SendInput(inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));

            return received == inputs.Length;
        }

        public static Rect GetVirtualDesktopDimension()
        {
            return new Rect(
                GetSystemMetrics(SM_XVIRTUALSCREEN),
                GetSystemMetrics(SM_YVIRTUALSCREEN),
                GetSystemMetrics(SM_CXVIRTUALSCREEN),
                GetSystemMetrics(SM_CYVIRTUALSCREEN)
                );
        }
    }
}