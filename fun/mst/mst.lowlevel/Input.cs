using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;

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
        const UInt32    KEYEVENTF_SCANCODE              = 0x0008    ;
        const UInt32    KEYEVENTF_UNICODE               = 0x0004    ;
 
        const UInt32    MOUSEEVENTF_ABSOLUTE            = 0x8000    ;
        const UInt32    MOUSEEVENTF_HWHEEL              = 0x01000   ;
        const UInt32    MOUSEEVENTF_MOVE                = 0x0001    ;
        const UInt32    MOUSEEVENTF_MOVE_NOCOALESCE     = 0x2000    ;
        const UInt32    MOUSEEVENTF_LEFTDOWN            = 0x0002    ;
        const UInt32    MOUSEEVENTF_LEFTUP              = 0x0004    ;
        const UInt32    MOUSEEVENTF_RIGHTDOWN           = 0x0008    ;
        const UInt32    MOUSEEVENTF_RIGHTUP             = 0x0010    ;
        const UInt32    MOUSEEVENTF_MIDDLEDOWN          = 0x0020    ;
        const UInt32    MOUSEEVENTF_MIDDLEUP            = 0x0040    ;
        const UInt32    MOUSEEVENTF_VIRTUALDESK         = 0x4000    ;
        const UInt32    MOUSEEVENTF_WHEEL               = 0x0800    ;
        const UInt32    MOUSEEVENTF_XDOWN               = 0x0080    ;
        const UInt32    MOUSEEVENTF_XUP                 = 0x0100    ;


        /// <summary>
        /// Synthesizes keystrokes, mouse motions, and button clicks.
        /// </summary>
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

        static MOUSEINPUT MoveTo(int x, int y)
        {
            return new MOUSEINPUT
            {
                dx          = x                                         ,
                dy          = y                                         ,
                mouseData   = 0                                         ,
                dwFlags     = MOUSEEVENTF_ABSOLUTE|MOUSEEVENTF_MOVE     ,
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
                dwFlags     = MOUSEEVENTF_ABSOLUTE|MOUSEEVENTF_LEFTDOWN ,
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
                dwFlags     = MOUSEEVENTF_ABSOLUTE|MOUSEEVENTF_LEFTUP   ,
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
    }
}