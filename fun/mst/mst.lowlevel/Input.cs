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


        const UInt16    VK_LBUTTON                      = 0x01  ;
        const UInt16    VK_RBUTTON                      = 0x02  ;
        const UInt16    VK_CANCEL                       = 0x03  ;
        const UInt16    VK_MBUTTON                      = 0x04  ;
        const UInt16    VK_XBUTTON1                     = 0x05  ;
        const UInt16    VK_XBUTTON2                     = 0x06  ;
        const UInt16    VK_BACK                         = 0x08  ;
        const UInt16    VK_TAB                          = 0x09  ;
        const UInt16    VK_CLEAR                        = 0x0C  ;
        const UInt16    VK_RETURN                       = 0x0D  ;
        const UInt16    VK_SHIFT                        = 0x10  ;
        const UInt16    VK_CONTROL                      = 0x11  ;
        const UInt16    VK_MENU                         = 0x12  ;
        const UInt16    VK_PAUSE                        = 0x13  ;
        const UInt16    VK_CAPITAL                      = 0x14  ;
        const UInt16    VK_KANA                         = 0x15  ;
        const UInt16    VK_HANGEUL                      = 0x15  ;
        const UInt16    VK_HANGUL                       = 0x15  ;
        const UInt16    VK_JUNJA                        = 0x17  ;
        const UInt16    VK_FINAL                        = 0x18  ;
        const UInt16    VK_HANJA                        = 0x19  ;
        const UInt16    VK_KANJI                        = 0x19  ;
        const UInt16    VK_ESCAPE                       = 0x1B  ;
        const UInt16    VK_CONVERT                      = 0x1C  ;
        const UInt16    VK_NONCONVERT                   = 0x1D  ;
        const UInt16    VK_ACCEPT                       = 0x1E  ;
        const UInt16    VK_MODECHANGE                   = 0x1F  ;
        const UInt16    VK_SPACE                        = 0x20  ;
        const UInt16    VK_PRIOR                        = 0x21  ;
        const UInt16    VK_NEXT                         = 0x22  ;
        const UInt16    VK_END                          = 0x23  ;
        const UInt16    VK_HOME                         = 0x24  ;
        const UInt16    VK_LEFT                         = 0x25  ;
        const UInt16    VK_UP                           = 0x26  ;
        const UInt16    VK_RIGHT                        = 0x27  ;
        const UInt16    VK_DOWN                         = 0x28  ;
        const UInt16    VK_SELECT                       = 0x29  ;
        const UInt16    VK_PRINT                        = 0x2A  ;
        const UInt16    VK_EXECUTE                      = 0x2B  ;
        const UInt16    VK_SNAPSHOT                     = 0x2C  ;
        const UInt16    VK_INSERT                       = 0x2D  ;
        const UInt16    VK_DELETE                       = 0x2E  ;
        const UInt16    VK_HELP                         = 0x2F  ;
        const UInt16    VK_LWIN                         = 0x5B  ;
        const UInt16    VK_RWIN                         = 0x5C  ;
        const UInt16    VK_APPS                         = 0x5D  ;
        const UInt16    VK_SLEEP                        = 0x5F  ;
        const UInt16    VK_NUMPAD0                      = 0x60  ;
        const UInt16    VK_NUMPAD1                      = 0x61  ;
        const UInt16    VK_NUMPAD2                      = 0x62  ;
        const UInt16    VK_NUMPAD3                      = 0x63  ;
        const UInt16    VK_NUMPAD4                      = 0x64  ;
        const UInt16    VK_NUMPAD5                      = 0x65  ;
        const UInt16    VK_NUMPAD6                      = 0x66  ;
        const UInt16    VK_NUMPAD7                      = 0x67  ;
        const UInt16    VK_NUMPAD8                      = 0x68  ;
        const UInt16    VK_NUMPAD9                      = 0x69  ;
        const UInt16    VK_MULTIPLY                     = 0x6A  ;
        const UInt16    VK_ADD                          = 0x6B  ;
        const UInt16    VK_SEPARATOR                    = 0x6C  ;
        const UInt16    VK_SUBTRACT                     = 0x6D  ;
        const UInt16    VK_DECIMAL                      = 0x6E  ;
        const UInt16    VK_DIVIDE                       = 0x6F  ;
        const UInt16    VK_F1                           = 0x70  ;
        const UInt16    VK_F2                           = 0x71  ;
        const UInt16    VK_F3                           = 0x72  ;
        const UInt16    VK_F4                           = 0x73  ;
        const UInt16    VK_F5                           = 0x74  ;
        const UInt16    VK_F6                           = 0x75  ;
        const UInt16    VK_F7                           = 0x76  ;
        const UInt16    VK_F8                           = 0x77  ;
        const UInt16    VK_F9                           = 0x78  ;
        const UInt16    VK_F10                          = 0x79  ;
        const UInt16    VK_F11                          = 0x7A  ;
        const UInt16    VK_F12                          = 0x7B  ;
        const UInt16    VK_F13                          = 0x7C  ;
        const UInt16    VK_F14                          = 0x7D  ;
        const UInt16    VK_F15                          = 0x7E  ;
        const UInt16    VK_F16                          = 0x7F  ;
        const UInt16    VK_F17                          = 0x80  ;
        const UInt16    VK_F18                          = 0x81  ;
        const UInt16    VK_F19                          = 0x82  ;
        const UInt16    VK_F20                          = 0x83  ;
        const UInt16    VK_F21                          = 0x84  ;
        const UInt16    VK_F22                          = 0x85  ;
        const UInt16    VK_F23                          = 0x86  ;
        const UInt16    VK_F24                          = 0x87  ;
        const UInt16    VK_NUMLOCK                      = 0x90  ;
        const UInt16    VK_SCROLL                       = 0x91  ;
        const UInt16    VK_OEM_NEC_EQUAL                = 0x92  ;
        const UInt16    VK_OEM_FJ_JISHO                 = 0x92  ;
        const UInt16    VK_OEM_FJ_MASSHOU               = 0x93  ;
        const UInt16    VK_OEM_FJ_TOUROKU               = 0x94  ;
        const UInt16    VK_OEM_FJ_LOYA                  = 0x95  ;
        const UInt16    VK_OEM_FJ_ROYA                  = 0x96  ;
        const UInt16    VK_LSHIFT                       = 0xA0  ;
        const UInt16    VK_RSHIFT                       = 0xA1  ;
        const UInt16    VK_LCONTROL                     = 0xA2  ;
        const UInt16    VK_RCONTROL                     = 0xA3  ;
        const UInt16    VK_LMENU                        = 0xA4  ;
        const UInt16    VK_RMENU                        = 0xA5  ;
        const UInt16    VK_BROWSER_BACK                 = 0xA6  ;
        const UInt16    VK_BROWSER_FORWARD              = 0xA7  ;
        const UInt16    VK_BROWSER_REFRESH              = 0xA8  ;
        const UInt16    VK_BROWSER_STOP                 = 0xA9  ;
        const UInt16    VK_BROWSER_SEARCH               = 0xAA  ;
        const UInt16    VK_BROWSER_FAVORITES            = 0xAB  ;
        const UInt16    VK_BROWSER_HOME                 = 0xAC  ;
        const UInt16    VK_VOLUME_MUTE                  = 0xAD  ;
        const UInt16    VK_VOLUME_DOWN                  = 0xAE  ;
        const UInt16    VK_VOLUME_UP                    = 0xAF  ;
        const UInt16    VK_MEDIA_NEXT_TRACK             = 0xB0  ;
        const UInt16    VK_MEDIA_PREV_TRACK             = 0xB1  ;
        const UInt16    VK_MEDIA_STOP                   = 0xB2  ;
        const UInt16    VK_MEDIA_PLAY_PAUSE             = 0xB3  ;
        const UInt16    VK_LAUNCH_MAIL                  = 0xB4  ;
        const UInt16    VK_LAUNCH_MEDIA_SELECT          = 0xB5  ;
        const UInt16    VK_LAUNCH_APP1                  = 0xB6  ;
        const UInt16    VK_LAUNCH_APP2                  = 0xB7  ;
        const UInt16    VK_OEM_1                        = 0xBA  ;
        const UInt16    VK_OEM_PLUS                     = 0xBB  ;
        const UInt16    VK_OEM_COMMA                    = 0xBC  ;
        const UInt16    VK_OEM_MINUS                    = 0xBD  ;
        const UInt16    VK_OEM_PERIOD                   = 0xBE  ;
        const UInt16    VK_OEM_2                        = 0xBF  ;
        const UInt16    VK_OEM_3                        = 0xC0  ;
        const UInt16    VK_OEM_4                        = 0xDB  ;
        const UInt16    VK_OEM_5                        = 0xDC  ;
        const UInt16    VK_OEM_6                        = 0xDD  ;
        const UInt16    VK_OEM_7                        = 0xDE  ;
        const UInt16    VK_OEM_8                        = 0xDF  ;
        const UInt16    VK_OEM_AX                       = 0xE1  ;
        const UInt16    VK_OEM_102                      = 0xE2  ;
        const UInt16    VK_ICO_HELP                     = 0xE3  ;
        const UInt16    VK_ICO_00                       = 0xE4  ;
        const UInt16    VK_PROCESSKEY                   = 0xE5  ;
        const UInt16    VK_ICO_CLEAR                    = 0xE6  ;
        const UInt16    VK_PACKET                       = 0xE7  ;
        const UInt16    VK_OEM_RESET                    = 0xE9  ;
        const UInt16    VK_OEM_JUMP                     = 0xEA  ;
        const UInt16    VK_OEM_PA1                      = 0xEB  ;
        const UInt16    VK_OEM_PA2                      = 0xEC  ;
        const UInt16    VK_OEM_PA3                      = 0xED  ;
        const UInt16    VK_OEM_WSCTRL                   = 0xEE  ;
        const UInt16    VK_OEM_CUSEL                    = 0xEF  ;
        const UInt16    VK_OEM_ATTN                     = 0xF0  ;
        const UInt16    VK_OEM_FINISH                   = 0xF1  ;
        const UInt16    VK_OEM_COPY                     = 0xF2  ;
        const UInt16    VK_OEM_AUTO                     = 0xF3  ;
        const UInt16    VK_OEM_ENLW                     = 0xF4  ;
        const UInt16    VK_OEM_BACKTAB                  = 0xF5  ;
        const UInt16    VK_ATTN                         = 0xF6  ;
        const UInt16    VK_CRSEL                        = 0xF7  ;
        const UInt16    VK_EXSEL                        = 0xF8  ;
        const UInt16    VK_EREOF                        = 0xF9  ;
        const UInt16    VK_PLAY                         = 0xFA  ;
        const UInt16    VK_ZOOM                         = 0xFB  ;
        const UInt16    VK_NONAME                       = 0xFC  ;
        const UInt16    VK_PA1                          = 0xFD  ;
        const UInt16    VK_OEM_CLEAR                    = 0xFE  ;

        const UInt32    MAPVK_VK_TO_VSC                 = 0     ;
        const UInt32    MAPVK_VSC_TO_VK                 = 1     ;
        const UInt32    MAPVK_VK_TO_CHAR                = 2     ;
        const UInt32    MAPVK_VSC_TO_VK_EX              = 3     ;
        const UInt32    MAPVK_VK_TO_VSC_EX              = 4     ;

        [DllImport("user32.dll")]
        static extern Int32 GetSystemMetrics(Int32 nIndex);

        [DllImport("user32.dll")]
        static extern UInt32 SendInput(
            int nInputs, 
            [MarshalAs(UnmanagedType.LPArray), In] INPUT[] pInputs, 
            int cbSize);

        [DllImport("user32.dll")]
        static extern UInt32 MapVirtualKey(
          UInt32 uCode      ,
          UInt32 uMapType
        );

        static UInt16 GetVirtualKey (Modifier modifier)
        {
            switch (modifier)
            {
                case Modifier.LeftControl:
                    return VK_LCONTROL;
                case Modifier.RightControl:
                    return VK_RCONTROL;
                case Modifier.LeftShift:
                    return VK_LSHIFT;
                case Modifier.RightShift:
                    return VK_RSHIFT;
                case Modifier.LeftAlt:
                    return VK_LMENU;
                case Modifier.RightAlt:
                    return VK_RMENU;
                default:
                    return 0;
            }
        }

        static UInt16 GetScanCode (Modifier modifier)
        {
            return (ushort) MapVirtualKey(GetVirtualKey(modifier), MAPVK_VK_TO_VSC);
        }

        static KEYBDINPUT VirtualKeyDown(Modifier modifier)
        {
            return new KEYBDINPUT 
            {
                wVk     = 0                                 ,
                wScan   = GetScanCode (modifier)            ,
                dwFlags = KEYEVENTF_SCANCODE                ,
                time    = 0                                 ,
            };
        }

        static KEYBDINPUT VirtualKeyUp(Modifier modifier)
        {
            return new KEYBDINPUT 
            {
                wVk     = 0                                 ,
                wScan   = GetScanCode (modifier)            ,
                dwFlags = KEYEVENTF_SCANCODE|KEYEVENTF_KEYUP,
                time    = 0                                 ,
            };
        }

        static KEYBDINPUT KeyDown(char ch)
        {
            return new KEYBDINPUT
            {
                wVk     = 0                                 ,
                wScan   = ch                                ,
                dwFlags = KEYEVENTF_UNICODE                 ,
                time    = 0                                 ,
            };
        }

        static KEYBDINPUT KeyUp(char ch)
        {
            return new KEYBDINPUT
            {
                wVk     = 0                                 ,
                wScan   = ch                                ,
                dwFlags = KEYEVENTF_UNICODE|KEYEVENTF_KEYUP ,
                time    = 0                                 ,
            };
        }

        static IEnumerable<KEYBDINPUT> KeyboardInput (char ch, Modifier modifier)
        {
            yield return VirtualKeyDown (modifier);
            yield return KeyDown(ch);
            yield return KeyUp(ch);
            yield return VirtualKeyUp (modifier);
        }

        static IEnumerable<KEYBDINPUT> KeyboardInput (char ch)
        {
            yield return KeyDown(ch);
            yield return KeyUp(ch);
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

            var inputs = s.SelectMany(KeyboardInput).Select(ToInput).ToArray();

            var received = SendInput(inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));

            return received == inputs.Length;
        }

        public static bool SendChar (char ch, Modifier modifier)
        {
            var inputs = KeyboardInput(ch, modifier).Select(ToInput).ToArray();            
        
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