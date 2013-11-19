using System;
using System.Windows;
using System.Windows.Media;

// ReSharper disable UnusedMember.Local

namespace mst.lowlevel
{
    public static class Mouse
    {
        static readonly Lazy<Matrix> s_virtualDesktopDimension = new Lazy<Matrix> (GetVirtualDesktopTransform);

        static int ToInt (this double v)
        {
            return (int) Math.Round(v);
        }

        static Matrix GetVirtualDesktopTransform()
        {
            var dim = Input.GetVirtualDesktopDimension();

            var transform = Matrix.Identity;

            transform.Translate (-dim.X, dim.Y);
            transform.Scale (0xFFFF/dim.Width, 0xFFFF/dim.Height);

            return transform;
        }

        static Tuple<int, int> TransformToMousePlane (int x, int y)
        {
            var result = s_virtualDesktopDimension.Value.Transform(new Point(x, y));
            return Tuple.Create (result.X.ToInt(), result.Y.ToInt());
        }

        public static bool LeftClick (int x, int y)
        {
            var p = TransformToMousePlane(x, y);
            return Input.SendClick(p.Item1,p.Item2);
        }

        public static bool LeftClickAndHold (int x, int y)
        {
            var p = TransformToMousePlane(x, y);
            return Input.SendClickAndHold(p.Item1,p.Item2);
        }

        public static bool MoveTo (int x, int y)
        {
            var p = TransformToMousePlane(x, y);
            return Input.SendMoveTo(p.Item1,p.Item2);
        }

        public static bool ReleaseLeft (int x, int y)
        {
            var p = TransformToMousePlane(x, y);
            return Input.SendReleaseLeft(p.Item1,p.Item2);
        }
    }
}