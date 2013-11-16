namespace mst.lowlevel
{
    public static class Mouse
    {
        public static bool LeftClick (int x, int y)
        {
            return Input.SendClick(x,y);
        }

        public static bool LeftClickAndHold (int x, int y)
        {
            return Input.SendClickAndHold(x,y);
        }

        public static bool MoveTo (int x, int y)
        {
            return Input.SendMoveTo(x,y);
        }

        public static bool ReleaseLeft (int x, int y)
        {
            return Input.SendReleaseLeft(x,y);
        }
    }
}