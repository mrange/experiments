namespace mst.lowlevel
{
    public static class Keyboard
    {
        public static bool Send (string s)
        {
            return Input.SendString (s);
        }
    }
}
