namespace mst.lowlevel
{
    public static class Keyboard
    {
        public static bool Send (string s)
        {
            return Input.SendString (s);
        }


        public static bool Send (char ch, Modifier modifier)
        {
            return Input.SendChar (ch, modifier);
        }
    }
}
