using System;
using System.Threading;

namespace mst.lowlevel.test
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Please focus the application");

            for (var iter = 5; iter > 0; --iter)
            {
                Console.WriteLine("{0}...", iter);
                Thread.Sleep(1000);
            }

            //Keyboard.Send ("Testing");
            Mouse.LeftClickAndHold(200,200);
            Mouse.ReleaseLeft(400,400);
        }
    }
}
