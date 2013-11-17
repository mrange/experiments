using System.Windows;

namespace SimpleGUI
{
    public partial class MainWindow
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        void Click_Ok (object sender, RoutedEventArgs e)
        {
            Input.Text = "OK";
        }

        void Click_Cancel (object sender, RoutedEventArgs e)
        {
            Input.Text = "Cancel";
        }

    }
}
