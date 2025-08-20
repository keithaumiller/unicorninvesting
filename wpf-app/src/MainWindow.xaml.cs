using System.Windows;

namespace WpfApp
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            DataContext = new MainViewModel();
        }

        private void SomeButton_Click(object sender, RoutedEventArgs e)
        {
            // Handle button click event
        }
    }
}