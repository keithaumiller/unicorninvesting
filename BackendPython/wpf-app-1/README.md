# WPF Application - Version 1 (Legacy)

Alternative or previous version of the Windows Presentation Foundation desktop application.

## Purpose
- Secondary or backup version of desktop portfolio management interface
- May contain experimental features or alternative UI approaches
- Legacy Windows-based client application
- **Status**: Being deprecated in favor of web-based Drupal interface

## Project Structure
Identical structure to main WPF application:

### WpfApp.csproj
**Purpose**: Visual Studio project file for WPF application variant

### src/ Directory Structure
- `App.xaml` / `App.xaml.cs` - Application definition and startup
- `MainWindow.xaml` / `MainWindow.xaml.cs` - Main window interface
- `Models/DataModel.cs` - Data models
- `Services/DataService.cs` - Business logic services
- `ViewModels/MainViewModel.cs` - MVVM view models
- `Views/UserControls` - Custom UI controls

## Migration Notes
- This variant will also be migrated to Drupal 11 web interface
- May contain features or approaches worth preserving in web migration
- Code review needed to identify unique functionality vs. main WPF app

## Usage
Instructions on how to use the application will be provided here. Users can interact with the main window to explore the functionalities offered by the application.