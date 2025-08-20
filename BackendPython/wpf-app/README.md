# WPF Application (Legacy)

Windows Presentation Foundation desktop application for portfolio management and trading interface.

## Purpose
- Desktop user interface for portfolio management
- Real-time trading dashboard and controls
- Legacy Windows-based client application
- **Status**: Being deprecated in favor of web-based Drupal interface

## Project Structure

### WpfApp.csproj
**Purpose**: Visual Studio project file for WPF application
**Framework**: .NET Framework with WPF UI components
**Dependencies**: Windows-specific desktop application framework

### src/ Directory Structure

#### Application Entry Point
- `App.xaml` - WPF application definition and global resources
- `App.xaml.cs` - Application startup logic and configuration

#### Main Interface
- `MainWindow.xaml` - Primary window XAML layout and controls
- `MainWindow.xaml.cs` - Main window code-behind and event handlers

#### Architecture Components
- `Models/DataModel.cs` - Data models for portfolio and market data representation
- `Services/DataService.cs` - Business logic and data access services  
- `ViewModels/MainViewModel.cs` - MVVM pattern view model for UI binding
- `Views/UserControls` - Custom user controls for specialized UI components

## Functionality
- Portfolio visualization and management
- Real-time market data display
- Trading order placement and management
- Performance analytics and charting
- Integration with backend R analytics engine

## Migration Notes
- Desktop functionality being migrated to Drupal 11 web interface
- Data models will be converted to Python/SQLAlchemy equivalents
- Business logic being refactored into Python FastAPI services
- UI components being redesigned as responsive web components

## Dependencies
- .NET Framework (Windows-only)
- WPF UI framework
- Integration with R backend analytics
- MySQL database connectivity for portfolio data## Overview
This WPF application demonstrates the Model-View-ViewModel (MVVM) design pattern. It provides a structured approach to building user interfaces in WPF, promoting separation of concerns and enhancing maintainability.

## Getting Started
To run the application, ensure you have the .NET SDK installed. Clone the repository and open the project in your preferred IDE. Build and run the application using the following commands:

```bash
git clone <repository-url>
cd wpf-app
dotnet build
dotnet run
```

## Features
- MVVM architecture for clear separation of concerns
- Data binding for dynamic UI updates
- User controls for reusable UI components

## Usage
Instructions on how to use the application will be provided here. Users can interact with the main window to explore the functionalities offered by the application.