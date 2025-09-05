<?php
// Direct test of Drupal PortfolioApiService methods

// Simulate the actual service path and logic
$backendPath = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';

// Copy the exact getAvailableSimulations logic from PortfolioApiService
function getAvailableSimulations($backendPath) {
    $simulations = [];
    
    try {
        // First, check main portfolio directories for live portfolios
        $main_portfolio_path = $backendPath . '/Myportolio';
        $config_file = $main_portfolio_path . '/config.json';
        
        if (file_exists($config_file)) {
            $config_data = json_decode(file_get_contents($config_file), TRUE);
            $simulations['Myportolio'] = [
                'id' => 'Myportolio',
                'name' => $config_data['portfolio_name'] ?? 'Myportolio',
                'description' => $config_data['description'] ?? 'Default portfolio',
                'status' => 'active',
                'type' => 'live',
                'last_updated' => file_exists($config_file) ? filemtime($config_file) : time(),
                'path' => $main_portfolio_path
            ];
        }
        
        // Then add simulations from backtests
        $simulations_path = $backendPath . '/Myportolio/simulations/backtests';
        if (is_dir($simulations_path)) {
            $backtest_dirs = scandir($simulations_path);
            
            foreach ($backtest_dirs as $dir) {
                if ($dir === '.' || $dir === '..') {
                    continue;
                }
                
                $backtest_path = $simulations_path . '/' . $dir;
                if (!is_dir($backtest_path)) {
                    continue;
                }
                
                // Check if directory starts with 'backtest_' and has lean_config.json
                if (strpos($dir, 'backtest_') === 0) {
                    $lean_config_file = $backtest_path . '/lean_config.json';
                    $results_file = $backtest_path . '/myportolio_results.json';
                    
                    if (file_exists($lean_config_file)) {
                        // Extract date from directory name if possible  
                        $display_name = 'Backtest ' . substr($dir, -8); // Last 8 chars for short ID
                        if (preg_match('/backtest_(\d{8})_/', $dir, $matches)) {
                            $date_str = $matches[1];
                            $formatted_date = substr($date_str, 0, 4) . '-' . substr($date_str, 4, 2) . '-' . substr($date_str, 6, 2);
                            $display_name = 'Backtest: ' . $formatted_date;
                        }
                        
                        $simulations[$dir] = [
                            'id' => $dir,
                            'name' => $display_name,
                            'description' => 'Historical backtest simulation',
                            'type' => 'backtest',
                            'status' => file_exists($results_file) ? 'completed' : 'incomplete',
                            'last_updated' => filemtime($lean_config_file),
                            'path' => $backtest_path
                        ];
                    }
                }
            }
        }
        
    } catch (Exception $e) {
        echo "Error: " . $e->getMessage() . "\n";
        
        // Return default Myportolio on error
        $simulations['Myportolio'] = [
            'id' => 'Myportolio', 
            'name' => 'Myportolio (Default)',
            'description' => 'Default portfolio simulation',
            'status' => 'active',
            'type' => 'live',
            'last_updated' => time(),
            'path' => $backendPath . '/Myportolio'
        ];
    }
    
    return $simulations;
}

// Test the method
echo "=== TESTING DRUPAL SERVICE LOGIC ===\n";

$simulations = getAvailableSimulations($backendPath);
echo "Available simulations: " . count($simulations) . "\n";

foreach ($simulations as $id => $data) {
    echo "  $id: " . $data['name'] . " (" . $data['type'] . ")\n";
}

// Test validation
$test_simulation = 'backtest_20250903_142955_5618caf5';
echo "\nTesting simulation: $test_simulation\n";
echo "Exists in array: " . (isset($simulations[$test_simulation]) ? "YES" : "NO") . "\n";

if (isset($simulations[$test_simulation])) {
    echo "Details: " . json_encode($simulations[$test_simulation], JSON_PRETTY_PRINT) . "\n";
}

echo "\n=== DROPDOWN GENERATION TEST ===\n";
$current_simulation_id = $test_simulation;

foreach ($simulations as $sim_id => $sim_data) {
    $selected = ($sim_id === $current_simulation_id) ? 'selected' : '';
    $status_icon = ($sim_data['status'] === 'active') ? '🟢' : '🔴';
    echo '<option value="' . htmlspecialchars($sim_id) . '" ' . $selected . '>' 
         . $status_icon . ' ' . htmlspecialchars($sim_data['name']) . '</option>' . "\n";
}

?>
