<?php
// Debug script to test simulation validation logic

$backendPath = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';

echo "=== DEBUGGING SIMULATION VALIDATION ===\n";

// Test the actual logic from PortfolioApiService
function getAvailableSimulations($backendPath) {
    $simulations = [];
    
    // Add live portfolios
    if (is_dir($backendPath)) {
        $directories = scandir($backendPath);
        
        foreach ($directories as $dir) {
            $full_path = $backendPath . '/' . $dir;
            
            if ($dir === '.' || $dir === '..' || $dir === 'utilities' || !is_dir($full_path)) {
                continue;
            }
            
            $config_file = $full_path . '/config.json';
            if (file_exists($config_file)) {
                $config_data = json_decode(file_get_contents($config_file), true);
                $simulations[$dir] = [
                    'id' => $dir,
                    'name' => isset($config_data['portfolio_name']) ? $config_data['portfolio_name'] : $dir,
                    'description' => isset($config_data['description']) ? $config_data['description'] : 'No description available',
                    'type' => 'live',
                    'status' => 'active',
                    'last_updated' => filemtime($config_file),
                    'path' => $full_path
                ];
            }
        }
    }
    
    // Add backtest simulations
    $myportolio_path = $backendPath . '/Myportolio';
    $backtests_path = $myportolio_path . '/simulations/backtests';
    
    if (is_dir($backtests_path)) {
        $backtest_dirs = scandir($backtests_path);
        
        foreach ($backtest_dirs as $backtest_dir) {
            if ($backtest_dir === '.' || $backtest_dir === '..') {
                continue;
            }
            
            $backtest_path = $backtests_path . '/' . $backtest_dir;
            
            if (is_dir($backtest_path) && strpos($backtest_dir, 'backtest_') === 0) {
                $lean_config_file = $backtest_path . '/lean_config.json';
                $results_file = $backtest_path . '/myportolio_results.json';
                
                if (file_exists($lean_config_file)) {
                    $display_name = 'Backtest: ' . $backtest_dir;
                    
                    if (preg_match('/backtest_(\d{8})_/', $backtest_dir, $matches)) {
                        $date_str = $matches[1];
                        $formatted_date = substr($date_str, 0, 4) . '-' . substr($date_str, 4, 2) . '-' . substr($date_str, 6, 2);
                        $display_name = 'Backtest: ' . $formatted_date;
                    }
                    
                    $simulations[$backtest_dir] = [
                        'id' => $backtest_dir,
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
    
    return $simulations;
}

function isValidSimulation($simulation_id, $simulations) {
    return isset($simulations[$simulation_id]);
}

// Test simulation detection
$simulations = getAvailableSimulations($backendPath);
echo "Found " . count($simulations) . " simulations:\n";

foreach ($simulations as $id => $data) {
    echo "- ID: $id\n";
    echo "  Name: " . $data['name'] . "\n";
    echo "  Type: " . $data['type'] . "\n";
    echo "\n";
}

// Test specific simulation IDs
$test_ids = [
    'Myportolio',
    'backtest_20250903_142955_5618caf5',
    'invalid_simulation'
];

echo "\n=== VALIDATION TESTS ===\n";
foreach ($test_ids as $test_id) {
    $is_valid = isValidSimulation($test_id, $simulations);
    echo "ID: $test_id -> " . ($is_valid ? "VALID" : "INVALID") . "\n";
}

echo "\n=== EXPECTED DROPDOWN BEHAVIOR ===\n";
$current_simulation_id = 'backtest_20250903_142955_5618caf5';
echo "Requested simulation: $current_simulation_id\n";
echo "Is valid: " . (isValidSimulation($current_simulation_id, $simulations) ? "YES" : "NO") . "\n";

if (isValidSimulation($current_simulation_id, $simulations)) {
    echo "Should NOT fallback to Myportolio\n";
    echo "Should show '$current_simulation_id' as selected\n";
} else {
    echo "Should fallback to Myportolio\n";
    echo "Should show 'Myportolio' as selected\n";
}

?>
