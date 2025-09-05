<?php
/**
 * Debug script to test simulation validation for specific backtest
 */

echo "=== DEBUG: Backtest Simulation Validation ===\n";

$simulation_id = 'backtest_20250903_145040_bef7f054';
echo "Testing simulation_id: '{$simulation_id}'\n";

// Simulate the getAvailableSimulations logic
$backend_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';
$simulations = [];

// Add main portfolio
$main_portfolio_path = $backend_path . '/Myportolio';
if (is_dir($main_portfolio_path)) {
    $config_file = $main_portfolio_path . '/config.json';
    $config_data = [];
    if (file_exists($config_file)) {
        $config_data = json_decode(file_get_contents($config_file), TRUE) ?: [];
    }
    
    $simulations['Myportolio'] = [
        'id' => 'Myportolio',
        'name' => $config_data['portfolio_name'] ?? 'Myportolio (Live)',
        'description' => $config_data['description'] ?? 'Live portfolio with real-time data',
        'status' => 'active',
        'type' => 'live',
        'last_updated' => file_exists($config_file) ? filemtime($config_file) : time(),
        'path' => $main_portfolio_path
    ];
    echo "Added Myportolio simulation\n";
}

// Add backtest simulations
$simulations_path = $backend_path . '/Myportolio/simulations/backtests';
echo "Checking backtests path: $simulations_path\n";

if (is_dir($simulations_path)) {
    $backtest_dirs = scandir($simulations_path);
    echo "Found directories: " . implode(', ', array_filter($backtest_dirs, function($d) { return $d !== '.' && $d !== '..'; })) . "\n";
    
    foreach ($backtest_dirs as $dir) {
        if ($dir !== '.' && $dir !== '..' && is_dir($simulations_path . '/' . $dir)) {
            $results_file = $simulations_path . '/' . $dir . '/myportolio_results.json';
            echo "Checking results file: $results_file\n";
            echo "File exists: " . (file_exists($results_file) ? 'YES' : 'NO') . "\n";
            
            if (file_exists($results_file)) {
                echo "Adding backtest: {$dir}\n";
                $results_data = json_decode(file_get_contents($results_file), TRUE) ?: [];
                
                $simulations[$dir] = [
                    'id' => $dir,
                    'name' => "Backtest " . substr($dir, -8),
                    'description' => "Backtest simulation - " . date('M j, Y', strtotime(substr($dir, 9, 8))),
                    'status' => 'completed',
                    'type' => 'backtest',
                    'last_updated' => filemtime($results_file),
                    'path' => $simulations_path . '/' . $dir
                ];
            } else {
                echo "Skipping $dir - no results file\n";
            }
        }
    }
} else {
    echo "Backtests directory does not exist\n";
}

echo "\nTotal simulations found: " . count($simulations) . "\n";
echo "Available simulation IDs: " . implode(', ', array_keys($simulations)) . "\n";

// Test if the specific simulation is valid
$is_valid = isset($simulations[$simulation_id]);
echo "\nIs '{$simulation_id}' valid: " . ($is_valid ? 'YES' : 'NO') . "\n";

if ($is_valid) {
    echo "Simulation details:\n";
    echo "  Name: " . $simulations[$simulation_id]['name'] . "\n";
    echo "  Type: " . $simulations[$simulation_id]['type'] . "\n";
    echo "  Status: " . $simulations[$simulation_id]['status'] . "\n";
    echo "  Path: " . $simulations[$simulation_id]['path'] . "\n";
} else {
    echo "Simulation not found in available list!\n";
}

?>
