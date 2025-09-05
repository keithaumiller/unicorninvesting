<?php

// Test script to verify PortfolioApiService simulation detection
// This mimics the PHP logic we implemented

$backendPath = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';

echo "=== SIMULATION DETECTION TEST (PHP) ===\n";

function getAvailableSimulations($backendPath) {
    $simulations = [];
    
    try {
        // First, add live portfolios (directories with config.json)
        if (is_dir($backendPath)) {
            $directories = scandir($backendPath);
            
            foreach ($directories as $dir) {
                $full_path = $backendPath . '/' . $dir;
                
                // Skip non-directories and utilities
                if ($dir === '.' || $dir === '..' || $dir === 'utilities' || !is_dir($full_path)) {
                    continue;
                }
                
                // Check if it has a config.json (valid simulation)
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
        
        // Second, add backtest simulations from Myportolio/simulations/backtests/
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
                    // Check for lean_config.json
                    $lean_config_file = $backtest_path . '/lean_config.json';
                    $results_file = $backtest_path . '/myportolio_results.json';
                    
                    if (file_exists($lean_config_file)) {
                        $display_name = 'Backtest: ' . $backtest_dir;
                        
                        // Try to extract date from backtest directory name
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
        
    } catch (Exception $e) {
        echo "Error: " . $e->getMessage() . "\n";
    }
    
    return $simulations;
}

// Test the function
$simulations = getAvailableSimulations($backendPath);

echo "Found " . count($simulations) . " simulations:\n\n";

foreach ($simulations as $sim_id => $sim_data) {
    echo "ID: " . $sim_id . "\n";
    echo "Name: " . $sim_data['name'] . "\n";
    echo "Type: " . $sim_data['type'] . "\n";
    echo "Status: " . $sim_data['status'] . "\n";
    echo "Description: " . $sim_data['description'] . "\n";
    echo "Last Updated: " . date('Y-m-d H:i:s', $sim_data['last_updated']) . "\n";
    echo "---\n";
}

echo "\n=== TEST COMPLETE ===\n";
