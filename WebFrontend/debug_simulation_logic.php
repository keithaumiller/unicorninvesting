<?php
/**
 * Debug script to see what happens with simulation selection
 */

// Debug the simulation selection logic
echo "=== DEBUG: Simulation Selection Logic ===\n";

$simulation_id = 'Myportolio';
echo "Input simulation_id: '{$simulation_id}'\n";

// Test the string comparison logic
$is_myportolio = ($simulation_id === 'Myportolio');
$is_backtest = strpos($simulation_id, 'backtest_') === 0;

echo "Is Myportolio: " . ($is_myportolio ? 'YES' : 'NO') . "\n";
echo "Is backtest: " . ($is_backtest ? 'YES' : 'NO') . "\n";
echo "Should use live portfolio: " . (!$is_backtest ? 'YES' : 'NO') . "\n";

// Check the condition used in getPortfolioConfig
$should_use_backtest = ($simulation_id !== 'Myportolio' && strpos($simulation_id, 'backtest_') === 0);
echo "Should use backtest method: " . ($should_use_backtest ? 'YES' : 'NO') . "\n";

echo "\n=== File Path Testing ===\n";
$backend_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';
$config_file = $backend_path . '/Myportolio/config.json';
echo "Config file path: {$config_file}\n";
echo "File exists: " . (file_exists($config_file) ? 'YES' : 'NO') . "\n";

if (file_exists($config_file)) {
    $config_data = json_decode(file_get_contents($config_file), TRUE);
    echo "Config data loaded: " . ($config_data ? 'YES' : 'NO') . "\n";
    if ($config_data) {
        echo "Portfolio name from config: " . ($config_data['portfolio_name'] ?? 'NOT SET') . "\n";
        echo "Strategy type from config: " . ($config_data['strategy_type'] ?? 'NOT SET') . "\n";
    }
}

echo "\n=== Available Simulations Test ===\n";
// Simulate the available simulations logic
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
        'path' => $main_portfolio_path
    ];
    echo "Added Myportolio simulation\n";
}

// Check backtest simulations
$simulations_path = $backend_path . '/Myportolio/simulations/backtests';
if (is_dir($simulations_path)) {
    $backtest_dirs = scandir($simulations_path);
    foreach ($backtest_dirs as $dir) {
        if ($dir !== '.' && $dir !== '..' && is_dir($simulations_path . '/' . $dir)) {
            $results_file = $simulations_path . '/' . $dir . '/myportolio_results.json';
            if (file_exists($results_file)) {
                echo "Found backtest: {$dir}\n";
                $simulations[$dir] = [
                    'id' => $dir,
                    'type' => 'backtest',
                    'path' => $simulations_path . '/' . $dir
                ];
            }
        }
    }
}

echo "Total simulations found: " . count($simulations) . "\n";
echo "Myportolio is valid: " . (isset($simulations['Myportolio']) ? 'YES' : 'NO') . "\n";

// Test what we get when we request Myportolio
echo "\n=== What happens when we request Myportolio config ===\n";
$test_simulation_id = 'Myportolio';
$should_use_backtest = ($test_simulation_id !== 'Myportolio' && strpos($test_simulation_id, 'backtest_') === 0);
echo "For '{$test_simulation_id}', should use backtest: " . ($should_use_backtest ? 'YES' : 'NO') . "\n";

if (!$should_use_backtest) {
    $config_file = $backend_path . '/Myportolio/config.json';
    if (file_exists($config_file)) {
        $config_data = json_decode(file_get_contents($config_file), TRUE);
        if ($config_data) {
            echo "Live portfolio config loaded successfully\n";
            echo "Portfolio name: " . ($config_data['portfolio_name'] ?? 'NOT SET') . "\n";
        } else {
            echo "Failed to decode config JSON\n";
        }
    } else {
        echo "Config file does not exist\n";
    }
}

?>
