<?php
/**
 * Debug what's happening in the dashboard request
 */

// Simulate the dashboard controller logic
echo "=== DEBUG: Dashboard Controller Logic ===\n";

// Simulate getting request parameter
$url_param = $_GET['simulation'] ?? null;
echo "URL parameter 'simulation': " . ($url_param ?? 'NULL') . "\n";

$current_simulation_id = $url_param ?? 'Myportolio';
echo "Current simulation ID after default: '{$current_simulation_id}'\n";

// Include the PortfolioApiService simulation
include_once '/workspaces/unicorninvesting/WebFrontend/web/modules/custom/unicornmetrics/src/Service/PortfolioApiService.php';

// Test simulation validation
echo "\n=== Testing Available Simulations ===\n";

$backend_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';
$simulations = [];

// Add main portfolio (simulate getAvailableSimulations logic)
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
}

// Add backtest simulations
$simulations_path = $backend_path . '/Myportolio/simulations/backtests';
if (is_dir($simulations_path)) {
    $backtest_dirs = scandir($simulations_path);
    foreach ($backtest_dirs as $dir) {
        if ($dir !== '.' && $dir !== '..' && is_dir($simulations_path . '/' . $dir)) {
            $results_file = $simulations_path . '/' . $dir . '/myportolio_results.json';
            if (file_exists($results_file)) {
                $results_data = json_decode(file_get_contents($results_file), TRUE) ?: [];
                
                $simulations[$dir] = [
                    'id' => $dir,
                    'name' => "Backtest " . substr($dir, -8), // Last 8 chars of hash
                    'description' => "Backtest simulation - " . date('M j, Y', strtotime(substr($dir, 9, 8))),
                    'status' => 'completed',
                    'type' => 'backtest',
                    'last_updated' => filemtime($results_file),
                    'path' => $simulations_path . '/' . $dir
                ];
            }
        }
    }
}

echo "Available simulations:\n";
foreach ($simulations as $id => $sim) {
    echo "  - {$id}: {$sim['name']} ({$sim['type']})\n";
}

// Test if current simulation is valid
$is_valid = isset($simulations[$current_simulation_id]);
echo "\nIs '{$current_simulation_id}' valid: " . ($is_valid ? 'YES' : 'NO') . "\n";

if (!$is_valid) {
    echo "Would fallback to: 'Myportolio'\n";
    $current_simulation_id = 'Myportolio';
}

echo "\nFinal simulation ID: '{$current_simulation_id}'\n";

// Test config retrieval
echo "\n=== Testing Config Retrieval ===\n";
if ($current_simulation_id !== 'Myportolio' && strpos($current_simulation_id, 'backtest_') === 0) {
    echo "Would use getBacktestConfig() method\n";
} else {
    echo "Would use live portfolio config\n";
    $config_file = $backend_path . '/Myportolio/config.json';
    if (file_exists($config_file)) {
        $config_data = json_decode(file_get_contents($config_file), TRUE);
        if ($config_data) {
            echo "Live config loaded:\n";
            echo "  - portfolio_name: " . ($config_data['portfolio_name'] ?? 'NOT SET') . "\n";
            echo "  - strategy_type: " . ($config_data['strategy_type'] ?? 'NOT SET') . "\n";
            echo "  - description: " . ($config_data['description'] ?? 'NOT SET') . "\n";
        }
    }
}

?>
