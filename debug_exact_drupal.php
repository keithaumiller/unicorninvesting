<?php
// Test exact Drupal PortfolioApiService logic

$backendPath = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';

function calculateBacktestPerformance($results_data) {
    $performance = $results_data['performance'] ?? [];
    
    return [
        'total_return_percent' => ($performance['total_return'] ?? 0) * 100,
        'sharpe_ratio' => $performance['sharpe_ratio'] ?? 0,
        'max_drawdown_percent' => ($performance['max_drawdown'] ?? 0) * 100,
        'trades_count' => $performance['trades_count'] ?? 0
    ];
}

function getAvailableSimulations($backendPath) {
    $simulations = [];
    
    try {
      // First add the main portfolio
      $main_portfolio_path = $backendPath . '/Myportolio';
      if (is_dir($main_portfolio_path)) {
        $config_file = $main_portfolio_path . '/config.json';
        $config_data = [];
        if (file_exists($config_file)) {
          $config_data = json_decode(file_get_contents($config_file), TRUE) ?: [];
        }
        
        $simulations['Myportolio'] = [
          'id' => 'Myportolio',
          'name' => isset($config_data['portfolio_name']) ? $config_data['portfolio_name'] : 'Myportolio (Live)',
          'description' => isset($config_data['description']) ? $config_data['description'] : 'Live portfolio with real-time data',
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
          
          // Check for results file
          $results_file = $backtest_path . '/myportolio_results.json';
          if (file_exists($results_file)) {
            $results_data = json_decode(file_get_contents($results_file), TRUE);
            
            if ($results_data) {
              $simulation_id = isset($results_data['simulation_id']) ? $results_data['simulation_id'] : $dir;
              $timestamp = isset($results_data['timestamp']) ? $results_data['timestamp'] : '';
              $performance = calculateBacktestPerformance($results_data);
              
              $simulations[$simulation_id] = [
                'id' => $simulation_id,
                'name' => 'Backtest ' . substr($simulation_id, -8), // Last 8 chars of ID
                'description' => sprintf('Backtest simulation - %s (%.2f%% return)', 
                                       date('M j, Y', strtotime($timestamp)), 
                                       $performance['total_return_percent']),
                'status' => 'completed',
                'type' => 'backtest',
                'last_updated' => filemtime($results_file),
                'path' => $backtest_path,
                'performance' => $performance
              ];
            }
          }
        }
      }
      
      // Sort by last updated (most recent first)
      uasort($simulations, function($a, $b) {
        return $b['last_updated'] - $a['last_updated'];
      });
      
    } catch (Exception $e) {
      echo "Error getting available simulations: " . $e->getMessage() . "\n";
      
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

echo "=== EXACT DRUPAL SERVICE TEST ===\n";

$simulations = getAvailableSimulations($backendPath);
echo "Simulations found: " . count($simulations) . "\n";

foreach ($simulations as $id => $data) {
    echo "  ID: $id\n";
    echo "  Name: " . $data['name'] . "\n";
    echo "  Type: " . $data['type'] . "\n\n";
}

// Test specific simulation validation
$test_simulation = 'backtest_20250903_142955_5618caf5';
echo "Testing simulation: $test_simulation\n";
echo "Exists: " . (isset($simulations[$test_simulation]) ? "YES" : "NO") . "\n";

// Test dropdown generation
echo "\n=== DROPDOWN TEST ===\n";
$current_simulation_id = $test_simulation;

foreach ($simulations as $sim_id => $sim_data) {
  $selected = ($sim_id === $current_simulation_id) ? 'selected' : '';
  $status_icon = ($sim_data['status'] === 'active') ? '🟢' : '🔴';
  echo '<option value="' . htmlspecialchars($sim_id) . '" ' . $selected . '>' 
       . $status_icon . ' ' . htmlspecialchars($sim_data['name']) . '</option>' . "\n";
}

?>
