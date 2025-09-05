<?php
/**
 * Test the actual PortfolioApiService isValidSimulation method
 */

// Simulate Drupal bootstrap (minimal)
$backend_path = '/workspaces/unicorninvesting/BackendPython/unicorn/4_portfolios';

// Simulate the PortfolioApiService logic
class TestPortfolioApiService {
    private $backendPath;
    
    public function __construct($backend_path) {
        $this->backendPath = $backend_path;
    }
    
    public function getAvailableSimulations(): array {
        $simulations = [];
        
        // Add main portfolio
        $main_portfolio_path = $this->backendPath . '/Myportolio';
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
        $simulations_path = $this->backendPath . '/Myportolio/simulations/backtests';
        if (is_dir($simulations_path)) {
            $backtest_dirs = scandir($simulations_path);
            foreach ($backtest_dirs as $dir) {
                if ($dir !== '.' && $dir !== '..' && is_dir($simulations_path . '/' . $dir)) {
                    $results_file = $simulations_path . '/' . $dir . '/myportolio_results.json';
                    if (file_exists($results_file)) {
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
                    }
                }
            }
        }
        
        return $simulations;
    }
    
    public function isValidSimulation(string $simulation_id): bool {
        $simulations = $this->getAvailableSimulations();
        return isset($simulations[$simulation_id]);
    }
}

echo "=== TESTING PortfolioApiService Methods ===\n";

$service = new TestPortfolioApiService($backend_path);

// Test getAvailableSimulations
$available = $service->getAvailableSimulations();
echo "Available simulations: " . count($available) . "\n";
foreach ($available as $id => $data) {
    echo "  - $id: {$data['name']} ({$data['type']})\n";
}

// Test specific simulation
$test_id = 'backtest_20250903_145040_bef7f054';
echo "\nTesting simulation: '$test_id'\n";
echo "Is valid: " . ($service->isValidSimulation($test_id) ? 'YES' : 'NO') . "\n";

// Test live portfolio
echo "\nTesting simulation: 'Myportolio'\n";
echo "Is valid: " . ($service->isValidSimulation('Myportolio') ? 'YES' : 'NO') . "\n";

?>
