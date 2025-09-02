<?php
/**
 * Test Management Dashboard
 * Standalone interface for running and monitoring tests
 */

// Set error reporting for debugging
error_reporting(E_ALL);
ini_set('display_errors', 1);

// Project paths
define('PROJECT_ROOT', dirname(__DIR__));
define('TESTS_DIR', PROJECT_ROOT . '/tests');
define('LOG_DIR', '/tmp');

/**
 * Execute a test and return the result
 */
function executeTest($testName, $testScript) {
    $command = "cd " . PROJECT_ROOT . " && " . $testScript;
    $output = [];
    $returnCode = 0;
    
    // Execute the test
    exec($command . " 2>&1", $output, $returnCode);
    
    return [
        'name' => $testName,
        'success' => $returnCode === 0,
        'returnCode' => $returnCode,
        'output' => implode("\n", $output),
        'timestamp' => date('Y-m-d H:i:s'),
        'duration' => 0 // Will be calculated in real implementation
    ];
}

/**
 * Get available tests
 */
function getAvailableTests() {
    return [
        'comprehensive' => [
            'name' => 'Comprehensive Test Suite',
            'script' => './tests/run_comprehensive_tests.sh',
            'description' => 'Run all tests with detailed reporting',
            'category' => 'full'
        ],
        'comprehensive_quick' => [
            'name' => 'Quick Test Suite',
            'script' => './tests/run_comprehensive_tests.sh --quick',
            'description' => 'Run essential tests only (faster)',
            'category' => 'quick'
        ],
        'system_validation' => [
            'name' => 'System Validation',
            'script' => 'python tests/system/test_complete_system_validation.py',
            'description' => 'Complete system health and component validation',
            'category' => 'system'
        ],
        'architecture' => [
            'name' => 'Architecture Test',
            'script' => 'python tests/system/test_system_architecture.py',
            'description' => 'Validate system architecture compliance',
            'category' => 'architecture'
        ],
        'frontend_basic' => [
            'name' => 'Frontend Basic Validation',
            'script' => 'python tests/WebFrontend/test_basic_validation.py',
            'description' => 'Basic frontend functionality tests',
            'category' => 'frontend'
        ]
    ];
}

/**
 * Get test logs from the log directory
 */
function getTestLogs($limit = 10) {
    $logs = [];
    $logFiles = glob(LOG_DIR . '/test_*.log');
    
    foreach ($logFiles as $logFile) {
        if (is_readable($logFile)) {
            $logs[] = [
                'file' => basename($logFile),
                'content' => file_get_contents($logFile),
                'modified' => date('Y-m-d H:i:s', filemtime($logFile)),
                'size' => filesize($logFile)
            ];
        }
    }
    
    // Sort by modification time (newest first)
    usort($logs, function($a, $b) {
        return strtotime($b['modified']) - strtotime($a['modified']);
    });
    
    return array_slice($logs, 0, $limit);
}

// Handle AJAX requests
if ($_SERVER['REQUEST_METHOD'] === 'POST' && isset($_POST['action'])) {
    header('Content-Type: application/json');
    
    switch ($_POST['action']) {
        case 'run_test':
            $testKey = $_POST['test'] ?? '';
            $tests = getAvailableTests();
            
            if (isset($tests[$testKey])) {
                $result = executeTest($tests[$testKey]['name'], $tests[$testKey]['script']);
                echo json_encode($result);
            } else {
                echo json_encode(['error' => 'Test not found']);
            }
            exit;
            
        case 'get_logs':
            $logs = getTestLogs();
            echo json_encode($logs);
            exit;
            
        case 'get_status':
            // Simple system status check
            $status = [
                'php_version' => PHP_VERSION,
                'project_root' => PROJECT_ROOT,
                'tests_dir_exists' => is_dir(TESTS_DIR),
                'timestamp' => date('Y-m-d H:i:s'),
                'available_tests' => count(getAvailableTests())
            ];
            echo json_encode($status);
            exit;
    }
}

$availableTests = getAvailableTests();
$recentLogs = getTestLogs(5);
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Unicorn Investing - Test Management Dashboard</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f8f9fa;
            color: #333;
        }
        
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        
        .header h1 {
            margin: 0 0 10px 0;
            font-size: 2.5em;
            font-weight: 300;
        }
        
        .header p {
            margin: 0;
            opacity: 0.9;
            font-size: 1.1em;
        }
        
        .dashboard-grid {
            display: grid;
            grid-template-columns: 2fr 1fr;
            gap: 30px;
            margin-bottom: 30px;
        }
        
        .card {
            background: white;
            border-radius: 12px;
            padding: 25px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            border: 1px solid #e1e5e9;
        }
        
        .card h2 {
            margin: 0 0 20px 0;
            color: #2c3e50;
            font-size: 1.5em;
            font-weight: 500;
        }
        
        .test-item {
            background: #f8f9fa;
            border: 1px solid #dee2e6;
            border-radius: 8px;
            padding: 15px;
            margin-bottom: 15px;
            transition: all 0.2s ease;
        }
        
        .test-item:hover {
            background: #e9ecef;
            border-color: #adb5bd;
        }
        
        .test-name {
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 5px;
        }
        
        .test-description {
            color: #6c757d;
            font-size: 0.9em;
            margin-bottom: 10px;
        }
        
        .test-category {
            display: inline-block;
            background: #e7f3ff;
            color: #0066cc;
            padding: 3px 8px;
            border-radius: 4px;
            font-size: 0.8em;
            font-weight: 500;
        }
        
        .btn {
            background: #007bff;
            color: white;
            border: none;
            padding: 8px 16px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 0.9em;
            transition: background-color 0.2s;
        }
        
        .btn:hover {
            background: #0056b3;
        }
        
        .btn:disabled {
            background: #6c757d;
            cursor: not-allowed;
        }
        
        .btn-success {
            background: #28a745;
        }
        
        .btn-success:hover {
            background: #1e7e34;
        }
        
        .btn-danger {
            background: #dc3545;
        }
        
        .btn-danger:hover {
            background: #c82333;
        }
        
        .status-indicator {
            display: inline-block;
            width: 12px;
            height: 12px;
            border-radius: 50%;
            margin-right: 8px;
        }
        
        .status-success { background: #28a745; }
        .status-error { background: #dc3545; }
        .status-warning { background: #ffc107; }
        .status-running { background: #17a2b8; animation: pulse 1.5s infinite; }
        
        @keyframes pulse {
            0% { opacity: 1; }
            50% { opacity: 0.5; }
            100% { opacity: 1; }
        }
        
        .log-item {
            background: #f8f9fa;
            border-left: 4px solid #007bff;
            padding: 10px 15px;
            margin-bottom: 10px;
            font-family: 'Courier New', monospace;
            font-size: 0.85em;
        }
        
        .log-meta {
            color: #6c757d;
            font-size: 0.8em;
            margin-bottom: 5px;
        }
        
        .output-container {
            background: #1e1e1e;
            color: #f8f8f2;
            padding: 20px;
            border-radius: 8px;
            font-family: 'Courier New', monospace;
            font-size: 0.9em;
            max-height: 400px;
            overflow-y: auto;
            margin-top: 15px;
            white-space: pre-wrap;
        }
        
        .hidden {
            display: none;
        }
        
        .loading {
            text-align: center;
            padding: 20px;
            color: #6c757d;
        }
        
        .system-status {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin-bottom: 20px;
        }
        
        .status-item {
            background: #f8f9fa;
            padding: 15px;
            border-radius: 8px;
            text-align: center;
        }
        
        .status-value {
            font-size: 1.2em;
            font-weight: 600;
            color: #2c3e50;
        }
        
        .status-label {
            font-size: 0.9em;
            color: #6c757d;
            margin-top: 5px;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>🦄 Test Management Dashboard</h1>
        <p>Unicorn Investing Platform - Test Execution & Monitoring</p>
    </div>
    
    <div class="dashboard-grid">
        <div class="card">
            <h2>🧪 Available Tests</h2>
            <div id="test-list">
                <?php foreach ($availableTests as $key => $test): ?>
                <div class="test-item">
                    <div class="test-name"><?php echo htmlspecialchars($test['name']); ?></div>
                    <div class="test-description"><?php echo htmlspecialchars($test['description']); ?></div>
                    <div style="margin-top: 10px;">
                        <span class="test-category"><?php echo htmlspecialchars($test['category']); ?></span>
                        <button class="btn" onclick="runTest('<?php echo $key; ?>')" id="btn-<?php echo $key; ?>">
                            Run Test
                        </button>
                    </div>
                </div>
                <?php endforeach; ?>
            </div>
        </div>
        
        <div class="card">
            <h2>📊 System Status</h2>
            <div class="system-status">
                <div class="status-item">
                    <div class="status-value"><?php echo count($availableTests); ?></div>
                    <div class="status-label">Available Tests</div>
                </div>
                <div class="status-item">
                    <div class="status-value"><?php echo PHP_VERSION; ?></div>
                    <div class="status-label">PHP Version</div>
                </div>
                <div class="status-item">
                    <div class="status-value" id="status-tests-dir">
                        <?php echo is_dir(TESTS_DIR) ? '✅' : '❌'; ?>
                    </div>
                    <div class="status-label">Tests Directory</div>
                </div>
            </div>
            
            <h3>📄 Recent Test Logs</h3>
            <div id="recent-logs">
                <?php if (empty($recentLogs)): ?>
                    <p style="color: #6c757d; font-style: italic;">No recent test logs found</p>
                <?php else: ?>
                    <?php foreach ($recentLogs as $log): ?>
                    <div class="log-item">
                        <div class="log-meta">
                            <?php echo htmlspecialchars($log['file']); ?> - 
                            <?php echo htmlspecialchars($log['modified']); ?> 
                            (<?php echo number_format($log['size']); ?> bytes)
                        </div>
                    </div>
                    <?php endforeach; ?>
                <?php endif; ?>
            </div>
        </div>
    </div>
    
    <div class="card">
        <h2>📺 Test Output</h2>
        <div id="test-output" class="hidden">
            <div id="test-status"></div>
            <div id="test-result" class="output-container"></div>
        </div>
        <div id="no-output" style="text-align: center; color: #6c757d; padding: 40px;">
            Select and run a test to see the output here
        </div>
    </div>

    <script>
        let currentTestRunning = false;
        
        function runTest(testKey) {
            if (currentTestRunning) {
                alert('A test is already running. Please wait for it to complete.');
                return;
            }
            
            currentTestRunning = true;
            const btn = document.getElementById('btn-' + testKey);
            const originalText = btn.textContent;
            
            // Update UI to show running state
            btn.textContent = 'Running...';
            btn.disabled = true;
            btn.className = 'btn btn-warning';
            
            document.getElementById('no-output').style.display = 'none';
            document.getElementById('test-output').classList.remove('hidden');
            document.getElementById('test-status').innerHTML = 
                '<span class="status-indicator status-running"></span>Running test: ' + testKey;
            document.getElementById('test-result').textContent = 'Initializing test...';
            
            // Make AJAX request to run the test
            const formData = new FormData();
            formData.append('action', 'run_test');
            formData.append('test', testKey);
            
            fetch(window.location.href, {
                method: 'POST',
                body: formData
            })
            .then(response => response.json())
            .then(data => {
                currentTestRunning = false;
                
                // Reset button
                btn.textContent = originalText;
                btn.disabled = false;
                
                if (data.error) {
                    btn.className = 'btn btn-danger';
                    document.getElementById('test-status').innerHTML = 
                        '<span class="status-indicator status-error"></span>Error: ' + data.error;
                    document.getElementById('test-result').textContent = data.error;
                } else {
                    btn.className = data.success ? 'btn btn-success' : 'btn btn-danger';
                    document.getElementById('test-status').innerHTML = 
                        '<span class="status-indicator ' + (data.success ? 'status-success' : 'status-error') + '"></span>' +
                        'Test completed: ' + data.name + ' - ' + (data.success ? 'PASSED' : 'FAILED') +
                        ' (Return code: ' + data.returnCode + ')';
                    document.getElementById('test-result').textContent = data.output;
                }
                
                // Auto-reset button color after 3 seconds
                setTimeout(() => {
                    btn.className = 'btn';
                }, 3000);
            })
            .catch(error => {
                currentTestRunning = false;
                btn.textContent = originalText;
                btn.disabled = false;
                btn.className = 'btn btn-danger';
                
                document.getElementById('test-status').innerHTML = 
                    '<span class="status-indicator status-error"></span>Error executing test';
                document.getElementById('test-result').textContent = 'Error: ' + error.message;
                
                setTimeout(() => {
                    btn.className = 'btn';
                }, 3000);
            });
        }
        
        // Auto-refresh logs every 30 seconds
        setInterval(() => {
            fetch(window.location.href, {
                method: 'POST',
                body: new URLSearchParams({action: 'get_logs'})
            })
            .then(response => response.json())
            .then(logs => {
                // Update recent logs if needed
                console.log('Logs refreshed:', logs.length);
            })
            .catch(error => console.error('Error refreshing logs:', error));
        }, 30000);
    </script>
</body>
</html>