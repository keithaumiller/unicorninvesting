<?php
/**
 * Test API Endpoint
 * RESTful API for test management and execution
 */

header('Content-Type: application/json');
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: GET, POST, OPTIONS');
header('Access-Control-Allow-Headers: Content-Type');

// Handle OPTIONS request for CORS
if ($_SERVER['REQUEST_METHOD'] === 'OPTIONS') {
    exit(0);
}

// Project paths
define('PROJECT_ROOT', dirname(__DIR__));
define('TESTS_DIR', PROJECT_ROOT . '/tests');
define('LOG_DIR', '/tmp');

/**
 * Execute a test and return structured result
 */
function executeTest($testScript, $timeout = 300) {
    $startTime = microtime(true);
    $command = "cd " . PROJECT_ROOT . " && timeout {$timeout} " . $testScript;
    
    $descriptorspec = [
        0 => ["pipe", "r"],
        1 => ["pipe", "w"],
        2 => ["pipe", "w"]
    ];
    
    $process = proc_open($command, $descriptorspec, $pipes);
    
    if (is_resource($process)) {
        fclose($pipes[0]);
        
        $stdout = stream_get_contents($pipes[1]);
        $stderr = stream_get_contents($pipes[2]);
        
        fclose($pipes[1]);
        fclose($pipes[2]);
        
        $returnCode = proc_close($process);
        $endTime = microtime(true);
        
        return [
            'success' => $returnCode === 0,
            'returnCode' => $returnCode,
            'output' => $stdout,
            'error' => $stderr,
            'duration' => round($endTime - $startTime, 2),
            'timestamp' => date('Y-m-d H:i:s')
        ];
    }
    
    return [
        'success' => false,
        'returnCode' => -1,
        'output' => '',
        'error' => 'Failed to start process',
        'duration' => 0,
        'timestamp' => date('Y-m-d H:i:s')
    ];
}

/**
 * Get test configuration
 */
function getTestConfig() {
    return [
        'comprehensive' => [
            'name' => 'Comprehensive Test Suite',
            'script' => './tests/run_comprehensive_tests.sh',
            'description' => 'Run all tests with detailed reporting',
            'category' => 'full',
            'timeout' => 600,
            'tags' => ['integration', 'system', 'full']
        ],
        'comprehensive_quick' => [
            'name' => 'Quick Test Suite',
            'script' => './tests/run_comprehensive_tests.sh --quick',
            'description' => 'Run essential tests only (faster)',
            'category' => 'quick',
            'timeout' => 300,
            'tags' => ['integration', 'quick']
        ],
        'system_validation' => [
            'name' => 'System Validation',
            'script' => 'python tests/system/test_complete_system_validation.py',
            'description' => 'Complete system health and component validation',
            'category' => 'system',
            'timeout' => 180,
            'tags' => ['system', 'health']
        ],
        'architecture' => [
            'name' => 'Architecture Test',
            'script' => 'python tests/system/test_system_architecture.py',
            'description' => 'Validate system architecture compliance',
            'category' => 'architecture',
            'timeout' => 60,
            'tags' => ['architecture', 'compliance']
        ],
        'frontend_basic' => [
            'name' => 'Frontend Basic Validation',
            'script' => 'python tests/WebFrontend/test_basic_validation.py',
            'description' => 'Basic frontend functionality tests',
            'category' => 'frontend',
            'timeout' => 120,
            'tags' => ['frontend', 'basic']
        ]
    ];
}

/**
 * Get system information
 */
function getSystemInfo() {
    return [
        'php_version' => PHP_VERSION,
        'project_root' => PROJECT_ROOT,
        'tests_dir_exists' => is_dir(TESTS_DIR),
        'python_version' => trim(shell_exec('python --version 2>&1') ?: 'Unknown'),
        'available_tests' => count(getTestConfig()),
        'log_dir_writable' => is_writable(LOG_DIR),
        'timestamp' => date('Y-m-d H:i:s'),
        'uptime' => trim(shell_exec('uptime') ?: 'Unknown'),
        'disk_space' => disk_free_space('.') ? round(disk_free_space('.') / 1024 / 1024 / 1024, 2) . ' GB' : 'Unknown'
    ];
}

/**
 * Get recent test logs
 */
function getTestLogs($limit = 20) {
    $logs = [];
    $logFiles = glob(LOG_DIR . '/test_*.log');
    
    foreach ($logFiles as $logFile) {
        if (is_readable($logFile)) {
            $content = file_get_contents($logFile);
            $logs[] = [
                'file' => basename($logFile),
                'content' => $content,
                'modified' => date('Y-m-d H:i:s', filemtime($logFile)),
                'size' => filesize($logFile),
                'lines' => substr_count($content, "\n") + 1
            ];
        }
    }
    
    // Sort by modification time (newest first)
    usort($logs, function($a, $b) {
        return strtotime($b['modified']) - strtotime($a['modified']);
    });
    
    return array_slice($logs, 0, $limit);
}

/**
 * Get test history from log analysis
 */
function getTestHistory($limit = 50) {
    $history = [];
    $logs = getTestLogs(100);
    
    foreach ($logs as $log) {
        // Parse test results from log content
        if (preg_match('/Test run duration|Total Tests:|Passed:|Failed:|Success Rate:/', $log['content'])) {
            $testName = str_replace(['test_', '.log'], '', $log['file']);
            
            // Extract metrics
            $totalTests = preg_match('/Total Tests:\s*(\d+)/', $log['content'], $matches) ? intval($matches[1]) : 0;
            $passedTests = preg_match('/Passed:\s*(\d+)/', $log['content'], $matches) ? intval($matches[1]) : 0;
            $failedTests = preg_match('/Failed:\s*(\d+)/', $log['content'], $matches) ? intval($matches[1]) : 0;
            $successRate = preg_match('/Success Rate:\s*(\d+(?:\.\d+)?)%/', $log['content'], $matches) ? floatval($matches[1]) : 0;
            
            $history[] = [
                'test_name' => $testName,
                'timestamp' => $log['modified'],
                'total_tests' => $totalTests,
                'passed_tests' => $passedTests,
                'failed_tests' => $failedTests,
                'success_rate' => $successRate,
                'log_file' => $log['file']
            ];
        }
    }
    
    return array_slice($history, 0, $limit);
}

// Route handling
$method = $_SERVER['REQUEST_METHOD'];
$path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$query = $_GET;

try {
    switch ($method) {
        case 'GET':
            if (isset($query['action'])) {
                switch ($query['action']) {
                    case 'tests':
                        echo json_encode(['tests' => getTestConfig()]);
                        break;
                        
                    case 'system':
                        echo json_encode(['system' => getSystemInfo()]);
                        break;
                        
                    case 'logs':
                        $limit = isset($query['limit']) ? intval($query['limit']) : 20;
                        echo json_encode(['logs' => getTestLogs($limit)]);
                        break;
                        
                    case 'history':
                        $limit = isset($query['limit']) ? intval($query['limit']) : 50;
                        echo json_encode(['history' => getTestHistory($limit)]);
                        break;
                        
                    case 'status':
                        echo json_encode([
                            'status' => 'ok',
                            'system' => getSystemInfo(),
                            'recent_logs' => getTestLogs(5)
                        ]);
                        break;
                        
                    default:
                        echo json_encode(['error' => 'Unknown action']);
                        break;
                }
            } else {
                echo json_encode([
                    'api' => 'Unicorn Investing Test Management API',
                    'version' => '1.0',
                    'endpoints' => [
                        'GET ?action=tests' => 'Get available tests',
                        'GET ?action=system' => 'Get system information',
                        'GET ?action=logs' => 'Get test logs',
                        'GET ?action=history' => 'Get test history',
                        'GET ?action=status' => 'Get overall status',
                        'POST {test: testKey}' => 'Execute a test'
                    ]
                ]);
            }
            break;
            
        case 'POST':
            $input = json_decode(file_get_contents('php://input'), true);
            
            if (!$input && $_POST) {
                $input = $_POST;
            }
            
            if (isset($input['test'])) {
                $testKey = $input['test'];
                $tests = getTestConfig();
                
                if (isset($tests[$testKey])) {
                    $test = $tests[$testKey];
                    $result = executeTest($test['script'], $test['timeout']);
                    $result['test_name'] = $test['name'];
                    $result['test_key'] = $testKey;
                    
                    echo json_encode($result);
                } else {
                    http_response_code(400);
                    echo json_encode(['error' => 'Test not found: ' . $testKey]);
                }
            } else {
                http_response_code(400);
                echo json_encode(['error' => 'Test parameter required']);
            }
            break;
            
        default:
            http_response_code(405);
            echo json_encode(['error' => 'Method not allowed']);
            break;
    }
    
} catch (Exception $e) {
    http_response_code(500);
    echo json_encode(['error' => 'Internal server error: ' . $e->getMessage()]);
}
?>