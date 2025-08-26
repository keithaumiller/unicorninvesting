<?php
/**
 * Diagnostic script to check module and routing status
 */

echo "=== Module Diagnostic ===\n";

// Test if we can access Drupal without authentication issues
$module_path = '/workspaces/unicorninvesting/WebFrontend/web/modules/custom/unicornmetrics';

// Check file permissions
$files_to_check = [
    'unicornmetrics.info.yml',
    'unicornmetrics.routing.yml',
    'src/Controller/DashboardController.php'
];

foreach ($files_to_check as $file) {
    $full_path = "$module_path/$file";
    if (file_exists($full_path)) {
        $perms = substr(sprintf('%o', fileperms($full_path)), -4);
        echo "✓ $file exists (permissions: $perms)\n";
    } else {
        echo "✗ $file missing\n";
    }
}

// Check if .htaccess exists and what it contains
$htaccess_path = '/workspaces/unicorninvesting/WebFrontend/web/.htaccess';
if (file_exists($htaccess_path)) {
    echo "\n=== .htaccess Status ===\n";
    echo "✓ .htaccess exists\n";
    
    // Check for any authentication directives
    $htaccess_content = file_get_contents($htaccess_path);
    if (strpos($htaccess_content, 'AuthType') !== false) {
        echo "⚠ .htaccess contains authentication directives\n";
    } else {
        echo "✓ No authentication directives in .htaccess\n";
    }
} else {
    echo "\n✗ .htaccess missing\n";
}

// Test Drupal bootstrap without web access
echo "\n=== Testing Basic Access ===\n";

// Create a simple test file to check if basic PHP execution works
$test_file = '/workspaces/unicorninvesting/WebFrontend/web/test_access.php';
file_put_contents($test_file, '<?php echo "PHP_OK"; ?>');

echo "✓ Test file created at web/test_access.php\n";

// Check what URLs respond to
$test_urls = [
    'https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/test_access.php',
    'https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/',
    'https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/admin',
    'https://solid-acorn-gw6xx47pqxfv99p-80.app.github.dev/user/login'
];

foreach ($test_urls as $url) {
    $response_code = null;
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_FOLLOWLOCATION, false);
    curl_setopt($ch, CURLOPT_TIMEOUT, 10);
    curl_setopt($ch, CURLOPT_HEADER, true);
    curl_setopt($ch, CURLOPT_NOBODY, true);
    
    $result = curl_exec($ch);
    $response_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    $effective_url = curl_getinfo($ch, CURLINFO_EFFECTIVE_URL);
    curl_close($ch);
    
    echo "URL: " . basename($url) . " → HTTP $response_code\n";
    
    if ($response_code == 302) {
        // Get the redirect location
        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
        curl_setopt($ch, CURLOPT_TIMEOUT, 10);
        
        $final_result = curl_exec($ch);
        $final_url = curl_getinfo($ch, CURLINFO_EFFECTIVE_URL);
        curl_close($ch);
        
        if (strpos($final_url, 'pf-signin') !== false) {
            echo "  → Redirects to GitHub authentication\n";
        } else {
            echo "  → Final URL: $final_url\n";
        }
    }
}

echo "\nDone!\n";
