<?php
/**
 * Simple script to check module status and routing
 */

echo "=== Module Status Check ===\n";

// Check if the module directory exists
$module_path = '/workspaces/unicorninvesting/WebFrontend/web/modules/custom/unicornmetrics';
if (is_dir($module_path)) {
    echo "✓ Module directory exists: $module_path\n";
} else {
    echo "✗ Module directory NOT found: $module_path\n";
    exit(1);
}

// Check for required files
$required_files = [
    'unicornmetrics.info.yml',
    'unicornmetrics.routing.yml',
    'src/Controller/DashboardController.php'
];

foreach ($required_files as $file) {
    $full_path = "$module_path/$file";
    if (file_exists($full_path)) {
        echo "✓ File exists: $file\n";
    } else {
        echo "✗ File missing: $file\n";
    }
}

// Read the current routing file
echo "\n=== Current Routing Configuration ===\n";
$routing_file = "$module_path/unicornmetrics.routing.yml";
if (file_exists($routing_file)) {
    $routing_content = file_get_contents($routing_file);
    echo $routing_content;
} else {
    echo "Routing file not found!\n";
}

// Read module info
echo "\n=== Module Info ===\n";
$info_file = "$module_path/unicornmetrics.info.yml";
if (file_exists($info_file)) {
    $info_content = file_get_contents($info_file);
    echo $info_content;
} else {
    echo "Info file not found!\n";
}

echo "\n=== Drupal Cache Directories ===\n";
$cache_dirs = [
    '/workspaces/unicorninvesting/WebFrontend/web/sites/default/files/php',
    '/workspaces/unicorninvesting/WebFrontend/web/sites/default/files/css',
    '/workspaces/unicorninvesting/WebFrontend/web/sites/default/files/js'
];

foreach ($cache_dirs as $dir) {
    if (is_dir($dir)) {
        $files = scandir($dir);
        $count = count($files) - 2; // subtract . and ..
        echo "Cache dir $dir: $count files\n";
    } else {
        echo "Cache dir $dir: does not exist\n";
    }
}

echo "\nDone!\n";
