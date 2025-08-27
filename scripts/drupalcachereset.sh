#Run this file as root to rebuild the drupal cache.
echo "Clearing Cache"
su www-data
cd /workspaces/unicorninvesting/WebFrontend
/usr/bin/php8.3 ./vendor/bin/drush.php cache:rebuild
exit