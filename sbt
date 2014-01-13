java -Xms512M -Xmx1G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=500M -jar `dirname $0`/sbt-launch.jar "$@"
# java -Xmx1024M -Xss2M -XX:MaxPermSize=512m -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch.jar "$@"

