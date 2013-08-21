java -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar `dirname $0`/sbt-launch.jar "$@"
# java -Xmx1024M -Xss2M -XX:MaxPermSize=512m -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch.jar "$@"

