@Echo Off
java -server -Xmx800m -XX:MaxPermSize=200m -XX:+UseConcMarkSweepGC -XX:+UseParNewGC -XX:+CMSIncrementalPacing -XX:ParallelGCThreads=2 -jar jars\weirdcanada-distro.jar -Drun.mode=test

