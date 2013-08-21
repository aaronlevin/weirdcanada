from fabric.api import run, sudo, env, put, cd

env.use_ssh_config = True
env.hosts = ['ec2-weirdcanada-admin']

def install_python():
    sudo("apt-get install python2.7")

def install_java():
    sudo('apt-get install openjdk-7-jdk')

def install_postgres():
    sudo('apt-get install postgresql')

def install_nginx():
    sudo('apt-get install nginx')

def install_sbt():
    sudo('wget http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch//0.12.4/sbt-launch.jar')

def setup_db():
    sudo("psql -u postgres psql postgres -c \"create user weirdcanada with encrypted password 'xxx';\"")
    sudo("psql -u postgres psql postgres -c \"create database weirdcanada;\"")
    sudo("psql -u postgres psql postgres -c \"grant all privileges on database weirdcanada to weirdcanada;\"")

def put_props():
    put('src/main/resources/props/production.default.props', '/home/ubuntu/weirdcanada/src/main/resources/props/production.default.props')
    put('src/main/resources/props/default.props', '/home/ubuntu/weirdcanada/src/main/resources/props/default.props')
    put('scripts/bash_variables', '/home/ubuntu/weirdcanada/scripts/bash_variables')

def fetch_changes():
    with cd('/home/ubuntu/weirdcanada'):
        run('git fetch origin')
        run('git merge origin/master --ff-only')

def update_cronjobs():
    fetch_changes()
    with cd('/home/ubuntu/weirdcanada'):
        run('touch cron.tmp')
        run('crontab -l > cron.tmp || true')
        run('cat scripts/cronjobs >> cron.tmp')
        run('sort -u cron.tmp > cron2.tmp')
        run('crontab cron2.tmp')
        run('rm cron.tmp cron2.tmp')

def build():
    with cd('/home/ubuntu/weirdcanada'):
        run('./sbt compile')
        run('./sbt assembly')
        run('cp target/scala-2.10/weirdcanada-assembly-0.0.1.jar deploy/jars/weirdcanada-admin.jar')

def start_admin_app():
    with cd('/home/ubuntu/weirdcanada'):
        run('dtach -n /tmp/weirdcanada-admin-session /home/ubuntu/weirdcanada/deploy/weirdcanada-admin')

def restart_admin_app():
    run('jps | grep \'weirdcanada-admin\.jar\' | grep -oP \'^\d+\' | while read line; do kill -9 "$line"; done')
    start_admin_app()

def deploy():
    put_props()
    fetch_changes()
    build()
    restart_admin_app()

def configure_box():
    install_pyton()
    install_java()
    install_postgres()
    install_nginx()
    install_sbt()
    setup_db()
    sudo('apt-get install dtach')
