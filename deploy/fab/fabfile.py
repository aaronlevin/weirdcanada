from fabric.api import run, sudo, env, put, cd

env.use_ssh_config = True
#env.hosts = ['ec2-weirdcanada-admin']

def install_python():
    sudo("apt-get install python2.7")
    sudo("apt-get install python-dev")

def install_java():
    sudo('apt-get install openjdk-7-jdk')

def install_postgres():
    sudo('apt-get install postgresql')
    sudo('apt-get install postgresql-server-9.1')

def install_nginx():
    sudo('apt-get install nginx')

def install_sbt():
    sudo('wget http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch//0.12.4/sbt-launch.jar')

def setup_db():
    sudo("psql -u postgres psql postgres -c \"create user weirdcanada with encrypted password 'xxx';\"")
    sudo("psql -u postgres psql postgres -c \"create database weirdcanada;\"")
    sudo("psql -u postgres psql postgres -c \"grant all privileges on database weirdcanada to weirdcanada;\"")

def put_props():
    put('site/src/main/resources/props/production.default.props', '~/weirdcanada/site/src/main/resources/props/production.default.props')
    put('site/src/main/resources/props/default.props', '~/weirdcanada/src/main/resources/props/default.props')
    put('scripts/bash_variables', '~/weirdcanada/scripts/bash_variables')

def put_distro_props():
    put('distro/src/main/resources/props/production.default.props', '~/weirdcanada/distro/src/main/resources/props/production.default.props')

def fetch_changes():
    with cd('~/weirdcanada'):
        run('git fetch origin')
        run('git merge origin/master --ff-only')

def update_cronjobs():
    fetch_changes()
    with cd('~/weirdcanada'):
        run('touch cron.tmp')
        run('crontab -l > cron.tmp || true')
        run('cat deploy/cronjobs >> cron.tmp')
        run('sort -u cron.tmp > cron2.tmp')
        run('crontab cron2.tmp')
        run('rm cron.tmp cron2.tmp')

def build_admin():
    with cd('~/weirdcanada'):
        run('./sbt site/compile')
        run('./sbt site/assembly')
        run('cp site/target/scala-2.10/weirdcanada-admin-assembly-0.0.1.jar deploy/jars/weirdcanada-admin.jar')

def build_distro():
    with cd('~/weirdcanada'):
        run('./sbt distro/compile')
        run('./sbt distro/assembly')
        run('cp distro/target/scala-2.10/weirdcanada-distro-assembly-1.0.jar deploy/jars/weirdcanada-distro.jar')

def start_admin_app():
    with cd('~/weirdcanada'):
        run('dtach -n /tmp/weirdcanada-admin-session ~/weirdcanada/deploy/weirdcanada-admin')

def start_distro_app():
    with cd('~/weirdcanada'):
        run('dtach -A /tmp/weirdcanada-distro-session ~/weirdcanada/deploy/weirdcanada-distro')

def restart_admin_app():
    run('jps | grep \'weirdcanada-admin\.jar\' | grep -oP \'^\d+\' | while read line; do kill -9 "$line"; done')
    start_admin_app()

def restart_distro_app():
    run('jps | grep \'weirdcanada-distro\.jar\' | grep -oP \'^\d+\' | while read line; do kill -9 "$line"; done')
    start_distro_app()

def deploy_admin():
    fetch_changes()
    put_props()
    update_cronjobs()
    build_admin()
    restart_admin_app()

def deploy_distro():
    fetch_changes()
    put_distro_props()
    build_distro()
    restart_distro_app()

def configure_box():
    install_pyton()
    install_java()
    install_postgres()
    install_nginx()
    install_sbt()
    setup_db()
    sudo('apt-get install dtach')
