from fabric.api import run, sudo, env, put

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

def setup_db():
    sudo("psql -u postgres psql postgres -c \"create user weirdcanada with encrypted password 'xxx';\"")
    sudo("psql -u postgres psql postgres -c \"create database weirdcanada;\"")
    sudo("psql -u postgres psql postgres -c \"grant all privileges on database weirdcanada to weirdcanada;\"")

def put_war():
    run('mkdir -p src/main')
    put('src/main/webapp', 'src/main')

def put_startup_script():
    put('deploy/weirdcanada-admin', '/tmp/weirdcanada-admin')
    sudo('cp /tmp/weirdcanada-admin /etc/init.d')
    sudo('chmod +x /etc/init.d/weirdcanada-admin')
    sudo('rm /tmp/weirdcanada-admin')

def deploy():
    put_war()
    run('mkdir -p deploy/jars')
    put('deploy/jars/weirdcanada-admin.jar', 'deploy/jars/weirdcanada-admin.jar')
    put_startup_script()

def start_admin_app():
    run('dtach -n /tmp/weirdcanada-admin-session deploy/weirdcanada-admin')

def configure_box():
    install_pyton()
    install_java()
    install_postgres()
    install_nginx()
    setup_db()
    sudo('apt-get install dtach')
