from fabric.api import run, sudo, env, put

env.use_ssh_config = True
env.hosts = ['ec2-weirdcanada-admin']

def install_python():
    sudo("apt-get install python2.7")

def install_java():
    sudo('apt-get install openjdk-7-jdk')

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
    run('dtach -n /etc/init.d/weirdcanada-admin')

def configure_box():
    install_pyton()
    install_java()
    sudo('apt-get install dtach')
