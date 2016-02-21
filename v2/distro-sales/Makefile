exe="/home/aaronlevin/dev/oss/weirdcanada/v2/distro-sales/.stack-work/install/x86_64-linux/lts-5.2/7.10.3/bin/distro-sales-exe"

build: $(wildcard src/*.hs) $(wildcard app/*.hs)
	stack build

release: dist/build/distro-sales-exe/distro-sales-exe
	patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $(exe)
	scp -l 8192 -vC $(exe) portal-web1:distro-sales
