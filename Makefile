.PHONY: all clean

all:
	cd src && $(MAKE)

xen:
	cd src && $(MAKE) xen

clean:
	cd src && $(MAKE) clean

run:
	sudo xl destroy main.xen || /bin/true
	sudo /sbin/brctl addbr internal || /bin/true
	sudo /sbin/ifconfig internal 10.0.0.1 netmask 255.255.255.0
	sudo rm -f /tmp/disk.pcap
	sudo dd if=/dev/zero of=/tmp/disk.pcap bs=1M count=128
	echo Capturing to /tmp/disk.pcap
	mir-run -b xen src/_build/main.xen -vif internal -vbd 51712:/tmp/disk.pcap

