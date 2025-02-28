efibootmgr -c -d /dev/sda -p 1 -L 'Void Linux' -l '/vmlinuz-6.12.11_1' -u "root=UUID=$(/usr/local/bin/get_root_uuid.sh) rw initrd=\\initramfs-6.12.11_1.img"
