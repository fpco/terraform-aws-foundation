# Clean up
# we ought to employ some of the methods described in
#
apt-get -y autoremove
apt-get -y clean

echo "clear consul's tmp path, if it is present"
rm -rf /home/consul/tmp/* || true

