wget -qO- https://mirror.ibcp.fr/pub/gnu/gsl/gsl-latest.tar.gz | \
tar xfz -
cd /gsl-2.7/
configure --prefix=${HPC_WORK}
make
make install
cd
