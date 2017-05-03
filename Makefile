# Makefile for visualize_gold
# REVISION
#   HNG, Jan 17,2014
                                                                                 
default: visualize
                                                                                 
all: createdir default

createdir:                                                                       
	(mkdir -p bin; mkdir -p input; mkdir -p output; mkdir -p tmp)

clean:
	(cd src; make $@)

visualize:
	(cd src; make $@)

