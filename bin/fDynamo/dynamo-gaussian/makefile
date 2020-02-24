HOME = /home/dynamo-gaussian
FC   = mpif90
FLGS = -DF95 -I$(HOME)/src -I. -J. -w -O2 -finit-local-zero

all:
	$(FC) $(FLGS) $(SRC) $(HOME)/src/dynamo.a
