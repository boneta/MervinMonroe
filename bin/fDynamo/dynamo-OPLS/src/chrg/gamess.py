#!/usr/bin/env python

import	os
import	sys
import	string
import	struct
import	time

import	charges


class GAMESS( charges.Charges ):

	def __init__( self ):
		charges.Charges.__init__( self )
		self.rtype = { 0: 'energy', 1: 'gradient', 2: 'hessian' }
# gamess compiled using the -i4 flag (mostly 32bits)
		self.__ins = 4
		self.__int = "i"
# gamess compiled using the -i8 flag (mostly em64t)
#		self.__ins = 8
#		self.__int = "l"


	def serve( self, sckt ):
		self.load( sckt )
		sys.stderr.write( '-- launching gamess-us ' )
		if( os.path.isfile( "calc.mos" ) ):
			guess = ""
		else:
			guess = "!"
		fd = file( 'calc.inp', 'wt' )
		fd.write( """ $contrl
runtyp = %s
scftyp = rhf
dfttyp = b3lyp
mplevl = 0
icharg = 0
mult   = 1
coord  = cart
nzvar  = 0
units  = angs
maxit  = 200
nosym  = 1
nprint = 7
%sirest  = -1
 $end

%s $guess
%sguess  = mosaved
%s $end

 $system
! 1 mword ~ 7.6 Mb
mwords = 10
 $end

 $dft
sg1    =.true.
swoff  = 1.d+03
 $end

 $scf
dirscf =.true.
ethrsh = 1.d+03
 $end

 $basis
ngauss = 3
gbasis = n21
ndfunc = 1
npfunc = 0
diffsp =.false.
diffs  =.false.
 $end

 $data
--slave--
c1
"""%( self.rtype[self.code], guess, guess, guess, guess ) )
		for i in xrange( self.naqm ):
			fd.write( '%-8s%8.3lf%20.10lf%20.10lf%20.10lf\n'%( 
				self.atmsmb[int(self.z[i])], self.z[i], self.cord[i*3], self.cord[i*3+1], self.cord[i*3+2] ) )
		fd.write( """ $end

 $force
purify =.true.
projct =.true.
temp   = 298.15
 $end

 $elpot
iepot  = 1
where  = pdc
 $end

 $pdc
ptsel  = geodesic
constr = charge
 $end
""" )
		fd.close()
		if( self.natm - self.naqm > 0 ):
			fd = file( 'mm_charges', 'wb' )
			fd.write( struct.pack( "i", self.__ins ) )
			fd.write( struct.pack( self.__int, self.natm - self.naqm ) )
			fd.write( struct.pack( "i", self.__ins ) )
			b = struct.pack( "i", 32 )
			for i in xrange( self.naqm, self.natm ):
				fd.write( b + struct.pack( "d", self.cord[i*3] ) + 
					struct.pack( "d", self.cord[i*3+1] ) +
					struct.pack( "d", self.cord[i*3+2] ) +
					struct.pack( "d", self.z[i] ) + b )
			fd.close()

		os.system( '/bin/sh gamess.sh' )
		time.sleep( 10 )
		sys.stderr.write( ' [done]\n' )

		gam_ener = None
		gam_qfit = []
		fd = file( 'calc.out', 'rt' )
		line = fd.readline()
		while( line != '' ):
			if( line[0:6] == ' FINAL' and not gam_ener ):
				gam_ener = string.atof( string.split( line )[-4] )
			if( line[0:13] == ' NET CHARGES:' and not gam_qfit ):
				fd.readline(); fd.readline(); fd.readline()
				for i in xrange( self.naqm ):
					gam_qfit.append( string.atof( string.split( fd.readline() )[1] ) )
			line = fd.readline()
		fd.close()
		gam_grad = []
		gam_hess = []
		fd = file( 'calc.dat', 'rt' )
		line = fd.readline()
		while( line != '' ):
			if( self.code == 1 and line[0:5] == ' $VIB' ):
				fd.readline()
				for i in xrange( int(3*self.naqm/5) + ( (3*self.naqm)%5 != 0 ) ):
					line = fd.readline()
					for j in xrange( 0, len( line ) - 1, 16 ):
						gam_grad.append( string.atof( line[j:j+16] ) )
			if( self.code == 2 and line[0:6] == ' $GRAD' ):
				fd.readline()
				for i in xrange( self.naqm ):
					for j in string.split( fd.readline() )[2:]:
						gam_grad.append( string.atof( j ) )
			if( self.code == 2 and line[0:6] == " $HESS" ):
				fd.readline()
				tmp_hess = []
				nl = int(3*self.naqm/5) + ( (3*self.naqm)%5 != 0 )
				for i in xrange( 3 * self.naqm ):
					tmp_hrow = []
					for j in xrange( nl ):
						line = fd.readline()
						for k in xrange( 5, len( line ) - 1, 15 ):
							tmp_hrow.append( string.atof( line[k:k+15] ) )
					tmp_hess.append( tmp_hrow )
				for i in xrange( 3 * self.naqm ):
					for j in xrange( i + 1 ):
						gam_hess.append( tmp_hess[j][i] )
			line = fd.readline()
		fd.close()

		self.save( sckt, gam_ener, gam_qfit, gam_grad, gam_hess )



if( __name__ == '__main__' ):
	GAMESS().run()
