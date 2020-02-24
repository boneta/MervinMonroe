#!/usr/bin/env python

import	os
import	sys
import	string

import	charges


class Jaguar( charges.Charges ):

	def __init__( self, nprocs = 1, tmpdir = None ):
		charges.Charges.__init__( self )
		self.__nprocs = nprocs
		if( tmpdir ):
			self.__tmpdir = tmpdir
		else:
			self.__tmpdir = os.getcwd() + os.sep + 'jaguar_tmp'
		self.__input = '&gen\nisymm=0 iunit=1\nip7=0 ip11=0 ip12=0\n'
# Restricted (0) / unrestricted (1)
		self.__input += 'iuhf=0\n'
# [SCF] Only ultrafine grid used; "tight" cutoffs
#		self.__input += 'iacc=1\n'
# [SCF] Accurate: mixed grid types, accurate cutoffs
		self.__input += 'iacc=2\n'
# *[SCF] Quick: mixed grid types, looser cutoffs
#		self.__input += 'iacc=3\n'
# *[SCF] Energy Convergence (5.0e-5), Density Convergence (5.0e-6)
#		self.__input += 'econv=5.0e-5 dconv=5.0e-6\n'
# [MP2]
#		self.__input += 'mp2=3\n'
# [DFT]
# [DFT] B3LYP
#		self.__input += 'dftname=b3lyp\n'
# [DFT] BLYP
#		self.__input += 'idft=2011\n'
# *[DFT] standard grids
#		self.__input += 'gdftmed=-10 gdftfine=-11 gdftgrad=-12\n'
# [DFT] fine grids
#		self.__input += 'gdftmed=-13 gdftfine=-13 gdftgrad=-13\n'
		self.__input += 'basis=%s\nmolchg=%d multip=%d\n'%( '6-31g**', 0, 1 )
# special stuff (heme group with iron)
		self.__input += 'iacscf=2\n'
#----------------------------------------------------------------		
		try:
			fd = file( 'guess', 'rt' )
		except:
			self.__guess = ''
		else:
			self.__guess = string.join( fd.readlines() )
			fd.close()

	def serve( self, sckt ):
		self.load( sckt )

		sys.stderr.write( '-- launching jaguar ' )
		fd = file( 'calc.in', 'wt' )
		fd.write( self.__input )
		fd.write( 'maxit=100\nicfit=1\n' )
		if( self.code == 0 ):
			fd.write( 'igeopt=0\n' )
		elif( self.code == 1 ):
			fd.write( 'igeopt=-1\n' )
		else:
			fd.write( 'igeopt=-1\nifreq=1 imw=1 irder=0\n' )
		fd.write( '&\n&zmat\n' )
		for i in xrange( self.naqm ):
			fd.write( '%-8s%20.10lf%20.10lf%20.10lf\n'%( 
				self.atmsmb[int(self.z[i])], self.cord[i*3], self.cord[i*3+1], self.cord[i*3+2] ) )
		fd.write( '&\n&pointch\n' )
		for i in xrange( self.naqm, self.natm ):
			fd.write( '%8.3lf%20.10lf%20.10lf%20.10lf\n'%( 
				self.z[i], self.cord[i*3], self.cord[i*3+1], self.cord[i*3+2] ) )
		fd.write( '&\n' )
		fd.write( self.__guess )
		fd.close()

		os.system( 'export JAGUAR_SCRATCH="%s"; jaguar run -PROCS %d -WAIT calc.in'%( self.__tmpdir, self.__nprocs ) )
		time.sleep( 10 )
		fd = file( 'calc.out', 'rt' )
		jag_self = .0
		jag_ener = .0
		jag_grad = []
		jag_hess = []
		jag_qfit = []
		line = fd.readline()
		while( line != '' ):
			if( line[0:37] == '   (D)    Point charge-point charge..' ):
				jag_self = string.atof( string.split( line )[-1] )
			if( line[0:18] == ' SCFE: SCF energy:' ):
				jag_ener = string.atof( string.split( line )[-4] )
			if( line[0:18] == ' Total LMP2 Energy' ):
				jag_ener = string.atof( string.split( line )[-1] )
			if( line[0:32] == '  forces (hartrees/bohr) : total' and self.code in [ 1, 2 ] and not jag_grad ):
				fd.readline(); fd.readline(); fd.readline()
				for i in xrange( self.naqm ):
					t = string.split( fd.readline() )
					for j in [-3, -2, -1]:
						jag_grad.append( string.atof( t[j] ) )
			if( line[0:46] == '  Atomic charges from electrostatic potential:' ):
				i = 0
				j = int(self.naqm/5)
				j += self.naqm%5 != 0
				while( i < j ):
					fd.readline(); fd.readline()
					line = fd.readline()
					for itm in string.split( line )[1:]:
						jag_qfit.append( string.atof( itm ) )
					i += 1
			line = fd.readline()
		fd.close()

		fd = file( 'calc.01.in', 'rt' )
		self.__guess = ''
		line = fd.readline()
		flag = True
		while( line != '' and flag ):
			if( line[0:6] == '&guess' ):
				self.__guess += line
				while( flag ):
					line = fd.readline()
					self.__guess += line
					flag = not ( line[0] == '&' )
			line = fd.readline()
		fd.close()

		if( self.code == 2 ):
			jag_hess = range( 3*self.naqm*(3*self.naqm+1)/2 )
			fd = file( 'calc.01.in', 'rt' )
			line = fd.readline()
			while( line != '' ):
				if( line[0:5] == '&hess' ):
					line = fd.readline()
					while( line[0] != '&' ):
						t = string.split( line )
						if( len( t ) == 1 ):
							j = string.atoi( t[0] )
						else:
							i = string.atoi( t[0] )
							k = j
							for c in t[1:]:
								if( i == k ):
									jag_hess[i*(i+1)/2-1] = string.atof( c )
								else:
									jag_hess[i*(i-1)/2+k-1] = string.atof( c )
								k += 1
						line = fd.readline()
				line = fd.readline()
			fd.close()

		print jag_ener - jag_self
		print jag_qfit
		self.save( sckt, jag_ener - jag_self, jag_qfit, jag_grad, jag_hess )



if( __name__ == '__main__' ):
	Jaguar().run()
