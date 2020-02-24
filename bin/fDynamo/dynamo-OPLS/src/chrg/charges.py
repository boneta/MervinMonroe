#!/usr/bin/env python

import	os
import	sys
import	socket
import	struct

class Charges:

	def __init__( self, unixfn = 'chrg.unix', scksiz = 1048576 ):
		self.unixfn = unixfn
		self.scksiz = scksiz
		self.atmsmb = [  'X',  'H', 'He', 'Li', 'Be',  'B',  'C',  'N',  'O',  'F', 'Ne', 'Na', 'Mg',
			'Al', 'Si',  'P',  'S', 'Cl', 'Ar',  'K', 'Ca', 'Sc', 'Ti',  'V', 'Cr', 'Mn', 'Fe',
		    'Co', 'Ni', 'Cu', 'Zn', 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr',  'Y', 'Zr',
			'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', 'Sb', 'Te',  'I', 'Xe',
			'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er',
			'Tm', 'Yb', 'Lu', 'Hf', 'Ta',  'W', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb',
			'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', 'Pa',  'U', 'Np', 'Pu', 'Am', 'Cm',
			'Bk', 'Cf', 'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt' ]
		self.code = None
		self.natm = None
		self.naqm = None
		self.z    = None
		self.cord = None


	def load( self, sckt ):
		buf = sckt.recv( self.scksiz )
		while( len( buf ) < 4 ):
			buf += sckt.recv( self.scksiz )
		total = struct.unpack( 'i', buf[0:4] )[0]
		while( len( buf ) < total ):
			buf += sckt.recv( self.scksiz )
		self.code = struct.unpack( 'i', buf[4:8] )[0]
		self.natm = struct.unpack( 'i', buf[8:12] )[0]
		self.naqm = struct.unpack( 'i', buf[12:16] )[0]
		nb   = 16 + 8 * self.natm
		self.z    = struct.unpack( '%dd'%( self.natm ), buf[16:nb] )
		nf   = nb +  8 * 3 * self.natm
		self.cord = struct.unpack( '%dd'%( 3 * self.natm ), buf[nb:nf] )


	def save( self, sckt, ener, chrg, grad = None, hess = None ):
		buf = struct.pack( 'd', ener )
		for i in xrange( self.naqm ):
			buf += struct.pack( 'd', chrg[i] )
		if( grad ):
			for i in xrange( 3*self.naqm ):
				buf += struct.pack( 'd', grad[i] )
		if( hess ):
			for i in xrange( 3*self.naqm*(self.naqm*3+1)/2 ):
				buf += struct.pack( 'd', hess[i] )
		sckt.sendall( buf )


	def serve( self, sckt ):
		# "Virtual" procedure for the external code
		# pre-processing: 	load( sckt )
		# calculation & parsing results
		# post-processing:	save( sckt, ener, chrg, [grad], [hess] )
		sys.stderr.write( '-- Redefine this function!!! (_Charges__serve)\n' )


	def run( self ):
		msckt = socket.socket( socket.AF_UNIX, socket.SOCK_STREAM )
		try:
			os.unlink( self.unixfn )
		except OSError:
			pass
		msckt.bind( self.unixfn )
		sys.stderr.write( '-- server ready...\n' )
		msckt.listen( 1 )
		while( 1 ):
			sckt, addr = msckt.accept()
			self.serve( sckt )
			sckt.close()
		# Never reached...
		msckt.close()
		sys.stderr.write( '-- server shutdown\n' )
