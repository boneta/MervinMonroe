#!/usr/bin/env python

import	os
import	sys
import	string
import	time

import	charges


class G03( charges.Charges ):

	def __init__( self, method = 'hf/3-21g*', charge = 0, multip = 1, memory = '512mb' ):
		charges.Charges.__init__( self )
		self.method = method
		self.charge = charge
		self.multip = multip
		self.memory = memory
		self.grms   = None


	def serve( self, sckt ):
		self.load( sckt )
		sys.stderr.write( '-- launching gaussian03 ' )
		fd = file( 'calc.com', 'wt' )
		fd.write( '%%Chk=calc\n%%Mem=%s\n#t %s charge guess=tcheck scf=(direct,conver=6) nosymm'%( 
			self.memory, self.method ) )
		if( self.code == 1 ):
			fd.write( ' force' )
		elif( self.code == 2 ):
			fd.write( ' freq=noraman' )
		fd.write( ' pop=chelpg fchk\n\n- slave -\n\n%d %d\n'%( self.charge, self.multip ) )
		for i in xrange( self.naqm ):
			fd.write( '%-8s%20.10lf%20.10lf%20.10lf\n'%( 
				self.atmsmb[int(self.z[i])], self.cord[i*3], self.cord[i*3+1], self.cord[i*3+2] ) )
		fd.write( '\n' )
		if( self.natm == self.naqm ):
			fd.write( '%28.10lf%20.10lf%20.10lf%8.3lf\n'%( 999.0, 999.0, 999.0, .0 ) )
		for i in xrange( self.naqm, self.natm ):
			fd.write( '%28.10lf%20.10lf%20.10lf%8.3lf\n'%( 
				self.cord[i*3], self.cord[i*3+1], self.cord[i*3+2], self.z[i] ) )
		fd.write( '\n\n\n' )
		fd.close()

		os.system( '. /usr/local/Chem/g03c02/g03.profile; g03 calc' )
		time.sleep( 10 )
		sys.stderr.write( ' [done]\n' )
		fd = file( 'Test.FChk', 'rt' )
		g03_ener = .0
		g03_qfit = []
		g03_grad = []
		g03_hess = []
		line = fd.readline()
		while( line != '' ):
			if( line[0:12] == 'Total Energy' ):
				g03_ener = string.atof( string.split( line )[3] )
			if( self.code in [ 1, 2 ] and line[0:18] == 'Cartesian Gradient' ):
				i = 0
				j = int(3*self.naqm/5)
				j += (3*self.naqm)%5 != 0
				while( i < j ):
					line = fd.readline()
					for itm in string.split( line ):
						g03_grad.append( string.atof( itm ) )
					i += 1
# - Don't compromise the GUESS -------------------------
				tmp = .0
				for i in xrange( 3*self.naqm ):
					tmp += g03_grad[i] ** 2
				tmp = ( tmp / 3*self.naqm ) ** .5
				if( not self.grms ):
					self.grms = tmp
				elif( tmp < 5. * self.grms ):
					self.grms = tmp
				else:
					os.unlink( 'calc.chk' )
# ------------------------------------------------------
				if( self.code == 2 ):
					line = fd.readline()
					i = 0
					j = int((3*self.naqm*(3*self.naqm+1)/2)/5)
					j += (3*self.naqm*(3*self.naqm+1)/2)%5 != 0
					while( i < j ):
						line = fd.readline()
						for itm in string.split( line ):
							g03_hess.append( string.atof( itm ) )
						i += 1
			if( line[0:11] == 'ESP Charges' ):
				i = 0
				j = int(self.naqm/5)
				j += self.naqm%5 != 0
				while( i < j ):
					line = fd.readline()
					for itm in string.split( line ):
						g03_qfit.append( string.atof( itm ) )
					i += 1
			line = fd.readline()
		fd.close()

		fd = file( 'calc.log', 'rt' )
		line = fd.readline()
		flag = True
		while( line != '' and flag ):
			if( line[0:29] == ' Self energy of the charges =' ):
				flag = False
				g03_ener -= string.atof( string.split( line )[6] )
			line = fd.readline()
		fd.close()

		os.unlink( 'Test.FChk' )
		self.save( sckt, g03_ener, g03_qfit, g03_grad, g03_hess )



if( __name__ == '__main__' ):
	G03( method = 'b3lyp/6-311g(d,p)', memory = '128mb' ).run()
