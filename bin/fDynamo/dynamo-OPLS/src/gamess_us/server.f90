subroutine gamess_server( code, natm, naqm, atmn, cord, ener, grad, hess )
	implicit none
	integer, intent( in ) :: code, natm, naqm
	real*8, dimension(1:*), intent( in ) :: atmn
	real*8, dimension(1:*), intent( in ) :: cord
	real*8, intent( inout ) :: ener
	real*8, dimension(1:*), intent( inout ) :: grad
	real*8, dimension(1:*), intent( inout ) :: hess

	integer :: system, i, j, k, l, m, n3, nh, istat
	character( len=256 ) :: str
	real*8, dimension(:), allocatable :: tmp

	real*8, parameter :: bohr = 0.529177249d0
	character( len=2 ), dimension(1:109), parameter :: smb = (/ &
			"H ", "He", "Li", "Be", "B ", "C ", "N ", "O ", "F ", "Ne", "Na", "Mg", &
			"Al", "Si", "P ", "S ", "Cl", "Ar", "K ", "Ca", "Sc", "Ti", "V ", "Cr", "Mn", "Fe", &
			"Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y ", "Zr", &
			"Nb", "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I ", "Xe", &
			"Cs", "Ba", "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", &
			"Tm", "Yb", "Lu", "Hf", "Ta", "W ", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", &
			"Bi", "Po", "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", "U ", "Np", "Pu", "Am", "Cm", &
			"Bk", "Cf", "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt" /)

	open( unit=999, file = "guess", action = "read", form = "unformatted", iostat = istat )
	close( 999 )

	open( unit=999, file = "input", action = "write", form = "formatted" )
	write( 999, "(a)" ) " $contrl"
	if( code == 1 ) then
		write( 999, "(a)" ) "runtyp=gradient"
	else if( code == 2 ) then
		write( 999, "(a)" ) "runtyp=hessian"
	else
		write( 999, "(a)" ) "runtyp=energy"
	end if
	write( 999, "(a)" ) "coord=cart"
	write( 999, "(a)" ) "nosym=1"
	write( 999, "(a)" ) "nprint=7"
	write( 999, "(a)" ) "units=angs"
	write( 999, "(a)" ) "maxit=200"
	write( 999, "(a)" ) "scftyp=rhf"
	write( 999, "(a)" ) "icharg=-1"
	write( 999, "(a)" ) "mult=1"
	write( 999, "(a)" ) "mplevl=0"
	write( 999, "(a)" ) "dfttyp=b3lyp"
	if( istat == 0 ) write( 999, "(a)" ) "irest=-1"
	write( 999, "(a)" ) " $end"

	if( istat == 0 ) then
		write( 999, "(a)" ) " $guess"
		write( 999, "(a)" ) "guess=mosaved"
		write( 999, "(a)" ) " $end"
	end if

	write( 999, "(a)" ) " $scf"
	write( 999, "(a)" ) "dirscf=.true."
	write( 999, "(a)" ) "conv=1.d-6"
	write( 999, "(a)" ) " $end"
	write( 999, "(a)" ) " $system"
	write( 999, "(a)" ) "mwords=70"
	write( 999, "(a)" ) " $end"
	write( 999, "(a)" ) " $basis"
	write( 999, "(a)" ) "gbasis=n31"
	write( 999, "(a)" ) "ngauss=6"
	write( 999, "(a)" ) "ndfunc=1"
	write( 999, "(a)" ) "npfunc=0"
	write( 999, "(a)" ) "diffsp=.false."
	write( 999, "(a)" ) "diffs=.false."
	write( 999, "(a)" ) " $end"
	write( 999, "(a)" ) " $data"
	write( 999, "(a)" ) "slave"
	write( 999, "(a)" ) "c1"
	do i = 1, naqm
		j = ( i - 1 ) * 3
		write( 999, "(a4,f8.1,3f20.10)" ) smb(int(atmn(i))), atmn(i), cord(j+1), cord(j+2), cord(j+3)
	end do
	write( 999, "(a)" ) " $end"
	close( 999 )

	m = natm - naqm
!	if( m > 0 ) then
		open( unit=999, file = "mm_charges", action = "write", form = "unformatted" )
		write( 999 ) m
		do i = naqm + 1, natm
			j = ( i - 1 ) * 3
			write( 999 ) cord(j+1), cord(j+2), cord(j+3), atmn(i)
!			do k = i + 1, natm
!				l = ( k - 1 ) * 3
!			end do
		end do
		close( 999 )
!	end if

	i = system( "./gamess.sh" )

	allocate( tmp(1:m) )
	open( unit = 999, file = "mm_output", action = "read", form = "unformatted" )
	read( 999, end = 999 ) ener
	n3 = naqm * 3
	nh = n3 * ( n3 + 1 ) / 2
	if( code == 1 .or. code == 2 ) then
		read( 999, end = 999 ) grad(1:n3)
		read( 999, end = 999 ) tmp(1:m)
		do i = 1, m
			j = ( i - 1 ) * 3
			grad( n3 + j + 1 ) = tmp(i)
		end do
		read( 999, end = 999 ) tmp(1:m)
		do i = 1, m
			j = ( i - 1 ) * 3
			grad( n3 + j + 2 ) = tmp(i)
		end do
		read( 999, end = 999 ) tmp(1:m)
		do i = 1, m
			j = ( i - 1 ) * 3
			grad( n3 + j + 3 ) = tmp(i)
		end do
	end if
	if( code == 2 ) read( 999, end = 999 ) hess(1:nh)
999	continue
	close( 999 )
	deallocate( tmp )

end subroutine
