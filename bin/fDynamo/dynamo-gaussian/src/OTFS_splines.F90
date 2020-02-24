module OTFS_splines

public
    
contains

    
    real*8 function spline_value( x, coef )
    
        real*8, intent(in) :: x
        real*8, dimension(:,:),intent(in) :: coef
        
        integer :: idx
        real*8 :: x_max, y_max, k
        
        idx = size( coef, dim = 1 )
       
        if( coef(idx,2) /= 0._8 ) then
        x_max = coef(idx,1)-coef(idx,3)/(coef(idx,2)*3) !Since natural Cubic Spines are used
        else
            x_max = coef(idx,1)*2-coef(idx-1,1)
        end if
        if( x < coef(1,1) ) then
            spline_value = coef(1,4)*(x-coef(1,1)) + &
                           coef(1,5)
            return
        elseif( x > x_max ) then
            k = spline_der( x_max, coef )
            y_max = coef(idx,2)*(x_max-coef(idx,1))**3 + &
                    coef(idx,3)*(x_max-coef(idx,1))**2 + &
                    coef(idx,4)*(x_max-coef(idx,1)) + &
                    coef(idx,5)
            spline_value = k*(x-x_max) + y_max
            return
        end if
        
        do while( x < coef(idx,1) )
            idx = idx - 1
        end do
        
        spline_value = coef(idx,2)*(x-coef(idx,1))**3 + &
                       coef(idx,3)*(x-coef(idx,1))**2 + &
                       coef(idx,4)*(x-coef(idx,1)) + &
                       coef(idx,5)
    
    end function spline_value
    
    
    
    real*8 function spline_der( x, coef ) !Derivative dy(x)/dx
    
        real*8, intent(in) :: x
        real*8, dimension(:,:),intent(in) :: coef
        
        integer :: idx
        
        idx = size( coef, dim = 1 )
        do while( x < coef(idx,1) )
            idx = idx - 1
        end do
        
        spline_der = coef(idx,2)*3*(x-coef(idx,1))**2 + &
                     coef(idx,3)*2*(x-coef(idx,1)) + &
                     coef(idx,4)
    
    end function spline_der
  
    
    real*8 function spline_integrate( x_min, x_max, coef )
    
        real*8, intent(in) :: x_min, x_max
        real*8, dimension(:,:),intent(in) :: coef
        
        real*8 :: tmp
        
        integer :: idx_min, idx_max, i
        
        idx_min = size( coef, dim = 1 )
        do while( x_min < coef(idx_min,1) )
            idx_min = idx_min - 1
        end do
        
        idx_max = size( coef, dim = 1 )
        do while( x_max < coef(idx_max,1) )
            idx_max = idx_max - 1
        end do
        
        tmp = 0._8
        
        if( idx_min == idx_max ) then
        
            tmp = tmp + coef(idx_min,2)*((x_max-coef(idx_max,1))**4-(x_min-coef(idx_min,1))**4)/4._8 + &
                        coef(idx_min,3)*((x_max-coef(idx_max,1))**3-(x_min-coef(idx_min,1))**3)/3._8 + &
                        coef(idx_min,4)*((x_max-coef(idx_max,1))**2-(x_min-coef(idx_min,1))**2)/2._8 + &
                        coef(idx_min,5)*((x_max-coef(idx_max,1))-(x_min-coef(idx_min,1)))
                      
        else
        
            tmp = tmp + coef(idx_max,2)*((x_max-coef(idx_max,1))**4)/4._8 + &
                        coef(idx_max,3)*((x_max-coef(idx_max,1))**3)/3._8 + &
                        coef(idx_max,4)*((x_max-coef(idx_max,1))**2)/2._8 + &
                        coef(idx_max,5)*((x_max-coef(idx_max,1)))
            tmp = tmp + coef(idx_min,2)*((coef(idx_min+1,1)-coef(idx_min,1))**4-(x_min-coef(idx_min,1))**4)/4._8 + &
                        coef(idx_min,3)*((coef(idx_min+1,1)-coef(idx_min,1))**3-(x_min-coef(idx_min,1))**3)/3._8 + &
                        coef(idx_min,4)*((coef(idx_min+1,1)-coef(idx_min,1))**2-(x_min-coef(idx_min,1))**2)/2._8 + &
                        coef(idx_min,5)*((coef(idx_min+1,1)-coef(idx_min,1))-(x_min-coef(idx_min,1)))
            
            do i = idx_min+1,idx_max-1
                tmp = tmp + coef(i,2)*(coef(i+1,1)-coef(i,1))**4/4._8 + &
                            coef(i,3)*(coef(i+1,1)-coef(i,1))**3/3._8 + &
                            coef(i,4)*(coef(i+1,1)-coef(i,1))**2/2._8 + &
                            coef(i,5)*(coef(i+1,1)-coef(i,1))             
            end do
        
        end if
        
        spline_integrate = tmp
    
    end function spline_integrate



    subroutine CubicSplines( n, x, y, coef )
    
        integer, intent(in) :: n
        real*8, dimension(n), intent(in) :: x, y
        real*8, dimension(n-1,5), intent(out) :: coef

        integer :: i
    
        real*8, dimension(n-1) :: h
        real*8, dimension(n-2) :: d, b
        real*8, dimension(n) :: s



        do i = 1, n-1
            h(i) = x(i+1)-x(i)
        end do
        
        do i = 1, n-2
            d(i) = (h(i)+h(i+1))*2
            b(i) = (y(i+2)-y(i+1))/h(i+1) - (y(i+1)-y(i))/h(i)
        end do
        b = b * 6
        
        s(1) = 0._8
        s(n) = 0._8
        s(2:n-1) = Thomas( n-2, d, h(1:n-2), h(2:n-1), b )
        
        coef(:,1) = x(1:n-1)
        coef(:,2) = (s(2:n)-s(1:n-1))/(h(1:n-1)*6)
        coef(:,3) = s(1:n-1)/2._8
        coef(:,4) = (y(2:n)-y(1:n-1))/h(1:n-1) - ((s(2:n)+2*s(1:n-1))/6._8)*h(1:n-1)
        coef(:,5) = y(1:n-1)

    end subroutine CubicSplines



    subroutine TensionSplines( n, x, y, coef, smooth )

        integer, intent(in) :: n
        real*8, intent(in) :: smooth
        real*8, dimension(n), intent(in) :: x, y
        real*8, dimension(n-1,5), intent(out) :: coef

        integer :: i, ierr

        real*8, dimension(n-1) :: h
        real*8, dimension(n) :: s, d
        real*8, dimension(n) :: ys
        real*8, dimension(n*9) :: temp
        do i = 1, n-1
            h(i) = x(i+1)-x(i)
        end do
d = 0.003_8
        !Uses double precision FITPACK
        call curvs( n, x, y, d, 0, smooth, sqrt( 2._8/real( n, kind = 8 ) ), ys, s, 0._8, temp, ierr )

        coef(:,1) = x(1:n-1)
        coef(:,2) = (s(2:n)-s(1:n-1))/(h(1:n-1)*6)
        coef(:,3) = s(1:n-1)/2._8
        coef(:,4) = (y(2:n)-y(1:n-1))/h(1:n-1) - ((s(2:n)+2*s(1:n-1))/6._8)*h(1:n-1)
        coef(:,5) = ys(1:n-1)

    end subroutine TensionSplines




    subroutine TensionSplinesSigma( n, x, y, d, coef )

        integer, intent(in) :: n
        real*8, dimension(n), intent(in) :: d
        real*8, dimension(n), intent(in) :: x, y
        real*8, dimension(n-1,5), intent(out) :: coef

        integer :: i, ierr

        real*8, dimension(n-1) :: h
        real*8, dimension(n) :: s
        real*8, dimension(n) :: ys
        real*8, dimension(n*9) :: temp
        do i = 1, n-1
            h(i) = x(i+1)-x(i)
        end do

        !Uses double precision FITPACK
        call curvs( n, x, y, d, 0, real( n, kind = 8 ), sqrt( 2._8/real( n, kind = 8 ) ), ys, s, 0._8, temp, ierr )

        coef(:,1) = x(1:n-1)
        coef(:,2) = (s(2:n)-s(1:n-1))/(h(1:n-1)*6)
        coef(:,3) = s(1:n-1)/2._8
        coef(:,4) = (y(2:n)-y(1:n-1))/h(1:n-1) - ((s(2:n)+2*s(1:n-1))/6._8)*h(1:n-1)
        coef(:,5) = ys(1:n-1)

    end subroutine TensionSplinesSigma


    subroutine smooth_ND( init_set, final_set, n )

      real*8, dimension(:,:), intent(in) :: init_set
      real*8, dimension(:,:), intent(out) :: final_set
      integer, intent(in) :: n

      integer :: ninit, nfinal, nd
      integer :: i, j
      real*8  :: xmin, xmax, xval

      real*8, dimension(n-1,5) :: coef
      real*8, dimension(:), allocatable :: x

      ninit = size( init_set, dim = 2 )
      nfinal = size( final_set, dim = 2 )
      nd = size( init_set, dim = 1 )

      allocate( x(ninit) )

      do i = 1, ninit
          x(i) = i-1
      end do

      do i = 1, nd
          
          call Cubic_Splines_fit( x, init_set(i,:), n, coef )
          do j = 1, nfinal
              xval = (j-1)*real( ninit-1, kind = 8 )/real( nfinal-1, kind = 8 )
              final_set(i,j) = spline_value( xval, coef )
          end do

      end do

      deallocate( x )

  end subroutine smooth_ND


  subroutine Cubic_Splines_fit( a, b, n, coef )
  
    real*8, dimension(:), intent(in) :: a, b	  !Vectors with coordinates of data points
    integer,intent(in) :: n			  !n-1: number of splines
    real*8, dimension(n-1,5), intent(out) :: coef !coef(:,1) - x_i values, coef(:,2:5) - spline coefficients
    
    integer :: np	!number of data points
    integer :: i, j, idx
    real*8 :: h		! x_i+1 - x_i
    real*8, dimension(n) :: x, y	!Coordinates of spline reference points
    integer, dimension(:), allocatable :: aidx	!indexes of splines to which each data point belongs
    real*8, dimension(n) :: Mval !Final M values
    real*8, dimension(n,n) :: M  !m = My
    real*8, dimension(:,:), allocatable :: C, CT  !C_ji = d/dy_i S(a_j)
    real*8, dimension(n,n+1) :: Cp !Coefficient matrix for the final equation system
    
    np = size( a )
    allocate( aidx(np), C(np,n), CT(n,np) )  
    
    x(1) = minval( a )
    x(n) = maxval( a )
    h = (x(n) - x(1))/real( n-1, kind = 8 )
    
    do i = 2, n-1
      x(i) = x(1) + h*(i-1)
    end do
    
    aidx = min( int( (a-x(1))/h ) + 1, n-1 ) !Defining, to which spline each data point belongs

    M = SplineMatrix( n, h )

    !---Expressing s(a_i) as a linear combination of y---
    !----and putting the coefficients into the matrix----
    C = 0.
    do i = 1, np
      idx = aidx(i)
      do j = 1, n
        C(i,j) = (a(i)-x(idx))**3/(6*h)*(M(idx+1,j)-M(idx,j))+&
	       (a(i)-x(idx))**2/2._8*M(idx,j)-&
	       (a(i)-x(idx))*h/6._8*(M(idx+1,j)+M(idx,j)*2)
      end do
      C(i,idx) = C(i,idx) + 1-(a(i)-x(idx))/h
      C(i,idx+1) = C(i,idx+1) + (a(i)-x(idx))/h
    end do
    !----------------------------------------------------
    
    !---Finding y by solving the least squares equation system---
    CT = transpose( C )
    Cp(:,1:n) = matmul( CT, C )
    do i = 1, n
      Cp(i,n+1) = dot_product( CT(i,:), b )
    end do

    if( solve_GJ( Cp, y ) ) then

      do i = 1, n
        Mval(i) = dot_product( M(i,:), y )
      end do
      
      coef(:,1) = x(1:n-1)
      coef(:,2) = (Mval(2:n)-Mval(1:n-1))/(6*h)
      coef(:,3) = Mval(1:n-1)/2.
      coef(:,4) = (y(2:n)-y(1:n-1))/h - ((Mval(2:n)+2*Mval(1:n-1))/6.)*h
      coef(:,5) = y(1:n-1)
    
    else
      write(*,*) "Error: Gauss-Jordan failed!"
    end if
    !------------------------------------------------------------
    
    deallocate( aidx, C, CT )
  
  end subroutine Cubic_Splines_fit



  function SplineMatrix( n, h ) 
    
    ! Generates the matrix M with coefficients of m expansion as a linear combination
    ! of y: m = My, m - (second derivative of spline)/2, all other coefficients can
    ! be expressed as a function of m and y.
    
    integer, intent(in) :: n
    real*8, intent(in) :: h
    real*8, dimension(n,n) :: SplineMatrix
    real*8, dimension(n-2,n-2) :: X
    integer, dimension(n-2,n) :: Y
    
    integer :: i, j
    real*8, parameter  :: lambda = 1.31695789692482_8

    do i = 1, n-2
      do j = 1, n-2
	X(i,j) = (1-2*iand( i+j, 1 ))*( cosh( (n-1-abs(j-i))*lambda ) - cosh( (n-1-i-j)*lambda ) )/(2*sinh(lambda)*sinh((n-1)*lambda))
      end do
    end do
    
    Y = 0.
    do i = 1, n-2
      Y(i,i) = 1
      Y(i,i+1) = -2
      Y(i,i+2) = 1
    end do
    
    SplineMatrix = 0.
    SplineMatrix(2:n-1,:) = matmul( X, Y )*(6/h**2)
    
    return
    
  end function SplineMatrix
  
  

  function solve_GJ( A, sol ) result( success )
  
  ! solves the linear equation system A using the Gauss-Jordan method
  ! Returns .true. if the system was solved succesfully, in other case returns .false.
  !
  ! sol - vector for solutions

    real*8, dimension(:,:), intent(in)  :: A
    real*8, dimension(:),   intent(out) :: sol
    real*8, dimension(:,:), allocatable :: tmp
    logical :: success
    
    integer :: i, j, n, imax
    
    n = size( A, dim = 1 )
    
    allocate( tmp(n+1, n) )
    
    tmp = 0._8
    tmp = transpose( A )  !transposing the matrix for efficiency (working with columns is faster)

    do i = 1, n
    
      !----ensure largest diagonal elements and check for singularity----    
      imax = i      
      do j = i+1,n
        if ( tmp(i, j) > tmp(i, i) ) imax = j
      end do   
      if ( tmp(i, imax) == 0 ) then
	success = .false.
        return      
      end if      
      if ( imax > i ) call exchg_col( tmp, i, imax )
      !------------------------------------------------------------------      
      
      tmp(i:, i) = tmp(i:, i) / tmp(i, i)
      do j = i+1, n
        tmp(i:, j) = tmp(i:, j) - tmp(i:, i)*tmp(i, j)
      end do
      
    end do    
    
    !now the matrix tmp(1:n, 1:n) is upper-diagonal
    
    do i = n, 2, -1
      tmp(:, i) = tmp(:, i)/tmp(i,i)
      do j = 1, i-1
        tmp(i:, j) = tmp(i:, j) - tmp(i:,i)*tmp(i, j)
      end do
    end do
    sol = tmp(n+1, :)
    success = .true.

    deallocate( tmp )
    return

  end function solve_GJ



    function Thomas( n, d, subd, supd, b )
    !Solves a tridiagonal system of linear equation
    !n - number of equations
    !d - main diagonal elements
    !subd - sub-diagonal elements
    !supd - sup-diagonal elements
    !b - right side values
    !WARNING! d and b values are not conserved!
    
        integer, intent(in) :: n
        real*8, dimension(:), intent(inout) :: d, subd, supd, b
        real*8, dimension(n) :: Thomas
        real*8, dimension(:), allocatable :: c
        
        integer :: i
        real*8  :: tmp
        
        do i = 2, n
            tmp = subd(i)/d(i-1)
            d(i) = d(i) - tmp*supd(i-1)
            b(i) = b(i) - tmp*b(i-1)
        end do
        
        Thomas(n) = b(n)/d(n)

        do i = n-1, 1, -1
            Thomas(i) = (b(i)-supd(i)*Thomas(i+1))/d(i)
        end do
        
        return
      
    end function Thomas
    
    
      !===Gauss-Jordan matrix inversion===
    
    logical function inverse_GJ( A, Ainv ) result( success )
    
    ! Calculates the inverse of a matrix using the Gauss-Jordan method.
    ! Returns .true. if the inverse was calculated succesfully, in other case returns .false.
    
        real*8, dimension(:,:), intent(in) :: A
        real*8, dimension(:,:), intent(out) :: Ainv
        real*8, dimension(:,:), allocatable :: tmp
        
        integer :: i, j, n, imax
        
        n = size( A, dim = 1 )
        
        allocate( tmp(n*2, n) )
        
        tmp = 0._8
        tmp(1:n,1:n) = transpose( A )  !transposing the matrix for efficiency (working with columns is faster)
        
        do i = 1, n
          tmp(i+n, i) = 1._8
        end do

        do i = 1, n

            !----ensure largest diagonal elements by the exchanges of columns----    
            imax = i      
            do j = i+1,n
                if ( tmp(i, j) > tmp(i, i) ) imax = j
            end do
        
            if ( tmp(i, imax) == 0 ) then  !check for singularity
                success = .false.
                return      
            end if      
            if ( imax > i ) call exchg_col( tmp, i, imax )
            !--------------------------------------------------------------------      
        
            tmp(i:, i) = tmp(i:, i) / tmp(i, i)
            do j = i+1, n
                tmp(i:, j) = tmp(i:, j) - tmp(i:, i)*tmp(i, j)
            end do
          
        end do
        
        !now the matrix tmp(1:n, 1:n) is upper-diagonal
        
        do i = n, 2, -1
            tmp(:, i) = tmp(:, i)/tmp(i,i)
            do j = 1, i-1
                tmp(i:, j) = tmp(i:, j) - tmp(i:,i)*tmp(i, j)
            end do
        end do
        
        Ainv = transpose( tmp(n+1:, :) )
        success = .true.
        deallocate( tmp )

        return

    end function inverse_GJ
    !===================================
    
    
    
    !===================================
    subroutine exchg_col( A, i, j )
    
    !exchanges columns i and j of the matrix A
    
        real*8, dimension(:,:), intent(inout) :: A
        integer, intent(in) :: i, j
        real*8, dimension(:), allocatable :: tmp
        
        allocate( tmp(size(A, dim = 1)) )
        
        tmp = A(:, i)    
        A(:, i) = A(:, j)
        A(:, j) = tmp
        
        deallocate( tmp ) 
        
        return
    
    end subroutine exchg_col
    !===================================    


end module OTFS_splines
