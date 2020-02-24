!===============================================================================
!                          The String method Module
!===============================================================================
!
! The details about the method can be found in Chem. Phys. Lett., 446 (2007), 182-190
! 
! 
! . Subroutines:
!
!   OTF_string_mpi_define                  Sets up the string method calculation
!   OTF_string_mpi_launch                  Launchs the string method calculation
!
!===============================================================================
module OTF_string_mpi

#ifdef	HACK_OTFS

! . Module declarations.
    use definitions,        only : dp
    use files,              only : next_unit
    use atoms,              only : atmcrd, atmmas, natoms
    use string,             only : encode_integer
    use files,              only : next_unit
    use constants,          only : R
    use coordinate_io,      only : coordinates_write
    use velocity,           only : atmvel, velocity_write
    use time,               only : time_print
    use random_numbers,     only : random
    use velocity,           only : atmvel
    use mopac_data,         only : denmat
    use energy_non_bonding, only : update

    use OTFS_splines
    
    include "mpif.h"

    private
    
    public :: OTF_string_mpi_define, OTF_string_mpi_prepare, OTF_string_mpi_calc, OTF_string_mpi_launch
    public :: OTF_string_REX_setup, average_string, get_FEP
    public :: OTF_string_REX
    save

    integer :: nCV, nact, nnodes !Number of CVs, number of atoms in active space, number of nodes
    integer :: node_idx !Simulation node
    
    integer :: string_freq = 1 !String update frequency (timesteps)
    integer :: output_freq = 100 !Data output frequency (string, convergence, restart file) (timesteps)
    integer :: rex_freq = 100 !Timesteps between replica exchanges
    
    integer :: step !Current step of dynamics 
    integer :: preparation_steps !Number of preparation steps
    integer :: ra_window_size !Running average window
    integer :: node_unit, convergence_unit, M_unit, string_unit, rex_unit, pos_unit, restart_unit !Units for output data writing
    integer :: ierr !To avoid redefining it everywhere
    integer :: size_denmat

    integer, dimension(mpi_status_size) :: status !For MPI

    real*8  :: g, g_init !gamma
    real*8  :: temp !Temperature
    real*8  :: k_pos !coefficient for node position optimization
    real*8  :: k_fc !coefficient for force constant optimization (not to be confused with Kentucky Fried Chicken)
    real*8  :: string_length
    real*8  :: dynamics_delta
    
    logical :: box, server !Whether the box is used, Whether the current node is the server
    logical :: string_defined = .false., OTF_string_preparation = .false., REX_active = .false.
    logical :: useQM = .false.
    
    character*200 :: dir !Directory for output data and data shared by all jobs

    character*20 :: sidx, sstep !String containing index of the simulation node
    character*20 :: CV_frmt, CV2_frmt, pos_frmt !Format for formatted printing of CV vectors
    
    integer, dimension(:), allocatable :: actidx !Indices of atoms in active space
    real*8, dimension(:), allocatable :: box_min, box_max !Box walls
    
    real*8, dimension(:), allocatable :: actmas !Reciprocal masses of atoms in active space
    real*8, dimension(:), allocatable :: actcrd !xyz coordinates of atoms in active space
    real*8, dimension(:), allocatable :: node_CV, CV !Coordinates of the string node, current CVs
    real*8, dimension(:,:), allocatable :: nodes, tmp_nodes, init_nodes !Initial coordinates of all nodes
    real*8, dimension(:,:), allocatable :: M, Jacobian
    real*8, dimension(:,:), allocatable :: REX_tmp !For coordinate and velocity exchange
    real*8, dimension(:), allocatable :: ra_window, K_init, K_final !Running average window, initial and final force constants,
    real*8, dimension(:), allocatable :: string_thr !Largest allowed displacement of the string image in each dimension during 1 timestep
    real*8, dimension(:), allocatable :: K !Vector of force constants
    real*8, dimension(:), allocatable :: node_positions !To equalize the exchange probabilities between replicas
    real*8, dimension(:,:), allocatable :: crdtmp, denmattmp !temporary arrays for coordinates/velocities and density matrix for REX

!==============================================================================
CONTAINS
!==============================================================================


  !==================================================================
    subroutine OTF_string_mpi_define( temperature,&
                                      file_definition,& !File with initial string
                                      directory,& !Directory where all the temporary and output files will be located
                                      number_of_nodes,&
                                      output_frequency,&
                                      timestep,&
                                      use_box ) !Default = .true.

        real*8, intent(in) :: temperature                                       
        character( len=* ), intent(in) :: file_definition
        character( len=* ), intent(in) :: directory
        integer, intent(in), optional :: output_frequency
        real*8, intent(in) :: timestep
        logical, intent(in), optional :: use_box
        integer, intent(in), optional :: number_of_nodes !If only analysis is done
        
        integer :: i, j, init_unit, nodes_number

        !---Define parameters---
        dir = directory
        temp = temperature
        dynamics_delta = timestep
        if( present( number_of_nodes ) ) then
            nodes_number = number_of_nodes
            node_idx = 2
        else
            call mpi_comm_rank( mpi_comm_world, node_idx, ierr )
            call mpi_comm_size( mpi_comm_world, nodes_number, ierr )
            node_idx = node_idx + 1
        end if

        server = .false.
        if( node_idx == 1 ) server = .true.
        write(sidx,*) node_idx
        sidx = adjustl( sidx )
        !-----------------------

        output_freq = 100
        if( present( output_frequency ) ) output_freq = output_frequency

        init_unit = next_unit()
        open( unit = init_unit, file = file_definition, status = "old" )
        read(init_unit,*) nCV, nnodes, nact

        allocate( actidx(nact), actmas(nact*3), actcrd(nact*3) )!, string_splines(nodes_number-1,5,nCV) )
        read(init_unit,*) actidx
        do i = 1, nact
          actmas(i*3-2:i*3) = 1._dp/atmmas(actidx(i))
        end do

        write(CV_frmt,*) nCV
        CV_frmt = "("//trim( adjustl( CV_frmt ) )//"F20.10)"
        write(CV2_frmt,*) nCV*2
        CV2_frmt = "("//trim( adjustl( CV2_frmt ) )//"F20.10)"
        write(pos_frmt,*) nodes_number
        pos_frmt = "("//trim( adjustl( pos_frmt ) )//"F20.10)"
        allocate( node_CV(nCV), CV(nCV), Jacobian(nCV,nact*3), K(nCV), M(nCV,nCV) )
        box = .true.
        if( present( use_box ) ) box = use_box
  
        !---Reading box if needed---
        if( box ) then
          allocate( box_min(nCV), box_max(nCV) )
          do i = 1, nCV
          read(init_unit,*) box_min(i), box_max(i)
          end do
        end if
        !---------------------------

        !---Initialize distance positions---
        !if( server ) then
            allocate( node_positions(nodes_number) )
            do i = 1, nodes_number
                node_positions(i) = i-1
            end do
        !end if
        !-----------------------------------------------------
  
        !---Read initial nodes and interpolate if needed---
        !if( nodes_number /= nnodes ) then
            allocate( nodes(nCV,nodes_number), tmp_nodes(nCV,nnodes) )

            read(init_unit,*) tmp_nodes
            if( nnodes < 4 ) then
              call distribute_nodes( tmp_nodes, nodes )
            else

              call interpolate_splines( nnodes, nodes_number, node_positions, tmp_nodes, nodes )

            end if
            nnodes = nodes_number

            deallocate( tmp_nodes )

        !else
        !    allocate( nodes(nCV,nnodes) )
        !    read(init_unit,*) nodes
        !end if
        allocate( tmp_nodes(nCV,nnodes), init_nodes(nCV,nnodes) )
        init_nodes = nodes
        node_CV = nodes(:,node_idx)
        !----------------------------------------------------
        close( init_unit )
        string_defined = .true.        
        
        write(*,*)
        write(*,"(A)") "---OTF String optimization---"
        write(*,"(A15,I5)") "Number of CVs", nCV
        write(*,"(A15,I5)") "Number of nodes", nnodes
        write(*,"(A15,I5)") "Node", node_idx
        write(*,"(A)") "-----------------------------"
        write(*,*)
        write(*,*)
  
    end subroutine OTF_string_mpi_define
    !==================================================================
  


    !==================================================================
    !This subroutine sets up the preparation stage of the string method calculation
    !and launches the preparation itself.
    subroutine OTF_string_mpi_prepare( force_constants_final,& !Target k values
                                       force_constants_init,& !initial k values for the extended potential
                                       steps,& !Timesteps
                                       string_thresholds,& !Threshold for average change of the node position during 1 timestep
                                       running_average_window ) !Default = 100
        
        real*8, dimension(nCV), intent(in) :: force_constants_final
        real*8, dimension(nCV), intent(in), optional :: force_constants_init
        integer, intent(in) :: steps
        real*8, dimension(nCV), intent(in), optional :: string_thresholds
        integer, intent(in), optional :: running_average_window
        
        preparation_steps = steps
        step = 0
        g = 1._dp
        
        ra_window_size = 100
        if( present( running_average_window ) ) ra_window_size = running_average_window
        
        allocate( ra_window(ra_window_size), K_init(nCV), K_final(nCV) )
        
        if( present( string_thresholds ) ) then
            allocate( string_thr(nCV) )
            string_thr = string_thresholds
        end if
        
        K_init = 0._dp
        if( present( force_constants_init ) ) K_init = force_constants_init
        K = K_init
 
        K_final = force_constants_final
        
        OTF_string_preparation = .true.
    
    end subroutine OTF_string_mpi_prepare
    !==================================================================



    !==================================================================
    subroutine OTF_string_mpi_launch( restart_file,&
                                  string_frequency,&
                                  force_constants,&
                                  gamma )
       
        character( len=* ), intent(in), optional :: restart_file
        real*8, dimension(nCV), intent(in), optional :: force_constants
        integer, intent(in), optional :: string_frequency
        real*8, intent(in), optional :: gamma
        
        logical :: rest
        integer :: i, tmp
        character*10 :: file_status

        OTF_string_preparation = .false.
        
        step = 0
        if( present( force_constants ) ) K = force_constants
        string_freq = 1
        if( present( string_frequency ) ) string_freq = string_frequency
        rest = present( restart_file )

        !---In case of restart, reading the state of the system---
        if( rest ) then
       
            restart_unit = next_unit() 
            open( restart_unit, file = restart_file, status = "old" )
            read(restart_unit,*) step, g

            if( server ) then
                read(restart_unit,*) K, nodes, init_nodes, node_positions
                node_CV = nodes(:,1)
            else
                do i = 1, node_idx
                    read(restart_unit,*) K
                end do
                do i = node_idx+1, nnodes
                    read(restart_unit,*)
                end do
                do i = 1, node_idx
                    read(restart_unit,*) node_CV
                end do
            end if

            close( restart_unit )
            call restart_clean 

        end if
        !---------------------------------------------------------

        if( present( gamma ) ) g = gamma
        write(*,*) "gamma= ", g

        if( rest ) then
            file_status = "old"
        else
            file_status = "replace"
        end if
        
        if( server ) then
            convergence_unit = next_unit()
            open( unit = convergence_unit, file = trim( dir )//"convergence.dat", status = file_status, position = "append" )
        end if

        node_unit = next_unit()
        open( unit = node_unit, file = trim( dir )//trim( sidx )//".dat", status = file_status, position = "append" )
        M_unit = next_unit()
        open( unit = M_unit, file = trim( dir )//trim( sidx )//".M", status = file_status, position = "append" )        
    
    contains
    
    
        !==================================================================
        subroutine restart_clean
            
            integer :: i, tmp, unit
            character*20 :: sstep
            
            write(sstep,*) step
            sstep = adjustl( sstep )
            
            !---Cleaning  node file (node_idx.dat)---
                call system( "mv "//trim( dir )//trim( sidx )//".dat "//trim( dir )//trim( sidx )//".dat.tmp" )
                call system( &
                "head -n "//trim( sstep )//" "//trim( dir )//trim( sidx )//".dat.tmp > "//trim( dir )//trim( sidx )//".dat" )
                call system( "rm "//trim( dir )//trim( sidx )//".dat.tmp" ) 
            !----------------------------------------
            
            write(sstep,*) step*(nCV+1)
            sstep = adjustl( sstep )        
            
            !---Cleaning M matrix file---
                call system( "mv "//trim( dir )//trim( sidx )//".M "//trim( dir )//trim( sidx )//".M.tmp" )
                call system( &
                "head -n "//trim( sstep )//" "//trim( dir )//trim( sidx )//".M.tmp > "//trim( dir )//trim( sidx )//".M" )
                call system( "rm "//trim( dir )//trim( sidx )//".M.tmp" )           
            !----------------------------
        
            if( server ) then
            
                write(sstep,*) step/output_freq
                sstep = adjustl( sstep )        
            
                !---Cleaning convergence file---
                    call system( "mv "//trim( dir )//"convergence.dat "//trim( dir )//"convergence.dat.tmp" )
                    call system( &
                    "head -n "//trim( sstep )//" "//trim( dir )//"convergence.dat.tmp > "//trim( dir )//"convergence.dat" )
                    call system( "rm "//trim( dir )//"convergence.dat.tmp" )           
                !-------------------------------
                
                write(sstep,*) step/rex_freq
                sstep = adjustl( sstep )        
            
                !---Cleaning force constant file---
                    call system( "mv "//trim( dir )//"K.dat "//trim( dir )//"K.dat.tmp" )
                    call system( "head -n "//trim( sstep )//" "//trim( dir )//"K.dat.tmp > "//trim( dir )//"K.dat" )
                    call system( "rm "//trim( dir )//"K.dat.tmp" )            
                !-------------------------------           

                !---Cleaning positions file---
                    call system( "mv "//trim( dir )//"node_positions.dat "//trim( dir )//"node_positions.dat.tmp" )
                    call system( &
                "head -n "//trim( sstep )//" "//trim( dir )//"node_positions.dat.tmp > "//trim( dir )//"node_positions.dat" )
                    call system( "rm "//trim( dir )//"node_positions.dat.tmp" )
                !-----------------------------  
                
                !---Cleaning REX exchange statistics file---
                i = 0
                unit = next_unit()
                open( unit=unit, file = trim( dir )//"REX.dat", status = "old" )
                do
                    read(unit,*,iostat=ierr) tmp
                    if( ierr<0 ) exit
                    if( tmp > step ) exit
                    i = i + 1
                end do
                close( unit )
                
                write(sstep,*) i
                sstep = adjustl( sstep )                 

                call system( "mv "//trim( dir )//"REX.dat "//trim( dir )//"REX.dat.tmp" )
                call system( "head -n "//trim( sstep )//" "//trim( dir )//"REX.dat.tmp > "//trim( dir )//"REX.dat" )
                call system( "rm "//trim( dir )//"REX.dat.tmp" )               
                !-------------------------------------------
            
            end if
        
        
        end subroutine restart_clean
        !==================================================================    

        
    end subroutine OTF_string_mpi_launch
    !==================================================================
    
    
    
    !==================================================================
    subroutine OTF_string_mpi_calc( gradient )
    
        real*8, dimension(3,natoms), intent(inout) :: gradient
        real*8, dimension(nact*3) :: act_grad !Gradient of the potential in the active space
        !real*8, dimension(:) :: act_grad !Gradient of the potential in the active space
        
        integer :: i
        if( .not. string_defined ) return

        step = step + 1

        call update_active_space

        call get_CV( actcrd, CV, Jacobian ) !Must be defined in the main program
        act_grad = matmul( K*(CV - node_CV), Jacobian )


        do i = 1, nact
            gradient(:,actidx(i)) = gradient(:,actidx(i)) + act_grad(i*3-2:i*3)
        end do
        !if( mod( step, rex_freq ) == 0 ) call OTF_string_REX
        !call OTF_string_REX

        if( OTF_string_preparation ) then !Handle separately the preparation mode

            if( step <= preparation_steps-ra_window_size ) &
                K = K_init + step/real( preparation_steps-ra_window_size, kind = 8 )*(K_final - K_init)            
            if( allocated( string_thr ) ) ra_window(mod( step, ra_window_size )+1) = maxval( abs( delta_z()/string_thr ) )

            if( (step == preparation_steps) .and. (.not. server) .and. allocated( string_thr ) ) then
                call mpi_send( sum( ra_window )/real( ra_window_size, kind = 8 ), 1, mpi_real8, 0, 0, mpi_comm_world, ierr )
                call mpi_recv( g, 1, mpi_real8, 0, mpi_any_tag, mpi_comm_world, status, ierr )
                g_init = g
            end if

        else

            node_CV = node_CV + delta_z()
            if( box ) then !Keep the node in the box
              do i = 1, nCV
                node_CV(i) = min( node_CV(i), box_max(i) )
                node_CV(i) = max( node_CV(i), box_min(i) )
              end do
            end if
            
            if( .not. OTF_string_preparation ) call statistics_write
        
        end if

        if( server ) then
            call OTF_string_mpi_server
        elseif( (OTF_string_preparation .and. (mod( step, rex_freq ) == 0) ) .or. &
                (.not. OTF_string_preparation .and. (mod( step, string_freq ) == 0 ) ) ) then
            call mpi_send( node_CV, nCV, mpi_real8, 0, 0, mpi_comm_world, ierr )
            call mpi_recv( node_CV, nCV, mpi_real8, 0, mpi_any_tag, mpi_comm_world, status, ierr )
        end if


    contains
    

        !==================================================================
        subroutine statistics_write
        
            write(node_unit,CV2_frmt) CV, K*( node_CV - CV )
            flush( node_unit )
            write(M_unit,CV_frmt) M
            write(M_unit,*)
            flush(M_unit)
        
        end subroutine statistics_write
        !==================================================================
        
        
        !==================================================================
        subroutine OTF_string_mpi_server
            
            integer :: i
            real*8 :: tmp
            
            if( OTF_string_preparation ) then
            
                !---When preparation is finished, calculate average gamma (if needed)---
                if( (step == preparation_steps) .and. allocated( string_thr ) ) then
                    g = sum( ra_window )/real( ra_window_size, kind = 8 )
                    do i = 2, nnodes
                        call mpi_recv( tmp, 1, mpi_real8, i-1, mpi_any_tag, mpi_comm_world, status, ierr )
                        g = g + tmp
                    end do
                    g = g/real( nnodes, kind = 8 )
                    g_init = g
                    do i = 2, nnodes
                        call mpi_send( g, 1, mpi_real8, i-1, 0, mpi_comm_world, ierr )
                    end do
                end if
                !-----------------------------------------------------------------------
                
            end if
            
            
            if( (OTF_string_preparation .and. (mod( step, rex_freq ) == 0) ) .or. &
                (.not. OTF_string_preparation .and. (mod( step, string_freq ) == 0 ) ) ) then 
            
                !---Get node coordinates from all nodes---
                tmp_nodes(:,1) = node_CV
                do i = 2, nnodes
                    call mpi_recv( tmp_nodes(:,i), nCV, mpi_real8, i-1, mpi_any_tag, mpi_comm_world, status, ierr )
                end do
                !-----------------------------------------            

                call interpolate_splines( nnodes, nnodes, node_positions, tmp_nodes, nodes ) !Reparameterize    

                !---Send node coordinates to all nodes---
                node_CV = nodes(:,1)
                do i = 2, nnodes
                    call mpi_send( nodes(:,i), nCV, mpi_real8, i-1, 0, mpi_comm_world, ierr )
                end do                
                !----------------------------------------
                
            end if
            
            if( .not. OTF_string_preparation ) then
            
                if( mod( step, output_freq ) == 0 ) then          
                    call convergence_write
                    call string_write
                    call restart_write        
                end if
            
            end if
            
        end subroutine OTF_string_mpi_server
        !==================================================================
        
        
        !==================================================================
        subroutine convergence_write
        
            write(convergence_unit,"(I10,F20.10)") &
            step, sum( sqrt( sum( (nodes - init_nodes)**2, dim = 1 ) ) ) / real( nnodes, kind = 8 )
            
            flush( convergence_unit )
        
        end subroutine convergence_write
        !==================================================================    
        

        !==================================================================
        subroutine string_write
        
            write(sstep,*) step
            string_unit = next_unit()
            open( string_unit, file = trim( dir )//trim( adjustl( sstep ) )//".string" )
            write(string_unit,CV_frmt) nodes
            
            close( string_unit )
        
        end subroutine string_write
        !==================================================================    
        
        
        !==================================================================
        subroutine restart_write
        
            write(sstep,*) step
            restart_unit = next_unit()
            open( restart_unit, file = trim( dir )//"restart_"//trim( adjustl( sstep ) )//".dat", status = "replace" )
            
            write(restart_unit,"(I10)") step
            write(restart_unit,"(F20.10)") g
            write(restart_unit,CV_frmt) K
            write(restart_unit,CV_frmt) nodes
            write(restart_unit,CV_frmt) init_nodes
            write(restart_unit,pos_frmt) node_positions
            
            close( restart_unit )
        
        end subroutine restart_write
        !==================================================================                
    
    
    end subroutine OTF_string_mpi_calc
    !==================================================================
    
    
    !==================================================================
    subroutine OTF_string_REX_setup( frequency & !Timesteps between replica exchanges, Default = 100
                                  )!  position_factor, & !To equalize the exchange probabilities between replicas, Default = 1.01
                                    ! force_factor ) !Default = 1.01
    
        integer, intent(in), optional :: frequency
!        real*8, intent(in), optional  :: position_factor
!        real*8, intent(in), optional  :: force_factor
        
        rex_freq = 100
        if( present( frequency ) ) rex_freq = frequency
!        k_pos = 0._dp
!        if( present( position_factor ) ) k_pos = position_factor
!        k_fc = 0._dp
!        if( present( force_factor ) ) k_fc = force_factor

        allocate( REX_tmp(3,natoms) )    

        if( allocated( denmat ) ) then
            useQM = .true.
            size_denmat = size( denmat )
            allocate( denmattmp(size( denmat, dim = 1 ),size( denmat, dim = 2 )) )
        end if

        REX_active = .true.
    
    end subroutine OTF_string_REX_setup
    !==================================================================
    
    
    
    !==================================================================
    subroutine OTF_string_REX( atmacc )
    
        real*8, dimension(3,natoms), intent(inout) :: atmacc
        integer :: exchange !If > -1, id of the replica to exchange with.

        if( .not. REX_active ) return
        if( OTF_string_preparation ) return
        if( .not. (mod( step, rex_freq ) == 0) ) return

        !---Send data to the server---
        if( server ) then
            call OTF_string_REX_server( exchange )
        else
            !Since force constants are the same in all nodes, only CVs have to be sent
            call mpi_send( CV, nCV, mpi_real8, 0, 0, mpi_comm_world, ierr )
            call mpi_recv( exchange, 1, mpi_integer, 0, mpi_any_tag, mpi_comm_world, status, ierr )
        end if
        !-----------------------------

        if( exchange > -1 ) then
        
            if( exchange+1 > node_idx ) then
                call mpi_send( atmcrd, natoms*3, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                call mpi_recv( atmcrd, natoms*3, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                call mpi_send( atmvel, natoms*3, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                call mpi_recv( atmvel, natoms*3, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )               
                call mpi_send( atmacc, natoms*3, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                call mpi_recv( atmacc, natoms*3, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                if( useQM ) then
                    call mpi_send( denmat, size_denmat, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                    call mpi_recv( denmat, size_denmat, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                end if 
            else
                call mpi_recv( REX_tmp, natoms*3, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                call mpi_send( atmcrd,  natoms*3, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                atmcrd = REX_tmp
                call mpi_recv( REX_tmp, natoms*3, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                call mpi_send( atmvel,  natoms*3, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                atmvel = REX_tmp            
                call mpi_recv( REX_tmp, natoms*3, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                call mpi_send( atmacc,  natoms*3, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                atmacc = REX_tmp
                if( useQM ) then
                    call mpi_recv( denmattmp, size_denmat, mpi_real8, exchange, mpi_any_tag, mpi_comm_world, status, ierr )
                    call mpi_send( denmat,    size_denmat, mpi_real8, exchange, 0, mpi_comm_world, ierr )
                    denmat = denmattmp
                end if
            end if
            call update !Update non-bonded interactions list 
        end if

    contains
    
        !==================================================================
        subroutine OTF_string_REX_server( exchange ) !Also is responsible for update of K and node_positions
        
            integer, intent(out) :: exchange
            
            integer :: i, n
            real*8 :: delta !Global energy change upon replica exchange
            real*8 :: t0 !Desired position of the mean of distributions of a given replica
            real*8 :: t !Current position of the system of given replica along the string
            real*8 :: sigma
            real*8, dimension(nCV,nnodes) :: CVs !Current CVs at all nodes
            !real*8, dimension(500,30) :: t_hist = 0._dp
            logical :: changed
            real*8 :: tmp
            
            save t_hist

            d0 = string_length/real( nnodes-1, kind = 8 )

            CVs(:,1) = CV
            do i = 2, nnodes
                call mpi_recv( CVs(:,i), nCV, mpi_real8, i-1, mpi_any_tag, mpi_comm_world, status, ierr )
            end do

            exchange = -1 !Handle case when i starts from 2    
            
            !---Calculate whether the exchange should be done and inform the nodes about the decision---
            do i = mod(step/rex_freq,2)+1, nnodes-1, 2  !Trigger exchanges ( 1<->2 3<->4 ... ; 1 2<->3 4 ... )

                delta = 0.5_dp*sum( K*( ( CVs(:,i)   - nodes(:,i+1) )**2 + &
                                        ( CVs(:,i+1) - nodes(:,i)   )**2 - &
                                        ( CVs(:,i)   - nodes(:,i)   )**2 - &
                                        ( CVs(:,i+1) - nodes(:,i+1) )**2 ) )
                P = min( 1._dp, exp( -delta/(R*temp) ) )

                if( random() < P ) then
                    if( i == 1 ) then !Check if the receiver is the server itself
                        exchange = 1 !The only node the first node (server) can perform exchange with is the node 2 (2-1=1)
                    else
                        call mpi_send( i, 1, mpi_integer, i-1, 0, mpi_comm_world, ierr )
                    end if
                    call mpi_send( i-1, 1, mpi_integer, i, 0, mpi_comm_world, ierr )
                    rex_unit = next_unit()
                    open( unit = rex_unit, file = trim( dir )//"REX.dat", position = "append" )
                    write(rex_unit,"(3I10)") step, i, i+1
                    close( rex_unit )
                else
                    if ( i == 1 ) then
                        exchange = -1
                    else
                        call mpi_send( -1, 1, mpi_integer, i-1, 0, mpi_comm_world, ierr )
                    end if
                    call mpi_send( -1, 1, mpi_integer, i, 0, mpi_comm_world, ierr )
                end if

                if( i == nnodes-2 ) call mpi_send( -1, 1, mpi_integer, nnodes-1, 0, mpi_comm_world, ierr )
                
            end do
            !-------------------------------------------------------------------------------------------
        
        end subroutine OTF_string_REX_server
        !================================================================== 
    
    end subroutine OTF_string_REX
    !==================================================================
    
    
    !==================================================================
    subroutine interpolate_splines( ninit, nfinal, positions, init_set, final_set, tension )
    
        real*8, dimension(:,:), intent(in)  :: init_set
        real*8, dimension(:), intent(inout) :: positions
        real*8, dimension(:,:), intent(out) :: final_set  
        integer, intent(in) :: ninit, nfinal
        real*8, intent(in), optional :: tension

        integer :: i, j
        real*8 :: Ltotal, Lnew
        real*8, dimension(ninit) :: L
        real*8, dimension(ninit-1,5) :: splines

        final_set(:,1) = init_set(:,1)
        final_set(:,nfinal) = init_set(:,ninit)
    
        !---Build array of string arc lengths up to each node---
        L(1) = 0._dp
        do i = 2, ninit
            L(i) = L(i-1) + sqrt( sum( (init_set(:,i-1) - init_set(:,i) )**2 ) )
        end do
        !-------------------------------------------------------
        string_length = L(ninit)
        
        !Reparameterize node positions to maintain the total arc length

        positions = positions*L(ninit)/positions(nfinal)
        if( (mod( step, rex_freq ) == 0) .and. (.not. OTF_string_preparation) .and. string_defined ) then !Write out the positions
            pos_unit = next_unit()
            open( unit = pos_unit, file = trim( dir )//"node_positions.dat", position = "append" )
            write(pos_unit,pos_frmt) positions
            close( pos_unit )
            pos_unit = next_unit()
            open( unit = pos_unit, file = trim( dir )//"K.dat", position = "append" )
            write(pos_unit,"(I10)",advance="no") step
            write(pos_unit,pos_frmt) K
            close( pos_unit )
        end if

        !Spline interpolation of each CV
        do i = 1, nCV
            if( present( tension ) ) then
                call TensionSplines( ninit, L, init_set(i,:), splines, tension ) !Get splines coefficients for i-th CV
            else

                call CubicSplines( ninit, L, init_set(i,:), splines ) !Get splines coefficients for i-th CV        

            end if
            !string_splines(:,:,i) = splines

            do j = 1, nfinal
                final_set(i,j) = spline_value( positions(j), splines )
            end do

        end do
        !-------------------------------

    contains
    
        subroutine TensionSplines( n, x, y, coef, smooth )
        
            integer, intent(in) :: n
            real*8, intent(in) :: smooth
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
            call curvs( n, x, y, 1._8, 1, smooth, sqrt( 2._8/real( n, kind = 8 ) ), ys, s, 0._8, temp, ierr )

            coef(:,1) = x(1:n-1)
            coef(:,2) = (s(2:n)-s(1:n-1))/(h(1:n-1)*6)
            coef(:,3) = s(1:n-1)/2._8
            coef(:,4) = (y(2:n)-y(1:n-1))/h(1:n-1) - ((s(2:n)+2*s(1:n-1))/6._8)*h(1:n-1)
            coef(:,5) = ys(1:n-1)   

        end subroutine TensionSplines    
    
    end subroutine interpolate_splines
    !==================================================================


    !===interpolate nodes during reparameterization===
    subroutine distribute_nodes( init_set, final_set )
    
        real*8, dimension(:,:), intent(in)  :: init_set
        real*8, dimension(:,:), intent(out) :: final_set
    
        integer :: ninit, nfinal
        integer :: i, j
        real*8 :: Ltotal, Lnew
        real*8, dimension(:), allocatable :: L
        
        ninit  = size( init_set,  dim = 2 )
        nfinal = size( final_set, dim = 2 )
        
        final_set(:,1) = init_set(:,1)
        final_set(:,nfinal) = init_set(:,ninit)
        
        allocate( L(ninit) )
    
        !---Build array of string arc lengths up to each node---
        L(1) = 0._dp
        do i = 2, ninit
            L(i) = L(i-1) + sqrt( sum( (init_set(:,i-1) - init_set(:,i) )**2 ) )
        end do
        Ltotal = L(ninit)
        !-------------------------------------------------------
        
        !---Obtain new nodes by simple linear interpolation---
        j = 0
        do i = 2, nfinal-1
            Lnew = (i-1)*Ltotal/real( nfinal-1, kind = 8 ) !string arc length (free energy change) up to new node i
            do while( L(j) < Lnew )
              j = j + 1
            end do
            final_set(:,i) = init_set(:,j-1) + ( init_set(:,j)-init_set(:,j-1) )*( Lnew-L(j-1) )/( L(j)-L(j-1) )
        end do
        !-----------------------------------------------------
        
        deallocate( L )
    
    end subroutine distribute_nodes
    !=================================================
    
    
    !==================================================================
    subroutine update_active_space
    
        integer :: i, j
        
        j = -2
        do i = 1, nact
            j = j + 3
            actcrd(j:j+2) = atmcrd(:,actidx(i))
        end do
    
    end subroutine update_active_space
    !==================================================================
    
    
    !==================================================================
    subroutine get_M
      
        integer :: i, j
        
        do i = 1, nCV
            do j = i, nCV
                M(i,j) = dot_product( Jacobian(i,:), actmas*Jacobian(j,:) )
                M(j,i) = M(i,j)
            end do
        end do    
    
    end subroutine get_M
    !==================================================================    
    
    
    !==================================================================
    function delta_z() !The change of the position of the node at the current step of dynamics
    
      real*8, dimension(nCV) :: delta_z
    
      call get_M
      delta_z = - matmul( M, K*(node_CV - CV) ) * dynamics_delta / g
      
      return
    
    end function delta_z
    !==================================================================     
    
    
    
!==============================ANALYSIS OF RESULTS==============================


    subroutine average_string( min_step, max_step, freq, name, copy_to_nodes )
    
        integer, intent(in) :: min_step, max_step, freq
        integer :: unit
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: copy_to_nodes !Copy average nodes to nodes array

        integer :: i
        real*8, dimension(nCV,nnodes) :: av_nodes, av_nodes_tmp !Average string over the given range
        character*80 :: filename

        av_nodes = 0._dp
        unit = next_unit()
        do i = min_step, max_step, freq
            write(filename,*) i
            open( unit = unit, file = trim( dir )//trim( adjustl( filename ) )//".string", status = "old" )
            read(unit,*) av_nodes_tmp
            av_nodes = av_nodes + av_nodes_tmp
        end do
        av_nodes = av_nodes/real( (max_step-min_step)/freq + 1, kind = 8 )
        
        if( present( copy_to_nodes ) ) then
	    if( copy_to_nodes ) nodes = av_nodes
	end if

        if( present( name ) ) then
            open( unit = unit, file = name, status = "replace" )
            write(unit,CV_frmt) av_nodes    
            close(unit)
        end if
    
    end subroutine average_string



    subroutine get_FEP( min_step, max_step, average_string, FEP, dF, mode )

        integer, intent(in) :: min_step, max_step
        real*8, dimension(:), intent(out), optional :: FEP
        real*8, dimension(nCV,nnodes), intent(out), optional :: dF
        character(len=*), intent(in), optional :: average_string, mode

        integer :: i, j, unit, np
        real*8 :: step !Step between interpolated points
        real*8 :: x
        
        real*8, dimension(nCV) :: dF_tmp, dz_tmp
        real*8, dimension(nCV,nnodes) :: av_nodes !Average string over the given range
        real*8, dimension(nCV,nnodes) :: dzi_ds !Derivatives of each CV at each node with respect to the string parameterized from 0 to 1
        real*8, dimension(nCV,nnodes) :: dF_dzi !Derivatives of F at each node with respect to the CVs
        real*8, dimension(nnodes-1,5,nCV) :: string_splines, dF_dzi_splines !Cubic splines coefficients for string and dF_dzi
        real*8, dimension(nnodes) :: eq_x
        
        character*80 :: filename
        
        write(*,"(A)") "---FEP calculation---"
        
        if( .not. ( present( FEP ) .or. present( dF ) ) ) then
            write(*,"(A)") "Either FEP or F derivatives should be requested"
            return
        end if
        
        dF_dzi = 0._dp
        unit = next_unit()
        do i = 1, nnodes
        
            write(filename,*) i
            open( unit = unit, file = trim( dir )//trim( adjustl( filename ) )//".dat", status = "old" )
            
            do j = 1, min_step-1
                read(unit,*)
            end do
            do j = min_step, max_step                           !
                read(unit,*) dF_tmp, dF_tmp                       !
                dF_dzi(:,i) = dF_dzi(:,i) + dF_tmp                !
            end do                                              !
                                                              ! Equation (21) in Chem. Phys. Lett., 446 (2007), 182-190
        end do                                                !
                                                              !
        dF_dzi = dF_dzi/real( max_step-min_step+1, kind = 8 ) !
        write(*,"(A)") "F derivatives: done"
        
        if( present( dF ) ) dF = dF_dzi
        
        if( present( FEP ) ) then
        
            if( present( average_string ) ) then
                open( unit = unit, file = average_string, status = "old" )
                read(unit,*) av_nodes
            else
                av_nodes = nodes
            end if
        
            FEP(1) = 0._dp !Arbitrary reference

            if( present( mode ) .and. (mode == "linear") ) then
            
                dzi_ds(:,1) = av_nodes(:,2)-av_nodes(:,1)
                dzi_ds(:,nnodes) = av_nodes(:,nnodes)-av_nodes(:,nnodes-1)
                do i = 2, nnodes-1
                  dzi_ds(:,i) = (av_nodes(:,i+1)-av_nodes(:,i-1))*0.5_dp
                end do
                do i = 2, nnodes                                                  !
                  FEP(i) = FEP(i-1) + dot_product( dzi_ds(:,i), dF_dzi(:,i) ) ! Equation (20) in Chem. Phys. Lett., 446 (2007), 182-190
                end do                                                    !
            !FEP = FEP/real( nnodes-1, kind = 8 )
            !
            else !Cubic splines interpolation
            
                np = size( FEP )
                step = (nnodes-1)/real( np-1, kind = 8 )
                do i = 1, nnodes
                    eq_x(i) = i
                end do
                do i = 1, nCV
                    call CubicSplines( nnodes, eq_x, dF_dzi(i,:), dF_dzi_splines(:,:,i) )
                    call CubicSplines( nnodes, eq_x, av_nodes(i,:), string_splines(:,:,i) )
                end do
                x = 1._dp
                do i = 2, np
                    x = x + step
                    do j = 1, nCV
                        dz_tmp(j) = spline_der( x - step*0.5_dp, string_splines(:,:,j) )
                        dF_tmp(j) = spline_value( x - step*0.5_dp, dF_dzi_splines(:,:,j) )
                    end do
                    FEP(i) = FEP(i-1) + dot_product( dz_tmp, dF_tmp )*step
                end do
                
            end if
            FEP = FEP/4.184_dp
            write(*,"(A)") "FEP: done"
        
        end if
        write(*,"(A)") "---------------------"
        
        close(unit)

    end subroutine get_FEP

#endif

end module OTF_string_mpi
