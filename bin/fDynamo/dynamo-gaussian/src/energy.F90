!===============================================================================
!
!        fDynamo v2.2 - a program for performing molecular simulations.
!                    Copyright (C) 2005-2007 Martin J. Field
!
!===============================================================================
!
!       This program is free software; you can redistribute it and/or     
!       modify it under the terms of the GNU General Public License       
!       as published by the Free Software Foundation; either version 2    
!       of the License, or (at your option) any later version.            
!
!       This program is distributed in the hope that it will be useful,   
!       but WITHOUT ANY WARRANTY; without even the implied warranty of    
!       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     
!       GNU General Public License for more details.                      
!
!       You should have received a copy of the GNU General Public License 
!       along with this program; if not, write to the Free Software       
!       Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,        
!       MA  02110-1301, USA.                                              
!
!===============================================================================
!
!  Email: martin.field@ibs.fr
!  WWWs:  http://www.ibs.fr and http://www.pdynamo.org
!
!===============================================================================
!                           The Energy Module (MOPAC)
!===============================================================================
!
! . MM energy scalars (all kJ mol^-1):
!
!   EANGLE                         The angle energy.
!   EBOND                          The bond energy.
!   EDIHEDRAL                      The dihedral energy.
!   EELECT                         The electrostatic energy.
!   EIMPROPER                      The improper energy.
!   ELJ                            The Lennard-Jones energy.
!
! . Remaining energy scalars:
!
!   ECONSTRAINT                    The constraint energy (kJ mol^-1).
!   EPI                            The harmonic path integral energy (kJ mol^-1).
!   EQM                            The QM energy (kJ mol^-1).
!   ETOTAL                         The total potential energy (kJ mol^-1).
!   GRMS                           The RMS gradient (kJ mol^1 A^-1).
!   VIRIAL                         The internal virial (multiplied by -3).
!
! . Arrays:
!
!   ATMDER                         The first derivatives.
!   ATMHES                         The second derivative matrix.
!
! . Subroutines:
!
!   ENERGY                         Calculate the potential energy.
!   GRADIENT                       Calculate the energy and the first
!                                  derivatives.
!   HESSIAN                        Calculate the energy and the first and second
!                                  derivatives.
!
!   ENERGY_INITIALIZE              Initialize the energy module.
!   ENERGY_PRINT                   Print the energy terms.
!
! . Notes:
!
!   The VIRIAL calculation is only correct if there are no fixed or quantum
!   atoms. This may be changed in future versions although in practice the
!   calculation of the virial with fixed atoms is not particularly useful.
!
!   ATMDER is always dimensioned to (1:3,1:NATOMS) even if there are fixed
!   atoms (this may change). ATMHES, however, has dimensions (N*(N+1))/2
!   where N = 3 * NFREE.
!
!   In a path integral calculation, the energy values printed are averages
!   for each term over the different polymers.
!
!===============================================================================
MODULE POTENTIAL_ENERGY

! . Module declarations.
#ifdef	HACK_CHRG
USE DEFINITIONS,        ONLY : DP, SKIP_CABINITIO
#else
USE DEFINITIONS,        ONLY : DP
#endif
USE PRINTING,           ONLY : PRINT_LINE, PRINT_SUMMARY_ELEMENT, PRINT_SUMMARY_ENDLINE, PRINT_SUMMARY_OPTIONS, &
                               PRINT_SUMMARY_START, PRINT_SUMMARY_STOP, PRINT_ERROR

USE ATOMS,              ONLY : ATMCRD, ATMFIX, NATOMS, NATOMSMM, NATOMSQM, NFIXED, NFREE
USE CONSTRAINT,         ONLY : ENERGY_CONSTRAINT
USE ENERGY_COVALENT,    ONLY : ENERGY_ANGLE, ENERGY_BOND, ENERGY_DIHEDRAL, ENERGY_IMPROPER
USE ENERGY_NON_BONDING, ONLY : ENERGY_NON_BONDING_CALCULATE, NNBLISTQM
USE QUANTUM_ENERGY,     ONLY : ENERGY_QUANTUM

#ifdef	HACK_SPLN
USE SPLINE,             ONLY : ENERGY_SPLINE
#endif

#ifdef	HACK_CHRG
USE CABINITIO,          ONLY : CABINITIO_ENERGY, CABINITIO_GRADIENT, CABINITIO_HESSIAN
#endif

#ifdef	HACK_ABIN
USE ABINITIO,          ONLY : ABINITIO_ENERGY, ABINITIO_GRADIENT, ABINITIO_HESSIAN
#endif

#ifdef	HACK_VRC
USE VRC,                ONLY : CALC_VRC
#endif

#ifdef	HACK_PERT
USE PERT
#endif

#ifdef	HACK_OTFS
USE OTF_STRING_MPI,     ONLY: OTF_STRING_MPI_CALC
#endif

USE HESSIAN_UPDATE
USE FILES


IMPLICIT NONE
PUBLIC
#ifndef PGPC
SAVE
#endif

! . Module scalars.
REAL ( KIND = DP ) :: EANGLE      = 0.0_DP, EBOND  = 0.0_DP, &
                      EDIHEDRAL   = 0.0_DP, EELECT = 0.0_DP, &
                      EIMPROPER   = 0.0_DP, ELJ    = 0.0_DP, &
                      ECONSTRAINT = 0.0_DP, EPI    = 0.0_DP, &
		      	      EQM         = 0.0_DP, ETOTAL = 0.0_DP, &
		              GRMS        = 0.0_DP, VIRIAL = 0.0_DP, QMLJ = 0.0_DP

#ifdef	HACK_SPLN
REAL ( KIND = DP ) :: ESPLN = 0.0_DP
#endif

#ifdef	HACK_VRC
REAL ( KIND = DP ) :: EVRC = 0.0_DP
#endif

! . Module arrays.
REAL ( KIND = DP ), ALLOCATABLE, DIMENSION(:,:) :: ATMDER
#ifdef	HACK_GH
REAL ( KIND = DP ), ALLOCATABLE, DIMENSION(:,:) :: ATMDER_CONSTRAINT
#endif
REAL ( KIND = DP ), ALLOCATABLE, DIMENSION(:)   :: ATMHES

! - Should calculate hessian numerically?
LOGICAL :: USE_HESSIAN_NUMERICAL = .FALSE.
! If 1 then calculate hessian on every step, else check for gradient update
INTEGER :: USE_HESSIAN_RECALC  = 1
! Counter for hessian evaluation (can be reseted)
INTEGER :: USE_HESSIAN_COUNT   = 0
! Kind of updater to be used (default to BFGS)
CHARACTER( LEN=6 ) :: USE_HESSIAN_METHOD = "BFGS"

!===============================================================================
CONTAINS
!===============================================================================

   !--------------------------
   SUBROUTINE ENERGY ( PRINT )
   !--------------------------

   ! . Scalar arguments.
   LOGICAL, INTENT(IN), OPTIONAL :: PRINT

   ! . Local scalars.
   LOGICAL :: QPRINT

#ifdef	HACK_PERT
   REAL( KIND=DP ) :: F_LAMBDA, F_EPI, F_VIRIAL
#endif

   ! . Reset all the energy module variables.
   CALL ENERGY_INITIALIZE

   ! . Return if there are no atoms.
   IF ( NATOMS <= 0 ) RETURN

   ! . Check the print flag.
   IF ( PRESENT ( PRINT ) ) THEN
      QPRINT = PRINT
   ELSE
      QPRINT = .TRUE.
   END IF

#ifdef	HACK_CHRG
!   ! . we DO need charges around QM atoms... (badly!)
!   IF( NATOMSMM > 0 .AND. NNBLISTQM <= 0 ) &
!        CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, QMLJ = QMLJ )

   ! . Do a QM calculation (and take back fitted charges for QM atoms)
   IF ( NATOMSQM > 0 .AND. .NOT. SKIP_CABINITIO ) CALL CABINITIO_ENERGY( EQM )
#endif

   ! . Process the different MM energy terms.
   IF ( NATOMSMM > 0 ) THEN
      CALL ENERGY_BOND                  ( EBOND,       VIRIAL )
      CALL ENERGY_ANGLE                 ( EANGLE              )
      CALL ENERGY_DIHEDRAL              ( EDIHEDRAL           )
      CALL ENERGY_IMPROPER              ( EIMPROPER           )
      CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, QMLJ = QMLJ )
   END IF

#ifndef	HACK_CHRG
   ! . Do a QM calculation.
#ifdef	HACK_ABIN
   IF ( NATOMSQM > 0 ) CALL ABINITIO_ENERGY( EQM )
#else
!!#ifdef	HACK_MNDO
!!   IF ( NATOMSQM > 0 ) CALL MNDO97_COMMON_CALL( .FALSE. )
!!#else
   IF ( NATOMSQM > 0 ) CALL ENERGY_QUANTUM ( EQM, EPI, VIRIAL, PRINT = QPRINT )
!!#endif
#endif
#endif

   ! . Calculate the constraint energy.
   CALL ENERGY_CONSTRAINT ( ECONSTRAINT, VIRIAL )

   ! . Sum up the total energy.
   ETOTAL = EANGLE + EBOND + EDIHEDRAL + EIMPROPER + EELECT + ELJ + EQM + EPI + ECONSTRAINT

#ifdef	HACK_SPLN
   ! . Calculate the spline contribution
   CALL ENERGY_SPLINE( ESPLN )

   ! . Sum up the total energy.
   ETOTAL = ETOTAL + ESPLN
#endif

#ifdef	HACK_VRC
   CALL CALC_VRC( EVRC )
   ETOTAL = ETOTAL + EVRC
#endif

#ifdef	HACK_PERT
   IF( PERT_CALC_EL ) THEN
       CUR_PERT_QM = EQM
       IF( LAMBDA_VALUE + LAMBDA_DELTA > 1._DP .OR. LAMBDA_DELTA == .0_DP ) THEN
           NXT_PERT_QM = CUR_PERT_QM
       ELSE
           F_LAMBDA = LAMBDA_VALUE
           LAMBDA_VALUE = LAMBDA_VALUE + LAMBDA_DELTA
           CALL ENERGY_QUANTUM( NXT_PERT_QM, F_EPI, F_VIRIAL, PRINT = .FALSE. )
           LAMBDA_VALUE = F_LAMBDA
       END IF
       IF( LAMBDA_VALUE - LAMBDA_DELTA < 0._DP .OR. LAMBDA_DELTA == .0_DP ) THEN
           PRV_PERT_QM = CUR_PERT_QM
       ELSE
           F_LAMBDA = LAMBDA_VALUE
           LAMBDA_VALUE = LAMBDA_VALUE - LAMBDA_DELTA
           CALL ENERGY_QUANTUM( PRV_PERT_QM, F_EPI, F_VIRIAL, PRINT = .FALSE. )
           LAMBDA_VALUE = F_LAMBDA
       END IF
       IF( PERT_WRITE .AND. PERT_UNIT > -1 ) THEN
           WRITE( PERT_UNIT, "(3F20.10)" ) PRV_PERT_EL + PRV_PERT_QM, CUR_PERT_EL + CUR_PERT_QM, NXT_PERT_EL + NXT_PERT_QM
           CALL FLUSH( PERT_UNIT )
       END IF
   END IF
   IF( PERT_CALC_LJ ) THEN
       IF( PERT_WRITE .AND. PERT_UNIT > -1 ) THEN
           WRITE( PERT_UNIT, "(3F20.10)" ) PRV_PERT_LJ, CUR_PERT_LJ, NXT_PERT_LJ
           CALL FLUSH( PERT_UNIT )
       END IF
   END IF
#endif

   ! . Print out the energy if required.
   IF ( QPRINT ) CALL ENERGY_PRINT

   END SUBROUTINE ENERGY

   !----------------------------
   SUBROUTINE GRADIENT ( PRINT )
   !----------------------------

   ! . Scalar arguments.
   LOGICAL, INTENT(IN), OPTIONAL :: PRINT

   ! . Local scalars.
   INTEGER :: IATOM
   LOGICAL :: QPRINT
!real*8 :: mierda_de_punteros

#ifdef	HACK_PERT
   REAL( KIND=DP ) :: F_LAMBDA, F_EPI, F_VIRIAL
#endif

   ! . Reset all the energy module variables.
   CALL ENERGY_INITIALIZE

   ! . Return if there are no free atoms.
   IF ( NFREE <= 0 ) RETURN

   ! . Check the print flag.
   IF ( PRESENT ( PRINT ) ) THEN
      QPRINT = PRINT
   ELSE
      QPRINT = .TRUE.
   END IF

   ! . Allocate and initialize the gradient array.
   ALLOCATE ( ATMDER(1:3,1:NATOMS) ) ; ATMDER(1:3,1:NATOMS) = 0.0_DP
#ifdef	HACK_GH
   ALLOCATE ( ATMDER_CONSTRAINT(1:3,1:NATOMS) ) ; ATMDER_CONSTRAINT(1:3,1:NATOMS) = 0.0_DP
#endif

#ifdef	HACK_CHRG
!   ! . we DO need charges around QM atoms... (badly!)
!   IF( NATOMSMM > 0 .AND. NNBLISTQM <= 0 ) &
!        CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, QMLJ = QMLJ )

   ! . Do a QM calculation (and take back fitted charges for QM atoms)
   IF ( NATOMSQM > 0 .AND. .NOT. SKIP_CABINITIO ) CALL CABINITIO_GRADIENT( EQM, ATMDER )
#endif

   ! . Process the different MM energy terms.
   IF ( NATOMSMM > 0 ) THEN
      CALL ENERGY_BOND                  ( EBOND,       VIRIAL, ATMDER )
      CALL ENERGY_ANGLE                 ( EANGLE,              ATMDER )
      CALL ENERGY_DIHEDRAL              ( EDIHEDRAL,           ATMDER )
      CALL ENERGY_IMPROPER              ( EIMPROPER,           ATMDER )
      CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, ATMDER, QMLJ = QMLJ )
   END IF

#ifndef	HACK_CHRG
   ! . Do a QM calculation.
#ifdef	HACK_ABIN
   IF ( NATOMSQM > 0 ) CALL ABINITIO_GRADIENT( EQM, ATMDER )
#else
!!#ifdef	HACK_MNDO
!!   IF ( NATOMSQM > 0 ) CALL MNDO97_COMMON_CALL( .TRUE. )
!!#else
   IF ( NATOMSQM > 0 ) CALL ENERGY_QUANTUM ( EQM, EPI, VIRIAL, GRADIENT = ATMDER, PRINT = QPRINT )
!!#endif
#endif
#endif

#ifdef	HACK_OTFS
   CALL OTF_STRING_MPI_CALC( ATMDER )
#endif

   ! . Calculate the constraint energy.
#ifdef	HACK_GH
   CALL ENERGY_CONSTRAINT ( ECONSTRAINT, VIRIAL, ATMDER_CONSTRAINT )
   ATMDER = ATMDER + ATMDER_CONSTRAINT
#else
   CALL ENERGY_CONSTRAINT ( ECONSTRAINT, VIRIAL, ATMDER )
#endif

   ! . Sum up the total energy.
   ETOTAL = EANGLE + EBOND + EDIHEDRAL + EIMPROPER + EELECT + ELJ + EQM + EPI + ECONSTRAINT

#ifdef	HACK_SPLN
   ! . Calculate the spline contribution
   CALL ENERGY_SPLINE( ESPLN, ATMDER )

   ! . Sum up the total energy.
   ETOTAL = ETOTAL + ESPLN
#endif

#ifdef	HACK_VRC
   CALL CALC_VRC( EVRC, ATMDER )
   ETOTAL = ETOTAL + EVRC
#endif

#ifdef	HACK_PERT
   IF( PERT_CALC_EL ) THEN
       CUR_PERT_QM = EQM
       IF( LAMBDA_VALUE + LAMBDA_DELTA > 1._DP .OR. LAMBDA_DELTA == .0_DP ) THEN
           NXT_PERT_QM = CUR_PERT_QM
       ELSE
           F_LAMBDA = LAMBDA_VALUE
           LAMBDA_VALUE = LAMBDA_VALUE + LAMBDA_DELTA
           CALL ENERGY_QUANTUM( NXT_PERT_QM, F_EPI, F_VIRIAL, PRINT = .FALSE. )
           LAMBDA_VALUE = F_LAMBDA
       END IF
       IF( LAMBDA_VALUE - LAMBDA_DELTA < 0._DP .OR. LAMBDA_DELTA == .0_DP ) THEN
           PRV_PERT_QM = CUR_PERT_QM
       ELSE
           F_LAMBDA = LAMBDA_VALUE
           LAMBDA_VALUE = LAMBDA_VALUE - LAMBDA_DELTA
           CALL ENERGY_QUANTUM( PRV_PERT_QM, F_EPI, F_VIRIAL, PRINT = .FALSE. )
           LAMBDA_VALUE = F_LAMBDA
       END IF
       IF( PERT_WRITE .AND. PERT_UNIT > -1 ) THEN
           WRITE( PERT_UNIT, "(3F20.10)" ) PRV_PERT_EL + PRV_PERT_QM, CUR_PERT_EL + CUR_PERT_QM, NXT_PERT_EL + NXT_PERT_QM
           CALL FLUSH( PERT_UNIT )
       END IF
   END IF
   IF( PERT_CALC_LJ ) THEN
       IF( PERT_WRITE .AND. PERT_UNIT > -1 ) THEN
           WRITE( PERT_UNIT, "(3F20.10)" ) PRV_PERT_LJ, CUR_PERT_LJ, NXT_PERT_LJ
           CALL FLUSH( PERT_UNIT )
       END IF
   END IF
#endif

   ! . Zero out the derivatives of fixed atoms.
   IF ( NFIXED > 0 ) THEN
      DO IATOM = 1,NATOMS
         IF ( ATMFIX(IATOM) ) ATMDER(1:3,IATOM) = 0.0_DP
      END DO
   END IF

   ! . Print out the energy if required.
   IF ( QPRINT ) CALL ENERGY_PRINT

   END SUBROUTINE GRADIENT


   !---------------------------
   SUBROUTINE HESSIAN( PRINT )
   !---------------------------
   LOGICAL, INTENT(IN), OPTIONAL :: PRINT

   REAL( KIND=DP ), DIMENSION(:), ALLOCATABLE :: DX, DG
   INTEGER :: FD, ERR, I, J
   LOGICAL :: QPRINT


   ! . Allocate some arrays
   ALLOCATE( DX(1:3*NFREE), DG(1:3*NFREE) )

   ! . Hessian should be calculated from scratch...
   USE_HESSIAN_COUNT = USE_HESSIAN_COUNT + 1
   ! . It turns out that hessian must be calculated...
   IF( MOD( USE_HESSIAN_COUNT, USE_HESSIAN_RECALC ) == 0 ) THEN
! # CALCULATE HESSIAN ###########################################################################
       IF( USE_HESSIAN_NUMERICAL ) THEN
           CALL NUM_HESSIAN
       ELSE
           CALL CALC_HESSIAN( .FALSE. )
       END IF
! # READ PREVIOUS DUMP FILE #( useful for IRC calculations )#####################################
!        CALL GRADIENT
!        ALLOCATE( ATMHES(1:3*NFREE*(3*NFREE+1)/2) )
!        FD = NEXT_UNIT()
!        OPEN( UNIT = FD, FILE = "hessian.dump", ACTION = "READ", FORM = "UNFORMATTED", STATUS = "OLD" )
!        READ( FD ) ATMHES(1:3*NFREE*(3*NFREE+1)/2)
!        CLOSE( FD )
! ###############################################################################################
   ! . Try to update hessian using the gradient vector
   ELSE
       FD = NEXT_UNIT()
       OPEN( UNIT = FD, FILE = "update.dump", ACTION = "READ", FORM = "UNFORMATTED", STATUS = "OLD", IOSTAT = ERR )
       ! . Ok, read and update hessian
       IF( ERR == 0 ) THEN
           CALL GRADIENT( .FALSE. )
           ALLOCATE( ATMHES(1:3*NFREE*(3*NFREE+1)/2) )
           READ( FD ) DX(1:3*NFREE)
           READ( FD ) DG(1:3*NFREE)
           READ( FD ) ATMHES(1:3*NFREE*(3*NFREE+1)/2)
           CLOSE( FD )
           J = 1
           DO I = 1, NATOMS
               IF( .NOT. ATMFIX( I ) ) THEN
                   DX(J:J+2) = ATMCRD(1:3,I) - DX(J:J+2)
                   DG(J:J+2) = ATMDER(1:3,I) - DG(J:J+2)
                   J = J + 3
               END IF
           END DO
! - check that there's some step to perform... (else just use the written hessian )
if( sqrt( sum( dx ** 2 ) ) > 1.d-6 .and. sqrt( sum( dg ** 2 ) ) > 1.d-6 ) then 
           SELECT CASE ( USE_HESSIAN_METHOD )
               CASE( "BFGS  " ); CALL    UPDATE_BFGS( 3 * NFREE, DX, DG, ATMHES )
               CASE( "SR1   " ); CALL     UPDATE_SR1( 3 * NFREE, DX, DG, ATMHES )
               CASE( "PSB   " ); CALL     UPDATE_PSB( 3 * NFREE, DX, DG, ATMHES )
               CASE( "BOFILL" ); CALL  UPDATE_BOFILL( 3 * NFREE, DX, DG, ATMHES )
               CASE DEFAULT ; CALL PRINT_ERROR ( "USE_HESSIAN_METHOD", "Valid updaters are: BFGS / SR1 / PSB / BOFILL" )
           END SELECT
else
	write(*,*) " -- Hessian just read (from previous calculation)..."
end if
       ! . Sorry, no update can be performed right now... :( 
       ELSE
           IF( USE_HESSIAN_NUMERICAL ) THEN
               CALL NUM_HESSIAN
           ELSE
               CALL CALC_HESSIAN( .FALSE. )
           END IF
       END IF
   END IF

   ! . Store current data for updating...
   J = 1
   DO I = 1, NATOMS
       IF( .NOT. ATMFIX( I ) ) THEN
           DX(J:J+2) = ATMCRD(1:3,I)
           DG(J:J+2) = ATMDER(1:3,I)
           J = J + 3
       END IF
   END DO
   FD = NEXT_UNIT()
   OPEN( UNIT = FD, FILE = "update.dump", ACTION = "WRITE", FORM = "UNFORMATTED", STATUS = "UNKNOWN" )
   WRITE( FD ) DX(1:3*NFREE)
   WRITE( FD ) DG(1:3*NFREE)
   WRITE( FD ) ATMHES(1:3*NFREE*(3*NFREE+1)/2)
   CALL FLUSH( FD )
   CLOSE( FD )

   DEALLOCATE( DX, DG )

   ! . Print out the energy if required.
   QPRINT = .TRUE.
   IF( PRESENT( PRINT ) ) QPRINT = PRINT
   IF ( QPRINT ) CALL ENERGY_PRINT

   END SUBROUTINE HESSIAN


   !--------------------------------
   SUBROUTINE CALC_HESSIAN ( PRINT )
   !--------------------------------

   ! . Scalar arguments.
   LOGICAL, INTENT(IN), OPTIONAL :: PRINT

   ! . Local scalars.
   INTEGER :: IATOM
   LOGICAL :: QPRINT

   ! . Reset all the energy module variables.
   CALL ENERGY_INITIALIZE

   ! . Return if there are no free atoms.
   IF ( NFREE <= 0 ) RETURN

   ! . Check the print flag.
   IF ( PRESENT ( PRINT ) ) THEN
      QPRINT = PRINT
   ELSE
      QPRINT = .TRUE.
   END IF

   ! . Allocate and initialize the gradient array.
   ALLOCATE ( ATMDER(1:3,1:NATOMS) ) ; ATMDER(1:3,1:NATOMS) = 0.0_DP

   ! . Allocate and initialize the Hessian array.
   ALLOCATE ( ATMHES(1:(3*NFREE*(3*NFREE+1))/2) ) ; ATMHES = 0.0_DP

#ifdef	HACK_CHRG
!   ! . we DO need charges around QM atoms... (badly!)
!   IF( NATOMSMM > 0 .AND. NNBLISTQM <= 0 ) &
!        CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, QMLJ = QMLJ )

   ! . Do a QM calculation (and take back fitted charges for QM atoms)
   IF ( NATOMSQM > 0 .AND. .NOT. SKIP_CABINITIO ) CALL CABINITIO_HESSIAN( EQM, ATMDER, ATMHES )
#endif

   ! . Process the different energy terms.
   IF ( NATOMSMM > 0 ) THEN
      CALL ENERGY_BOND                  ( EBOND,       VIRIAL, ATMDER, ATMHES )
      CALL ENERGY_ANGLE                 ( EANGLE,              ATMDER, ATMHES )
      CALL ENERGY_DIHEDRAL              ( EDIHEDRAL,           ATMDER, ATMHES )
      CALL ENERGY_IMPROPER              ( EIMPROPER,           ATMDER, ATMHES )
      CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, ATMDER, ATMHES, QMLJ = QMLJ )
   END IF

#ifndef	HACK_CHRG
   ! . Do a QM calculation.
#ifdef	HACK_ABIN
   IF ( NATOMSQM > 0 ) CALL ABINITIO_HESSIAN( EQM, ATMDER, ATMHES )
#else
!!#ifdef	HACK_MNDO
!!   IF ( NATOMSQM > 0 ) CALL MNDO97_HESSIAN
!!#else
   IF ( NATOMSQM > 0 ) CALL ENERGY_QUANTUM ( EQM, EPI, VIRIAL, ATMDER, ATMHES, QPRINT )
!!#endif
#endif
#endif

   ! . Calculate the constraint energy.
   CALL ENERGY_CONSTRAINT ( ECONSTRAINT, VIRIAL, ATMDER, ATMHES )

   ! . Sum up the total energy.
   ETOTAL = EANGLE + EBOND + EDIHEDRAL + EIMPROPER + EELECT + ELJ + EQM + EPI + ECONSTRAINT

#ifdef	HACK_SPLN
   ! . Calculate the spline contribution
   CALL ENERGY_SPLINE( ESPLN, ATMDER, ATMHES )

   ! . Sum up the total energy.
   ETOTAL = ETOTAL + ESPLN
#endif

   ! . Zero out the derivatives of fixed atoms.
   IF ( NFIXED > 0 ) THEN
      DO IATOM = 1,NATOMS
         IF ( ATMFIX(IATOM) ) ATMDER(1:3,IATOM) = 0.0_DP
      END DO
   END IF

   ! . Print out the energy if required.
   IF ( QPRINT ) CALL ENERGY_PRINT

   END SUBROUTINE CALC_HESSIAN

   !---------------------------
   SUBROUTINE ENERGY_INITIALIZE
   !---------------------------

   ! . Initialize the MM energy terms.
   EBOND     = 0.0_DP
   EANGLE    = 0.0_DP
   EDIHEDRAL = 0.0_DP
   EIMPROPER = 0.0_DP
   EELECT    = 0.0_DP
   ELJ       = 0.0_DP

   ! . Initialize the remaining energy terms.
   ECONSTRAINT = 0.0_DP
   EPI         = 0.0_DP
   EQM         = 0.0_DP
   ETOTAL      = 0.0_DP
   GRMS        = 0.0_DP
   VIRIAL      = 0.0_DP
   QMLJ        = 0.0_DP

#ifdef	HACK_SPLN
   ESPLN       = 0.0_DP
#endif

#ifdef	HACK_VRC
   EVRC        = 0.0_DP
#endif

#ifdef	HACK_PERT
   PRV_PERT_EL = .0_DP
   CUR_PERT_EL = .0_DP
   NXT_PERT_EL = .0_DP
   PRV_PERT_LJ = .0_DP
   CUR_PERT_LJ = .0_DP
   NXT_PERT_LJ = .0_DP
   PRV_PERT_QM = .0_DP
   CUR_PERT_QM = .0_DP
   NXT_PERT_QM = .0_DP
#endif

   ! . Deallocate the derivative arrays.
   IF ( ALLOCATED ( ATMDER ) ) DEALLOCATE ( ATMDER )
#ifdef	HACK_GH
   IF ( ALLOCATED ( ATMDER_CONSTRAINT ) ) DEALLOCATE ( ATMDER_CONSTRAINT )
#endif
   IF ( ALLOCATED ( ATMHES ) ) DEALLOCATE ( ATMHES )

   END SUBROUTINE ENERGY_INITIALIZE

   !----------------------
   SUBROUTINE ENERGY_PRINT
   !----------------------

   ! . Write out the header.
   CALL PRINT_SUMMARY_OPTIONS ( HEADER_COLOR = "#BB00BB", VARIABLEWIDTH = 16 )
   CALL PRINT_SUMMARY_START ( "Potential Energy Terms" )

   ! . Write out the total energy and the RMS gradient.
   IF ( ALLOCATED ( ATMDER ) ) THEN

      ! . Calculate the RMS gradient.
      GRMS = SQRT ( SUM ( ATMDER * ATMDER ) / REAL ( 3*NFREE, DP ) )

      ! . Write out the energy.
      WRITE ( PRINT_LINE, "(F16.4)" ) ETOTAL ; CALL PRINT_SUMMARY_ELEMENT ( "Potential Energy" )
      WRITE ( PRINT_LINE, "(F16.4)" ) GRMS   ; CALL PRINT_SUMMARY_ELEMENT ( "RMS Gradient"     )

   ! . Write out the total energy.
   ELSE
      WRITE ( PRINT_LINE, "(F16.4)" ) ETOTAL ; CALL PRINT_SUMMARY_ELEMENT ( "Potential Energy" )
      CALL PRINT_SUMMARY_ENDLINE
   END IF

   ! . Write out the QM energy terms.
   IF ( NATOMSQM > 0 ) THEN
      WRITE ( PRINT_LINE, "(F16.4)" ) EQM ; CALL PRINT_SUMMARY_ELEMENT ( "QM Energy" )
      WRITE ( PRINT_LINE, "(F16.4)" ) QMLJ ; CALL PRINT_SUMMARY_ELEMENT ( "QM Lennard-Jones" )
      WRITE ( PRINT_LINE, "(F16.4)" ) EPI ; CALL PRINT_SUMMARY_ELEMENT ( "PI Energy" )
   END IF
   
   ! . Write out the MM energy terms.
   IF ( NATOMSMM > 0 ) THEN
      WRITE ( PRINT_LINE, "(F16.4)" ) EBOND     ; CALL PRINT_SUMMARY_ELEMENT ( "Bond Energy"          )
      WRITE ( PRINT_LINE, "(F16.4)" ) EANGLE    ; CALL PRINT_SUMMARY_ELEMENT ( "Angle Energy"         )
      WRITE ( PRINT_LINE, "(F16.4)" ) EDIHEDRAL ; CALL PRINT_SUMMARY_ELEMENT ( "Dihedral Energy"      )
      WRITE ( PRINT_LINE, "(F16.4)" ) EIMPROPER ; CALL PRINT_SUMMARY_ELEMENT ( "Improper Energy"      )
      WRITE ( PRINT_LINE, "(F16.4)" ) EELECT    ; CALL PRINT_SUMMARY_ELEMENT ( "Electrostatic Energy" )
      WRITE ( PRINT_LINE, "(F16.4)" ) ELJ       ; CALL PRINT_SUMMARY_ELEMENT ( "Lennard-Jones Energy" )
   END IF

   ! . Write out the constraint energy.
   WRITE ( PRINT_LINE, "(F16.4)" ) ECONSTRAINT ; CALL PRINT_SUMMARY_ELEMENT ( "Constraint Energy" )

#ifdef	HACK_SPLN
   WRITE ( PRINT_LINE, "(F16.4)" ) ESPLN ; CALL PRINT_SUMMARY_ELEMENT ( "Spline Energy" )
#endif

#ifdef	HACK_VRC
   WRITE ( PRINT_LINE, "(F16.4)" ) EVRC ; CALL PRINT_SUMMARY_ELEMENT ( "V.Const. Energy" )
#endif

   ! . Write out the terminator.
   CALL PRINT_SUMMARY_STOP

   END SUBROUTINE ENERGY_PRINT

!-!! - Standard Gradient calcs initialize vectors !!!
!-!SUBROUTINE NUM_GRADIENT
!-!    IMPLICIT NONE
!-!    INTEGER  :: I
!-!
!-!    ATMDER = .0_DP
!-!    IF ( NFREE <= 0 ) RETURN
!-!    IF ( NATOMSMM > 0 ) THEN
!-!        CALL ENERGY_BOND ( EBOND, VIRIAL, ATMDER )
!-!        CALL ENERGY_ANGLE ( EANGLE, ATMDER )
!-!        CALL ENERGY_DIHEDRAL ( EDIHEDRAL, ATMDER )
!-!        CALL ENERGY_IMPROPER ( EIMPROPER, ATMDER )
!-!        CALL ENERGY_NON_BONDING_CALCULATE ( EELECT, ELJ, VIRIAL, ATMDER, QMLJ = QMLJ )
!-!    END IF 
!-!    IF ( NATOMSQM > 0 ) THEN
!-!#ifdef	HACK_CHRG
!-!        IF( .NOT. SKIP_CABINITIO ) CALL CABINITIO_GRADIENT( EQM, ATMDER )
!-!#else
!-!#ifdef	HACK_ABIN
!-!        CALL ABINITIO_GRADIENT( EQM, ATMDER )
!-!#else
!-!        CALL ENERGY_QUANTUM ( EQM, EPI, VIRIAL, GRADIENT = ATMDER, PRINT = .FALSE. )
!-!#endif
!-!#endif
!-!    END IF
!-!    CALL ENERGY_CONSTRAINT ( ECONSTRAINT, VIRIAL, ATMDER )
!-!    IF ( NFIXED > 0 ) THEN
!-!        DO I = 1, NATOMS
!-!            IF ( ATMFIX(I) ) ATMDER(1:3,I) = .0_DP
!-!        END DO
!-!    END IF
!-!END SUBROUTINE NUM_GRADIENT


SUBROUTINE NUM_HESSIAN
    IMPLICIT NONE
    INTEGER                                       :: I, J, K, II, JJ, KK
    INTEGER, DIMENSION(:), ALLOCATABLE            :: IDX
    REAL ( KIND=DP )                              :: DE
    REAL ( KIND=DP ), PARAMETER                   :: H_STEP = 1.E-4_DP
    REAL ( KIND=DP ), DIMENSION(1:3)              :: CRD_SRC
    REAL ( KIND=DP ), DIMENSION(1:10)             :: EBACKUP
    REAL ( KIND=DP ), DIMENSION(:,:), ALLOCATABLE :: DER_BAK, FHES

    ALLOCATE ( IDX(1:NFREE), DER_BAK(1:3,1:NATOMS), FHES(1:3*NFREE,1:3*NFREE) )

    CALL GRADIENT( .FALSE. )

    DER_BAK     = ATMDER
    EBACKUP (1) = EBOND
    EBACKUP (2) = EANGLE
    EBACKUP (3) = EDIHEDRAL
    EBACKUP (4) = EIMPROPER
    EBACKUP (5) = EELECT
    EBACKUP (6) = ELJ
    EBACKUP (7) = QMLJ
    EBACKUP (8) = EQM
    EBACKUP (9) = EPI
    EBACKUP(10) = ECONSTRAINT

    J = 0
    DO I = 1, NATOMS
        IF( .NOT. ATMFIX(I) ) THEN
            J = J + 1
            IDX(J) = I
        END IF
    END DO

    DO I = 1, NFREE
        CRD_SRC(1:3) = ATMCRD(1:3,IDX(I))
        DO J = 1, 3
            ATMCRD(J,IDX(I)) = CRD_SRC(J) + H_STEP
            CALL GRADIENT( .FALSE. )
            K = 3 * ( I - 1 ) + J
            DO II = 1, NFREE
                DO JJ = 1, 3
                    KK = 3 * ( II - 1 ) + JJ
                    FHES(K,KK) = ( ATMDER(JJ,IDX(II)) - DER_BAK(JJ,IDX(II)) ) / H_STEP
                END DO
            END DO
            ATMCRD(J,IDX(I)) = CRD_SRC(J)
        END DO
    END DO

    ALLOCATE( ATMHES(1:(3*NFREE*(3*NFREE+1))/2) )
    ATMHES = 0.0_DP
    DO I = 1, 3*NFREE
        ATMHES(I*(I+1)/2) = FHES(I,I)
        DO J = I+1, 3*NFREE
            ATMHES(J*(J-1)/2+I) = ( FHES(I,J) + FHES(J,I) ) / 2._DP
        END DO
    END DO

    EBOND       = EBACKUP(1)
    EANGLE      = EBACKUP(2)
    EDIHEDRAL   = EBACKUP(3)
    EIMPROPER   = EBACKUP(4)
    EELECT      = EBACKUP(5)
    ELJ         = EBACKUP(6)
    QMLJ        = EBACKUP(7)
    EQM         = EBACKUP(8)
    EPI         = EBACKUP(9)
    ECONSTRAINT = EBACKUP(10)
    ATMDER      = DER_BAK

    DEALLOCATE ( IDX, DER_BAK, FHES )
END SUBROUTINE NUM_HESSIAN

END MODULE POTENTIAL_ENERGY
