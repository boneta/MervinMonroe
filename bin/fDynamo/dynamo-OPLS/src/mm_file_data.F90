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
!                    The Molecular Mechanics File Data Module
!===============================================================================
!
! . Electrostatic and Lennard-Jones options:
!
!   REF_SCALE_EL                   The electrostatics 1-4 scale factor.
!   REF_SCALE_LJ                   The Lennard-Jones 1-4 scale factor.
!
! . Link data:
!
!   NRESLINK                       The number of link definitions.
!   RESLINK                        The link definitions.
!
! . Parameter data:
!
!   NANGLPS                        The number of angle parameters.
!   ANGLPS                         The angle parameter data.
!   NBONDPS                        The number of bond parameters.
!   BONDPS                         The bond parameter data.
!   NDIHEPS                        The number of dihedral parameters.
!   DIHEPS                         The dihedral parameter data.
!   NIMPRPS                        The number of improper parameters.
!   IMPRPS                         The improper parameter data.
!
! . Residue data:
!
!   NRESIDUES                      The number of residue definitions.
!   RESIDUES                       The residue definitions.
!
! . Atom type data:
!
!   NTYPES                         The number of different types.
!   TYPES                          The atom type definitions.
!
! . Variant data:
!
!   NVARIANTS                      The number of variant definitions.
!   VARIANTS                       The variant defintions.
!
! . Subroutines:
!
!   MM_FILE_ALLOCATE               Create the data structure arrays.
!   MM_FILE_INITIALIZE             Initialize the data structure.
!   MM_FILE_SUMMARY                Write out a summary of the data structure.
!
!   ALLOCATE_RESIDUE_TYPE          Allocate a residue type.
!   ALLOCATE_VARIANT_TYPE          Allocate a variant type.
!   INITIALIZE_LINK_TYPE           Initialize a link type.
!   INITIALIZE_RESIDUE_TYPE        Initialize a residue type.
!   INITIALIZE_VARIANT_TYPE        Initialize a variant type.
!
!===============================================================================
MODULE MM_FILE_DATA

! . Module declarations.
USE DEFINITIONS, ONLY : DP
USE PRINTING,    ONLY : PRINT_LINE, PRINT_SUMMARY_ELEMENT, PRINT_SUMMARY_OPTIONS, PRINT_SUMMARY_START, PRINT_SUMMARY_STOP

USE ATOMS,       ONLY : ATOM_NAME_LENGTH
USE SEQUENCE,    ONLY : RESIDUE_NAME_LENGTH

IMPLICIT NONE
PUBLIC
#ifndef PGPC
SAVE
#endif

! . Module parameters.
CHARACTER ( LEN =  1 ), PARAMETER :: DUMMY_TYPE   = "X"
CHARACTER ( LEN =  2 ), PARAMETER :: LINK_LATERAL = "*R", LINK_MINUS = "-R", LINK_PLUS = "+R"
INTEGER,                PARAMETER :: DUMMY_CODE   = 0
INTEGER,                PARAMETER :: TYPE_NAME_LENGTH = ATOM_NAME_LENGTH

! . Atom type definition.
TYPE ATOM_TYPE
   CHARACTER ( LEN = TYPE_NAME_LENGTH ) :: NAME
   INTEGER            :: NUMBER
   REAL ( KIND = DP ) :: EPSILON, SIGMA
END TYPE ATOM_TYPE

! . Angle parameter type definition.
TYPE ANGLE_PARAMETER
   INTEGER            :: TYPE1, TYPE2, TYPE3
   REAL ( KIND = DP ) :: EQ, FC
END TYPE ANGLE_PARAMETER

! . Bond parameter type definition.
TYPE BOND_PARAMETER
   INTEGER            :: TYPE1, TYPE2
   REAL ( KIND = DP ) :: EQ, FC
END TYPE BOND_PARAMETER

! . Dihedral parameter type definition.
TYPE DIHEDRAL_PARAMETER
   INTEGER            :: TYPE1, TYPE2, TYPE3, TYPE4
   REAL ( KIND = DP ) :: V0, V1, V2, V3
END TYPE DIHEDRAL_PARAMETER

! . Residue type definition.
TYPE RESIDUE_TYPE
   CHARACTER ( LEN = RESIDUE_NAME_LENGTH ) :: NAME
   INTEGER :: NATOMS, NBONDS, NIMPROPERS
   CHARACTER ( LEN = ATOM_NAME_LENGTH ), DIMENSION(:),   POINTER :: NAMES
   CHARACTER ( LEN = ATOM_NAME_LENGTH ), DIMENSION(:,:), POINTER :: BONDS, IMPROPERS
   INTEGER,                              DIMENSION(:),   POINTER :: TYPES
   REAL ( KIND = DP ),                   DIMENSION(:),   POINTER :: CHARGES
END TYPE RESIDUE_TYPE

! . Variant type definition.
TYPE VARIANT_TYPE
   CHARACTER ( LEN = RESIDUE_NAME_LENGTH ) :: NAME, RESIDUE_NAME
   INTEGER :: NADDS, NBONDS, NCHARGES, NDELETES, NIMPROPERS
   CHARACTER ( LEN = ATOM_NAME_LENGTH ), DIMENSION(:),   POINTER :: ADDATM, CHGATM, DELATM
   CHARACTER ( LEN = ATOM_NAME_LENGTH ), DIMENSION(:,:), POINTER :: BONDS, IMPROPERS
   INTEGER,                              DIMENSION(:),   POINTER :: ADDTYP
   REAL ( KIND = DP ),                   DIMENSION(:),   POINTER :: ADDCHG, CHGCHG
END TYPE VARIANT_TYPE

! . Link type definition.
TYPE LINK_TYPE
   CHARACTER ( LEN = RESIDUE_NAME_LENGTH ) :: NAME
   TYPE(VARIANT_TYPE)                      :: RESIDUE1, RESIDUE2
END TYPE LINK_TYPE

! . The electrostatic and LJ options.
REAL ( KIND = DP ) :: REF_SCALE_EL = 1.0_DP, REF_SCALE_LJ = 1.0_DP

! . The parameter data.
INTEGER :: NANGLPS = 0, NBONDPS = 0, NDIHEPS = 0, NIMPRPS = 0
TYPE(ANGLE_PARAMETER),    ALLOCATABLE, DIMENSION(:) :: ANGLPS
TYPE(BOND_PARAMETER),     ALLOCATABLE, DIMENSION(:) :: BONDPS
TYPE(DIHEDRAL_PARAMETER), ALLOCATABLE, DIMENSION(:) :: DIHEPS, IMPRPS

! . The link data.
INTEGER :: NRESLINK = 0
TYPE(LINK_TYPE), ALLOCATABLE, DIMENSION(:) :: RESLINK

! . The residue data.
INTEGER :: NRESIDUES = 0
TYPE(RESIDUE_TYPE), ALLOCATABLE, DIMENSION(:) :: RESIDUES

! . The type data.
INTEGER :: NTYPES = 0
TYPE(ATOM_TYPE), ALLOCATABLE, DIMENSION(:) :: TYPES

! . The variant data.
INTEGER :: NVARIANTS
TYPE(VARIANT_TYPE), ALLOCATABLE, DIMENSION(:) :: VARIANTS

!===============================================================================
CONTAINS
!===============================================================================

   !--------------------------
   SUBROUTINE MM_FILE_ALLOCATE
   !--------------------------

   ! . Allocate the data structure arrays.
   ALLOCATE ( ANGLPS(1:NANGLPS),   BONDPS(1:NBONDPS),     &
              DIHEPS(1:NDIHEPS),   IMPRPS(1:NIMPRPS),     &
              RESLINK(1:NRESLINK), RESIDUES(1:NRESIDUES), &
              TYPES(1:NTYPES),     VARIANTS(1:NVARIANTS)  )

   END SUBROUTINE MM_FILE_ALLOCATE

   !----------------------------
   SUBROUTINE MM_FILE_INITIALIZE
   !----------------------------

   ! . Local scalars.
   INTEGER :: I

   ! . Initialize the MM counters.
   NANGLPS   = 0
   NBONDPS   = 0
   NDIHEPS   = 0
   NIMPRPS   = 0
   NRESLINK  = 0
   NRESIDUES = 0
   NTYPES    = 0
   NVARIANTS = 0

   ! . Initialize the non-bond options.
   REF_SCALE_EL = 1.0_DP
   REF_SCALE_LJ = 1.0_DP

   ! . Free the parameter arrays.
   IF ( ALLOCATED ( ANGLPS ) ) DEALLOCATE ( ANGLPS )
   IF ( ALLOCATED ( BONDPS ) ) DEALLOCATE ( BONDPS )
   IF ( ALLOCATED ( DIHEPS ) ) DEALLOCATE ( DIHEPS )
   IF ( ALLOCATED ( IMPRPS ) ) DEALLOCATE ( IMPRPS )

   ! . Free the type array.
   IF ( ALLOCATED ( TYPES ) ) DEALLOCATE ( TYPES )

   ! . Free the link data.
   IF ( ALLOCATED ( RESLINK ) ) THEN
      DO I = 1,NRESLINK
         CALL INITIALIZE_LINK_TYPE ( RESLINK(I) )
      END DO
      DEALLOCATE ( RESLINK )
   END IF

   ! . Free the residue data.
   IF ( ALLOCATED ( RESIDUES ) ) THEN
      DO I = 1,NRESIDUES
         CALL INITIALIZE_RESIDUE_TYPE ( RESIDUES(I) )
      END DO
      DEALLOCATE ( RESIDUES )
   END IF

   ! . Free the variant data.
   IF ( ALLOCATED ( VARIANTS ) ) THEN
      DO I = 1,NVARIANTS
         CALL INITIALIZE_VARIANT_TYPE ( VARIANTS(I) )
      END DO
      DEALLOCATE ( VARIANTS )
   END IF

   END SUBROUTINE MM_FILE_INITIALIZE

   !-------------------------
   SUBROUTINE MM_FILE_SUMMARY
   !-------------------------

   ! . Write out a summary of the MM data.
   CALL PRINT_SUMMARY_OPTIONS ( HEADER_COLOR = "#FF00FF" )
   CALL PRINT_SUMMARY_START ( "Summary of MM File Data" )
   WRITE ( PRINT_LINE, "(I14)"   ) NTYPES       ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Types"     )
   WRITE ( PRINT_LINE, "(I14)"   ) NRESIDUES    ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Residues"  )
   WRITE ( PRINT_LINE, "(I14)"   ) NVARIANTS    ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Variants"  )
   WRITE ( PRINT_LINE, "(I14)"   ) NRESLINK     ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Links"     )
   WRITE ( PRINT_LINE, "(I14)"   ) NANGLPS      ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Angles"    )
   WRITE ( PRINT_LINE, "(I14)"   ) NBONDPS      ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Bonds"     )
   WRITE ( PRINT_LINE, "(I14)"   ) NDIHEPS      ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Dihedrals" )
   WRITE ( PRINT_LINE, "(I14)"   ) NIMPRPS      ; CALL PRINT_SUMMARY_ELEMENT ( "Number of Impropers" )
   WRITE ( PRINT_LINE, "(F14.4)" ) REF_SCALE_EL ; CALL PRINT_SUMMARY_ELEMENT ( "Elect. 1-4 Scaling"  )
   WRITE ( PRINT_LINE, "(F14.4)" ) REF_SCALE_LJ ; CALL PRINT_SUMMARY_ELEMENT ( "LJ 1-4 Scaling"      )
   CALL PRINT_SUMMARY_STOP

   END SUBROUTINE MM_FILE_SUMMARY

   !============================================================================
   ! . Miscellaneous routines.
   !============================================================================

   !-----------------------------------------
   SUBROUTINE ALLOCATE_RESIDUE_TYPE ( RTYPE )
   !-----------------------------------------

   ! . Scalar arguments.
   TYPE(RESIDUE_TYPE), INTENT(INOUT) :: RTYPE

   ! . Allocate the atom arrays.
   IF ( RTYPE%NATOMS > 0 ) ALLOCATE ( RTYPE%CHARGES(1:RTYPE%NATOMS), &
                                      RTYPE%NAMES(1:RTYPE%NATOMS),   &
                                      RTYPE%TYPES(1:RTYPE%NATOMS) ) 

   ! . Allocate the bonds arrays.
   IF ( RTYPE%NBONDS > 0 ) ALLOCATE ( RTYPE%BONDS(1:2,1:RTYPE%NBONDS) )

   ! . Allocate the improper arrays.
   IF ( RTYPE%NIMPROPERS > 0 ) ALLOCATE ( RTYPE%IMPROPERS(1:4,1:RTYPE%NIMPROPERS) )

   END SUBROUTINE ALLOCATE_RESIDUE_TYPE

   !-----------------------------------------
   SUBROUTINE ALLOCATE_VARIANT_TYPE ( VTYPE )
   !-----------------------------------------

   ! . Scalar arguments.
   TYPE(VARIANT_TYPE), INTENT(INOUT) :: VTYPE

   ! . Allocate the added atom arrays.
   IF ( VTYPE%NADDS > 0 ) ALLOCATE ( VTYPE%ADDATM(1:VTYPE%NADDS), &
                                     VTYPE%ADDCHG(1:VTYPE%NADDS), &
                                     VTYPE%ADDTYP(1:VTYPE%NADDS) ) 

   ! . Allocate the bonds arrays.
   IF ( VTYPE%NBONDS > 0 ) ALLOCATE ( VTYPE%BONDS(1:2,1:VTYPE%NBONDS) )

   ! . Allocate the charges arrays.
   IF ( VTYPE%NCHARGES > 0 ) ALLOCATE ( VTYPE%CHGATM(1:VTYPE%NCHARGES), &
                                        VTYPE%CHGCHG(1:VTYPE%NCHARGES) )

   ! . Allocate the deletes arrays.
   IF ( VTYPE%NDELETES > 0 ) ALLOCATE ( VTYPE%DELATM(1:VTYPE%NDELETES) )

   ! . Allocate the improper arrays.
   IF ( VTYPE%NIMPROPERS > 0 ) ALLOCATE ( VTYPE%IMPROPERS(1:4,1:VTYPE%NIMPROPERS) )

   END SUBROUTINE ALLOCATE_VARIANT_TYPE

   !----------------------------------------
   SUBROUTINE INITIALIZE_LINK_TYPE ( LTYPE )
   !----------------------------------------

   ! . Scalar arguments.
   TYPE(LINK_TYPE), INTENT(INOUT) :: LTYPE

   ! . Initialize the link scalars.
   LTYPE%NAME       = " "

   ! . Initialize the variant types.
   CALL INITIALIZE_VARIANT_TYPE ( LTYPE%RESIDUE1 )
   CALL INITIALIZE_VARIANT_TYPE ( LTYPE%RESIDUE2 )

   END SUBROUTINE INITIALIZE_LINK_TYPE

   !-------------------------------------------
   SUBROUTINE INITIALIZE_RESIDUE_TYPE ( RTYPE )
   !-------------------------------------------

   ! . Scalar arguments.
   TYPE(RESIDUE_TYPE), INTENT(INOUT) :: RTYPE

   ! . Deallocate the atom arrays.
   IF ( RTYPE%NATOMS > 0 ) DEALLOCATE ( RTYPE%CHARGES, RTYPE%NAMES, RTYPE%TYPES ) 

   ! . Deallocate the bonds arrays.
   IF ( RTYPE%NBONDS > 0 ) DEALLOCATE ( RTYPE%BONDS )

   ! . Deallocate the improper arrays.
   IF ( RTYPE%NIMPROPERS > 0 ) DEALLOCATE ( RTYPE%IMPROPERS )

   ! . Initialize the residue variables.
   RTYPE%NAME       = " "
   RTYPE%NATOMS     = 0
   RTYPE%NBONDS     = 0
   RTYPE%NIMPROPERS = 0

   END SUBROUTINE INITIALIZE_RESIDUE_TYPE

   !-------------------------------------------
   SUBROUTINE INITIALIZE_VARIANT_TYPE ( VTYPE )
   !-------------------------------------------

   ! . Scalar arguments.
   TYPE(VARIANT_TYPE), INTENT(INOUT) :: VTYPE

   ! . Deallocate the added atom arrays.
   IF ( VTYPE%NADDS > 0 ) DEALLOCATE ( VTYPE%ADDATM, VTYPE%ADDCHG, VTYPE%ADDTYP ) 

   ! . Deallocate the bonds arrays.
   IF ( VTYPE%NBONDS > 0 ) DEALLOCATE ( VTYPE%BONDS )

   ! . Deallocate the charges arrays.
   IF ( VTYPE%NCHARGES > 0 ) DEALLOCATE ( VTYPE%CHGATM, VTYPE%CHGCHG )

   ! . Deallocate the deletes arrays.
   IF ( VTYPE%NDELETES > 0 ) DEALLOCATE ( VTYPE%DELATM )

   ! . Deallocate the improper arrays.
   IF ( VTYPE%NIMPROPERS > 0 ) DEALLOCATE ( VTYPE%IMPROPERS )

   ! . Initialize the variant variables.
   VTYPE%NAME         = " "
   VTYPE%RESIDUE_NAME = " "
   VTYPE%NADDS        = 0
   VTYPE%NBONDS       = 0
   VTYPE%NCHARGES     = 0
   VTYPE%NDELETES     = 0
   VTYPE%NIMPROPERS   = 0

   END SUBROUTINE INITIALIZE_VARIANT_TYPE

END MODULE MM_FILE_DATA
