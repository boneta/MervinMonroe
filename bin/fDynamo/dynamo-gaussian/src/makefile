CC   = gcc
FC   = gfortran
#CC   = mpicc
#FC   = mpif90
FLGS = -DUSE_XOR -DF95 -I. -J. -w -O2 -finit-local-zero

#FLGS += -DHACK_SPLN
#FLGS += -DHACK_PERT
#FLGS += -DHACK_URC
#FLGS += -DHACK_VRC
FLGS += -DHACK_CHRG
#FLGS += -DHACK_ABIN
#FLGS += -DHACK_KIE
#FLGS += -DHACK_GH
#FLGS += -DHACK_AMBER
#FLGS += -DHACK_OTFS

OBJS =	BLASD.o\
		LAPACK_DLAMC1.o\
		LAPACK.o\
		boundary.o\
		definitions.o\
		constants.o\
		elements.o\
		io_units.o\
		printing.o\
		string.o\
		time.o\
		files.o\
		parsing.o\
		conjugate_gradient.o\
		diagonalization.o\
		linear_algebra.o\
		linear_equations.o\
		random.o\
		sort.o\
		special_functions.o\
		statistics.o\
		baker_optimization.o\
		atoms.o\
		sequence.o\
		symmetry.o\
		atom_manipulation.o\
		connectivity.o\
		geometry.o\
		sas.o\
		transformation.o\
		superimpose.o\
		coordinate_io.o\
		pdb_io.o\
		xyz_io.o\
		zmatrix.o\
		zmatrix_io.o\
		constraint.o\
		hack_spline.o\
		wham.o\
		hack_pert.o\
		mm_file_data.o\
		mm_file_io.o\
		mm_terms.o\
		mm_system.o\
		mm_system_edit.o\
		mm_system_io.o\
		build_coordinates.o\
		mopac_data.o\
		mopac_parameters.o\
		energy_covalent.o\
		energy_nb.o\
		hessian_update.o\
		gaussian_basis.o\
		mopac_density.o\
		mopac_fock.o\
		mopac_hamiltonian.o\
		mopac_integrals.o\
		mopac_scf.o\
		mopac_analysis.o\
		mopac_gradients.o\
		quantum_energy.o\
		quantum_properties.o\
		velocity.o\
		hack_abin.o\
		hack_chrg.o\
		hack_vrc.o\
		OTFS_fitpack.o\
		OTFS_splines.o\
		OTFS_mpi.o\
		energy.o\
		numerical_derivatives.o\
		dcd_io.o\
		multipoles.o\
		normal_mode_utilities.o\
		optimize_bfgs.o\
		optimize_coordinates.o\
		reaction_path.o\
		self_avoiding_walk.o\
		lbfgs.o\
		neb_climb.o\
		neb_spline.o\
		dcd_analysis.o\
		normal_mode.o\
		thermodynamics_RRHO.o\
		hack_urc.o\
		hack_gh.o\
		dynamics_utilities.o\
		dynamics_langevin_verlet.o\
		dynamics_leapfrog_verlet.o\
		dynamics_velocity_verlet.o\
		monte_carlo_energy.o\
		monte_carlo.o\
		quickdock.o\
		amberfile_io.o\
		my_random.o\
		lbfgsb.o cg+.o\
		mini_wrapper.o\
		hack_kie.o\
		dynamo.o



.SUFFIXES: .F90 .F .c

#-------------------------------------------------------------------------------

all: $(OBJS)
	rm -f dynamo.a
	ar ruvc dynamo.a $(OBJS)


clean:
	rm -f *.o *.mod

#-------------------------------------------------------------------------------
LAPACK_DLAMC1.o:
	$(FC) -c -DUSE_XOR -DF95 -I. -J. -w -O0 -frecord-marker=4 LAPACK_DLAMC1.F -o LAPACK_DLAMC1.o

LAPACK.o:
	$(FC) -c -DUSE_XOR -DF95 -I. -J. -w -O0 -frecord-marker=4 LAPACK.F -o LAPACK.o

.F.o:
	$(FC) -c $(FLGS) -frecord-marker=4 $< -o $@

.F90.o:
	$(FC) -c $(FLGS) -frecord-marker=4 $< -o $@

.c.o:
	$(CC) -c $< -o $@

