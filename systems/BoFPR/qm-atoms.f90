
  !! QM ATOMS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  qm_charge = 0
  ! FAD
  acs =  atom_selection( subsystem = (/ "B" /) )
  ! NAD
  acs =  acs .or. atom_selection( subsystem = (/ "C" /), &
                                  residue_number = (/ 1 /), &
                                  atom_name = (/ "C4N", "H4N1", "H4N2", "C5N", "H5N", &
                                  "C6N", "H6N", "N1N", "C2N", "H2N", "C3N", "C7N", &
                                  "O7N", "N7N", "H7N1", "H7N2", "C1D", "H1D", "O4D", &
                                  "C2D", "O2D", "HO2D", "H2D", "C3D", "O3D", "HO3D", &
                                  "H3D", "C4D", "H4D" /) )
