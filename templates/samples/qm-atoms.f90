
  !! QM ATOMS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  qm_charge = 0
  !
  acs =  atom_selection( subsystem = (/ "X" /) )
  !
  acs =  acs .or. atom_selection( subsystem = (/ "Y" /), &
                                  residue_number = (/ 1 /), &
                                  atom_name = (/ "NNN", "NN ", &
                                                 "NNN" /) )
