
  !! QM ATOMS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  qm_charge = -1
  ! BPET
  acs =  atom_selection( subsystem = (/ "B" /) )
  ! SER 160 (132)
  acs =  acs .or. atom_selection( subsystem = (/ "A" /), &
                                  residue_number = (/ 132 /), &
                                  atom_name = (/ "HG ", "OG ", "CB ", "HB1", "HB2" /) )
  ! HID 237 (209)
  acs =  acs .or. atom_selection( subsystem = (/ "A" /), &
                                  residue_number = (/ 209 /), &
                                  atom_name = (/ "NE2", "CE1", "ND1", "CD2", "CG ", &
                                                 "HE1", "HD1", "HD2", "CB ", "HB1", "HB2" /) )
  ! ASP 206 (178)
  acs =  acs .or. atom_selection( subsystem = (/ "A" /), &
                                  residue_number = (/ 178 /), &
                                  atom_name = (/ "OD1", "OD2", "CG ", "CB ", "HB1", "HB2"/) )

  ! WATER MOLECULE (1427)
  acs =  acs .or. atom_selection( subsystem = (/ "BOX" /), &
                                  residue_number = (/ 1427 /) )
