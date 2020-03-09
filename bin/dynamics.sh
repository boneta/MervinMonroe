#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                   dynamics                    #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe dynamics [options]


##  DEFAULT VARIABLES  ##############################################

  name_def="dyn"
  temperature_def="298.15"
  qm_method_def="AM1"
  # qm_charge_def="0"


##  SCRIPT  #########################################################

  ## Checks if no arguments are in the input
  if [ "$1" == "" ]; then
    echo "ERROR: No input arguments. Use -h for help."
    exit
  fi

  ## Input arguments assignment
  while [ $# -gt 0 ]; do
    arg="$1"
    shift
    case $arg in

      "--name" )              # name of the PMF (optional)
        name=$1
        shift
        ;;

      "-s"|"--system" )       # system selection
        system=$1
        system_dir=${mervinmonroe}/${systems_subfolder}/${system}
        shift
        # check if system is set
        if [[ ! -d "$system_dir" ]]; then
          echo "ERROR: System not recognized"
          exit
        fi
        ;;

      "-f"|"--file" )         # configuration file
        pmf_file=$1
        shift
        ;;

      "-c"|"--coord" )        # coordinate files
        coord_file=$1
        shift
        # check if coordenate file exists
        if [[ ! -f "${workdir}/${coord_file}" ]]; then
          echo "ERROR: Coordenate file not found"
          exit
        fi
        ;;

      "-v"|"--vel" )          # velocities files
        continuation=1
        vel_file=$1
        shift
        # check if coordenate file exists
        if [[ ! -f "${workdir}/${vel_file}" ]]; then
          echo "ERROR: Velocities file not found"
          exit
        fi
        ;;

      # "-t"|"--temperature" ) # temperature for the simulation
      #   temperature=$1
      #   shift
      #   ;;

      "--method" )            # QM method
        qm_method=$1
        shift
        ;;

      # "--charge" )            # QM charge
      #   qm_charge=$1
      #   shift
      #   ;;

      "--constr" )           # use constraints
        constr=1
        ;;

      "-j" )                 # .job only
        job_only=1
        ;;

      "-h"|"--help" )         # print help and exit
        echo "---------------  MERVIN MONROE  ---------------"
        echo "     A lazy interface for fDynamo software     "
        echo
        echo "Version $mervinmonroe_version"
        echo
        echo
        echo "··············     dynamics     ···············"
        echo
        echo "USAGE:   mervinmonroe dynamics [options]"
        echo
        echo "OPTIONS:                                     "
        echo " --name              name of the PMF (def: $name_def)"
        echo " -s | --system       set the system previously defined"
        echo " -f | --file         configuration file"
        echo " -c | --coord        coordinates file"
        echo " -v | --vel          velocities file (continue production)"
        echo " --method            QM method (def: $qm_method_def)"
        echo " --constr            include constraints"
        # echo " --charge            QM charge (def: $qm_charge_def)"
        # echo " -t | --temperature  temperature bath (def: )"
        echo " -j                  job only (creates files but do not launch)"
        echo " -h | --help         print this help and exit"
        echo
        exit ;;
      *)
        echo "ERROR: Wrong option '$arg'. Use -h for help."
        exit ;;
    esac
  done

  ## Default variables if not input
  name=${name:=$name_def}
  qm_method=${qm_method:=$qm_method_def}
  # temperature=${temperature:=$temperature_def}
  # qm_charge=${qm_charge:=$qm_charge_def}
  continuation=${continuation:=0}
  constr=${constr:=0}
  job_only=${job_only:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No coordinates file set"; exit; fi
  if [ ! -n "$pmf_file" ]; then echo "ERROR: No configuration file set"; exit; fi

  ## Read config pmf file
  # remove comments (lines starting with #)
  sed -i '/^#/d' ${workdir}/${pmf_file}
  # read i and j
  i_val=`sed -n 1p ${workdir}/${pmf_file}`
  j_val=`sed -n 2p ${workdir}/${pmf_file}`
  # read all atoms in an array starting in 0
  qm_atoms=(`sed -n '/^&$/,/^&$/p' ${workdir}/${pmf_file} | sed '/^&/d'`)
  # count number of atoms
  qm_atoms_n=`echo "${#qm_atoms[@]} / 3" | bc`
  # read constrains
  constr1_line=`echo "$qm_atoms_n+5" | bc`
  constr1=(`sed -n ${constr1_line}p ${workdir}/${pmf_file}`)
  constr2_line=`echo "$constr1_line+1" | bc`
  constr2=(`sed -n ${constr2_line}p ${workdir}/${pmf_file}`)
  # read MD options
  md_line=`echo "$constr2_line+1" | bc`
  md_options=(`sed -n ${md_line}p ${workdir}/${pmf_file}`)
  temperature=${md_options[0]}
  equilibration_ps=${md_options[1]}
  production_ps=${md_options[2]}


  ## Build the f90 file
  # copy system files
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
  cp ${mervinmonroe}/${templates_subfolder}/dynamics/01-dynamics  ${workdir}/${name}.f90
  # declare atoms as integers
  integer_atoms="a1"
  for n in `seq 2 $qm_atoms_n`; do
    integer_atoms+=", a${n}"
  done
  sed -i "s/MERVIN_ATOMS/$integer_atoms/g" ${workdir}/${name}.f90
  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # set the initial coordinates to be read
  sed -i "s/MERVIN_COORD_IN/$coord_file/g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/dynamics/02-dynamics >> ${workdir}/${name}.f90
  sed -i "s/MERVIN_METHOD/$qm_method/g" ${workdir}/${name}.f90

  # Constraints
  if [ "$constr" == "1" ]; then
    # define constrained atoms
    echo >> ${workdir}/${name}.f90
    for n in `seq 0 $((qm_atoms_n - 1))`; do
      echo "  a$((n + 1)) = atom_number( subsystem = '${qm_atoms[$((n * 3))]}', residue_number = ${qm_atoms[$((n * 3 + 1))]}, atom_name =  '${qm_atoms[$((n * 3 + 2))]}' )" >> ${workdir}/${name}.f90
    done
    echo >> ${workdir}/${name}.f90

    # constrain 1
    if [ "${constr1[0]}" == "m" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/dynamics/03-dynamics-multiple >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr1[2]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr1[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr1[5]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A3/a${constr1[6]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A4/a${constr1[7]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr1[9]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_STEP/${constr1[10]}/g" ${workdir}/${name}.f90
      if [ "${constr1[1]}" == "-1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_antisym/g" ${workdir}/${name}.f90
      elif [ "${constr1[1]}" == "1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_sym/g" ${workdir}/${name}.f90
      fi
    elif [ "${constr1[0]}" == "d" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/dynamics/03-dynamics-distance >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr1[1]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr1[3]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr1[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr1[6]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_STEP/${constr1[7]}/g" ${workdir}/${name}.f90
    fi

    # constrain 2
    if [ "${constr2[0]}" == "m" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/dynamics/04-dynamics-multiple >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr2[2]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr2[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr2[5]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A3/a${constr2[6]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A4/a${constr2[7]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr2[9]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_STEP/${constr2[10]}/g" ${workdir}/${name}.f90
      if [ "${constr2[1]}" == "-1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_antisym/g" ${workdir}/${name}.f90
      elif [ "${constr2[1]}" == "1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_sym/g" ${workdir}/${name}.f90
      fi
    elif [ "${constr2[0]}" == "d" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/dynamics/04-dynamics-distance >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr2[1]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr2[3]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr2[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr2[6]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_STEP/${constr2[7]}/g" ${workdir}/${name}.f90
    fi
  fi

  # New or continue .f90
  if [ "$continuation" == "1" ]; then
    cat ${mervinmonroe}/${templates_subfolder}/dynamics/05-dynamics-continue >> ${workdir}/${name}.f90
    sed -i "s/MERVIN_VEL_IN/${vel_file}/g" ${workdir}/${name}.f90
  else
    cat ${mervinmonroe}/${templates_subfolder}/dynamics/05-dynamics-new >> ${workdir}/${name}.f90
    sed -i "s/MERVIN_EQUILIBRATION_STEPS/${equilibration_ps}000/g" ${workdir}/${name}.f90
  fi

  # MD options
  sed -i "s/MERVIN_TEMPERATURE/${temperature}/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_PRODUCTION_STEPS/${production_ps}000/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_COORD_OUT/${name}/g" ${workdir}/${name}.f90


  ## Compile
  ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}.f90

  ## Build the jobber
  cp ${mervinmonroe}/${templates_subfolder}/dynamics/jobber  ${workdir}/${name}.job
  sed -i "s/MERVIN_JOBNAME/${system}-${name}/g" ${workdir}/${name}.job
  sed -i "s|MERVIN_WORKDIR|${workdir}|g" ${workdir}/${name}.job

  ## launch
  if [ ${job_only} == "0" ]; then
    { qsub ${name}.job ; } 2>/dev/null
    { sbatch ${name}.job ; } 2>/dev/null
  fi
