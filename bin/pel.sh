#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                      pel                      #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe pel [options]


##  DEFAULT VARIABLES  ################################################

  name_def="scan"
  qm_method_def="AM1"


##  SCRIPT  ###########################################################

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

      -n|--name )                 # name of the PEL (optional)
        name=$1
        shift
        ;;

      -s|--system )               # system selection
        system=$1
        system_dir=${mervinmonroe}/${systems_subfolder}/${system}
        shift
        # check if system is set
        if [[ ! -d "$system_dir" ]]; then
          echo "ERROR: System not recognized"
          exit
        fi
        ;;

      -f|--file )                 # configuration file
        pel_file=$1
        shift
        ;;

      -c|--coord )                # initial coordinate file
        coord_file=$1
        shift
        # check if coordenate file exists
        if [[ ! -f "${workdir}/${coord_file}" ]]; then
          echo "ERROR: Coordenate file not found"
          exit
        fi
        ;;

      --method )                  # QM method
        qm_method=$1
        shift
        ;;

      --process )                 # process ended PEL
        process=1
        ;;

      -j )                        # .job only
        job_only=1
        ;;

      -h|--help )                 # print help and exit
        echo "---------------  MERVIN MONROE  ---------------"
        echo "     A lazy interface for fDynamo software     "
        echo
        echo "Version $mervinmonroe_version"
        echo
        echo
        echo "··············       scan       ···············"
        echo
        echo "USAGE:   mervinmonroe scan [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -s | --system  <system>           set the system previously defined"
        echo " -f | --file  <.mm>                configuration file"
        echo " -c | --coord  <.crd>              intial coordinates file"
        echo
        echo " -n | --name  <name>               name of the scan (def: $name_def)"
        echo " --method  <qm method>             QM method (def: $qm_method_def)"
        echo " -j                                job only (creates files but do not launch)"
        echo " -h | --help                       print this help and exit"
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
  job_only=${job_only:=0}

  ## Process (if requested)
  if [ "$process" == 1 ]; then
    mkdir scan-crd
    mv pes.*.crd scan-crd/
    cp ep.XX.out ${name}-${qm_method}.dat
    exit
  fi

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  if [ ! -n "$pel_file" ]; then echo "ERROR: No configuration file set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No initial coordinates set"; exit; fi

  ## Read config pel file
  # remove comments (lines starting with #)
  sed -i '/^#/d' ${workdir}/${pel_file}
  i_val=`sed -n 1p ${workdir}/${pel_file}`
  # read all atoms in an array starting in 0
  qm_atoms=(`sed -n '/^&$/,/^&$/p' ${workdir}/${pel_file} | sed '/^&/d'`)
  # count number of atoms
  qm_atoms_n=`echo "${#qm_atoms[@]} / 3" | bc`
  # read constrains
  constr1_line=`echo "$qm_atoms_n+4" | bc`
  constr1=(`sed -n ${constr1_line}p ${workdir}/${pel_file}`)

  ## Build the f90 file
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
  cp ${mervinmonroe}/${templates_subfolder}/pel/01-pel  ${workdir}/${name}.f90
  # declare atoms as integers
  integer_atoms="a1"
  for n in `seq 2 $qm_atoms_n`; do
    integer_atoms+=", a${n}"
  done
  sed -i "s/MERVIN_ATOMS/$integer_atoms/g" ${workdir}/${name}.f90
  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/pel/02-pel >> ${workdir}/${name}.f90
  sed -i "s/MERVIN_METHOD/$qm_method/g" ${workdir}/${name}.f90
  # define constrained atoms
  echo >> ${workdir}/${name}.f90
  for n in `seq 0 $((qm_atoms_n - 1))`; do
    echo "  a$((n + 1)) = atom_number( subsystem = '${qm_atoms[$((n * 3))]}', residue_number = ${qm_atoms[$((n * 3 + 1))]}, atom_name =  '${qm_atoms[$((n * 3 + 2))]}' )" >> ${workdir}/${name}.f90
  done
  echo >> ${workdir}/${name}.f90

  # constrain 1
  if [ "${constr1[0]}" == "m" ]; then
    cat ${mervinmonroe}/${templates_subfolder}/pel/03-pel-multiple >> ${workdir}/${name}.f90
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
    cat ${mervinmonroe}/${templates_subfolder}/pel/03-pel-distance >> ${workdir}/${name}.f90
    sed -i "s/MERVIN_FC/${constr1[1]}/g" ${workdir}/${name}.f90
    sed -i "s/MERVIN_A1/a${constr1[3]}/g" ${workdir}/${name}.f90
    sed -i "s/MERVIN_A2/a${constr1[4]}/g" ${workdir}/${name}.f90
    sed -i "s/MERVIN_D0/${constr1[6]}/g" ${workdir}/${name}.f90
    sed -i "s/MERVIN_STEP/${constr1[7]}/g" ${workdir}/${name}.f90
  fi

  # print options
  # constrain 1
  if [ "${constr1[0]}" == "m" ] && [ "${constr1[1]}" == "-1" ]; then
    print_constr1="geometry_distance( atmcrd, a${constr1[4]}, a${constr1[5]} ) - geometry_distance( atmcrd, a${constr1[6]}, a${constr1[7]} ), \&"
  elif [ "${constr1[0]}" == "m" ] && [ "${constr1[1]}" == "1" ]; then
    print_constr1="geometry_distance( atmcrd, a${constr1[4]}, a${constr1[5]} ) + geometry_distance( atmcrd, a${constr1[6]}, a${constr1[7]} ), \&"
  elif [ "${constr1[0]}" == "d" ]; then
    print_constr1="geometry_distance( atmcrd, a${constr1[3]}, a${constr1[4]}), \& "
  fi

  sed -i "s/MERVIN_PRINT_CONSTR1/$print_constr1/g" ${workdir}/${name}.f90

  ## Compile
  ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}.f90

  ## Build the jobber
  cp ${mervinmonroe}/${templates_subfolder}/pel/jobber  ${workdir}/${name}.jobber
  sed -i "s/MERVIN_JOBNAME/${system}-${name}/g" ${workdir}/${name}.jobber
  sed -i "s|MERVIN_MSG_FOLDER|${msg_folder}|g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_Q_SGE/${queue_sge}/g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_Q_SLURM/${queue_slurm}/g" ${workdir}/${name}.jobber
  sed -i "s|MERVIN_WORKDIR|${workdir}|g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_I/${i_val}/g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_COORD/${coord_file}/g" ${workdir}/${name}.jobber

  ## launch
  if [ ${job_only} == "0" ]; then
    { qsub ${name}.jobber ; } 2>/dev/null
    { sbatch ${name}.jobber ; } 2>/dev/null
  fi
