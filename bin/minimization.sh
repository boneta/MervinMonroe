#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                 minimization                  #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe minimization [options]


##  DEFAULT VARIABLES  ################################################

  name_def="mini"
  qm_method_def="AM1"
  functional_def="M062X"
  basis_def="6-31+G(d,p)"
  cg_steps_def=100000
  cg_tolerance_def=0.2
  lbfgsb_steps_def=1000
  lbfgsb_tolerance_def=0.1


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

      -n|--name )                 # name of the minimization (optional)
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
        pmf_file=$1
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

      -gauss )                    # Gaussian calc
        gauss_min="-gauss"
        ;;

      --functional )              # DFT functional
        qm_method=$1
        shift
        ;;

      --basis )                   # Basis Set
        qm_method=$1
        shift
        ;;

      --cg-steps )                # Conjugate Gradient maximum steps
        cg_steps=$1
        shift
        ;;

      --cg-tolerance )            # Conjugate Gradient convergence tolerance
        cg_tolerance=$1
        shift
        ;;

      --lbfgsb-steps )            # L-BFGS-B maximum steps
        lbfgsb_steps=$1
        shift
        ;;

      --lbfgsb-tolerance )        # L-BFGS-B convergence tolerance
        lbfgsb_tolerance=$1
        shift
        ;;

      --constr )                  # use constraints
        constr=1
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
        echo "··············   minimization   ···············"
        echo
        echo "USAGE:   mervinmonroe minimization [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -s | --system  <system>           set the system (must be already imported)"
        echo " -c | --coord  <.crd>              intial coordinates file"
        echo
        echo " -n | --name  <name>               name of the minimization (def: $name_def)"
        echo " -f | --file  <.mm>                configuration file (2D pmf style)"
        echo " --method  <qm method>             QM method (def: $qm_method_def)"
        echo " -gauss                            use Gaussian"
        echo " --functional  <functional>        DFT functional (def: $functional_def)"
        echo " --basis  <basis>                  basis set (def: $basis_def)"
        echo " --cg-steps  <#>                   Conjugate Gradient maximum steps (def: $cg_steps_def)"
        echo " --cg-tolerance  <#>               Conjugate Gradient convergence tolerance (def: $cg_tolerance_def)"
        echo " --lbfgsb-steps  <#>               L-BFGS-B maximum steps (def: $lbfgsb_steps_def)"
        echo " --lbfgsb-tolerance  <#>           L-BFGS-B convergence tolerance (def: $lbfgsb_tolerance_def)"
        echo " --constr                          include constraints"
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
  functional=${functional:=$functional_def}
  basis=${basis:=$basis_def}
  cg_steps=${cg_steps:=$cg_steps_def}
  cg_tolerance=${cg_tolerance:=$cg_tolerance_def}
  lbfgsb_steps=${lbfgsb_steps:=$lbfgsb_steps_def}
  lbfgsb_tolerance=${lbfgsb_tolerance:=$lbfgsb_tolerance_def}
  constr=${constr:=0}
  job_only=${job_only:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No initial coordinates set"; exit; fi

  ## Read config pmf file if constrain requested
  if [ "$constr" == "1" ]; then
    if [ ! -n "$pmf_file" ]; then echo "ERROR: No configuration file set"; exit; fi
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
  fi

  ## Build the f90 file
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
  cp ${mervinmonroe}/${templates_subfolder}/mini/01-mini${gauss_min}  ${workdir}/${name}.f90

  # declare atoms as integers
  integer_atoms="a1"
  if [ "$constr" == "1" ]; then
    for n in `seq 2 $qm_atoms_n`; do
      integer_atoms+=", a${n}"
    done
  fi
  sed -i "s/MERVIN_ATOMS/$integer_atoms/g" ${workdir}/${name}.f90
  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # set the initial coordinates to be read
  sed -i "s/MERVIN_COORD_IN/$coord_file/g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/mini/02-mini${gauss_min} >> ${workdir}/${name}.f90
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
      cat ${mervinmonroe}/${templates_subfolder}/mini/03-mini-multiple >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr1[2]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr1[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr1[5]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A3/a${constr1[6]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A4/a${constr1[7]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr1[9]}/g" ${workdir}/${name}.f90
      if [ "${constr1[1]}" == "-1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_antisym/g" ${workdir}/${name}.f90
      elif [ "${constr1[1]}" == "1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_sym/g" ${workdir}/${name}.f90
      fi
    elif [ "${constr1[0]}" == "d" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/mini/03-mini-distance >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr1[1]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr1[3]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr1[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr1[6]}/g" ${workdir}/${name}.f90
    fi

    # constrain 2
    if [ "${constr2[0]}" == "m" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/mini/04-mini-multiple >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr2[2]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr2[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr2[5]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A3/a${constr2[6]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A4/a${constr2[7]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr2[9]}/g" ${workdir}/${name}.f90
      if [ "${constr2[1]}" == "-1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_antisym/g" ${workdir}/${name}.f90
      elif [ "${constr2[1]}" == "1" ]; then
        sed -i "s/MERVIN_WEIGHT/cof_sym/g" ${workdir}/${name}.f90
      fi
    elif [ "${constr2[0]}" == "d" ]; then
      cat ${mervinmonroe}/${templates_subfolder}/mini/04-mini-distance >> ${workdir}/${name}.f90
      sed -i "s/MERVIN_FC/${constr2[1]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A1/a${constr2[3]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_A2/a${constr2[4]}/g" ${workdir}/${name}.f90
      sed -i "s/MERVIN_D0/${constr2[6]}/g" ${workdir}/${name}.f90
    fi
  fi

  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/mini/05-mini${gauss_min} >> ${workdir}/${name}.f90
  # optimization algorithms options
  sed -i "s/MERVIN_CG_STEPS/$cg_steps/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_CG_TOLERANCE/$cg_tolerance/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_LBFGSB_STEPS/$lbfgsb_steps/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_LBFGSB_TOLERANCE/$lbfgsb_tolerance/g" ${workdir}/${name}.f90
  # set output coordinates
  sed -i "s/MERVIN_COORD_OUT/${coord_file%.*}-mini/g" ${workdir}/${name}.f90

  ## Compile and with_gaussian
  if [ "$gauss_min" == "-gauss" ]; then
    # with_gaussian
    cp ${mervinmonroe}/${templates_subfolder}/locate/with_gaussian.f90  ${workdir}/with_gaussian.f90
    sed -i "s/MERVIN_FUNCTIONAL/$functional/g" ${workdir}/with_gaussian.f90
    sed -i "s/MERVIN_BASIS/$basis/g" ${workdir}/with_gaussian.f90
    qm_charge=`awk '/qm_charge/{print $3}' ${workdir}/${name}.f90  | sed -n 2p`
    sed -i "s/MERVIN_CHARGE/$qm_charge/g" ${workdir}/with_gaussian.f90
    ${mervinmonroe}/${scripts_subfolder}/compile.sh --version gauss -f ${name}.f90
  else
    ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}.f90
  fi

  ## Build the job
  cp ${mervinmonroe}/${templates_subfolder}/mini/jobber  ${workdir}/${name}.job
  sed -i "s/MERVIN_JOBNAME/${system}-${name}/g" ${workdir}/${name}.job
  sed -i "s|MERVIN_MSG_FOLDER|${msg_folder}|g" ${workdir}/${name}.job
  sed -i "s/MERVIN_Q_SGE/${queue_sge}/g" ${workdir}/${name}.job
  sed -i "s/MERVIN_Q_SLURM/${queue_slurm}/g" ${workdir}/${name}.job
  sed -i "s|MERVIN_WORKDIR|${workdir}|g" ${workdir}/${name}.job
  if [ "$gauss_min" == "-gauss" ]; then
    sed -i "s/MERVIN_CORES_SGE/$ -pe mp8 8/g" ${workdir}/${name}.job
    sed -i "s/MERVIN_CORES_SLURM/8/g" ${workdir}/${name}.job
  else
    sed -i "s/MERVIN_CORES_SGE/ /g" ${workdir}/${name}.job
    sed -i "s/MERVIN_CORES_SLURM/1/g" ${workdir}/${name}.job
  fi

  ## launch
  if [ ${job_only} == "0" ]; then
    { qsub ${name}.job ; } 2>/dev/null
    { sbatch ${name}.job ; } 2>/dev/null
  fi
