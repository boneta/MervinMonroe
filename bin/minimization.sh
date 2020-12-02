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
  # qm_charge_def="0"
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
        echo " --method  <qm method>             QM method (def: $qm_method_def)"
        echo " --cg-steps  <#>                   Conjugate Gradient maximum steps (def: $cg_steps_def)"
        echo " --cg-tolerance  <#>               Conjugate Gradient convergence tolerance (def: $cg_tolerance_def)"
        echo " --lbfgsb-steps  <#>               L-BFGS-B maximum steps (def: $lbfgsb_steps_def)"
        echo " --lbfgsb-tolerance  <#>           L-BFGS-B convergence tolerance (def: $lbfgsb_tolerance_def)"
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
  cg_steps=${cg_steps:=$cg_steps_def}
  cg_tolerance=${cg_tolerance:=$cg_tolerance_def}
  lbfgsb_steps=${lbfgsb_steps:=$lbfgsb_steps_def}
  lbfgsb_tolerance=${lbfgsb_tolerance:=$lbfgsb_tolerance_def}
  job_only=${job_only:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No initial coordinates set"; exit; fi


  ## Build the f90 file
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
  cp ${mervinmonroe}/${templates_subfolder}/mini/01-mini  ${workdir}/${name}.f90

  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # set the initial coordinates to be read
  sed -i "s/MERVIN_COORD_IN/$coord_file/g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/mini/02-mini >> ${workdir}/${name}.f90
  sed -i "s/MERVIN_METHOD/$qm_method/g" ${workdir}/${name}.f90
  # optimization algorithms options
  sed -i "s/MERVIN_CG_STEPS/$cg_steps/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_CG_TOLERANCE/$cg_tolerance/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_LBFGSB_STEPS/$lbfgsb_steps/g" ${workdir}/${name}.f90
  sed -i "s/MERVIN_LBFGSB_TOLERANCE/$lbfgsb_tolerance/g" ${workdir}/${name}.f90
  # set output coordinates
  sed -i "s/MERVIN_COORD_OUT/${coord_file%.*}-mini/g" ${workdir}/${name}.f90

  ## Compile
  ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}.f90

  ## Build the job
  cp ${mervinmonroe}/${templates_subfolder}/mini/jobber  ${workdir}/${name}.job
  sed -i "s/MERVIN_JOBNAME/${system}-${name}/g" ${workdir}/${name}.job
  sed -i "s|MERVIN_MSG_FOLDER|${msg_folder}|g" ${workdir}/${name}.job
  sed -i "s/MERVIN_Q_SGE/${queue_sge}/g" ${workdir}/${name}.job
  sed -i "s/MERVIN_Q_SLURM/${queue_slurm}/g" ${workdir}/${name}.job
  sed -i "s|MERVIN_WORKDIR|${workdir}|g" ${workdir}/${name}.job

  ## launch
  if [ ${job_only} == "0" ]; then
    { qsub ${name}.job ; } 2>/dev/null
    { sbatch ${name}.job ; } 2>/dev/null
  fi
