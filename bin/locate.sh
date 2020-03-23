#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                    locate                     #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe locate [options]


##  DEFAULT VARIABLES  ################################################

  name_def="locate"
  qm_method_def="AM1"
  ts_search_def=".false."
  functional_def="M062X"
  basis_def="6-31+G(d,p)"


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

      -n|--name )                 # name of the location
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

      -ts )                       # TS - Saddle point search
        ts_search=".true."
        ;;

      -gauss )                    # Gaussian calc
        gauss_loc="-gauss"
        ;;

      --functional )              # DFT functional
        qm_method=$1
        shift
        ;;

      --basis )                   # Basis Set
        qm_method=$1
        shift
        ;;

      -irc )                      # IRC and locate after
        ts_search=".true."
        irc=1
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
        echo "··············      locate      ···············"
        echo
        echo "USAGE:   mervinmonroe locate [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -s | --system  <system>           set the system previously defined"
        echo " -c | --coord  <.crd>              intial coordinates file"
        echo
        echo " -n | --name  <name>               name of the localization (def: $name_def)"
        echo " -ts                               TS search"
        echo " -irc                              IRC and locate jobs afterwards (implies ts)"
        echo " --method  <qm method>             QM method (def: $qm_method_def)"
        echo " -gauss                            use Gaussian"
        echo " --functional  <functional>        DFT functional (def: $functional_def)"
        echo " --basis  <basis>                  basis set (def: $basis_def)"
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
  ts_search=${ts_search:=$ts_search_def}
  functional=${functional:=$functional_def}
  basis=${basis:=$basis_def}
  job_only=${job_only:=0}
  irc=${irc:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No initial coordinates set"; exit; fi

  ## Build the f90 file
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
  cp ${mervinmonroe}/${templates_subfolder}/locate/01-locate${gauss_loc}  ${workdir}/${name}.f90

  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # set the initial coordinates to be read
  sed -i "s/MERVIN_COORD_IN/$coord_file/g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/locate/02-locate${gauss_loc} >> ${workdir}/${name}.f90
  sed -i "s/MERVIN_METHOD/$qm_method/g" ${workdir}/${name}.f90
  # set saddle search (TS or minima)
  sed -i "s/MERVIN_SADDLE/$ts_search/g" ${workdir}/${name}.f90
  # set output coordinates
  if [ "$ts_search" == ".true." ]; then
    sed -i "s/MERVIN_COORD_OUT/${name}-ts/g" ${workdir}/${name}.f90
  else
    sed -i "s/MERVIN_COORD_OUT/${name}-loc/g" ${workdir}/${name}.f90
  fi

  ## Compile and with_gaussian
  if [ "$gauss_loc" == "-gauss" ]; then
    # with_gaussian
    cp ${mervinmonroe}/${templates_subfolder}/locate/with_gaussian.f90  ${workdir}/with_gaussian.f90
    sed -i "s/MERVIN_FUNCTIONAL/$functional/g" ${workdir}/with_gaussian.f90
    sed -i "s/MERVIN_BASIS/$basis/g" ${workdir}/with_gaussian.f90
    qm_charge=`awk '/qm_charge/{print $3}' ${workdir}/${name}.f90  | sed -n 2p`
    sed -i "s/MERVIN_CHARGE/$qm_charge/g" ${workdir}/with_gaussian.f90
    # compile
    ${mervinmonroe}/${scripts_subfolder}/compile.sh --version gauss --locate -f ${name}.f90
  else
    # compile
    ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std --locate -f ${name}.f90
  fi

  ## Build the job
  cp ${mervinmonroe}/${templates_subfolder}/locate/jobber  ${workdir}/${name}.job
  sed -i "s/MERVIN_JOBNAME/${system}-${name}-${coord_file%.*}/g" ${workdir}/${name}.job
  sed -i "s/MERVIN_Q_SGE/${queue_sge}/g" ${workdir}/${name}.job
  sed -i "s/MERVIN_Q_SLURM/${queue_slurm}/g" ${workdir}/${name}.job
  sed -i "s|MERVIN_WORKDIR|${workdir}|g" ${workdir}/${name}.job
  if [ "$gauss_loc" == "-gauss" ]; then
    sed -i "s/MERVIN_CORES_SGE/$ -pe mp8 8/g" ${workdir}/${name}.job
    sed -i "s/MERVIN_CORES_SLURM/8/g" ${workdir}/${name}.job
  else
    sed -i "s/MERVIN_CORES_SGE/ /g" ${workdir}/${name}.job
    sed -i "s/MERVIN_CORES_SLURM/1/g" ${workdir}/${name}.job
  fi

  if [ ${irc} == "1" ]&&[ "$ts_search" == ".true." ]; then
    echo ""                                                                                      >> ${name}.job
    echo "${mervinmonroe}/mervinmonroe irc  \\"                                                  >> ${name}.job
    echo "  -s $system --method $qm_method --name TS -c ${name}-ts.crd --locate --hessian update.dump"  >> ${name}.job
  fi

  ## launch
  if [ ${job_only} == "0" ]; then
    { qsub ${name}.job ; } 2>/dev/null
    { sbatch ${name}.job ; } 2>/dev/null
  fi
