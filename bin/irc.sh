#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                      irc                      #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe irc [options]


##  DEFAULT VARIABLES  ################################################

  name_def="IRC"
  qm_method_def="AM1"
  irc_direction_def="full"
  irc_steps_def=600


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

      -d|--direction )            # IRC direction
        irc_direction=$1
        shift
        ;;

      --steps )                   # Number IRC Steps
        irc_steps=$1
        shift
        ;;

      -he|--hessian )             # hessian dump
        hessian_dump=$1
        shift
        ;;

      -l|--locate )               # locate too
        locate=1
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
        echo "··············        irc       ···············"
        echo
        echo "USAGE:   mervinmonroe irc [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -s | --system  <system>           set the system previously defined"
        echo " -c | --coord  <.crd>              intial coordinates file"
        echo
        echo " -n | --name  <name>               name of the irc (def: $name_def)"
        echo " --method  <qm method>             QM method (def: $qm_method_def)"
        echo " -d | --direction  <dir>           direction of IRC (def: $irc_direction_def)"
        echo "                                     full          back and forward"
        echo "                                     back          only back (-1)"
        echo "                                     for           only forward (1)"
        echo " --steps <#>                       number of IRC steps (def: $irc_steps_def)"
        echo " -l | --locate                     launch locate jobs after IRC"
        echo " -he | --hessian <hessian>         hessian TS file to use (i.e.: update.dump)"
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
  irc_direction=${irc_direction:=$irc_direction_def}
  irc_steps=${irc_steps:=$irc_steps_def}
  job_only=${job_only:=0}
  locate=${locate:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  # if [ ! -n "$pel_file" ]; then echo "ERROR: No configuration file set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No initial coordinates set"; exit; fi

  ## Build the f90 file
  cp ${mervinmonroe}/${templates_subfolder}/irc/01-irc  ${workdir}/${name}.f90

  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # set the initial coordinates to be read
  sed -i "s/MERVIN_COORD_IN/$coord_file/g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/irc/02-irc >> ${workdir}/${name}.f90
  sed -i "s/MERVIN_METHOD/$qm_method/g" ${workdir}/${name}.f90


  case $irc_direction in
    "full" )
      for dir in back for; do
        mkdir ${name}-${dir}
        cp $coord_file ${name}-${dir}/
        cp ${name}.f90 ${name}-${dir}/${name}-${dir}.f90
        cd ${name}-${dir}
        cp ${system_dir}/*.bin  .
        cp ${system_dir}/nofix.f90  .
        sed -i "s/MERVIN_IRC_NSTEPS/$irc_steps/g" ${name}-${dir}.f90
        if [ "$dir" == "back" ]; then
          sed -i "s/MERVIN_IRC_DIRECTION/-1/g" ${name}-${dir}.f90
        elif [ "$dir" == "for" ]; then
          sed -i "s/MERVIN_IRC_DIRECTION/1/g" ${name}-${dir}.f90
        fi
        sed -i "s/MERVIN_COORD_OUT/${name}-${dir}/g" ${name}-${dir}.f90
        ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}-${dir}.f90
        cp ${mervinmonroe}/${templates_subfolder}/irc/jobber  ${name}-${dir}.job
        sed -i "s/MERVIN_JOBNAME/${system}-${name}-${dir}/g" ${name}-${dir}.job
        sed -i "s|MERVIN_MSG_FOLDER|${msg_folder}|g" ${name}-${dir}.job
        sed -i "s/MERVIN_Q_SGE/${queue_sge}/g" ${name}-${dir}.job
        sed -i "s/MERVIN_Q_SLURM/${queue_slurm}/g" ${name}-${dir}.job
        sed -i "s|MERVIN_WORKDIR|`pwd`|g" ${name}-${dir}.job
        if [ ${locate} == "1" ]; then
          echo ""                                                                          >> ${name}-${dir}.job
          echo "${mervinmonroe}/mervinmonroe locate  \\"                                   >> ${name}-${dir}.job
          echo "  -s $system --method $qm_method --name ${dir}-locate -c ${name}-${dir}.crd" >> ${name}-${dir}.job
        fi
        if [ "$hessian_dump" != "" ]; then
          cp ../${hessian_dump} update.dump
        fi
        if [ ${job_only} == "0" ]; then
          { qsub ${name}-${dir}.job ; } 2>/dev/null
          { sbatch ${name}-${dir}.job ; } 2>/dev/null
        fi
        cd ..
      done
      rm ${workdir}/${name}.f90
    ;;
    "back"|"for")
      dir=$irc_direction
      cp ${system_dir}/*.bin  .
      cp ${system_dir}/nofix.f90  .
      sed -i "s/MERVIN_IRC_NSTEPS/$irc_steps/g" ${name}.f90
      if [ "$dir" == "back" ]; then
        sed -i "s/MERVIN_IRC_DIRECTION/-1/g" ${name}.f90
      elif [ "$dir" == "for" ]; then
        sed -i "s/MERVIN_IRC_DIRECTION/1/g" ${name}.f90
      fi
      sed -i "s/MERVIN_COORD_OUT/${name}/g" ${name}.f90
      ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}.f90
      cp ${mervinmonroe}/${templates_subfolder}/irc/jobber  ${name}.job
      sed -i "s/MERVIN_JOBNAME/${system}-${name}/g" ${name}.job
      sed -i "s|MERVIN_MSG_FOLDER|${msg_folder}|g" ${name}.job
      sed -i "s/MERVIN_Q_SGE/${queue_sge}/g" ${name}.job
      sed -i "s/MERVIN_Q_SLURM/${queue_slurm}/g" ${name}.job
      sed -i "s|MERVIN_WORKDIR|`pwd`|g" ${name}.job
      if [ ${locate} == "1" ]; then
        echo ""                                                                     >> ${name}.job
        echo "${mervinmonroe}/mervinmonroe locate  \\"                              >> ${name}.job
        echo "  -s $system --method $qm_method --name ${dir}-locate -c ${name}.crd" >> ${name}.job
      fi
      if [ "$hessian_dump" != "" ]; then
        cp ${hessian_dump} update.dump
      fi
      if [ ${job_only} == "0" ]; then
        { qsub ${name}.job ; } 2>/dev/null
        { sbatch ${name}.job ; } 2>/dev/null
      fi
    ;;
  esac
