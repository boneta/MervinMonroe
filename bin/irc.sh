#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                      irc                      #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe irc [options]


##  DEFAULT VARIABLES  ##############################################

  name_def="IRC"
  qm_method_def="AM1"
  irc_direction_def="full"


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

      "--name" )              # name of the location
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

      # "-f"|"--file" )         # configuration file
      #   pel_file=$1
      #   shift
      #   ;;

      "-c"|"--coord" )        # initial coordinate file
        coord_file=$1
        shift
        # check if coordenate file exists
        if [[ ! -f "${workdir}/${coord_file}" ]]; then
          echo "ERROR: Coordenate file not found"
          exit
        fi
        ;;

      "--method" )            # QM method
        qm_method=$1
        shift
        ;;

      "-d"|"--direction" )   # IRC direction
        irc_direction=$1
        shift
        ;;

      "-l"|"--locate" )      # locate too
        locate=1
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
        echo "··············        irc       ···············"
        echo
        echo "USAGE:   mervinmonroe irc [options]"
        echo
        echo "OPTIONS:                                     "
        echo " --name  <name>                 name of the irc (def: $name_def)"
        echo " -s | --system  <system>        set the system previously defined"
        echo " -c | --coord  <.crd>           intial coordinates file"
        echo " --method  <qm method>          QM method (def: $qm_method_def)"
        echo " -d | --direction  <>           direction of IRC (def: $irc_direction_def)"
        echo "                                  full          back and forward"
        echo "                                  back          only back (-1)"
        echo "                                  for           only forward (1)"
        echo " -l | --locate                  launch locate jobs after IRC"
        echo " -j                             job only (creates files but do not launch)"
        echo " -h | --help                    print this help and exit"
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
  job_only=${job_only:=0}
  locate=${locate:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  # if [ ! -n "$pel_file" ]; then echo "ERROR: No configuration file set"; exit; fi
  if [ ! -n "$coord_file" ]; then echo "ERROR: No initial coordinates set"; exit; fi

  ## Build the f90 file
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
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
        if [ "$dir" == "back" ]; then
          sed -i "s/MERVIN_IRC_DIRECTION/-1/g" ${name}-${dir}.f90
        elif [ "$dir" == "for" ]; then
          sed -i "s/MERVIN_IRC_DIRECTION/1/g" ${name}-${dir}.f90
        fi
        sed -i "s/MERVIN_COORD_OUT/${name}-${dir}/g" ${name}-${dir}.f90
        ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}-${dir}.f90
        cp ${mervinmonroe}/${templates_subfolder}/irc/jobber  ${name}-${dir}.job
        sed -i "s/MERVIN_JOBNAME/${system}-${name}-${dir}/g" ${name}-${dir}.job
        sed -i "s|MERVIN_WORKDIR|`pwd`|g" ${name}-${dir}.job
        if [ ${locate} == "1" ]; then
          echo ""                                                                          >> ${name}-${dir}.job
          echo "${mervinmonroe}/mervinmonroe locate  \\"                                   >> ${name}-${dir}.job
          echo "  -s $system --method $qm_method --name ${dir}-locate -c ${name}-${dir}.crd" >> ${name}-${dir}.job
        fi
        if [ ${job_only} == "0" ]; then
          { qsub ${name}-${dir}.job ; } 2>/dev/null
          { sbatch ${name}-${dir}.job ; } 2>/dev/null
        fi
        cd ..
      done
    ;;
    "back"|"for")
      dir=$irc_direction
      mkdir ${name}-${dir}
      cp $coord_file ${name}-${dir}/
      cp ${name}.f90 ${name}-${dir}/${name}-${dir}.f90
      cd ${name}-${dir}
      cp ${system_dir}/*.bin  .
      cp ${system_dir}/nofix.f90  .
      if [ "$dir" == "back" ]; then
        sed -i "s/MERVIN_IRC_DIRECTION/-1/g" ${name}-${dir}.f90
      elif [ "$dir" == "for" ]; then
        sed -i "s/MERVIN_IRC_DIRECTION/1/g" ${name}-${dir}.f90
      fi
      sed -i "s/MERVIN_COORD_OUT/${name}-${dir}/g" ${name}-${dir}.f90
      ${mervinmonroe}/${scripts_subfolder}/compile.sh --version std -f ${name}-${dir}.f90
      cp ${mervinmonroe}/${templates_subfolder}/irc/jobber  ${name}-${dir}.job
      sed -i "s/MERVIN_JOBNAME/${system}-${name}-${dir}/g" ${name}-${dir}.job
      sed -i "s|MERVIN_WORKDIR|`pwd`|g" ${name}-${dir}.job
      if [ ${locate} == "1" ]; then
        echo ""                                                                          >> ${name}-${dir}.job
        echo "${mervinmonroe}/mervinmonroe locate  \\"                                   >> ${name}-${dir}.job
        echo "  -s $system --method $qm_method --name ${dir}-locate -c ${name}-${dir}.crd" >> ${name}-${dir}.job
      fi
      if [ ${job_only} == "0" ]; then
        { qsub ${name}-${dir}.job ; } 2>/dev/null
        { sbatch ${name}-${dir}.job ; } 2>/dev/null
      fi
      cd ..
    ;;
  esac
