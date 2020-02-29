#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                  correction                   #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe correction [options]


##  DEFAULT VARIABLES  ##############################################

  name_def="corr"
  functional_def="M062X"
  basis_def="6-31+G(d,p)"
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
        corr_file=$1
        shift
        ;;

      "-c"|"--coord" )        # coordinate files location
        coord_folder="$1"
        shift
        # check if coordenate file exists
        if [[ ! -d "${workdir}/${coord_folder}" ]]; then
          echo "ERROR: Coordenate folder not found"
          exit
        fi
        ;;

      "--functional" )        # DFT functional
        qm_method=$1
        shift
        ;;

      "--basis" )             # Basis Set
        qm_method=$1
        shift
        ;;

      # "--charge" )            # QM charge
      #   qm_charge=$1
      #   shift
      #   ;;

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
        echo "·············     correction     ··············"
        echo
        echo "USAGE:   mervinmonroe correction [options]"
        echo
        echo "OPTIONS:                                     "
        echo " --name              name of the PMF (def: $name_def)"
        echo " -s | --system       set the system previously defined"
        echo " -f | --file         configuration file"
        echo " -c | --coord        coordinates folder"
        echo " --functional        DFT functional (def: $functional_def)"
        echo " --basis             basis set (def: $basis_def)"
        # echo " --charge            QM charge (def: $qm_charge_def)"
        echo " -j                  job only (creates files but do not launch)"
        echo " -h | --help         print this help and exit"
        echo
        exit ;;
      *)
        echo "ERROR: Wrong option '$subscript'. Use -h for help."
        exit ;;
    esac
  done

  ## Default variables if not input
  name=${name:=$name_def}
  functional=${functional:=$functional_def}
  basis=${basis:=$basis_def}
  # qm_charge=${qm_charge:=$qm_charge_def}
  job_only=${job_only:=0}

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi
  if [ ! -n "$corr_file" ]; then echo "ERROR: No configuration file set"; exit; fi
  if [ ! -n "$coord_folder" ]; then echo "ERROR: No coordinates folder set"; exit; fi

  ## Read config pmf file
  # remove comments (lines starting with #)
  sed -i '/^#/d' ${workdir}/${corr_file}
  # read i and j
  i_val=`sed -n 1p ${workdir}/${corr_file}`
  j_val=`sed -n 2p ${workdir}/${corr_file}`

  ## Build the f90 file
  # copy system files
  cp ${system_dir}/*.bin  ${workdir}/
  cp ${system_dir}/nofix.f90  ${workdir}/
  cp ${mervinmonroe}/${templates_subfolder}/correction/01-corr  ${workdir}/${name}.f90
  # set the system binary to be read
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.f90
  # set the crd folder
  sed -i "s|MERVIN_CRD_FOLDER|${coord_folder}|g" ${workdir}/${name}.f90
  # include the QM atoms
  cat ${system_dir}/qm-atoms.f90 >> ${workdir}/${name}.f90
  # continue the f90
  cat ${mervinmonroe}/${templates_subfolder}/correction/02-corr >> ${workdir}/${name}.f90
  # modify with_gaussian.f90
  cp ${mervinmonroe}/${templates_subfolder}/correction/with_gaussian.f90  ${workdir}/with_gaussian.f90
  sed -i "s/MERVIN_FUNCTIONAL/$functional/g" ${workdir}/with_gaussian.f90
  sed -i "s/MERVIN_BASIS/$basis/g" ${workdir}/with_gaussian.f90
  qm_charge=`awk '/qm_charge/{print $3}' ${workdir}/${name}.f90  | sed -n 2p`
  sed -i "s/MERVIN_CHARGE/$qm_charge/g" ${workdir}/with_gaussian.f90


  ## Compile
  ${mervinmonroe}/${scripts_subfolder}/compile.sh --version gauss -f ${name}.f90

  ## Build the jobber
  cp ${mervinmonroe}/${templates_subfolder}/correction/jobber  ${workdir}/${name}.jobber
  sed -i "s/MERVIN_JOBNAME/${system}-${name}/g" ${workdir}/${name}.jobber
  sed -i "s|MERVIN_WORKDIR|${workdir}|g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_I/${i_val}/g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_J/${j_val}/g" ${workdir}/${name}.jobber
  sed -i "s/MERVIN_BIN/`ls *.bin`/g" ${workdir}/${name}.jobber
  sed -i "s|MERVIN_CRD_FOLDER|${coord_folder}|g" ${workdir}/${name}.jobber
