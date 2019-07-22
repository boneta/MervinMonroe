#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                     info                      #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe info [options]


##  DEFAULT VARIABLES  ##############################################

  # name_def="sys"

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

      "-s"|"--system" )       # system
        system=$1
        system_dir=${mervinmonroe}/${systems_subfolder}/${system}
        shift
        # check if system is set
        if [[ ! -d "$system_dir" ]]; then
          echo "ERROR: System not recognized"
          exit
        fi
        echo "SYSTEM: '$system'"
        ls -lh $system_dir
        echo
        exit
        ;;

      "--sys-avail" )         # list systems imported
        ls -lh ${mervinmonroe}/${systems_subfolder}
        exit
        ;;

      "--template" )          # print template
        template=$1
        case $template in
          "qm-atoms.f90")
            cat ${mervinmonroe}/${templates_subfolder}/samples/qm-atoms.f90
            exit
          ;;
          "pes")
            cat ${mervinmonroe}/${templates_subfolder}/samples/pes.mervin
            exit
          ;;
          "pel")
            cat ${mervinmonroe}/${templates_subfolder}/samples/pel.mervin
            exit
          ;;
          *)
            echo "ERROR: Wrong template '$arg'."
            exit ;;
        esac
        ;;

      "-h"|"--help" )         # print help and exit
        echo "---------------  MERVIN MONROE  ---------------"
        echo "     A lazy interface for fDynamo software     "
        echo
        echo "Version $mervinmonroe_version"
        echo
        echo
        echo "··············       info       ···············"
        echo
        echo "USAGE:   mervinmonroe info [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -s | --system  <system>          show files of the specified system"
        echo " --sys-avail                      list imported systems"
        echo " --template  <template name>      print a template sample:"
        echo "                                    qm-atoms.f90    selection of atoms for the qm part"
        echo "                                    pes             PES configuration file"
        echo "                                    pel             PEL configuration file"
        echo " -h | --help                      print this help and exit"
        echo
        exit ;;
      *)
        echo "ERROR: Wrong option '$arg'. Use -h for help."
        exit ;;
    esac
  done
