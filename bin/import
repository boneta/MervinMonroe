#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                    import                     #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe import [options]


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

      "-s"|"--system" )       # system name
        system=$1
        system_dir=${mervinmonroe}/${systems_subfolder}/${system}
        shift
        ;;

      "-f"|"--file" )         # files to import
        import_files=$@
        break
        ;;

      # "-j" )                 # .job only
      #   job_only=1
      #   ;;

      "-h"|"--help" )         # print help and exit
        echo "---------------  MERVIN MONROE  ---------------"
        echo "     A lazy interface for fDynamo software     "
        echo
        echo "Version $mervinmonroe_version"
        echo
        echo
        echo "··············     importer     ···············"
        echo
        echo "USAGE:   mervinmonroe importer [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -s | --system  <system name>                         set the system name"
        echo " -f | --file  <.bin> <nofix.f90> <qm-atoms.f90>       files to import"
        echo " -h | --help                                          print this help and exit"
        echo
        exit ;;
      *)
        echo "ERROR: Wrong option '$arg'. Use -h for help."
        exit ;;
    esac
  done

  ## Check for mandatory inputs
  if [ ! -n "$system" ]; then echo "ERROR: No system set"; exit; fi

  # check if system is set
  if [[ -d "$system_dir" ]]; then
    echo "CAUTION. There is a system with the same name."
    read -p "Overwrite? (y/n): " overwrite
    overwrite=${overwrite,,}       # make it lowercase
    if [ "$overwrite" == "y" ] || [ "$overwrite" == "yes" ]; then
      echo "Overwriting."
      rm -rf $system_dir
    else
      echo "Import cancelled. Exiting."
      exit
    fi
  fi

  mkdir $system_dir
  cp $import_files $system_dir/

  if [ "$?" == "0" ]; then
    echo "System '$system' imported correctly."
  else
    echo "ERROR. Operation could not be completed correctly."
  fi
