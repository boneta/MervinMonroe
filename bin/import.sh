#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                    import                     #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe import [options]


##  DEFAULT VARIABLES  ################################################

  # name_def="sys"

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

      -s|--system )               # system name
        system=$1
        system_dir=${mervinmonroe}/${systems_subfolder}/${system}
        shift
        ;;

      -f|--files )                # files to import
        import_files=$@
        break
        ;;

      --remove )                  # remove system
        if [ ! -n "$1" ]; then echo "ERROR: No system defined"; exit; fi
        system=$1
        system_dir=${mervinmonroe}/${systems_subfolder}/${system}
        if [[ -d "$system_dir" ]]; then
          read -p "Remove '$system' system? (y/n): " remove
          remove=${remove,,}       # make it lowercase
          if [ "$remove" == "y" ] || [ "$remove" == "yes" ]; then
            echo "Removing."
            rm -rf $system_dir
          else
            echo "Remove cancelled. Exiting."
          fi
        else
          echo "ERROR: System '$system' could not be found"
        fi
        exit ;;

      -h|--help )                 # print help and exit
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
        echo " -f | --files  <.bin> <nofix.f90> <qm-atoms.f90>      files to import"
        echo
        echo " --remove  <system name>                              remove system"
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
    else
      echo "Import cancelled. Exiting."
      exit
    fi
  else
    mkdir $system_dir
  fi

  cp -f $import_files $system_dir/

  if [ "$?" == "0" ]; then
    echo "System '$system' imported correctly."
  else
    echo "ERROR. Operation could not be completed correctly."
  fi
