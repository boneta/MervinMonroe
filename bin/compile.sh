#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                    compile                    #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe compile [options]


##  DEFAULT VARIABLES  ##############################################

  locate_def="false"
  FC="gfortran"
  Olevel="-O3"

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

      "-f" )                   # code file
        code_file=$1
        shift
        ;;

      "-v"|"--version" )       # Dynamo version
        dynamo_v=$1
        shift
        ;;

      "-l"|"--locate" )        # locate compilation
        locate="true"
        exit
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
        echo " -f  <.f90>                       code to compile"
        echo " -v | --version  <option>         Dynamo version to use"
        echo "                                    std          standard"
        echo "                                    gauss        with gaussian"
        echo " -l | --locate                    locate compilation (use 'panadero.f90')"
        echo " -h | --help                      print this help and exit"
        echo
        exit ;;
      *)
        echo "ERROR: Wrong option '$arg'. Use -h for help."
        exit ;;
    esac
  done

  ## Default variables if not input
  locate=${locate:=$locate_def}

  ## Check for mandatory inputs
  if [ ! -n "$dynamo_v" ]; then echo "ERROR: No Dynamo version specified"; exit; fi
  if [ ! -n "$code_file" ]; then echo "ERROR: No code file (.f90)"; exit; fi

  cd $workdir

  case $dynamo_v in
    "std" )
      dydir=$dynamo_std
      if [ "$locate" == "true" ]; then
        # compile 'panadero.f90'
        $FC -DF95 -I${dydir}/modules -w $Olevel -frecord-marker=4 -finit-local-zero \
          -J${mervinmonroe}/${templates_subfolder}/compile/ \
          -c ${mervinmonroe}/${templates_subfolder}/compile/panadero.f90
        # compile program
        $FC -DF95 -I${dydir}/modules -w $Olevel -frecord-marker=4 -finit-local-zero \
          -I${mervinmonroe}/${templates_subfolder}/compile/ \
          ${mervinmonroe}/${templates_subfolder}/compile/panadero.o \
          $code_file ${dydir}/DYNAMO.a
      else
        $FC -DF95 -I${dydir}/modules -J${dydir}/modules -w $Olevel -frecord-marker=4 -finit-local-zero \
          $code_file ${dydir}/DYNAMO.a
      fi
      ;;
    "gauss" )
      dydir=$dynamo_gauss
      $FC -DF95 -I${dydir}/src -I. -J. -w $Olevel -frecord-marker=4 -finit-local-zero \
        $code_file ${dydir}/src/dynamo.a
      ;;
    *)
      echo "ERROR: Wrong Dynamo version specified ('$dynamo_v')"
      exit ;;
  esac
