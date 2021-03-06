#!/bin/bash

#################################################
###               MERVIN MONROE               ###
#################################################
#                    compile                    #
#################################################

#                by Sergio Boneta


## USAGE:   mervinmonroe compile [options]


##  DEFAULT VARIABLES  ################################################

  locate_def="false"
  dynamo_v_def="std"
  FC="gfortran"
  Olevel="-O3"

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

      -f )                        # code file
        code_file=$1
        shift
        ;;

      -v|--version )              # Dynamo version
        dynamo_v=$1
        shift
        ;;

      -l|--locate )               # locate compilation
        locate=true
        ;;

      -h|--help )                 # print help and exit
        echo "---------------  MERVIN MONROE  ---------------"
        echo "     A lazy interface for fDynamo software     "
        echo
        echo "Version $mervinmonroe_version"
        echo
        echo
        echo "··············      compile     ···············"
        echo
        echo "USAGE:   mervinmonroe compile [options]"
        echo
        echo "OPTIONS:                                     "
        echo " -f  <.f90>                        code to compile"
        echo
        echo " -v | --version  <option>          Dynamo version to use (default: $dynamo_v_def)"
        echo "                                     std          standard"
        echo "                                     gauss        with gaussian"
        echo " -l | --locate                     locate compilation (use 'panadero.f90')"
        echo " -h | --help                       print this help and exit"
        echo
        exit ;;
      *)
        echo "ERROR: Wrong option '$arg'. Use -h for help."
        exit ;;
    esac
  done

  ## Default variables if not input
  locate=${locate:=$locate_def}
  dynamo_v=${dynamo_v:=$dynamo_v_def}

  ## Check for mandatory inputs
  # if [ ! -n "$dynamo_v" ]; then echo "ERROR: No Dynamo version specified"; exit; fi
  if [ ! -n "$code_file" ]; then echo "ERROR: No code file (.f90)"; exit; fi

  case $dynamo_v in
    "std" )
      dydir=$dynamo_std
      if [ "$locate" == "true" ]; then
        # compile 'panadero.f90'
        $FC -DF95 -I${dydir}/modules -w $Olevel -frecord-marker=4 -finit-local-zero \
          -J${mervinmonroe}/${templates_subfolder}/compile/ \
          -c ${mervinmonroe}/${templates_subfolder}/compile/panadero.f90 \
          -o ${mervinmonroe}/${templates_subfolder}/compile/panadero.o
        # compile program
        $FC -DF95 -I${dydir}/modules -w $Olevel -frecord-marker=4 -finit-local-zero \
          -I${mervinmonroe}/${templates_subfolder}/compile/ \
          ${mervinmonroe}/${templates_subfolder}/compile/panadero.o \
          $code_file ${dydir}/DYNAMO.a
      else
        $FC -DF95 -I${dydir}/modules -w $Olevel -frecord-marker=4 -finit-local-zero \
          -I${mervinmonroe}/${templates_subfolder}/compile/ -J${dydir}/modules \
          $code_file ${dydir}/DYNAMO.a
      fi
      ;;
    "gauss" )
      dydir=$dynamo_gauss
      if [ "$locate" == "true" ]; then
        # compile 'panadero.f90'
        $FC -DF95 -I${dydir}/src -w $Olevel -frecord-marker=4 -finit-local-zero \
          -J${mervinmonroe}/${templates_subfolder}/compile/ \
          -c ${mervinmonroe}/${templates_subfolder}/compile/panadero_gauss.f90 \
          -o ${mervinmonroe}/${templates_subfolder}/compile/panadero_gauss.o
        # compile program
        $FC -DF95 -I${dydir}/src -w $Olevel -frecord-marker=4 -finit-local-zero \
          -I${mervinmonroe}/${templates_subfolder}/compile/ \
          ${mervinmonroe}/${templates_subfolder}/compile/panadero_gauss.o \
          $code_file ${dydir}/src/dynamo.a
      else
        $FC -DF95 -I${dydir}/src -w $Olevel -frecord-marker=4 -finit-local-zero \
          -I${mervinmonroe}/${templates_subfolder}/compile/ -J. \
          $code_file ${dydir}/src/dynamo.a
      fi
      ;;
    *)
      echo "ERROR: Wrong Dynamo version specified ('$dynamo_v')"
      exit ;;
  esac
