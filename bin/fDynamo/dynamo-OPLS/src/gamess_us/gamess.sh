GWD=`pwd`
SCR=`pwd`
DDI=$GWD/ddikick.x
EXE=$GWD/gamess.x
JOB=__tmp__

export ERICFMT=$GWD/ericfmt.dat
export MCPPATH=$GWD/mcpdata
export  EXTBAS=/dev/null
export  NUCBAS=/dev/null

export  MAKEFP=$SCR/$JOB.efp
export   GAMMA=$SCR/$JOB.gamma
export TRAJECT=$SCR/$JOB.trj
export RESTART=$SCR/$JOB.rst

export IRCDATA=$SCR/$JOB.irc
export   INPUT=$SCR/input
export  OUTPUT=$SCR/$JOB.out
export   PUNCH=$SCR/$JOB.dat
export  AOINTS=$SCR/$JOB.F08
export  MOINTS=$SCR/$JOB.F09
export DICTNRY=$SCR/guess
export DRTFILE=$SCR/$JOB.F11
export CIVECTR=$SCR/$JOB.F12
export CASINTS=$SCR/$JOB.F13
export  CIINTS=$SCR/$JOB.F14
export  WORK15=$SCR/$JOB.F15
export  WORK16=$SCR/$JOB.F16
export CSFSAVE=$SCR/$JOB.F17
export FOCKDER=$SCR/$JOB.F18
export  WORK19=$SCR/$JOB.F19
export  DASORT=$SCR/$JOB.F20
export DFTINTS=$SCR/$JOB.F21
export DFTGRID=$SCR/$JOB.F22
export  JKFILE=$SCR/$JOB.F23
export  ORDINT=$SCR/$JOB.F24
export  EFPIND=$SCR/$JOB.F25
export PCMDATA=$SCR/$JOB.F26                                     
export PCMINTS=$SCR/$JOB.F27
export   MLTPL=$SCR/$JOB.F28
export  MLTPLT=$SCR/$JOB.F29
export  DAFL30=$SCR/$JOB.F30
export  SOINTX=$SCR/$JOB.F31
export  SOINTY=$SCR/$JOB.F32
export  SOINTZ=$SCR/$JOB.F33
export  SORESC=$SCR/$JOB.F34
export   SIMEN=$SCR/$JOB.simen
export  SIMCOR=$SCR/$JOB.simcor
export GCILIST=$SCR/$JOB.F37
export HESSIAN=$SCR/$JOB.F38
export QMMMTEI=$SCR/$JOB.F39
export SOCCDAT=$SCR/$JOB.F40
export  AABB41=$SCR/$JOB.F41
export  BBAA42=$SCR/$JOB.F42
export  BBBB43=$SCR/$JOB.F43
export  MCQD50=$SCR/$JOB.F50
export  MCQD51=$SCR/$JOB.F51
export  MCQD52=$SCR/$JOB.F52
export  MCQD53=$SCR/$JOB.F53
export  MCQD54=$SCR/$JOB.F54
export  MCQD55=$SCR/$JOB.F55
export  MCQD56=$SCR/$JOB.F56
export  MCQD57=$SCR/$JOB.F57
export  MCQD58=$SCR/$JOB.F58
export  MCQD59=$SCR/$JOB.F59
export  MCQD60=$SCR/$JOB.F60
export  MCQD61=$SCR/$JOB.F61
export  MCQD62=$SCR/$JOB.F62
export  MCQD63=$SCR/$JOB.F63
export  MCQD64=$SCR/$JOB.F64
export NMRINT5=$SCR/$JOB.F65
export NMRINT6=$SCR/$JOB.F66
export DCPHFH2=$SCR/$JOB.F67
export DCPHF21=$SCR/$JOB.F68
export ELNUINT=$SCR/$JOB.F67
export NUNUINT=$SCR/$JOB.F68
export   GVVPT=$SCR/$JOB.F69
export  NUMOIN=$SCR/$JOB.F69
export NUMOCAS=$SCR/$JOB.F70
export  NUELMO=$SCR/$JOB.F71
export NUELCAS=$SCR/$JOB.F72

export MKL_SERIAL=YES
export MKL_NUM_THREADS=1

ulimit -c 0
rm -f $JOB.*
export DDI_RSH=ssh
export DDI_VER=new
export NCPUS=1
export NCPUS=2
export NNODES=1
export HOSTLIST="`hostname`:cpus=$NCPUS"

$DDI $EXE $JOB -ddi $NNODES $NCPUS $HOSTLIST -scr $SCR < /dev/null >& /dev/null

# -----------------------------------------------------------------------
# Parallel hints... (SHM related)
#
#check with $contrl/exetyp=check + $system/parall=.true. the required size
#of the calculation. If you have a total of ~16Gb of RAM and 8 cores:
#
#	$system/mwords = 8
#	$system/memddi = 1920
#
#sets a total of (1_word = 8_bytes):
#
#	8 * 8 [mwords] + 8 * 1920 [memddi] / 8 [ncpus] ~ 1984 Mb per core
#
#check this amount of memmory against shm kernel configuration:
#
#	sysctl -a | grep shm
#	getconf PAGE_SIZE
#
#and set (using a maximum of 15Gb for SHM and with a PAGE_SIZE of 4Kb):
#
#	sysctl -w kernel.shmmni=4096
#	sysctl -w kernel.shmmax=16106127360
#	sysctl -w kernel.shmall=1006632960
#
#value for shmall taken from /usr/include/linux/shm.h:
#
#	SHMALL = ( SHMMAX / PAGE_SIZE ) * ( SHMMNI / 16 )
# -----------------------------------------------------------------------
