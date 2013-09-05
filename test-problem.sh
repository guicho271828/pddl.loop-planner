#! /bin/bash

TIME_LIMIT=1800
MEMORY_USAGE=1000000
OPTIONS="ipc seq-sat-lama-2011"
	
while getopts ":t:m:o:" opt
do
    case ${opt} in
	t) # echo limit execution time under 30 min (same as ICAPS)
	    TIME_LIMIT=${OPTARG:-$TIME_LIMIT} ;;
	
	m) # limit memory usage under 1 GB
	    MEMORY_USAGE=${OPTARG:-$MEMORY_USAGE} ;;

	o) # specifies the search option
	    OPTIONS=${OPTARG:-$OPTIONS} ;;
	
	* ) echo "unsupported option" ;;
    esac
done

shift $(($OPTIND - 1))

echo "ulimit -m $MEMORY_USAGE"
echo "ulimit -t $TIME_LIMIT"

ulimit -m $MEMORY_USAGE
ulimit -t $TIME_LIMIT

PDDL=$1

if [[ $PDDL =~ .*\.pddl ]]
then
    PROBLEM_NAME=${PDDL%.pddl}
    PROBLEM_NUM=${PDDL:0:3}
elif [[ $PDDL =~ pfile.* ]]
then
    # pfile1 etc...
    PROBLEM_NAME=$PDDL
    PROBLEM_NUM=${PDDL:4}
fi  

SAS=$PROBLEM_NAME.sas
SAS_PLUS=$PROBLEM_NAME.sasp
FD_DIR=~/repos/downward

# lm_ff_syn = LAMA/FF synergy

# '--heuristic "hlm=lmcut(lm_rhw(reasonable_orders=true,lm_cost_type=2,cost_type=2))"
#          --search "lazy_wastar([hlm],preferred=[hlm],w=2)"'

rm -f $PROBLEM_NAME.time
rm -f $PROBLEM_NAME.plan*
rm -f $SAS
rm -f $SAS_PLUS
rm -f elapsed.time
rm -f output
rm -f output.sas
rm -f sas_plan.*
rm -f plan_numbers_and_cost
rm -f downward.tmp.*

TRANSLATE=$FD_DIR/src/translate/translate.py
PREPROCESS=$FD_DIR/src/preprocess/preprocess # < OUTPUT.SAS
SEARCH_DIR=$FD_DIR/src/search
SEARCH="$SEARCH_DIR/downward $OPTIONS"

if [ -e $2 ]
then
    DOMAIN=$2
elif [ -e domain.pddl ]
then
    DOMAIN=domain.pddl
elif [ -e $PROBLEM_NUM-domain.pddl ]
then
    DOMAIN=$PROBLEM_NUM-domain.pddl
else
    DOMAIN=
fi


echo $'\x1b[34;1m'---- process $PPID started -----------------------------
echo PROBLEM_NAME:   $PROBLEM_NAME
echo DOMAIN:         $DOMAIN
echo SEARCH COMMAND: $SEARCH
echo --------------------------------------------------------$'\x1b[0m'


$TRANSLATE $DOMAIN $PDDL >& $PROBLEM_NAME.translate.log
echo Translation Finished
mv output.sas $SAS
for groups in $(ls *.groups)
do
    mv $groups $PROBLEM_NAME.$groups
done

$PREPROCESS < $SAS >& $PROBLEM_NAME.preprocess.log
echo Preprocessing Finished
mv output $SAS_PLUS

$SEARCH < $SAS_PLUS >& $PROBLEM_NAME.search.log
ERROR=$?

mv elapsed.time $PROBLEM_NAME.time
mv plan_numbers_and_cost $PROBLEM_NAME.cost

echo $'\x1b[34;1m'---- process $PPID finished ----------------------------
if [[ -e sas_plan.1 ]]
then
    echo Result:
    for plan in $(ls sas_plan.*)
    do
	mv $plan $PROBLEM_NAME.plan.${plan##*.}
	echo $PROBLEM_NAME.plan.${plan##*.}
    done
    cat $PROBLEM_NAME.cost
else
    echo "Search Failed: No path could be found in the current configuration."
fi
echo --------------------------------------------------------$'\x1b[0m'

exit $ERROR