(in-package :pddl.loop-planner)
(cl-syntax:use-syntax :annot)

@export
(defparameter *unit-cost*
  (merge-pathnames "src/search/unitcost" *fd-dir*))
@export
(defparameter *state-size*
  (merge-pathnames "src/search/dispatch" *fd-dir*))

@export
(defun lama-2011-bound (bound)
  (substitute #\Space #\Newline
              (format nil "$(
echo $SASP >&2;
BOUND=~a;
UNIT_COST=$(~a $SASP);
if [[ $UNIT_COST == 'unit' ]]; then
echo \"--heuristic hlm,hff=lm_ff_syn(lm_rhw(
     reasonable_orders=true,lm_cost_type=2,cost_type=2))
 --search iterated([
     lazy_greedy([hff,hlm],preferred=[hff,hlm]),
     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=5),
     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=3),
     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=2),
     lazy_wastar([hff,hlm],preferred=[hff,hlm],w=1)],
     repeat_last=true,continue_on_fail=true,bound=$BOUND)\" ;
elif [[ $UNIT_COST == 'nonunit' ]]; then
echo \"  --heuristic hlm1,hff1=lm_ff_syn(lm_rhw(
            reasonable_orders=true,lm_cost_type=1,cost_type=1))
        --heuristic hlm2,hff2=lm_ff_syn(lm_rhw(
            reasonable_orders=true,lm_cost_type=2,cost_type=2))
        --search iterated([
            lazy_greedy([hff1,hlm1],preferred=[hff1,hlm1],
                        cost_type=1,reopen_closed=false),
            lazy_greedy([hff2,hlm2],preferred=[hff2,hlm2],
                        reopen_closed=false),
            lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=5),
            lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=3),
            lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=2),
            lazy_wastar([hff2,hlm2],preferred=[hff2,hlm2],w=1)],
            repeat_last=true,continue_on_fail=true,bound=$BOUND) \" ;
fi)"
                      bound *unit-cost*)))

