what to do next?
problems searched          = 44
current minumum time/base  =  9.00
corresponding plan,problem =
  ((#<PDDL-PLAN {1005A49D83}>
    #<PDDL-PROBLEM
      CELL-ASSEMBLY-MODEL2A-1-STEADY-STATE-0-2-5-7-15-17-19>))
Current time limit         = 35
Current memory limit       = 200000000
   [Condition of type SIMPLE-ERROR]

LOOP-PLANNER-TEST> (rb-minimum parallelized-loop-plan-results)
((#<PDDL-PLAN {1005A49D83}>
  #<PDDL-PROBLEM CELL-ASSEMBLY-MODEL2A-1-STEADY-STATE-0-2-5-7-15-17-19>))
9

LOOP-PLANNER-TEST> (print-timed-action-graphically (reschedule *** :minimum-slack))

0|(INIT-ACTION)
0||(AC* SLIDE-BASE-OUT (BASE19-3567 TABLE-OUT CARRY-OUT))
0||(AC* SLIDE-BASE-IN (BASE0-3561 CARRY-IN TABLE-IN))
0|--|(AC* MOVE-ARM-HOLDING (ARM1 TRAY-A GASKET-MACHINE BASE2-3562))
0   ||(AC* SET-BASE (BASE2-3562 ARM1 GASKET-MACHINE))
0    |--|(AC* ASSEMBLE-WITH-MACHINE (BASE2-3562 GASKET-MACHINE INSERT-GASKET NOTHING-DONE))
0    |----|(AC* MOVE-ARM (ARM1 GASKET-MACHINE INSPECTION-MACHINE))
0         ||(AC* EJECT-BASE (BASE17-3566 ARM1 INSPECTION-MACHINE))
0|--|(AC* MOVE-ARM (ARM2 OILING-MACHINE SCREW-MACHINE-A))
0   ||(AC* EJECT-BASE (BASE7-3564 ARM2 SCREW-MACHINE-A))
0    |--|(AC* MOVE-ARM-HOLDING (ARM2 SCREW-MACHINE-A OILING-MACHINE BASE7-3564))
0       ||(AC* SET-BASE (BASE7-3564 ARM2 OILING-MACHINE))
0        |-|(AC* ASSEMBLE-WITH-MACHINE (BASE7-3564 OILING-MACHINE OIL-CYLINDER SCREW-A))
0        |--|(AC* MOVE-ARM (ARM2 OILING-MACHINE SCREW-MACHINE-A))
0          |-|(AC* MOVE-ARM-HOLDING (ARM1 INSPECTION-MACHINE TABLE-OUT BASE17-3566))
0            ||(AC* SET-BASE (BASE17-3566 ARM1 TABLE-OUT))
0             |-|(AC* MOVE-ARM (ARM1 TABLE-OUT INSPECTION-MACHINE))
0               |-|(AC* MOVE-ARM (ARM1 INSPECTION-MACHINE TABLE2))
0                 ||(AC* EJECT-BASE (BASE15-3565 ARM1 TABLE2))
0                  |-|(AC* MOVE-ARM-HOLDING (ARM1 TABLE2 INSPECTION-MACHINE BASE15-3565))
0                    ||(AC* SET-BASE (BASE15-3565 ARM1 INSPECTION-MACHINE))
0                     |----|(AC* MOVE-ARM (ARM1 INSPECTION-MACHINE GASKET-MACHINE))
0                     |--|(AC* ASSEMBLE-WITH-MACHINE (BASE15-3565 INSPECTION-MACHINE INSPECT-BASE SCREW-C))
0           |-|(AC* MOVE-ARM (ARM2 SCREW-MACHINE-A TABLE1))
0             ||(AC* EJECT-BASE (BASE5-3563 ARM2 TABLE1))
0              |-|(AC* MOVE-ARM-HOLDING (ARM2 TABLE1 SCREW-MACHINE-A BASE5-3563))
0                ||(AC* SET-BASE (BASE5-3563 ARM2 SCREW-MACHINE-A))
0                 |-|(AC* ASSEMBLE-WITH-MACHINE (BASE5-3563 SCREW-MACHINE-A SCREW-A ATTATCH-A))
0                 |-|(AC* MOVE-ARM (ARM2 SCREW-MACHINE-A TRAY-B))
0                          ||(AC* EJECT-BASE (BASE2-3562 ARM1 GASKET-MACHINE))
0                           |-|(AC* MOVE-ARM-HOLDING (ARM1 GASKET-MACHINE TABLE1 BASE2-3562))
0                             ||(AC* SET-BASE (BASE2-3562 ARM1 TABLE1))
0                              |---|(AC* MOVE-ARM (ARM1 TABLE1 TRAY-A))
0                                  ||(AC* PICKUP-COMPONENT (PART-A ARM1 TRAY-A))
0                                   |---|(AC* MOVE-ARM-HOLDING (ARM1 TRAY-A TABLE1 PART-A))
0                                       |--|(AC* ASSEMBLE-WITH-ARM (PART-A ATTATCH-A INSERT-GASKET BASE2-3562 ARM1 TABLE1))
0                                          |--|(AC* MOVE-ARM (ARM1 TABLE1 TABLE-IN))
0                                             ||(AC* EJECT-BASE (BASE0-3561 ARM1 TABLE-IN))
0                   |-|(AC* MOVE-ARM (ARM2 TRAY-B OILING-MACHINE))
0                     ||(AC* EJECT-BASE (BASE7-3564 ARM2 OILING-MACHINE))
0                      |-|(AC* MOVE-ARM-HOLDING (ARM2 OILING-MACHINE TRAY-B BASE7-3564))
0                        |----|(AC* MOVE-ARM-HOLDING (ARM2 TRAY-B TABLE2 BASE7-3564))
0                             ||(AC* SET-BASE (BASE7-3564 ARM2 TABLE2))
0                              |----|(AC* MOVE-ARM (ARM2 TABLE2 TRAY-B))
0                                   ||(AC* PICKUP-COMPONENT (PART-B ARM2 TRAY-B))
0                                    |----|(AC* MOVE-ARM-HOLDING (ARM2 TRAY-B TABLE2 PART-B))
0                                         |--|(AC* ASSEMBLE-WITH-ARM (PART-B ATTATCH-B OIL-CYLINDER BASE7-3564 ARM2 TABLE2))
0                                            |--|(AC* MOVE-ARM (ARM2 TABLE2 TRAY-C))
0                                               ||(AC* PICKUP-COMPONENT (PART-C ARM2 TRAY-C))
0                                                |--|(AC* MOVE-ARM-HOLDING (ARM2 TRAY-C TABLE2 PART-C))
0                                                   |-|(AC* ASSEMBLE-WITH-ARM (PART-C ATTATCH-C ATTATCH-B BASE7-3564 ARM2 TABLE2))
0                                                     ||(AC* EJECT-BASE (BASE7-3564 ARM2 TABLE2))
0                                                      |-|(AC* MOVE-ARM-HOLDING (ARM2 TABLE2 SCREW-MACHINE-C BASE7-3564))
0                                                        ||(AC* SET-BASE (BASE7-3564 ARM2 SCREW-MACHINE-C))
0                                                         |-|(AC* ASSEMBLE-WITH-MACHINE (BASE7-3564 SCREW-MACHINE-C SCREW-C ATTATCH-C))
0                                                           ||(AC* EJECT-BASE (BASE7-3564 ARM2 SCREW-MACHINE-C))
0                                                            |-|(AC* MOVE-ARM-HOLDING (ARM2 SCREW-MACHINE-C TABLE2 BASE7-3564))
0                                                              ||(AC* SET-BASE (BASE7-3564 ARM2 TABLE2))
NIL
LOOP-PLANNER-TEST> 


(:INIT
 ;; base 19
 (BASE-PRESENT TABLE-OUT)               ; * から導出
 (FINISHED NOTHING-DONE BASE19-3567)
 (FINISHED INSERT-GASKET BASE19-3567)
 (FINISHED ATTATCH-A BASE19-3567)
 (FINISHED SCREW-A BASE19-3567)
 (FINISHED OIL-CYLINDER BASE19-3567)
 (FINISHED ATTATCH-B BASE19-3567)
 (FINISHED ATTATCH-C BASE19-3567)
 (FINISHED SCREW-C BASE19-3567)
 (FINISHED INSPECT-BASE BASE19-3567)
 (AT BASE19-3567 TABLE-OUT)             ; *
 ;; base 17
 (BASE-PRESENT INSPECTION-MACHINE)      ; * から導出
 (FINISHED NOTHING-DONE BASE17-3566)
 (FINISHED INSERT-GASKET BASE17-3566)
 (FINISHED ATTATCH-A BASE17-3566)
 (FINISHED SCREW-A BASE17-3566)
 (FINISHED OIL-CYLINDER BASE17-3566)
 (FINISHED ATTATCH-B BASE17-3566)
 (FINISHED ATTATCH-C BASE17-3566)
 (FINISHED SCREW-C BASE17-3566)
 (AT BASE17-3566 INSPECTION-MACHINE)    ; *
 (FINISHED INSPECT-BASE BASE17-3566)
 ;; base 15
 (BASE-PRESENT TABLE2)                  ;
 (FINISHED NOTHING-DONE BASE15-3565)
 (FINISHED INSERT-GASKET BASE15-3565)
 (FINISHED ATTATCH-A BASE15-3565)
 (FINISHED SCREW-A BASE15-3565)
 (FINISHED OIL-CYLINDER BASE15-3565)
 (FINISHED ATTATCH-B BASE15-3565)
 (FINISHED ATTATCH-C BASE15-3565)
 (FINISHED SCREW-C BASE15-3565)
 (AT BASE15-3565 TABLE2)                ;
 ;; base 7
 (BASE-PRESENT SCREW-MACHINE-A)         ;
 (FINISHED NOTHING-DONE BASE7-3564)
 (FINISHED INSERT-GASKET BASE7-3564)
 (FINISHED ATTATCH-A BASE7-3564)
 (AT BASE7-3564 SCREW-MACHINE-A)        ;
 (FINISHED SCREW-A BASE7-3564)
 ;; base 5
 (BASE-PRESENT TABLE1)                  ;
 (FINISHED NOTHING-DONE BASE5-3563)
 (FINISHED INSERT-GASKET BASE5-3563)
 (AT BASE5-3563 TABLE1)                 ;
 (FINISHED ATTATCH-A BASE5-3563)
 ;; base 2
 (FINISHED NOTHING-DONE BASE2-3562)
 (HOLD ARM1 BASE2-3562)                 ; hold は release 型の述語なので関係ない
 ;; base 0
 (FINISHED NOTHING-DONE BASE0-3561)
 (AT BASE0-3561 CARRY-IN)               ; (at ?b - base ?conveyor -
                                        ; conveyor) 型の at はmutex を
                                        ; 持つ述語ではない。
                                        ;
                                        ; mutex があるのは (at ?b -
                                        ; base ?table - table) のみ
 
 (ARM-PRESENT OILING-MACHINE)
 (AT ARM2 OILING-MACHINE)
 (FREE ARM2)
 (ARM-PRESENT TRAY-A)
 (AT ARM1 TRAY-A) ... ) ;; 省略
(:GOAL
 (AND ;;base19
  (FINISHED INSPECT-BASE BASE19-3567)
  (AT BASE19-3567 CARRY-OUT)
  ;;base17
  (FINISHED NOTHING-DONE BASE17-3566)
  (FINISHED INSERT-GASKET BASE17-3566)
  (FINISHED ATTATCH-A BASE17-3566)
  (FINISHED SCREW-A BASE17-3566)
  (FINISHED OIL-CYLINDER BASE17-3566)
  (FINISHED ATTATCH-B BASE17-3566)
  (FINISHED ATTATCH-C BASE17-3566)
  (FINISHED SCREW-C BASE17-3566)
  (FINISHED INSPECT-BASE BASE17-3566)
  (AT BASE17-3566 TABLE-OUT)
  ;;base15
  (FINISHED NOTHING-DONE BASE15-3565)
  (FINISHED INSERT-GASKET BASE15-3565)
  (FINISHED ATTATCH-A BASE15-3565)
  (FINISHED SCREW-A BASE15-3565)
  (FINISHED OIL-CYLINDER BASE15-3565)
  (FINISHED ATTATCH-B BASE15-3565)
  (FINISHED ATTATCH-C BASE15-3565)
  (FINISHED SCREW-C BASE15-3565)
  (AT BASE15-3565 INSPECTION-MACHINE)
  (FINISHED INSPECT-BASE BASE15-3565)
  ;;base7
  (FINISHED NOTHING-DONE BASE7-3564)
  (FINISHED INSERT-GASKET BASE7-3564)
  (FINISHED ATTATCH-A BASE7-3564)
  (FINISHED SCREW-A BASE7-3564)
  (FINISHED OIL-CYLINDER BASE7-3564)
  (FINISHED ATTATCH-B BASE7-3564)
  (FINISHED ATTATCH-C BASE7-3564)
  (FINISHED SCREW-C BASE7-3564)
  (AT BASE7-3564 TABLE2)
  ;;base5
  (FINISHED NOTHING-DONE BASE5-3563)
  (FINISHED INSERT-GASKET BASE5-3563)
  (FINISHED ATTATCH-A BASE5-3563)
  (AT BASE5-3563 SCREW-MACHINE-A)
  (FINISHED SCREW-A BASE5-3563)
  ;;base2
  (FINISHED NOTHING-DONE BASE2-3562)
  (FINISHED INSERT-GASKET BASE2-3562)
  (AT BASE2-3562 TABLE1)
  (FINISHED ATTATCH-A BASE2-3562)
  ;;base0
  (FINISHED NOTHING-DONE BASE0-3561)
  (HOLD ARM1 BASE0-3561)))