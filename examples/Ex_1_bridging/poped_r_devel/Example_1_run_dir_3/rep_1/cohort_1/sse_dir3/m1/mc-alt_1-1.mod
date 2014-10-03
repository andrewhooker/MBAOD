$PROBLEM    PK model
$INPUT      ID TIME DV AMT WT
$DATA       /Users/ahooker/Documents/_PROJECTS/AOD/repos/MBAOD/examples/Example_1_poped_r/Example_1_run_dir_3/rep_1/cohort_1/sse_dir3/m1/mc-sim-1.dat
            IGNORE=@
$SUBROUTINE ADVAN1 TRANS2
$PK 

 CL  = THETA(1) * EXP(ETA(1))
 V   = THETA(2) * EXP(ETA(2))
 ;BASCL = THETA(3)
 ;EMAX  = THETA(4)
 ;E50   = THETA(5)
 ;HL    = THETA(6)

 ;CL    = BASCL+TVCL*((EMAX*WT**HL)/(E50**HL+WT**HL))
 ;V     = TVV *(WT/70)

 SC    = V
  
$ERROR 
 IPRED = F
 Y= IPRED*(1+EPS(1))+EPS(2)

;$SIM (1000) ONLYSIM NSUBPROBLEM=1
;$TABLE ID TIME DV AMT FILE=outA.tab NOAPPEND NOPRINT NOHEADER
$THETA  (0,1,100) ; TVCL
 (0,20,100) ; TVV
; (0, 1, 10)            ; BASCL
; (0, 2, 10)            ; EMAX
; (0, 25,  200)         ; E50
; (0, 5, 20)            ; HL
$OMEGA  0.05
 0.05
$SIGMA  0.015
 0.0015
$ESTIMATION METHOD=1 INTER MAXEVAL=9999 SIG=3 PRINT=5 NOABORT POSTHOC
$COVARIANCE UNCONDITIONAL

