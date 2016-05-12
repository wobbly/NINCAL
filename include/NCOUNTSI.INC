*******************************************************************************
.CLISTRD  -  READ   LIST NAME.
CLISTRD  TRAP      CTFORMAT IF FORMAT
         MOVE      "N" TO OVER
         READ      COUNTS,CTKEY;CTKEY,CTLISTN
         GOTO      NOCTREC IF OVER
         RETURN
*******************************************************************************
.CSEXRD  -  READ   SEX COUNTS.
CSEXRD   TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READ      COUNTS,CTKEY;CTKEY,CTMALE,CTFEMALE,CTDUAL,CTCOMP,CTUNKN:
                   CTSEED
         GOTO      NOCTREC IF OVER
         RETURN
*******************************************************************************
.CSCFRD  -  READ   SCF COUNTS.
CSCFRD   TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READ      COUNTS,CTKEY;CTKEY,CTUNKN,CTFEMALE,CTMALE,CTDUAL,CTCOMP:
                   CTTOTAL
         GOTO      NOCTREC IF OVER
         RETURN
*******************************************************************************
.CSCFRDKS-  READ   SCF COUNTS.
CSCFRDKS TRAP      FORMAT IF FORMAT
         MOVE       "N" TO OVER
         READKS    COUNTS;CTKEY,CTUNKN,CTFEMALE,CTMALE,CTDUAL,CTCOMP:
                   CTTOTAL
         GOTO      NOCTREC IF OVER
.         SCAN      "SCF" IN CTKEY
.         RETURN    IF EQUAL
         MOVE      "SCF" TO SCF
.         GOTO      NOCTREC
         RETURN
*******************************************************************************
.CSCFRDKP-  READ   SCF COUNTS.
CSCFRDKP TRAP      KPFORMAT IF FORMAT
         TRAP      KPFORMAT IF RANGE
         MOVE       "N" TO OVER
         READKP    COUNTS;CTKEY,CTUNKN,CTFEMALE,CTMALE,CTDUAL,CTCOMP:
                   CTTOTAL
         GOTO      NOCTREC IF OVER
         RETURN
...............................................................................
KPFORMAT MOVE      "Y" TO OVER
         TRAPCLR   FORMAT
         TRAPCLR   RANGE
         NORETURN
         RETURN
*******************************************************************************
.CZIPRD  -  READ   ZIP COUNTS.
CZIPRD   TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READ      COUNTS,CTKEY;CTKEY,CTTOTAL
         GOTO      NOCTREC IF OVER
         RETURN
*******************************************************************************
.CZIPRDKS  -  READ   ZIP COUNTS.
CZIPRDKS TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READKS    COUNTS;CTKEY,CTTOTAL
         GOTO      NOCTREC IF OVER
         RETURN
*******************************************************************************
.CDTTRD - READ UPDATE RECORD
CDTTRD   TRAP      CTFORMAT IF FORMAT
         MOVE      "N" TO OVER
         READ      COUNTS,CTKEY;CTKEY,CTDATE
         GOTO      NOCTREC IF OVER
         RETURN
NOCTREC  
         MOVE      "Y" TO OVER
         RETURN
CTFORMAT
         TRAPCLR   FORMAT
CTFORMT1 KEYIN    *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL!!!!",*B:
                   *B,*B,*B,*B,ANS,*P1:24,*EL;
         CMATCH    "Q" TO ANS
         RETURN    IF EQUAL
         CMATCH    "X" TO ANS
         STOP      IF EQUAL
         GOTO      CTFORMT1
*******************************************************************************
FORMAT   TRAPCLR   FORMAT
         NORETURN
         CALL      NOCTREC
         RETURN
*******************************************************************************
.END OF INCLUDE
*******************************************************************************
