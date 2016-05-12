..............................................................................
.
. NCOUIO INCLUSION
. NIN COUNTS FILE I/O ROUTINES
.
. FILE NAME : 
. REC LENGTH: 62 FIXED
. INDEX KEY : 1-14 ()
.
..............................................................................
.
. ENTRY POINT : NCOUKEY
. REQUIRED    : 'NCOUFLD'
. RETURNED    : COUNTS RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
*******************************************************************************
NCOUKEY  BRANCH     NCOUFLAG OF NCOU1
	 CALL       NCOUOPEN
NCOU1    BRANCH     NCOUPATH OF CLISTRD,CSEXRD,CSCFRD,CZIPRD,CDTTRD
*******************************************************************************
.CLISTRD  -  READ   LIST NAME.
CLISTRD  TRAP      CTFORMAT IF FORMAT
         MOVE      "N" TO OVER
         READ      NCOUFILE,NCOUFLD;CTKEY,CTLISTN
         RETURN
*******************************************************************************
.CSEXRD  -  READ   SEX NCOUFILE.
CSEXRD   TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READ      NCOUFILE,NCOUFLD;CTKEY,CTMALE,CTFEMALE,CTDUAL,CTCOMP,CTUNKN:
                   CTSEED
         RETURN
*******************************************************************************
.CSCFRD  -  READ   SCF NCOUFILE.
CSCFRD   TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READ      NCOUFILE,NCOUFLD;CTKEY,CTUNKN,CTFEMALE,CTMALE,CTDUAL,CTCOMP:
                   CTTOTAL
         RETURN
*******************************************************************************
.CZIPRD  -  READ   ZIP NCOUFILE.
CZIPRD   TRAP      CTFORMAT IF FORMAT
         MOVE       "N" TO OVER
         READ      NCOUFILE,NCOUFLD;CTKEY,CTTOTAL
         GOTO      NOCTREC IF OVER
         RETURN
*******************************************************************************
.CDTTRD - READ UPDATE RECORD
CDTTRD   TRAP      CTFORMAT IF FORMAT
         MOVE      "N" TO OVER
         READ      NCOUFILE,NCOUFLD;CTKEY,CTDATE
         GOTO      NOCTREC IF OVER
         RETURN
..............................................................................
.
. ENTRY POINT : NCOUKS
. REQUIRED    : 
. RETURNED    : COUNTS RECORD
. DESCRIPTION : KEY SEQ. ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
*******************************************************************************
NCOUKS  BRANCH     NCOUFLAG OF NCOU3
	 CALL       NCOUOPEN
NCOU3   BRANCH     NCOUPATH OF CSCFRDKS,CZIPRDKS
.CSCFRDKS-  READ   SCF NCOUFILE.
CSCFRDKS TRAP      ctFORMT2 IF FORMAT
         READKS    NCOUFILE;CTKEY,CTUNKN,CTFEMALE,CTMALE,CTDUAL,CTCOMP:
                   CTTOTAL
         MOVE      "SCF" TO SCF
         RETURN
*******************************************************************************
.CZIPRDKS  -  READ   ZIP NCOUFILE.
CZIPRDKS TRAP      CTFORMAT IF FORMAT
         READKS    NCOUFILE;CTKEY,CTTOTAL
         RETURN
*******************************************************************************
..............................................................................
.
. ENTRY POINT : NCOUKP
. REQUIRED    : 
. RETURNED    : COUNTS RECORD
. DESCRIPTION : KEY PRIOR SEQ. ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
*******************************************************************************
NCOUKP  BRANCH     NCOUFLAG OF NCOU8
	 CALL       NCOUOPEN
NCOU8
.CSCFRDKP-  READ   SCF NCOUFILE.
CSCFRDKP TRAP      KPFORMAT IF FORMAT
         TRAP      KPFORMAT IF RANGE
         READKP    NCOUFILE;CTKEY,CTUNKN,CTFEMALE,CTMALE,CTDUAL,CTCOMP:
                   CTTOTAL
         RETURN
...............................................................................
KPFORMAT MOVE      YES TO OVER
         TRAPCLR   FORMAT
         TRAPCLR   RANGE
         NORETURN
         RETURN
NOCTREC  
         MOVE      YES TO OVER
         RETURN
CTFORMAT
         TRAPCLR   FORMAT
CTFORMT1 KEYIN    *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL!!!!",*B:
                   *B,*B,*B,*B,STR1,*P1:24,*EL;
         CMATCH    "Q" TO STR1
         RETURN    IF EQUAL
         CMATCH    "X" TO STR1
         STOP      IF EQUAL
         GOTO      CTFORMT1
*******************************************************************************
ctFORMT2 TRAPCLR   FORMAT
         NORETURN
         CALL      NOCTREC
         RETURN
..............................................................................
.
. ENTRY POINT : NCOUOPEN
. REQUIRED    : 'NCOUFLAG'
. RETURNED    : 'NCOUFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN COUNTS FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCOUOPEN TRAP      NCOUGONE giving error IF IO
         OPEN      NCOUFILE,NCOUNAME
         TRAPCLR   IO
         MOVE      C1 TO NCOUFLAG
         RETURN
.
NCOUGONE MOVE      NCOUNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

