..............................................................................
.
. NCKIIO INCLUSION
. NIN OWNER FILE I/O ROUTINES
.
.
.  FILE NAME : CHKINFO
. REC LENGTH: 45 FIXED
..............................................................................
. ENTRY POINT : NCKISEQ
. REQUIRED    :
. RETURNED    : CHECK INFO RECORD
. DESCRIPTION : SEQUENTIAL CHECK INFO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLGS
.
NCKISEQ  BRANCH    NCKIFLAG TO NCKI4
           CALL      NCKIOPEN
NCKI4    FILEPI    1;NCKIFLE1
         READ      NCKIFLE1,SEQ;NCKIVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCKIWRT
. REQUIRED    : 'NCKIFLD'
. RETURNED    :
. DESCRIPTION : EXACT AIM KEY
.
NCKIWRT  BRANCH    NCKIFLG1 TO NCKI5
         CALL      NCKIOPEN
NCKI5    FILEPI    1;NCKIFLE1
         WRITE     NCKIFLE1;NCKIVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCKIDEL
. REQUIRED    : 'Previous Valid Aim Read'
. RETURNED    :
. DESCRIPTION : EXACT AIM KEY TXT DELETE
.
NCKIDEL  BRANCH    NCKIFLG1 TO NCKI7
         CALL      NCKIOPEN
NCKI7    FILEPI    1;NCKIFLE1
.         DELETE    NCKIFLE1,NCKIFLD,NCKIFLD2,NCKIFLD3
         DELETE    NCKIFLE1
         RETURN
..............................................................................
.
. ENTRY POINT : NCKIAIM
. REQUIRED    : NCKIFLD,&/OR,NCKIFLD2,&/ORD,NCKIFLD3
. RETURNED    : CHECK INFO RECORD
. DESCRIPTION : AIM CHECK INFO FILE READ
.               APPLICA TION'S RESPONSIBILITY TO TEST FLGS
.
NCKIAIM  BRANCH    NCKIFLG1 TO NCKI8
         CALL      NCKIOPN1
NCKI8    FILEPI    1;NCKIFLE1
         READ      NCKIFLE1,NCKIFLD,NCKIFLD2,NCKIFLD3;NCKIVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCKIKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : CHECK INFO RECORD
. DESCRIPTION : AIM KEY GENEREIC CHECK INFO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCKIKG   BRANCH    NCKIFLG1 TO NCKI10
         CALL      NCKIOPN1
NCKI10   FILEPI    1;NCKIFLE1
         READKG    NCKIFLE1;NCKIVARS
         RETURN
..............................................................................
.
         IFZ      PC
. ENTRY POINT : NCKIKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : CHECK INFO RECORD
. DESCRIPTION : KEY GENERIC PRIOR CHECK INFO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCKIKGP  BRANCH    NCKIFLG1 TO NCKI11
         CALL      NCKIOPN1
NCKI11   FILEPI    1;NCKIFLE1
         READKGP   NCKIFLE1;NCKIVARS
         RETURN
         XIF
...............................................................................
. 
. ENTRY POINT : NCKIOPEN
. REQUIRED    : 'NCKIFLG1' 'NCKIPATH'
. RETURNED    : 'NCKIFLG1' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN CHECK INFO FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCKIOPEN BRANCH    NCKIPATH OF NCKIOPN1,NCKIOPN2
NCKIOPN1 TRAP      NCKIGONE IF IO
         OPEN      NCKIFLE1,NCKINAME
         TRAPCLR   IO
         MOVE      C1 TO NCKIFLG1
         RETURN
..............................................................................
NCKIOPN2 TRAP      NCKIGONE IF IO
         OPEN      NCKIFLE1,NCKINAME
         TRAPCLR   IO
         MOVE      C1 TO NCKIFLG1
         RETURN
.
..............................................................................
NCKIGONE MOVE      NCKINAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
