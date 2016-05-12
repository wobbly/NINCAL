..............................................................................
.
. NPAYIO INCLUSION
. NIN PAY-TO FILE I/O ROUTINES
.
.
. FILE NAME : NINPAY
. REC LENGTH: 147 FIXED
. INDEX KEY : 2-6
. AIM KEY   : 7-56
.
. Patch:       Added logic to trap for IO errors using routine
.              found in comlogic.inc with variables set in
.              driver program
. Patch Author: Andrew harkins
. Patch Date:  July 9,1998
.
.
. Patch #100 - Created as test for Year 2000 conversion
.              Record length changed to 147 FIXED
.              This takes into consideration two new fields
.              added on 16may95 as well as new date format
. Patch Author:  Andrew Harkins
. Patch Date:  May 21,1998
..............................................................................
.
. ENTRY POINT : NPAYKEY
. REQUIRED    : 'NPAYFLD'
. RETURNED    : PAY-TO RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPAYKEY  BRANCH    NPAYFLAG TO NPAY1
         CALL      NPAYOPEN

NPAY1      trap      IOMssg giving Error if IO
           branch    NPAYLOCK to NPAY1L,NPAY1R,NPAY1N

.NPAY1L - file locks
NPAY1L
           FILEPI    1;NPAYFILE
         READ      NPAYFILE,NPAYFLD;PAYVARS
         trapclr   IO
         RETURN

.NPAY1R - record locks
NPAY1R
         READLK    NPAYFILE,NPAYFLD;PAYVARS
         trapclr   IO
         RETURN

.NPAY1N - no locks                  
NPAY1N
           READ      NPAYFILE,NPAYFLD;PAYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYTST
. REQUIRED    : NPAYFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NPAYTST  BRANCH    NPAYFLAG TO NPAY2
         CALL      NPAYOPEN
NPAY2    trap      IOMssg giving Error if IO
           FILEPI    1;NPAYFILE
         READ      NPAYFILE,NPAYFLD;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYKS
. REQUIRED    :
. RETURNED    : PAYER RECORD
. DESCRIPTION : KEY SEQUENTIAL PAY-TO FILE READ
.
NPAYKS   BRANCH    NPAYFLAG TO NPAY3
         CALL      NPAYOPEN
NPAY3    trap      IOMssg giving Error if IO
           branch    NPAYLOCK to NPAY3L,NPAY3R,NPAY3N

.NPAY3L - file locks
NPAY3L   FILEPI    1;NPAYFILE
         READKS    NPAYFILE;PAYVARS
         trapclr   IO
         RETURN

.NPAY3R - record locks
NPAY3R   
         READKSLK  NPAYFILE;PAYVARS
         trapclr   IO
         RETURN

.NPAY3N - no locks                  
NPAY3N   
         READKS    NPAYFILE;PAYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYSEQ
. REQUIRED    :
. RETURNED    : PAYER RECORD
. DESCRIPTION : SEQUENTIAL PAY-TO FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPAYSEQ  BRANCH    NPAYPATH OF NPAY4A,NPAYGONE,NPAY4C
NPAY4A   BRANCH    NPAYFLAG TO NPAY4B
         CALL      NPAYOPEN
NPAY4B   trap      IOMssg giving Error if IO
           branch    NPAYLOCK to NPAY4BL,NPAY4BR,NPAY4BN

.NPAY4BL - file locks
NPAY4BL  FILEPI    1;NPAYFILE
         READ      NPAYFILE,SEQ;PAYVARS
         trapclr   IO
         RETURN

.NPAY4BR - record locks
NPAY4BR   
         READLK    NPAYFILE,SEQ;PAYVARS
         trapclr   IO
         RETURN

.NPAY4BN - no locks                  
NPAY4BN   
         READ      NPAYFILE,SEQ;PAYVARS
         trapclr   IO
         RETURN
         
NPAY4C   BRANCH    NPAYFLG3 TO NPAY4D
         CALL      NPAYOPEN
         
NPAY4D   trap      IOMssg giving Error if IO
           branch    NPAYLOCK to NPAY4DL,NPAY4DR,NPAY4DN

.NPAY4DL - file locks
NPAY4DL  FILEPI    1;NPAYFLE3
         READ      NPAYFLE3,SEQ;PAYVARS
         trapclr   IO
         RETURN

.NPAY4DR - record locks
NPAY4DR   
         READLK    NPAYFLE3,SEQ;PAYVARS
         trapclr   IO
         RETURN

.NPAY4DN - no locks                  
NPAY4DN   
         READ      NPAYFLE3,SEQ;PAYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYWRT
. REQUIRED    : 'NPAYFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NPAYWRT  MOVE      C1 TO NPAYPATH
NPAYWRT1 COMPARE   NPAYPATH TO NPAYFLAG
         GOTO      NPAY5 IF EQUAL
         CALL      NPAYOPEN
NPAY5    trap      IOMssg giving Error if IO
.           FILEPI    1;NPAYFILE
.         WRITE     NPAYFILE,NPAYFLD;PAYVARS
           FILEPI    1;PAYFlist
         WRITE     PAYFList;PAYVARS
         trapclr   IO
.         CALL      NPAYWRT2
         RETURN
NPAYWRT2 BRANCH    NPAYFLG2 OF NPAY5B
         CALL      NPAYOPN2
NPAY5B   trap      IOMssg giving Error if IO
           FILEPI    1;NPAYFIL2
         INSERT    NPAYFIL2
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE PAY-TO FILE
.
NPAYUPD  BRANCH    NPAYFLAG TO NPAY6
         CALL      NPAYOPEN
NPAY6    trap      IOMssg giving Error if IO
           FILEPI    1;PAYFlist
         UPDATE    PAYFList;PAYVARS
.          FILEPI    1;NPAYFILE
.         UPDATE    NPAYFILE;PAYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYDEL
. REQUIRED    : 'NPAYFLD'or Npaytst Valid read
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NPAYDEL  BRANCH    NPAYFLAG TO NPAY7
         CALL      NPAYOPEN
NPAY7    trap      IOMssg giving Error if IO
.           FILEPI    1;NPAYFILE
.         DELETE    NPAYFILE,NPAYFLD
           FILEPI    1;PAYFList
         DELETE    PAYFList
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYAIM
. REQUIRED    : NPAYFLD2
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPAYAIM  BRANCH    NPAYFLG2 TO NPAY9
         CALL      NPAYOPN2
NPAY9    trap      IOMssg giving Error if IO
           BRANCH    NPAYLOCK TO NPAY9L,NPAY9R,NPAY9N

NPAY9L   FILEPI    1;NPAYFIL2
         READ      NPAYFIL2,NPAYFLD2;PAYVARS
         trapclr   IO
         RETURN
NPAY9R   
         READLK    NPAYFIL2,NPAYFLD2;PAYVARS
         trapclr   IO
         RETURN         
NPAY9N   
         READ      NPAYFIL2,NPAYFLD2;PAYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : AIM KEY GENEREIC MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPAYKG   BRANCH    NPAYFLG2 TO NPAY10
         CALL      NPAYOPN2
NPAY10   trap      IOMssg giving Error if IO
           BRANCH    NPAYLOCK TO NPAY10L,NPAY10R,NPAY10N
NPAY10L  FILEPI    1;NPAYFIL2
         READKG    NPAYFIL2;PAYVARS
         trapclr   IO
         RETURN
NPAY10R  
         READKGLK  NPAYFIL2;PAYVARS
         trapclr   IO
         RETURN         
NPAY10N  
         READKG    NPAYFIL2;PAYVARS
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NPAYKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : MAILER RECORD
. DESCRIPTION : KEY GENERIC PRIOR MAILER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NPAYKGP  BRANCH    NPAYFLG2 TO NPAY11
         CALL      NPAYOPN2
NPAY11   trap      IOMssg giving Error if IO
           BRANCH    NPAYLOCK TO NPAY11L,NPAY11R,NPAY11N
NPAY11L  FILEPI    1;NPAYFIL2
         READKGP   NPAYFIL2;PAYVARS
         trapclr   IO
         RETURN
NPAY11R  
         READKGPLK NPAYFIL2;PAYVARS
         trapclr   IO
         RETURN
NPAY11N  
         READKGP   NPAYFIL2;PAYVARS
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NPAYOPEN
. REQUIRED    : 'NPAYFLAG'
. RETURNED    : 'NPAYFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN PAY-TO FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NPAYOPEN BRANCH    NPAYPATH OF NPAYOPN1,NPAYOPN2,NPAYOPN3
NPAYOPN1 TRAP      NPAYGONE IF IO
         BRANCH    NPAYLOCK TO NPAYOP,NPAYOPR,NPAYOP
         
NPAYOP
.         OPEN      NPAYFILE,NPAYNAME
         OPEN      PayFList
         TRAPCLR   IO
         MOVE      C1 TO NPAYFLAG
         RETURN
NPAYOPR
.         OPEN      NPAYFILE,NPAYNAME,LOCKMANUAL,SINGLE
         OPEN      PayFList,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NPAYFLAG
         RETURN         

NPAYOPN2 TRAP      NPAYGONE IF IO
         BRANCH    NPAYLOCK TO NPAYOP2,NPAYOP2R,NPAYOP2
         
NPAYOP2 
.          OPEN      NPAYFIL2,NPAYNAME
         OPEN      PayFList
         TRAPCLR   IO
         MOVE      C1 TO NPAYFLG2
         RETURN
NPAYOP2R
.         OPEN      NPAYFIL2,NPAYNAME,LOCKMANUAL,SINGLE
         OPEN      PayFList,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NPAYFLG2
         RETURN         
.
NPAYOPN3 TRAP      NPAYGONE IF IO
         BRANCH    NPAYLOCK TO NPAYOP3,NPAYOP3R,NPAYOP3
         
NPAYOP3  
         OPEN      NPAYFLE3,NPAYNAME
         OPEN      PayFList
         TRAPCLR   IO
         MOVE      C1 TO NPAYFLG3
         RETURN
NPAYOP3R 
         OPEN      NPAYFLE3,NPAYNAME,LOCKMANUAL,SINGLE
         OPEN      PayFList,LOCKMANUAL,SINGLE
         TRAPCLR   IO
         MOVE      C1 TO NPAYFLG3
         RETURN
.
NPAYGONE MOVE      NPAYNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

