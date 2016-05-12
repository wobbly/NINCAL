..............................................................................
.
. NMOBIO INCLUSION
. NIN MONNEY ON ACCOUNT BALANCE FILE I/O ROUTINES
.
.
. FILE NAME : NINMOB
. REC LENGTH:  FIXED
. INDEX KEY :
.
. 17OCT2005 ASH - Added trapping logic to all IO operations
. PATCH 1.1 17APR01 ASH ADDED BRANCH FOR GUI PROGRAMS
. add test read 18mar96 DLH.
. tighten up code and PI's around write & delete 2jun95 DLH.
. primary write & update access changed to nmobfle2 9/26/94 DLH.
..............................................................................
.READBAL - READ ACCOUNT BALANCE FILE.
NMObKEY  BRANCH    NMObPATH OF NMOb1A,NMOb1C
NMOB1a   BRANCH    NMOBFLAG TO NMOB1b
         CALL      NMOBOPEN
NMOB1b
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFILE
         READ      NMOBFILE,NMObFLD;nmobmlr,nmobmcnt,BALANCE,nmobbrk
         GOTO      NOACC1 IF OVER
         trapclr IO
         RETURN
NMOB1c   BRANCH    NMOBFLG2 TO NMOB1d
         CALL      NMOBOPEN
NMOB1d
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFLE2
         READ      NMOBFLE2,NMOAFLD4;nmobmlr,nmobmcnt,BALANCE,nmobbrk
         GOTO      NOACC1 IF OVER
         trapclr IO
         RETURN
..............................................................................
.
. ENTRY POINT : NMobTST
. REQUIRED    : KEY
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NMObtst  BRANCH    NMObPATH OF NMOb2A,NMOb2C
NMOB2a   BRANCH    NMOBFLAG TO NMOB2b
         CALL      NMOBOPEN
NMOB2b
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFILE
         READ      NMOBFILE,NMObFLD;str1
         trapclr IO
         RETURN
NMOB2c   BRANCH    NMOBFLG2 TO NMOB2d
         CALL      NMOBOPEN
NMOB2d
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFLE2
         READ      NMOBFLE2,NMOAFLD4;str1
         trapclr IO
         RETURN
...............................................................................
.READKS - READ ACCOUNT BALANCE FILE.
NMObKs  BRANCH    NMObPATH OF NMOb3A,NMOb3C
NMOB3a   BRANCH    NMOBFLAG TO NMOB3b
         CALL      NMOBOPEN
NMOB3b
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFILE
         READKS    NMOBFILE;nmobmlr,nmobmcnt,balance,nmobbrk
         GOTO      NOACC1 IF OVER
         trapclr IO
         RETURN

NMOB3c   BRANCH    NMOBFLG2 TO NMOB3d
         CALL      NMOBOPEN
NMOB3d
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFLE2
         READks    NMOBFLE2;nmobmlr,nmobmcnt,BALANCE,nmobbrk
         GOTO      NOACC1 IF OVER
         trapclr IO
         RETURN
...............................................................................
.READBALS - READ ACCOUNT BALANCE FILE.
NMObseq  BRANCH    NMObPATH OF NMOb4A,NMOb4C
NMOB4a   BRANCH    NMOBFLAG TO NMOB4b
         CALL      NMOBOPEN
NMOB4b
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFILE
               READ      NMOBFILE,SEQ;nmobmlr,nmobmcnt,balance,nmobbrk
         GOTO      NOACC1 IF OVER
         trapclr IO
         RETURN
NMOB4c   BRANCH    NMOBFLG2 TO NMOB4d
         CALL      NMOBOPEN
NMOB4d
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFLE2
         READ     NMOBFLE2,seq;nmobmlr,nmobmcnt,BALANCE,nmobbrk
         GOTO      NOACC1 IF OVER
         trapclr IO
         RETURN
...............................................................................
.WRITEBAL - WRITE ACCOUNT BALANCE FILE.
NMOBWRT
.          move      c2 to nmobpath
.         BRANCH    NMOBFLG2 TO NMOB5
.         CALL      NMOBOPEN
               BRANCH    NMObFLaG TO NMOb5
               MOVE      C1 TO NMObPATH
               CALL      NMObOPEN
NMOB5
         move      c2 to nmobpath
         BRANCH    NMOBFLG2 TO NMOB5b
         CALL      NMOBOPEN
NMOb5B
         COMPARE   C0 TO CHANGE           *NEG. BALANCE?
         GOTO      WRTBALOK IF EQUAL        *NO, OK.
         GOTO      NEGBAL IF NOT LESS             *YES, NOT ALLOWED
WRTBALOK UNPACK    NMObFLD INTO nmobmlr
.         ADD       CHANGE TO BALANCE
               MOVE      CHANGE TO BALANCE            *DLH 25FEB93
         move      nmoabrk to nmobbrk
               move      mlr to nmobmlr
               move      mcnt to nmobmcnt
         pack      nmobfld from mlr
         trap IOMssg giving Error if IO
         FILEPI    2;NMOBFLE2,nmobfile
         WRITE     NMOBFLE2,NMOAFLD4;nmobmlr,nmobmcnt,balance,nmobbrk
         INSERT    NMObFiLE,nmobfld
         trapclr IO
.              BRANCH    NMObFLaG TO NMOb5B
.              MOVE      C1 TO NMObPATH
.              CALL      NMObOPEN
.NMOb5B
.         pack      nmobfld from mlr
.         filepi    1;nmobfile
.         INSERT    NMObFiLE,nmobfld
         RETURN
...............................................................................
.UPDBAL - READ & update ACCOUNT BALANCE FILE.
NMOBUPD  move      c2 to nmobpath
         BRANCH    NMOBFLG2 TO NMOB6
         CALL      NMOBOPEN
NMOB6
;         FILEPI    7;NMOBFLE2                             ;reduced 22 March 2004 belv related to A01 on moanotes DLH
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFLE2
         READ      NMOBFLE2,NMOAFLD4;nmobmlr,nmobmcnt,balance,nmobbrk
         GOTO      NMOBWRT IF OVER
         trapclr IO
         ADD       CHANGE TO BALANCE
         COMPARE   C0 TO BALANCE          *NEGATIVE BALANCE?
         GOTO      UPBALOK IF EQUAL         *NO, OK.
         GOTO      NEGBAL IF NOT LESS           *YES, NOT ALLOWED
UPBALOK
         trap IOMssg giving Error if IO
         FILEPI    1;NMOBFLE2
         UPDATE    NMOBFLE2;nmobmlr,nmobmcnt,balance,nmobbrk
         trapclr IO
         RETURN
...............................................................................
.DELBAL - DELETE'S BALANCE RECORD
NMOBDEL
               BRANCH    NMObFLaG TO NMOb7
               MOVE      C1 TO NMObPATH
               CALL      NMObOPEN
NMOB7
               MOVE      C2 TO NMObPATH
         BRANCH    NMOBFLG2 OF NMOB7B
         CALL      NMOBOPEN
NMOB7b
         trap IOMssg giving Error if IO
         FILEPI    2;NMOBFLE2,nmobfile
         DELETE    NMOBFLE2,NMOAFLD4
         deletek    NMObFiLE,nmobfld
         trapclr IO
.              BRANCH    NMObFLaG TO NMOb7B
.              MOVE      C1 TO NMObPATH
.              CALL      NMObOPEN
.NMOb7B   filepi     1;nmobfile
.         deletek    NMObFiLE,nmobfld
         RETURN
...............................................................................
NOACC1
.START PATCH 1.1 REPLACED LOGIC
.         DISPLAY   *P1:24,*EL,"NO BALANCE RECORD FOUND";
.         MOVE      "Y" TO OVER
.         RETURN
               IF             (NmobMsgFlag <> "N")
        IFZ     PC
                DISPLAY *P1:24,*EL,"NO BALANCE RECORD FOUND";
        XIF
        IFNZ    PC
                alert   note,"NO BALANCE RECORD FOUND!",result
        XIF
               endif
        MOVE    "Y" TO OVER
        RETURN
.END PATCH 1.1 REPLACED LOGIC
...............................................................................
NEGBAL
.START PATCH 1.1 REPLACED LOGIC
.         DISPLAY   *P1:24,*EL,*B,"This entry would result in a negative":
.                   " balance, ",*HON,"NOT applied",*B,*W4,*HOFF;
..         compare    c1 to prio   .override ?  .THIS CODE ONLY TO BE USED
..         return     if equal     .yes     .WITH WRITTEN PERMISSION FROM
..                                         .THE HEAD OF I.S. & ACCOUNTING.
        IFZ     PC
                DISPLAY *P1:24,*EL,*B,"This entry would result in a negative":
                        " balance, ",*HON,"NOT applied",*B,*W4,*HOFF;
        XIF
        IFNZ    PC
                alert   caution,"This entry would result in a Negative Balance.  Balance NOT applied!",result
               IF   (NmobOkFlag = Yes)
                              move No to code96ok
                              return
               endif               
        XIF
.         compare    c1 to prio   .override ?  .THIS CODE ONLY TO BE USED
.         return     if equal     .yes     .WITH WRITTEN PERMISSION FROM
.                                         .THE HEAD OF I.S. & ACCOUNTING.
.END PATCH 1.1 REPLACED LOGIC
         NORETURN
         STOP
.         RETURN
...............................................................................
NMObOPEN  BRANCH   NMObPATH TO NMObOPN1,NMObOPN2
NMOBOPN1  TRAP      NMOBGONE IF IO
          move      nmobnme1 to nmobname
          OPEN      NMOBFILE,NMOBNAME
          TRAPCLR   IO
          MOVE      C1 TO NMOBFLAG
          RETURN
NMOBOPN2  TRAP      NMOBGONE giving error IF IO
          move      nmobnme2 to nmobname
          OPEN      NMOBFLE2,NMOBNaME
          TRAPCLR   IO
          MOVE      C1 TO NMOBFLG2
          RETURN
NMOBGONE  MOVE      NMOBNAME TO FILENAME
          GOTO      FILEGONE
