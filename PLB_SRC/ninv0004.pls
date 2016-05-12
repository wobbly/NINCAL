PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 2.7
.         INCLUDE   NINVDD.inc
              INCLUDE         ninvdd.inc
.begin patch 2.7
         include   norddd.inc
release   init      "2.72"         DLH Check for corrupt records
Reldate   Init      "22 march 2012"
.release       init            "2.71"         DLH 2005Sep12 Invoice COnversion
.release      init            "2.7"         DLH 2005March02 Invoice COnversion
.release  init      "2.6"         02OCT2000 ASH NEW SERVER ADDED
.release  init      "2.5"         26Aug99 dlh NINadj nadjust Y2K
.release  init      "2.4"         4May99 dlh NINVOICE Y2K
.release  init      "2.3"         19OCT95 dlh remove update use delete add.
.RELEASE  INIT      "2.2"         05aug94 added order inc for olon move.
.RELEASE  INIT      "2.1"        03AUG93 TYPIST INITIALS TO 3 BYTES.
.        
.RELEASE  INIT      "2.0"        DLH  23MAR92. CONS,COMLOGIC, NINVDD.
.begin patch 2.4
.begin patch 2.7
.begin patch 2.71
.NINVOICE IFILE     KEYLEN=6,VAR=303
.end patch 2.71
INFILE    FILE     
.INFILE   IFILE     KEYLEN=6,VAR=403
NINVPRT       IFILE           KEYLEN=6,FIX=303,DUP
.NINVPRT  IFILE     KEYLEN=6,FIX=403,DUP
.end patch 2.7
.end patch 2.4
.NINVOICE IFILE     KEYLEN=6,VAR=305,STATIC=12
.NINVPRT  IFILE     KEYLEN=6,FIX=305,DUP
.NINVOICE IFILE     KEYLEN=6,VAR=297,STATIC=12
.NINVPRT  IFILE     KEYLEN=6,FIX=297,DUP
.DUMFILE  FILE
READCNT  FORM      4
.TYPIST   DIM       3
DUPCODE  DIM       1       USED IF DUPE RECORD
DUPSTATB DIM       1         "   "  "      "

.
.begin patch 2.7
          call      debug
          erase     "\\nins1\e\data\ninvoice.cop"
          copyfile  "E:\data\ninvoice.dat|NINS1:502","E:\data\ninvoice.cop|NINS1:502"
          call      debug
              Open            Infile,"ninvoice|NINS1:502"
.             Open            ninvoice,"ninvoice",share 
.         OPEN      INFILE,"NINVOICE.ISI|NINS1:502"
.end patch 2.7
.end patch 2.83
.begin patch 2.7
.         OPEN      NINVOICE,"NINvoice.ISI|NINS1:502"
.end patch 2.7
.         OPEN      NINVOICE,"NINVOICE",SHARE
.         PREP      DUMFILE,"g:\DATA\NINVPRT.DAT"
.         CLOSE     DUMFILE
.begin patch 2.4
.         PREP      NINVPRT,"g:\DATA\NINVPRT","g:\DATA\NINVPRT","6","305"
.START PATCH 2.6 REPLACED LOGIC
.         PREP      NINVPRT,"g:\DATA\NINVPRT","g:\DATA\NINVPRT","6","403"
         PACK      STR35,NTWKPATH1,"NINVPRT"
         PACK      STR45,NTWKPATH1,"NINVPRT"
.begin patch 2.7
.        PREP      NINVPRT,STR35,STR45,"6","403"
.             PREP            NINVPRT,STR35,STR45,"6","288"
              PREP            NINVPRT,STR35,STR45,"6","303"
.end patch 2.7
.END PATCH 2.6 REPLACED LOGIC
.end patch 2.4
         MOVE      "NINV0004" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "INVOICE PRINT PREP" TO STITLE
         CALL      PAINT
.
READ     FILEPI    1;INFILE
        READ      INFILE,SEQ;INVVARS:
.begin patch 2.71
                              Inits
.                             TYPIST            
.end patch 2.71
         GOTO      EOJ IF OVER
.begin patch 2.4
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.end patch 2.4
.begin patch 2.72
          Call      trim using lrn
          call      trim using invnum
          if        (lrn = "")
.call subroutine send email
          goto      Read
          endif
          if        (invnum = "")
.call subroutine send email
          goto      Read
          endif
.end patch 2.72
         move      lrn to nordfld
         move      c1 to nordpath
         call      nordkey
         move      olon to lon
         MOVE      INVNUM TO NINVFLD
         REP       " 0" IN NINVFLD
         filepi    1;ninvprt
         READ      NINVPRT,NINVFLD;DUPCODE,DUPSTATB;
         GOTO      WRITE IF OVER
         CMATCH    "R" TO DUPSTATB              *REPRINT?????
         GOTO      WRITOK IF NOT EQUAL
         GOTO      WRITE
WRITOK   CMATCH    "R" TO STATB
         GOTO      WRITE IF NOT EQUAL            *NEITHER COPY A REPRINT.
         MOVE      DUPCODE TO CODE
         MOVE      DUPSTATB TO STATB
UPDATE  
         FILEPI    1;NINVprt
         DELETE    NINVprt,NINVFLD
.         FILEPI    1;NINVPRT,NINVOICE
.         UPDATE     NINVPRT;INVVARS:
.                              TYPIST
.         GOTO      DELETE
WRITE
         FILEPI    1;NINVPRT
         WRITE      NINVPRT,NINVFLD;INVVARS:
.begin patch 2.71
                              Inits
.              TYPIST
.begin patch 2.71
.
         MOVE      LRN TO NINVFLD
         REP       " 0" IN NINVFLD
DELETE 
.begin patch 2.7
.         read      
.begin patch 2.71
.         FILEPI    1;NINVOICE
              Call            PINVTST 
.                             read               ninvoice,ninvfld;;
              if    not over      
              call            Pinvdel
              endif
.         DELETE    NINVOICE,NINVFLD         
.end patch 2.71
.            call           Pinvdel
.end patch 2.7
         ADD       C1 TO READCNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED : ",READCNT
         GOTO      READ
EOJ      WEOF      NINVPRT,SEQ
         IFNZ      PC
         FLUSH     NINVPRT
         XIF
         CLOSE     NINVPRT,EOFSIZE
         STOP
.begin patch 2.7
              Include         ninvio.inc
.end patch 2.7

         include   nordio.inc
         INCLUDE   COMLOGIC.inc


