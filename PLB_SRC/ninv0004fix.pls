PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.begin patch 2.7
.         INCLUDE   NINVDD.inc
              INCLUDE         ninvdd.inc
.begin patch 2.7
         include   norddd.inc
release       init            "1.00"         DLH Put invoices back in print file
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
              Open            Infile,"ninvoice|NINS1:502"

.              Open            NINVprt,"ninvoice|NINS1:502"
              Open            NINVprt,"ninvPRT|NINS1:502"
.END PATCH 2.6 REPLACED LOGIC
.end patch 2.4
         MOVE      "NINV4fix" TO PROGRAM
         MOVE      "Names in the News" TO COMPNME
         MOVE      "INVOICE PRINT fix" TO STITLE
         CALL      PAINT
.
READ
.          MOve      "594246",ninvfld
.          move      Ninvfld,n6
.          move      c2,Ninvpath

        FILEPI    1;INFILE
        READ      INFILE,SEQ;INVVARS:
                              Inits
         GOTO      EOJ IF OVER
.begin patchy
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
.end patchy
          GOTO      NEXT


          Loop
          call      Ninvkey
          Until     Over          
          READ      NINVPRT,Ninvfld;str1
          if        over
          WRite     Ninvprt,ninvfld;invvars,"GS"
          endif
          add       c1,n6
          move      n6,ninvfld
          repeat
          goto      eoj
.end patch 2.71
         GOTO      EOJ IF OVER
.begin patch 2.4
.         REP       " 0" IN AR
.         REP       " 0" IN AP1
.         REP       " 0" IN AP2
.end patch 2.4
nEXT
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
              if    not over
.                             read               ninvoice,ninvfld;;
              call            Pinvdel
              endif      
.         DELETE    NINVOICE,NINVFLD         
.end patch 2.71
.            call           Pinvdel
.end patch 2.7
         ADD       C1 TO READCNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES PROCESSED : ",READCNT
         GOTO      READ
EOJ      
         STOP
.begin patch 2.7
              Include         ninvio.inc
.end patch 2.7

         include   nordio.inc
         INCLUDE   COMLOGIC.inc


