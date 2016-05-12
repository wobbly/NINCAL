PC       EQU       0
.nord0026.dbs daily order register.

         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.patch2.4
                                        include   compdd.inc
                                        include   cntdd.inc
.         INCLUDE   NMLRDD.inc
;patch2.4
         INCLUDE   GNXTDD.INC
         INCLUDE   NORDDD.inc
         INCLUDE   HP.INC
.START PATCH 2.2 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 2.2 - ADDED LOGIC
.START PATCH 2.3 - ADDED LOGIC
         INCLUDE   NSEL2DD.INC
.END PATCH 2.3 - ADDED LOGIC
release  init      "2.5"        DLH     14MARCH2007         Pacific Lists
.release  init      "2.4"        DMB    26MAY2004 Mailer Conversion
.RELEASE  INIT      "2.3"          29jan04 ASH DATACARD CONVERSION
.RELEASE  INIT      "2.2"          06MAY99 ASH REPLACE OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.RELEASE  INIT      "2.1"          29DEC98 ASH NINORD Y2K, File expansion
.RELEASE  INIT      "2.0"          28JUN94 DLH
.RELEASE  INIT      "1.3"          26MAY94 DLH LASER.
.RELEASE  INIT      "1.2"          19FEB93 DLH MINOR FIXES , COUNTER DISPLAYS,
.                                 PROBLEM IF RUN FROM COMMAND LINE.
.RELEASE  INIT      "1.1"         18MAR92  DLH NORDDD, PCBUS
.RELEASE  INIT      "1.0"          D.L.H.  17SEP91
.Start Patch #2.1 - increased file size
.NINORD    FILE      FIXED=566
.NINORD2   FILE      FIXED=566
PRTFILE1    FILE      FIXED=696
PRTFILE2   FILE      FIXED=696
.end Patch #2.1 - increased file size
.
DATE     DIM       8
. 
. OTHER  VARIABLES.
. ....................
.Start patch #2.1 - new var to hold increased OQTY
NUM9     FORM      9
.End patch #2.1 - new var to hold increased OQTY
HOLDREC  FORM      6       *HOLD CALCULATED NEXT INV ##
NEXTREC  FORM      6        *CURRENT INV NUMBER FOR COMPARE
.
TOTAL    FORM      5
REPRINT  FORM      5
.begin patch 2.5
SubTotP   FOrm      5                     .PL
SubTotN   FOrm      5                   .NIN
RPrintP   Form      5                   .PL
RPrintN   FOrm      5                   .NIN
SubQtyP   Form      11
SubQtyN   FOrm      11
.End patch 2.5
.Start Patch #2.1 - increase var due to OQTY increase
.NOT ESSENTIAL FOR FIRST TWO, BUT DID IT JUST TO BE SAFE - ASH
.TOTQTY   FORM      9
.QTYMASK  INIT      "ZZZ,ZZZ,ZZ9"
.PRTQTY   INIT      "Z,ZZZ,ZZ9"
TOTQTY   FORM      11
QTYMASK  INIT      "ZZ,ZZZ,ZZZ,ZZ9"
PRTQTY   INIT      "ZZZ,ZZZ,ZZ9"
.End Patch #2.1 - increase var due to OQTY increase
PAGE     FORM      4
LINES    FORM      2
LOCAL    INIT      "LOCAL"
PRTFLAG  DIM       1
FUNCBR   FORM      "0"
.Start Patch #2.1 - increase var
.RDATE    dim       8
RDATE    dim       10
.End Patch #2.1 - increase var
lrlast   dim       6
lrnumb   form      6
.
.
         MOVE      "DAILY ORDER REGISTER" TO STITLE
         MOVE      "Names In The News" TO COMPNME
         move      c0 to page
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         XIF
         MOVE      FUNC TO FUNCBR
         MATCH     "NORD0026" TO PROGRAM        *CHAINED FROM DSINIT?
         IF        NOT EQUAL                    *NO
         MOVE      "NORD0026 " TO PROGRAM
         MOVE      "NREG" TO PRTNAME
         ENDIF
         COMPARE   C1 TO FUNCBR
         IF        EQUAL
         MOVE      "NPRINT.srt" TO INPNAME
         ELSE
         MOVE      "NPRINT.RTN" TO INPNAME
         ENDIF
.Start Patch #2.1 - replaced logic, increase var due to OQTY increase
.         MOVE      C0 TO N7
         MOVE      C0 TO NUM9
.End Patch #2.1 - replaced logic, increase var due to OQTY increase
         MOVE      C0 TO N8
         MOVE      C0 TO N9
         MOVE      DATE TO TODAY
         CALL      PAINT
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         COMPARE   C1 TO FUNCBR
         IF        EQUAL
         OPEN      PRTFILE1,INPNAME,READ
         ELSE
         OPEN      PRTFILE2,INPNAME,READ
         ENDIF
          GOTO      PRTGET
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH pdrive,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
START   CALL      HD1
.
READ     COMPARE   C1 TO FUNCBR
         IF        EQUAL
         READ      PRTFILE1,seq;ORDVARS
         goto      moj if over
.         GOTO       TOTAL IF OVER
         ELSE
.START PATCH 2.2 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.2 - NEW LOGIC
.START PATCH 2.3 ADDED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.END PATCH 2.3 ADDED LOGIC
         READ       PRTFILE2,SEQ;ORDVARS
         GOTO       TOTAL IF OVER
.START PATCH 2.2 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.2 - NEW LOGIC
         ENDIF
         ADD       C1 TO N9
         DISPLAY   *P15:8,N9
         CMATCH    "Q" TO OSTAT
         GOTO       READ IF EQUAL
         CMATCH    "R" TO OSTAT
         IF        EQUAL
         ADD       C1 TO REPRINT
         DISPLAY   *P10:14,*EL,"REPRINTED ORDERS: ",REPRINT
.begin patch 2.5
          if        (OCompID = "P")
          add       c1,RPrintP
          else
          add       c1,RPrintN
          endif
.End patch 2.5
         GOTO      READ
         ENDIF
         ADD       C1 TO N8
         BRANCh    FUNCBR OF READ
MISSCHK  COMPARE   C1 TO FUNCBR
         GOTO      ADDONE IF NOT EQUAL
         COMPARE    C1 TO N8
         IF        EQUAL
         CMATCH    "X" TO OSTAT
           if        equal
           sub       c1 from n8
           branch    funcbr of ordloop,READ 
           endif
         MOVE      OLRN TO HOLDREC
         ENDIF
         MOVE      OLRN TO NEXTREC
         COMPARE   NEXTREC TO HOLDREC
         GOTO      MISSING IF NOT EQUAL
ADDONE   CMATCH    "X" TO OSTAT
         GOTO      CANCEL IF EQUAL
         ADD       C1 TO HOLDREC
         ADD       C1 TO TOTAL
.Start Patch #2.1 - replaced logic, increase var due to OQTY increase
.         MOVE      C0 TO N7
.         MOVE      OQTY TO N7
.         ADD       N7 TO TOTQTY
         MOVE      C0 TO NUM9
         MOVE      OQTY TO NUM9
         ADD       NUM9 TO TOTQTY
.End Patch #2.1 - replaced logic, increase var due to OQTY increase
.begin patch 2.5
          if        (OCompID = "P")
          add       c1,SubTOtP
          add       Num9,SubQtyP
          else
          add       c1,SubTotN
          add       Num9,SubQtyN
          endif
.End patch 2.5
         DISPLAY   *P10:12,*EL,"NEW DAILY ORDERS: ",N8
.
         PACK      MKEY FROM OMLRNUM,OCOBN
         CALL      NMLRKEY
.
.Start Patch #2.1 - replaced logic, increase var due to OQTY increase
.         MOVE      "Z,ZZZ,ZZ9" TO PRTQTY
.         EDIT      N7 TO PRTQTY
         MOVE      "ZZZ,ZZZ,ZZ9" TO PRTQTY
         EDIT      NUM9 TO PRTQTY
.End Patch #2.1 - replaced logic, increase var due to OQTY increase
         COMPARE   "57" TO LINES
         CALL      HD1 IF NOT LESS
         pack      rdate from ortndtem,slash,ortndted,slash,ortndtec,ortndtey
.Start Patch #2.1 - added century
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEY,*20,OODES,*69,O2DES
.START PATCH 2.2 - REPLACED LOGIC, OODES --> OFDESC
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OODES,*69,O2DES
.START PATCH 2.3 - REPLACED LOGIC
.         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
.                   *69,O1DES,*106,PRTQTY,*119,rdate:
.                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OFDESC,*69,O2DES
         PRINT     *N,*1,OLRN,*9,OMLRPON,*20,MCOMP:
                   *69,O1DES,*106,PRTQTY,*119,rdate:
                   *N,*9,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY,*20,OFDESC,*69,NSEL2NAME
.END PATCH 2.3 - REPLACED LOGIC
.END PATCH 2.2 - REPLACED LOGIC, OODES --> OFDESC
.End Patch #2.1 - added century
         ADD       C3 TO LINES
         branch    funcbr of ordloop,read
HD       compare   c0 to page
         if        equal
         PRINT     HPtmsr17,hpdups,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
         ADD       C1 TO PAGE
         PRINT     *F:
                   *n,*1,"CONFIDENTIAL":
                   *34,"***   N I N   O R D E R   R E G I S T E R   ":
                   "R E P O R T  ***":
                   *119,"DATE: ",DATE:
                   *N,*119,"PAGE:     ",PAGE;
         MOVE      C3 TO LINES
         RETURN
.
HD1      CALL      HD
         PRINT     *N,*1,"  LR",*9,"MAILER":
                   *20,"CLIENT COMPANY NAME",*69,"LIST":
                   *N,*1,"NUMBER",*9,"  PO##",*20,"OFFER DESCRIPTION":
                   *69,"DESCRIPTION",*108,"QUANTITY",*118,"RETURN DATE"
         ADD       C3 TO LINES
         RETURN
.
MISSING  
         COMPARE   "57" TO LINES
         CALL      HD1 IF NOT LESS
         PRINT     *L,*1,hpbon,"********* MISSING LR ## ******  ",HOLDREC,hpboff:
                   *FLUSH
.         PRINT     *1,"********* MISSING LR ## ******  ",HOLDREC:
         ADD       C1 TO HOLDREC
         ADD       C2 TO LINES
         GOTO      MISSCHK
CANCEL   
         COMPARE   "57" TO LINES
         CALL      HD1 IF NOT LESS
         PRINT     *L,*1,hpbon,HOLDREC," ********* CANCELLED LR ## ******  ":
                   hpboff,*flush
.         PRINT     *1,HOLDREC," ********* CANCELLED LR ## ******  ":
         ADD       C1 TO HOLDREC
         ADD       C2 TO LINES
        branch     funcbr of ordloop,READ
.
.middle of job
moj      close     PRTFILE1
         move      c1 to nordpath
         move      "NORDDALY" to gnxtfld       
         call      gnxtkey                  .get starting place.
         move      gnxtnum to nordfld
         move      "NORDNXT" to gnxtfld
         call      gnxtkey                  .get stopping place.
         move      gnxtnum to lrlast
         move      nordfld to n6
.         add       c1 to n6
.         MOVE      n6 TO HOLDREC
         move      c0 to n8
         call      nordkey
ordloop  call      nordks
         goto      eoj if over
.START PATCH 2.2 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 2.2 - NEW LOGIC
         match     olrn to lrlast
         goto      eoj if equal         
         add       c1 to n8
         ADD       C1 TO N9
         DISPLAY   *P15:8,N9
         CMATCH    "Q" TO OSTAT
         GOTO       ordloop IF EQUAL
         goto       misschk
.
eoj      move      "NORDDALY" to gnxtfld
         call       gnxtkey
         if         not over
         move       lrlast to lrnumb
         sub        c1 from lrnumb
         move       lrnumb to gnxtnum
         call       gnxtupd
         else                       .shit in your pants
         display   *p1:24,*el,"OOOOOOOOOOOOOOH MY GODDDDDDDDDDDDD!!!"
         endif
.         
TOTAL    COMPARE   "57" TO LINES
         CALL      HD IF NOT LESS
         IFNZ      PC
         PRINT     *1,"------------------------":
                   "------------------------":
                   "------------------------":
                   "------------------------":
                   "------------------------":
                   "----------"
         XIF
         IFZ       PC
         PRINT     *1,*RPTCHAR "-":132
         XIF
         EDIT      TOTQTY TO QTYMASK
         PRINT     *N,*7,"DAILY QUANTITY    : ",QTYMASK:
                   *N,*7,"NUMBER OF ORDERS  :       ",TOTAL:
                   *N,*7,"NUMBER OF REPRINTS:       ",REPRINT
.begin patch 2.5
SubTOTAL    COMPARE   "57" TO LINES
         CALL      HD IF NOT LESS
         IFNZ      PC
         PRINT     *1,"------------------------":
                   "------------------------":
                   "------------------------":
                   "------------------------":
                   "------------------------":
                   "----------"
         XIF
         IFZ       PC
         PRINT     *1,*RPTCHAR "-":132
         XIF
         Move      "ZZ,ZZZ,ZZZ,ZZ9",QtyMask

         EDIT      SubQTYN TO QTYMASK
         PRINT     *N,*7,"NIN DAILY QUANTITY    : ",QTYMASK:
                   *N,*7,"NIN NUMBER OF ORDERS  :       ",SubTotN:
                   *N,*7,"NIN NUMBER OF REPRINTS:       ",RPRINTN
      
        COMPARE   "57" TO LINES
         CALL      HD IF NOT LESS
         IFNZ      PC
         PRINT     *1,"------------------------":
                   "------------------------":
                   "------------------------":
                   "------------------------":
                   "------------------------":
                   "----------"
         XIF
         IFZ       PC
         PRINT     *1,*RPTCHAR "-":132
         XIF
         Move      "ZZ,ZZZ,ZZZ,ZZ9",QtyMask

         EDIT      SubQTYP TO QTYMASK
         PRINT     *N,*7,"PL  DAILY QUANTITY    : ",QTYMASK:
                   *N,*7,"PL  NUMBER OF ORDERS  :       ",SubTotP:
                   *N,*7,"PL  NUMBER OF REPRINTS:       ",RPRINTP
.End patch 2.5
         shutdown  "cls"
         STOP
.patch2.4
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch2.4
         include   nordio.inc
         INCLUDE   GNXTIO.INC
.START PATCH 2.2 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 2.2 - ADDED LOGIC
.START PATCH 2.3 - ADDED LOGIC
         INCLUDE   NSEL2IO.INC
.END PATCH 2.3 - ADDED LOGIC
         INCLUDE   COMLOGIC.inc
