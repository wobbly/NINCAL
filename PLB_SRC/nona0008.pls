.WRITTEN 13MArch95
...............................................................................
.INPUT FILE SORTED FROM master detail file:
..
.run with each control
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NMOADD.inc
         INCLUDE   NMOBDD.inc
.patch1.45
          include   compdd.inc
          include   cntdd.inc
.         INC       NMLRDD.INC
.patch1.45
         include   hp.inc
         INCLUDE   NDATDD.inc
.         include   nbrkdd.inc
release  init      "1.52"          DLH  New reason code 23
Reldate   INit      "14 October 2010"
.release  init      "1.51"          DLH New reason code
.Reldate  INit      "05 November 2007"
.release  init      "1.5"          DLH  16August2007        PLI Conversion
.release  init      "1.45"          JD  26MAY2004 Mailer Conversion
.release  init      "1.4"          DLH add subtotals by reason code
.release  init      "1.3"          DLH Replace nineteen with CC
.RELEASE  INIT      "1.2"          ASH NINMOA Y2K,File expansion
.Release  init      "1.1"          DLH 21Mar95 add net change
.release  init      "1.0"          DLH 13March95 new 
BR       FORM      2
DD1      DIM       2
HOLDMM   DIM       2           USED TO CHECK FOR CURRENT MONTH TRANSACTION.
HOLDYY   DIM       2           USED TO CHECK FOR CURRENT YEAR TRANSACTION.
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
balrec   dim       1
SELECT   FORM      2           BRANCH FOR REASON
SAMEMLR  DIM       4           USED TO VERIFY STILL READING CORRECT ACCOUNT
samebrk  dim       4      
DATE     DIM       8
PAGE     FORM      4           PAGE NUMBER
LINES    FORM      2           LINE NUMBER
MONTH    DIM       10
YEAR     DIM       4
.NINETEEN INIT      "19"
ctrlnum  dim       3
JAN      INIT      "JANUARY"
FEB      INIT      "FEBRUARY"
MAR      INIT      "MARCH"
APR      INIT      "APRIL"
MAY      INIT      "MAY"
JUN      INIT      "JUNE"
JUL      INIT      "JULY"
AUG      INIT      "AUGUST"
SEP      INIT      "SEPTEMBER"
OCT      INIT      "OCTOBER"
NOV      INIT      "NOVEMBER"
DEC      INIT      "DECEMBER"
reasonM  INIT      "$$,$$$,$$$.99-"
DOLLAR   INIT      "$$,$$$,$$$.99"
DOLLAR1  INIT      "$$$,$$$,$$$.99-"
AMNTAPP  DIM       13
AMNTRECD DIM       13
SUBAPP   FORM      8.2
SUBRECD  FORM      8.2
MTDAPP   FORM      8.2
MTDRECD  FORM      8.2
MTDCHNG  FORM      9.2
MTDTAPP  FORM      8.2      M-T-D TOTAL APP
MTDTRECD FORM      8.2      M-T-D TOTAL REC'D
MTDTCHNG FORM      9.2      M-T-D TOTAL CHANGE
TOTAPP   FORM      9.2
TOTRECD  FORM      9.2
.begin patch 1.4
TotReas1 form      8.2      .totals by reason code
TotReas2 form      8.2      .totals by reason code
TotReas3 form      8.2      .totals by reason code
TotReas4 form      8.2      .totals by reason code
TotReas5 form      8.2      .totals by reason code
TotReas6 form      8.2      .totals by reason code
TotReas7 form      8.2      .totals by reason code
TotReas8 form      8.2      .totals by reason code
TotReas9 form      8.2      .totals by reason code
TotReas10 form      8.2      .totals by reason code
TotReas11 form      8.2      .totals by reason code
TotReas12 form      8.2      .totals by reason code
TotReas13 form      8.2      .totals by reason code
TotReas14 form      8.2      .totals by reason code
TotReas15 form      8.2      .totals by reason code
TotReas16 form      8.2      .totals by reason code
TotReas17 form      8.2      .totals by reason code
TotReas18 form      8.2      .totals by reason code
TotReas19 form      8.2      .totals by reason code
TotReas20 form      8.2      .totals by reason code
TotReas21 form      8.2      .totals by reason code
.begin patch 1.51
TotReas22 form      8.2      .totals by reason code
.end patch 1.51
.begin patch 1.52
TotReas23 form      8.2      .totals by reason code
.end patch 1.52
TotReas99 form      8.2      .totals by reason code
calcReas  FORM      8.2
CntReas1 form      4      .count by reason code
CntReas2 form      4      .count by reason code
CntReas3 form      4      .count by reason code
CntReas4 form      4      .count by reason code
CntReas5 form      4      .count by reason code
CntReas6 form      4      .count by reason code
CntReas7 form      4      .count by reason code
CntReas8 form      4      .count by reason code
CntReas9 form      4      .count by reason code
CntReas10 form      4      .count by reason code
CntReas11 form      4      .count by reason code
CntReas12 form      4      .count by reason code
CntReas13 form      4      .count by reason code
CntReas14 form      4      .count by reason code
CntReas15 form      4      .count by reason code
CntReas16 form      4      .count by reason code
CntReas17 form      4      .count by reason code
CntReas18 form      4      .count by reason code
CntReas19 form      4      .count by reason code
CntReas20 form      4      .count by reason code
CntReas21 form      4      .count by reason code
.begin patch 1.51
CntReas22 form      4      .count by reason code
.end patch 
.begin patch 1.52
CntReas23 form      4      .count by reason code
.end patch 1.52
CntReas99 form      4      .count by reason code
cntReas  FORM      4
.end patch 1.4
TOTAMASK DIM       14
TOTRMASK DIM       14
SUBAMASK DIM       13
SUBRMASK DIM       13
MTDRMASK DIM       13
MTDAMASK DIM       13
MTDTMASK DIM       15
.Start Patch #1.2 - expanded var
.TRANDTE   DIM       8
.DINVDTE  DIM       8
TRANDTE   DIM      10
DINVDTE  DIM       10
.End Patch #1.2 - expanded var
FIRSTPAS INIT      "Y"
TOTAL    FORM      9.2
XFOOT    FORM      9.2       USED TO VERF. DETAIL ENTRIES TO BALANCE
DETAIL$  FORM      8.2
TOTDOLL  INIT      "$$$,$$$,$$$.99"
TOTDOLL1 INIT      "$$$,$$$,$$$.99-"
COUNT    FORM      5
LBRACKET DIM       1      USED FOR '<'
RBRACKET DIM       1      USED FOR '>'
.
LOCAL    INIT      "LOCAL"
PRTFLAG  DIM       1
lasrflag init      "T"            generally true unless a/p clerk has req
.begin patch 1.5
CoTitle   INit      "NIN"
.end patch 1.5
+..............................................................................
.MAIN
         MOVE       "MONEY by control PRINT" TO STITLE
         CLOCK     DATE TO DATE
         UNPACK    DATE INTO MM,STR1,DD1,STR1,YY
         MATCH     "NONA0008" TO PROGRAM    .ENTRY FROM DSINIT ?
         IF        NOT EQUAL                .NO
         MOVE       "NONA0008" TO PROGRAM       .SET DEFAULTS
         MOVE      "LOCAL" TO PRTNAME
         MOVE       "Names In The News" TO COMPNME
         ENDIF
         match     "NOLASER" in comment
         if        equal
         move      "F" to lasrflag
         endif
         MOVE      DATE TO TODAY
         CALL      PAINT
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Today's Date : "
.begin patch 1.5
          if        (company = C2)
          move      "PLI",COTItle
          else
          Move      "NIN",COTitle
          endif
.end patch 1.5
         MOVE      C3 TO Nmoapath
         unpack    inpname into str4,str3
         clear     nmoanme3
         append    "nmoa" to nmoanme3
         append    str3 to nmoanme3
         reset     nmoanme3
         move      nmoanme3 to inpname
         move      str3 to ctrlnum
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
         OPEN      NMOAFLE3,inpname
         MOVE       C1 TO NMOAFLG3
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
         PACK      PRTFILE WITH PDRIVE,PRTNAME
.         SPLOPEN   PRTFILE
         SPLOPEN   "\\nins2\Laser2","R"
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
+............................................................................
START   
         move      c2 to nmobpath
         PACK      YEAR FROM CC,YY
         MOVE      MM TO NMM
         MOVE      MM TO HOLDMM
         MOVE      YY TO HOLDYY
         LOAD      MONTH FROM NMM OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
                   NOV,DEC
         call      header
..............................................................................
MAIN     MOVE      NO TO OVER
         CALL      NMOASEQ
         GOTO      DONE IF OVER
         ADD       C1 TO COUNT
         DISPLAY   *P10:12,"RECORDS PROCESSED : ",COUNT,"  ",TOTAL
         CLEAR     OLSTNAME
         PACK      MKEY FROM MLR,MCNT
         match     nmoabrk to samebrk
         if        not equal
         pack      nbrkfld from nmoabrk,z3
         call      nbrkkey
         if        over
         clear     brcomp
         endif
         endif
         MATCH     mlr TO SAMEMLR
         CALL      BREAK IF NOT EQUAL
         CMATCH    B1 TO LIST
         CALL      READCARD IF NOT EOS
         CALL      PRINT
         GOTO      MAIN
BREAK
         CMATCH    YES TO FIRSTPAS
         GOTO      FIRST IF EQUAL
         PACK      NMOAFLD4 FROM samebrk,SAMEMLR
         MOVE      MKEY TO SAMEMLR
         move      nmoabrk to samebrk
         CALL      READMLR
         RETURN

FIRST    MOVE      NO TO FIRSTPAS
         MOVE      MKEY TO SAMEMLR
         move      nmoabrk to samebrk
         CALL      READMLR
         RETURN
PRINT
.Start Patch #1.2 - remmed and replaced line
.         UNPACK    INVDATE INTO MM,DD,YY
.         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    INVDATE INTO STR2,YY,MM,DD
         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,STR2,YY
.End Patch #1.2 - remmed and replaced line
         CLEAR     TRANDTE
         TYPE      TRANDATE
         CALL      EDTEAPP IF EQUAL
         CLEAR     AMNTAPP
         CLEAR     AMNTRECD
         CLEAR     LBRACKET
         CLEAR     RBRACKET
         ADD       ONAMOUNT TO XFOOT
         COMPARE   c0 TO ONAMOUNT
         GOTO      EDITRECD IF LESS
         GOTO      EDITAPP
PRINT1   CLEAR     RDESC
         COMPARE   "99" TO REASON
         CALL      REASON IF EQUAL
         LOAD      RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
                   REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
.begin patch 1.51
.                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21
                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REAS22,REAS23
.end patch 1.52
.
.begin patch 1.4
         move      c0 to calcreas
         move      c0 to cntreas
         LOAD      Calcreas FROM REASON OF totreas1,totreas2,totreas3,totreas4,totreas5:
                   totreas6,totreas7,totreas8,totreas9,totreas10,totreas11,totreas12,totreas13,totreas14:
.begin patch 1.51
.                   totreas15,totreas16,totreas17,totreas18,totreas19,totreas20,totreas21
                   totreas15,totreas16,totreas17,totreas18,totreas19,totreas20,totreas21,TotReas22,TotReas23
.end patch 1.52
         LOAD      cntreas FROM REASON OF cntreas1,cntreas2,cntreas3,cntreas4,cntreas5:
                   cntreas6,cntreas7,cntreas8,cntreas9,cntreas10,cntreas11,cntreas12,cntreas13,cntreas14:
.begin patch 1.51
.                   cntreas15,cntreas16,cntreas17,cntreas18,cntreas19,cntreas20,cntreas21
                   cntreas15,cntreas16,cntreas17,cntreas18,cntreas19,cntreas20,cntreas21,cntreas22,CntReas23
.end patch 1.52
         if        (reason = "99")
         move      totreas99 to calcreas
         add       onamount to calcreas
         add       c1 to cntreas
         else
         add       onamount to calcreas
         add       c1 to cntreas
         store     Calcreas into REASON OF totreas1,totreas2,totreas3,totreas4,totreas5:
                   totreas6,totreas7,totreas8,totreas9,totreas10,totreas11,totreas12,totreas13,totreas14:
.begin patch 1.51
.                   totreas15,totreas16,totreas17,totreas18,totreas19,totreas20,totreas21
                   totreas15,totreas16,totreas17,totreas18,totreas19,totreas20,totreas21,TotReas22,TotReas23
.End patch 1.52
         store     Cntreas into REASON OF cntreas1,cntreas2,cntreas3,cntreas4,cntreas5:
                   cntreas6,cntreas7,cntreas8,cntreas9,cntreas10,cntreas11,cntreas12,cntreas13,cntreas14:
.begin patch 1.51
.                   cntreas15,cntreas16,cntreas17,cntreas18,cntreas19,cntreas20,cntreas21
                   cntreas15,cntreas16,cntreas17,cntreas18,cntreas19,cntreas20,cntreas21,cntreas22,CntReas23
.end patch 1.52
         endif
.end patch 1.4
         COMPARE   "59" TO LINES
         call      header if not less
         cmatch    true to lasrflag
         if        equal
         print     *1,hpbon,mcomp,str1,brcomp,hpboff
         PRINT     *1,TRANDTE,*17,CONTROL,*24,LRNUM,*34,INVOICE:
                   *44,DINVDTE,*75,CHECKNUM,*FLUSH;
         PRINT     *56,LBRACKET,*57,AMNTRECD,RBRACKET:
                   *95,RDESC,*120,TRANSNUM:
                   *L,*10,LIST,B1,OLSTNAME,*95,COMMENT,*l
         else
         print     *1,p24bon,mcomp,str1,brcomp,p24boff
         PRINT     *1,TRANDTE,*17,CONTROL,*24,LRNUM,*34,INVOICE:
                   *44,DINVDTE,*75,CHECKNUM,*FLUSH;
         PRINT     *56,LBRACKET,*57,AMNTRECD,RBRACKET:
                   *95,RDESC,*120,TRANSNUM:
                   *L,*10,LIST,B1,OLSTNAME,*95,COMMENT,*l
         endif
         ADD       c5 TO LINES
         RETURN
.
REASON   MOVE      "ENTRY CORRECTION" TO RDESC
         RETURN
.
EDITAPP  MOVE      DOLLAR TO AMNTRECD
         MOVE      ONAMOUNT TO DETAIL$
         ADD       ONAMOUNT TO SUBAPP
         ADD       ONAMOUNT TO TOTAPP
         EDIT      DETAIL$ TO AMNTRECD
         MATCH     MM TO HOLDMM        *CURRENT MONTH TRANS?
         GOTO      PRINT1 IF NOT EQUAL   *NO.
         MATCH     YY TO HOLDYY        *CURRENT YEAR?
         GOTO      PRINT1 IF NOT EQUAL     *NO.
         ADD       ONAMOUNT TO MTDAPP
         GOTO      PRINT1
EDITRECD MOVE      DOLLAR TO AMNTRECD
         MULT      "-1" BY ONAMOUNT
         MOVE      ONAMOUNT TO DETAIL$
         ADD       ONAMOUNT TO SUBRECD
         ADD       ONAMOUNT TO TOTRECD
         EDIT      DETAIL$ TO AMNTRECD
         MOVE      "<" TO LBRACKET
         MOVE      ">" TO RBRACKET
         MATCH     MM TO HOLDMM        *CURRENT MONTH TRANS?
         GOTO      PRINT1 IF NOT EQUAL   *NO.
         MATCH     YY TO HOLDYY        *CURRENT YEAR?
         GOTO      PRINT1 IF NOT EQUAL     *NO.
         ADD       ONAMOUNT TO MTDRECD
         GOTO      PRINT1
EDTEAPP  
.Start patch #1.2 - remmed and replaced line
.         UNPACK    TRANDATE INTO MM,DD,YY
.         PACK      TRANDTE FROM MM,SLASH,DD,SLASH,YY
         UNPACK    TRANDATE INTO STR2,YY,MM,DD
         PACK      TRANDTE FROM MM,SLASH,DD,SLASH,STR2,YY
.End patch #1.2 - remmed and replaced line         
         RETURN
PRINTOT  
         MOVE      DOLLAR TO AMNTRECD
         MULT      "-1" BY BALANCE
         MOVE      BALANCE TO DETAIL$
         EDIT      DETAIL$ TO AMNTRECD
         ADD       BALANCE TO TOTAL
         MOVE      DOLLAR TO SUBRMASK
         EDIT      SUBRECD TO SUBRMASK
         MOVE      DOLLAR TO SUBAMASK
         EDIT      SUBAPP TO SUBAMASK
         MOVE      DOLLAR TO MTDRMASK
         EDIT      MTDRECD TO MTDRMASK
         MOVE      DOLLAR TO MTDAMASK
         EDIT      MTDAPP TO MTDAMASK
         MOVE      c0 TO MTDCHNG
         ADD       MTDAPP TO MTDCHNG
         SUB       MTDRECD FROM MTDCHNG
         MOVE      DOLLAR1 TO MTDTMASK
         EDIT      MTDCHNG TO MTDTMASK
         COMPARE   "59" TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *L,*L,*33,"TOTAL RECEIVED      : <",SUBRMASK,">"
         PRINT     *L,*33,"TOTAL  APPLIED      :  ",SUBAMASK
         add       c7 to lines
         compare   mtdchng to c0 
         if        less
         PRINT     *L,*L,*33,"Net Change          : <",MTDTMASK,">"
         else
         PRINT     *L,*L,*33,"Net Change          : ",MTDTMASK
         endif
         MULT      "-1" BY BALANCE
         cmatch    no to balrec
         if        equal
         call      nobalan
         goto      movez
         endif
.begin patch 1.4
         CALL      HEADER
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas1 TO MTDAMASK
         PRINT     *33,"Reason Code 1  :  ",cntreas1,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas2 TO MTDAMASK
         PRINT     *33,"Reason Code 2  :  ",cntreas2,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas3 TO MTDAMASK
         PRINT     *33,"Reason Code 3  :  ",cntreas3,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas4 TO MTDAMASK
         PRINT     *33,"Reason Code 4  :  ",cntreas4,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas5 TO MTDAMASK
         PRINT     *33,"Reason Code 5  :  ",cntreas5,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas6 TO MTDAMASK
         PRINT     *33,"Reason Code 6  :  ",cntreas6,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas7 TO MTDAMASK
         PRINT     *33,"Reason Code 7  :  ",cntreas7,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas8 TO MTDAMASK
         PRINT     *33,"Reason Code 8  :  ",cntreas8,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas9 TO MTDAMASK
         PRINT     *33,"Reason Code 9  :  ",cntreas9,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas10 TO MTDAMASK
         PRINT     *33,"Reason Code 10 :  ",cntreas10,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas11 TO MTDAMASK
         PRINT     *33,"Reason Code 11 :  ",cntreas11,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas12 TO MTDAMASK
         PRINT     *33,"Reason Code 12 :  ",cntreas12,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas13 TO MTDAMASK
         PRINT     *33,"Reason Code 13 :  ",cntreas13,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas14 TO MTDAMASK
         PRINT     *33,"Reason Code 14 :  ",cntreas14,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas15 TO MTDAMASK
         PRINT     *33,"Reason Code 15 :  ",cntreas15,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas16 TO MTDAMASK
         PRINT     *33,"Reason Code 16 :  ",cntreas16,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas17 TO MTDAMASK
         PRINT     *33,"Reason Code 17 :  ",cntreas17,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas18 TO MTDAMASK
         PRINT     *33,"Reason Code 18 :  ",cntreas18,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas19 TO MTDAMASK
         PRINT     *33,"Reason Code 19 :  ",cntreas19,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas20 TO MTDAMASK
         PRINT     *33,"Reason Code 20 :  ",cntreas20,b1,MTDAMASK
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas21 TO MTDAMASK
         PRINT     *33,"Reason Code 21 :  ",cntreas21,b1,MTDAMASK
.begin patch 1.51
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas22 TO MTDAMASK
         PRINT     *33,"Reason Code 22 :  ",cntreas22,b1,MTDAMASK
.end patch 1.51
.begin patch 1.52
         MOVE      reasonm TO MTDAMASK
         EDIT      totreas23 TO MTDAMASK
         PRINT     *33,"Reason Code 23 :  ",cntreas23,b1,MTDAMASK
.end patch 1.52

         MOVE      reasonm TO MTDAMASK
         EDIT      totreas99 TO MTDAMASK
         PRINT     *33,"Reason Code 99 :  ",cntreas99,b1,MTDAMASK
.end patch 1.4

.         COMPARE   XFOOT TO BALANCE
.         CALL      NOXFOOT IF NOT EQUAL
movez    MOVE      c0 TO XFOOT
         MOVE      c0 TO SUBRECD
         MOVE      c0 TO SUBAPP
         ADD       MTDAPP TO MTDTAPP
         ADD       MTDRECD TO MTDTRECD
         MOVE      c0 TO MTDAPP
         MOVE      c0 TO MTDRECD
         move      yes to balrec
         RETURN
NOXFOOT  COMPARE   "62" TO LINES
         CALL      HEADER IF NOT LESS
         PRINT     *N,*37,"++++++++++++++++++++++++++++++++++++":
                   *N,*37,"WARNING DOES NOT XFOOT. ":
                   *N,*37,"CALCULATED BALANCE = ",XFOOT:
                   *N,*37,"++++++++++++++++++++++++++++++++++++"
         add       c5 to lines
         RETURN
.
nobalan  COMPARE   "59" TO LINES
         call      header if not less
         PRINT     *N,*37,"++++++++++++++++++++++++++++++++++++":
                   *N,*37,"NO BALANCE RECORD FOUND= ":
                   *N,*37,"++++++++++++++++++++++++++++++++++++"
         add       c4 to lines
         RETURN
HEADER
         ADD       C1 TO PAGE
.         CALL      READMLR
         compare    c1 to page
         if         equal
         cmatch     true to lasrflag
         if         equal
         print      hpreset,hpcour,hp17ptch,hpdupl,*f
         else
         print      p2417cpi,*f
         endif
         endif
.begin patch 1.5
.         PRINT     *F,*32,"* * *   M O N E Y   O N    A C C O U N T   ":
.                   "CONTROL   J O U R N A L    * * * ":

         PRINT     *F,*1,CoTitle,*32,"* * *   M O N E Y   O N    A C C O U N T   ":
                   "CONTROL   J O U R N A L    * * * ":
.end patch 1.5
                   *L,*1,"Control ",ctrlnum:
                   *55,DD1,"   ",MONTH,"   ",YEAR,*121,"PAGE: ",PAGE,*L:
                   *l:
                   *1,"TRANSACTION",*15,"CONTROL",*24,"LIST",*34,"INVOICE":
                   *43,"  INVOICE",*57," AMOUNT",*77,"CHECK":
                   *120,"TRANSACTION",*95,"REASON/COMMENTS":
                   *L,*1,"  DATE",*15,"NUMBER",*24,"RENTAL ##",*34,"NUMBER":
                   *45,"DATE",*77,"NUMBER",*120,"NUMBER":
                   *L,*L
         MOVE      c7 TO LINES
         RETURN
DONE     PACK      NMOAFLD4 FROM samebrk,SAMEMlr
.         CALL      NMOBKEY
         CALL      PRINTOT
         splclose
         release
         shutdown  "cls"
         STOP
...............................................................................
.READ MAILER FILE
READMLR
         CALL      NMLRKEY
         CALL      NOMLR IF OVER
         RETURN
NOMLR    MOVE      "***NO MAILER FOUND***" TO MCOMP
         RETURN
.READ DATACARD FILE
READCARD
         CLEAR     OLSTNAME
         TYPE      LIST
         RETURN    IF NOT EQUAL
         MOVE      LIST TO NDATFLD
         MOVE      C1 TO NDATPATH
         CALL      NDATKEY
         RETURN
.patch2.45
                                        include   compio.inc
                                        include   cntio.inc
.         INCLUDE   NMLRIO.INC
.patch2.45
         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
.         include   nbrkio.inc
         INCLUDE   NDATIO.inc
         INCLUDE   COMLOGIC.inc

