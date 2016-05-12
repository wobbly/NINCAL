******************************
*    NOBILLED PRINT PROGRAM  *
******************************
. 
.CREATED BY DAVID HERRICK FOR NAMES IN THE NEWS CALIF.
. 
. 
PC       EQU       0
         INCLUDE   COMMON.INc
         inc       hp.inc
         INCLUDE   CONS.INC
.begin patch 1.7
          Include   PrtPagedd.inc
          include   winapi.inc
          include   Norddd.inc
.end patch 1.7
release  init      "1.81"        DLH use dsinit
Reldate   Init      "2016 May 2"
.release  init      "1.8"        DLH convert to Sunbelt PDF 
.Reldate   Init      "2013 April 23"
.release  init      "1.7"        DLH convert to PDF autosave and delete file
.Reldate   Init      "29 July 2011"
.release  init      "1.6"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.5"        DLH 7nov95 reason code 3. 
.release  init      "1.4"        DLH 17nov94 reason code 5. 
.RELEASE  INIT      "1.3"        JD  14JUL94 print to laser.
.RELEASE  INIT      "1.2"       DLH 03MAR92
. 
. 
***********************************
*    DEFINE FILES AND VARIABLES   *
***********************************
. 
. 
. 
*   MASTER FILE   *
. 
NOBILMST IFILE     KEYLEN=6,fixed=35
. 
.OLRN     FORM      6
.REASON   FORM      1
REASON   DIM       1
DATE     DIM       8
.OQTY     FORM      7
.OPPM     FORM      5
PASSNAME DIM       10 
. 
. 
. 
. 
. 
. 
. 
. 
SYSMO    DIM       2        TEMP FIX PRINT ONLY CURRENT MONTH DLH 28MAY93
TWO5     INIT      "                         "
TWO9     INIT      "                            "
SYSDATE  DIM       8
DETDATE  DIM       8
FORTY5  FORM      "45"
ONE      FORM      "1"
FOUR     FORM      "4"
ONETHOUS FORM      "1000"
DASH5    INIT      "-----"
DASH6    INIT      "------"
DASH7    INIT      "-------"
DASH8    INIT      "--------"
DASH9    INIT      "---------"
PAGECNTR FORM      "00"
LINECNTR FORM      2
PAGENUM  FORM      2
HEADATE  DIM       8
MESSAGE1 DIM       25
MESSOUT  DIM       25
WRITEOFF FORM      9.2
OQTYMASK DIM       11
TOTNMASK DIM       11
DOLLMASK DIM       14
QTYCNTR  FORM      9
ORDCNTR  FORM      3
TOTORD   FORM      3
TOTQTY   FORM      9
BTOTQTY  FORM      9
BCOUNT   FORM      3
LROUT    DIM       6
QTYOUT   FORM      9
DATEOUT  DIM       8
PRICEOUT DIM       5
NAMEOUT  DIM       10
c58      form      "58"
BLANKS   INIT      "      "
NUM1     INIT      "NO BILLING NECESSARY"
NUM2     INIT      "WRITE OFF"
NUM3     INIT      "Free Test"
NUM4     INIT      "BROKER EXCH NO SEL. CHARGED"
NUM5     INIT      "BROKER Rent->EXCH NO SEL.  "
HEADISP  INIT      "NIN NOBILL PRINT PROGRAM"
TOTLINE1 INIT      "TOTAL NUMBER OF ORDERS = "
TOTLINE2 INIT      "TOTAL NUMBER OF NAMES  = "
TOTLINE3 INIT      "TOTAL NUMBER OF BRK EXCH ORDERS = "
TOTLINE4 INIT      "TOTAL NUMBER OF BRK EXCH NAMES  = "
ESTIMATE INIT      "ESTIMATED DOLLARS WRITE OFF = "
. 
.begin patch 1.7
timestamp2          dim       16
userlogn  dim       7
font1          font
font2          font
font3          font
font4          font
font5          font
               create         font1,"FIXED",size=10
               create         font2,"FIXED",size=10,bold
               create         font3,"FIXED",size=10,bold,italic
               create         font4,"Arial",size=14,italic
.begin patch 1.8
.          CALL      GETpdfPATH
.end patch 1.8
.end patch 1.7

. 
. 
EXIT     TRAP      DONE IF F5
. 
         MATCH     "NEOM0009" TO PROGRAM   .CHAINED FROM DSINIT?
         if         equal
         MOVE      TODAY TO DATE
         Else  
         MOVE      "NEOM0009" TO PROGRAM
         CLOCK     DATE TO DATE
         ENDIF

         MOVE      "NIN" TO COMPNME
         MOVE      "EXIT" TO PF5
         MOVE      "NIN NOBILL PRINT " TO STITLE
.         DISPLAY   *ES,*P28:2,HEADISP
. 
         TRAP      BADTIME IF IO
.         CLOCK     DATE TO DATE
         TRAPCLR   IO
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         REP       ZFILL,MM
         REP       ZFILL,DD
         REP       ZFILL,YY
         PACK      SYSDATE FROM MM,SLASH,DD,SLASH,YY
         MOVE      MM TO SYSMO
         XIF
         IFZ       PC
         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         MOVE      DATE TO SYSDATE
         MOVE      MM TO SYSMO
         XIF
. 
         CALL      PAINT
         CALL      FUNCDISP
.begin patch 1.7
          pack      str55 from "\\nins1\e\data\text\nobilled.dat"
          pack      Taskname from "\\nins1\e\data\text\nobilled.",cc,yy,mm
          rep       Zfill,taskname
          Copyfile  str55,taskname
          if        not zero
.it failed send up error message and allow operator to correct and continue
          endif
.end patch 1.7
OPENMST  DISPLAY   *P1:23,*EL,"OPENING NOBILMST FILE",*W2
         TRAP      NOFILE IF IO
         OPEN      NOBILMST,"NOBILLED"
         TRAPCLR   IO
         DISPLAY   *P1:23,*EL,"NOBILMST OPENED",*W2
.
CHOOSE
.begin patch 1.7
.         KEYIN     *P25:14," (P)rint report, (K)ill file":
.                   " ",STR1;
.         CMATCH    "P" TO STR1
.         GOTO      BEGIN IF EQUAL
.         CMATCH    "K" TO STR1
.         GOTO      CHOOSE IF NOT EQUAL
.         KEYIN     *P1:24,"THIS WILL DELETE ALL RECORDS IN THE FILE OK??",STR1;
.         CMATCH    "Y" TO STR1
.         GOTO      DOWN IF NOT EQUAL
..         CLOSE     NOBILMST,DELETE
          Goto      Begin
.end patch 1.7
.
.begin patch 1.7

.         CLOSE     NOBILMST
..START PATCH 1.6 REPLACED LOGIC
..         PREPARE   NOBILMST,"g:\data\text\NOBILLED","g:\data\index\NOBILLED","6","35"
.         PACK      STR35,NTWKPATH1,"text\NOBILLED"
.         PACK      STR45,NTWKPATH1,"INDEX\NOBILLED"
..         PREPARE   NOBILMST,STR35,STR45,"6","35"
..END PATCH 1.6 REPLACED LOGIC
.         CLOSE     NOBILMST
..
.DOWN     STOP
.end patch 1.7
. ...................
.START PATCH 1.6 REPLACED LOGIC
.BEGIN    SPLOPEN   "g:\DATA\NOBILL.LST"
BEGIN    PACK      STR35,NTWKPATH1,"NOBILL.LST"
.begin patch 1.7
          Move      c1,Nordpath
          move      c3,Nordlock
          CALL      PDF995Auto
          
          pack      str25,"Nobill"
.begin patch 1.8
          pack      str55,"c:\work\pdf\",str25,".pdf"
.          PRTOPEN   Laser,"PDF995",str25
          PRTOPEN   Laser,"PDF:",str55
.          pack      str55,str25,".pdf"
.end patch 1.8

.         SPLOPEN   STR35
.end patch 1.7
.END PATCH 1.6 REPLACED LOGIC
         CALL      HEADINGS
.
LOOPER   READKS    NOBILMST;OLRN
.          READKS    NOBILMST;OLRN,REASON,MM,DD,YY,OQTY,OPPM,PASSNAME
         GOTO      TOTALS IF OVER
         ADD       C1 TO N4
         DISPLAY   *P10:16,"RECORDS PROCESSED: ",N4,B1,OLRN
.begin patch 1.7
.They are alwasy from this month
.         MATCH     MM TO SYSMO
.         GOTO      LOOPER IF NOT EQUAL       .DH 28MAY93
          packkey   Nordfld,Olrn
          call      Nordkey
.end patch 1.7
         GOTO      PROCESS
.
. 
. 
. 
*   REPORT HEADINGS   *
. 
. 
. 
HEADINGS MOVE      c0 TO LINECNTR
         ADD       c1 TO PAGENUM
         MOVE      SYSDATE,HEADATE
.begin patch 1.7
.         PRINT     hp17ptch,*F,*4,"CONFIDENTIAL",TWO9,"* * * NIN ORDERS MARKED ":
.                   *69,"BILLED BOOK * * * ",TWO5,"DATE ",HEADATE
.         PRINT     *112,"PAGE ",PAGENUM
.         PRINT     *N,*23,"DATE        LR##               REASON":
.                   *75,"ORD QTY     PRICE        NAME":
.                   *L,*22,DASH6,"      ",DASH5,"              ",DASH8:
.                   *58,"                ",DASH9,"     ",DASH7,"        ":
.                   *99,DASH6
.         PRINT     *N,*N
.         add       c8 to linecntr
          If (Pagenum = c1)
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0
          else
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0,*Newpage
          endif
          move      "1500",row
          prtpage   Laser;*p=2:row,"CONFIDENTIAL",*p2750:row,"* * * NIN ORDERS MARKED BILLED BOOK * * * ",*p6750:row,"DATE ",Headate
          add       eightlpi to row
          prtpage   Laser;*p6750:row,"PAGE ",PAGENUM
          add       eightlpi to row
          add       eightlpi to row
          add       eightlpi to row
          prtpage   Laser;*p=1500:row,"DATE",*p=2250:row,"LR##",*p=3250:row,"REASON":
                    *p4750:row,"ORD QTY",*p=5900:row,"PRICE",*p=6750:row,"NAME"
          add       eightlpi to row
          add       eightlpi to row
.end patch 1.7

         RETURN
. 
. 
. 
PROCESS  MOVE      "UNKNOWN" TO MESSAGE1
         MOVE      C0 TO N1
         MOVE      REASON TO N1
         LOAD      MESSAGE1 FROM N1 OF NUM1,NUM2,NUM3,NUM4,num5
         REP       ZFILL,MM
         REP       ZFILL,DD
         REP       ZFILL,YY
         PACK      DETDATE FROM MM,SLASH,DD,SLASH,YY
         ADD       ONE TO ORDCNTR
         MOVE      DETDATE TO DATEOUT
         MOVE      OLRN TO LROUT
         MOVE      MESSAGE1 TO MESSOUT
         MOVE      OQTY TO QTYOUT
         ADD       QTYout TO QTYCNTR
         MOVE      OPPM TO PRICEOUT
         MOVE      PASSNAME TO NAMEOUT
         MOVE      "ZZZ,ZZZ,ZZ9" TO OQTYMASK
         EDIT      QTYOUT TO OQTYMASK
         MATCH    "4" TO REASON
         CALL      CALCSEL IF EQUAL
.begin patch 1.7
.         compare   c58 to linecntr
.         call      headings if not less

          if        (row > 9500)
          call      Headings
          endif
          prtpage   Laser;*p=1250:row,DATEout,*p=2250:row,Lrout,*p=3205:row,messout:
                    *p4550:row,OQTYmask,*p=5900:row,PRICEout,*p=6750:row,NAMEout

          add       Eightlpi to row

.         PRINT     *21,DATEOUT,"     ",LROUT,"     ",MESSOUT,"     ":
.                   *75,OQTYMASK,"     ",PRICEOUT,"     ",NAMEOUT
.         add       c1 to linecntr
.end patch 1.7
         GOTO      LOOPER
. 
. 
. 
BADTIME  TRAPCLR   IO
         DISPLAY   *B,*P15:12,*EL,"CLOCK FILE NOT ON-LINE !!!!",*W5
. 
NOFILE   TRAPCLR   IO
         DISPLAY   *B,*P15:12,*EL,"FILE NAME NOT ON-LINE !!!",*W4
         NORETURN
         STOP
. 
. 
TOTALS   MOVE      ORDCNTR TO TOTORD
         MOVE      QTYCNTR TO TOTQTY
         MOVE      TOTQTY TO WRITEOFF
         DIVIDE    ONETHOUS INTO WRITEOFF
         MULT      FOUR BY WRITEOFF
         MOVE      "ZZZ,ZZZ,ZZ9" TO TOTNMASK
         EDIT      TOTQTY TO TOTNMASK
         MOVE      "$$$,$$$,$$9.Z9" TO DOLLMASK
         EDIT      WRITEOFF TO DOLLMASK
.begin patch 1.7
          if        (row > 9500)
          call      Headings
          endif
.         compare   c58 to linecntr
.         call      headings if not less
.         PRINT     *N
.         PRINT     *81,TOTLINE1,TOTORD
.         PRINT     *81,TOTLINE2,TOTNMASK
.         MOVE      "ZZZ,ZZZ,ZZ9" TO TOTNMASK
.         EDIT      BTOTQTY TO TOTNMASK
.         PRINT     *81,TOTLINE3,BCOUNT
.         PRINT     *81,TOTLINE4,TOTNMASK
.         PRINT     *81,ESTIMATE,*+,DOLLMASK
          add       Eightlpi to row
          prtpage   Laser;*p=81:row,TOTLINE1,TOTORD
          add       Eightlpi to row
          prtpage   Laser;*p=81:row,TOTLINE2,TOTNMASK
          add       Eightlpi to row
         MOVE      "ZZZ,ZZZ,ZZ9" TO TOTNMASK
         EDIT      BTOTQTY TO TOTNMASK
.          prtpage   Laser;*p=81:row,TOTLINE3,BCOUNT
.          add       Eightlpi to row
.          prtpage   Laser;*p=81:row,TOTLINE4,TOTNMASK
.          add       Eightlpi to row
          prtpage   Laser;*p=81:row,ESTIMATE,DOLLMASK

          prtclose  Laser

.         SPLCLOSE

.It takes some time for the file to be created, so we must check
          Move      "gspranz" to UserLogn
          move    C0,N9
          move    "                                        ",APIFileName
.begin patch 1.8

.          clear   APIFileName
.          pack    APIFileName,"C:\WORK\PDF\",str55,hexzero  ."
.          clock   timestamp,timestamp1
.          move    timestamp1,time1
.          loop
.                    clock   timestamp,timestamp2
.                    move    timestamp2,time2
.                    sub     time1,time2,time3
.                    if (time3 > 1000) .10 Seconds Maximum
.                              break
.                    endif
.          repeat
.end patch 1.8
          move      "Here is your No Billed Report",MailSubjct
          Clear     MailBody
          append    Str55,MailBody
          append    CRLF,MailBody
          reset     MailBody
          pack      MailTo from userlogn,"@nincal.com"
          pack      MailFrom from "Creques@nincal.com"
          pack      Mailcc from "Creques@nincal.com"
.begin patch 1.8
.          PAck      MailAttach from "c:\work\pdf\",str55              ."
          PAck      MailAttach from str55              
.end patch 1.8
          call      SendMail
.Clean up afterwards
.begin patch 1.8
.          CALL      PDF995Auto0
.end patch 1.8

.          return
.end patch 1.7
. 
. 
EOJ
.begin patch 1.7
         CLOSE     NOBILMST
.         display   *p2:23,*blinkon,"Please wait I'm PRINTING !!!!!",*blinkoff
.         execute "f:\public\NPRINT g:\DATA\nobill.LST Q=LASER2 NT NB f=0 S=NTS0_FPNW"
         PACK      STR35,NTWKPATH1,"text\NOBILLED"
         PACK      STR45,NTWKPATH1,"INDEX\NOBILLED"
         PREPARE   NOBILMST,STR35,STR45,"6","35"
         CLOSE     NOBILMST
.end patch 1.7

         DISPLAY   *P1:4,*Ef,*P10:12,"JOB DONE!!!!!!",*W2
         STOP
.
CALCSEL  ADD       ONE TO BCOUNT
         ADD       QTYOUT TO BTOTQTY
         RETURN
. 
. 
DONE      BEEP
         DISPLAY   *P1:1,*HON,*ES,"JOB ABORTED VIA F5 KEY, OH NO!!!!",*W3
         GOTO      EOJ
         INCLUDE   COMLOGIC.inc
.begin patch 1.7
          include nordio.inc
.end patch 1.7

