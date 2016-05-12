.WRITTEN 16MAY88
...............................................................................
.INPUT FILE SORTED FROM ONACCNTMONEY:
..
.SORTXA ONACCNTMONEY,ONACCOUNT/SORT;KEY="115-118,1-4,108-114"
.
.OUTPUT :    1. JOURNAL TO PRINTER
.            2. INDEX TO FILE 'PAGE03/TEXT:PRINT'
.
.PAGE03/TEXT GETS SORTED 30-54,5-29 AND PRINTED VIA PROGRAM NINP45B.
...............................................................................
PC        EQU       0
          INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   NMOADD.inc
          INCLUDE   NMOBDD.inc
.         include   nbrkdd.inc
.patch3.1
          include   compdd.inc
          include   cntdd.inc
.         INC       NMLRDD.INC
.patch3.1
          INCLUDE   NPGEDD.inc
          include   hp.inc
          INCLUDE   NDATDD.inc
          include   gnxtdd.inc
.Begin Patch 3.31
           Include        MOANotesDD.inc
.End Patch 3.31
          
.>Patch 3.3
          include nescdd.inc
        include winapi.inc    
.>Patch 3.3
.
release  init      "3.41"           DLH 
reldate   init      "2014 January 3"
.release  init      "3.40"           DLH Sunbelt PDF
.reldate   init      "2013 April 24"
.release  init      "3.34"           DLH Email output, move the pdf file within code instead of using winbatch
.reldate   init      "02 October 2012"
.release  init      "3.33"           DLH REas23
.reldate   init      "14 October 2010"
.release  init      "3.32"           DLH 24 June 2008  suppress detail print of records more that 1 year old
.reldate   init      "24 June 2008"
.release  init      "3.31"           DLH          5Nov2007  New reason code  new files structure
.release  init      "3.3"           DMB 21AUG2005 REmove use of pcl2pdf
.release  init      "3.2"           ASH 21DEC2004 PAGE.SRT Conversion
.release  init      "3.1"           JD  26MAY2004 Mailer Conversion
.release  init      "3.00"         29aug2003 JD added dsprog..
.release  init      "2.51"         13DEC2000 JD updated date print. use CC.
.release  init      "2.5"         02OCT2000 ASH NEW SERVER ADDED
.release  init      "2.4"         28Oct98 ASH NINMOA Y2K,File expansion
.release  init      "2.3"         15sep98 ASH NINMLR Y2K file expansion
.release  init      "2.2"         30apr98 JD cleaned up lines per page print.
.release  init      "2.1"        12aPR96 DLH ADD X-FOOT ON FINAL TOTALS
.release  init      "2.0"        02jun95 DLH use beg & end tran numbers for current flag.
.release  init      "1.9"        25jan95 DLH new reason codes.
.RELEASE  INIT      "1.81"       DLH 07JUL94 COMMENT PROBLEM.
.RELEASE  INIT     "1.8"        JD   28MAR94   print to laser.
.RELEASE  INIT     "1.7"        DLH  07MAY92   NMOAXX, NMOBXX
.RELEASE  INIT      "1.6"        DLH 12 MAR92   NEW INCLUDES CONVERT TO PCBUS
.RELEASE  INIT      "1.5"        D.L. HERRICK 20AUG91
.                               ADDED CONS.inc
.                               UPDATE DMLRIO,DDATIO.
.ACCOUNT1 IFILE     KEYL=7,FIXED=17
.ACCOUNT  FILE      FIXED=114
.PAGEFILE FILE
FileCheck FIle
trapcount form      4
ONE      FORM      "1"
TWO      FORM      "2"
ZERO     FORM      "0"
ANS      DIM       1
BR       FORM      2
DD1      DIM       2
HOLDMM   DIM       2           USED TO CHECK FOR CURRENT MONTH TRANSACTION.
HOLDYY   DIM       2           USED TO CHECK FOR CURRENT YEAR TRANSACTION.
.V        FORM      "07"         VERTICAL DISPLAY VARIABLE
V2       FORM      2           VERTICAL DISPLAY VARIABLE
V3       FORM      2           VERTICAL DISPLAY VARIABLE
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
balrec   dim       1
SELECT   FORM      2           BRANCH FOR REASON
SAMEMLR  DIM       4           USED TO VERIFY STILL READING CORRECT ACCOUNT
samebrk  dim       4           broker break
DATE     DIM       8
PAGE     FORM      4           PAGE NUMBER
LINES    FORM      2           LINE NUMBER
MONTH    DIM       10
YEAR     DIM       4
NINETEEN INIT      "19"
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
DOLLAR   INIT      "$$,$$$,$$$.99"
DOLLAR1  INIT      "$$$,$$$,$$$.99-"
AMNTAPP  DIM       13
AMNTRECD DIM       13
SUBAPP   FORM      8.2
SUBRECD  FORM      8.2
SUBAPPb  FORM      8.2
SUBRECDb FORM      8.2
MTDAPP   FORM      8.2
MTDRECD  FORM      8.2
MTDBAPP   FORM      8.2
MTDBRECD  FORM      8.2
MTDCHNG  FORM      9.2
MTDTAPP  FORM      8.2      M-T-D TOTAL APP
MTDTRECD FORM      8.2      M-T-D TOTAL REC'D
MTDTCHNG FORM      9.2      M-T-D TOTAL CHANGE
MTDTNINAPP  FORM      8.2      M-T-D TOTAL APP
MTDTNINRECD FORM      8.2      M-T-D TOTAL REC'D
MTDTPLIAPP  FORM      8.2      M-T-D TOTAL APP
MTDTPLIRECD FORM      8.2      M-T-D TOTAL REC'D
TOTAPP   FORM      9.2
TOTRECD  FORM      9.2
TOTAMASK DIM       14
TOTRMASK DIM       14
SUBAMASK DIM       13
SUBRMASK DIM       13
MTDRMASK DIM       13
MTDAMASK DIM       13
totlMASK DIM       15
MTDTMASK DIM       15
.Start patch #2.4 - expanded var
.TRANDTE   DIM      8
TRANDTE   DIM      10
.End patch #2.4 - expanded var
trancnt  form      5
tranNINcnt  form      5
tranPLIcnt  form      5
.Start Patch #2.4 - expanded var
.DINVDTE  DIM       8
DINVDTE  DIM       10
.End Patch #2.4 - expanded var
FIRSTPAS INIT      "Y"
TOTALB   FORM      9.2
TOTAL    FORM      9.2
XFOOT    FORM      9.2       USED TO VERF. DETAIL ENTRIES TO BALANCE
DETAIL$  FORM      8.2
TOTDOLL  INIT      "$$$,$$$,$$$.99"
TOTDOLL1 INIT      "$$$,$$$,$$$.99-"
COUNT    FORM      5
LBRACKET DIM       1      USED FOR '<'
RBRACKET DIM       1      USED FOR '>'
BEGNUM   FORM      7      *HOLD BEGINNING TRANSACTION NUMBER.
ENDNUM   FORM      7
.START PATCH 3.2 ADDED LOGIC
HoldMlrKey          dim       6
HoldBrkKey          dim       25
MNUM2     DIM       6         .Temporary var!!
.END PATCH 3.2 ADDED LOGIC
.>Patch 3.3
PRFILE    pfile
.Column Defs
Header1   form    9
Title1   form    9
Title2   form    9
Title3   form    9
.Column8  form    9
.Column9  form    9
.Column10 form    9
.Column11 form    9
Column4R  form    9
Column5R  form    9
Column6R form    9
Column7R form    9
Column8R form    9
Column9R form    9
Column10R form    9
.begin patch 3.32
OldFlag   Init      "N"                 .default is don't print old details
DateChk   DIm       8
.end patch 3.32

.Fonts
font8    font
        create  font8,"Times New Roman",size=8
font8i    font
        create  font8i,"Times New Roman",size=8,italic        
.Position of Columns
          move    "4250" to Header1
          move    "1400" to Title1      
          move    "5450" to Title2
          move    "4400" to Title3      
        move    "200",column
.        move    "575",column1
        move    "900",column1
.        move    "1000",column2
        move    "1600",column2
.        move    "1500",column3
        move    "2300",column3
.        move    "3200",column4
        move    "2500",column4
        move    "3600",column5
        move    "4400",column6
        move    "5400",column7
        move    "6250",column8
        move    "6800",column9
        move    "7300",column10
.        move    "8000",column11
        move    "3000",column4R
        move    "4400",column5R
        move    "5100",column6R
        move    "5800",column7R
        move    "7200",column8R
        move    "7200",column9R
        move    "7900",column10R
        
COMPCOMPKEY external "COMP001A;COMPCOMPKEY"
holda     dim       500       .length of Company record - used by independent routines to hold Company record
NOESCFLAG FORM      1
BrokeFlag FORM    1
.>Patch 3.3         
.>Patch 3.3 remove after test
.         move "dbmob" to NMOBNME1 
.         move "dbmob2" to NMOBNME2
.>Patch 3.3 remove after test
.
+..............................................................................
.MAIN
.         DISPLAY   *P1:1,*ES:
.                   *P23:1,"   M O N E Y    O N    A C C O U N T   ":
.                   "P R I N T"
.         MOVE      "NONA0002" TO PROGRAM
          MOVE      "Names in the News" TO COMPNME
          MOVE      "MONEY ON ACCOUNT PRINT" TO STITLE
.beginning patch 3.0
          MATCH     "NONA0002" TO PROGRAM         .ENTRY FROM DSINIT?
          IF NOT EQUAL                     . NO.
                    CLOCK     DATE TO DATE
                    CLOCK     DATE TO today
          endif
.ending patch 3.0
          trap      done if f5
          move      "ABORT" to pf5
          CALL      PAINT
          call      funcdisp
          move      "07" to v
.         IFNZ      PC
.         OPEN      NMOAFLE3,"ONACCOUNT/SORT"
.         PREPARE   PAGEFILE,"PAGE03:PRINT"
.         UNPACK    DATE INTO MM,DD1,YY
.         XIF
          IFZ PC
.beginning patch 3.0
                    MATCH     "NONA0002" TO PROGRAM         .ENTRY FROM DSINIT?
                    IF NOT EQUAL                     . NO.
                    pack      Taskname from "\\nins1\e\DATA\text\ninmoa.DAT,\\nins1\e\data\ninmoa.SRT -122-125,1-4,13-16,17-18,19-20"
                    Sort      Taskname
                              move      "NINMOA.SRT",INpname
                              OPEN      NMOAFLE3,INPname
                             UNPACK    DATE INTO MM,STR1,DD1,STR1,YY
.begin patch 3.32             
                    Clock     Timestamp,Datechk
.end patch 3.32               
.>patch 3.3                             
.Begin patch 3.40
                              move    "ninmoa.pdf",prtname
.>patch 3.3
                              pack      str55 from "c:\work\pdf\",prtname       ."
                              PACK      STR35,NTWKPATH1,"ninmoa.pdf"
.                             splopen   STR35
.
.>Patch 3.3 Logic Addition for PDF Quality Control
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.                              result
.                              if (result = C0)
..Prepare Flag file
.                                        pack      str45 from PDFPATH,"\flag.dat"
.                                        prep      tempfile,str45
.                                        write     tempfile,SEQ;"flag set"
.                                        close     tempfile
.                              endif
.>Patch 3.3 Logic Addition for PDF Quality Control                              
.                               PRTOPEN prfile,"PDF995",PRTNAME
                               PRTOPEN prfile,"PDF:",str55
.end patch 3.40
                               PRTPAGE prfile;*UNITS=*HIENGLISH:
                                             *ORIENT=*PORTRAIT:
                                                   *Duplex=2;                                                 
.>patch 3.3 End Addition

                              goto pager
                    endif
                    OPEN      NMOAFLE3,inpname
                    UNPACK    today INTO MM,STR1,DD1,STR1,YY
                    PACK      STR35,NTWKPATH1,prtname
.begin patch 3.32             
                    pack      DateCHK from CC,yy,mm,dd1
.end patch 3.32               
.>Patch 3.3 Comment Out                 
.                   splopen   STR35
.>Patch 3.3 Logic Addition for PDF Quality Control
.Begin patch 3.40
.        call       GetPDFPath
.                    pack      str45 from PDFPATH,"\res\pdf995.ini"
. 
.                   call      "GU$INI;WRITE_TO_INI" USING str45:
.                             "Parameters":
.                             "ProcessPDF":
.                             "\\nins1\e\apps\winbatch\Del995flag.exe":
.          result
.          if (result = C0)
..Prepare Flag file
.                    pack      str45 from PDFPATH,"\flag.dat"
.                    prep      tempfile,str45
.                    write     tempfile,SEQ;"flag set"
.                    close     tempfile
.          endif
.>Patch 3.3 Logic Addition for PDF Quality Control                              
.           PRTOPEN prfile,"PDF995",PRTNAME
          pack      str55 from "c:\work\pdf\",prtname  ."
           PRTOPEN prfile,"PDF:",str55
.end patch 3.40
           PRTPAGE prfile;*UNITS=*HIENGLISH:
                         *ORIENT=*PORTRAIT:
                               *Duplex=2;                                                 
.>patch 3.3 End Addition
.end patch 3.0
pager
                    CLEAR     NPGENAME
                    APPEND    "PAGE" TO NPGENAME
                    MOVE      "03" TO INDNUM
                    APPEND    INDNUM TO NPGENAME
                    RESET     NPGENAME
                    REP       ZFILL IN NPGENAME
                    clear     str45
                    pack      str45 from ntwkpath1,npgename
.START PATCH 3.2 REPLACED LOGIC
.         PREPARE   NPGEFILE,str45
.;         PREPARE   NPGEFILE,NPGENAME
.         move      c1 to npgeflag
                    PREPARE   NPGEFLE2,str45,"\\nins1\E\DATA\INDEX\page","3-8","66"
                    MOVE      C1 TO NPGEFLG2
.END PATCH 3.2 REPLACED LOGIC
.         PREPARE   PAGEFILE,"g:\DATA\PAGE03"
.         UNPACK    DATE INTO MM,STR1,DD1,STR1,YY
.START PATCH 2.5 REPLACED LOGIC
.         splopen   "g:\data\ninmoa.lst"
.         PACK      STR35,NTWKPATH1,"ninmoa.lst"
.         splopen   STR35
.END PATCH 2.5 REPLACED LOGIC
          XIF
          move      "NONALAST" TO GNXTFLD
          CALL      GNXTKEY
          MOVE      C0 TO BEGNUM
          MOVE      GNXTNUM TO BEGNUM
          move      "NONANXT" TO GNXTFLD
          CALL      GNXTKEY
          MOVE      C0 TO endNUM
          MOVE      GNXTNUM TO endNUM
          add       c1 to endnum
          MOVE      C3 TO NMOAPATH
          MOVE      C2 TO NMObPATH
          MOVE      C1 TO NMOAFLG3
.         OPEN      ACCOUNT1,"ONACCNTBALAN"
          PACK      YEAR FROM cc,YY
.         PACK      YEAR FROM NINETEEN,YY
          MOVE      MM TO NMM
          MOVE      MM TO HOLDMM
          MOVE      YY TO HOLDYY
          LOAD      MONTH FROM NMM OF JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT:
                    NOV,DEC
MAIN
          MOVE      NO TO OVER
          CALL      NMOASEQ
          GOTO DONE IF OVER
          ADD       C1 TO COUNT
          DISPLAY   *P10:12,"RECORDS PROCESSED : ",COUNT,"  ",TOTAL
          CLEAR     OLSTNAME
          PACK      MKEY FROM MLR,MCNT
          DISPLAY   *P1:13,*EL,"mlr = ",Mlr," ",mcomp,b1,samemlr:
                    *p1:14,*el,"brk = ",nmoabrk," ",brcomp,b1,samebrk
          cmatch    b1 to nmoabrk
          call      zfillbrk if equal
          cmatch    b1 to nmoabrk
          call      zfillbrk if eos
          match     nmoabrk to samebrk
          call      bbreak if not equal
          MATCH     Mlr TO SAMEMLR
          CALL      mBREAK IF NOT EQUAL
          CMATCH    " " TO LIST
.>Patch 3.3
.If func has been set to two then just include escrow clients.
          if (FUNC = "2")
                    GOTO MAIN IF (NOESCFLAG = 1)
          Endif     
.>Patch 3.3
          CALL      READCARD IF NOT EOS
          CALL      PRINT
          GOTO MAIN
mBREAK
          PACK      NMOAFLD4 FROM sAMEbrk,samemlr
          REP       ZFILL IN NMOAFLD4
          CALL      NMOBKEY
          cmatch    "Y" to over
          if equal
                    move      "0.00" to balance
                    move      no to balrec
          endif
.>Patch 3.3
          if (noescflag = 1)
                    call movez
                    goto NOMLRTOT
          endif
.>Patch 3.3
          CALL      PRINTOTM
.>Patch 3.3         
NOMLRTOT
          MOVE      mkey TO SAMEMLR
          CALL      READMLR
.>Patch 3.3         
          if (noescflag = 1)
                    noreturn  
                    goto main 
          endif
.>Patch 3.3         
.begin patch xxx
.          CALL      HEADER
          CALL      HEADER1
.begin patch xxx
          CALL      WRITPAGE
          RETURN
bBREAK

          CMATCH    YES TO FIRSTPAS
          GOTO FIRST IF EQUAL
          PACK      NMOAFLD4 FROM SAMEbrk,samemlr
          REP       ZFILL IN NMOAFLD4
          CALL      NMOBKEY
          if over
                    move      "0.00" to balance
          endif
.>Patch 3.3
          if (noescflag = 1)
                    if (brokeflag = 1)            
                              move c0 to brokeflag
                              goto NOTOTB
                              
                    else
                              call movez
                              call brkexit                  
                              goto NOTOTB1                  
                    endif
          else
                    move c0 to brokeflag
          endif
.>Patch 3.3         
          CALL      PRINTOTm
NOTOTB              
          CALL      PRINTOTB
.>Patch 3.3                   
NOTOTB1             
          MOVE      nmoabrk TO SAMEbrk
          pack      nbrkfld from nmoabrk,z3
          clear     brcomp
          CALL      nbrkkey
          MOVE      mkey TO SAMEMLR
          CALL      READMLR
.>Patch 3.3         
          if (noescflag = 1)
                    noreturn  
                    goto main 
          endif     
.>Patch 3.3         
.begin patch xxx
.          CALL      HEADER
          CALL      HEADER1
.begin patch xxx
          CALL      WRITPAGE
          RETURN
FIRST
          MOVE      NO TO FIRSTPAS
          MOVE      mkey TO SAMEMLR
          CALL      READMLR
          MOVE      nmoabrk TO SAMEbrk
          pack      nbrkfld from nmoabrk,z3
          clear     brcomp
          CALL      nbrkkey
.>Patch 3.3         
          goto main if (noescflag = 1)  
.>Patch 3.3         
          CALL      HEADER
          CALL      WRITPAGE
          RETURN
PRINT
.Start Patch #2.4 - remmed and replaced lines
.         UNPACK    INVDATE INTO MM,DD,YY
.         PACK      DINVDTE FROM MM,SLASH,DD,SLASH,YY
          UNPACK    INVDATE INTO str2,YY,MM,DD
          PACK      DINVDTE FROM MM,SLASH,DD,SLASH,STR2,YY
.End Patch #2.4 - remmed and replaced lines
          CLEAR     TRANDTE
          TYPE      TRANDATE
          CALL      EDTEAPP IF EQUAL
          CLEAR     AMNTAPP
          CLEAR     AMNTRECD
.patch 3.3 Comment Out        
          CLEAR     LBRACKET
          CLEAR     RBRACKET
.patch 3.3 Comment Out
.patch 3.3 Replace logic
          move b1 to LBRACKET
          move b1 to RBRACKET
.patch 3.3 Replace logic
          ADD       ONAMOUNT TO XFOOT
          COMPARE   ZERO TO ONAMOUNT
          GOTO EDITRECD IF LESS
          GOTO EDITAPP
PRINT1
          CLEAR     RDESC
          COMPARE   "99" TO REASON
          CALL      REASON IF EQUAL
          LOAD      RDESC FROM REASON OF REAS1,REAS2,REAS3,REAS4,REAS5:
                    REAS6,REAS7,REAS8,REAS9,REAS10,REAS11,REAS12,REAS13,REAS14:
.Begin Patch 3.31
.                   REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21
                    REAS15,REAS16,REAS17,REAS18,REAS19,REAS20,REAS21,REas22,REAS23
.end Patch 3.31
            PackKey        MOANotesFld,transnum
            call           MOANoteskey
          if             Not over
          MOve        "there are notes",str25
          else      
          clear     str25
          endif

.>Patch 3.3         Logic Added
          COMPARE  "9900" to ROW
.>Patch 3.3                             
.>Patch 3.3         Comment Out
.         COMPARE   "56" TO LINES
.>Patch 3.3         Comment Out         
          call      header if not less
.>Patch 3.3         Comment Out         
.         PRINT     *1,TRANDTE,*17,CONTROL,*24,LRNUM,*34,INVOICE:
.                   *44,DINVDTE,*75,CHECKNUM,*FLUSH;
.         PRINT     *56,LBRACKET,*57,AMNTRECD,RBRACKET:
.                   *95,RDESC,*120,TRANSNUM:
.                   *L,*10,LIST," ",OLSTNAME,*95,ONACOM
.begin patch 3.32
          move      Datechk to N9
          MOve      TranDate to N8
          call      debug
          If        (Oldflag = "N" & N8+365 <= N9)
.Don't print it
          Else
.end patch 3.32
.>Patch 3.3         Comment Out         
.>Patch 3.3         Logic Updated to Prt Page
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,TRANDTE;           
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,CONTROL;          
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,LRNUM;              
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,INVOICE;                      
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,DINVDTE;           
          pack str30 with LBRACKET,AMNTRECD,RBRACKET
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,str30;                                
.         prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,LBRACKET,AMNTRECD,RBRACKET;                               
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Left,*ll,CHECKNUM;                    
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,RDESC;                       
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Left,*ll,TRANSNUM;          
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,LIST," ",OLSTNAME;           
.begin patch 3.31
.         prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,ONACOM;            
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,STR25;             
.end patch 3.31
        add     eightlpi,row            
.>Patch 3.3         Logic Updated to Prt Page     
          ADD       c2 TO LINES
.begin patch 3.32
          Endif
.end patch 3.32
          RETURN
REASON
          MOVE      "ENTRY CORRECTION" TO RDESC
          RETURN
EDITAPP
          MOVE      DOLLAR TO AMNTRECD
          MOVE      ONAMOUNT TO DETAIL$
          ADD       ONAMOUNT TO SUBAPP
          ADD       ONAMOUNT TO SUBAPPB
          ADD       ONAMOUNT TO TOTAPP
          EDIT      DETAIL$ TO AMNTRECD
.         unpack    recdate into mm,dd,yy
.         call      datetest
.         compare   c0 to dateflag
.         if        not equal
.         clear      mm
.         clear      dd
.         clear      yy
.         unpack    trandate into mm,dd,yy
.         endif
.         MATCH     MM TO HOLDMM        *CURRENT MONTH TRANS?
.         GOTO      PRINT1 IF NOT EQUAL   *NO.
.         MATCH     YY TO HOLDYY        *CURRENT YEAR?
.         GOTO      PRINT1 IF NOT EQUAL     *NO.
          move      c0 to n7
          move      transnum to n7
          compare   n7 to begnum
          goto print1 if not less
          compare   endnum to n7
          goto print1 if not less
          add       c1 to trancnt        *current trans
          ADD       ONAMOUNT TO MTDAPP
.begin patch 3.31   
          if        (MoaCOmp = "P")
          ADD       ONAMOUNT TO MTDTPLIAPP
          add       c1 to tranPLIcnt        *current trans
          else
          ADD       ONAMOUNT TO MTDTNINAPP
          add       c1 to tranNINcnt        *current trans
          endif
.end patch 3.31     
          add       onamount to mtdBapp
          GOTO PRINT1
EDITRECD
          MOVE      DOLLAR TO AMNTRECD
          MULT      "-1" BY ONAMOUNT
          MOVE      ONAMOUNT TO DETAIL$
          ADD       ONAMOUNT TO SUBRECD
          ADD       ONAMOUNT TO SUBRECDB
          ADD       ONAMOUNT TO TOTRECD
          EDIT      DETAIL$ TO AMNTRECD
          MOVE      "<" TO LBRACKET
          MOVE      ">" TO RBRACKET
.         MATCH     MM TO HOLDMM        *CURRENT MONTH TRANS?
.         GOTO      PRINT1 IF NOT EQUAL   *NO.
.         MATCH     YY TO HOLDYY        *CURRENT YEAR?
.         GOTO      PRINT1 IF NOT EQUAL     *NO.
          move      transnum to n7
          compare   n7 to begnum
          goto print1 if not less
          compare   endnum to n7
          goto print1 if not less
          add       c1 to trancnt        *current trans
          ADD       ONAMOUNT TO MTDRECD
          ADD       ONAMOUNT TO MTDBRECD
.begin patch 3.31   
          if        (MoaCOmp = "P")
          ADD       ONAMOUNT TO MTDTPLIRECD
          else
          ADD       ONAMOUNT TO MTDTNINRECD
          endif
.end patch 3.31     
          GOTO PRINT1
EDTEAPP
.Start Patch #2.4 - remmed and replaced lines
.         UNPACK    TRANDATE INTO MM,DD,YY
.         PACK      TRANDTE FROM MM,SLASH,DD,SLASH,YY
          UNPACK    TRANDATE INTO str2,YY,MM,DD
          PACK      TRANDTE FROM MM,SLASH,DD,SLASH,STR2,YY
.End Patch #2.4 - remmed and replaced lines
          RETURN
.client / mailer totals
PRINTOTM
          MOVE      DOLLAR TO AMNTRECD
.>patch 3.3 Comment Out
          MULT      "-1" BY BALANCE
          MOVE      BALANCE TO DETAIL$
          EDIT      DETAIL$ TO AMNTRECD
          ADD       BALANCE TO TOTAL
.>patch 3.3 Comment Out       
.>Patch 3.3 Logic Replaced
.         if (FUNC <> "2")
.                   MULT      "-1" BY BALANCE
.                   ADD       BALANCE TO TOTAL
.         endif     
.         MOVE      BALANCE TO DETAIL$
.         EDIT      DETAIL$ TO AMNTRECD
.>Patch 3.3 Logic Replaced
          MOVE      DOLLAR TO SUBRMASK
          EDIT      SUBRECD TO SUBRMASK
          MOVE      DOLLAR TO SUBAMASK
          EDIT      SUBAPP TO SUBAMASK
          MOVE      DOLLAR TO MTDRMASK
          EDIT      MTDRECD TO MTDRMASK
          MOVE      DOLLAR TO MTDAMASK
          EDIT      MTDAPP TO MTDAMASK
          MOVE      ZERO TO MTDCHNG
          ADD       MTDAPP TO MTDCHNG
          SUB       MTDRECD FROM MTDCHNG
          MOVE      DOLLAR1 TO MTDTMASK
          EDIT      MTDCHNG TO MTDTMASK
.>Patch 3.3         Logic Added
          COMPARE  "9900" to ROW
.>Patch 3.3                             
.>Patch 3.3         Comment Out
.         COMPARE   "56" TO LINES
.>Patch 3.3         Comment Out         
          call      header if not less
.>Patch 3.3         Comment Out         
.         print     *l,*l,*33,"TOTAL RECEIVED      : <",SUBRMASK,">":
.                   *74,"M-T-D RECEIVED   : <",MTDRMASK,">"
.         PRINT     *33,"TOTAL  APPLIED      :  ",SUBAMASK:
.                   *74,"M-T-D APPLIED    :  ",MTDAMASK
.         PRINT     *33,"TOTAL  ON  ACCOUNT  : <",AMNTRECD,">":
.                   *74,"M-T-D CHANGE     : ",MTDTMASK
.>Patch 3.3         Comment Out
.>Patch 3.3 Logic Replaced Prt Page
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"TOTAL RECEIVED      : <",SUBRMASK,">",*ULOFF,*boldoff;           
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"M-T-D RECEIVED   : <",MTDRMASK,">",*ULOFF,*boldoff;              
        add     eightlpi,row          
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"TOTAL  APPLIED      :  ",SUBAMASK,*ULOFF,*boldoff;               
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"M-T-D APPLIED    :  ",MTDAMASK,*ULOFF,*boldoff;                  
        add     eightlpi,row          
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"TOTAL  ON  ACCOUNT  : <",AMNTRECD,">",*ULOFF,*boldoff;           
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"M-T-D CHANGE     : ",MTDTMASK,*ULOFF,*boldoff;                 
        add     eightlpi,row  
        add     eightlpi,row          
.>Patch 3.3 Logic Replaced Prt Page
          add       c4 to lines
          cmatch    no to balrec
          if equal
                    call      nobalan
                    goto movez
          endif
          MULT      "-1" BY BALANCE
          COMPARE   XFOOT TO BALANCE
          CALL      NOXFOOT IF NOT EQUAL
movez
          MOVE      ZERO TO XFOOT
          MOVE      ZERO TO SUBRECD
          MOVE      ZERO TO SUBAPP
          ADD       MTDAPP TO MTDTAPP
          ADD       MTDRECD TO MTDTRECD
          MOVE      ZERO TO MTDAPP
          MOVE      ZERO TO MTDRECD
          move      yes to balrec
          RETURN
NOXFOOT
.>Patch 3.3         Logic Added
          COMPARE  "9900" to ROW
.>Patch 3.3                             
.>Patch 3.3         Comment Out
.         COMPARE   "56" TO LINES
.>Patch 3.3         Comment Out         
          call      header if not less
.>Patch 3.3         Comment Out         
.         PRINT     *N,*37,"++++++++++++++++++++++++++++++++++++":
.                   *N,*37,"WARNING DOES NOT XFOOT. ":
.                   *N,*37,"CALCULATED BALANCE = ",XFOOT:
.                   *N,*37,"++++++++++++++++++++++++++++++++++++"
.>Patch 3.3         Comment Out         
.>Patch 3.3 Logic Added
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"++++++++++++++++++++++++++++++++++++",*ULOFF,*boldoff;           
        add     eightlpi,row  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"WARNING DOES NOT XFOOT. ",*ULOFF,*boldoff;             
        add     eightlpi,row  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"CALCULATED BALANCE = ",XFOOT,*ULOFF,*boldoff;                              
        add     eightlpi,row  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"++++++++++++++++++++++++++++++++++++",*ULOFF,*boldoff;                     
        add     eightlpi,row  
.>Patch 3.3
          add       c5 to lines
          RETURN
nobalan
.>Patch 3.3         Logic Added
          COMPARE  "9900" to ROW
.>Patch 3.3         Logic Added                   
.>Patch 3.3         Comment Out
.         COMPARE   "56" TO LINES
.>Patch 3.3         Comment Out         
          call      header if not less
.>Patch 3.3         Comment Out                   
.         PRINT     *N,*37,"++++++++++++++++++++++++++++++++++++":
.                   *N,*37,"NO BALANCE RECORD FOUND= ":
.                   *N,*37,"++++++++++++++++++++++++++++++++++++"
.>Patch 3.3         Comment Out         
.>Patch 3.3 Logic Added
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"++++++++++++++++++++++++++++++++++++",*ULOFF,*boldoff;           
        add     eightlpi,row  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"NO BALANCE RECORD FOUND= ",*ULOFF,*boldoff;                      
        add     eightlpi,row  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ULON,*boldon,*ll,"++++++++++++++++++++++++++++++++++++",*ULOFF,*boldoff;                     
        add     eightlpi,row  
.>Patch 3.3                   
          add       c4 to lines
          RETURN
.broker totals
PRINTOTb
          MOVE      DOLLAR TO MTDRMASK
          EDIT      MTDBRECD TO MTDRMASK
          MOVE      DOLLAR TO MTDAMASK
          EDIT      MTDBAPP TO MTDAMASK
          MOVE      ZERO TO MTDCHNG
          ADD       MTDBAPP TO MTDCHNG
          SUB       MTDBRECD FROM MTDCHNG
          MOVE      DOLLAR1 TO MTDTMASK
          EDIT      MTDCHNG TO MTDTMASK
          MOVE      DOLLAR TO SUBRMASK
          EDIT      SUBRECDB TO SUBRMASK
          MOVE      DOLLAR TO SUBAMASK
          EDIT      SUBAPPB TO SUBAMASK
          move      c0 to totalb
          ADD       subAPPb TO totalb
          SUB       subrecdb FROM totalb
          MOVE      totalb TO DETAIL$
          MOVE      DOLLAR TO  amntrecd
          EDIT      DETAIL$ TO AMNTRECD
          match     "0000" to samebrk
          goto brkexit if equal
.>Patch 3.3         Comment Out
.         COMPARE   "56" TO LINES
.>Patch 3.3         Comment Out
.>Patch 3.3         Logic Added
          COMPARE  "9900" to ROW
.>Patch 3.3                   
          call      header if not less

.>Patch 3.3         Comment Out
.         PRINT     *L,*33,"Totals for : ",brcomp:
.                   *l,*33,"TOTAL RECEIVED      : <",SUBRMASK,">":
.                   *74,"M-T-D RECEIVED   : <",MTDRMASK,">"
.         PRINT     *33,"TOTAL  APPLIED      :  ",SUBAMASK:
.                   *74,"M-T-D APPLIED    :  ",MTDAMASK
.         PRINT     *33,"TOTAL  ON  ACCOUNT  : <",AMNTRECD,">":
.                   *74,"M-T-D CHANGE     : ",MTDTMASK
.>Patch 3.3         Comment Out                   
.>Patch 3.3 Logic Replaced Prt Page
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*boldon,*ll,"Totals for : ",brcomp;                           
        add     eightlpi,row                  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"TOTAL RECEIVED      : <",SUBRMASK,">";           
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D RECEIVED   : <",MTDRMASK,">";              
        add     eightlpi,row          
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"TOTAL  APPLIED      :  ",SUBAMASK;               
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D APPLIED    :  ",MTDAMASK;                  
        add     eightlpi,row          
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"TOTAL  ON  ACCOUNT  : <",AMNTRECD,">";           
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D CHANGE     : ",MTDTMASK,*boldoff;                  
        add     eightlpi,row  
        add     eightlpi,row          
.>Patch 3.3 Logic Replaced Prt Page
          add       c5 to lines
brkexit
          MOVE      ZERO TO MTDBAPP
          MOVE      ZERO TO MTDBrecd
          MOVE      ZERO TO subrecdb
          MOVE      ZERO TO subappb
          MOVE      ZERO TO totalb
          RETURN
HEADER
          ADD       C1 TO PAGE
.         CALL      READMLR
          compare   c1 to page
.>Patch 3.3 Comment Out       
.         if equal
.>Patch 3.3 Comment Out       
.         print      hpreset,hpcour,hp17ptch,hpdupl,*f
.>Patch 3.3 Comment Out
.                   print     hpreset,hptmsr17,hpdupl,*f
.>Patch 3.3 Comment Out                 
.>Patch 3.3 
          if not equal
                    PRTPAGE prfile;*NEWPAGE:
                    *UNITS=*HIENGLISH:
                *ORIENT=*PORTRAIT:
                    *Duplex=2               
.>Patch 3.3
          endif
.Start Patch #2.3 - remmed and replaced line
.only change is placement of variable printed after MCOMP!!!!!
.         PRINT     *F,*35,"* * *   M O N E Y   O N    A C C O U N T   ":
.                   "J O U R N A L    * * * ":
.                   *L,*55,DD1,"   ",MONTH,"   ",YEAR,*121,"PAGE: ",PAGE,*L:
.                   *L,*1,"MLR/BRK ",MNUM,dash,nmoabrk," ",*19,MCOMP:
.                   "  ",*47,brcomp,*L,*19,maddr,*47,braddr,*L,*19,mcity,b1,mstate:
.                   b1,mzip,*47,brcity,b1,brstate,b1,brzip,*l:
.                   *1,"TRANSACTION",*15,"CONTROL",*24,"LIST",*34,"INVOICE":
.                   *43,"  INVOICE",*57," AMOUNT",*77,"CHECK":
.                   *120,"TRANSACTION",*95,"REASON/COMMENTS":
.                   *L,*1,"  DATE",*15,"NUMBER",*24,"RENTAL ##",*34,"NUMBER":
.                   *45,"DATE",*77,"NUMBER",*120,"NUMBER":
.                   *L,*L
.>Patch 3.3 Comment Out       
.         PRINT     *F,*35,"* * *   M O N E Y   O N    A C C O U N T   ":
.                   "J O U R N A L    * * * ":
.                   *L,*55,DD1,"   ",MONTH,"   ",YEAR,*121,"PAGE: ",PAGE,*L:
.                   *L,*1,"MLR/BRK ",MNUM,dash,nmoabrk," ",*19,MCOMP:
.                   "  ",*67,brcomp,*L,*19,maddr,*47,braddr,*L,*19,mcity,b1,mstate:
.                   b1,mzip,*47,brcity,b1,brstate,b1,brzip,*l:
.                   *1,"TRANSACTION",*15,"CONTROL",*24,"LIST",*34,"INVOICE":
.                   *43,"  INVOICE",*57," AMOUNT",*77,"CHECK":
.                   *120,"TRANSACTION",*95,"REASON/COMMENTS":
.                   *L,*1,"  DATE",*15,"NUMBER",*24,"RENTAL ##",*34,"NUMBER":
.                   *45,"DATE",*77,"NUMBER",*120,"NUMBER":
.                   *L,*L
.>Patch 3.3 Comment Out                           
.>Patch 3.3 Prtpage Header
          clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
.        prtpage prfile;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
.        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"NIN Money On Account Journal";         
        add     eightlpi,row        
        add     eightlpi,row        
        pack str30 with DD1,"   ",MONTH,"   ",YEAR
.        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,DD1,"   ",MONTH,"   ",YEAR;                  
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,str30;          
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
.begin patch xxx          
          Call      Header1
.        add     eightlpi,row        
.        add     eightlpi,row  
.          prtpage prfile;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"MLR/BRK ",MNUM,dash,NMOABRK," ";                      
.          prtpage prfile;*pTitle1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,MCOMP;                       
.          prtpage prfile;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,BRCOMP;         
.        add     eightlpi,row  
.          prtpage prfile;*pTitle1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,MADDR;                       
.          prtpage prfile;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,BRADDR;                 
.        add     eightlpi,row  
.          prtpage prfile;*pTitle1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,mcity,b1,mstate,b1,mzip;                         
.          prtpage prfile;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,brcity,b1,brstate,b1,brzip,*boldoff;                       
.        add     eightlpi,row          
.          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,*boldon,"Transaction";                 
.          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,"Control";                  
.          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,"List";             
.          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"Invoice";                              
.          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,"Invoice";                   
.          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Left,*ll,"Check";           
.          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Left,*ll,"Transaction";             
.        add     eightlpi,row            
.          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*ll,"Date";                
.          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,"Number";         
.          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,"Rental##";                   
.          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"Number";                     
.          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,"Date";            
.          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"Amount";                                       
.          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Left,*ll,"Number";                    
.          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,"Reason/Comments";                     
.          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Left,*ll,"Number",*ULOFF,*boldoff;            
.        add     eightlpi,row   
.        add     eightlpi,row       
.end patch xxx          

.>Patch 3.3 Prtpage Header End
.End Patch #2.3 - remmed and replaced line
          MOVE      c10 TO LINES
          RETURN
.begin patch xxx
Header1
          COMPARE  "8500" to ROW
          call      header if not less

        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"MLR/BRK ",MNUM,dash,NMOABRK," ";                      
          prtpage prfile;*pTitle1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,MCOMP;                       
          prtpage prfile;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,BRCOMP;         
        add     eightlpi,row  
          prtpage prfile;*pTitle1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,MADDR;                       
          prtpage prfile;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,BRADDR;                 
        add     eightlpi,row  
          prtpage prfile;*pTitle1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,mcity,b1,mstate,b1,mzip;                         
          prtpage prfile;*pTitle2:row,*ALIGNMENT=*LEFT,*font=font8,*ll,brcity,b1,brstate,b1,brzip,*boldoff;                       
        add     eightlpi,row          
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ll,*boldon,"Transaction";                 
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,"Control";                  
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,"List";             
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"Invoice";                              
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,"Invoice";                   
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Left,*ll,"Check";           
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Left,*ll,"Transaction";             
        add     eightlpi,row            
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*ULON,*ll,"Date";                
          prtpage prfile;*pColumn1:row,*ALIGNMENT=*Left,*ll,"Number";         
          prtpage prfile;*pColumn2:row,*ALIGNMENT=*Left,*ll,"Rental##";                   
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"Number";                     
          prtpage prfile;*pColumn4R:row,*ALIGNMENT=*Left,*ll,"Date";            
          prtpage prfile;*pColumn5R:row,*ALIGNMENT=*Right,*ll,"Amount";                                       
          prtpage prfile;*pColumn6R:row,*ALIGNMENT=*Left,*ll,"Number";                    
          prtpage prfile;*pColumn7R:row,*ALIGNMENT=*Left,*ll,"Reason/Comments";                     
          prtpage prfile;*pColumn8R:row,*ALIGNMENT=*Left,*ll,"Number",*ULOFF,*boldoff;            
        add     eightlpi,row   
        add     eightlpi,row       
          REturn
.end patch xxx


LASTPAGE
          ADD       C1 TO PAGE
          MOVE      TOTDOLL TO TOTAMASK
          MOVE      TOTDOLL TO TOTRMASK
          MOVE      TOTDOLL1 TO TOTlMASK
          EDIT      TOTAL TO TOTlmask
          EDIT      TOTAPP TO TOTAMASK
          EDIT      TOTRECD TO TOTRMASK
          MOVE      C0 TO XFOOT
          MOVE      TOTRECD TO XFOOT
          SUB       TOTAPP FROM XFOOT
          MOVE      DOLLAR TO MTDRMASK
          EDIT      MTDTRECD TO MTDRMASK
          MOVE      DOLLAR TO MTDAMASK
          EDIT      MTDTAPP TO MTDAMASK
          MOVE      ZERO TO MTDCHNG
          ADD       MTDTAPP TO MTDCHNG
          SUB       MTDTRECD FROM MTDCHNG
          MOVE      DOLLAR1 TO MTDTMASK
          EDIT      MTDCHNG TO MTDTMASK
          add       c1 to begnum
          sub       c1 from endnum
.>Patch 3.3 Comment Out       
.         PRINT     *F,*35,"* * *   M O N E Y   O N    A C C O U N T   ":
.                   "J O U R N A L    * * * ":
.                   *L,*48,DD1,"   ",MONTH,"   ",YEAR,*121,"PAGE: ",PAGE
.         PRINT     *L,*l,"Number of New transactions : ",trancnt:
.                   *L,"Range of New transactions : ",begnum,"-",endnum
.         PRINT     *L,*L,*33,"TOTAL RECEIVED   : <",TOTRMASK,">":
.                   *74,"M-T-D RECEIVED   : <",MTDRMASK,">"
.         PRINT     *L,*33,"TOTAL APPLIED    :  ",TOTAMASK:
.                   *74,"M-T-D APPLIED    :  ",MTDAMASK
.         PRINT     *L,*L,*33,"TOTAL ON ACCOUNT : ","<",TOTlmask,">":
.                   *74,"M-T-D CHANGE     : ",MTDTMASK;
.         PRINT     *FLUSH,*33,"TOTAL ON ACCOUNT : ","<",TOTLmask,">"
.         add       c10 to lines
.>Patch 3.3 Comment Out       
.>Patch 3.3
          PRTPAGE prfile;*NEWPAGE:
                              *UNITS=*HIENGLISH:
                          *ORIENT=*PORTRAIT:
                              *Duplex=2    
          clear     row
        move      "200",row
        prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*font=font8i,"Confidential";
.        prtpage prfile;*pcolumn9:row,*ALIGNMENT=*LEFT,*font=font8,"Date:";
.        clock   timestamp,str8
.        unpack  str8,str2,yy,mm,dd
.        clear   str10
.        pack    str10,mm,slash,dd,slash,str2,yy
.        prtpage prfile;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
.        prtpage prfile;*font=font8,str10;
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"NIN Money On Account Journal",*boldoff;          
        add     eightlpi,row        
        add     eightlpi,row        
        pack str30 with DD1,"   ",MONTH,"   ",YEAR
        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,str30;                  
.        prtpage prfile;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,DD1,"   ",MONTH,"   ",YEAR,*boldoff;           
        prtpage prfile;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                  
        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Number Of New Transactions: ",trancnt;            
.         call trim using endnum        
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"Range of New transactions : ",begnum,"-",endnum;           
        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"TOTAL RECEIVED   : <",TOTRMASK,">";              
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D RECEIVED   : <",MTDRMASK,">";              
        add     eightlpi,row  
        add     eightlpi,row          
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"TOTAL APPLIED    :  ",TOTAMASK;                  
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D APPLIED    :  ",MTDAMASK;                  
        add     eightlpi,row  
        add     eightlpi,row          
          prtpage prfile;*pColumn3:row,*ALIGNMENT=*Left,*ll,"TOTAL ON ACCOUNT : ","<",TOTlmask,">";           
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D CHANGE     : ",MTDTMASK,*boldoff;                              
        add     eightlpi,row  
        add     eightlpi,row            
.>Patch 3.3 
          COMPARE   TOTAL TO XFOOT
          CALL      NOXFOOT IF NOT EQUAL
.begin patch 3.31
          MOVE      TOTDOLL TO TOTAMASK
          MOVE      TOTDOLL TO TOTRMASK
          MOVE      DOLLAR TO MTDRMASK
          EDIT      MTDTNINRECD TO MTDRMASK
          MOVE      DOLLAR TO MTDAMASK
          EDIT      MTDTNINAPP TO MTDAMASK
          MOVE      ZERO TO MTDCHNG
          ADD       MTDTNINAPP TO MTDCHNG
          SUB       MTDTNINRECD FROM MTDCHNG
          MOVE      DOLLAR1 TO MTDTMASK
          EDIT      MTDCHNG TO MTDTMASK
        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Number Of New NIN Transactions: ",tranNINcnt;               
        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D RECEIVED   : <",MTDRMASK,">";              
        add     eightlpi,row  
        add     eightlpi,row          
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D APPLIED    :  ",MTDAMASK;                  
        add     eightlpi,row  
        add     eightlpi,row          
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D CHANGE     : ",MTDTMASK,*boldoff;                              
        add     eightlpi,row        
        add     eightlpi,row  
          MOVE      TOTDOLL TO TOTAMASK
          MOVE      DOLLAR TO MTDRMASK
          EDIT      MTDTPLiRECD TO MTDRMASK
          MOVE      DOLLAR TO MTDAMASK
          EDIT      MTDTPLIAPP TO MTDAMASK
          MOVE      ZERO TO MTDCHNG
          ADD       MTDTPLIAPP TO MTDCHNG
          SUB       MTDTPLiRECD FROM MTDCHNG
          MOVE      DOLLAR1 TO MTDTMASK
          EDIT      MTDCHNG TO MTDTMASK
        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pColumn:row,*ALIGNMENT=*Left,*boldon,*ll,"Number Of New PLi Transactions: ",tranPLicnt;               
        add     eightlpi,row        
        add     eightlpi,row  
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D RECEIVED   : <",MTDRMASK,">";              
        add     eightlpi,row  
        add     eightlpi,row          
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D APPLIED    :  ",MTDAMASK;                  
        add     eightlpi,row  
        add     eightlpi,row          
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"M-T-D CHANGE     : ",MTDTMASK,*boldoff;                              
.end patch 3.31
.Begin patch 3.32
          if        (OldFlag = "N")
          add     eightlpi,row  
          add     eightlpi,row          
          prtpage prfile;*pColumn6:row,*ALIGNMENT=*Left,*ll,"**Details records older that one year NOT printed",*boldoff;                             
          endif
.end patch 3.32


          RETURN
DONE
          PACK      NMOAFLD4 FROM SAMEbrk,samemlr
          REP       ZFILL IN NMOAFLD4
          CALL      NMOBKEY
.>Patch 3.3
          if (noescflag = 1)
                    call movez
                    call brkexit
                    goto FINISH                   
          endif
.>Patch 3.3         
          CALL      PRINTOTM
          call      printotb
FINISH    
          CALL      LASTPAGE
.START PATCH 3.2 REMOVED LOGIC
.         CALL      NPGEEOF
.END PATCH 3.2 REMOVCED LOGIC
.         CLOSE     PAGEFILE
.>Patch 3.3 
          prtclose prfile
.Give the email a chance of rendering itself before updating the INI file.
.Begin patch 3.40
.          call      GetPDfPAth
.          pack      str45 from PDFPATH,"\flag.dat"
.          pack      APIFileName,str45,hexzero
.          loop
.                    call      FindFirstFile
.                    until (APIResult = 0 | APIResult = hexeight)
.                    pause     "1"
.          repeat
          move      "200",str3
          call      waitin using str3
.          pause     "2"
.fix 2014 January 03
          unpack    today into mm,str1,dd,str1,yy
.end fix 2014 January 03
          rep       zfill in mm
          rep       zfill in yy
          pack      str55 from "\\nins1\e\data\moa",mm,yy,".pdf|10.10.30.103:502"
          Copyfile   "c:\work\pdf\ninmoa.pdf",str55
          if        Zero                .copy was good
          erase     "c:\work\pdf\ninmoa.pdf"
          Move      c0,Trapcount
          Move      "Here is your MOA Report",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          MOve      "GemmaSpranza@nincal.com,Creques@nincal.com",MailTo
          move      "2000",str4
          call      waitin using str4
.end patch 3.40

CheckFile
          pack      mailattach from "\\nins1\e\data\moa",mm,yy,".pdf"
          Move      mailattach,MailBody
          trap      WaitForEnd giving error if IO
          open      FileCheck,MailAttach,Exclusive          
          Close     FIleCHeck
          Move      Yes,Mailtrace


          call      SendMail

          else

          Move      "MOA Report - rename and email failed",MailSubjct
          Move      "Creques@nincal.com",MailFrom
          MOve      "GemmaSpranza@nincal.com,Creques@nincal.com,davidherrick@nincal.com",MailTo
          Move      str55,MailBody
          Clear     mailattach
          call      SendMail
          endif

.Begin patch 3.40
.          call      pdf995auto0
.end patch 3.40

.          erase     str35
.>Patch 3.3 

          shutdown
          STOP
...............................................................................
READMLR
          CALL      NMLRKEY
          CALL      NOMLR IF OVER
.START PATCH 3.2 REPLACED LOGIC
          move      COMPNUM,MNUM2
.END PATCH 3.2 REPLACED LOGIC
.>Patch 3.3
          if (FUNC = "2")
                    call      CompCompKey using COMPNUM,holda
                    reset holda to 282
                    move holda to str6
                    call trim using str6
                    if (str6 = "")
                              move "000000" to str6
                    endif
                    pack nescfld with compnum,str6
                    CALL NESCKEY        
                    If Over
                              move c1 to noescflag
                    else 
.                             MULT      "-1" BY BALANCE
.                             ADD       BALANCE TO TOTAL                                            
                              move c1 to brokeflag                              
                              move c0 to noescflag
                    Endif
          Endif               
.>Patch 3.3
          RETURN
NOMLR
          MOVE      "***NO MAILER FOUND***" TO MCOMP
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
...............................................................................
WRITPAGE
          MOVE      brcomp TO MNAME
          MOVE      PAGE TO N4
          MOVE      N4 TO INDPAGE
.START PATCH 3.2 REPLACED LOGIC
.         MOVE      MNUM TO INDMNUM
.         MOVE      MNAME TO INDNAME
.         MOVE      MCOMP TO INDCOMP
.         CALL      NPGEWRT
          MOVE      MNUM2,INDMNUM
          MOVE      MNAME,INDNAME
          MOVE      MCOMP,INDCOMP
.
          move      INDMNUM,NPGEFLD
          move      "NPGEWRT",Location
          pack      KeyLocation,"Key: ",NPGEFLD
          CALL      NPGEWRT
          call      PageUpdate
.END PATCH 3.2 REPLACED LOGIC
          RETURN
...............................................................................
zfillbrk
          move      "0000" to nmoabrk
          return
.START PATCH 3.2 ADDED LOGIC
PageUpdate
.Update previous record!!
          call      Trim using HoldMlrKey
          if (HoldMlrKey <> "")
                    pack      NPGEFLD,HoldMlrKey
.Re-pack immediately!!!
                    move      INDMNUM,HoldMlrKey
.
                    move      HoldBrkKey,str35
                    call      Trim using str35
                    move      INDNAME,HoldBrkKey
.
                    move      "NPGEKEY",Location
                    pack      KeyLocation,"Key: ",NPGEFLD
                    CALL      NPGEKEY
                    loop
                              until over
                              until (INDMNUM <> NPGEFLD)
                              call      Trim using INDNAME
                              if (str35 = INDNAME)
                                        move      PAGE,N4
                                        MOVE      N4 TO INDPAGE2
                                        move      "NPGEUPD",Location
                                        pack      KeyLocation,"Key: ",NPGEFLD
                                        CALL      NPGEUPD
                                        break
                              endif
                              move      "NPGEKS",Location
                              pack      KeyLocation,"Key: ",NPGEFLD
                              CALL      NPGEKS
                    repeat
          else
                    move      INDMNUM,HoldMlrKey
                    move      INDNAME,HoldBrkKey
          endif
          return
.END PATCH 3.2 ADDED LOGIC
WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
.                   if        (trapcount > 60)   . 5 min are you kidding me
                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Nona0003 - ",str35,b1,str55
                    Move      "CReques@nincal.com",MailFrom
                    Move      "dherric@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    mailattach,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    endif
          
                    goto      checkfile
...............................................................................
.         INCLUDE   NMLRIO.inc
.patch2.45
          include   compio.inc
          include   cntio.inc
.         INCLUDE   NMLRIO.INC
.patch2.45
.Begin Patch 3.31
           Include        MOANotesIO.inc
.End Patch 3.31
          INCLUDE   NMOAIO.inc
          INCLUDE   NMOBIO.inc
          INCLUDE   NDATIO.inc
          INCLUDE   NPGEIO.inc
.         include   nbrkio.inc
.>Patch 3.3
          include nescio.inc
.>Patch 3.3
          include   gnxtio.inc
          INCLUDE   COMLOGIC.inc