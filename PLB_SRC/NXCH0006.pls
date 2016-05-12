.*#**#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#**#*#*#*#*#*#**#**#*#*#*#*#*#*#*#*#*#*#*#*#*
.* TEST EXCHANGE PROGRAM :
.*  READS THE EXCHANGE ACCOUNT FILE SEQUENCIALLY AND FOR EACH ACCOUNT
.* GOES TO THE DETAIL FILE (NINEXCHNG) TO MAKE SURE THAT THE ENTRY NUMBER
.* IN THE ACCOUNT FILE (NINXNUM) AGREES WITH THE NINXCHNG FILE. IF IT
.* DOES NOT THEN AN ERROR MESSAGE IS PRINTED OUT REGARDING THAT ACCOUNT
.*#**#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#**#*#*#*#*#*#**#**#*#*#*#*#*#*#*#*#*#*#*#*#*
.*  VARIABLES USED BY THIS PROGRAM
.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
.Patch1.7
           include compdd.inc
           include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch1.7
         INCLUDE   NXNGDD.inc
         INCLUDE   NXCHDD.inc
          Include   PrtPagedd.inc
         INCLUDE   HP.inc
.begin patch 2.3
          include   Norddd.inc
.end patch 2.3
.*#**#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#**#*#*#*#*#*#**#**#*#*#*#*#*#*#*#*#*#*#*#*#*
Release   init      "2.51"      DLH  add some detail to email body
reldate   Init      "2013 October 13"
.Release   init      "2.5"      DLH  Sunbelt PDF
.reldate   Init      "2013 April 24"
.Release   init      "2.4"      DLH  64 bit 0S
.reldate   Init      "28 April 2011"
.Release   init      "2.3"      DLH  Look for details marked rental or cancelled where order is not
..                                    remove locks when reading/testing only
.reldate   Init      "8 February 2010"
.Release   init      "2.3"      DLH  detail switch default set to yes
.reldate   Init      "9 July 2009"
.Release   init      "2.2"      DLH  sendmail
.reldate   Init      "24 April 2008"
.Release  init      "2.1"      DMB  August 2006 Read Var for PDF995 ini to accomodate longer strings using taskname
.Release  init      "2.0"      dlh  march2005  prtpage / email
.release  init     "1.8"     ASH 14MAR2005        Exchange File Conversion
.release  init     "1.7"     DMB 26MAY2004        Mailer Conversion
.release  init     "1.6"     03OCT2000 ASH NEW SERVER ADDED
.release  init     "1.5"     22Sep98  ASH NINMLR Y2K File expansion
.ELEASE  INIT     "1.4"     02MAR93  DLH INCLUDES
.RELEASE          INIT      "1.3"     18FEB93
.* WRITTEN   07/16/82       By David Herrick
.* UPDATED    9DEC87
. .............................................................................
.
. FILES
. ...................
.
.ACCOUNT  FILE
.
.ACCOUNT1 AFILE     FIX=22
.
.XCHNG    IFILE     KEYL=13,STATIC=14    DETAIL FILE
.
.MLRMST   IFILE     KEYL=7    MASTER MAILER FILE FOR OUTPUT
.
. .............................................................................
.
. ACCOUNT FILE
. ...................
.
.START PATCH 1.8 REPLACED LOGIC
.MLR1     DIM       4
.MLR2     DIM       4     MAILER NUMBERS
MLR1     DIM       6
MLR2     DIM       6     MAILER NUMBERS
.END PATCH 1.8 REPLACED LOGIC
FixEntryFlag        Dim       1                   Y= fix entry #'s in master
.START PATCH 2.0 REPLACED LOGIC
.AKEY1   INIT       "01R"
.AKEY2    INIT      "02R"
AKEY1   INIT       "01X"
AKEY2    INIT      "02X"
.END PATCH 2.0 REPLACED LOGIC
.Begin Patch 9.87
.end Patch 9.87
.
. .............................................................................
.
. WORK FIELDS
. ....................
.
.Start Patch #1.5 - expanded fields to reflect expansion of MCOMP
.MDES1    DIM       25
.MDES2    DIM       25
MDES1    DIM       45
MDES2    DIM       45
.End Patch #1.5 - expanded fields to reflect expansion of MCOMP
ENTRY1   FORM      5        ENTRY NUMBER
COUNT    FORM      7            NUMBER OF ACCOUNT RECORDS READ
OKCOUNT  FORM      7
BADCOUNT FORM      5            NUMBER OF DETAIL RECORDS NOT FOUND
BADCOUNT1 FORM      5            NUMBER OF DETAIL RECORDS that do not match order status  ie rent/exchange, live/canc
StatCOUNT FORM      8            NUMBER OF status RECORDS checked
PAGE     FORM      "0000"
LINE     FORM      "00"
TOTAL1   FORM      9
TOTAL2   FORM      9
VAR1     FORM      9
VAR2     FORM      9
STOP     FORM      5
BRANCH   FORM      1
ZERO5    FORM      "00000"
DETSW    DIM       1         *Y=PERFORM TOTALS CHECK < 1000, N=NO CHECK
NUM      FORM      1         BRANCH INDICATOR FOR MODE.
HENTRY   DIM       5         HOLD ENTRY FOR OUTPUT
.START PATCH 1.8 REPLACED LOGIC
.HKEY     DIM       8         HOLD KEY FOR MATCH
.HKEY1    DIM       5
.OLDKEY   DIM       8
HKEY     DIM       12        HOLD KEY FOR MATCH
HKEY1    DIM       5
OLDKEY   DIM       12
.END PATCH 1.8 REPLACED LOGIC
TYPE1    DIM       6
TYPE2    DIM       6
DATE     DIM       8
TIME     DIM       8
DIFF     FORM      10
DIFF1    FORM      10
THOUS    FORM      "1000"
.START PATCH 2.0 ADDED LOGIC
.Reset995Flag       Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
PDFFlag   Dim       1
.hexeight           integer 4,"4294967295"
userlogn  dim       7
.Define Fonts to be used
font1          font
font2          font
font3          font
font4          font
font5          font
.PrFIle         Pfile
.NINLogo  PICT
                    include   winapi.inc
timestamp2          dim       16
.END PATCH 2.0 ADDED LOGIC
.begin patch 2.3
SysDate   Form      5
.end patch 2.3
. .............................................................................
.
. PROGRAM BODY
. .............................................................................
.
         MOVE      "Names In The News" TO COMPNME
         MOVE      "NXCH0006" TO PROGRAM
         MOVE      "TEST EXCHANGE ACCOUNTS" TO STITLE
         CLOCK     DATE TO TODAY
         MOVE      TODAY TO DATE
         MOVE      "ABORT" TO PF5
         CALL       PAINT
         CALL       FUNCDISP
.         DISPLAY   *P01:01,*ES,*P14:01," EXCHANGE TEST PROGRAM "

         TRAP      IO IF IO
         TRAP      EOJ IF F5
.START PATCH 2.0 ADDED LOGIC
.Create fonts to be used
               create         font1,"FIXED",size=10
               create         font2,"FIXED",size=10,bold
               create         font3,"FIXED",size=10,bold,italic
               create         font4,"Arial",size=14,italic
          create    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.END PATCH 2.0 ADDED LOGIC
.
.begin patch 9.87
.begin patch 2.5
.          CALL      GETpdfPATH
.end patch 2.5
.end patch 9.87

.         OPEN      nxchfile,"NINxchng"
PASSQUES MOVE      C1 TO NUM
         KEYIN     *P12:12,*EF,"CHOOSE MODE":
                   *P12:13,"-----------":
                   *P12:15,"1)CHECK MASTER VERIFY DETAIL (DEFAULT)":
                   *P12:16,"2)CHECK DETAIL VERIFY/Repair MASTER (inserts missing records)":
                   *p12:17,"3)CHECK INDIVIDUAL ACCOUNT":
                   *P12:18,"4)CHECK DETAIL VERIFY/Repair MASTER (inserts missing records fixes entry #)":
                   *P12:21,"5) TO EXIT":
                   *P20:23,"YOUR CHOICE : ",*RV,*T30,NUM
         DISPLAY   *P12:12,*EL:
                   *P12:13,*EL:
                   *P12:15,*EL:
                   *P12:16,*EL:
                   *P12:17,*EL:
                   *P12:18,*EL:
                   *P12:19,*EL:
                   *P12:21,*EL:
                   *P12:23,*EL
         CALL      FUNCDISP
.START PATCH 2.0 ADDED LOGIC
          If (num = c1)
                    OPEN      NXCHFILE,NXCHNAME,REad
                    TRAPCLR   IO
                    MOVE      C1 TO NXCHFLAG
                    move      c3,nxchlock
                    OPEN      NXNGFILE,NXNGNAME
                    TRAPCLR   IO
                    MOVE      C1 TO NXNGFLAG
                    move      c3,nxnglock
          Elseif (Num = c2)
                    OPEN      NXCHFILE,NXCHNAME,REad
                    TRAPCLR   IO
                    MOVE      C1 TO NXCHFLAG
                    move      c3,nxchlock
          Elseif (Num = c5)
                    Stop
          endif
.END PATCH 2.0 ADDED LOGIC
         BRANCH    NUM OF PASS1,SPOOL,SPOOL,EOJ
         GOTO      PASSQUES
.begin patch 2.3
PASS1    MOVE      Yes TO DETSW
.PASS1    MOVE      NO TO DETSW
.end patch 2.3
.         KEYIN     *P12:12,"Do you want balances checked? ",DETSW
         KEYIN     *P12:12,"Balance < 1000 ? ",*RV,*T30,DETSW
.START PATCH 2.0 REPLACED LOGIC
.         GOTO      SPOOL
.SPOOL    MOVE      YES TO STR1
.         KEYIN     *P12:12,*EL,"Do you want spooling? ",*RV,*T30,STR1:
.                   *P1:12,*EL
.         CMATCH    YES TO STR1
.         GOTO      YESPOOL IF EQUAL
.         CMATCH    NO TO STR1
.         GOTO      BEGIN IF EQUAL
.         GOTO      SPOOL
.YESPOOL
SPOOL
.END PATCH 2.0 REPLACED LOGIC
.START PATCH 1.6 REPLACED LOGIC
.         SPLOPEN   "g:\data\TESTEXCH"
.         DISPLAY   *P12:22,"FILENAME IS g:\data\TESTEXCH.LST"
         PACK      STR35,NTWKPATH1,"TESTEXCH"
.START PATCH 2.0 REPLACED LOGIC
.         SPLOPEN   STR35
.         DISPLAY   *P12:22,"FILENAME IS \\nins1\E\data\TESTEXCH.LST"
..........................................................
.First check 995 autolaunch settings
.begin patch 2.5
.          CALL      PDF995Auto
.                                        call       GetPDFPath
.                                        pack      str55 from PDFPATH,"\res\pdf995.ini"
.
.                                        pack      str55 from PDFPATH,"\res\pdf995.ini"
.                                        call      "GU$INI;WRITE_TO_INI" USING Str55:
.                                                  "Parameters":
.                                                  "ProcessPDF":
.                                                  "\\nins1\e\apps\winbatch\Del995Flag.exe":
.                                                  result
.
.                                        if (result = C0)
..Prepare Flag file
.                                                  move      C1,PDFFlag
.                                                  pack      str55 from PDFPATH,"\flag.dat"
.                                                  prep      tempfile,Str55
.                                                  write     tempfile,SEQ;"flag set"
.                                                  close     tempfile
.                                        else
.                                                  move      C2,PDFFlag
.                                        endif
.                                        if (PDFFlag = "2")
..Send message to I.S.
.                                                  move      "This is a message from       NLCR0038",MailSubjct
.                                                  CLear     MailBody
.                                                  Append    "Failure to update pdf995.ini.",MailBOdy
.                                                  append    CRLF,Mailbody
.                                                  Append    "Field: ",Mailbody
.                                                  append    str25,Mailbody
.                                                  append    CRLF,Mailbody
.                                                  Append    "Value: ",Mailbody
.                                                  append    str35,mailbody
.                                                  append    CRLF,Mailbody
.                                                  reset     Mailbody
.                                                  move      "InformationServices@nincal.com",Mailto
.                                                  move      "InformationServices@nincal.com",MailFrom
.                                                  
.                                                  call      SendMail
.                                                  move      C0,PDFFlag
.                                        endif

          
.          pack      str25,"NXCH0006"
          pack      str55,"c:\work\pdf\NXCH0006.pdf"
.         SplOpen   "PDF995","A"
.         PRtopen   Laser,"",str25
.          PRTOPEN   Laser,"PDF995",str25
          PRTOPEN   Laser,"PDF:",str55
.          pack      str55,str25,".pdf"
.end patch 2.5
          Call      Header
.         SPLOPEN   STR35
.         DISPLAY   *P12:22,"FILENAME IS \\nins1\E\data\TESTEXCH.LST"
          DISPLAY   *P12:22,"FILENAME IS ",str55          ."
.END PATCH 2.0 REPLACED LOGIC
.END PATCH 1.6 REPLACED LOGIC
.START PATCH 2.0 REMOVED LOGIC
.         print     hplin8,*f
.END PATCH 2.0 REMOVED LOGIC
BEGIN
         DISPLAY   *P01:24,*EL;
.
         DISPLAY   *P5:12,*EL,"READING ACCOUNT FILE NUMBER OF RECORDS READ":
                   " EQUALS : ":
                   *P5:14,*EL,"NUMBER OF GOOD RECORDS EQUALS : ":
                   *P5:16,*EL,"NUMBER OF BAD RECORDS EQUALS  : "
          if        (num = c4)
          MOve      yes,fixEntryFlag
          endif
         BRANCH    NUM OF START,PASS2,PASS3,pass3
START
         MOVE      C0 TO TOTAL1
         MOVE      C0 TO TOTAL2
         DISPLAY   *P57:12,*EL,COUNT:
                   *P37:14,*EL,OKCOUNT:
                   *P37:16,*EL,BADCOUNT
.        FILEPI    1;ACCOUNT
.         READ      ACCOUNT,SEQ;MLR1,MLR2,FILL8,ENTRY,FLAG
         call      nxngseq
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNT
         CLEAR     NXCHFLD1
         PACK      NXCHFLD1 FROM acckey,ZERO5
.         PACK      ACCKEY FROM MLR1,MLR2
         MOVE      C0 TO STOP
         MOVE      ENTRY TO STOP
         REP       ZFILL,NXCHFLD1
         MOVE      "NO BEG BALANCE " TO ERROR
         move      c1 to nxchpath
.        FILEPI    1;NXCHFILE
.         READ      NXCHFILE,NXCHFLD1;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         call      nxchkey
         IF        OVER
          CLEAR     NXCHFLD1
          PACK      NXCHFLD1 FROM acckey,B4,C0
.         FILEPI    1;NXCHFILE
.          READ      NXCHFILE,NXCHFLD1;EXKEY,LR,USAGE1,USAGE2:
.                    QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         call       nxchkey
            IF        NOT OVER
            MOVE      "BEG BAL NO ZFILL" TO ERROR
            DISPLAY   *P1:24,*EL,"BEG BAL NOT ZFILL FOR: ",EXKEY
         ELSE
            DISPLAY   *P1:24,*EL,"NO BEG BAL FOR: ",EXKEY
         ENDIF
          CALL      PRINT
         ENDIF
.        GOTO      START

         MOVE      "- BAL" TO ERROR
         CALL      BEGBAL
         COMPARE   C0 TO USAGE1
         CALL      PRINT IF LESS
         COMPARE   C0 TO USAGE2
         CALL      PRINT IF LESS
         COMPARE   ENTRY TO ZERO5
         GOTO      EXIT IF EQUAL
         MOVE      ENTRY TO STOP
.         CLEAR     NXCHFLD1
.         PACK      NXCHFLD1 FROM ACCKEY,ZERO5
READKS
.        FILEPI    1;NXCHFILE
.        READKS    NXCHFILE;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         call      nxchks
         GOTO      MISSING IF OVER       *GAP IN DETAIL ENTRIES.
         SCAN      ACCKEY IN EXKEY       *SAME ACCOUNT?
         GOTO      MISSING IF EOS
         RESET     EXKEY
.START PATCH 1.8 REPLACED LOGIC
.         BUMP      EXKEY,8
         BUMP      EXKEY,12
.END PATCH 1.8 REPLACED LOGIC
         MOVE      C0 TO ENTRY1
         MOVE      EXKEY TO ENTRY1
         RESET     EXKEY,0
         RESET     EXKEY
         COMPARE   ENTRY1 TO C0
         GOTO      EXIT IF EQUAL
         COMPARE   ENTRY1 TO STOP
         GOTO      COMP IF EQUAL
         CALL      CALC
.         ADD       C1 TO ENTRY1
.         PACK      NXCHFLD1 FROM MLR1,MLR2,ENTRY1
.         REP       ZFILL IN NXCHFLD1
         GOTO      READKS
.
MISSING  MOVE      "LOST READING DETAIL" TO ERROR
         CALL      PRINT
         GOTO      START
BEGBAL   MOVE      C0 TO TOTAL1
         MOVE      C0 TO TOTAL2
         MOVE      USAGE1 TO TOTAL1
         MOVE       USAGE2 TO TOTAL2
         RETURN
COMP     CALL      CALC                   ADD LAST ENTRY
         CMATCH    YES TO DETSW          INCLUDE DIFF < 1000 ?
         GOTO      COMP1a IF NOT EQUAL     NO.
         MOVE      "TOTALS" TO ERROR
         COMPARE   TOTAL1 TO USAGE1
         IF        NOT EQUAL
         CALL      PRINT
         BRANCH    NUM OF START,EXIT,PASS3,pass3
.        GOTO      START
         ENDIF
         COMPARE   TOTAL2 TO USAGE2
         IF        NOT EQUAL
         CALL      PRINT
         BRANCH    NUM OF START,EXIT,PASS3,pass3
.        GOTO      START
         ENDIF
         GOTO      EXIT
COMP1a    MOVE      TOTAL1 TO DIFF
         MOVE      TOTAL2 TO DIFF1
         SUB       USAGE1 FROM DIFF
         SUB       USAGE2 FROM DIFF1
         ADD       DIFF TO DIFF1
         MOVE      "TOTALS" TO ERROR
         COMPARE   DIFF1 TO C0
         CALL      NEG IF NOT LESS
         COMPARE   DIFF1 TO THOUS      DIFF > 1000 ?
         IF        LESS
         CALL      PRINT               YES
         BRANCH    NUM OF START,EXIT,PASS3,pass3
.        GOTO      START
         ENDIF
         GOTO      EXIT
NEG      MULT      SEQ BY DIFF1
         RETURN
EXIT     ADD       C1 TO OKCOUNT
         GOTO      START
CALC     COMPARE   ENTRY1 TO C0        AT BEG BALANCE ?
         RETURN    IF EQUAL            YES.
         CMATCH    "R" TO STAT            *IF RENTAL DON'T ADD IT.
         RETURN    IF EQUAL
         CMATCH    "C" TO STAT
         RETURN    IF EQUAL               *IF CANCELLED DON'T ADD IT.
         MOVE      C0 TO BRANCH
         MOVE      MLRSW TO BRANCH
         BRANCH    BRANCH OF QTY1,QTY2
         CLEAR     ERROR
         APPEND    "MLRSW LR: " TO ERROR
         APPEND    LR TO ERROR
         RESET     ERROR
.         MOVE      "MLRSW" TO ERROR
.         NORETURN
         CALL      PRINT
         RETURN
QTY1     ADD       QTY TO TOTAL1
         RETURN
QTY2     ADD       QTY TO TOTAL2
         RETURN
.
PRINT
         UNPACK    acckey INTO MLR1,MLR2
         ADD       C1 TO BADCOUNT
.START PATCH 2.0 REMOVED LOGIC
.         COMPARE   C0 TO LINE
.         CALL      HEADER IF EQUAL
.         ADD       C5 TO LINE
.         COMPARE   "85" TO LINE
.         CALL       HEADER IF EQUAL
.END PATCH 2.0 REMOVED LOGIC
.START PATCH 1.8 REPLACED LOGIC
.         PACK      MKEY FROM MLR1,Z3
.         MOVE      "UNKNOWN" TO MDES1
.         MOVE      "UNKNOWN" TO MDES2
.         REP       ZFILL IN MKEY
.         CALL      NMLRKEY
.         MOVE      MCOMP TO MDES1
.         PACK      MKEY FROM MLR2,Z3
.         REP       ZFILL IN MKEY
.         CALL      NMLRKEY
.         MOVE      MCOMP TO MDES2
........................
          pack      COMPFLD,MLR1
          MOVE      "UNKNOWN",MDES1
          MOVE      "UNKNOWN",MDES2
          REP       ZFILL,COMPFLD
          move      "COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          CALL      COMPKEY
          MOVE      COMPCOMP,MDES1
.
          PACK      COMPFLD,MLR2
          REP       ZFILL,COMPFLD
          move      "2-COMPKEY",Location
          CALL      COMPKEY
          MOVE      COMPCOMP,MDES2
.END PATCH 1.8 REPLACED LOGIC
         MOVE      TOTAL1 TO VAR1        .CALCULATE VARIANCES
         MOVE      TOTAL2 TO VAR2
         SUB       USAGE1 FROM VAR1
         SUB       USAGE2 FROM VAR2
.START PATCH 2.0 REPLACED LOGIC
.         PRINT     HPLINE:
.                   *L,*5,MLR1," ",MDES1,"  ACCOUNT ENTRY : ",ENTRY1:
.                   *L,*5,MLR2," ",MDES2,"  MASTER KEYS : ",NXCHFLD1," ",ACCKEY:
.                   HPBON:
.                   *L,*5,"  ERROR = ",ERROR,*5,"  ERROR = ",ERROR,HPBOFF:
.                   HPGOTH10,*L,*5,"  TOTALS Calc'd = ",TOTAL1," ",TOTAL2:
.                   " Listed = ",USAGE1:
.                   " ",USAGE2," Variance",B1,VAR1,B1,VAR2,HPLINE,HPLIN8
.        add        c5 to line
          if        (row > 9500)
          call      Header
          endif
          prtpage   Laser;*p=1:row,MLR1," ",MDES1,"  ACCOUNT ENTRY : ",ENTRY1
          add       Eightlpi to row
          prtpage   Laser;*p=1:row,MLR2," ",MDES2,"  MASTER KEYS : ",NXCHFLD1," ",ACCKEY
          add       Eightlpi to row
          PrtPage   Laser;*p=1:row,"  ERROR = ",ERROR,"  TOTALS Calc'd = ",TOTAL1," ",TOTAL2
          add       Eightlpi to row
          prtpage   Laser;*p=1:row," Listed = ",USAGE1:
                    " ",USAGE2," Variance",B1,VAR1,B1,VAR2
          add       Eightlpi to row
          add       Eightlpi to row
.END PATCH 2.0 REPLACED LOGIC
         BRANCH    NUM OF RET,NORET,RET
RET      RETURN
NORET    NORETURN
         GOTO      PASS2A
PASS2
.START PATCH 1.8 REPLACED LOGIC
.         MOVE      "0000000000000" TO NXCHFLD1    *SET UP FOR READKS
         MOVE      "00000000000000000" TO NXCHFLD1    *SET UP FOR READKS
.END PATCH 1.8 REPLACED LOGIC
.        FILEPI    2;NXCHFILE
.         READ      NXCHFILE,NXCHFLD1;;
.         READKS    NXCHFILE;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         call      nxchtst
         call      nxchks
         UNPACK    EXKEY INTO HKEY,HENTRY
PASS2A   call      nxchks
.        FILEPI    1;NXCHFILE
.         READKS    NXCHFILE;EXKEY,LR,USAGE1,USAGE2:
.                   QTY,LIST,DAT,STAT,MLRSW,TYPE,XCHCOMNT
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNT
         DISPLAY   *P12:12,*EL,"NUMBER OF DETAIL RECORDS READ : ",COUNT
         DISPLAY   *P12:14,*EL,"NUMBER OF ERRORS : ",BADCOUNT
         DISPLAY   *P12:16,*EL,"NUMBER OF OK ACCOUNTS, : ",OKCOUNT
.         BUMP      EXKEY BY 8
.         MATCH     "00000" TO EXKEY
         UNPACK    EXKEY INTO ACCKEY,HKEY1
         MATCH     ACCKEY TO HKEY              *SAME ACCOUNT?
         GOTO      PASS2B IF EQUAL             YES.
.  NO. IT'S A BREAK
.         RESET     EXKEY,0
.         RESET     EXKEY
.         MOVE      EXKEY TO ACCKEY
         UNPACK    ACCKEY INTO MLR1,MLR2
         MOVE      HKEY TO OLDKEY
         MOVE      ACCKEY TO HKEY
         MOVE      HENTRY TO ENTRY
         MOVE      HKEY1 TO HENTRY
         MOVE      "NO MASTER" TO ERROR
         PACK      nxngfld1 FROM AKEY1,MLR1
         PACK      nxngfld2 FROM AKEY2,MLR2
.        FILEPI    1;NXNGFILE
.         READ      NXNGFILE,nxngfld1,nxngfld2;;
         call      nxngtst
         GOTO      REV IF OVER
         ADD       C1 TO OKCOUNT
         GOTO      PASS2A
REV      UNPACK    OLDKEY INTO MLR1,MLR2
         PACK      OLDKEY FROM MLR2,MLR1
         PACK      nxngfld1 FROM AKEY1,MLR2
         PACK      nxngfld2 FROM AKEY2,MLR1
         call      nxngtst
.         READ      NXNGFILE,nxngfld1,nxngfld2;;
         GOTO      PASS2C IF OVER
         MOVE      "MASTER REVERSED" TO ERROR
         GOTO      DELQUES
PASS2B   MOVE      HKEY1 TO HENTRY             *SAVE ENTRY#.
         GOTO      PASS2A
PASS2C   PACK      acckey FROM MLR1,MLR2
         PACK      NXCHFLD1 FROM OLDKEY,ENTRY
         MOVE      "NO MASTER - ADDED ONE " TO ERROR
         TRAP      DUPEKEY IF IO
         move      b1 to flag
.         FILEPI    1;NXNGFILE
.         WRITE     NXNGFILE;OLDKEY,FILL8,ENTRY," "
.START PATCH 2.0 REMOVED LOGIC
.         call      nxngwrt
.END PATCH 2.0 REMOVED LOGIC
         TRAPCLR   IO
         CALL      PRINT
DUPEKEY
         TRAPCLR   IO
         NORETURN
         MOVE      "MASTER NOT IN INDEX FILE, OK NOW!?" TO ERROR
         CALL      PRINT
DELQUES  PACK      OLDKEY FROM MLR1,MLR2
         PACK      NXCHFLD1 FROM OLDKEY,ENTRY
         COMPARE   C0 TO ENTRY
         CALL      PRINT IF NOT EQUAL
         PACK      NXCHFLD1 FROM OLDKEY,ENTRY
         REP       ZFILL IN NXCHFLD1
.         FILEPI    3;NXCHFILE
.         READ      NXCHFILE,NXCHFLD1;;
         call      nxchtst
         CALL      PRINT IF OVER
.         DELETE    NXCHFILE,NXCHFLD1
         call      nxchdel
         MOVE      "DETAIL REVERSED-DELETED" TO ERROR
         PACK      NXCHFLD1 FROM HKEY,HENTRY
         call      nxchtst
.         READ      NXCHFILE,NXCHFLD1;;           *RESTORE FILE POSITION.
         PACK      NXCHFLD1 FROM OLDKEY,ENTRY *RESTORE VALUE FOR PRINT
         CALL      PRINT
.
.
PASS3    move      no to detsw
         KEYIN     *P1:4,*EF:
                   *P10:12,"MAILER 1 ",*ZF,*JR,MLR1:
                   *P10:14,"MAILER 2 ",*ZF,*JR,MLR2
         PACK      nxngfld1 FROM AKEY1,MLR1
         PACK      nxngfld2 FROM AKEY2,MLR2
         PACK      ACCKEY FROM MLR1,MLR2
         call      nxngtst
         GOTO      REV1 IF OVER
         GOTO      PASS3A
REV1     PACK      nxngfld1 FROM AKEY1,MLR2
         PACK      nxngfld2 FROM AKEY2,MLR1
         PACK      ACCKEY FROM MLR2,MLR1
         call      nxngtst
         IF        OVER
         DISPLAY   *P1:24,*EL,"NO SUCH ACCOUNT",*B,*W
         GOTO      PASS3
         ENDIF
PASS3A   call      nxngaim
         PACK      nxchfld1 FROM ACCKEY,ZERO5
         REP       ZFILL IN nxchfld1
         CALL      NXCHKEY
         MOVE      "- BAL" TO ERROR
         CALL      BEGBAL
         COMPARE   C0 TO USAGE1
         CALL      PRINT IF LESS
         COMPARE   C0 TO USAGE2
         CALL      PRINT IF LESS
         COMPARE   ENTRY TO ZERO5
         GOTO      EXIT IF EQUAL
         MOVE      ENTRY TO STOP
READKS1  call      nxchks
         GOTO      MISSING1 IF OVER       *GAP IN DETAIL ENTRIES.
         SCAN      ACCKEY IN EXKEY       *SAME ACCOUNT?
         GOTO      MISSING1 IF EOS
         RESET     EXKEY
.START PATCH 1.8 REPLACED LOGIC
.         BUMP      EXKEY,8
         BUMP      EXKEY,12
.END PATCH 1.8 REPLACED LOGIC
         MOVE      C0 TO ENTRY1
         MOVE      EXKEY TO ENTRY1
         RESET     EXKEY,0
         RESET     EXKEY
         COMPARE   ENTRY1 TO C0
         GOTO      EXIT IF EQUAL
         COMPARE   ENTRY1 TO STOP
         GOTO      COMP IF EQUAL
         CALL      CALC
         call       comp31
         goto      readks1
MISSING1  MOVE      "LOST READING DETAIL" TO ERROR
         CALL      PRINT
         GOTO      READKS1
COMP31    CMATCH    YES TO DETSW          INCLUDE DIFF < 1000 ?
         GOTO      COMP31a IF NOT EQUAL     NO.
         MOVE      "TOTALS" TO ERROR
         COMPARE   TOTAL1 TO USAGE1
         IF        NOT EQUAL
         CALL      PRINT
         MOVE      USAGE1 TO TOTAL1
         ENDIF
         COMPARE   TOTAL2 TO USAGE2
         IF        NOT EQUAL
         CALL      PRINT
         MOVE      USAGE2 TO TOTAL2
         ENDIF
          return
COMP31a   move      c0 to diff
         move      c0 to diff1
         MOVE      TOTAL1 TO DIFF
         MOVE      TOTAL2 TO DIFF1
         SUB       USAGE1 FROM DIFF
         SUB       USAGE2 FROM DIFF1
         MOVE      "TOTALS" TO ERROR
         COMPARE   DIFF1 TO C0
         CALL      NEG IF NOT LESS
         COMPARE   DIFF1 TO THOUS      DIFF > 1000 ?
         IF        LESS
         CALL      PRINT               YES
         MOVE      USAGE1 TO TOTAL1
         endif
         COMPARE   DIFF TO C0
         CALL      NEG IF NOT LESS
         COMPARE   DIFF TO THOUS      DIFF > 1000 ?
         IF        LESS
         CALL      PRINT               YES
         MOVE      USAGE2 TO TOTAL2
         endif
         return

.
HEADER
         CLOCK     DATE TO DATE
         CLOCK     TIME TO TIME
         ADD       C1 TO PAGE
.START PATCH 2.0 REPLACED LOGIC
.         PRINT     HPLINE:
.                   *F,*L,*L,*25,"*** NAMES IN THE NEWS ***",*67,"PAGE :",PAGE:
.                   *L,*67,"DATE: ",DATE:
.                   *L,*15,"EXCHANGE TEST PRINTOUT SHOWING PROBLEMS ACCOUNTS":
.                   *67,"TIME: ",TIME
.         MOVE      C5 TO LINE
          If (Page = c1)
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0
          else
                    prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT:
                              *MarginL=0,*MarginT=0,*Newpage
          endif
          prtpage   Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
          move      "1500",row
          prtpage   Laser;*p=1750:row,"EXCHANGE TEST PRINTOUT SHOWING PROBLEMS ACCOUNTS":
                    *p=1:row,"Date: ",date,*p=6750:row,"Time: ",time
          add       eightlpi to row
          add       eightlpi to row
          add       eightlpi to row
          add       eightlpi to row
.END PATCH 2.0 REPLACED LOGIC
         RETURN
EOJ
.START PATCH 2.0 REMOVED LOGIC
.         COMPARE   C0 TO LINE
.         CALL      HEADER IF EQUAL
.END PATCH 2.0 REMOVED LOGIC
         BRANCH    NUM OF EOJM,EOJD
EOJM     MOVE      "MASTER" TO TYPE1
         MOVE      "DETAIL" TO TYPE2
         GOTO      EOJ1
EOJD     MOVE      "DETAIL" TO TYPE1
         MOVE      "MASTER" TO TYPE2
EOJ1
.START PATCH 2.0 REPLACED LOGIC
.         COMPARE   "85" TO LINE
.         CALL       HEADER IF EQUAL
.         CALL       HEADER IF NOT LESS
.         PRINT      HPLINE:
.                   *L,*20,"NUMBER OF ACCOUNTS WITH DISCREPANCIES:",BADCOUNT:
.                   *L,*20,"NUMBER OF ",TYPE2," ACOUNTS CHECKED :",COUNT:
.                   *L,*20,"NUMBER OF GOOD ",TYPE1," ACCOUNTS   :",OKCOUNT:
.                   *L,*L,*2,"NOTE: TOTAL ERRORS MUST BE RESEARCHED";
.         PRINT     " CAREFULLY!!!!";
.         PRINT     *2,"NOTE: TOTAL ERRORS MUST BE RESEARCHED";
.         PRINT     " CAREFULLY!!!!"
.         CMATCH    NO TO DETSW
.         IF        EQUAL
.         PRINT     *2,"BALANCE DIFFERENCES LESS THAN 1000 NOT PRINTED",hplin6
.         ENDIF
.         SPLCLOSE
.         EXECUTE   "f:\public\NPRINT f:\data\testexch.LST Q=LASER2 NT NB f=0 S=NTS0_FPNW /d"
.         STOP
.................................................
          if (row > 9500)
                    call      Header
          endif
          clock     Time to Time
          add       Eightlpi to row
          PRtpage   Laser;*p=500:row,*boldon,"NUMBER OF ACCOUNTS WITH DISCREPANCIES:",BADCOUNT
          add       Eightlpi to row
          PRtpage   Laser;*p=500:row,"NUMBER OF ",TYPE2," ACOUNTS CHECKED :",COUNT
          add       Eightlpi to row
          PRtpage   Laser;*p=500:row,"NUMBER OF GOOD ",TYPE1," ACCOUNTS   :",OKCOUNT
          add       Eightlpi to row
          PRtpage   Laser;*p=500:row,"NOTE: TOTAL ERRORS MUST BE RESEARCHED CAREFULLY!!!!",*boldoff
          CMATCH    NO TO DETSW
          IF EQUAL
                    add       Eightlpi to row
                    PRtpage   Laser;*p=500:row,"BALANCE DIFFERENCES LESS THAN 1000 NOT PRINTED"
          ENDIF
          add       Eightlpi to row
          PRtpage   Laser;*p=1:row,"Date: ",date,*p=6750:row,"Time: ",time
.begin patch 2.3
.          If (page <= c1)
.                    prtpage   Laser;*newpage
.          endif
          if        (num = c1)   default pass
          call      CheckRent
          clock     Time to Time
          add       Eightlpi to row
          PRtpage   Laser;*p=1:row,"Date: ",date,*p=6750:row,"Time: ",time
          endif
          If (page <= c1)
                    prtpage   Laser;*newpage
          endif
.end patch 2.3
          call      CreatePDF
          STOP
...............................................................................
CreatePDF
          prtclose Laser
.         Pause     "20"
.It takes some time for the file to be created, so we must check
.begin patch 2.5
.         pack      APIFileName from PDFPATH,"\flag.dat"
.
.         for N3,C1,"100"
.         repeat
.end patch 2.5
         pause     "5"
          Move      "DHerric" to UserLogn
.begin patch 2.5
.          move    C0,N9
.          move    "                                        ",APIFileName
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
.end patch 2.5
          move      "Here is your Exchange Verification Report",MailSubjct
          Clear     MailBody
          append    Str55,MailBody
           append   "<br>",mailbody
           append    "NUMBER OF ACCOUNTS WITH DISCREPANCIES: ",mailbody
           append     badcount,mailbody
           append   "<br>",mailbody
           append    "Number of detail records that do not match associated order: ",mailbody
           append     badcount1,mailbody
           append   "<br>",mailbody
           append   "<br>",mailbody
           append   "<br>",mailbody
          reset     MailBody
          pack      MailTo from userlogn,"@nincal.com,rwhitin@nincal.com"
          pack      MailFrom from userlogn,"@nincal.com"
.begin patch 2.5
.          PAck      MailAttach from "c:\work\pdf\",str55              ."
          PAck      MailAttach from str55              
.end patch 2.5
           move       c1,mailtype
          call      SendMail
.Clean up afterwards
.begin patch 2.5
.          if (Reset995Flag = yes)
.          CALL      PDF995Auto0
.
.          endif
.end patch 2.5
          Erase     Mailattach
          return
.END PATCH 2.0 REPLACED LOGIC
IO       DISPLAY   *P10:20,*EL,"FILE NOT FOUND",*B,*W5
         STOP
.begin patch 2.3
Checkrent
          if        (row > 9500)
          call      Header
          endif
          add       Eightlpi to row
          add       Eightlpi to row
          PRtpage   Laser;*p=500:row,*boldon,"Checking Cancellation/Rent Details"
          add       Eightlpi to row
          PRtpage   Laser;*p=500:row,"NUMBER OF Records Checked   :",statcount
          add       Eightlpi to row
          add       Eightlpi to row
          OPEN      NXCHFILE,NXCHNAME,REad
          move      c1,nordpath
          move      c3,nordlock
          move      c1,Statcount
          Display   *P5:16,*EL,"Checking Status, RECORDS EQUALS : "
Loop      Loop
          MOVe      B1,stat
          call      nxchseq
          until     over
          Display   *P37:16,*EL,StatCOUNT
          add       c1,statcount
          if        (stat = "C" or stat = "X" or Stat = "R")
          unpack    Date into mm,str1,dd,str1,yy
          call      Cvtjul
          move      Juldays to SysDate
.check date if over 3 years ago skip
          unpack    Dat into str2,yy,mm,dd
          call      Cvtjul
                    if        (sysdate-1095 <= Juldays)
                    packkey   nordfld from lr
                    call      Nordkey
                              if        not over
                                        pack    str8 from oodtec,oodtey,oodtem,oodted
                                        rep       Zfill in str8
                                        if        (str8 <> dat)   .compare the order date
                                        call      debug
                                        goto      loop                 . not the correct order
                                        endif
                                        if        (Stat = "R")               .verify order rental only
                                                  if        (OELCODE = "2" or OELCODE = "3")
                                                  call      NotRent
                                                  endif
                                        elseif    (stat = "C" or stat = "X")  .check cancelled
                                                  if        (Ostat <> "Q" & Ostat <> "X")   .not cancelled
                                                  call      NotCanc
                                                  endif
                                        endif
                              endif
                    Endif          
          else
.          goto loop
          endif
          Move      " ",stat
          Move      " ",Ostat
          repeat
          Return
NotCanc
          if        (row > 9500)
          call      Header
          endif
          prtpage   Laser;*p=1:row,LR,"Not Cancelled in order file, Stat= ",stat," Ostat= ",Ostat,b1,exkey
          add       Eightlpi to row
          add         c1,badcount1
          Return
NotRent
          if        (row > 9500)
          call      Header
          endif
          prtpage   Laser;*p=1:row,LR,"Not a Rental in order file, Stat= ",stat," Oelcode = ",Oelcode,b1,exkey
          add       Eightlpi to row
          add         c1,badcount1
          Return          
.end patch 2.3
.Patch1.7
                              include compio.inc
                              include   cntio.inc
.      INCLUDE   NMLRIO.inc
.Patch1.7
         INCLUDE   NXNGIO.inc
         INCLUDE   NXCHIO.inc
.Begin patch 2.3
          Include   Nordio.inc
.end patch 2.3
         INCLUDE   COMLOGIC.inc

