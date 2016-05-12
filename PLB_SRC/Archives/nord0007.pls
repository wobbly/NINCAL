..NORD0007 - ORDER INQUIRY.
.           ON SCREEN DISKIN,
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         INCLUDE   NMLRDD.inc
         include   nbrkdd.inc
         INCLUDE   NDATDD.inc
.START PATCH 3.6 - ADDED LOGIC
         INCLUDE   NOFRDD.INC
.END PATCH 3.6 - ADDED LOGIC
release   init     "3.81"       26FEB2002 DLH - cleanup keyin time outs for PLBclient
.release   init     "3.8"       11FEB2000 ASH - Added code to filter Offer
.release   init     "3.7"       Jan00 DLH add denied Lcr status
.release   init     "3.6"       06May99 ASH replaced OODES{NINORD.DAT} --> OFDESC{NINOFR.DAT}
.release   init     "3.5"       10Feb99 DLH make next page options consistant with release 3.4
.Release  init      "3.4B"      02FEB99 JD End of file search default
.RELEASE  INIT      "3.4"     30DEC98 ASH NINORD Y2K, File expansion
.Release  init      "3.3"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.                            Currently it just suppresses them will need option to 
.                            select in the future
.Release   init     "3.2"    DLH 17Apr98 add (P)rior option at EOF.
.Release   init      "3.1"   DLH 30mar98 turn of Read locks on MLr & order & datacard
.release  init       "3.01"       10Apr97 DLH add some *lc's.
.release  init       "3.0"        Dec96 DLH allow date select by day
.release  init      "2.6"        16aug96 broker now part of order aim key DLH.
.release  init      "2.5"        12feb96 jd add broker help.
.release  init      "2.4"       27jul95 DLH order totals
.release  init      "2.3"       27Jun95 DLH allow secondary pick on Broker - primary mlr or list
.release  init      "2.2"       22may95 DLH display selection criteria and eoj
.                               if no records found
.Release  init      "2.1"       25JAN95 DLH force caps
.release  init      "2.0"          DLH 06MAY94 ALLOW SEARCH BY BRK.CNSLT ONLY
.                                 IE AS PRIMARY KEY.
.RELEASE  INIT      "1.91"        DLH 05nov93 display mailer number on screen.
.
.RELEASE  INIT      "1.9"         DLH 19OCT93 DISPLAY SPLIT ON RENT/EXCHANGE
.                                ORDERS.
.RELEASE  INIT      "1.8"         DLH 27SEP93 BEGIN ADDING BROKER/CONSULTANT
.                                SEARCH.
.RELEASE  INIT      "1.7"         DLH 20SEP93 ADDED SECONDARY CHECK FOR MLR KEY.
.      
.RELEASE   INIT     "1.6"         JD  10SEP93   EXPANDED MAILER PO KEYIN 12.
.RELEASE   INIT     "1.5"        DLH 08APR92   NORDXX, NDATXX, LISTHELP.INC
.
.RELEASE  INIT      "1.4"       D.L. HERRICK  19AUG1991
.                              CORRECTED MESSAGE IF RECORDS FOUND BUT NONE
.                              THAT MET CRITERIA, NEW MLR IO INCLUDE.
.
.RELEASE  INIT      "1.3"       DLH 22JUNE87  ADD LIST STATUS DISPLAY.
.
.RELEASE  INIT      "1.2"        DLH FEB 87 CONVERT TO RMS.
.
.RELEASE  INIT      "1.1"        DLH CONVERT TO DORDXXX.inc INCLUDES;.
.
.RELEASE  INIT      "1.0"         DLH CREATED.
.
. .............................................................................
.nord0007  PROGRAM VARIABLES .................
.
AllButton       CHECKBOX
BrokrgButton    CHECKBOX
ManageButton    CHECKBOX
GeneralResult   FORM    4
CheckValue      FORM    "0"
DisableValue    FORM    "0"
ordsel          form     1

KMAILER  DIM       4                  KEYIN FIELD FOR MAILER##;
KBRK     DIM       4
KLIST    DIM       6                  KEYIN FIELD FOR LIST##;
KMAILERP DIM       12                 KEYIN FIELD FOR MAILER PO#;
.START PATCH 3.8 - ADDED VAR
KOFFER   DIM       7
.END PATCH 3.8 - ADDED VAR
QUES     INIT      "????"
VTAB     FORM      2
VTAB1    FORM      2
VTAB2    FORM      2
VALKEY   FORM      1
HIT      FORM      1
DATESW   FORM      1
DATE     DIM       8
CHKMM    FORM      2                  HOLDS MONTH PARAMETER
CHKYR    FORM      2                  HOLDS YEAR PARAMETER
WORK02   DIM       2
MO       DIM       2                  DATE DISPLAY;
DAY      DIM       2                   "      "
YEAR     DIM       2                   "      "
PASS     FORM      3                  NUMBER OF RECORD DISPLAYS;
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
AKEY1    DIM       3                  USED TO BUILD MAILER & LIST FILE AIM KEY.
CHKMLR   DIM       4                  USED TO ELIMINATE DUP MAILER DISP.
KEYCOUNT FORM      2                  USED TO CHECK AIM KEY LENGTH, FREE FLOAT
VARIN    FORM      2
WSW      DIM       1            WITHDRAWN SWITCH DEFAULT NO
KEYFLAG    FORM      1            MLR KEY CHECK BRANCH 
.BRKFLAG  FORM      1             .1=NO BRK/CONSULTANT 2= YES BRK IS PRIMARY.
readflag form      1             .1=eadaimordai 2=readkgp
mlrflag  form       1            .1 =no mlr selected, 2=selected
listflag form      1              .1 = no list selected, 2=selected
ODATE    init      "Order Date "
MDATE    INIT      "Mail Date  "
RDate    init      "Return Date"
NDate    init      " NO  Date  "
LODATE   FORM      5         julian low order date   (yyjjj)
HIDATE   FORM      5         julian high order date  (yyjjj)
daterng  dim       1
ordtot   form      5
.Start patch #3.4 - increase var
qtytot   form      10
.End patch #3.4 - increase var
.
. .............................................................................
. MAINLINE
. .............................................................................
         KEYIN     *CL
         TRAP      EXIT1 IF F5
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
.         TRAPCLR   IO
         TRAP      IO GIVING ERROR NORESET IF IO
         Trap      Exit1 if F5
         TRAP      F3 IF F3
         CLOCK     DATE TO DATE
         IFNZ      PC
         UNPACK    DATE INTO MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         MOVE      C1 TO NDATPATH      .SET ACCESS TO ISI.
         move      c3 to nmlrlock    .DLH 30Mar98 set to no locks on read
         move      c3 to nordlock    .DLH 30Mar98 set to no locks on read
         move      c3 to ndatlock    .DLH 30Mar98 set to no locks on read
         GOTO      START
F3       TRAPCLR   F3
         DISPLAY   *RESETSW
         TRAP      F3 IF F3
         GOTO      START
.
START
         MOVE      "NORD0007" TO PROGRAM
         MOVE      "Names in the News Ca., Inc" TO COMPNME
         MOVE      "LIST USAGE INQUIRY" TO STITLE
         move      c2 to readflag
.        MOVE      C1 TO BRKFLAG              .default use aim key not brk
         CALL      PAINT
         move      c0 to n5                        .7/20/93 dlh
         move      c0 to qtytot
         move      c0 to ordtot
.START PATCH 3.8 - ADDED REPLACED LOGIC
.         DISPLAY   *P28:09,"1) MAILER##: ____":
.                   *P28:11,"2) LIST##:   ______":
.                   *P28:13,"3) MAILER PO ## ____________":
.                   *P28:15,"4) Brk/Cons ##: ____":
.                    *P28:17,"5) (",*cyan,"E",*white,")nd/(B)eginning of File: _":
..                  *P28:17,"5) (",*cyan,"B",*white,")eginning/(E)nd of File: _":
.                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
.                   " or (?) FOR HELP":
.                   *P20:22,"(F3) ESCAPE SEARCH",*HOFF
         DISPLAY   *P28:09,"1) MAILER##: ____":
                   *P28:11,"2) LIST##:   ______":
                   *P28:13,"3) MAILER PO ## ____________":
                   *P28:15,"4) Brk/Cons ##: ____":
                   *P28:17,"5) (",*cyan,"E",*white,")nd/(B)eginning of File: _":
.                  *P28:17,"5) (",*cyan,"B",*white,")eginning/(E)nd of File: _":
                   *P28:19,"6) OFFER##:  ____":
                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
                   " or (?) FOR HELP":
                   *P20:22,"(F3) ESCAPE SEARCH",*HOFF
.END PATCH 3.8 - ADDED REPLACED LOGIC
.
KEYIN0
         clear     omlrnum
         MOVE      C0 TO VARIN              *NUMBER OF VARIABLES IN
         move      c1 to mlrflag            *clear flag
.        CLEAR     KMAILER
.         KEYIN     *P40:09,*ZF,*JR,*T180,KMAILER
         KEYIN     *P40:09,*ZF,*JR,*T180,KMAILER,*p40:09,*DV,Kmailer
.begin patch 3.81
.         DISPLAY   *P40:09,KMAILER
          scan      star in kmailer
          goto      exit if equal
          reset     kmailer

          scan      "?" in Kmailer
         if        equal
         call      mlrhelp
         MOVE      MNUM TO KMAILER
         DISPLAY   *P40:09,KMAILER
         GOTO      DISMLR
         ENDIF

.         TYPE      KMAILER
          count     n2,kmailer

         if        (n2 < c1)
         clear     KMailer
         goto      keyin2
         endif
.         GOTO      KEYIN2 IF EOS
KEYIN1
         MOVE      C1 TO VARIN              *NUMBER OF VARIABLES IN
         move      c2 to mlrflag           *setflag
         CLEAR     MNUM
.         CMATCH    "*",KMAILER
.         GOTO      EXIT IF EQUAL
.         CMATCH    "?" TO KMAILER
.         if        equal
.         call      mlrhelp
.         MOVE      MNUM TO KMAILER
.         DISPLAY   *P40:09,KMAILER
.         GOTO      DISMLR
.         ENDIF
.         BUMP      KMAILER
.         GOTO      DISMLR IF EOS
.         GOTO      KEYIN1
.end patch 3.81
DISMLR
         MOVE      C1 TO VARIN              *NUMBER OF VARIABLES IN
         move      c2 to mlrflag           *setflag
         CLEAR     MNUM
         RESET     KMAILER
         MOVE      KMAILER,OMLRNUM
         MOVE      "000",OCOBN
         MOVE      "MAILER NOT FOUND " TO MCOMP
         CALL      MLRREAD
DISMLR1  MOVE      YES TO STR1
         KEYIN     *p40:09,*dv,kmailer,*P45:09,*DV,MCOMP:
                   *P75:09,"OK?",*T60,*RV,*uc,STR1,*lc;
         REPLACE   REPYN IN STR1
         CMATCH    NO,STR1

         If        equal
         clear     KMailer
         Goto      Keyin0
         endif
.         GOTO      KEYIN0 IF EQUAL
.         CMATCH    YES,STR1
.         GOTO      KEYIN2 IF EQUAL
.         GOTO      DISMLR1
KEYIN2
         CLEAR     KLIST
         move      c1 to listflag                  .clear flag
         KEYIN     *P40:11,*ZF,*JR,*T60,KLIST
         DISPLAY   *P40:11,KLIST
         MATCH     "00000<",KLIST
         GOTO      KEYIN0 IF EQUAL
         MATCH     "00000*" TO KLIST
         GOTO      EXIT IF EQUAL
         MATCH     "00000?" TO KLIST
         IF        EQUAL
         CALL      LISTHELP IF EQUAL
         CMATCH    B1 TO LSTNUM
         IF        NOT EOS
         MOVE      LSTNUM TO KLIST
         DISPLAY   *P40:11,KLIST
         ENDIF
         ENDIF
.begin patch 3.81
.         TYPE      KLIST
          count    n2,Klist
               If             (N2 = c0)
         clear     Klist
         goto      keyin3
         endif
.         GOTO      KEYIN3 IF EOS
.end patch 3.81
.         GOTO      READ1 IF EOS
.        ADD       C1 TO VARIN
DISLST
         RESET     KLIST
         PACK      NDATFLD FROM KLIST
         MOVE      C1 TO NDATPATH
         CALL      LSTREAD
DISLST1  MOVE      YES TO STR1
         KEYIN     *P46:11,*DV,OLSTNAME,*P75:11,"OK?",*T60,*RV,*uc,STR1,*lc;
         REPLACE   REPYN IN STR1
         CMATCH    NO,STR1
               If             Equal
               clear          Klist
               Goto           keyin2
               endif
.         GOTO      KEYIN2 IF EQUAL
         CMATCH    YES,STR1
         ADD       C1 TO VARIN
         move      c2 to listflag
.
KEYIN3
         CLEAR     KMAILERP
         KEYIN     *P42:13,*JL,*T60,KMAILERP,*P42:13,*el,*Dv,KMAILERP
.         DISPLAY   *P42:13,KMAILERP
         cmatch    b1 to kmailerp
         goto      keyin4 if equal
         GOTO      KEYIN4 IF EOS
         SCAN      "<" IN KMAILERP
         GOTO      KEYIN2 IF EQUAL
         RESET     KMAILERP
         SCAN      "*" IN KMAILERP
         GOTO      EXIT IF EQUAL
.        GOTO      KEYIN3X IF EOS
.        GOTO      KEYIN4 IF EOS
         ADD       C1 TO VARIN
KEYIN3X
         reset     kmailerp
.         BRANCH    VARIN OF READ1,READ1,READ1,READ1,READ1
         BRANCH    VARIN OF READ1                   .dh amuck 30jul96
         GOTO      keyin4
KEYIN4   CLEAR     KBRK
.         move      c1 to brkflag                      .clear flag
         KEYIN     *P42:15,*JR,*ZF,*T60,KBRK
         match     "000*" IN KBRK
         GOTO      EXIT IF EQUAL
         cmatch    b1 to kbrk
         goto      keyin5 if equal
         GOTO      KEYIN5 IF EOS
         match     "000<" IN KBRK
         GOTO      KEYIN3 IF EQUAL
         scan      "?" to KBRK
         IF        EQUAL
         pack      nbrkfld from kbrk,z3
         call      brkhelp
         move      brknum to kbrk
         endif
         DISPLAY   *P42:15,KBRK
         RESET     Kbrk
         MOVE      "NOT FOUND " TO brCOMP
         pack      nbrkfld from kbrk,z3
         CALL      nbrkkey
         if        over
         clear     kbrk
         endif
         MOVE      YES TO STR1
         KEYIN     *p42:15,*dv,kbrk,*P46:15,*DV,brCOMP:
                   *P75:15,"OK?",*T60,*RV,*uc,STR1,*lc;
         REPLACE   REPYN IN STR1
         CMATCH    NO,STR1
         GOTO      KEYIN4 IF EQUAL
         match     "000<" IN KBRK
         GOTO      KEYIN3 IF EQUAL
         match     "000*" IN KBRK
         GOTO      EXIT IF EQUAL
         ADD       C1 TO VARIN
KEYIN5  
         move      "E" to str1
         keyin     *P58:17,*T60,*RV,str1
         match     "000*" IN str1
         GOTO      EXIT IF EQUAL
         match     "000<" IN str1
         GOTO      KEYIN4 IF EQUAL
         rep        "B1E2" in str1
         move      str1 to readflag
         branch    readflag to keyin5a,keyin5a
         move      c2 to readflag
KEYIN5a  MOVE      YES TO STR1
         KEYIN     *P75:17,"OK?",*T30,*RV,*uc,STR1,*lc;
         REPLACE   REPYN IN STR1
.begin patch 3.81
               reset          str1
               if             (str1 = no)
               goto           keyin5
               endif
.         CMATCH    NO,STR1
.         GOTO      KEYIN5 IF EQUAL
.end patch 3.81
.START PATCH 3.8 - REMMED LOGIC
.         BRANCH    VARIN OF READ1,READ1,READ1,read1,read1
.         goto      exit
.END PATCH 3.8 - REMMED LOGIC

KEYIN5X 
.         type      kbrk
.         goto      read1 if not equal
.         move      c2 to brkflag
.         goto      readok
.         GOTO      EXIT
.
..............
.START PATCH 3.8 - REPLACED LOGIC
KEYIN6
         CLEAR     KOFFER
         KEYIN     *P40:19,*ZF,*JR,*T60,str3
         DISPLAY   *P40:19,str3
         MATCH     "00<",str3
         GOTO      KEYIN5 IF equal
         MATCH     "00*" TO str3
         GOTO      EXIT IF EQUAL
.         TYPE      str3
.begin patch 8.31
.         IF EOS
               if             (str3 = "")
.end patch 8.31

                   BRANCH    VARIN OF READ1,READ1,READ1,read1,read1
                   goto      exit
         ENDIF
DISOFFER
         RESET     str3
         call      Trim using str3
         if        (str3 = "")
                   clear        KOFFER
                   BRANCH    VARIN OF READ1,READ1,READ1,read1,read1
                   goto      exit
         endif
         pack      KOFFER,KMAILER,str3
         pack      NOFRFLD,KOFFER
         move      "DISOFFER-NOFRKEY",Location
         pack      KeyLocation,"Key: ",NOFRFLD
         call      NOFRKEY
         if over
                   DISPLAY      *P46:19,"OFFER NOT FOUND!";
                   GOTO KEYIN6
         endif
DISOFF1  MOVE      YES TO STR1
         KEYIN     *P46:19,*DV,OFDESC,*P75:19,"OK?",*T60,*RV,*uc,STR1,*lc;
         REPLACE   REPYN IN STR1
         CMATCH    NO,STR1
         GOTO      KEYIN6 IF EQUAL
         BRANCH    VARIN OF READ1,READ1,READ1,read1,read1
         goto      exit
.END PATCH 3.8 - REPLACED LOGIC
READ1
         CLEAR     NORDFLD1
         MOVE      "01R" TO AKEY1
.         APPEND    " 1R",KEY1
.         ENDSET    KEY1
         TYPE      KMAILER
         GOTO      READ1A IF not equal
         CMATCH    B1,KMAILER
         GOTO      READ1A IF EQUAL
         goto      read1a if  eos
.         APPEND    KMAILER,KEY1
.         ENDSET    KEY1
         PACK      NORDFLD1 FROM AKEY1,KMAILER
         GOTO      READ1B
.
.READ1A   APPEND    QUES,KEY1
READ1A
.         ENDSET    KEY1
.         RESET     KEY1
         SUB       C1 FROM VARIN
         CLEAR     NORDFLD1
.
READ1B
         CLEAR     NORDFLD2
.         APPEND    " 2R",KEY2
.         ENDSET    KEY2
         MOVE      "02R" TO AKEY1
         TYPE      KLIST
.begin patch 8.31
.         GOTO      READ1BB IF EOS
         GOTO      READ1BB IF not equal
.end patch 8.31
         CMATCH    B1,KLIST
         GOTO      READ1BB IF EQUAL
.         APPEND    KLIST,KEY2
.         ENDSET    KEY2
         PACK      NORDFLD2 FROM AKEY1,KLIST
         GOTO      READ1C
READ1BB
.         APPEND    QUES,KEY2
.         ENDSET    KEY2
.         RESET     KEY2
         CLEAR     NORDFLD2
READ1C
         CLEAR     NORDFLD3
         CMATCH    B1,KMAILERP
         GOTO      READ1CC IF EQUAL
         GOTO      READ1CC IF EOS
         MOVE      "03L" TO AKEY1
         PACK      NORDFLD3 FROM AKEY1,KMAILERP
         GOTO      READ1D
READ1CC
         CLEAR     NORDFLD3
.
read1d   
         CLEAR     NORDFLD4
         MOVE      "04R" TO AKEY1
         TYPE      Kbrk
         GOTO      READ1dd IF EOS
         CMATCH    B1,Kbrk
         GOTO      READ1dd IF EQUAL
         PACK      NORDFLD4 FROM AKEY1,Kbrk
         GOTO      READ1e
read1dd  clear     nordfld4

READ1e   CALL      KEYEDIT
         COMPARE   "3",VALKEY
         GOTO      NOGOOD IF EQUAL
         GOTO      READOK
.
.
NOGOOD
         DISPLAY   *P1:24,*HON,*EL,*B,"NOT ENOUGH INFORMATION TO SEARCH ON!!":
                   *W,*RESETSW:
                   *P20:21,*HON,"ENTER (*) TO EXIT, (<) TO BACKUP":
                   " or (?) FOR HELP",*HOFF;
         PAUSE     "3"
         GOTO      START
.
READOK
         CALL      DATECHK
         call      keychk
         call      ordsel
         MOVE      "0",HIT
         MOVE      "01",VTAB
         MOVE      "02",VTAB1
         MOVE      "03",VTAB2
.
         MOVE      "01",PASS
.         DISPLAY   *P1:24,*EL,"KEY1 ",NORDFLD1,"  KEY2 ",NORDFLD2:
.                   " KEY3 ",NORDFLD3,*B,*W3
.         compare   c2 to brkflag
.         goto      readok1 if equal
         call      rotdial
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G aim ",*HOFF:
                   ordtot,b1,qtytot;
         compare   c1 to readflag
         if        equal
        call      nordaim
         else
         CALL      NORDlast
         endif
         GOTO      NOMORE IF OVER
.START PATCH 3.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.6 - NEW LOGIC
         compare   c1 to ordsel               .all orders?
         if        not equal                  .no only brokerage or Management
         clear     str2
         pack      str2 from osales10,osales
         compare   c2 to ordsel
          if        equal
          match     "06" to str2
          goto      readkg if equal            .sorry its management, wanted BRk
          match     "19" to str2
          goto      readkg if equal            .sorry its management, wanted BRk
          endif
         compare   c3 to ordsel
          if        equal
          match     "06" to str2
          goto      readkg if not equal            .sorry its Brokerage, wanted Man
.          match     "19" to str2
.          goto      readkg if not equal            .sorry its Brokerage, wanted Man.
          endif
         endif
.START PATCH 3.8 - REPLACED LOGIC
         if (KOFFER <> "" & KOFFER <> OODNUM)
                  goto     readnext
         endif
.END PATCH 3.8 - REPLACED LOGIC
         branch    KEYFLAG of datebr,chkMkey
         goto      datebr
.
readok1  move      c3 to nordpath
         pack      nordfld4 from kbrk,b6
         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G isam",*HOFF;
         call      rotdial
         CALL      NORDtst
.        GOTO      NOMORE IF OVER
         goto      readnext
.
datebr   cmatch    yes to daterng
         if        equal
         BRANCH    DATESW OF ODATE1r,MDATE1r,RDATE1r,CALLMLR
         else
         BRANCH    DATESW OF ODATE1,MDATE1,RDATE1,CALLMLR
         endif
         DISPLAY   *P01:24,*EL,"BRANCH FAILURE AFTER READ!!!!!",*B,*W10,*B;
         STOP
chkMkey  match    str12 to omlrky
         goto     datebr if equal
.                
.readnext branch    brkflag of readnxt1,readbrk
.readnxt1 branch    readflag of readkg,readkgp
readnext branch    readflag of readkg,readkgp
         goto      readkg
readbrk  branch    readflag of readks,readkp
         goto       readks
READKG
         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G kg ",*HOFF:
                   ordtot,b1,qtytot;
         call      rotdial
         CALL       NORDKG
         GOTO      NOMORE2 IF OVER
.START PATCH 3.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.6 - NEW LOGIC
         DISPLAY   *P01:24,*EL;
.
         compare   c1 to ordsel               .all orders?
         if        not equal                  .no only brokerage or Management
         clear     str2
         pack      str2 from osales10,osales
         compare   c2 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          match     "19" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          endif
         compare   c3 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if not equal            .sorry its Brokerage, wanted Man
.          match     "19" to str2
.          goto      readnext if not equal            .sorry its Brokerage, wanted Man.
          endif
         endif
.START PATCH 3.8 - REPLACED LOGIC
         if (KOFFER <> "" & KOFFER <> OODNUM)
                  goto     readnext
         endif
.END PATCH 3.8 - REPLACED LOGIC
         branch    KEYFLAG of datebr1,chkMkey1
         goto      datebr1
READKgP
         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G kgp ",*HOFF:
                   ordtot,b1,qtytot;
         call      rotdial
         CALL       NORDKgp
         GOTO      NOMORE2 IF OVER
.START PATCH 3.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.6 - NEW LOGIC
         DISPLAY   *P01:24,*EL;
.
         compare   c1 to ordsel               .all orders?
         if        not equal                  .no only brokerage or Management
         clear     str2
         pack      str2 from osales10,osales
         compare   c2 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          match     "19" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          endif
         compare   c3 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if not equal            .sorry its Brokerage, wanted Man
.          match     "19" to str2
.          goto      readnext if not equal            .sorry its Brokerage, wanted Man.
          endif
         endif
.START PATCH 3.8 - REPLACED LOGIC
         if (KOFFER <> "" & KOFFER <> OODNUM)
                  goto     readnext
         endif
.END PATCH 3.8 - REPLACED LOGIC
         branch    KEYFLAG of datebr1,chkMkey1
         goto      datebr1
READKs
         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G ks ",*HOFF:
                   ordtot,b1,qtytot;
         call      rotdial
         CALL       NORDKs
         GOTO      NOMORE2 IF OVER
.START PATCH 3.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.6 - NEW LOGIC
         match     obrknum to kbrk
         goto      nomore2 if not equal
         DISPLAY   *P01:24,*EL;
.
         compare   c1 to ordsel               .all orders?
         if        not equal                  .no only brokerage or Management
         clear     str2
         pack      str2 from osales10,osales
         compare   c2 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          match     "19" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          endif
         compare   c3 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if not equal            .sorry its Brokerage, wanted Man
.          match     "19" to str2
.          goto      readnext if not equal            .sorry its Brokerage, wanted Man.
          endif
         endif
         branch    mlrflag to nomlrchk,domlrchk

nomlrchk branch    listflag to nolstck,dolstck
         goto       nolstck
domlrchk match     Kmailer to omlrnum
         goto      readnext if not equal 
         branch    listflag to nolstck,dolstck
dolstck  match     Klist to olnum
         goto      readnext if not equal 
nolstck  
.START PATCH 3.8 - REPLACED LOGIC
         if (KOFFER <> "" & KOFFER <> OODNUM)
                  goto     readnext
         endif
.END PATCH 3.8 - REPLACED LOGIC
         branch    KEYFLAG of datebr,chkMkey
         goto      datebr
.
         branch    KEYFLAG of datebr1,chkMkey1
         goto      datebr1
READKP
         DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G kp ",*HOFF:
                   ordtot,b1,qtytot;
         call      rotdial
         CALL       NORDKp
         GOTO      NOMORE2 IF OVER
.START PATCH 3.6 - NEW LOGIC
.EXTRACT OFFER DESCRIPTION FROM OFFER FILE AS OPPOSED TO RELYING ON NINORD.DAT
         bump      OODNUM,4
         pack      NOFRFLD,OMLRNUM,OODNUM
         reset     OODNUM
         move      "Rest-NOFRKEY",Location
         call      NOFRKEY
.END PATCH 3.6 - NEW LOGIC
         DISPLAY   *P01:24,*EL;
.
         compare   c1 to ordsel               .all orders?
         if        not equal                  .no only brokerage or Management
         clear     str2
         pack      str2 from osales10,osales
         compare   c2 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          match     "19" to str2
          goto      readnext if equal            .sorry its management, wanted BRk
          endif
         compare   c3 to ordsel
          if        equal
          match     "06" to str2
          goto      readnext if not equal            .sorry its Brokerage, wanted Man
.          match     "19" to str2
.          goto      readnext if not equal            .sorry its Brokerage, wanted Man.
          endif
         endif
         branch    mlrflag to nomlrchk,domlrchk
         goto      nomlrchk
.
datebr1  
         cmatch    yes to daterng
         if        equal
         BRANCH    DATESW OF ODATE1r,MDATE1r,RDATE1r,CALLMLR
         else
         BRANCH    DATESW OF ODATE1,MDATE1,RDATE1,CALLMLR
         endif
         DISPLAY   *P01:24,*EL,"BRANCH FAILURE AFTER READ!!!!!",*B,*W10,*B;
         STOP
.        
chkMkey1   match     str12 to omlrky
         goto      datebr1 if equal
         goto      readnext
.
ODATE1
         MOVE      OODTEM,NMM
         MOVE      OODTEY,NYY
OCHKYR
         COMPARE   CHKYR,NYY
         GOTO      OCHKMM IF ZERO
         GOTO      CALLMLR IF NOT LESS
         GOTO      readnext
OCHKMM
         COMPARE   CHKMM,NMM
         GOTO      CALLMLR IF ZERO
         GOTO      CALLMLR IF NOT LESS
         GOTO      readnext
.
MDATE1
         MOVE      OMDTEM,NMM
         MOVE      OMDTEY,NYY
MCHKYR
         COMPARE   CHKYR TO NYY
         GOTO      MCHKMM IF EQUAL
         GOTO      CALLMLR IF NOT LESS
         GOTO      readnext
MCHKMM
         COMPARE   CHKMM TO NMM
         GOTO      CALLMLR IF ZERO
         GOTO      CALLMLR IF NOT LESS
         GOTO      readnext
.
RDATE1
         MOVE      ORTNDTEM,NMM
         MOVE      ORTNDTEY,NYY
RCHKYR
         COMPARE   CHKYR TO NYY
         GOTO      RCHKMM IF EQUAL
         GOTO      CALLMLR IF NOT LESS
         GOTO      readnext
RCHKMM
         COMPARE   CHKMM TO NMM
         GOTO      CALLMLR IF ZERO
         GOTO      CALLMLR IF NOT LESS
         GOTO      readnext
.
Odate1r  MOVE      OODTEM,mm
         MOVE      OODTEY,yy
         MOVE      OODTEd,dd
         call      cvtjul
         GOTO      chkrng
mdate1r  MOVE      OMDTEM,Mm
         MOVE      OMDTEY,yy
         MOVE      OMDTEd,dd
         call      cvtjul
         GOTO      chkrng
rdate1r  MOVE      ORTNDTEM,mM
         MOVE      ORTNDTEY,yy
         MOVE      ORTNDTEd,dd
chkrng
         COMPARE   LODATE TO JULdays              CHECK IF IN DTE RNG.
         GOTO      readnext IF LESS              NO. TRY NEXT ORDER.
         COMPARE   JULdays TO HIDATE
         GOTO      readnext IF LESS              NO. TRY NEXT ORDER.
         goto      callmlr
CALLMLR
         ADD       C1 TO N5
         CALL      MLRREAD
         COMPARE   C1,PASS
         GOTO      CALLMLR1 IF EQUAL
         ADD       "4",VTAB
         ADD       "4",VTAB1
         ADD       "4",VTAB2
         COMPARE   "21",VTAB
         CALL      NEXTPAGE IF NOT LESS
CALLMLR1
         move      olnum to ndatfld
         move      c1 to ndatpath
         call      ndatkey
         CALL      DISSREC
         GOTO      readnext
.
DISSREC
         ADD       C1,PASS
.        DISPLAY   *P01:VTAB,*EF,"MAILER: ",MCOMP,*P35:VTAB,"OFFER: ":
.                  OODES,*P68:VTAB,*HON,"PO##: ",*HOFF,OMLRPON:
.                  *P01:VTAB1,*EL,"LIST: ",OLNUM,*P14:VTAB1,O1DES;
.05nov93 dlh
.START PATCH 3.6 - REPLACED LOGIC, OODES --> OFDESC
.         DISPLAY   *P01:VTAB,*EF,*yellow,"Mlr",*hoff,*white,MNUM,b1,MCOMP:
.                   *P35:VTAB,*yellow,"OFFER: ",*hoff,*white:
.                   OODES:
.                   *P68:VTAB,*yellow,"PO##: ",*HOFF,*white,OMLRPON:
.                   *P01:VTAB1,*EL,*yellow,"LIST: ",OLNUM,*hoff,*white:
.                   *P14:VTAB1,O1DES;
         DISPLAY   *P01:VTAB,*EF,*yellow,"Mlr",*hoff,*white,MNUM,b1,MCOMP:
                   *P35:VTAB,*yellow,"OFFER: ",*hoff,*white:
                   OFDESC:
                   *P68:VTAB,*yellow,"PO##: ",*HOFF,*white,OMLRPON:
                   *P01:VTAB1,*EL,*yellow,"LIST: ",OLNUM,*hoff,*white:
                   *P14:VTAB1,O1DES;
.END PATCH 3.6 - REPLACED LOGIC, OODES --> OFDESC
         cmatch    "W" to status
         if         equal
         display  *P01:VTAB1,*EL,*yellow,"LIST: ",*hoff:
                  *bgcolor=7,*black,OLNUM,*P14:VTAB1,O1DES,*bgcolor=1,*white;
.pcbus            *bkgnd white,*black,OLNUM,*P14:VTAB1,O1DES,*bkgnd blue,*white;
         endif
         reset    runcodes
         scan     olnum in runcodes
                 if       not equal
         reset   cancodes
         scan    ostat in cancodes
                 if not equal
.begin patch 3.3
         move    "lpz" to str3
         scan     ostat in str3      .pending or lcr?
.         cmatch  "p" to ostat          .pending or lcr?
                 if not equal          .no add them up
.end patch 3.3
.Start patch #3.4 - remmed and replaced logic
.                 move     c0 to n7
.                 move     oqty to n7
.                 add      n7 to qtytot
.                 add      c1 to ordtot
                 move     c0 to n9
                 move     oqty to n9
                 add      n9 to qtytot
                 add      c1 to ordtot
.End patch #3.4 - remmed and replaced logic
.begin patch 3.3
                         endif
.end patch 3.3
                         endif
                 endif
CHECK1
         CMATCH    "2",OELCODE
         GOTO      CHECKOK IF EQUAL
CHECK2
         CMATCH    "3",OELCODE
         GOTO      CHECKOK IF EQUAL
         GOTO      DISSCOM
CHECKOK  
.Start patch #3.4 - remmed and replaced logic
.         MOVE      C0 TO N7
.         MOVE      OEXQTY TO N7
.         COMPARE    C0 TO N7
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE    C0 TO N9
.End patch #3.4 - remmed and replaced logic
         GOTO       CHECKOK1 IF NOT EQUAL
         DISPLAY   *P55:VTAB1,*green,"EXCH",*white;
         GOTO      DISSCOM
CHECKOK1 DISPLAY    *P55:VTAB1,*red,"SPLT",*white;       
DISSCOM
         CMATCH    "C",OCOMSLCT
         GOTO      DISS1 IF NOT EQUAL
         DISPLAY   *P60:VTAB1,"COMSLCT";
.
DISS1
.Start patch #3.4 - ADD CENTURY
.         DISPLAY   *P01:VTAB2,*EL,*yellow,"LR##",*HOFF,*white,OLRN:
.                   *P11:VTAB2,*yellow,"ODTE: ",*hoff,*white,OODTEM,"/",OODTED,"/",OODTEY:
.                   *P27:VTAB2,*yellow,"SLCT: ",*hoff,*white,O2DES:
.                   *P70:VTAB2,*yellow,"QTY:",*hoff,*white,OQTY;
.         DISPLAY   *P68:VTAB1,*yellow,"MDTE:",*hoff,*white,OMDTEM,"/",OMDTED,"/",OMDTEY;
         DISPLAY   *P01:VTAB2,*EL,*yellow,"LR##",*HOFF,*white,OLRN:
                   *P11:VTAB2,*yellow,"ODTE: ",*hoff,*white,OODTEM,"/",OODTED,"/",OODTEC,OODTEY:
                   *P27:VTAB2,*yellow,"SLCT: ",*hoff,*white,O2DES:
                   *P68:VTAB2,*yellow,"QTY:",*hoff,*white,OQTY;
         DISPLAY   *P68:VTAB1,*yellow,"MDTE:",*hoff,*white,OMDTEM,"/",OMDTED,"/",OMDTEY;
.End patch #3.4 - ADD CENTURY
         CMATCH    "X" TO OSTAT
         GOTO      CANCEL IF EQUAL
         CMATCH    "Q" TO OSTAT
         GOTO      CANCEL IF EQUAL
.begin patch 3.3
         CMATCH    "x" TO OSTAT
         GOTO      CANCEL1 IF EQUAL
         CMATCH    "p" TO OSTAT
         GOTO      Pending IF EQUAL
.end patch 3.3
.begin patch 3.7
         CMATCH    "z" TO OSTAT
         GOTO      CANCEL2 IF EQUAL
         CMATCH    "l" TO OSTAT
         GOTO      lcr IF EQUAL
.end patch 3.4
         RETURN
CANCEL
         DISPLAY   *B,*P58:VTAB2,*HON,"*CANCELLED*",*HOFF,*HOFF;
         RETURN
.begin patch 3.3
CANCEL1
         DISPLAY   *B,*P58:VTAB2,*HON,"*Pend/Canc*",*HOFF,*HOFF;
         RETURN
CANCEL2
         DISPLAY   *B,*P58:VTAB2,*HON,"*LCR/Canc*",*HOFF,*HOFF;
         RETURN
Pending
         DISPLAY   *B,*P58:VTAB2,*HON,"*Pending*",*HOFF,*HOFF;
         RETURN
LCR      cmatch    "z" to ohist
         if         equal
         DISPLAY   *B,*P58:VTAB2,*HON,"*LCR/Denied",*HOFF,*HOFF;
         else
         DISPLAY   *B,*P58:VTAB2,*HON,"*LCR*",*HOFF,*HOFF;
         endif
         RETURN
.end patch 3.3
.
MLRREAD
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       zfill,MKEY
         CALL      NMLRKEY
         CALL      NOMAILER IF OVER
         RETURN
.
NOMAILER
         MOVE      "MAILER DESC. NOT FOUND",MNAME
         RETURN
.
LSTREAD  MOVE      C1 TO NDATPATH
         MOVE      KLIST TO NDATFLD
         CALL      NDATKEY
         CALL      LSTREAD1 IF OVER
         RETURN
.
LSTREAD1
         MOVE      "NOTE: NO LIST FOUND              ",OLSTNAME
         RETURN
.
NEXTPAGE
.begin patch 3.5
.         KEYIN     *P01:24,*EL,"ENTER to see the rest,":
.                   " (*) to exit, (P)rior":
.                   *B,*p45:24,*cyan,*dv,ordtot,*dv," Orders ",*dv,qtytot," Names":
.                   *white,*T200,*UC,*p44:24,STR1,*lc;
.         CMATCH    "*",STR1
.         GOTO      START IF EQUAL
.         cmatch    "P" to STR1
.         if        equal
.         move      c2 to readflag
.         else
.         move      c1 to readflag
.         endif
         if        (readflag = 1)
         KEYIN     *P01:24,*EL,"ENTER to see the rest,":
                   " (*) to exit, (P)rior":
                   *B,*p45:24,*cyan,*dv,ordtot,*dv," Orders ",*dv,qtytot," Names":
                   *white,*T254,*UC,*p44:24,STR1,*lc;
         CMATCH    "*",STR1
         GOTO      START IF EQUAL
         cmatch    "P" to STR1
         if        equal
         move      c2 to readflag
         else
         move      c1 to readflag
         endif
         else
         KEYIN     *P01:24,*EL,"ENTER to see the rest,":
                   " (*) to exit, (N)ext ":
                   *B,*p45:24,*cyan,*dv,ordtot,*dv," Orders ",*dv,qtytot," Names":
                   *white,*T254,*UC,*p44:24,STR1,*lc;
         CMATCH    "*",STR1
         GOTO      START IF EQUAL
         cmatch    "N" to STR1
         if        equal
         move      c1 to readflag
         else
         move      c2 to readflag
         endif
         endif
.end patch 3.5
         MOVE      "01",VTAB
         MOVE      "02",VTAB1
         MOVE      "03",VTAB2
         RETURN
.
NOMORE
         clear     str25
         load      str25 from datesw of ODate,MDate,RDate:
                   NDate
         clear     mcomp
         cmatch    b1 to kmailer
         if        not eos
         clear     mkey
         move      "000" to str3
         pack      mkeY from kmailer,str3
         move      c1 to nmlrpath
         call      nmlrkey
         endif
         clear     olstname
         clear     ndatfld
         move      klist to ndatfld
         cmatch    b1 to ndatfld
         if        not eos
         rep       zfill in ndatfld
         move      c1 to ndatpath
         call      ndatkey
         endif
.START PATCH 3.8 - REPLACED LOGIC
.         DISPLAY   *p1:4,*ef:
.                   *P28:09,"1) MAILER##: ":
.                   *P28:11,"2) LIST##:   ":
.                   *P28:13,"3) MAILER PO ## ":
.                   *P28:15,"4) Brk/Cons ##: ":
.                   *P40:09,KMAILER,b1,MCOMP:
.                   *p40:11,klist,*P47:11,OLSTNAME:
.                   *P42:13,KMAILERP:
.                   *P42:15,KBRK:
.                   *p28:17,"   Date(s)   ",str25,b1,chkmm,slash,chkyr:
.                   *p28:19,"   Mailers key  ",str12
         unpack    KOFFER,STR4,STR3
         DISPLAY   *p1:4,*ef:
                   *P28:09,"1) MAILER##: ":
                   *P28:11,"2) LIST##:   ":
                   *P28:13,"3) MAILER PO ## ":
                   *P28:15,"4) Brk/Cons ##: ":
                   *P40:09,KMAILER,b1,MCOMP:
                   *p40:11,klist,*P47:11,OLSTNAME:
                   *P42:13,KMAILERP:
                   *P42:15,KBRK:
                   *p28:17,"   Date(s)   ",str25,b1,chkmm,slash,chkyr:
                   *p28:19,"   Mailers key  ",str12:
                   *p28:21,"   OFFER        ",STR3
.END PATCH 3.8 - REPLACED LOGIC
         KEYIN     *P01:24,*EL,*B,"NO RECORDS FOUND !!!!!!",*T65,STR1;
         GOTO      START
.
NOMORE1
         clear     str25
         load      str25 from datesw of ODate,MDate,RDate:
                   NDate
         clear     mcomp
         cmatch    b1 to kmailer
         if        not eos
         clear     mkey
         move      "000" to str3
         pack      mkeY from kmailer,str3
         rep       zfill in mkey
         move      c1 to nmlrpath
         call      nmlrkey
         endif
         clear     olstname
         clear     ndatfld
         move      klist to ndatfld
         cmatch    b1 to ndatfld
         if        not eos
         rep       zfill in ndatfld
         move      c1 to ndatpath
         call      ndatkey
         endif
.START PATCH 3.8 - REPLACED LOGIC
.         DISPLAY   *p1:4,*ef:
.                   *P28:09,"1) MAILER##: ":
.                   *P28:11,"2) LIST##:   ":
.                   *P28:13,"3) MAILER PO ## ":
.                   *P28:15,"4) Brk/Cons ##: ":
.                   *P40:09,KMAILER,*P45:09,MCOMP:
.                   *p40:11,klist,*P47:11,OLSTNAME:
.                   *P42:13,KMAILERP:
.                   *P42:15,KBRK:
.                   *p28:17,"   Date(s)   ",str25,b1,chkmm,slash,chkyr:
.                   *p28:19,"   Mailers key  ",str12
         UNPACK    KOFFER,STR4,STR3
         DISPLAY   *p1:4,*ef:
                   *P28:09,"1) MAILER##: ":
                   *P28:11,"2) LIST##:   ":
                   *P28:13,"3) MAILER PO ## ":
                   *P28:15,"4) Brk/Cons ##: ":
                   *P40:09,KMAILER,*P45:09,MCOMP:
                   *p40:11,klist,*P47:11,OLSTNAME:
                   *P42:13,KMAILERP:
                   *P42:15,KBRK:
                   *p28:17,"   Date(s)   ",str25,b1,chkmm,slash,chkyr:
                   *p28:19,"   Mailers key  ",str12:
                   *p28:21,"   OFFER        ",STR3
.END PATCH 3.8 - REPLACED LOGIC
         KEYIN     *P01:24,*EL,*B,"NO RECORDS FOUND MEETING THE CRITERIA !!!!":
                   *T254,STR1;
         GOTO      START
NOMORE2
         COMPARE   C0 TO N5
         GOTO      NOMORE1 IF EQUAL
         KEYIN     *P01:24,*EL,"That's all I can find, hit ENTER (P)rior":
                   *B,*p45:24,*cyan,*dv,ordtot,*dv," Orders ",*dv,qtytot," Names":
                   *white,*T254,*UC,*p44:24,STR1,*lc;
         cmatch    "P" to STR1
         if        equal
         move      c2 to readflag
         goto      readnext
         endif
         GOTO      START
.
KEYEDIT
.
         MOVE      "0",VALKEY
         MATCH     "0000",KLIST
         CALL      CUME IF EQUAL
.
         MATCH     "0000",KMAILER
         CALL      CUME IF EQUAL
.
         MATCH     "0000",KMAILERP
         CALL      CUME IF EQUAL
         RETURN
.
CUME
         ADD       C1,VALKEY
         RETURN
.
DATECHK  MOVE      C4 TO STR1
         DISPLAY   *P01:08,*EF,*P28:08,"DATE SELECT OPTIONS":
                   *P28:09,"-------------------":
                   *P28:11,"1) ORDER DATE":
                   *P28:13,"2) MAIL DATE":
                   *P28:15,"3) RETURN DATE":
                   *P28:17,*CYAN,"4)",*WHITE," NO DATE SELECT, (*) to exit.":
                   *P28:22,"All date selects are from date entered forward";
.
         KEYIN     *P28:19,"YOUR CHOICE##: ",*T60,*RV,STR1;
         CMATCH    B1,STR1
         GOTO      DATECHK IF EQUAL
         GOTO      DATECHK IF EOS
         CMATCH    "*",STR1
         GOTO      START IF EQUAL
         MOVE      STR1,DATESW
         move      "M" to str1
         move      no to daterng
         KEYIN     *P28:11,*Ef,"by (",*cyan,"M",*white,")onth/year or (R)ange",str1
         rep       "mMrR" in str1
         cmatch    "R" to str1
         IF        NOT EQUAL
         BRANCH    DATESW OF ODATE,MDATE,RDATE,NODATE
         ELSE
         keyin    *p1:15,*el,"beginning date ",*jr,*zf,mm,"/",*jr,*zf,dd,"/",*jr,*zf,yy
         call     cvtjul
         move     juldays to lodate
         keyin    *p1:16,*el,"ending date    ",*jr,*zf,mm,"/",*jr,*zf,dd,"/",*jr,*zf,yy
         call     cvtjul
         move     juldays to hidate
         move     yes to daterng
         BRANCH    DATESW OF ODATE,MDATE,RDATE,NODATE
         ENDIF         
         GOTO      DATECHK
.
ODATE
         display   *P28:07,*EF,"SELECTION BY ORDER DATE "
         cmatch    yes to daterng
         return    if equal
         keyin     *P28:13,*ZF,*JR,"MONTH: ",*T60,WORK02
         DISPLAY   *P35:13,*EL,WORK02
         MATCH     "0*",WORK02
         GOTO      DATECHK IF EQUAL
         TYPE      WORK02
         GOTO      ODATE IF EOS
         MOVE      WORK02,CHKMM
         KEYIN     *P28:15,*ZF,*JR,"YEAR: ",*T60,WORK02
         DISPLAY   *P34:15,*EL,WORK02
         MATCH     "0*",WORK02
         GOTO      DATECHK IF EQUAL
         MATCH     "0<",WORK02
         GOTO      ODATE IF EQUAL
         MOVE      WORK02,CHKYR
         RETURN
.
MDATE
         display     *P28:07,*EF,"SELECTION BY MAIL DATE "
         cmatch    yes to daterng
         return    if equal
         keyin     *P28:13,*ZF,*JR,"MONTH: ",*T60,WORK02
         DISPLAY   *P34:13,*EL,WORK02
         MATCH     "0*",WORK02
         GOTO      DATECHK IF EQUAL
         MOVE      WORK02,CHKMM
         KEYIN     *P28:15,*ZF,*JR,"YEAR: ",*T60,WORK02
         DISPLAY   *P28:15,*EL,WORK02
         MATCH     "0*",WORK02
         GOTO      DATECHK IF EQUAL
         MOVE      WORK02,CHKYR
         RETURN
.
RDATE
         display   *P28:07,*EF,"SELECTION BY  RETURN DATE "
         cmatch    yes to daterng
         return    if equal
         keyin     *P28:13,*ZF,*JR,"MONTH: ",*T60,WORK02
         DISPLAY   *P34:13,*EL,WORK02
         MATCH     "0*",WORK02
         GOTO      DATECHK IF EQUAL
         MOVE      WORK02,CHKMM
         KEYIN     *P28:15,*ZF,*JR,"YEAR: ",*T60,WORK02
         DISPLAY   *P34:15,*EL,WORK02
         MATCH     "0*",WORK02
         GOTO      DATECHK IF EQUAL
         MOVE      WORK02,CHKYR
         RETURN
NODATE
         RETURN
keyCHK   DISPLAY   *P01:08,*EF,*P28:08,"SELECT by Mailers Key":
                   *P28:09,"-------------------":
                   *P28:17,*CYAN," Enter for NO KEY SELECT, (*) to exit.":
                   *white
.
         KEYIN     *P28:19,"KEY##: ",*T60,str12;
         move      c1 to KEYFLAG
         CMATCH    B1,str12
         RETURN    IF eos
         scan      "*",str12
         goto      start IF EQUAL
         reset     str12
         move      c2 To KEYFLAG
         return

ordsel 
        move            c1 to ordsel           default all
         COMPARE   "101" to portn         *wildcat client dial in ????
         IF       not  EQUAL
        move            c1 to ordsel           default all
.       CREATE          Allbutton=15:16:5:20,   "All Orders",FONT=">Script(20)"
        CREATE          Allbutton=15:16:5:20,   "&All Orders"
        CREATE          BrokrgButton=17:20:5:22, "&Brokerage Only"
        CREATE          manageButton=21:22:5:22, "&Management Only"
        ACTIVATE        ALLButton,AllAction,GeneralResult 
        ACTIVATE        BrokrgButton,BrokrgAction,GeneralResult 
        ACTIVATE        manageButton,ManageAction,GeneralResult 
        SETITEM         ALLButton, 0, 1
.
ordsel1 LOOP
        compare         c1 to generalresult
        if              equal
        destroy         allbutton
        destroy         brokrgbutton
        destroy         managebutton
        return
        endif          
        WAITEVENT
        REPEAT   
        STOP
        else
        display         *p15:5,"All orders will be selected",*b,*w3
        endif
.        
        return
.
* ***************************************************************************
*  EXIT AND FERROR SUBROUTINES
* ****************************************************************************
.
EXIT     DISPLAY   *P01:01,*ES,*P29:12,*HON,"B Y E !!!",*HOFF;
         TRAP      EXIT1 IF F5
EXIT1    BEEP
         STOP
IODIS    DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!",*B;
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      IODIS
IO       TRAPCLR   IO
         TRAP      IO GIVING ERROR NORESET IF IO
         NORETURN
IODIS1   DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!",*B;
         KEYIN     *P70:24,*EOFF,STR1,*RESETSW;
         CMATCH    "Q",STR1
         GOTO      EXIT IF EQUAL
         GOTO      IODIS1
RANGE
         TRAPCLR   RANGE
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         NORETURN
RANGED   DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      RANGED
         STOP
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      FORMAT
         STOP
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         KEYIN     *P70:24,*EOFF,STR1;
         CMATCH    "Q",STR1
         GOTO      EXIT1 IF EQUAL
         GOTO      PARITY
         STOP
AllAction
                IF              ( GeneralResult = 0 )
                 SETITEM        ALLButton, 0, 1
                 move  c1 to ordsel             .set to default
                 SETITEM        BrokrgButton, 0, 0
                 SETITEM        manageButton, 0, 0
                ENDIF
                RETURN
BrokrgAction
                IF              ( GeneralResult = 0 )
                 SETITEM        BrokrgButton, 0, 1
                 move  c2 to ordsel             .set to default
                 SETITEM        AllButton, 0, 0
                 SETITEM        manageButton, 0, 0
                ENDIF
                RETURN
ManageAction
                IF              ( GeneralResult = 0 )
                 SETITEM        manageButton, 0, 1
                 move  c3 to ordsel             .set to default
                 SETITEM        BrokrgButton, 0, 0
                 SETITEM        AllButton, 0, 0
                ENDIF
                RETURN
.
         INCLUDE   NMLRIO.inc
         INCLUDE   MLRHELP.inc
         include   nbrkio.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NDATIO.inc
         INCLUDE   LISTHELP.inc
         include   brkhelp.inc
.START PATCH 3.6 - ADDED LOGIC
         INCLUDE   NOFRIO.INC
.END PATCH 3.6 - ADDED LOGIC
         INCLUDE   COMLOGIC.inc

