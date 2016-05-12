*******************************************************************************
.NORD013f     "UNBILLED Management Exchange fee"
.NOte program works in 2 passes - first identifies orders that qualify
.second pass produces report
PC       EQU       0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NORDDD.INC
         INCLUDE   NOWNDD.INC
.patch1.88
                              include        compdd.inc
                              include        cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch1.88
         include  ndat3dd.inc
.         INCLUDE   NXCGDD.INC
         include   xMGtdd.inc
.patch1.88
.         include   nbrkdd.inc
.patch1.88
         include   hp.inc
.Begin patch 1.8
               Include        nshpdd.inc               
.end patch 1.8
.Begin Patch 2.32
	INclude	Ndatdd.inc
.End Patch 2.32
.begin patch 1.53
         include   consacct.inc
.end patch 1.53
.
Release	Init	"2.34"            DLH Proposed extra test, new starting LR
Reldate	Init	"01 July 2008"
.Release	Init	"2.33"            DLH Internal INdex & sort
.Reldate	Init	"23 April 2008"
.Release       Init           "2.32"            03Jan2008 DLH More PLI conversion
.				Need to firm up how to handle the PL rates add to Ndat3?????????
.Release       Init           "2.31"            17Sep2007 DLH PLI conversion
.				Exclude old PLI imported orders
.Release       Init           "2.30"            27Mar2007 DLH SEndMail
.Release       Init           "2.29"            01Mar2007 JD Turned On Interfaith Alliance rateten.
.Release       Init           "2.28"           31Aug2006 JD Turned On Unicef at 5.00. was 4.00
.Release        Init                          "2.27"                        Output fixed vars from  2.26
.Release        Init                          "2.26"                        16March06 DMS bug fix for 1.6
.Release       INit            "2.25"         13Feb06 DLH Excel output
.Release        Init           "2.24"             09jan2006 JD updated starting lr#.
.Release       Init           "2.23"              04jan2005 JD Turned off SC at 2.00 now 4.00
.Release        init           "2.22"             18Nov2005 JD New sunindex cmd, PLB90
.Release       Init           "2.21"              14Nov2005 JD Turned On Smile Train at 3.00. was 4.00
.Release       Init           "2.2"              30jun2005 JD Turned off Care at 2.00 now 4.00
.Release       Init           "2.10 "           30dec2004 JD Turned off Spol at 2.00 now 4.00
.Release       Init           "2.01 "           01Sep2004 JD Turned off TNC/NWF at 2.00 now 4.00
.Release       Init           "2.00 "           30JUL2004 JD added nwf/spoly at 2.00.
.Release       Init           "1.93"           19JUL2004 DLH Report Layout cleanup
.Release       Init           "1.92"           07JUL2004 DLH Played with message part (I hate SPam Server bug)
.Release       Init           "1.91"           06JUL2004 DLH new exch fee rate Add Minimum, Cap, & cleanup .
.Release       Init           "1.90"           02JUL2004 JD new exch fee rate.
.Release       Init           "1.88"          26MAY2004 DMB Mailer COnversion
.Release       Init           "1.86"          4May2004  DLH suppress DNC Using kerry for pres using
.Release        Init           "1.85"          29April2004  Forced Pop Connection 6476 to 4/m.
.Release       Init           "1.84"          12April2004  DLH suppress kerry for pres using DNC
.Release        Init           "1.83"          23February2004 DLH tweak splopen  Mode changed to "R" for new Laser8
.Release        Init           "1.82"          28jul2003 skipped cancelled orders.
.Release        Init           "1.81"           27jun2003 JD updated starting lr#.
.Release        Init           "1.8"           11Dec2002 DLH use Shipped qty if avail. if a split order use the same rent/exh
.                                             ratio and apply to shipped qty
.Release        Init           "1.7"          09May2002 JD added extra code tdmc billing for ending date check.
.Release        Init           "1.6"          29Apr2002 DLH add aging.
.RELEASE  INIT      "1.55"      31Jan2002 Jd updated starting lr # 1/2001
.RELEASE  INIT      "1.54"     20Aug2001 DLH update to sort32.exe
.RELEASE  INIT      "1.53"     08Aug2001 DLH rateten moved to consacct.inc
.RELEASE  INIT      "1.52"     11APR2001 JD added new rateten list,moved print to Laser8
.RELEASE  INIT      "1.51"     11APR2001 JD added new rateten list,moved print to Laser8
.RELEASE  INIT      "1.5"     02OCT2000 ASH NEW SERVER ADDED
.RELEASE  INIT      "1.4"     03SEP99 ASH EXCHMGNT Y2K
.release  init      "1.3"     03May99 DLH Add update mode
.release  init      "1.2"     13JAN99 ASH NINORD Y2K, File expansion
.Release  init      "1.1"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.release  init      "1.0"            DLH 26Jan98 
. .............................................................................
. WORK VARIABLES
.
PDATE    DIM       8
TOTDOLRS FORM      7.2
.begin patch 2.32
TOTPLDOLRS 	FORM      	7.2
TOTNDOLRS 	FORM      	7.2
.end patch 2.32
DOLLARS  FORM      10.4
totnames form      10
AR       FORM      7.2
. 
.
.begin patch 1.6
.UNBILINC FORM      7.2
UNBILINC       FORM           9.2
UNLNC30        FORM           9.2
UNLNC60        FORM           9.2
UNLNC90        FORM           9.2
UNLNC90P       FORM           9.2
UNBILTOT       FORM           12.2
.Begin Patch 2.32
UNPLBILINC       FORM           9.2
UNPLLNC30        FORM           9.2
UNPLLNC60        FORM           9.2
UNPLLNC90        FORM           9.2
UNPLLNC90P       FORM           9.2
UNPLBILTOT       FORM           12.2
UNNBILINC       FORM           9.2
UNNLNC30        FORM           9.2
UNNLNC60        FORM           9.2
UNNLNC90        FORM           9.2
UNNLNC90P       FORM           9.2
UNNBILTOT       FORM           12.2
.End Patch 2.32

daternge       form           5                    .Date of records wanted Julian
JanDays        Form           "31"
FebDays        Form           "28"
MarDays        Form           "31"
AprDays        Form           "30"
MayDays        Form           "31"
JunDays        Form           "30"
JulyDays        Form           "31"
AugDays        Form           "31"
SepDays        Form           "30"
OctDays        Form           "31"
NovDays        Form           "30"
DecDays        Form           "31"
CHKJUL         FORM           5
.end patch 1.6


mlrbrk   dim       4
mrgbrk   dim       12
brnum    dim       4
cntnum   dim       3
ordnum   form      4
cnthold  dim       3
mdate    dim       8
Datecalca form     5
. 
. .............................................................................
.
.
. PROGRAM VARIABLES
. .................
.
DATE     DIM       8
TIME     DIM       8
+ *****************************************************************************
FERROR   DIM       25                 ERROR MESSAGE DISPLAY FIELD.;
LINES    FORM      2
PAGE     FORM      5
PBREAK   FORM      "59"
COUNTO   FORM      7                  NUMBER OF ORDERS READ.
COUNTO1  FORM      5                  NUMBER OF ORDERS CALCULATED.
COUNTI   FORM      5                  NUMBER OF INVOICES READ
COUNTI1  FORM      5                  NUMBER OF INVOICES CALCULATED
SYSJDATE FORM      5
June04   form      5
OUTPUT   iFILE     KEYLEN=6
.begin patch 1.53
.moved to consacct.inc 8/8/01
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-014943-004493" $10 EXCHANGE MAN FEE LISTS
.added KCET  #004493          08/08/2001
.end patch 1.53
..RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476-014943" $10 EXCHANGE MAN FEE LISTS
.added Amer Humane  #14943          04/11/2001
.RATETEN  INIT      "005172-016291-016533-018246-018678-019534-018575-006476" $10 EXCHANGE MAN FEE LISTS
..5172 planned parenthood
..16291 PP Voter fund
..16533 PP High Dollar
..18246 PP Action Fund
..18678  High dollar democrats master
..19534  La Raza
.added ZPG #18575 & 6476          02/02/2000
.added La Raza #19534 11/30/99
.removed UCS Sep 99 DLH
.RATETEN  INIT      "005172-016291-016533-018246-018678-005670" $10 EXCHANGE MAN FEE LISTS
.JUne 99 KCET rate reduced to $2 per SMM
.RATETEN  INIT      "004493-005172-016291-016533-018246-018678-005670" $10 EXCHANGE MAN FEE LISTS
.                   PP, KCET,  UCS 9/1/98
.ncjw #015445 removed from list 9/24/98 DLH per SMM
xmode   form      1                  0 or 1 = add mode 2= update
.begin patch 1.3
written  form     5
updated  form     5
xmgtnewsw dim     1
.end patch 1.3
.begin patch 1.8
shipflag       Dim            1
splitcalc      form           10.4
.end patch 1.8
.begin patch 1.91
.xRATEsav  form 3.2
.end patch 1.91
.............................................................................................
.begin patch 2.25
.some excel goodies
sheetno    form      2
NumberofSheets Integer        4,"0x00000000"
RowNumber     Dim             9
books   automation
book    automation
sheets  automation
sheet   automation
Rowcol  automation
ex      automation      class="Excel.Application"
RecordHeader form 9
RecordTop form  9
N92     form    9.2
MailDate dim    4
VT_BOOL EQU 11          .Boolean
OTRUE   variant
OFALSE  variant
.end patch 2.25
.
. .............................................................................
. MAINLINE
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NORD013F" TO PROGRAM
         MOVE      "UNBILLED Exchange Fees" TO STITLE
          MOVE      "Names In The News" TO COMPNME
.
         CLOCK      DATE TO PDATE
         CLOCK     TIME TO TIME
         CALL      PAINT
         CALL      FUNCDISP
         move       c1 to nmlrpath
.begin patch 1.83
         Call           GetWinVer
.end patch 1.83
.begin patch 1.6
.START PATCH 2.26 REPLACED LOGIC
.               clock          date to today
.               unpack         today into mm,str1,dd,str1,yy
.               clock          timestamp to timestamp
.               unpack         timestamp into cc
.
DAVE2               clock          timestamp to timestamp
               unpack         timestamp into cc,YY,MM,DD
               move           C0,N2
               move           MM,N2
.END PATCH 2.26 REPLACED LOGIC
               load           str2 from n2 of JanDays,Febdays,Mardays,Aprdays,MayDays,JunDays,JulyDays,AugDays:
                              SepDays,OctDays,NovDays,DecDays
               move           Str2 to DD
               call           cvtjul
               move           juldays to daternge
               sub            c3 from daternge
.end patch 1.6
         GOTO      TRAPS


NOTHING  RETURN
.
TRAPS    TRAP      IO GIVING ERROR NORESET IF IO
         DISPLAY   *P1:24,*EL,"OPENING FILES";
         TRAP      RANGE GIVING ERROR NORESET IF RANGE
         TRAP      FORMAT GIVING ERROR NORESET IF FORMAT
         TRAP      PARITY GIVING ERROR NORESET IF PARITY
         MOVE      "input " TO FERROR
         move      c1 to nordpath
         MOVE      "                    " TO FERROR
         CALL      FUNCDISP
.         UNPACK    DATE INTO MM,STR1,DD,STR1,YY
         UNPACK    PDATE INTO MM,STR1,DD,STR1,YY
         CALL      CVTJUL
         MOVE      JULDAYS TO SYSJDATE
                              move      "30" to dd
                              move      "06" to mm
                              move      "04" to yy
         CALL      CVTJUL
         MOVE      JULDAYS TO JUne04
         TRAP      ABORT IF F5
.

START    MOVE      C0 TO LINES
         MOVE      C0 TO TOTDOLRS
.begin patch 2.32         
         	MOVE      	C0,TOTPLDOLRS
         	MOVE      	C0,TOTNDOLRS
.End patch 2.32         
.temp
.         goto      printit
.temp
.Start Patch #2.1 - increase file length
.         PREPARE   OUTPUT,"G:\DATA\UNBILLxG.DAT","G:\DATA\UNBILLXG.ISI","6","294"
.begin patch 1.3
.         PREPARE   OUTPUT,"G:\DATA\UNBILLxG.DAT","G:\DATA\UNBILLXG.ISI","6","328"
.START PATCH 1.5 REPLACED LOGIC
.         PREPARE   OUTPUT,"G:\DATA\UNBILLxG.DAT","G:\DATA\UNBILLXG.ISI","6","328",exclusive
         PACK      STR35,NTWKPATH1,"UNBILLxG.DAT"
         PACK      STR45,NTWKPATH1,"UNBILLXG.ISI"
         PREPARE   OUTPUT,STR35,STR45,"6","328",exclusive
.END PATCH 1.5 REPLACED LOGIC
.end patch 1.3
.End Patch #2.1 - increase file length
.
START1
.         move      "305233" to nordfld
.         move      "350000" to nordfld
.         move      "404000" to nordfld
.begin patch 1.91
.         move      "460000" to nordfld           
.         move      "485000" to nordfld              ;DLH 06July 2004
.         move      "510000" to nordfld              ;DLH 08 November 2004
.begin patch 2.24
         move      "550000" to nordfld               ;JD  09 Jan      2006
.end patch 2.24
.begin patch 2.32
.         move      "640000" to nordfld               
.end patch 2.32
.edn patch 1.91
.begin patch 2.34
         move      "665000" to nordfld               
.end patch 2.34
         call      nordkey
         ADD       C1 TO COUNTO
.         DISPLAY   *P1:12,*EL,"orders processed ",COUNTO;
         DISPLAY   *P1:12,*EL,"orders processed ",*p20:12,COUNTO,b2,olrn,b2,OODTEm,"/",OODTEd,"/",OODTEC,OODTEy
         DISPLAY   *P1:13,*EL,"records updated  ",*p20:13,Updated;
         DISPLAY   *P1:14,*EL,"records written  ",*p20:14,written;
.begin patch 1.1
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      GETREC IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       cancelled Pending order ?
         GOTO      GETREC IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      GETREC IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      GETREC IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 1.1
.begin patch 1.82
.begin patch 2.31
         type      olrn                 old PLI records ?
         GOTO      getrec IF not equal 
.end patch 2.31
.         
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         goto      getrec if equal
.end patch 1.82

         RESET      EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      GETREC1 IF EQUAL
          GOTO      GETREC
.
GETREC
.          DISPLAY   *P01:24,*EL,*HON,"S-E-A-R-C-H-I-N-G",*HOFF;
.begin patch 1.6
               call           RotDial
.end patch 1.6
.
         CALL      NORDks
         GOTO      output IF OVER
         ADD       C1 TO COUNTO
         DISPLAY   *P20:12,*EL,COUNTO,b2,olrn,b2,OODTEm,"/",OODTEd,"/",OODTEC,OODTEy:
                   *P20:13,*EL,Updated:
                   *P20:14,*EL,written;

.begin patch 1.1
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      GETREC IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       cancelled Pending order ?
         GOTO      GETREC IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      GETREC IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      GETREC IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 1.1
.begin patch 1.86
.begin patch 1.84
.               IF             (Omlrnum = "9498" & Obrknum = "0193" & olnum = "011947" & oodtey = "04")
               IF             (Omlrnum = "9498" & olnum = "011947" & oodtey = "04")
               goto           getrec
               ElseIF         (Omlrnum = "0071" & olnum = "021334" & oodtey = "04")
               goto           getrec
.end patch 1.86
               endif
.end patch 1.84
.begin patch 1.82
.begin patch 2.31
         type      olrn                 old PLI records ?
         GOTO      getrec IF not equal 
.end patch 2.31

         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         goto      getrec if equal
.end patch 1.82

        RESET      EXCODES
         SCAN      OELCODE IN EXCODES             EXCHANGE ?
         GOTO      GETREC1 IF EQUAL
         GOTO      GETREC
GETREC1
         move      olnum to ndat3fld
         rep       zfill in ndat3fld
         call      ndat3key
         goto      getrec if over
.start patch 1.7
         cmatch    yes to ndat3exh
         goto      CHKDTES if equal
         cmatch    no to ndat3exh
         if        equal
         cmatch    b1 to ndat3ex2   .do we still bill?
         goto      GETREC if eos     .NO
         goto      GETREC if equal   .NO
         unpack    ndat3ex2 to mm,dd,cc,yy      .DOES IT QUALIFY
         call      cvtjul
         move      c0 to datecalca
         move      juldays to datecalca
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         move      oodtec to cc
         call      cvtjul
         if        (JULDAYS <= DATECALCA)   .is order date BEFORE we stopped billing?
         goto      BILLED
         endif
         ENDIF
         goto     GETREC
chKDTES
.end patch 1.7
         clear     mm
         clear     dd
         clear     yy

         unpack    ndat3ex1 to mm,dd,cc,yy
         call      cvtjul
         move      c0 to Datecalca
         move      juldays to datecalca
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         move      oodtec to cc
         call      cvtjul
         if        (juldays < datecalca)            .is order date before
         goto      getrec            .we started billling? goto getrec if YES
         endif
         cmatch    b1 to ndat3ex2   .do we still bill?
         goto      billed if eos     .yes
         goto      billed if equal   .yes
         unpack    ndat3ex2 to mm,dd,cc,yy      .no does this one qualify?
         call      cvtjul
         move      c0 to datecalca
         move      juldays to datecalca
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         move      oodtec to cc
         call      cvtjul
         if        (datecalca < juldays)   .is order date after we stopped billing?
         goto      getrec            . goto getrec if YES
         endif

.********************************************************************
.add code for before no longer bill date  Here!!!!!!!!!!!!!!!!!!!!!!!
.********************************************************************

.Billed?
Billed
         MOVE       OLRN TO xmgtFLD
         REP        ZFILL IN xmgtFLD
         CALL       xmgtKEY
.begin patch 1.3
.level 1
               if         over
               move       yes to xmgtnewsw
               else
.begin patch 2.34
	Type	Xmgtord
	goto	Getrec if equal 
.end patch 2.34

               move       "U" to xmgtnewsw          .update
.begin patch 1.91
.               move       xmgtrate to xratesav      .7/2/04 
.end patch 1.91
               endif
.end level 1
..level 1 
.         IF         OVER
.end patch 1.3
         move      olrn to nordfld
         call      nordkey


.begin patch 2.32               .get rates from nindat3
	MOve	Ndat3ExRt,xmgtrate

.begin patch 1.91
.         move      oodtem to mm
.         move      oodted to dd
.         move      oodtey to yy
.         move      oodtec to cc
.         call      cvtjul
.          if        (juldays > june04)
.         MOVE       C4 TO xmgtRATE
.                             else
...begin patch 1.9
..         MOVE       C2 TO xmgtRATE                         ;MMMM not sure this is correct
..end patch 1.91
..                             endif
..end patch 1.9
.         RESET      RATETEN
.         SCAN       OLNUM IN RATETEN
..level 2        
.         IF         EQUAL
.         MOVE       C10 TO xmgtRATE
.         ENDIF
..end level 2
..begin patch 2.21   Turn on 11/05 turn off ????
..         match      "021666",Olnum
..                              if         equal
..                              move       c3 to xmgtrate
..                              endif
..end patch 2.21   Turn on 11/05 turn off ????
..begin patch 2.28   Turn on 09/06 turn off ????
..         match      "000995",Olnum
..                              if         equal
..                              move       c5 to xmgtrate
..                              endif
..end patch 2.28   Turn on 09/06 turn off ????
..End patch 2.32               .get rates from nindat3
.
.
.begin patch 1.9   Turn off 7/1/05
.         match      "011507",Olnum
.                             if         equal
.                             move       c2 to xmgtrate
.                             endif
.begin patch 1.9   Turn off 7/1/05
.         match      "011507",Olnum
.                             if         equal
.                             move       c2 to xmgtrate
.                             endif
.begin patch 1.9   Turn off 9/1/04                  
.         match      "002303",Olnum
.                             if         equal
.                             move       c2 to xmgtrate
.                             endif
.begin patch 2.0   Turn off 9/1/04                  
.         match      "020411",Olnum
.                             if         equal
.                             move       c2 to xmgtrate
.                             endif
.end patch 2.0
.begin patch 2.23   Turn off 1/1/06
.         match      "002700",Olnum
.                             if         equal
.                             move       c2 to xmgtrate
.                             endif
.end patch 2.23   Turn off 1/1/06
.begin patch 2.0   Turn off 1/1/05
.         match      "012594",Olnum
.                             if         equal
.                             move       c2 to xmgtrate
.                             endif
.end patch 2.0
.begin patch 1.3
         cmatch     yes to xmgtnewsw
         if         equal
         unpack     pdate into mm,str1,dd,str1,yy
         PACK       xmgtDATE FROM CC,YY,MM,DD
         endif
.end patch 1.3
.begin patch 1.8
               packkey        nshpfld,Olrn
               move           c3 to nshplock
               call           nshpkey
               if             over
               move           No to shipflag
               else
               Move           yes to shipflag               
               endif
.end patch 1.8

         move     c0 to n9 
         move     oexqty to n9
         compare  n9 to c0
.................
         if       equal
                  move      oqty to xmgtqty
                                 If          (shipflag = yes)
                                 move        Squant to xmgtqty
                                 endif
         else
                  MOVE      oexqty to xmgtQTY                 .mAKE SURE CORRECT QTY
                                 If          (shipflag = yes)
splittest                        move        oqty to n9
                                 move        oexqty to splitcalc
                                 calc        splitcalc=(splitcalc/n9)
                                 move        squant to n9
                                 calc        n9=(splitcalc*n9)
                                 move        n9 to xmgtqty
                                 endif
         endif
.END PATCH 1.4 - REINSTATED LOGIC
         move       olrn to xmgtlr
         MOVE       OLRN TO xmgtFLD
         REP        ZFILL IN xmgtFLD
         move       olon to xmgtown
         move       olnum to xmgtlist
.begin patch 1.3
.         CALL       xmgtWRT
         cmatch     yes to xmgtnewsw
         if         equal
xmwrt
         CALL       xmgtWRT
         add        c1 to written
         else
         TYPE       xmgtORD
         GOTO       GETREC IF EQUAL          .ITS IN THE FILE AND ORDER CREATED
xmupd
.begin patch 1.91
.         move       xratesav to xmgtrate
.end patch 1.91
         call       xmgtupd
         add        c1 to updated
         endif
.end patch 1.3
         GOTO       WRITE 
.begin patch 1.3
.         ENDIF
..end level 1
.end patch 1.3
         TYPE       xmgtORD
         GOTO       GETREC IF EQUAL          .ITS IN THE FILE AND ORDER CREATED
.UPDATE THE FILE WITH CORRECT QTY AND CONTINUE ON         
           
WRITE    WRITE      OUTPUT,oLRN;ORDVARS           
     
          GOTO      GETREC
.
*......................................................................
HEADER   ADD       C1 TO PAGE
         PRINT     *F,*L,*1,hpdtch12,"CONFIDENTIAL   N I N   U N B I L L":
                   " E D   ExChange charge   O R D E R S",hpt650,"DATE:",PDATE:
                     *L,*1,PROGRAM,hpt650,"PAGE ## ",PAGE:
                   *L,*L,*1,hpdtch10,"MAILER / List":
.Begin patch 1.93
.                   *L,*L,*1,hpdtch10,"MAILER ",hpt225,"List":
.                   hpt450,"Order/Rtn",hpt550," LR     ","Quantity/Exchange Qty"  
                   hpt350,"Order/Rtn",hpt450," LR ",hpt500,"Quantity/Exchange Qty":
                   hpt725,"Rate"
.end patch 1.93
         MOVE      C5 TO LINES
         RETURN
.output - close sort and reopen file for printing
.
output   Close     output
         pause     "5"
.START PATCH 1.5 REPLACED LOGIC
.         execute   "f:\netutils\sort g:\data\unbillxg.dat,g:\data\unbillxg.srt /s(22,4,C,A,16,6,C,A,3,4,C,A) ver"
.         if        not over
.         Erase     "g:\data\unbillxg.dat"
.begin patch 1.54
.begin patch 2.33
.         PACK      TASKNAME,NTWKPATH2,"sort ",NTWKPATH1,"unbillxg.dat,",NTWKPATH1,"unbillxg.srt /s(22,4,C,A,16,6,C,A,3,4,C,A) ver"
.         PACK      TASKNAME,NTWKPATH2,"sort32 ",NTWKPATH1,"unbillxg.dat,",NTWKPATH1,"unbillxg.srt /s(22,4,C,A,16,6,C,A,3,4,C,A) ver"
         PACK      TASKNAME,NTWKPATH1,"unbillxg.dat,",NTWKPATH1,"unbillxg.srt,c:\work -22-25,16-21,3-6"
.end patch 1.54
	Sort	Taskname
.         execute   TASKNAME
.end patch 2.33
         if        not over
         PACK      STR35,NTWKPATH1,"unbillxg.dat"
         Erase     STR35
.END PATCH 1.5 REPLACED LOGIC
         Else
         Display   *p1:24,*el,"sort BOmbed",S$ERROR$,*w40
         endif 
rename   trapclr    io
         trap       rename if IO
         Display    *p1:24,*el,"renaming unbillxg.srt to .dat"
.START PATCH 1.5 REPLACED LOGIC
.         rename     "g:\data\unbillxg.srt","g:\data\unbillxg.dat"
         PACK      STR35,NTWKPATH1,"unbillxg.SRT"
         PACK      STR45,NTWKPATH1,"unbillxg.dat"
         rename     STR35,STR45
.END PATCH 1.5 REPLACED LOGIC
         trapclr     io
         TRAP      nothing if IO 
         Display    *p1:24,*el,"reINDEXING unbillxg"
.START PATCH 1.5 REPLACED LOGIC
.         execute   "f:\netutils\sunidxnt.exe g:\data\unbillxg.dat,g:\data\unbillxg.isi -7-12"
.         PACK      TASKNAME,NTWKPATH2,"sunidxnt.exe ",NTWKPATH1,"unbillxg.dat,",NTWKPATH1,"unbillxg.isi -7-12"
.START PATCH 2.22
.begin patch 2.33
.         PACK      TASKNAME,"f:\apps\plb\code\sunindex.exe ",NTWKPATH1,"unbillxg.dat,",NTWKPATH1,"unbillxg.isi -7-12"
	PACK      TASKNAME,NTWKPATH1,"unbillxg.dat,",NTWKPATH1,"unbillxg.isi -7-12"
.end PATCH 2.22
	INdex	TASKNAME
.         execute   TASKNAME
.end patch 2.33
.END PATCH 1.5 REPLACED LOGIC
printit
         Display    *p1:24,*el,"PRINTING"

.START PATCH 1.5 REPLACED LOGIC
.          open       output,"g:\data\unbillxg"
          PACK       STR35,NTWKPATH1,"unbillxg"
          open       output,STR35
.END PATCH 1.5 REPLACED LOGIC
.begin patch 1.83
               if             (osflag = c1 or Osflag = C5 or OsFlag = C6)         .nt or win2000 or Windows XP
               splopen        "\\NTS0\Laser8","R"
               Elseif         (osflag = c3 or OsFlag = C4)         .win 95 98
               splopen        "\\NTS0\Laser8","R"
               Elseif         (osflag = c0)         .Don't know prompt for printer
               splopen        "","R"
               endif
.          Splopen    "\\nts0\Laser8","A"
.          Splopen    "","A"
.          Splopen    "\\nts0\Laser2","A"
          PRINT     hpreset,hpdupL,*F
          move       c0 to lines
          move       c0 to counto1
          
Output1
         read       output,seq;ordvars
         goto      exit if over
         CALL      ROTDIAL
         add       c1 to counto1
         DISPLAY   *P10:15,*EL,"printing record : ",COUNTO1;
.begin patch 1.9
         MOVE       OLRN TO xmgtFLD
         REP        ZFILL IN xmgtFLD
         CALL       xmgtKEY
.end patch 1.9
.begin patch 2.32
	PackKey	Ndatfld,Olnum
	REp	Zfill,ndatfld
	move	c1,ndatpath
	call	NdatKey
.End patch 2.32

.begin patch 1.85
.         MOVE       C2 TO xmgtRATE
.         RESET      RATETEN
.         SCAN       OLNUM IN RATETEN
..level 2        
.         IF         EQUAL
.         MOVE       C10 TO xmgtRATE
.         ENDIF
.         match      "006476",Olnum
.         if         equal                                                           
.         move       c4 to xmgtrate
.         endif                              
*......................................................................
PRINT
         COMPARE   PBREAK TO LINES
         CALL      HEADER IF NOT LESS
         COMPARE   C0 TO LINES
         CALL      HEADER IF EQUAL
         MOVE       C0 TO N2
         CLEAR    MKEY
         APPEND   OMLRNUM TO MKEY
         APPEND   "000" TO MKEY
         RESET    MKEY
         REP     ZFILL IN MKEY
         call     nmlrkey 
         MOVE     C1 TO NOWNPATH
         MOVE     OLON TO NOWNFLD
         REP      ZFILL IN NOWNFLD
         CALL     NOWNKEY 
.Start Patch #1.2 - added century
.         PRINT     *1,hpdtch85:
.                   mcomp,HPT225,hpdtch06,o1des,hpdtch85,hpt450,oOdtem,"/",oOdted,"/",oOdtey:
.                   b1,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEy,hpt550,hpdtch06,olrn,b2,oqty,"/",oexqty,b1,xmgtRATE,hpdtch85
.Begin patch 1.93
         PRINT     *1,hpdtch85,mcomp,hpt350,oOdtem,"/",oOdted,"/",OODTEC,oOdtey:
                   hpt450,olrn,hpt500,oqty,"/",oexqty,hpt725,xmgtRATE,hpdtch85:
                   *L,*1,Elstcde,*5,o1des,hpt350,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEC,ORTNDTEy
.                   mcomp,HPT225,hpdtch06,o1des,hpdtch85,hpt450,oOdtem,"/",oOdted,"/",OODTEC,oOdtey:
.                   b1,ORTNDTEM,"/",ORTNDTEd,"/",ORTNDTEC,ORTNDTEy,hpt550,hpdtch06,olrn,b2,oqty,"/",oexqty,b1,xmgtRATE,hpdtch85
.End Patch #1.2 - added century
.         ADD       C1 TO LINES
         ADD       C2 TO LINES
.end patch 1.93
           move     obrknum to brnum
         move     obrkcnt to cntnum
.Start Patch #1.2 - replaced var
.         move     c0 to n6 
.         move     oexqty to n6
.         compare  n6 to c0
.         if       equal
.         move     c0 to n6 
.         move      oqty to n6
.         add      n6 to totnames
.         else
.         add      n6 to totnames
.         MOVE     n6 TO xmgtQTY                 .mAKE SURE CORRECT QTY
.         endif
.         MOVE     N6 TO DOLLARS
.
         move     c0 to n9 
         move     oexqty to n9
         compare  n9 to c0
         if       equal
         move     c0 to n9 
         move      oqty to n9
         add      n9 to totnames
         else
         add      n9 to totnames
.START PATCH 1.4 - REINSTATED LOGIC
..START MINIPATCH #1.2 - CANNOT MOVE N9 TO XMGTQTY UNTIL XMGTQTY IS 9 BYTES LONG!!
..         MOVE     n9 TO xmgtQTY                 .mAKE SURE CORRECT QTY
.          count   n1,oexqty
.                  if (n1 = 9)
.                           bump      oexqty,c2
.                           move      oexqty to xmgtqty
.                           reset     oexqty
.                  elseif (n1 = 8)
.                           bump      oexqty,c1
.                           move      oexqty to xmgtqty
.                           reset     oexqty
.                  else
.                           move      oexqty to xmgtqty
.                  endif
..END MINIPATCH #1.2 - CANNOT MOVE N9 TO XMGTQTY UNTIL XMGTQTY IS 9 BYTES LONG!!
..............
                  MOVE     n9 TO xmgtQTY                 .mAKE SURE CORRECT QTY
.END PATCH 1.4 - REINSTATED LOGIC
         endif
         MOVE     N9 TO DOLLARS
.End Patch #1.2 - replaced var
         DIVIDE   "1000" INTO DOLLARS
         MULT      xmgtRATE BY DOLLARS
.begin patch 1.91
               If             (Dollars < 15)
               Move           "15.00" to Dollars
               endif
               If             (XmgtRate <> 10 & Dollars > 400)        ;if its not List Management only check for $400 cap
               Move           "400.00" to Dollars
               endif               
.end patch 1.91
               
         ADD       DOLLARS TO TOTDOLRS
.begin patch 2.32         
	IF	(eLSTCDE = "P")
	add	Dollars,TotPLDolrs
	else
	add	Dollars,totNDolrs
	Endif
.begin patch 2.32         
.begin patch 1.6
               clear      mm
               clear      dd
               clear      yy
               move       ortndtem to mm
               move       ortndted to dd
               move       ortndtey to yy
               move       c0 to juldays
               call       cvtjul


.               sub            DateRnge from Juldays
.
SAM              if             (juldays <= Daternge)
               goto           Bill30
               endif
.               COMPARE        c0 TO JULDAYS
.               GOTO           BILL30 IF LESS          *SHOULD BE BILLED THIS MONTH.
.
               if             (juldays <= (Daternge + "31"))
               goto           Bill60
               endif

.               COMPARE        C31 TO JULDAYS
.               GOTO           BILL60 IF LESS
.
               if             (juldays <= (Daternge + "61"))
               goto           Bill90
               endif

.               COMPARE        "61" TO JULDAYS         *
.               GOTO           BILL90 IF LESS
               ADD            Dollars TO UNLNC90P
               Goto           PrintExit

.
Bill30         add            Dollars to UNLNC30
.begin patch 2.32
	if	(Elstcde = "P")
	add            Dollars to UNPLLNC30
	Else
	add            Dollars to UNNLNC30	
	Endif
.End patch 2.32
               goto           PrintExit
Bill60         add            Dollars to UNLNC60
.begin patch 2.32
	if	(Elstcde = "P")
	add            Dollars to UNPLLNC60
	Else
	add            Dollars to UNNLNC60	
	Endif
.End patch 2.32
               goto           PrintExit
Bill90         add            Dollars to UNLNC90
.begin patch 2.32
	if	(Elstcde = "P")
	add            Dollars to UNPLLNC90
	Else
	add            Dollars to UNNLNC90	
	Endif
.End patch 2.32
               goto           PrintExit
.
PrintExit
.end patch 1.6
         goto     output1
* ***************************************************************************
*  EXIT
* ****************************************************************************
.
EXIT
	Move	"Nord013f - output",MailSubjct
	Move	"DavidHerrick@nincal.com",MailFrom
	Move	"DavidHerrick@nincal.com",MailTo
	Clear	MailBody
            Append         "Start Time ",Mailbody   Array <Text message >
            Append         Time,Mailbody   Array <Text message >
            Append         b1,Mailbody   Array <Text message >
            Append         counto,Mailbody   Array <Text message >
            Append         " Orders Examined",Mailbody   Array <Text message >
	Append	CRLF,MailBOdy
.
            Append         "Total Dollars: ",Mailbody   Array <Text message >
            Append         TotDolrs,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "30 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnLnc30,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "60 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnLnc60,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "90 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnLnc90,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "Over 90 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnLnc90P,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         b25,Mailbody   Array <Text message >
            Append         b25,Mailbody   Array <Text message >
            Append         b25,Mailbody   Array <Text message >
            Append         b25,Mailbody   Array <Text message >
            Append         b25,Mailbody   Array <Text message >
            Append         b25,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy
.begin patch 2.32
.
            Append         "PL Total Dollars: ",Mailbody   Array <Text message >
            Append         TotPLDolrs,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "PL 30 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnPLLnc30,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "PL 60 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnPLLnc60,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "PL 90 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnPLLnc90,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "PL Over 90 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnPLLnc90P,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         b25,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy
            Append         "NIN  Total Dollars: ",Mailbody   Array <Text message >
            Append         TotNDolrs,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "NIN  30 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnNLnc30,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "NIN  60 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnNLnc60,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "NIN  90 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnNLnc90,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         "NIN  Over 90 Day Dollars: ",Mailbody   Array <Text message >
            Append         UnNLnc90P,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy

            Append         b25,Mailbody   Array <Text message >
	Append	CRLF,MailBOdy
.end Patch 2.32
	reset	Mailbody

.                    Move    "NTS4.nincal.com",SmtpEmailServer                   Address of email server
.              move          "Nord013F - output" to SmtpSubject Subject
.              append         "Start Time ",SmtpTextMessage(1)   Array <Text message >
.              append         Time,SmtpTextMessage(1)   Array <Text message >
.              append         b1,SmtpTextMessage(1)   Array <Text message >
.              append         counto,SmtpTextMessage(1)   Array <Text message >
.              append         " Orders Examined",SmtpTextMessage(1)   Array <Text message >
.              reset          smtpTextMessage(1)

..begin patch 1.6
.               Clear          SmtpTextMessage(2)
.              Append         "Total Dollars: ",SmtpTextMessage(2)   Array <Text message >
.              Append         TotDolrs,SmtpTextMessage(2)   Array <Text message >
.               Reset          SmtpTextMessage(2)
.
.               Clear          SmtpTextMessage(3)
.              Append         "30 Day Dollars: ",SmtpTextMessage(3)   Array <Text message >
.              Append         UnLnc30,SmtpTextMessage(3)   Array <Text message >
.               Reset          SmtpTextMessage(3)
.
.               Clear          SmtpTextMessage(4)
.              Append         "60 Day Dollars: ",SmtpTextMessage(4)   Array <Text message >
.              Append         UnLnc60,SmtpTextMessage(4)   Array <Text message >
.               Reset          SmtpTextMessage(4)
.
.               Clear          SmtpTextMessage(5)
.              Append         "90 Day Dollars: ",SmtpTextMessage(5)   Array <Text message >
.              Append         UnLnc90,SmtpTextMessage(5)   Array <Text message >
.               Reset          SmtpTextMessage(5)
.
.               Clear          SmtpTextMessage(6)
.              Append         "Over 90 Day Dollars: ",SmtpTextMessage(6)   Array <Text message >
.              Append         UnLnc90P,SmtpTextMessage(6)   Array <Text message >
.               Reset          SmtpTextMessage(6)
.
.;begin patch 1.92
.               Clear          SmtpTextMessage(7)
.              Append         b25,SmtpTextMessage(7)   Array <Text message >
.              Append         b25,SmtpTextMessage(7)   Array <Text message >
.              Append         b25,SmtpTextMessage(7)   Array <Text message >
.              Append         b25,SmtpTextMessage(7)   Array <Text message >
.              Append         b25,SmtpTextMessage(7)   Array <Text message >
.              Append         b25,SmtpTextMessage(7)   Array <Text message >
.               Reset          SmtpTextMessage(7)
..
.              Move           "7",SmtpTextIndexLast                               Index to last entry in TextMessage array
.;             Move           "6",SmtpTextIndexLast                               Index to last entry in TextMessage array
.;end patch 1.92
..             Move           "3",SmtpTextIndexLast                               Index to last entry in TextMessage array
..end patch 1.6
.              move       "DHerric" to str45
.              move       "David Herrick" to str55
.              call       Mailmesg
	call	sendmail
         PRINT     *L,"START TIME ",TIME," ",COUNTO,"  ORDERS EXAMINED"
         CLOCK     TIME TO TIME
         PRINT     *L,"END TIME   ",TIME," ",COUNTO1,"  ORDERS PRINTED":
                   HPT550,TOTNAMES,B1,TOTDOLRS
         BEEP
         SPLCLOSE
         release 
.begin patch 2.25
.Open Excel application
OhMy
              Create  ex
              rep	zfill,str4
              pack            Taskname,"\\nins1\d\users\dherric\monday2008.xls"
.              pack            Taskname,"\\nins1\d\users\dherric\monday.xls"       
              getprop ex,*Workbooks=books
              Books.Open using *Filename=taskname   
                    books.item giving book using 1
              getprop book,*Sheets=sheets
.              getprop book,*workSheets=sheets
.Reset Default of Worksheets found in a Workbook         .eight
.should try getting the property here and reseting it when done.
        getprop ex,*SheetsInNewWorkbook=NumberofSheets
.Create Workbooks collection
        clock   timestamp,str8
        unpack  str8,str2,yy,mm,dd
        clear   str10
        pack    str10,mm,slash,dd,slash,str2,yy
        clock   time to time
        move      mm to sheetno
              if              (sheetno = c1)
              sheets.item giving sheet using 1
              Elseif          (sheetno = c2)
              sheets.item giving sheet using 2
              Elseif          (sheetno = c3)
              sheets.item giving sheet using 3
              Elseif          (sheetno = c4)
              sheets.item giving sheet using 4
              Elseif          (sheetno = c5)
              sheets.item giving sheet using 5
              Elseif          (sheetno = c6)
              sheets.item giving sheet using 6
              Elseif          (sheetno = c7)
              sheets.item giving sheet using 7
              Elseif          (sheetno = c8)
              sheets.item giving sheet using 8
              Elseif          (sheetno = c9)
              sheets.item giving sheet using 9
              Elseif          (sheetno = 10)
              sheets.item giving sheet using 10
              Elseif          (sheetno = 11)
              sheets.item giving sheet using 11
              Elseif          (sheetno = 12)
              sheets.item giving sheet using 12
              endif
        pack    RowNumber,"C","33"
        setprop sheet.range(RowNumber),*Value=str10
        pack    RowNumber,"D","33"
        setprop sheet.range(RowNumber),*Value=Time
        pack    RowNumber,"A","33"
        setprop sheet.range(RowNumber),*Value="Management Exch Fee Total"
        pack    RowNumber,"B","33"
        setprop sheet.range(RowNumber),*Value=TotDolrs,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","34"
        setprop sheet.range(RowNumber),*Value="Management Exch Fee 30 day"
.begin patch 2.27
        pack    RowNumber,"B","34"
        setprop sheet.range(RowNumber),*Value=UnLnc30,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","35"
        setprop sheet.range(RowNumber),*Value="Management Exch Fee 60 day"
        pack    RowNumber,"B","35"
        setprop sheet.range(RowNumber),*Value=UnLnc60,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","36"
        setprop sheet.range(RowNumber),*Value="Management Exch Fee 90 day"
        pack    RowNumber,"B","36"
        setprop sheet.range(RowNumber),*Value=UnLnc90,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"A","37"
        setprop sheet.range(RowNumber),*Value="Management Exch Fee 90+ day"
        pack    RowNumber,"B","37"
        setprop sheet.range(RowNumber),*Value=UnLnc90p,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
.end patch 2.27
.begin patch 2.32
        pack    RowNumber,"E","33"
        setprop sheet.range(RowNumber),*Value=TotNDolrs,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","34"
        setprop sheet.range(RowNumber),*Value=UnNLnc30,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","35"
        setprop sheet.range(RowNumber),*Value=UnNLnc60,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","36"
        setprop sheet.range(RowNumber),*Value=UnNLnc90,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"E","37"
        setprop sheet.range(RowNumber),*Value=UnNLnc90p,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","33"
        setprop sheet.range(RowNumber),*Value=TotPLDolrs,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","34"
        setprop sheet.range(RowNumber),*Value=UnPLLnc30,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","35"
        setprop sheet.range(RowNumber),*Value=UnPLLnc60,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","36"
        setprop sheet.range(RowNumber),*Value=UnPLLnc90,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"
        pack    RowNumber,"F","37"
        setprop sheet.range(RowNumber),*Value=UnPLLnc90p,*Numberformat="$##,####0.00_);[Red]($##,####0.00)"

.End patch 2.32
        book.save giving N9 
        trapclr Object
CleanUp
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
.do some housekeeping first

        destroy Rowcol
        destroy sheet
        destroy sheets
        destroy book
        destroy books
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
.If User has quit out of the SaveAs routine we do not want any prompts informing them their
.Worksheet has not been saved.  If we did not suppress these message, instances of Excel might
.be left open.
.        setprop ex,*DisplayAlerts=OFALSE
        ex.quit
        destroy ex
.end patch 2.25
.         STOP
         shutdown
ABORT    TRAPCLR   F5
         PRINT     *L,"*****JOB ABORTED BY OPERATOR"
         GOTO      EXIT
* ***************************************************************************
*  ERROR SUBROUTINES
* ****************************************************************************
.
IO
         TRAPCLR   IO
         NORETURN
         DISPLAY   *P1:23,*EL,FERROR," NOT ON LINE",*B,*B,*B:
                   *P1:24,*EL,"ERROR = ",ERROR
         DISPLAY   *P1:24,*EL,"IO ERROR INFORM COMPUTER PERSONNEL !!!";
         BEEP
         PRINT     *L,"*** JOB ABORTED - I/O ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      IO
RANGE
         TRAPCLR   RANGE
         NORETURN
         DISPLAY   *P1:24,*EL,"RANGE ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - RANGE ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      RANGE
FORMAT
         TRAPCLR   FORMAT
         NORETURN
         DISPLAY   *P1:24,*EL,"FORMAT ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - FORMAT ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      FORMAT
PARITY
         TRAPCLR   PARITY
         NORETURN
         DISPLAY   *P1:24,*EL,"PARITY ERROR INFORM COMPUTER PERSONNEL !!!":
                   *B,*B,*B:
                   *P1:23,*EL,"ERROR = ",ERROR
         BEEP
         PRINT     *L,"*** JOB ABORTED - PARITY ERROR"
         KEYIN     *P70:24,*EOFF,str1;
         CMATCH    "Q",str1
         GOTO      EXIT IF EQUAL
         GOTO      PARITY
.patch1.88
                              include        compio.inc
                              include        cntio.inc
.         INCLUDE   NMLRIO.inc
.patch1.88
         INCLUDE   NORDIO.inc
.         INCLUDE   NXCGIO.INC
         include   xmgtio.inc
.patch1.88
.         include   nbrkio.inc
.patch1.88
         include   ndat3io.inc
         INCLUDE   NOWNIO.INC
 ;Begin patch 1.8
               Include        nshpio.inc               
.end patch 1.8
.Begin Patch 2.32
	INclude	NdatIO.inc
.End Patch 2.32
        INCLUDE   COMLOGIC.inc

