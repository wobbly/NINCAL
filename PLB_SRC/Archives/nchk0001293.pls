. ............................................................................
.NINP29EDIT - CHECK REGISTER EDIT PROGRAM
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         INC       CONSacct.inc
         INC       NCSHDD.inc
.begin patch 2.88
.         INC       NINVDD.inc
         INC                  ninvdd.inc
         Include              NinvAcddd.inc
.end patch 2.88
.START PATCH 2.86 REPLACED LOGIC
.         INC       NMLRDD.inc
              INCLUDE         COMPDD.inc
              INCLUDE         CNTDD.inc
.END PATCH 2.86 REPLACED LOGIC
         INC       NORDDD.inc
         INC       NOWNDD.inc
         INC       NPAYDD.inc
         INC       NADJDD.inc
         inc       ndatdd.inc
         INCLUDE   NESCDD.INC
         include   ndat3dd.inc
.begin patch 2.4
         include   nacddd.inc
         include   nshpdd.inc
.end patch 2.4
         inc       nmrgdd.inc
         inc       hp.inc
.START PATCH 2.7 ADDED LOGIC
        include NCTRDD.INC
.END PATCH 2.7 ADDED LOGIC
release    init       "2.92"           DLH 24Oct2007  More not allowed ap checking
Reldate	INit	"19Oct2007"
.release    init       "2.91"           JD 2007Oct19 identify NIN exclusives on print
.Reldate	INit	"19Oct2007"
.release    init       "2.9"           DLH 2007Aug20 PLI
.Reldate	INit	"20Aug2007"
.release    init       "2.89"           DLH 2005Dec22 compute ???
.release    init       "2.88"           DLH 2005March02 Invoice Conversion
.release    init       "2.87"           ASH 12JAN2005 ESCROW CONVERSION
.release    init       "2.86"           ASH 27MAY2004 MAILER CONVERSION
.release    init       "2.85"           JD26JUL2002 added check against cash receipts total.
.release    init       "2.82"          ASH17JUN2002 CONVERSION OF CONTROLS.DAT, NINCHK.DAT, DAT25N.DAT
.release    init       "2.81"           JD01FEB2002 new mask field for total ap.
.release    init       "2.8"           JD05NOV2001 fixed mask92b allow NEgative value and neg apsw.
.release    init       "2.7"          ASH 02APR2001      Increased CONTROLS.DAT Record size, changed implementation
.release    init       "2.61"          JD  14mar2001 added nininc adjustment
.release    init       "2.6"          DLH 22Nov2000 new external codes
.release    init       "2.5"          DLH 20Aug99 adj y2k etc
.release    init       "2.4"          Jul99 DLH NINV y2k etc
.RELEASE   INIT       "2.3"            25mar99 fixed moa field for ar. 
.RELEASE   INIT       "2.2"           22JAN99 ASH NINORD Y2K, CONSACCT.INC
.release   init       "2.1"           26jun98 JD added check dupe lr #.
.release   init       "2.0"          08Apr98 DLH duplexing
.release  init      "1.9"           16apr97  made escrow check match check reg.
.release  init      "1.8"           16aug96 DLH panasonic
.RELEASE   INIT     "1.7  "        27mar95 print diff on ap/added escrow tot.
.RELEASE   INIT     "1.6  "        01jan95 flag escrow clients
.release   init     "1.5"           31Dec94 DLH skip external D
.release   init     "1.4"           14nov94 splits consacct.inc compute.inc
.RELEASE  INIT      "1.3"           01NOV94 DLH HANDLE EXTERNAL 'O' & 'Q'
.RELEASE  INIT      "1.2"           16MAR94 JD PRINT TO LASER.
.RELEASE  INIT     "1.1"           09NOV92 DLH PRINT LIST OWNER INVOICE# 
.RELEASE  INIT      "1.0"             5/08/92  INCLUDE'S
.
.START PATCH 2.7 REMOVED LOGIC
.editfile Ifile     KEYLEN=3 fixed=3
.END PATCH 2.7 REMOVED LOGIC
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       8
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
...............................................................................
.
. WORK VARIABLES
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
MO       DIM       2
DY       DIM       2
YR       DIM       2
TYPIST   DIM       2
.MOA      FORM      7.2
.Start patch #2.3 - increase var
MOA      FORM      10.2
.End patch #2.3 - increase var
.
TOTAR    FORM      10.2
TOTAP1   FORM      10.2
TOTAP2   FORM      10.2
TOTAP    FORM      10.2
TOTNIN   FORM      10.2
TOTLR    FORM      10.2
TOTSTAX  FORM      10.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
TOTMOA   FORM      10.2
.TOTMOA   FORM      9.2
TOTneg   FORM      10.2
.begin patch 2.92
NoCheckFLag		Dim	1               "Y" = no check allowed, "N" = ok
NoCheckFLag1	Dim	1               "NoCheckFLag was set print at EOJ
.end patch 2.92
.
ZERO     FORM      "0"
.
FORM2    FORM      2
FORM22   FORM      2.2
.begin patch 2.4
.FORM7    FORM      7
.end patch 2.4
.begin patch 2.5
shipsw   dim        1
mrgsw    dim        1
.end patch 2.5
FORM52   FORM      5.2
FORM11   FORM      11
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
CO       FORM      1
. 
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
.MASK32   INIT      "ZZZ.ZZ-"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
.MASK92   INIT      "ZZZ,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
MASK92b   INIT      "Z,ZZZ,ZZZ,ZZZ.99-"
.START PATCH 2.81
TOTMASK  DIM       14
.end  PATCH 2.81.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
.START PATCH #2.2 - INCREASED VAR
.M$AR     DIM       13
M$AR     DIM       17
.END PATCH #2.2 - INCREASED VAR
M$PPM    DIM       6
M$QTY    DIM       9
.START PATCH #2.2 - INCREASED VAR
.M$AP1    DIM       13
.M$AP2    DIM       13
.M$STAX   DIM       8
.M$CTAX   DIM       8
M$AP1    DIM       17
M$AP2    DIM       17
M$STAX   DIM       10
M$CTAX   DIM       10
.END PATCH #2.2 - INCREASED VAR
M$POST   DIM       6
.START PATCH #2.2 - INCREASED VAR
.M$LRINC  DIM       13
.M$NINC   DIM       13
.M$GROSS  DIM       13
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
.END PATCH #2.2 - INCREASED VAR
.
MT$AR    DIM       17
MT$MOA    DIM       17
mt$neg    dim       17
MT$AP1    DIM       17
MT$AP2   DIM       17
MT$STAX  DIM       17
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       17
MT$NINC  DIM       17
.
PAGE     FORM      4
LINES    FORM      2
PRTFLAG  DIM       1
LOCAL    INIT      "LOCAL"
negflag  dim       1
MOAFLAG  FORM      1
ESCFLAG  DIM       1
.MESSAGE  DIM       8
EDKEY    DIM       3
.START PATCH 2.82 REPLACED LOGIC
.holdnum  dim       3
holdnum  dim       11
.END PATCH 2.82 REPLACED LOGIC
.START PATCH #2.2 - INCREASED VAR
DIFF     FORM      10.2
.DIFF     FORM      8.2
.END PATCH #2.2 - INCREASED VAR
APAMT    FORM      10.2
NUM102   FORM      10.2           RETURNED NUMERIC FIELD W/DOL.CENT CONVERTED.
TOTESCRO FORM      10.2
MT$ESC    DIM       17
AP2SW    DIM       1
lasrflag init      "T"            generally true unless a/p clerk has req
duprflag init      "N"
errflag  init      "N"
prevlr   dim       6
ControlFlag form              "0"
excl     dim       1
.
.         DISPLAY   *P1:1,*EF," CHECK REGISTER EDIT PRINT PROGRAM"
         MOVE      "NCHK0001" TO PROGRAM
         MOVE      "Names In The News Ca Inc" TO COMPNME
         MOVE      C1 TO NPAYPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NCSHPATH
         MOVE      C1 TO NESCPATH
         MOVE      inpname TO NCSHNAME
.         MOVE      "NINCSH" TO INPNAME
         MOVE      "CHECK REGISTER EDIT PRINT PROG" TO STITLE
         scan      "NOLASER" in comment
         if        equal
         move      "F" to lasrflag
         endif
         CALL      PAINT
         IFNZ      PC
         CLOCK     DATE TO DATE
         MOVE      "99/99/99" TO DATEMASK
         EDIT      DATE TO DATEMASK
         XIF
         IFZ      PC
         CLOCK     DATE TO DATE
         MOVE      DATE TO DATEMASK
         XIF
         MOVE      ZERO TO PAGE
         MOVE      "57" TO LINES
          MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
         keyin     *cl
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
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
         GOTO      INPUT IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      INPUT
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
INPUT    move     c0 to apamt
         move     c0 to diff
         CALL     NCSHSEQ
         GOTO      TOTAL IF OVER
              match         "419073" to CLR
              call          Debug if equal
              match         "596346" to CLR
              call          Debug if equal

.
              if (ControlFlag = C0)
               pack           NCTRFLD,CNUM,CNUMDATE
               rep            zfill,NCTRFLD
                      move    "START-NCTRKEY",Location
              pack    KeyLocation,"Key: ",NCTRFLD
                      call    NCTRKEY
               move           C1,ControlFlag
              endif
         COMPARE   ZERO TO PAGE
         CALL      HEADER IF EQUAL
.
         ADD       "1" TO COUNT
         DISPLAY   *P15:08,COUNT

.
         CMATCH    "M" TO CEXTCD
         GOTO      INPUT IF EQUAL
.
.begin patch 2.6
.         CMATCH    "D" TO CEXTCD       31jan95  DLH
         if        (CEXTCD = "D" or CEXTCD = "d" or CEXTCD = "N")
.         GOTO      INPUT IF EQUAL
         goto      input
         endif
.end patch 2.6
.
         CMATCH    " " TO CLR
         GOTO      INPUT IF EQUAL
         GOTO      INPUT IF EOS
.
         MOVE      C1 TO MOAFLAG
         CMATCH    "O" TO CEXTCD
         IF        EQUAL
         MOVE      C2 TO MOAFLAG
         ENDIF
         CMATCH    "Q" TO CEXTCD
         IF        EQUAL
         MOVE      C2 TO MOAFLAG
         ENDIF
.
         MOVE      CLR TO NINVFLD
         MOVE      C1 TO NINVPATH
         CALL      NINVKEY
         IF        OVER
         COMPARE   "57" TO lines
         CALL      header IF NOT LESS
         PRINT     *N,"NO INVOICE FOUND FOR : ",NINVFLD," !!!!",*N
         ADD       C3 TO LINES
         GOTO      INPUT
         ENDIF
.         REP       ZFILL IN AR
.         REP       ZFILL IN AP1
.         REP       ZFILL IN AP2
.         move      c0 to apamt
.         MOVE      AP1 TO CVTFLD                  .a/p1 from inv record
.         CALL      CVT
.         MOVE      cvtfld TO APAMT
.         div       hund into apamt
.
.         MOVE      ZERO TO FORM7
.         MOVE      QTYSHP TO FORM7
.
.START pATCH #2.2 - REPLACED VAR
.         MOVE      PPM TO FORM72
.         DIVIDE    HUND INTO FORM72
.         MOVE      FORM72 TO FORM32
         move      ap1 to apamt
         MOVE      PPM TO CMPT92
         DIVIDE    HUND INTO CMPT92
         MOVE      CMPT92 TO FORM32
.END pATCH #2.2 - REPLACED VAR
.
         MOVE      CLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         if        (CLR = "596346")
         call      debug
         endif
.
         IF        OVER
         CLEAR     OLON
         COMPARE   "57" TO lines
         CALL      header IF NOT LESS
         PRINT     *N,"NO ORDER FOUND FOR : ",NORDFLD," !!!!",*N
         ADD       C3 TO LINES
         ENDIF
.
.         type      obrknum
.         if        not equal
.         move      "0000" to obrknum
.         endif
.         pack      nescfld from OBRKNUM,CMLR
.         rep       zfill in nescfld
.         call      nesckey
.         if        over
.
         move      c2 to nescpath
.START PATCH 2.87 REPLACED LOGIC - TEMPORARY PATCH
.         pack      nescfld2 FROM LON,olnum
.         rep       zfill in nescfld2
         pack      nescfld2 FROM LON,"  ",olnum
.END PATCH 2.87 REPLACED LOGIC - TEMPORARY PATCH
         call      nesckey
         if        not over
         move      "<ESCROW>" to MESSAGE
         move      yes to escflag
         else    
         CLEAR     MESSAGE
         move      no to escflag
         endif
. 
         PACK      MKEY FROM MLRN,COBN
         CALL      NMLRKEY
.
.
OWNPREP  MOVE      OLON TO NOWNFLD
         PACK      NPAYFLD FROM OLON,PAYTN
         MOVE      "***MISSING OWNER***" TO OWNOCPY
         CALL      NOWNKEY
         MOVE      OWNOCPY TO PCOMP
         CALL      NPAYKEY
         IF        OVER
         MOVE      OWNOCPY TO PCOMP
         ENDIF
.begin patch 3.35   ---- we don't write ourselves checks the money is already in the bank
	MOve	No,NoCheckFlag
	if	(COmpany <> "2" & Lon = "0033")
	MOVe	yes,NoCheckFlag
	Elseif	(COmpany = "2" & Lon = "1490")
	MOVe	yes,NoCheckFlag
	endif
.end patch 3.35
.
         MOVE      YES TO SUBPPSW
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
.start patch 2.91
         IF        (elstcde = "C")
         move      "*" to excl
         else
         move      b1 to excl
         endif
.end patch 2.91
         call      wipecvars
         MOVE      LRn to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         move      no to shipsw

         CALL      NMRGKEY
.begin patch 2.9
         if       not over
         move     yes to mrgsw
         endif
         MOVE      clr to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if       not over
         move     yes to shipsw
         endif
.end patch 2.9
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
     ;         MOVE      NordFLD to nmrgfld
.         REP       ZFILL IN NMRGFLD
.         move      c0 to nmrgrqty
.         move      c0 to nmrgiqty
.         move      c0 to nmrgnet
.         move      no to shipsw
.         MOVE      NordFLD to nshpfld
.         REP       ZFILL IN NshpFLD
.         CALL      NshpKEY
.         if        not over
.         move      yes to shipsw
.         endif
.         move      no to mrgsw
.         CALL      NMRGKEY
.         if        not over
.         move      yes to mrgsw
.         endif
.         move      c1 to ndatpath
.         move      olnum to ndatfld
.         call      ndatkey
.         call      wipecvars
..
.              call           Ninvacdrecclear
.               CLEAR          NInvAcdfld
.               pack           NInvAcdFld from Invnum
.               call           NInvAcdRecClear
.               call           NinvAcdTst
.               Call           NInvAcdRecLoad

         CALL      COMPUTE
.         
.this is the diff between ncsh0002 & this program???
         move      ap to ap1
         move      ap to apamt
.this is the diff between ncsh0002 & this program???
.
CHKADJ   MOVE      CLR TO NADJFLD
. 
         CALL      NADJKEY
         GOTO      MASKIT IF OVER
.
.begin patch 2.5
.         MOVE      ASRECADJ TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO FORM82
.         MOVE      CVTFLD TO FORM82
.         MULTIPLY  ".01"  BY FORM82
.         ADD       FORM82 TO ar
          add       asrecadj to ar
.end patch 2.5
. 
.begin patch 2.5
.         MOVE      ASLRINC TO CVTFLD
.         CALL      CVT
.START pATCH #2.2 - REPLACED VAR
.         MOVE      ZERO TO FORM72
.         MOVE      CVTFLD TO FORM72
.         MULTIPLY  ".01"  BY FORM72
.         ADD       FORM72 TO LRINC
.         MOVE      ZERO TO CMPT92
.         MOVE      CVTFLD TO CMPT92
.         MULTIPLY  ".01"  BY CMPT92
.         ADD       CMPT92 TO LRINC
         add       aslrinc to lrinc
.END pATCH #2.2 - REPLACED VAR
.end patch 2.5
.
.begin patch 2.5
.         MOVE      ASPAYAD1 TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO AP1
.         ADD       FORM82 TO APamt           27mar95 calc diff.
         add        aspayad1 to ap1
         add        aspayad1 to apamt
.end patch 2.5
         compare   c0 to ap1
         if        less
         move      yes to negflag
         endif
         compare   c0 to apamt
         if        less
         move      yes to negflag
         endif
.
.begin patch 2.5
.         MOVE      ASPAYAD2 TO CVTFLD
.         CALL      CVT
.         MOVE      ZERO TO FORM82
.         MOVE      CVTFLD TO FORM82
.         DIV       HUND INTO FORM82
.         ADD       FORM82 TO AP2
          add       aspayad2 to ap2
.end patch 2.5
.begin patch 2.61
         add        asnininc to nininc
.end patch 2.61
         COMPARE   c0 TO ap2
         if        equal
         move      no to ap2sw
         else
         MOVE      yes TO AP2SW
         endif
         compare   c0 to ap2
         if        less
         move      yes to negflag
         endif
.
MASKIT
         compare   c0 to ap1
         if        less
         move      yes to negflag
         endif
         compare   c0 to apamt
         if        less
         move      yes to negflag
         endif
.
         compare   c0 to ap2
         if        less
         move      yes to negflag
         endif
.
         cmatch    yes to escflag
         if        equal
         compare  c0 to ap2
         if       equal
         add       ap1 to totescro
         else
         add       ap2 to totescro
         endif
         endif
.START PATCH #2.2 - INCREASED VAR
.         MOVE      MASK72 TO M$GROSS
         MOVE      MASK92b TO M$GROSS
.END PATCH #2.2 - INCREASED VAR
         EDIT      GROSS TO M$GROSS
.
.START PATCH #2.2 - INCREASED VAR
.         MOVE      MASK72 TO M$AR
         MOVE      MASK92b TO M$AR
.END PATCH #2.2 - INCREASED VAR
         COMPARE   C1 TO MOAFLAG
         IF        EQUAL
         EDIT      AR TO M$AR
         ADD       AR TO TOTAR
         ELSE
         MOVE      C0 TO AR
         move      c0 to moa
         move      camount to moa
.         mult      ".01" by moa
         EDIT      moa TO M$AR
         ADD       moa TO TOTMOA
         ENDIF
.
.START PATCH #2.2 - REPLACED VAR
.         MOVE      MASK72 TO M$AP1
         MOVE      MASK92b TO M$AP1
.END PATCH #2.2 - REPLACED VAR
         EDIT      AP1 TO M$AP1
         cmatch    yes to escflag              .escrow ?      turned off 11/07/01 jd
         if        not equal                       .yes
.         cmatch    yes to ap2sw                .2 a/ps?
.         if        equal                       .yes
.         ADD       AP1 TO TOTAP1                .so 2nd a/p is escrow
.begin patch 2.92
	IF	(NoCheckFLag <> Yes)
         ADD       AP1 TO TOTAP1                
         	Else
         	move	Yes,NoCheckFlag1                        .flag for eoj
	endif
.end patch 2.92
         goto      chkneg1                     .go handle a/p2
.         else
.         goto      mascap2                     .so A/p1 is escrow
         endif
.         endif
.         ADD       AP1 TO TOTAP1
chkneg1  cmatch    yes to negflag
         if        equal
         subtract  ap1 from totap1
         add       ap1 to totneg
         endif
.
mascap2  
.START PATCH #2.2 - INCREASED VAR
.         MOVE      MASK72 TO M$AP2
         MOVE      MASK92b TO M$AP2
.END PATCH #2.2 - INCREASED VAR
         EDIT      AP2 TO M$AP2
         cmatch    yes to escflag
         if        equal
         cmatch    yes to ap2sw
         goto      noadd2 if equal
         endif
         ADD       AP2 TO TOTAP2
.
         cmatch    yes to negflag
         if        equal
         subtract  ap2 from totap2
         add       ap2 to totneg
         endif
noadd2   COMPARE   ZERO TO AP2
         CALL      ZEROAP2 IF EQUAL
.

.START PATCH #2.2 - INCREASED VAR
.         MOVE      MASK72 TO M$LRINC
.         EDIT      LRINC TO M$LRINC
.         ADD       LRINC TO TOTLR
..
.         MOVE      MASK72 TO M$NINC
.         EDIT      NININC TO M$NINC
.         ADD       NININC TO TOTNIN
..
.         MOVE      MASK42 TO M$STAX
.         EDIT      TAXES TO M$STAX
.         ADD       TAXES TO TOTSTAX
..
.         MOVE      ZERO TO TAXES
.         MOVE      MASK42 TO M$CTAX
.         EDIT      TAXES TO M$CTAX
...
         MOVE      MASK92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
         ADD       LRINC TO TOTLR
.
         MOVE      MASK92 TO M$NINC
         EDIT      NININC TO M$NINC
         ADD       NININC TO TOTNIN
.
         MOVE      MASK72 TO M$STAX
         EDIT      TAXES TO M$STAX
         ADD       TAXES TO TOTSTAX
.
         MOVE      ZERO TO TAXES
         MOVE      MASK72 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.END PATCH #2.2 - INCREASED VAR
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
         ADD       POST TO TOTPOST
.
         GOTO      PRINT
.
.
ZEROAP2  CLEAR     OWNOCPY
         RETURN
.
*......................................................................
.
PRINT
         COMPARE   apamt TO ap1
         GOTO      INTDIFF IF NOT EQUAL
         cmatch    yes to negflag
         goto      intdiff if equal
         COMPARE   "57" TO LINES
         CALL      HEADER IF NOT LESS
         match     clr to PREVLR
         if        not equal
.begin patch 2.92
	IF	(NoCheckFLag = Yes)
         	PRINT     	*1,MLRN,"/",COBN,*12,LRN:
                   	*26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                   	*79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   	*117,M$CTAX,*127,M$POST:
                   	*N,*1,MCOMP,Hpbon,HPUNON,*26,OWNOCPY,*64,M$AP2,b1,"NO CHECK",HpBoff,HPUNOff:
                   	*N,*6,LOINVN,*20,O1DES,excl,*N
         	Else
         PRINT     *1,MLRN,"/",COBN,*12,LRN:
                   *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *N,*1,MCOMP,*26,OWNOCPY,*64,M$AP2,b1,message:
.                   *N,*6,LOINVN,*20,O1DES,*N
                   *N,*6,LOINVN,*20,O1DES,excl,*N
	endif                   
         else
         PRINT     *1,Hpbon,*1,"Duplicate **":
                   *n,*1,mLRN,"/",COBN,*12,LRN:
                   *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *N,*1,MCOMP,*26,OWNOCPY,*64,M$AP2,b1,message:
.                   *N,*6,LOINVN,*20,O1DES,hpboff
                   *N,*6,LOINVN,*20,O1DES,excl,hpboff
         move      yes to duprflag
         endif
         move      clr to prevlr
         ADD       C4 TO LINES
         CLEAR     O1DES
         GOTO      INPUT
.
INTDIFF 
         MOVE      ap1 TO DIFF
         SUBTRACT  ApAMT  FROM  DIFF
         COMPARE   "57" TO LINES
         CALL      HEADER IF NOT LESS
         match     clr to prevlr
         if        equal
         PRINT     *1,Hpbon,*1,"Duplicate **":
                   *n,*1,mLRN,"/",COBN,*12,LRN:
                   *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *N,*1,MCOMP,*26,OWNOCPY,*64,M$AP2,b1,message:
.start patch 2.91
.                   *N,*6,LOINVN,*20,O1DES,hpboff;
                   *N,*6,LOINVN,*20,O1DES,excl,hpboff;
.end patch 2.91
         else
         PRINT     *1,Hpbon:
                   *1,mLRN,"/",COBN,*12,LRN:
                   *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *N,*1,MCOMP,*26,OWNOCPY,*64,M$AP2,b1,message:
.start patch 2.91
.                   *N,*6,LOINVN,*20,O1DES,hpboff;
                   *N,*6,LOINVN,*20,O1DES,excl,hpboff;
.end patch 2.91
endif
         cmatch    yes to negflag
         if        not equal          
         PRINT     *N,*6,LOINVN,*20,O1DES,b1,"DIFF: ",diff,*flush,*N
         add       c2 to lines
        goto       alines
         else
         PRINT     *N,*6,LOINVN,*20,O1DES,b1,hpbon,"NEGATIVE: ",apamt,hpboff,*flush,*N
         add       c2 to lines
         goto      alines
         endif
         endif
.       
         PRINT     *1,MLRN,"/",COBN,*12,LRN:
                   *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                   *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                   *117,M$CTAX,*127,M$POST:
                   *N,*1,MCOMP,*26,OWNOCPY,*64,M$AP2,b1,message;
         cmatch    yes to negflag
         if        not equal          
         PRINT     *N,*6,LOINVN,*20,O1DES,b1,"DIFF: ",diff,*flush,*N
         else
         PRINT     *N,*6,LOINVN,*20,O1DES,b1,hpbon,"NEGATIVE: ",apamt,hpboff,*flush,*N

         endif
alines   ADD       C4 TO LINES
         CLEAR     O1DES
         move      clr to prevlr
         clear     negflag
         GOTO      INPUT
*......................................................................
.
TOTAL
.begin patch 2.85
          move     no to errflag
         compare    totar to NCTRAMT2
         if         not equal
         move       yes to errflag
         endif
.end patch 2.85
.
         COMPARE   "57" TO LINES
         CALL      HEADER IF NOT LESS
         MOVE      MASK92b TO MT$AR
         EDIT      TOTAR TO MT$AR
         MOVE      MASK92b TO MT$MOA
.         mult      seq by totmoa
.        EDIT      TOTMOA TO MT$MOA
         MOVE      MASK92b TO MT$NEG
         mult      seq by totneg
         EDIT      TOTneg TO MT$neg
         MOVE      MASK92b TO MT$AP1
.START PATCH 2.81
.         MOVE      "ZZZ,ZZZ,Z9.99" TO TOTMASK
         EDIT      TOTAP1 TO MT$AP1
.         EDIT      TOTAP1 TO totmask
.end PATCH 2.81
         MOVE      MASK92b TO MT$AP2
         EDIT      TOTAP2 TO MT$AP2
         MOVE      MASK92b TO MT$NINC
         EDIT      TOTNIN TO MT$NINC
         MOVE      MASK92b TO MT$LRINC
         EDIT      TOTLR TO MT$LRINC
         MOVE      MASK92b TO MT$STAX
         EDIT      TOTSTAX TO MT$STAX
         MOVE      MASK62 TO MT$CTAX
         EDIT      TOTCTAX TO MT$CTAX
         MOVE      MASK52 TO MT$POST
         EDIT      TOTPOST TO MT$POST
         cmatch    yes to duprflag
         if        equal
         print     *1,hpbon,hpunon,"*******************DUPLICATE RECORD IN REPORT PLEASE DOUBLE CHECK********************":
                   hpboff,hpunoff
         endif
.begin patch 2.85
         cmatch    yes to errflag
         if        equal
         print     *1,hpbon,hpunon,"*******************AR TOTAL DOES NOT MATCH CASH PLEASE RE-RUN CASH********************":
                   hpboff,hpunoff
         endif
.end patch 2.85
         PRINT     *26,"REGISTER TOTAL",*48,MT$AR,*62,MT$AP1:
.START PATCH 2.81
.         PRINT     *26,"REGISTER TOTAL",*48,MT$AR,*62,totmask:
                   *89,MT$LRINC:
                   *115,MT$CTAX:
                   *N,*62,MT$AP2,*77,MT$NINC,*103,MT$STAX,*124,MT$POST
         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         MOVE      MASK92b TO MT$AP1
         EDIT      TOTAP TO MT$AP1
.         MOVE      "ZZZ,ZZZ,Z9.99" TO TOTMASK
.         EDIT      TOTAP TO totmask
         MOVE      MASK92b TO MT$ESC
         EDIT      TOTESCRO TO MT$ESC
         PRINT     *26,"TOTAL ACCOUNTS PAYABLE",*62,MT$AP1
.END PATCH 2.81
.         PRINT     *26,"TOTAL ACCOUNTS PAYABLE",*62,totmask
         PRINT     *26,"TOTAL APPLIED FROM MOA",*48,totmoa
         PRINT     *26,"TOTAL ESCROW APPLIED TO MOA",*48,MT$esc
         PRINT     *26,"TOTAL NEGATIVE AP'S",*48,MT$NEG
.begin patch 2.92
	if	(NoCheckFlag1 = yes)                .we tried to print a check to ourselves)
         PRINT     *26,HpBon,HpUnon,"This Control is trying to Print a check payable to US!!!"
         PRINT     *26,"PLease correct and rerun!!!",HpBoff,HpUnoff
	endif
.end patch 2.92
         GOTO      EOJ
.............................................................................
HEADER
         ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
.START PATCH 2.82 REPLACED LOGIC
.         move       cnum to holdnum
              pack            holdnum,cnum,cnumdate
.END PATCH 2.82 REPLACED LOGIC
         cmatch     true to lasrflag
         if         equal
.         print      hp17ptch,HPTOP,hpdupl,*f
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         else
         print      p2417cpi
         endif
         endif
         MOVE      C7 TO LINES
.START PATCH 2.82 REPLACED LOGIC
.         PRINT     *F,*n,*1,"*EDIT*",*9,"CONTROL NO. ",CNUM:
.                   *31,"***  N I N   C H E C K   ":
.                   *57,"R E G I S T E R   R E P O R T  ***":
.                   *116,"DATE ",DATEMASK:
.                   *N,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
.                   *N,*N,*1,"CLIENT",*12,"LR ##":
.                   *26,"LIST OWNER PAY-TO":
.                   *52,"--------ACCOUNTS--------":
.                   *79,"-------COMMISSIONS------":
.                   *108,"------TAXES----",*128,"OUR":
.                   *N,*1,"NUMBER",*10,"LO-INV":
.                   *20,"LIST NAME",*53,"RECEIVABLE":
.                   *69,"PAYABLE",*81,"NIN INCOME":
.                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
.                   *126,"POSTAGE",*N,*126,LINES
              unpack          cnumdate,CC,YY,MM,DD
              pack            str10,MM,SLASH,DD,SLASH,CC,YY
.begin patch 2.9
	If	(Company = c2)
         PRINT     *F,*n,*1,"*EDIT*",*9,"CONTROL NO. ",CNUM,"-",str10:
                   *36,"***  P L I   C H E C K   ":
                   *62,"R E G I S T E R   R E P O R T  ***":
                   *116,"DATE ",DATEMASK:
                   *N,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *N,*N,*1,"CLIENT",*12,"LR ##":
                   *26,"LIST OWNER PAY-TO":
                   *52,"--------ACCOUNTS--------":
                   *79,"-------COMMISSIONS------":
                   *108,"------TAXES----",*128,"OUR":
                   *N,*1,"NUMBER",*10,"LO-INV":
                   *20,"LIST NAME",*53,"RECEIVABLE":
                   *69,"PAYABLE",*81,"NIN INCOME":
                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
                   *126,"POSTAGE",*N,*126,LINES
	Else
         PRINT     *F,*n,*1,"*EDIT*",*9,"CONTROL NO. ",CNUM,"-",str10:
                   *36,"***  N I N   C H E C K   ":
                   *62,"R E G I S T E R   R E P O R T  ***":
                   *116,"DATE ",DATEMASK:
                   *N,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *N,*N,*1,"CLIENT",*12,"LR ##":
                   *26,"LIST OWNER PAY-TO":
                   *52,"--------ACCOUNTS--------":
                   *79,"-------COMMISSIONS------":
                   *108,"------TAXES----",*128,"OUR":
                   *N,*1,"NUMBER",*10,"LO-INV":
                   *20,"LIST NAME",*53,"RECEIVABLE":
                   *69,"PAYABLE",*81,"NIN INCOME":
                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
                   *126,"POSTAGE",*N,*126,LINES
	endif
.end patch 2.9
.END PATCH 2.82 REPLACED LOGIC
         RETURN
.
*............................................................
.
CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
         RESET     MPCHARS
         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
         GOTO      CVTMP IF EQUAL                YES.
         RESET     CVTFLD                        NO.
         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
         RETURN    IF EQUAL                      ITS OK.
FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
         RETURN                                POP THE STACK.
CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
         RESET     CVTFLD
         TYPE      CVTFLD                        VALID NUMERIC?
         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
         RETURN
.
EOJ
.START PATCH 2.7 REPLACED LOGIC
.         OPEN      editFILE,"CONTROLS"
.         PACK      EDKEY FROM holdnum
.         filepi    3;editfile
.         READ      EDITFILE,EDKEY;;
.         IF        OVER
.         WRITE     EDITFILE,EDKEY;holdnum
.         CLOSE     EDITFILE
.         ENDIF
        pack    NCTRFLD,holdnum
        move    "EOJ-NCTRKEY",Location
        pack    KeyLocation,"Key: ",NCTRFLD
        call    NCTRKEY
        if not over
                move    C1,NCTRCODE
                move    "EOJ-NCTRUPD",Location
                call    NCTRUPD
        endif
.END PATCH 2.7 REPLACED LOGIC
         shutdown  "cls"
         STOP
.
         INCLUDE   NCSHIO.inc
         include   ndatio.inc
.START PATCH 2.86 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc
              INCLUDE         COMPIO.inc
              INCLUDE         CNTIO.inc
.END PATCH 2.86 REPLACED LOGIC
         INCLUDE   NOWNIO.inc
         INCLUDE   NPAYIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NADJIO.inc
         include   nacdio.inc
         INCLUDE   NESCIO.INC
.begin patch 2.88
.         INCLUDE   NINVIO.inc
.         include   compute.inc
         INCLUDE              ninvio.inc
         Include              NinvAcdio.inc
         include              compute.inc
.end patch 2.88
         include   nshpio.inc
         include   ndat3io.inc
         include   nmrgio.inc
.START PATCH 2.7 ADDED LOGIC
        include NCTRIO.INC
.END PATCH 2.7 ADDED LOGIC
         INCLUDE   COMLOGIC.inc
