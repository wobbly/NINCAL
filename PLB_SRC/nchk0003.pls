..............................................................................
.NCHK0003 - CHECK REGISTER  PROGRAMM 
...............................................................................
.
PC       EQU        0
         INC       COMMON.inc
. 
         INC       CONS.inc
         INC       CONSACCT.inc
.
         include   gnxtdd.inc

         inc       hp.inc
. 
         INC       NCSHDD.inc
;         INC       NINVDD.inc
          INC                 ninvdd.inc
          include   NinvAcddd.inc
         include   ndatdd.inc
         include   nacddd.inc
         include   nshpdd.inc
.
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
.
         INC       NBILDD.inc
.
         INC       NORDDD.inc
.
         INC       NOWNDD.inc
.
         INC       NPAYDD.inc
         INCLUDE   NDAT3DD.INC
.
         INC       NADJDD.inc
         inc       nescdd.inc
         INC       NMOADD.INC
         inc       nmrgdd.inc
         INC       NMOBDD.INC
          include   nctrdd.inc
Release     Init           "3.39"    DLH  Review Inter comp transfer
Reldate   Init      "12 November 2009"
.Release     Init           "3.38"    DLH  turn off anoying beep on MOB write
.Reldate   Init      "26 October 2009"
.Release     Init           "3.37"    DLH  Closing PL account
.Reldate   Init      "25 February 2008"
.Release     Init           "3.36"    DLH  Escrow Correction
.Reldate  Init      "05 March 2008"
.Release     Init           "3.35"    JD  2008FEb29 Clearing intercomp trans field
.Release     Init           "3.34"   DLH 2008FEb1 PLI CONVERSION tighten up MOA & Inter comp tranfers
.Release     Init           "3.33"   DLH 2007Dec18 PLI CONVERSION further clarification
.Release     Init           "3.32"   DLH 2007Nov08 PLI CONVERSION MOA further clarification
.Release     Init           "3.31"   DLH 2007Nov08 PLI CONVERSION MOA
.Release     Init           "3.3"   DLH 2007July11 PLI CONVERSION
.see archive for details of previous changes
.Release        Init           "3.2"   DLH 2005March02 Invoice CONVERSION
.Release        Init           "3.13"   ASH 12JAN2005 ESCORW CONVERSION
.Release        Init           "3.12"   ASH 27MAY2004 MAILER CONVERSION
.Release        Init           "3.11"   DLH 30April2003   argh fix key field at MOA write
.release    init       "3.10"         ASH17JUN2002 CONVERSION OF CONTROLS.DAT, NINCHK.DAT, DAT25N.DAT
.release    init       "3.00"          JD16NOV2001 added update to controls.dat
.release    init       "2.95"          JD27jun2001 added forced open on nincsh seq.
.release    init       "2.94"          JD  14mar2001 added nininc adjustment
.release   init       "2.93"            22Nov2000 DLH New ext codes
.release   init       "2.92"            04OCT2000 ASH NEW SERVER ADDED
.release   init       "2.91"            08SEP00 JD fixed tot moa print at eoj.
.release   init       "2.9"            20aug99 DLH adjustment y2k
.release   init       "2.8"            26apr99 DLH nininv y2k
.RELEASE   INIT       "2.7"            25mar99 fixed moa field for ar.
.release  init      "2.6"            01JAN99 ASH NINORD Y2K, File expansion; CONSACCT.INC var expansion
.release  init      "2.5"            27Oct98 ASH NINMOA Y2K,File expansion
.release  init      "2.4"            8Apr98 DLH Duplex
.release  init      "2.3"            17mar97 ext "Q" do not add to aps.
.release  init      "2.2"           17Dec96 DLH fixed recon file
.                                  needs the payee not the mlr that paid us
.RELEASE   INIT      "2.1 "          23sep96 jd added payor to recon file.
.RELEASE   INIT      "2.0 "         17aug95 jd fixed neg flag ap1/ap2.
.RELEASE   INIT     "1.9  "        26JUN95 JD ADD FLAG/TOTAL FOR NEG AP'S
.RELEASE   INIT     "1.8  "        JD 09JUN95 UPDATED MTD CASH VARS TO CTD=BY CHECK
.RELEASE   INIT     "1.7  "        01jan95 flag escrow clients
.RELEASE  INIT      "1.6"         DLH 15NOV94 NEW COMPUTE, CONSACCT.INC
.
.release  init      "1.5"           17MAR94 jd print to laser
.release  init      "1.4"          16FEB94 DLH  login changes. for \/D
.release  init      "1.31"         16feb94 DLH prep for new ap fields in MTDFILE
.RELEASE  INIT     "1.3"          09NOV92 DLH PRINT LO INVOICE NUMBER.
.RELEASE  INIT     "1.2"          06MAY92 JD
.RELEASE  INIT     "1.1"          06APR92 COMPUTEP.inc, NMLRXX, NOWNXX, nordxx,
. 
.RELEASE  INIT      "1.0"
...............................................................................
.begin patch 3.3    corrected file length
ACCOUNTD FILE      FIXED=155
.ACCOUNTD FILE      FIXED=125
.ACCOUNTD FILE      FIXED=119
.end patch 3.3    
.MTDFILE.
.
P        INIT      "P"           1-1      RECORD ID
MTDACR   FORM      7.2           2-11
MTDFILL1 DIM       1            12-12
MTDACP   FORM      7.2          13-22     total ap1 by check date
MTDFILL2 DIM       1            23-23     total ap2 by check date
MTDSAP   FORM      7.2          24-33
MTDFILL3 DIM       1            34-34
MTDNIN   FORM      7.2          35-44
MTDFILL4 DIM       1            45-45
MTDLR    FORM      7.2          46-55
MTDFILL5 DIM       1            56-56
MTDSTX   FORM      5.2          57-64
MTDFILL6 DIM       1            65-65
MTDCTX   FORM      5.2          66-73
MTDFILL7 DIM       1            74-74
MTDPOST  FORM      5.2          75-82
MTDFILL8 DIM       13           83-95
MTDCNTNO FORM      3            96-98
MTDFILL9 DIM       1            98-99
MTDMO    FORM      2            99-101
MTDDY    FORM      2           102-103
MTDYR    FORM      2           104-105
ctdAP1  form      7.2         106-115      total ap1 by check date
ctdAP2  form      7.2         116-125      total ap2 by check date
CTDAP3  Form        7.2       126-135     *$ for INtercomp transfer
MTDXNIN   Form      7.2       136-145
MTDSAP3   Form      7.2       146-155
.
...........................................
.RECONN FILE
.ORIGINAL FORMAT
.
.MLR       1-4
.CNT       5-7
.LR        8-13
.CHK##    14-19         1 OR 2
.LO##     20-23
.ACR      24-30     DIM
.AP       31-37     DIM    1 OR 2
.NIN      38-46     DIM 
.LR       47-55
.STAX     56-61
.CTAX     62-67
.POST     68-72
.CONTROL  73-74
.DATE     75-80
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       6
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
SYSCC    DIM       2
.
DATEMASK DIM       8
PRTFLAG  DIM       1
LOCAL    INIT      "LOCAL"
.FILES.
...............................................................................
.
MTDFILE  FILE      FIXED=125
.
RECON    FILE
.begin patch 3.37           track interomcany transfers no more physical check
TRANSFER    FILE
.end patch 3.37
.
DATEFILE FILE               ;CREATED BY CHAIN HOLDS DATE ENTERED FOR CONTROL.
.
. ISAM KEY VARIABLES
.................................................
KEY      DIM       6     USED ON INVOICE & ADJUSTMENTS. 
PAYKEY   DIM       5    *PAY-TO FILE.
.
. WORK VARIABLES
.
RECNUM   FORM      1
.
Cntrl    FORM      3            *HOLDS CONTROL NUMBER FOR EOJ.
CNTRDATE dim        10
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
MO       DIM       2
DY       DIM       2
UNO      FORM      "01"
YR       DIM       2
ANS      DIM       1
TYPIST   DIM       2
MOA      FORM      10.2
.
TOTAR    FORM      10.2
TOTAP1   FORM      10.2
TOTAP2   FORM      10.2
TOTAP    FORM      10.2
.begin patch 3.3
TOTAP3   FORM      10.2
TOTXNIN   FORM      10.2
TotTrans  Form      10.2
.end patch 3.3
.begin patch 3.32
TOTCheck   FORM      10.2
.end patch 3.32
TOTNIN   FORM      10.2
TOTLR    FORM      10.2
TOTSTAX  FORM      9.2
TOTCTAX  FORM      6.2
TOTPOST  FORM      5.2
TOTMOA   FORM      10.2
TOTneg   FORM      10.2
TOTESCRO FORM      10.2
.
shipsw   dim       1
mrgsw    dim        1
.begin patch 3.3
NoCheckFLag                   Dim       1               "Y" = no check allowed, "N" = ok
.NoCheckFLag1       Dim       1               "NoCheckFLag" was set earlier print at EOJ
.end patch 3.3

FORM2    FORM      2
FORM22   FORM      2.2
FORM52   FORM      5.2
FORM11   FORM      11
.begin patch 3.3
.CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
.MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
.MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
.NUM10    FORM      10
.end patch 3.3
COUNT    FORM      5
CO       FORM      1
HOLDMO   FORM      2                  *USED TO CHECK DATE
. 
DATEOK   DIM       2
.
.PRINT MASK VARIABLES
.
MASK22   INIT      "ZZ.ZZ"
MASK42   INIT      "Z,ZZZ.ZZ-"
MASK72   INIT      "Z,ZZZ,ZZZ.ZZ-"
MASK52   INIT      "ZZ,ZZZ.ZZ-"
MASK62   INIT      "ZZZ,ZZZ.ZZ-"
MASK7    INIT      "Z,ZZZ,ZZZ"
.
MASK92b   INIT      "Z,ZZZ,ZZZ,ZZZ.99"
TOTHEAD  DIM       8      *USED FOR PRINTING TOTALS.
M$RTAX   DIM       5      *RETURN-TO TAX PERCENT
M$AR     DIM       17
M$PPM    DIM       6
M$QTY    DIM       9
M$AP1    DIM       17
M$AP2    DIM       17
.begin patch 303
M$AP3     DIM       17
M$XNINC             DIM       15
M$Trans   DIM       17        
.end patch 3.3
M$STAX   DIM       10
M$CTAX   DIM       10
M$POST   DIM       6
M$LRINC  DIM       15
M$NINC   DIM       15
M$GROSS  DIM       15
.
MT$AR    DIM       17
mt$neg    dim       17
MT$MOA    DIM       17
MT$ESC    DIM       17
MT$AP1    DIM       17
MT$AP2   DIM       17
.begin patch 3.31
Calc92    FOrm      9.2
transDiff Form      9.2
TOTLR3    FORM      10.2
CodeIARAMT  FORM      9.2
M$ARCdeI     DIM       17             cextcd code I ar
MT$LRINC3 DIM       17
.end patch 3.31
.begin patch 3.3
MT$AP3              DIM       17
MT$XNINC            DIM       17
MT$Trans            DIM       17        
Trans     Form      10.2
.end patch 3.3
MT$STAX  DIM       17
MT$CTAX  DIM       10
MT$POST  DIM       9
MT$LRINC DIM       17
MT$NINC  DIM       17
.
PAGE     FORM      4
LINES    FORM      2
MOAFLAG  FORM      1
negflag1 dim       1
negflag2 dim       1
.begin patch 3.3
negflag3  dim                 1
.end patch 3.3

ESCFLAG  DIM       1
CHANGE   FORM      9.2         CHANGE TO BE APPLIED TO BALANCE.
.
         MOVE      "NCHK0003" TO PROGRAM
         MOVE      "CHECK REGISTER " TO STITLE
         MOVE      "NAMES IN THE NEWS" TO COMPNME
         CALL      PAINT
         MOVE      C1 TO NORDPATH          .SET ACCESS TO ISI BY LR#.
         MOVE      C1 TO NPAYPATH
         MOVE      "NINCSH" TO NCSHNAME
         MOVE      "NINCSH" TO INPNAME
         open      ncshfile,ncshname
         MOVE       C1  TO NCSHPATH
         MOVE      C1 TO NINVPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C2 TO NESCPATH
         move      c1 to ncshflag
         IFNZ      PC
         OPEN      DATEFILE,"CONTROL_DATE"
         XIF 
         IFZ      PC
         OPEN      DATEFILE,"CNTDATE"
         XIF 
        READ      DATEFILE,SEQ;DATE
         CLOSE     DATEFILE
         MOVE      "99/99/99" TO DATEMASK
         EDIT      DATE TO DATEMASK
         pack      SYSCC,CC
         UNPACK    DATE INTO SYSMO,SYSDY,SYSYR
         UNPACK    DATE INTO MM,DD,YY
         MOVE      DATEMASK TO TODAY
         CALL      PAINT               .REDISPLAY SHOWING CHECK DATE.
         MOVE      C0 TO PAGE
         CALL      DATETEST
         BRANCH    DATEFLAG OF BADDATE
         GOTO      OK
BADDATE  KEYIN     *HON,*BLINKON,"BAD DATE",*B
         GOTO      BADDATE
.begin patch 3.3        .move later so we can open the correct one
OK       
          DISPLAY   *P1:23,"OPENING  MONTH-TO-DATE FILE        "
.         IFNZ      PC
.         DISPLAY   *P1:23,"OPENING  CHECK RECON. FILE        "
.         PREP      RECON,"CHECK_RECON:INVOICE"
.         OPEN      MTDFILE,"MTD_CASH",EXCLUSIVE
.         XIF
.         IFZ      PC
.         OPEN      MTDFILE,"MTDCASH",EXCLUSIVE
         DISPLAY   *P1:23,"OPENING  CHECK RECON. FILE        "
         PACK      STR35,NTWKPATH1,"CHKRECON"
         PREP      RECON,STR35
.end patch 3.3
.begin patch 3.37
         DISPLAY   *P1:23,"OPENING Transfer RECON. FILE        "
         PACK      STR35,NTWKPATH1,"TRNSRECON"
         PREP      Transfer,STR35
.end patch 3.37
          MOVE      "Exit" TO PF5
         KEYIN     *CL
         TRAP      END IF F5
         CALL      FUNCDISP
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : "
.
INPGET   TRAP      FILENG giving error IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         CLOSE     TESTFILE
.begin patch 3.3
          TRAP      FILEIO giving error IF IO
.          GOTO      PRTGET
          GOTO      PrepOK
.end patch 3.3
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
.
.begin patch 3.3
PrepOK
.         If        (CCOMPID = "P")
          If        (Company = C2)
          OPEN      MTDFILE,"MTDCASHP",EXCLUSIVE
          Move      "P",CCompID
.         else
          ElseIF    (Company = C1)
          OPEN      MTDFILE,"MTDCASH",EXCLUSIVE
          Move      "N",CCompID
          endif
.end patch 3.3
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

INPUT    CALL      NCSHSEQ
         GOTO      EOJ IF OVER
         MOVE      SYSMO TO HOLDMO
.
         COMPARE   C0 TO PAGE
         CALL      HEADER IF EQUAL
.
.begin patch 3.3
         CLEAR     MESSAGE
         move      no to escflag
.end patch 3.3

.begin patch 3.35
           move      c0 to trans
.end patch 3.35
         ADD       "1" TO COUNT
         DISPLAY   *P15:08,COUNT
         BRANCH    COUNT OF SETNUM
         GOTO      CHKEXT
SETNUM   MOVE      CNUM TO cntrl
          pack      CNTRDATE,CNUMDATE
.
CHKEXT
.
.begin patch 3.3
.         CMATCH    "M" TO CEXTCD
.         GOTO      INPUT IF EQUAL
.         CMATCH    "D" TO CEXTCD
.         GOTO      INPUT IF EQUAL
         if         (CEXTCD = "D" or CEXTCD = "d" or CEXTCD = "N" or CEXTCD = "M")
         goto       input
         endif
.end patch 3.3
.
.
         CMATCH    " " TO CLR
         GOTO      INPUT IF EQUAL
         GOTO      INPUT IF EOS
.
.begin patch 3.3
          Move      c1,MoaFlag
          if        (CEXTCD = "O")         
          MOVE                C2 TO MOAFLAG
          ENDIF
          if        (CEXTCD = "Q")         
          MOVE                C3 TO MOAFLAG
          ENDIF
.end patch 3.3
.
         MOVE      CLR TO NINVFLD
         CALL      NINVKEY
         IF        OVER
         COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         PRINT     *L,"NO INVOICE FOUND FOR : ",NINVFLD," !!!!",*L
         ADD       C3 TO LINES
         GOTO      INPUT
         ENDIF
.
.
         move      ppm to form32
.
         MOVE      LRN TO NORDFLD
         REP       ZFILL IN NORDFLD
         CALL      NORDKEY
         IF        OVER
         CLEAR     OLON
         CLEAR     OELCODE
         COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         PRINT     *L,"NO ORDER FOUND FOR : ",NORDFLD," !!!!",*L
         ADD       C3 TO LINES
         ENDIF
.
         PACK      MKEY FROM MLRN,COBN
         CALL      NMLRKEY
.
.
OWNPREP  MOVE      OLON TO NOWNFLD
         PACK      NPAYFLD FROM OLON,PAYTN
         REP       ZFILL IN NOWNFLD
         MOVE      "***MISSING OWNER***" TO OWNOCPY
         CALL      NOWNKEY
         MOVE      OWNOCPY TO PCOMP
         CALL      READPAY
.begin patch 3.33   ---- we don't write ourselves checks the money is already in the bank
          reset     runcodes
          MOve      No,NoCheckFlag
          if        (COmpany <> "2" & Lon = "0033")
          scan      Olnum,Runcodes
                    IF        not equal
                    MOVe      yes,NoCheckFlag
                    endif
          Elseif    (COmpany = "2" & Lon = "1490")
          scan      Olnum,Runcodes
                    IF        not equal
                    MOVe      yes,NoCheckFlag
                    endif
          endif
.end patch 3.33

.
.begin patch 3.3 --- moved to after adjustments
.         move      c2 to nescpath
.         call      nesckey
.         if        not over
.         move      "<ESCROW>" to MESSAGE
.         move      yes to escflag
.         else    
.         CLEAR     MESSAGE
.         move      no to escflag
.         endif
.end patch 3.3 --- moved to after adjustments
.
.
         MOVE      YES TO SUBPPSW
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         move      no to shipsw
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         CALL      NshpKEY
         if        not over
         move      yes to shipsw
         endif
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
         call      wipecvars
                    call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
         CALL      COMPUTE
.
CHKADJ   MOVE      CLR TO NADJFLD
         CALL      NADJKEY
.begin patch 3.3
          Goto      PrepEscrow if over
.         GOTO      MASKIT IF OVER
.end patch 3.3
         add       asrecadj to ar
         add       aslrinc to lrinc
         add       aspayad1 to ap1

         compare   c0 to ap1
         if        less
         move      yes to negflag1
         endif
.
         add       aspayad2 to ap2
.Begin patch 3.3
                    add                 aspayad3 to ap3
          Add       AsXninc,Xninc
.end patch 3.3

         add        asnininc to nininc
         compare   c0 to ap2
         if        less
         move      yes to negflag2
         endif
.begin patch 3.3
         compare   c0 to ap3
         if        less
         move      yes to negflag3
         endif
.end patch 3.3
.begin patch 3.3 --- moved to after adjustments
PrepEscrow
         move      c2 to nescpath
.begin patch 3.36
.         pack      nescfld2 FROM LON,"  ",olnum
         pack      nescfld2 FROM "00",LON,olnum
.begin patch 3.36
         CLEAR     MESSAGE
         move      no to escflag

          If        ((AP3 >= c0 | Xninc >= c0) and CEXTCD  = "I")
          Goto      CheckEscrow
          Elseif    ((AP3 <= c0 | Xninc <= c0) and CEXTCD  <> "I")
          Goto      CheckEscrow
          else
          GOto      SkipEscrow
          endif

CheckEscrow
         call      nesckey
         if        not over
         pack      str8 from cc,yy,mm,dd
         move      str8,n8
         call         trim using NescSdate
         move         c0,n9
         move      NEscSDate,n9
                      if         (n8 < n9 | N9 = c0)
                      move      "<ESCROW>" to MESSAGE
                      move      yes to escflag
                      endif
.         else    
.         CLEAR     MESSAGE
.         move      no to escflag
         endif
SkipEscrow          
.end patch 3.3 --- moved to after adjustments

.
MASKIT   
         MOVE      mask92b TO M$GROSS
         EDIT      GROSS TO M$GROSS
.
         MOVE      mask92b TO M$AR
         COMPARE   C1 TO MOAFLAG
         IF        EQUAL
         EDIT      ar TO M$AR
         ADD       ar TO TOTAR
         ELSE
         MOVE      C0 TO ar
         move      c0 to moa
         move      camount to moa
         EDIT      moa TO M$AR
         ADD       moa TO TOTMOA
         ENDIF
.
         MOVE      mask92b TO M$AP1
         EDIT      AP1 TO M$AP1
.begin Patch 3.3         
          IF        (CHKn1 <> "ESCROW" & NoCheckFlag <> Yes)
.         match     "ESCROW" to chkn1
.        if        not equal
         ADD       AP1 TO TOTAP1
         endif
.End Patch 3.3         
         cmatch    yes to negflag1
         if        equal
         subtract  ap1 from totap1
         add       ap1 to totneg
         endif

         COMPARE   C3 TO MOAFLAG
         IF        EQUAL
         subtract  ap1 from totap1
         endif
.
         MOVE      mask92b TO M$AP2
         EDIT      ap2 TO M$AP2
         match     "ESCROW" to chkn2
         GOTO      NOADD2 IF EQUAL

         ADD       ap2 TO TOTAP2
         cmatch    yes to negflag2
         if        equal
         subtract  ap2 from totap2
         add       ap2 to totneg
         endif
.
         COMPARE   C3 TO MOAFLAG
         IF        EQUAL
         subtract  ap2 from totap2
         endif
.
NOADD2   COMPARE   C0 TO ap2
         CALL      ZEROAP2 IF EQUAL

.begin patch 3.3
         MOVE      MASK92b TO M$AP3
         EDIT      AP3 TO M$AP3
         ADD       AP3 TO TOTAP3
.end patch 3.3
.
         MOVE      mask92 TO M$LRINC
         EDIT      LRINC TO M$LRINC
         ADD       LRINC TO TOTLR
.
         MOVE      mask92 TO M$NINC
         EDIT      NININC TO M$NINC
         ADD       NININC TO TOTNIN

.begin patch 3.3
         MOVE      MASK92 TO M$XNINC
         EDIT      XNINC TO M$XNINC
         ADD       XNINC TO TOTXNIN
.end patch 3.3
.
         MOVE      MASK72 TO M$STAX
         EDIT      TAXES TO M$STAX
         ADD       TAXES TO TOTSTAX
.
         MOVE      C0 TO TAXES
         MOVE      MASK72 TO M$CTAX
         EDIT      TAXES TO M$CTAX
.
         MOVE      MASK32 TO M$POST
         EDIT      POST TO M$POST
         ADD       POST TO TOTPOST
.
         GOTO      PRINT
.
READPAY  CALL      NPAYKEY 
         IF        OVER
         MOVE      OWNOCPY TO PCOMP
         ENDIF
         RETURN
.
ZEROAP2  CLEAR     OWNOCPY
         RETURN
.
.*......................................................................
..
PRINT    COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         
.begin patch 3.3         
          IF        (NoCheckFLag = Yes)
          PRINT               *1,MLRN,"/",COBN,*12,LRN,*19,HPBon,"No Check",HPBoff:
                    *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                    *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                    *117,M$CTAX,*127,M$POST:
                    *L,*6,LOINVN,*19,CHKN2,*26,OWNOCPY,*64,M$AP2,*79,M$XNINC:
                    *N,*19,CHKN3,*64,M$AP3,B1,MESSAGE;

          Else                   
                    If        (CEXTCD = "I")
                    Move      Ap3,CodeIARAMT
                    add       Ap1,CodeIARAMT
                    add       Xninc,CodeIARAMT
                    move      Mask92,M$ARCdeI 
                    Edit      CodeIAramt,M$ARCdeI 
                    Move      c0,calc92
                    Calc      Calc92=(AR-CodeIARamt)
                    add       calc92,transDiff
                    sub       Ap3,TotAp3
                    Sub       AR,TotaR
                    add       CodeIAramt,TotAR
                    PRINT     *1,MLRN,"/",COBN,*12,LRN:
                              *26,PCOMP,*50,M$ARCDEI,*64,M$AP1," ",CEXTCD:
                              *79,M$XNINC,*91,M$AP3," *",*107,M$STAX:
                              *117,M$CTAX,*127,M$POST:
                              *N,*1,MCOMP,*26,OWNOCPY,*64,M$AP2,b1,message,*n
                    Sub       LRinc,TotLR
                    add       ap3,Totlr3

                    Else

...........................

                    PRINT               *1,MLRN,"/",COBN,*12,LRN,*19,CHKN1:
                              *26,PCOMP,*50,M$AR,*64,M$AP1," ",CEXTCD:
                              *79,M$NINC,*91,M$LRINC,*107,M$STAX:
                              *117,M$CTAX,*127,M$POST:
                              *L,*6,LOINVN,*19,CHKN2,*26,OWNOCPY,*64,M$AP2,*79,M$XNINC:
                              *N,*19,CHKN3,*64,M$AP3,B1,MESSAGE;
                    endif                         
          Endif
.end patch 3.3      
.begin patch 3.3
          if        ((Ap3 <> 0 or XNinc <> 0) & CEXTCD  <> "I")      We have an IntraCorp transaction/transfer
          Sub       Trans,Trans
          add       Ap3,Trans                      .*lr
          add       Ap1,Trans                     .LO AP
          add       Xninc,Trans                             .*NIN
          MOVE      MASK102 TO M$Trans
          EDIT      Trans TO M$Trans
          add       Trans,TotTrans
          
          COMPARE   "57" TO LINES
          CALL      HEADER IF NOT LESS

                    If        (Ocompid <> "P"  and (Ocompid2 = "P" or Elstcde = "P"))
.NIN mailer PLI List          
.ap = LO
.AP3 & Xninc PLI as manager generally 10%
.lr & NIN to to NIN Generally 20%  as broker - cut check to NIN
                    PRINT     *N,*1,MLRN,"/",COBN,*12,LRN:
                              *26,"Pacific Lists",*50,M$AR,*64,M$Trans,"=":
                              *N,*64,M$AP3," + ":
                               *79,M$XNINC," + ",*91,M$AP1:
.begin patch 3.37
.                              *N,1,"** A/P1 above was in check to Pacific Lists - Please process this LR again ":
.                              "for payment to the List owner"
                              HPBON:
                              *N,1,"** A/P1 above is a Transfer to Pacific Lists -  PROCESS this LR again ":
                              "for payment to the List owner",HPBOFF
.end patch 3.37
                    ADD       C3 TO LINES
                    endif
.PLI mailer and NIN list
                    If        (Ocompid = "P"  and (Ocompid2 = "N" or Elstcde = "C"))
.ap = LO
.AP3 & Xninc NIN as manager generally 10% 
.lr & NIN to to PLI Generally 20%  as broker - cut check to PLI
                    PRINT     *N,*1,MLRN,"/",COBN,*12,LRN:
                              *26,"Names in the News",*50,M$AR,*64,M$Trans,"=":
                              *N,*64,M$AP3," + ":
                               *79,M$XNINC," + ",*91,M$AP1:
.begin patch 3.37
.                              *N,1,"** A/P1 above was  in check to Names in the News - Please process this LR again ":
.                              "for payment to the List owner"
                              HPBON:
                              *N,1,"** A/P1 above is a transfer to Names in the News - PROCESS this LR again ":
                              "for payment to the List owner",HPBOFF
.end patch 3.37
                    ADD       C3 TO LINES
                    endif
          Sub       Ap1,TotAP1      . I belv already taken care of --- never mind
.         Sub       Ap3,TotAP3
          Endif
          
.end patch 3.3

         cmatch    yes to negflag1
         if        equal
         move      m$ap1 to m$ap2
         goto      printneg
         endif
         cmatch    yes to negflag2
         if        equal
         goto      printneg
         else
         PRINT     *L
         goto       alines
         endif    
printneg PRINT     *L,*64,hpbon,"NEGATIVE: ",M$AP2,hpboff
ALINES   
.begin patch 3.3
.         ADD       "3" TO LINES
          ADD       c4 TO LINES
.end patch 3.3
         clear     negflag1
         clear     negflag2
.begin patch 3.3         
         clear     negflag3
.         WRITE     RECON,SEQ;MLRN,COBN,LRN,CHKN1,LON:
.                   *zf,ar,AP1,NININC,LRINC,TAXES,TAXES,POST,CNUM:
.                   SYSMO,SYSDY,SYSYR,pcomp
         WRITE     RECON,SEQ;MLRN,COBN,LRN,CHKN1,LON:
                   *zf,ar,AP1,NININC,LRINC,TAXES,TAXES,POST,CNUM:
                   SYSMO,SYSDY,SYSYR,pcomp,Ccompid,XNinc
          if        (AP2 <> c0)
.         COMPARE   C0 TO ap2
.         GOTO      MOARITE IF EQUAL
.         WRITE     RECON,SEQ;MLRN,COBN,LRN,CHKN2,LON:
.                   ar,ap2,NININC,LRINC,TAXES,TAXES,POST,CNUM:
.                   SYSMO,SYSDY,SYSYR,pcomp
          WRITE               RECON,SEQ;MLRN,COBN,LRN,CHKN2,LON:
                    ar,ap2,NININC,LRINC,TAXES,TAXES,POST,CNUM:
                    SYSMO,SYSDY,SYSYR,pcomp,Ccompid,XNinc
          endif
.begin patch 3.33
.         IF        (Trans <> c0)
          IF        (Trans <> c0 & CEXTCD  <> "I")                .Only put in recon if it happenned in this control
.end patch 3.33
.begin patch 3.37    . no real check so don't put in recon
.                   WRITE               RECON,SEQ;MLRN,COBN,LRN,CHKN3,LON:
.                             ar,trans,NININC,LRINC,TAXES,TAXES,POST,CNUM:
.                             SYSMO,SYSDY,SYSYR,pcomp,Ccompid,XNinc
                   WRITE     Transfer,SEQ;MLRN,COBN,LRN,CHKN3,LON:
                             ar,trans,NININC,LRINC,TAXES,TAXES,POST,CNUM:
                             SYSMO,SYSDY,SYSYR,pcomp,Ccompid,XNinc
.end patch 3.37    . no real check so don't put in recon
          endif
.end patch 3.3         
MOARITE   cmatch    yes to escflag
          goto      input if not equal 
.........
.begin patch 3.34
.if int is an inter company transfer and not code "I" DO NOT WRITE MOA
          if        (Trans <> c0 & CEXTCD  <> "I")
          goto      INput
          endif
.end patch 3.34

          move      z3 to mcnt
.START PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
.         PACK      NMOAFLD FROM nescmlr,mcnt
.Temporary
          pack      COMPFLD,NESCMLR
          move      "MOARITE-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          PACK      NMOAFLD FROM COMPOLDMLR,mcnt
.END PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
         move      c4 to nmoapath
         MOVE      CLR TO LRNUM
.START PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
.         PACK      NMOAFLD4 FROM nescbrk,nescmlr
.Temporary
          move      COMPOLDMLR,str6
.
          pack      COMPFLD,nescbrk
          move      "MOARITEB-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          call      Trim using COMPOLDBRK
          if (COMPOLDBRK = "")
                    move      "0000",COMPOLDBRK
          endif
          PACK      NMOAFLD4 FROM COMPOLDBRK,str6
.END PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
         REP       ZFILL IN NMOAFLD4
         pack      recdate from syscc,sysyr,sysmo,sysdy
         pack      trandate from syscc,sysyr,sysmo,sysdy
         move      "19" to reason
.START PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
.         move      nescmlr to mlr
.         move      nescmlr to nmobmlr
          move      str6 to mlr
          move      str6 to nmobmlr
.END PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
         move      mcnt to nmobmcnt
         move      cnum to control
         move      invnum to invoice
.HARDCODED CENTURY, TEMPORARY!!!!
         call      TRIM using invdtey
         count     N1,invdtey
         if (N1 > 0)
                    pack      invdate from invdtec,invdtey,invdtem,invdted         
.                   pack      invdate from cc,invdtey,invdtem,invdted
         else
                   clear     invdate
         endif
         MOVE      C0 TO ONAMOUNT
.START PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
.         MOVE      NESCBRK TO NMOABRK
.         move      NESCBRK to nmobbrk
.         move      NESCBRK to nmoabrk
         MOVE      COMPOLDBRK TO NMOABRK
         move      COMPOLDBRK to nmobbrk
         move      COMPOLDBRK to nmoabrk
.END PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
         COMPARE    C0 TO ap2
         IF        EQUAL
         move      c0 to change
         move      ap1 to change
         COMPARE   C0 TO CHANGE                    -NO AP ?
         GOTO      INPUT IF EQUAL                  -YES EXIT
         mult      seq by change
         move      change to onamount
         ADD       AP1 TO TOTESCRO
         ELSE
         move      c0 to change
         move      ap2 to change
         COMPARE   C0 TO CHANGE                    -NO AP ?
         GOTO      INPUT IF EQUAL                  -YES EXIT
         mult      seq by change
         move      change to onamount
         ADD       ap2 TO TOTESCRO
         ENDIF
         move      olnum to LIST
         CLEAR     CHECKNUM
.dlh 14mar95
         MOVE      "NONANXT" TO GNXTFLD
         CALL      GNXTKEY
         MOVE      GNXTNUM TO n7
ADDONEB  ADD       C1 TO n7
         MOVE      n7 TO TRANSNUM
         REP       ZFILL IN TRANSNUM               .dlh 13mar95
         MOVE      TRANSNUM TO NMOAFLD
         MOVE      C2 TO NMOAPATH
         CALL      NMOATST
         GOTO      ADDONEB IF NOT OVER
         MOVE      n7 TO STR7
         BUMP      STR7 BY 1
         MOVE      STR7 TO GNXTNUM
         REP       ZFILL IN GNXTNUM
         CALL      GNXTUPD
.NOTE GNXT ONLY ALLOWS 6 BYTE NUMBER. DLH.
         MOVE      "0" TO INAMOUNT
         move      olnum to LIST
.....dlh-end
         MOVE       C2 TO NMOBPATH
         MOVE       INITS TO NMOAINIT

;begin patch 3.11
;Nmoafld is reused during gnxt routine so must be rebuilt
.START PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
.               PACK      NMOAFLD FROM nescmlr,mcnt
.Temporary
          pack      COMPFLD,NESCMLR
          move      "ADDONEB-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          PACK      NMOAFLD FROM COMPOLDMLR,mcnt
.END PATCH 3.13 REPLACED LOGIC - TEMPORARY PATCH
;end patch 3.11
.begin patch 3.31
         Move       CCompID,MoaCOmp
.end patch 3.31

         CALL       NMOAWRT
         CALL       NMOBUPD
.begin patch xxx
.         display    *p1:22,*el,"mob updated",*b;
         display    *p1:22,*el,"mob updated";
.end patch xxx
         GOTO       INPUT
*......................................................................
.
TOTAL    COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         MOVE      mask92b TO MT$AR
         EDIT      TOTAR TO MT$AR
         MOVE      mask92b TO MT$MOA
         MOVE      mask92b TO MT$NEG
         mult      seq by totneg
         EDIT      TOTneg TO MT$neg
         MOVE      mask92b TO MT$AP1
         EDIT      TOTAP1 TO MT$AP1
         MOVE      mask92b TO MT$AP2
         EDIT      TOTAP2 TO MT$AP2
         MOVE      mask92b TO MT$NINC
         EDIT      TOTNIN TO MT$NINC
.Begin patch 3.3

         MOVE      mask92b TO MT$AP3
         EDIT      TOTAP2 TO MT$AP3
         MOVE      mask92b TO MT$XNINC
         EDIT      TOTXNIN TO MT$XNINC
.end patch 3.3
.Begin patch 3.31
         MOVE      MASK92b TO MT$LRINC3
         EDIT      TOTLR3 TO MT$LRINC3
.end patch 3.31

         MOVE      mask92b TO MT$LRINC
         EDIT      TOTLR TO MT$LRINC
         MOVE      mask92b TO MT$STAX
         EDIT      TOTSTAX TO MT$STAX
         MOVE      MASK62 TO MT$CTAX
         EDIT      TOTCTAX TO MT$CTAX
         MOVE      MASK52 TO MT$POST
         EDIT      TOTPOST TO MT$POST
         COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         PRINT     *L,*26,TOTHEAD," By Control",*48,MT$AR,*62,MT$AP1:
                   *77,MT$NINC,*90,MT$LRINC:
                   *117,MT$CTAX:
.Begin patch 3.3
.                   *L,*62,MT$AP2,*107,MT$STAX,*124,MT$POST
.Begin patch 3.31
.                   *L,*62,MT$AP2,*107,MT$STAX,*124,MT$POST:
                   *N,*62,MT$AP2,*77,MT$NINC,*90,MT$LRINC3," *",*103,MT$STAX,*124,MT$POST:
.end patch 3.31
                   *L,*62,MT$AP3,*77,MT$XNINC
.end patch 3.3
         MOVE      C0 TO TOTAP
         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
.Begin patch 3.34
.Begin patch 3.3
.         ADD       TOTAP3 TO TOTAP
.end patch 3.3
.end patch 3.34
         MOVE      mask92b TO MT$AP1
         EDIT      TOTAP TO MT$AP1
         MOVE      mask92b TO MT$ESC
         EDIT      TOTESCRO TO MT$ESC
         COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         PRINT     *26,"TOTAL ACCOUNTS AP 1 & 2",*62,MT$AP1
         PRINT     *26,"TOTAL APPLIED FROM MOA",*48,totmoa
         PRINT     *26,"TOTAL APPLIED TO MOA",*62,MT$ESC
         PRINT     *26,"TOTAL NEGATIVE AP'S",*48,MT$NEG
.begin patch 3.3
         MOVE      mask92b TO MT$AP3
         EDIT      TOTAP3 TO MT$AP3
         PRINT     *26,"TOTAL Inter-CO PAYABLE",*90,MT$AP3
         MOVE      MASK92b TO MT$Trans
         EDIT      TOTTrans TO MT$Trans
         PRINT     *26,"TOTAL InterComp Transfer's ",*62,MT$Trans
.begin patch 3.32
          MOVE                mask92b TO MT$AP3
          MOve      co,Totcheck
          add       Totap,Totcheck
          Add       TotTrans,Totcheck
.begin patch 3.34
.begin patch 3.33
.         sub       totap3,Totcheck                   .included in transfer
.end patch 3.33
.end patch 3.34
          edit      Totcheck,MT$AP3
.begin patch 3.37
.          PRINT               *26,"TOTAL Check $ Amount ",*62,MT$AP3
          PRINT               *26,"TOTAL Payable $ Amnt ",*62,MT$AP3
.end patch 3.37

.end patch 3.32
.end patch 3.3
         PRINT     *FLUSH
         RETURN
.............................................................................
HEADER
         ADD       c1 TO PAGE
         compare    c1 to page
         if         equal
;         print      hp17ptch,hpdupl,*f
         PRINT     HPtmsr17,hpdupl,hptop:                .compressed
                   033,"&l66P":               page length
                   033,"&l65F":               number lines
                   *f
         endif
          unpack    cnumdate,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
.begin patch 3.3
          if        (Company = c2)
      PRINT     *f,*n,*1,"CONTROL NO. ",CNUM,"-",str10:
                   *31,"***  P L I   C H E C K   ":
                   *57,"R E G I S T E R   R E P O R T  ***":
                   *116,"DATE ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *L,*L,*1,"CLIENT",*12,"LR ##":
                   *26,"LIST OWNER PAY-TO":
                   *52,"--------ACCOUNTS--------":
                   *79,"-------COMMISSIONS------":
                   *108,"------TAXES----",*128,"OUR":
                   *L,*1,"NUMBER",*10,"LO-INV":
                   *19,"CHECK ##",*53,"RECEIVABLE":
                   *69,"PAYABLE",*81,"NIN INCOME":
                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
                   *126,"POSTAGE",*L
          Else
         PRINT     *f,*n,*1,"CONTROL NO. ",CNUM,"-",str10:
                   *31,"***  N I N   C H E C K   ":
                   *57,"R E G I S T E R   R E P O R T  ***":
                   *116,"DATE ",DATEMASK:
                   *L,*1,"CONFIDENTIAL",*116,"PAGE ",PAGE:
                   *L,*L,*1,"CLIENT",*12,"LR ##":
                   *26,"LIST OWNER PAY-TO":
                   *52,"--------ACCOUNTS--------":
                   *79,"-------COMMISSIONS------":
                   *108,"------TAXES----",*128,"OUR":
                   *L,*1,"NUMBER",*10,"LO-INV":
                   *19,"CHECK ##",*53,"RECEIVABLE":
                   *69,"PAYABLE",*81,"NIN INCOME":
                   *94,"LR INCOME",*109,"STATE",*119,"CITY":
                   *126,"POSTAGE",*L
          EndIf
.end patch 3.3
.END PATCH 3.1 ADDED LOGIC
         MOVE      "6" TO LINES
         RETURN
.
*............................................................
.
.begin patch 3.3
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:23,*EL,*B,"FORMAT ERROR READING LR: ",LRN
.         RETURN                                POP THE STACK.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.End Patch 3.3
.
EOJ      
         MOVE      "REGISTER" TO TOTHEAD
         CALL      TOTAL
         READ      MTDFILE,RECNUM;P:
                   MTDACR:
                   MTDFILL1:
                   MTDACP:
                   MTDFILL2:
                   MTDSAP:
                   MTDFILL3:
                   MTDNIN:
                   MTDFILL4:
                   MTDLR:
                   MTDFILL5:
                   MTDSTX:
                   MTDFILL6:
                   MTDCTX:
                   MTDFILL7:
                   MTDPOST:
                   MTDFILL8:
                   MTDCNTNO:
                   MTDFILL9:
                   MTDMO:
                   MTDDY:
                   MTDYR:
                   ctdap1:
                   ctdap2:
                   ctDap3:
                   MTDXNIN:
                   MTDSAP3
.Begin patch 3.3 added ctdap3 & mtdXnin
.end patch 3.3 added ctdap3 & mtdXnin

         DISPLAY   *P10:17,MTDMO,B4,HOLDMO,*W4
         COMPARE   MTDMO TO HOLDMO
         GOTO      NEWMON IF NOT EQUAL
         COMPARE   UNO TO cntrl
         GOTO      ZEROAR IF EQUAL
         ADD       TOTAR TO MTDACR
         ADD       TOTAP1 TO MTDACP
         ADD       TOTAP2 TO MTDSAP
         ADD       TOTAP1 TO ctdaP1
         ADD       TOTAP2 TO ctdAP2
.Begin patch 3.3
         ADD       TOTAP3 TO ctdAP3
         ADD       TOTxNIN TO MTDxNIN
         ADD       TOTAP3 TO MTDSAP3
.End patch 3.3
         ADD       TOTNIN TO MTDNIN
         ADD       TOTLR TO MTDLR
         ADD       TOTSTAX TO MTDSTX
         ADD       TOTCTAX TO MTDCTX
         ADD       TOTPOST TO MTDPOST
         MOVE      cntrl TO MTDCNTNO
         ADD       c1 TO MTDCNTNO
         MOVE      SYSMO TO MTDMO
         MOVE      SYSDY TO MTDDY
         MOVE      SYSYR TO MTDYR
         GOTO      WRITEMTD
...............................................................................
.newmon - month change & 1st control of newmonth ?
NEWMON
         COMPARE   UNO TO cntrl
         GOTO      ZEROAPS IF NOT EQUAL
...............................................................................
ZEROAR   MOVE      C0 TO MTDACR
         ADD       TOTAR TO MTDACR
         ADD       TOTAP1 TO MTDACP
         ADD       TOTAP2 TO MTDSAP
.begin patch 3.3
          ADD                 TOTAP3 TO MTDSAP3
          MOVE                C0 TO MTDXNIN
          Add       TotXnin,mtdXnin
          move                c0 to ctdap3
.end patch 3.3
         move      c0 to ctdap1
         move      c0 to ctdap2
         ADD       TOTAP1 TO ctdAP1
         ADD       TOTAP2 TO ctdAP2
         MOVE      C0 TO MTDNIN
         ADD       TOTNIN TO MTDNIN
         MOVE      C0 TO MTDLR
         ADD       TOTLR TO MTDLR
         MOVE      C0 TO MTDSTX
         ADD       TOTSTAX TO MTDSTX
         MOVE      C0 TO MTDCTX
         ADD       TOTCTAX TO MTDCTX
         MOVE      C0 TO MTDPOST
         ADD       TOTPOST TO MTDPOST
         MOVE      cntrl TO MTDCNTNO
         ADD       c1 TO MTDCNTNO
         MOVE      SYSMO TO MTDMO
         MOVE      SYSDY TO MTDDY
         MOVE      SYSYR TO MTDYR
         GOTO      WRITEMTD
ZEROAPS
         move      c0 to ctdap1
         move      c0 to ctdap2
         ADD       TOTAP1 TO ctdAP1
         ADD       TOTAP2 TO ctdAP2
.begin patch 3.3
         move      c0 to ctdap3
         ADD       TOTAP3 TO ctdAP3
         ADD       TOTXNIN TO MTDXNIN
         ADD       TOTAP3 TO MTDSAP3
.end patch 3.3
      
         ADD       TOTAR TO MTDACR
         ADD       TOTAP1 TO MTDACP
         ADD       TOTAP2 TO MTDSAP
         ADD       TOTNIN TO MTDNIN
         ADD       TOTLR TO MTDLR
         ADD       TOTSTAX TO MTDSTX
         ADD       TOTCTAX TO MTDCTX
         ADD       TOTPOST TO MTDPOST
         MOVE      cntrl TO MTDCNTNO
         MOVE      SYSMO TO MTDMO
         MOVE      SYSDY TO MTDDY
         MOVE      SYSYR TO MTDYR
WRITEMTD 
         MOVE      cntrl TO MTDCNTNO
         ADD       c1 TO MTDCNTNO
         MOVE      MTDACR TO TOTAR
         MOVE      MTDACP TO TOTAP1
         MOVE      MTDSAP TO TOTAP2
         MOVE      MTDNIN TO TOTNIN
         MOVE      MTDLR TO TOTLR
         MOVE      MTDSTX TO TOTSTAX
         MOVE      MTDCTX TO TOTCTAX
         MOVE      MTDPOST TO TOTPOST
.begin patch 3.3
         MOVE      MTDSAP3 TO TOTAP3
         MOVE      MTDXNIN TO TOTXNIN

.end patch 3.3
         MOVE      "  M-T-D " TO TOTHEAD
         CALL      TOTAL
         WRITE     MTDFILE,RECNUM;P:
                   MTDACR:
                   MTDFILL1:
                   MTDACP:
                   MTDFILL2:
                   MTDSAP:
                   MTDFILL3:
                   MTDNIN:
                   MTDFILL4:
                   MTDLR:
                   MTDFILL5:
                   MTDSTX:
                   MTDFILL6:
                   MTDCTX:
                   MTDFILL7:
                   MTDPOST:
                   MTDFILL8:
                   *ZF,MTDCNTNO:
                   MTDFILL9:
                   MTDMO:
                   MTDDY:
                   MTDYR:
                   ctdap1:
                   ctdap2:
                   ctDap3:
                   MTDXNIN:
                   MTDSAP3
.begin patch 3.3 added CTdAP3, mtdxnin, mtdsap3                   
.end patch 3.3 added CTdAP3, mtdxnin, mtdsap3                   
CONTRUPD
         move      cntrl to str3
         pack      nctrfld FROM str3,CNTRDATE
         rep       zfill in nctrfld
         call      nctrkey
         move      c2 to nctrcode
         call      nctrupd
.
         IFNZ      PC
         FLUSH     MTDFILE
         CLOSE     MTDFILE
         XIF
         IFZ       PC
         CLOSE     MTDFILE
         XIF
         WEOF      RECON,SEQ
         CLOSE     RECON
.begin patch 3.37         
         WEOF      Transfer,SEQ
         CLOSE     Transfer
.end patch 3.37         
         MOVE      mask92b TO MT$AP1
         move      c0 to totap1
         add       ctdap1 to totap1
         EDIT      TOTAP1 TO MT$AP1
         move      c0 to totap2
         add       ctdap2 to totap2
         MOVE      mask92b TO MT$AP2
         EDIT      TOTAP2 TO MT$AP2
         COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         PRINT     *L,*26,TOTHEAD," TOTAL A/P One By Check Date.",*62,MT$AP1:
                   *L,*62,MT$AP2
         MOVE      C0 TO TOTAP
         ADD       TOTAP1 TO TOTAP
         ADD       TOTAP2 TO TOTAP
         MOVE      mask92b TO MT$AP1
         EDIT      TOTAP TO MT$AP1
         COMPARE   "57" TO LINES
         CALL      HEADER IF GREATER
         PRINT     *26,"TOTAL A/P by Check Date",*62,MT$AP1
         PRINT     *FLUSH
EOJ1     splclose
         shutdown  "cls"
         STOP
.begin patch 3.3
FileIO
      Move    "This is a Error e-mail from Nchk0003",MailSubjct

.   Set the text message that is send with the attachments
          Clear     MailBody
          append    "This is an error message",MailBody
          Append    CRLF,MailBody
          Append    "This is from subroutine FileIO",MailBody         
          Append    CRLF,MailBody
          Append    error,MailBody      
          Append    CRLF,MailBody
          reset     MailBody
          pack      MailFrom from User,"@nincal.com"
          move      "InformationServices@nincal.com",MailTO
          call      SendMail
          shutdown  "cls"
          STOP

.
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
         INC       NPAYIO.inc
         INC       NADJIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NOWNIO.inc
         INCLUDE   NCSHIO.inc
         INCLUDE   NDAT3IO.INC
          INC                 ninvio.inc
          Include   NInvAcdIO.inc
          INCLUDE             compute.inc
         include   nacdio.inc
         include   ndatio.inc
         include   nshpio.inc
         INCLUDE   NESCIO.INC
         INCLUDE   NMOAIO.INC
         INCLUDE   NMOBIO.INC
         include   gnxtio.inc
         include   nmrgio.inc
         include nctrio.inc
         INCLUDE   COMLOGIC.inc
