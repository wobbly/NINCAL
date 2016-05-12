. ...............................................................................
.
PC       EQU       0
.
          INC       COMMON.inc
          INCLUDE   CONS.inc
          INCLUDE   CONSACCT.inc
          INC       NCSHDD.inc
          INC       ninvdd.inc
          Include   NInvAcddd.inc
          INCLUDE   COMPDD.inc
          INCLUDE   CNTDD.inc
          INC       NBILDD.inc
          INC       NORDDD.inc
          INC       NOWNDD.inc
          INC       NPAYDD.inc
          INC       NADJDD.inc
          inc       njstdd.inc
          INCLUDE   NACDDD.inc
          INCLUDE   NDATDD.INC
          include   nshpdd.inc
          INC       NMTXDD.inc
          INCLUDE   NDAT3DD.INC
          INCLUDE   NESCDD.INC
          include   nmrgdd.inc
          INCLUDE   NRTNDD.inc
          INCLUDE   NSEL2DD.INC
          inc       hp.inc

.
.* Need to add code to cut adjustment record when external code is 14.
.* DLH 10/20/94.
.
RELEASE   INIT     "3.43"              DLH Review Inter comp Transfer
Reldate   Init      "12 Nov 2009"
.RELEASE   INIT     "3.42"              DLH add missing CNR on CV to match CV produced by Nmrg0002
.Reldate   Init      "26 Feb 2009"
.RELEASE   INIT     "3.41"              DLH Suppress checks for Intercomp transfer
.Reldate   Init      "25 Feb 2009"
.RELEASE   INIT     "3.40"              DLH Change to always use NIN account
.Reldate   Init      "16 Feb 2009"
.RELEASE   INIT     "3.39"              JD  added adjusted ap1 back in Interco transfer
.Reldate  Init      "14 May 2008"
.RELEASE   INIT     "3.38"             DLH Escrow file correction
.Reldate  Init      "20 February 2008"
.RELEASE   INIT     "3.37"             DLH extra code intra company transfer with adjusted ap1
.Reldate  Init      "20 February 2008"
.RELEASE   INIT     "3.36"             DLH Extra safety check with INvoice vars before update
.Reldate  Init      "29 November 2007"
.RELEASE   INIT     "3.35"             DLH 11July2007  Pacific Lists
.Reldate  Init      "10 Sept 2007"
.See saved version 3.33 for ealier patches
.RELEASE   INIT     "3.34"             DLH 11July2007  Pacific Lists
.Reldate  Init      "11 July 2007"
.See saved version 3.33 for ealier patch details
.RELEASE   INIT     "3.33"             JD12oct2005  saving nmrgnet, getting overwritten by compute.
.RELEASE   INIT     "3.32"             JD06oct2005  new var nmrgdd.inc nmrgdisa
.RELEASE   INIT     "3.31"             JD 2005Sept13  Qtybild getting overwritten.
.RELEASE   INIT     "3.3"             DLH 2005March02 Invoice Conversion
.RELEASE   INIT     "3.26"             ASH 12JAN05  ESCROW CONVERSION
.RELEASE   INIT     "3.25"             JD  15DEC04  Turned of duplexing. 
.RELEASE   INIT     "3.21"             JD  21JULY04  moved LOINV#. 
.RELEASE   INIT     "3.20"             JD  25JUNE04  new var nmrgdd.inc
.RELEASE   INIT     "3.11"            ASH 27MAY2004 MAILER CONVERSION
.RELEASE   INIT     "3.1"            JD26May2004 print full payto company info.
.RELEASE   INIT     "3.01"          JD24Mar2004 turned off HPFIXED print for CV on Stub.
.RELEASE   INIT     "3.00"          JD   11Mar2004 New signature print and mailer tax note.
.RELEASE   INIT     "2.87"         ASH  26JAN2004 DATACARD CONVERSION
.RELEASE   INIT     "2.86"         ASH  27JAN2003 FIXED A SMALL BUG NOT CAUGHT WHEN INVOICE FILE WAS REORGANIZED
.RELEASE   INIT     "2.85"         DLH  6Mar2002 New Note on stub.
.RELEASE   INIT     "2.84"         jd  12dec2001 New signature print pos.
.RELEASE   INIT     "2.83"        DLH 22OCT2000 New external codes
.RELEASE   INIT     "2.82"        ASH 04OCT2000 NEW SERVER ADDED
.RELEASE   INIT     "2.81"        JD  02Nov99 use correct var  
.RELEASE   INIT     "2.8"        dlh 20Aug99 adjust Y2K
.RELEASE   INIT     "2.7"        dlh 26APR99 NININV Y2K
.REVIEW DATES ( PENDING)
.RELEASE   INIT     "2.6"        ASH 15JAN99 NINORD Y2K, File expansion
.RELEASE   INIT     "2.5"         JD 03dec98 print tax status below mcomp.
.RELEASE   INIT     "2.4"        JD 11apr96 print cv on stub
.RELEASE   INIT     "2.3"        DLH BUG FIX WAS READING ORDER TO LATE IN CYCLE
.RELEASE   INIT     "2.2"         JD 21mar95 include nmrgdd for new compute chng.
.RELEASE   INIT     "2.1"        JD 10jan95 flag escrow clients
.RELEASE  INIT      "2.0"        DLH 05JAN95 ACCOUNT FOR THE ODD CIRCUMSTANCE
.                                WHEN NO NO A/P1 BUT THERE IS AN A/P2. PREVIOUS
.                                CODE WOULD CUT A CHECK FOR A/P1 ANYWAY
.RELEASE  INIT      "1.9"        DLH 14NOV94 CONSACCT. NEW COMPUTE, SPLITS.
.RELEASE  INIT     "1.8"         JD  12aug94  print to laser.
.RELEASE  INIT     "1.7"         JD  30JUN94  SPOOL OUTPUT.
.RELEASE  INIT     "1.6"         DLH 09AUG93 PRINT pay-to info on stub.
.RELEASE  INIT     "1.5"         DLH 27APR93 INSERT CHECK NUMBERS TO AIM FILE.
.RELEASE  INIT       "1.4"         DLH 26APR93   ADD MLR CHECK # TO INV REC.
.RELEASE  INIT       "1.3"         DLH 09MAR93   PAR27N.
.RELEASE  INIT      "1.2"         DLH 04MAR93   MINOR CLEANUP COMMENT OUT SOME
.                                UNUSED VARIABLES.
.RELEASE  INIT      "1.0"        JOSE DUENAS     04/25/90.
.RELEASE  INIT      "1.1"        JOSE DUENAS     05/07/92 INCLUDE'S.
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       6
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
.
DATEMASK DIM       8
chk1date dim       8
.FILES.
...............................................................................
.
DATEFILE FILE                             
.*DATE ENTERED FOR CHECK CONTROL.
CHKMST   FILE      FIXED=78
.
. WORK VARIABLES
.
.
.
.
.
shipsw   dim        1
mrgsw    dim        1
XQTY     FORM      9
NUM      FORM      1
CENTS    FORM      2
AMT      DIM       17
FORM7    FORM      7
COUNT    FORM      5
COUNTP   FORM      5
.
.CHKMST FILE VARIABLES
CHECK    DIM       6
CHKCODE  DIM       1
.begin patch 3.41
CHKICODE  DIM       1
Icheck    Dim       6
Fill64    Dim       64
.FILL71   DIM       71
.end patch 3.41
.
.
.PRINT MASK VARIABLES
.
MASK92b   INIT      "$,***,***,***.99-"
MASK72   INIT      "$***,***.99-"
M$AP1    DIM       17
M$AP2    DIM       17
.begin patch 3.35
M$AP3    DIM       17
.end patch 3.35
CHKNUM   FORM      6
.begin patch 3.41
CHKINUM   FORM      6
.end patch 3.41
STAT501  DIM       6
TAX501   FORM      1
APAMT    FORM      9
REFERENC DIM       12
SPACES   INIT      "           "
REF      INIT      "REFERENCE:##"
APCHECK  FORM      "000000001"
.begin patch 3.35
AP3SW    DIM       1
Intraflag Dim       1                      .set to Y if ap3 or xninc > 0 and not Cextcd "I"
.                                      adds AP1 to check to PL or NIN for inter comp transfer
AP3AMT   FORM      10.2
.end patch 3.35
AP2SW    DIM       1
ADJSW    DIM       1 
APSW     DIM       1
DIM1     DIM       1
TIPE     DIM       8
STAT     INIT      "P"
NOCHK    INIT      "NO CHK"
ESCROW   INIT      "ESCROW"
ESCFLAG  DIM       1
LOWN     FORM      4
REP      DIM       1
febdat  form      5
feb      dim       1 
mrgy     dim       1
prtlines form      2
RTNTAB   FORM      3
PERCENT  FORM      4.2
CALCmPER  FORM      7.4
TOTNCOA  FORM      8
TOTBILL  FORM      8
invqty1  form      7
invqty2  form      7
Num26    form      "26"
PayToSW   dim       1
ap2save  form      10.2
ap1save  form      10.2
ap3save  form      10.2                           .*LR                                                                                      
Xnincsave  form      10.2     .*NIN
ap1amt   form      10.2
qtybild2 form      9
.
mrgnetsv form      8
.BEGIN PATCH 3.34
CHKNflag  Dim       1         .flag for check ## file
.END PATCH 3.34
         MOVE      "NCHK002L" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      C1 TO NINVPATH
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NPAYPATH
         MOVE      C1 TO NMLRPATH
         MOVE      C1 TO NCSHPATH
         move      c2 to nescpath
         move      c1 to ncshflag
         MOVE       "02" TO MM
         MOVE       "09" TO DD
         MOVE       "96" TO YY
         CALL      CVTJUL
         move      juldays to febdat
         MOVE      "NINCSH" TO NCSHNAME
         open      ncshfile,ncshname
         MOVE      "CHECK PRINT PROGRAM" TO STITLE
         CALL      PAINT
         IFNZ      PC
         OPEN      DATEFILE,"CHECK_DATE"
         XIF
         IFZ       PC
         OPEN      DATEFILE,"CHKDATE"
         XIF
         READ      DATEFILE,SEQ;DATE
         CLOSE     DATEFILE
         MOVE      "99/99/99" TO DATEMASK
         EDIT      DATE TO DATEMASK
         UNPACK    DATE INTO SYSMO,SYSDY,SYSYR
         UNPACK    DATE INTO MM,DD,YY
         pack       chk1date from cc,yy,mm,dd
          CALL      DATETEST
         BRANCH    DATEFLAG OF BADDATE
         GOTO      OK
BADDATE  KEYIN     *HON,*BLINKON,"BAD DATE",*B
         GOTO      BADDATE
OK      
         PACK      STR35,NTWKPATH1,"NCHK2.LST"
         SPLOPEN   STR35
.begin patch 3.35
.begin patch 3.40
          OPEN                CHKMST,"PAR27N"
          FILEPI              1;CHKMST
.begin patch 3.41
.          READ                CHKMST,SEQ;CHKCODE,CHECK,FILL71
          READ                CHKMST,SEQ;CHKCODE,CHECK,CHKIcode,Icheck,FILL64
.end patch 3.41
          CLOSE               CHKMST
          If        (Company = c1)
.                   OPEN                CHKMST,"PAR27N"
.                   FILEPI              1;CHKMST
.                   READ                CHKMST,SEQ;CHKCODE,CHECK,FILL71
.                   CLOSE               CHKMST
          Move      "N",CHKNflag
          Elseif    (Company = c2)
.                   OPEN                CHKMST,"PAR27P"
.                   FILEPI              1;CHKMST
.                   READ                CHKMST,SEQ;CHKCODE,CHECK,FILL71
.                   CLOSE               CHKMST
.end patch 3.40
          Move      "P",CHKNflag
          endif
.         CALL       NCSHSEQ                      .get first record for compid
.         GOTO      DONE IF OVER                   .get out if over
.            open      ncshfile,ncshname              .re-open file so no problems 
.         if        (CCOMPID = " " or CCOMPID = "N")
.                   OPEN      CHKMST,"PAR27N"
.                   FILEPI    1;CHKMST
.                   READ      CHKMST,SEQ;CHKCODE,CHECK,FILL71
.                   CLOSE     CHKMST
.                   Move      "N",CHKNflag
.                   elseIf    (CCOMPID = "P")
.                   OPEN      CHKMST,"PAR27P"
.                   FILEPI    1;CHKMST
.                   READ      CHKMST,SEQ;CHKCODE,CHECK,FILL71
.                   CLOSE     CHKMST
.                   Move      "P",CHKNflag
.                   endif
.end patch  3.34
.end patch 3.35
          MOVE      CHECK TO CHKNUM
.begin patch 3.41
          MOVE      ICHECK TO CHKINUM

.end patch 3.41
.
INPUT     CALL       NCSHSEQ
          GOTO      DONE IF OVER
          ADD       C1 TO COUNT
          DISPLAY   *P10:12,"NUMBER OF CHECK ENTRIES PROCESSED: ",COUNT
CHKEXT
.EXTERNALS ? OR NO LR ## - IF SO SKIP
.begin patch 3.35
.                   CMATCH    "M" TO CEXTCD
.                   GOTO      INPUT IF EQUAL
.                   if         (CEXTCD = "D" or CEXTCD = "d" or CEXTCD = "N")
.                   if         (CEXTCD = "D" or CEXTCD = "d" or CEXTCD = "N" or CEXTCD = "M" or CEXTCD = "I")
          if         (CEXTCD = "D" or CEXTCD = "d" or CEXTCD = "N" or CEXTCD = "M")
.end patch 3.35
          goto       input
          endif
          CMATCH    B1 TO CLR
          GOTO      INPUT IF EQUAL
..................................................

          move      no to PayToSW
          MOVE      CLR TO NINVFLD
          REP       ZFILL IN NINVFLD
          CALL      NINVKEY
          GOTO      INPUT IF OVER
          CMATCH    "P",STATB                          .PREVIOUSLY CLOSED?
          GOTO      INPUT IF EQUAL                     .YES
          GOTO      INPUT IF EOS                       .INVALID STATUS
.begin patch 3.36
          Clear     InvVarsHold
          pack      InvVarsHold,InvVars
.end patch 3.36
.begin patch 3.35
          if        (CEXTCD = "P" or CEXTCD = "Q")
.                   CMATCH    "P" TO CEXTCD                    .LO was prepaid do not cut check or update check number
.                   GOTO      UPDPRE IF EQUAL
.                   CMATCH    "Q" TO CEXTCD                    .prepaid to lo & pulling MOA
.                   GOTO      UPDPRE IF EQUAL
.begin patch 3.36
          unpack    Invvarshold,Invvars
          unpack    NCSHCHK,str6,str6
          Move      Stat to Statb
          Move      str6,IMLRCHK
.         move      ap2save to ap2
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
          pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
          call      NInvUpd
          
          CALL      UPDADJ
          GOTO      INPUT
          endif

.
.end patch 3.35
          move      ap2 to ap2save
.begin patch 3.35 save them all  --- PLI
          move      ap1 to ap1save
          move      ap3 to ap3save
          move      XNinc to Xnincsave
.begin patch 3.35 save them all  --- PLI

          MOVE      LRN TO NORDFLD                  .GET ORDER INFO 
          REP       ZFILL IN NORDFLD
          CALL      NORDKEY
          CALL      CODE IF NOT OVER                .SET RENT/EXCHANGE MASK
          MOVE      ORTNNUM TO NRTNFLD
          REP       ZFILL IN NRTNFLD
          CALL      NRTNKEY
          CALL      CNTRTN
.
          MOVE      C1 TO NDATPATH
          move      olnum to ndatfld
          call      ndatkey
          call      wipecvars
          MOVE      NinvFLD to nshpfld
          REP       ZFILL IN NshpFLD
          move      no to shipsw
          call      nshpkey
          if        not over
          move      yes to shipsw
          endif

          MOVE      YES TO SUBPPSW                   .SET FLAG FOR COMPUTE
          MOVE      NinvFLD to nmrgfld
          REP       ZFILL IN NMRGFLD
          move      c0 to nmrgrqty
          move      c0 to nmrgiqty
          move      c0 to nmrgnet
          move      c0 to mrgnetsv
          CALL      NMRGKEY
          if        over
          move      no to mrgy
          move      no to mrgsw
          else
          move      yes to mrgy
          move      yes to mrgsw
          move      nmrgnet to mrgnetsv
          endif
          call      Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
          CALL      COMPUTE
          CALL      chkADJ
          MOVE      C0 TO APAMT
          MOVE      AP TO APAMT
          MOVE      YES TO APSW
          COMPARE   APCHECK TO APAMT
          IF          NOT GREATER
          MOVE      NO TO APSW
.begin patch 3.36
          Unpack    InvVarsHold,Invvars
          unpack    NCSHCHK,str6,str6
          Clear     CHKN1
          Move      NoChk to CHKN1
          Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
          Move      str6,IMLRCHK
          Move      Stat to Statb
.         move      ap2save to ap2
.begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
.end patch 3.35
          pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
          call      NInvUpd
          CALL      UPDADJ
.                   CALL      UPDATAP
          ENDIF
CHK2
          MOVE      C0 TO APAMT
          MOVE      AP2 TO APAMT
          if        (APAmt >= ApCheck)
          Move      "Y" to AP2Sw
          else
          MOVE                "N" TO AP2SW
          endif

.begin patch 3.35
CHK3
          MOVE                C0 TO APAMT
          Move      Ap3,Apamt
          if        (APAmt >= ApCheck)
          Move      "Y" to AP3Sw
          else
          MOVE                "N" TO AP3SW
          endif
.end patch 3.35

NINCAL 
.begin patch 3.35
          if        (APsw = "Y" or AP2sw = "Y" or AP3sw = "Y")
.         CMATCH    "Y" TO APSW
.                   GOTO      NINCAL1 IF EQUAL
.                   CMATCH    "Y" TO AP2SW
.                   GOTO      NINCAL1 IF EQUAL
          goto      NINCAL1
          endif
.end patch 3.35
          GOTO      INPUT

NINCAL1   
.begin patch 3.35   ---- we don't write ourselves checks the money is already in the bank
          if        (CHKNflag = "N" & Lon = "0033")
          Goto      Update
          Elseif    (CHKNflag = "P" & Lon = "1490")
          Goto      Update
          endif
.end patch 3.35
.         MOVE      LON,LOWN
.                   COMPARE   LOWN TO NINCA
.                   GOTO      UPDATE IF EQUAL

.
          CALL      READPAY
          CALL      READMLR
          GOTO      MASKIT
.
.
CHKADJ    MOVE      CLR TO NADJFLD
          REP       ZFILL IN NADJFLD
          MOVE      NO TO ADJSW
          CALL      NADJKEY 
          IF        OVER
          RETURN
          ELSE
          MOVE      YES TO ADJSW
          ENDIF
          add        asrecadj to formar
          add       aslrinc to lrinc
                    add       aspayad1 to ap
                    add       aspayad2 to ap2       
                    add       aspayad3 to ap3       
                    add       ASXninc,Xninc
.will need to add code for ap3 when adjustment record modified
          RETURN
.
.
MASKIT
.AP1
.begin patch 3.35
.                   cmatch    yes to apsw
.                   goto      noap1 if not equal
.for each payable we must 
.See if we have
.for AP1 & AP2 see if escrow if so no check and no check number increment
.For AP3 is it NIN or PLI - cut check
.lets take care of 1st AP
.if an inter company order PL using NIN or VS, are we receiving paymen from mlr ? if so its not CExtcd = "I"
.and we can cut a check to the owner AP1 else move AP1 to AP3 and write check to NIN or PL
          if        (Cextcd = "I" or (Cextcd <> "I" & ap3 <=0 & Xninc <= 0))
                    if        (APSW = YEs)
                    MOVE                mask92b TO M$AP1
                    move                c0 to ap1amt
                    Move                ap to ap1amt
                    EDIT                AP1amt TO M$AP1
                    MOVE                M$AP1 TO AMT
.check for escrow
.begin patch 3.36
                    Unpack    InvVarsHold,invvars
                    Call      CHKESCR
                              IF        (ESCFlag = Yes)
                                        unpack    NCSHCHK,str6,str6
                                        Clear     CHKN1
                                        Move      Escrow to CHKN1
                                        Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
                                        Move      str6,IMLRCHK
                                        Move      Stat to Statb
                              Else
.not escrow lets print a check          
                                        Call      PRint
                                        unpack    NCSHCHK,str6,str6
                                        Clear     CHKN1
                                        Move      chknum to CHKN1
                                        rep       Zfill in chkn1
                                        Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
                                        Move      str6,IMLRCHK
                                        Move      Stat to Statb
                                        endif
.begin patch 3.35
.                   move                ap1save to ap1
.                   move                ap2save to ap2
.                   move                ap3save to ap3
.                   move                Xnincsave to XNinc
.end patch 3.35
                    pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
                    call      NInvUpd
                    CALL                UPDADJ
.begin patch 3.35
.increment checknumber
                    If        (EscFlag <> Yes)
                    Add       C1,chknum
                    endif
.end patch 3.35
                    Endif
          else                          .need to move $ to other company to record income and pay LO          
.begin patch 3.37      need to use adjusted amount
          add       Apamt,Ap1amt
.         add       Ap1amt,AP3
.begin patch 3.37
          move      Yes,Ap3sw                    .double verfify switch is on
          endif     
          
.                   MOVE      mask92b TO M$AP1
.         move      c0 to ap1amt
.                   Move      ap to ap1amt
.                   EDIT      AP1amt TO M$AP1
.                   MOVE      M$AP1 TO AMT
..............................................................................................................
.AP2
          IF        (Ap2SW = YES)
          MOVE                mask92b TO M$AP2
          EDIT                AP2 TO M$AP2
          Move      M$AP2,AMT

          MOVE                LON TO NOWNFLD
          REP                 ZFILL IN NOWNFLD
          CALL                NOWNKEY
.begin patch 3.36
          uNPACK    InvVarsHold,Invvars 
          Call      CHKESCR
                    IF        (ESCFlag = Yes)
                    unpack    NCSHCHK,str6,str6
                    Clear     CHKN2
                    Move      Escrow to CHKN2
                    Unpack    Chk1Date into CHK2DTEc,CHK2DTEY,CHK2DTEm,CHK2DTEd
                    Else
.not escrow lets print a check          
                    Call      PRint2
                    unpack    NCSHCHK,str6,str6
                    Clear     CHKN2
                    Move      chknum to CHKN2
                    rep       Zfill in chkn2
                    Unpack    Chk1Date into CHK2DTEc,CHK2DTEY,CHK2DTEm,CHK2DTEd
                    endif
.begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
.end patch 3.35
          pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
          call      NInvUpd
.begin patch 3.35
.increment checknumber
                    IF        (ESCFlag <> Yes)
                    Add       C1,chknum
                    endif
.end patch 3.35
          CMATCH              YES TO ADJSW
          CALL                UPDADJ IF EQUAL
          
          Endif
          
.                   MOVE                mask92b TO M$AP3
.                   ADD       XNinc,Ap3
.                   add       Ap1,ap3
.                   Edit      AP3, M$AP3
.................................................................................................
.AP3
          IF        (Ap3SW = YES)
          MOVE                mask92b TO M$AP3
          move      C0,Ap3Amt
          ADD       XNinc,AP3AMT
          add       Ap3,Ap3amt
          add       Ap1,Ap3amt
.start patch 3.39
          add     aspayad1 to ap3amt
.end patch 3.39
          Edit      AP3Amt,M$AP3
          MOVE                M$AP3 TO AMT
          

                    IF        (CHKNflag = "N")    .Names in the News Mailer check will be to PLI - List Manager
                    Move      "1490",Nownfld
                    Elseif    (CHKNflag = "P")        .Pacific Lists Mailer check will be to NIN - List Manager
                    MOVe      "0033",Nownfld
                    endif
          REP                 ZFILL IN NOWNFLD
          CALL                NOWNKEY

.print a check      
          unpack    InvVarsHOld,Invvars
                    if        (Cextcd <> "I")         .we are processing mlrs check not intra comp check
.begin patch 3.41
.                    Call      PRint3                 No more checks for intercomp transfer we are using same account
.end patch 3.41
                    unpack    NCSHCHK,str6,str6
                    Clear     CHKN3
.begin patch 3.41
.                    Move      chknum to CHKN3
                    Move      chkInum to CHKN3           .no REAL check  intercomp transfer
.end patch 3.41
                    rep       Zfill in chkn3
                    Unpack    Chk1Date into CHK3DTEc,CHK3DTEY,CHK3DTEm,CHK3DTEd
.begin patch 3.35
.                   move                ap1save to ap1
.                   move                ap2save to ap2
.                   move                ap3save to ap3
.                   move                Xnincsave to XNinc
.end patch 3.35
                    pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
                    call      NInvUpd
.begin patch 3.35
.increment checknumber
.begin patch 3.41
.                    Add       C1,chknum
                    Add       C1,chkInum
.end patch 3.41
.end patch 3.35
          
                    endif
          Endif
.DH/JD go bad
          Move      C0,ap1amt
          Move      C0,ap1save
          Move      C0,ap2save
          Move      C0,ap3save
          Move      C0,Xnincsave
.end DH/JD go bad
          Goto      INput

.PRINT1             CALL      PRINT                    
.         CALL      UPDINV
.                   ADD       C1 TO CHKNUM
.                   CMATCH    YES TO AP2SW
.                   if        not equal
.                   CMATCH    YES TO ADJSW
.                   CALL      UPDADJ IF EQUAL
.                   goto      input


.No AP1 So get payee info for AP2
.noap1
.         MOVE      LON TO NOWNFLD
.                   REP       ZFILL IN NOWNFLD
.                   CALL      NOWNKEY
.                   MOVE      mask92b TO M$AP2
.                   EDIT      AP2 TO M$AP2
.                   MOVE      M$AP2 TO M$AP1
.                   MOVE      M$AP1 TO AMT
.                   CALL      CHKESCR
.                   MATCH     YES TO ESCFLAG
.                   IF        EQUAL
.         Clear     CHKN2
.         Move      Escrow to CHKN2
.         rep       Zfill in chkn2
.         Move      Stat to Statb
.         move      ap2save to ap2
.         Unpack    Chk1Date into CHK2DTEc,CHK2DTEY,CHK2DTEm,CHK2DTEd
.         call      Ninvupd
.                   GOTO      CHK2INS
.                   Else
.                   CALL                PRINT
..insert info for 2nd check
.         Move      CHKNum to CHKN2
.         rep       Zfill in chkn2
.         Unpack    Chk1Date into CHK2DTEc,CHK2DTEY,CHK2DTEm,CHK2DTEd
.         Move      Stat to Statb
.         move      ap2save to ap2
.         call      Ninvupd
.         Endif
.begin patch 3.35
.CHK2INS 
.                   CMATCH    YES TO ADJSW
.                   CALL      UPDADJ IF EQUAL
.                   MATCH     YES TO ESCFLAG
.                   IF        not EQUAL
.                   ADD       C1 TO CHKNUM
.                   endif
.                   GOTO      INPUT
.end patch 3.35
*......................................................................
.
CODE
          MOVE      C0 TO NUM
          MOVE      OELCODE,NUM
          BRANCH    NUM OF RENT,EXCH,EXCH

RENT      MOVE      "RENTAL" TO TIPE
          RETURN

EXCH      MOVE      C0 TO XQTY
          MOVE      OEXQTY TO XQTY
          COMPARE   XQTY TO C0
          IF        EQUAL
          MOVE      "EXCHANGE" TO TIPE
          RETURN
          ELSE
          MOVE      "RENT-EX" TO TIPE
          RETURN
          ENDIF
.
READMLR   PACK      MKEY FROM MLRN,COBN
          MOVE      NO TO OVER
          CALL      NMLRKEY
          CMATCH    YES TO OVER
          CALL      NOMLR IF EQUAL
          CALL      NOMLR IF OVER
          MOVE      compnum TO nmtxfld
          CALL      NMTXKEY
          CLEAR     STAT501
          MOVE      C0 TO TAX501
          MOVE      MTXC501,TAX501
          BRANCH    TAX501 OF C0,C0,C3,C4,C5,C6
C0
          RETURN
C3        MOVE      "501C-3" TO STAT501
          RETURN
C4        MOVE      "501C-4" TO STAT501
          RETURN
C5        MOVE      "501C-5" TO STAT501
          RETURN
C6        MOVE      "501C-6" TO STAT501
          RETURN
................................................................................
READPAY   move      no to PayToSW
          MOVE      OLON TO NOWNFLD
          REP       ZFILL IN NOWNFLD
          MOVE      PAYTN TO DIM1
          PACK      NPAYFLD FROM OLON,DIM1
          REP       ZFILL IN NPAYFLD
          CALL      NPAYKEY
          GOTO      NOWNKEY IF OVER
          move      pname to ownlonm
          MOVE      PCOMP TO OWNOCPY
          MOVE      PSTREET TO OWNLOSA
          MOVE      PCITY TO OWNLOCTY
          MOVE      PSTATE TO OWNLOS
          MOVE      PZIP TO OWNLOZC
          move      yes to PayToSW
          RETURN
.
AP2YES
          MOVE      YES TO AP2SW
          GOTO      NINCAL
.
.
*......................................................................
.
PRINT
          MOVE      C0 TO PRTLINES
          MATCH     B12 TO LOINVN
          if        not equal
          MOVE      REF TO REFERENC
          else
          MOVE      SPACES TO REFERENC
          endif
DETAIL
          PRINT     *F
          print      "!R!CALLSA,4.50,9.30; EXIT;"
          PRINT     *N,DATEMASK,hpt125,"LR",LRN," - ",MCOMP:
                    hpt625,M$AP1
          PRINT     hpt225,stat501,*l,*1,hpbon,"Income or list cost reimbursement for Your list :",hpboff
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
          move      O2DES,NSEL2NAME
          endif

          call      Trim using NSEL2NAME
          PRINT     hpt125,"LIST- ",hpt200,O1DES,hpt575,TIPE:
                    *L,hpt200,NSEL2NAME
                    if         (PayToSW = "Y")
                    PRINT     *L,hpt125,pname;
          PRINT     *L,hpt125,pcomp;
          PRINT     *L,hpt125,pstreet;
          PRINT     *L,hpt125,pcity,hpt300,pstate,B4,pzip
          PRINT     *L,hpt125,REFERENC,LOINVN;
          
          else
          
          PRINT     *L,hpt125,ownlonm;
          PRINT     *L,hpt125,ownocpy;
          PRINT     *L,hpt125,ownlosa;
          PRINT     *L,hpt125,ownlocty,hpt300,ownlos,B4,ownlozc
          PRINT     *L,hpt125,REFERENC,LOINVN;
          endif


          add       "13" to prtlines
          match     yes to mrgy
          goto      datepr if not equal
          move      mrgnetsv to nmrgnet

          compare    c0 to nmrgnnet
          if         not equal
          move       nmrgnnet to nmrgnet
          endif
          
          MOVE      NMRGNET TO CALCmPER
          MOVE      NMRGIQTY TO N7
          DIVIDE    N7 INTO CALCmPER
          MULT      "100" BY CALCmPER
          MOVE      C0 TO PERCENT
          ADD       CALCmPER TO PERCENT
          ADD        NMRGID TO TOTREJ 
          ADD        NMRGERR TO TOTREJ 
          ADD        NMRGDISF TO TOTREJ 
          ADD        NMRGNPER TO TOTREJ 
          ADD        NMRGDMA TO TOTREJ 
          ADD        NMRGZ4 TO TOTREJ 
          ADD        NMRGPRIS TO TOTREJ 
          ADD        NCOAMNF TO TOTREJ 
          ADD        Nmrgdisa TO TOTREJ 
.begin patch 3.42
          add        nmrgcnr to totrej
.end patch 3.42

          MOVE      invdtem TO MM
          MOVE      invdted TO DD
          MOVE      invdtey TO YY
          CALL      CVTJUL
          move      no to feb
          compare   juldays to febdat
          goto      nocust if equal
          if        not less
          move      yes to feb
          ADD        NMRGCUST TO TOTREJ      .*turned off if billed after 2/08/96
          endif

nocust    add        nmrgconv to totrej
          MOVE       NMRGRQTY TO TOTBILL
          SUB        TOTREJ FROM TOTBILL
          add        ncoanix1 to totncoa
          add        ncoanix2 to totncoa
          add        ncoanix3 to totncoa
          move       c0 to qtybild2
          move       qtybild to qtybild2
          move       c0 to invqty1
          move       irexqty to invqty1
          compare    c0 to invqty1
          if         not equal
          move       c0 to invqty2
          move       qtyBILD to invqty2
          add        invqty1 to invqty2
          move       c0 to qtybild2
          move       invqty2 to qtyBILD2
          endif
          
          PRINT               *L,*31,"COMPUTER VERIFICATION"
          PRINT          *L,*RTNTAB,RTCOMP," MERGE"
          PRINT               *L,HPt125,"INPUT QTY:",NMRGRQTY," NET OUTPUT:",NMRGNET:
                              " M/P##",OMLRKY:
                              *L,HPT125,"TOTAL BILLABLE NAMES:",TOTBILL," ",percent,"% NAMES MAILED";
          move      c0 to n2
          move      onetper to n2
          compare   c0 to n2                    .net name order?
          if         equal
          PRINT      *l,hpt125,"ORDER QTY: ",OQTY,"  ","BILLED QTY: ",qtyBILD2
          else
          cmatch    "F" to onetfm
                    if        equal
                    PRINT     *l,hpt125,"ORDER QTY: ",OQTY,"  ","BILLED QTY: ",qtyBILD2:
                                        "  ",onetper,"% FLAT"
                    else
                    PRINT     *l,hpt125,"ORDER QTY: ",OQTY,"  ","BILLED QTY: ",qtyBILD2:
                                        "  ",onetper,"% NET"
                    endif
          endif
          PRINT      *L,HPT125,"INTRA DUPES: ",hpt400,NMRGID;
          PRINT      *L,HPT125,"ERROR REJECTS: ",hpt400,NMRGERR;
          PRINT      *L,HPT125,"DECEASED REJECTS: ",hpt400,NMRGDISF;
          PRINT      *L,HPT125,"NONPERSONAL REJECTS: ",hpt400,NMRGNPER;
          PRINT      *L,HPT125,"DMA REJECTS: ",hpt400,NMRGDMA;
          PRINT      *L,HPT125,"ZIP+4 CRT REJECTS: ",hpt400,NMRGZ4;
          PRINT      *L,HPT125,"PRISON REJECTS: ",hpt400,NMRGPRIS;
          PRINT      *L,HPT125,"NCOA MTCH NON FWD: ",hpt400,NCOAMNF;
          add        "17" to prtlines
          cmatch     yes to feb
          if         equal
          PRINT      *L,HPT125,"CUSTOMER REJECTS: ",hpt400,NMRGCUST;
          add        c1 to prtlines
          ENDIF    
          PRINT      *L,HPT125,"CONVERSION REJECTS: ",hpt400,NMRGconv;
          add        c1 to prtlines
          PRINT      *L,HPT125,"DISASTER DROPS: ",hpt400,NMRGDisa;
          add        c1 to prtlines
.begin  Patch 3.42 - new var.
         COMPARE    C0 TO NMRGCNR
         IF         NOT EQUAL
          PRINT      *L,HPT125,"CNR Matches: ",hpt400,NMRGCNR:
                     hpt400,"____________";
          Else
          PRINT      *L,hpt400,"____________";
         ENDIF    

.          PRINT      *L,hpt400,"____________";
.end  Patch 3.42 - new var.
          PRINT      *L,HPT125,"TOTAL REJECTS: ",hpt400,TOTREJ;
          add        c2 to prtlines
          COMPARE    C0 TO NMRGrep
          IF         NOT EQUAL
          move       yes to rep
          endif

          COMPARE    C0 TO NMRGelmx
          if         not equal
          goto       printrep
          else
          cmatch     yes to rep
                    if         equal         
printrep
                    PRINT               *N,*N,HPT125,"NOTE: INPUT QTY MINUS TOTAL REJECTS,":
                                        "ELIMINATOR,TDMC,";
                    PRINT      *l,HPT125,"NCOA TOTAL, MAILDROP, UNUSED DUPES, NIXIE, FAMILY";
                    PRINT      *l,HPT125,"HOUSE HITS, INDEPENDENT REJECTS, CNR REJECTS,";
                    add        c4 to prtlines
                    cmatch     yes to feb
                              if         not equal
                              PRINT      *l,HPT125,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT"
                              add        c1 to prtlines
                              else
                              PRINT      *l,HPT125,"CUSTOMER SUPPRESS = NET OUTPUT"
                              add        c1 to prtlines
                              endif
                    goto       cvexit
                    endif
          endif
PRINTREG
                    PRINT      *N,*N,HPT125,"NOTE: INPUT QTY MINUS TOTAL REJECTS,":
                              "ELIMINATOR,TDMC,";
          PRINT      *l,HPT125,"NCOA TOTAL, MAILDROP, UNUSED DUPES, NIXIE, FAMILY, ";
          add        c3 to prtlines
          cmatch     yes to feb
          if         not equal
          PRINT      *l,HPT125,"HOUSE HITS, CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT"
          add        c1 to prtlines
          else
          PRINT      *l,HPT125,"HOUSE HITS, CUSTOMER SUPPRESS = NET OUTPUT"
          add        c1 to prtlines
          endif
cvexit
.
datepr    cmatch    b1 to stat501
          goto      addlines if eos
          match     yes to mrgy
          if        equal
          print     *l,*1,"Mailers's tax status is provided as a service though";
          print     *l,*1,"its accuracy cannot be guaranteed."
          add        c2 to prtlines
          else
          print     *n,*n,*n,*n,*n,*n,*n,*n,*n,*n:
                    *n,*n,*n,*n,*n,*n,*n,*n,*n,*n:
                    *n,*n,*n
          print     *l,*1,"Mailers's tax status is provided as a service though";
          print     *l,*1,"its accuracy cannot be guaranteed."
          add       num26 to prtlines
          endif
addlines
          call      prtlnes
          PRINT     DATEMASK,hpt625,AMT
          PRINT     *L,*L
          if         (PayToSW = "Y")
          PRINT     hpt125,pcomp:
                    *L,hpt125,pstreet:
                    *L,hpt125,pCiTY,hpt300,pstate,B4,pzip
          else
          PRINT     hpt125,OWNOCPY:
                    *L,hpt125,OWNLOSA:
                    *L,hpt125,OWNLOCTY,hpt300,OWNLOS,B4,OWNLOZC
          endif

          PRINT     *F
          MOVE        C0 TO TOTREJ
          MOVE        C0 TO TOTNCOA
          move       no to feb
..begin patch 3.35
..increment checknumber
.         Add       C1,chknum
..end patch 3.35
          RETURN
*......................................................................
.AP2
PRINT2
          MOVE      C0 TO PRTLINES
          MATCH     B12 TO LOINVN
          if        not equal
          MOVE      REF TO REFERENC
          else
          MOVE      SPACES TO REFERENC
          endif
.DETAIL
.signature line
          PRINT     *F
          print      "!R!CALLSA,4.50,9.30; EXIT;"
          PRINT     *N,DATEMASK,hpt125,"LR",LRN," - ",MCOMP:
                    hpt625,M$AP2
          PRINT     hpt225,stat501,*l,*1,hpbon,"Income or list cost reimbursement for Your list :",hpboff
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
          move      O2DES,NSEL2NAME
          endif

          call      Trim using NSEL2NAME
          PRINT     hpt125,"LIST- ",hpt200,O1DES,hpt575,TIPE:
                    *L,hpt200,NSEL2NAME
                    if         (PayToSW = "Y")
                    PRINT     *L,hpt125,pname;
          PRINT     *L,hpt125,pcomp;
          PRINT     *L,hpt125,pstreet;
          PRINT     *L,hpt125,pcity,hpt300,pstate,B4,pzip
          PRINT     *L,hpt125,REFERENC,LOINVN;
          
          else
          
          PRINT     *L,hpt125,ownlonm;
          PRINT     *L,hpt125,ownocpy;
          PRINT     *L,hpt125,ownlosa;
          PRINT     *L,hpt125,ownlocty,hpt300,ownlos,B4,ownlozc
          PRINT     *L,hpt125,REFERENC,LOINVN;
          endif


          add       "13" to prtlines
          match     yes to mrgy
          goto      datepr2 if not equal
          move      mrgnetsv to nmrgnet

          compare    c0 to nmrgnnet
          if         not equal
          move       nmrgnnet to nmrgnet
          endif
          
          MOVE      NMRGNET TO CALCmPER
          MOVE      NMRGIQTY TO N7
          DIVIDE    N7 INTO CALCmPER
          MULT      "100" BY CALCmPER
          MOVE      C0 TO PERCENT
          ADD       CALCmPER TO PERCENT
          ADD        NMRGID TO TOTREJ 
          ADD        NMRGERR TO TOTREJ 
          ADD        NMRGDISF TO TOTREJ 
          ADD        NMRGNPER TO TOTREJ 
          ADD        NMRGDMA TO TOTREJ 
          ADD        NMRGZ4 TO TOTREJ 
          ADD        NMRGPRIS TO TOTREJ 
          ADD        NCOAMNF TO TOTREJ 
          ADD        Nmrgdisa TO TOTREJ 

          MOVE      invdtem TO MM
          MOVE      invdted TO DD
          MOVE      invdtey TO YY
          CALL      CVTJUL
          move      no to feb
          compare   juldays to febdat
          goto      nocust2 if equal
          if        not less
          move      yes to feb
          ADD        NMRGCUST TO TOTREJ      .*turned off if billed after 2/08/96
          endif

nocust2             add        nmrgconv to totrej
          MOVE       NMRGRQTY TO TOTBILL
          SUB        TOTREJ FROM TOTBILL
          add        ncoanix1 to totncoa
          add        ncoanix2 to totncoa
          add        ncoanix3 to totncoa
          move       c0 to qtybild2
          move       qtybild to qtybild2
          move       c0 to invqty1
          move       irexqty to invqty1
          compare    c0 to invqty1
          if         not equal
          move       c0 to invqty2
          move       qtyBILD to invqty2
          add        invqty1 to invqty2
          move       c0 to qtybild2
          move       invqty2 to qtyBILD2
          endif
          
          PRINT               *L,*31,"COMPUTER VERIFICATION"
          PRINT          *L,*RTNTAB,RTCOMP," MERGE"
          PRINT               *L,HPt125,"INPUT QTY:",NMRGRQTY," NET OUTPUT:",NMRGNET:
                              " M/P##",OMLRKY:
                              *L,HPT125,"TOTAL BILLABLE NAMES:",TOTBILL," ",percent,"% NAMES MAILED";
          move      c0 to n2
          move      onetper to n2
          compare   c0 to n2                    .net name order?
          if         equal
          PRINT      *l,hpt125,"ORDER QTY: ",OQTY,"  ","BILLED QTY: ",qtyBILD2
          else
          cmatch    "F" to onetfm
                    if        equal
                    PRINT     *l,hpt125,"ORDER QTY: ",OQTY,"  ","BILLED QTY: ",qtyBILD2:
                                        "  ",onetper,"% FLAT"
                    else
                    PRINT     *l,hpt125,"ORDER QTY: ",OQTY,"  ","BILLED QTY: ",qtyBILD2:
                                        "  ",onetper,"% NET"
                    endif
          endif
          PRINT      *L,HPT125,"INTRA DUPES: ",hpt400,NMRGID;
          PRINT      *L,HPT125,"ERROR REJECTS: ",hpt400,NMRGERR;
          PRINT      *L,HPT125,"DECEASED REJECTS: ",hpt400,NMRGDISF;
          PRINT      *L,HPT125,"NONPERSONAL REJECTS: ",hpt400,NMRGNPER;
          PRINT      *L,HPT125,"DMA REJECTS: ",hpt400,NMRGDMA;
          PRINT      *L,HPT125,"ZIP+4 CRT REJECTS: ",hpt400,NMRGZ4;
          PRINT      *L,HPT125,"PRISON REJECTS: ",hpt400,NMRGPRIS;
          PRINT      *L,HPT125,"NCOA MTCH NON FWD: ",hpt400,NCOAMNF;
          add        "17" to prtlines
          cmatch     yes to feb
          if         equal
          PRINT      *L,HPT125,"CUSTOMER REJECTS: ",hpt400,NMRGCUST;
          add        c1 to prtlines
          ENDIF    
          PRINT      *L,HPT125,"CONVERSION REJECTS: ",hpt400,NMRGconv;
          add        c1 to prtlines
          PRINT      *L,HPT125,"DISASTER DROPS: ",hpt400,NMRGDisa;
          add        c1 to prtlines

          PRINT      *L,hpt400,"____________";
          PRINT      *L,HPT125,"TOTAL REJECTS: ",hpt400,TOTREJ;
          add        c2 to prtlines
          COMPARE    C0 TO NMRGrep
          IF         NOT EQUAL
          move       yes to rep
          endif

          COMPARE    C0 TO NMRGelmx
          if         not equal
          goto       printrep2
          else
          cmatch     yes to rep
                    if         equal         
printrep2
                    PRINT               *N,*N,HPT125,"NOTE: INPUT QTY MINUS TOTAL REJECTS,":
                                        "ELIMINATOR,TDMC,";
                    PRINT      *l,HPT125,"NCOA TOTAL, MAILDROP, UNUSED DUPES, NIXIE, FAMILY";
                    PRINT      *l,HPT125,"HOUSE HITS, INDEPENDENT REJECTS, CNR REJECTS,";
                    add        c4 to prtlines
                    cmatch     yes to feb
                              if         not equal
                              PRINT      *l,HPT125,"CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT"
                              add        c1 to prtlines
                              else
                              PRINT      *l,HPT125,"CUSTOMER SUPPRESS = NET OUTPUT"
                              add        c1 to prtlines
                              endif
                    goto       cvexit2
                    endif
          endif
.PRINTREG
                    PRINT      *N,*N,HPT125,"NOTE: INPUT QTY MINUS TOTAL REJECTS,":
                              "ELIMINATOR,TDMC,";
          PRINT      *l,HPT125,"NCOA TOTAL, MAILDROP, UNUSED DUPES, NIXIE, FAMILY, ";
          add        c3 to prtlines
          cmatch     yes to feb
          if         not equal
          PRINT      *l,HPT125,"HOUSE HITS, CUSTOMER SUPPRESS, CUSTOMER REJECTS = NET OUTPUT"
          add        c1 to prtlines
          else
          PRINT      *l,HPT125,"HOUSE HITS, CUSTOMER SUPPRESS = NET OUTPUT"
          add        c1 to prtlines
          endif
cvexit2
.
datepr2
          cmatch    b1 to stat501
          goto      addlines2 if eos
          match     yes to mrgy
          if        equal
          print     *l,*1,"Mailers's tax status is provided as a service though";
          print     *l,*1,"its accuracy cannot be guaranteed."
          add        c2 to prtlines
          else
          print     *n,*n,*n,*n,*n,*n,*n,*n,*n,*n:
                    *n,*n,*n,*n,*n,*n,*n,*n,*n,*n:
                    *n,*n,*n
          print     *l,*1,"Mailers's tax status is provided as a service though";
          print     *l,*1,"its accuracy cannot be guaranteed."
          add       num26 to prtlines
          endif
addlines2
          call      prtlnes
          PRINT     DATEMASK,hpt625,Amt
          PRINT     *L,*L
          if         (PayToSW = "Y")
          PRINT     hpt125,pcomp:
                    *L,hpt125,pstreet:
                    *L,hpt125,pCiTY,hpt300,pstate,B4,pzip
          else
          PRINT     hpt125,OWNOCPY:
                    *L,hpt125,OWNLOSA:
                    *L,hpt125,OWNLOCTY,hpt300,OWNLOS,B4,OWNLOZC
          endif

          PRINT     *F
          MOVE        C0 TO TOTREJ
          MOVE        C0 TO TOTNCOA
          move       no to feb
..begin patch 3.35
..increment checknumber
.         Add       C1,chknum
..end patch 3.35
          RETURN
*......................................................................
.*LR
PRINT3
          MOVE      C0 TO PRTLINES
          MATCH     B12 TO LOINVN
          if        not equal
          MOVE      REF TO REFERENC
          else
          MOVE      SPACES TO REFERENC
          endif
.DETAIL
.signature
          PRINT     *F
          print      "!R!CALLSA,4.50,9.30; EXIT;"
          PRINT     *N,DATEMASK,hpt125,"LR",LRN," - ",MCOMP:
                    hpt625,M$AP3
          PRINT     hpt225,stat501,*l,*1,hpbon,"Inter Company list cost transfer :",hpboff:
                      hpt625,"##",chknum
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
          move      O2DES,NSEL2NAME
          endif

          call      Trim using NSEL2NAME
          PRINT     hpt125,"LIST- ",hpt200,O1DES,hpt575,TIPE:
                    *L,hpt200,NSEL2NAME
.                   if         (PayToSW = "Y")
.                   PRINT     *L,hpt125,pname;
.                   PRINT     *L,hpt125,pcomp;
.                   PRINT     *L,hpt125,pstreet;
.                   PRINT     *L,hpt125,pcity,hpt300,pstate,B4,pzip
.                   PRINT     *L,hpt125,REFERENC,LOINVN;
          
.         else
          
          PRINT     *L,hpt125,ownlonm;
          PRINT     *L,hpt125,ownocpy;
          PRINT     *L,hpt125,ownlosa;
          PRINT     *L,hpt125,ownlocty,hpt300,ownlos,B4,ownlozc
          PRINT     *L,hpt125,REFERENC,LOINVN;
.         endif


          add       "13" to prtlines
.
          cmatch    b1 to stat501
.begin patch 12/13/07 DLH
          goto      addlines3 if eos
.                   goto      addlines2 if eos
.end patch 12/13/07 DLH
          match     yes to mrgy
          if        equal
          print     *l,*1,"Mailers's tax status is provided as a service though";
          print     *l,*1,"its accuracy cannot be guaranteed."
          add        c2 to prtlines
          else
          print     *n,*n,*n,*n,*n,*n,*n,*n,*n,*n:
                    *n,*n,*n,*n,*n,*n,*n,*n,*n,*n:
                    *n,*n,*n
          print     *l,*1,"Mailers's tax status is provided as a service though";
          print     *l,*1,"its accuracy cannot be guaranteed."
          add       num26 to prtlines
          endif
addlines3
          call      prtlnes
          PRINT     DATEMASK,hpt625,AMT
          PRINT     *L,*L
.         if         (PayToSW = "Y")
.                   PRINT     hpt125,pcomp:
.                             *L,hpt125,pstreet:
.                             *L,hpt125,pCiTY,hpt300,pstate,B4,pzip
.         else
          PRINT     hpt125,OWNOCPY:
                    *L,hpt125,OWNLOSA:
                    *L,hpt125,OWNLOCTY,hpt300,OWNLOS,B4,OWNLOZC
.         endif

          PRINT     *F
          MOVE        C0 TO TOTREJ
          MOVE        C0 TO TOTNCOA
          move       no to feb
..begin patch 3.35
..increment checknumber
.         Add       C1,chknum
..end patch 3.35
          RETURN
.end patch 3.35
*..............................................................................
prtlnes
compline  compare   "49" to prtlines
                    if       not equal
          PRINT     b1
          add       c1 to prtlines
          goto      compline
          endif
          return
......................................................................          
NOOWN    
          MOVE      "*************************",OWNOCPY
          MOVE      "***************",OWNLOSA
          MOVE      "**********",OWNLOCTY
          MOVE      "**",OWNLOS
          MOVE      "**********",OWNLOZC
          RETURN
.
NOMLR   
          MOVE      "*************************",MCOMP
          RETURN
.
.CVT      ENDSET    CVTFLD                        CHECK LAST BYTE.
.         RESET     MPCHARS
.         SCAN      CVTFLD IN MPCHARS             IS IT A MINUSOVRPNCH?
.         GOTO      CVTMP IF EQUAL                YES.
.         RESET     CVTFLD                        NO.
.         TYPE      CVTFLD                        CHECK NUMERIC VALIDITY.
.         RETURN    IF EQUAL                      ITS OK.
.FORMERR  DISPLAY   *P1:24,*EL,*B,"FORMAT ERROR READING LR: ",LRN;
.         RETURN                                POP THE STACK.
.CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
.         RESET     CVTFLD
.         TYPE      CVTFLD                        VALID NUMERIC?
.         GOTO      FORMERR IF NOT EQUAL          NO.
.         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
.         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
.         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
.         RETURN
.
.UPDate - list owner is to ourselves do not cut check.
.
.
UPDATE
.begin patch 3.36
          Unpack    InvVarsHold,InvVars 
          unpack    NCSHCHK,str6,str6
          Clear     CHKN1
          Move      Nochk to CHKN1
          Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
          Move      str6,IMLRCHK
          Move      Stat to Statb
.         move                ap2save to ap2
.begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
.end patch 3.35
          pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
          call      NInvUpd
          CALL                UPDADJ
          GOTO                INPUT
UPDXXXX
.begin patch 3.36
          Unpack    InvVarsHold,InvVars 
          unpack    NCSHCHK,str6,str6
          Clear     CHKN1
          Move      Escrow to CHKN1
          Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
          Move      str6,IMLRCHK
          Move      Stat to Statb
.         move                ap2save to ap2
.begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
.end patch 3.35
          pack      InvVarsHold,InvVars                    .resave vars - might be multiple checks
.end patch 3.36
          call      NInvUpd
          CALL                UPDADJ
.begin patch 3.35
.                   GOTO                INPUT
          Return
.end patch 3.35
.
.updatap - no payables update invoice reflecting no check written.
.UPDATAP
.         unpack    NCSHCHK,str6,str6
.         Clear     CHKN1
.         Move      NoChk to CHKN1
.         Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
.         Move      str6,IMLRCHK
.         Move      Stat to Statb
.         move      ap2save to ap2
..begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
..end patch 3.35
.         call      NInvUpd
.                   CALL      UPDADJ
.                   RETURN
.updpre -  external code 'P' Owner has been previously paid, CLose invoice
.          but do not update existing check info. 
.UPDPRE
.         unpack    NCSHCHK,str6,str6
.         Move      Stat to Statb
.         Move      str6,IMLRCHK
.         move      ap2save to ap2
..begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
..end patch 3.35
.         call      NInvUpd
.                   CALL      UPDADJ
.                   GOTO      INPUT
         
.
.updinv - check cut update invoice with data.
.UPDINV
.         unpack    NCSHCHK,str6,str6
.         Clear     CHKN1
.         Move      chknum to CHKN1
.         rep       Zfill in chkn1
.         Unpack    Chk1Date into CHK1DTEc,CHK1DTEY,CHK1DTEm,CHK1DTEd
.         Move      str6,IMLRCHK
.         Move      Stat to Statb
.         move      ap2save to ap2
..begin patch 3.35
.         move                ap1save to ap1
.         move                ap2save to ap2
.         move                ap3save to ap3
.         move                Xnincsave to XNinc
..end patch 3.35
.         call      NInvUpd
.                   RETURN
.
.upadj - mark adjustment closed/paid
UPDADJ    
          MOVE      CLR TO NADJFLD
          REP       ZFILL IN NADJFLD
          CALL      NADJKEY
          RETURN    IF OVER
          FILEPI    1;NADJFILE
          UPDATAB   NADJFILE;*2,"P"
          RETURN
CHKESCR
          move      c2 to nescpath
.begin patch 3.38
.                   pack      nescfld2 FROM LON,"  ",olnum
          pack      nescfld2 FROM "00",LON,olnum
.end patch 3.38
          CALL      NESCKEY
          IF        NOT OVER
.          MOVE      YES TO ESCFLAG
         move         c0,n9
         call         trim using NescSdate  
         move      NEscSDate,n9
                      if         (n8 < n9 | N9 = c0)
                      move      yes to escflag
                      else
                      CLEAR     MESSAGE
                      endif

          ELSE
          MOVE      NO TO ESCFLAG
          ENDIF
          RETURN

CNTRTN    SETLPTR   RTCOMP
          ENDSET    RTCOMP
CHKRHEAD  CMATCH    B1 TO RTCOMP
          GOTO      SETRHEAD IF NOT EQUAL
          BUMP      RTCOMP BY -1
          GOTO      CHKRHEAD IF NOT EOS
SETRHEAD  MOVEFPTR  RTCOMP TO N3
          MOVE      "80" TO RTNTAB
          SUBTRACT  N3 FROM RTNTAB
          DIVIDE    C2 INTO RTNTAB
          RESET     RTCOMP
          SETLPTR   RTCOMP
          RETURN
.
.
.
.DOne - update running totals and exit.
DONE
.BEGIN PATCH 3.35
.        OPEN      CHKMST,"PAR27N",EXCLUSIVE
.         FILEPI     1;CHKMST
.         write      CHKMST,SEQ;CHKCODE,CHKNUM,FILL71
.         CLOSE     CHKMST
.begin patch 3.40
          OPEN      CHKMST,"PAR27N",EXCLUSIVE
          FILEPI     1;CHKMST
.begin patch 3.41

.          write      CHKMST,SEQ;CHKCODE,CHKNUM,FILL71
          WRite      CHKMST,SEQ;CHKCODE,CHKNum,CHKIcode,chkInum,FILL64
.end patch 3.41

          CLOSE     CHKMST
.         If        (CHKNflag = "N")
.         OPEN      CHKMST,"PAR27N",EXCLUSIVE
.                   FILEPI     1;CHKMST
.                   write      CHKMST,SEQ;CHKCODE,CHKNUM,FILL71
.                   CLOSE     CHKMST
.         Elseif    (CHKNflag = "P")
.         OPEN      CHKMST,"PAR27P",EXCLUSIVE
.                   FILEPI     1;CHKMST
.                   write      CHKMST,SEQ;CHKCODE,CHKNUM,FILL71
.                   CLOSE     CHKMST
.         endif
.END PATCH 3.35

EOJ      
          SPLCLOSE
          shutdown  "cls"
          STOP
.
          INCLUDE   NMTXIO.inc
          INCLUDE   NORDIO.inc
          INCLUDE   NADJIO.inc
          include   njstio.inc
          INCLUDE   NPAYIO.inc
          INCLUDE   NOWNIO.inc
          INCLUDE   COMPIO.inc
          INCLUDE   CNTIO.inc
          INCLUDE   NCSHIO.inc
          INCLUDE             ninvio.inc
          Include   NInvAcdIO.inc
          INCLUDE             compute.inc
          INCLUDE   NDAT3IO.INC
          INCLUDE   NESCIO.INC
          include   nmrgio.inc
          INCLUDE   NRTNIO.inc
          INCLUDE   NACDIO.INC
          INCLUDE   NDATIO.INC
          include   nshpio.inc
          INCLUDE   NSEL2IO.INC
          INCLUDE   COMLOGIC.inc

