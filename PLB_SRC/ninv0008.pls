PC       EQU       0

         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NRCHGDD.INC
.patch4.5
        include   compdd.inc
        include   cntdd.inc
.         INCLUDE   NMLRDD.INC
.patch4.5
         INCLUDE   NORDDD.INC
         INCLUDE   NOWNDD.INC
         INCLUDE   NXRFDD.INC
         INCLUDE   NSHPDD.INC
         INCLUDE   NDATDD.INC
         INCLUDE   NMTXDD.INC
         INCLUDE   NDAT3DD.INC
         inc       npasdd.inc
         include   TINVDD.inc
         inc       hp.inc

.==========================================================
Release    init      "4.75"          DLH Automate external report for EOM
Reldate    Init      "3 March 2010"
.Release    init      "4.74"          DLH Internal INdex
.Reldate    Init               "23 April 2008"
.Release    init      "4.73"          DLH Adding Prompt for list 1103 Cit soldier
.Reldate    Init               "02/13/07"
.release  init      "4.72"        DMB      27SEP2006        Fixed Bug that was incorrectly displaying previously viewed triplex billing info
.Reldate    Init               "11/23/05"
.release  init      "4.71"        JD       23NOV2005        PLB 9.0 new index
.release  init      "4.7"        ASH    30DEC2004 Replaced all OLD nrchgdd vars with new vars
.release  init      "4.6"        DMB    05OCT2004 Cleaned up prompt code
.release  init      "4.5"        DMB    26MAY2004 Mailer Conversion   
.Release    init      "4.4"          DMB 02/03/04 Adding Prompt for list 18922 elephant sanctuary.
.Release    init      "4.3"          DB 03/11/03 Adding Prompt for list 2312 african wildlife foundation
.Release    init      "4.2"          DB 03/26/02 If spawned from another program and lr used is not the same as lr input from other prog then grab shipping info
.Release    init      "4.1d"        DB 08/27/01 Set Save as default after hitting calculate
.Release    init      "4.1c"        DB 08/27/01 Allow for Triplex Billing to Show on recall of running charge records
.Release    init      "4.1b"        DB 08/27/01 Expanded Var Rcselect to fit new formatting of Datalist
.Release    init      "4.1a"        DB 08/21/01 Changed name of file in plb_src
.Release    init      "4.1"        DB 08/14/01 Change chain subroutine to winshow then chain to prog
.RELEASE   INIT      "4.0"        DB 08/14/01 Initial Release of GUI Running Charges Prog.
.=====================================================================================
.RELEASE   INIT      "3.9"        02OCT2000 ASH NEW SERVER ADDED
.RELEASE   INIT      "3.8"        07SEP99 ASH EXCHARGE File Expansion
.release   init      "3.7"        02JAN99 JD  Fixed mcomp print/mtax 501c status print.
.RELEASE   INIT      "3.6"       13JAN99 ASH NINORD Y2K, File expansion
.release   init      "3.5"       30Oct98 ASH TDMCINV Y2K, File expansion
.Release   init      "3.41"      29Jun98 DLH display tdmc invoice date(s)
.release   init      "3.4"       28Jan98 DLH print @/m$ on in house
.release  init      "3.3"       10apr97 DLH add option to print a select owner
.release  init      "3.2"       05Nov96 DLH ndat3 code "e"
.release  init      "3.1"       16JUL96 DLH ADDED OPTION TO revise DUPE LR.
.release  init      "3.0"       15JUL96 DLH ADDED OPTION TO ADD DUPE LR.
.release  init      "2.8"      30Apr96 DLh add rent/exch breakout on reports
.release  init      "2.7"      15apr96 DLH replace rollout with execute, convert
.                             to plb
.RELEASE  init      "2.6"      07sep95 added  nprint of runprt.lst at eof.
.RELEASE  init      "2.5"     17aug95 added option to add if not exchange.
.RELEASE  init      "2.4"    11apr95 DLH display tdmc billing info.
.release  init      "2.3"    ;24feb95 DLH add auto entry from ninv0007 option.
.RELEASE  INIT      "2.2"    06dec95 jd automated eom spooling of report.
.RELEASE  INIT      "2.1"   19oct94 DLH.  ADDED TRIPLEX BILLING INFO, NDAT3XX.
.RELEASE  INIT         "2.0"          JD  15JUL94 print to laser
.RELEASE  INIT         "1.9"          DLH 19MAY93 DISPLAY DATE OF ENTRY.
.RELEASE   INIT        "1.8"          DLH 25SEP92 READ SHIPPED QTY.
.RELEASE   INIT        "1.7"          DLH 7JUL92 ADD NXRFxx.INC.
.
.RELEASE  INIT      "1.6"          DLH 12MAR92 CONVERT TO PCBUS.incLUDES
.RELEASE  INIT      "FIX"          02MAR92 DLH - PRINT TIME DATE SELECTS.
.*                                     02/22/91 DLH.
.*                       Added lockout of non Exchange orders.
.*                       Added NINORD includes.
.*                                     07/20/84 DLH.        Added spooling.
.*                                     06/19/84 DLH.
.*                       Added Display and Keyin of mailer contact.
.*                                     10/04/82 DLH.
.*                       Added Date check as opening statement.
.*                                     04/27/82 DLH.
.*                       Moved Common Inclusion to correct location.
.*******************************************************************************
.begin patch 4,75
ExtFlag   form      1                 2=from dsprog (external report)
.end patch 4,75
.=========================================================
CHAINFLE FILE
.NOFILE   DIM       8
STAT501  DIM       6
TAX501   FORM      1
AT1       FORM      8.2        .dont use  used as total in printing options
form122  form      12.2
TDMCAMT  FORM       8.2
...
AP1      FORM      8.2          .dont use
BRANCH     FORM        1

HLN      DIM       6          HOLD LIST NUMBER
CONTACT  DIM       3
HOWNER   DIM       4
LDESC    DIM       35
PCE      DIM       2
DATE     DIM       8
PMO      DIM       2
PYR      DIM       2
RKEY     DIM       22
KEY      DIM       6
....
LAR      FORM      8.2         LIST TOTALS
LSH      FORM      10          LIST TOTALS
LAP      FORM      8.2         LIST TOTALS
LINC     FORM      8.2         LIST TOTALS
LrAR     FORM      8.2         LIST rental TOTALS
LrSH     FORM      10          LIST rental TOTALS
LrAP     FORM      8.2         LIST rental TOTALS
LrINC    FORM      8.2         LIST rental TOTALS
LeAR     FORM      8.2         LIST exchange TOTALS
LeSH     FORM      10          LIST exchange TOTALS
LeAP     FORM      8.2         LIST exchange TOTALS
LeINC    FORM      8.2         LIST exchange TOTALS
LeARold  FORM      8.2         LIST exchange TOTALS order date pre 98
LeSHold  FORM      10          LIST exchange TOTALS order date pre 98
LeAPold   FORM      8.2         LIST exchange TOTALS order date pre 98
LeINCold  FORM      8.2         LIST exchange TOTALS order date pre 98
powner    dim       4
exchrent dim       4
exchflag form      1
....
OAR      FORM      8.2         OWNER TOTALS
OSH      FORM      10           OWNER TOTALS
OAP      FORM      8.2         OWNER TOTALS
OINC     FORM      8.2         OWNER TOTALS
GAR      FORM      8.2         GRAND TOTALS
GSH      FORM      10           GRAND TOTALS
GAP      FORM      8.2         GRAND TOTALS
GINC     FORM      8.2         GRAND TOTALS
.END PATCH #3.8 - INCREASED VARS
LKEY     DIM       6           LIST KEY
ONAME    DIM       25
.BLANK31  DIM       31
LINES    FORM      "00"
.BLANK25  DIM       25
HOLDLR   DIM       6
ROLLBR   FORM      1          HOLDS ONE IF ROLLOUT OCCURRED.
ROLLFILE INIT      "EXCHARGE/ROLL"
PerM     form      3.2
form94   form     9.4
form104  form     10.4
strtdate form     5               .starting date for management exchange fees
QUES       INTEGER  1,"0x000024"
NewFlg   form     1   .if new record
WRTFLG   form     1    . 0 test read then update .1 means new record -writeout  .2 is update\write on previous read
RevFLG   form     1   .if revision
EndFlg   form     1   .C0-no exit C1-exit
Dupflg   form     1   . 1 if duplicate record to be created
revtyps  init    "DH JD "            ALLOW Deletion ?
colChrgs collection
colbutt  collection
White       color
LTGray       color
.==============================================================================
.Basic Running Charge Fixed Value
RCBasic   form     "3.90"     .per thousand

.RCShip    form    "50.00"     .flat
RCShip    form     7
..............................................................
.Calculate Vars
.RCSelect  dim       33
.Patch4.1b
RCSelect  dim       55    .changed to accomodate new datalist formatting
.EndPatch4.1b
N22       form      2.2
RPAREN   INIT      ")"
form92   form      9.2
TotSel   form      9.2
TotAr    form      8.2
Flat     form      10
Flatstr  init      "flat"
TotQty   form      9
shipqty  form      9.4
subtotal form      9.2
ARSAV    dim       12
.unsure of what this var is for
Carr    init    0x7f
.===========================================================================================
.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"
.===========================================================================================
Xprt     plform  Exprint
pass    plform  Passwrd
abt     plform  About
x       plform  Ninv0008
.===========================================================================================
.These two vars should be put in the collection once fields begin to be used
.NrchgEditMiscCom
.NrchgEditMiscCom2
.NrchgEditMiscOne
.NrchgEditMiscTwo
.NrchgEditSub -will be disabled and modified only through Calculate button
.NrchgEditAP--don't use

          listins colChrgs,NrchgEditMag,NrchgEditSubOne,NrchgEditSubTwo:
          NrchgEditAR

.         listins colChrgs,NrchgEditBasic,NrchgEditMag,NrchgEditSelect:
.                NrchgEditSubOne,NrchgEditSubTwo,NrchgEditAR,NrchgEditAP


        listins colButt,NrchgNew,NrchgRevise,NrchgQuit,NrchgDelete,NrchgSave,NrchgExit:
          NrchgCalculate


.===========================================================================================
        move    "Ninv0008.PLS",Wprognme
        move    "NIN Running Charge Program",Wfunction
        move    "David Baca",Wauthor
        move    release,Wrelease
        move    Reldate,Wreldate

     winhide
  formload x
  formload abt
  formload pass
  formload Xprt

  create  white=*white
  create  ltgray=*ltgray





.==========================================================================

         MOVE      C1 TO NDATPATH         SET ACCESS TO ISAM KEY
         MOVE      C1 TO NORDPATH         SET ACCESS TO ISAM KEY

         move      "01" to mm
         move      "01" to dd
         move      "98" to yy
         move      "20" to cc
         call      cvtjul
         move      juldays to strtdate

         CLOCK     TIMESTAMP TO STR8
.START PATCH 4.7 REPLACED LOGIC
.         UNPACK    STR8,CE,YR,MO
.         REPLACE   ZFILL,YR
.         REPLACE   ZFILL,MO
         UNPACK    STR8,NRCHGCE,NRCHGYR,NRCHGMO
         REPLACE   ZFILL,NRCHGYR
         REPLACE   ZFILL,NRCHGMO
.END PATCH 4.7 REPLACED LOGIC
.         MOVE      "UNKNOWN" TO NOFILE
.         BRANCH    ROLLBR OF SPOOL
.============================================
  create  NRCHG0001;mFile,FData
  create  NRCHG0001;mEdit,EData,mFile
  create  NRCHG0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result

.===========================================================================================

START1
.START PATCH 4.7 REPLACED LOGIC
.          pack str7,MO,SLASH,CE,YR
           pack str7,NrchgMO,SLASH,NrchgCE,NrchgYR
.END PATCH 4.7 REPLACED LOGIC
         SETITEM NrchgRecDate,0,str7
.START PATCH 4.7 REPLACED LOGIC
.         CLEAR     LR
         CLEAR     NRCHGLR
.END PATCH 4.7 REPLACED LOGIC
         clear     branch
           TRAP      FILEgone giving error IF IO  
.            TRAP      NRCHGGONE IF IO
          if        (Program = "NINV0001")
.         MATCH     "NINV0001" TO PROGRAM     *CHAINED FROM NINV0001 ?
.         IF           EQUAL                        *YES
.START PATCH 4.7 REPLACED LOGIC
.                   UNPACK     COMMENT INTO LR,B1,STR9
.                   REP        ZFILL IN LR
.                   MOVE       C1 TO BRANCH
.                move c1 to newflg
.                setitem NrchgEditLR,0,LR
.                setitem NrchgStatLR,0,LR
....................................
                    UNPACK     COMMENT INTO NRCHGLR,B1,STR9
                    REP        ZFILL IN NRCHGLR
                    MOVE       C1 TO BRANCH
                move c1 to newflg
                setitem NrchgEditLR,0,NRCHGLR
                setitem NrchgStatLR,0,NRCHGLR
.END PATCH 4.7 REPLACED LOGIC
.         endif
          Elseif    (Program = "NINV0007")
.         MATCH     "NINV0007" TO PROGRAM     *CHAINED FROM NINV0007 ?
.         IF          EQUAL                        *YES
.START PATCH 4.7 REPLACED LOGIC
.                UNPACK     COMMENT INTO LR,B1,STR9
.                REP        ZFILL IN LR
..Patch4.2
.                move       LR to HOLDLR
..
.                MOVE       C2 TO BRANCH
.                move c1 to newflg
.                setitem NrchgEditLR,0,LR
.                setitem NrchgStatLR,0,LR
...................................
                UNPACK     COMMENT INTO NRCHGLR,B1,STR9
                REP        ZFILL IN NRCHGLR
                move       NRCHGLR to HOLDLR
                MOVE       C2 TO BRANCH
                move c1 to newflg
                setitem NrchgEditLR,0,NRCHGLR
                setitem NrchgStatLR,0,NRCHGLR
.END PATCH 4.7 REPLACED LOGIC
.         .endif
.begin patch 4,75         
          Elseif    (Program = "NINV0008")
.         MATCH     "NINV0008" TO PROGRAM     *CHAINED FROM DSPROG ?
.         IF          EQUAL                        *YES
          UNpack    Today into Pmo,str4,Pyr
          move      cc,PCE
          move      c2,ExtFlag
          move      "Indexing Excharge",Location
          pack       KeyLocation,"Key: ",NordFld
          pack      taskname from NTWKPATH1,"TEXT\EXCHARGE.DAT,":
                    NTWKPATH1,"EXPRINT -61-64,65-66,20-23,14-19,1-6"
          INDEX   taskname
          goto      external
          
         endif
.end patch 4,75         
.         MOVE         C2 TO BRANCH
         setfocus NrchgEditLR
         setprop  NrchgOK,default=c1
         move     c1 to Endflg


 loop
.        clearevent
        waitevent
  repeat

.WILL be Activated by lost focus of NrchgEditLR
POPULATE
.called by lost focus event of NrchgEditLR

.Will read file to see if previous record has been created
.START PATCH 4.7 REPLACED LOGIC
.         clear     LR
.         getitem   NrchgEditLR,0,LR
.         setitem   NrchgStatLR,0,LR
.         if (lr = "" OR LR="000000")
.                   alert caution,"LR is not on file!",result,"No Record"
.                 return                                                          
.         endif
.         call clean
.         setitem NrchgEditLR,0,LR
.         setitem NrchgStatLR,0,LR
.         MOVE      LR TO NORDFLD
.         REP       ZFILL IN NORDFLD
.         CALL      NORDKEY              .Test if Lr is valid
.         if over
.                   alert caution,"LR is not on file!",result,"No Record"
.                   SETFOCUS NrchgEditLR
..        GOTO      NORD IF OVER
.         return
.         endif
.          MOVE LR,NRCHGFLD
...................................
         clear     NRCHGLR
         getitem   NrchgEditLR,0,NRCHGLR
         setitem   NrchgStatLR,0,NRCHGLR
         if (NRCHGlr = "" OR NRCHGLR="000000")
                    alert caution,"LR is not on file!",result,"No Record"
                  return
         endif
         call clean
         setitem NrchgEditLR,0,NRCHGLR
         setitem NrchgStatLR,0,NRCHGLR
         MOVE      NRCHGLR TO NORDFLD
         REP       ZFILL IN NORDFLD
         move      "Nordkey",Location
         pack       KeyLocation,"Key: ",NordFld
         CALL      NORDKEY              .Test if Lr is valid
         if over
                    alert caution,"LR is not on file!",result,"No Record"
          SETFOCUS NrchgEditLR
.        GOTO      NORD IF OVER
         return
         endif
           MOVE NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
          move      "exchange test read",Location
          pack       KeyLocation,"Key: ",NRCHGFLD
         CALL NRCHGTST
         if over
         move c1 to newflg
         endif

         if (newflg =c1)
Exch
.Check to see if an exchange record
         CMATCH    "2" TO OELCODE
                    Goto      GetVars if Equal
.                   GOTO      ORDOK IF EQUAL         *EXCHANGE ENTIRE
         CMATCH    "3" TO OELCODE
                    Goto      GetVars if Equal
.                   GOTO      ORDOK IF EQUAL         *EXCHANGE

NOEXCH

.if in new mode and an not an exchange
          move    "I",progcode                   .Not an exchange use password to Continue
          pack    str55,"NOT An Exchange Order!! To Continue "
          setitem PasswordStatMssg1,0,str55
          setprop PasswordStatMssg1,visible=1
          setitem PasswordEdit,0,""
          setfocus PasswordEdit
          clear   NPASFLD
          setprop Passwrd,visible=1
          if (passflag = "Y")
                    goto GetVars
          else
                        alert caution,"Not a valid Password!",result,"Not Valid"
                    setfocus NrchgEditLR
                return
          endif
         endif


.=====================================================
GetVars
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      OMLRNUM TO MLR
.         MOVE      OCOBN TO CONTACT
.         MOVE      OLNUM TO LN
.         MOVE      OLON TO OWNER
.         MOVE      C0 TO QTY
.         COMPARE   C0 TO BRANCH
.         IF          EQUAL
.                   MOVE      LR TO NSHPFLD
.                   CALL      NSHPKEY
.                   IF        OVER
.                             MOVE      OQTY TO QTY
.                   ELSE
.                   move    squant,QTY
.
.                   endif
.         ELSE
..patch4.2
.                if (LR = HOLDLR)
.                             MOVE      STR9 TO QTY
.                else
.                   MOVE      LR TO NSHPFLD
.                   CALL      NSHPKEY
.                             IF        OVER
.                                       MOVE      OQTY TO QTY
.                             ELSE
.                   move    squant,QTY
.                             endif
.                endif
..
.         ENDIF
...........................................
         MOVE      OMLRNUM TO NRCHGMLR
         MOVE      OCOBN TO CONTACT
         MOVE      OLNUM TO NRCHGLN
         MOVE      OLON TO NRCHGOWNER
         MOVE      C0 TO NRCHGQTY
         COMPARE   C0 TO BRANCH
         IF          EQUAL
          MOVE      NRCHGLR TO NSHPFLD
                    move      "NSHPKEY",Location
                    pack       KeyLocation,"Key: ",NshpFLD

                    CALL      NSHPKEY
                    IF        OVER
                              MOVE      OQTY TO NRCHGQTY
                    ELSE
                    move    squant,NRCHGQTY

                    endif
         ELSE
               if (NRCHGLR = HOLDLR)
                              MOVE      STR9 TO NRCHGQTY
                else
                    MOVE      NRCHGLR TO NSHPFLD
                    move      "NSHPKEY",Location
                    pack       KeyLocation,"Key: ",NshpFLD
                    CALL      NSHPKEY
                              IF        OVER
                                        MOVE      OQTY TO NRCHGQTY
                              ELSE
                    move    squant,NRCHGQTY
                              endif
                endif
         ENDIF
.END PATCH 4.7 REPLACED LOGIC

ORDOK    CLEAR     LKEY
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      LN,HLN
         MOVE      NRCHGLN,HLN
.END PATCH 4.7 REPLACED LOGIC
         MOVE      HLN TO NDATFLD
         CALL      NDATKEY
.         GOTO      NOLST IF OVER               .pulling list number from system should be Valid
         MOVE      hln TO NDAT3FLD
         CLEAR     NDATTDMC
         rep       zfill in ndat3fld
                    move      "Ndat3KEY",Location
                    pack       KeyLocation,"Key: ",Ndat3key
         CALL      NDAT3KEY
         IF        NOT OVER                  *TDMC BILLING INFO.
           CLEAR     STR10
         move      "Nothing" to str10
         CMATCH    "B" TO NDATTDMC
         IF        EQUAL
           MOVE      "BOTH" TO STR10
         ENDIF
         CMATCH    "R" TO NDATTDMC
         IF        EQUAL
           MOVE      "RENT/SPLIT" TO STR10
         ENDIF
         CMATCH    "E" TO NDATTDMC
         IF        EQUAL
           MOVE      "EXCH/Only" TO STR10
         ENDIF
         cmatch    "Y" to ndatdolc
         clear      str6
         if         equal
                        move       "$/DATE" to str6
         endif

         pack       str55,"TDMC RUN/CHARGE: WE BILL: ",str10,b1,str6
         setitem    NrchgStatStatus,0,str55
         ENDIF
         if (newflg = c1)
                goto newdata
         endif

.=========================================================================================
START2
.START PATCH 4.7 REPLACED LOGIC
.          MOVE LR,NRCHGFLD
          MOVE NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
                    move      "Nrchgtst",Location
                    pack       KeyLocation,"Key: ",NRCHGFLD
          CALL NRCHGTST

          if NOT over
                    move      "Nrchgkey        ",Location
                    pack       KeyLocation,"Key: ",NRCHGFLD
                              CALL NRCHGKEY
                        move c0,Wrtflg
                              call CONT

                        setprop   colbutt,enabled=c0
                        setprop   NrchgNext,enabled=c1
                        setprop   NrchgRevise,enabled=c1
                        setprop   NrchgNew,enabled=c1
                              setprop   NrchgExit,enabled=c1
                    CLOCK     TIMESTAMP TO STR6
.START PATCH 4.7 REPLACED LOGIC
.                        pack      str8,ce,yr,mo
                        pack      str8,Nrchgce,Nrchgyr,Nrchgmo
.END PATCH 4.7 REPLACED LOGIC
                        match     str6 to str8
                        if not    equal
                                  setprop NrchgRevise,enabled=c0
                        endif
                        move      c1 to Endflg
                        return
.                endif
          endif
NewData
.START PATCH 4.7 REPLACED LOGIC
.         MOVE LR,NRCHGFLD
         MOVE NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
                    move      "Nrchgtst",Location
                    pack       KeyLocation,"Key: ",NRCHGFLD
         CALL NRCHGTST
         if not over
          if (newflg = c1)    .if this is created after hitting the new button
                    ALERT        TYPE=QUES,"Your creating another entry for this LR......do you wish to continue?",result,"LR on File"
                    If (Result = c7)    .no
.                                                  goto newdata
                                                   move c0 to Newflg
                                                   setprop NrchgNext,enabled=c0
                                                   call Newbutton
                                                   call clean
                                                   setfocus NrchgEditLR
                                                   return
                                        else
                                                   move c1 to Dupflg
                                        endif

                    endif
          endif
          clear     WRTFLG
          move      c1 to WRTFLG
          setprop   NrchgEditLR,enabled=c0
            CLOCK     TIMESTAMP TO STR6
.START PATCH 4.7 REPLACED LOGIC
.          UNPACK    str6,CE,YR,MO
.          pack         str7,MO,SLASH,CE,YR
          UNPACK    str6,NrchgCE,NrchgYR,NrchgMO
          pack          str7,NrchgMO,SLASH,NrchgCE,NrchgYR
.END PATCH 4.7 REPLACED LOGIC
          SETITEM NrchgRecDate,0,str7
          call      Setvars
          CALL SHIPFILL
          call cont
          setprop   NrchgEditOwn,bgcolor=white
          setprop   NrchgEditOwn,enabled=c1
          setprop   NrchgEditShipped,bgcolor=white
          setprop   NrchgEditShipped,enabled=c1
          setprop   colChrgs,bgcolor=white
          setprop   colChrgs,enabled=c1
          setprop   NrchgDataSelect,enabled=c1
          setprop   colButt,enabled=c0
          setprop   NrchgNext,enabled=c0
          setprop   NrchgCalculate,enabled=c1
          setprop   NrchgQuit,enabled=c1
          setprop   NrchgSave,enabled=c1
          setprop   NrchgOK,enabled=c0
          move      C0 to Endflg
.patch 4.6
.patch4.3
          if (OLNUM = "002312")
                alert caution,"African Wildlife foundation list please add $28 flat charge if needed.",result,"Flat Charge"
.          endif
.endpatch4.3
.patch4.3
          Elseif (OLNUM = "001103")
                alert caution,"Citizens Soldier please add $10/m running charge if needed.",result,"per/M Charge"
.          endif
.endpatch4.3
.patch4.4
          elseif (OLNUM = "018922")
.START PATCH 4.7 REPLACED LOGIC
.                                       if (QTY < 5000)
                                        if (NRCHGQTY < 5000)
.END PATCH 4.7 REPLACED LOGIC
                    alert caution,"Elephant Sanctuary needs to be charged a $10 email fee for orders < 5000",result,"Flat Charge"
                                        endif
.           endif
.endpatch4.4
          endif
.end patch4.6
                              RETURN







Shipfill
         CLEAR STR9
.START PATCH 4.7 REPLACED LOGIC
.         MOVE QTY TO STR9
         MOVE NRCHGQTY TO STR9
.END PATCH 4.7 REPLACED LOGIC
         call    Trim using str9
         setitem NrchgEditShipped,0,STR9
         RETURN

.============================================================================================
.Next Lr Button
NextLR
.          getitem NrchgEditLr,0,LR
.START PATCH 4.7 REPLACED LOGIC
.          getitem NrchgStatLR,0,LR
.          MOVE LR,NRCHGFLD
          getitem NrchgStatLR,0,NRCHGLR
          MOVE NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
.          CALL NRCHGTST
          CALL NRCHGKS
               if not over
                        move c2,wrtflg
                              CALL CONT
                        setprop   colbutt,enabled=c0
                        setprop   NrchgRevise,enabled=c1
                        setprop   NrchgNew,enabled=c1
                              setprop   NrchgExit,enabled=c1
                    CLOCK     TIMESTAMP TO STR6
.START PATCH 4.7 REPLACED LOGIC
.                        pack      str8,ce,yr,mo
                        pack      str8,NRCHGce,NRCHGyr,NRCHGmo
.END PATCH 4.7 REPLACED LOGIC
                        match     str6 to str8
                        if not    equal
                                  setprop NrchgRevise,enabled=c0
                        endif
                else
                        alert note,"Done!  End of File!!!",result,"No more records"
                    return
                endif
          RETURN
.============================================================================================
CONT
.START PATCH 4.7 REPLACED LOGIC
.          move       lr to tinvfld
          move       NRCHGlr to tinvfld
.END PATCH 4.7 REPLACED LOGIC
          call       tinvkey
          if         not over
                    move       tinvdolr to form122
                    mult       ".01" by form122
                    move        c0 to TDMCAMT
                    add         form122 to TDMCAMT
distdmc  
          move       "99/99/9999" to str10
          unpack     tinvdate,str4,str4
          unpack     tinvdate,str5
          pack       str8,str4,str5         
          edit       str8 to str10
          clear      str29
          pack       str29,"Triplex Billed: ",TDMCAMT
          clear      str35
                    pack       str35,"TDMC##: ",tinvinv,b5,str10
          call       tinvks
          if over
                     GOTO LOAD
          endif
          match      tinvfld to tinvlr
          if not equal
                     GOTO LOAD
          endif
          move       c0 to form122
          move       tinvdolr to form122
          mult       ".01" by form122
          add         form122 to TDMCAMT
          goto       distdmc
//
           else
                    Clear Str29
                    Clear str35
//
         
         endif
LOAD

           if       (newflg =c1)
          setitem    NrchgStatTricomm,0,Str29
          setitem    NrchgStatTriNum,0,str35
                return
         endif
.Patch4.1c=============================================
          setitem    NrchgStatTricomm,0,Str29
          setitem    NrchgStatTriNum,0,str35
.EndPatch4.1c============================================
         CALL LOADFRM
         return
.============================================================================================
DELREC
.START PATCH 4.7 REPLACED LOGIC
.          getitem NrchgStatLR,0,LR
.         MOVE    LR,NRCHGFLD
          getitem NrchgStatLR,0,NRCHGLR
         MOVE    NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
         ALERT         TYPE=QUES,"Are you sure you want to Delete this LR!",result,"Deleting Record"
         If (Result = c6)    .yes
                CALL NRCHGDEL
          IF OVER
                    alert caution,"LR is not on file!",result,"No Record"
                        return
          Endif
          alert note,"Record Deleted!",result,"Deleted Record"
                call deletebutton
          return
         endif
         return
LOADFRM
.START PATCH 4.7 REPLACED LOGIC
.         setitem   NrchgEditLR,0,LR
.         setitem   NrchgStatLR,0,LR
..Loads the form
.         call      Setvars
.         clear     str7
.         pack      str7,MO,slash,CE,YR
.         setitem   NrchgRecDate,0,str7
.         setitem   NrchgEditTriplex,0,TPI
.         clear     str9
.         move      QTY TO STR9
.         CALL      TRIM USING STR9
.         setitem   NrchgEditShipped,0,STR9
.         clear     str11
.         move      AR TO STR11
.         CALL      TRIM USING STR11
.         setitem   NrchgEditAR,0,STR11
.         clear     str11
.         move      AP to STR11
.         setitem   NrchgEditAP,0,str11
...................................
         setitem   NrchgEditLR,0,NRCHGLR
         setitem   NrchgStatLR,0,NRCHGLR
.Loads the form
         call      Setvars
         clear     str7
         pack      str7,NRCHGMO,slash,NRCHGCE,NRCHGYR
         setitem   NrchgRecDate,0,str7
         setitem   NrchgEditTriplex,0,NRCHGTPI
         clear     str9
         move      NRCHGQTY TO STR9
         CALL      TRIM USING STR9
         setitem   NrchgEditShipped,0,STR9
         clear     str11
         move      NRCHGAR TO STR11
         CALL      TRIM USING STR11
         setitem   NrchgEditAR,0,STR11
         clear     str11
         move      NRCHGAP to STR11
         setitem   NrchgEditAP,0,str11
.END PATCH 4.7 REPLACED LOGIC
           RETURN

CLEAN
.activated by new button

         SETITEM   NrchgEditLR,0,""
         setitem NrchgStatLR,0,""
         setitem   NrchgEditMlrNo,0,""
         setitem   NrchgStatMlr,0,""
         setitem   NrchgStatCCTO,0,""
         setitem   NrchgEditList,0,""
         setitem   NrchgStatLstNm,0,""
         setitem   NrchgEditOwn,0,""
         setitem   NrchgStatTextOComp,0,""
         setitem   NrchgEditTriplex,0,""
         setitem   NrchgStatTriComm,0,""
         setitem   NrchgStatTriNum,0,""
         setitem   NrchgEditShipped,0,""
         setitem   NrchgStatStatus,0,""
         setitem   NrchgEditBasic,0,""
         setitem   NrchgEditMag,0,""
         setitem   NrchgEditSelect,0,""
         setitem   NrchgEditSubOne,0,""
         setitem   NrchgEditSubTwo,0,""
         setitem   NrchgEditSecretSub,0,""
         setitem   NrchgEditAR,0,""
         setitem   NrchgEditSub,0,""
         setitem   NrchgEditAP,0,""
           RETURN

VERIFY
.OWNCHECKS
.        call owncheck
OwnCheck
.Does check on lost focus
.START PATCH 4.7 REPLACED LOGIC
.         move      c0 to owner
         move      c0 to Nrchgowner
.END PATCH 4.7 REPLACED LOGIC
         move      c0 to NOWNFLD
         getitem   NrchgEditOwn,0,NOWNFLD
         call      zfillit using nownfld,C0
         if (NOWNFLD="0000" or NOWNFLD="")
                     alert caution,"Must have a valid Owner!",result,"Need an Owner"
                   setitem   NrchgEditOwn,0,""
           setitem   NrchgStatTextOComp,0,""
                     setfocus NrchgEditOwn
           return
           endif
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      NOWNFLD to owner
         MOVE      NOWNFLD to Nrchgowner
.END PATCH 4.7 REPLACED LOGIC
         CALL      NOWNKEY
           if   over
                     alert caution,"Must have a valid Owner!",result,"Need an Owner"
                   setitem   NrchgEditOwn,0,""
           setitem   NrchgStatTextOComp,0,""
                     setfocus NrchgEditOwn
                     return
           ENDIF
         setitem   NrchgEditOwn,0,NOWNFLD
         setitem   NrchgStatTextOComp,0,OWNOCPY
.         RETURN



.Shipping
        getitem NrchgEditShipped,0,str9
        call zfillit using str9,C0
        if (str9 = "000000000")
        alert caution,"SHIPPING QTY HAS NULL VALUE!",result,"Shipping Qty"
        setfocus NrchgEditShipped
        return

        endif

        if (str9 = "")
        alert caution,"SHIPPING QTY HAS NULL VALUE!",result,"Shipping Qty"
        setfocus NrchgEditShipped
        return
        endif

.AR CHECK
        getitem   NrchgEditAR,0,str11
        call    REMOVECHAR using str11,period
        call zfillit using str11,C0
        IF (STR11="00000000000" | STR11="")
        alert caution,"AR HAS NULL VALUE!",result,"INVALID AR"
          SETFOCUS NrchgEditAR
        RETURN
        ENDIF

        scan DASH,str11     .check to see if this is correct syntax
        IF EQUAL
                  alert caution,"AR CONTAINS A NEGATIVE VALUE!",result,"NEGATIVE AR"
          SETFOCUS NrchgEditAR
                return
        endif



.APCHECK
.For use in AP which is now disabled
.        clear     str11
.        getitem   NrchgEditAR,0,str11
.        scan DASH TO AP     .check to see if this is correct syntax
.        reset     str11
.                   IF EQUAL
.                 alert caution,"AP CONTAINS A NEGATIVE VALUE!",result
.         SETFOCUS NrchgEditAP
.                return
.        endif

DATA
.START PATCH 4.7 REPLACED LOGIC
.         getitem NrchgStatLR,0,LR
.         getitem   NrchgEditMlrNo,0,str7
.         unpack    str7,mlr,mlrcnt
.         getitem   NrchgEditList,0,LN
.         getitem   NrchgEditOwn,0,OWNER
.         getitem   NrchgEditTriplex,0,TPI
.         CLEAR     STR9
.         getitem   NrchgEditShipped,0,STR9
.         MOVE      STR9 TO QTY
.         CLEAR     STR11
.         getitem   NrchgEditAR,0,STR11
.         MOVE      STR11 TO AR
.         CLEAR     STR11
.         getitem   NrchgEditAP,0,STR11
.         MOVE      STR11 TO AP
.         getitem   NrchgRecDate,0,str7
.         unpack    str7,MO,STR1,CE,YR
........................................
         getitem NrchgStatLR,0,NRCHGLR
         getitem   NrchgEditMlrNo,0,str7
         unpack    str7,NRCHGmlr,NRCHGmlrcnt
         getitem   NrchgEditList,0,NRCHGLN
         getitem   NrchgEditOwn,0,NRCHGOWNER
         getitem   NrchgEditTriplex,0,NRCHGTPI
         CLEAR     STR9
         getitem   NrchgEditShipped,0,STR9
         MOVE      STR9 TO NRCHGQTY
         CLEAR     STR11
         getitem   NrchgEditAR,0,STR11
         MOVE      STR11 TO NRCHGAR
         CLEAR     STR11
         getitem   NrchgEditAP,0,STR11
         MOVE      STR11 TO NRCHGAP
         getitem   NrchgRecDate,0,str7
         unpack    str7,NRCHGMO,STR1,NRCHGCE,NRCHGYR
.END PATCH 4.7 REPLACED LOGIC

         If (WRTFLG = C2)            .in database but read on a keyseq-used to keep FP on correct record
                call NRCHGUPD
         Endif

         IF (WRTFLG = C0)            .already in database
.START PATCH 4.7 REPLACED LOGIC
.                MOVE LR TO NRCHGFLD
                MOVE NRCHGLR TO NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
                    call NRCHGTST
                    if over
                              alert     caution,"INVALID READ ON LR...CANNOT UPDATE!!!!",result,"ERROR ON UPDATE"
                        RETURN
                    else
                              call NRCHGUPD

                    endif
        Endif
.Update record information

         IF (WRTFLG = C1)             .if new record
          call      NRCHGWRT
         ENDIF
.START PATCH 4.7 REPLACED LOGIC
.         getitem NrchgStatLR,0,LR
.          MOVE LR,NRCHGFLD
         getitem NrchgStatLR,0,NRCHGLR
           MOVE NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
         CALL NRCHGTST
         call clean
         call loadfrm
         call savebutton
         move c0 to Dupflg
         move c0 to WRTflg
           move c0 to newflg
           move c0 to revflg
         move C1 to EndFlg
         call branch1
.         move C0 to Branch
           RETURN
.If from prog 1 or 7 will return to that prog after save
Branch1

        BRANCH    BRANCH TO EOJX,eojy
        return
.        STOP
EOJX     
         MOVE      "NINV0008" TO PROGRAM
         winshow
         CHAIN     "NINV0001"
         stop
         return
EOJy
         MOVE      "NINV0008" TO PROGRAM

         winshow
         CHAIN     "NINV0007"
         stop
         return
ReviseButton
         setitem   NrchgEditLR,0,""
         move      c1 to RevFlg
         move      C0 to EndFlg
         setprop   colButt,enabled=c0
         setprop   NrchgEditLR,enabled=c0
         setprop   NrchgEditOwn,enabled=c1
         setprop   NrchgEditOwn,bgcolor=white
         setprop   colChrgs,enabled=c1
         setprop   colChrgs,bgcolor=white
         setprop   NrchgEditSub,enabled=c0
         setprop   NrchgEditSub,bgcolor=ltgray
         setprop   NrchgDataSelect,enabled=c1
         setprop   NrchgSAVE,enabled=c1
         setprop   NrchgQUIT,enabled=c1
         setprop   NrchgCalculate,enabled=c1
         setprop   NrchgNext,enabled=c0
         setprop   NrchgOK,enabled=c0
         reset   revtyps
         scan    INITS,revtyps
         if equal
                   setprop NrchgDelete,enabled=c1
         endif


         RETURN

SAVEBUTTON
.after save routine is complete
         setprop   NrchgEditLR,enabled=c1
         setprop   NrchgEditLR,bgColor=white
         setprop   NrchgEditOwn,enabled=c0
         setprop   NrchgEditOwn,bgColor=ltgray
         setprop   NrchgEditTriplex,enabled=c0
         setprop   NrchgEditShipped,enabled=c0
         setprop   NrchgEditShipped,bgColor=ltgray
         setitem   NrchgEditSecretSub,0,""
         setprop   ColChrgs,enabled=c0
         setprop   colChrgs,bgcolor=ltgray
         setprop   NrchgDataSelect,enabled=c0
         setitem   NrchgDataSelect,c0,c0
         setprop   colButt,enabled=c0
         setprop   NrchgNew,enabled=c1
         setprop   NrchgEXIT,enabled=c1
         setprop   NrchgRevise,enabled=c1
         setprop   NRCHGOK,enabled=c1
.Patch4.1d
         setprop NRCHGSAVE,default=c0
.EndPatch4.1d
         return
NEWBUTTON
.done after new button click event
          setprop   colButt,enabled=c0
          setprop   NrchgNew,enabled=c1
          setprop   NrchgExit,enabled=c1
          move      c1 to endflg
         RETURN

DELETEBUTTON
           move c0 to newflg
         move c0 to revflg
         move C1 to EndFlg
           setprop   colbutt,enabled=c0
         setprop   NrchgEditOwn,enabled=c0
         setprop   NrchgEditOwn,bgcolor=ltgray
         setprop   NrchgEditTriplex,enabled=c0
         setprop   NrchgEditTriplex,bgcolor=ltgray
         setprop   NrchgEditShipped,enabled=c0
         setprop   NrchgEditShipped,bgcolor=ltgray
         setprop   colchrgs,enabled=c0
         setprop   colchrgs,bgcolor=ltgray
         setitem   NrchgDataSelect,c0,c0
         setprop   NrchgDataSelect,enabled=c0
           setprop   NrchgEditLr,bgcolor=white
           setprop   NrchgEditLR,enabled=c1
           setprop   NrchgNew,enabled=c1
           setprop   NrchgExit,enabled=c1
         call clean
         move c0,wrtflg
         setprop NrchgOK,enabled=c1
         setfocus NrchgEditLR
         RETURN

QUITBUTTON

         move c0 to Dupflg
           move c0 to newflg
         move c0 to revflg
         move C1 to EndFlg
.         move C0 to Branch
           setprop   colbutt,enabled=c0
         setprop   NrchgEditOwn,enabled=c0
         setprop   NrchgEditOwn,bgcolor=ltgray
         setprop   NrchgEditTriplex,enabled=c0
         setprop   NrchgEditTriplex,bgcolor=ltgray
         setprop   NrchgEditShipped,enabled=c0
         setprop   NrchgEditShipped,bgcolor=ltgray
         setprop   colchrgs,enabled=c0
         setprop   colchrgs,bgcolor=ltgray
         setitem   NrchgDataSelect,c0,c0
         setprop   NrchgDataSelect,enabled=c0
           setprop   NrchgEditLr,bgcolor=white
           setprop   NrchgEditLR,enabled=c1
           setprop   NrchgNew,enabled=c1
           setprop   NrchgExit,enabled=c1
         if (wrtflg = c2 | wrtflg = c1)
          call clean
                move c0,wrtflg
                setprop NrchgOK,enabled=c1
                setfocus NrchgEditLR
                RETURN
         endif
           IF (WRTFLG = C0)
                    setprop NrchgRevise,enabled=c1
                    move c0 to WRTFLG
.START PATCH 4.7 REPLACED LOGIC
.                getitem NrchgStatLR,0,LR
.                   MOVE LR,NRCHGFLD
                getitem NrchgStatLR,0,NRCHGLR
                    MOVE NRCHGLR,NRCHGFLD
.END PATCH 4.7 REPLACED LOGIC
          CALL NRCHGkey
          call clean
          call loadfrm

           ENDIF
.Patch4.1d
                setprop NRCHGSAVE,default=c0
.EndPatch4.1d
                setprop NrchgOK,enabled=c1
         RETURN


.===========================================================================================
.................................................................................
FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3
.        ,FileGo3
FileGo1
          setprop   NrprtWinPrint,visible=c1

FileGo2
        return
FileGo3
        call click_NrchgExit
        Return

EditGo
        return
HelpGo
        setprop AboutMssg,visible=C1
        return
..................................................................
Datalist

.uses a call style branch to place datalist items in variables and loop back through
         clear n2
         move c0 to form92
         move c0 to n22

         GETITEM NrchgDataSelect,c0,n2
           move    c0 to RCSelect
         clear   flat
         loop
                  until (N2=c0)
                move    c0 to RCSelect
                GETITEM NrchgDataSelect,n2,RCSelect
                scan    Flatstr in RcSelect
                reset   RcSelect
                if equal
                    call    RemoveChar using RcSelect,RPAREN
                    scan    "+" in RcSelect
                    bump    RcSelect
                    move    RCSELECT,N6
                    add     n6 to flat
                    goto direct
                endif

                call    RemoveChar using RcSelect,RPAREN
                scan    "+" in RcSelect
                bump    RcSelect
                move    RCSELECT,N22
                add     n22 to form92
direct
                GETITEM NrchgDataSelect,c3,n2       
        repeat

SelectChg
                    move c0 to str9
                getitem NrchgEditShipped,0,str9
                move    str9 to n9
                divide  "1000",n9,form94
                move    form94 to shipqty
                mult    form92 to form94
                move    form94 to TotSel
                add     flat to TotSel
                clear subtotal
                    add     totsel to subtotal
                move    totsel to str12
                call    Trim using str12
                setitem NrchgEditSelect,0,str12
                if (Dupflg = c1)
                    setitem NrchgEditBasic,0,""
                        goto Shipchg
                endif
BasicChg
                clear   form92
                mult    RCBasic,shipqty,form92
                add     form92 to subtotal
                move    form92 to str13
                call trim using str13
                setitem NrchgEditBasic,0,str13

Shipchg
                clear   Rcship
                getitem NrchgEditMag,0,str7
                if (str7="" or str7="0")
                        move c0,RCShip
                else
                    move     str7,RCShip
                endif
                add     RCSHIP to subtotal
Subtotal
                move    subtotal to str18
                call trim using str18
                setitem NrchgEditSub,0,str18
FillNew

AddMisc
                clear totar
                add subtotal to TotAR
                clear form92
                getitem NrchgEditSubOne,0,str11
                clear form92
                move str11 to form92
                add form92 to TotAR
                getitem NrchgEditSubTwo,0,str11
                clear form92
                move str11 to form92
                add form92 to TotAR


                move totAR to str11
                call trim using str11
                setitem NrchgEditAR,0,str11
.Patch4.1d
                setprop NRCHGSAVE,default=c1
                setfocus NrchgEditAR
.EndPatch4.1d
                return


Setvars
...will load vars with NINorder vars except owner which if in exchargefile
.                   will use that number
.START PATCH 4.7 REPLACED LOGIC
.          MOVE      NRCHGFLD TO LR
.          PACK      MKEY FROM MLR,CONTACT
.          MOVE      CONTACT TO MLRCNT
.          REP       ZFILL,MLRCNT
          MOVE      NRCHGFLD TO NRCHGLR
          PACK      MKEY FROM NRCHGMLR,CONTACT
          MOVE      CONTACT TO NRCHGMLRCNT
          REP       ZFILL,NRCHGMLRCNT
.END PATCH 4.7 REPLACED LOGIC
          CALL      NMLRKEY
.START PATCH 4.7 REPLACED LOGIC
.          pack      Str7,mlr,mlrcnt
          pack      Str7,NRCHGmlr,NRCHGmlrcnt
.END PATCH 4.7 REPLACED LOGIC
          setitem   NrchgEditMlrNo,0,str7
          setitem   NrchgStatMlr,0,MCOMP
          setitem   NrchgStatCCTO,0,MCCTO
.START PATCH 4.7 REPLACED LOGIC
.          setitem   NrchgEditList,0,LN
          setitem   NrchgEditList,0,NrchgLN
.END PATCH 4.7 REPLACED LOGIC
          setitem   NrchgStatLstNm,0,OLSTNAME
.START PATCH 4.7 REPLACED LOGIC
.          MOVE      OWNER TO NOWNFLD
.          call      zfillit using nownfld,C0
.          CALL      NOWNKEY
.           setitem   NrchgEditOwn,0,OWNER
          MOVE      NRCHGOWNER TO NOWNFLD
          call      zfillit using nownfld,C0
          CALL      NOWNKEY
            setitem   NrchgEditOwn,0,NRCHGOWNER
.END PATCH 4.7 REPLACED LOGIC
          setitem   NrchgEditOwn,0,NOWNFLD
          setitem   NrchgStatTextOComp,0,OWNOCPY
          return

.==============================================================================================
.==============================================================================================
.==============================================================================================
Print1
          call OrderSetMouseBusy
.         TRAP      STOP IF F1
         move       c1 to nordpath
.         MOVE      "EXPRINT",NOFILE
         Getitem    NrprtCheckIndex,0,N1
         If (N1 = C1)
                        IFNZ        PC
                    PREP      CHAINFLE,"CHAINFLE/CHN"
                        XIF
* 59-60= YR            2
* 61-62 = MO           2
* 20-23 = OWNER        4
* 14-19 = LIST NO      6
* 7-10 = MLR            4
* 1-6    lr             6   = 20   mlr not used anymore 2/98 DLH
DATE
         MOVE      "0",LINES
         clear     str6
         getitem   NrprtEditDate,0,str6
         call        zfillit using str6,C0
         if (str6 = ""|str6 = "000000")
                    alert caution,"A Valid Date is required!!!",result,"No Date"
                call OrderSetMouseFree
                return
         endif

         unpack    str6,PMO,PCE,PYR

.======================================================================================
.turn on before live
.                       pack      taskname from NTWKPATH2,"sunIDXNT ",NTWKPATH1,"TEXT\EXCHARGE.DAT,":
.START PATCH 4.71
.begin patch 4.72
.                       pack      taskname from NTWKPATH2,"sunindex ",NTWKPATH1,"TEXT\EXCHARGE.DAT,":
.End PATCH 4.71
                        pack      taskname from NTWKPATH1,"TEXT\EXCHARGE.DAT,":
                         NTWKPATH1,"EXPRINT -61-64,65-66,20-23,14-19,1-6"
                       INDEX   taskname
.                      execute   taskname
.end patch 4.72
.========================================================================================

*  PRINT DELIMITER PMO,PYR
         Endif
SPOOL
         MOVE      "0" TO ROLLBR
         clear     n1
         getitem   NrprtCheckSpool,0,n1
         if (n1 = c1)
.========================================================================================
.Turn on before live
           IFNZ        PC
                   SPLOPEN   "RUNPRT/PRT:PRINT","Q",4
           XIF
           IFZ         PC
                pack      str35,NTWKPATH1,"RUNPRT.LST"
                SPLOPEN   STR35,"Q"
                 XIF
        endif
.=====================================================================

powner
         move       C0,Powner
         getitem     NrprtEditOwn,0,Powner
         call        zfillit using Powner,C0
WHICH
         clear     n1
         getitem    NrprtRadioInt,0,n1
         if (n1 = c1)
                goto Internal
         else
                goto External
         endif
INTERNAL

         REPLACE   ZFILL,PMO
         REPLACE   ZFILL,PYR
.START PATCH 4.7 REPLACED LOGIC
.         REPLACE   ZFILL,CE
         REPLACE   ZFILL,NRCHGCE
.END PATCH 4.7 REPLACED LOGIC
.turn on before live
.=============================================================
         pack      str55,NTWKPATH1,"EXPRINT|10.10.30.103:502"
         OPEN      EXPRINT,STR55
.=============================================================
...............................................................................
PRT1
            filepi    1;exprint
.START PATCH 4.7 REPLACED LOGIC
.         READKS    EXPRINT;LR,MLR,MLRCNT,LN,OWNER,TPI,QTY,AR,AP,CE,YR,MO
         READKS    EXPRINT;NRCHGLR,NRCHGMLR,NRCHGMLRCNT,NRCHGLN,NRCHGOWNER,NRCHGTPI,NRCHGQTY,NRCHGAR,NRCHGAP,NRCHGCE,NRCHGYR,NRCHGMO
.END PATCH 4.7 REPLACED LOGIC
         GOTO      EOJ2 IF OVER
         ADD       C1 TO N5
.         DISPLAY   *P10:12,"RECORDS IN ",N5
.START PATCH 4.7 REPLACED LOGIC
.         MATCH     PYR,YR
.         GOTO      PRT1 IF NOT EQUAL
.         MATCH     PMO,MO
.         GOTO      PRT1 IF NOT EQUAL
.         MATCH     PCE,CE
         MATCH     PYR,NRCHGYR
         GOTO      PRT1 IF NOT EQUAL
         MATCH     PMO,NRCHGMO
         GOTO      PRT1 IF NOT EQUAL
         MATCH     PCE,NRCHGCE
.END PATCH 4.7 REPLACED LOGIC
         GOTO      PRT1 IF NOT EQUAL
         type      powner
         if        equal
.START PATCH 4.7 REPLACED LOGIC
.         match     powner to owner
.         goto      prt1 if not equal
.         endif
.         MATCH     OWNER,HOWNER
         match     powner to NRCHGOWNER
         goto      prt1 if not equal
         endif
         MATCH     NRCHGOWNER,HOWNER
.END PATCH 4.7 REPLACED LOGIC
         GOTO      BREAK1 IF NOT EQUAL
.START PATCH 4.7 REPLACED LOGIC
.         MATCH     LN,HLN
         MATCH     NRCHGLN,HLN
.END PATCH 4.7 REPLACED LOGIC
         GOTO      BREAK2 IF NOT EQUAL
         CALL      DETAIL
         GOTO      PRT1
. ............................................................................
BREAK1   COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      LTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
* SUBROUTINE OWNER TOTALS COMMENTED OUT APRIL 4 1982 AS PER S.A.
         CALL      OH
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      LDESC
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      LHEAD
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         CALL      DETAIL
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
.
         GOTO      PRT1
.
. ............................................................................
BREAK2
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LTOT
.
         CALL      OH
         CALL      LDESC
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LHEAD
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      DETAIL
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         GOTO      PRT1
. ............................................................................
* LIST TOTALS
LTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         PRINT *L,*L,*L,*3,"LIST *":
               *L,*13,"--------",*67,"-------",*100,"---------":
                   *115,"--------":
               *L,*1,"Exchange*",*10,LeAR,*64,LeSH,*98,LeAP,*112,LeINC:
               *L,*13,"--------",*67,"-------",*100,"---------":
                   *115,"--------":
               *L,*1,Hpbon,"Exchange*",*10,LeAR,*64,LeSH,*98,LeAP,*112,LeINC:
               *L,*13,"--------",*67,"-------",*100,"---------":
                   *115,"--------",hpboff:
               *L,*2,"Rental*",*10,LrAR,*64,LrSH,*98,LrAP,*112,LrINC:
               *L,*13,"--------",*67,"-------",*100,"---------":
               *L,*13,"--------",*67,"-------",*100,"---------":
                   *115,"--------":
               *L,*3,"TOTAL*",*10,LAR,*64,LSH,*98,LAP,*112,LINC
         MOVE      "0",LAR
         MOVE      "0",LSH
         MOVE      "0",LAP
         MOVE      "0",LINC
         MOVE      "0",LeAR
         MOVE      "0",LeSH
         MOVE      "0",LeAP
         MOVE      "0",LeINC
         MOVE      "0",LrAR
         MOVE      "0",LrSH
         MOVE      "0",LrAP
         MOVE      "0",LrINC
         ADD       c10,LINES
        RETURN
* OWNER TOTALS
OTOT
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         MOVE      "0",OAR
         MOVE      "0",OSH
         MOVE      "0",OAP
         MOVE      "0",OINC
        RETURN
* OWNER HEADING
OH
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      OWNER,HOWNER
         MOVE      NRCHGOWNER,HOWNER
.END PATCH 4.7 REPLACED LOGIC
         MOVE      HOWNER TO NOWNFLD
         rep       zfill in nownfld
         CALL      NOWNKEY
         PRINT     hp17ptch,*F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"OWNER: ",HOWNER,"  ",OWNOCPY
         MOVE      "5",LINES
        RETURN
* GRAND TOTAL HEADING
GH
         PRINT     hp17ptch,*F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"GRAND TOTALS: ":
                   *L,*23,"AR",*65,"SHIPPED":
                   *98,"AP",*116,"LR INC.":
           *L,*2," TOTAL **",*19,"--------",*65,"-------",*95,"---------":
                   *115,"--------",*L,*16,GAR,*62,GSH,*93,GAP,*112,GINC
        RETURN
...............................................................................
LDESC    CLEAR     LKEY
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      LN,HLN
         MOVE      NRCHGLN,HLN
.END PATCH 4.7 REPLACED LOGIC
         MOVE      HLN TO LKEY
         MOVE      LKEY TO NDATFLD
         CALL      NDATKEY
           IF          OVER
           MOVE        "NO LIST FOUND!!!!!!!!" TO OLSTNAME
           ENDIF
           CLEAR     NXRFMLR             *CLEAR VARIABLE IN CASE OVER.
           MOVE        HLN TO NXRFFLD
           MOVE        C1 TO NXRFPATH
           CALL      NXRFKEY
           PRINT     *L,*1,"LIST: ",LKEY,"  ",OLSTNAME:
                       *L,*1,"X-REF MLR## ",NXRFMLR
         ADD       "2",LINES
         RETURN
...............................................................................
* DETAIL HEADING
LHEAD    
         PRINT     *16,"AR",*69,"QTY":
                   *L,*1,"LR",*15,"AMT",*26,"MAILER",*67,"SHIPPED":
                   *85,"INVOICE NO.",*103,"AP",*116,"LR INC.":
                   *L,*1,"------",*13,"--------",*26,"------",*67,"-------":
                   *85,"-----------",*100,"---------",*115,"--------"
         ADD       "3",LINES
         RETURN
...............................................................................
DETAIL
.START PATCH 4.7 REPLACED LOGIC
.         PACK      MKEY FROM MLR,MLRCNT
.         CALL      NMLRKEY
..         CALL      MLRREAD
.         move      lr to nordfld
         PACK      MKEY FROM NRCHGMLR,NRCHGMLRCNT
         CALL      NMLRKEY
         move      NRCHGlr to nordfld
.END PATCH 4.7 REPLACED LOGIC
         rep       zfill in nordfld
         call      nordkey
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         call      cvtjul
.         
         move      c1 to exchflag
         clear     exchrent
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         if        equal
         move      c1 to exchflag                  .true
         move      "Exch" to ExchRent          
         else
         move     c2 to exchflag                   .false its rental
         move     "rent" to exchRent
         MOVE     C0 TO N9
         MOVE     OEXQTY TO N9
         COMPARE  C0 TO N9
         IF       NOT EQUAL
         MOVE     "SPLT" TO EXCHRENT
         ENDIF
         endif
         move     c0 to form104
.START PATCH 4.7 REPLACED LOGIC
.         move     qty to form104
.         mult     ".001" by form104
.         move     c0 to form94
.         move     ar to form94
.         divide   form104 into form94
.         move     c0 to perm
.         add      form94 to perm 
.         MOVE      AR,AT1
.         MOVE      AP,AP1
..............................
         move     NRCHGqty to form104
         mult     ".001" by form104
         move     c0 to form94
         move     NRCHGar to form94
         divide   form104 into form94
         move     c0 to perm
         add      form94 to perm 
         MOVE      NRCHGAR,AT1
         MOVE      NRCHGAP,AP1
.END PATCH 4.7 REPLACED LOGIC
         SUB       AP1,AT1
         if        (juldays < strtdate)
.START PATCH 4.7 REPLACED LOGIC
.         PRINT     *1,hpbon,LR,*10,AR,*22,MCOMP,hpboff,*65,QTY,"@",perm,*86,TPI:
.                   *98,AP,*112,AT1,b3,ExchRent
.         else
.         PRINT     *1,LR,*10,AR,*22,MCOMP,*48,*65,QTY,"@",perm,*86,TPI:
.                   *98,AP,*112,AT1,b3,ExchRent
.         endif          
.         ADD       AR,LAR
.         ADD       QTY,LSH
.         ADD       AP,LAP
.         ADD       AT1,LINC
..         
.         compare   c1 to exchflag
.         if        equal
.         if        (juldays < strtdate)
.         ADD       AR,LeARold
.         ADD       QTY,LeSHold
.         ADD       AP,LeAPold
.         ADD       AT1,LeINCold
.         else
.         ADD       AR,LeAR
.         ADD       QTY,LeSH
.         ADD       AP,LeAP
.         ADD       AT1,LeINC
.         endif
.         endif
..
.         compare   c2 to exchflag
.         if        equal
.         ADD       AR,LrAR
.         ADD       QTY,LrSH
.         ADD       AP,LrAP
.         ADD       AT1,LrINC
.         endif
..
.         ADD       AR,OAR
.         ADD       QTY,OSH
.         ADD       AP,OAP
.         ADD       AT1,OINC
..
.         ADD       AR,GAR
.         ADD       QTY,GSH
.         ADD       AP,GAP
.         ADD       AT1,GINC
..
.         ADD       C1,LINES
............................................
         PRINT     *1,hpbon,NRCHGLR,*10,NRCHGAR,*22,MCOMP,hpboff,*65,NRCHGQTY,"@",perm,*86,NRCHGTPI:
                   *98,NRCHGAP,*112,AT1,b3,ExchRent
         else
         PRINT     *1,NRCHGLR,*10,NRCHGAR,*22,MCOMP,*48,*65,NRCHGQTY,"@",perm,*86,NRCHGTPI:
                   *98,NRCHGAP,*112,AT1,b3,ExchRent
         endif          
         ADD       NRCHGAR,LAR
         ADD       NRCHGQTY,LSH
         ADD       NRCHGAP,LAP
         ADD       AT1,LINC
.         
         compare   c1 to exchflag
         if        equal
         if        (juldays < strtdate)
         ADD       NRCHGAR,LeARold
         ADD       NRCHGQTY,LeSHold
         ADD       NRCHGAP,LeAPold
         ADD       AT1,LeINCold
         else
         ADD       NRCHGAR,LeAR
         ADD       NRCHGQTY,LeSH
         ADD       NRCHGAP,LeAP
         ADD       AT1,LeINC
         endif
         endif
.
         compare   c2 to exchflag
         if        equal
         ADD       NRCHGAR,LrAR
         ADD       NRCHGQTY,LrSH
         ADD       NRCHGAP,LrAP
         ADD       AT1,LrINC
         endif
.
         ADD       NRCHGAR,OAR
         ADD       NRCHGQTY,OSH
         ADD       NRCHGAP,OAP
         ADD       AT1,OINC
.
         ADD       NRCHGAR,GAR
         ADD       NRCHGQTY,GSH
         ADD       NRCHGAP,GAP
         ADD       AT1,GINC
.
         ADD       C1,LINES

.END PATCH 4.7 REPLACED LOGIC
         RETURN
...............................................................................

...............................................................................
EOJ2
         CALL      LTOT
         CALL      OTOT
         CALL      GH
         PRINT     *F
         SPLCLOSE
         release
                    call                GetWinVer
.         path      exist,"c:\windows"
.         if        over
.START PATCH 1.2 REPLACED LOGIC
.         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\nmrgcd.lst \\NINS2\laser2 "
.         else
.         EXECUTE   "c:\command.com /c copy g:\DATA\nmrgcd.lst \\NINS2\laser2 "
                    If                  (osflag = c1 | osflag = C5)
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"runprt.lst \\NINS2\laser2 "
         Execute   TASKNAME
.         else
                    Elseif              (osflag = c3 | osflag = C4)
         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"runprt.lst \\NINS2\laser2 "
                    Elseif              (osflag = c6 | osflag = c8 | osflag = c9)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"runprt.lst \\NINS2\laser2 "
.end patch 1.22
         EXECUTE   TASKNAME
                              endif
         BEEP
         CLOSE     EXPRINT
         call clean2
         alert caution,"Done Printing!",result,"Printing Complete"
         setprop    NrprtWinPrint,visible=c0
         call OrderSetMouseFree
         return
.         STOP             turned off stop to return to prog
...............................................................................
EXTERNAL
.==========================================================================
.turn on before live
         pack      str35,NTWKPATH1,"RUNPRT.LST"
         SPLOPEN   STR35,"Q"
.==========================================================================
         REPLACE   ZFILL,PMO
         REPLACE   ZFILL,PYR
         REPLACE   ZFILL,PCE
.turn on before live
.=============================================================
         pack      str35,NTWKPATH1,"EXPRINT"
         OPEN      EXPRINT,STR35
.=============================================================

          MOVE      C0 TO N5
  
...............................................................................
PRT11
             filepi    1;exprint
.START PATCH 4.7 REPLACED LOGIC
.           READKS    EXPRINT;LR,MLR,MLRCNT,LN,OWNER,TPI,QTY,AR,AP,CE,YR,MO
.         GOTO      EOJ22 IF OVER
.         ADD       C1 TO N5
..         DISPLAY   *P10:14,"EXTERNAL REP COUNT ",N5
.        MATCH     PYR,YR
.         GOTO      PRT11 IF NOT EQUAL
.
.         MATCH     PMO,MO
.         GOTO      PRT11 IF NOT EQUAL
.         MATCH     PCE,CE
.         GOTO      PRT11 IF NOT EQUAL
..............................
           READKS    EXPRINT;NRCHGLR,NRCHGMLR,NRCHGMLRCNT,NRCHGLN,NRCHGOWNER,NRCHGTPI,NRCHGQTY,NRCHGAR,NRCHGAP,NRCHGCE,NRCHGYR,NRCHGMO
         GOTO      EOJ22 IF OVER
         ADD       C1 TO N5
.         DISPLAY   *P10:14,"EXTERNAL REP COUNT ",N5
        MATCH     PYR,NRCHGYR
         GOTO      PRT11 IF NOT EQUAL

         MATCH     PMO,NRCHGMO
         GOTO      PRT11 IF NOT EQUAL
         MATCH     PCE,NRCHGCE
         GOTO      PRT11 IF NOT EQUAL
.END PATCH 4.7 REPLACED LOGIC
PRTDB
         type      powner
         if        equal
.START PATCH 4.7 REPLACED LOGIC
.         match     powner to owner
.         goto      prt11 if not equal
.         endif
.         MATCH     OWNER,HOWNER
.         GOTO      BREAK11 IF NOT EQUAL
.         MATCH     LN,HLN
         match     powner to NRCHGowner
         goto      prt11 if not equal
         endif
         MATCH     NRCHGOWNER,HOWNER
         GOTO      BREAK11 IF NOT EQUAL
         MATCH     NRCHGLN,HLN
.END PATCH 4.7 REPLACED LOGIC
         GOTO      BREAK22 IF NOT EQUAL
         CALL      DETAIL1
         GOTO      PRT11
. ............................................................................
BREAK11  COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LTOT1
.
* OWNER TOTALS COMMENTED OUT APRIL 1982 AS PER S.A.
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      OH1
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LDESC1
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LHEAD1
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      DETAIL1
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         GOTO      PRT11
. ............................................................................
BREAK22
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LTOT1
.
.
         CALL      OH1
         CALL      LDESC1
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
         CALL      LHEAD1
.
         COMPARE   "50",LINES
         CALL      NEWPAGE IF NOT LESS
          CALL      DETAIL1
.
         GOTO      PRT11
. ............................................................................
* LIST TOTALS
LTOT1
         PRINT *L,*L,*L,*3,"* LIST *":
               *L,*19,"--------",*81,"-------":
                   *L,*3,"*Exchange",*16,LeAR,*78,LeSH:
               *L,*19,"--------",*81,"-------":
                   *L,*3,"* Rental*",*16,LrAR,*78,LrSH:
               *L,*19,"--------",*81,"-------":
               *L,*19,"--------",*81,"-------":
                   *L,*3,"* TOTAL *",*16,LAR,*78,LSH
.
         MOVE      "0",LAR
         MOVE      "0",LSH
         MOVE      "0",LAP
         MOVE      "0",LINC
         MOVE      "0",LeAR
         MOVE      "0",LeSH
         MOVE      "0",LeAP
         MOVE      "0",LeINC
         MOVE      "0",LrAR
         MOVE      "0",LrSH
         MOVE      "0",LrAP
         MOVE      "0",LrINC
         ADD       "9",LINES
        RETURN
* OWNER TOTALS
OTOT1
         MOVE      "0",OAR
         MOVE      "0",OSH
         MOVE      "0",OAP
         MOVE      "0",OINC
        RETURN
.
* OWNER HEADING
OH1
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      OWNER,HOWNER
         MOVE      NRCHGOWNER,HOWNER
.END PATCH 4.7 REPLACED LOGIC
         MOVE      HOWNER TO NOWNFLD
         move      c1 to nownpath
         CALL      NOWNKEY
         PRINT     hp17ptch, *F,*1,"CONFIDENTIAL",*128,PMO,"/",PCE,PYR:
                 *L,*45,"*** NIN EXCHANGE CHARGES REPORT ***":
                             *L,*49,"=== ======== ======= ======":
                   *L,*L,*44,"OWNER: ",HOWNER,"  ",OWNOCPY
         MOVE      "5",LINES
        RETURN
...............................................................................
LDESC1    CLEAR     LKEY
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      LN TO HLN
         MOVE      NRCHGLN TO HLN
.END PATCH 4.7 REPLACED LOGIC
         MOVE      HLN,LKEY
         MOVE      LKEY TO NDATFLD
         CALL      NDATKEY
         PRINT     *L,*L,*1,"LIST: ",LKEY,"  ",OLSTNAME
         ADD       "2",LINES
         RETURN
...............................................................................
* DETAIL HEADING
LHEAD1
         PRINT     *16,"AR",*82,"QTY":
                   *L,*1,"LR",*15,"AMT",*26,"MAILER",*81,"SHIPPED":
                   *93,"INVOICE NO.":
                   *L,*1,"------",*13,"--------",*26,"------",*81,"-------":
                   *93,"----------"
         MOVE      "3",LINES
         RETURN
...............................................................................
DETAIL1
.START PATCH 4.7 REPLACED LOGIC
.         PACK      MKEY FROM MLR,MLRCNT
.         CALL      NMLRKEY
.         move      lr to nordfld
         PACK      MKEY FROM NRCHGMLR,NRCHGMLRCNT
         CALL      NMLRKEY
         move      NRCHGlr to nordfld
.END PATCH 4.7 REPLACED LOGIC
         rep       zfill in nordfld
         call      nordkey
         move      c1 to exchflag
         clear     exchrent
         reset     excodes
         SCAN      OELCODE IN EXCODES
         if        equal
         move      c1 to exchflag                  .true
         move      "Exch" to ExchRent          
         else
         move     c2 to exchflag                   .false its rental
         move     "rent" to exchRent
         endif
         MOVE      compnum TO nmtxfld
.         MOVE      mlr TO NMTXFLD
         CALL      NMTXKEY
         CLEAR     STAT501
         MOVE      C0 TO TAX501
         MOVE      MTXC501,TAX501
         BRANCH    TAX501 OF C0,C0,C3,C4,C5,C6
C0
         goto      detail1a
C3       MOVE      "501C-3" TO STAT501
         goto      detail1a
C4       MOVE      "501C-4" TO STAT501
         goto      detail1a
C5       MOVE      "501C-5" TO STAT501
         goto      detail1a
C6       MOVE      "501C-6" TO STAT501
DETAIL1A
.START PATCH 4.7 REPLACED LOGIC
.         MOVE      AR,AT1
.         MOVE      AP,AP1
.         SUB       AP1,AT1
.         PRINT     *1,LR,*10,AR,*22,MCOMP,B1,stat501,*79,QTY,*93,TPI,b5,exchrent
.         ADD       AR,LAR
.         ADD       QTY,LSH
.         ADD       AP,LAP
         MOVE      NRCHGAR,AT1
         MOVE      NRCHGAP,AP1
         SUB       AP1,AT1
         PRINT     *1,NRCHGLR,*10,NRCHGAR,*22,MCOMP,B1,stat501,*79,NRCHGQTY,*93,NRCHGTPI,b5,exchrent
         ADD       NRCHGAR,LAR
         ADD       NRCHGQTY,LSH
         ADD       NRCHGAP,LAP
.END PATCH 4.7 REPLACED LOGIC
         ADD       AT1,LINC
         ADD       C1,LINES
.
.         
         compare   c1 to exchflag
         if        equal
.START PATCH 4.7 REPLACED LOGIC
.         ADD       AR,LeAR
.         ADD       QTY,LeSH
.         ADD       AP,LeAP
         ADD       NRCHGAR,LeAR
         ADD       NRCHGQTY,LeSH
         ADD       NRCHGAP,LeAP
.END PATCH 4.7 REPLACED LOGIC
         ADD       AT1,LeINC
         endif
.
         compare   c2 to exchflag
         if        equal
.START PATCH 4.7 REPLACED LOGIC
.         ADD       AR,LrAR
.         ADD       QTY,LrSH
.         ADD       AP,LrAP
.         ADD       AT1,LrINC
.         endif
..
.         ADD       AR,OAR
.         ADD       QTY,OSH
.         ADD       AP,OAP
.         ADD       AT1,OINC
..............................
         ADD       NRCHGAR,LrAR
         ADD       NRCHGQTY,LrSH
         ADD       NRCHGAP,LrAP
         ADD       AT1,LrINC
         endif
.
         ADD       NRCHGAR,OAR
         ADD       NRCHGQTY,OSH
         ADD       NRCHGAP,OAP
         ADD       AT1,OINC
.END PATCH 4.7 REPLACED LOGIC
.
         RETURN
...............................................................................
NEWPAGE  PRINT     *F
         MOVE      "0",LINES
         RETURN
...............................................................................
EOJ22
         CALL      LTOT1
         CALL      OTOT1
         PRINT     *F
         SPLCLOSE
         BEEP
         CLOSE     EXPRINT
          call clean2
            alert caution,"Done Printing!",result,"Printing Complete"
          setprop   NrprtWinPrint,visible=c0
          call OrderSetMouseFree
.begin patch 4,75
          if        (extFlag = c2)
          shutdown 
          stop
          else
          return
          endif
.         return          
.end patch 4,75

.         STOP      turned off stop to return to prog

clean2
        setitem NrprtRadioExt,0,c1
        setitem NrprtEditOwn,0,""
        setitem NrprtEditDate,0,""
        setitem NrprtCheckIndex,0,c1
        setitem NrprtCheckSpool,0,c1
        return

.==============================================================================================
.============================================================================================
OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return
.==============================================================================================
         INCLUDE   NRCHGIO.INC
         INCLUDE   NOWNIO.inc
.patch4.5
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.patch4.5
         INCLUDE   NDATIO.inc
         INCLUDE   NORDIO.inc
         INCLUDE   NXRFIO.inc
         INCLUDE   NSHPIO.inc
         INCLUDE   NMTXIO.inc
         include   tinvio.inc
         INCLUDE   NDAT3IO.INC
         include   npasio.inc
         INCLUDE   COMLOGIC.inc
