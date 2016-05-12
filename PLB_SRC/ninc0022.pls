...............................................................................
.Revenue by a particular sales perspon
...............................................................................
.
PC       EQU       0
         INC       COMMON.inc
.
         INC       CONS.inc
         INCLUDE   CONSacct.inc
         INCLUDE   NINVDD.inc
         include   nacddd.inc
         include   nshpdd.inc
         include   compdd.inc
         include   cntdd.inc
.
         INC       NBILDD.inc
         INCLUDE   NOWNDD.INC
         INCLUDE   NDATDD.inc
         INCLUDE   NORDDD.INC
         INCLUDE   NDAT3DD.INC
.
         INC       GNXTDD.inc
         include   nmrgdd.inc
         INC       NJSTDD.inc
         include   ninvacddd.inc

shipsw   dim       1
mrgsw    dim       1
.
release    init       "3.0"         DLH updated to Denise was Jennifer Deere needs to be gone over with a fine tooth comb

.RELEASE         INIT      "1.1"        DLH 24MAR92    NINVXX, NMLRXX, NBILXX, NJSTXX
.
.RELEASE  INIT      "1.0"
...........................................
.CLOCK    FUNCTION
........................
DATE     DIM       10
SYSMO    DIM       2
SYSDY    DIM       2
SYSYR    DIM       2
SYSDAT   DIM       8                        SYSTEM DATE
.
.FILES.
...............................................................................
.
output3   file
OUTPUT   IFILE     KEYLEN=54,FIX=180
.
. ISAM KEY VARIABLES
.
HBILLKEY DIM       8    *FOR MATCH MLR/BILL-TO BREAK?
KEY54    DIM       54   *OUTPUT FILE KEY.
.
. WORK VARIABLES
.
.
.
ELEVEN   FORM      "11"
FIFTY1   FORM      "51"
PASS     FORM      1
ANS      DIM       1
ADJAR    FORM      7.2
ADJLR    FORM      7.2
ADJAP    FORM      7.2
ZERO     FORM      "0"
ONE      FORM      "1"
BROK     DIM       1
.
FORM7    FORM      7
TAB      FORM      "37"
CVTFLD   DIM       10             WORK FIELD USED FOR MP CONVERSION.
MPCHANGE INIT      "}0J1K2L3M4N5O6P7Q8R9"
MPCHARS  INIT      "}JKLMNOPQR"   VALID MINUS OVERPUNCH CHARACTERS
NUM10    FORM      10
COUNT    FORM      5
COUNT1   FORM      5
CO       FORM      1
DATEMASK DIM       10
.
CoName   DIM       45
str54    dim       54
ARTOT    FORM      10.2       55-67
APTOT    FORM      10.2       68-80
LRTOT    FORM      10.2       81-93
NINtot   form      10.2       94-106
QTYTOT   FORM      9         107-115
ADJARTOT FORM      10.2      116-128
ADJAPTOT FORM      10.2      129-141
ADJLRTOT FORM      10.2      142-154
adjNINtot form      10.2    155-167
FINLRTOT form      10.2     168-180

.===================================================================================
.Print Vars

incfletwo  file
prfile     pfile
PSLSFILE   PFILE
.===================================================================================
.Titles
Title1  init "DATE"
Title2  init "Accounts"
Title2a init "Receivable"
Title3  init "Accounts"
Title3a init "Payable"
Title4  init "LR Income"
Title5  init "Adjusted"
Title5a init "Accounts"
Title5b init "Receivable"
Title6  init "Adjusted"
Title6a init "Accounts"
Title6b init "Payable"
Title7 init "Adjusted LR"
Title7a init "Income"
Title8  init "Adjusted"
Title8a init "Total LR"
Title8b init "Income"

.Font
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
font7   font
font8   font
font9   font
font14   font
.============================================================
.Create Fonts
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
        create  font7,"Times New Roman",size=9
        create  font8,"Times New Roman",size=10
        create  font9,"Times New Roman",size=10,italic
        create  font14,"Times New Roman",size=11
.============================================================
.Printed # TOTALS
PARTOT    FORM      13
PAPTOT    FORM      13
PLRTOT    FORM      13
PADJARTOT FORM      13
PADJAPTOT FORM      13
PADJLRTOT FORM      13
.begin patch 1.8
PADJNINtot form     13
PFINLRTOT form      13
.================================================================================
.GRAND TOTALS
ARGRND     form        13
APGRND     form        13
LRGRND     form        13
ADJARGRND  form        13
ADJAPGRND  form        13
ADJLRGRND  form        13
ADJNINGRND form        13
FINLRGRND  form        13
.=================================================================================
.Pos of Header info
TitleH  form  9
TitleH2 form  9
.Headers for Sales Report
TitleH3 form  9
TitleH4 form  9
.===================================================================================
RowCount form 9         .count of number of entries per pg
PgCnt   form  9         .Page #
NEWPG   FORM 1         .COUNTER TO SHOW- IF NEW PAGE PUT TOTALS AT TOP OF PAGE
Copy     form 4        .# of copies
.====================================================================================
salesinclistview listview
White     color
Black     color
.Oh my this is all ancient
.Listviewobject for Sorting of Teams
Team1   init    "01-03-04-05-07-08-10-11-12-13-14-15-16-17-18"         .SALES TEAM
Team2   init    "            "         .Sue A. TEAM
Team3   init    "               "            .AL TEAM  MM(10) added 8/05
Team4   init    "02-06-19"            .LM
Team5   init    "00-00-00"            .Uncategorized
T1      init    "1"                                                     .SA
T2      init    "2"                                                     .JC
T3      init    "3"                                                     .SM
T4      init    "4"                                                     .SM
NAME1   init    "BROKERAGE TEAM"
NAME2   init    "SUE'S TEAM"
NAME3   init    "ANN'S TEAM"
NAME4   init    "LIST MANAGEMENT"
NAME5   init    "Uncategorized"
seller  dim      2
team    dim      1
Stamp   dim      10
holdtm dim       1
SLSARTOT    FORM    13
SLSFINLRTOT form    13
n13         form    13
n13a         form    13
saleskey    dim       46
.vrs for listview
LVARTOT   form    9.2
LVADJLRTOT  form    9.2
LVFINLRTOT  form    9.2
LVLRTOT     form    9.2
LVLRSTR     DIM     13
LVARSTR     DIM     13
LVADJLRSTR  DIM     13
LVFINLRSTR  DIM     13
str46       dim     46
...................................................
.
BEGINV   DIM       6
specl    dim       1
.
brker    dim       1
sellerv  dim       2
. .............................................................................
         TRAP      ABORT IF F5
         MOVE      "EXIT" TO PF5
         MOVE      "NINC0022" TO PROGRAM
         MOVE      "Sales Revenue For Year" TO STITLE
         MOVE      "NIN" TO COMPNME
         CALL      PAINT
         CALL      FUNCDISP
         MOVE      C2 TO NINVPATH        .SET ACCESS TO ISI BY INV#.
         MOVE      C1 TO NORDPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NMLRPATH
         move      c3 to nmlrlock
         move      c3 to nordlock
         move      c3 to NINVLOCK
.
         MOVE      "NINCLAST" TO GNXTFLD
         CALL      GNXTKEY
         MOVE      GNXTNUM TO NINVFLD
         MOVe      "000001",ninvfld  
         REP       ZFILL IN NINVFLD
.
         PACK      STR35,NTWKPATH1,"INCOMEDIH"
         PACK      STR45,NTWKPATH1,"INCOMEDIH"
         prepare   output3,"\\nins1\e\data\DIHinvs.dat"
         PREPARE   OUTPUT,STR35,STR45,"54","182",exclusive   .added 2 for salesperson
         XIF
         MOVE      ONE TO PASS
.
         clock     timestamp to timestamp
         unpack    timestamp into cc,sysyr,sysmo,sysdy
         pack      date from sysmo,slash,sysdy,slash,cc,sysyr
         pack      today from sysmo,slash,sysdy,slash,sysyr
         move      sysdy to dd
         move      sysmo to mm
         move      sysyr to yy
         call      datetest
         move      Comln2 to sysdy
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      DATe TO DATEMASK
         CALL      PAINT
.
         CREATE White=*WHITE
         CREATE BLACK=*BLACK
         CREATE salesinclistview=1:1:1:1,SORTHEADER=1,SORTORDER=2:
               APPEARANCE=1:
               BORDER=1,BGCOLOR=white:
               TABID=5,FULLROW=1:
               FONT=FONT9,FGCOLOR=black:
               visible=1,ENABLED=1
         salesinclistview.INSERTCOLUMN  USING "TEAM",30,0
         salesinclistview.INSERTCOLUMN  USING "Salesperson",30,1
         salesinclistview.INSERTCOLUMN  USING "Co",100,2
         salesinclistview.INSERTCOLUMN  USING "AR",100,3
         salesinclistview.INSERTCOLUMN  USING "LRTOT",100,4
         salesinclistview.INSERTCOLUMN  USING "ADJLRTOT",100,5
         salesinclistview.INSERTCOLUMN  USING "FINLRTOT",100,6
         ACTIVATE salesinclistview

DATEDIS
         KEYIN     *P10:10,*DV,DATEMASK," OK? ",*T05,STR1;
         CMATCH    no TO str1
         GOTO      BEGIN IF Not EQUAL
         KEYIN     *P10:10,*+,*jr,*zf,SYSMO,"/",*jr,*zf,SYSDY,"/",*jr,*zf,cc,*jr,*zf,SYSYR
         move      sysdy to dd
         move      sysmo to mm
         move      sysyr to yy
         call      datetest
         move      Comln2 to sysdy
.
         PACK      DATE FROM SYSMO,SLASH,SYSDY,SLASH,cc,SYSYR
         pack      today from sysmo,slash,sysdy,slash,sysyr
         REP       ZFILL,sysdy
         REP       ZFILL,sysMO
         PACK      SYSDAT FROM cc,sysyr,sysmo,sysdy
         MOVE      date TO DATEMASK
         GOTO      DATEDIS
.
BEGIN    CALL      NINVTST
.=================================================================
.give correct file definition when choosing different month
        MOVE      " " TO BROK
.=================================================================
.
INPUT    CALL      NINVKS
         GOTO      GETADJ IF OVER
.
         ADD       "1" TO COUNT
         DISPLAY   *P10:12,"NUMBER OF INVOICES Read: ",COUNT

         move      cc to str2
         match     str2 to invdtec
         GOTO      INPUT IF NOT EQUAL
         MATCH     SYSYR TO INVDTEY
         GOTO      INPUT IF NOT EQUAL
.
.
.
         MOVE      LRN TO NORDFLD
         CALL      NORDKEY
         RESET     RUNCODES
         SCAN      OLNUM IN RUNCODES
         goto      input if equal
         pack      seller,osales10,osales
         rep          Zfill,Seller  
         if           (Seller <> "05")
         goto      input
           endif
         MOVE      OLON TO NOWNFLD
         CALL      NOWNKEY
         PACK      MKEY FROM MLRN,z3
         CALL      NMLRKEY
         call      swap

         match     "0285",obrknum
         IF        EQUAL
         MOVE      C2 TO BROK
         GOTO      COMP
         ELSE
         match     "0266",obrknum
         IF        EQUAL
         MOVE      C3 TO BROK
         GOTO      COMP
         ELSE
         RESET     MCCTO
         match     "0171",obrknum
         IF        EQUAL
         MOVE      C1 TO BROK
         GOTO      COMP
         ELSE
         MOVE      " " TO BROK
         ENDIF
         ENDIF
         ENDIF
.

COMP
         MOVE      NordFLD to nmrgfld
         REP       ZFILL IN NMRGFLD
         MOVE      NordFLD to nshpfld
         REP       ZFILL IN NshpFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         move      no to shipsw
         call      wipecvars
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         CALL      nshpKEY
         if        not over
         move      yes to shipsw
         endif
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
               call           NinvAcdTst
               Call           NInvAcdRecLoad 
               CALL      COMPUTE
.
         GOTO      WRITEREC
.
SWAP
.
         MOVE      MCCTO TO BILNAME
          move      mcomp to CONAME
         MOVE      MADDR TO BILADDR
         MOVE      MCITY TO BILCITY
         MOVE      MSTATE TO BILSTATE
         MOVE      MZIP TO BILZIP
         RETURN
.
.
*............................................................................
GETADJ
         MOVE      "2" TO PASS
         DISPLAY   *P10:14,*EL,"NUMBER OF ADJUSTMENTS PROCESSED ",COUNT1
.
         CALL      NJSTSEQ
         GOTO      START IF OVER

.
         UNPACK    JSTDATE INTO invdtec,invdtey,INVDTEM,INVDTED
         MATCH     SYSYR TO INVDTEY
         GOTO      GETADJ IF NOT EQUAL

         MOVE      JSTLR TO NORDFLD
         CALL      NORDKEY
         pack      seller,osales10,osales
         rep          Zfill,Seller  
         if           (Seller <> "05")
         goto      getadj
         endif
         
.=============================================
         ADD       "1" TO COUNT1
         DISPLAY   *P10:14,*EL,"NUMBER OF ADJUSTMENTS PROCESSED ",COUNT1

         match     "0285",obrknum
         IF        EQUAL
                   MOVE      C2 TO BROK
          ELSE
          match     "0266",obrknum
                   IF        EQUAL
                          MOVE      C3 TO BROK
          ELSE
                          RESET     MCCTO
                    match     "0171",obrknum
                             IF        EQUAL
                                 MOVE      C1 TO BROK
                             ELSE
                                       MOVE      " " TO BROK
                             ENDIF
             ENDIF
         ENDIF
         MOVE      ZERO TO ADJAR
         move       jstar to adjar
.
         MOVE      ZERO TO ADJLR
          move      jstlrinc to adjlr
.
         MOVE      ZERO TO ADJAP
         move       jstap1 to adjap
.
.
         MOVE      ZERO TO CMPT92
          add       jstap2 to adjap
         PACK      MKEY FROM jstmlr,z3
         CALL      NMLRKEY

         CALL      swap
.
         MOVE      ZERO TO FORMAR
         MOVE      ZERO TO AP
         MOVE      ADJLR TO LRINC
.
         MOVE      ADJAR TO FORMAR
         MOVE      ADJAP TO AP
         MOVE      ADJLR TO LRINC
.
*......................................................................
.
WRITEREC MOVE      CO TO STR1
         MOVE      BROK TO ANS
         match     str35,CONAME
         if        equal
         move      omlrnum to CONAME
         endif

         reset team1
         reset team2
         reset team3
         reset team4
         scan  seller in team1
         if equal
               move t1 to team
               reset seller
               goto LV
         endif
         scan  seller in team2
         if equal
punt
               move t2 to team
               reset seller
               goto LV
         endif
         scan  seller in team3
         if equal
               move t3 to team
               reset seller
               goto LV
         endif
         scan  seller in team4
         if equal
               move t4 to team
               reset seller
               goto LV
         endif
         move c5 to team

LV
         pack saleskey with team,coname
         reset saleskey
         scan "1Special" in saleskey
          if equal
test2000
                        reset saleskey
          endif
          reset saleskey




               salesinclistview.GetItemCount giving result
               sub c1 from result
               compare c0,result
               goto add if less
SKIP
               for n9,"0",result
.TEAM
                    salesinclistview.GetItemText giving str1 using n9,0
.Salesperson
.         salesinclistview.GetItemText giving str2 using n9,1

.Company
                    salesinclistview.GetItemText giving str45 using n9,2
                    pack str46,str1,str45
.         pack str47,str2,str45
                    pack saleskey with team,coname
.         pack saleskey with seller,coname
                    match str46,saleskey
.         match str47,saleskey
                   if equal
.         call  GETFIGURES
.GETFIGURES
                    salesinclistview.GetItemText giving team using n9,0
                    salesinclistview.GetItemText giving sellerv using n9,1
                    salesinclistview.GetItemText giving str13 using n9,3
          clear LVARTOT
                    move str13 to LVARTOT
                    salesinclistview.GetItemText giving str13 using n9,4
          clear LVLRTOT
                    move str13 to LVLRTOT
                    salesinclistview.GetItemText giving str13 using n9,5
          clear LVADJLRTOT
                    move str13 to LVADJLRTOT
.salesinclistview.GetItemText giving str13 using n9,6
.move str13 to LVFINLRTOT
.testt
         if (jstmlr = "7216")
testing1
         reset jstmlr
         endif
.=============================================
                    branch pass,NINV,NADJ
NINV
                    ADD       FORMAR TO LVARTOT
                    move      LVARTOT to LVARSTR
                    ADD       LRINC TO  LVLRTOT
                    move      LVLRTOT to LVLRSTR
                    goto      LVINSERT
NADJ
                    move      LVARTOT to LVARSTR
                    move      LVLRTOT to LVLRSTR
                    ADD       LRINC TO  LVADJLRTOT
                    goto      LVINSERT
LVInsert
                     clear     LVFINLRTOT
                     Add       LVLRTOT TO LVFINLRTOT
                     Add       LVADJLRTOT TO LVFINLRTOT
                     move      LVADJLRTOT to LVADJLRSTR
                     move      LVFINLRTOT to LVFINLRSTR
                     salesinclistview.deleteitem giving n1 using n9
                     salesinclistview.InsertItem giving N8 using team
                     salesinclistview.SetItemText giving N1 using N8,sellerv,1
                     salesinclistview.SetItemText giving N1 using N8,str45,2
                     salesinclistview.SetItemText giving N1 using N8,LVARSTR,3
                     salesinclistview.SetItemText giving N1 using N8,LVLRSTR,4
                     salesinclistview.SetItemText giving N1 using N8,LVADJLRSTR,5
                     salesinclistview.SetItemText giving N1 using N8,LVFINLRSTR,6
.ClearVars
CLEARLVARS
                     clear LVARSTR
                     clear LVADJLRSTR
                     clear LVADJLRTOT
                     clear LVARTOT
                     clear LVFINLRSTR
                     clear LVFINLRTOT
                     clear TEAM
                     clear str13
                     clear saleskey
                     clear str46

.      call clearvars
                     goto  cont
                    endif
                  repeat

ADD
         if (jstmlr = "7216")
testing2
         reset jstmlr
         endif
.=============================================
                     salesinclistview.InsertItem giving N8 using team
                     salesinclistview.SetItemText giving N1 using N8,seller,1
                     salesinclistview.SetItemText giving N1 using N8,coname,2
                     clear LVARSTR
                     clear LVLRSTR
                     clear LVADJLRSTR
                     clear LVADJLRTOT
                     clear LVARTOT
                     clear LVLRTOT
                     clear LVFINLRSTR
                     clear LVFINLRTOT
                     clear TEAM
                     clear str13
                     clear saleskey
                     clear str46

                     branch pass,NINV1,NADJ1
NINV1
                     move      FORMAR TO LVARTOT
                     move      LVARTOT to LVARSTR
                     move      LRINC TO  LVLRTOT
                     move      LVLRTOT to LVLRSTR
                     goto      FIRSTINS
NADJ1
                     move      LRINC TO  LVADJLRTOT
                     move      LVADJLRTOT to LVADJLRSTR
                     goto      FIRSTINS
FIRSTINS
                     clear     LVFINLRTOT
                     Add       LVLRTOT TO LVFINLRTOT
                     Add       LVADJLRTOT TO LVFINLRTOT
                     move      LVFINLRTOT to LVFINLRSTR
                     salesinclistview.SetItemText giving N1 using N8,LVARSTR,3
                     salesinclistview.SetItemText giving N1 using N8,LVLRSTR,4
                     salesinclistview.SetItemText giving N1 using N8,LVADJLRSTR,5
                     salesinclistview.SetItemText giving N1 using N8,LVFINLRSTR,6

                     clear LVARSTR
                     clear LVADJLRSTR
                     clear LVADJLRTOT
                     clear LVARTOT
                     clear LVFINLRSTR
                     clear LVFINLRTOT
                     clear TEAM
                     clear str13
                     clear saleskey
                     clear str46
.         call clearvars
                     goto  cont
cont
          PACK      KEY54 WITH ans,CONAME,sysdat
         write    output3,seq;lrn,lrinc
         read     OUTPUT,KEY54;str54,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
                   ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,FINLRTOT,sellerv
         GOTO      WRITE IF OVER
         BRANCH    PASS OF ADDINV,ADDADJ
ADDINV
         ADD       FORMAR TO ARTOT
         ADD       AP TO APTOT
         ADD       LRINC TO LRTOT
         add       nininc to nintot
         MOVE      QTYbild TO N9
         ADD       N9 TO QTYTOT
         GOTO      UPDATE
ADDADJ
         ADD       FORMAR TO ADJARTOT
         ADD       AP TO ADJAPTOT
         ADD       LRINC TO ADJLRTOT
.
UPDATE
        move sellerv to seller
        clear      FINLRTOT
        Add        LRTOT TO FINLRTOT
        Add        ADJLRTOT TO FINLRTOT
          Update     OUTPUT;KEY54,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
                   ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,FINLRTOT,seller
.================================================

         call      wiper
         BRANCH    PASS OF INPUT,GETADJ
WRITE
         BRANCH    PASS OF ADDINV1,ADDADJ1
ADDINV1
         add       nininc to nintot
         MOVE      FORMAR TO ARTOT
         MOVE      AP TO APTOT
         MOVE      LRINC TO LRTOT
         MOVE      QTYbild TO N9
         MOVE      N9 TO QTYTOT
         GOTO      WRITE1
ADDADJ1
         MOVE      FORMAR TO ADJARTOT
         MOVE       AP TO ADJAPTOT
         MOVE       LRINC TO ADJLRTOT
.
WRITE1
        clear      FINLRTOT
        Add        LRTOT TO FINLRTOT
        Add        ADJLRTOT TO FINLRTOT
          WRITE     OUTPUT,KEY54;KEY54,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
                   ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,FINLRTOT,seller
.================================================
         call      wiper
         BRANCH    PASS OF INPUT,GETADJ

wiper
           move      c0 to NINtot
           move      c0 to ADJNINtot
         MOVE      C0 TO FORM7
         MOVE      C0 TO QTYTOT
         move      c0 to artot
         move      c0 to aptot
         move      c0 to lrtot
         move      c0 to adjartot
         move      c0 to adjaptot
         move      c0 to adjlrtot
         move      c0 to qtytot
         move      c0 to form7
         MOVE      BROK TO ANS
         return
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
         NORETURN                                POP THE STACK.
         BRANCH    PASS OF INPUT,GETADJ
CVTMP    REPLACE   MPCHANGE IN CVTFLD            CHANGE MP TO NUMBER.
         RESET     CVTFLD
         TYPE      CVTFLD                        VALID NUMERIC?
         GOTO      FORMERR IF NOT EQUAL          NO.
         MOVE      CVTFLD TO NUM10               MOVE INTO NUMERIC.
         MULTIPLY  "-1"   BY NUM10               CHANGE TO MINUS.
         MOVE      NUM10  TO CVTFLD              MOVE BACK TO DIM.
         RETURN
.
ABORT
         TRAPCLR   F5
         stop
GETFIGURES

.==========================================================================================
.Begin Print Program
START
         call GETWINVER

        Display    *P10:11,*EF,*cyan,"Printing in Process!!"
.===========================================================================
                              PRTOPEN prfile,"\\NINs2\Laser8","IncomeDIH.lst",noprint,spoolfile="c:\work\incomeDIH.lst"

.===========================================================================
.Set up columns
        move    "100",column
        move    "3400",column1
        move    "4300",column2
        move    "5300",column3
        move    "6300",column4
        move    "7300",column5
        move    "8300",column6
        move    "9300",column7
        move    "10300",column8
.Headers
        move    "5260",TitleH
        move    "9000",TitleH2
        move    "3800",TitleH3
        move    "6600",TitleH4
.===================================================================================


                        pack      taskname from NTWKPATH2,"sort32 ","\\nins1\e\data\incomeDIH.dat ","\\nins1\e\data\incomeDIH.srt ":
                         "/s (168,13,n,d)"
                       execute   taskname




        DISPLAY   *P10:14,*EF,"Opening....................... Income DIH!!!!!!!!!!!!!"
        Pause      C1
        OPEN      INCFLETWO,"\\nins1\e\data\incomeDIH.srt",SHARE
        move c1 to RowCount
        clear PgCnt
Page
        clear Rowcount
        add c1 to Pgcnt
        prtpage prfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                       *ORIENT=*LANDSCAPE;
        clear   row
        move    "300",row
        prtpage prfile;*pcolumn:row,*font=font12,"Confidential";
        prtpage prfile;*pTitleH:row,*ALIGNMENT=*Center,*font=font12,"Names in the News - Yearly Billing By Client ";
        prtpage prfile;*pTitleH2:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock timestamp,str8
        unpack str8,str2,yy,mm,dd
        clear str10
        pack  str10,mm,slash,dd,slash,str2,yy
        prtpage prfile;*font=font12,str10;
        add     eightlpi,row
        add     eightlpi,row
          add     eightlpi,row
        add     eightlpi,row
.Headers
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title5,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title6,*uloff,*boldoff;
       prtpage prfile;*pcolumn8:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title8,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title2,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title3,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title5a,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title6a,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title7,*uloff,*boldoff;
       prtpage prfile;*pcolumn8:row,*ALIGNMENT=*Right,*font=font14,*boldon,Title8a,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
       prtpage prfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Client",*ULOFF,*boldoff;
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title1,*uloff,*boldoff;
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title2a,*uloff,*boldoff;
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title3a,*uloff,*boldoff;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title4,*uloff,*boldoff;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title5b,*uloff,*boldoff;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title6b,*uloff,*boldoff;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title7a,*uloff,*boldoff;
       prtpage prfile;*pcolumn8:row,*ALIGNMENT=*Right,*font=font14,*boldon,*ulon,Title8b,*uloff,*boldoff;
       add     eightlpi,row
       add     eightlpi,row

.Loop to read file and splat on page

       loop
       read      INCFLETWO,SEQ;b1,str45,str8,ARTOT,APTOT,LRTOT,nintot,QTYTOT:
                 ADJARTOT,ADJAPTOT,ADJLRTOT,adjnintot,FINLRTOT,seller

       until over

                    reset team1
                    reset team2
                    reset team3
                    reset team4
                    scan  seller in team1
                    if equal
                    move t1 to team
                    reset seller
                    goto next
                    endif
                    scan  seller in team2
                    if equal
                    move t2 to team
                    reset seller
                    goto next
                    endif
                    scan  seller in team3
                    if equal
                    move t3 to team
                    reset seller
                    goto next
                    endif
                    scan  seller in team4
                    if equal
                    move t4 to team
                    reset seller
                    goto next
                    endif
                    move c5 to team
next
       ADD       C1 TO N5
       DISPLAY   *P10:14,*EF,"Records Processed ",N8
.Create Running Grand Totals
.AR
       MOVE ARTOT TO PARTOT
       add  PARTOT TO ARGRND
.AP
       MOVE APTOT TO PAPTOT
       ADD  PAPTOT TO APGRND
.LRINC
       MOVE LRTOT TO PLRTOT
       ADD PLRTOT TO LRGRND
.ADJ ACCT RECEIVE
       MOVE ADJARTOT TO PADJARTOT
       ADD PADJARTOT TO ADJARGRND
.ADJ ACCT PAY
       MOVE ADJAPTOT TO PADJAPTOT
       ADD PADJAPTOT TO ADJAPGRND
.ADJ LR INCOME
       MOVE ADJLRTOT TO PADJLRTOT
       ADD PADJLRTOT TO ADJLRGRND
.ADJ TOTAL LR INC
       move FINLRTOT TO PFINLRTOT
       ADD PFINLRTOT TO FINLRGRND
....NEED TO ADD BEFORE LIVE


.Individual Client Record
       prtpage prfile;*pcolumn:row,*font=font8,str45;
.Formats Date
       unpack str8,str2,yy,mm,dd
       clear str10
       pack  str10,mm,slash,dd,slash,str2,yy
                     move  str10 to stamp
       prtpage prfile;*pcolumn1:row,*ALIGNMENT=*Right,*font=font8,Str10;
       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*Right,*font=font8,PARTOT;
.AR
                      clear str13
        move partot to str13
       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*Right,*font=font8,PAPTOT;
       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*Right,*font=font8,PLRTOT;
       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*Right,*font=font8,PADJARTOT;
       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*Right,*font=font8,PADJAPTOT;
       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*Right,*font=font8,PADJLRTOT;
       prtpage prfile;*pcolumn8:row,*ALIGNMENT=*Right,*font=font8,PFINLRTOT;
.Adjusted Total LR Income
                      clear str13
        move pfinlrtot to str13
       add     eightlpi,row
.Counts how many clients per pg
Row
       add c1 to RowCount


Row2
.Reduced Rows on page to make room for Copyright label
.Max # of clients on a page is 46
       if (ROWCOUNT = "46")
           add     eightlpi,row
           add     eightlpi,row
           move "7750",row
           prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
           goto Page
       endif

       repeat

LastPage
.Write total on same page if less than 42 rows
       if (ROWCOUNT < "42")
                 goto totals
       else

.Added to correct page # and page label for next to last page
.========================================================================
           move "7750",row
           prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.========================================================================
                        move c1 to newpg
                        add  c1 to pgcnt
                    prtpage prfile;*NEWPAGE:
                           *UNITS=*HIENGLISH:
                                     *ORIENT=*LANDSCAPE;
                        goto Totals
       endif
Totals
.Grand Totals for clients
       if (newpg = c1)
                    move "1380",row
       else
                    add     eightlpi,row
                    prtpage prfile;*p3400:row,*pensize=10,*line=10300:row;
                add     "30",row
       endif

       prtpage prfile;*pcolumn:row,*ALIGNMENT=*Right,*font=font9,*boldon,*ulon,"Grand Total",*ALLOFF;

       prtpage prfile;*pcolumn2:row,*ALIGNMENT=*Right,*font=font8,ARGRND;

       prtpage prfile;*pcolumn3:row,*ALIGNMENT=*Right,*font=font8,APGRND;

       prtpage prfile;*pcolumn4:row,*ALIGNMENT=*Right,*font=font8,LRGRND;

       prtpage prfile;*pcolumn5:row,*ALIGNMENT=*Right,*font=font8,ADJARGRND;

       prtpage prfile;*pcolumn6:row,*ALIGNMENT=*Right,*font=font8,ADJAPGRND;

       prtpage prfile;*pcolumn7:row,*ALIGNMENT=*Right,*font=font8,ADJLRGRND;

       prtpage prfile;*pcolumn8:row,*ALIGNMENT=*Right,*font=font8,FINLRGRND;

          add     eightlpi,row
          add     eightlpi,row
.Total Records Printed
       prtpage prfile;*pcolumn:row,*ALIGNMENT=*Left,*font=font9,"Records Printed: ";
       move n8 to str8
       call trim using str8
       prtpage prfile;*ALIGNMENT=*Left,*font=font9,n8;
.Adds Footer for last page
       move "7750",row
       prtpage prfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage prfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
       PRTCLOSE prfile
.==========================================================================================

Retry
.=====================================================================
.Beg Patch2.2

        clear n3
        KEYIN     *P10:10,*EF,"How many Copies?",*jr,*zf,*T05,COPY;
        if (copy = c0)
                move c1 to copy
        else
          KEYIN     *P10:10,*DV,"How many Copies?",COPY," OK? ",*T05,STR1;
                CMATCH    no TO str1
                GOTO      Retry IF EQUAL
        endif
        if (osflag = "1" or osflag = "5" or osflag = "6")         NT,W2K,XP
          goto looper2
        endif
...........................................................
.==============================================================
Looper1
                    PRTPLAY "c:\work\incomeDIH.lst","\\NINs2\Laser8"
        goto EOJ
.================================================================
Looper2
                    PRTPLAY "c:\work\incomeDIH.lst","\\NINs2\Laser8"
.======================================================================


.=====================================================================
EOJ
        DISPLAY   *P10:10,*EF,"Job Finished.....Printing on Laser8!!!!!!!!!!!!!"
        PAUSE     c1
.        erase "c:\work\incomeDIH.lst"
.Report to break by sales by team
         move    YES to str1
         KEYIN     *P10:10,*ef,*rv,"Do you want the report by team"," OK? ",*T05,STR1;
         CMATCH    YES TO str1
                       goto finish if not equal
                                  PRTOPEN PSLSfile,"\\NINs2\Laser8","SalesIncomeDIH.lst",noprint,spoolfile="c:\work\salesincomeDIH.lst"
             salesinclistview.SortColumn using *Column=0,*Type=3,*Column1=6,*Type1=4
             move c1 to RowCount
             clear PgCnt
                        clear newpg
             move c0 to n9
             salesinclistview.GetItemCount giving result
             sub c1 from result
Headers
        clear Rowcount
        add c1 to Pgcnt
        prtpage pslsfile;*NEWPAGE:
                 *UNITS=*HIENGLISH:
                *ORIENT=*Portrait;
        clear   row
        move    "300",row
        prtpage pslsfile;*pcolumn:row,*font=font12,"Confidential";
        prtpage pslsfile;*pTitleH3:row,*ALIGNMENT=*Center,*font=font12,"Names in the News - Yearly Billing By Client ";
        prtpage pslsfile;*pTitleH4:row,*ALIGNMENT=*Left,*font=font12,"Date:";
        clock timestamp,str8
        unpack str8,str2,yy,mm,dd
        clear str10
        pack  str10,mm,slash,dd,slash,str2,yy
        prtpage pslsfile;*font=font12,str10;
        add     eightlpi,row
        add     "50",row
        add     eightlpi,row
        add     eightlpi,row
.Headers
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title8,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title2,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,Title8a,*uloff,*boldoff;
       add     eightlpi,row
       add     "20",row
       prtpage pslsfile;*pcolumn:row,*font=font12,*boldon,*ULON,"Client",*ULOFF,*boldoff;
       prtpage pslsfile;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=font14,*boldon,*ulon,Title1,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,*ulon,Title2a,*uloff,*boldoff;
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font14,*boldon,*ulon,Title8b,*uloff,*boldoff;
       add     eightlpi,row
       add     eightlpi,row
       add c1 to RowCount
Salesloop
       loop
         until (n9 > result)
                    salesinclistview.GetItemText giving str1 using n9,0
                    compare n9 to c0
                    if equal
                                        move str1 to holdtm
                    endif
                    if (str1 <> holdtm)
                                        move str1 to holdtm
                                        goto SalesLastPage
                    endif
                    move holdtm to n2
         load str16 with n2 to NAME1,NAME2,NAME3,NAME4,Name5
         prtpage pslsfile;*pTitleH3:485,*font=font12,*ll,*ALIGNMENT=*Center,str16;
         salesinclistview.GetItemText giving str45 using n9,2

.IF AR and Adj Total LR Income are both Zero skip
         clear str13
         CLEAR LVARTOT
         salesinclistview.GetItemText giving str13 using n9,3
         clear n13
         call trim using str13
         move str13 to LVARTOT
         add LVARTOT to n13

         clear LVFINLRTOT
         clear n13a
         salesinclistview.GetItemText giving str13 using n9,6
         call trim using str13
         move str13 to LVFINLRTOT
         add LVFINLRTOT to n13a
         if ((n13 = C0)&(n13a = C0))
               add c1 to n9
               goto salesloop
         endif


.==================================================================
.Client
         prtpage pslsfile;*pcolumn:row,*font=font8,*ll,*ALIGNMENT=*Left,str45;
.Datestamp
         prtpage pslsfile;*pcolumn1:row,*font=font8,*ll,*ALIGNMENT=*Left,stamp;
         clear str9
.AR
         clear str13
         CLEAR LVARTOT
         salesinclistview.GetItemText giving str13 using n9,3
         clear n13
         call trim using str13
         move str13 to LVARTOT
         add LVARTOT to n13
.
         add n13 to SLSARTOT
         prtpage pslsfile;*pcolumn3:row,*font=font8,*ll,*ALIGNMENT=*RIGHT,n13;
.Adj Total LR Income
         clear LVFINLRTOT
         clear str13
         salesinclistview.GetItemText giving str13 using n9,6
         clear n13
         call trim using str13
         move str13 to LVFINLRTOT
         add LVFINLRTOT to n13
.move str13 to n13
         add n13 to SLSFINLRTOT
         prtpage pslsfile;*pcolumn4:row,*font=font8,*ll,*ALIGNMENT=*RIGHT,n13;
         add eightlpi,row
         add "50",row
         add c1 to RowCount
         add c1 to n9
.Reduced Rows on page to make room for Copyright label
.Max # of clients on a page is 46
           if (ROWCOUNT = "47")
           add     eightlpi,row
           add     eightlpi,row
           move "10200",row
           prtpage pslsfile;*pcolumn:row,*font=font8,*ALIGNMENT=*Left,"Page# ";
           prtpage pslsfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
           goto Headers
           endif
           repeat
SalesLastPage
.Write total on same page if less than 46 rows
       if (ROWCOUNT < "46")
                 goto Salestotals
       else

.Added to correct page # and page label for next to last page
.========================================================================
           move "10200",row
           prtpage pslsfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
           prtpage pslsfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
.========================================================================
                  move c1 to newpg
                  add  c1 to pgcnt
                    prtpage pslsfile;*NEWPAGE:
                                         *UNITS=*HIENGLISH:
                                       *ORIENT=*PORTRAIT;
                  goto SalesTotals
       endif
SalesTotals
.Grand Totals for clients
       if (newpg = c1)
                    move "1380",row
       else
                          add     eightlpi,row
            prtpage pslsfile;*p3400:row,*pensize=10,*line=10300:row;
            add     "30",row
       endif
       prtpage pslsfile;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font9,*boldon,*ulon,"Grand Total",*ALLOFF;
                     move slsartot to str13
                     call trim using str13
       prtpage pslsfile;*pcolumn3:row,*ALIGNMENT=*RIGHT,*font=font8,SLSARTOT;
                     move slsfinlrtot to str13
                     call trim using str13
       prtpage pslsfile;*pcolumn4:row,*ALIGNMENT=*RIGHT,*font=font8,SLSFINLRTOT;
       add     eightlpi,row
       add     eightlpi,row
.Total Records Printed
.       prtpage pslsfile;*pcolumn:row,*ALIGNMENT=*Left,*font=font9,"Records Printed: ";
.       move n8 to str8
.       call trim using str8
.       prtpage pslsfile;*ALIGNMENT=*Left,*font=font9,n8;
.Adds Footer for last page
       move "10200",row
       prtpage pslsfile;*pcolumn:row,*font=font12,*ALIGNMENT=*Left,"Page# ";
       prtpage pslsfile;*font=font12,*ALIGNMENT=*Left,PgCnt;
                     clear slsfinlrtot
                     clear slsartot

       if (n9 > result)
                      prtclose pslsfile
                      PRTPLAY "c:\work\salesincomeDIH.lst","\\NINs2\Laser8"
                      PRTPLAY "c:\work\salesincomeDIH.lst","\\NINs2\Laser8"
                     else
                                         clear newpg
                     clear pgcnt
                      goto Headers
                     endif
       DISPLAY   *P10:10,*EF,"Sales Job Finished.....Printing on Laser8!!!!!!!!!!!!!"
       PAUSE     c1
       erase "c:\work\salesincomeDIH.lst"







.EndPatch2.6
finish

         STOP
.
         INCLUDE   NORDIO.inc
.Patch2.9
                              include   compio.inc
                              include   cntio.inc
.         INCLUDE   NMLRIO.inc
.Patch2.9
                              Include   ninvacdio.inc
         INCLUDE   NINVIO.inc
         INCLUDE   NBILIO.inc
         INCLUDE   NJSTIO.inc
         INCLUDE   GNXTIO.INC
.begin patch 1.8
         INCLUDE   COMPUTE.inc
         include   nacdio.inc
         include   nownio.inc
         include   nshpio.inc
.end patch 1.8
         INCLUDE  NDAT3IO.INC
         inc       nmrgio.inc
         INCLUDE   COMLOGIC.inc

