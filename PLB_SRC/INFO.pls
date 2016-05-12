...........................................................
. Program:      Info.PLS
. Function:     OrderInfo External Subroutines
. Author:       Andrew Harkins
. Orig. Date:   January 12, 2001
.Release  1.10 added INFo:DisplayCompany
.relDate  01 May 2008
. Release:      1.0
. Notes:        This separately compiled program contains logic for using the OrderInfo Screen
.               (Nord001h.plf) to display Information on:  Mailer, Broker, List, Owner, ShipTo,
.               XSTAT.  Code has been dumped into a separately compiled .PLC in order to allow
.               other applications the use of this logic without having to copy routines into
.               new source code each time it is desired.
.
.               A program which calls these subroutines externally might also use Nord001h.plf for other
.               functions, independent of these subroutines.
.
.               In order to use these subroutines, calling program must declare them as External
.               Routines.  For example:
.
.EXTERNAL ROUTINES FROM INFO.PLC
.OrderLoadForm external "INFO;LoadForm"
.OrderDisplayMailer external "INFO;DisplayMailer"
.OrderDisplayBroker external "INFO;DisplayBroker"
.OrderDisplayShipto external "INFO;DisplayShipto"
.OrderDisplayOwner external "INFO;DisplayOwner"
.begin patch 2.9
.OrderDisplayOCnt external "INFO;DisplayOCnt"
.end patch 2.9
.OrderDisplayList external "INFO;DisplayList"
.OrderDisplayXSTAT external "INFO;DisplayXstat"
.
.               Program with above declarations might then have following line of code to call a specific
.               subroutine:
.
.               call          OrderDisplayList using Nord0001,Order1EditList,N4,MouseForm,T1,L1
.
.               Parameters MUST match datatypes to those listed in this program.
.
.               External declarations do not have to include all subroutines.  A program may
.               only have need of 1 or 2 of these subroutines.  Those would be the only ones
.               requiring external declaration.
.
.               Any changes made to this source code might alter results for any program already
.               utilizing these routines!!!!!
.
.               Any changes made to this source code requiring compilation prior to activation.
.
.               This program uses an external subroutine found in Nordtest.plc!!
...........................................................
PC      EQU     0

        include common.inc
        include cons.inc
        include norddd.inc
.START PATCH 1.9 REPLACED LOGIC
.        include nmlrdd.inc
.        include nmlr2dd.inc
.        include nbrkdd.inc
.        include nbrk2dd.inc
          include   compdd.inc
          include   cntdd.inc
.END PATCH 1.9 REPLACED LOGIC
        include nxrfdd.inc
        include ndatdd.inc
        include nrtndd.inc
        include nowndd.inc
        include nmdldd.inc
        include nxngdd.inc
        include nxchdd.inc
        include nusedd.inc
        include nloldd.inc
        include nofrdd.inc
.PATCH    1.6
          include   compnotesdd.inc
.END PATCH  1.6
.START PATCH 1.6 ADDED LOGIC
          include   nsel2dd.inc
          include   ntxtdd.inc
.END PATCH 1.6 ADDED LOGIC
.START PATCH 1.7 ADDED LOGIC
          include   nmrgdd.inc
.END PATCH 1.7 ADDED LOGIC
.PATCH 2.0
          include   nseldd.inc
.END PATCH 2.0
.START PATCH 2.3 ADDED LOGIC
          include   nmlrxydd.inc
.END PATCH 2.3 ADDED LOGIC
          Include   NDatCntDD.inc
.>START PATCH 2.4 ADDED LOGIC
.START PATCH 2.7 REMOVED LOGIC
.         include   nfuldd.inc
.END PATCH 2.7 REMOVED LOGIC
.>END PATCH 2.4 ADDED LOGIC

.EXTERNAL ROUTINES
GetHistory external "NORDtest;OrderGetHistory"
OrderLoadUniverseInSpecial external "NORDtest;OrderLoadUniverseInSpecial"
.START PATCH 1.4 ADDED LOGIC
OrderLoadOmit external "NORDtest;OrderLoadOmit"
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.5 ADDED LOGIC
OrderLoadListNumber external "NORDtest;OrderLoadListNumber"
.END PATCH 1.5 ADDED LOGIC
release   init      "2.9"     11OCT2006 DLH       DisplayOcnt added as part of MIN COnversion
.release  init      "2.8"     11OCT2006 DMB       Fulfillment information is now stored on the order record.  We will be using this field to do display fulfillment info.
.release  init      "2.7"     20JUNE2006          DMS       conversion from nfuldd.inc to compdd.inc
.release init    "2.6"   17AUG2005       ASH     Selected/Sorted records for Omit Display based on Order Date
.release init    "2.5"   18MAY2005       ASH     LISTMLR Conversion
.release init    "2.4"    26MAY2005       DMB     Work Order 346 - Added Dispay for fulfillment company
.release init    "2.3"   12NOV2004       ASH     Work Order 327 - Added Mailer Cross Ref. system
.release init    "2.2"   01OCT2004       ASH     Work Order 494 - Added REVDATE to Assoc. Lists
.release init    "2.1"   27JUL2004       ASH     Removed clearing of body in email messages so that Signature appears.
.release init    "2.0"   23JUN2004       DMB     Added code to replase defunct datacard exch only stat to use select exch only stat
.release init    "1.9"   26MAY2004       ASH     MAILER CONVERSION
.                                                 6/3/2004 ASH Added other Credit Statuses
.release init    "1.8"   12MAY2004       ASH     Added Email Addresses
.release init    "1.7"   11MAY2004       ASH     Added Merge Net to Continuation Info Box
.release init    "1.6"   26JAN2004       ASH     DATACARD CONVERSION
.release init    "1.5"   20OCT2003       ASH     ADDED LOGIC FOR REVERSE SEARCHES - CORRESPONDS TO PATCH 3.72.3 IN NORDTEST
.release init    "1.4"   16JUN2003       ASH     ADDED DISPLAYOMIT
.release init    "1.3"   02APR2002       ASH     REMOVE EDIT TEXT BOX REFERENCE FROM DISPLAYXSTAT
.release init    "1.2"   14MAR2002       ASH     Idea of the Month - Add Mailer Cross Reference
.release init    "1.1"   08MAR2001       ASH     Added DisplayMessage Routine
.                                               Removed dynamic creation/destruction, adding objects to form
.release init    "1.0"   09JAN2001       ASH     Initial release

.START PATCH 1.1 REMOVED LOGIC
.InfoListView    ListView
.InfoEditText    EditText
.END PATCH 1.1 REMOVED LOGIC

.START PATCH 1.8 ADDED LOGIC
Note    automation
Mes     automation      class="Outlook.Application"
.END PATCH 1.8 ADDED LOGIC
.Pointers to Objects
WindPtr         Window  ^
EditPtr         EditText ^
EditPtr1        EditText ^
DimPtr  Dim     ^
DimPtr1 Dim     ^
DimPtr2 Dim     ^
DimPtr3 Dim     ^
DimPtr4 Dim     ^
DimPtr5 Dim     ^
FrmPtr  Form    ^
FrmPtr1 Form    ^
FrmPtr2 Form    ^
FrmPtr3 Form    ^
FrmPtr4 Form    ^
.
.
InfoString dim  47
ScrRight form   4
ScrBottom form  4
MouseForm form  10
FarRight form   4
FarBottom form  4
T1      form    4
L1      form    4

area    dim     3
str45a  dim     45
EFLAG   dim     1            mlr switch CONSTANT EXCHANGE
CALCPER FORM    7.4
AKey1A  init    "01L"
AKey2A  init    "02L"
.START PATCH 1.7 ADDED LOGIC
PERCENT   form      4.2
nordin    form      8
nordout   form      8
.END PATCH 1.7 ADDED LOGIC

RED     COLOR
BLACK   COLOR
.START PATCH 1.8 ADDED LOGIC
BLUE      COLOR
.END PATCH 1.8 ADDED LOGIC
.START PATCH        1.7       ADDED LOGIC
          SHUTDOWN  // THIS IS A CALLED PROGRAM - U1 ERROR IF RUN ALONE
.END PATCH          1.7       ADDED LOGIC
LoadForm Routine
inf     plform  Nord001h        .OrderInfo
        formload inf
        create  RED=*RED
        create  black=*black
.START PATCH 1.8 ADDED LOGIC
          create    blue=*blue
.END PATCH 1.8 ADDED LOGIC
        return

DisplayMailer Routine WindPtr,EditPtr,EditPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Mailer #
.EditPtr1 = EditText Box which holds Mailer Contact #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseForm - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,MNUM
                getitem EditPtr1,0,MCONTCT
                pack    MKEY,MNUM,MCONTCT
                if (MKEY <> "")
                        move    C1,NMLRPATH
                        move    "Info,MLR-NMLRKEY",Location
                        pack    KeyLocation,"Key: ",MKEY
                        call    NMLRKEY
                        if not over
                                if (N4 = 8)     .Left Mouse Button
                                        pack    InfoString,MCOMP
                                        setitem OrderInfoStatText1,0,InfoString
                                        setitem OrderInfoStatText2,0,MNAME
                                        setitem OrderInfoStatText3,0,MADDR
                                        call    TRIM using MCITY
                                        if (MCITY <> "")
                                                pack    InfoString,MCITY,", ",MSTATE,B1,MZIP
                                        else
                                                pack    InfoString,MSTATE,B1,MZIP
                                        endif
                                        setitem OrderInfoStatText4,0,InfoString
                                        cmatch  YES,MCOPIES
                                        if equal
                                                setprop OrderInfoStatText1,fgcolor=red
                                        else
                                                setprop OrderInfoStatText1,fgcolor=black
                                        endif
                                        clear   InfoString
                                        call    TRIM using MCCTO
                                        count   N1,MCCTO
                                        if (N1 > C0)
                                                append  "CC: ",InfoString
                                                append  MCCTO,InfoString
                                                append  B2,InfoString
                                        endif
                                        setprop OrderInfoStatText5,fgcolor=red
                                        if (MSTAT = "*")
                                                append  "Credit On Hold",InfoString
                                        elseif (MSTAT = "N")
                                                append  "New - Credit On Hold",InfoString
                                        elseif (MSTAT = "I")
                                                append  "Inactive-Credit On Hold",InfoString
                                        elseif (MSTAT = "B")
                                                append  "Credit Risk",InfoString
                                        elseif (MSTAT = "P")
                                                append  "Political-Credit On Hold",InfoString
                                        elseif (MSTAT = "W")
                                                append  "Warning - See Notes!",InfoString
.                                                pack    NMLR2FLD,MKEY
.                                                move    "Info,MLR-NMLR2KEY",Location
.                                                pack    KeyLocation,"Key: ",NMLR2FLD
.                                                call    NMLR2KEY
.                                                call    Trim using MLR2NOTES
.                                                setprop OrderInfo,height=200
.                                                create  OrderInfo;InfoEditText=100:199:1:249,border=1,style=1,wordwrap=1,readonly=1
.                                                activate InfoEditText
.                                                setitem InfoEditText,0,MLR2NOTES
.START PATCH 1.9 ADDED LOGIC
                                        elseif (MSTAT = "M")
                                                append  "Must Prepay!",InfoString
                                        elseif (MSTAT = "9")
                                                append  "On hold until over 90's paid!",InfoString
                                        elseif (MSTAT = "G")
                                                append  "Guarantees are always required!",InfoString
                                        elseif (MSTAT = "g")
                                                append  "Guarantees no longer accepted!",InfoString
.END PATCH 1.9 ADDED LOGIC
                                        else
                                                append  "Credit OK",InfoString
                                                setprop OrderInfoStatText5,fgcolor=black
                                        endif
                                        reset   InfoString
                                        setitem OrderInfoStatText5,0,InfoString
.START PATCH 1.8 ADDED LOGIC
                                                  setitem OrderInfoStatText6,0,""
.END PATCH 1.8 ADDED LOGIC
.
.START PATCH 1.9 REPLACED LOGIC
.                                        pack    NMLR2FLD,MKEY
.                                        move    "Info,MLR-NMLR2KEY",Location
.                                        pack    KeyLocation,"Key: ",NMLR2FLD
.                                        call    NMLR2KEY
.                                        if not over
.                                                call    Trim using MLR2NOTES
.         ALERT     NOTE,"HERE",RESULT
                                                  move      COMPNUM to COMPNOTEFLD
                                                  move      "O.LoadMailer-COMPNOTEKEY",Location
                                                  pack      KeyLocation,"Key: ",COMPNOTEFLD
                                                  call      COMPNOTEKEY
                                                  if not over
                                                            call    Trim using COMPNOTES
.END PATCH 1.9 REPLACED LOGIC
                                                setprop OrderInfo,height=200
.START PATCH 1.1 REPLACED LOGIC
.                                                create  OrderInfo;InfoEditText=100:199:1:249,border=1,style=1,wordwrap=1,readonly=1
.                                                activate InfoEditText
                                                setprop InfoEditText,top=100,height=98
.END PATCH 1.1 REPLACED LOGIC
.START PATCH 1.9 REPLACED LOGIC
.                                                setitem InfoEditText,0,MLR2NOTES
                                                setitem InfoEditText,0,COMPNOTES
.END PATCH 1.9 REPLACED LOGIC
                                        endif
.Position it correctly
                                        call    SetInfoScreen using WindPtr
                                        setprop OrderInfo,title="Mailer Information"
                                        setprop OrderInfo,visible=1
                                else           .if (N4 = 16)        .Right Mouse Button
                                        move    C2 to NXRFPATH
.START PATCH 2.5 REPLACED LOGIC
.                                        move    MNUM,NXRFFLD2
                                        move    COMPNUM,NXRFFLD2
.END PATCH 2.5 REPLACED LOGIC
                                        move    "Info,MLR-NXRFKEY",Location
                                        pack    KeyLocation,"Key: ",NXRFFLD2
                                        call    NXRFKEY
                                        if over
                                                move    "000000",nxrflist
                                        endif
                                        move    MNUM,str4
                                        clear   result
                                        move    NXRFLIST,NDATFLD
                                        move    C1,NDATPATH
                                        move    "Info,MLR-NDATTST",Location
                                        pack    KeyLocation,"Key: ",NDATFLD
                                        call    NDATTST
                                        if not over
.START PATCH 1.1 REPLACED LOGIC
.                                                create  OrderInfo;InfoListView=1:99:1:249,fullrow=1
                                                setprop InfoListView,height=98
.Next line added per SA request - 10/19/2000
.                                                getprop WindPtr,title=str55
.                                                scan    "NIN Order Program",str55
.                                                if equal
.                                                eventreg InfoListView,6,LoadUniverseInSpecial
.                                                eventreg InfoListView,10,InfoListViewKeyPress,CHAR=str1,RESULT=N9
.                                                endif
.                                                activate InfoListView
.END PATCH 1.1 REPLACED LOGIC
                                                InfoListView.DeleteAllContents
                                                InfoListView.InsertColumn using "List",60,1
                                                InfoListView.InsertColumn using "Name",140,2
                                                InfoListView.InsertColumn using "Universe",70,3
                                                InfoListView.InsertColumn using "Withdrawn",70,4
.START PATCH 2.2 ADDED LOGIC
                                                InfoListView.InsertColumn using "Rev. Date",70,5
.END PATCH 2.2 ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
                                                            setprop   InfoListView,sortorder=1
.END PATCH 1.4 ADDED LOGIC
                                                loop
                                                        move    NXRFLIST,NDATFLD
                                                        move    C1,NDATPATH
                                                        move    "Info,MLR-NDATKEY",Location
                                                        pack    KeyLocation,"Key: ",NDATFLD
                                                        call    NDATKEY
                                                        if not over
                                                                InfoListView.InsertItem giving N9 using LSTNUM
                                                                InfoListView.SetItemText using N9,OLSTNAME,1
                                                                move    UNIVERSE,str10
                                                                call    FormatNumeric using str10,str13
                                                                InfoListView.SetColumnFormat using 2,1
                                                                InfoListView.SetItemText using N9,str13,2
                                                                if (STATUS = "W" | STATUS = "T")
                                                                        move    "YES",str3
                                                                else
                                                                        clear   str3
                                                                endif
                                                                InfoListView.SetItemText using N9,str3,3
.START PATCH 2.2 ADDED LOGIC
                                                                                unpack    REVDATE,CC,YY,MM,DD
                                                                                pack      str10,MM,SLASH,DD,SLASH,CC,YY
                                                                    InfoListView.SetItemText using N9,str10,4
.END PATCH 2.2 ADDED LOGIC
                                                        endif
                                                        call    NXRFKS
                                                        until over
.START PATCH 2.5 REPLACED LOGIC
.                                                        match  MNUM,NXRFMLR
                                                        match  COMPNUM,NXRFMLR
.END PATCH 2.5 REPLACED LOGIC
                                                        until not equal
                                                repeat
.Put focus on 1rst item
                                                InfoListView.SetItemState giving N9 using 0,2,2
.Position it correctly
                                                call    SetInfoScreen using WindPtr
                                                setprop OrderInfo,title="Mailer Information, Associated Lists"
                                                setprop OrderInfo,visible=1
                                                setfocus InfoListView
                                        endif
                                endif
                        endif
                endif
        endif
        return

LoadUniverseInSpecial
        InfoListView.GetNextItem giving N9 using C2
        InfoListView.GetItemText giving str3 using N9,3
        if (str3 <> "YES")
                InfoListView.GetItemText giving str13 using N9,2
                call    OrderLoadUniverseInSpecial using str13
        endif
        return

.START PATCH 1.4 ADDED LOGIC
LoadOmit
          InfoListView.GetNextItem giving N9 using C2
.START PATCH 2.6 REPLACED LOGIC
.         InfoListView.GetItemText giving str6 using N9,0
.         InfoListView.GetItemText giving str10 using N9,1
.         InfoListView.GetItemText giving str11 using N9,2
          InfoListView.GetItemText giving str6 using N9,1
          InfoListView.GetItemText giving str10 using N9,2
          InfoListView.GetItemText giving str11 using N9,3
.END PATCH 2.6 REPLACED LOGIC
.dh test aug 2010
          call    OrderLoadOmit using str6,str10,str11
          return
.END PATCH 1.4 ADDED LOGIC

.START PATCH 1.5 ADDED LOGIC
LoadListNumber
        InfoListView.GetNextItem giving N9 using C2
        InfoListView.GetItemText giving str6 using N9,0
          call      OrderLoadListNumber using str6
          return
.END PATCH 1.5 ADDED LOGIC

InfoListViewKeyPress
        if (N9 = 120)     .F9 Key closes Net Screen
                setprop OrderInfo,visible=0
        elseif (N9 = 0 AND str1 = B1)
.START PATCH 1.4 REPLACED LOGIC
.                   call      LoadUniverseInSpecial
                    getprop OrderInfo,title=str55
                    scan    "Omit Information, 5 Past Orders",str55
                    if equal
                              call      LoadOmit
.START PATCH 1.5 ADDED LOGIC
                    elseif (str55 = "NINCA Order Search")
                              call      LoadListNumber
                    elseif (str55 = "NIN Order Search")
                              call      LoadListNumber
.END PATCH 1.5 ADDED LOGIC
                    else
                              call      LoadUniverseInSpecial
                    endif
.END PATCH 1.4 REPLACED LOGIC
        endif
        return

DisplayBroker Routine WindPtr,EditPtr,EditPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Broker #
.EditPtr1 = EditText Box which holds Broker Contact #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,BRKNUM
                getitem EditPtr1,0,BRKCNT
.DH 18 Aug 2010
                    Rep       Zfill,brknum
                    rep       Zfill,brkcnt
                    Packkey   NBRKFLD,BRKNUM,BRKCNT
.                pack    NBRKFLD,BRKNUM,BRKCNT
.DH 18 Aug 2010
                if (NBRKFLD <> "")
                        move    C1,NBRKPATH
                        move    "Info,Broker-NBRKKEY",Location
                        pack    KeyLocation,"Key: ",NBRKFLD
                        call    NBRKKEY
                        if not over
                                setitem OrderInfoStatText1,0,BRCOMP
                                setitem OrderInfoStatText2,0,BRADDR
                                call    TRIM using BRCITY
                                if (BRCITY <> "")
                                        pack    InfoString,BRCITY,", ",BRSTATE,B1,BRZIP
                                else
                                        pack    InfoString,BRSTATE,B1,BRZIP
                                endif
                                setitem OrderInfoStatText3,0,InfoString
                                clear   InfoString
                                call    Trim using BRTELE
                                if (BRTELE <> "" AND BRTELE <> "0000000000")
                                        unpack  BRTELE,area,str3,str4
                                        pack    InfoString,"(",area,")",str3,DASH,str4
                                endif
                                setitem OrderInfoStatText4,0,InfoString
                                setprop OrderInfoStatText5,fgcolor=red
                                if (BRCREDIT = "*")
                                        move    "Credit On Hold",InfoString
                                elseif (BRCREDIT = "N")
                                        move    "New - Credit On Hold",InfoString
                                elseif (BRCREDIT = "I")
                                        move    "Inactive - Credit On Hold",InfoString
                                elseif (BRCREDIT = "B")
                                        move    "Credit Risk",InfoString
                                elseif (BRCREDIT = "P")
                                        move    "Political - Credit On Hold",InfoString
                                elseif (BRCREDIT = "W")
                                        move    "Warning - See Notes!",InfoString
.                                        pack    NBRK2FLD,BRKNUM
.                                        move    "Info,Broker-NBRK2KEY",Location
.                                        pack    KeyLocation,"Key: ",NBRK2FLD
.                                        call    NBRK2KEY
.                                        call    Trim using BRK2NOTES
.                                        setprop OrderInfo,height=200
.                                        create  OrderInfo;InfoEditText=100:199:1:249,border=1,style=1,wordwrap=1,readonly=1
.                                        activate InfoEditText
.                                        setitem InfoEditText,0,BRK2NOTES
.START PATCH 1.9 ADDED LOGIC
                                elseif (BRCREDIT = "M")
                                        move      "Must Prepay!",InfoString
                                elseif (BRCREDIT = "9")
                                        move      "On hold until over 90's paid!",InfoString
                                elseif (BRCREDIT = "G")
                                        move      "Guarantees are always required!",InfoString
                                elseif (BRCREDIT = "g")
                                        move      "Guarantees no longer accepted!",InfoString
.END PATCH 1.9 ADDED LOGIC
                                else
                                        move    "Credit OK",InfoString
                                        setprop OrderInfoStatText5,fgcolor=black
                                endif
                                setitem OrderInfoStatText5,0,InfoString
.START PATCH 1.8 ADDED LOGIC
                                        call      Trim using CNCTEMAIL
                                        if (CNCTEMAIL = "")
                                                  setitem OrderInfoStatText6,0,""
                                        else
                                                  eventreg OrderInfoStatText6,4,OrderInfoStatText6Click,RESULT=N9
                                                  setitem OrderInfoStatText6,0,CNCTEMAIL
                                                  setprop OrderInfoStatText6,fgcolor=blue
                                        endif
.END PATCH 1.8 ADDED LOGIC
.patch1.6
.                                       pack    NBRK2FLD,BRKNUM
.                                       move    "Info,Broker-NBRK2KEY",Location
.                                       pack    KeyLocation,"Key: ",NBRK2FLD
.                                       call    NBRK2KEY
                                        move      COMPNUM to COMPNOTEFLD
                                        move      "O.LoadBroker-COMPNOTEKEY",Location
                                        pack      KeyLocation,"Key: ",COMPNOTEFLD
                                        call      COMPNOTEKEY
.patch1.6
                                        if not over
.COMMENTOUT1.6
.                                                 call    Trim using BRK2NOTES
                                                  call    Trim using COMPNOTES
.ENDCOMMENTOUT1.6
                                                  setprop OrderInfo,height=200
.START PATCH 1.1 REPLACED LOGIC
.                                        create  OrderInfo;InfoEditText=100:199:1:249,border=1,style=1,wordwrap=1,readonly=1
.                                        activate InfoEditText
                                        setprop InfoEditText,top=100,height=98
.END PATCH 1.1 REPLACED LOGIC
.COMMENTOUT1.6
.                                                 setitem InfoEditText,0,BRK2NOTES
                                                  setitem InfoEditText,0,COMPNOTES
.ENDCOMMENTOUT1.6
                                        endif
.patch1.6
.Position it correctly
                                call    SetInfoScreen using WindPtr
                                setprop OrderInfo,title="Broker Information"
                                setprop OrderInfo,visible=1
                        endif
                endif
        endif
        return

.START PATCH 1.8 ADDED LOGIC
OrderInfoStatText6Click
          getitem OrderInfoStatText6,0,taskname
          if (taskname <> "")
                    scan      at,taskname
                    if equal
                              reset     taskname
.Open Outlook application
                              create  mes
.Create Message
                              mes.createitem giving Note using 0
.Recipient
                              setprop note,*To=taskname
.Subject
                              setprop note,*Subject=""
.Body
.START PATCH 2.1 REMOVED LOGIC - CLEARING OF BODY CLEAR SIGNATURE
.                             setprop note,*Body=""
.END PATCH 2.1 REMOVED LOGIC - CLEARING OF BODY CLEAR SIGNATURE
                              Note.Display
                    endif
          endif
          return
.END PATCH 1.8 ADDED LOGIC

DisplayShipTo Routine WindPtr,EditPtr,FrmPtr4,DimPtr,DimPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds ShipTo #
.FrmPtr4 = Screen from which routine is called
.DimPtr  = Mailer #
.DimPtr1 = Offer #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,str6
                if (FrmPtr = C1)
                        count   N1,str6
                        if (N1 = 6)     .OREUSE
                                move    str6,OREUSE
                                move    "0001",str6
                        endif
                endif
                move    str6,NRTNFLD
                if (NRTNFLD <> "")
                        move    C1,NRTNPATH
                        move    "Info,ShpTo-NRTNKEY",Location
                        pack    KeyLocation,"Key: ",NRTNFLD
                        call    NRTNKEY
                        if not over
                                move    DimPtr,str7
                                match   "0677",str7
                                goto REPMLR if equal
                                match   "0210",str7
                                goto REPMLR if equal
                                match   "1361",str7         "CUSTOM LISTS"
                                goto REPMLR if equal
                                match   "0053",str7         "ANACAPA
                                goto REPMLR if equal
                                match   "0702",str7
                                goto REPMLR if equal
                                match   "0965",str7         "
                                goto REPMLR if equal
                                match   "2531",str6
                                goto KEEPRTN if equal
                                clear   str45
                                clear   str45a
                                pack    MKEY,str7,"000"
                                move    C1,NMLRPATH
                                move    "Info,ShpTo-NMLRKEY",Location
                                pack    KeyLocation,"Key: ",MKEY
                                call    NMLRKEY
                                move    mcomp to str45
                                move    rtcomp to str45a
                                rep     uplow in str45
                                rep     uplow in str45a
                                reset   str45
                                reset   str45a
                                match   str45 to str45a
                                goto KEEPRTN if equal
                                move    MCOMP,InfoString
                                setitem OrderInfoStatText1,0,InfoString
                                clear   InfoString
                                append  "C/O ",InfoString
                                goto LOADWINDOW

KEEPRTN                         move    RTCNTCT,InfoString
                                setitem OrderInfoStatText1,0,InfoString
                                clear   InfoString
                                goto LOADWINDOW

REPMLR

                                pack    NOFRFLD,str7,DimPtr1
                                rep     zfill in NOFRFLD
                                move    "Info,ShpTo-NOFRKEY",Location
                                pack    KeyLocation,"Key: ",NOFRFLD
                                call    NOFRKEY
                                if over
                                        clear   OFDESC
                                endif
                                move    OFDESC,InfoString
                                setitem OrderInfoStatText1,0,InfoString
                                clear   InfoString
LOADWINDOW
                                append  RTCOMP,InfoString
                                reset   InfoString
                                setitem OrderInfoStatText2,0,InfoString
.Address field of record "0001" says "Reuse of LR #"
                                clear   InfoString
                                append  RTADDR,InfoString
                                if (FrmPtr = C1)
                                        match   "0001",str6
                                        if equal
                                                append  B1,InfoString
                                                append  OREUSE,InfoString
                                        endif
                                endif
                                reset   InfoString
                                setitem OrderInfoStatText3,0,InfoString
                                setitem OrderInfoStatText4,0,RT2ADDR
                                call    TRIM using RTCITY
                                if (RTCITY <> "")
                                        pack    InfoString,RTCITY,", ",RTSTATE,B1,RTZIP
                                else
                                        pack    InfoString,RTSTATE,B1,RTZIP
                                endif
                                setitem OrderInfoStatText5,0,InfoString
.START PATCH 1.8 ADDED LOGIC
                                        setitem OrderInfoStatText6,0,""
.END PATCH 1.8 ADDED LOGIC
.Position it correctly
                                call    SetInfoScreen using WindPtr
                                setprop OrderInfo,title="Ship-To Information"
                                setprop OrderInfo,visible=1
                        endif
                endif
        endif
        return

DisplayOwner Routine WindPtr,EditPtr,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Owner #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,NOWNFLD
                if (NOWNFLD <> "")
                        move    C1,NOWNPATH
                        move    "Info,Owner-NOWNKEY",Location
                        pack    KeyLocation,"Key: ",NOWNFLD
                        call    NOWNKEY
                        if not over
                                setitem OrderInfoStatText1,0,OWNOCPY
                                setitem OrderInfoStatText2,0,OWNLONM
                                setitem OrderInfoStatText3,0,OWNLOSA
                                call    TRIM using OWNLOCTY
                                if (OWNLOCTY <> "")
                                        pack    InfoString,OWNLOCTY,", ",OWNLOS,B1,OWNLOZC
                                else
                                        pack    InfoString,OWNLOS,B1,OWNLOZC
                                endif
                                setitem OrderInfoStatText4,0,InfoString
                                unpack  OWNTELE,area,str3,str4
                                pack    str55,"(",area,")",str3,DASH,str4,B5
                                clear   InfoString
                                append  str55,InfoString
                                unpack  OWNFAX,area,str3,str4
                                pack    str55,"Fax: (",area,")",str3,DASH,str4,B5
                                append  str55,InfoString
                                reset   InfoString
                                setitem OrderInfoStatText5,0,InfoString

.START PATCH 1.8 ADDED LOGIC
.begin patch 30 APril 2008 DLH
                                        setitem OrderInfoStatText6,0,OwnEmail
.                                       setitem OrderInfoStatText6,0,""
.end patch 30 APril 2008 DLH
.END PATCH 1.8 ADDED LOGIC
.Position it correctly
                                call    SetInfoScreen using WindPtr
                                setprop OrderInfo,title="Owner Information"
                                setprop OrderInfo,visible=1
                        endif
                endif
        endif
        return
...............................................................................................................................
.begin patch 2.9
DisplayOCnt Routine WindPtr,EditPtr,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Owner #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,NDatCntFLD
                if (NDatCntFLD <> "")
                        move    C1,NDatCntPATH
                        move    "Info,Owner-NDatCntKEY",Location
                        pack    KeyLocation,"Key: ",NDatCntFLD
                        call    NDatCntKEY
                        if not over

                                setitem OrderInfoStatText1,0,NDatCnt
                                setitem OrderInfoStatText2,0,NDatCntEML
                                unpack  NDatCntPhn,area,str3,str4
                                pack    str55,"(",area,")",str3,DASH,str4,B5
                                clear   InfoString
                                append  str55,InfoString
                                setitem OrderInfoStatText3,0,InfoString
                                clear   InfoString
                                unpack  NDatCntfax,area,str3,str4
                                pack    str55,"Fax: (",area,")",str3,DASH,str4,B5
                                append  str55,InfoString
                                reset   InfoString
                                setitem OrderInfoStatText4,0,InfoString
                      setitem OrderInfoStatText5,0,""
                      setitem OrderInfoStatText6,0,""
                                call    SetInfoScreen using WindPtr
                                setprop OrderInfo,title="Contact Information"
                                setprop OrderInfo,visible=1
                        endif
                endif
        endif
        return
.end patch 2.9
................................................................................................................................
DisplayList Routine WindPtr,EditPtr,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds List #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,NDATFLD
                if (NDATFLD <> "")
                        if (N4 = 8)     .Left Mouse Button
                                move    C0,UNIVERSE
.begin patch xxx
                                 Clear      Min
.end patch xxx
                                move    C1,NDATPATH
                                move    "Info,List-NDATKEY",Location
                                pack    KeyLocation,"Key: ",NDATFLD
                                call    NDATKEY
                                if not over
                                        setitem OrderInfoStatText1,0,OLSTNAME
                                        clear   InfoString
.START PATCH 1.6 REPLACED LOGIC
.                                        unpack  REVDATE,MM,str1,DD,str1,STR2,YY
                                                  unpack  REVDATE,CC,YY,MM,DD
                                                  pack      str10,MM,SLASH,DD,SLASH,CC,YY
.END PATCH 1.6 REPLACED LOGIC
                                        call    CVTJUL
                                        move    JULDAYS,howmany
                                        clock   timestamp,timestamp
.START PATCH 1.6 REPLACED LOGIC
.                                        unpack  timestamp,str2,YY,MM,DD
                                                  unpack  timestamp,CC,YY,MM,DD
.END PATCH 1.6 REPLACED LOGIC
                                        call    CVTJUL
                                        sub     howmany,JULDAYS
.START PATCH 1.2 REPLACED LOGIC
.                                        append  "Updated: ",InfoString
.                                        append  REVDATE,InfoString
.                                        if (JULDAYS > 90)
.                                                append  " - Over 90 days old!!",InfoString
.                                        endif
                                        append  "Updated: ",InfoString
.START PATCH 1.6 REPLACED LOGIC
.                                        append  REVDATE,InfoString
                                                  append  str10,InfoString
.END PATCH 1.6 REPLACED LOGIC
                                        if (JULDAYS > 90)
.START PATCH 2.5 REPLACED LOGIC
.                                                append  " Over 90 days old!",InfoString
                                                append  " - 90+ days old!",InfoString
.END PATCH 2.5 REPLACED LOGIC
                                        endif
                                                  move      C1,NXRFPATH
                                                  clear     NXRFFLD2
                                                  pack      NXRFFLD,NDATFLD
                                                  move    "Info,List-NXRFKEY",Location
                                                  pack    KeyLocation,"Key: ",NXRFFLD
                                                  call      NXRFKEY
                                                  if not over
                                                            append    " Mlr: ",InfoString
                                                            append    NXRFMLR,InfoString
                                                  endif
.END PATCH 1.2 REPLACED LOGIC
                                        reset   InfoString
                                        setitem OrderInfoStatText2,0,InfoString
                                        clear   InfoString
                                        append  "Universe: ",InfoString
                                        move    UNIVERSE,str10
                                        call    FormatNumeric using str10,str13
                                        append  str13,InfoString
                                        append  B2,InfoString
.begin patch xxx
                                        append  "Minimum: ",InfoString    
                                        append  Min,InfoString
                                        append  B2,InfoString
.end patch xxx
                                        
                                        if (ELSTCDE = "C")
                                                append  "List is Exclusive!",InfoString
                                        endif
                                        reset   InfoString
                                        setitem OrderInfoStatText3,0,InfoString
                                        move    B5,InfoString
                                        clear   InfoString
                                        if (STATUS = "W" OR STATUS = "T")
                                                append  "List is Withdrawn!  ",InfoString
                                        endif
.START PATCH 1.6 REPLACED LOGIC
.                                        move    TextData,str55  .Only search in first line!
.                                        rep     lowup,str55     .Just in case text entered in lower case
.                                        scan    "*EXCHANGE ONLY",str55
.                                        if equal
.                                                append  "*Exchange Only!",InfoString
.                                        else
.                                                reset   str55
.                                                scan    "EXCHANGE ONLY",str55
.                                                if equal
.                                                        append  "Exchange Only!",InfoString
.                                                endif
.                                        endif
.
                                                  if (NDATCONV = "1") .Converted Datacard
.PATCH 2.0
                                                            pack      NSELFLD1,"01X",LSTNUM
                                                            pack      NSELFLD2,"02X","BASE"
                                                            clear NSELFLD3
                                                            move      "D.Load-NSELAIM",Location
                                                            pack      KeyLocation,"Key: ",NSELFLD1
                                                            call      NSELAIM
                                                            if not over
                                                                      if (NSELEXC = "2")
                                              append  "Exchange Only!",InfoString
                                                                      endif
                                                            endif
.                                                           if (NDATEXCH = "1")
.                                                                 append  "Exchange Only!",InfoString
.                                                           endif
.PATCH 2.0
                                                  else
                                                            pack      NTXTFLD,LSTNUM,"1"
                                                            move      "D.Load-NTXTKEY",Location
                                                            pack      KeyLocation,"Key: ",NTXTFLD
                                                            call      NTXTKEY
                                                            if not over
                                                                      move      NTXTTEXT,str55
                                                                      rep     lowup,str55     .Just in case text entered in lower case
                                                                      scan    "*EXCHANGE ONLY",str55
                                                                      if equal
                                                                              append  "*Exchange Only!",InfoString
                                                                      else
                                                                              reset   str55
                                                                              scan    "EXCHANGE ONLY",str55
                                                                              if equal
                                                                                      append  "Exchange Only!",InfoString
                                                                              endif
                                                                      endif
                                                            endif
                                                  endif
.END PATCH 1.6 REPLACED LOGIC
                                        reset   InfoString
                                        setitem OrderInfoStatText4,0,InfoString
                                        clear   InfoString
.START PATCH 2.5 REPLACED LOGIC
.                                        move    NDATFLD,NMDLFLD
                                        pack      NMDLFLD,NDATFLD," "
.END PATCH 2.5 REPLACED LOGIC
                                        move    "Info,List-NMDLKEY",Location
                                        pack    KeyLocation,"Key: ",NMDLFLD
                                        call    NMDLKEY
                                        if not over
                                                if (MDLCALL <> "" AND MDLCALL <> "  ")
                                                        move    C2,NUSEPATH
                                                        move    MDLCALL,NUSEFLD2
                                                        move    "Info,List-NUSEKEY",Location
                                                        pack    KeyLocation,"Key: ",NUSEFLD2
                                                        call    NUSEKEY
                                                        if not over
                                                                pack    InfoString,"Suggested Caller: ",NUSEUSER
                                                        else
                                                                pack    InfoString,"Suggested Caller: ",MDLCALL
                                                        endif
                                                endif
                                        endif
                                        setitem OrderInfoStatText5,0,InfoString
.START PATCH 1.8 ADDED LOGIC
                                                  setitem OrderInfoStatText6,0,""
.END PATCH 1.8 ADDED LOGIC
.Position it correctly
                                        call    SetInfoScreen using WindPtr
                                        setprop OrderInfo,title="List Information"
                                        setprop OrderInfo,visible=1
                                endif
                        else    .Right Mouse Button
.START PATCH 2.5 REPLACED LOGIC
.                                move    NDATFLD,NMDLFLD
                                        pack      NMDLFLD,NDATFLD," "
.END PATCH 2.5 REPLACED LOGIC
                                move    "Info,List-NMDLKEY",Location
                                pack    KeyLocation,"Key: ",NMDLFLD
                                call    NMDLKEY
                                if not over
.START PATCH 1.1 REPLACED LOGIC
.                                        create  OrderInfo;InfoEditText=1:99:1:249,style=1,wordwrap=1,readonly=1
.                                        activate InfoEditText
                                        setprop InfoEditText,height=98
.END PATCH 1.1 REPLACED LOGIC
                                        setitem InfoEditText,0,MDLTEXT
.Position it correctly
                                        call    SetInfoScreen using WindPtr
                                        setprop OrderInfo,title="List Information"
                                        setprop OrderInfo,visible=1
                                endif
                        endif
                endif
        endif
        return

.START PATCH 1.3 REPLACED LOGIC
.DisplayXSTAT Routine WindPtr,EditPtr,DimPtr,DimPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
..WindPtr = Window which calls this routine
..EditPtr = EditText Box which holds List #
DisplayXSTAT Routine WindPtr,DimPtr2,DimPtr,DimPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.DimPtr2 = List #
.END PATCH 1.3 REPLACED LOGIC
.DimPtr  = Mailer #
.DimPtr1 = LR #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                call    Trim using DimPtr
                if (DimPtr = "")
                        return
                endif
.START PATCH 1.3 REPLACED LOGIC
.                getitem EditPtr,0,str6
.                call    Trim using str6
.                if (str6 = "")
.                        return
.                endif
.                call    ZFILLIT using str6,C0
                call    Trim using DimPtr2
                if (DimPtr2 = "")
                        return
                endif
.START PATCH 2.3 REPLACED LOGIC
.                call    ZFILLIT using DimPtr2,C0
..END PATCH 1.3 REPLACED LOGIC
.                call    ZFILLIT using DimPtr,C0
..START PATCH 1.3 REPLACED LOGIC
..                move    str6,NDATFLD
.                move    DimPtr2,NDATFLD
..END PATCH 1.3 REPLACED LOGIC
.                move    C1,NDATPATH
.                move    "Info,Xstat-NDATKEY",Location
.                pack    KeyLocation,"Key: ",NDATFLD
.                call    NDATKEY
.                if not over
.                        move    NDATFLD,OLNUM
.                        move    DimPtr,OMLRNUM
.                        pack    MKEY,OMLRNUM,"000"
.                        move    C1,NMLRPATH
.                        move    "Info,Xstat-NMLRKEY",Location
.                        pack    KeyLocation,"Key: ",MKEY
.                        call    NMLRKEY         .get Mailer Name
.                        call    OrderTestXSTAT  .found in NLOLIO2.INC
.                        if (taskname = "")
.                                pack    NORDFLD1,"01R",OMLRNUM
.                                pack    NORDFLD2,"02R",OLNUM
.                                clear   NORDFLD3
.                                clear   NORDFLD4
.                                call    GetHistory using taskname,DimPtr1,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C1
.                        endif
.                        call    Parsitup using InfoString,taskname,C1
.                        setitem OrderInfoStatText1,0,InfoString
.                        call    Parsitup using InfoString,taskname,C1
.                        setitem OrderInfoStatText2,0,InfoString
.                        call    Parsitup using InfoString,taskname,C1
.                        setitem OrderInfoStatText3,0,InfoString
.                        call    Parsitup using InfoString,taskname,C1
.                        setitem OrderInfoStatText4,0,InfoString
.                        call    Parsitup using InfoString,taskname,C1
.                        setitem OrderInfoStatText5,0,InfoString
..START PATCH 1.8 ADDED LOGIC
.                             setitem OrderInfoStatText6,0,""
..END PATCH 1.8 ADDED LOGIC
..Position it correctly
.                        call    SetInfoScreen using WindPtr
.                        setprop OrderInfo,title="Mailer Information"
.                        setprop OrderInfo,visible=1
.                endif
................................................
                    call    ZFILLIT using DimPtr2,C0
                    call    ZFILLIT using DimPtr,C0
                    move    DimPtr2,NDATFLD
                    move    C1,NDATPATH
                    move    "Info,Xstat-NDATKEY",Location
                    pack    KeyLocation,"Key: ",NDATFLD
                    call    NDATKEY
                    if not over
                              if (N4 = 8)     .Left Mouse Button
                                        move    NDATFLD,OLNUM
                                        move    DimPtr,OMLRNUM
                                        pack    MKEY,OMLRNUM,"000"
                                        move    C1,NMLRPATH
                                        move    "Info,Xstat-NMLRKEY",Location
                                        pack    KeyLocation,"Key: ",MKEY
                                        call    NMLRKEY         .get Mailer Name
                                        call    OrderTestXSTAT  .found in NLOLIO2.INC
                                        if (taskname = "")
                                                  pack    NORDFLD1,"01R",OMLRNUM
                                                  pack    NORDFLD2,"02R",OLNUM
                                                  clear   NORDFLD3
                                                  clear   NORDFLD4
                                                  call    GetHistory using taskname,DimPtr1,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C1
                                        endif
                                        call    Parsitup using InfoString,taskname,C1
                                        setitem OrderInfoStatText1,0,InfoString
                                        call    Parsitup using InfoString,taskname,C1
                                        setitem OrderInfoStatText2,0,InfoString
                                        call    Parsitup using InfoString,taskname,C1
                                        setitem OrderInfoStatText3,0,InfoString
                                        call    Parsitup using InfoString,taskname,C1
                                        setitem OrderInfoStatText4,0,InfoString
                                        call    Parsitup using InfoString,taskname,C1
                                        setitem OrderInfoStatText5,0,InfoString
                                        setitem OrderInfoStatText6,0,""
.Position it correctly
                                        call    SetInfoScreen using WindPtr
                                        setprop OrderInfo,title="Mailer Information"
                                        setprop OrderInfo,visible=1
                              else           .if (N4 = 16)        .Right Mouse Button
                                        move      C1,NXRFPATH
                                        clear     NXRFFLD2
                                        pack      NXRFFLD,DimPtr2
                                        move      "Info,List-NXRFKEY",Location
                                        pack      KeyLocation,"Key: ",NXRFFLD
                                        call      NXRFKEY
                                        if not over
.Start of temporary logic until all Mailer fields use Company Number
                                                  pack      COMPFLD3,DimPtr
                                                  move      "Info,List-COMPKEY3",Location
                                                  pack      KeyLocation,"Key: ",COMPFLD3
                                                  call      COMPKEY3
                                                  if not over
                                                            move      COMPNUM,str6
.
.START PATCH 2.5 REPLACED LOGIC
.                                                           pack      COMPFLD3,NXRFMLR
.                                                           move      "Info,List,2-COMPKEY3",Location
.                                                           pack      KeyLocation,"Key: ",COMPFLD3
.                                                           call      COMPKEY3
                                                            pack      COMPFLD,NXRFMLR
                                                            move      "Info,List,2-COMPKEY",Location
                                                            pack      KeyLocation,"Key: ",COMPFLD
                                                            call      COMPKEY
.END PATCH 2.5 REPLACED LOGIC
                                                            if not over
.End of temporary logic until all Mailer fields use Company Number
                                                                      pack      NMLRXYFLD,COMPNUM,str6
                                                                      move      "Info,List-NMLRXYKEY",Location
                                                                      pack      KeyLocation,"Key: ",NMLRXYFLD
                                                                      call      NMLRXYKEY
                                                                      if not over
                                                                                setprop InfoEditText,height=98
                                                                                setitem InfoEditText,0,NMLRXYNOTE
.Position it correctly
                                                                                call    SetInfoScreen using WindPtr
                                                                                setprop OrderInfo,title="Mlr/Mlr Information"
                                                                                setprop OrderInfo,visible=1
                                                                      else
                                                                                pack      NMLRXYFLD,str6,COMPNUM
                                                                                move      "Info,List2-NMLRXYKEY",Location
                                                                                pack      KeyLocation,"Key: ",NMLRXYFLD
                                                                                call      NMLRXYKEY
                                                                                if not over
                                                                                          setprop InfoEditText,height=98
                                                                                          setitem InfoEditText,0,NMLRXYNOTE
.Position it correctly
                                                                                          call    SetInfoScreen using WindPtr
                                                                                          setprop OrderInfo,title="Mlr/Mlr Information"
                                                                                          setprop OrderInfo,visible=1
                                                                                endif
                                                                      endif
                                                            endif
                                                  endif
                                        endif
                              endif
                    endif
.END PATCH 2.3 REPLACED LOGIC
        endif
        return

DisplayMessage Routine WindPtr,DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4,DimPtr5,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.DimPtr  = Title of Message Box
.DimPtr1 = OrderInfoStatText1
.DimPtr2 = OrderInfoStatText2
.DimPtr3 = OrderInfoStatText3
.DimPtr4 = OrderInfoStatText4
.DimPtr5 = OrderInfoStatText5
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                setitem OrderInfoStatText1,0,DimPtr1
                setitem OrderInfoStatText2,0,DimPtr2
                setitem OrderInfoStatText3,0,DimPtr3
                setitem OrderInfoStatText4,0,DimPtr4
                setitem OrderInfoStatText5,0,DimPtr5
.START PATCH 1.8 ADDED LOGIC
                    setitem OrderInfoStatText6,0,""
.END PATCH 1.8 ADDED LOGIC
.Position it correctly
                if (L1 <> C0 & T1 <> C0)
                        call    SetInfoScreen using WindPtr
                endif
                setprop OrderInfo,title=DimPtr
                setprop OrderInfo,visible=1
        endif
        return

InfoClose Routine
        setprop OrderInfo,visible=0
        setprop OrderInfo,winpos=3
        setprop OrderInfo,height=100
        setprop OrderInfoStatText1,fgcolor=black
        setprop OrderInfoStatText5,fgcolor=black
.START PATCH 1.8 ADDED LOGIC
          setprop OrderInfoStatText6,fgcolor=black
.END PATCH 1.8 ADDED LOGIC

.START PATCH 1.1 REPLACED LOGIC
.        destroy InfoListView
.        destroy InfoEditText
        setprop InfoListView,height=0
        setprop InfoEditText,top=1,height=0
.END PATCH 1.1 REPLACED LOGIC
        return

SetInfoScreen Routine WindPtr
.LOGIC in this section broken down into following generalized equation:
.
.OrderInfo_Top=(TopCoordinateOfMouseClick + TopCoordinateOfObjectWhereClickOccurred + Cushion + TopCoordinateOfProgram1Screen
.If ((OrderInfo_Top + OrderInfo_Height) > ScreenHeight)
.       OrderInfo_Top=(OrderInfo_Top - TopCoordinateOfMouseClick - Cushion
.Endif
.
.OrderInfo_Left=(LeftCoordinateOfMouseClick + LeftCoordinateOfObjectWhereClickOccurred + Cushion + LeftCoordinateOfProgram1Screen
.If ((OrderInfo_Left + OrderInfo_Width) > ScreenWidth)
.       OrderInfo_Left=(OrderInfo_Left - LeftCoordinateOfMouseClick - Cushion
.Endif
.
.
.Getinfo
.This is done each time in case the user changes their screen dimensions in the middle of
.using this program
        clear   str25
        getinfo system,str25
        bump    str25,12
        move    str25,str4
        move    str4,ScrRight
        bump    str25,4
        move    str25,str4
        move    str4,ScrBottom
.
        setprop OrderInfo,winpos=1
        getprop WindPtr,top=H,left=V
.Break down mouse coordinates - figured in terms of object where mouse was clicked
.MouseForm established at MouseDown_Event
        move    "10000",N7
        div     N7,MouseForm,N9 .N9=left
        mult    N9,N7
        sub     N7,MouseForm,N8 .N8=top
.Add to STATIC coordinates of object where mouse was clicked
.T1/L1 established at MouseDown_Event
        add     N9,L1           .L1=left
        add     N8,T1           .T1=top
.Calulate totals for positions
        add     T1,H
        add     "44",H          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
        add     L1,V
        add     "7",V           .Compensate to allow second click to make invisible
.Test to see if object will fit on page
        Getprop OrderInfo,height=N9,width=N8
        add     N9,H,FarBottom
        if (FarBottom > ScrBottom)
                sub     N9,H
                sub     "20",H  .Compensate to allow second click to make invisible
        endif
        add     N8,V,FarRight
        if (FarRight > ScrRight)
                sub     N8,V
                sub     "10",V  .Compensate to allow second click to make invisible
        endif
.Set coordinates
        setprop OrderInfo,top=H,left=V
        return

SetFormVars
        move    FrmPtr,N4
        move    FrmPtr1,MouseForm
        move    FrmPtr2,T1
        move    FrmPtr3,L1
        return

.START PATCH 1.4 ADDED LOGIC
DisplayOmit Routine WindPtr,EditPtr,EditPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Mailer #
.EditPtr1 = EditText Box which holds List #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.  NOT USED IN THIS ROUTINE BUT NEEDED BY SETFORMVARS!!!!
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,str4
                getitem EditPtr1,0,str6
                    move      C1,NORDPATH
                    clear     NORDFLD1
                    clear     NORDFLD2
                    clear     NORDFLD3
                    clear     nordfld4
                    clear     nordfld5
                    clear     NORDFLD6
                    clear     NORDFLD7
                    clear     NORDFLD8
                    if (str4 <> "")
.                             pack      NORDFLD1,"01R",str4
                              REp       Zfill,str4
                              packKey   NORDFLD1,"01R",str4
                    else
                              alert     note,"Valid Mailer Required to find past Omit records!",result
                              return
                    endif
                    if (str6 <> "")
.                             pack      NORDFLD2,"02R",str6
                              rep       Zfill,str6
                              packkey   NORDFLD2,"02R",str6
                    else
                              alert     note,"Valid List Required to find past Omit records!",result
                              return
                    endif
                    move      "Info-NORDLAST",Location
                    call      NORDLAST
                    if not over
                              move      "Info-NORDKGP",Location
                        setprop InfoListView,height=98
                        InfoListView.DeleteAllContents
.START PATCH 1.7 REPLACED LOGIC
.                        InfoListView.InsertColumn using "LR",60,1
.                        InfoListView.InsertColumn using "Order Date",70,2
.                        InfoListView.InsertColumn using "Qty",70,3
.                        InfoListView.InsertColumn using "Select",140,4
.START PATCH 2.6 ADDED LOGIC
                        InfoListView.InsertColumn using "Order Date Sort",0,0
.END PATCH 2.6 ADDED LOGIC
                        InfoListView.InsertColumn using "LR",50,1
                        InfoListView.InsertColumn using "Order Date",70,2
                        InfoListView.InsertColumn using "Qty",60,3
                        InfoListView.InsertColumn using "Net",45,4
                        InfoListView.InsertColumn using "Select",140,5
                              InfoListView.SetColumnFormat using 2,1
                              InfoListView.SetColumnFormat using 3,1
.END PATCH 1.7 REPLACED LOGIC
                              setprop   InfoListView,sortorder=2
                              move      C0,howmany
                              loop
                                        if (OSTAT = "0" | OSTAT = "B")
                                                  add       C1,howmany
.START PATCH 2.6 REPLACED LOGIC
.                                        InfoListView.InsertItem giving N9 using OLRN
                                        move      OODTEM,MM
                                        move      OODTED,DD
                                        move      OODTEC,CC
                                        move      OODTEY,YY
                                        call      CVTJUL
                                        move      JULDAYS,str5
                                        InfoListView.InsertItem giving N9 using str5
                                        InfoListView.SetItemText using N9,OLRN,1
.END PATCH 2.6 REPLACED LOGIC
                                                  pack      str10,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
.START PATCH 2.6 REPLACED LOGIC
.                                        InfoListView.SetItemText using N9,str10,1
                                        InfoListView.SetItemText using N9,str10,2
.END PATCH 2.6 REPLACED LOGIC
                                        move    OQTY,str9
                                        call    FormatNumeric using str9,str11
.START PATCH 2.6 REPLACED LOGIC
.                                        InfoListView.SetItemText using N9,str11,2
                                        InfoListView.SetItemText using N9,str11,3
.END PATCH 2.6 REPLACED LOGIC
.START PATCH 1.7 ADDED LOGIC
                                                  clear     str10
                                                  move      C0,nordin
                                                  move      C0,nordout
                                                  move      C0,calcper
                                                  move      C0,percent
                                                  move      OLRN,NMRGFLD
                                                  move      "NMRGKEY",Location
                                                  pack      KeyLocation,"Key: ",NMRGFLD
                                                  call      NMRGKEY
                                                  if not over
                                                            add       NMRGIQTY,nordin
                                                            add       NMRGNET,nordout
                                                            compare   C0,nordout
                                                            if not equal
                                                                      move      C0,CALCPER
                                                                      move      nordout,CALCPER
                                                                      divide    nordin,CALCPER
                                                                      mult      "100",CALCPER
                                                                      move      C0,PERCENT
                                                                      add       CALCPER,PERCENT
                                                                      move      PERCENT,str10
                                                            endif
                                                  endif
.START PATCH 2.6 REPLACED LOGIC
.                                                 InfoListView.SetItemText using N9,str10,3
                                                  InfoListView.SetItemText using N9,str10,4
.END PATCH 2.6 REPLACED LOGIC
.END PATCH 1.7 ADDED LOGIC
.START PATCH 1.6 REPLACED LOGIC
.                                                 call      Trim using O2DES
.                                                 InfoListView.SetItemText using N9,O2DES,3
                                                  packkey   NSEL2FLD,"1",OLRN
                                                  move      "NSEL2KEY",Location
                                                  pack      KeyLocation,"Key: ",NSEL2FLD
                                                  call      NSEL2KEY
                                                  if over
                                                            move      O2DES,NSEL2NAME
                                                  endif
                                                  call      Trim using NSEL2NAME
.START PATCH 1.7 REPLACED LOGIC
.                                                 InfoListView.SetItemText using N9,NSEL2NAME,3
.START PATCH 2.6 REPLACED LOGIC
.                                                 InfoListView.SetItemText using N9,NSEL2NAME,4
                                                  InfoListView.SetItemText using N9,NSEL2NAME,5
.END PATCH 2.6 REPLACED LOGIC
.END PATCH 1.7 REPLACED LOGIC
.END PATCH 1.6 REPLACED LOGIC
                                        endif
.START PATCH 2.6 REPLACED LOGIC
.                                       if (howmany >= 5)
                                        if (howmany >= 30)         .dh testing
.END PATCH 2.6 REPLACED LOGIC
                                                  goto      DisplayOmitLoopBreak
                                        endif
                                        call      NORDKGP
                                        until over
                                        if        (Olrn = "665449")
                                        call      debug
                                        endif
                              repeat
                    else
                              return
                    endif
DisplayOmitLoopBreak
.START PATCH 2.6 ADDED LOGIC
                    InfoListView.GetItemCount giving N9
                    if (N9 > 5)
.                   if (N9 > 10)        DH testing
                              sub       C1,N9
                              for result,N9,"5",SEQ
.                             for result,N9,"10",SEQ        DH testing
                                        InfoListView.DeleteItem using result
                              repeat
                    endif
.END PATCH 2.6 ADDED LOGIC
.Put focus on 1rst item
                    InfoListView.SetItemState giving N9 using 0,2,2
.Position it correctly
                    call    SetInfoScreen using WindPtr
.                   setprop OrderInfo,title="Omit Information, 10 Past Orders"
                    setprop OrderInfo,title="Omit Information, 5 Past Orders"
                    setprop OrderInfo,visible=1
                    setfocus InfoListView
          endif
          return
.END PATCH 1.4 ADDED LOGIC

.START PATCH 1.5 ADDED LOGIC
DisplaySearchTitle Routine
          setprop OrderInfo,title="NIN Order Search"
          return
.END PATCH 1.5 ADDED LOGIC
.>Patch 2.4 Begin Patch
DisplayFulfillment Routine WindPtr,EditPtr,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Owner #  (Which will be used to extract the fulfillment comp and read the fulfillment file)
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseFrom - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
.Start Patch 2.8 Comment Out Replacing owner lookup with order field read.                
.               getitem EditPtr,0,NOWNFLD
.               if (NOWNFLD <> "")
.                       move    C1,NOWNPATH
.                       move    "Info,Owner-NOWNKEY",Location
.                       pack    KeyLocation,"Key: ",NOWNFLD
.                       call    NOWNKEY
.                       if not over
.End Patch 2.8 Comment Out Replacing owner lookup with order field read. 
.Start Patch 2.8 Replacing owner lookup with order field read.
                getitem EditPtr,0,COMPFLD
                    if (COMPFLD <> "")
.End Patch 2.8 Replacing Owner Lookup with Order Field Read.

.START PATCH 2.7 REPLACED LOGIC
.                                       pack      NFULFLD,OWNCTN
.                                       if (NFULFLD <> "    ")
.                                                 rep       zfill,NFULFLD
.                                                 move      C1,NFULPATH
.                                                 move      "O.LoadOwner-NFULKEY",Location
.                                                 pack      KeyLocation,NFULFLD
.                                                 call      NFULKEY
.                                                 if not over
.                                                   setitem OrderInfoStatText3,0,NFULCOMP
.                                                   setitem OrderInfoStatText4,0,NFULCNT
.                                                   unpack  NFULFAX,area,str3,str4
.                                                   pack    str55,"Fax: (",area,")",str3,DASH,str4,B5
.                                                   clear   InfoString
.                                                   append  str55,InfoString
.                                                   reset   InfoString
.                                                   setitem OrderInfoStatText5,0,InfoString
.                                                           setitem OrderInfoStatText1,0,""
.                                                           setitem OrderInfoStatText2,0,""
.                                                           call      Trim using NFULEMAIL
.                                                           if (NFULEMAIL = "")
.                                                                     setitem OrderInfoStatText6,0,""
.                                                           else
.                                                                     eventreg OrderInfoStatText6,4,OrderInfoStatText6Click,RESULT=N9
.                                                                     setitem OrderInfoStatText6,0,NFULEMAIL
.                                                                     setprop OrderInfoStatText6,fgcolor=blue
.                                                           endif
.                                                           call    Trim using NFULNOTES
.                                                           setprop OrderInfo,height=200
.                                                 setprop InfoEditText,top=100,height=98
.                                                           setitem InfoEditText,0,NFULNOTES
..Position it correctly
.                                                   call    SetInfoScreen using WindPtr
.                                                   setprop OrderInfo,title="Fulfillment Information"
.                                                   setprop OrderInfo,visible=1
.                                                 endif
.                                 endif
.Start Patch 2.8 Comment Out Replacing owner lookup with order field read.                
.                                       pack      COMPFLD6,OWNCTN
.                                       if (COMPFLD6 = "    ")
.                                                 pack      COMPFLD6, "////"  // will force an over
.                                       else
.                                                 rep       zfill, COMPFLD6  // will make "23" "0023"
.                                       endif
.                                       move      C1,COMPPATH
.                                       move      "O.LoadFul-COMPKEY6", Location
.                                       pack      KeyLocation, COMPFLD6
.                                       call      COMPKEY6
.                                       if not over
.End Patch 2.8 Comment Out Replacing owner lookup with order field read.   
.Start Patch 2.8 Replacing owner lookup with order field read.
                                        if (COMPFLD = "      " Or COMPFLD = "")
                                                  pack      COMPFLD, "////"  // will force an over
                                        else
                                                  rep       zfill, COMPFLD  // will make "23" "0023"
                                        endif
                                        call      zfillit using COMPFLD
                                        move      C1,COMPPATH
                                        move      "O.LoadFul-COMPKEY", Location
                                        pack      KeyLocation, COMPFLD
                                        call      COMPKEY
                                        if not over                                       
.End Patch 2.8 Replacing owner lookup with order field read.
                                                  if (COMPSVBFLG="T")
.Grab the active contact full name if there is one
                                                            Packkey CNCTFLD2 to "01X",COMPNUM
                                                            Call      CNCTAIM
                                                            loop
                                                            until over
                                                            until (CNCTTYPE = "4" & CNCTINACTIVE <> "T")
                                                                      call      CNCTKG
                                                            repeat
                                                            setitem OrderInfoStatText1,0,CNCTFNAME
                                                            setitem OrderInfoStatText2,0,COMPCOMP
                                                            setitem OrderInfoStatText3,0,COMPADDR
                                                            call      Trim using COMPCITY
                                                            if (COMPCITY <> "")
                                                                      pack      InfoString,COMPCITY,", ",COMPSTATE,B1,COMPZIP
                                                            else
                                                                      pack      InfoString,COMPSTATE,B1,COMPZIP
                                                            endif
                                                            setitem OrderInfoStatText4,0,InfoString
                                                            unpack  COMPFAX,area,str3,str4
                                                            pack    str55,"Fax: (",area,")",str3,DASH,str4,B5
                                                            clear   InfoString
                                                            append  str55,InfoString
                                                            reset   InfoString
                                                            setitem OrderInfoStatText5,0,InfoString
                                                            call      TRIM using CNCTFNAME
                                                            if (CNCTFNAME<>"")   // if contact, load cnctemail
                                                                      call      Trim using CNCTEMAIL
                                                                      if (CNCTEMAIL = "")  // use compemail
                                                                                move      COMPEMAIL, str50
                                                                      else      // there was value in cnctemail
                                                                                move CNCTEMAIL, str50
                                                                      endif
                                                            else  // there was no contact
                                                                      call      Trim using COMPEMAIL
                                                                      if (COMPEMAIL = "")
                                                                                clear str50  // just to be sure
                                                                      else
                                                                                move COMPEMAIL, str50
                                                                      endif
                                                            endif
                                                            call trim using str50
                                                            if (str50 = "")
                                                                      setitem OrderInfoStatText6,0,""
                                                            else
                                                                      eventreg OrderInfoStatText6,4,OrderInfoStatText6Click,RESULT=N9
                                                                      setitem OrderInfoStatText6,0,str50
                                                                      setprop OrderInfoStatText6,fgcolor=blue
                                                            endif
.
                                                            move      COMPNUM to COMPNOTEFLD
                                                            move      "O.LoadFul-COMPNOTEKEY",Location
                                                            pack      KeyLocation,"Key: ",COMPNOTEFLD
                                                            call      COMPNOTEKEY
                                                            if over
                                                                      clear     COMPNOTES
                                                            endif
                                                            call    Trim using COMPNOTES
                                                            setprop OrderInfo,height=200
                                                            setprop InfoEditText,top=100,height=98
                                                            setitem InfoEditText,0,COMPNOTES
.Position it correctly
                                                            call    SetInfoScreen using WindPtr
                                                            setprop OrderInfo,title="Fulfillment Information"
                                                            setprop OrderInfo,visible=1
                                                  endif  // end compsvbflg=t
                                  endif // end if not over
.END PATCH 2.7 REPLACED LOGIC
.                             endif
                endif
        endif
        return
.>Patch 2.4 End Patch

.Begin patch 1.1
.added DH 05/01/08    so converted programs can use
.need to add contact file at some point (when ptr2 has a value)
DisplayCompany Routine WindPtr,EditPtr,EditPtr1,FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.WindPtr = Window which calls this routine
.EditPtr = EditText Box which holds Mailer #
.EditPtr1 = EditText Box which holds Mailer Contact #
.Used by SetFormVars
.FrmPtr  = N4 - Type of Click, ie; left, right, double, etc.
.FrmPtr1 = MouseForm - XY coordinate of Mouse at MouseDown event on object
.FrmPtr2 = T1 - Top of object clicked
.FrmPtr3 = L1 - Left of object clicked
        getprop OrderInfo,visible=N1
        if (N1 = C1)
                call    InfoClose
        else
                call    SetFormVars
                getitem EditPtr,0,STR6
                packKEy    COMPFLD,str6
                if (CompFld <> "")
                        move    C1,CompPath
                        move    "Info,Company-COMPKEY",Location
                        pack    KeyLocation,"Key: ",CompFld
                        call    CompKEY
                        if not over
                                if (N4 = 8)     .Left Mouse Button
                                        pack    InfoString,CompCOmp
                                        setitem OrderInfoStatText1,0,InfoString
.                                        setitem OrderInfoStatText2,0,MNAME
                                        setitem OrderInfoStatText2,0,""
                                        setitem OrderInfoStatText3,0,CompADDR
                                        call    TRIM using CompCITY
                                        if (CompCITY <> "")
                                                pack    InfoString,CompCITY,", ",CompSTATE,B1,CompZIP
                                        else
                                                pack    InfoString,CompSTATE,B1,CompZIP
                                        endif
                                        setitem OrderInfoStatText4,0,InfoString
                              IF        (COMPREGCDE = "T")
                                                setprop OrderInfoStatText1,fgcolor=red
                                        else
                                                setprop OrderInfoStatText1,fgcolor=black
                                        endif
                                        clear   InfoString
                                        setprop OrderInfoStatText5,fgcolor=red
                                        if (compcredit = "*")
                                                append  "Credit On Hold",InfoString
                                        elseif (compcredit = "N")
                                                append  "New - Credit On Hold",InfoString
                                        elseif (compcredit = "I")
                                                append  "Inactive-Credit On Hold",InfoString
                                        elseif (compcredit = "B")
                                                append  "Credit Risk",InfoString
                                        elseif (compcredit = "P")
                                                append  "Political-Credit On Hold",InfoString
                                        elseif (compcredit = "W")
                                                append  "Warning - See Notes!",InfoString
                                        elseif (compcredit = "M")
                                                append  "Must Prepay!",InfoString
                                        elseif (compcredit = "9")
                                                append  "On hold until over 90's paid!",InfoString
                                        elseif (compcredit = "G")
                                                append  "Guarantees are always required!",InfoString
                                        elseif (compcredit = "g")
                                                append  "Guarantees no longer accepted!",InfoString
                                        else
                                                append  "Credit OK",InfoString
                                                setprop OrderInfoStatText5,fgcolor=black
                                        endif
                                        reset   InfoString
                                        setitem OrderInfoStatText5,0,InfoString

                                        call      Trim using COMPEMAIL
                                        if (COMPEMAIL = "")
                                                  setitem OrderInfoStatText6,0,""
                                        else
                                                  eventreg OrderInfoStatText6,4,OrderInfoStatText6Click,RESULT=N9
                                                  setitem OrderInfoStatText6,0,COMPEMAIL
                                                  setprop OrderInfoStatText6,fgcolor=blue
                                        endif


                              move      COMPNUM to COMPNOTEFLD
                              move      "O.LoadMailer-COMPNOTEKEY",Location
                              pack      KeyLocation,"Key: ",COMPNOTEFLD
                              call      COMPNOTEKEY

                              if not over
                              call    Trim using COMPNOTES
                                                setprop OrderInfo,height=200
                                                setprop InfoEditText,top=100,height=98
                                                setitem InfoEditText,0,COMPNOTES
                                        endif
.Position it correctly
                                        call    SetInfoScreen using WindPtr
                                        setprop OrderInfo,title="Mailer Information"
                                        setprop OrderInfo,visible=1
                                else           .if (N4 = 16)        .Right Mouse Button
                                        move    C2 to NXRFPATH
                                        move    COMPNUM,NXRFFLD2
                                        move    "Info,MLR-NXRFKEY",Location
                                        pack    KeyLocation,"Key: ",NXRFFLD2
                                        call    NXRFKEY
                                        if over
                                                move    "000000",nxrflist
                                        endif
                                        move    MNUM,str4
                                        clear   result
                                        move    NXRFLIST,NDATFLD
                                        move    C1,NDATPATH
                                        move    "Info,MLR-NDATTST",Location
                                        pack    KeyLocation,"Key: ",NDATFLD
                                        call    NDATTST
                                        if not over
                                                setprop InfoListView,height=98
                                                InfoListView.DeleteAllContents
                                                InfoListView.InsertColumn using "List",60,1
                                                InfoListView.InsertColumn using "Name",140,2
                                                InfoListView.InsertColumn using "Universe",70,3
                                                InfoListView.InsertColumn using "Withdrawn",70,4
                                                InfoListView.InsertColumn using "Rev. Date",70,5
                                                            setprop   InfoListView,sortorder=1
                                                loop
                                                        move    NXRFLIST,NDATFLD
                                                        move    C1,NDATPATH
                                                        move    "Info,MLR-NDATKEY",Location
                                                        pack    KeyLocation,"Key: ",NDATFLD
                                                        call    NDATKEY
                                                        if not over
                                                                InfoListView.InsertItem giving N9 using LSTNUM
                                                                InfoListView.SetItemText using N9,OLSTNAME,1
                                                                move    UNIVERSE,str10
                                                                call    FormatNumeric using str10,str13
                                                                InfoListView.SetColumnFormat using 2,1
                                                                InfoListView.SetItemText using N9,str13,2
                                                                if (STATUS = "W" | STATUS = "T")
                                                                        move    "YES",str3
                                                                else
                                                                        clear   str3
                                                                endif
                                                                InfoListView.SetItemText using N9,str3,3
                                                                                unpack    REVDATE,CC,YY,MM,DD
                                                                                pack      str10,MM,SLASH,DD,SLASH,CC,YY
                                                                    InfoListView.SetItemText using N9,str10,4
                                                        endif
                                                        call    NXRFKS
                                                        until over
                                                        match  COMPNUM,NXRFMLR
                                                        until not equal
                                                repeat
                                                InfoListView.SetItemState giving N9 using 0,2,2
                                                call    SetInfoScreen using WindPtr
                                                setprop OrderInfo,title="Mailer Information, Associated Lists"
                                                setprop OrderInfo,visible=1
                                                setfocus InfoListView
                                        endif
                                endif
                        endif
                endif
        endif
        return
.end patch 1.1
.PATCH 2.0
          include   nselio.inc
.END PATCH 2.0
        include nordio.inc
.START PATCH 1.9 REPLACED LOGIC
.       include nmlrio.inc
.       include nmlr2io.inc
.        include nbrkio.inc
.       include nbrk2io.inc
          include   compIO.inc
          include   cntIO.inc
.END PATCH 1.9 REPLACED LOGIC
        include nxrfio.inc
        include ndatio.inc
        include nrtnio.inc
        include nownio.inc
        include nmdlio.inc
        include nuseio.inc
        include nxngio.inc
        include nxchio.inc
        include nlolio2.inc
        include nofrio.inc
.PATCH    1.6
                      include compnotesio.inc
.END PATCH  1.6
.START PATCH 1.6 ADDED LOGIC
          include   nsel2io.inc
          include   ntxtio.inc
.END PATCH 1.6 ADDED LOGIC
.START PATCH 1.7 ADDED LOGIC
          include   nmrgio.inc
.END PATCH 1.7 ADDED LOGIC
.START PATCH 2.3 ADDED LOGIC
          include   nmlrxyio.inc
.END PATCH 2.3 ADDED LOGIC
.>START PATCH 2.4 ADDED LOGIC
.START PATCH 2.7 REMOVED LOGIC
.         include   nfulio.inc
.END PATCH 2.7 REMOVED LOGIC
.>END PATCH 2.4 ADDED LOGIC
        Include     NDatCntio.inc
        include comlogic.inc
