******************************************************************************
* ADDADJ -  THIS IS THE ADJUSTMENT PROG
******************************************************************************

. Program to CREATE new ADJUST.DETAIL REC.& either UPDATE CURRENT
.
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NPASDD.INC
.begin patch 1.6
          InCLUDE             ninvdd.inc
          include   NInvAcddd.inc
.end patch 1.6
         INCLUDE   NADJDD.inc
         INCLUDE   NJSTDD.inc
         INCLUDE   NCKIDD.INC
           include   nusedd.inc
         include   nonodd.inc
         INCLUDE   GNXTDD.inc
.START PATCH 1.3 ADDED LOGIC
          INCLUDE   NCSHDD.INC
          INCLUDE   NCHKDD.INC
.END PATCH 1.3 ADDED LOGIC
.begin patch 1.4
mrgsw          dim       1
shipsw         dim       1
               include        norddd.inc
.START PATCH 1.5 REPLACED LOGIC
.               include        nmlrdd.inc
          include   compdd.inc
          include   cntdd.inc
.END PATCH 1.5 REPLACED LOGIC
               include        ndatdd.inc
               include        ndat3dd.inc
               include        nshpdd.inc
               include        nmrgdd.inc
               include        nowndd.inc
               include        nacddd.inc
               include        consacct.inc
.START PATCH 1.93 ADDED LOGIC
               INCLUDE        NTYPDD.INC
.END PATCH 1.93 ADDED LOGIC
CALC102        FORM           10.2
holdar         form           10.2
holdap         form           10.2
holdap2        form           10.2
holdlrinc      form           10.2
holdNIninc     form           10.2
.begin patch 1.9
          INclude   Nadjres.inc
.begin patch 1.9

.end patch 1.4
PC      EQU     1
Release   INit      "1.93"     AH           .Adjusted TYPOUT logic
REldate   Init      "13 November 2013"
.Release   Init      "1.92"    DLH            Code 38
.Reldate   Init      "27 March 2009"
.Release  Init      "1.91"    DLH            short pay notice logo
.Reldate  Init      "16 October 2008"
.Release  Init      "1.9"     DLH
.Reldate  Init      "23 September 2008"
.Release            Init                "1.8.1"             DLH Calc button
.RelDate            Init                "14 April 2008"
.Release            Init                "1.8"               DLH Pacific Lists
.RelDate            Init                "23 October 2007"
.Release            Init                "1.7"               DMS            14June2006 NONODD CONVERSION
.RelDate            Init                "14 June 2006"
.Release        INit           "1.6"            DLH            03March2005 Invoice CONVERSION
.RelDate        Init           "03 March 2005"
.Release        INit           "1.5"            ASH            26MAY2004 MAILER CONVERSION
.RelDate        Init           "26 May 2003"
.Release        INit           1.4            DLH            20August2003   calc logic for qty adjust
.RelDate        Init           "20 August 2003"
.Release        INit           1.32           DLH            21July2003     logic for global passing of LR to program
.RelDate        Init           "21 July 2003"
.release            init      "1.31"    ASH       03/07/03  ADDED ADJUSTMENT INVOICE OPTION IN MS WORD FORMAT
..release           init      "1.3"     ASH       02/26/03  ADDED DYNAMIC PRODUCTION OF SHORT PAYMENT NOTICE
.                                                             ADDED AdjustShortPay Button & New logic under Save Button/Load Routine
.release  init      "1.2"     DMB       03/05/02  Added Date Verification to not allow new adj if before previous billing date EXCEPT if IS
.release  init      "1.1"     DMB       02/25/02  Added Date Verification to not allow new adj if before previous billing Date
.release  init      "1.0"

.START PATCH 1.3 ADDED LOGIC
CreateShortPayNotice external "NCSH002A;CreateShortPayNotice"
TempForm form       10.2
.END PATCH 1.3 ADDED LOGIC

WindPtr   Window    ^
DimPtr    Dim       ^
DimPtr1   Dim       ^
FrmPtr    Form      ^
FrmPtrA   Form      ^
FrmPtr1   Form      ^
EditPtr EditText ^
* *****************************************************************************
. FILES.
. .............................................................................
PADJDET   IFILE     KEYLEN=7,FIXED=193
.PADJDET  IFILE     KEYLEN=7,FIXED=175
INVNOA  DIM         8
. ........ADJUSTMENT FILE MASTER - also used for READ and WRITE of DETAIL

N92       form      9.2
N52       form      5.2
. .TOTAL AREAS
xfoot     form      10.2
TOTAR   FORM  10.2
TOTAP1  FORM  10.2
TOTAP2  FORM  10.2
.begin patch 1.8
TOTAP3  FORM  10.2
.end patch 1.8
TOTLR   FORM  10.2
TOTNIN  FORM  10.2
.begin patch 1.8
TOTXNIN  FORM  10.2
.end patch 1.8
TOTSTX  FORM  5.2
TOTCTX  FORM  5.2
TOTPOST FORM  5.2
CDE25SW  DIM       1            Y=YES TO CODE 25, ELSE NO (DEFAULT)
*********************************************
.Check Date verification var
CHKDATE  FORM      5
*********************************************
.begin patch 1.9

..adjustment reason code descriptions
..
.adjres1  init      "Adjustment to Quantity"
.adjres2  init      "Shipping"
.adjres3  init      "Selection fee"
.adjres4  init      "Running Charges"
.adjres5  init      "Change in price"
.adjres6  init      "Not Used!"
.adjres7  init      "Adjust Within A/P "
.adjres8  init      "Adjust A/R & LR "
.adjres9  init      "Adjust A/P & LR"
.adjres10  init      "Cancel entire Bill"
.adjres11  init      "No invoice A/P to Lr "
.adjres12  init      "Late Lo inv LR to A/P "
.adjres13  init      "Adjustment of Income"
.adjres14  init      "Advance Payment to LO"
.adjres15  init      "Adjustment of Tax"
.adjres16  init      "Short Payment"
.adjres17  init      "Commission"
.adjres18  init      "Postage"
.adjres19  init      "Direct Payment to LO"
.adjres20  init      "Not Used!"
.adjres21  init      "Advance Payment to LO"
.adjres22  init      "Reduction of A/R"
.adjres23  init      "Reduction of A/P (Contra)"
.adjres24  init      "Discount Earned"
.adjres25  init      "Additional Billing"
.adjres26  init      "Write off of A/R"
.adjres27  init      "Prepayment"
.adjres28  init      "Write off of A/P"
.adjres29  init      "Canadian withholding tax"
..adjres29  init      "Canadian 10% withholding tax"
.adjres30  init      "Taking Credit-Original open"
.adjres31  init      "Credit Transfer"
.adjres32  init      "Refund Credit Taken"
.adjres33  init      "Cancelled/Billing Adjusted "
.adjres34  init      "Adj due to Order Change    "
.adjres35  init      "Court Imposed Bankruptcy Charge"
.adjres36  init      "Bankruptcy, Un-collectible A/R"
.Adjres37  Init      "Void Check"
.adjres99  init      "Entry Correction"
.end patch 1.9
.
. ...........
.DimToForms
DimAR     dim       15
DimAP1    dim       15
DimAP2    dim       15
.begin patch 1.8
DimAP3    dim       15
.end patch 1.8
DimLR     dim       15
DimNIN    dim       15
.begin patch 1.8
DimXnin   Dim       15
.end patch 1.8
DimSTAX   dim       10
DimCTAX   dim       10
DimPOST   dim       10

ExitFlag dim        %%        .Initialized by calling program
LRFlag  init    "N"
NewFlag   init      "N"
ReturnFlag init "N"
InvBFlag init       "N"       .Used to determine if Invoice record was marked as busy
AdjBFlag init       "N"       .Used to determine if Adjusment Master record was marked as busy
ReadFlag form       1
hold      dim       189       .length of JSTVARS
.hold     dim       177       .length of JSTVARS
.START PATCH        1.7       REPLACED LOGIC
.hold3   dim     390     .FOR NOTES
hold3   dim     530     .FOR NOTES
.END PATCH          1.7       REPLACED LOGIC
.START PATCH        1.7       ADDED LOGIC
ndate2    dim       8
ntime2    dim       4
.END PATCH          1.7       ADDED LOGIC
ISInits   init    "DH RW GS "
.ISInits   init    "DH AH DM DB JD "
SecFlag form        "0"
RptCan  dim     1
TabNum    form      "01"
.Objects that are created and destroyed dynamically
.Note that this EditText, generally, fills spot for ErrorMssgStat4!!
EditTextBoxes   EditText (2)
Buttons         Button  (2)
.CheckBoxes      CheckBox (2)
.ComboBoxes      ComboBox (2)
StatTextBoxes   StatText (2)
.ListViews       ListView (2)
.Following Collection is used to dynamically destroy objects listed above
.Each time a form is loaded with those objects, dump them into this collection
.and then destroy whole collection when needed
ObjectColl      Collection

.Colors
white   color
grey    color

AdjustLoadListViews Routine WindPtr
..EXTERNAL ROUTINES FROM NORDTEST.PLC
.OrderLoadNotes2 external "NORDTEST;OrderLoadNotes2"
.OrderLoadNotes external "NORDTEST;OrderLoadNotes"
.OrderEnableOrder4 external "NORDTEST;OrderEnableOrder4"
.OrderDisableOrder4 external "NORDTEST;OrderDisableOrder4"
.
.Open Adjustment Print File
        OPEN        PADJDET,"NINPADJ|10.10.30.103:502"
.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=220:220:220
rpt2           plform  Report2
pss            plform  Passwrd
err            plform  Error
.begin patch 1.4
NADJ001M1           plform  nadj001m                                      ;testing calcs for qty change adjustment
.end patch 1.4
NADJ01A1       plform         NADJ01A1
NADJ001A       plform         NADJ001A
NADJ01A2       plform         NADJ01A2

          formload NADJ001A,WindPtr
          formload NADJ01A1,WindPtr
          formload NADJ01A2,WindPtr
.begin patch 1.4
          formload NADJ001M1,WindPTr
.end patch 1.4
          formload err
          formload pss
          formload rpt2

          AdjustListView.InsertColumn using "##",20,1
          AdjustListView.InsertColumn using "A/R",60,2
        AdjustListView.InsertColumn using "A/P 1",60,3
        AdjustListView.InsertColumn using "A/P 2",60,4
        AdjustListView.InsertColumn using "LR Adj.",60,5
          AdjustListView.InsertColumn using "* LR",60,6
        AdjustListView.InsertColumn using "NIN Adj.",60,7
        AdjustListView.InsertColumn using "* NIN ",60,8
          AdjustListView.InsertColumn using "Credit",40,9
          AdjustListView.InsertColumn using "Bill",40,10
        AdjustListView.InsertColumn using "Reason",45,11
          AdjustListView.InsertColumn using "Date",60,12
          AdjustListView.InsertColumn using "St. Tax",60,13
          AdjustListView.InsertColumn using "City Tax",60,14
        AdjustListView.InsertColumn using "Post. Adj",60,15
          AdjustListView.InsertColumn using "ReUse",45,16
          AdjustListView.InsertColumn using "Other Detail",0,17
        AdjustListView.SetColumnFormat using 1,1
        AdjustListView.SetColumnFormat using 2,1
          AdjustListView.SetColumnFormat using 3,1
          AdjustListView.SetColumnFormat using 4,1
          AdjustListView.SetColumnFormat using 5,1
          AdjustListView.SetColumnFormat using 6,1
.
        AdjustListViewA.InsertColumn using "##",20,1
        AdjustListViewA.InsertColumn using "A/R",65,2
        AdjustListViewA.InsertColumn using "A/P 1 & 2",65,3
        AdjustListViewA.InsertColumn using "LR Adj.",65,4
        AdjustListViewA.InsertColumn using "S/C Tax",65,5
        AdjustListViewA.InsertColumn using "Post",65,6
          AdjustListViewA.InsertColumn using "ReUse",45,7
          AdjustListViewA.InsertColumn using "CRD",35,8
          AdjustListViewA.InsertColumn using "CRTB",40,9
        AdjustListViewA.InsertColumn using "Reason",90,10
          AdjustListViewA.InsertColumn using "Date",60,11
          AdjustListViewA.InsertColumn using "Other Detail",0,12
        AdjustListViewA.SetColumnFormat using 1,1
        AdjustListViewA.SetColumnFormat using 2,1
          AdjustListViewA.SetColumnFormat using 3,1
          AdjustListViewA.SetColumnFormat using 4,1
          AdjustListViewA.SetColumnFormat using 5,1
.
        AdjustListViewB.InsertColumn using "##",20,1
        AdjustListViewB.InsertColumn using "A/R",65,2
        AdjustListViewB.InsertColumn using "A/P 1 & 2",65,3
        AdjustListViewB.InsertColumn using "LR Adj.",65,4
        AdjustListViewB.InsertColumn using "S/C Tax",65,5
        AdjustListViewB.InsertColumn using "Post",65,6
          AdjustListViewB.InsertColumn using "ReUse",45,7
          AdjustListViewB.InsertColumn using "CRD",35,8
          AdjustListViewB.InsertColumn using "CRTB",40,9
        AdjustListViewB.InsertColumn using "Reason",90,10
          AdjustListViewB.InsertColumn using "Date",60,11
          AdjustListViewB.InsertColumn using "Other Detail",0,12
        AdjustListViewB.SetColumnFormat using 1,1
        AdjustListViewB.SetColumnFormat using 2,1
          AdjustListViewB.SetColumnFormat using 3,1
          AdjustListViewB.SetColumnFormat using 4,1
          AdjustListViewB.SetColumnFormat using 5,1
.
        AdjustListViewC.InsertColumn using "##",20,1
        AdjustListViewC.InsertColumn using "A/R",65,2
        AdjustListViewC.InsertColumn using "A/P 1 & 2",65,3
        AdjustListViewC.InsertColumn using "LR Adj.",65,4
        AdjustListViewC.InsertColumn using "S/C Tax",65,5
        AdjustListViewC.InsertColumn using "Post",65,6
          AdjustListViewC.InsertColumn using "ReUse",45,7
          AdjustListViewC.InsertColumn using "CRD",35,8
          AdjustListViewC.InsertColumn using "CRTB",40,9
        AdjustListViewC.InsertColumn using "Reason",90,10
          AdjustListViewC.InsertColumn using "Date",60,11
          AdjustListViewC.InsertColumn using "Other Detail",0,12
        AdjustListViewC.SetColumnFormat using 1,1
        AdjustListViewC.SetColumnFormat using 2,1
          AdjustListViewC.SetColumnFormat using 3,1
          AdjustListViewC.SetColumnFormat using 4,1
          AdjustListViewC.SetColumnFormat using 5,1
.
        deleteitem AdjustComboReason,0
.         move      "  ",str2
.        for N2 from C1 to "38"
.        for N2 from C1 to "39"
        for N2 from C1 to "40"
.                   load      str45 from N2 of str2,adjres1,adjres2,adjres3,adjres4,adjres5:
.                             adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
.                             adjres13,adjres14,adjres15,adjres16,adjres17,adjres18:
.                             adjres19,adjres20,adjres21,adjres22,adjres23,adjres24:
.                             adjres25,adjres26,adjres27,adjres28,adjres29,adjres30:
.                             adjres31,adjres32,adjres33,adjres34,adjres35,adjres36,adjres99
                    call      AdjustLoadCombo using N2,str45
                    sub       C1,N2,N3
                    if (N3 > C0)
.                             if (N3 = "37")
.                             if (N3 = "38")
                              if (N3 = "39")
                                        move      "99",N3
                              endif
                              move      N3,str3
                              call      Trim using str3
                              pack      str55,str3," - ",str45
                    endif
                insertitem AdjustComboReason,N2,str55
        repeat
          setitem   AdjustComboReason,0,1
.
          call      AdjustDisableLower
          call      AdjustDisableButtons2
          call      AdjustDisableButtons3
          move      "N",PassFlag
.Establish initials
          clock     PORT,str3
               move           C0,PORTN
          unpack    str3,str2,str1
          pack      str3,str1,str2
          move      str3,PORTN
               move    C1,NUSEPATH
               move    PORTN,NUSEFLD
               rep     zfill,NUSEFLD
               call    NUSEKEY
          move      NUSEINIT,INITS
          return
AdjustLoadCombo Routine FrmPtr,DimPtr
          load      DimPtr from FrmPtr of "  ",adjres1,adjres2,adjres3,adjres4,adjres5:
                    adjres6,adjres7,adjres8,adjres9,adjres10,adjres11,adjres12:
                    adjres13,adjres14,adjres15,adjres16,adjres17,adjres18:
                    adjres19,adjres20,adjres21,adjres22,adjres23,adjres24:
                    adjres25,adjres26,adjres27,adjres28,adjres29,adjres30:
                    adjres31,adjres32,adjres33,adjres34,adjres35,adjres36,adjres37,adjres38,adjres99
.                   adjres31,adjres32,adjres33,adjres34,adjres35,adjres36,adjres37,adjres99
.                   adjres31,adjres32,adjres33,adjres34,adjres35,adjres36,adjres99
          return

AdjustDisableForm Routine
          deactivate NADJ001A
          deactivate NADJ01A1
          deactivate NADJ01A2
          return
AdjustEnableForm Routine
          activate   NADJ001A
          if (TabNum = 1)
                    activate NADJ01A1
          elseif (TabNum = 2)
                    activate NADJ01A2
          endif
               call           trim using GlobalVar1
               if             (GlobalVar1 <> "" & GlobalVar1 <> " ")
               setitem        AdjustSearchLR,0,GlobalVar1
               getitem        AdjustSearchLR,0,NINVFLD
          call      Trim using NINVFLD
          call      AdjustDisableButtons1
          call      AdjustDisableButtons3
.         call      AdjustDisableAdjustAll2
          move      C0,ReadFlag
          AdjustListView.DeleteAllItems giving N9
                        if (NINVFLD <> "")
.                   move      str6,NINVFLD
                    call      ZFILLIT using NINVFLD,C0
                    call      AdjustLoadScreen
                        endif
                        if (ReadFlag = C0)
                    call      AdjustClearScreen
                    call      AdjustClearTotals
                        endif
          call      AdjustLoadNotes
          call      AdjustEnableButtons1
               endif
          return

AdjustDisableLower
          setprop   AdjustComboCategory,bgcolor=grey,enabled=0
          setprop   AdjustComboCredit,bgcolor=grey,enabled=0
          setprop   AdjustComboReason,bgcolor=grey,enabled=0
          setprop   AdjustEditAP1,bgcolor=grey,enabled=0
          setprop   AdjustEditAP2,bgcolor=grey,enabled=0
.begin patch 1.8
          setprop   AdjustEditAP3,bgcolor=grey,enabled=0
.end patch 1.8
          setprop   AdjustEditAR,bgcolor=grey,enabled=0
          setprop   AdjustEditAdjDate,bgcolor=grey,enabled=0
          setprop   AdjustEditCityTax,bgcolor=grey,enabled=0
          setprop   AdjustEditInvDate,bgcolor=grey,enabled=0
          setprop   AdjustEditInvoice,bgcolor=grey,enabled=0
          setprop   AdjustEditLR,bgcolor=grey,enabled=0
          setprop   AdjustEditLRAdj,bgcolor=grey,enabled=0
          setprop   AdjustEditNINAdj,bgcolor=grey,enabled=0
.begin patch 1.8
          setprop   AdjustEditXNINAdj,bgcolor=grey,enabled=0
.end patch 1.8
          setprop   AdjustEditNumber,bgcolor=grey,enabled=0
          setprop   AdjustEditPostage,bgcolor=grey,enabled=0
          setprop   AdjustEditQty,bgcolor=grey,enabled=0
          setprop   AdjustEditQtyReason,bgcolor=grey,enabled=0
          setprop   AdjustEditReason,bgcolor=grey,enabled=0
          setprop   AdjustEditReuse,bgcolor=grey,enabled=0
          setprop   AdjustEditStateTax,bgcolor=grey,enabled=0
.begin patch 1.4
               setprop        AdjustButton001,enabled=0,visible=0
.end patch 1.4
          return

AdjustEnableLower
          setprop   AdjustComboCategory,bgcolor=white,enabled=1
          setprop   AdjustComboCredit,bgcolor=white,enabled=1
          setprop   AdjustComboReason,bgcolor=white,enabled=1
.START PATCH 1.3 ADDED LOGIC
          setprop   AdjustShortPay,height=0
.END PATCH 1.3 ADDED LOGIC
          setprop   AdjustEditAP1,bgcolor=white,enabled=1
          setprop   AdjustEditAP2,bgcolor=white,enabled=1
.begin patch 1.8
          setprop   AdjustEditAP3,bgcolor=white,enabled=1
.end patch 1.8
          setprop   AdjustEditAR,bgcolor=white,enabled=1
.         setprop   AdjustEditAdjDate,bgcolor=white,enabled=1
          setprop   AdjustEditCityTax,bgcolor=white,enabled=1
.         setprop   AdjustEditInvDate,bgcolor=white,enabled=1
.         setprop   AdjustEditInvoice,bgcolor=white,enabled=1
          setprop   AdjustEditLR,bgcolor=white,enabled=1
          setprop   AdjustEditLRAdj,bgcolor=white,enabled=1
          setprop   AdjustEditNINAdj,bgcolor=white,enabled=1
.begin patch 1.8
          setprop   AdjustEditXNINAdj,bgcolor=white,enabled=1
.end patch 1.8
.         setprop   AdjustEditNumber,bgcolor=white,enabled=1
          setprop   AdjustEditPostage,bgcolor=white,enabled=1
          setprop   AdjustEditQty,bgcolor=white,enabled=1
          setprop   AdjustEditQtyReason,bgcolor=white,enabled=1
          setprop   AdjustEditReason,bgcolor=white,enabled=1
          setprop   AdjustEditReuse,bgcolor=white,enabled=1
          setprop   AdjustEditStateTax,bgcolor=white,enabled=1
.begin patch 1.4
               setprop        AdjustButton001,visible=1,enabled=1
.end patch 1.4
          return

AdjustEnableButtons1
          setprop   AdjustPrint,enabled=1
.         setprop   AdjustSearch,enabled=1
          setprop   AdjustNew,enabled=1
          setprop   AdjustOK,enabled=1
.         return

.AdjustEnableButtons2
          if (ReadFlag = 1)
.                   setprop   AdjustModify,enabled=1
.                   setprop   AdjustDelete,enabled=1
                    setprop   AdjustListView,enabled=1
          endif
          return

AdjustEnableButtons3
          setprop   AdjustSave,enabled=1
          setprop   AdjustQuit,enabled=1
          return

AdjustDisableButtons1
          setprop   AdjustPrint,enabled=0
          setprop   AdjustSearch,enabled=0
          setprop   AdjustNew,enabled=0
          setprop   AdjustOK,enabled=0
AdjustDisableButtons2
.         setprop   AdjustModify,enabled=0
.         setprop   AdjustDelete,enabled=0
          setprop   AdjustReprint,enabled=0
          return

AdjustDisableButtons3
          setprop   AdjustSave,enabled=0
          setprop   AdjustQuit,enabled=0
          setprop   AdjustReprint,enabled=0
          return

AdjustClearScreen
          setitem   AdjustComboCategory,0,1
          setitem   AdjustComboCredit,0,1
          setitem   AdjustComboReason,0,1
.START PATCH 1.3 ADDED LOGIC
          setprop   AdjustShortPay,height=0
.END PATCH 1.3 ADDED LOGIC
          setitem   AdjustEditAP1,0,""
          setitem   AdjustEditAP2,0,""
.begin patch 1.8
          setitem   AdjustEditAP3,0,""
.end patch 1.8
          setitem   AdjustEditAR,0,""
          setitem   AdjustEditAdjDate,0,""
          setitem   AdjustEditCityTax,0,""
          setitem   AdjustEditInvDate,0,""
          setitem   AdjustEditInvoice,0,""
          setitem   AdjustEditLR,0,""
          setitem   AdjustEditLRAdj,0,""
          setitem   AdjustEditNINAdj,0,""
.begin patch 1.8
          setitem   AdjustEditXNINAdj,0,""
.end patch 1.8
          setitem   AdjustEditNumber,0,""
          setitem   AdjustEditPostage,0,""
          setitem   AdjustEditQty,0,""
          setitem   AdjustEditQtyReason,0,""
          setitem   AdjustEditReason,0,""
          setitem   AdjustEditReuse,0,""
          setitem   AdjustEditStateTax,0,""
          setitem   AdjustStatInvStat2,0,""
          return

AdjustClearTotals
          setitem   AdjustStatTotLR2,0,""
          setitem   AdjustStatTotAR2,0,""
          setitem   AdjustStatTotAP1B,0,""
          setitem   AdjustStatTotAP2B,0,""
.begin patch 1.8
          setitem   AdjustStatTotAP3B,0,""
.end patch 1.8
          setitem   AdjustStatTotLRInc2,0,""
          setitem   AdjustStatTotNINInc2,0,""
.begin patch 1.8
          setitem   AdjustStatTotXNINInc2,0,""
.end patch 1.8
          setitem   AdjustStatTotSTax2,0,""
          setitem   AdjustStatTotCTax2,0,""
          setitem   AdjustStatTotPost2,0,""
          return

AdjustLoadScreen
          move    C1,NINVPATH
          move      "A.LoadScreen-NINVKEY",Location
          pack      KeyLocation,"Key: ",NINVFLD
          call      NINVKEY
          if over
                    pack      taskname,"Invoice Record for LR ## ",NINVFLD," does not exist!"
                    alert     caution,taskname,result
                    setfocus AdjustSearchLR
                    return
          endif
          move      NINVFLD,NADJFLD
          move      "A.LoadScreen-NADJTST",Location
          pack      KeyLocation,"Key: ",NADJFLD
          call      NADJTST
          if over
                    pack      taskname,"No Adjustments for LR ## ",NADJFLD
                    alert     caution,taskname,result
                    return
          endif
.begin patch 1.91
          Packkey   Nordfld,Lrn
          move      c1,Nordpath
          call      Nordkey
.end patch 1.91
.         pack      NJSTFLD1,"01X",INVNUM
          call      AdjustCalcTotals using C1,N2
.         pack      NJSTFLD1,"01X",NINVFLD
.         move      "A.LoadScreen-NJSTAIM",Location
.         pack      KeyLocation,"Key: ",NJSTFLD1
.         call      NJSTAIM
.         loop
.                   until over
.                   call      AdjustLoadListView
.                   move      "A.LoadScreen-NJSTKG",Location
.                   call      NJSTKG
.         repeat
          call      AdjustLoadTotal
        AdjustListView.SetItemState GIVING N9 USING *Index=0,*State=3,*Statemask=3
        AdjustListView.EnsureVisible using 0,0
        setfocus AdjustListView
        call        Click_AdjustListView
          move      C1,ReadFlag
          call      AdjustLoadNotes
          return
.
.
. ......TREAT VARIABLES FROM REC. FOR DISPLAY & CALC. PURPOSES....;
.MVEARJ
AdjustLoadListView
          pack      hold,JSTVARS
        AdjustListView.InsertItem giving N9 using JSTSUBNO
          move      JSTAR,str12
          unpack    str12,str9,str3
          if (JSTAR < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAR < C0)
                    pack      DimAR,DASH,str11,str3
          else
                    pack      DimAR,str11,str3
          endif
        AdjustListView.SetItemText using N9,DimAR,1
..............................
          move      JSTAP1,str12
          unpack    str12,str9,str3
          if (JSTAP1 < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAP1 < C0)
                    pack      DimAP1,DASH,str11,str3
          else
                    pack      DimAP1,str11,str3
          endif
        AdjustListView.SetItemText using N9,DimAP1,2
......................................
          move      JSTAP2,str12
          unpack    str12,str9,str3
          if (JSTAP2 < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAP2 < C0)
                    pack      DimAP2,DASH,str11,str3
          else
                    pack      DimAP2,str11,str3
          endif
        AdjustListView.SetItemText using N9,DimAP2,3
...................................................................
          move      JSTLRINC,str12
          unpack    str12,str9,str3
          if (JSTLRINC < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTLRINC < C0)
                    pack      DimLR,DASH,str11,str3
          else
                    pack      DimLR,str11,str3
          endif
        AdjustListView.SetItemText using N9,DimLR,4
...........................................................
.begin patch 1.8
          move      JSTAP3,str12
          unpack    str12,str9,str3
          if (JSTAP3 < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAP3 < C0)
                    pack      DimAP2,DASH,str11,str3
          else
                    pack      DimAP2,str11,str3
          endif
        AdjustListView.SetItemText using N9,DimAP3,5
.end patch 1.8
          move      JSTNININC,str12
          unpack    str12,str9,str3
          if (JSTNININC < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTNININC < C0)
                    pack      DimNIN,DASH,str11,str3
          else
                    pack      DimNIN,str11,str3
          endif
.begin patch 1.8
.        AdjustListView.SetItemText using N9,DimNIN,5
        AdjustListView.SetItemText using N9,DimNIN,6
.end patch 1.8
.begin patch 1.8
          move      JSTXNINC,str12
          unpack    str12,str9,str3
          if (JSTXNINC < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTXNINC < C0)
                    pack      DimXNIN,DASH,str11,str3
          else
                    pack      DimXNIN,str11,str3
          endif
        AdjustListView.SetItemText using N9,DimXNIN,7
.end patch 1.8
.Begin patch 1.8
.    AdjustListView.SetItemText using N9,JSTCD,6
.        AdjustListView.SetItemText using N9,JSTCRCT,7
.
          AdjustListView.SetItemText using N9,JSTCD,8
          AdjustListView.SetItemText using N9,JSTCRCT,9
.end patch 1.8
          move      C0,N2
          call      Trim using JSTREASN
          if (JSTREASN = "99")
."37" plus "1" to account for the first entry being blank
                    move      "38",N2
          else
                    move      JSTREASN,N2
                    add       C1,N2
          endif
          clear     str45
          call      AdjustLoadCombo using N2,str45
          call      Trim using str45
.Begin patch 1.8
.        AdjustListView.SetItemText using N9,str45,8
        AdjustListView.SetItemText using N9,str45,10
.end patch 1.8
          unpack    JSTDATE,str2,YY,MM,DD
          call      Trim using MM
          if (MM <> "")
                    pack      str10,MM,SLASH,DD,SLASH,str2,YY
          else
                    clear     str10
          endif
.Begin patch 1.8
.        AdjustListView.SetItemText using N9,str10,9
        AdjustListView.SetItemText using N9,str10,11
.end patch 1.8
          move      JSTSTAX,str12
          unpack    str12,str9,str3
          if (JSTSTAX < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTSTAX < C0)
                    pack      DimSTAX,DASH,str11,str3
          else
                    pack      DimSTAX,str11,str3
          endif
.Begin patch 1.8
.        AdjustListView.SetItemText using N9,DimSTAX,10
        AdjustListView.SetItemText using N9,DimSTAX,12
.end patch 1.8
          move      JSTCTAX,str12
          unpack    str12,str9,str3
          if (JSTCTAX < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTCTAX < C0)
                    pack      DimCTAX,DASH,str11,str3
          else
                    pack      DimCTAX,str11,str3
          endif
.Begin patch 1.8
.        AdjustListView.SetItemText using N9,DimCTAX,11
        AdjustListView.SetItemText using N9,DimCTAX,13
.Begin patch 1.8
          move      JSTPOST,str12
          unpack    str12,str9,str3
          if (JSTPOST < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTPOST < C0)
                    pack      DimPOST,DASH,str11,str3
          else
                    pack      DimPOST,str11,str3
          endif
.Begin patch 1.8
.        AdjustListView.SetItemText using N9,DimPOST,12
.        AdjustListView.SetItemText using N9,JSTREUSE,13
.        AdjustListView.SetItemText using N9,hold,14
        AdjustListView.SetItemText using N9,DimPOST,14
        AdjustListView.SetItemText using N9,JSTREUSE,15
        AdjustListView.SetItemText using N9,hold,16
.end patch 1.8
          return

AdjustLoadInvStats
          if (STATB = "P")
                    move      "Invoice has been Paid!",str25
          else
                    clear     str25
          endif
          setitem   AdjustStatInvStat2,0,str25
.
          call      Trim using INVDTEM
          if (INVDTEM <> "")
                    pack      str10,INVDTEM,SLASH,INVDTED,SLASH,INVDTEC,INVDTEY
          else
                    clear     str10
          endif
.
          setitem   AdjustEditInvDate,0,str10
          return

AdjustLoadScreen1
          move    C1,NINVPATH
          move      "A.LoadScreen1-NINVKEY",Location
          pack      KeyLocation,"Key: ",NINVFLD
          call      NINVKEY
          setitem   AdjustEditInvoice,0,INVNUM
.
          call      AdjustLoadInvStats
.
          setitem   AdjustEditLR,0,JSTLR
.
          setitem   AdjustEditNumber,0,JSTSUBNO
.
          call      Trim using JSTDATE
          if (JSTDATE <> "")
                    unpack    JSTDATE,str2,YY,MM,DD
                    pack      str10,MM,SLASH,DD,SLASH,str2,YY
          else
                    clear     str10
          endif
          setitem   AdjustEditAdjDate,0,str10
.
          move      JSTAR,str12
          unpack    str12,str9,str3
          if (JSTAR < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAR < C0)
                    pack      DimAR,DASH,str11,str3
          else
                    pack      DimAR,str11,str3
          endif
          setitem   AdjustEditAR,0,DimAR
.
          move      JSTAP1,str12
          unpack    str12,str9,str3
          if (JSTAP1 < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAP1 < C0)
                    pack      DimAP1,DASH,str11,str3
          else
                    pack      DimAP1,str11,str3
          endif
          setitem   AdjustEditAP1,0,DimAP1
.
          move      JSTAP2,str12
          unpack    str12,str9,str3
          if (JSTAP2 < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAP2 < C0)
                    pack      DimAP2,DASH,str11,str3
          else
                    pack      DimAP2,str11,str3
          endif
          setitem   AdjustEditAP2,0,DimAP2
.
.begin patch 1.8
          move      JSTAP3,str12
          unpack    str12,str9,str3
          if (JSTAP3 < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTAP3 < C0)
                    pack      DimAP3,DASH,str11,str3
          else
                    pack      DimAP3,str11,str3
          endif
          setitem   AdjustEditAP3,0,DimAP3
.end patch 1.8
.
          move      JSTLRINC,str12
          unpack    str12,str9,str3
          if (JSTLRINC < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTLRINC < C0)
                    pack      DimLR,DASH,str11,str3
          else
                    pack      DimLR,str11,str3
          endif
          setitem   AdjustEditLRAdj,0,DimLR
.
          move      JSTNININC,str12
          unpack    str12,str9,str3
          if (JSTNININC < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTNININC < C0)
                    pack      DimNIN,DASH,str11,str3
          else
                    pack      DimNIN,str11,str3
          endif
          setitem   AdjustEditNINAdj,0,DimNIN
.bEGIN PATCH 1.8

          move      JSTXNINC,str12
          unpack    str12,str9,str3
          if (JSTXNINC < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTXNINC < C0)
                    pack      DimxNIN,DASH,str11,str3
          else
                    pack      DimxNIN,str11,str3
          endif
          setitem   AdjustEditxNINAdj,0,DimxNIN
.
.END PATCH 1.8
.
          move      JSTSTAX,str12
          unpack    str12,str9,str3
          if (JSTSTAX < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTSTAX < C0)
                    pack      DimSTAX,DASH,str11,str3
          else
                    pack      DimSTAX,str11,str3
          endif
          setitem   AdjustEditStateTax,0,DimSTAX
.
          move      JSTCTAX,str12
          unpack    str12,str9,str3
          if (JSTCTAX < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTCTAX < C0)
                    pack      DimCTAX,DASH,str11,str3
          else
                    pack      DimCTAX,str11,str3
          endif
          setitem   AdjustEditCityTax,0,DimCTAX
.
          move      JSTPOST,str12
          unpack    str12,str9,str3
          if (JSTPOST < C0)
                    call      RemoveChar using str9,DASH
          endif
          call      Trim using str9
          if (str9 <> "")
                    call      FormatNumeric using str9,str11
          else
                    clear     str11
          endif
          if (JSTPOST < C0)
                    pack      DimPOST,DASH,str11,str3
          else
                    pack      DimPOST,str11,str3
          endif
          setitem   AdjustEditPostage,0,DimPOST
.
          setitem   AdjustEditReuse,0,JSTREUSE
.
          move      C0,N9
          if (JSTCD = "C")
                    move      C2,N9
          elseif (JSTCD = "D")
                    move      C3,N9
          endif
          setitem   AdjustComboCredit,0,N9
.
          move      C0,N9
          move      JSTCRCT,N9
          add       C1,N9
          setitem   AdjustComboCategory,0,N9
.
          move      JSTREASN,str2
          call      Trim using str2
          if (str2 <> "")
                    rep       zfill,str2
          endif
          setitem   AdjustEditReason,0,str2
.
          move      C0,N4
          move      str2,N4
          move      C1,N3
          for N2 from C1 to "50"
                    getitem   AdjustComboReason,N2,str2
                    call      Trim using str2
                    move      C0,N5
                    move      str2,N5
                    if (N5 = N4)
                              move      N2,N3
                              break
                    endif
          repeat
          setitem   AdjustComboReason,0,N3
.START PATCH 1.3 ADDED LOGIC
          if (N4 = 16)        .Short Payment
                    setprop   AdjustShortPay,height=20
          else
                    setprop   AdjustShortPay,height=0
          endif
.END PATCH 1.3 ADDED LOGIC
.
          move      JSTQTY,str9
          call      FormatNumeric using str9,str11
          setitem   AdjustEditQty,0,str11
.
          setitem   AdjustEditQtyReason,0,JSTQRSN
.
          call      AdjustTestPrintFile
          if over
                    setprop   AdjustReprint,enabled=1
          else
                    setprop   AdjustReprint,enabled=0
          endif
          return

AdjustLoadTotal
          setitem   AdjustStatTotLR2,0,NINVFLD
          AdjustListView.GetItemCount giving N9
          move      N9,str9
          call      Trim using str9
          call      FormatNumeric using str9,str11
          pack      str45,str11," Record(s) read."
          setitem   AdjustStatRecords,0,str45
.
          move      TOTAR,str13
          unpack    str13,str10,str3
          if (TOTAR < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTAR < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotAR2,0,str25
.
          move      TOTAP1,str13
          unpack    str13,str10,str3
          if (TOTAP1 < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTAP1 < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotAP1B,0,str25
.
          move      TOTAP2,str13
          unpack    str13,str10,str3
          if (TOTAP2 < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTAP2 < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotAP2B,0,str25

.BEGIN PATCH 1.8
          move      TOTAP3,str13
          unpack    str13,str10,str3
          if (TOTAP3 < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTAP3 < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotAP3B,0,str25
.END PATCH 1.8
.
          move      TOTLR,str13
          unpack    str13,str10,str3
          if (TOTLR < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTLR < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotLRInc2,0,str25
.
          move      TOTNIN,str13
          unpack    str13,str10,str3
          if (TOTNIN < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTNIN < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotNINInc2,0,str25
.BEGIN PATCH 1.8
          move      TOTXNIN,str13
          unpack    str13,str10,str3
          if (TOTXNIN < C0)
                    call      RemoveChar using str10,DASH
          endif
          call      Trim using str10
          if (str10 <> "")
                    call      FormatNumeric using str10,str13
          else
                    clear     str13
          endif
          if (TOTXNIN < C0)
                    pack      str25,DASH,str13,str3
          else
                    pack      str25,str13,str3
          endif
          setitem   AdjustStatTotXNINInc2,0,str25
.END PATCH 1.8

.
          move      TOTSTX,str8
          unpack    str8,str5,str3
          if (TOTSTX < C0)
                    call      RemoveChar using str5,DASH
          endif
          call      Trim using str5
          if (str5 <> "")
                    call      FormatNumeric using str5,str6
          else
                    clear     str6
          endif
          if (TOTSTX < C0)
                    pack      str25,DASH,str6,str3
          else
                    pack      str25,str6,str3
          endif
          setitem   AdjustStatTotSTax2,0,str25
.
          move      TOTCTX,str8
          unpack    str8,str5,str3
          if (TOTCTX < C0)
                    call      RemoveChar using str5,DASH
          endif
          call      Trim using str5
          if (str5 <> "")
                    call      FormatNumeric using str5,str6
          else
                    clear     str6
          endif
          if (TOTCTX < C0)
                    pack      str25,DASH,str6,str3
          else
                    pack      str25,str6,str3
          endif
          setitem   AdjustStatTotCTax2,0,str25
.
          move      TOTPOST,str8
          unpack    str8,str5,str3
          if (TOTPOST < C0)
                    call      RemoveChar using str5,DASH
          endif
          call      Trim using str5
          if (str5 <> "")
                    call      FormatNumeric using str5,str6
          else
                    clear     str6
          endif
          if (TOTPOST < C0)
                    pack      str25,DASH,str6,str3
          else
                    pack      str25,str6,str3
          endif
          setitem   AdjustStatTotPost2,0,str25
          return

AdjustVerifyData
          move      NO,CDE25SW
.
          getitem   AdjustEditLR,0,NINVFLD
          call      Trim using NINVFLD
          if (NINVFLD = "")
                    alert     caution,"You must enter a valid LR Number!",result
                    setfocus AdjustEditLR
                    move      YES,ReturnFlag
                    return
          else
                    call      ZFillIt using NINVFLD,C0
                    move    C1,NINVPATH
                    move    "Verify-NINVKEY",Location
                  pack    KeyLocation,"Key: ",NINVFLD
                    call    NINVKEY
                    if over
                              alert     caution,"LR has not been Invoiced!",result
                              setfocus AdjustEditLR
                              move      YES,ReturnFlag
                              return
                    elseif (STATB = "P")
                              clear     taskname
                              append    "Invoice has already been Paid!",taskname
                              append    NewLine,taskname
                              append    "Are you sure you want to continue?",taskname
                              reset     taskname
                              alert     plain,taskname,result
                              if (result <> 1)
                                        setfocus AdjustEditLR
                                        move      YES,ReturnFlag
                                        return
                              endif
                              move      YES,CDE25SW
                    endif
          endif
          if (CODE = STAR)
                    Clear     Taskname
                    append    "Invoice is currently Busy! ",Taskname
                    append    NewLine,taskname
                    append    "Are you sure you want to continue?",taskname
                    reset     taskname
.                    alert     caution,Taskname,result
                    alert     plain,Taskname,result
                              if (result <> 1)
                              setfocus AdjustEditLR
                              move      YES,ReturnFlag
                              return               
                              endif
          else
                    call      AdjustMarkInvBusy
          endif
          move      NINVFLD,NADJFLD
          move      "Verify-NADJKEY",Location
          pack      KeyLocation,"Key: ",NADJFLD
          call      NADJKEY
          if not over
                              if (ASCODE = STAR)
                              clear     taskname
                              append    "Master Adjustment is currently Busy!  Yes to continue.",taskname
                              append    NewLine,taskname
                              append    "Else:  I will set associated Invoice record as Busy!",taskname
                              reset     taskname
                              alert     plain,Taskname,result
                              if (result <> 1)
                              call      AdjustMarkInvBusy
                              move      NO,InvBFlag         .Do not let it get cleared
                              setfocus AdjustEditLR
                              move      YES,ReturnFlag
                              return
                              endif
                    else
                              call      AdjustMarkAdjBusy
                    endif
          endif
.Get Current Totals/Next Number
          call      AdjustCalcTotals using C0,N2
          if (N2 = SEQ)
                    goto      Click_AdjustQuit
          endif
          if (NewFlag = YES)
                    move      N2,JSTSUBNO
                    rep       zfill,JSTSUBNO
          endif
.
          move      NINVFLD,JSTLR
          move      INVNUM,JSTINVNO
          pack      JSTINVDT,INVDTEC,INVDTEY,INVDTEM,INVDTED
.Following fields currently are not Verified
.         getitem   AdjustEditAdjDate,0,str10
.
.         getitem   AdjustEditNumber,0,str2
.
.         getitem   AdjustEditInvDate,0,str10
.
.         getitem   AdjustEditInvoice,0,str6
.
          getitem   AdjustEditAR,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          move      C0,JSTAR
          move      str15,JSTAR
.
          getitem   AdjustEditAP1,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          call      RemoveChar using DASH,COMMA
          move      C0,JSTAP1
          move      str15,JSTAP1
.
          getitem   AdjustEditAP2,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          move      C0,JSTAP2
          move      str15,JSTAP2
.
.BEGIN PATCH 1.8

          getitem   AdjustEditAP3,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          move      C0,JSTAP3
          move      str15,JSTAP3
.END PATCH 1.8
.
          getitem   AdjustEditLRAdj,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          move      C0,JSTLRINC
          move      str15,JSTLRINC
.
          getitem   AdjustEditNINAdj,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          move      C0,JSTNININC
          move      str15,JSTNININC
.
.BEGIN PATCH 1.8
          getitem   AdjustEditxNINAdj,0,str15
          call      Trim using str15
          call      RemoveChar using str15,COMMA
          move      C0,JSTxNINC
          move      str15,JSTxNINC
.END PATCH 1.8

          getitem   AdjustEditStateTax,0,str10
          call      Trim using str10
          call      RemoveChar using str10,COMMA
          move      C0,JSTSTAX
          move      str10,JSTSTAX
.
          getitem   AdjustEditCityTax,0,str10
          call      Trim using str10
          call      RemoveChar using str10,COMMA
          move      C0,JSTCTAX
          move      str10,JSTCTAX
.
          getitem   AdjustEditPostage,0,str10
          call      Trim using str10
          call      RemoveChar using str10,COMMA
          move      C0,JSTPOST
          move      str10,JSTPOST
.
          getitem   AdjustEditReuse,0,JSTREUSE
.
          getitem   AdjustComboCredit,0,result
          if (result = 2)
                    move      "C",JSTCD
          elseif (result = 3)
                    move      "D",JSTCD
          else
                    alert     caution,"You must supply a Credit Code!",result
                    setfocus AdjustComboCredit
                    move      YES,ReturnFlag
                    return
          endif
.
          getitem   AdjustComboCategory,0,result
          getitem   AdjustComboCategory,result,JSTCRCT
.
.         getitem   AdjustEditReason,0,str2
          getitem   AdjustComboReason,0,result
          getitem   AdjustComboReason,result,str45
          unpack    str45,str5,str40
          if (str40 = "Not Used!")
                    move      "  ",JSTREASN
                    rep       zfill,JSTREASN
          else
                    move      str5,JSTREASN
                    call      Trim using JSTREASN
                    move      C0,N2
                    move      JSTREASN,N2
                    move      N2,JSTREASN
                    rep       zfill,JSTREASN
.                   if (CDE25SW = YES & JSTREASN <> "25")
                    if (CDE25SW = YES & (JSTREASN <> "25" & JSTREASN <> "37"))
                              alert     caution,"You must use reason '25'or '37' for a Paid Invoice!",result
                              setfocus AdjustComboReason
                              move      YES,ReturnFlag
                              return
                    endif
          endif
          if (JSTREASN = "00" | JSTREASN = "06")
                    alert     caution,"You must use a valid Reason!",result
                    setfocus AdjustComboReason
                    move      YES,ReturnFlag
                    return
          endif
          if (JSTREASN = "14")
                    move      C0,xfoot
                    move      AP1,xfoot
                    add       totap1,xfoot             .add previous adj's
                    add       JSTAP1,xfoot
                    if (xfoot <> C0)
                              clear     taskname
                              append    "Does Not Balance!!!!",taskname
                              append    NewLine,taskname
                              append    "AP billed:    ",taskname
                              append    AP1,taskname
                              append    NewLine,taskname
                              append    "This ADJ.:    ",taskname
                              append    JSTAP1,taskname
                              append    NewLine,taskname
                              append    "TOTADJ:       ",taskname
                              append    totap1,taskname
                              append    b1,taskname
                              append    xfoot,taskname
                              reset     taskname
                              alert     caution,taskname,result
                              scan      INITS,ISInits
                              if equal
                                        move      C0,SecFlag
                                        call      AdjustSetISPassword
                                    setprop Report2,visible=1
                                        if (SecFlag = C0)
                                                  setfocus AdjustComboReason
                                                  move      YES,ReturnFlag
                                                  return
                                        endif
                              else
                                        setfocus AdjustComboReason
                                        move      YES,ReturnFlag
                                        return
                              endif
                    endif
          elseif (JSTREASN = "16")
. make sure only a/r was adjusted.
                move    c0,xfoot
                add     JSTAR,xfoot
                add     JSTAP1,xfoot
                add     JSTAP2,xfoot
.BEGIN PATCH 1.8
                add     JSTAP3,xfoot
.END PATCH 1.8
                add     JSTLRINC,xfoot
                add     JSTNININC,xfoot
.BEGIN PATCH 1.8
              add   JSTXNINC,xfoot
.END PATCH 1.8
                add     JSTSTAX,xfoot
                add     JSTCTAX,xfoot
                add     JSTPOST,xfoot
                sub JSTAR,xfoot
                    if (xfoot <> C0)
                              alert     caution,"Does Not Balance!!!!  Only A/R adjustment allowed!",result
                              scan      INITS,ISInits
                              if equal
                                        move      C0,SecFlag
                                        call      AdjustSetISPassword
                                    setprop Report2,visible=1
                                        if (SecFlag = C0)
                                                  setfocus AdjustComboReason
                                                  move      YES,ReturnFlag
                                                  return
                                        endif
                              else
                                        setfocus AdjustComboReason
                                        move      YES,ReturnFlag
                                        return
                              endif
                    endif
.begin 1.92
          elseif (JSTREASN = "38")
. make sure only a/r was adjusted.
                move    c0,xfoot
                add     JSTAR,xfoot
                add     JSTAP1,xfoot
                add     JSTAP2,xfoot
                add     JSTAP3,xfoot
                add     JSTLRINC,xfoot
                add     JSTNININC,xfoot
              add   JSTXNINC,xfoot
                add     JSTSTAX,xfoot
                add     JSTCTAX,xfoot
                add     JSTPOST,xfoot
                sub JSTAR,xfoot
                    if (xfoot <> C0)
                              alert     caution,"Does Not Balance!!!!  Only A/R adjustment allowed!",result
                              scan      INITS,ISInits
                              if equal
                                        move      C0,SecFlag
                                        call      AdjustSetISPassword
                                    setprop Report2,visible=1
                                        if (SecFlag = C0)
                                                  setfocus AdjustComboReason
                                                  move      YES,ReturnFlag
                                                  return
                                        endif
                              else
                                        setfocus AdjustComboReason
                                        move      YES,ReturnFlag
                                        return
                              endif
                    endif
.end 1.92
          else
                    move      c0,xfoot
                    add       JSTAR,xfoot
                  sub         JSTAP1,xfoot
                  sub         JSTAP2,xfoot
.BEGIN PATCH 1.8
                    sub JSTAP3,xfoot
.END PATCH 1.8
                  sub         JSTLRINC,xfoot
                  sub         JSTNININC,xfoot
.BEGIN PATCH 1.8
                  Sub         JSTXNINC,xfoot
.END PATCH 1.8
                  sub         JSTSTAX,xfoot
                  sub         JSTCTAX,xfoot
                  sub         JSTPOST,xfoot
                    if (xfoot <> C0)
                              alert     caution,"Does Not Balance!!!!",result
                              scan      INITS,ISInits
                              if equal
                                        move      C0,SecFlag
                                        call      AdjustSetISPassword
                                    setprop Report2,visible=1
                                        if (SecFlag = C0)
                                                  setfocus AdjustComboReason
                                                  move      YES,ReturnFlag
                                                  return
                                        endif
                              else
                                        setfocus AdjustComboReason
                                        move      YES,ReturnFlag
                                        return
                              endif
                    endif
          endif
.
          getitem   AdjustEditQty,0,JSTQTY
          call      Trim using JSTQTY
          call      RemoveChar using JSTQTY,COMMA
.
          getitem   AdjustEditQtyReason,0,JSTQRSN
          call      Trim using JSTQRSN
          return

AdjustCalcTotals LRoutine FrmPtrA,FrmPtr1
          move      C0,TOTAR
          move      C0,TOTAP1
          move      C0,TOTAP2
.BEGIN PATCH 1.8
          move      C0,TOTAP3
.END PATCH 1.8
          move      C0,TOTLR
          move      C0,TOTnin
.BEGIN PATCH 1.8
          move      C0,TOTXnin
.END PATCH 1.8
          move      C0,TOTSTX
          move      C0,TOTCTX
          move      C0,TOTPOST
          move      C1,FrmPtr1
          pack      NJSTFLD1,"01X",NINVFLD
          move      "A.CalcAP-NJSTAIM",Location
          pack      KeyLocation,"Key: ",NJSTFLD1
          call      NJSTAIM
          loop
                    until over
                    add       JSTAR,TOTAR
                    add       JSTAP1,TOTAP1
                    add       JSTAP2,TOTAP2
.BEGIN PATCH 1.8
                    add       JSTAP3,TOTAP3
.END PATCH 1.8
                    add       JSTLRINC,TOTLR
                    add       JSTNININC,TOTNIN
.BEGIN PATCH 1.8
                    add       JSTXNINC,TOTXNIN
.END PATCH 1.8
                    add       JSTSTAX,TOTSTX
                    add       JSTCTAX,TOTCTX
                    add       JSTPOST,TOTPOST
                    if (FrmPtr1 >= 98 AND NewFlag = YES)
                              clear     taskname
                              append    "You already have maximum number of Adjustments!",taskname
                              append    NewLine,taskname
                              append    "This record will NOT be saved!",taskname
                              reset     taskname
                              alert     caution,taskname,result
                              move      SEQ,FrmPtr1
                    else
                              add       C1,FrmPtr1
                    endif
                    if (FrmPtrA = C1)
                              call      AdjustLoadListView
                    endif
                    move      "A.CalcAP-NJSTKG",Location
                    call      NJSTKG
          repeat
          return

AdjustMarkInvBusy
          move    C1,NINVPATH
          move    "A.MarkBusy-NINVUPD",Location
        pack    KeyLocation,"Key: ",NINVFLD
          move      "*",CODE
          call      NINVUPD
          move      YES,InvBFlag
          return

AdjustMarkInvFree
          move    C1,NINVPATH
          move    "A.MarkFree-NINVUPD",Location
        pack    KeyLocation,"Key: ",NINVFLD
          move      "F",CODE
          call      NINVUPD
          move      NO,InvBFlag
          return

AdjustMarkAdjBusy
          move      "A.MarkFree-NADJUPD",Location
        pack    KeyLocation,"Key: ",NADJFLD
          move      STAR,ASCODE
          call      NADJUPD
          move      YES,AdjBFlag
          return

AdjustMarkAdjFree
          move      "A.MarkFree-NADJUPD",Location
        pack    KeyLocation,"Key: ",NADJFLD
          move      "J",ASCODE
          call      NADJUPD
          move      NO,AdjBFlag
          return

AdjustTestPrintFile
.Adjustment Print File
          pack      INVNOA,JSTINVNO,JSTSUBNO
          rep       ZFILL,INVNOA
          move      "A.TestPrint-read Padjdet",Location
          pack      KeyLocation,"Key: ",INVNOA
          trap      IOMssg giving Error if IO
          filepi    1;PADJDET
          read      PADJDET,INVNOA;;
          trapclr   IO
          return

AdjustWriteToPrintFile
          move      "J",JSTBUSY
          pack      INVNOA,JSTINVNO,JSTSUBNO
          rep       ZFILL,INVNOA
          move      "A.WritePrint-write Padjdet",Location
          pack      KeyLocation,"Key: ",INVNOA
          trap      IOMssg giving Error if IO
          filepi    1;PADJDET
          write     PADJDET,INVNOA;JSTVARS,INITS
          trapclr   IO
          return

AdjustSetISPassword
        call    Report2DestroyObjects
.         setprop   Report2OK,height=0
        setprop Report2,title="NIN Administrator Mode Password"
        move    NO,RptCan
        create  Report2;StatTextBoxes(1)=50:70:10:110,"Password",""
        create  Report2;StatTextBoxes(2)=90:110:10:110,"",""
        create  Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=10,EditType=5,SelectAll=1,Style=1,Border=1,Password=1
        create  Report2;Buttons(1)=205:230:50:100,"O&K",zorder=500,default=1
        activate StatTextBoxes(1)
        activate StatTextBoxes(2)
        activate EditTextBoxes(1)
        activate Buttons(1),AdjustAdminOK,result
        listins ObjectColl,StatTextBoxes(1),StatTextBoxes(2),EditTextBoxes(1),Buttons(1)
        setfocus EditTextBoxes(1)
        return

AdjustAdminOK
        getitem EditTextBoxes(1),0,str10
        call    Trim using str10
        if (str10 = "")
                setitem StatTextBoxes(2),0,"Not a valid Password!!"
                setfocus EditTextBoxes(1)
        else
                    if (str10 = "DANGER")
                              move      C1,SecFlag
                    endif
                setprop Report2,visible=0
        endif
        return

..Routine which destroys all objects created from above routines
Report2DestroyObjects
        destroy ObjectColl
        return

AdjustFormatNumeric LRoutine EditPtr,FrmPtr
          if (FrmPtr = C1)
                    getitem   EditPtr,0,str15
                    call      RemoveChar using str15,COMMA
                    scan      DASH,str15
                    if equal
                              move      DASH,str1
                              reset     str15
                    else
                              clear     str1
                    endif
                    call      RemoveChar using str15,DASH
                    move      C0,N92
                    move      str15,N92
                    move      N92,str15
                    unpack    str15,str9,str3
                    call      Trim using str9
                    if (str9 <> "")
                              call      FormatNumeric using str9,str11
                    else
                              clear     str11
                    endif
                    pack      str15,str1,str11,str3
                    setitem   EditPtr,0,str15
          elseif (FrmPtr = C2)
                    getitem   EditPtr,0,str10
                    call      RemoveChar using str10,COMMA
                    scan      DASH,str10
                    if equal
                              move      DASH,str1
                              reset     str10
                    else
                              clear     str1
                    endif
                    call      RemoveChar using str10,DASH
                    move      C0,N52
                    move      str10,N52
                    move      N52,str10
                    unpack    str10,str5,str3
                    call      Trim using str5
                    if (str5 <> "")
                              call      FormatNumeric using str5,str6
                    else
                              clear     str6
                    endif
                    pack      str10,str1,str6,str3
                    setitem   EditPtr,0,str10
          endif
          return

AdjustDisableSave
        setitem AdjustSave,0,"Save"
        return
AdjustEnableSave
        setitem AdjustSave,0,"Sa&ve"
        return

AdjustSwitchTab LRoutine FrmPtr
        if (TabNum <> FrmPtr)
                move    TabNum,N2
                call    AdjustTabClick
                move    FrmPtr,N2
                call    AdjustTabChange
                setitem AdjustTabControl001,0,FrmPtr
        endif
        return

AdjustTabClick
        if (N2 = C1)
                Deactivate Nadj01a1
        elseif (N2 = C2)
                Deactivate Nadj01a2
        elseif (N2 = C3)
        elseif (N2 = C4)
        elseif (N2 = C5)
          endif
        return

AdjustTabChange
        move    N2,TabNum
        if (N2 = C1)
                Activate Nadj01a1
        elseif (N2 = C2)
                Activate Nadj01a2
                    setprop   Adjust2New,default=1
        elseif (N2 = C3)
        elseif (N2 = C4)
        elseif (N2 = C5)
        endif
        return

AdjustSetSaveDefault
          setprop   AdjustSave,default=1
          return

.LOGIC FOR NOTES SCREEN
AdjustEnableAdjust2
.Called by Adjust2New_Click
        setprop Adjust2EditLR,enabled=1
.START PATCH        1.7       REPLACED LOGIC
.        setprop Adjust2EditNote1,enabled=1
.        setprop Adjust2EditNote2,enabled=1
.        setprop Adjust2EditNote3,enabled=1
.        setprop Adjust2EditNote4,enabled=1
.        setprop Adjust2EditNote5,enabled=1
.        setprop Adjust2EditNote6,enabled=1
           setprop NADJ01A2EDITNOTES, enabled=1
           setprop NADJ01A2EDITNOTES,readonly=C0  // turn off read-only if it's on
.START PATCH        1.7       REPLACED LOGIC
          setprop   Adjust2DataList,enabled=0
        return

AdjustDisableAdjustAll2
.called by:  AdjustLoadNotes
        setprop Adjust2Cancel,enabled=0
        setprop Adjust2Save,enabled=0
        setprop Adjust2New,enabled=0
AdjustDisableAdjust2
.called by:  Adjust2Cancel_Click, Adjust2Save_Click
        setprop Adjust2EditLR,enabled=0
.START PATCH        1.7       REPLACED LOGIC
.        setprop Adjust2EditNote1,enabled=0
.        setprop Adjust2EditNote2,enabled=0
.        setprop Adjust2EditNote3,enabled=0
.        setprop Adjust2EditNote4,enabled=0
.        setprop Adjust2EditNote5,enabled=0
.        setprop Adjust2EditNote6,enabled=0
          setprop NADJ01A2EDITNOTES, enabled=1  // allow scrolling
          setprop NADJ01A2EDITNOTES,readonly=C1 // but  don't allow writing
.END PATCH          1.7       REPLACED LOGIC
          setprop   Adjust2DataList,enabled=1
        return

AdjustLoadNotes
        call    AdjustDisableAdjustAll2
        clear   hold3
        deleteitem Adjust2DataList,0
        move    NINVFLD,NONOFLD
        move    "A.LoadNotes-NONOKEY",Location
        pack    KeyLocation,"Key: ",NONOFLD
        call    NONOKEY
        if not over
                unpack  NTIME,str2,str3
                pack    str5,str2,COLON,str3
                unpack  NDATE,MM,DD,YY,STR2
.START PATCH        1.7       REPLACED LOGIC
.                pack    hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NLINE1:
.                        NLINE2,NLINE3,NLINE4,NLINE5,NLINE6,NINITS
                    pack      hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,STR5,B1,NINITS,NDATE,NTIME
. STR5 ABOVE IS TIME, STR2 IS CC
.END PATCH          1.7       REPLACED LOGIC
                insertitem Adjust2DataList,0,hold3
                loop
                        move    "A.LoadNotes-NONOKS",Location
                        pack    KeyLocation,"Key: ",NONOFLD
                        call    NONOKS
                        until over
                        until (NOTEKEY <> NONOFLD)
                        unpack  NTIME,str2,str3
                        pack    str5,str2,COLON,str3
                        unpack  NDATE,MM,DD,YY,STR2
.START PATCH        1.7       REPLACED LOGIC
.                        pack    hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1:
.                                NLINE1,NLINE2,NLINE3,NLINE4,NLINE5,NLINE6,NINITS
                              pack    hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NINITS,NDATE,NTIME
.END PATCH          1.7       REPLACED LOGIC
                        insertitem Adjust2DataList,0,hold3
                repeat
                setitem Adjust2DataList,1,1
        endif
          setprop   Adjust2New,enabled=1
        call    AdjustLoadNotes2
        return

AdjustLoadNotes2
.START PATCH 1.7 ADDED LOGIC
          Adjust2DataList.GetCount giving result
          if (result = 0)
                    setitem Adjust2EditLR,0,""
                    setitem   Adjust2EditDate,0,""
                    setitem   Adjust2EditTime,0,""
                    setitem   Adjust2EditInits,0,""
                    setitem   NADJ01A2EDITNOTES,0,""
                    return
          endif
.END PATCH 1.7 ADDED LOGIC
        getitem Adjust2DataList,0,result
        getitem Adjust2DataList,result,hold3
.START PATCH        1.7       REPLACED LOGIC
.        unpack  hold3,NOTEKEY,str1,str10,str2,str5,str1,NLINE1,NLINE2,NLINE3:
.                NLINE4,NLINE5,NLINE6,NINITS
        unpack  hold3,NOTEKEY,str1,str10,str2,str5,str1,NINITS,NDATE,NTIME
.END PATCH          1.7       REPLACED LOGIC

        call    Trim using str10
        count   N2,str10
        if (N2 <> 10)
                clear   str10
        endif
        setitem Adjust2EditLR,0,NOTEKEY
        setitem Adjust2EditDate,0,str10
        setitem Adjust2EditTime,0,str5
        setitem Adjust2EditInits,0,NINITS

.START PATCH        1.7       REPLACED LOGIC
.         setitem Adjust2EditNote1,0,NLINE1
.       setitem Adjust2EditNote2,0,NLINE2
.       setitem Adjust2EditNote3,0,NLINE3
.       setitem Adjust2EditNote4,0,NLINE4
.       setitem Adjust2EditNote5,0,NLINE5
.       setitem Adjust2EditNote6,0,NLINE6

. need to make sure it's the right one
. now do read on notekey, ndate, and ntime
.         save old vars
          move ndate, ndate2
          move ntime, ntime2
          move      notekey,NONOFLD   // notekey will change - old value still in nonofld
          move      "Nadj0001.LoadNotes-NONOKEY",Location
          pack      KeyLocation,"Key: ",NONOFLD
          call      NONOKEY
          if not over
                    if (nonofld=notekey && ndate2=ndate && ntime2=ntime)  // is found record the one I want?
                              call      TRIM using NLINE
                              setitem   NADJ01A2EDITNOTES,0,NLINE
                    else
                              loop
                                        move      "Nadj0001.LoadNotes-NONOKS",Location
                                        pack      KeyLocation,"Key: ",NONOFLD
                                        call      NONOKS
                                        until over
                                        until (NOTEKEY <> NONOFLD)
                                        if (nonofld=notekey && ndate2=ndate && ntime2=ntime) // is found record the one I want?
                                                  call      TRIM using NLINE
                                                  setitem   NADJ01A2EDITNOTES,0,NLINE
                                        endif
                              repeat
                    endif
          endif
.END PATCH          1.7       REPLACED LOGIC
          setfocus Adjust2DataList
        return
.START PATCH 1.3 ADDED LOGIC
.START PATCH 1.31 REPLACED LOGIC
.AdjustLoadShortPayVars Routine DimPtr,FrmPtr
AdjustLoadShortPayVars Routine DimPtr,FrmPtr,DimPtr1
.END PATCH 1.31 REPLACED LOGIC
          call      Trim using DimPtr
          if (DimPtr = "")
                    return
          endif
          pack      str8,"00000000"
          clear     NCHKFLD
          move      DimPtr,NCSHFLD3
          move      "NCSHKEY",Location
          pack      KeyLocation,"Key: ",NCSHFLD3
          call      NCSHKEY
          loop
                    until over
                    until (NCSHFLD3 <> CLR)
                    call      Trim using CCE
                    call      Trim using CYR
                    call      Trim using CMO
                    call      Trim using CDY
                    pack      str9,CCE,CYR,CMO,CDY
                    if (str9 > str8)
                              move      str9,str8
                              pack      NCHKFLD,CNUM,CNUMDATE,NCSHCHK
                    endif
                    move      "NCSHKS",Location
                    call      NCSHKS
          repeat
          if (str8 = "00000000" | NCHKFLD = "")             .No record found!!!  Should never happen.
                    goto ShortPayTemplate
          endif
          move      "NCHKKEY",Location
          pack      KeyLocation,"Key: ",NCHKFLD
          call      NCHKKEY
          if over
                    goto ShortPayTemplate
          endif
.Prep vars to be printed on Short Payment Notice
          unpack    NCHKDATE,CC,YY,MM,DD
          pack      str11,MM,SLASH,DD,SLASH,CC,YY
.
          unpack    NCHKAMT,str12,str3
          call      Trim using str12
          call      FormatNumeric using str12,str15
          pack      str25,str15,str3
.
          unpack    FrmPtr,str10,str3
          call      Trim using str10
          call      FormatNumeric using str10,str15
          pack      str24,str15,str3
.
.START PATCH 1.31 REPLACED LOGIC
.         call      CreateShortPayNotice using DimPtr,NCHKNUM,str11,str25,str24
.begin patch 1.91
.         call      CreateShortPayNotice using DimPtr,NCHKNUM,str11,str25,str24,DimPtr1
....
          clear     str2
          pack      str2 from osales10,osales
          IF        (Ocompid = "P")
          Move      C2,company
          Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
          Move      C2,company
          else
          Move      C1,company
          endif
          call      debug
          call      CreateShortPayNotice using DimPtr,NCHKNUM,str11,str25,str24,DimPtr1,Company
.end patch 1.91
.....
.END PATCH 1.31 REPLACED LOGIC
          return

ShortPayTemplate
.DOn't beleive this is ever called ????????????????????
          clear     str1
.START PATCH 1.31 REPLACED LOGIC
.         call      CreateShortPayNotice using DimPtr,str1,str1,str1,str1
.begin patch 1.91
....
          clear     str2
          pack      str2 from osales10,osales
          IF        (Ocompid = "P")
          Move      C2,company
          Elseif    (Ocompid2 = "P" & (str2 = "27" | str2 = "28"))
          Move      C2,company
          else
          Move      C1,company
          endif
.         call      CreateShortPayNotice using DimPtr,str1,str1,str1,str1,DimPtr1
          call      debug
          call      CreateShortPayNotice using DimPtr,str1,str1,str1,str1,DimPtr1,company
.end patch 1.91
.END PATCH 1.31 REPLACED LOGIC
          return
.END PATCH 1.3 ADDED LOGIC
.begin patch 1.4
PostQtyAdj
               Clear          str14
               getprop        Nadj001Meditnumber007,text=str14        ;ar
               setprop       AdjustEditAR,text=str14
.
               Clear          Str14
               Getprop        Nadj001Meditnumber008,text=str14
               setprop        AdjustEditAP1,text=str14
.
               clear          str14
               Getprop        Nadj001Meditnumber012,text=str14
               setprop        AdjustEditAP2,text=str14
.
               clear          str14
               getprop        Nadj001Meditnumber009,text=str14
               setprop        AdjustEditLradj,text=str14
.
               clear          str14
               move           calc102 to str14
                Getprop        Nadj001Meditnumber010,text=str14
               setprop        AdjustEditNinadj,text=str14
                Getprop        NAdj001mEditNumber001,text=jstqty
               call           Trim using JSTQTY
               call           RemoveChar using JSTQTY,COMMA
               Setprop        AdjustEditQty,text=JSTQTY
               Move           c1 to JstQrsn
               setitem        AdjustEditReason,0,JSTQRSN
          setitem   AdjustComboReason,0,2

          setprop  nadj001m,visible=0
.let's be anal and make sure we have not messed up invoice variables
                move          c1 to ninvpath
                packkey       Ninvfld from lrn
                rep           zfill,ninvfld
                call          Ninvkey
                move          c0 to str14
          setprop    NAdj001mEditNumber001,value=C0
               Setprop        Nadj001Meditnumber007,text=str14
               Setprop        Nadj001Meditnumber008,text=str14
               Setprop        Nadj001Meditnumber012,text=str14
               Setprop        Nadj001Meditnumber009,text=str14
               Setprop        Nadj001Meditnumber010,text=str14

                return


.START PATCH 1.93 ADDED LOGIC
AdjUpdateTypistTable LROUTINE FrmPtr
.Routine Modifies Typist table when records are updated.  Called by Modify button
.FrmPtr  = "1" - New record, "2" - Update
        clear   NTYPDET
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM
        pack    Typdate,CC,YY,MM
        packkey NTYPFLD from TypDate,Inits
        move    Inits,NtypType
        call    NTYPTST
        if over
                if (FrmPtr = 1)
                        move    C1,ADJCount
                else
                        goto AdjUpdateTypistTableEnd
                endif
                call    NTYPWRT
        else
                call    NTYPKEY
                if (FrmPtr = 1)
                        add    C1,AdjCOUNT
                else
                        goto AdjUpdateTypistTableEnd
                endif
                call    NTYPUPD
        endif
        clear   NTYPDET
        unpack  timestamp,CC,YY,MM
        pack    Typdate,CC,YY,MM
.Update  Typist Totals value...
        packkey NTYPFLD from Typdate,"99 "
        move    "99 " to Ntyptype
        call    NTYPTST
        if over
                call NONINE using FrmPtr
                goto AdjUpdateTypistTableEnd
        endif
        packkey NTYPFLD from Typdate,"99 "
        move    "99 " to Ntyptype
        call    NTYPKEY
                if (FrmPtr = 1)
                        add    C1,AdjCOUNT
                else
                        goto AdjUpdateTypistTableEnd
                endif
        call    NTYPUPD
AdjUpdateTypistTableEnd
        return

NONINE LROUTINE FrmPtr
.Routine ONLY creates New '99' records...
.FrmPtr  = "1" - New record, "2" - Update
          Move      "NADJ0001 - output",MailSubjct
          Move      "DavidHerrick@nincal.com",MailFrom
          Move      "DavidHerrick@nincal.com",MailTo
          Clear     MailBody
          Append    " adjustment Typist",MailBody
          Append    CRLF,MailBOdy
          Append    "No 99 record found !   key =     ",Mailbody
          Append    NTypFld,Mailbody
          Append    CRLF,MailBOdy
          Reset     Mailbody  
          Call      SendMail              
.
        clear   NTYPDET
        unpack  timestamp,CC,YY,MM
        pack    Typdate,CC,YY,MM
        packkey Ntypfld from Typdate,"99 "
        Move    "99 " to Ntyptype
        if (FrmPtr = 1)
                move    C1,AdjCOUNT
        else
                goto NONINEEnd
        endif
        CALL    NTYPWRT
NONINEEnd
        return
.END PATCH 1.93 REPLACED LOGIC

.         include   nonocode.inc
          INCLUDE   NPASIO.INC
.begin patch 1.6
.         INCLUDE   NINVIO.INC
          INCLUDE             ninvio.inc
          Include   NInvAcdio.inc
.end patch 1.6
          INCLUDE   NADJIO.inc
          INCLUDE   NJSTIO.inc
          INCLUDE   NCKIIO.INC
          include   nonoio.inc
          include     nuseio.inc
        INCLUDE   GNXTio.inc
.START PATCH 1.3 ADDED LOGIC
          INCLUDE   NCSHIO.INC
          INCLUDE   NCHKIO.INC
.END PATCH 1.3 ADDED LOGIC
.begin patch 1.4
               include        nordio.inc
               include        ndatio.inc
               include        ndat3io.inc
               include        nshpio.inc
               include        nmrgio.inc
               include        nownio.inc
.START PATCH 1.5 REPLACED LOGIC
.               include        nmlrio.inc
          include   compio.inc
          include   cntio.inc
.END PATCH 1.5 REPLACED LOGIC
               include        nacdio.inc
.START PATCH 1.93 ADDED LOGIC
        INCLUDE        NTYPIO.INC
.END PATCH 1.93 ADDED LOGIC
.begin patch 1.6
.               include        compute.inc
               include        compute.inc
.end patch 1.6
.end patch 1.4

          INCLUDE   COMLOGIC.INC
