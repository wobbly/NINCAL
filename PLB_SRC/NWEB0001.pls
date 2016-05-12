PC        EQU       1
........................................
. Program:      NWEB0001.PLS
. Function:     File Management for tracking website user processes
. Author:       David Strahan
. Date:         January 25,2006
. Release:      1.0
. Notes:
........................................
.
.Include Files
          include   common.inc
        include     cons.inc
        include     NWEBDD.INC
        include               compdd.inc
        include               cntdd.inc
        include     NWEBDD2.INC
        include               npasdd.inc
.
Release   Init      "1.1"	DLH   . add dump of all records using 9999

.Release   Init      "1.0"
.
DimPtr              dim       ^
FrmPtr              form      ^
hold                dim       200       // holding space for NINWEB2
ReturnFlag          init      "N"       // always N unless verifyData finds incomplete record
ExitFlag  init      "N"       // if New or Modify, should not be able to exit- save or quit
MoDayYrTime         dim       16
JDate               dim       9         // hold julian date
FromDate  dim       8
ToDate              dim       8
.
hold2               dim       160       // holding space for NINWeb2
NewFlag2  init      "N"       // always N unless user clicks the new button
ReturnFlag2         init      "N"       // always N unless verifyData finds incomplete record
ExitFlag2 init      "N"       // if New or Modify, should not be able to exit- save or quit
PassFlag1 init      "N"
PrintFlag init      "N"
Choice              form      1
.Vars used for Report Screen
RptCan    dim       1
ComboBoxes          ComboBox (1)
StatTextBoxes       StatText (1)
.Following Collection is used to dynamically destroy objects listed above
.Each time a form is loaded with those objects,   dump them into this collection
.and then destroy whole       collection when     needed
ObjectColl          Collection
.
NWEBFLDback         dim       56
NWEBFLDback2        dim       56
temp                dim       60
.
.Set vars for About Box
        move    "NWeb0001.pls",Wprognme
        move    "NIN Website Trackable File Maintenance",Wfunction
        move    "David Strahan",Wauthor
        move    "1.0",Wrelease
	move	release,wrelease
        move    "February 2,2006",Wreldate
.
font2   font
.
white     color
grey      color
        create  font2,"Arial",size=8,bold=0
        create  white=*white
        create  grey=220:220:220
.
x         plform    NWEB0001
abt     plform  about
mss1      plform    Error
one       plform    nweb001a
pass      plform    Passwrd
rpt2      plform    Report2
          winhide
          formload x
          formload mss1
          formload abt
          formload pass
          formload rpt2
          formload one,NWEB0001
.
.Load ListView Columns
.Screen 1
          WebListView.InsertColumn using "Session ID",0,0 // hidden
          WebListView.InsertColumn using "UserID",0, 1 // hidden
          WebListView.InsertColumn using "User Name",75,2
          WebListView.InsertColumn using "User Type",75,3
          WebListView.InsertColumn using "Company Number",100,4
          WebListView.InsertColumn using "Process Accessed",125,5
          WebListView.InsertColumn using "Date", 100, 6
          WebListView.InsertColumn using "Email Address",150,7
          WebListView.InsertColumn using "Time", 0,8 // hidden col
          WebListView.InsertColumn using "Complete Record", 0,9 // hidden col
          WebListView.InsertColumn using "Julian Date", 0,10 // hidden col
.beginning states
          EVENTREG  X, 17, XRESIZE
.Screen 1
          call WebDisableList
          call WebDisableButtons
          call WebDisableFields
          call WebEnableUpper
          setprop WebDelete, enabled=0
.
          //external call
          call "NWEB001B;LoadListViewColumns" using NWEB0001          // dave using tempfile
.Loop starts here

          loop
                    eventwait
          repeat
.
ClickTab Routine FrmPtr
          if (FrmPtr = 1)
                    deactivate one
          elseif (FrmPtr = 2)
                    //external call
                    call "NWEB001B;DeactivateForm" using NWEB0001
          endif
          return
ChangeTab Routine FrmPtr
          if (FrmPtr = 1)
                    activate one
          elseif (FrmPtr = 2)
                    //external call
                    call "NWEB001B;ActivateForm" using NWEB0001
          endif
          return
.
fileExit routine    // may be called by called program
          if (ExitFlag = "N" && ExitFlag2 = "N")  // Y means we're in middle of save or modify
                    winshow
                    stop
          endif
          return
.
WebAAMReadUName Routine DimPtr
.Calling routine has already trimmed passed variable
          call ClearWebFld    // clear all fields for safety
          pack      NWEBFLD3,"03X", DimPtr        // user name field
          call WebCommonAAMRead
          return
.
WebAAMReadCoNum Routine DimPtr
.Calling routine has already trimmed passed variable
          call ClearWebFld    // clear all fields for safety
          pack      NWEBFLD5,"05X", DimPtr        // company number field
          call WebCommonAAMRead
          return
.
WebAAMReadPAcc Routine DimPtr
.Calling routine has already trimmed passed variable
          call ClearWebFld    // clear all fields for safety
.begin patch 1.1 dh goes bad
          if        (DimPtr = "9999")
          call      WebcommonSeqRead
          return
          endif
.end patch 1.1 dh goes bad
          pack      NWEBFLD6,"06X", DimPtr        // process accessed field
          call WebCommonAAMRead
          return
.
WebAAMReadUsrT Routine DimPtr
.Calling routine has already trimmed passed variable
          call ClearWebFld    // clear all fields for safety
          
          pack      NWEBFLD4,"04X", DimPtr        // user type field
          call WebCommonAAMRead
          return
.begin patch 1.1 dh goes bad
WebCommonSeqRead
          move      "WebOK-NWEBSEQ",Location
          pack      KeyLocation,"SEQ: ",NWEBFLD,comma,NWEBFLD1,comma,NWEBFLD2,comma,NWEBFLD3,comma,NWEBFLD4,comma,NWEBFLD5,comma,NWEBFLD6,comma,NWEBFLD7
          call      NWEBSEQ
          loop
                    until over
                    call      WebLoadListView
                    move      "WebOK-NWEBSEQ",Location
                    pack      KeyLocation,"SEQ: ",NWEBFLD,comma,NWEBFLD1,comma,NWEBFLD2,comma,NWEBFLD3,comma,NWEBFLD4,comma,NWEBFLD5,comma,NWEBFLD6,comma,NWEBFLD7
                    call      NWEBSEQ
          repeat
          WebListView.GetItemCount giving result
          move      result,str9
          call      Trim using str9
          call      FormatNumeric using str9,str11
          pack      str35,str11," records found"
          setItem WebStatTextRecFnd, 0, str35
          if (result <> 0)
                    WebListView.SetItemState giving result using 0,2,2 // select listview obj 1
                    // WebListView.EnsureVisible using 0,0 // scroll to it if necessary
                    call      Click_WebListView //call to  WebRetrieveLVRecord
          else
                    call      WebClearRecord
                    call      WebDisableList
                    call      WebDisableButtons
                    setprop WebDelete, enabled=0   // ugly
          endif
          return
.end patch 1.1 bad dave.
WebCommonAAMRead
          move      "WebOK-NWEBAIM",Location
          pack      KeyLocation,"Key: ",NWEBFLD,comma,NWEBFLD1,comma,NWEBFLD2,comma,NWEBFLD3,comma,NWEBFLD4,comma,NWEBFLD5,comma,NWEBFLD6,comma,NWEBFLD7
          call      NWEBAIM
          loop
                    until over
                    call      WebLoadListView
                    move      "WebOK-NWEBKG",Location
                    pack      KeyLocation,"Key: ",NWEBFLD,comma,NWEBFLD1,comma,NWEBFLD2,comma,NWEBFLD3,comma,NWEBFLD4,comma,NWEBFLD5,comma,NWEBFLD6,comma,NWEBFLD7
                    call      NWEBKG
          repeat
          WebListView.GetItemCount giving result
          move      result,str9
          call      Trim using str9
          call      FormatNumeric using str9,str11
          pack      str35,str11," records found"
          setItem WebStatTextRecFnd, 0, str35
          if (result <> 0)
                    WebListView.SetItemState giving result using 0,2,2 // select listview obj 1
                    // WebListView.EnsureVisible using 0,0 // scroll to it if necessary
                    call      Click_WebListView //call to  WebRetrieveLVRecord
          else
                    call      WebClearRecord
                    call      WebDisableList
                    call      WebDisableButtons
                    setprop WebDelete, enabled=0   // ugly
          endif
          return
.
WebLoadListView     // insert ListView item
          // create and pack new variables
          unpack NWEBRECORD.NWEBTIME,CC,YY,MM,DD,HH,MN,SS
          unpack NWEBRECORD.NWEBTIME,str8
          if (str8 < FromDate | str8 > ToDate)
                    return
          endif
.
          pack MoDayYrTime,MM,"/",DD,"/",CC,YY," ",HH,":",MN
//  get juliandate
          clear JULDAYS
          call cvtjul
          pack JDate,JULDAYS,HH,MN
          //Need to pack hold var before I start Trimming
          pack      hold, NWEBRECORD.NWEBSESSION, NWEBRECORD.NWEBID, NWEBRECORD.NWEBUSERNAME, NWEBRECORD.NWEBUSERTYPE, NWEBRECORD.NWEBCOMP, NWEBRECORD.NWEBPROCESS,MoDayYrTime,NWEBRECORD.NWEBUSEREMAIL, NWEBRECORD.NWEBTIME, JDate
.
          WebListView.InsertItem giving result using NWEBRECORD.NWEBSESSION //creates new row and assigns value to 1st col
          call      Trim using NWEBRECORD.NWEBID
          WebListView.SetItemText using result,NWEBRECORD.NWEBID,1 // col 2
          call      Trim using NWEBRECORD.NWEBUSERNAME
          WebListView.SetItemText using result,NWEBRECORD.NWEBUSERNAME,2 //col 3
          call      Trim using NWEBRECORD.NWEBUSERTYPE // mistaken NWEBUSERNAME again
          if (NWEBRECORD.NWEBUSERTYPE = "A")
                    move "Administrator", str15
          else if (NWEBRECORD.NWEBUSERTYPE = "C")
                    move "Client", str15
          else if (NWEBRECORD.NWEBUSERTYPE = "O")
                    move "Consultant", str15
          else if (NWEBRECORD.NWEBUSERTYPE = "B")
                    move "Broker", str15
.
.         else you have invalid data
.
          endif
          call Trim using str15
          WebListView.SetItemText using result,str15,3 //col 4
.
          call      Trim using NWEBRECORD.NWEBCOMP
          move      NWEBRECORD.NWEBCOMP,COMPFLD
          move      "WebLLV-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          call      Trim using COMPCOMP
          WebListView.SetItemText using result,COMPCOMP,4 //col 5
.
.         uncomment next line to get company numbers
.         WebListView.SetItemText using result,NWEBRECORD.NWEBCOMP,4 //col 5
          call      Trim using NWEBRECORD.NWEBPROCESS
          move      NWEBRECORD.NWEBPROCESS,NWEB2FLD
          move      "WebLLV-NWEB2KEY",Location
          pack      KeyLocation,"Key: ",NWEB2FLD
          call      NWEB2KEY
          call      Trim using NWEB2RECORD.NWEB2NAME
          WebListView.SetItemText using result,NWEB2RECORD.NWEB2NAME,5 //col 6
          // insert Date stuff here
          call      Trim using MoDayYrTime
          WebListView.SetItemText using result,MoDayYrTime,6 //col 7
          call      Trim using NWEBRECORD.NWEBUSEREMAIL
          WebListView.SetItemText using result,NWEBRECORD.NWEBUSEREMAIL,7 //col 8
          call      Trim using NWEBRECORD.NWEBTIME
          WebListView.SetItemText using result,NWEBRECORD.NWEBTIME,8 //col 9
          WebListView.SetItemText using result,hold,9 // col 10
          // insert julian date for sort here
          call      Trim using JDate
          WebListView.SetItemText using result,JDate,10 //col 11
          return
.
WebRetrieveLVRecord // load highlighted listview entry
          WebListView.GetNextItem giving result using C2
          WebListView.GetItemText giving hold using result, 9 // populate hold with full record
          unpack    hold,NWEBRECORD.NWEBSESSION,NWEBRECORD.NWEBID,NWEBRECORD.NWEBUSERNAME,NWEBRECORD.NWEBUSERTYPE,NWEBRECORD.NWEBCOMP,NWEBRECORD.NWEBPROCESS,MoDayYrTime,NWEBRECORD.NWEBUSEREMAIL,NWEBRECORD.NWEBTIME, JDate
          call      WebLoadCurrentLV
          return
.
WebLoadCurrentLV    // fill lower part of GUI with selected record
          setitem   WebEditSID,0,NWEBRECORD.NWEBSESSION
          setitem WebEditUID,0, NWEBRECORD.NWEBID
          setitem WebEditUName, 0, NWEBRECORD.NWEBUSERNAME
          WebComboBox.selectString using NWEBRECORD.NWEBUSERTYPE, 0
          setitem WebEditCoNum,0, NWEBRECORD.NWEBCOMP // gives number
          move      NWEBRECORD.NWEBCOMP,COMPFLD
          move      "WebLLV-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
          call      Trim using COMPCOMP
          setitem WebStatCoName, 0, COMPCOMP
          setitem WebEditPAcc,0,NWEBRECORD.NWEBPROCESS // gives number
          move      NWEBRECORD.NWEBPROCESS,NWEB2FLD
          move      "WebLLV-NWEB2KEY",Location
          pack      KeyLocation,"Key: ",NWEB2FLD
          call      NWEB2KEY
          call      Trim using NWEB2RECORD.NWEB2NAME
          setitem WebStatProcName, 0, NWEB2RECORD.NWEB2NAME
          setitem WebEditDate, 0, MoDayYrTime
          setitem WebEditEMail,0,NWEBRECORD.NWEBUSEREMAIL
.because record is found, do the following:
          call WebDisableButtons
          call WebDisableFields
          call WebEnableList  // be able to click in list view
          call WebEnableUpper
          setprop WebModify, enabled=1  // ugly code
          move      YES,PrintFlag       .Print Button used dynamically
          setprop WebDelete, enabled=1
          return
.
ClearWebFld
          clear     NWEBFLD
          clear     NWEBFLD1
          clear     NWEBFLD2
          clear     NWEBFLD3
          clear     NWEBFLD4
          clear     NWEBFLD5
          clear     NWEBFLD6
          clear     NWEBFLD7
          return
.
WebClearRecord
          WebEditSID.SelectAll
          WebEditSID.Clear
          WebEditUID.SelectAll
          WebEditUID.Clear
          WebEditUName.SelectAll
          WebEditUName.Clear
          WebComboBox.setcursel using 0
          WebEditCoNum.SelectAll
          WebEditCoNum.Clear
          WebEditPAcc.SelectAll
          WebEditPAcc.Clear
          WebEditDate.SelectAll
          WebEditDate.Clear
          WebEditEMail.SelectAll
          WebEditEMail.Clear
          setItem WebStatCoName, 0, ""
          setItem WebStatProcName, 0, ""
          return
.
WebDeleteButton // here check MultFlag
          if (PassFlag1 = NO)
                    move       "W",progcode
                    setitem PasswordStatMssg1,0,"Tracking Records"
                    setprop PasswordStatMssg1,visible=1,alignment=1
                    setitem PasswordStatMssg,0,"Enter Password to Delete or Modify,"
                    setprop PasswordStatMssg,alignment=1
                    setitem PasswordEdit,0,""
                    setfocus PasswordEdit
                    clear     NPASFLD
                    setprop   Passwrd,visible=1
                    reset NPASFLD
                    if (PassFlag =      NO)
                              return
                    endif
                    move      YES,PassFlag1
          endif
          alert plain, "Are you sure you want to delete the selected record(s)?", result
          if (result = 1)     // yes
                    move      SEQ,result
                    move      result,N9
                    loop
                              move      result,N9
                              WebListView.GetNextItem giving result using C2,N9  // -1 is error code
                              until (result = SEQ)
                              WebListView.GetItemText giving hold using result, 9 // populate hold with full record
                              unpack    hold, NWEBRECORD.NWEBSESSION, NWEBRECORD.NWEBID, NWEBRECORD.NWEBUSERNAME, NWEBRECORD.NWEBUSERTYPE, NWEBRECORD.NWEBCOMP, NWEBRECORD.NWEBPROCESS,MoDayYrTime,NWEBRECORD.NWEBUSEREMAIL, NWEBRECORD.NWEBTIME, JDate
                              pack NWEBFLD, NWEBRECORD.NWEBSESSION, NWEBRECORD.NWEBPROCESS, NWEBRECORD.NWEBTIME
                              move      "WebDeleteButton-NWEBTST",Location
                              pack      KeyLocation,"Key: NWEBFLD"
                              call NWEBTST        // give valid read
                              if over
                                        alert note, "no valid read", result
                                        return
                              endif
                              move      "WebDeleteButton-NWEBDEL",Location
                              pack      KeyLocation,"Key: NWEBFLD"
                              call NWEBDEL        // needs to be preceeded by valid read
                    repeat
          endif
.
          call CLICK_WebOK    // same as previous WebOK click
          if (result = 2)     // no
                    call WebQuitButton
                    return
          endif
          if (result = 3)     // cancel
                    call WebQuitButton // deletes listview, calls clear record and a read
                    return
          endif
.
WebQuitButton
.         undos whatever changes user might have been making (no save)
          call WebRetrieveLVRecord
          return
.
WebSaveButton
          call WebVerifyInput
          if (ReturnFlag = "Y")
                    return
          endif
          pack NWEBFLD, NWEBRECORD.NWEBSESSION, NWEBRECORD.NWEBPROCESS, NWEBRECORD.NWEBTIME
                    move      "WebSaveButton-NWEBTST",Location
                    pack      KeyLocation,"Key: NWEBFLD"
                    call NWEBTST        // gives read
                    if over
                    //Should never happen
                              alert     note,"Record no longer Exists!",result
                              call WebQuitButton
                              return
                    endif
.move changed fields into variables
          getitem WebEditSID,0,NWEBRECORD.NWEBSESSION
          getitem WebEditUID,0,NWEBRECORD.NWEBID
          getitem WebEditUName,0,NWEBRECORD.NWEBUSERNAME
          WebComboBox.getCurSel giving result
          if (result= 4)
                    move "A", NWEBRECORD.NWEBUSERTYPE
          elseif (result= 1)
                    move "C", NWEBRECORD.NWEBUSERTYPE
          elseif (result= 2)
                    move "O", NWEBRECORD.NWEBUSERTYPE
          elseif (result= 3)
                    move "B", NWEBRECORD.NWEBUSERTYPE
          endif
          getitem WebEditCoNum,0,NWEBRECORD.NWEBCOMP
          getitem WebEditPAcc,0,NWEBRECORD.NWEBPROCESS
          getitem WebEditEMail,0,NWEBRECORD.NWEBUSEREMAIL
          move      "WebSaveButton-NWEBUPD",Location
          pack      KeyLocation,"Key: NWEBFLD"
          call NWEBUPD // needs previous key read
. load new values into NWEBFLDback
          pack      NWEBFLDback,NWEBRECORD.NWEBSESSION,NWEBRECORD.NWEBPROCESS,NWEBRECORD.NWEBTIME
.  change search edit text box
                    if (Choice=1)
                              setitem   WebEditSearch,0,NWEBRECORD.NWEBUSERNAME
                    elseif (Choice=2)
                              setitem   WebEditSearch,0,NWEBRECORD.NWEBUSERTYPE
                    elseif (Choice=3)
                              setitem   WebEditSearch,0,NWEBRECORD.NWEBCOMP
                    elseif (Choice=4)
                              setitem   WebEditSearch,0,NWEBRECORD.NWEBPROCESS
                    endif
          call Click_WebOK // will highlight the first found listview obj
          move      C0,N4     // Initialize
          WebListView.GetItemCount giving howmany
          sub       C1,howmany
          for result,C0,howmany
                    WebListView.GetNextItem giving N9 using 0, result
                    WebListView.GetItemText giving hold using N9, 9 // populate hold with full record
                    unpack hold, NWEBRECORD.NWEBSESSION, NWEBRECORD.NWEBID, NWEBRECORD.NWEBUSERNAME, NWEBRECORD.NWEBUSERTYPE, NWEBRECORD.NWEBCOMP, NWEBRECORD.NWEBPROCESS,MoDayYrTime,NWEBRECORD.NWEBUSEREMAIL, NWEBRECORD.NWEBTIME, JDate
                    pack      NWEBFLDback2,NWEBRECORD.NWEBSESSION,NWEBRECORD.NWEBPROCESS,NWEBRECORD.NWEBTIME
                    if (NWEBFLDback2 = NWEBFLDback)
                              move      C1,N4
                              break
                    endif
          repeat
          if (N4 = C0)        // no match
                    move      C0,N9   // n9 used to be result
          endif
.  else we found a match!
          WebListView.SetItemState giving result using 0,0,2 // deslect obj 1
          WebListView.SetItemState giving result using n9, 2, 2
          WebListView.EnsureVisible using n9,0
          call WebRetrieveLVRecord
          return
.
WebModifyButton
          if (PassFlag1 = NO)
                    move      "W",progcode
                    setitem PasswordStatMssg1,0,"Tracking Records"
                    setprop PasswordStatMssg1,visible=1,alignment=1
                    setitem PasswordStatMssg,0,"Enter Password to Delete or Modify,"
                    setprop PasswordStatMssg,alignment=1
                    setitem PasswordEdit,0,""
                    setfocus PasswordEdit
                    clear     NPASFLD
                    setprop   Passwrd,visible=1
                    reset NPASFLD
                    if (PassFlag =      NO)
                              return
                    endif
                    move      YES,PassFlag1
          endif
          // just have this make save and the record fields available for
          // editing
          alert plain, "Are you sure you want to modify the selected record?", result
                    //if (result = 1)   // yes
          if (result = 2)     // no
                    call WebQuitButton
                    return
          elseif (result = 3) // cancel
                    call WebQuitButton // deletes listview, calls clear record and a read
                    return
          elseif (result=1)
                    call WebDisableUpper
                    call WebDisableList
                    call WebEnableFields
                    call WebEnableButtons
                    setprop WebDelete, enabled=1  // allow delete
                    setprop WebModify, enabled=0  // sloppy
                    move      NO,PrintFlag        .Print Button used dynamically
                    return
          endif
.
WebPrintSelect
.Allows   selection of different Reports
          call      Report2DestroyObjects
          setprop   Report2,title="NIN Web Reports"
          create    Report2;StatTextBoxes(1)=70:90:10:110,"Report","'>MS Sans Serif'(8)"
          create    Report2;ComboBoxes(1)=70:91:80:310,"",";T)racking Report - Selected Records;)User File Report"
          activate StatTextBoxes(1)
          activate ComboBoxes(1)
          listins   ObjectColl,StatTextBoxes(1),ComboBoxes(1)
          setfocus ComboBoxes(1)
          return

Report2DestroyObjects
          destroy   ObjectColl
          return

WebPrintButton
          call      WebPrintSelect
          setprop   Report2,visible=1
          if (RptCan = YES)
                    return
          endif
          getitem   ComboBoxes(1),0,N1
          if (N1 = 1)
                    if (PrintFlag <> YES)
                              alert     note,"Valid Tracking Records need to be Selected!",result
                              setfocus WebListView
                              return
                    endif
                    setmode   *mcursor=*wait
                    pack      taskname,"c:\work\NWEB0002.dat"
                    erase     taskname
                    prepare   tempfile,taskname
                    // loop through and write selected items
                              move      SEQ,result
                              move      result,N9
                              loop
                                        move      result,N9
                                        WebListView.GetNextItem giving result using C2,N9  // -1 is error code
                                        until (result = SEQ)
                                        WebListView.GetItemText giving hold using result, 9 // populate hold with full record
                                        unpack    hold, NWEBRECORD.NWEBSESSION, NWEBRECORD.NWEBID, NWEBRECORD.NWEBUSERNAME, NWEBRECORD.NWEBUSERTYPE, NWEBRECORD.NWEBCOMP, NWEBRECORD.NWEBPROCESS,MoDayYrTime,NWEBRECORD.NWEBUSEREMAIL, NWEBRECORD.NWEBTIME, JDate
                                        write     tempfile,SEQ;NWEBRECORD.NWEBUSERNAME:
                                                  NWEBRECORD.NWEBUSERTYPE:
                                                  NWEBRECORD.NWEBCOMP:
                                                  NWEBRECORD.NWEBPROCESS:
                                                  MoDayYrTime:
                                                  NWEBRECORD.NWEBUSEREMAIL
                              repeat
                              //close tempfile
//Dump into Excel
                    //external call
                    call "NWEB0002;NWebPrintUsersRoutine" using tempfile
                    close tempfile
                    erase     taskname
                    setmode   *mcursor=*arrow
          else
                    move      "!\\nins1\winbatch\WebUserFileFTP",taskname
                    execute   taskname
          endif
          return
.
WebEnableExit routine         // allows called program to change webexit status
          move "N", ExitFlag2
          if (ExitFlag="N")
                    setprop WebExit, enabled=1
          endif
          return
.
WebDisableExit routine  // allows called program to change webexit status
          move "Y", ExitFlag2
          setprop WebExit, enabled=0
          return
.
WebEnableUpper
          move "N", ExitFlag
          setprop WebEditSearch, enabled=1, bgcolor=white
          setprop WebOk, enabled=1
          if (ExitFlag2 = "Y")          // is second program ready to close?
                    setprop WebExit, enabled=0
          else
                    setprop WebExit, enabled=1
          endif
          setprop WebRadioCoNum, enabled=1
          setprop WebRadioUName, enabled=1
          setprop WebRadioUType, enabled=1
          setprop WebRadioPAcc, enabled=1
          return
.
WebDisableUpper
          move "Y", ExitFlag
          setprop WebEditSearch, enabled=0, bgcolor=grey
          setprop WebOk, enabled=0
          setprop WebExit, enabled=0
          setprop WebRadioCoNum, enabled=0
          setprop WebRadioUName, enabled=0
          setprop WebRadioUType, enabled=0
          setprop WebRadioPAcc, enabled=0
          return
.
WebEnableList
          setprop WebListView, enabled=1
          return
.
WebDisableList
          setprop WebListView, enabled=0
          return
.
WebDisableButtons
          setprop WebModify, enabled=0
          move      NO,PrintFlag        .Print Button used dynamically
          setprop WebSave, enabled=0
          setprop WebQuit, enabled=0
          return
.
WebEnableButtons
          setprop WebModify, enabled=1
          move      YES,PrintFlag       .Print Button used dynamically
          setprop WebSave, enabled=1
          setprop WebQuit, enabled=1
          return
.
WebDisableFields
          setprop WebEditSID, enabled=0, bgcolor=grey
          setprop WebEditUID, enabled=0, bgcolor=grey
          setprop WebEditUName, enabled=0, bgcolor=grey
          setprop WebComboBox, enabled=0, bgcolor=grey
          setprop WebEditCoNum, enabled=0, bgcolor=grey
          setprop WebEditPAcc, enabled=0, bgcolor=grey
          setprop WebEditEMail, enabled=0, bgcolor=grey
          setprop WebEditDate, enabled=0, bgcolor=grey
          return
.
WebEnableFields
          setprop WebEditSID, enabled=1, bgcolor=white
          setprop WebEditUID, enabled=1, bgcolor=white
          setprop WebEditUName, enabled=1, bgcolor=white
          setprop WebComboBox, enabled=1, bgcolor=white
          setprop WebEditCoNum, enabled=1, bgcolor=white
          setprop WebEditPAcc, enabled=1, bgcolor=white
          setprop WebEditEMail, enabled=1, bgcolor=white
          return
.
WebVerifyInput
          getitem WebEditSID,0,temp
          call    TRIM using temp
          count   HowMany,temp
          if (howmany = 0)
                    alert caution,"Session ID required!",result
                    setfocus WebEditSID
                    move "Y", ReturnFlag
                    return
          endif
          getitem WebEditUID,0,temp
          call    TRIM using temp
          count   HowMany,temp
          if (howmany = 0)
                    alert caution,"User ID required!",result
                    setfocus WebEditUID
                    move "Y", ReturnFlag
                    return
          endif
          getitem WebEditUName,0,temp
                 call    TRIM using temp
                 count   HowMany,temp
                 if (howmany = 0)
                              alert caution,"User Name required!",result
                         setfocus WebEditUName
                         move "Y", ReturnFlag
                         return
                    endif
          WebComboBox.getCurSel giving result
          if (result= 0)      // empty combo box selection
                    alert caution, "User Type required!", result
                    setfocus WebComboBox
                     move "Y", ReturnFlag
                    return
          endif
          getitem WebEditCoNum,0, temp
          if (temp="000000")
                    alert caution,"Non-zero Company Number required!",result
                    setfocus WebEditCoNum
                    move "Y", ReturnFlag
                    return
          endif
          getitem WebEditPAcc,0,temp
          if (temp="0000")
                    alert caution,"Non-zero Process Number required!",result
                    setfocus WebEditCoNum
                    move "Y", ReturnFlag
                    return
          endif
          getitem WebEditEmail,0,temp
          call    TRIM using temp
          count   HowMany,temp
          if (howmany = 0)
                    alert caution,"Email address required!",result
                    setfocus WebEditEmail
                    move "Y", ReturnFlag
                    return
          endif

          move "N", ReturnFlag          // record is complete!
        return
.WebMenuCode
FileMenuRoutine Routine
          if (result = 1)
                    //alert   note,"Exit option",result
                    call FileExit
          endif
          return
XRESIZE
           NWEB0001.Scale
           RETURN

.
HelpMenuRoutine Routine
          if (result = 1)
                    setprop AboutMssg, visible=1
          endif
          return
.
          include nwebio2.inc
          include   nwebio.inc
        include     compio.inc
        include     cntio.inc
        include     npasio.inc
          include   comlogic.inc