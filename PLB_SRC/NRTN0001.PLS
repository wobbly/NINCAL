........................................
. Program:      RETURN.PLS
. Function:     Return-To File Maintenance
. Author:       Andrew Harkins
. Orig. Date:   August 28,1998
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc
        include nrtndd.inc
        include npasdd.inc
        include gnxtdd.inc
        include stabbdd.inc
.begin patch 1.7        
          include compdd.inc
          include cntdd.inc
          include Compnotesdd.inc       
.end patch 1.7        
           include    Ntypdd.inc
Release   Init      "1.71"   DLH had typist
Reldate   Init      "2015 April 28"
.Release   Init      "1.7"   DLH handle company & contact #'s from master company file, prepatory for migration
..                             fix double insertion in search list view
.Reldate   Init      "2013 October 30"
.Release  Init      "1.6.3"   DLH 12Apr2007  inactive byte
.Reldate  Init      "12 April 2007"
.release init    "1.6.2"   ASH 09AUG2004 Logo Conversion
.release init    "1.6.1"   ASH 18NOV2003 CHANGED EDITTEXT BACKGROUND COLOR
.release init    "1.6"   ASH 20MAR 2000 REPLACED AAM SEARCH LOGIC
.release init    "1.5"   ASH 19MAR99 moved: 1) VArs associated with Passwrd.plf now found in NPASDD.INC
.release init    "1.4"   ASH 19JAN99 added: 1) Logic for 30 minute timeout
.release init    "1.3"   ASH 03Dec98 fixes: 1) Added logic to allow blank chars. in first 3 of search
.                                             criteria, provided string is longer than 3
.release init    "1.2"   ASH 23OCT98 Added logic to prevent '?' as search criteria for first 4 chars
.release init    "1.1"   ASH 14OCT98 Disabled Print while OK is processing
.release init    "1.0"   ASH 28AUG98 ORIGINAL RELEASE, INCLUDES LOGIC FOR DYNAMIC SIZING
.Start Patch #1.4 - Timer logic
Timer   Timer
.End Patch #1.4 - Timer logic
.Files to open
file2   file    uncomp
prfile  pfile

.START PATCH #1.5 - VARS NOW DECLARED IN NPASDD.INC
.Vars used for Password Box
.progcode init   "R"
.END PATCH #1.5 - VARS NOW DECLARED IN NPASDD.INC

.Used for New Records with New Return Numbers
newrtn   form    4      USED TO BE NEWBRK!!!!!
.START PATCH #1.5 - VARS NOW DECLARED IN NPASDD.INC
.PassFlag init   "N"
.END PATCH #1.5 - VARS NOW DECLARED IN NPASDD.INC
ExitFlag init   "Y"
ReturnFlag init "N"
UpdateFlag init "N"
AamFlag init    "N"
NewFlag init    "N"
.begin patch 1.7
.CompFlag init   "N"
CompCFlag init   "N"               .company name change flag
.end patch 1.7
HoldFlag init   "N"
AKey1   init    "01F"
filler  init    "0000"
area    dim     3
newnum  dim     13
newdate dim     10
olddate dim     10
.Length of record plus space for B1
.begin patch 1.7
.hold    dim     228
hold    dim     237
.end patch 1.7
.key holds longest possible Aam
key     dim     45
holdkey dim     4
holdnum dim     4
holdstat dim    1

.Vars used for printing
linenumber form 1
maxline form    "7"
page    form    9

.Vars used for Report Screen
RptCan  dim     1
FromNo  dim     4
ToNo    dim     4
FromDate dim    8
ToDate  dim     8
.
date    dim     8
.Var used for Updating
tstRTNvars  list
tstRTNUM       DIM     4           1-4      RETURN-TO NUMBER. **KEY**
tstRTCNTCT      DIM     45         5-49     RETURN-TO CONTACT NAME
tstRTCOMP       DIM     45         50-94    RETURN-TO COMPANY NAME
tstRTADDR       DIM     25         95-119   RETURN-TO ADDRESS
tstRT2ADDR      DIM     25        120-144   RETURN-TO ADDRESS2
tstRTCITY       DIM     15        145-159   RETURN-TO CITY
tstRTSTATE      DIM     2         160-161   RETURN-TO STATE
tstRTZIP        DIM     10        162-171   RETURN-TO ZIPCODE
tstRTCOUN       DIM     15        172-186   RETURN-TO COUNTRY CODE
tstRTNAME       DIM     10        187-196   PASSWORD ID
tstRTREVDAT     DIM     8         197-204   REVISED DATE
tstRTBLANK      DIM     3         205-207   NOT USED    
tstRTTELE       DIM     10        208-217   TELEPHONE NUMBER
tstRTFAX        DIM     10        218-227   FAX NUMBER.
.begin patch 1.7
tstRTCOMPN    Dim         6      228-233  Company number
tstRTCNTN     Dim         3      233-236  Contact Number 
.end patch 1.7

         listend

.Colors
white   color
grey    color

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
.Start Patch #1.0 - added logic for dynamic form sizing
font5   font
.End Patch #1.0 - added logic for dynamic form sizing

.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu

.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"

.Start Patch #1.0 - added logic for dynamic form sizing
coll1   collection
specs   form          4(4)
size    form          "1.000"
infostring dim        590

.Getinfo - NOT YET IMPLEMENTED!!!!!!!
.        getinfo system,infostring
.        bump    infostring,12
.        move    infostring,str4
.        bump    infostring,4
.        move    infostring,str5     
.End Patch #1.0 - added logic for dynamic form sizing
.............................

.Set Vars used for About Box
        move    "NRTN0001.PLS",Wprognme
        move    "Return-To File Maintenance",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
          move      Reldate,Wreldate
.        move    "March 20, 2000",Wreldate

.Declare forms, Always declare child forms first
rpt     plform  Report
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
x       plform  Nrtn0001
        winhide
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload pss
        formload mss1
        formload rpt
        
.Start Patch #1.4 - timer logic
        CREATE  TIMER,18000     .30 minutes
        ACTIVATE TIMER,Timeout,RESULT
.End Patch #1.4 - timer logic
.Create Menus
        create  Nrtn0001;mFile,FData
        create  Nrtn0001;mEdit,EData,mFile
        create  Nrtn0001;mHelp,HData,mEdit

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under this one
        activate mHelp,HelpGo,result
      
.Create Colors for EditText Inquiry
        create  white=*white
.START PATCH 1.6.1 REPLACED LOGIC
.        create  grey=*ltgray
        create  grey=220:220:220
          call      ReturnDisableLower
.END PATCH 1.6.1 REPLACED LOGIC        

.Start Patch #1.0 - added logic for dynamic form sizing
.COLLECTION NOT YET USED!!!
.        listins Coll1,Return,ReturnCancel,ReturnDelete,ReturnEditAddress,ReturnEditAddress2:
.        ReturnEditCity,ReturnEditCompany,ReturnEditContact,ReturnEditCountry:
.        ReturnEditDate,ReturnEditFax,ReturnEditNumber,ReturnEditState,ReturnEditTelephone:
.        ReturnEditUser,ReturnEditZip,ReturnExit,ReturnInquiryGroup,ReturnModify,ReturnNew,ReturnOK:
.        ReturnPrint,ReturnSave,ReturnSearchKey,ReturnSearchList,ReturnStatAddress,ReturnStatAddress2:
.        ReturnStatBy,ReturnStatCity,ReturnStatCompany,ReturnStatContact:
.        ReturnStatCountry,ReturnStatFax,ReturnStatLastUpdate,ReturnStatNumMssg,ReturnStatOwner:
.        ReturnStatSearch,ReturnStatSearch2,ReturnStatState,ReturnStatState2,ReturnStatTelephone:
.        ReturnStatZip,ReturnUpdate
.        
.        if (str4 = "1024")
.                create  font5,"Arial",size=12
.                setprop coll1,font=font5
.                move    "1.25",size
.                getprop Return,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop Return,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnCancel,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnCancel,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnDelete,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnDelete,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnEditAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnEditCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditDate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditNumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditNumber,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5       
..
.                getprop ReturnEditState,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditState,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditUser,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditUser,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnEditZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnEditZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnExit,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnExit,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnInquiryGroup,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnInquiryGroup,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnModify,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnModify,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnNew,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnNew,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnOK,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnOK,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnPrint,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnPrint,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnSave,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnSave,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnSearchKey,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnSearchKey,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnSearchList,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnSearchList,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnStatAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatAddress,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatAddress2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatBy,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatBy,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatCity,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatCompany,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatContact,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.                getprop ReturnStatCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatCountry,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatFax,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatLastUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatLastUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatNumMssg,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatNumMssg,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatOwner,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatOwner,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5       
..
.                getprop ReturnStatSearch,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatSearch,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatSearch2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatSearch2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatState,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatState,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatState2,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatState2,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatTelephone,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnStatZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnStatZip,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5
..
.                getprop ReturnUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4)
.                mult    size,specs
.                setprop ReturnUpdate,top=specs(1),height=specs(2),left=specs(3),width=specs(4),font=font5                
..
.        endif
.End Patch #1.0 - added logic for dynamic form sizing   


.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic        
.Open files
        trap    StateFileError if IO
        open    NSTFILE,NSTNAME
        trapclr IO
.START PATCH #1.5 - VARS NOW DECLARED IN NPASDD.INC
        move    "R",progcode
        move    "N",PassFlag
.END PATCH #1.5 - VARS NOW DECLARED IN NPASDD.INC
.Main Loop
        clock   date to date
        unpack  date,MM,STR1,DD,STR1,YY
        pack    newdate,MM,SLASH,DD,SLASH,CC,YY
.Set Error Message Stat Text Boxes
        call    SetReturnErrorMssgDefault
.Set Flags to Open NINRTN.DAT
        move    C0,NRTNFLAG
        move    C0,NRTNFLG2
.Display Password Form
        setprop Passwrd,visible=1
        setfocus ReturnSearchKey
        loop
                waitevent
.Start Patch #1.4 - timer logic                
                setitem timer,0,18000   .reset to 30 minutes
.End Patch #1.4 - timer logic                
        repeat
        
.Start Patch #1.4 - timer logic
Timeout
        beep
        beep
        beep
        stop
.End Patch #1.4 - timer logic
SetReturnErrorMssgDefault
.Set Default for Return-To File Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=1
        setprop ErrorMssgStat4,visible=1
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0,"To Search By Return-To Number:"
        setitem ErrorMssgStat2,0,"Enter 4 Digit Number"
        setitem ErrorMssgStat3,0,"To Search By Company Name:"
        setitem ErrorMssgStat4,0,"Enter Search Name"
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return

SetReturnReportScreen
.Set Default for Report Screen
        setitem ReportStatText1,0,"From Number :"
        setitem ReportStatText2,0,"To Number   :"
        setprop ReportStatText3,width=200
        setitem ReportStatText3,0,"Enter Dates As MMDDYYYY"
        setitem ReportStatText4,0,"From Date   :"
        setitem ReportStatText5,0,"To Date     :"
        setprop ReportEditText1,edittype=3,maxchars=4
        setitem ReportEditText1,0,""
        setprop ReportEditText2,edittype=3,maxchars=4
        setitem ReportEditText2,0,""
        setprop ReportEditText3,visible=0
        setprop ReportEditText4,edittype=3,maxchars=8
        setitem ReportEditText4,0,""
        setprop ReportEditText5,edittype=3,maxchars=8
        setitem ReportEditText5,0,""
        setprop ReportCheck1,visible=0
        setprop ReportCheck2,visible=0
        setprop ReportCheck3,visible=0
        setprop ReportCheck4,visible=0
        setfocus ReportEditText1
        move    NO,RptCan
        return

ReturnClearRec
.Clear all Text Fields
        setitem ReturnEditNumber,0,""
        setitem ReturnEditContact,0,""
        setitem ReturnEditCompany,0,""
        setitem ReturnEditAddress,0,""
        setitem ReturnEditAddress2,0,""
        setitem ReturnEditCity,0,""
        setitem ReturnEditState,0,""
        setitem ReturnEditZip,0,""
        setitem ReturnEditCountry,0,""
        setitem ReturnEditUser,0,NPASuser
        setitem ReturnEditDate,0,newdate
        setitem ReturnEditTelephone,0,""
        setitem ReturnEditFax,0,""
        setitem ReturnStatState2,0,""
        setfocus ReturnEditNumber
        return

FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
        PRTOPEN prfile,"-",WPrognme
        CLOCK   DATE,DATE
        unpack  date,mm,str1,dd,str1,yy
        goto    ReturnStartPrint
FileGo2
        PRTOPEN prfile,"@",WPrognme
        CLOCK   DATE,DATE
        unpack  date,mm,str1,dd,str1,yy        
        goto    ReturnStartPrint
FileGo3        
        if (ExitFlag = "Y")
                winshow
                stop
        endif
        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return       
        
ReturnLoadList
        setitem ReturnEditNumber,0,RTNUM
        setitem ReturnEditContact,0,RTCNTCT
        setitem ReturnEditCompany,0,RTCOMP
        setitem ReturnEditAddress,0,RTADDR
        setitem ReturnEditAddress2,0,RT2ADDR
        setitem ReturnEditCity,0,RTCITY
        setitem ReturnEditState,0,RTSTATE
        call    trim using RTSTATE
        count   N2,RTSTATE
        if (N2 = "0")
                move    "",STNAME
        else
                pack    NSTFLD,RTSTATE
                trap    StateFileError if IO
                read    NSTFILE,NSTFLD;STVARS
                trapclr IO
                if over
                        move    "Unknown State",STNAME
                endif
        endif
        setitem ReturnStatState2,0,STNAME
        setitem ReturnEditZip,0,RTZIP
        setitem ReturnEditCountry,0,RTCOUN
        setitem ReturnEditUser,0,RTNAME

.begin patch 1.6.3
          if        (RTActive = "N")
          setItem   ReturnCheck001,0,c1
          else
          setItem   ReturnCheck001,0,c0

          endif
.end patch 1.6.3
.begin patch 1.7
                    setitem   ReturnEditCompN,0,RTCOmpN
                    setitem   ReturnEditCntN,0,RTCntN
                    setitem   ReturnStatCompN,0,""
                    setitem   ReturnStatCntN,0,""
          if        (RTcompN <> "000000" & RTcompN <> "")
          packkey   compfld,RTcompN
          call      compkey
                    if        not over
                    setitem   ReturnStatCompN,0,compcomp
                    endif
          Packkey   CNCTFLD,RTcompN,RTcntN
          call      CNCTKEY
                    if        not over
                    setitem   ReturnStatCntN,0,CNCTFNAME
                    endif
          endif     
.end patch 1.7
.Format Date
        clear   olddate
        count   N2,RTREVDAT
        if (N2 = "8")
                unpack  RTREVDAT,CC,YY,MM,DD
                pack    olddate,MM,slash,DD,slash,CC,YY
        endif
        setitem ReturnEditDate,0,olddate
        clear   Newnum
        count   N2,RTTELE
        if (N2 = "10")
                unpack  RTTELE,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop ReturnEditTelephone,maxchars=13
        else
                setprop ReturnEditTelephone,maxchars=10
        endif
        setitem ReturnEditTelephone,0,Newnum
        clear   Newnum
        count   N2,RTFAX
        if (N2 = "10")
                unpack  RTFAX,area,str3,str4
                pack    Newnum,"(",area,")",str3,Dash,str4
                setprop ReturnEditFax,maxchars=13
        else
                setprop ReturnEditFax,maxchars=10
        endif
        setitem ReturnEditFax,0,Newnum
        setprop ReturnModify,default=1
        packkey NRTNFLD,RTNUM
        packkey NRTNFLD2,"01X",RTCOMP
        return

.Disable Upper Screen
ReturnDisableUpper
        setprop ReturnSearchKey,enabled=0
        setprop ReturnOK,enabled=0
        setprop ReturnExit,enabled=0
        setprop ReturnPrint,enabled=0
        setprop ReturnNew,enabled=0
        setprop ReturnSearchList,enabled=0,bgcolor=grey
        return

.Enable Upper Screen
ReturnEnableUpper
.Allow Exit
        move    "Y",ExitFlag
        setprop ReturnSearchKey,enabled=1
        setprop ReturnOK,enabled=1
        setprop ReturnExit,enabled=1
        setprop ReturnPrint,enabled=1
        setprop ReturnNew,enabled=1
        setprop ReturnSearchList,enabled=1,bgcolor=white
        return

.Disable Lower Screen
ReturnDisableLower
        setprop ReturnEditAddress,enabled=0,bgcolor=grey
        setprop ReturnEditAddress2,enabled=0,bgcolor=grey
        setprop ReturnEditCity,enabled=0,bgcolor=grey
        setprop ReturnEditContact,enabled=0,bgcolor=grey
        setprop ReturnEditCompany,enabled=0,bgcolor=grey
        setprop ReturnEditDate,enabled=0,bgcolor=grey
        setprop ReturnEditFax,enabled=0,bgcolor=grey
        setprop ReturnEditNumber,enabled=0,bgcolor=grey
        setprop ReturnEditState,enabled=0,bgcolor=grey
        setprop ReturnEditTelephone,enabled=0,bgcolor=grey
        setprop ReturnEditUser,enabled=0,bgcolor=grey
        setprop ReturnEditZip,enabled=0,bgcolor=grey
        setprop ReturnEditCountry,enabled=0,bgcolor=grey
.begin patch 1.7
        setprop ReturnEditCompN,enabled=0,bgcolor=grey
        setprop ReturnEditCntN,enabled=0,bgcolor=grey
.end patch 1.7

        setprop ReturnStatNumMssg,visible=0
        setprop ReturnModify,enabled=1
        setprop ReturnCancel,visible=0
.        setprop ReturnUpdate,visible=0
.        setprop ReturnDelete,visible=0
        return

.Enable Lower Screen
ReturnEnableLower
        move    "N",UpdateFlag
        move    "N",ExitFlag
        setprop ReturnEditAddress,enabled=1,bgcolor=white
        setprop ReturnEditAddress2,enabled=1,bgcolor=white
        setprop ReturnEditCity,enabled=1,bgcolor=white
        setprop ReturnEditContact,enabled=1,bgcolor=white
        setprop ReturnEditCompany,enabled=1,bgcolor=white
        setprop ReturnEditDate,enabled=1,bgcolor=white
        setprop ReturnEditFax,enabled=1,bgcolor=white
        setprop ReturnEditNumber,enabled=1,bgcolor=white
        setprop ReturnEditState,enabled=1,bgcolor=white
        setprop ReturnEditTelephone,enabled=1,bgcolor=white
        setprop ReturnEditUser,enabled=1,bgcolor=white
        setprop ReturnEditZip,enabled=1,bgcolor=white
        setprop ReturnEditCountry,enabled=1,bgcolor=white
        setprop ReturnStatNumMssg,visible=1
        setprop ReturnModify,enabled=0
        setprop ReturnCancel,visible=1
.        setprop ReturnUpdate,visible=1
        SetItem ReturnCheck001,0,0                             .default ??
.begin patch 1.7
        setprop ReturnEditCompN,enabled=1,bgcolor=white
        setprop ReturnEditCntN,enabled=1,bgcolor=white
.end patch 1.7

.        setprop ReturnDelete,visible=1
        return

.Verify Data Entry
ReturnVerifyData
        getitem ReturnEditNumber,0,RTNUM
        call    TRIM using RTNUM
        count   HowMany,RTNUM
        if (HowMany < 4)
                alert caution,"4 Digit Return-To Number Required!",result
                setfocus ReturnEditNumber
                move "Y",ReturnFlag
                return
        endif
        setitem ReturnEditNumber,0,RTNUM
.
        getitem ReturnEditContact,0,RTCNTCT
        call    TRIM using RTCNTCT
        setitem ReturnEditContact,0,RTCNTCT     
.
        getitem ReturnEditCompany,0,RTCOMP
        call    TRIM using RTCOMP
        count   HowMany,RTCOMP
        if (HowMany = 0)
                alert caution,"Company Name Required!",result
                setfocus ReturnEditCompany
                move "Y",ReturnFlag
                return
        endif
        setitem ReturnEditCompany,0,RTCOMP
.
        getitem ReturnEditAddress,0,RTADDR
        call    TRIM using RTADDR
        count   HowMany,RTADDR
        if (HowMany = 0)
                alert caution,"Address Required!",result
                setfocus ReturnEditAddress
                move "Y",ReturnFlag
                return
        endif        
        setitem ReturnEditAddress,0,RTADDR
.
        getitem ReturnEditAddress2,0,RT2ADDR
        call    TRIM using RT2ADDR
        setitem ReturnEditAddress2,0,RT2ADDR        
.
        getitem ReturnEditCity,0,RTCITY
        call    TRIM using RTCITY
        count   HowMany,RTCITY
        if (HowMany = 0)
                alert caution,"City Required!",result
                setfocus ReturnEditCity
                move "Y",ReturnFlag
                return
        endif        
        setitem ReturnEditCity,0,RTCITY
.
        getitem ReturnEditState,0,RTSTATE
        call    TRIM using RTSTATE   
        setitem ReturnEditState,0,RTSTATE
.
        getitem ReturnEditZip,0,RTZIP
        call    TRIM using RTZIP         
        setitem ReturnEditzip,0,RTZIP
.
        getitem ReturnEditCountry,0,RTCOUN
        call    TRIM using RTCOUN     
        setitem ReturnEditCountry,0,RTCOUN        
.
        move    NPASuser,RTNAME
        setitem ReturnEditUser,0,RTNAME
.
        if (UpdateFlag = "Y")
                unpack newdate,MM,STR1,DD,STR1,CC,YY
        else
                unpack olddate,MM,STR1,DD,STR1,CC,YY
        endif
        pack    RTREVDAT,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        setitem ReturnEditDate,0,str10
.        
        clear   Newnum
        getitem ReturnEditTelephone,0,HowMany
        getitem ReturnEditTelephone,0,Newnum
        if (HowMany <> 0)
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10)
                        alert   caution,"10 Digit Number Only!",result
                        setfocus ReturnEditTelephone
                        move    "Y",ReturnFlag
                        return
                endif
        endif
        move    Newnum,RTTELE
        count   N9,Newnum
        if (N9 > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif
        setitem ReturnEditTelephone,0,Newnum
.
        clear   Newnum
        getitem ReturnEditFax,0,HowMany
        getitem ReturnEditFax,0,Newnum
        if (HowMany <> 0)
                if (HowMany = 13)
                        unpack  Newnum,str1,area,str1,str3,str1,str4
                        pack    Newnum,area,str3,str4
                elseif (HowMany <> 10)
                        alert caution,"10 Digit Number Only!",result
                        setfocus ReturnEditFax
                        move "Y",ReturnFlag
                        return
                endif
        endif
        move    Newnum,RTFAX
        count   N9,Newnum
        if (N9 > "0")
                unpack  Newnum,area,str3,str4
                pack    Newnum,"(",area,")",str3,dash,str4
        endif        
        setitem ReturnEditFax,0,Newnum
.begin patch 1.6.3
          GetItem   ReturnCheck001,0,n1
          if        (n1=c1)
          move      "N",RTActive
          else
          clear     RTActive
          endif
.end patch 1.6.3
.        
.begin patch 1.7
          setitem   ReturnStatCompN,0,""
          setitem   ReturnStatCntN,0,""
          getitem   ReturnEditCompN,0,str6
          call      TRIM using str6
          count     n2,str6
          if        (n2 = 6)
          move      str6,rtCompN
          Elseif    (n2 = 5)
          pack      rtCompN from "0",str6
          Elseif    (n2 = 4)
          pack      rtCompN from "00",str6
          Elseif    (n2 = 3)
          pack      rtCompN from "000",str6
          Elseif    (n2 = 2)
          pack      rtCompN from "0000",str6
          Elseif    (n2 = 1)
          pack      rtCompN from "00000",str6
          Else
          Clear     rtCompN
          count   HowMany,RTCompN
                    if (HowMany < 6)
                    alert caution,"6 Digit Master Company ## Should be entered!",result
                    setfocus ReturnEditCompn
.                    move "Y",ReturnFlag
.                    return
                    endif
          endif
          getitem   ReturnEditCntN,0,str3
          call      TRIM using str3
          count     n2,str3
          if        (n2 = 3)
          move      str3,rtCntN
          Elseif    (n2 = 2)
          pack      rtCntN from "0",str3
          Elseif    (n2 = 1)
          pack      rtCntN from "00",str3
          else
          Clear     rtCntN
          count   HowMany,RTCntN
                    if (HowMany < 3)
                    alert caution,"3 Digit Master Contact ## Should be entered!",result
                    setfocus ReturnEditCntN
.                    move "Y",ReturnFlag
.                    return
                    endif
          endif
          setitem   ReturnEditCompN,0,rtCompN
          setitem   ReturnEditCntN,0,rtCntN
          if        (rtCompN <> "000000")
          packkey   compfld,rtCompN
          call      compkey
                    if        not over
                    setitem   ReturnStatCompN,0,compcomp
                    endif
          Packkey   CNCTFLD,rtCompN,rtCntN
          call      CNCTKEY
                    if        not over
                    setitem   ReturnStatCntN,0,CNCTFNAME
                    endif
          endif     
.end patch 1.7
        packkey NRTNFLD,RTNUM
.        packkey        NRTNFLD2,RTCOMP
        return

RefreshReturnSearchList
.Clear DataList
        deleteitem ReturnSearchList,0
.First Test for Blank Entry After "?"
        count    result,key
        if (result < 3)
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Name Must Have 3+ Characters"
                setprop ErrorMssg,visible=1
                call    SetReturnErrorMssgDefault
                setfocus ReturnSearchKey
                setprop ReturnOK,enabled=1
                return
        endif
        setprop ReturnSearchList,enabled=1,bgcolor=white
        move    C3,NRTNLOCK
        pack    NRTNFLD2,AKey1,key
        move    "Driver-NRTNAIM",Location
        call    NRTNAIM
        if Over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                setprop ErrorMssgStat5,visible=0
                setprop ErrorMssgStat1,visible=1
                setprop ErrorMssgStat2,visible=1
                setprop ErrorMssgStat3,visible=1
                setprop ErrorMssgStat4,visible=1

                setprop ReturnOK,enabled=1
                setfocus ReturnSearchKey
                return
        else
                move "Y",AamFlag
                pack hold,RTNUM,B1,RTCOMP,RTCNTCT,RTADDR,RT2ADDR,RTCITY:
.begin patch 1.7
.                RTSTATE,RTZIP,RTCOUN,RTNAME,RTREVDAT,RTBLANK,RTTELE,RTFAX
                RTSTATE,RTZIP,RTCOUN,RTNAME,RTREVDAT,RTBLANK,RTTELE,RTFAX,RTCOMPN,RTCNTN
                insertitem ReturnSearchList,9999,hold
.end patch 1.7

        endif
        move    "Driver-NRTNKG",Location
        loop
                call NRTNKG
                until over
                        pack hold,RTNUM,B1,RTCOMP,RTCNTCT,RTADDR,RT2ADDR,RTCITY:
.begin patch 1.7
.                RTSTATE,RTZIP,RTCOUN,RTNAME,RTREVDAT,RTBLANK,RTTELE,RTFAX
                RTSTATE,RTZIP,RTCOUN,RTNAME,RTREVDAT,RTBLANK,RTTELE,RTFAX,RTCOMPN,RTCNTN
                insertitem ReturnSearchList,9999,hold
.end patch 1.7
        repeat
.Put focus on first item in DataList
        setitem ReturnSearchList,1,1
.Load the Inquiry Screen immediately
        call ReturnData
        return
          
ReturnGetNext
.Locate and Display next logical Return Number
        move    "NRTNNXT",GNXTFLD
        call    GNXTKEY 
        if over
.Change StatText Boxes For Error Message
                setprop ErrorMssgStat1,visible=0
                setprop ErrorMssgStat2,visible=0
                setprop ErrorMssgStat3,visible=0
                setprop ErrorMssgStat4,visible=0
                setprop ErrorMssgStat5,visible=1
                setitem ErrorMssgStat5,0,"  Error Getting Return-To ID Number!"
                setitem ErrorMssgOK,0,"&Stop"
.Display Error Message
                setprop ErrorMssg,visible=1
.Reset StatText Boxes
                call    SetReturnErrorMssgDefault
.Terminate Program
                stop
        endif
        bump    GNXTNUM,2
        move    GNXTNUM,newrtn
        loop
                add     C1,newrtn
                pack    NRTNFLD,newrtn
                rep     zfill,NRTNFLD
.                add     C1,newrtn
.                move    newrtn,RTNUM
.                rep     zfill,RTNUM
.                pack    NRTNFLD,RTNUM
.                rep     zfill,NRTNFLD                
                call    NRTNTST
                until over
        repeat
        return

.Print Heading
ReturnPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        add     C1,page
        move     "300",row
        prtpage prfile;*UNITS=*HIENGLISH;
        clear   column
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*font=font2,mm,slash,dd,slash,cc,yy,*font=font3;
        move    "2800",column
        prtpage prfile;*pcolumn:row,*font=font1,*ulon,"Names In The News";
        prtpage prfile;*p6000:row,*font=font2,*uloff,"page ",page
        add     sixlpi,row
        add     "200",column
.START PATCH 1.62 REMOVED LOGIC
.        prtpage prfile;*pcolumn:row,*font=font4,"California Inc.";
.END PATCH 1.62 REMOVED LOGIC
        add     sixlpi,row
        add     sixlpi,row
        sub     "250",column
        prtpage prfile;*pcolumn:row,*font=font2,"MASTER Return-To Listing";
        add     eightlpi,row 
        add     eightlpi,row                  
        return

ReturnPrintColumn
        add     eightlpi,row
        add     C1,result
      
.Logic behind this is to hold the row for the three different "columns" in N8
.Only allowing N8 to increment to the next "row" of records when you hit the
.first in that "column".
        if (result <= C1)
                move row,N8        
        elseif (result = C2)
                move N8,row
                move column2,column
        elseif (result = C3)
                move N8,row
                move column3,column
        elseif (result > C3)
.Start next row and skip two lines
                move    C1,result
                clear   column
                clear   N2
                loop
                        add eightlpi,N8
                        add C1,N2
                        until (N2 = "10")
                repeat
                move    N8,row
                if (row > "10000")
.10720 equals 7 rows of records                
rowtest                
                        prtpage prfile;*NEWPAGE;
                        call    ReturnPrintHeader
                        clear   column
                        add     eightlpi,row
                        move    row,N8
                endif                  
        endif
        return
ReturnPrintRecord
                call    ReturnPrintColumn
                prtpage prfile;*pcolumn:row,*boldon,"##",RTNUM,*boldoff;
                add     eightlpi,row
                prtpage prfile;*pcolumn:row,RTCNTCT;
                add     eightlpi,row
                prtpage prfile;*pcolumn:row,RTCOMP;
                add     eightlpi,row
.Decide whether Address2 should be printed
                call    Trim using RT2ADDR
                count   N5,RT2ADDR
                if (N5 <> "0")
                        call    Trim using RTADDR
                        pack    str55,RTADDR,", ",RT2ADDR
                else
                        pack    str55,RTADDR
                endif
                prtpage prfile;*pcolumn:row,str55;                
                add     eightlpi,row
                prtpage prfile;*pcolumn:row,RTCITY,B1,RTSTATE,B1,RTZIP,B1,RTCOUN;
                add     eightlpi,row
                prtpage prfile;*pcolumn:row,"Updated: ",olddate;
                add     eightlpi,row
                prtpage prfile;*pcolumn:row,"By: ",RTNAME;
                return
                              
.Include IO file
        include comlogic.inc
        include nrtnio.inc
        include npasio.inc
        include gnxtio.inc
.begin patch 1.7        
          include compio.inc
          include cntio.inc
           include    Ntypio.inc
          include Compnotesio.inc       
.end patch 1.7        
