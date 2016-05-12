PC       EQU       0
         INC       COMMON.inc
         include   cons.inc
         INCLUDE   HP.INC
         include   nowndd.inc
.Patch1.8
                              include   compdd.inc
                              include   cntdd.inc
.         include   nMLRdd.inc
.Patch1.8
         INCLUDE   NUSEDD.INC
         INCLUDE   NDATDD.INC
.ADDES FOR SEARCH PLF........................
.Patch1.8
.         include nbrkdd.inc
.Patch1.8
         include nrtndd.inc
         include ncmpdd.inc
         include norddd.inc
         include winapi.inc
         include ncntdd.inc
.begin release init 1.4
         INCLUDE   NREFDD.INC
         include   nmdldd.inc
.end release init 1.4
.START PATCH 2.0 REMOVED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         INCLUDE   NFULDD.INC
..END PATCH 1.5 ADDED LOGIC
.END PATCH 2.0 REMOVED LOGIC

.START PATCH 1.73 ADDED LOGIC
          INCLUDE   NADDDD.INC
          INCLUDE   NARRDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NSRCDD.INC
          INCLUDE   NTXTDD.INC
.END PATCH 1.73 ADDED LOGIC
Release   Init      "2.41"     DLH ALLOW OVERRIDE OF INVALID FAX #
Reldate   Init      "17 Nov 2011"
.Release   Init      "2.4"     DLH add Email info
.Reldate   Init      "11 Nov 2011"
.Release   INit      "2.3"     DLH additional fax # checking
.Reldate   Init      "14 March 2011"
.Release   INit      "2.2"     DLH Move batch server
.Release  INit      "2.1"     DLH 24 May 2007  PLI
.RELEASE   INIT       "2.0"    DMS 22JUN2006 FULFILLMENT CONVERSION
.RELEASE   INIT       "1.9"    ASH 26AUG2005 Fixed Datacard view Bug - Work Order:  884
.RELEASE   INIT       "1.8"    DMB 26MAY2004 Mailer Conversion
.RELEASE   INIT       "1.73"    ASH 29JAN2004 DATACARD CONVERSION
.RELEASE   INIT       "1.72"    DMB 15JAN2003 Added our list number to tempfile being built
.RELEASE   INIT       "1.71"   DMB 10OCT2002 Changed Name from user file to contact file
.RELEASE   INIT       "1.7"    DMB 16SEP2002 Added check box for a usage request
.RELEASE   INIT       "1.6"    DMB 10SEP2002 Added check box to added statement to include datacard
.RELEASE   INIT      "1.5"     ASH 01MAY2002 NINFUL CONVERSION
.RELEASE   INIT      "1.4"     DLH 10Apr02  add basic datacard display
.RELEASE   INIT      "1.3"     DMB 12/06/01 Change to Omit SCF Box on fax and replace with Text when box is check by user
.RELEASE   INIT      "1.21"    DMB 08/15/01 Select field now highlights field when field receives focus
.RELEASE   INIT      "1.2"     DMB 07/09/01 ADDED CODE TO DISPLAY DATE CORRECTLY REPL CC WITH STR2
.RELEASE  INIT       "1.1"     DMB 06/22/01 Added Code for clearing of scf variables after intial faxing
.RELEASE  INIT       "1.0"     DMB 04/25/01 New GUI Request for Counts
........................................................................................
.Files to open
.tempfile file
.START PATCH 1.9 ADDED LOGIC
CreateWebCard external "NDAT002W;CreateWebCard"
.END PATCH 1.9 ADDED LOGIC
Dcard    DIM       1           ;code to include statement to submit datacard
Omit     DIM       1           ;code to omit scf request box
SecondReq dim   1
NRCOSLCT DIM       35
NRCOCMNT DIM       60
NRCOG01  DIM       10
NRCOG02  DIM       10
NRCOG03  DIM       10
NRCOG04  DIM       10
NRCOG05  DIM       10
NRCOG06  DIM       10
NRCOG07  DIM       10
NRCOG08  DIM       10
NRCOG09  DIM       10
NRCOG10  DIM       10
NRCOG11  DIM       10
NRCOG12  DIM       10
NRCOG13  DIM       10
NRCOG14  DIM       10
NRCOG15  DIM       10
LONGDIST DIM       1
HOLDMLR  DIM       4
userinfo dim       500
userlogn dim       7
userlogw dim       7
BEGIN    FORM      2
LAST     FORM      2
badfaxflag dim     1            =Y if Facsys printer not defined
.osflag   form   1          1=win 95,98, 2=NT
Carr    init    0x7f
+..............................................................................
fon      dim    14
prefix   dim    3
LPAREN   INIT   "("
RPAREN   INIT   ")"
AREA     INIT     "510"
yesno1   integer  1,"0x000004"
CheckFlg form    1
..............................................................
.START PATCH 1.9 REMOVED LOGIC
..begin patch 1.4
.Juldate        form      5
.holdate        dim       10
.headtxt        dim     80
.headtxt1       dim     80
..Colors
.white   color
.grey    color
.RED     COLOR
.BLACK   COLOR
.Yellow  Color
.Green   Color
.Blue    Color
..Define Fonts to be used
.font1   font
.font2   font
.font3   font
.font4   font
.font5   font
.oldflag  dim       1             Y=yes card older than 36 months
.NRcoColl1      Collection
.addrdata        dim     500           subset of pg1data
.arrdata        dim     500           subset of pg1data
.srcedata        dim     500           subset of pg1data
.magdata         dim     100           subset of pg1data
.seldata        dim     500           subset of pg1data
.clndata         dim     100           subset of pg1data
.netdata         dim     100           subset of pg1data
.deldata         dim     100           subset of pg1data
.samdata         dim     100           subset of pg1data
.sexdata         dim     17           subset of pg1data
.rtndata         dim     500           subset of pg1data
.pg1data         dim     2256
.cr       init      0177           .causes line feed cr in edittext
.DESC18   DIM       18
.DESC14   DIM       14
.DESC10   DIM       10
.DESC20   DIM       20
.DESC38   DIM       38     *NET INFO ONLY?
.TELEDISP DIM       13
.TELEMASK INIT      "(999)999-9999"
..end patch 1.4
.END PATCH 1.9 REMOVED LOGIC
TabNum    form   "01"
.START PATCH 1.73 ADDED LOGIC
hold2     dim       4500      .length of largest possible text record
.END PATCH 1.73 ADDED LOGIC
..............................................................

.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
.START PATCH 1.9 REMOVED LOGIC
..begin patch 1.4
.mView           Menu
..end patch 1.4
.END PATCH 1.9 REMOVED LOGIC
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
.FData   init    "&File;&Print;Pre&view;-;E&xit"
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
.START PATCH 1.9 REMOVED LOGIC
.VData           Init          ")View;":                          ;select datacard info view
.                         "/C)Counts",011,"CTRL-C;":           ;Main view
.                         "/D)Datacard",011,"CTRL-D;"       ;datacard info
.END PATCH 1.9 REMOVED LOGIC
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
..................................................................................

         MOVE      C1 TO NMLRPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NDATPATH
         move      c1 to nusepath
         CLOCK     DATE TO TODAY
       getinfo  system,str6
       unpack   str6 into str1,str2
       unpack   str2 into str1
       move     c0 to osflag
. 0 = unknown
. ..1 = Windows NT
. ..2 = WIN32s Windows 3.1x (obsolete)
. ..3 = Window 95
. ..4 = Window 98
. ..5 = Windows 2000
. ..8 = Windows CE
       if       (str1 = "3" or str1 = "4")
       move     c1 to osflag
       endif
       if       (str1 = "1" or str1 = "5" or str1 = "6")
       move     c2 to osflag
       endif
.......................................
.         DISABLED FOR TESTING ONLY
.         move      c0 to nusefld
........................................
.test
..         MOVE      c0 TO NUSEFLD
..         move      "119",nusefld                            .HARDCODE FOR TESTING
         MOVE      PORTN TO NUSEFLD .removed FOR TESTING only
         REP       ZFILL IN NUSEFLD
         CALL      NUSEKEY

         goto      userng if over
         scan      "INVALID" in nuseuser
         goto      userng if equal
         reset     nuseuser
         scan      "LOGIN" in  userinfo
         if         equal
         bump      userinfo by 6
         clear     userlogw
         move      userinfo,userlogw
         MOVEFPTR  userlogw TO BEGIN
         SCAN      "," IN userlogw
         if        equal
         MOVEFPTR  userlogw TO LAST
         SUB       C3 FROM LAST
         RESET     userlogw
         SETLPTR   userlogw TO LAST
         clear     userlogn
         APPEND    userlogw TO userlogn
         reset     userlogn
         goto      start1
         endif
         endif
         move      userinfo,userlogn
.note  nuseuser must match notework user name exactly!!!!!!!!!!!!
START1
        call    Trim using NUSEUSER
        scan    "BILLING",NUSEUSER
        if not equal
                move    NUSEUSER,str1
                loop
                        bump    NUSEUSER,1
                        cmatch  B1,NUSEUSER
                        until equal
                        until eos
                repeat
                if not eos
                        bump    NUSEUSER,1
                        move    NUSEUSER,str6
                        clear   userlogn
                        pack    userlogn,str1,str6
                        reset   nuseuser      .reset var for correct display on form
                endif
        endif

.;
        move c3 to ncntpath
        move portn to ncntfld1
        REP  ZFILL IN ncntfld1
        call ncntkey
.;
        move    "NRCO0001.PLS",Wprognme
        move    "Request for Counts",Wfunction
        move    "David Baca",Wauthor
        move    release,Wrelease
.        move    "March 2002",Wreldate
          move      Reldate,Wreldate
srch    plform  Search
abt     plform  About
.START PATCH 1.9 REPLACED LOGIC
..begin release 1.4
.NDATA              plform  NDat1111A
.NDATB              plform  NDat1111B
..end release 1.4
NRCO001A plform     NRCO001A
NDAT001B plform     NDAT001B
.END PATCH 1.9 REPLACED LOGIC
x       plform  Nrco0001
.x plform  Devnrco0001

     winhide

  formload x
.START PATCH 1.9 REPLACED LOGIC
..begin release 1.4
.        formload   NDatA,Nrco0001
.        formload   NDatB,Nrco0001
..end release 1.4
          formload NRCO001A,NRCO0001
          formload NDAT001B,NRCO0001
.END PATCH 1.9 REPLACED LOGIC
  formload srch
  formload abt
  create  NRCO0001;mFile,FData
.  create  DevNRCO0001;mFile,FData
  create  NRCO0001;mEdit,EData,mFile
.  create  DevNRCO0001;mEdit,EData,mFile
  create  NRCO0001;mOptions,OData,mEdit
.  create  DevNRCO0001;mOptions,OData,mEdit
  create  NRCO0001;mHelp,HData,mOptions
.START PATCH 1.9 REMOVED LOGIC
..begin release 1.4
.        CREATE               Nrco0001;mView,VData,mOptions
..Create Colors
.        create  white=*white
.        create  grey=*ltgray
.        create  RED=*RED
.        create  black=*black
.        create  Yellow=*yellow
.        create  Green=*Green
.        create  Blue=*Blue
..Create fonts to be used
.        create  font1,"FIXED",size=10
.        create  font2,"FIXED",size=10,bold
.        create  font3,"FIXED",size=10,bold,italic
.        create  font4,"Arial",size=14,italic
.               ListIns        NRcoColl1,NrcoEditList,NrcoAdd,NrcoCheckOmit,NrcoCheckRequest,NrcoDataScf,NrcoEditCnt:
.                              NrcoEditComm,NrcoEditDate,NrcoEditFax,NrcoEditMlrNme,NrcoEditOwn,NrcoEditRegion,NrcoEditSelect:
.                              NrcoEditText001,NrcoExit,NrcoFax,NrcoGroupBox0001,NrcoGroupBox002,NrcoMlrNo,NrcoRemove:
.                              NrcoStatLstNm,NrcoStatRevDate,NrcoStatText001,NrcoStatText002,NrcoStatText003,NrcoStatText004:
.                              NrcoStatText005,NrcoStatText006,NrcoStatText007,NrcoStatText008,NrcoStatText009,NrcoStatText010:
.                              NrcoStatText011,NrcoStatText012,NrcoStatText013,NrcoStatTextOComp,NrcoStatUser
.
..end release 1.4
.END PATCH 1.9 REMOVED LOGIC
.  create  DevNRCO0001;mHelp,HData,mOptions

.Create SubMenu
  create  NRCO0001;sSearch,SData,mOptions,1
.  create  DevNRCO0001;sSearch,SData,mOptions,1
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.START PATCH 1.9 REMOVED LOGIC
..begin release 1.4
.        Activate            mView,ViewGo,result
..end release 1.4
.END PATCH 1.9 REMOVED LOGIC
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.Activate SubMenus
  activate sSearch,SearchGo,result
.START PATCH 1.9 REPLACED LOGIC
..begin patch 1.4
.               move           c1 to result
.               call           ViewGo
..end patch 1.4
          Data2WebBrowser.Navigate2 USING "about:blank"
          setprop   Data2StatList,top=30
          setprop   Data2StatListNum,top=30
.         setprop   Data2WebBrowser,top=50,width=620
          setprop   Data2WebBrowser,top=50,width=715,height=380
.END PATCH 1.9 REPLACED LOGIC
  move c1,checkflg
.;Patch1.71 to show contact Name
  if (ncntname = "")
          alert caution,"Can't Find your name in Contact File - Call I.S.!",result,"Call I.S."
                    stop
  else
            setitem NrcoStatUser,0,CNTNAME
  endif
.  setitem NrcoStatUser,0,NUSEUSER
.; Subpatch1.71
           EVENTREG  X, 17, XRESIZE

  SetFocus NrcoEditList
  loop
        waitevent
  repeat

LISTCHECK
  clear ndatfld
  getitem NrcoEditList,0,ndatfld

Test
 branch CheckFlg to ListCheck1,ListCheck2
ListCheck1
  if (ndatfld = "")
                setitem       NrcoStatLstNm,0,""
                setitem         NrcoStatRevDate,0,""
                call ownerclear
                return
  else
        goto ListCheck3
  endif
ListCheck2
  if (ndatfld = "")
                    alert caution," Enter a Valid List #!", result
                setfocus NrcoEditlist
                return
  endif

Listcheck3
  call zfillit using ndatfld,C0
  call ndatkey
  if over
          alert caution," Enter a Valid List #!", result
                setfocus NrcoEditList
                return
  endif
.  cmatch "C" to elstcde
.  if equal
	if	(elstcde = "C" or Elstcde = "P")
          alert caution," NIN managed lists are not allowed!", result
          setfocus NrcoEditList
          return
 endif

 reset Whitney
.START PATCH 1.73 REPLACED LOGIC
. scan ownnum in Whitney
          unpack    OWNNUM,str2,str4
          scan      str4,Whitney
.END PATCH 1.73 REPLACED LOGIC
 if equal
        alert caution," Whitney lists are not allowed!", result
        setfocus NrcoEditList
        return
 endif
.START PATCH 1.9 REMOVED LOGIC
..begin patch 1.4
..START PATCH 1.73 REPLACED LOGIC
..               move      revdate to holdate
..               unpack    revdate to mm,str1,dd,str1,cc,yy
.                   unpack    REVDATE,CC,YY,MM,DD
.                   pack      holdate,MM,SLASH,DD,SLASH,CC,YY
.                   move      holdate,str10
..END PATCH 1.73 REPLACED LOGIC
.               call      cvtjul
.               move      juldate to n5
.               sub       juldays from n5
.               if        (n5 > 1095)         difference greater than 36 months
.               move      yes to oldflag
.               else
.               move      no to oldflag
.               endif
.
.               MOVE      lstnum TO NMDLFLD
.               REP       ZFILL IN NMDLFLD
.               setprop       NDat0001AEditText005,enabled=1,FGColor=Black,Font=Font3
.               CALL      NMDLKEY
.               if        not over
.               setprop       NDat0001AEditText005,enabled=1,FGColor=Green,Font=Font3
.               endif
.
.               pack   headtxt from mlstname
..               pack   headtxt from mlstname,b10,"mlr##",nxrfmlr
..START PATCH 1.73 REPLACED LOGIC
..               pack   headtxt1 from olstname,b10,revdate,b10,lstnum
.               pack   headtxt1 from olstname,b10,str10,b10,lstnum
..END PATCH 1.73 REPLACED LOGIC
.              setitem       NDat0001AEditText005,0,Headtxt
.              setitem       NDat0001AStatText003,0,"Unit of Sale"
.              setprop       NDat0001AStatText003,Font=Font1,Visible=0
.
.              IF        ("W" = status)
.              setprop       NDat0001AEditText005,enabled=1,FGColor=Yellow,Font=Font3
.              endif
.
.               cmatch         yes to oldflag
.               if             equal
.               setitem       NDat0001AEditText004,0,Headtxt1
.               setprop       NDat0001AEditText004,enabled=1,FGColor=REd,STYLE=3Dflat,Font=Font3
.        else
.               setitem       NDat0001AEditText004,0,Headtxt1
.               setprop       NDat0001AEditText004,enabled=1,FGColor=Black,STYLE=3DOut,Font=Font3
.               endif
.
.               SETMODE *LTGRAY=ON
..START PATCH 1.73 REPLACED LOGIC
..                  SETITEM   Ndat0001AEditText001, 0, TEXTData
.         setitem   Ndat0001AEditText001,0,""     .Initialize object
.         clear     NTXTTEXT
.         clear     hold2
.         for N1,C1,"9"
.                   pack      NTXTFLD,LSTNUM,N1
.                   move      "NTXTKEY",Location
.                   pack      KeyLocation,"Key: ",NTXTFLD
.                   call      NTXTKEY
.                   until over
.                   append    NTXTTEXT,hold2
.         repeat
.         if (hold2 <> "")
.                   reset     hold2
.                   call      Trim using hold2
.         endif
.         setitem   Ndat0001AEditText001,0,hold2
..END PATCH 1.73 REPLACED LOGIC
.         SetProp   Ndat0001AEditText001,Font=Font1
.         Setitem   NDat0001AEditText002,0,Unitdata
.               Setprop NDat0001AEditText002,Font=font1,BGColor=blue,FGColor=White,visible=0
..START PATCH 1.73 REPLACED LOGIC
..         MOVE      ADDCDE1 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    cr,addrdata
..         append    "******Addressing******",addrdata
..         append    cr,addrdata
..         append    desc18,addrdata
..         append    cr,addrdata
..         MOVE      ADDCDE2 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    desc18,addrdata
..         append    cr,addrdata
..         MOVE      ADDCDE3 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    desc18,addrdata
..         append    cr,addrdata
..         MOVE      ADDCDE4 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    desc18,addrdata
..         append    cr,addrdata
..         MOVE      ADDCDE5 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    desc18,addrdata
..         append    cr,addrdata
..         MOVE      ADDCDE6 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    desc18,addrdata
..         append    cr,addrdata
..         MOVE      ADDCDE7 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DSRC IF OVER
..         MOVE      NREFDESC TO DESC18
..         append    desc18,addrdata
..         append    cr,addrdata
..DSRC     reset     addrdata
..         MOVE      SCODE1 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    cr,srcedata
..         append    "********Source********",srcedata
..         append    cr,srcedata
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep1,srcedata
..         append    cr,srcedata
..         MOVE      SCODE2 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep2,srcedata
..         append    cr,srcedata
..         MOVE      SCODE3 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep3,srcedata
..         append    cr,srcedata
..         MOVE      SCODE4 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep4,srcedata
..         append    cr,srcedata
..         MOVE      SCODE5 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep5,srcedata
..         append    cr,srcedata
..         MOVE      SCODE6 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep6,srcedata
..         append    cr,srcedata
..         MOVE      SCODE7 TO NREFFLD
..         CALL      NREFKEY
..         GOTO      DARR IF OVER
..         MOVE      NREFDESC TO DESC14
..         append    desc14,srcedata
..         append    dash,srcedata
..         append    scodep7,srcedata
..         append    cr,srcedata
..DARR     reset     srcedata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,ARRCODE1;ARRCODE1,DESC18
..         GOTO      DMAG IF OVER
..         append    cr,arrdata
..         append    "*****Arrangement*****",arrdata
..         append    cr,arrdata
..         append    desc18,arrdata
..         append    cr,arrdata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,ARRCODE2;ARRCODE2,DESC18
..         GOTO      DMAG IF OVER
..         append    desc18,arrdata
..         append    cr,arrdata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,ARRCODE3;ARRCODE3,DESC18
..         GOTO      DMAG IF OVER
..         append    desc18,arrdata
..         append    cr,arrdata
..DMAG     reset     arrdata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,MAGSPEC1;MAGSPEC1,DESC18
..         GOTO      DSEL IF OVER
..         append    cr,magdata
..         append    "*****Magnetic media*****",magdata
..         append    cr,magdata
..         append    desc18,magdata
..         append    cr,magdata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,MAGSPEC2;MAGSPEC2,DESC18
..         GOTO      DSEL IF OVER
..         append    desc18,magdata
..         append    cr,magdata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,MAGSPEC3;MAGSPEC3,DESC18
..         GOTO      DSEL IF OVER
..         append    desc18,magdata
..         append    cr,magdata
..DSEL     reset     magdata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE1;SELCDE1,DESC10
..         GOTO      DCLN IF OVER
..         append    cr,seldata
..         append    "******Selections******",seldata
..         append    cr,seldata
..         append    desc10,seldata
..         append    at,seldata
..         append    sel1m,seldata
..         append    cr,seldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE2;SELCDE2,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel2m,seldata
..         append    cr,seldata
..         READ      NREFFILE,SELCDE3;SELCDE3,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel3m,seldata
..         append    cr,seldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE4;SELCDE4,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel4m,seldata
..         append    cr,seldata
..         READ      NREFFILE,SELCDE5;SELCDE5,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel5m,seldata
..         append    cr,seldata
..         READ      NREFFILE,SELCDE6;SELCDE6,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel6m,seldata
..         append    cr,seldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE7;SELCDE7,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel7m,seldata
..         append    cr,seldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE8;SELCDE8,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel8m,seldata
..         append    cr,seldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE9;SELCDE9,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel9m,seldata
..         append    cr,seldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,SELCDE10;SELCDE10,DESC10
..         GOTO      DCLN IF OVER
..         append    desc10,seldata
..         append    at,seldata
..         append    sel10m,seldata
..         append    cr,seldata
..DCLN     reset     seldata
..         MATCH     "C02",CLEANCDE
..         GOTO      DCLN2 IF EQUAL
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,CLEANCDE;CLEANCDE,DESC38
..         GOTO      DNET IF OVER
..         append    cr,clndata
..         append    " ******Cleaned******",clndata
..         append    cr,clndata
..         append    desc38,clndata
..         append    cr,clndata
..         GOTO      DNET
..DCLN2
..         append    clninfo,clndata
..         append    cr,clndata
..DNET     reset     clndata
..         append    cr,netdata
..         append    "*Net arrangement*",netdata
..         append    cr,netdata
..         MATCH     "N02",NETNAME
..         GOTO      DNET2 IF EQUAL
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,NETNAME;NETNAME,DESC38
..         GOTO      DDEL IF OVER
..         append    desc38,netdata
..         append    cr,netdata
..         GOTO      DDEL
..DNET2
..         append    netinfo,netdata
..         append    cr,netdata
..DDEL     reset     netdata
..         append    cr,deldata
..         append    "*****Delivery*****",deldata
..         append    cr,deldata
..         filepi    1;nreffile           .dlh 07MAY93
..         READ      NREFFILE,DELCODE;DELCODE,DESC38;
..         GOTO      DSAM IF OVER
..         append    desc38,deldata
..         append    cr,deldata
..DSAM     reset     deldata
..         append    cr,samdata
..         append    "******Sample******",samdata
..         append    cr,samdata
..          filepi    1;nreffile           .dlh 07MAY93
..          READ      NREFFILE,SAMPLE;SAMPLE,DESC38;
..         GOTO      DSEX IF OVER
..         append    desc38,samdata
..         append    cr,samdata
..DSEX     reset      samdata
..         append     cr,sexdata
..         append     "Sex:",sexdata
..         append     sex,sexdata
..         append     cr,sexdata
..         reset      sexdata
..         append     "**Return Tape: ",rtndata
..         append     cr,rtndata
..         append     rline1,rtndata
..         append     cr,rtndata
..         append     rline2,rtndata
..         append     cr,rtndata
..         append     rline3,rtndata
..         append     cr,rtndata
..         append     rline4,rtndata
..         append     cr,rtndata
..         append     rline5,rtndata
..         reset      rtndata
.........................................................
..ADDRESSING
.         clear     addrdata
.         pack      NADDFLD1,"01X",LSTNUM
.         move      "NADDAIM",Location
.         pack      KeyLocation,"Key: ",NADDFLD1
.         call      NADDAIM
.         if not over
.                   append    cr,addrdata
.                   append    "******Addressing******",addrdata
.                   append    cr,addrdata
.                   loop
.                             until over
.                             call      DataLoadRefAddressing
.                             move      "NADDKG",Location
.                             pack      KeyLocation,"Key: ",NADDFLD1
.                             call      NADDKG
.                   repeat
.                   reset     addrdata
.         endif
..SOURCE
.         clear     srcedata
.         pack      NSRCFLD1,"01X",LSTNUM
.         move      "NSRCAIM",Location
.         pack      KeyLocation,"Key: ",NSRCFLD1
.         call      NSRCAIM
.         if not over
.                   append    cr,srcedata
.                   append    "********Source********",srcedata
.                   append    cr,srcedata
.                   loop
.                             until over
.                             call      DataLoadRefSource
.                             move      "NSRCKG",Location
.                             pack      KeyLocation,"Key: ",NSRCFLD1
.                             call      NSRCKG
.                   repeat
.                   reset     srcedata
.         endif
..ARRANGEMENT
.         clear     arrdata
.         pack      NARRFLD1,"01X",LSTNUM
.         move      "NARRAIM",Location
.         pack      KeyLocation,"Key: ",NARRFLD1
.         call      NARRAIM
.         if not over
.                   append    cr,arrdata
.                   append    "*****Arrangement*****",arrdata
.                   append    cr,arrdata
.                   loop
.                             until over
.                             call      DataLoadRefArrangement
.                             move      "NARRKG",Location
.                             pack      KeyLocation,"Key: ",NARRFLD1
.                             call      NARRKG
.                   repeat
.                   reset     arrdata
.         endif
..SELECT
.         clear     seldata
.         pack      NSLTFLD1,"01X",LSTNUM
.         move      "NSLTAIM",Location
.         pack      KeyLocation,"Key: ",NSLTFLD1
.         call      NSLTAIM
.         if not over
.                   append    cr,seldata
.                   append    "******Selections******",seldata
.                   append    cr,seldata
.                   loop
.                             until over
.                             call      DataLoadRefSelection
.                             move      "NSLTKG",Location
.                             pack      KeyLocation,"Key: ",NSLTFLD1
.                             call      NSLTKG
.                   repeat
.                   reset     seldata
.         endif
..CLEANED
.         clear     clndata
.         if (CLEANCDE = "C002")
.                   append    cr,clndata
.                   append    " ******Cleaned******",clndata
.                   append    cr,clndata
.                   append    CLNINFO,clndata
.                   reset     clndata
.         else
.                   pack      NREFFLD,CLEANCDE
.                   move      "D.Load6-NREFKEY",Location
.                   pack      KeyLocation,"Key: ",NREFFLD
.                   call      NREFKEY
.                   if not over
.                             append    cr,clndata
.                             append    " ******Cleaned******",clndata
.                             append    cr,clndata
.                             call      Trim using NREFDESC
.                             append    NREFDESC,clndata
.                             append    cr,clndata
.                             reset     clndata
.                   endif
.         endif
..NET NAME
.         reset     netdata
.         if (NETNAME = "N002")
.                   append    cr,netdata
.                   append    "*Net arrangement*",netdata
.                   append    cr,netdata
.                   append    netinfo,netdata
.                   reset     netdata
.         else
.                   pack      NREFFLD,NETNAME
.                   move      "D.Load7-NREFKEY",Location
.                   pack      KeyLocation,"Key: ",NREFFLD
.                   call      NREFKEY
.                   if not over
.                             append    cr,netdata
.                             append    "*Net arrangement*",netdata
.                             append    cr,netdata
.                             call      Trim using NREFDESC
.                             append    NREFDESC,netdata
.                             append    cr,netdata
.                             reset     netdata
.                   endif
.         endif
..DELIVERY
.         clear     deldata
.         pack      NREFFLD,DELCODE
.         move      "D.Load8-NREFKEY",Location
.         pack      KeyLocation,"Key: ",NREFFLD
.         call      NREFKEY
.         if not over
.                   append    cr,deldata
.                   append    "*****Delivery*****",deldata
.                   append    cr,deldata
.                   call      Trim using NREFDESC
.                   append    NREFDESC,deldata
.                   append    cr,deldata
.                   reset     deldata
.         endif
..SAMPLE
.         clear     samdata
.         pack      NREFFLD,SAMPLE
.         move      "D.Load9-NREFKEY",Location
.         pack      KeyLocation,"Key: ",NREFFLD
.         call      NREFKEY
.         if not over
.                   append    cr,samdata
.                   append    "******Sample******",samdata
.                   append    cr,samdata
.                   call      Trim using NREFDESC
.                   append    NREFDESC,samdata
.                   append    cr,samdata
.                   reset     samdata
.         endif
..GENDER
.         clear     sexdata
.         append    cr,sexdata
.         append    "Sex:",sexdata
.         append    sex,sexdata
.         append    cr,sexdata
.         reset     sexdata
..END PATCH 1.73 ADDED LOGIC
.         move      c0 to n3                 .did
.         move      portn to n3         .someone get in
.         compare   c0 to n3            .not via master ?
.         if        equal               .yes
.        clock      port to str3           .plb note
..bytes 1-2 are 0-99
..byte  3   is the hundreds field
.        unpack     str3 into str2,str1
.        pack       str3 from str1,str2
.        move       str3 to portN
.         endif
..START PATCH 1.73 REPLACED LOGIC
..         MOVE      OWNNUM TO NOWNFLD
.         unpack    OWNNUM,str2,str4
.         MOVE      str4 TO NOWNFLD
..END PATCH 1.73 REPLACED LOGIC
.OWNREAD  CALL      NOWNKEY
.         MOVE      TELEMASK TO TELEDISP
.         EDIT      OWNTELE TO TELEDISP
.         CLEAR     STR7
.         CLEAR     STR3
.         CLEAR     STR4
.        clock      port to str3           .plb note
..bytes 1-2 are 0-99
..byte  3   is the hundreds field
.         unpack    ownfax into str3,str7
.         CLEAR     STR3
.         unpack    str7 into str3,str4
.         clear     pg1data
.         append    "LO## " to pg1data
..START PATCH 1.73 REPLACED LOGIC
..         append    ownnum to pg1data
.         unpack    OWNNUM,str2,str4
.         append    str4 to pg1data
..END PATCH 1.73 REPLACED LOGIC
.         append    cr,pg1data
.         append    ownlonm,pg1data
.         append    cr,pg1data
.         append    ownocpy,pg1data
.         append    cr,pg1data
.         append    ownlosa,pg1data
.         append    cr,pg1data
.         append    ownlocty,pg1data
.         append    ",",pg1data
.         append    b1,pg1data
.         append    ownlos,pg1data
.         append    b1,pg1data
.         append    ownlozc,pg1data
..START PATCH 1.5 REPLACED LOGIC
..         cmatch    b1 to ownctn
..         if        not equal
..         append    "CCTO: ",pg1data
..         append    ownctn to pg1data
..         endif
.         call      Trim using OWNCTN
.         if (OWNCTN <> "")
.                   pack      NFULFLD,OWNCTN
.                   rep       zfill,NFULFLD
.                   move      C1,NFULPATH
.                   move      "OWNREAD-NFULKEY",Location
.                   pack      KeyLocation,NFULFLD
.                   call      NFULKEY
.         else
.                   clear     NFULFLD
.                   clear     NFULCOMP
.         endif
.         if (NFULCOMP <> "")
.                   append    "CCTO: ",pg1data
.                   append    NFULCOMP,pg1data
.         endif
..END PATCH 1.5 REPLACED LOGIC
.         append    cr,pg1data
.         append    teledisp,pg1data
.         append    " Fax:",pg1data
.         append    str3,pg1data
.         append    dash,pg1data
.         append    str4,pg1data
.         append    cr,pg1data
.         append    sexdata,pg1data
.         append     " Min: ",pg1data
.         append     Min,pg1data
.         append    seldata,pg1data
.         append    cr,pg1data
.         append    rtndata,pg1data
.         append    netdata,pg1data
.         append    arrdata to pg1data
.         append    magdata,pg1data
.         append    srcedata,pg1data
.         append    clndata,pg1data
.         append     cr,pg1data
.         append     "Universe: ",pg1data
.         append     universe,pg1data
.         append    cr,pg1data
.         append     deldata to pg1data
.         append     samdata to pg1data
.         append    cr,pg1data
.              append      "Put Up: ",pg1data
..START PATCH 1.73 REPLACED LOGIC
..              move          "99/99/9999" to str10
..              edit          NewDate to str10
.                   unpack    NEWDATE,CC,YY,MM,DD
.                   pack      str10,MM,SLASH,DD,SLASH,CC,YY
..END PATCH 1.73 REPLACED LOGIC
.              append        Str10,pg1data
.         reset     pg1data
.         Setitem NDat0001AEditText003,0,pg1data
.         setprop Ndat0001AEditText003,Font=Font1,FGCOlor=white,BGColor=blue
..end patch 1.4
.END PATCH 1.9 REMOVED LOGIC
 if (CheckFlg = "2")
          goto MainVerify
 endif
 setitem  NrcoStatLstNm,0,OLSTNAME
.START PATCH 1.73 REPLACED LOGIC
. setitem        NrcoStatRevDate,0,revdate
          unpack    REVDATE,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
          setitem        NrcoStatRevDate,0,str10
.END PATCH 1.73 REPLACED LOGIC
 enableitem         NrcoFax
.START PATCH 1.73 REPLACED LOGIC
. move              ownnum to nownfld
          unpack    OWNNUM,str2,str4
          move      str4 to nownfld
.END PATCH 1.73 REPLACED LOGIC
 call               zfillit using nownfld,C0
 call               nownkey


OWNERPOP
  setitem NrcoEditOwn,0,OWNLON
  setitem NrcoEditCnt,0,OWNLONM
  setitem NrcoStatTextOComp,0,OWNOCPY
  setitem NrcoEditFax,0,OWNFAX
  enableitem      NrcoEditFax
  enableitem      NrcoEditOwn
  call Reform
.START PATCH 1.9 ADDED LOGIC
.Screen 2
          setitem   Data2StatListNum,0,LSTNUM
.Test Logic
          call      CreateWebCard using LSTNUM
          pack      taskname,"c:\work\data",LSTNUM,".HTM"
          pack      APIFileName,str55,str55,str55,str55,str55
          pack      APIFileName,taskname,hexzero
          call      FindFirstFile
          if (APIResult <> 0 & APIResult <> hexeight)
                    Data2WebBrowser.Navigate2 USING taskname
          else
                    Data2WebBrowser.Navigate2 USING "about:blank"
          endif
.END PATCH 1.9 ADDED LOGIC
  return

OWNERCLEAR
          setitem NrcoEditOwn,0,""
          setitem NrcoEditCnt,0,""
          setitem NrcoStatTextOComp,0,""
          setitem NrcoEditFax,0,""
          return

OWNCHECK
          clear NOWNFLD
          getitem NrcoEditOwn,0,NOWNFLD
          if (NOWNFLD="")
          CALL OWNERCLEAR
          return
          endif
          call zfillit using NOWNFLD,C0
          call NOWNKEY

          IF OVER
          alert caution," Enter a Valid Owner #!", result
                return
          else
                call ownerclear
                call ownerpop
          endif
          return


MLR
          getitem NrcoMlrNo,0,str4
          if (str4="")
          setitem NrcoEditMlrNme,0,""

                    return
          endif
          call zfillit using str4,C0
          pack mkey from str4,Z3
          call NMLRKey

          if over        alert caution," This is an invalid Mailer #.  Please try again.", result
                setitem NrcoEditMlrNme,0,""
                return
          else
                setitem NrcoMlrNo,0,str4
                setitem NrcoEditMlrNme,0,MCOMP
          endif
          match str4 to HOLDMLR
          If NOT EQUAL
                MOVE STR4 TO HOLDMLR
                    CALL CountClear
                setitem NrcoEditRegion,0,""
                    return
          endif
          return
AddSCF

          getitem NrcoDataSCF,c1,n2
          getitem NrcoEditRegion,0,str10
          if (str10="")
          alert caution,"Item added to list cannot be blank. Please try again",result
                setfocus NrcoEditRegion
          return
        endif

          if (n2="15")
          alert caution,"You have reached the maximum 15 items in this list.",result
          return
          else
          insertitem NrcoDataSCF,9999,str10
          setfocus NrcoEditRegion
          return
          endif
          return

DelSCF

          getitem NrcoDataSCF,0,n2
          if (n2=c0)
                    alert type=yesno1," Do you want to delete all of the entries in the datalist?", result
                              if (result=6)    . 6 = yes , 7 = no
                              deleteitem NrcoDataSCF,n2
                              return
                        else
                              alert note,"Select a field in the SCF\State Field, then Remove.",result
                                return
                              endif
          endif

          deleteitem NrcoDataSCF,n2
          return

MainVerify
. call listcheck
.OwnVerify
          getitem NrcoEditOwn,0,NOWNFLD
          if (NOWNFLD="")
          alert caution," Enter a Valid Owner #!", result
          setfocus NrcoEditOwn
          return
          endif
          call zfillit using NOWNFLD,C0
          call NOWNKEY

          IF OVER
          alert caution," Enter a Valid Owner #!", result
                setfocus NrcoEditOwn
                return
          endif
.Fax Verify
. seven or ten digit fax number

          getitem NrcoEditFax,0,str14
          call trim using str14
          call    RemoveChar using str14,dash
          call    RemoveChar using str14,LPAREN
          call    RemoveChar using str14,RPAREN
          call    REMOVECHAR using str14,b1

.begin patch 2.41
          if (str14 = "" or str14 = "0000000000")
                    alert type=yesno1,"Invalid Fax Number - Continue?",result
                    if (result <> C6)     . 6 = yes , 7 = no
                    setfocus NrcoEditFax
                    return
                    endif
          endif
..begin patch 2.3
.          if (str14 = "0000000000")
.          alert caution, "Fax Number must be a valid ten digit field.",result
.          setfocus NrcoEditFax
.          return
.          endif
..end patch 2.3
.end patch 2.41

  count n2,str14
  if            (n2 = c7)  .local
                move str14 to ownfax
                clear longdist
          elseif        (n2=c10) .if ten digit is num in local area code
                    move str14 to ownfax
                    move c1 to longdist
                    unpack ownfax into str3,str7
                    match "510" to str3
                    if equal
                              move str7 to ownfax
                              clear longdist
                    endif

          else
                              alert caution,"Fax Number must be a valid ten digit field.",result
                    setfocus NrcoEditFax
                    return
          endif

.Mlr Name Verify
          getitem NrcoEditMlrNme,0,MCOMP
          if (MCOMP="")
                              alert caution,"Mailer Name Required.",result
                    setfocus NrcoEditMlrNme
                    return
          endif

GetData
.own contact
          getitem NrcoEditCnt,0,OWNLONM
.own fax
.         getitem NrcoEditFax,0,OWNFAX
.own company
          getitem NrcoStatTextOComp,0,OWNOCPY
.Mailer Name
          getitem NrcoEditMlrNme,0,MCOMP
.ListName
          getitem NrcoStatLstNm,0,OLSTNAME
.Select
          getitem NrcoEditSelect,0,NRCOSLCT
.Comments
          getitem NrcoEditComm,0,NRCOCMNT
..................................................................
.Datalist
.uses a call style branch to place datalist items in variables and loop back through
          clear n2
          GETITEM NrcoDataSCF,c1,n2

        for     N9, "1" to N2
                GETITEM NrcoDataSCF,n9,str10
                store   str10 in N9 of NRCOG01,NRCOG02,NRCOG03,NRCOG04,NRCOG05,NRCOG06,NRCOG07:
                NRCOG08,NRCOG09,NRCOG10,NRCOG11,NRCOG12,NRCOG13,NRCOG14,NRCOG15

        repeat
..................................................................
. SECOND REQUEST
          getitem NrcoCheckRequest,0,N1
          if (N1 = C1)
                    move      YES,SecondReq
          else
                    move      NO,SecondReq
          endif
. NEED BY DATE
          getitem NrcoEditDate,0,str10
.Patch 1.3
.Omit Check Box
          getitem NrcoCheckOmit,0,N1
          if (N1 = C1)
                    move      YES,OMIT
          else
                    move      NO,OMIT
          endif
.Patch 1.3
.Patch1.6
                    clear n1
          getitem Nrco0001CheckDataCard,0,N1
.Patch1.7
          clear DCARD
.SubPatch1.7
          if (N1 = C1)
.Patch1.7
.                   move      YES,DCARD
                              clear n1
                    getitem NrcoCheckUsage,0,N1
                                        if (n1 = c1)
                              move "B" to DCARD   ;Both
                    else
                              move "D" to DCARD   ;DataCard Only
                    endif
.SubPatch1.7
          else
.Patch1.7
.                   move      NO,DCARD
                              clear n1
                    getitem NrcoCheckUsage,0,N1
                                        if (n1 = c1)
                              move "U" to DCARD   ;Usage Only
                    else
                              move "N" to DCARD   ;None
                    endif
.EndPatch1.7
          endif
.subpatch1.6
.Check for ASAP then clean up
          call    RemoveChar using str10,SLASH
          call    RemoveChar using str10,dash
          call    Trim using str10
.        type       str10
.         if not equal
.                   call    RemoveChar using str10,period
.        endif
          move      str10 to str8
          call      write
          return

WRITE
          call OrderSetMouseBusy
..Find out system information
        clock   timestamp,timestamp
        pack    str55,"\\nins1\d\users\",userlogn,"\",timestamp,".dat"
        prepare tempfile,str55
..        write   tempfile,seq;SecondReq,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,nuseuser,str8,MCOMP,OLSTNAME:
..                nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
..                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
..Patch1.3
.       write   tempfile,seq;SecondReq,Omit,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,nuseuser,str8,MCOMP,OLSTNAME:
.                nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
..EndPatch1.3
..Patch1.3
.      write   tempfile,seq;SecondReq,Omit,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,nuseuser,str8,MCOMP,OLSTNAME:
.               nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.               nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
..EndPatch1.3
..Patch1.71
.      write   tempfile,seq;SecondReq,Omit,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
.               nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.               nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
..EndPatch1.71
.begin patch 2.4
..Patch1.72
.      write   tempfile,seq;SecondReq,Omit,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
.               ndatfld,nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.               nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
.EndPatch1.72
      write   tempfile,seq;SecondReq,Omit,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
               ndatfld,nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
               nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15,OwnEmail
          weof      tempfile,seq
          flush     tempfile
.end patch 2.4

        close   tempfile
        clear   taskname
          call Get64OS      
............................................................
.NOTE --- this should be changed to a call to nrco0002
............................................................
...        append  "c:\progra~1\lanbatch\batch -X -SA -Q\\nts0\c\lanbat~2 \\nts0\c\apps\winbatch\butil job=COUNTREQ ",TASKNAME
..        append  "!f:\apps\winbatch\butil job=COUNTREQ ",TASKNAME
.        append  "!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 \\nts0\c\apps\winbatch\butil job=COUNTREQ ",TASKNAME
.          IF  (Bit64Flag = No)    .if not 64 bit we can submit to batch else run locally
.        append  "!\\nins1\lanbatch\batch32 -X -SC -Q\\nins1\ServerC \\nins1\winbatch\butil job=COUNTREQ ",TASKNAME
.          else
        append  "!\\nins1\winbatch\butil job=COUNTREQ ",TASKNAME
.          endif
.        append  "!\\nts0\c\apps\winbatch\butil job=COUNTREQ ",TASKNAME
        append  " infile=",taskname
        append  timestamp,taskname
.begin patch 2.1
          Append    " co=",taskname
          append    Company,taskname
.end patch 2.1
        append  " P=",taskname
          
..Get default printer
        move    PORTN,NCNTFLD1
        rep     zfill,NCNTFLD1
        move    C3,NCNTPATH
        move    "NRCO0001-NCNTKEY",Location
        pack    KeyLocation,"Key: ",NCNTFLD1
        call    NCNTKEY
        if over
                move    C2,CNTPRINT    .Laser 3
        endif
        move        CNTPRINT,str1
        rep         zfill,str1
        append  str1,taskname
        APPEND  " B=",TASKNAME
        APPEND  userlogn TO TASKNAME
        RESET   TASKNAME
        execute taskname
        call OrderSetMouseFree
        call scfclear
        alert note,"Done!",result,"Printing"
        setprop NrcoFax,Default=0
        setfocus NrcoEditList
        return


CountClear
          setitem NrcoEditSelect,0,""
          setitem NrcoEditComm,0,""
          deleteitem NrcoDataSCF,c0
.Select
          getitem NrcoEditSelect,0,NRCOSLCT
.Comments
          getitem NrcoEditComm,0,NRCOCMNT
          return
FormClear
.May be used for clear button ask JD or AH
          call ownerclear
          setitem NrcoMlrNo,0,""
          setitem NrcoEditList,0,""
          setitem NrcoStatRevDate,0,""
          setitem NrcoStatLstNm,0,""
          setitem NrcoEditMlrNme,0,""
          setitem NrcoEditDate,0,""
          setitem NrcoCheckRequest,0,0
          call countclear
          return
SCFClear
          clear NRCOG01
          clear NRCOG02
          clear NRCOG03
          clear NRCOG04
          clear NRCOG05
          clear NRCOG06
          clear NRCOG07
          clear NRCOG08
          clear NRCOG09
          clear NRCOG10
          clear NRCOG11
          clear NRCOG12
          clear NRCOG13
          clear NRCOG14
          clear NRCOG15
          return

userng
          clear     taskname
          append    "I'm sorry I've lost track of who you are,",taskname
          append    NewLine,taskname
          append    "Please leave the program and try again!",taskname
          reset     taskname
          alert     caution,taskname,result
          stop
          
Reform
          getitem NrcoEditFax,0,str14
        call    Trim using str14
        call    RemoveChar using str14,dash
        call    RemoveChar using str14,LPAREN
        call    RemoveChar using str14,RPAREN
        call    REMOVECHAR using str14,b1
        if (str14 = "")
                clear   str14
        endif
        count n2,str14
        if (n2=c10)
                unpack str14,str3,prefix,str4
                pack    fon,"(",str3,") ",prefix,"-",str4
                setitem NrcoEditFax,0,fon
                return
        endif
        if (n2=c7)
                unpack str14,str3,str4
                pack    fon,"(",area,") ",str3,"-",str4
                setitem NrcoEditFax,0,fon
                return
.        else
.                alert caution,"Fax Number must be a ten digit field.",result
.               setfocus NrcoEditFax
        endif

        return

RefDate
          getitem NrcoEditDate,0,str10
        type        str10
          if equal
.Reformatting date
        call    Trim using str10
        call    RemoveChar using str10,SLASH
        call    RemoveChar using str10,dash
            if (str10 = "")
                clear   str10
        else
                unpack  str10,dd,mm,str2,yy
                pack    str10,dd,"/",mm,"/",str2,yy
        endif
        setitem NrcoEditDate,0,str10
        return

        else

        return

          endif





.................................................................................
FileGo
.Flag set to "N" if in Modify or New mode
.        branch result to FileGo1,FileGo2,FileGo3,FileGo3
        branch result to FileGo1
FileGo1
        call click_nrcoExit
        RETURN

.FileGo2
.        RETURN

.FileGo3
.        if (ExitFlag = "Y")
.                winshow
.                stop
.        endif
.        return
Optionsgo
        return
.begin patch 1.4
.ViewGo
...        setprop Options,visible=1
.        return
.end patch 1.4
EditGo
        return
HelpGo
        setprop AboutMssg,visible=1
        return

SearchGo
        branch  result to SearchGo1,SearchGo2,SearchGo3,SearchGo4
SearchGo1
.BROKER
        move    C1,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo2
.LIST
        move    C2,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo3
.MAILER
        move    C3,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchGo4
.SHIP-TO
        move    C4,SrchFlag
        call    SearchSetTitle
        call    SearchSetVisible
        return
SearchLoad
.Called by SearchDataList_DoubleClick
.Only load if not in Inquiry mode
.        getprop SamplesEditMailer,enabled=N9
.        if (N9 <> C1)
.                return
.        endif
        branch SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4
.        return
SearchLoad1
.BROKER - not an option with this program
        return
SearchLoad2
.LIST
        unpack srchstr,str6,str1,str35,str1,str10
        setitem NrcoEditList,0,str6
        setitem NrcoStatLstNm,0,str35
        setitem NrcoStatRevDate,0,str10
        setfocus NrcoEditList
        return
SearchLoad3
.MAILER
        unpack  Srchstr,str4,str1,str3,str1,str45
        setitem NrcoMlrNo,0,str4
        setitem NrcoEditMlrNme,0,str45
        setfocus NrcoMlrNo
        return
SearchLoad4
.SHIP-TO - not an option with this program
        return
OrderSetMouseBusy
        setmode *mcursor=*wait
        return
OrderSetMouseFree
        setmode *mcursor=*arrow
        return
................................................................
.START PATCH 1.9 REMOVED LOGIC
..begin patch 1.4
.ViewGO
.        if     (result = 1)
.                   setprop        Ndat0001AEditText001,visible=0
.                   setprop        Ndat0001AEditText002,visible=0
.                   setprop        Ndat0001AEditText003,visible=0
.                   setprop        Ndat0001AEditText004,visible=0
.                   setprop        Ndat0001AEditText005,visible=0
.               SetProp        NRcoColl1,visible=1
.        elseif              (result = 2)
.               SetProp        NRcoColl1,visible=0
.                   setprop        Ndat0001AEditText001,visible=1
.                   setprop        Ndat0001AEditText002,visible=1
.                   setprop        Ndat0001AEditText003,visible=1
.                   setprop        Ndat0001AEditText004,visible=1
.                   setprop        Ndat0001AEditText005,visible=1
.         endif
..
.         RETURN
..START PATCH 1.73 ADDED CODE
.DataLoadRefAddressing
.         pack      NREFFLD,"A",NADDNUM
.         move      "D.Load1-NREFKEY",Location
.         pack      KeyLocation,"Key: ",NREFFLD
.         call      NREFKEY
.         call      Trim using NREFDESC
.         append    NREFDESC,addrdata
.         append    cr,addrdata
.         return
.
.DataLoadRefSelection
.         pack      NREFFLD,"L",NSLTNUM
.         move      "D.Load2-NREFKEY",Location
.         pack      KeyLocation,"Key: ",NREFFLD
.         call      NREFKEY
.         call      Trim using NREFDESC
.         append    NREFDESC,seldata
.         append    at,seldata
.         append    NSLTPRICE,seldata
.         append    cr,seldata
.         return
.
.DataLoadRefArrangement
.         pack      NREFFLD,"R",NARRNUM
.         move      "D.Load4-NREFKEY",Location
.         pack      KeyLocation,"Key: ",NREFFLD
.         call      NREFKEY
.         call      Trim using NREFDESC
.         append    NREFDESC,arrdata
.         append    cr,arrdata
.         return
.
.DataLoadRefSource
.         pack      NREFFLD,"S",NSRCNUM
.         move      "D.Load5-NREFKEY",Location
.         pack      KeyLocation,"Key: ",NREFFLD
.         call      NREFKEY
.         call      Trim using NREFDESC
.         append    NREFDESC,srcedata
.         append    dash,srcedata
.         append    NSRCPER,srcedata
.         append    cr,srcedata
.         return
..END PATCH 1.73 ADDED CODE
.END PATCH 1.9 REMOVED LOGIC
................................................................
.START PATCH 1.9 ADDED LOGIC
CountTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
        if (N2 = C1)
                Deactivate NRCO001A
        elseif (N2 = C2)
                Deactivate NDAT001b
        endif
        return

CountTabChange
        move    N2,TabNum
.
        if (N2 = C1)
                Activate NRCO001A
                    getprop   NRCO0001,width=result
                    sub       "95",result
                    setprop   NRCO0001,width=result
                    getprop   NrcoTabControl,width=result
                    sub       "95",result
                    setprop   NrcoTabControl,width=result
        elseif (N2 = C2)
                Activate NDAT001b
                    Data2WebBrowser.Refresh
.Strange work-around - zorder for this object is somehow lost when DEACTIVATE/ACTIVATE is used on Child Form!!!
                    getprop   Data2WebBrowser,zorder=result
                    setprop   Data2WebBrowser,zorder=result
                    getprop   NRCO0001,width=result
                    add       "95",result
                    setprop   NRCO0001,width=result
                    getprop   NrcoTabControl,width=result
                    add       "95",result
                    setprop   NrcoTabControl,width=result
        endif
        return
XRESIZE
           NRCO0001.Scale
           RETURN
.END PATCH 1.9 ADDED LOGIC
.begin release init 1.4
         INCLUDE   NREFio.INC
         include   nmdlio.inc
.end release init 1.4
.Patch1.8
.Patch1.8
                              include   compio.inc
                              include   cntio.inc
.        INCLUDE   NMLRIO.INC
.Patch1.8
         INCLUDE   NOWNIO.INC
         INCLUDE   NDATIO.INC
         INCLUDE   NUSEIO.INC
.ADDED FOR SEARCH.PLF............................................
.Patch1.8
.        include nbrkio.inc
.Patch1.8
        include nrtnio.inc
        include searchio.inc      .contains logic for search.plf
        include ncmpio.inc
        include ncntio.inc
.START PATCH 2.0 REMOVED LOGIC
..START PATCH 1.5 ADDED LOGIC
.         INCLUDE   NFULIO.INC
..END PATCH 1.5 ADDED LOGIC
.END PATCH 2.0 REMOVED LOGIC
.START PATCH 1.73 ADDED LOGIC
          INCLUDE   NADDIO.INC
          INCLUDE   NARRIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NSRCIO.INC
          INCLUDE   NTXTIO.INC
.END PATCH 1.73 ADDED LOGIC
        INCLUDE COMLOGIC.inc
