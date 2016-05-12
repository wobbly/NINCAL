PC       EQU       0
         INC       COMMON.inc
         include   cons.inc
         INCLUDE   HP.INC
         include   nowndd.inc
                              include   compdd.inc
                              include   cntdd.inc
         INCLUDE   NUSEDD.INC
         INCLUDE   NDATDD.INC
.ADDES FOR SEARCH PLF........................
         include nrtndd.inc
         include ncmpdd.inc
         include norddd.inc
         include winapi.inc
         include ncntdd.inc
         INCLUDE   NREFDD.INC
         include   nmdldd.inc
          INCLUDE   NADDDD.INC
          INCLUDE   NARRDD.INC
          INCLUDE   NSLTDD.INC
          INCLUDE   NSRCDD.INC
          INCLUDE   NTXTDD.INC
Release   Init      "2.50"     DLH merge with Nrco0002 see archives for previous changes
Reldate   Init      "23 August 2012"
.RELEASE  INIT       "1.0"     DMB 04/25/01 New GUI Request for Counts
........................................................................................
.Files to open
.tempfile file
CreateWebCard external "NDAT002W;CreateWebCard"
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
TabNum    form   "01"
hold2     dim       4500      .length of largest possible text record
..............................................................

.Menu
.Set Up Menu Bar
mFile    menu
mEdit    menu
mOptions menu
mHelp    menu
.Set Up SubMenu for Options
sSearch submenu

.Present Data for Menu Bar
.FData   init    "&File;&Print;Pre&view;-;E&xit"
FData   init    "&File;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Search;-;&Preferences"
HData   init    "&Help;&About"

.Present Data for SubMenu
SData   init    ";&Broker;&List;&Mailer;&Ship-To"
.begin patch 2.50
RESULT2 FORM    9
RESULT3 FORM    9
.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font
font6   font
prfile  pfile
Emailflag  dim     1            =Y Email/fax the pdf, N = print
faxnum     dim     10
FileCheck  FIle
trapcount  form      4
PICT1   PICT
        CREATE      PICT1=3:13:30:50:
                "\\nins1\e\apps\PLB\CODE\2NDREQUEST7.BMP"
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=7
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
FontCourier                   font
FontO7              font
FontO18B  font

          create    fontCourier,"Courier New",size=10
          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
.end patch 2.50


..................................................................................

         MOVE      C1 TO NMLRPATH
         MOVE      C1 TO NOWNPATH
         MOVE      C1 TO NDATPATH
         move      c1 to nusepath
         CLOCK     DATE TO TODAY
.begin patch 2.50
         call     GetWinVer
.end patch 2.50
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
          move      Reldate,Wreldate
srch    plform  Search
abt     plform  About
NRCO001A plform     NRCO001A
NDAT001B plform     NDAT001B
x       plform  Nrco0001

     winhide

  formload x
          formload NRCO001A,NRCO0001
          formload NDAT001B,NRCO0001
  formload srch
  formload abt
  create  NRCO0001;mFile,FData
  create  NRCO0001;mEdit,EData,mFile
  create  NRCO0001;mOptions,OData,mEdit
  create  NRCO0001;mHelp,HData,mOptions

.Create SubMenu
  create  NRCO0001;sSearch,SData,mOptions,1
.  create  DevNRCO0001;sSearch,SData,mOptions,1
.Activate Menus
.FileGo leads to stop
  activate mFile,FileGo,result
.Need this when it works
  activate mEdit,EditGo,result
  activate mOptions,OptionsGo,result
.Only a SubMenu under this one
  activate mHelp,HelpGo,result
.Activate SubMenus
  activate sSearch,SearchGo,result
          Data2WebBrowser.Navigate2 USING "about:blank"
          setprop   Data2StatList,top=30
          setprop   Data2StatListNum,top=30
.         setprop   Data2WebBrowser,top=50,width=620
          setprop   Data2WebBrowser,top=50,width=715,height=380
  move c1,checkflg
  if (ncntname = "")
          alert caution,"Can't Find your name in Contact File - Call I.S.!",result,"Call I.S."
                    stop
  else
            setitem NrcoStatUser,0,CNTNAME
  endif
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
  cmatch "C" to elstcde
  if equal
          alert caution," NIN exclusive lists are not allowed!", result
          setfocus NrcoEditList
          return
 endif

 reset Whitney
. scan ownnum in Whitney
          unpack    OWNNUM,str2,str4
          scan      str4,Whitney
 if equal
        alert caution," Whitney lists are not allowed!", result
        setfocus NrcoEditList
        return
 endif

 if (CheckFlg = "2")
          goto MainVerify
 endif
 setitem  NrcoStatLstNm,0,OLSTNAME
          unpack    REVDATE,CC,YY,MM,DD
          pack      str10,MM,SLASH,DD,SLASH,CC,YY
          setitem        NrcoStatRevDate,0,str10
 enableitem         NrcoFax
          unpack    OWNNUM,str2,str4
          move      str4 to nownfld
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
.Screen 2
          setitem   Data2StatListNum,0,LSTNUM

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

          if (str14 = "" or str14 = "0000000000")
                    alert type=yesno1,"Invalid Fax Number - Continue?",result
                    if (result <> C6)     . 6 = yes , 7 = no
                    setfocus NrcoEditFax
                    return
                    endif
          endif

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
.Omit Check Box
          getitem NrcoCheckOmit,0,N1
          if (N1 = C1)
                    move      YES,OMIT
          else
                    move      NO,OMIT
          endif
                    clear n1
          getitem Nrco0001CheckDataCard,0,N1
          clear DCARD
          if (N1 = C1)
                              clear n1
                    getitem NrcoCheckUsage,0,N1
                                        if (n1 = c1)
                              move "B" to DCARD   ;Both
                    else
                              move "D" to DCARD   ;DataCard Only
                    endif
          else
                              clear n1
                    getitem NrcoCheckUsage,0,N1
                                        if (n1 = c1)
                              move "U" to DCARD   ;Usage Only
                    else
                              move "N" to DCARD   ;None
                    endif
          endif
.Check for ASAP then clean up
          call    RemoveChar using str10,SLASH
          call    RemoveChar using str10,dash
          call    Trim using str10
          move      str10 to str8
          call      write
          return

WRITE
          call OrderSetMouseBusy
..Find out system information
        clock   timestamp,timestamp
.begin patch 2.50
.no reason to write we are printing from within this program we have all data
.        pack    str55,"\\nins1\d\users\",userlogn,"\",timestamp,".dat|10.10.103:502"
.        prepare tempfile,str55
.
.      write   tempfile,seq;SecondReq,Omit,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
.               ndatfld,nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.               nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15,OwnEmail
.          weof      tempfile,seq
.          flush     tempfile
.
.        close   tempfile
.        clear   taskname
.end patch 2.50
          call Get64OS      
.begin patch 2.50
CreatePrint
.begin patch 2.50
.        open    tempfile,str55,SHARE
.        read    tempfile,seq;SecondReq,OMIT,DCARD,OWNLONM,today,LONGDIST,OWNFAX,OWNOCPY,cntname,str8,MCOMP,OLSTNAME:
.                listname,nrcoslct,nrcocmnt,nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06,nrcog07:
.                nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15,OwnEmail
.        close   tempfile
.end patch 2.50
        move    "800",column
        move    "1700",column1
        move    "2500",column2
        move    "4200",column4
        move    "5900",column6
          Clear     EmailFlag
                    call      Trim using OwnEmail
                    if        (OwnEmail <> "")               .we have somthing
                              scan      "@",OwnEmail
                              if        equal                         .presuming valid
                              reset     OwnEmail
                              move      OwnEmail,MailTo
                              move      Yes,Emailflag                    .send it
                              endif
                    Else
                    
                    move    Ownfax,faxnum
                    match   "0000000000",faxnum
                              if      equal                                       .no good zeroed out
                              move      No,EmailFlag
                    
                              else
                                        type    faxnum
                                        if not equal
                                        else
                                        count   N2,faxnum
                                        compare C10,N2
                                                  if equal
                                                  move    yes,EmailFlag      .Looks like we have number, FAX IT
                                                  move    C1,LONGDIST
                                                  unpack  faxnum,str3,str7
                                                  match   "510",str3
                                                            if equal
                                                            move    str7,faxnum
                                                            clear   LONGDIST
                                                            else
                                                            match   B3,str3
                                                                      if equal
                                                                            move    str7,faxnum
                                                                            clear   LONGDIST
                                                                      endif
                                                            endif
                                                  pack      Mailto from"IMCEAFACSYS-",longdist,faxnum,"@nincal.com"
                                                  endif
                                        endif
                              endif
                    endif


        call    Trim using LONGDIST
.
        if (Emailflag = No)
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
                if (CNTPRINT = "1" | CNTPRINT = "3")      .Laser2
                        PRTOPEN prfile,"\\NINs2\laser2","CountsReq"
                else                                    .Laser3 = Default
                        PRTOPEN prfile,"\\NINs2\laser3 Blankstock","CountsReq"
                endif
        else
..First check 995 autolaunch settings
                    call      PDF995Auto
                    call      SetPDFFlag
                    PRTOPEN prfile,"PDF995","CountsReq"
        endif

        prtpage prfile;*UNITS=*HIENGLISH,*Font=FontCourier;
.
        if (SecondReq = YES)
                PRTPAGE PRFILE;*PICTRECT=*OFF,*PICT=2655:8575:column:7600:pict1
        endif
.
        move    "300",row
...............................
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     "60",row
          add     eightlpi,row

          prtpage   prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p3000:row,*font=font5,*boldon,"REQUEST FOR COUNTS",*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Name:";
        prtpage prfile;*pcolumn1:row,OWNLONM;
        prtpage prfile;*p5000:row,"Date:";
        prtpage prfile;*pcolumn6:row,today;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"Fax #:";
        unpack  OWNFAX,str2,str1,str3,str4
        prtpage prfile;*pcolumn1:row,"(",str2,str1,") ",str3,"-",str4;
.
        prtpage prfile;*p3500:row,"Co: ";
        prtpage prfile;OWNOCPY;

        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"From:";
        prtpage prfile;*pcolumn1:row,cntname;
        add     eightlpi,row
        add     eightlpi,row
        call    Trim using str8
        if (str8 = "")
                clear   str10
        else
                    type      str8
                    if equal
                  unpack  str8,MM,DD,str2,YY
                          pack    str10,MM,DASH,DD,DASH,str2,YY
                    else
                              move      str8,str10
                    endif
        endif
        prtpage prfile;*pcolumn:row,"PLEASE PROVIDE THE FOLLOWING COUNTS BY :  ",str10;
        move    row,howmany
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"MAILER:";
        call    Trim using MCOMP
        prtpage prfile;*pcolumn1:row,MCOMP;
        add     eightlpi,row
        add     eightlpi,row
.
        prtpage prfile;*pcolumn:row,*font=font5,"LIST";
        pack str12,"(","##",Ndatfld,") "
        prtpage prfile;*font=font2,*ll,str12;
        prtpage prfile;*font=font5,":";
        call    Trim using OLSTNAME
        prtpage prfile;*pcolumn1:row,*font=font5,OLSTNAME;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"SELECT:";
        call    Trim using nrcoslct
        prtpage prfile;*pcolumn1:row,nrcoslct;
        add     eightlpi,row
        add     eightlpi,row
.
        prtpage prfile;*pcolumn:row,"Notes:";
.
        call    Trim using nrcocmnt
        prtpage prfile;*pcolumn1:row,nrcocmnt;
        add     eightlpi,row
        add     eightlpi,row
        If (Dcard <>"N")
          if (DCARD = "B")                     ;Both
                    prtpage prfile;*pcolumn:row,*boldon,"Please fax back updated DATACARD and Continuation USAGE with Counts.",*boldoff;
          elseif (DCARD = "D")                 ;Datacard Only
                    prtpage prfile;*pcolumn1:row,*boldon,"Please fax back updated DATACARD with counts.",*boldoff;
          elseif (DCARD = "U")                 ;Usage Only
                    prtpage prfile;*pcolumn1:row,*boldon,"Please fax back Continuation USAGE with counts.",*boldoff;
          endif
          add     eightlpi,row
          add     eightlpi,row

        Else
          add     eightlpi,row
        Endif

    if (OMIT = YES)
        add     eightlpi,row
        prtpage prfile;*pcolumn1:row,*boldon,"Names Updated Through ____________________________";
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn1:row,*boldon,"Information Provided by __________________________";

    else
.
        move    row,N10
        add     "500",column2,result2
        add     "500",column4,result3
        add     eightlpi,row
        prtpage prfile;*presult2:row,"Geography";
        add     eightlpi,row
        prtpage prfile;*presult3:row,"Quantity";
        add     eightlpi,row
        prtpage prfile;*presult2:row,"SCF/State";
        add     "200",column2,result2
        add     "200",column4,result3
        for N9 from "1" to "15"
                add     "310",row
                load    str10 from N9 of nrcog01,nrcog02,nrcog03,nrcog04,nrcog05,nrcog06:
                        nrcog07,nrcog08,nrcog09,nrcog10,nrcog11,nrcog12,nrcog13,nrcog14,nrcog15
                call    Trim using str10
                prtpage prfile;*presult2:row,str10;
                if (N9 = 2)
                        move    row,N7
                elseif (N9 = 13)
                        move    row,N8
                endif
        repeat
        add     "310",row
        prtpage prfile;*presult2:row,"Total:";
.
        add     "310",row
        prtpage prfile;*presult2:row,"Counts provided by:";
.
        move    N10,row

        if (DCARD <> "N")
                  prtpage prfile;*pensize=10,*RECT=row:9970:column2:column6;
                  prtpage prfile;*pcolumn4:row,*line=column4:9970;
        else
                  prtpage prfile;*pensize=10,*RECT=row:9835:column2:column6;
                  prtpage prfile;*pcolumn4:row,*line=column4:9835;
        endif

        add     "600",N10
        prtpage prfile;*pensize=20,*RECT=row:N10:column2:column6;
        prtpage prfile;*pcolumn4:row,*line=column4:N10,*pensize=10;
.
        for N9 from "1" to "14"
                add     "310",N10
                prtpage prfile;*pcolumn2:N10,*line=column6:N10;
        repeat
        add     "310",N10
        move    C0,N9
        add     "310",N10,N9
        prtpage prfile;*pensize=20,*RECT=N10:N9:column2:column6;
        prtpage prfile;*pcolumn4:N10,*line=column4:N9,*pensize=10;
    endif
        PRTCLOSE prfile
.
          if        (Emailflag = Yes)              .good to go
          call      GetPDfPAth
          pack      str45 from PDFPATH,"\flag.dat"
          pack      APIFileName,STR45,hexzero
          loop
                    call      FindFirstFile
                    until (APIResult = 0 | APIResult = hexeight)
                    pause     "1"
          repeat
          pause     "2"
          erase     str45          
          Pack      MailSubjct from "Counts Request for List - ",Olstname
          
.          Clear     MailSubjct
.          append    "Counts Request for List - ",MailSubjct
.          append    OLSTNAME,MailSubjct
.          reset     MailSubjct
.   Set the text message that is send with the attachments
          Clear     MailBody
          append    Mailto,mailbody
          append    CRLF,mailbody
          append      "CountsReq.pdf",MailBOdy
          append    CRLF,mailbody
          Append    "Please find enclosed counts request for our Mailer:",mailbody
          append    CRLF,mailbody
          append    mcomp,mailbody
          Append    " for your list:",mailbody
          append    CRLF,mailbody
          append    OLSTNAME,mailbody
          append    CRLF,mailbody
          reset     MailBody
.          move      "davidHerrick@nincal.com",mailto
          call      trim using cntname
          call      RemoveChar using cntname,b1
          Pack      MailFrom from cntname,"@nincal.com"
          pack      mailcc from cntname,"@nincal.com"
          Pack      MailAttach from "c:\work\pdf\CountsReq.pdf"
CheckFile
          Move      MailAttach,Str55
          trap      WaitForEnd giving error if IO
          open      FileCheck,"c:\work\pdf\CountsReq.pdf",Exclusive     
          Close     FIleCHeck
          trap      IOMssg GIVING ERROR IF IO
          call      waitin using "10"

          call      waitin using "5"
          call      debug
          call      SendMail
          call      waitin using "10"
          call      PDF995Auto0

          erase     mailattach
          endif

        move    "                                        ",APIFileName
          goto      DonePrinting

WaitForEnd
                    TrapClr   IO
.check the error if file does not exist just get out
                    add       C1,Trapcount
                    call      Waitin using "5"
.                    pause     c5
                    noreturn
.                   if        (trapcount > 240)   . 20 min are you kidding me
                   if        (trapcount > 60)   . 5 min are you kidding me
.                    if        (trapcount > 36)   . 3 min are you kidding me
                    Pack       MailSubjct,"Counts Request - ",str25
                    Move      "CReques@nincal.com",MailTO
                    append    CRLF,MailBOdy
                    append    "c:\work\pdf\CountsReq.pdf",MailBody
                    append    CRLF,MailBOdy
                    append    str55,MailBody
                    append    CRLF,MailBOdy
                    append    "I am sorry I could not send the file",Mailbody
                    reset     Mailbody
                    Move      B1,Mailattach
                    call      SendMail
                    return
                    
                    endif
          
                    goto      checkfile
          
DonePrinting
.end patch 2.50
        call OrderSetMouseFree
        call scfclear
        alert note,"Done!",result,"Creating"
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
          shutdown

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
        branch result to FileGo1
FileGo1
        call click_nrcoExit
        RETURN

Optionsgo
        return
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

         INCLUDE   NREFio.INC
         include   nmdlio.inc
                              include   compio.inc
                              include   cntio.inc
         INCLUDE   NOWNIO.INC
         INCLUDE   NDATIO.INC
         INCLUDE   NUSEIO.INC
        include nrtnio.inc
        include searchio.inc      .contains logic for search.plf
        include ncmpio.inc
        include ncntio.inc
          INCLUDE   NADDIO.INC
          INCLUDE   NARRIO.INC
          INCLUDE   NSLTIO.INC
          INCLUDE   NSRCIO.INC
          INCLUDE   NTXTIO.INC
        INCLUDE COMLOGIC.inc
