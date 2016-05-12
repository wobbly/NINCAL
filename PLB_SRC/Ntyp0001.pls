.
. PURPOSE - Correct Typist records
.           1)TOTAL ORDERS PRINTED.
.           2)NUMBER OF NEW ORDERS PER TYPIST.
.           3)NUMBER OF Updates PER TYPIST.
.           4)PERCENTAGES OF ABOVE COMPARED TO TOTALS.
. .............................................................................
. FILES.
. ......
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NTYPDD.inc
         include   npasdd.inc

RElease   Init     "1.02"        DLH   allow change of initials
Reldate   Init      "2015 July 7" 
.RElease   Init     "1.01"        DLH   Update to reflect updates rather than reprints add upd pending
.Reldate   Init      "2013 October 21" 
.RElease   Init     "1.00"        DH 
.Reldate   Init      "23 April 2012"
.Set Up Menu Bar
mFile    menu
mEdit    menu
mHelp    menu
mOptions menu
.Set Up SubMenu for Options
sColor  submenu


.Present Data for Colors SubMenu
CData   init    ";&Background;&Text"
Holdkey    Dim        9


.Define Colors for Each Object
FTC     color
BGC     color
white     color
grey  color

colornum dim   9(2)
Fred     form    3
Fgreen   form    3
Fblue    form    3
colorflag form   1
prfile  pfile
NINLogo   PICT
          CREATE    NINLogo=3:13:30:50:
                    "\\nins1\e\netutils\NIN logo black outline.jpg"
font1   font
font2   font
font3   font
font4   font
font5   font
.....
font6   font
FontO7    font
FontO18B  font

          create    fontO7,"Times New Roman",size=7
          create    fontO18B,"Times New Roman",size=18,Bold

        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=9
        create  font3,"Helvetica",size=9
        create  font4,"Fixed",size=10
        create  font5,"Arial",size=11
        create  font6,"Arial",size=14
.Present Data for Menu Bar
FData   init    "&File;&Print;Pre&view;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
OData   init    "&Options;&Color"
HData   init    "&Help;&About"


.Create Colors for EditText Inquiry
          create    white=*white
.................................
.Set Vars used for About Box
        move    "NTYP0001.PLS",Wprognme
        move    "Typist File Maintenance",Wfunction
        move    "David Herrick",Wauthor
        move    Release,Wrelease
        Move    Reldate to Wreldate

.Declare forms, Always declare child forms first
mss1    plform  Error
pss     plform  Passwrd
abt     plform  About
x       plform  NTYP0001
        winhide
.Load Forms, Always load parent form first
.Load Forms, Always load parent form first
        formload x
        formload abt
        formload pss
        formload mss1
.        formload rpt

.                CREATE  TIMER,18000     .30 minutes
.        ACTIVATE TIMER,Timeout,RESULT
.Create Menus
        create  NTYP0001;mFile,FData
        create  NTYP0001;mEdit,EData,mFile
        create  NTYP0001;mOptions,OData,mEdit
        create  NTYP0001;mHelp,HData,mOptions
.Create SubMenus

.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
.Need this when it works
        activate mEdit,EditGo,result
.Only a SubMenu under these
        activate mOptions
        activate mHelp,HelpGo,result
.Activate SubMenus
.        activate sColor,ColorGo,result

.Create Colors for EditText Inquiry
        create  grey=*ltgray

          setprop   NTyp0001ButtonSave,Enabled=0
          setprop   NTyp0001ButtonSave,Visible=0
          setfocus  NTyp0001EditTextKey


           EVENTREG  X, 17, XRESIZE



    loop
                waitevent
        repeat

LoadForm
          getitem   NTyp0001EditTextKey,0,str9
          packkey    NTypfld,str9
          move        str9 to holdKey
          call      NTypkey
          if        over
          setfocus  NTyp0001EditTextKey
          alert     note,"That Record Does Not Exist!",result
          setprop   NTyp0001ButtonSave,Enabled=0
          setprop   NTyp0001ButtonSave,Visible=0
          return
          endif
          Clear     str5
          move      Subcount,str5
          setitem   NTyp0001EditNumber1,0,str5
          Clear     str5
          move      repcount,str5
          setitem   NTyp0001EditNumber2,0,str5
          Clear     str5
          move      LSUBCNT,str5
          setitem   NTyp0001EditNumber3,0,str5
          Clear     str5
          move      LRepCNT,str5
          setitem   NTyp0001EditNumber4,0,str5
          Clear     str5
          move      InvCOUNT,str5
          setitem   NTyp0001EditNumber5,0,str5
          Clear     str5
          move      InvRcnt,str5
          setitem   NTyp0001EditNumber6,0,str5
          Clear     str9
          move      SubQty,str9
          setitem   NTyp0001EditNumber7,0,str9
          setprop   NTyp0001ButtonSave,Enabled=1
          setprop   NTyp0001ButtonSave,Visible=1
.add more fields
.new adjustments
          Clear     str5
          move      ADJCount,str5
          setitem   NTyp0001EditNumber8,0,str5
.new Pending
          Clear     str5
          move      PndCOunt,str5
          setitem   NTyp0001EditNumber9,0,str5
.new datacards
          Clear     str5
          move      lstCount,str5
          setitem   NTyp0001EditNumber10,0,str5
.Updated datacards
          Clear     str5
          move      lstuCount,str5
          setitem   NTyp0001EditNumber11,0,str5
.upd Pending
          Clear     str5
          move      PndUCount,str5
          setitem   NTyp0001EditNumber12,0,str5




          Return
Verify
          Clear     str5
          getitem   NTyp0001EditNumber1,0,str5
          move      str5,Subcount

          Clear     str5
          Getitem   NTyp0001EditNumber2,0,str5
          move      str5,repcount


          Clear     str5
          move      LSUBCNT,str5
          Getitem   NTyp0001EditNumber3,0,str5
          move      str5,LSUBCNT

          Clear     str5
          Getitem   NTyp0001EditNumber4,0,str5
          move      str5,LRepCNT

          Clear     str5
          Getitem   NTyp0001EditNumber5,0,str5
          move      str5,InvCOUNT

          Clear     str5
          Getitem   NTyp0001EditNumber6,0,str5
          move      str5,InvRcnt


          Clear     str9
          Getitem   NTyp0001EditNumber7,0,str9
          move      str9,SubQty
.add more fields
.new adjustments
          Clear     str5
          getitem   NTyp0001EditNumber8,0,str5
          move      str5,ADJCount
.new Pending
          Clear     str5
          getitem   NTyp0001EditNumber9,0,str5
          move      str5,PndCOunt
.new datacards
          Clear     str5
          getitem   NTyp0001EditNumber10,0,str5
          move      str5,lstCount
.Updated datacards
          Clear     str5
          getitem   NTyp0001EditNumber11,0,str5
          move      str5,lstuCount
.upd Pending
          Clear     str5
          getitem   NTyp0001EditNumber12,0,str5
          move      str5,PndUCOunt
.............................


          setprop   NTyp0001ButtonSave,Enabled=0
          setprop   NTyp0001ButtonSave,Visible=0
          

          getitem   NTyp0001EditTextKey,0,str9

          call      Ntyptst

          if          (str9 = Holdkey)
          if          (str9 = "201507KQ" | str9 = "201507KQ ")
          Unpack    str9 into str6,NtypTYPE
          endif
          Call      NtypUpd
          Else
          call      Ntypdel
          packkey   Ntypfld,str9
          Unpack    str9 into str6,NtypTYPE
          call      Ntypwrt
          endif


          setfocus  NTyp0001EditTextKey


          Return




EditGo
HelpGo
        setprop AboutMssg,visible=1
        return
FileGo
.Flag set to "N" if in Modify or New mode
        branch result to FileGo1,FileGo2,FileGo3,FileGo3
FileGo1
        RETURN
FileGo2
        RETURN
FileGo3
                winshow                 
                stop



XRESIZE
           NTyp0001.Scale
           RETURN

         INCLUDE   NTYPio.inc
         include   npasio.inc
         Include    Comlogic.inc
