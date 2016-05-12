pc      equ        0
         include   common.inc
         include   cons.inc
.         include   tncXdd.inc
         include   SXRFdd.inc
         include   Npkgdd.inc
         include   statsdd.inc
         include   norddd.inc
         include   ndatdd.inc
          include  hp.inc
.begin patch 1.3
          include  SlctClnDD.inc
.end patch 1.3
.START PATCH 1.6A ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 1.6A ADDED LOGIC
.START PATCH 1.8.1 ADDED LOGIC
          include   COMPDD.INC
          include   CNTDD.INC
.END PATCH 1.8.1 ADDED LOGIC
release   init     "1.8.2"           03Feb2005 JD must have lr #.
.release   init     "1.8.1"           27JAN2005   ASH       NINSTATS Conversion
.release   init     "1.8"           09AUG2004     ASH       Logo Conversion
.release   init     "1.7"           08APR04 updated printer setting for automation
.release  init      "1.6A"    ASH       29JAN2004  DATACARD CONVERSION
.reldate  init      "JANUARY 29, 2004"
.release   init     "1.6"           03Mar04 Update Input file name
.release   init     "1.5"           23Jan04 updated to receive new input format.
;release   init     "1.4"          14Feb02 DLH turn off file clean up so Henry does not kill the files.
.release   init     "1.3"          07Feb02 DLH added Select Clean
.release   init     "1.25"          07DEC01 JD DUPE LR CLEANUP.
.release   init     "1.23"         19NOV01 ASH CONVERTED STATSFILE, CONVERTED NINPKG
.release   init     "1.22"         23Oct2001 JD Dupe Lr cleanup
.elease  init      "1.21"         20Aug2001 DLH
.release  init      "1.2"         05July2001 DLH  new Indices see Statsdd
.release  init      "1.1"         04OCT2000 ASH NEW SERVER ADDED
.release  init      "1.0"         06Jun00 DLH
.release   init     "0.002"       27Jan00 DLH per Brian T TNC list codes starting with F are seeds.
.                                                                       .......
.release   init     "0.001"       ..24Jan00 (double aught) To apply the TRIPLEX TNC  MERGE file
.                                                                       .......
except   file
batch    file                 .created for temp use
RECSIN   FORM      6
STR256   DIM       300
.str13    dim       13
str90    dim       90
str56    dim       56
DECIMAL  FORM      8.6
PERCENT  INIT      "%"
cntrbflg FORM      "0"
input    file       
TIME     INIT      "HH:MM:SS"
.offer    dim         3
minusflg dim        1
statlrsv dim        6
mqty     dim        8
mqtyb    dim       10
LAQTY    form        8
lAQTYB   FORM       8
LASRCDE  DIM       14
lamdate  dim        8
z96      dim        6
mmyy     dim        4
Akey1    dim        3
RESP     dim        8
Rresp     dim        8
NCI      dim        8
pci      dim        8
CI       dim        8
revenue  dim        9
gift     dim        5
lstcpm   dim        10
tlst$     dim        12
pckcpm    dim        6
tpck      dim        12
totcpm    dim        9
mailcost  dim        10
unitpc    dim        8
totpc     dim        9
totcst    dim        15
totcst13  dim        13
totlcost  dim        11
netrev8   dim        8
netrev    dim        13
nrpci     dim        10
nrnci     dim        10 
nrpcia    dim        9          was 8
nrncia    dim        9          was 8  dlh 03jun97
CstA9     dim        9
CstA      dim        10
pcaci     dim        10
Ncaci     dim        10
CTA       dim        31
cost$     dim        6
nqty      dim        12
inv       dim        10
lcpm      dim        7
type      dim        1
NAMresp   dim        10
NAMrev    dim        10
IAMresp   dim        10
IaMrev    dim        10
Assocresp dim        9
assocrev  dim        9
assocrresp dim       8
basresp   dim        9
BRResp    dim        8
BREv      dim        9
TAresp    dim        9
inmailcpm dim        6
aggrindx  dim        4
premcost  dim        6
oldflag   form       1
page      form       4
lines     form       2
weeks     form       5
Savelr        dim            6
.INput file
srccode   dim       14             1-10   key
Lstcode        dim        4    15-18
pkgcode        dim        8    19-26
tstcode        dim       8     27-34
tstcell        dim       8     35-42
technique      dim       2     43-44
incomenw       dim       12     45-53
returnnw       dim       8    114-57
incomeRN       dim       12    105-66
returnRN       dim       8    114-70
incomeRJ       dim       12    105-712
returnRJ       dim       8    114-83
incomeUR       dim       12    105-122
returnUR       dim       8    114-126
incomeAD       dim       12    105-105
returnAD       dim       8    114-1012
incomeDO       dim       12    105-118
returnDO       dim       8     114-122

.work vars
incometmp      form      9.2
incometmp1     form      9
returntmp      form      5
.

weeksout  dim        5
.copycmd  init      "!c:\command.com /C copy \\SRV2008A\d\data\exceptnc.lst \\NINs2\Laser3 "
.START PATCH 1.1 REPLACED LOGIC
.copycmd  init      "!c:\command.com /C copy \\SRV2008A\d\data\exceptnc.lst lpt1: "
copycmd  DIM        50
         PACK       copycmd,"!c:\command.com /C copy ",NTWKPATH1,"exceptnc.lst lpt1: "
.END PATCH 1.1 REPLACED LOGIC
ninlist  dim        6
.
Path    init      "C:\work"
Fname   Dim        50
pathname dim       200
dupeflag  form       1           0=no dupe, 1 = dupe
OQTY1     DIM        9
O2DES1    DIM        35
formstuff
.ErrorMssgEditTextBox that is created and destroyed dynamically
.Note that this EditText fills spot for ErrorMssgStat4!!
ErrorMssgEdit1  EditText

.Colors
white   color
grey    color
RED     COLOR
BLACK   COLOR
Yellow  Color

.Define Fonts to be used
font1   font
font2   font
font3   font
font4   font
font5   font

.Set Up Menu Bar
mFile   menu
mEdit   menu
mHelp   menu

.Present Data for Menu Bar
FData   init    "&File;&Print;-;E&xit"
EData   init    "&Edit;<1&Undo;-;<2&Cut;<3&Copy;<4&Paste;<5&Delete;-;<6&Select All"
HData   init    "&Help;&About"
.some goodies for on moving Icon
ICONID   FORM      4
CURICON  FORM      4
FRAMES   FORM      4
ICON$ANIM    ICON
FORM32   FORM      32
FORM32A  FORM      32
FORM32B  FORM      32
ordkodes dim       4
;statmail init      "0179-0173-0170-1746"
.START PATCH 1.8.1 REPLACED LOGIC
.statmail init      "0173-0170"
statmail init      "000619-000913"
.END PATCH 1.8.1 REPLACED LOGIC
.
ANIMICON PLFORM    ANIMATE  * CONTAINS ALL THE ICONS

.end icon goodies
.Declare forms, Always declare child forms first
.RUSure   plform  AreYouSure
mss1     plform  Error
abt      plform  About
x        plform  stat0005a

        move    "stat0005.PLS",Wprognme
        move    "Apply TDMC file",Wfunction
        move    "David Herrick",Wauthor
        move    release,Wrelease
        move    "July 5 2001",Wreldate
         winhide

.Load Forms, Always load parent form first
        formload x
.        formload STAT0005,NMDL0001
        formload abt,stat0005a
        formload mss1,stat0005a
        FORMLOAD ANIMICON,stat0005a
.
.Create Menus
        create  STAT0005a;mFile,FData
        create  STAT0005a;mEdit,EData,mFile
        create  STAT0005a;mHelp,HData,mEdit

.        activate Default_Resource
.Activate Menus
.FileGo leads to stop
        activate mFile,FileGo,result
        activate mEdit,EditGo,result
        activate mHelp,HelpGo,result
        

.Create Colors for EditText Inquiry
        create  white=*white
        create  grey=*ltgray
        create  RED=*RED
        create  Yellow=*Yellow
        create  black=*black
        
.Create fonts to be used
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8 
        create  font3,"Helvetica",size=9
        create  font4,"Arial",size=14,italic
        create  font5,"Arial",size=10
.
.Dynamically reset Animate as same size as STAT0005
anim1
        getprop STAT0005a,height=H
        setprop Animate,height=H
        getprop STAT0005a,width=V
        setprop Animate,width=V
.Must clear Resize Event which was tiggered when Animate was resized,
.as this is where the AnimateIt subroutine sits.
        clearevent
        moveaddr STAT0005a,AnimateWindow
        move    C1,AnimateCurIcon
        move    C4,AnimateFrames
        move    C0,AnimateIconID
        move    "400",H
        move    "560",V
.
        setfocus Stat5EditText001
.patch1.6 Comment Out
..       move       "MRG*.*",Fname
..        GETFNAME        OPEN,"Open Test", FNAME,PATH,"PLS"
..        IF      NOT OVER
.       DISPLAY *HD,*R,"File name returned: ",FName
.        DISPLAY        *HD,*R,"     Path: ",PATH,"  ",*w3;
..        ELSE
.       KEYIN   *HD,*R,"Dialog cancelled...",str1;
..        stop
..        ENDIF
..        setfocus Stat5EditText001
..        clear    pathname
..        pack     pathname from path,slash,fname
                      move "\\nins1\e\data\mrgsrce.dat" to pathname
.Patch1.6
        open      input,pathname,exclusive
        
        setitem   Stat5edittext001,0,pathname
        setprop   stat5edittext001,Visible=1
        setprop   stat5edittext002,Visible=1
        setprop   stat5Stattext001,Visible=1
        setprop   stat5Stattext002,Visible=1
.        open      input,"c:\work\tncmrgm.tpx",exclusive
.        open      input,"c:\data\DMA1999a.csv",exclusive
        CLOCK       DATE TO TODAY
        CLOCK     TIME TO TIME
.        prepare     except,"\\nins1\d\users\dherric\exceptnc.tdmc",exclusive
.        prepare     except,"c:\data\except1.TNC",exclusive
.        read        except,seqeof;str1
        move       "stat0005" to program
        move       "Names in the News CA" to compnme
.        call       paint
.        splopen     "g:\data\exceptnc_tdmc.lst"
;Patch1.7
                             Call           GetWinVer
               if             (osflag = c1 or Osflag = C5 or OsFlag = C6)         .nt or win2000 or Windows XP
               splopen        "\\NINs2\Laser8","R"
               Elseif         (osflag = c3 or OsFlag = C4)         .win 95 98
               splopen        "\\NINs2\Laser8","R"
               Elseif         (osflag = c0)         .Don't know prompt for printer
               splopen        "","R"
               endif
.        splopen     "","R"
;Patch1.7
.        splopen     "c:\data\exceptnc.lst"
        print       hpport,*rptchar "*":80:
                    *n,*n,*n,hpbon:
                    *n,*1,"Program    : ",program,b2,"Date: ",today:
                    b2,"Time: ",time:
                    *n,*1,"Company    : ",compnme:
                    *n,*1,"User       : ":
                    *n,*1,"Deliver To : Henry Most",hpboff:
                    *n,*rptchar "*":80
        move       c1 to statpath
        clear       statpdate

looper  CALL   ANIMATEIT
.patch 1.21
.        move        "0173" to statmlr
.end patch 1.21
        read       input,seq;statlr:
                    str56:          filler dim 56
                        mqty:               dim       8
                        str1:
                        srccode        dim        14
        
        goto       eoj if over
.begin patch 1.82
           type     statlr
                              goto      looper if not equal
.end patch 1.82
        move       c0 to dupeflag
                      MOVE       C0 TO LAQTYB
        scan       "AHOMQ990801001" in srccode
        call       debug if equal
                      bump       srccode by 11
                      match      "949" to srccode
                      if         equal
                      MOVE        mqty to laqtyB
                      ADD        LAQTYB TO LAQTY
                      reset      srccode
                      move       srccode to lasrcde
                      goto       looper
                      endif
.        display    *p10:10,"records in ",recsin;
        reset      srccode
        add        c1 to recsin
        move       recsin to str6
        setitem   Stat5edittext002,0,str6
        setfocus   Stat5edittext002
        match      "SOURCECODE" in srccode
        goto       looper if equal
        match      "              " in srccode
        goto       looper if equal
        cmatch     b1 to srccode
        goto       looper if eos
.begin patch .002
        cmatch     "F" to lstcode
        goto       looper if equal
.end patch .002
           type     statlr
                                if       equal
         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi
         clear     nordfld
         move      statlr to nordfld
         rep       zfill in nordfld
         call      nordkey
.START PATCH 1.8.1 REPLACED LOGIC - TEMPORARY LOGIC
.                             move      omlrnum to statmlr
          move      omlrnum,COMPFLD3
          move      "COMPKEY3",Location
          pack      KeyLocation,"Key: ",COMPFLD3
          call      COMPKEY3
          move      COMPNUM,statmlr
.END PATCH 1.8.1 REPLACED LOGIC - TEMPORARY LOGIC
                              endif
                              reset     statmail
                              scan      statmlr in statmail
                              goto      looper if not equal
         move       srccode to statsrce
.        clear      str6
.        clear      str4
.        count      n1,lstcode
.        if         (n1 < c4)    .string less than 4 bytes   
.                cmatch     "Z" to lstcode              .for some reason TDMC still has uses some old list codes
.                if         not equal
.                move       "Z" to str1
.                pack       str4 from str1,lstcode
.                move       str4 to lstcode
.                endif
.        endif
.        clear      statkycd
.        move       lstcode to statkycd             .DLH 21Dec99
........
        move       c0 to statresp
        move       c0 to statrev
        move       c0 to statmqty
        call       trim using mqty
        move       mqty to statmqty

.        clear      statlr
.................................................................        

******************************************************************************
.
.        clear       ninlist
         PACKKEY     statFLD FROM statmlr,statsrce
.         move        ninlist to statlist
.         rep         zfill in statlist
.
.                  
         
         move      b8 to str8
.         goto      tempdlh
         compare   c1 to statflag
         call      statopen if not equal
         FILEPI    1;statFILE
.begin patch 1.2
.START PATCH 1.23 REPLACED LOGIC
.         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,str12,*264,Statpckm
.
.START PATCH 1.8.1 REPLACED LOGIC - TEMPORARY LOGIC
.         READ      statFILE,statFLD;*35,str8,*202,str6,*233,statlcpm,statImcst,statlrsv,str25,*288,Statpckm
         READ      statFILE,statFLD;*37,str8,*204,str6,*235,statlcpm,statImcst,statlr2,statlrsv,str25,*293,Statpckm
.END PATCH 1.8.1 REPLACED LOGIC - TEMPORARY LOGIC
.END PATCH 1.23 REPLACED LOGIC
.         READ      statFILE,statFLD;*191,str6,*238,statlr
         if        not over
         move      str6 to statlist       *retain list number (it may have been 
.                                         *nmanually cleaned. DLH 16Oct97         
         move      str8 to statmdate      *retain maildate - no longer in dmatext file as part of source code
.START PATCH 1.23 REPLACED LOGIC
.         move       str12 to statkycd      *retain corrected list code
         move       str25 to statkycd      *retain corrected list code
.END PATCH 1.23 REPLACED LOGIC
         move       statlrsv to statlr
         else
.         move      b6 to statlr
         endif

getord
.         if        (statlr = "" or statlr = "      " or statlr = b1)
.         clear     statlr
.         goto      findorder
.         endif
.end test test test
         type      statlr
         goto      needclean if equal                     *we have an lr so skip order file search
.         endif
.
.
Findorder
.         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
.         UNPACK   STATMDaTE INTO MM,DD,STR2,YY
.         move       c0 to dupeflag
.         CALL      NORDAIM
.         IF        NOT OVER
.                 move      "pxlz" to ordkodes       ..14Mar00 DLH
.                 reset     ordkodes
.                 scan      ostat in ordkodes
.                 goto      nordloop if equal
.                   clear    str4
.                   UNPACK   STATMDaTE INTO MM,DD,STR2,YY
.                   PACK     STR4 FROM MM,YY
.                   PACK     MMYY FROM  OMDTEM,OMDTEY
.                   REP      ZFILL IN STR4
.                   REP      ZFILL IN MMYY
.                   MATCH    MMYY TO STR4
.                    IF       EQUAL
.                    MOVE     OLRN TO STATLR
.                    ENDIF
NORDLOOP          
.                   MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
.                   CALL     NORDKG
.                   goto     needclean if over
..                   unpack   oodnum into str4,str3
..                   match    offer to str3
..                   goto     nordloop if not equal
.                 move      "pxlz" to str4       ..14Mar00 DLH
.                 scan      ostat in str4
.                 goto      nordloop if equal
.                    clear     mmyy
.                   UNPACK   STATMDaTE INTO MM,DD,STR2,YY
.                   clear     str4
.                   PACK     STR4 FROM MM,YY
.                    PACK     MMYY FROM OMDTEM,OMDTEY
.                    REP      ZFILL IN MMYY
.                   REP      ZFILL IN STR4
.                    MATCH    MMYY TO STR4
. .                   goto     nordloop if not equal
.                     IF       EQUAL
.                               type   STATLR 
.                               if     equal
.                               call   except3 
.                               clear  statlr
.                               else
gotone
.                               move   c0 to dupeflag
.                               MOVE     OLRN TO STATLR
.                               endif
.                               goto     EXCEPT3
..                             ELSE
.                              ENDIF
.                     else
. .                    goto     nordloop
.                    ENDIF
.         endif

needclean
         type      statlr
         goto      wksout if not equal
         cmatch    b1 to statlr
         goto      wksout if eos
         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi
         clear     nordfld
         move      statlr to nordfld
         rep       zfill in nordfld
         call      nordkey
         call      except2 if over
         call      getlist
         packkey   statmdate from omdtem,omdted,omdtec,omdtey
                              move      statmdate to lamdate
         MOVE      OQTY TO OQTY1
.START PATCH 1.6A REPLACED LOGIC
.         MOVE      O2DES TO O2DES1
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.END PATCH 1.6A REPLACED LOGIC
         clear     stattype
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
         if        equal
         move      "E" to Stattype
         else    
         move      "R" to stattype
         endif
         MOVE      C0 TO N9
         MOVE      OEXQTY TO N9
         COMPARE   C0 TO N9

         IF        Not EQUAL
         move      "S" to stattype
         endif

         clear     statsel
.begin patch 1.3
.START PATCH 1.6A REPLACED LOGIC
.               packkey        SlctClnFld from O2des
               packkey        SlctClnFld from NSEL2NAME
.END PATCH 1.6A REPLACED LOGIC
               call           SlctClnKey
               if             not over
               move           SlctClnText to Statsel
               else
.START PATCH 1.6A REPLACED LOGIC
.               move           O2Des to Statsel
               move           NSEL2NAME to Statsel
.END PATCH 1.6A REPLACED LOGIC
               endif
.         move      O2DES to statsel        .show list select
.end patch 1.3



.calc  weeksout
wksout
        move       c0 to statwkso


.




.         endif
stattest 
.begin patch 1.2
.         move       c3 to statpath
.         pack        statfld3 from statmlr,statsrce
         move       c1 to statpath
         pack        statfld from statmlr,statsrce
.end patch 1.2
         CALL        statTST
         IF          NOT OVER
         CALL        statDEL
         ENDIF
        type        statlr
        if          not equal
        move        Statxdesc1 to statsel
        endif
        if          (statlr = "" or statlr = "      ")
        move        Statxdesc1 to statsel
        endif
        if          (statsel = "" or statsel = "      ")
        move        Statxdesc1 to statsel
        endif

         IF          (DUPEFLAG = C1)            .MULTIPLE LRS'S
         CLEAR       STATLR
         ENDIF

         call        statwrt
         clear     statsel
         clear     statldes
        clear       stattype
         clear        statlr
         goto        looper
.          call      getord
.         call       getlist
.TEMPDLH
.         rep       zfill in statlist
.         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
.         MOVE      "01R",aKEY1
.         PACKkey      NORDFLD1 FROM AKEY1,statmlr
.         MOVE      "02R",aKEY1
.         PACKkey      NORDFLD2 FROM AKEY1,statLIST
.         CLEAR     NORDFLD3
.         CLEAR     NORDFLD4
.         Clear     olrn
.         goto      getord
.begin patch .003
.        move        "list not found" to Statxdesc1
.end patch .003

getlist
.        clear       statlist
.        clear       statldes
.        clear       Statxfld
.        move        "0173" to statmlr

.        call         trim using statkycd
.        packkey       statxfld from statmlr,statkycd
.        move        c1 to Statxpath
         clear       Statxlist
         clear       ninlist
.
.         move        "0173" to statmlr
.
.         match       "0173" to statmlr
.         if          equal
.          display    *p1:23,*el,TNCxfld
.          cmatch      b1 to tncxfld
.          cmatch      b1 to statkycd
.          if          eos
.          move        "000" to tncxfld
.          move        "000" to statkycd
.        packkey       statxfld from statmlr,statkycd
.          endif
.          call        TNCxkey
.          call        Statxkey
.         if          over
.                  move        "000000" to Statxlist
.                   clear         Statdesc                    .    nin desc
.                  display     *p10:14,tncxfld," ",tncxlist
.                  match       "000000" to Statxlist
.                                    if          equal
.                                    move        no to str1
.                                    else
.                                    move        yes to str1
.                                    endif
..                 display     *p10:15," not found Enter it? ",str1,*p10:15,*el
.                  rep         "nN" in str1
.                  cmatch       no to str1
.                  goto         skipit if equal
.                  unpack      statxfld into Statxmlr,statxcode
.                  call        Statxwrt
.                  clear       stattype
.                  goto        skipit1
.skipit
.                  noreturn
.                  goto        except
.                 endif
.skipit1
.begin patch .003      move TNC list name to select so if we have no order LR
.                  move         Statxdesc1 to statsel
.end patch .003        maybe we can still ID
.                 display    *p1:23,*el,TNCxlist
.                  move        Statxlist to ninlist
.                       move        statxlist to statlist
.                  match       "000000" to ninlist
.                         if           equal
.                   if     (ninlist = "002303")          .if its the house file
.                   move   Statxdesc1 to statsel           .show the select
.                   endif
.
.          if     (ninlist = "000000" or ninlist = "      ")
.                 move         Statxdesc1 to statldes
.                 goto        except
.                 else
                 move        olnum to ndatfld
                 move        c1 to ndatpath
                 move        c3 to ndatlock
                 rep         zfill in ndatfld
                 call        ndatkey
                         if          not over
                         move        olstname to statldes
                     move        lstnum to statlist                                                                      move        lstnum to statlist
                        endif
.                 endif
.         endif
.         pack      statcampn from b25,b5

.         unpack     pkgcode into str5,str6                 tnc
.         packkey    Npkgfld from statmlr,str6
.START PATCH 1.23 REPLACED LOGIC
.         packkey    Npkgfld from statmlr,pkgcode
.         clear      statpanel
..         call       tncpkey
.         call       Npkgkey
.         if         not over
.         move       Npkgdesc to statpanel
.         else 
.         call       except4
.         endif
...........
.         clear     NPKGFLD2
.         clear     NPKGFLD3
.         pack      NPKGFLD1,"01X",STATMLR
.         pack      NPKGFLD4,"04X",PKGCODE
.         clear     STATPANEL
.         move      C1,NPKGPATH
.         move      "NPKGAIM",Location
.         pack      KeyLocation,"Key: ",NPKGFLD1,NPKGFLD4
.         call      NPKGAIM
          if not over
                    move      NPKGPNAME,STATPANEL
          else
                    call      except4
          endif
.END PATCH 1.23 REPLACED LOGIC
.
         rep         zfill in ninlist
         return

..begin patch 1.2
..         PACK        statFLD FROM NINLIST,statmlr,statcampn,statsrce
..         PACK        statFLD FROM statmlr,statsrce
..end patch 1.2
..         move        ninlist to statlist
.         rep         zfill in statlist
..
..
.
.         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
.         MOVE      "01R",aKEY1
.         PACK      NORDFLD1 FROM AKEY1,statmlr
.         MOVE      "02R",aKEY1
.         PACK      NORDFLD2 FROM AKEY1,statLIST
.         CLEAR     NORDFLD3
.         CLEAR     NORDFLD4
.         move      b6 to statlr
.         Clear     olrn
.         move      b8 to str8
.         compare   c1 to statflag
.         call      statopen if not equal
.         FILEPI    1;statFILE
..begin patch 1.2
.         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,str12,*264,Statpckm
..         READ      statFILE,statFLD;*191,str6,*238,statlr
.         if        not over
.         move      str6 to statlist       *retain list number (it may have been
..                                         *nmanually cleaned. DLH 16Oct97
.         move      str8 to statmdate      *retain maildate - no longer in dmatext file as part of source code
.         move       str12 to statkycd      *retain corrected list code
.         rep       zfill in statlist
.test test test
EXCEPT
.         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY,"No List ",tncxfld
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
                     *n,statpanel,hpbon,"No List Number Found ",hpboff,Statxfld,B1,STATMQTY,*n,*n
         add         c4 to lines
         GOTO        LOOPER

EXCEPT2 
.         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY," no lr"
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
                     *n,statpanel,hpbon," No LR",hpboff,b1,statlr,b1,olrn,B1,STATMQTY,*n
         add         c4 to lines
         return
       
EXCEPT3
.  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY," to many lrs"
         compare     c0 to page
         call        header if equal
         if          (lines = "51" or lines > "51")  
         call        Hd1
         endif
          branch      dupeflag to except3a
         move        c1 to dupeflag
.        print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                    statldes:
.                    *n,*68,statseL:
.                    *n,hpbon,"To MANY LR'S ",STATLR,B1,oqty1,b1,o2des1,STATMQTY:
.                    *n,*1,OLRN,b1,oqty,b1,o2des,hpboff
.        add         c5 to lines
.        CLEAR       STATLR
.        return
.begin patch 1.22
         move        olrn to savelr
         packkey     nordfld from statlr
         move        c1 to nordpath
         call        nordkey
.end patch 1.22
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
.begin patch 1.22
.                     *n,hpbon,"to MANY LR'S ",STATLR,B1,oqty1,b1,o2des1,STATMQTY:
.end patch 1.22
.START PATCH 1.6A REPLACED LOGIC
.                     *n,hpbon,"to MANY LR'S ",STATLR,B1,oqty,b1,o2des,STATMQTY;
                     *n,hpbon,"to MANY LR'S ",STATLR,B1,oqty,b1,NSEL2NAME,STATMQTY;
.END PATCH 1.6A REPLACED LOGIC
         packkey     nordfld from savelr
         call        nordkey
.end patch 1.06
.START PATCH 1.6A REPLACED LOGIC
.         print       *n,*1,OLRN,b1,oqty,b1,o2des,hpboff
         print       *n,*1,OLRN,b1,oqty,b1,NSEL2NAME,hpboff
.END PATCH 1.6A REPLACED LOGIC
         add         c5 to lines
.
          clear      o2des
          clear      statsel
.
         return
.START PATCH 1.6A REPLACED LOGIC
.except3a  print       *n,hpbon,*1,OLRN,b1,oqty,b1,o2des,hpboff
except3a  print       *n,hpbon,*1,OLRN,b1,oqty,b1,NSEL2NAME,hpboff
.END PATCH 1.6A REPLACED LOGIC
         add         c1 to lines
         return
      
ExCEPt4 
.        write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY," no lr" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
                     *n,statpanel,hpbon," No Package desc",hpboff,b1,statlr,b1,olrn,B1,pkgcode,*n
         add         c4 to lines
         return


Header   PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
         move      c1 to page
.START PATCH 1.8 REPLACED LOGIC
.HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      hpt1000,"DATE: ",TODAY:
.                   *N,*01,"(0173)":
.                      hpt275,"Statistical Data Exception Report:":
.                      hpt700,"PAGE:    ",PAGE:
.                   *N,"Weeks Out : ",statwkso:
.                      hpt275,"TDMC FILE:",Fname:
.                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
.                   hpt325,"TNC##",hpt350,"List Name",hpboff,*n
HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
                      *52,"NAMES IN THE NEWS":
                      hpt1000,"DATE: ",TODAY:
                   *N,*01,"(0173)":
                      hpt275,"Statistical Data Exception Report:":
                      hpt700,"PAGE:    ",PAGE:
                   *N,"Weeks Out : ",statwkso:
                      hpt275,"TDMC FILE:",Fname:
                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
                   hpt325,"TNC##",hpt350,"List Name",hpboff,*n
.END PATCH 1.8 REPLACED LOGIC
         add         c1 to page
         move        c8 to lines
         return
eoj
.      weof        except,seqeof
.         close       except
         clear       statsrce
                              clear       statldes
                              clear       statlist
                              clear       statmqty
                              clear       statsel
         move       lasrcde to statsrce
                              move       "LOS ANGELES NAMES                      " TO STATSEL
                              MOVE       "LA NAMES                               " TO STATLDES
                              MOVE       "017321" TO STATLIST
                              MOVE       LAQTY TO STATMQTY
                              MOVE       B6 TO STATLR
                              move       lamdate to statmdate
   move       c1 to statpath
         pack        statfld from statmlr,statsrce
.end patch 1.2
         CALL        statTST
         IF          NOT OVER
         CALL        statDEL
         ENDIF
                              call        statwrt
         print       *f,*flush
         splclose
         release
.begin patch 1.4
.         path      exist,"c:\windows"
.         if        not over
.         execute    "!c:\command.com /c copy \\SRV2008A\d\data\exceptnc_tdmc.lst lpt1:"
.         else
.         execute    "!c:\winnt\system32\cmd.exe /c copy \\SRV2008A\d\data\exceptnc_tdmc.lst LPT1:"
.         endif
.end patch 1.4

.         display   *p2:23,"Please wait I'm PRINTING !!!!!"
.         pause     "5"
.         Keyin       *p10:12,"Ready to sort and Reindex the NINSTATS file ? ",str1
.         cmatch      yes to str1
.         if          equal
.begin patch 1.4
.         path      exist,"c:\windows"
.         if        not over
..START PATCH 1.1 REPLACED LOGIC
.end patch 1.4
.         execute    "!c:\command.com /c Copy \\SRV2008A\d\data\text\ninstats.dat \\SRV2008A\d\data\text\ninstats.tmp"         
.         if         not over
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\sort32 \\SRV2008A\d\data\text\ninstats.tmp \\SRV2008A\d\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstats,L276 -191-196,1-4,5-34,117-130"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat2,L276 -238-243"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat3,L276 -1-4,117-130"
.         endif
.         else
.         execute      "c:\winnt\system32\cmd.exe /c Copy \\SRV2008A\d\data\text\ninstats.dat \\SRV2008A\d\data\text\ninstats.tmp"         
.         if         not over
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\sort32 \\SRV2008A\d\data\text\ninstats.tmp \\SRV2008A\d\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstats,L276 -191-196,1-4,5-34,117-130"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat2,L276 -238-243"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat3,L276 -1-4,117-130"
........
.START PATCH 1.23 REPLACED LOGIC
.         PACK       TASKNAME,"!c:\command.com /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.         execute    TASKNAME
.         if         not over
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute    TASKNAME
..         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -191-196,1-4,5-34,117-130"
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH1,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -1-4,117-130"
.         execute    TASKNAME
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L286 -238-243"
.         execute    TASKNAME
..         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH1,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat3,L286 -1-4,117-130"
..        execute    TASKNAME
.         endif
.         else
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.         execute      TASKNAME
.         if         not over
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstatS,L286 -1-4,117-130"
..         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -191-196,1-4,5-34,117-130"
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L286 -238-243"
.         execute      TASKNAME
...................
.begin patch 1.4
.                  PACK       TASKNAME,"!c:\command.com /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.                  execute    TASKNAME
.                  if         not over
.                            PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.                    execute    TASKNAME
.                            PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH1,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-4,117-141"
.                            execute    TASKNAME
.                            PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
.                            execute    TASKNAME
.                  endif
.         else
.                  PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.                  execute      TASKNAME
.                  if         not over
.                            PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.                            execute      TASKNAME
.                            PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstatS,L501 -1-4,117-141"
.                            execute      TASKNAME
.                            PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
.                            execute      TASKNAME
.END PATCH 1.23 REPLACED LOGIC
.
.        PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat3,L286 -1-4,117-130"
.        execute      TASKNAME
.END PATCH 1.1 REPLACED LOGIC
.         endif
.         endif
.         endif 
.end patch 1.4
         stop
FileGo
        stop
.        return
EditGo
HelpGo
        setprop AboutMssg,visible=1
        return    

.         CALL      CARDREAD
SetStatErrorMssgDefault
.Set Default for OrderFile Maintenance
        setprop ErrorMssgStat1,visible=1
        setprop ErrorMssgStat2,visible=1
        setprop ErrorMssgStat3,visible=0
        setprop ErrorMssgStat4,visible=0
        setprop ErrorMssgStat5,visible=0
        setitem ErrorMssgStat1,0," "
        setitem ErrorMssgStat2,0," "
        setitem ErrorMssgStat5,0,"      That Record Does Not Exist!"
        setitem ErrorMssgOK,0,"&OK"
        return
.debug    return
.         include     tncXio.inc
.begin patch 1.3
          include  SlctClnIO.inc
.end patch 1.3
         include   SXRFio.inc
         include   Npkgio.inc
         include    statsio.inc
         include    nordio.inc
         include     ndatio.inc
.START PATCH 1.6A ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 1.6A ADDED LOGIC
.START PATCH 1.8.1 ADDED LOGIC
          include   COMPIO.INC
          include   CNTIO.INC
.END PATCH 1.8.1 ADDED LOGIC
         include    comlogic.inc     
