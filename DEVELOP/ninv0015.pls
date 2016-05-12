.H.PROGRAM THAT ALLOWS CHANGING OF INVOICE FIELDS THAT ARE NORMALLY RESTRICTED.
PC         EQU         0
               INCLUDE   COMMON.inc
          INCLUDE   CONS.inc
               include   consacct.inc
.begin patch 2.4
.                   INCLUDE   NINVDD.inc
                    INCLUDE        ninvdd.inc
.end patch 2.4
Release	Init	"3.0"		.gui DLH
reldate	Init	"2014 July 11"
.release        init           "2.5"           20October2008  DLH Chkn3 keying
.release        init           "2.4"           08March2005  DLH Invoice Conversion
.;release  init      "2.3"           22Apr99 DLH see ninv0001 release 9.0
.RELEASE  INIT      "2.2"           29jan97 jd added keyin mlr #.
.RELEASE  INIT      "2.1"          13oct95 jd added keyin for net info.
.RELEASE  INIT      "2.0"         20MAR92   DLH
SAVELR     DIM         6      
ITEM     FORM      2
.begin patch 3.0
.Colors
white          color
grey           color
RED            COLOR
BLACK          COLOR
Yellow         Color
Green          Color
Blue           Color
Cyan           Color
MAGENTA        COlor
.Define Fonts to be used
font1                  font
Font4                  font
font5                  font
Font08                 font
font09                 font
Font09I                font
Font09B                font
Font09BI               font
Font012                font
Font012B               font
Font012I               font
Font014                font
Font014B               font
Font014BI              font
Font018I               font
Font07                 font
Font07dot5             font
Font07dot5B            font
Font07dot5I            font
Font07dot5BI           font
Font018B               font
Font018BI              font
PRTPG24B               font
PRTPG24I               font
PRTPG10                font
PrFIle         Pfile
NINLogo        PICT
.IntegralStoreDetail           external       "INT001A;IntegralStoreDetail"
.IntegralTestDetail            external       "INT001A;IntegralTestDetail"

Timer          Timer
Tabnum         form           2
SaveTab        form           2
X              plform         Ninv0015
abt            plform         About
               winhide

               formload       x
.Create fonts to be used
..Create fonts to be used
               create               font1,"Helvetica",size=14,bold
               create               Font08,"Helvetica",size=8
               create               font5,"Helvetica",size=11
               create               font09,"Helvetica",size=9
               create               Font09I,"Helvetica",size=9,Italic
               create               Font09B,"Helvetica",size=9,Bold
               create               Font09BI,"Helvetica",size=9,Bold,Italic
               create               Font012,"Helvetica",size=12
               create               Font012I,"Helvetica",size=12,Italic
               create               Font012B,"Helvetica",size=12,Bold
               create               Font014,"Helvetica",size=14
               create               Font014B,"Helvetica",size=14,Bold
               create               Font014BI,"Helvetica",size=14,Bold,Italic
               create               Font018I,"Helvetica",size=18,Italic
               create               Font07,"Helvetica",size=7
               create               Font018B,"Helvetica",size=18,Bold
               create               Font018BI,"Helvetica",size=18,Bold,Italic
               create               PRTpg24B,"Helvetica",size=24,Bold
               create               PRTpg24I,"Helvetica",size=24,Italic
               create               PRTpg10,"Helvetica",size=10
.               create         font4,"Arial",size=14,italic
              create          NINLogo=3:13:30:50:
               "\\nins1\e\netutils\NIN logo black outline.jpg"
.Create Colors
               create         white=*white
               create         grey=*ltgray
               create         RED=*RED
               create         black=*black
               create         Yellow=*yellow
               create         Green=*Green
               create         Blue=*Blue
               create         Cyan=*Cyan
               create         Magenta=*Magenta
               InvListView001.InsertColumn using "Rec",0,1
               InvListView001.InsertColumn using "Code",30,2
               InvListView001.InsertColumn using "Description",165,3
               InvListView001.InsertColumn using "Quantity",100,4
               InvListView001.InsertColumn using "Rate",75,5
               InvListView001.InsertColumn using "Per",30,6
               InvListView001.InsertColumn using "%",50,7
               InvListView001.InsertColumn using "P Code",50,8
               InvListView001.InsertColumn using "Q Code",30,9
               InvListView001.InsertColumn using "Amount",75,10
               activate        InvEditTextLR
               Setfocus        InvEditTextLR
               InvoiceListView.InsertColumn using "LR",50,1
               InvoiceListView.InsertColumn using "Mailer P.O.",80,2
               InvoiceListView.InsertColumn using "Mailer",40,3
               InvoiceListView.InsertColumn using "List",55,4
               InvoiceListView.InsertColumn using "Broker",50,5
               InvoiceListView.InsertColumn using "Mailer Key",80,6
               InvoiceListView.InsertColumn using "Other               Detail",100,7
.Set Following extended               properties for all ListView objects:
.  FullRowSelect
.  DragAndDrop
.  OneClickActivate
hexer               integer               1,"0x0070"
.
.Create/Activate Objects on OptionsOrd.plf
.Screen       1
              create          Options;OptionsBilledNo=60:80:40:255,"'Include Billed in Search' Default = 'No'",zorder=100
              create          Options;OptionsCancNo=80:100:40:300,"'Include Cnc/open in Search' Default = 'No'",zorder=100
              create          Options;OptionsPassInit=100:120:40:305,"Skip Password Check at Program Execution",zorder=100
              listins         Options1Coll,OptionsScreenInit,OptionsBilledNo,OptionsCancNo,OptionsPassInit
              setitem         OptionsBilledNo,0,c0
              setitem         OptionsCancNo,0,c0
               setitem        OptionsPassInit,0,c0
              setprop         Options1Coll,visible=1        .always        start with the first tab visible

.Screen       5
.             create          Options;OptionsScreen5BrkFilter=80:100:40:250,"Filter Broker Report Records",zorder=100
.             create          Options;OptionsScreen5EMailOption=100:120:40:250,"Display EMail            Option Message",zorder=100
.             create          Options;OptionsScreen5FileDefault=120:140:40:250,"Use Default Notes File",zorder=100
              listins         Options5Coll,OptionsScreen5BrkFilter,OptionsScreen5EMailOption,OptionsScreen5FileDefault

.Screen       6
.              create          Options;OptionsScreen6Init=80:100:40:250,"Open             Program        on Campaign Screen",zorder=100
.              listins Options6Coll,OptionsScreen6Init

.Screen       7
.              create          Options;OptionsScreen7Proj=80:100:40:250,"Allow Viewing of Projection Breakdown",zorder=100
.              listins Options7Coll,OptionsScreen7Proj
.Screen       8
.              create          Options;OptionsScreen8OrderSearch=80:100:40:250,"Include Order            File in        Searches",zorder=100
.              listins Options8Coll,OptionsScreen8OrderSearch

.Screen       9
              create          Options;OptionsScreen9View=80:100:40:250,"Disable Viewing of Screen 9",zorder=100
              listins         Options9Coll,OptionsScreen9View

.Screen       10
              create          Options;OptionsScreen10View=80:100:40:250,"Disable Viewing of Screen 10",zorder=100
              create          Options;OptionsScreen10Usage=100:120:40:250,"Automatically Calculate Usage/Orders",zorder=100
              listins         Options10Coll,OptionsScreen10View,OptionsScreen10Usage

               InvoiceListView.SetExtendedStyle giving N9 using 0,hexer
               InvListView001.SetExtendedStyle giving N9 using 0,hexer
               call           openpref
.....................................................................................................................................
               CREATE         TIMER,27000     ..45 minutes
               ACTIVATE       TIMER,Timeout,RESULT
               SETWTITLE      "Names in the News - Billing Repair "
               TRAP           IOmssg GIVING ERROR IF IO

               Call           InvButtonDefault
               formload       Abt
Main
               EVENTREG  X, 17, XRESIZE

               loop
               waitevent
               setitem        timer,0,18000   .reset to 30 minutes
               repeat
               goto           timeout

.end patch 3.0
           MOVE        "NINV0015" TO PROGRAM
           MOVE      "Names In The News Ca Inc" TO COMPNME
           MOVE        "FIX BAD INVOICE DATA" TO STITLE
           CALL        PAINT
           MOVE      C1 TO NINVPATH        SET ACCESS TO ISI BY LR#.
.
START    CALL      WIPEVAR
           KEYIN     *P1:3,*EF,"WHAT LR? ",*ZF,*JR,NINVFLD
           SCAN     "N" IN NINVFLD
           GOTO     START1 IF EQUAL
           MOVE     C1 TO NINVPATH
         MATCH    "00000*",NINVFLD
         GOTO      EOJ IF EQUAL
           CALL        NINVKEY
         GOTO      START IF OVER
           GOTO        START3
START1     CALL      WIPEVAR
           KEYIN     *P1:3,*EF,"WHAT INV?",*ZF,*JR,NINVFLD
           MOVE      C2 TO NINVPATH
         MATCH    "00000*",NINVFLD
         GOTO      EOJ IF EQUAL
           CALL        NINVKEY
         GOTO      START IF OVER
START3   DISPLAY   *P1:5,"(1) INVOICE NUMBER: ",INVNUM,"      Status (O)pen or (P)aid:",statb:
                   *P1:6,"(2) 1ST CHECK NUMBER: ",CHKN1:
                   *P1:7,"(3) 2ND CHECK NUMBER: ",CHKN2:
                   *P30:7,"(22) 3RD CHECK NUMBER: ",CHKN3:
                   *p1:8,"(12)mlrs check number:",imlrchk:
                   *p1:9,"(17) Quantity:",qtybild:
                   *p35:10,"(18) Mlr Number        ",mlrn:
                   *p1:10,"(19) Quantity in        ",QTYIN:
                   *P1:11,"(4) CHECK DATE: ",CHK1DTEM,CHK1DTED,chk1dtec,CHK1DTEY:
                   *P1:12,"(5) INVOICE DATE: ",INVDTEM,INVDTED,invdtec,INVDTEY:
                   *P1:13,"(6) RCV CHK DATE: ",MLRPAYD:
                   *P1:14,"(7) A/R AMOUNT: ",AR:
                   *P30:14,"(8) A/P 1     : ",AP1:
                   *P1:15,"(9) A/P 2     : ",AP2:
                   *P30:15,"(20) A/P 3     : ",AP3:
                   *P1:16,"(21) *LR     : ",Xninc:
                     *P1:17,"(10) LR NUMBER: ",LRN:
                     *p1:18,"(11) broker/cnt:",ibrknum,slash,ibrkcnt:
                     *p1:19,"(13) split/qty:",irexqty,"      Guar Letter": ",wsjpc:
                     *p1:20,"(14) split/$ppm:",iexppm:
                     *p1:21,"(15) net name %:   ",irnetper:
                     *p1:22,"(16) net run charge: ",inetrc:
                     *p1:23,"(23) MLRpay:   ",MlrpayR:
                     *p45:23,"(24) Credit Letter code: ",LET90D
         KEYIN     *P1:24,*EL,"IS THIS THE INVOICE YOU WANT? ",STR1;            ."
         CMATCH    YES,STR1
         GOTO      START IF NOT EQUAL
GETITEM  KEYIN     *P1:24,*EL,"WHAT ITEM NUMBER DO YOU WANT TO CHANGE? ",ITEM;
         COMPARE   "99",ITEM
         GOTO      UPDATE IF EQUAL
         BRANCH    ITEM OF T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,t11,t12:
                   t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24
         GOTO      GETITEM
T1
.          goto      getitem
          move      invnum to ninvfld
.         move      c2 to ninvpath
.         call      ninvkey
.         call      ninvdel
         KEYIN     *P21:5,*zf,*jr,INVNUM
         move      lrn to ninvfld
         move      c1 to ninvpath
         call      ninvupd
                DISPLAY   *P1:23,*EL,*B,"KEY ",*HON," CHANGED",*hoff,*B
         GOTO      GETITEM
T2       KEYIN     *P23:6,CHKN1
         GOTO      GETITEM
T3       KEYIN     *P23:7,CHKN2
         GOTO      GETITEM
T4       KEYIN     *p35:11,"format mmddccyy":
                       *P17:11,*-,*ZF,*JR,CHK1DTEM,CHK1DTED,chk1dtec,CHK1DTEY,*+
         GOTO      GETITEM
T5       KEYIN     *p35:12,"format mmddccyy":
                       *P19:12,*-,*ZF,*JR,INVDTEM,INVDTED,invdtec,INVDTEY,*+
         GOTO      GETITEM
T6       KEYIN     *P19:13,MLRPAYD
         GOTO      GETITEM
T7       KEYIN     *P17:14,AR
         GOTO      GETITEM
T8       KEYIN     *P47:14,AP1
         GOTO      GETITEM
T9       KEYIN     *P17:15,AP2
         GOTO      GETITEM
T20       KEYIN     *P47:15,AP3
         GOTO      GETITEM
T21       KEYIN     *P17:16,Xninc
         GOTO      GETITEM
.start patch 2.5
T22       KEYIN     *P30:7,chkn3
         GOTO      GETITEM
T23       KEYIN     *P30:23,Mlrpayr
         GOTO      GETITEM
T24       KEYIN     *P61:23,Let90D
         GOTO      GETITEM
         
         
.end patch 2.5         
T10        MOVE        LRN TO SAVELR
           REP         ZFILL IN SAVELR
           KEYIN     *P17:17,*ZF,*JR,LRN,*DV,B1,*DV,SAVELR
           MOVE      SAVELR TO NINVFLD
           MOVE     C1 TO NINVPATH
           CMATCH   B1 TO SAVELR
           GOTO     T10A IF EOS       .OLD LR WAS NULL
           MATCH    "000000" TO SAVELR
           GOTO     T10A IF EQUAL
           CALL     NINVTST
           FILEPI    1;NINVFILE
           DELETEK   NINVFILE,SAVELR
T10A       MOVE      C2 TO NINVPATH
           MOVE      INVNUM TO NINVFLD
           REP         ZFILL IN NINVFLD
           CALL      NINVTST
           GOTO      BUGGED IF OVER
           MOVE      C1 TO NINVPATH
           REP         ZFILL IN LRN
           DISPLAY   *P1:24,*EL,"NEW LR EQUALS ",LRN,*W5
           BRANCH    NINVFLAG TO T10B
           CALL      NINVOPEN
T10B       FILEPI    1;NINVFILE
           INSERT    NINVFILE,LRN
           MOVE      LRN TO NINVFLD
           CALL      NINVTST
           IF          NOT OVER
           DISPLAY   *P1:23,*EL,*B,"KEY CHANGED",*B
           ELSE
           DISPLAY   *P1:23,*EL,*B,"KEY ",*HON,"NOT",*HOFF," CHANGED",*B
           ENDIF
           GOTO      GETITEM
T11      KEYIN     *P17:18,*jr,*zf,ibrknum,*dv,slash,*jr,*zf,Ibrkcnt
         GOTO      GETITEM
T12      KEYIN     *P23:18,imlrchk
         GOTO      GETITEM
T13      KEYIN     *P23:19,*jr,irexqty
         GOTO      GETITEM
T14      KEYIN     *P23:20,*jr,iexppm
         GOTO      GETITEM
T15      KEYIN     *P23:21,*jr,irnetper
         GOTO      GETITEM
T16      KEYIN     *P23:22,*jr,*zf,inetrc
         GOTO      GETITEM
T17      KEYIN     *P23:09,*jr,qtybild
         GOTO      GETITEM
T18      KEYIN     *P 2:10,*jr,*zf,mlrn
         GOTO      GETITEM
T19      KEYIN     *P25:10,*jr,qtyin
         GOTO      GETITEM
UPDATE   CALL      NINVUPD
         GOTO      START
EOJ      STOP
WIPEVAR  CLEAR          CODE
         CLEAR                STATB
           CLEAR              MLRN
           CLEAR              LRN
           CLEAR          SAVELR
           RETURN
BUGGED   DISPLAY    *P1:24,*EL,*HON,*B,"B U G G E D !!!!!"
           STOP
.begin patch 3.0
.......................................................................................................
HelpGo
               setprop        AboutMssg,visible=1
               return

.......................................................................................................
FileGo
              branch          result to FileGo1,FileGo2,FileGo2
              return
.filego1 - report options for printing
FileGo1
              return
FileGo2
              winshow
.              goto            outofhere
              stop
.......................................................................................................
.timeout -- may need to add code to remove busy

Timeout
               stop
*==============================================================================
Optionsgo
               branch         result to SearchGo,OptionsGo1
OptionsGo1
              setprop         Options,visible=1
              return
OptionsWritePref
              return
OptionsTabClick
              return

OptionsTabChange
              return
...........................................................................................................
.Open Preferences File
openpref
               return
............................................................................................................
SearchGo
               branch         result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6
SearchGo1
.BROKER
              move            C1,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
               return
SearchGo2
.LIST
               move           C2,SrchFlag
               call           SearchSetTitle
               call           SearchSetVisible
               return
SearchGo3
.MAILER
              move            C3,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchGo4
.SHIP-TO
. not an option with this program
               return
SearchGo5
.CAMPAIGN
              move            C5,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return

.START PATCH 3.66 ADDED LOGIC
SearchGo6
.OWNER
              move            C6,SrchFlag
              call            SearchSetTitle
              call            SearchSetVisible
              return
SearchLoad
               branch         SrchFlag to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
SearchLoad1
.BROKER

               unpack         Srchstr,str4,str1,str3,str1,str45,str55
              setitem       InvEditBrk,0,str4
               setitem        InvStatText036,0,str45
              setfocus      InvEditBRk
               return
SearchLoad2
              unpack          Srchstr,str6
              setitem         InvEditText033,0,str6
              unpack          Srchstr,str6,str1,str35
              setitem         InvEditText034,0,str35
              setfocus       InvEditText034
               return
SearchLoad3
              unpack          Srchstr,str4
              setitem         InvEditMlr,0,str4
               setitem        InvStatText035,0,str45
               Setfocus       InvEditMlr
               return
SearchLoad4
.SHIP-TO - not an option with this program
               return
SearchLoad5
.Campaign - not an option with this program
               return
SearchLoad6
.OWNER
              setitem         InvEditOwner,0,str4
              setitem         InvStatText037,0,str25
              setfocus       InvEditOwner
               return
.end patch 10.0
*******************************************************************************
RestoreSearchButton
              Setprop InvoiceViewSearch,height=20
               Return
*******************************************************************************
InvoiceSetSearchKey
              setitem       InvEditTextLR,0,OLRN
              return
*******************************************************************************
InvoiceSetSearchDefault
.Position Search Form in Default Position
.               getprop        InvoiceSearch,top=T1,left=L1
              add             "52",L1
              setprop         SearchForm,winpos=1
              getprop         NINV0001,top=H,left=V
              add            T1,H,SerTop
              add            "50",SerTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
              add            L1,V,SerLeft
              setprop         SearchForm,top=SerTop,left=SerLeft                          .Default
              return
XRESIZE
           Ninv0015.Scale
           RETURN
.end patch 3.0
.........................................................................................................................
;begin patch 2.4
;          INCLUDE    NINVIO.inc
           INCLUDE      ninvio.inc
;end patch 2.4
           INCLUDE   COMLOGIC.inc

