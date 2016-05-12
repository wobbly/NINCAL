         INclude   common.inc
         include   cons.inc
         include   nwfdd.inc
         include   nwfXdd.inc
         include   statsdd.inc
         include   norddd.inc
        include     SXrfdd.inc
         include   ndatdd.inc
          include  hp.inc
.START PATCH 2.3.1 ADDED LOGIC
          include   COMPDD.INC
          include   CNTDD.INC
.END PATCH 2.3.1 ADDED LOGIC
release   init      "2.3.1"         ASH 27JAN2005 Stats file Conversion
.release   init      "2.3"         ASH  09AUG2004 Logo Conversion
.release   init      "2.2"         23Mar04 JD If no lr or over, do listname lookup and apply.
;release   init      "2.01"       12Jul02 DLH use GetWinVer
;release   init      "2.00"       12Jul02 Updated to apply from 1 master csv file.
;release   init      "1.76"       08Apr02 DLH change to statxfld1
;release   init      "1.75"       03apr02 updated maildate check.
;elease   init      "1.74"      30JAN02 turned off reindex master file.
;release   init      "1.73"      19NOV01 STATSFILE CONVERSION
;release   init      "1.72"      03Aug2001 use actual resp/revenue instead of projections
;release   init      "1.7"      05July2001 DLH new indices see statsdd
;release   init      "1.6"      04OCT2000 ASH NEW SERVER ADDED
;release   init      "1.5"      17Apr00 JD added lcstpm/statpckm = statimcst
;release   init      "1.4"        29Jun99 DLH add code for copy (etc) with NT machines
;                                added lifetime value filed
;release  init      "1.3"      12Apr99 DLH source expanded from 6 to 10
;Release   init     "1.2"      09APR99 JD TURNED OFF NUMERIC CHECK OF STATESRC.
;Release   init     "1.1"     01Jun98 DLH write stats bat when needed.
;Release   init     "1.02"    24oct97 DLH/JD KAFAKEIETHEKTHAETHTEH
;Release   init     "1.01"   26Aug97 DLH primary key revision
;Release  init      "1.00"   10Jul97 DLH
;Release init       "prexxx" 09jul97 add code to handle new 6/97 contribs
;                            remove nwf reads and writes, use uniform 
;                            ninstats.dat as master file
;release  init      "not3"   remove code to handle pre dec96 input data handle in excel macro
;                            start change to master stat file.
;release  init      "not2"   add code to handle pre dec96 input data
;release  init      "not1"   add code for retest 15may97 DLH
;release  init      "not"
;....................for MWF file post November 1996 format
;....................Files from Nov and Prior campaigns need to be processed
;....................By statold1.dbc until NWf updates their formats.
;********************Now handled by Excel Macros and Data Junction
except   file
batch    file                 .created for temp use
RECSIN   FORM      6
STR256   DIM       300
str60    dim       60
str60sav dim       60
;str13    dim       13
DECIMAL  FORM      8.6
PERCENT  INIT      "%"
cntrbflg FORM      "0"
input    file
TIME     INIT      "HH:MM:SS"
offer    dim         3
minusflg dim        1
mqty     dim       8
dim30    dim       30
mqtyb    dim       10
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
lstcpmf  form       4.2
mailcstf form       6.2
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
mdater    form       5
ldater    form       5
weeksout  dim        5
mailing   init       "FY"
xdes      dim        30
xsel      dim        30
;copycmd  init      "!c:\command.com /C copy g:\data\except.lst \\NINs2\Laser3 "
;START PATCH 1.6 REPLACED LOGIC
;copycmd  init      "!c:\command.com /C copy g:\data\except.lst lpt1: "
copycmd  DIM         50
         PACK        copycmd,"!c:\command.com /C copy ",NTWKPATH1,"except.lst \\NINs2\Laser2 "
;END PATCH 1.6 REPLACED LOGIC
;
         open      input,"c:\work\nwf.csv",exclusive
        CLOCK       DATE TO TODAY
        CLOCK     TIME TO TIME
;START PATCH 1.6 REPLACED LOGIC
;        open        except,"g:\data\stats\nwf\excepts.nwf"
        PACK        STR35,NTWKPATH1,"stats\nwf\excepts.nwf"
        open        except,STR35
;END PATCH 1.6 REPLACED LOGIC
        read        except,seqeof;str1
        move       "stat0001" to program
        move       "Names in the News" to compnme
        call       paint
;START PATCH 1.6 REPLACED LOGIC
;        splopen     "g:\data\except.lst"
        PACK        STR35,NTWKPATH1,"EXCEPT.LST"
        splopen     STR35
;END PATCH 1.6 REPLACED LOGIC
        print       hpport,*rptchar "*":80:
                    *n,*n,*n,hpbon:
                    *n,*1,"Program    : ",program,b2,"Date: ",today:
                    b2,"Time: ",time:
                    *n,*1,"Company    : ",compnme:
                    *n,*1,"User       : ":
                    *n,*1,"Deliver To : Henry",hpboff:
                    *n,*rptchar "*":80
        move       c1 to statpath
;looper  read       input,seq;str256
;START PATCH 1.73 REPLACED LOGIC
;looper  read       input,seq;*cdfon,str60:
;                     statldes:
;                     statSEL:
;                     statsrce:
;                     mqty:
;                     resp:
;                     str6:
;                      str6:
;                     str6:
;                     str6:
;                     revenue:
;                     str6:
;                     str6:
;                     lstcpm:
;                     tlst$:
;                     str6:
;                     str6:
;                     str6:
;                     str6
;        goto       eoj if over
looper  read       input,seq;*cdfon,str60:
                     statldes:
                     statSEL:
                     str6:
                     str14:
                     mqty:
                     resp:
                     str6:
                      str6:
                     str6:
                     str6:
                     revenue:
                     str6:
                     str6:
                     lstcpm:
                     tlst$:
                     str6:
                     str6:
                     str6:
                     str6
        goto       eoj if over
               if             (str14 = "AAZLAAAH")
.               call           debug
               endif 
                                                  clear    xdes
                                                  clear    xsel
                                                  move     statsel to xsel
                                                  move     statldes to xdes
                                                  move         str14,STATSRCE
;END PATCH 1.73 REPLACED LOGIC
        SCAN       "Source Key" IN STATSRCE
        goto       looper if equal
        reset      statsrce
        display    *p10:10,"records in ",recsin;
        add        c1 to recsin
;................................................................
        reset      str60
        match       str60 to Mailing
        if          equal
        reset       str60
        move        str60 to str60sav
        goto        looper
        endif
;................................................................        
;Mail date
;        compare    c2 to recsin
;        if         equal
        scan       "Mail Date" in str60
        if         equal
Maild   scan       slash in statldes
        goto       looper if not equal      .not a valid format, get out
;        bump       statsel by -30
        reset      statldes
        move       statldes to mm
       bump       statldes by 3
        move       statldes to dd
        type       dd
        if         not equal
        move       dd to str1
        pack       dd from b1,str1
        endif
        scan       slash in statldes
        bump       statldes by 1
;        match      "00" to statsel
;          if         not equal
;        pack       str4 from "19",statsel
;        else
;        pack       str4 from "20",statsel
;          endif
        match      "00" to statldes
        if         equal
        move       "00" to yy
        pack       str4 from "20",statldes
        else
        MATCH      "01" TO STATldes
        IF         EQUAL
        move       "01",yy
        pack       str4 from "20",statldes
        ELSE
        MATCH      "02" TO STATldes
        IF         EQUAL
        move       "02",yy
        pack       str4 from "20",statldes
        else
        MATCH      "03" TO STATldes
        IF         EQUAL
        move       "03",yy
        pack       str4 from "20",statldes
                      else
        MATCH      "04" TO STATldes
        IF         EQUAL
        move       "04",yy
        pack       str4 from "20",statldes
        else
        pack       str4 from "19",statldes
          endif
          endif
          ENDIF
          endif
                               endif
        call       cvtjul
        move       juldays to mdater
        pack       statmdate from mm,dd,str4
        move       c0 to n4
        goto       looper
        endif
;................................................................        
;last update date
        scan       "Last Update" in str60
;        compare    c3 to recsin
        if         equal
        scan       slash in statldes
        reset      statldes
        move       statldes to mm
       bump       statldes by 3
        move       statldes to dd
        type       dd
        if         not equal
        move       dd to str1
        pack       dd from b1,str1
        endif
        scan       slash in statldes
        bump       statldes by 1
;        match      "00" to statldes
;          if         not equal
;        pack       str4 from "19",statldes
;        else
;        pack       str4 from "20",statldes
;          endif
        match      "00" to statldes
        if         equal
        move       "00",yy
        pack       str4 from "20",statldes
        else
        MATCH      "01" TO statldes
        IF         EQUAL
        move       "01",yy
        pack       str4 from "20",statldes
        ELSE
        MATCH      "02" TO statldes
        IF         EQUAL
        move       "02",yy
        pack       str4 from "20",statldes
        else
        MATCH      "03" TO STATldes
        IF         EQUAL
        move       "03",yy
        pack       str4 from "20",statldes
                      else
        MATCH      "04" TO STATldes
        IF         EQUAL
        move       "04",yy
        pack       str4 from "20",statldes
        else
        pack       str4 from "19",statldes
          endif
          endif
          ENDIF
          endif
                               endif
;        move       statldes to str4
        call       cvtjul
        move       juldays to ldater
        pack       nwfudate from mm,dd,str4
        move       nwfudate to statpdate
        move       c0 to n4
        sub        mdater from ldater
        move       ldater to n6
        div        c7 into  n6
        move       n6 to statwkso
        goto       looper
        endif
;................................................................        
;weeks out
;        compare    c4 to recsin
        scan       "Weeks Out" in str60
        if         equal
;dlh 24feb00
;        call       trim using statsel
;        reset      statsel
;        bump       statsel by 26
;        move       statsel to str6
;        MOVE       C0 TO N6
;        MOVE        TO N6
;        add        N6 to statwkso
        display    *p10:11,"weeks",n6,b1,statwkso
        goto       looper
        endif
;................................................................        
;mail processed thru.
;
        scan       "Mail Processed" in str60
;        compare    c5 to recsin
        if         equal
;        scan       slash in statsel
;        reset      statsel
;        move       statsel to mm
;       bump       statsel by 3
;        move       statsel to dd
;        type       dd
;        if         not equal
;        move       dd to str1
;        pack       dd from b1,str1
;        endif
;        scan       slash in statsel
;        bump       statsel by 1
;        match      "00" to statsel
;        if         equal
;        pack       str4 from "20",statsel
;        else
;        MATCH      "01" TO STATSEL
;        IF         EQUAL
;        pack       str4 from "20",statsel
;        ELSE
;        pack       str4 from "19",statsel
;          endif
;          ENDIF
;.        move       statsel to str4
;        pack       statpdate from mm,dd,str4
        goto       looper
        endif
;................................................................        
         scan        "TOTALS" in statldes
         goto        looper if equal
         reset       statldes
         REP         LOWUP IN STR60
         scan        "PANEL" in str60
         if          equal
         scan        "SUB-TOTALS" in statldes
         goto        looper if equal
         reset        str60
         scan        "SUB-" in str60
         goto        looper if equal
         endif
         reset       str60
;

;         branch      cntrbflg of nwfsela,nwfselb
;         
chkpanel
         scan        "PANEL" in str60
         if          equal
         move        str60 to statpanel
;..........................................................................
;Default to subs as NWF keeps changing format     05mar98 DLH        
         move        c1 to cntrbflg
         move        "016" to offer
;.............................................................................
         reset       str60
         scan        "HOLIDAY CARD" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;         move        str60 to statpanel
;
         reset       statpanel
         endif

         reset       str60
         scan        "CARD FOLLOW" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;         move        str60 to statpanel
;
         reset       statpanel
         endif

         reset       str60
         scan        "WRAPPING PAPER" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;         move        str60 to statpanel
;
         reset       statpanel
         endif

         reset       str60
         scan        "SPRING 5" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;         move        str60 to statpanel
;
         reset       statpanel
         endif

         reset       str60
         scan        "CARDS AND SEED" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;         move        str60 to statpanel
;
         reset       statpanel
         endif
;
         reset       str60
         scan        "CARD" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
         reset       statpanel
         endif
;
         reset       str60
         scan        "SEED" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;
         reset       statpanel
         endif

         reset       str60
         scan        "CONTRI" in str60
         if          equal
         move        c2 to cntrbflg
         move        "014" to offer
;         move        str60 to statpanel

         reset       statpanel
         endif


         reset       str60
         Scan        "Subscib" in str60     ;looks like a typo to me
         if          equal
         move        c1 to cntrbflg
         move        "016" to offer
         move        str60 to statpanel
         reset       statpanel
         endif
;
        reset       str60
;        endif
         Scan        "MEMBER" in str60
         if          equal
         move        c1 to cntrbflg
         move        "015" to offer
         reset       statpanel
         endif
;10/9/2003 DLH don't know why this is here   
;         goto        looper
         endif

;         reset       str60
;         scan        "PANEL" in str60
;         if          equal
;         move        str60 to statpanel
;         scan        "WRAPPING" in statpanel
;         if          equal
;         move        c2 to cntrbflg
;         move        "014" to offer
;         reset       statpanel
;         endif
;         reset       str60
;         scan        "PANEL" in str60
;         if          equal
;         move        str60 to statpanel
;         scan        "NOTE" in statpanel
;         if          equal
;         move        c2 to cntrbflg
;         move        "014" to offer
;         reset       statpanel
;         endif
         reset       statpanel
         Scan        "SUBSCRI" in statpanel
         if          equal
         move        c1 to cntrbflg
         move        "016" to offer
         reset       statpanel
         endif
         reset       statpanel
         Scan        "MEMBER" in statpanel
         if          equal
         move        c1 to cntrbflg
         move        "015" to offer
         reset       statpanel
         endif       
;
         reset       statpanel
         scan        "RETEST" in statpanel
         if          equal
         compare     c2 to cntrbflg
         if          not equal
         move        c1 to cntrbflg
         move        "016" to offer
         reset       statpanel
         endif
; .        endif
;10/9/2003 - desperate move - why are we going to looper at this point? DH
;         goto        looper
         endif
PANELANS
         branch      cntrbflg of nwfsela,nwfselb
;fell thru GREAT!!!!!   set as subs
         move       c1 to cntrbflg
         move       "016" to offer
         goto       nwfsela        
;.................................................................
;mwfselb - contributors         
nwfselb
         display     *p1:23,*el,"Processing contributors"
         reset       str60
;begin patch 1.3
;         unpack      str256 into statsrce:      1-6 
;                     str12:                    7-18
;                     str12:                   19-30  ... 
;                     str12:                   31-42  ...
;                     str12:                   43-54  ...
;                     str6:                    57-60  dh added
;                     statldes:                 61-90
;                     statSEL:                  91-120
;                     str4:                   121-124
;                     type:                   125-125 .note change 27mar97DLH
;                     str2:                   126-127
;                     statmqty:               128-135     8
;                     str3:                   136-138
;                     statresp:               138-145       7
;                     str1:                   146-146
;                     statrev:                147-155       9
;                     str3:                   156-158
;                     statlcpm:               159-165       4.2
;                     str1:                   166-166
;                     statImcst:              167-175      6.2
;                     Weeksout                176-180      5.0
;end patch 1.3

;        move         c0 to n5
;        rep          zfill in weeksout
;        move         weeksout to n5
;        compare      n5 to c0
;        if           not equal
;        move         n5 to statwkso
;        endif

******************************************************************************
;        clear        mqty
;        append       mqtyb to mqty
;        reset        mqty
;        goto         b1
;
;
;        move       c0 to statresp
;        move       c0 to statlcpm
;        move       c0 to statImcst
;        move       c0 to statrev
;begin release .001
;        move       c0 to statmqty
;        call       trim using mqty
;        move       mqty to statmqty
;
;        move       c0 to statresp
;        call       trim using resp
;        move       resp to statresp
;
;        move       c0 to statlcpm
;        call       trim using lstcpm
;        move       lstcpm to statlcpm

;        move       c0 to statrev
;        call       trim using revenue
;        move       revenue to statrev
;
;        move       c0 to statImcst
;        call       trim using tlst$
;        move       tlst$ to statImcst
         goto       b1
;nwfsela - subscribers/membership
nwfsela
         display     *p1:23,*el,"Processing Subscribers"
;         display     *p1:24,*el,"Doing Subs",*b
         reset       str256
;begin patch 1,3
;         unpack      str256 into statsrce:      1-6 
;                     str12:                    7-18
;                     str12:                   19-30  ... 
;                     str12:                   31-42  ...
;                     str12:                   43-54  ...
;                     statkycd:                61-66    6 bytes
;                     str4:                    67-70
;                     statldes:                 71-100
;                     statSEL:                 101-130
;                     type:                   131-131 .note change 27mar97DLH
;                     str6:                   132-137
;                     statmqty:               138-145     8
;                     str3:                   146-148
;                     statresp:               149-155       7
;                     str1:                   156
;                     statrev:                157-165       9
;                     str3:                   166-168
;                     statlcpm:               169-175       4.2
;                     str1:                   176-176
;                     statImcst               177-185      6.2
;         unpack      str256 into statsrce:      1-10
;                     str8:                   11-22
;                     str12:                   23-34  ...
;                     str12:                   35-46  ...
;                     str12:                   47-58  ...
;                     statkycd:
;                     statldes:
;                     statSEL:
;                     type:
;                     str6:
;                     statmqty:
;                     str3:
;                     statresp:
;                     str1:
;                     statrev:
;                     str3:
;                     statlcpm:
;                     str1:
;                     statImcst
;
;        move       c0 to statresp
;        move       c0 to statlcpm
;        move       c0 to statImcst
;        move       c0 to statrev
;begin release .001
;        move       c0 to statmqty
;        call       trim using mqty
;        move       mqty to statmqty
;
;        move       c0 to statresp
;        call       trim using resp
;        move       resp to statresp
;
;        move       c0 to statlcpm
;        call       trim using lstcpm
;        move       lstcpm to statlcpm

;        move       c0 to statrev
;        call       trim using revenue
;        move       revenue to statrev
;
;        move       c0 to statImcst
;        call       trim using tlst$
;        move       tlst$ to statImcst
;
        match      "                              " to statldes
        goto       looper if equal
        goto       looper if eos
;        match      "Name" to statldes
;        goto       looper if equal
        match      "Ninca ##" to str14
        goto       looper if equal
;
        rep        "$ " in revenue
        rep        "$ " in lstcpm
        rep        "$ " in tlst$
        rep        "( " in revenue
        rep        "( " in lstcpm
        rep        "( " in tlst$
        rep        ")-" in revenue
        rep        ")-" in lstcpm
        rep        ")-" in tlst$
        rep        "% " in revenue
        rep        "% " in lstcpm
        rep        "% " in tlst$
        call       trim using mqty
        call       trim using resp
        call       trim using revenue
        call       trim using lstcpm
        call       trim using tlst$
        move       "," to str1
        call       removechar using mqty,str1
        call       removechar using revenue,str1
        call       removechar using resp,str1
        call       removechar using tlst$,str1
;
         call      trim using totcst13
         move      totcst13 to totcst
         call      trim using netrev8
         move      netrev8 to netrev
;                  
         move        csta9 to csta
         move        nrpcia to nrpci
         move        nrncia to nrnci
         move        str12 to cta
;        clear        mqty
;        append       mqtyb to mqty
;        reset        mqty
b1
;
;break out assoc response rate
        move        c0 to nwfasrr
        scan        period in assocrresp
        bump        assocrresp by 1
        move        assocrresp to decimal
        mult        ".000001" by decimal
        add         decimal to nwfasrr
;
;break out tota list cost
        move        c0 to nwftlst$
        scan        period in TOTLCOST
        bump        TOTLCOST by 1
        move        TOTLCOST to decimal
        mult        ".01" by decimal
        add         decimal to nwftlst$
        bump        totlcost by -2
        lenset      totlcost
        reset       totlcost
        move        c0 to decimal
        move        totlcost to decimal
        add         decimal to nwftlst$
;
;
;break out basic response rate
        move        c0 to nwfbasrr
        scan        period in brresp
        bump        brresp by 1
        move        brresp to decimal
        mult        ".000001" by decimal
        add         decimal to nwfbasrr
;break out number of assoc responses
        move        c0 to nwfasresp
respa   clear       str1
        append      assocresp to str1
        type        str1
        if          not equal
        goto        respax
        else
        bump        assocresp by 1
        goto        respax if eos
        goto        respa
        endif
respax  bump        assocresp by -1
        lenset      assocresp
        reset       assocresp
        move        assocresp to nwfasresp
;
;break out number of basic responses
        move        c0 to nwfbasresp
respb   clear       str1
        append      basresp to str1
        type        str1
        if          not equal
        goto        respbx
        else
        bump        basresp by 1
        goto        respbx if eos
        goto        respb
        endif
respbx  bump        basresp by -1
        lenset      basresp
        reset       basresp
        move        basresp to nwfbasresp
;
;assoc revenue
reva    clear       str1
        append        assocrev to str1
        type        str1
        if          not equal
        goto        revax
        else
        bump        assocrev by 1
        goto        revax if eos
        goto        reva
        endif
revaX    bump        assocrev by -1
        lenset      assocrev
        reset       assocrev
        move        assocrev to nwfasrev
;
revb     clear       str1
        append       brev to str1
        type        str1
        if          not equal
        goto        revbx
        else
        bump        brev by 1
        goto        revbx if eos
        goto        revb
        endif
revbX    bump        brev by -1
        lenset      brev
        reset       brev
        move        brev to nwfbasrev

;         type         statsrce
;         if          not equal
;         display     *p1:24,*el,"reject nonNumeric Source ",statsrce
;         goto         looper
;         endif
         
         scan        "Select" in nwfsel
         goto        looper if equal
         reset       nwfsel
;break out response rate
        move        c0 to nwfrr
        scan        period in rresp
        bump        rresp by 1
        move        rresp to decimal
        mult        ".000001" by decimal
        add         decimal to nwfrr
;break out P C I
        move        c0 to nwfpci
        scan        period in pci
        bump        pci by 1
        rep         zfill in pci
        move        pci to decimal
        mult        ".000001" by decimal
        add         decimal to nwfpci
;break out N C I
        move        c0 to nwfNci
        scan        period in Nci
        bump        Nci by 1
        rep         zfill in nci
        move        Nci to decimal
        mult        ".000001" by decimal
        add         decimal to nwfNci
;list type
        clear       stattype
        append      type to stattype
        reset       stattype
;        
;break out mail qty
;        move        c0 to nwfmqty
mqty
;        clear       str1
;        append      mqty to str1
;        type        str1
;        if          not equal
;        goto        mqtyx
;        else
;        bump        mqty by 1
;        goto        mqtyx if eos
;        goto        mqty
;        endif
;mqtyx   bump        mqty by -1
;        lenset      mqty
;        reset       mqty
        move        mqty to statmqty
;        display     *p1:24,*el,mqty,b1,nwfmqty;
;break out number of responses
        move        c0 to nwfresp
resp    clear       str1
        display     *p1:23,*el,resp
;        append      resp to str1
;        type        str1
;        if          not equal
;        goto        respx
;        else
;        bump        resp by 1
;        goto        respx if eos
;        goto        resp
;        endif
;respx   bump        resp by -1
;        lenset      resp
;        reset       resp
        move        resp to statresp
;
rev
        move        revenue to statrev

;break out aver gift
avergft move        c0 to nwfavgft
        scan        period in gift
        bump        gift by 1
        move        c0 to decimal
        clear       str2
        move        gift to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfavgft
        reset       gift
        scan        period in gift
        bump        gift by -1
        lenset      gift
        reset       gift
        move        c0 to decimal
        move        gift to decimal
        add         decimal to nwfavgft
;break out N C I
        move        c0 to nwfNci
        scan        period in Nci
        bump        Nci by 1
        rep         zfill in nci
        move        Nci to decimal
        mult        ".000001" by decimal
        add         decimal to nwfNci
;
        move        c0 to decimal
;        display     *p1:23,*el,z96
        move        c0 to nwf96z
        scan        period in z96
        bump        z96 by 1
        rep         zfill in z96
        clear       str2
        move        z96 to str2
        move        str2 to decimal
;        display     *p1:23,*el,z96,decimal
        mult        ".01" by decimal
        add         decimal to nwf96z
        reset       z96
        scan        period in z96
        bump        z96 by -1
        lenset      z96
        reset       z96
        move        c0 to n8
        move        z96 to n8
        add         n8 to nwf96z
;        display     *p1:24,*el,z96,b1,nwf96z;
;
;break out list cost per m
        move        c0 to nwflstcpm
;        scan        period in lstcpm
;        bump        lstcpm by 1
;        move        c0 to decimal
;        clear       str2
;        move        lstcpm to str2
;        move        str2 to decimal
;        mult        ".01" by decimal
;        add         decimal to nwflstcpm
;        reset       lstcpm
;        scan        period in lstcpm
;        bump        lstcpm by -1
;        lenset      lstcpm
;        reset       lstcpm
;        move        c0 to decimal
        move        lstcpm to statlcpm
;        add         decimal to nwflstcpm
;
;break out total list $
        move        c0 to nwftlst$
;        scan        period in tlst$
;        bump        tlst$ by 1
;        move        c0 to decimal
;        clear       str2
;        move        tlst$ to str2
;        move        str2 to decimal
;        mult        ".01" by decimal
;        add         decimal to nwftlst$
;        reset       tlst$
;        scan        period in tlst$
;        bump        tlst$ by -1
;        lenset      tlst$
;        reset       tlst$
;        move        c0 to decimal
;        move        tlst$ to statImcst             updated 4/17/00. jd
         move        c0 to lstcpmf
         move        c0 to mailcstf
         move        statlcpm to lstcpmf
         move        tlst$ to mailcstf
;
         add         lstcpmf to mailcstf
         move        mailcstf to statimcst
;        add         decimal to nwftlst$
;
;break out cost per m
        move        c0 to nwfpckcpm
        scan        period in pckcpm
        bump        pckcpm,1
        move        c0 to decimal
        clear       str2
        move        pckcpm to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfpckcpm
        reset       pckcpm
        scan        period in pckcpm
        bump        pckcpm,-1
        lenset      pckcpm
        reset       pckcpm
        move        c0 to decimal
        move        pckcpm to decimal
        add         decimal to nwfpckcpm
        move        c0 to statpCKM
        move        tlst$ to statpckm             updated 4/17/00. jd
;        add         nwftpck to statpckm
;
;break out total package cost 
        move        c0 to nwftpck
        scan        period in tpck
        bump        tpck by 1
        move        c0 to decimal
        clear       str2
        move        tpck to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwftpck
        reset       tpck
        scan        period in tpck
        bump        tpck by -1
        lenset      tpck
        reset       tpck
        move        c0 to decimal
        move        tpck to decimal
        add         decimal to nwftpck
        move        c0 to statpack
        add         nwftpck to statpack
;
;break out tota cost per member
;        display     *p1:23,*el,totcpm
        move        c0 to nwftcpm
        scan        period in totcpm
        bump        totcpm by 1
        move        c0 to decimal
        clear       str2
        move        totcpm to str2
        move        str2 to decimal
        mult        ".01" by decimal
;        display     *p1:24,*el,totcpm,b1,str2,b1,decimal,*w2
        add         decimal to nwftcpm
        reset       totcpm
        scan        period in totcpm
        bump        totcpm by -1
        lenset      totcpm
        reset       totcpm
        move        c0 to decimal
        move        totcpm to decimal
        add         decimal to nwftcpm
;break out total mail cost
        move        c0 to nwftmcst
        scan        period in mailcost
        bump        mailcost by 1
        move        c0 to decimal
        clear       str2
        move        mailcost to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwftmcst
        reset       mailcost
        scan        period in mailcost
        bump        mailcost by -1
        lenset      mailcost
        reset       mailcost
        move        c0 to decimal
        move        mailcost to decimal
        add         decimal to nwftmcst
;
;break out unit premium cost 
        move        c0 to nwfupcst
        scan        period in unitpc
        bump        unitpc by 1
        move        c0 to decimal
        clear       str2
        move        unitpc to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfupcst
        reset       unitpc
        scan        period in unitpc
        bump        unitpc by -1
        lenset      unitpc
        reset       unitpc
        move        c0 to decimal
        move        unitpc to decimal
        add         decimal to nwfupcst
;
;break out total premium cost 
        move        c0 to nwftpcst
        scan        period in totpc
        bump        totpc by 1
        move        c0 to decimal
        clear       str2
        move        totpc to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwftpcst
        reset       totpc
        scan        period in totpc
        bump        totpc by -1
        lenset      totpc
        reset       totpc
        move        c0 to decimal
        move        totpc to decimal
        add         decimal to nwftpcst
;
        move        c0 to nwftotcst
        scan        period in totcst
        move        c0 to decimal
        clear       str2
        bump        totcst by 1
        move      totcst to str2
          move      str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwftotcst
        reset       totcst
        scan        period in totcst
        lenset      totcst
        reset       totcst
        move        c0 to n8
        move        totcst to n8
        add         n8 to nwftotcst
;break out net revenue
        move        c0 to nwfnetrev
        scan        dash in netrev
        if          equal
        rep         "- " in netrev
        move        true to minusflg
        else
        move        false to minusflg 
        endif
        reset       netrev
        scan        period in netrev
        bump        netrev by 1
        move        c0 to decimal
        clear       str2
        move        netrev to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfnetrev
        reset       netrev
        scan        period in netrev
        bump        netrev by -1
        lenset      netrev
        reset       netrev
        move        c0 to decimal
        move        netrev to decimal
        add         decimal to nwfnetrev
        cmatch      true to minusflg
        if          equal
        mult        seq by nwfnetrev
        endif
;
;break out net revenue Pos CI
        move        c0 to nwfnrpci
        scan        dash in nrpci
        if          equal
        rep         "- " in nrpci
        move        true to minusflg
        else
        move        false to minusflg 
        endif
        reset       nrpci
        scan        period in nrpci
        bump        nrpci by 1
        move        c0 to decimal
        clear       str2
        move        nrpci to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfnrpci
        reset       nrpci
        scan        period in nrpci
        bump        nrpci by -1
        lenset      nrpci
        reset       nrpci
        move        c0 to decimal
        move        nrpci to decimal
        add         decimal to nwfnrpci
        cmatch      true to minusflg
        if          equal
        mult        seq by nwfnrpci
        endif
;
;break out net revenue Neg CI
        move        c0 to nwfnrNci
        scan        dash in nrnci
        if          equal
        rep         "- " in nrnci
        move        true to minusflg
        else
        move        false to minusflg 
        endif
        reset       nrnci
        scan        period in nrNci
        bump        nrNci by 1
        move        c0 to decimal
        clear       str2
        move        nrNci to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfnrNci
        reset       nrNci
        scan        period in nrNci
        bump        nrNci by -1
        lenset      nrNci
        reset       nrNci
        move        c0 to decimal
        move        nrNci to decimal
        add         decimal to nwfnrNci
        cmatch      true to minusflg
        if          equal
        mult        seq by nwfnrNci
        endif
;
;break out cost to aqcuire
        move        c0 to nwfCSTA
        scan        dash in csta
        if          equal
        rep         "- " in csta
        move        true to minusflg
        else
        move        false to minusflg 
        endif
        reset       csta
        scan        period in CSTA
        bump        CSTA by 1
        move        c0 to decimal
        clear       str2
        move        CSTA to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfCSTA
        reset       CSTA
        scan        period in CSTA
        bump        CSTA by -1
        lenset      CSTA
        bump        csta by -2
;        reset       CSTA
;cstaloop cmatch     b1 to csta
;         if         equal
;         bump       csta,1
;         goto       cstaloop
;         endif        
        move        c0 to decimal
        move        CSTA to decimal
        add         decimal to nwfCSTA
        cmatch      true to minusflg
        if          equal
        mult        seq by nwfcsta
;        display     *p1:24,*el,csta,b1,nwfcsta,*w;
        endif
;
;break out Pos CI CA
        move        c0 to nwfPCACI
        scan        dash in pcaci
        if          equal
        rep         "- " in pcaci
        move        true to minusflg
        else
        move        false to minusflg 
        endif
        reset       pcaci
        scan        period in PCACI
        bump        PCACI by 1
        move        c0 to decimal
        clear       str2
        move        PCACI to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfPCACI
        reset       PCACI
        scan        period in PCACI
        bump        PCACI by -1
        lenset      PCACI
        reset       PCACI
        move        c0 to decimal
        move        PCACI to decimal
        add         decimal to nwfPCACI
        cmatch      true to minusflg
        if          equal
        mult        seq by nwfPCACI
        endif
;break out Neg CI CA
        move        c0 to nwfNCACI
        scan        dash in ncaci
        if          equal
        rep         "- " in ncaci
        move        true to minusflg
        else
        move        false to minusflg 
        endif
        reset       ncaci
        scan        period in NCACI
        bump        NCACI by 1
        move        c0 to decimal
        clear       str2
        move        NCACI to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfNCACI
        reset       NCACI
        scan        period in NCACI
        bump        NCACI by -1
        lenset      NCACI
        reset       NCACI
        move        c0 to decimal
        move        NCACI to decimal
        add         decimal to nwfNCACI
        cmatch      true to minusflg
        if          equal
        mult        seq by nwfNCACI
        endif
;
;break out CTA index 
        move        c0 to nwfcta
        scan        period in cta
        bump        cta by 1
        move        c0 to decimal
        clear       str2
        move        cta to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfcta
        reset       cta
        scan        period in cta
        bump        cta by -1
        lenset      cta
        reset       cta
        move        c0 to decimal
        move        cta to decimal
        add         Decimal to nwfcta
;
;break out Cost per $
        move        c0 to nwfcost$
        scan        period in cost$
        bump        cost$ by 1
        move        c0 to decimal
        clear       str2
        move        cost$ to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfCost$
        reset       cost$
        scan        period in cost$
        bump        cost$ by -1
        lenset      cost$
        reset       cost$
        move        c0 to decimal
        move        cost$ to decimal
        add         decimal to nwfcost$
;
;break out net qty
        move        c0 to nwfnqty
nqty    clear       str1
        append      nqty to str1
        type        str1
        if          not equal
        goto        nqtyx
        else
        bump        nqty by 1
        goto        nqtyx if eos
        goto        nqty
        endif
nqtyx   bump        nqty by -1
        lenset      nqty
        reset       nqty
        move        nqty to nwfnqty
;break out inv$
        move        c0 to nwfinv
        scan        period in inv
        bump        inv by 1
        move        c0 to decimal
        clear       str2
        move        inv to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwfinv
        reset       inv
        scan        period in inv
        bump        inv by -1
        lenset      inv
        reset       inv
        move        c0 to decimal
        move        inv to decimal
        add         decimal to nwfinv
;
;break out list cost per m
        move        c0 to nwflcpm
        scan        period in lcpm
        bump        lcpm by 1
        move        c0 to decimal
        clear       str2
        move        lcpm to str2
        move        str2 to decimal
        mult        ".01" by decimal
        add         decimal to nwflcpm
        reset       lcpm
        scan        period in lcpm
        bump        lcpm by -1
        lenset      lcpm
        reset       lcpm
        move        c0 to decimal
        move        lcpm to decimal
        add         decimal to nwflcpm
;        move        statldes to nwfxfld
;        rep         lowup in nwfxfld
;         clear       nwfxlist
;         clear       ninlist
statred
;        move        statldes to statxfld1
;        rep         lowup in statxfld1
         clear       statxlist
         clear       ninlist
;
.START PATCH 2.3.1 REPLACED LOGIC
.         move        "0170" to statmlr
         move        "000913" to statmlr
.END PATCH 2.3.1 REPLACED LOGIC
;   for now anyway
;
;         match       "0170" to statmlr
;         if          equal
;          display    *p1:23,*el,nwfxfld
;         call        nwfxkey
;dh testing
;         clear       statxfld1
;begin patch 1.76
;         packkey       statxfld1 from STATMLR,statldes
;         packkey       statxfld1 from statldes
;end patch 1.76
;
;         move        c2 to statxpath
;         call        statxkey
;         goto        except if over
;         goto        CHKUPP if over
;         GOTO        DISXLST
;CHKUPP
;          clear       STATXFLD1
;         clear       statxlist
;         clear       ninlist
;         move        statldes to str30
;         rep         lowup in str30
;         clear       statxfld1
;begin patch 1.76
;               PackKey        StatxFld1 from Statmlr,str30
;         packkey     statxfld1 from str30
;end patch 1.76
;         MOVE        C2 TO STATXPATH
;         call        statxkey
;         goto        except if over
;DISXLST
;         display    *p1:23,*el,statxlist
;        move        statxlist to ninlist
;        match       "000000" to ninlist
;        goto        except if equal
;        endif
;
;        rep         zfill in ninlist

;        PACK        NWFFLD FROM NINLIST,statcampn,statmdate,statsrce
;         CALL        NWFTST
;         IF          NOT OVER
;         CALL        NWFDEL
;         ENDIF
;         call        nwfwrt
;
;         PACK        statFLD FROM NINLIST,statmlr,statcampn,statmdate,statsrce
statlook
;         clear      dim30
;         move       statcampn to dim30
; .        movefptr   dim30 to n2
;         sub        c1 from n2
;         setlptr    dim30 to n2
;         reset      dim30
         clear       statfld
;         append      ninlist to statfld
;         append      statmlr to statfld
;         append      dim30 to statfld
;         append      statsrce to statfld
;begin patch 1.7
;         PACKkey     statFLD FROM NINLIST,statmlr,statcampn,statsrce    added 12/13/99 jd
         PACKkey     statFLD FROM statmlr,statsrce
;end patch 1.7
;        move        ninlist to statlist
;        rep         zfill in statlist
;
;cleanup selects field
;
         clear     dim30
         move      statsel to dim30
         scan      "MONTH" in dim30
         if         equal
         movefptr   dim30 to n2
         sub        c1 from n2
         setlptr    dim30 to n2
         reset      dim30
         clear      statsel
         append     dim30 to statsel
         setlptr    dim30
         reset      dim30,0
         add        c6 to n2
         bump       dim30 to n2
         append     "MOS" to statsel
         append     dim30 to statsel
         reset      statsel
         clear      dim30
         move       statsel to dim30
         endif
         reset     dim30

         scan      "DONORS" in dim30
         if         equal
         movefptr    dim30 to n2
         sub        c1 from n2
         setlptr    dim30 to n2
         reset      dim30
         clear      statsel
         append     dim30 to statsel
         setlptr    dim30
         reset      dim30,0
         add        c7 to n2
         reset      dim30 to 30
         bump       dim30 to n2
         Append     "DNRS" to statsel
         append     dim30 to statsel
         reset      statsel
         clear      dim30
         move       statsel to dim30
         endif
         reset     dim30

         scan      "PAID SUBS" in dim30
         if         equal
         movefptr    dim30 to n2
         sub        c1 from n2
         compare    seq to n2
         goto       pdsub1 if equal
         setlptr    dim30 to n2
         reset      dim30
         clear      statsel
         append     dim30 to statsel
pdsub1   setlptr    dim30
         reset      dim30,0
         add        c10 to n2
         reset      dim30 to 30
         bump       dim30 to n2
         Append     "SUB(PD)" to statsel
         append     dim30 to statsel
         reset      statsel
         clear      dim30
         move       statsel to dim30
         endif
         reset     dim30

         scan      "DIRECT MAIL SUBS" in dim30
         if         equal
         movefptr    dim30 to n2
         sub        c1 from n2
         compare    seq to n2
         goto       dms1 if equal
         setlptr    dim30 to n2
         reset      dim30
         clear      statsel
         append     dim30 to statsel
dms1     setlptr    dim30
         reset      dim30,0
         add        "17" to n2
         reset      dim30 to 30
         bump       dim30 to n2
         Append     "SUB(DMS)" to statsel
         append     dim30 to statsel
         reset      statsel
         clear      dim30
         move       statsel to dim30
         endif
         reset     dim30

         scan      "CATALOG BUYERS" in dim30
         if         equal
         movefptr    dim30 to n2
         sub        c1 from n2
         compare    seq to n2
         goto       Cb1 if equal
         setlptr    dim30 to n2
         reset      dim30
         clear      statsel
         append     dim30 to statsel
Cb1      setlptr    dim30
         reset      dim30,0
         add        "15" to n2
         reset      dim30 to 30
         bump       dim30 to n2
         Append     "CAT BYRS" to statsel
         append     dim30 to statsel
         reset      statsel
         clear      dim30
         move       statsel to dim30
         endif
         reset     dim30
;done put it back
         move      dim30 to statsel
;                  
DELDUPE
;         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
;         MOVE      "01R",aKEY1
;         PACK      NORDFLD1 FROM AKEY1,statmlr
;         MOVE      "02R",aKEY1
;         PACK      NORDFLD2 FROM AKEY1,statLIST
;         CLEAR     NORDFLD3
;         CLEAR     NORDFLD4
         CLEAR     STATLR
         Clear     olrn
;begin patch 1.7
;         PACKkey     statFLD3 FROM statmlr,statsrce    added 12/13/99 jd
         PACKkey     statFLD FROM statmlr,statsrce
;         move      c3 to statpath
         compare   c1 to statflag
;         call      statopen3 if not equal
;         FILEPI    1;statFLE3
;         READ      statFLE3,statFLD3;*191,str6,*238,statlr
         call      statopen if not equal
         FILEPI    1;statFILE
;START PATCH 1.73 REPLACED LOGIC
;         READ      statFILE,statFLD;*191,str6,*238,statlr

statschk
.START PATCH 2.3.1 REPLACED LOGIC
.         READ      statFILE,statFLD;*142,statldes,*172,STATSEL,*202,str6,*249,statlr
         READ      statFILE,statFLD;*144,statldes,*174,STATSEL,*204,str6,*254,statlr
.end PATCH 2.3.1 REPLACED LOGIC
;END PATCH 1.73 REPLACED LOGIC
;end patch 1.7
         if        not over
         call      statdel
;         delete    statfle3,statfld3
         if        (str6="000000" or str6="000001")
         else
         move      str6 to statlist       *retain list number (it may have been
         endif
;                                         *nmanually cleaned. DLH 16Oct97         
         rep       zfill in statlist
         type      statlr
         goto      needclean if equal                     *we have an lr so skip order file search
         endif
;
;begin patch 2.2
         clear       statxlist
         clear       ninlist
.START PATCH 2.3.1 REPLACED LOGIC
.         move        "0170" to statmlr
         move        "000913" to statmlr
.END PATCH 2.3.1 REPLACED LOGIC
;   for now anyway
;
.START PATCH 2.3.1 REPLACED LOGIC
.         match       "0170" to statmlr
         match       "000913" to statmlr
.END PATCH 2.3.1 REPLACED LOGIC
         if          equal
         clear       statxfld1
;begin patch 1.76
         packkey       statxfld1 from STATMLR,xdes
;end patch 1.76
         move        c2 to statxpath
         call        statxkey
         goto        CHKUPP if over
         GOTO        DISXLST
CHKUPP   CLEAR       STATXFLD1
         clear       statxlist
         clear       ninlist
         move        xdes to str30
         rep         lowup in str30
         clear       statxfld1
;begin patch 1.76
               PackKey        StatxFld1 from Statmlr,str30
;end patch 1.76
         MOVE        C2 TO STATXPATH
         call        statxkey
         goto        except if over
DISXLST  display    *p1:23,*el,statxlist
         move        statxlist to ninlist
         match       "000000" to ninlist
         goto        except if equal
         endif
;

         rep         zfill in ninlist
                              MOVE       NINLIST TO STATLIST
                              move       xsel to statsel
                              move       xdes to statldes

         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         MOVE      "01R",aKEY1
.START PATCH 2.3.1 REPLACED LOGIC
.         PACK      NORDFLD1 FROM AKEY1,statmlr
          call      Trim using statmlr
          if (statmlr <> "")
                    pack      COMPFLD,statmlr
                    move      "COMPKEY",Location
                    pack      KeyLocation,"Key: ",COMPFLD
                    call      COMPKEY
          else
                    pack      COMPOLDMLR,"(((("
          endif
         PACK      NORDFLD1 FROM AKEY1,COMPOLDMLR
.END PATCH 2.3.1 REPLACED LOGIC
         MOVE      "02R",aKEY1
         PACK      NORDFLD2 FROM AKEY1,statLIST
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
.end patch 2.2
         UNPACK   STATMDaTE INTO MM,DD,STR2,YY
         match    "96" to yy
         goto     needclean if equal
         CALL      NORDAIM
         IF        NOT OVER
                   unpack   oodnum into str4,str3
                   match    offer to str3
                   goto     nordloop if not equal
                   clear    str4
                   UNPACK   STATMDaTE INTO MM,DD,STR2,YY
                   PACK     STR4 FROM MM,YY
                   PACK     MMYY FROM  OMDTEM,OMDTEY
                   REP      ZFILL IN STR4
                   REP      ZFILL IN MMYY
                   MATCH    MMYY TO STR4
                    IF       EQUAL
                    MOVE     OLRN TO STATLR
                    ENDIF
NORDLOOP          
                   MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
                   CALL     NORDKG
                   goto     needclean if over
                   CMATCH    "p" TO OSTAT       Pending order ?
                   GOTO      NORDLOOP IF EQUAL     YES, skip.
                   CMATCH    "x" TO OSTAT       Cancelled Pending order ?
                   GOTO      NORDLOOP IF EQUAL     YES, skip.
                   CMATCH    "l" TO OSTAT       LCR order ?
                   GOTO      NORDLOOP IF EQUAL     YES, skip.
                   CMATCH    "z" TO OSTAT       Cancelled LCR order ?
                   GOTO      NORDLOOP IF EQUAL     YES, skip.
                   unpack   oodnum into str4,str3
                   match    offer to str3
                   goto     nordloop if not equal
                    clear     mmyy
                   UNPACK   STATMDaTE INTO MM,DD,STR2,YY
                   clear     str4
                   PACK     STR4 FROM MM,YY
                    PACK     MMYY FROM OMDTEM,OMDTEY
                    REP      ZFILL IN MMYY
                   REP      ZFILL IN STR4
                    MATCH    MMYY TO STR4
                    goto     nordloop if not equal
;                     IF       EQUAL
                               type   STATLR 
                               goto   except3 if EQUAL
;                               goto     EXCEPT3
;.                             ELSE
                               MOVE     OLRN TO STATLR
;                              ENDIF
;                     else
                     goto     nordloop
;                    ENDIF
         endif
needclean
         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi
         clear     nordfld
         move      statlr to nordfld
         rep       zfill in nordfld
         cmatch    b1 to statlr
         if        eos
         call      except2
                              goto      STATTEST
                              endif
;         else
         call      nordkey
                              if        over
;         call      except2 if over
         call      except2 
                              goto      STATTEST
;                             else
;                             goto      stattest
                              endif
;begin patch 2.2
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
;         call      getlist            Jd turned off 8/2/04
;end patch 2.2
;         endif
stattest
;         CALL        statTST
;         IF          NOT OVER
;         CALL        statDEL
;         ENDIF
         if          (c0 = statresp & statrev >0)
         call         except4
         endif
         if          (c0 < statresp & statrev < 1)
         call         except4
         endif
         if          (statrev < 0)
         call         except5
         endif
         
         call        statwrt
         goto        looper

getlist
                 move        olnum to ndatfld
                 move        c1 to ndatpath
                 move        c3 to ndatlock
                 rep         zfill in ndatfld
                 call        ndatkey
                         if          not over
                         move        olstname to statldes
                     move        lstnum to statlist                                                                      move        lstnum to statlist
                        endif
         return
EXCEPT
         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statsel,B1,STATMQTY,"No List" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     xdes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon,"No List Number Found",hpboff,*n,*n
         add         c3 to lines
         GOTO        LOOPER

EXCEPT2  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     xdes,b1,statsel,B1,STATMQTY," no lr" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon," No LR",hpboff,b1,statlr,b1,olrn,*n
         add         c3 to lines
         return
       
EXCEPT3  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     xdes,b1,statsel,B1,STATMQTY," to many lrs" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon,"to MANY LR'S ",STATLR,B1,OLRN,hpboff,*n
         add         c3 to lines
         CLEAR       STATLR
         goto        stattest
EXCEPT4  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     xdes,b1,statsel,B1,STATMQTY," Stats Bad?" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon," Check stats":
                     statresp,b1,statrev,hpboff,*n
         add         c3 to lines
         return
EXCEPT5  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     xdes,b1,statsel,B1,STATMQTY," Stats Bad?" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon," Check REVENUE":
                     statresp,b1,statrev,hpboff,*n
         add         c3 to lines
         return
       


Header   PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
         move      c1 to page
.START PATCH 2.3 REPLACED LOGIC
.HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      hpt700,"DATE: ",TODAY:
.                   *N,*01,"(170)":
.                      hpt275,"Statistical Data Exception Report:":
.                      hpt700,"PAGE:    ",PAGE:
.                   *N,"Weeks Out : ",statwkso:
.                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
.                   hpt325,"NWF##",hpt350,"List Name",hpboff,*n
HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
                      *52,"NAMES IN THE NEWS":
                      hpt700,"DATE: ",TODAY:
                   *N,*01,"(170)":
                      hpt275,"Statistical Data Exception Report:":
                      hpt700,"PAGE:    ",PAGE:
                   *N,"Weeks Out : ",statwkso:
                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
                   hpt325,"NWF##",hpt350,"List Name",hpboff,*n
.END PATCH 2.3 REPLACED LOGIC
         add         c1 to page
         move        c8 to lines
         return
eoj      weof        except,seqeof
         close       except
         print       *f,*flush
         splclose
         release
;begin patch 1.4
;         .execute    "copy g:\data\except.lst lpt1:"
;begin patch 2.01
                    call                GetWinVEr
;         path      exist,"c:\windows"
;         if        not over
;        append    "c:\command.com /c copy g:\data\" to taskname
;START PATCH 1.6 REPLACED LOGIC
;         execute    "!c:\command.com /c copy g:\DATA\except.LST \\NINs2\Laser8"
;         else
;.         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
;         execute    "!c:\winnt\system32\cmd.exe /c copy g:\DATA\except.LST \\NINs2\Laser8"
;         endif
;
                    If                  (osflag = c3 | osflag = C4)
         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser8"
         execute    TASKNAME
;         else
                    ElseIf             (osflag = c1 | osflag = C5)
;         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
         PACK       TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser8"
         execute    TASKNAME
                    ElseIf             (osflag = C6)
         PACK       TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser8"
         execute    TASKNAME
;end patch 2.01
         endif
;END PATCH 1.6 REPLACED LOGIC
;         execute    "F:\PUBLIC\NPRINT.exe g:\DATA\except.LST Q=LASER5 f=0 nb S=SRV2008A_fpnw "
;endpatch 1.4
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
;         execute    copycmd
;         pause     "5"
;        move    no to str1
;         Keyin       *p10:12,"Ready to sort and Reindex the NINSTATS file ? ",str1
;         cmatch      yes to str1
;         if          equal
;begin patch 1.4
;         prepare     batch,"c:\work\stats.bat"
;         path      exist,"c:\windows"
;         if        not over
;START PATCH 1.6 REPLACED LOGIC
;         execute    "!c:\command.com /c copy g:\DATA\except.LST \\NINs2\Laser5"
;         execute    "!c:\command.com /c Copy g:\data\text\ninstats.dat g:\data\text\ninstats.tmp"         
;         execute    "!c:\command.com /c f:\netutils\sort32 g:\data\text\ninstats.tmp g:\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) w(c:) verbose"
;         execute    "!c:\command.com /c f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstats,L276 -191-196,1-4,5-34,117-130"
;         execute    "!c:\command.com /c f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat2,L276 -238-243"
;         execute    "!c:\command.com /c f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat3,L276 -1-4,117-130"
;         else
;.         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
;         execute      "c:\winnt\system32\cmd.exe /c Copy g:\data\text\ninstats.dat g:\data\text\ninstats.tmp"         
;         execute      "c:\winnt\system32\cmd.exe /c f:\netutils\sort32 g:\data\text\ninstats.tmp g:\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) w(c:) verbose"
;         execute      "c:\winnt\system32\cmd.exe /c f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstats,L276 -191-196,1-4,5-34,117-130"
;         execute      "c:\winnt\system32\cmd.exe /c f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat2,L276 -238-243"
;         execute      "c:\winnt\system32\cmd.com /c f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat3,L276 -1-4,117-130"
;...
;START PATCH 1.73 REPLACED LOGIC
;         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser5"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) w(c:) verbose"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstatS,L286 -1-4,117-130"
;.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -191-196,1-4,5-34,117-130"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L286 -238-243"
;         execute    TASKNAME
;.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat3,L286 -1-4,117-130"
;.         execute    TASKNAME
;         else
;.         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
;         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
;.         execute    TASKNAME
;         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) w(c:) verbose"
;         execute    TASKNAME
;         PACK       TASKNAME,"c:\winnt\system32\cmd.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstatS,L286 -1-4,117-130"
;.         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -191-196,1-4,5-34,117-130"
;         execute    TASKNAME
;         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L286 -238-243"
;         execute    TASKNAME
;....................
;         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser5"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) w(c:) verbose"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstatS,L501 -1-4,117-141"
;         execute    TASKNAME
;         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
;         execute    TASKNAME
;         else
;         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
;         execute    TASKNAME
;         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) w(c:) verbose"
; .        execute    TASKNAME
;         PACK       TASKNAME,"c:\winnt\system32\cmd.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstatS,L501 -1-4,117-141"
;         execute    TASKNAME
;         PACK       TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
;         execute    TASKNAME
;END PATCH 1.73 REPLACED LOGIC
;         PACK       TASKNAME,"c:\winnt\system32\cmd.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat3,L286 -1-4,117-130"
;         execute    TASKNAME
;END PATCH 1.6 REPLACED LOGIC
;         write        batch,seq;"Copy g:\data\text\ninstats.dat g:\data\text\ninstats.tmp"         
;         write        batch,seq;"f:\netutils\sort g:\data\text\ninstats.tmp g:\data\text\ninstats.dat  /s(1,4,N,a,187,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
;         write        batch,seq;"f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstats,L272 -187-192,1-4,5-34,117-126"
;         write        batch,seq;"f:\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat2,L272 -234-239"
;         weof         batch,seq
;         close        batch
;         shutdown    "c:\work\stats.bat"
;         endif
;end patch 1.4
;         endif
         stop         
.DEBUG     RETURN
         include     nwfio.inc
         include     nwfXio.inc
         include    statsio.inc
         include    nordio.inc
        include     SXrfio.inc
        include     ndatio.inc
.START PATCH 2.3.1 ADDED LOGIC
          include   COMPIO.INC
          include   CNTIO.INC
.END PATCH 2.3.1 ADDED LOGIC
         include    comlogic.inc     

