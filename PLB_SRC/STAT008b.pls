         include   common.inc
         include   cons.inc
         include   statsdd.inc
         include   nwfdd.inc
         include   nwfXdd.inc
         include   norddd.inc
        include     SXrfdd.inc
          include  SlctClnDD.inc
          include  hp.inc
.START PATCH 1.3 ADDED LOGIC
	INCLUDE	NSEL2DD.INC
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.4.1 ADDED LOGIC
	include	COMPDD.INC
	include	CNTDD.INC
.END PATCH 1.4.1 ADDED LOGIC
......................................................................................
release	init	"1.4.1"	ASH	27JAN2005	NINSTATS CONVERSION
reldate	init	"January 27, 2005"
.release	init	"1.4"	ASH	09AUG2004  LOGO CONVERSION
.reldate	init	"AUGUST 09, 2004"
.release	init	"1.3"	ASH	29JAN2004  DATACARD CONVERSION
.reldate	init	"JANUARY 29, 2004"
.Release        Init           "1.2"          12Apr2002    DLH Use GetWinVer
;Release        Init           "1.1"          08Apr2002    DLH change to statxfld1
.release   init      "1.0"      NOV212001 intial.
except   file
batch    file                 .created for temp use
RECSIN   FORM      6
STR256   DIM       300
str60    dim       60
package  dim       30
.str13    dim       13
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
weeksout  dim        5
str30ck   dim        30
mochk     form       "00"
Savelr        dim            6
dupeflag  	form       1           0=no dupe, 1 = dupe
.copycmd  init      "!c:\command.com /C copy g:\data\except.lst \\nts0\laser3 "
.START PATCH 1.6 REPLACED LOGIC
.copycmd  init      "!c:\command.com /C copy g:\data\except.lst lpt1: "
copycmd  DIM         50
         PACK        copycmd,"!c:\command.com /C copy ",NTWKPATH1,"except.lst \\nts0\laser2 "
.END PATCH 1.6 REPLACED LOGIC
.
         open      input,"c:\work\unicefNEW.csv",exclusive
        CLOCK       DATE TO TODAY
        CLOCK     TIME TO TIME
.START PATCH 1.6 REPLACED LOGIC
        PACK        STR55,NTWKPATH1,"stats\unicef\excepts.uni"
        open        except,STR55
.END PATCH 1.6 REPLACED LOGIC
        read        except,seqeof;str1
        move       "stat008b" to program
        move       "Names in the News" to compnme
        call       paint
.START PATCH 1.6 REPLACED LOGIC
        PACK        STR35,NTWKPATH1,"EXCEPTunf.LST"
        splopen     STR35
.END PATCH 1.6 REPLACED LOGIC
        print       hpport,*rptchar "*":80:
                    *n,*n,*n,hpbon:
                    *n,*1,"Program    : ",program,b2,"Date: ",today:
                    b2,"Time: ",time:
                    *n,*1,"Company    : ",compnme:
                    *n,*1,"User       : ":
                    *n,*1,"Deliver To : Henry",hpboff:
                    *n,*rptchar "*":80
        move       c1 to statpath
looper  read       input,seq;*cdfon,statsrce:
                     str5:
                     b1:
                     package:
                     statldes:
                     str40:
                     mqty:
                     resp:
                     str6:
                     revenue:
                     str6:
                     str6:
                     lstcpm:
                     tlst$:
                     str6:
                     str6:
                     str6:
                     str6:
                     str6:
                     str6:
                     str6
        goto       eoj if over
        display    *p10:10,"records in ",recsin;
        move   c0 to dupeflag
        add        c1 to recsin
.................................................................
.campaign
       compare    c1 to recsin
       if         not greater
       goto       looper
        endif
.................................................................
.Mail date
.
        scan       dash in package
        goto       dasher if equal      .not a valid format, get out
        scan       "_" in package
        goto       unders if equal      .not a valid format, get out
.        bump       statsel by -30
        reset      package
        move       package to mm
       bump        package by 2
        move       package to yy
        goto       yrchk
.        scan       slash in statsel
unders
        reset       package
        move        package to str2
        if          (str2 = "10"| str2 = "11" |str2 ="12")
.       reset       package
.       setlptr    package to n2
        move       package to mm
        else
       lenset      package
        bump       package by 1
        pack       mm from c0,package
        endif
        setlptr    package,c30
        reset      package
        scan       "_" in package
        bump       package by 1
        move       package to yy
        goto       yrchk
DASHER

.        bump        package by -2
.       lenset      package
        reset       package
        move        package to str2
        if          (str2 = "10"| str2 = "11" |str2 ="12")
.       reset       package
.       setlptr    package to n2
        move       package to mm
        else
       lenset      package
        bump       package by 1
        pack       mm from c0,package
        endif
        setlptr    package,c30
        reset      package
        scan       dash in package
        bump       package by 1
        move       package to yy
yrchk
        match      "00" to package
        if         equal
        pack       str4 from "20",package
        else
        MATCH      "01" TO package
        IF         EQUAL
        pack       str4 from "20",package
        ELSE
        pack       str4 from "19",package
          endif
          ENDIF
.        move       statsel to str4
        pack       statmdate from mm,"01",str4
        match      "                              " to statldes
        goto       looper if equal
        goto       looper if eos
.
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
        move        mqty to statmqty
.        display     *p1:24,*el,mqty,b1,nwfmqty;
.break out number of responses
        move        c0 to nwfresp
resp    clear       str1
        display     *p1:23,*el,resp
        move        resp to statresp
.
rev
        move        revenue to statrev

        move        lstcpm to statlcpm
.        add         decimal to nwflstcpm
.
.break out total list $
        move        c0 to nwftlst$
.        move        tlst$ to statImcst             updated 4/17/00. jd
         move        c0 to lstcpmf
         move        c0 to mailcstf
         move        statlcpm to lstcpmf
         move        tlst$ to mailcstf
.
         add         lstcpmf to mailcstf
         move        mailcstf to statimcst
.        add         decimal to nwftlst$
.
.break out cost per m
        move        c0 to statpCKM
        move        tlst$ to statpckm             updated 4/17/00. jd
.break out tota cost per member
statred
.        move        Str35 to statxfld1
.        rep         lowup in statxfld1
         clear       statxlist
         clear       ninlist
.
.START PATCH 1.4.1 REPLACED LOGIC
.         move        "0055" to statmlr
..   for now anyway
..
.         match       "0055" to statmlr
         move        "000811" to statmlr
.   for now anyway
.
         match       "000811" to statmlr
.END PATCH 1.4.1 REPLACED LOGIC
         if          equal
.          display    *p1:23,*el,nwfxfld
.         call        nwfxkey
.dh testing
         clear       statxfld1
.begin patch 1.1
               Packkey        StatxFld1 from statmlr,statldes
.         packkey       statxfld1 from str35
.end patch 1.1
.
         move        c2 to statxpath
         call        statxkey
.         goto        except if over
         goto        CHKUPP if over
         GOTO        DISXLST
CHKUPP   CLEAR       STATXFLD1
         clear       statxlist
         clear       ninlist
         move        statldes to str30ck
         rep         lowup in str30ck
         clear       statxfld1
.begin patch 1.1
               Packkey        StatxFld1 from statmlr,str30ck
.         packkey     statxfld1 from str35ck
.end patch 1.1
         MOVE        C2 TO STATXPATH
         call        statxkey
         goto        except if over
DISXLST  display    *p1:23,*el,statxlist
         move        statxlist to ninlist
         match       "000000" to ninlist
         goto        except if equal
         endif
.
         rep         zfill in ninlist

         PACK        NWFFLD FROM NINLIST,statcampn,statmdate,statsrce
.         CALL        NWFTST
.         IF          NOT OVER
.         CALL        NWFDEL
.         ENDIF
.         call        nwfwrt
.
.         PACK        statFLD FROM NINLIST,statmlr,statcampn,statmdate,statsrce
statlook
.         clear      dim30
.         move       statcampn to dim30
. .        movefptr   dim30 to n2
.         sub        c1 from n2
.         setlptr    dim30 to n2
.         reset      dim30
         clear       statfld
.         append      ninlist to statfld
.         append      statmlr to statfld
.         append      dim30 to statfld
.         append      statsrce to statfld
.begin patch 1.7
.         PACKkey     statFLD FROM NINLIST,statmlr,statcampn,statsrce    added 12/13/99 jd
         call    trim using statsrce
         reset     statsrce
         PACKkey     statFLD FROM statmlr,statsrce
.end patch 1.7
         move        ninlist to statlist
         rep         zfill in statlist
.
.cleanup selects field
.
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
.done put it back
         move      dim30 to statsel
.                  
DELDUPE
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         MOVE      "01R",aKEY1
.START PATCH 1.4.1 REPLACED LOGIC
.         PACK      NORDFLD1 FROM AKEY1,statmlr
	pack	COMPFLD,statmlr
	move	"COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
         PACK      NORDFLD1 FROM AKEY1,COMPOLDMLR
.END PATCH 1.4.1 REPLACED LOGIC
         MOVE      "02R",aKEY1
         PACK      NORDFLD2 FROM AKEY1,statLIST
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         CLEAR     STATLR
         Clear     olrn
.begin patch 1.7
.         PACKkey     statFLD3 FROM statmlr,statsrce    added 12/13/99 jd
         PACKkey     statFLD FROM statmlr,statsrce
.         move      c3 to statpath
         compare   c1 to statflag
.         call      statopen3 if not equal
.         FILEPI    1;statFLE3
.         READ      statFLE3,statFLD3;*191,str6,*238,statlr
         call      statopen if not equal
         FILEPI    1;statFILE
.START PATCH 1.4.1 REPLACED LOGIC
.         READ      statFILE,statFLD;*202,str6,*249,statlr
         READ      statFILE,statFLD;*204,str6,*254,statlr
.END PATCH 1.4.1 REPLACED LOGIC
         if        not over
         call      statdel
         move      str6 to statlist       *retain NIN list number (it may have been manually corrected
..22Feb01 DLH
.        else                          .its over
.         move      str6 to statlist       *retain list number (it may have been
         endif
.end test test test
         rep       zfill in statlist
         type      statlr
         goto      needclean if equal                     *we have an lr so skip order file search
.                                         *nmanually cleaned. DLH 16Oct97
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         UNPACK   STATMDaTE INTO MM,DD,STR2,YY
         match    "96" to yy
         goto     needclean if equal
         CALL      NORDAIM
         IF        NOT OVER
.                   unpack   oodnum into str4,str3
.                   match    offer to str3
.                   goto     nordloop if not equal
                    move   c0 to dupeflag
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
.                   unpack   oodnum into str4,str3
.                   match    offer to str3
.                   goto     nordloop if not equal
                    clear     mmyy
                   UNPACK   STATMDaTE INTO MM,DD,STR2,YY
                   clear     str4
                   PACK     STR4 FROM MM,YY
                    PACK     MMYY FROM OMDTEM,OMDTEY
                    REP      ZFILL IN MMYY
                   REP      ZFILL IN STR4
                    MATCH    MMYY TO STR4
                    goto     nordloop if not equal
                    type   STATLR
                    goto   except3 if EQUAL
                    MOVE     OLRN TO STATLR
                    goto     nordloop
         endif
needclean
         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi
         clear     nordfld
         move      statlr to nordfld
         rep       zfill in nordfld
.         cmatch    b1 to nordfld
.         if        eos
.         call      except2
.         else
         call      nordkey
         call      except2 if over
         clear     statsel
.begin patch 1.3
.START PATCH 1.3 REPLACED LOGIC
.               packkey        SlctClnFld from O2des
		packkey	NSEL2FLD,"1",OLRN
		move	"NSEL2KEY",Location
		pack	KeyLocation,"Key: ",NSEL2FLD
		call	NSEL2KEY
		if over
			move	O2DES,NSEL2NAME
		endif
               packkey        SlctClnFld from NSEL2NAME
.END PATCH 1.3 REPLACED LOGIC
               call           SlctClnKey
               if             not over
               move           SlctClnText to Statsel
               else
.START PATCH 1.3 REPLACED LOGIC
.               move           O2Des to Statsel
               move           NSEL2NAME to Statsel
.END PATCH 1.3 REPLACED LOGIC
               endif
.         endif
stattest
.         move       c3 to statpath
.         pack        statfld3 from statmlr,statsrce
         move       c1 to statpath
         pack        statfld from statmlr,statsrce
.end patch 1.1
         CALL        statTST
         IF          NOT OVER
         CALL        statDEL
         ENDIF
         move       c1 to statpath
         if          (c0 = statresp & statrev >0)
         call         except4
         endif
         if          (c0 < statresp & statrev < 1)
         call         except4
         endif
         if          (statrev < 0)
         call         except5
         endif
         if          (statmqty <= 0)    .no names mailed?
         call         except5
         endif
.begin patch .003
        type        statlr
        if          not equal
        move        StatXdesc1 to statsel
        endif
        if          (statlr = "" or statlr = "      ")
        move        StatXdesc1 to statsel
        endif
        if          (statsel = "" or statsel = "      ")
        move        Statxdesc1 to statsel
        endif
.end patch .003
. 
         IF          (DUPEFLAG = C1)            .MULTIPLE LRS'S
         CLEAR       STATLR
         ENDIF
         call        statwrt
         clear       statsel
         clear       statldes
        clear        stattype
         clear       statlr
         goto        looper
.
EXCEPT
         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statsel,B1,STATMQTY,"No List" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon,"No List Number Found",hpboff,*n,*n
         add         c3 to lines
         GOTO        LOOPER

EXCEPT2  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statsel,B1,STATMQTY," no lr" 
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
                     statldes,b1,statsel,B1,STATMQTY," to many lrs" 
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
                     statldes,b1,statsel,B1,STATMQTY," Stats Bad?" 
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
                     statldes,b1,statsel,B1,STATMQTY," Stats Bad?" 
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
.START PATCH 1.4 REPLACED LOGIC
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
.END PATCH 1.4 REPLACED LOGIC
         add         c1 to page
         move        c8 to lines
         return
eoj      weof        except,seqeof
         close       except
         print       *f,*flush
         splclose
         release
.begin patch 1.01
.         .execute    "copy g:\data\except.lst lpt1:"
.begin patch 1.2
                    call                GetWinVer
;         path      exist,"c:\windows"
;         if        not over
.        append    "c:\command.com /c copy g:\data\" to taskname
.START PATCH 1.6 REPLACED LOGIC
.         execute    "!c:\command.com /c copy g:\DATA\except.LST \\nts0\laser8"
.         else
..         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
.         execute    "!c:\winnt\system32\cmd.exe /c copy g:\DATA\except.LST \\nts0\laser8"
.         endif
.
                    If                  (osflag = c3 |  osflag = c4)
         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"except.LST \\nts0\laser8"
         execute    TASKNAME
;         else
                    ElseIf              (osflag = c1 |  osflag = c5)
.         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
         PACK       TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"except.LST \\nts0\laser8"
         execute    TASKNAME
                    ElseIf              (osflag = c6)
         PACK       TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"except.LST \\nts0\laser8"
         execute    TASKNAME
         endif
.END PATCH 1.6 REPLACED LOGIC
.         execute    "F:\PUBLIC\NPRINT.exe g:\DATA\except.LST Q=LASER5 f=0 nb S=NTS0_fpnw "
.endpatch 1.01
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
.         execute    copycmd
         stop
.DEBUG     RETURN
         include     nwfio.inc
         include     nwfXio.inc
         include    statsio.inc
         include    nordio.inc
          include  SlctClnIO.inc
        include     SXrfio.inc
.START PATCH 1.3 ADDED LOGIC
	INCLUDE	NSEL2IO.INC
.END PATCH 1.3 ADDED LOGIC
.START PATCH 1.4.1 ADDED LOGIC
	include	COMPIO.INC
	include	CNTIO.INC
.END PATCH 1.4.1 ADDED LOGIC
         include    comlogic.inc     
