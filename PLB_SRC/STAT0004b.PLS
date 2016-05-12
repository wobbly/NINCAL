pc      equ        0
         include   common.inc
         include   cons.inc
         include   tncXdd.inc
         include   statsdd.inc
         include   norddd.inc
         include   ndatdd.inc
          include  hp.inc
.START PATCH 1.3.1 ADDED LOGIC
          include   COMPDD.INC
          include   CNTDD.INC
.END PATCH 1.3.1 ADDED LOGIC
release   init      "1.3.1"      27JAN2005        ASH       NINSTATS Conversion
.release   init      "1.3"      09AUG2004         ASH       Logo Conversion
.release   init      "1.2"      05Jul2001 DLH
.release   init      "1.1"      04OCT2000 ASH NEW SERVER ADDED
.release   init      "PRE"      .DEC99 DLH new code to apply TNC stats
.                               two versions needed  one to apply old dawson stuff version a
.                                                    one to apply new/current data
.                               THIS VERSION IS CURRENTLY TWEAKED FOR OLD excel result file as CSV INPUT
except   file
batch    file                 .created for temp use
RECSIN   FORM      6
STR256   DIM       300
.str13    dim       13
DECIMAL  FORM      8.6
PERCENT  INIT      "%"
cntrbflg FORM      "0"
input    file       
TIME     INIT      "HH:MM:SS"
.offer    dim         3
minusflg dim        1
mqty     dim       11
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

.INput file
srccode   dim       14             1-10   key
Lstcode        dim        4    15-18
pkgcode        dim        8    19-26
tstcode        dim       8     27-34
tstcell        dim       8     35-42
technique      dim       2     43-44
incomenw       dim       12     45-53
returnnw       dim       4    114-57
incomeRN       dim       12    105-66
returnRN       dim       4    114-70
incomeRJ       dim       12    105-712
returnRJ       dim       4    114-83
incomeUR       dim       12    105-122
returnUR       dim       4    114-126
incomeAD       dim       12    105-105
returnAD       dim       4    114-1012
incomeDO       dim       12    105-118
returnDO       dim       4     114-122

.work vars
incometmp      form      9.2
incometmp1     form      9
returntmp      form      5
.

weeksout  dim        5
.copycmd  init      "!c:\command.com /C copy g:\data\exceptnc.lst \\NINs2\Laser3 "
.START PATCH 1.1 REPLACED LOGIC
.copycmd  init      "!c:\command.com /C copy g:\data\exceptnc.lst lpt1: "
copycmd  DIM        50
         pack       copycmd,"!c:\command.com /C copy ",NTWKPATH1,"exceptnc.lst lpt1: "
.END PATCH 1.1 REPLACED LOGIC
ninlist  dim        6
.
        open      input,"\\nins1\d\users\dherric\tncres.csv",exclusive
        CLOCK       DATE TO TODAY
        CLOCK     TIME TO TIME
        prepare     except,"\\nins1\d\users\dherric\except.TNC",exclusive
        read        except,seqeof;str1
        move       "stat0004" to program
        move       "Names in the News CA" to compnme
        call       paint
.START PATCH 1.1 REPLACED LOGIC
.        splopen     "g:\data\exceptnc.lst"
        PACK        STR35,NTWKPATH1,"EXCEPTNC.LST"
        splopen     STR35
.END PATCH 1.1 REPLACED LOGIC
        print       hpport,*rptchar "*":80:
                    *n,*n,*n,hpbon:
                    *n,*1,"Program    : ",program,b2,"Date: ",today:
                    b2,"Time: ",time:
                    *n,*1,"Company    : ",compnme:
                    *n,*1,"User       : ":
                    *n,*1,"Deliver To : DH",hpboff:
                    *n,*rptchar "*":80
        move       c1 to statpath
getdate keyin      *p10:12,"returns thru mmddccyy :",statpdate
        keyin      *p10:14,"Date ok? ",*dv,statpdate,*dv,b10,str1
        rep        "nN" in str1
        cmatch     no to str1
        goto       getdate if equal
looper  read       input,seq;*cdfon,srccode:   dim       14     1-14   key
                        Lstcode:        dim        4    15-18
                        pkgcode:        dim        8    19-26
                        tstcode:        dim       8     27-34
                        tstcell:        dim       8     35-42
                        technique:      dim       2     43-44
                        str4:            maildate mmyy
                        returnnw:       
                        incomenw:       
                        str8        
        goto       eoj if over
        display    *p10:10,"records in ",recsin;
        add        c1 to recsin
        move       srccode to statsrce
.need more code here this works for july's file
        clear      str6
.        clear      str4

.        unpack     srccode into str5,str4
        unpack     str4 into mm,yy
        move       "01" to dd
        if         (yy = "99")
        move       "19" to cc
        else
        move       "20" to cc
        endif
        pack       statmdate from mm,dd,cc,yy
........
        call       rtrim using str8
        move       c0 to statmqty
        move       str8 to statmqty

        move       c0 to statresp
        move       c0 to statrev

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomenw
        if         equal
        reset      incomenw
        move       incomenw to incometmp
        add        incometmp to statrev
        else
        reset      incomenw
        call       rtrim using incomenw
        move       incomenw to incometmp1
        add        incometmp1 to statrev
        endif

.        move       c0 to incometmp
.        move       c0 to incometmp1
.        scan       "." in incomeRN
.        if         equal
.        reset      incomern
.        move       incomern to incometmp
.        add        incometmp to statrev
.        else
.        reset      incomern
...        call       rtrim using incomern
.        move       incomern to incometmp1
.        add        incometmp1 to statrev
.        endif

.        move       c0 to incometmp
.        move       c0 to incometmp1
.        scan       "." in incomeRJ
.        if         equal
.        reset      incomeRJ
.        move       incomeRJ to incometmp
.        add        incometmp to statrev
.        else
.        reset      incomeRJ
.        call       rtrim using incomeRJ
.        move       incomeRJ to incometmp1
.        add        incometmp1 to statrev
.        endif

.        move       c0 to incometmp
.        move       c0 to incometmp1
.        scan       "." in incomeUR
.        if         equal
. .       reset      incomeUR
.        move       incomeUR to incometmp
.        add        incometmp to statrev
.        else
.        reset      incomeUR
.        call       rtrim using incomeUR
.        move       incomeUR to incometmp1
.        add        incometmp1 to statrev
.        endif

.        move       c0 to incometmp
.        move       c0 to incometmp1
.        scan       "." in incomeAD
.        if         equal
.        reset      incomeAD
.        move       incomeAD to incometmp
.        add        incometmp to statrev
.        else
.        reset      incomeAD
.        call       rtrim using incomeAD
.        move       incomeAD to incometmp1
.        add        incometmp1 to statrev
.        endif

.        move       c0 to incometmp
.        move       c0 to incometmp1
.        scan       "." in incomeDO
.        if         equal
.        reset      incomeDO
.        move       incomeDO to incometmp
.        add        incometmp to statrev
.        else
.        reset      incomeDO
.        call       rtrim using incomeDO
.        move       incomeDO to incometmp1
.        add        incometmp1 to statrev
.        endif
.
        move       c0 to returntmp
        move       returnnw to returntmp
        add        returntmp to statresp

.        move       c0 to returntmp
.        move       returnRn to returntmp
.        add        returntmp to statresp

.        add        returnRN to statresp
.        move       c0 to returntmp
.        move       returnRJ to returntmp
.        add        returntmp to statresp
.        add        returnRJ to statresp

. .       move       c0 to returntmp
.        move       returnUR to returntmp
.        add        returntmp to statresp
..        add        returnUR to statresp
.
.        move       c0 to returntmp
.        move       returnAD to returntmp
.        add        returntmp to statresp
..        add        returnAD to statresp
.
.        move       c0 to returntmp
.        move       returnDO to returntmp
.        add        returntmp to statresp
..        add        returnDO to statresp

.        move       package to statpanel
        clear      statlr
        clear      statkycd
.................................................................        

******************************************************************************
.
.        unpack             Lstcode into str1,TNCxfld
        move        Lstcode to TNCxfld

        move        c1 to tncxpath
         clear       TNCxlist
         clear       ninlist
.
.START PATCH 1.3.1 REPLACED LOGIC
.         move        "0173" to statmlr
         move        "000619" to statmlr
.END PATCH 1.3.1 REPLACED LOGIC
.   for now anyway
.
.START PATCH 1.3.1 REPLACED LOGIC
.         match       "0173" to statmlr
         match       "000619" to statmlr
.END PATCH 1.3.1 REPLACED LOGIC
         if          equal
.          display    *p1:23,*el,TNCxfld
         call        TNCxkey
         if          over
         move        "000000" to TNCxlist
.         clear        TNCxstat
         clear         TNCdesc                    .    nin desc
         move         TNCdesc1 to statldes
         move         "Not FOund" to TNCdesc1           .TNC DESC - Key
         goto        except
         endif
         display    *p1:23,*el,TNCxlist
         move        TNCxlist to ninlist
.         match       "000000" to ninlist
.                if           equal
          if     (ninlist = "000000" or ninlist = "      ")
                 move         TNCdesc1 to statldes
                 goto        except
                 else
                 move        tncxlist to ndatfld
                 move        c1 to ndatpath
                 move        c3 to ndatlock
                 rep         zfill in ndatfld
                 call        ndatkey
                         if          not over
                         move        olstname to statldes
                         endif
                 endif
         endif
         clear      statcampn 

         unpack     pkgcode into str5,tncpfld
         clear      statpanel
         call       tncpkey
         if         not over
         move       tncpdesc to statpanel
         endif
.
         rep         zfill in ninlist


.         PACK        statFLD FROM NINLIST,statmlr,statcampn,statsrce
         PACK        statFLD FROM statmlr,statsrce
         move        ninlist to statlist
         rep         zfill in statlist
.
         move      dim30 to statsel
.                  
         
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         MOVE      "01R",aKEY1
.START PATCH 1.3.1 REPLACED LOGIC - TEMPORARY PATCH
.         PACK      NORDFLD1 FROM AKEY1,statmlr
          pack      COMPFLD,statmlr
          move      "COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
         PACK      NORDFLD1 FROM AKEY1,COMPOLDMLR
.END PATCH 1.3.1 REPLACED LOGIC - TEMPORARY PATCH
         MOVE      "02R",aKEY1
         PACK      NORDFLD2 FROM AKEY1,statLIST
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         move      b6 to statlr
         Clear     olrn
         compare   c1 to statflag
         call      statopen if not equal
         FILEPI    1;statFILE
.START PATCH 1.3.1 REPLACED LOGIC
.         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,str12,*264,Statpckm
         READ      statFILE,statFLD;*37,str8,*204,str6,*235,statlcpm,statImcst,statlr2,statlr,str12,*293,Statpckm
.END PATCH 1.3.1 REPLACED LOGIC
.         READ      statFILE,statFLD;*191,str6,*238,statlr
         if        not over
         move      str6 to statlist       *retain list number (it may have been 
         move      str8 to statmdate      *retain maildate - no longer in dmatext file as part of source code
        move       str12 to statkycd      *retain corrected list code
.                                         *nmanually cleaned. DLH 16Oct97         
         rep       zfill in statlist
         type      statlr
         goto      needclean if equal                     *we have an lr so skip order file search
         endif
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         UNPACK   STATMDaTE INTO MM,DD,STR2,YY
         CALL      NORDAIM
         IF        NOT OVER
.                   unpack   oodnum into str4,str3
.                   match    offer to str3
.                   goto     nordloop if not equal
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
.                     IF       EQUAL
                               type   STATLR 
                               goto   except3 if EQUAL
.                               goto     EXCEPT3
..                             ELSE
                               MOVE     OLRN TO STATLR
.                              ENDIF
.                     else
                     goto     nordloop
.                    ENDIF
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
.calc  weeksout
         unpack    statmdate into mm,dd,str2,yy
         call      cvtjul
         move      juldays to weeks
         unpack    statpdate into mm,dd,str2,yy
         call      cvtjul
         sub       weeks from juldays
         divide    n7 into juldays  
         move      juldays to statwkso
.check do we have qty mailed ?
.




.         endif
stattest CALL        statTST
         IF          NOT OVER
         CALL        statDEL
         ENDIF
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
EXCEPT
         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statsel,B1,STATMQTY,"No List ",tncxfld 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL,B1,STATMQTY:
                     *n,statpanel,hpbon,"No List Number Found ",hpboff,tncxfld,*n,*n
         add         c3 to lines
         GOTO        LOOPER

EXCEPT2 
         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
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
.START PATCH 1.3 REPLACED LOGIC
.HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      hpt700,"DATE: ",TODAY:
.                   *N,*01,"(0173)":
.                      hpt275,"Statistical Data Exception Report:":
.                      hpt700,"PAGE:    ",PAGE:
.                   *N,"Weeks Out : ",statwkso:
.                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
.                   hpt325,"TNC##",hpt350,"List Name",hpboff,*n
HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
                      *52,"NAMES IN THE NEWS":
                      hpt700,"DATE: ",TODAY:
                   *N,*01,"(0173)":
                      hpt275,"Statistical Data Exception Report:":
                      hpt700,"PAGE:    ",PAGE:
                   *N,"Weeks Out : ",statwkso:
                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
                   hpt325,"TNC##",hpt350,"List Name",hpboff,*n
.END PATCH 1.3 REPLACED LOGIC
         add         c1 to page
         move        c8 to lines
         return
eoj      weof        except,seqeof
         close       except
         print       *f,*flush
         splclose
         release
.begin patch 1.4
.         .execute    "copy g:\data\exceptnc.lst lpt1:"
         path      exist,"c:\windows"
         if        not over
.        append    "c:\command.com /c copy g:\data\" to taskname
.START PATCH 1.1 REPLACED LOGIC
.         execute    "!c:\command.com /c copy g:\DATA\exceptnc.lst \\NINs2\Laser5"
.         else
..         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
.         execute    "!c:\winnt\system32\cmd.exe /c copy g:\DATA\exceptnc.lst \\NINs2\Laser5"
...
         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"exceptnc.lst \\NINs2\Laser5"
         execute    TASKNAME
         else
.         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
         PACK       TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"exceptnc.lst \\NINs2\Laser5"
         execute    TASKNAME
.END PATCH 1.1 REPLACED LOGIC
         endif

.endpatch 1.4
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
.         execute    copycmd
         pause     "5"
         Keyin       *p10:12,"Ready to sort and Reindex the NINSTATS file ? ",str1
         cmatch      yes to str1
         if          equal
.begin patch 1.4
.         prepare     batch,"c:\work\stats.bat"
         path      exist,"c:\windows"
         if        not over
.START PATCH 1.1 REPLACED LOGIC
.         execute    "!c:\command.com /c copy g:\DATA\except.LST \\NINs2\Laser5"
.         execute    "!c:\command.com /c Copy g:\data\text\ninstats.dat g:\data\text\ninstats.tmp"         
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\sort32 g:\data\text\ninstats.tmp g:\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstats,L276 -191-196,1-4,5-34,121-130"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat2,L276 -238-243"
.         else
..         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
.         execute      "c:\winnt\system32\cmd.exe /c Copy g:\data\text\ninstats.dat g:\data\text\ninstats.tmp"         
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\sort32 g:\data\text\ninstats.tmp g:\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstats,L276 -191-196,1-4,5-34,121-130"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat2,L276 -238-243"
.........
.START PATCH 1.3.1 REPLACED LOGIC
.         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser5"
.         execute    TASKNAME
.         PACK       TASKNAME,"!c:\command.com /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"         
.         execute    TASKNAME
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute    TASKNAME
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L282 -1-4,117-130"
.         execute    TASKNAME
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L282 -238-243"
.         execute    TASKNAME
.         else
..         append    "c:\winnt\system32\cmd.exe /c copy g:\data\" to taskname
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L282 -1-4,117-130"
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L282 -238-243"
.         execute      TASKNAME
...............................................
                    PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"except.LST \\NINs2\Laser5"
                    execute    TASKNAME
                    PACK       TASKNAME,"!c:\command.com /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"         
                    execute    TASKNAME
                    PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,6,N,a,204,6,N,d,41,4,N,d,37,2,N,d,39,2,n,D) f(tab) w(c:) verbose"
                    execute    TASKNAME
                    PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-6,119-143"
                    execute    TASKNAME
                    PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -254-259"
                    execute    TASKNAME
          else
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,6,N,a,204,6,N,d,41,4,N,d,37,2,N,d,39,2,n,D) f(tab) w(c:) verbose"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-6,119-143"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -254-259"
                    execute      TASKNAME
.END PATCH 1.3.1 REPLACED LOGIC
.END PATCH 1.1 REPLACED LOGIC
.         write        batch,seq;"Copy g:\data\text\ninstats.dat g:\data\text\ninstats.tmp"
.         write        batch,seq;"\\SRV2008A\c\netutils\sort g:\data\text\ninstats.tmp g:\data\text\ninstats.dat  /s(1,4,N,a,187,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         write        batch,seq;"\\SRV2008A\c\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstats,L272 -187-192,1-4,5-34,117-126"
.         write        batch,seq;"\\SRV2008A\c\netutils\Sunidxnt g:\data\text\ninstats.dat,g:\data\index\ninstat2,L272 -234-239"
.         weof         batch,seq
.         close        batch
.         shutdown    "c:\work\stats.bat"
         endif
.end patch 1.4
         endif 
         stop         
         include     tncXio.inc
         include    statsio.inc
         include    nordio.inc
         include     ndatio.inc
.START PATCH 1.3.1 ADDED LOGIC
          include   COMPIO.INC
          include   CNTIO.INC
.END PATCH 1.3.1 ADDED LOGIC
         include    comlogic.inc     

