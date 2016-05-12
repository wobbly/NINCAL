pc      equ        0
         include   common.inc
         include   cons.inc
.         include   tncXdd.inc
         include   Npkgdd.inc
         include   SXRFdd.inc
         include   statsdd.inc
         include   norddd.inc
         include   ndatdd.inc
          include  hp.inc
.begin patch 1.3
          include  SlctClnDD.inc
.end patch 1.3
.START PATCH 1.5 ADDED LOGIC
          INCLUDE   NSEL2DD.INC
.END PATCH 1.5 ADDED LOGIC
.START PATCH 1.53 ADDED LOGIC
          INCLUDE   COMPDD.INC
          INCLUDE   cntDD.INC
.END PATCH 1.53 ADDED LOGIC
......stat0004A must be run first
Release   init      "1.53.1"  27JAN2005 ASH       CONVERTED NINSTATS
reldate   init      "January 27, 2005"
.Release  init      "1.53"    25OCT2004 ASH       CONVERTED MAILER PACKAGE TO 6 BYTES
.reldate  init      "October 25, 2004"
.release  init      "1.52"    ASH       09AUG2004 Logo Conversion
.reldate  init      "August 09, 2004"
.release  init      "1.51"JD             10JUN2004 turned back packing of statmdate from srccode. IF we don't get from TDMC
;release  init      "1.5"     ASH       29JAN2004  DATACARD CONVERSION
.reldate  init      "June 10, 2004"
.release   init     "1.4"          12Jul02 DLH added GetWinVer
;release   init     "1.3"          07Feb02 DLH added Select Clean
.release   init     "1.2"          19NOV01 ASH CONVERTED STATSFILE, CONVERTED NINPKG
.release   init     "1.1"          05July2001 DLH Change index keys and files see DD
.release   init     "1.06"        16May2001 DLH Dupe Lr cleanup
.release   init     "1.05"        10April2001 DLH Replace tncxdd
.release   init     "1.04"        06Jan2001 DLH Reformat/reindex no longer optional.
.release   init     "1.03"        03oct2000 ASH NEW SERVER ADDED
.release   init     "1.02"        28Aug2000 DLH change from stop to shutdown
.                                as program is invoked by stat0004a
.
.Release   init     "1.01"       ..15May2000 DLH turn off exception file writes.
.                                 and added exception 6 package not found.
.release   init     "1.00"       ..30Mar00 DLH save cost variable on stat file read
.Release   init     "0.003"       ..20Jan00 DLH if no lr drop tnc list desc into select
.                                .print tnc list name on exception report for no list exception
.release   init     "0.002"       ..13Jan00 (double aught) IF list = 2303 TNC move tncdesc1 to select.
.release   init     "0.001"       ..10Jan00 (double aught) TNC adds mailed qty
.release   init      "PRE"      .DEC99 DLH new code to apply TNC stats
.                               two versions needed  one to apply old dawson stuff version a
.                                                    one to apply new/current data
.                               THIS VERSION IS CURRENTLY TWEAKED FOR DMA text FILE INPUT
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
nrpci               dim        10
nrnci               dim        10
nrpcia              dim        9          was 8
nrncia              dim        9          was 8  dlh 03jun97
CstA9               dim        9
CstA                dim        10
pcaci               dim        10
Ncaci               dim        10
CTA                 dim        31
cost$               dim        6
nqty                dim        12
inv                 dim        10
lcpm                dim        7
type                dim        1
NAMresp             dim        10
NAMrev              dim        10
IAMresp             dim        10
IaMrev              dim        10
Assocresp           dim        9
assocrev            dim        9
assocrresp          dim       8
basresp             dim        9
BRResp              dim        8
BREv                dim        9
TAresp              dim        9
inmailcpm           dim        6
aggrindx            dim        4
premcost            dim        6
oldflag             form       1
page                form       4
lines               form       2
weeks               form       5
dupeflag            form       1           0=no dupe, 1 = dupe
Savelr        dim            6
.INput file
srccode             dim       14             1-10   key
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
OQTY1     DIM        9
.begin patch 1.06
.O2DES1    DIM        35
.end patch 1.06

weeksout  dim        5
.copycmd  init      "!c:\command.com /C copy \\nins1\e\data\exceptnc.lst \\NINs2\Laser3 "
.START PATCH 1.03 REPLACED LOGIC
.copycmd  init      "!c:\command.com /C copy \\nins1\e\data\exceptnc.lst lpt1: "
copycmd  DIM        60
         PACK       copycmd,"!c:\command.com /C copy ",NTWKPATH1,"exceptnc.lst lpt1: "
.END PATCH 1.03 REPLACED LOGIC
.ninlist  dim        6
.
         MOVE      "Apply DMA file" TO STITLE
         CMATCH    B1 TO PROGRAM       .CHAINED FROM DSINIT WITH INFO?
         IF        EOS                 .NO
         MOVE      "STAT0004" TO PROGRAM
getdate keyin      *p10:12,"returns thru mmddccyy :",statpdate
        keyin      *p10:14,"Date ok? ",*dv,statpdate,*dv,b10,str1
        rep        "nN" in str1
        cmatch     no to str1
        goto       getdate if equal
         else
         move      comment to statpdate
        keyin      *p10:14,"Date ok? ",*dv,statpdate,*dv,b10,str1
        rep        "nN" in str1
        cmatch     no to str1
        goto       getdate if equal
         ENDIF
        open      input,"c:\work\DMAin.csv",exclusive
.        open      input,"c:\work\DMA2000.csv",exclusive
.        open      input,"\\nins1\d\users\dherric\DMA1999a.csv",exclusive
.        open      input,"c:\data\DMA1999a.csv",exclusive
        CLOCK       DATE TO TODAY
        CLOCK     TIME TO TIME
.        prepare     except,"\\nins1\d\users\dherric\except.TNC",exclusive
.        prepare     except,"c:\data\except1.TNC",exclusive
.        read        except,seqeof;str1
        move       "stat0004" to program
        move       "Names in the News CA" to compnme
        call       paint
        splopen     "\\nins1\e\data\exceptnc.lst"
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
.temp turned off 3/5/02.
.        open       statflst,exclusi
.        open       statflst,exclusive
.        move       c1 to statflag
.        move       c1 to statflag2
.begin release .001
.looper  read       input,seq;*cdfon,srccode:   dim       14     1-14   key
.                       Lstcode:        dim        4    15-18
.                       pkgcode:        dim        8    19-26
.                        tstcode:        dim       8     27-34
.                        tstcell:        dim       8     35-42
.                       technique:      dim       2     43-44
.                        returnnw:
.                        incomenw:
.                        returnRN:
.                        incomeRN:
.                        returnRJ:
.                        incomeRJ:
.                        returnUR:
.                        incomeUR:
.                        returnAD:
.                        incomeAD:
.                        returnDO:
.                        incomeDO
looper  read       input,seq;*cdfon,srccode:   dim       14     1-14   key
                        Lstcode:        dim        4    15-18
                        pkgcode:        dim        8    19-26
                        tstcode:        dim       8     27-34
                        tstcell:        dim       8     35-42
                        technique:      dim       2     43-44
                        mqty:           dim       11
                        returnnw:       
                        incomenw:       
                        returnRN:       
                        incomeRN:       
                        returnRJ:       
                        incomeRJ:       
                        returnUR:       
                        incomeUR:       
                        returnAD:       
                        incomeAD:       
                        returnDO:       
                        incomeDO       
.end release .001
        
        goto       eoj if over
        move       c0 to dupeflag
.
        scan       "AHOMQ000401010" in srccode
.        scan       "Z027613TZ98" in srccode
        call       debug if equal
        display    *p10:10,"records in ",recsin;
        add        c1 to recsin
        match      "SOURCECODE" in srccode
        goto       looper if equal
        match      "              " in srccode
        goto       looper if equal
        cmatch     b1 to srccode
        goto       looper if eos
.START PATCH 1.53.1 REPLACED LOGIC 
.         move        "0173" to statmlr
         move        "000619" to statmlr
.END PATCH 1.53.1 REPLACED LOGIC 
        move       srccode to statsrce
        clear      str6
        clear      str4

        clear      statkycd
        move       lstcode to statkycd             .DLH 21Dec99
.DH 07Feb00   lets handle old source code as well
        clear      mm
        clear      yy
        cmatch     "Z" to srccode
        if         equal
.        call       debug
        unpack     srccode into str5,str3,str1,str2
        move       "01" to dd
        move       str2 to YY

                  if         (str1 = "W" & YY = "98")
                  move       c11 to mm
                  move       "24" to dd
                  goto        mdatedone
                  endif

                  if         (str1 = "W" & YY = "99")
                  move       c11 to mm
                  move       "01" to dd
                  goto        mdatedone
                  endif

                  if         (str1 = "Z" & YY = "99")
                  move       "12" to mm
                  move       "24" to dd
                  goto        mdatedone
                  endif

                  if         (str1 = "X")
                  move       c10 to mm
                  else
                  if         (str1 = "Y")
                move       c11 to mm
                else
                        if         (str1 = "Z")
                        move       "12" to mm
                        else
                        pack       mm from c0,str1
                        rep        zfill in mm
          endif
                  endif
                  endif
        else
.        unpack     srccode into str5,str4
        unpack     srccode into str5,str6
.        unpack     str4 into yy,mm
        unpack     str6 into yy,mm,dd
        endif
mdatedone
        if         (yy = "99" or YY = "98" or yy = "97")
        move       "19" to cc
        else
        move       "20" to cc
        endif
        pack       statmdate from mm,dd,cc,yy
........
        move       c0 to statresp
        move       c0 to statrev
.begin release .001
        move       c0 to statmqty
        call       trim using mqty
        move       mqty to statmqty
.end release .001

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomenw
        if         equal
        reset      incomenw
        call       trim using incomenw
        move       incomenw to incometmp
        add        incometmp to statrev
        else
        reset      incomenw
        call       trim using incomenw
        move       incomenw to incometmp1
        add        incometmp1 to statrev
        endif

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomeRN
        if         equal
        reset      incomern
        call       trim using incomern
        move       incomern to incometmp
        add        incometmp to statrev
        else
        reset      incomern
        call       trim using incomern
        move       incomern to incometmp1
        add        incometmp1 to statrev
        endif

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomeRJ
        if         equal
        reset      incomeRJ
        call       trim using incomeRJ
        move       incomeRJ to incometmp
        add        incometmp to statrev
.        move        incomerj to statrev
        else
        reset      incomeRJ
        call       trim using incomeRJ
        move       incomeRJ to incometmp1
        add        incometmp1 to statrev
        endif

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomeUR
        if         equal
        reset      incomeUR
        call       trim using incomeUR
        move       incomeUR to incometmp
        add        incometmp to statrev
        else
        reset      incomeUR
        call       trim using incomeUR
        move       incomeUR to incometmp1
        add        incometmp1 to statrev
        endif

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomeAD
        if         equal
        reset      incomeAD
        call       trim using incomeAD
        move       incomeAD to incometmp
        add        incometmp to statrev
        else
        reset      incomeAD
        call       trim using incomeAD
        move       incomeAD to incometmp1
        add        incometmp1 to statrev
        endif

        move       c0 to incometmp
        move       c0 to incometmp1
        scan       "." in incomeDO
        if         equal
        reset      incomeDO
        call       trim using incomeDO
        move       incomeDO to incometmp
        add        incometmp to statrev
        else
        reset      incomeDO
        call       trim using incomeDO
        move       incomeDO to incometmp1
        add        incometmp1 to statrev
        endif
.
        move       c0 to returntmp
        call       trim using returnnw
        move       returnnw to returntmp
        add        returntmp to statresp

        move       c0 to returntmp
        call       trim using returnRN
        move       returnRn to returntmp
        add        returntmp to statresp

.        add        returnRN to statresp
        move       c0 to returntmp
        call       trim using returnRJ
        move       returnRJ to returntmp
        add        returntmp to statresp
.        add        returnRJ to statresp

        move       c0 to returntmp
        call       trim using returnUR
        move       returnUR to returntmp
        add        returntmp to statresp
.        add        returnUR to statresp

        move       c0 to returntmp
        call       trim using returnad
        move       returnAD to returntmp
        add        returntmp to statresp
.        add        returnAD to statresp

        move       c0 to returntmp
        call       trim using returndo
        move       returnDO to returntmp
        add        returntmp to statresp
.        add        returnDO to statresp

.        move       package to statpanel
.        clear      statlr
.        clear      statkycd
.................................................................        
.begin patch 1.1a  ----move before cross ref stuffins
.begin patch 1.1
.         PACK        statFLD FROM NINLIST,statmlr,statcampn,statsrce
.START PATCH 1.53.1 REPALCED LOGIC
.         MOVE         "0173" TO STATMLR                 .JUST PARANOID
         MOVE         "000619" TO STATMLR                 .JUST PARANOID
.END PATCH 1.53.1 REPALCED LOGIC
         PACKkey        statFLD FROM statmlr,statsrce
.end patch 1.1
.
         compare   c1 to statflag
         call      statopen if not equal
         FILEPI    1;statFILE
.patch 10jan00 dma now has qty mailed
.         READ      statFILE,statFLD;*191,str6,*198,statmqty,*238,statlr          .added statmqty 14Dec99 TNC not currently sending
.         READ      statFILE,statFLD;*191,str6,*238,statlr          
.         READ      statFILE,statFLD;*35,str8,*191,str6,*238,statlr           ..24Jan00 retain maildate
..begin patch 1.0  30Mar00 retain cost variables ,statlcpm,statImcst,& *264,Statpckm           
.begin patch 1.1
.         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,*264,Statpckm
.         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,str12,*264,Statpckm
.START PATCH 1.2 REPLACED LOGIC
.         READ      statFILE,statFLD;*35,str8,*57,statpanel,*191,str6,*222,statlcpm,statImcst,statlr,str12,*264,Statpckm,*277,statpckcde
masread
.START PATCH 1.53.1 REPLACED LOGIC 
.          READ      statFILE,statFLD;*35,str8,*57,statpanel,*142,statldes,*202,str6,*233,statlcpm,statImcst,statlr,str25,*288,Statpckm,*303,statpckcde
          READ      statFILE,statFLD;*37,str8,*59,statpanel,*144,statldes,*204,str6,*235,statlcpm,statImcst,statlr2,statlr,str25,*293,Statpckm,*308,statpckcde
.END PATCH 1.53.1 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.end patch 1.1
.end. patch 1.0  30Mar00 retain cost variables ,statlcpm,statImcst,& *264,Statpckm           
         if        not over
         move      str6 to statlist       *retain NIN list number (it may have been manually corrected
..22Feb01 DLH
         move      str8 to statmdate      *retain maildate - no longer in dmatext file as part of source code
.begin patch 1.1
.START PATCH 1.2 REPLACED LOGIC
.        move       str12 to statkycd      *retain corrected list code
.        call       trim using str12
.        move       str12 to lstcode
         move       str25 to statkycd      *retain corrected list code
        call       trim using str25
.        move       str25 to lstcode
.END PATCH 1.2 REPLACED LOGIC
        else                          .its over
        clear      statlr
        call       getlist
        endif

         match      b25 to statkycd
                              if         equal
                              clear      statkycd
                              move       lstcode to statkycd
                              endif
.        call       getlist
          if     (statlist = "002303")          .if its the house file
.label for debugging
.          goto    looper                         . added 7/09/01 jd
.         no not looper as they want it to be in the history file   DLH 18July2001
TNCIN
        clear       Statxfld
        call        trim using lstcode
        packkey     STATXFLD from statmlr,lstcode
        move        c1 to Statxpath
                      call        statxkey
        move   Statxdesc1 to statsel           .show the select
          call    getpackage
          noreturn
          goto    wksout
          endif
        call       getpackage

.clean and prep
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         MOVE      "01R",aKEY1
.START PATCH 1.53.1 REPLACED LOGIC - TEMPORARY LOGIC
.         PACK      NORDFLD1 FROM AKEY1,statmlr
          pack      COMPFLD,statmlr
          move      "TNCIN-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
         PACK      NORDFLD1 FROM AKEY1,COMPOLDMLR
.END PATCH 1.53.1 REPLACED LOGIC - TEMPORARY LOGIC 
         MOVE      "02R",aKEY1
         PACK      NORDFLD2 FROM AKEY1,statLIST
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         Clear     olrn
         move      b8 to str8
         goto      getord
.end patch 1.1

******************************************************************************
.
getlist
.        clear       ninlist
.begin patch .003
.        move        "list not found" to tncdesc1
.end patch .003
        clear       statlist
        clear       statldes
.        clear       tncxfld
.START PATCH 1.53.1 REPLACED LOGIC
.         move        "0173" to statmlr
         move        "000619" to statmlr
.END PATCH 1.53.1 REPLACED LOGIC
        clear       Statxfld
        call        trim using lstcode
.        count       n2 in lstcode
.        if          (n2 = c4)     .proper 4 byte list code
        packkey     STATXFLD from statmlr,lstcode
.        else
.                clear       Statxfld
.                 packkey     STATXFLD from statmlr,lstcode
.        endif

        move        c1 to Statxpath
         clear       Statxlist
         clear       statlist
.         clear       ninlist
.
.
.START PATCH 1.53.1 REPLACED LOGIC
.         match       "0173" to statmlr
         match       "000619" to statmlr
.END PATCH 1.53.1 REPLACED LOGIC
         if          equal
.          display    *p1:23,*el,TNCxfld
.          cmatch      b1 to tncxfld
          cmatch      b1 to Statxfld
          if          eos
.          move        "000" to tncxfld
          packkey     STATXFLD from Statmlr,z3
          endif
.          call        TNCxkey
          call        Statxkey
         if          over
.         move        "000000" to TNCxlist
         move        "000000" to Statxlist
.         clear        TNCxstat
.          clear         TNCdesc                    .    nin desc
.        move         lname to TNCdesc1           .TNC DESC - Key
.        move        lname to TNCxfld1 pck
.        move        c2 to tncxpath
.        call        tncxkey
.         display     *p10:14,tncxfld," ",lname," ",tncxlist
.         display     *p10:14,tncxfld," ",tncxlist
         display     *p10:14,Statxfld," ",Statxlist
.         keyin       *p10:15," not found Enter it? ",str1
.         match       "000000" to tncxlist
         match       "000000" to Statxlist
                 if          equal
                 move        no to str1
                 else
                 move        yes to str1
                 endif
         display     *p10:15," not found Enter it? ",str1,*p10:15,*el
         rep         "nN" in str1
         cmatch       no to str1
         goto         skipit if equal
.         keyin       *p10:16,*el," Code = ",str3
.         move        str3 to tncxfld
.         move        tncxfld to TNCxLcode
         unpack       Statxfld to StatxMLr,Statxcode
.         move        c1 to tncxpath
         move        c1 to Statxpath
.         call        tncxwrt
         call        Statxwrt
         clear       stattype
         goto        skipit1
skipit
          noreturn
          call   getpackage             .we are going to print exception - lets check package first
          goto        except
         endif
skipit1
.begin patch .003      move TNC list name to select so if we have no order LR
.         move         TNCdesc1 to statsel
         move         statxdesc1 to statsel
.end patch .003        maybe we can still ID
         display    *p1:23,*el,Statxlist
.         move        Statxlist to ninlist
                 move        StatXlist to statlist
.          if     (ninlist = "002303")          .if its the house file
          if     (statlist = "002303")          .if its the house file
inhouse
.label for debugging
.          goto    looper                         . added 7/09/01 jd
.         no not looper as they want it to be in the history file   DLH 18July2001
          call    getpackage
          noreturn
          move   Statxdesc1 to statsel           .show the select
          goto    wksout
          endif
..........................................................................................
.
.          if     (ninlist = "000000" or ninlist = "      ")
          if     (statlist = "000000" or statlist = "      ")
                 move         Statxdesc1 to statldes
                 goto        except
                 else
                 move        StatXlist to ndatfld
                 move        StatXlist to statlist
                 move        c1 to ndatpath
                 move        c3 to ndatlock
                 rep         zfill in ndatfld
                 call        ndatkey
                         if          not over
                         move        olstname to statldes
                         endif
                 endif
         endif
         pack      statcampn from b25,b5


.begin patch 1.1a  ----move before cross ref stuffins
..begin patch 1.1
..         PACK        statFLD FROM NINLIST,statmlr,statcampn,statsrce
.         PACK        statFLD FROM statmlr,statsrce
..end patch 1.1
         rep         zfill in statlist
         return
getpackage
         reset      statpckcde
         scan       pkgcode in statpckcde
              if            equal
              reset         statpckcde
          return
              endif
.
.START PATCH 1.2 REPLACED LOGIC
.         packkey    Npkgfld from statmlr,pkgcode
.         move       pkgcode to statpckcde
.          clear      statpanel
.         call       Npkgkey
.         if         not over
.         move       Npkgdesc to statpanel
.         else
.         call       except6
.         endif
...........
          clear     NPKGFLD2
          clear     NPKGFLD3
.START PATCH 1.53.1 REPLACED LOGIC
..START PATCH 1.53 REPLACED LOGIC
..        pack      NPKGFLD1,"01X",STATMLR
.         move      "COMPKEY3",Location
.         pack      COMPFLD3,STATMLR
.         pack      KeyLocation,"Key: ",COMPFLD3
.         call      COMPKEY3
.         pack      NPKGFLD1,"01X",COMPNUM
..END PATCH 1.53 REPLACED LOGIC
          pack      NPKGFLD1,"01X",STATMLR
.END PATCH 1.53.1 REPLACED LOGIC
          pack      NPKGFLD4,"04X",PKGCODE
          move      PKGCODE,STATPCKCDE
          clear     STATPANEL
          move      C1,NPKGPATH
          move      "NPKGAIM",Location
          pack      KeyLocation,"Key: ",NPKGFLD1,NPKGFLD4
          call      NPKGAIM
          if not over
                    move      NPKGPNAME,STATPANEL
          else
                    call      except6
          endif
.END PATCH 1.2 REPLACED LOGIC
          return
.
getord
.                  
.clean and prep
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
..patch 10jan00 dma now has qty mailed
..         READ      statFILE,statFLD;*191,str6,*198,statmqty,*238,statlr          .added statmqty 14Dec99 TNC not currently sending
..         READ      statFILE,statFLD;*191,str6,*238,statlr
..         READ      statFILE,statFLD;*35,str8,*191,str6,*238,statlr           ..24Jan00 retain maildate
...begin patch 1.0  30Mar00 retain cost variables ,statlcpm,statImcst,& *264,Statpckm
..begin patch 1.1
..         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,*264,Statpckm
.         READ      statFILE,statFLD;*35,str8,*191,str6,*222,statlcpm,statImcst,statlr,str12,*264,Statpckm
..end patch 1.1
..end. patch 1.0  30Mar00 retain cost variables ,statlcpm,statImcst,& *264,Statpckm
.         if        not over
.         move      str6 to statlist       *retain list number (it may have been
...22Feb01 DLH
.         move      str8 to statmdate      *retain maildate - no longer in dmatext file as part of source code
..begin patch 1.1
.        move       str12 to statkycd      *retain corrected list code
.        call       trim using str12
.        move       str12 to lstcode
..end patch 1.1
.end patch 1.1a
..22Feb01 DLH
.                                         *nmanually cleaned. DLH 16Oct97         
.         move      str8 to statmdate                          ..added 24jan00, turned off 14Nov00
         rep       zfill in statlist
.test test test
         if        (statlr = "" or statlr = "      " or statlr = b1)
         clear     statlr
         goto      findorder
         endif
.end test test test
         type      statlr
         goto      needclean if equal                     *we have an lr so skip order file search
.         endif
.
.        goto       needclean   .temp dlh
.
findorder
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         MOVE      "01R",aKEY1
.START PATCH 1.53.1 REPLACED LOGIC - TEMPORARY LOGIC 
.         PACK      NORDFLD1 FROM AKEY1,statmlr
          pack      COMPFLD,statmlr
          move      "findorder-COMPKEY",Location
          pack      KeyLocation,"Key: ",COMPFLD
          call      COMPKEY
         PACK      NORDFLD1 FROM AKEY1,COMPOLDMLR
.END PATCH 1.53.1 REPLACED LOGIC - TEMPORARY LOGIC 
         MOVE      "02R",aKEY1
         PACK      NORDFLD2 FROM AKEY1,statLIST
         CLEAR     NORDFLD3
         CLEAR     NORDFLD4
         move      b6 to statlr
         Clear     olrn
         move      b8 to str8
         MOVE      C2 TO NORDPATH     .SET ACCESS TO AIM.
         UNPACK   STATMDaTE INTO MM,DD,STR2,YY
         move   c0 to dupeflag
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
.new cleanup
.begin patch 1.06
.                 MOVE      OQTY TO OQTY1
.                 MOVE      O2DES TO O2DES1
.end patch 1.06
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
.new cleanup
.begin patch 1.06
.                 MOVE      OQTY TO OQTY1
.                 MOVE      O2DES TO O2DES1
.end patch 1.06
.                     IF       EQUAL
                               type   STATLR
                               if     equal
                               call   except3
                               else
                               move   c0 to dupeflag
                               MOVE     OLRN TO STATLR
                               endif
.                               goto     EXCEPT3
..                             ELSE
.                              ENDIF
.                     else
                     goto     nordloop
.                    ENDIF
         endif
needclean
         type      statlr
         if        not equal
         call      except2
         goto      wksout
         endif
.         goto      wksout if not equal
         cmatch    b1 to statlr
         if        eos
         call      except2
         goto      wksout
         endif
.         goto      wksout if eos
         MOVE      C1 TO NORDPATH     .SET ACCESS TO isi
         clear     nordfld
         move      statlr to nordfld
         rep       zfill in nordfld
         call      nordkey
         call      except2 if over
.START PATCH 1.5 ADDED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.END PATCH 1.5 ADDED LOGIC
.begin patch 1.06
.         MOVE      OQTY TO OQTY1
.         MOVE      O2DES TO O2DES1
.end patch 1.06
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
.START PATCH 1.5 REPLACED LOGIC
.               packkey        SlctClnFld from O2des
               packkey        SlctClnFld from NSEL2NAME
.END PATCH 1.5 REPLACED LOGIC
               call           SlctClnKey
               if             not over
               move           SlctClnText to Statsel
               else
.START PATCH 1.5 REPLACED LOGIC
.               move           O2Des to Statsel
               move           NSEL2NAME to Statsel
.END PATCH 1.5 REPLACED LOGIC
               endif
.         move      O2DES to statsel        .show list select
.end patch 1.3


.calc  weeksout
wksout
         unpack    statmdate into mm,dd,str2,yy
         call      cvtjul
         move      juldays to weeks
         unpack    statpdate into mm,dd,str2,yy
         call      cvtjul
         sub       weeks from juldays
         divide    c7 into juldays
         move      juldays to statwkso
         if         (juldays < c0)
         move       c0 to statwkso
         endif


.




.         endif
.begin patch 1.1
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
.
.chkpack
.         reset      statpckcde
.         scan       pkgcode in statpckcde
.         goto       writer if equal
..
.         packkey    Npkgfld from statmlr,pkgcode,b2
.         move       pkgcode to statpckcde
.          clear      statpanel
.         call       Npkgkey
.         if         not over
.         move       Npkgdesc to statpanel
.         else
.         call       except6
.         endif
.
.writer
         call        statwrt
         clear       statsel
         clear       statldes
        clear        stattype
         clear       statlr
         goto        looper
.end patch 1.1

EXCEPT
.         write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY,"No List ",tncxfld 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL:
                     *n,statpanel,hpbon,"No List Number Found ",hpboff,Statxfld,B1,STATMQTY,*n
         add         c3 to lines
         GOTO        LOOPER

EXCEPT2 
.        write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY," no lr" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
                     *n,statpanel,hpbon," No LR",hpboff,b1,statlr,b1,olrn,B1,STATMQTY,*n
         add         c4 to lines
         return
       
EXCEPT3 
.        write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY," to many lrs" 
         compare     c0 to page
         call        header if equal
         if          (lines = "51" or lines > "51")  
         call        Hd1
         endif
         branch      dupeflag to except3a
         move        c1 to dupeflag
.begin patch 1.06
         move        olrn to savelr
         packkey     nordfld from statlr
         move        c1 to nordpath
         call        nordkey
.end patch 1.06
.START PATCH 1.5 ADDED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.END PATCH 1.5 ADDED LOGIC
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
.begin patch 1.06
.                     *n,hpbon,"to MANY LR'S ",STATLR,B1,oqty1,b1,o2des1,STATMQTY:
.end patch 1.06
.START PATCH 1.5 REPLACED LOGIC
.                     *n,hpbon,"to MANY LR'S ",STATLR,B1,oqty,b1,o2des,STATMQTY;
                     *n,hpbon,"to MANY LR'S ",STATLR,B1,oqty,b1,NSEL2NAME,STATMQTY;
.END PATCH 1.5 REPLACED LOGIC
         packkey     nordfld from savelr
         call        nordkey
.START PATCH 1.5 ADDED LOGIC
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
.END PATCH 1.5 ADDED LOGIC
.end patch 1.06
.START PATCH 1.5 REPLACED LOGIC
.         print       *n,*1,OLRN,b1,oqty,b1,o2des,hpboff
         print       *n,*1,OLRN,b1,oqty,b1,NSEL2NAME,hpboff
.END PATCH 1.5 REPLACED LOGIC
         add         c5 to lines
.test.test.test 31May01
          clear      o2des
          clear      statsel
.test.test.test 31May01
         return
except3a
.begin patch 1.06
         packkey     nordfld from olrn
         move        c1 to nordpath
         call        nordkey
.end patch 1.06
.START PATCH 1.5 REPLACED LOGIC
.         print       *n,hpbon,*1,OLRN,b1,oqty,b1,o2des,hpboff
          packkey   NSEL2FLD,"1",OLRN
          move      "NSEL2KEY",Location
          pack      KeyLocation,"Key: ",NSEL2FLD
          call      NSEL2KEY
          if over
                    move      O2DES,NSEL2NAME
          endif
         print       *n,hpbon,*1,OLRN,b1,oqty,b1,NSEL2NAME,hpboff
.END PATCH 1.5 REPLACED LOGIC
         add         c1 to lines
         return

.         goto        stattest
EXCEPT4
.  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel," Stats Bad?",B1,STATMQTY
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL:
                     *n,statpanel,hpbon," Check stats":
                     statresp,b1,statrev,hpboff,B1,STATMQTY,*n
         add         c3 to lines
         return
EXCEPT5
.  write       except,seqeof;ninlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
.                     statldes,b1,statsel,B1,STATMQTY," Stats Bad?" 
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes,b1,statseL:
                     *n,statpanel,hpbon," Check REVENUE & Qty Mailed":
                     statresp,b1,statrev,hpboff,B1,STATMQTY,*n
         add         c3 to lines
         return
Except6       
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,statlist,b1,statmlr,b1,statcampn,b1,statmdate,b1,statsrce:
                     statldes:
                     *n,*68,statseL:
                     *n,statpanel,hpbon," No Package desc",hpboff,b1,statlr,b1,olrn,B1,pkgcode,*n
         add         c4 to lines
         return


Header   PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
         move      c1 to page
.START PATCH 1.52 REPLACED LOGIC
.HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
.                      *52,"NAMES IN THE NEWS CA., INC.":
.                      hpt700,"DATE: ",TODAY:
.                   *N,*01,"(0173)":
.                      hpt275,"Statistical Data Exception Report:":
.                      hpt700,"PAGE:    ",PAGE:
.                   *N,"Weeks Out : ",statwkso:
.                      hpt275,"Input: DMASTAT",statpdate:
.                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
.                   hpt325,"TNC##",hpt350,"List Name",hpboff,*n
HD1      PRINT     *F,*n,hpbon,"CONFIDENTIAL":
                      *52,"NAMES IN THE NEWS":
                      hpt700,"DATE: ",TODAY:
                   *N,*01,"(0173)":
                      hpt275,"Statistical Data Exception Report:":
                      hpt700,"PAGE:    ",PAGE:
                   *N,"Weeks Out : ",statwkso:
                      hpt275,"Input: DMASTAT",statpdate:
                   *n,*1,"List##",b1,"Mlr##",hpt075,"Campaign",hpt250,"Mail Date":
                   hpt325,"TNC##",hpt350,"List Name",hpboff,*n
.END PATCH 1.52 REPLACED LOGIC
         add         c1 to page
         move        c8 to lines
         return
eoj
.      weof        except,seqeof
.         close       except
         print       *f,*flush
         splclose
         release
.begin patch 1.4
.         .execute    "copy \\nins1\e\data\exceptnc.lst lpt1:"
;begin patch 1.4
                    call                GetWinVer
;         path      exist,"c:\windows"
;         if        not over
.        append    "c:\command.com /c copy \\nins1\e\data\" to taskname
                    If                  (osflag = c3 | osflag = C4)
         execute    "!c:\command.com /c copy \\nins1\e\data\exceptnc.lst \\NINs2\Laser8"
;         else
                    ElseIf                  (osflag = c1 | osflag = C5)
.         append    "c:\winnt\system32\cmd.exe /c copy \\nins1\e\data\" to taskname
         execute    "!c:\winnt\system32\cmd.exe /c copy \\nins1\e\data\exceptnc.lst \\NINs2\Laser8"
                    ElseIf                  (osflag = c6)
         execute    "!c:\windows\system32\cmd.exe /c copy \\nins1\e\data\exceptnc.lst \\NINs2\Laser8"
;end patch 1.4
         endif

.         execute    "F:\PUBLIC\NPRINT.exe \\nins1\e\data\exceptnc.lst Q=LASER5 f=0 nb S=SRV2008A_fpnw "
.endpatch 1.4
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
.         execute    copycmd
         pause     "5"
         move       yes to str1
         Display   *p10:12,"Ready or not I Am going to sort and Reindex the NINSTATS file!",*w,*b
         cmatch      yes to str1
         if          equal
;begin patch 1.4
                    call                GetWinVer
;         path      exist,"c:\windows"
;         if        not over
                    If                  (osflag = c3 | osflag = c4)
.         execute    "!c:\command.com /c Copy \\nins1\e\data\text\ninstats.dat \\nins1\e\data\text\ninstats.tmp".sorting by mlr list & date???
.START PATCH 1.03 REPLACED LOGIC
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\sort32 \\nins1\e\data\text\ninstats.tmp \\nins1\e\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstats,L276 -191-196,1-4,5-34,117-130"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat2,L276 -238-243"
.         execute    "!c:\command.com /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat3,L276 -1-4,117-130"
.         else
.         execute      "c:\winnt\system32\cmd.exe /c Copy \\nins1\e\data\text\ninstats.dat \\nins1\e\data\text\ninstats.tmp"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\sort32 \\SRV2008A\d\data\text\ninstats.tmp \\SRV2008A\d\data\text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstats,L276 -191-196,1-4,5-34,117-130"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat2,L276 -238-243"
.         execute      "c:\winnt\system32\cmd.exe /c \\SRV2008A\c\netutils\Sunidxnt \\SRV2008A\d\data\text\ninstats.dat,\\SRV2008A\d\data\index\ninstat3,L276 -1-4,117-130"
.............
.START PATCH 1.2 REPLACED LOGIC
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute    TASKNAME
..begin patch 2.12
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -1-4,117-130"
..         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -191-196,1-4,5-34,117-130"
..end patch 2.12
.         execute    TASKNAME
.         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L286 -238-243"
.         execute    TASKNAME
..begin patch 2.12
..         PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat3,L286 -1-4,117-130"
..         execute    TASKNAME
..end patch 2.12
.         else
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,191,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.         execute      TASKNAME
..begin patch 2.12
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -1-4,117-130"
..         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L286 -191-196,1-4,5-34,117-130"
..end patch 2.12
.         execute      TASKNAME
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L286 -238-243"
.         execute      TASKNAME
......................
          goto       eojjd
.START PATCH 1.53.1 REPLACED LOGIC
.          PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.          execute    TASKNAME
.                  PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-4,117-141"
.          execute    TASKNAME
.                  PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
.          execute    TASKNAME
.;         else
.                    Elseif              (osflag = c1 | osflag = c5)
.                  PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.          execute      TASKNAME
.                  PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.          execute      TASKNAME
.                  PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-4,117-141"
.          execute      TASKNAME
.                  PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
.          execute      TASKNAME
..............................................
                    PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,6,N,a,204,6,N,d,41,4,N,d,37,2,N,d,39,2,n,D) f(tab) w(c:) verbose"
                    execute    TASKNAME
                    PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-6,119-143"
                    execute    TASKNAME
                    PACK       TASKNAME,"!c:\command.com /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -254-259"
                    execute    TASKNAME
          Elseif (osflag = c1 | osflag = c5)
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,6,N,a,204,6,N,d,41,4,N,d,37,2,N,d,39,2,n,D) f(tab) w(c:) verbose"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-6,119-143"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -254-259"
                    execute      TASKNAME
.END PATCH 1.53.1 REPLACED LOGIC
.END PATCH 1.2 REPLACED LOGIC
.begin patch 2.12
.         PACK         TASKNAME,"c:\winnt\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat3,L286 -1-4,117-130"
.         execute      TASKNAME
.end patch 2.12
.END PATCH 1.03 REPLACED LOGIC
.START PATCH 1.53.1 REPLACED LOGIC
.                    Elseif              (osflag = c6)
.                  PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
.          execute      TASKNAME
.                  PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,4,N,a,202,6,N,d,39,4,N,d,35,2,N,d,37,2,n,D) f(tab) w(c:) verbose"
.          execute      TASKNAME
.                  PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-4,117-141"
.          execute      TASKNAME
.                  PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -249-254"
.          execute      TASKNAME
...........................................
          Elseif (osflag = c6)
                    PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c Copy ",NTWKPATH1,"text\ninstats.dat ",NTWKPATH1,"text\ninstats.tmp"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c ",NTWKPATH2,"sort32 ",NTWKPATH1,"text\ninstats.tmp ",NTWKPATH1,"text\ninstats.dat  /s(1,6,N,a,204,6,N,d,41,4,N,d,37,2,N,d,39,2,n,D) f(tab) w(c:) verbose"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstats,L501 -1-6,119-143"
                    execute      TASKNAME
                    PACK         TASKNAME,"c:\Windows\system32\cmd.exe /c ",NTWKPATH2,"Sunidxnt ",NTWKPATH1,"text\ninstats.dat,",NTWKPATH1,"index\ninstat2,L501 -254-259"
                    execute      TASKNAME
.END PATCH 1.53.1 REPLACED LOGIC
;end patch 1.4
         endif
         endif
.begin patch 1.02
.         stop
eojjd
         shutdown
.end patch 1.02
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
.START PATCH 1.5 ADDED LOGIC
          INCLUDE   NSEL2IO.INC
.END PATCH 1.5 ADDED LOGIC
.START PATCH 1.53 ADDED LOGIC
          INCLUDE   COMPIO.INC
          INCLUDE   cntio.INC
.END PATCH 1.53 ADDED LOGIC
         include    comlogic.inc
