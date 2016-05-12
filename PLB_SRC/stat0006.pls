Fpc      equ        0
         include   common.inc
         include   cons.inc
         include   consacct.inc
.         include   tncXdd.inc
         include   Npkgdd.inc
         include   SXRFdd.inc
         include   statsdd.inc
         include   norddd.inc
         include   ndatdd.inc
          include  hp.inc
;PATCH1.0
	INCLUDE	TNCBAKDD.INC
;PATCH1.0
release   init     "1.2.1"       27JAN2005	ASH	NINSTATS CONVERSION
.release   init     "1.2"       09AUG2004	ASH	LOGO CONVERSION
.release   init     "1.1"       11Nov2003 DMB 	Added new tncbakdd/io also updated verbage for exclusions.
.release   init     "1 .0"       26Mar03 automated backend/cost app.
;release   init     "0.3"       12Jul02 DLH Use GetWinVer
;release   init     "0.2"       19NOV01 ASH CONVERTED STATSFILE
.release   init     "0.1"       05july2001 DLH NEW indices seetatsdd
.release   init     "0.002"       04OCT2000 ASH NEW SERVER ADDED
.release   init     "0.001"       ..17Mar00 (double aught) To apply the TNC Reconciliation file (final costs)
batch    file                 .created for temp use
RECSIN   FORM      6
STR256   DIM       300
.str13    dim       13
str90    dim       90
DECIMAL  FORM      8.6
PERCENT  INIT      "%"
cntrbflg FORM      "0"
input    file       
input2   ifile
TIME     INIT      "HH:MM:SS"
.offer    dim         3
minusflg dim        1
mqty     dim       10
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
nqty      form       7
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
backend1  form       5.2
backend   form       7.2
backend2  form       7.3
totback   form       7.2
stotcstm  form       7.2
spkgcostm form       9
stotalc   form       7.2
totcostw  form       7.2
costtemp  form       7.6

srccode   dim       14             1-10   key
Lstcode        dim        4    15-18
pkgcode        dim        8    19-26
lstcost        dim        12   list cost per m
Totalcost        dim        12   total cost per m
TotalcostM        dim        12   total cost per m
totcostm        dim        12   list cost per m
pkgcostm        dim        12   total cost per m
.work vars
incometmp      form      9.2
incometmp1     form      9
returntmp      form      5
pkgnum         dim       8
pkgdesc        dim       24
ack            dim       8
ackN           form      3.2
bre            dim       8
pkgfld         dim       8
check          form      5
check2          form      5
.

weeksout  dim        5
copycmd  DIM        60
         PACK       copycmd,"!c:\command.com /C copy ",NTWKPATH1,"exceptnc.lst lpt1: "
ninlist  dim        6
.
        open      input,"c:\work\tnccosts.csv",exclusive
        open      input2,"tncback"
        CLOCK       DATE TO TODAY
        CLOCK     TIME TO TIME
        move       "stat0006" to program
        move       "Names in the News CA" to compnme
        move       "Apply TNC Recon" to STITLE
        call       paint
.START PATCH .002 REPLACED LOGIC
.        splopen     "\\nts0\d\data\exceptnc_Recon.lst"
        PACK        STR35,NTWKPATH1,"exceptnc_Recon.lst"
        splopen     STR35
.END PATCH .002 REPLACED LOGIC
.        splopen     "c:\data\exceptnc_Recon.lst"
        print       hpport,*rptchar "*":80:
                    *n,*n,*n,hpbon:
                    *n,*1,"Program    : ",program,b2,"Date: ",today:
                    b2,"Time: ",time:
                    *n,*1,"Company    : ",compnme:
                    *n,*1,"User       : ":
                    *n,*1,"Deliver To : Henry Most",hpboff:
                    *n,*rptchar "*":80
         move      "08" to mm
         move      "01" to dd
         move      "02" to yy
         call      cvtjul
         move      juldays to check
        move       c1 to statpath
looper
  read       input,seq;*cdfon,b1:
                        b1:
                        str40:            dim       14     1-14
                        b1:                        dim        8    15-22
                        srccode:
                        Mqty:                        dim        4    23-26
                        pkgcode:
                        b1:                          dim       40    27-66   list name
                        pkgcostM:
                        b1:
                        lstcost:
                        b1:
                        TotalcostM:
                        totalcost

        goto       eoj if over
        scan       "AHOMQ990801001" in srccode
        call       debug if equal
        reset      srccode
        scan       "AHOMQ" in srccode
        goto       looper if not equal
        reset      srccode
        display    *p10:10,"records in ",recsin;
        add        c1 to recsin
        match      "SOURCECODE" in srccode
        goto       looper if equal
        match      "              " in srccode
        goto       looper if equal
        cmatch     b1 to srccode
        goto       looper if eos
.START PATCH 1.2.1 REPLACED LOGIC
.        move       "0173" to statmlr
        move       "000619" to statmlr
.END PATCH 1.2.1 REPLACED LOGIC
        rep        "$ " in lstcost
        rep        "$ " in TotalcostM
        rep        "$ " in Totalcost
        move       "," to str1
        call       removechar using lstcost,str1
        call       removechar using Totalcost,str1
        call       removechar using pkgcostm,str1
        call       removechar using Totalcostm,str1
        call       removechar using mqty,str1
        call       trim using srccode
        call       trim using lstcost
        call       trim using Totalcost
        call       trim using pkgcostm
        call       trim using Totalcostm
        call       trim using mqty
.................................................................

******************************************************************************
.PATCH1.1COMMENT OUT
.         pack        pkgfld from pkgcode
.         read        input2,pkgfld;pkgnum,pkgdesc,bre,ack
.PATCH1.1 END COMMENT OUT
.PATCH1.1
	move	PKGCODE to TNCBAKFLD
	call	trim	using	TNCBAKFLD
	call	TNCBAKKEY
.PATCH1.1
         goto        except2 if over
         PACK        statFLD FROM statmlr,srccode
.PATCH1.1COMMENT OUT
.        rep        "$ " in ack
.        call       removechar using ack,str1
.        call       trim using ack
.PATCH1.1 END COMMENT OUT
;PATCH1.1
.	CLEAR 	N32
.	MOVE	ack TO N32
.	goto 	except3 IF (N32 <= C0)
	goto 	except3 IF (TNCBAKCOST <= C0)

;PATCH1.1
         move      c1 to statpath
         call      statkey
         goto      except if over
JDTEST
         UNPACK   STATMDaTE INTO MM,DD,STR2,YY
         call     cvtjul
         move     juldays to check2
         if       (check2 >= check)
         goto     doback
         else
         goto     looper
         endif
doback
         move      statresp to backend
         move      statresp to backend2
         move      mqty to nqty
;         move      statresp to nqty
.PATCH1.1
.         move      ack to ackn
	MOVE	TNCBAKCOST TO ACKN
.PATCH1.1
         move      c0 to backend1
         move      c0 to costtemp
         move      c0 to stotcstm
         calc      backend1=(backend*.7+backend2*.9225*ackn)
         move      backend1 to totback
         add       totback to stotalc
         move      totalcost to totcostw
        add        totback to totcostw
          move      nqty to costtemp
         calc      stotcstm=(totcostw/(costtemp/1000))
        move       c0 to Statpckm                 package cost per m
        move       c0 to statImcst                in mail cost per m
        move       c0 to statlcpm                 list cost per m
         move      stotcstm to statimcst
         move      stotcstm to statpckm
        move       lstcost to statlcpm
        sub        statlcpm from statpckm
         call        statdel
         call        statwrt
.         call        statupd
         goto        looper
******************************************************************************
EXCEPT
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,srccode,b1,str40,b1,lstcode,b1,pkgcode:
                     *n,hpbon,"RECORD NOT FOUND IN STATS FILE",hpboff,*n
         add         c3 to lines
         GOTO        LOOPER

EXCEPT2
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,srccode,b1,str40,b1,lstcode,b1,pkgcode:
                     *n,hpbon,"PACKAGE NOT FOUND IN BACKEND COST FILE ",hpboff,*n
         add         c3 to lines
         GOTO        LOOPER

;PATCH1.1
EXCEPT3
         compare     c0 to page
         call        header if equal
         compare     "51" to lines
         call        Hd1 if equal
         call        hd1 if not less
         print       *1,srccode,b1,str40,b1,lstcode,b1,pkgcode:
                     *n,hpbon,"NO VALID BACKEND COST ASSOCIATED WITH THIS PACKAGE ",hpboff,*n
         add         c3 to lines
         GOTO        LOOPER
;PATCH1.1
Header   PRINT     hptmsr17,hpdupl,hptop,*F                .compressed
         move      c1 to page
.START PATCH 1.2 REPLACED LOGIC
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
.END PATCH 1.2 REPLACED LOGIC
         add         c1 to page
         move        c8 to lines
         return
eoj      print       *f,*flush
         splclose
         release
;begin patch .3
                    call                GetWinVer
                    If                  (osflag = c3 | osflag = c4)
         PACK       TASKNAME,"!c:\command.com /c copy ",NTWKPATH1,"exceptnc_recon.lst \\nts0\laser8"
         execute    TASKNAME
;         else
                    ElseIf                  (osflag = c1 | osflag = c5)
         PACK       TASKNAME,"!c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"exceptnc_recon.lst \\nts0\laser8"
         execute    TASKNAME
                    ElseIf                  (osflag = c6)
         PACK       TASKNAME,"!c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"exceptnc_recon.lst \\nts0\laser8"
         execute    TASKNAME
;end patch .3
.END PATCH .002 REPLACED LOGIC
         endif

         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         stop
.debug    return
.         include     tncXio.inc
         include   Npkgio.inc
         include   SXRFio.inc
         include    statsio.inc
         include    nordio.inc
         include     ndatio.inc
;PATCH1.1
	INCLUDE	TNCBAKIO.INC
;PATCH1.1
         include    comlogic.inc

