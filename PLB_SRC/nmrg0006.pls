PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   norddd.inc
.patch1.27
                              include   compdd.inc
                              include   cntdd.inc
.         include   nmlrdd.inc
.patch1.27
         INCLUDE   NMRGDD.inc
         include   ninvdd.inc
IN       FORM      5
APPLIED  FORM      5
first    init      "Y"
eop      form      "58"
lines    form      2
page     form      2
TDMCLR   DIM       10
recno    dim       2
lname    dim       28
IQTY     dim       7
convers  dim       7
ncoa     dim       7
dma      dim       7
intra    dim       7
dim23    dim       23
nonpers  dim       7
net      dim       7
dim14    dim       14
dim24    dim       24
lrno     dim       6
elim     dim       7
notused  dim       7
disf     dim       7
.patch 1.28 Code Added
DPVomits dim                   7 
.Patch 1.28 Code Added
.Start Patch #1.1 - added undeclared vars
fill3    dim       3
dim18    dim       18
dim12    dim       12
.End Patch #1.1 - added undeclared vars
INPUT    FILE      var=460
skipped  file      var=449
.skipped  file      var=441
skipcnt  form      5
.patch1.26
NUM       FORM      1
.patch1.26
Release   Init      "1.30"    DLH Skip if no order
reldate   Init      "21 April 2008"
.release  init      "1.29"         JD   09Dec2004   Do not allow DPV omits.
.release  init      "1.28"        DMB   17JUN2004 added code to allow for DPV omits from PIDI, add eliminator hits.
.release  init      "1.27"        DMB   26MAY2004 Mailer Conversion
.release  init      "1.26"    DMB 08SEO2003 Added code to allow adding of billed records.
.release  init      "1.25"        JD  27feb2003 upd to handle diff file formats.
.release  init      "1.22"        DLH  12Jul2002 Use GetWinVer
.release  init      "1.21"        JD  23mar2001 updated to use flat no csv file.
.release  init      "1.2"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.12"        JD28Aug00 read csv file format.
.release  init      "1.1"       ASH 08oct98 NINMLR,NINRTN,NINPAY Y2K Recompile
.release  init      "1.0"       JDaug2098
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      "NMRG0006" TO PROGRAM
         MOVE      "APPLY PIDI MERGE INFO" TO STITLE
         CLOCK    DATE TO TODAY
         CALL      PAINT
.START PATCH 1.2 REPLACED LOGIC
.         splopen   "g:\data\nmrgpidi.lst"
.         open      skipped,"g:\data\text\mrgeskip.ped"
         pack      str35,NTWKPATH1,"nmrgpidi.lst"
         pack      str45,NTWKPATH1,"text\mrgeskip.ped"
         splopen   STR35
         open      skipped,STR45
.END PATCH 1.2 REPLACED LOGIC
         move       c1 to nordpath
         move       c1 to ninvpath
         call       header
.START PATCH 1.2 REPLACED LOGIC
.         OPEN      INPUT,"g:\data\pidi.csv"
.patch1.26
         MOVE       NO TO STR1
         KEYIN     *P10:12,"APPLY ALREADY BILLED RECORDS ?? ",*uc,*T20,STR1
         REPLACE   "Y1N2" IN STR1
         MOVE      STR1,NUM
.patch1.26
        pack      str35,NTWKPATH1,"pidi.csv"
.        pack      str35,NTWKPATH1,"pidi.prn"
         OPEN      INPUT,STR35
.END PATCH 1.2 REPLACED LOGIC

LOOP
.         READ      INPUT,seq;lrno:
.                         dim12:
.                        dim18:
.                        lname:
.                        IQTY:
.                        fill3:
.                        intra:
.                        fill3:
.                        ncoa:
.                        fill3:
.                        nonpers:
.                                                                               fill3:                                                                          ;patch1.28 Code Added
.                                                                               DPVomits:                                                             ;patch1.28 Code Added
.                        fill3:
.                        dma:
.                        fill3:
.                        elim:
.                        fill3:
.                       notused:
.                        fill3:
.                        net
         READ      INPUT,seq;*cdfon,lrno:
                         dim12:
                         dim12:                         
                        lname:
                        IQTY:
                        intra:
                        ncoa:
                        nonpers:
                              DPVomits:                                                             ;patch1.28 Code Added
                        dma:
                        elim:
                       notused:
                        net

         GOTO      STOP IF OVER
         ADD       C1 TO IN
         DISPLAY   *P10:10,"RECORDS READ : ",IN
         type      lrno
         goto      loop if not equal
         call      wipemrg
         move      lrno to nmrglr
         MATCH     B6 TO NMRGLR
         GOTO      LOOP IF EQUAL

.         scan      comma in iqty
.         if        equal
.         reset     iqty
         call       trim using iqty
.         move      iqty to str6
.         move      str6 to nmrgrqty
.         move      str6 to nmrgiqty
.         clear     str6
.         else
.         reset     iqty
.         move      iqty to nmrgrqty
.         move      iqty to nmrgiqty
.         endif
.         scan      comma in ncoa
.         if        equal
.         reset     ncoa
         call       trim using ncoa
.         move      ncoa to str6
.         move      str6 to ncoamnf
.         clear     str6
.         else
.         reset     ncoa
.         move      ncoa to ncoamnf
.         endif
.         scan      comma,dma
. .        if        equal
.         reset     dma
         call      trim using dma
.         move      dma to str6
.         move      str6 to nmrgdma
.         clear     str6
.         reset     dma
.         move      dma to nmrgdma
.         endif
.         scan      comma,intra
.         if        equal
.         reset     intra
         call      trim using intra
.         move      intra to str6
.         move      str6 to nmrgid
.         clear     str6
.         else
.         reset     intra
.         move      intra to nmrgid
.         endif
.         scan      comma,elim
.         if        equal
.         reset     elim
         call      trim using elim
.         move      elim to str6
.         move      str6 to nmrgelim
.         clear     str6
.         else
.         reset     elim
.         move      elim to nmrgelim
.         endif
.         scan      comma,notused
.         if        equal
.         reset     notused
         call      trim using notused
.         move      notused to str6
.         move      str6 to nmrgudup
.         clear     str6
.         else
.         reset     notused
.         move      notused to nmrgudup
.         endif
.         scan      comma,nonpers
.         if        equal
.         reset     nonpers
         call      trim using nonpers
.         move      nonpers to str6
.         move      str6 to nmrgnper
.         clear     str6
.         else
.         reset     nonpers
.         move      nonpers to nmrgnper
.         endif
.         scan      comma,notused
.         if        equal
.         reset     notused
.         call      removechar using notused,comma
.         move      notused to str6
.         move      str6 to nmrgudup
.         clear     str6
.         else
.         reset     notused
.         move      notused to nmrgudup
.         endif
.         scan      comma,net
.         if        equal
.         reset    net
         call      trim using net
.         move      net to str6
.         move      str6 to nmrgnet
.         clear     str6
.         else
.         reset     net
.         move      net to nmrgnet
.         endif
.              move     lrno to nordfld
.              call     nordkey
.              if       (omlrnum = "0055")
.         move      elim to nmrgdisf
.         else
.         move      elim to nmrgelim
.         endif
.patch1.28
         call      trim using DPVOmits                                                    ;not in use yet
.patch1.28
         move      iqty to nmrgrqty
         move      iqty to nmrgiqty
         move      ncoa to ncoamnf
         move      dma to nmrgdma
         move      intra to nmrgid
         move      elim to nmrgelim                   ;turned back on 6/25/04 not sure why turned off.
         move      notused to nmrgudup
.         move      disf to nmrgdisf
.Start Patch #1.1 - remmed and replaced line
.         move      nonpers to nmrgnpern
         move      nonpers to nmrgnper
.End Patch #1.1 - remmed and replaced line
.                             move      dpvomits to nmrgdpv
                              move      c0 to nmrgdpv
         move      net to nmrgnet
         move      lname to nmrglnam
         MOVE      NMRGLR TO NMRGFLD
         REP       ZFILL IN NMRGFLD
.patch1.26
         BRANCH    NUM OF NOCHK,chk
CHK
.patch1.26
         move      nmrgfld to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         if        not over
         move      "Previously billed/skip it" to mcomp
         add       c1 to skipcnt
         goto      skipwrt
         endif
.patch1.26
NOCHK
.patch1.26
.begin patch 1.30
          clear     omlrky
          packkey   Nordfld from nmrglr
          call      Nordkey
          if        Not OVer
          move      "Order not found!!" to mcomp
          PACK      MKEY FROM OMLRNUM,OCOBN
          REP       ZFILL IN MKEY
          CALL      NMLRKEY
.
                    CALL      NMRGTST
                    IF        OVER
                    CALL      NMRGWRT
                    ADD       C1 TO APPLIED
                    ELSE
                    CALL      NMRGDEL
                    CALL      NMRGWRT
                    ADD       C1 TO APPLIED
                    ENDIF
          Endif
.         move      nmrglr to nordfld
.         move      "Order not found!!" to mcomp
.         clear     omlrky
.         call      nordkey
.         if        not over
.         PACK      MKEY FROM OMLRNUM,OCOBN
.         REP       ZFILL IN MKEY
.         CALL      NMLRKEY
.         endif
.end patch 1.30
det      PRINT     *1,nmrglr,*08,mcomp,*55,nmrgrqty,*65,omlrky
         add       c1 to lines
         compare   eop to lines
         call      header if not less
         DISPLAY   *P10:14,"RECORDS SKIPPED : ",skipcnt
         DISPLAY   *P10:12,"RECORDS APPLIED : ",APPLIED
         GOTO      LOOP
HEADER
         ADD       C1 TO PAGE
         PRINT     *F,*N
         PRINT     *1,today,*21,"* * * NEW PIDI MERGE INFO * * *";
         PRINT     *70,"Page: ",PAGE
         PRINT     *N
         PRINT     *1,"LR ##",*16,"MAILER",*59,"QTY",*66,"M/P ##"
         move       c6 to lines
         RETURN
.
skipwrt  write     skipped,SEQ;TDMCLR,B1:
                        NMRGLNAM,b2:
                        NMRGKCOD,B1:
                        NMRGRQTY,B1:
                        NMRGIQTY,B1:
                        NMRGTREJ,B1:
                        NMRGID,B1:  
                        NMRGNETI,B1:
                        NMRGELIM,B1:
                        NMRGHDRP,B1:
                        NMRGCS,B1:
                        NMRGUDUP,B1:
                        NMRGND,B1:
                        NMRGDUPM,B1:
                        NMRGNET,B1:
                        NMRGZIPV,B1:
                        NMRGZIPC,B1:
                        NMRGZIP4,B1:
                        NCOAMWF,B1:
                        NCOAMNF,B1:
                        NCOATOTM,B1:
                        NIXIEM,B1:
                        NCOAUNM,B1:
                        NCOANFRJ,B1:
                        NCOANIX1,B1:
                        NCOANIX2,B1:
                        NCOANIX3,B1:
                        NMRGERR,B1:
                        NMRGDISF,B1:
                        NMRGNPER,B1:
                        NMRGDMA,B1:
                        NMRGELMX,B1:
                        NMRGZ4,B1:
                        NMRGNIX,B1:
                        NMRGTDMC,B1:
                        NMRGPRIS,B1:
                        NMRGDROP,B1:
                        NCOAREJ,B1:
                        NMRGCUST,B1:
                        NMRGFAM,B1:
                        NMRGHH,B1:
                        str8:               *FAMILY DUPE DROPS/DUP FIELD.
                        b1:
                        nmrgrep:           new field 8/24/95. (DNC)
                        b1:
                        nmrgnnet:            new field 8/24/95. (DNC)
                                                                                nmrgdpv:             new field 6/25/04. PIDI
                                                                                nmrgfil2
         goto       det
wipemrg
         clear      nmrgLR  
         move      c0 to nmrgFILL
         move      c0 to nmrgLNAM
         move      c0 to nmrgKCOD
         move      c0 to nmrgRQTY
         move      c0 to nmrgCONV
         move      c0 to nmrgIQTY
         move      c0 to nmrgTREJ
         move      c0 to nmrgID  
         move      c0 to nmrgNETI
         move      c0 to nmrgELIM
         move      c0 to nmrgHDRP
         move      c0 to nmrgCS  
         move      c0 to nmrgUDUP
         move      c0 to nmrgND  
         move      c0 to nmrgDUPM
         move      c0 to nmrgNET 
         move      c0 to nmrgZIPV
         move      c0 to nmrgZIPC
         move      c0 to nmrgZIP4
         move      c0 to NCOAMWF 
         move      c0 to NCOAMNF 
         move      c0 to NCOATOTM
         move      c0 to NIXIEM  
         move      c0 to NCOAUNM 
         move      c0 to NCOANFRJ
         move      c0 to NCOANIX1
         move      c0 to NCOANIX2
         move      c0 to NCOANIX3
         move      c0 to nmrgERR 
         move      c0 to nmrgDISF
         move      c0 to nmrgNPER
         move      c0 to nmrgDMA 
         move      c0 to nmrgELMX
         move      c0 to nmrgZ4  
         move      c0 to nmrgNIX
         move      c0 to nmrgTDMC
         move      c0 to NCOAREJ 
         move      c0 to nmrgCUST
         move      c0 to nmrgPRIS
         move      c0 to nmrgDROP
         move      c0 to nmrgHH  
         move      c0 to nmrgFAM
         move      c0 to nmrgrep
         move      c0 to nmrgnnet 
         move      c0 to nmrgFIL1
                              move      c0 to nmrgdpv
                              move      c0 to nmrgfil2
        return 
STOP 
         compare   eop to lines
         call      header if not less
         PRINT     *L,*10,"NUMBER OF RECORDS APPLIED: ",applied
         splclose
         weof      skipped,seqeof
         close     skipped
.begin patch 1.22
                    call                GetWinVer
.         path      exist,"c:\windows"
.         if        over
.START PATCH 1.2 REPLACED LOGIC
.         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\nmrgpidi.LST \\NINs2\Laser2 "
.         else
.         EXECUTE   "c:\command.com /c copy g:\DATA\nmrgpidi.LST \\NINs2\Laser2 "
                    If                  (osflag = c1 | osflag = C5)
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"nmrgpidi.LST \\NINs2\Laser2 "
         Execute   TASKNAME
.         else
                    Elseif              (osflag = c3 | osflag = C4)
         PACK      TASKNAME,"c:\command.com /c copy ",NTWKPATH1,"nmrgpidi.LST \\NINs2\Laser2 "
                    Elseif              (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"nmrgpidi.LST \\NINs2\Laser2 "
.end patch 1.22
         EXECUTE   TASKNAME
.END PATCH 1.2 REPLACED LOGIC
         endif
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         pause     "10"
         stop
         INCLUDE   NMRGIO.inc
.patch1.27
                              include   compio.inc
                              include   cntio.inc
.         include   nmlrio.inc
.patch1.27
         include   nordio.inc
         include   ninvio.inc
         INCLUDE   COMLOGIC.inc

