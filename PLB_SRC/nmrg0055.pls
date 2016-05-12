.NAMES IN THE NEWS CALIF. DWB TO NINCAL MERGE UPDATE
.input created from spreasheet sent to accounting. Exported (text no tabs)
.new column on end of sheet used for LR# (keyed in from unbilled report)
.File saved to g:\data\epsimrg.dat.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         include   norddd.inc
;patch1.42
			include	compdd.inc
			include	cntdd.inc
.         include   nmlrdd.inc
;patch1.42
         INCLUDE   NMRGDD.inc
         include   ninvdd.inc
IN       FORM      5
APPLIED  FORM      5
first    init      "Y"
eop      form      "58"
lines    form      2
page     form      2
TDMCLR   DIM       10
byte2    dim       1
recno    dim       3
lname    dim       8
IQTY     dim       8
convers  dim       8
ncoa     dim       8
dma      dim       8
intra    dim       8
dim23    dim       23
net      dim       8
dim14    dim       14
dim24    dim       24
lrno     dim       6
errorr   dim       8
dead     dim       8
pris     dim       8
NonPer	Dim	8
zip4     dim       8
num      form      1
INPUT    FILE      var=441
skipped  file      var=449
;skipped  file      var=441
skipcnt  form      5

Release	Init	"1.47"	DLH 19DEc2006  NOn Personal
.release  init      "1.46"        JD14Feb2006 New input directory.
;release  init      "1.45"        JD   27aug2004 added dead,prison dedcuts after lr.
;release  init      "1.42"        DMB	26MAY2004	Mailer Conversion
;release  init      "1.41"        DLH 12Jul2002 use getwinver
;release  init      "1.4"        ASH 02OCT2000 NEW SERVER ADDED
.release  init      "1.32"        JD28Aug00 read csv file format.
.release  init      "1.31"        JD05Aug99 added apply billed orders.
.release  init      "1.3"        JS22Jun99 NT copy.
.release  init      "1.2"        JDnov2898 print for mcomp .
.release  init      "1.1"       JDnov0797 updated report title.
.release  init      "1.0"       JDJAN0797 updated version of nmrg0003.(epsilon)
         MOVE      "Names In The News Ca." TO COMPNME
         MOVE      "NMRG0005" TO PROGRAM
         MOVE      "APPLY EPSILON MERGE INFO" TO STITLE
         CLOCK    DATE TO TODAY
         CALL      PAINT
.START PATCH 1.4 REPLACED LOGIC
.         splopen   "g:\data\nmrgepsi.lst"
.         open      skipped,"g:\data\text\mrgeskip.ped"
         PACK      STR35,NTWKPATH1,"NMRGEdwb.LST"
         PACK      STR45,NTWKPATH1,"text\mrgeskip.ped"
         splopen   STR35
         open      skipped,STR45
.END PATCH 1.4 REPLACED LOGIC
         move       c1 to nordpath
         move       c1 to ninvpath
         call       header
         MOVE       NO TO STR1
         KEYIN     *P10:12,"APPLY ALREADY BILLED RECORDS ?? ",*uc,*T20,STR1
         REPLACE   "Y1N2" IN STR1
         MOVE      STR1,NUM
.START PATCH 1.4 REPLACED LOGIC
.         OPEN      INPUT,"g:\data\epsimrg.csv"
.         PACK      STR35,NTWKPATH1,"epsimrg.csv"
         PACK      STR35,"\\nins1\d\accounting\dwbmrg.csv"
         OPEN      INPUT,STR35
.END PATCH 1.4 REPLACED LOGIC
LOOP     READ      INPUT,SEQ;*cdfon:           1-2
                        recno:               3-4         4
                        lname:
                        IQTY:               13-20        8
                        lrno:                99-104       6
			errorr:
			zip4:
			pris:
                        ncoa:               29-36        8
                        intra:
                        dma:                37-44        8
                        net                77-84        8
.                        str8:              85-98        14
.                        lrno                99-104       6
                        
.end patch 1.47

         GOTO      STOP IF OVER
         ADD       C1 TO IN
         DISPLAY   *P10:10,"RECORDS READ : ",IN
         type      lrno
         goto      loop if not equal
         call      wipemrg
         move      lrno to nmrglr
         MATCH     B6 TO NMRGLR
         GOTO      LOOP IF EQUAL
         move      iqty to nmrgrqty
         move      iqty to nmrgiqty
         move      convers to nmrgconv
         move      ncoa to ncoamnf
         move      dma to nmrgdma
         move      intra to nmrgid
         move      pris to nmrgpris
         move      dead   to nmrgdisf
.begin patch 1.47
	MOve	NonPer,NmrgNPer         
	move       zip4,nmrgz4
	move       errorr,nmrgerr
.end patch 1.47
         move      net to nmrgnet 
         move      lname to nmrglnam
         MOVE      NMRGLR TO NMRGFLD
         REP       ZFILL IN NMRGFLD
         BRANCH    NUM OF NOCHK,chk
CHK      move      nmrgfld to ninvfld
         rep       zfill in ninvfld
         call      ninvkey
         if        not over
         move      "Previously billed/skip it" to mcomp
         add       c1 to skipcnt
         goto      skipwrt
         endif
nochk
         CALL      NMRGTST
         IF        OVER
         CALL      NMRGWRT
         ADD       C1 TO APPLIED
           ELSE
         CALL      NMRGDEL
         CALL      NMRGWRT
         ADD       C1 TO APPLIED
         ENDIF
         move      nmrglr to nordfld
         move      "Order not found!!" to mcomp
         clear     omlrky
         call      nordkey
         if        not over
         PACK      MKEY FROM OMLRNUM,OCOBN
         REP       ZFILL IN MKEY
         CALL      NMLRKEY
         endif
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
         PRINT     *1,today,*21,"* * * NEW EPSILON MERGE INFO * * *";
         PRINT     *70,"Page: ",PAGE
         PRINT     *N
         PRINT     *1,"LR ##",*08,"MAILER",*55,"QTY",*65,"M/P ##"
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
								nmrgdpv            new field 6/25/04  PIDI
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
        return 
STOP 
         compare   eop to lines
         call      header if not less
         PRINT     *L,*10,"NUMBER OF RECORDS APPLIED: ",applied
         splclose
         weof      skipped,seqeof
         close     skipped
;begin patch 1.41
.begin patch 1.3
                    call                GetWinVer
;         path      exist,"c:\windows"
;         if        over
.START PATCH 1.4 REPLACED LOGIC
.         Execute   "c:\winnt\system32\cmd.exe /c copy g:\DATA\nmrgepsi.LST \\nts0\LASER2 "
.         else
.         EXECUTE   "c:\command.com /c copy g:\DATA\nmrgepsi.LST \\nts0\LASER2 "
                    If                  (osflag = c1 | osflag = c5)
         PACK      TASKNAME,"c:\winnt\system32\cmd.exe /c copy ",NTWKPATH1,"nmrgepsi.LST \\nts0\LASER2 "
         Execute   TASKNAME
;         else
                    elseif                  (osflag = c3 | osflag = c4)
         EXECUTE   "c:\command.com /c copy \\nins1\E\DATA\nmrgepsi.LST \\nts0\LASER2 "
.END PATCH 1.4 REPLACED LOGIC
                    ElseIf                  (osflag = c6)
         PACK      TASKNAME,"c:\windows\system32\cmd.exe /c copy ",NTWKPATH1,"nmrgepsi.LST \\nts0\LASER2 "
;end patch 1.41
         endif
.end patch 1.3
         display   *p2:23,"Please wait I'm PRINTING !!!!!"
         pause     "10"
         stop
         INCLUDE   NMRGIO.inc
;patch1.42
			include	compio.inc
			include	cntio.inc
.         include   nmlrio.inc
;patch1.42
         include   nordio.inc
         include   ninvio.inc
         INCLUDE   COMLOGIC.inc

