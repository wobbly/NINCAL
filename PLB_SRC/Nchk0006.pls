...............................................................................
.NCHK0006 - MTD CASH RESET PROGRAM
...............................................................................
.
PC       EQU        0

         INC       COMMON.INC
. 
         INC       CONS.INC
.
         inc       hp.inc
. 
Release   Init      "1.3"    DLH backup file, remove prompt for ok - just do it
Reldate   INit      "2011 September 1"
.Release   Init      "1.23"    DLH remove  PLI
.Reldate   INit      "29 January 2010"
.Release  Init      "1.22"    DLH 12 July 2007  PLI
.RELEASE   INIT     "1.21  "        20NOV2001 added check for eos on OK keyin.
.RELEASE   INIT     "1.2  "        ASH 04OCT2000 NEW SERVER ADDED
.RELEASE   INIT     "1.1  "        JD 09JUN95 UPDATED MTD CASH VARS TO CTD=BY CHECK
.RELEASE   INIT     "1.0  "       28FEB95 new.
...............................................................................
RECNUM   FORM      1
.MTDFILE.
.
P        INIT      "P"           1-1      RECORD ID
MTDACR   FORM      7.2           2-11
MTDFILL1 DIM       1            12-12
MTDACP   FORM      7.2          13-22     total ap1 by check date
MTDFILL2 DIM       1            23-23     total ap2 by check date
MTDSAP   FORM      7.2          24-33
MTDFILL3 DIM       1            34-34
MTDNIN   FORM      7.2          35-44
MTDLR    FORM      7.2          46-55
MTDFILL5 DIM       1            56-56
MTDSTX   FORM      5.2          57-64
MTDFILL6 DIM       1            65-65
MTDCTX   FORM      5.2          66-73
MTDFILL7 DIM       1            74-74
MTDPOST  FORM      5.2          75-82
MTDFILL8 DIM       13           83-95
MTDCNTNO FORM      3            96-97
MTDFILL9 DIM       1            98-98
MTDMO    FORM      2            99-100
MTDDY    FORM      2           101-102
MTDYR    FORM      2           103-104
CTDAP1  form      7.2         105-114      total ap1 by CHECK date
CTDAP2  form      7.2         115-124      total ap2 by CHECK date
.
...............................................................................
.
MTDFILE  FILE      FIXED=124
.begin patch 1.22
cmpflag   Dim       1
.end patch 1.22
.
.
         MOVE      "NCHK0006" TO PROGRAM
         MOVE      "MTD CASH RESET " TO STITLE
         MOVE      "NAMES IN THE NEWS" TO COMPNME
.         CLOCK     DATE TO DATE
.begin patch 1.3"
          Unpack    Today into mm,str1,dd,str1,yy
          pack      str6 from cc,yy,mm,dd
          rep       Zfill in str6
.end patch 1.3"
         CALL      PAINT               .REDISPLAY SHOWING CHECK DATE.
.begin patch 1.22
Seltcmp
.begin patch 1.23
.         Keyin     *p1:10,"Select Company (N)IN or (P)LI ",cmpflag
.         Cmatch    "N",cmpflag
.         goto      OPenFIle if equal
.         Cmatch    "P",cmpflag
.         goto      OPenFIle if equal
.         Beep
.         goto      Seltcmp
OpenFile  
.         If        (CMPFlag = "P")
.                   PACK      STR35,NTWKPATH1,"text\MTDCaSHP"
.                   OPEN      MTDFILE,STR35,EXCLUSIVE
.         Else
          
          PACK      STR35,NTWKPATH1,"text\MTDCaSH"
.begin patch 1.3"
          pack      str45 from str35,".dat"
          pack      str55 from str35,str6
          copyfile  str45,str55
.end patch 1.3"
          
          OPEN      MTDFILE,STR35,EXCLUSIVE
.         endif
.end patch 1.22
.end patch 1.23
          
Check
.begin patch 1.3"
          move      Yes,str1
.         keyin     *P1:12,*blinkon,*red,"This will ZERO OUT MTD TOTALS     OK???   ",*uc,str1,*lc,*white
         keyin     *P1:12,*blinkon,*red,"This will ZERO OUT MTD TOTALS     OK???   ",*uc,*rv,*t5,str1,*lc,*white
.end patch 1.3"
         cmatch    b1 to str1
         goto      check if eos
         cmatch    yes to str1
         stop      if not equal
.START PATCH 1.2 REPLACED LOGIC
.         OPEN      MTDFILE,"\\nts0\d\data\text\MTDCaSH",EXCLUSIVE
.begin patch 1.22
.         PACK      STR35,NTWKPATH1,"text\MTDCaSH"
.         OPEN      MTDFILE,STR35,EXCLUSIVE
.end patch 1.22
.END PATCH 1.2 REPLACED LOGIC
         DISPLAY   *P1:23,"ZEROING OUT MTD TOTALS        "
         READ      MTDFILE,RECNUM;P:
                   MTDACR:
                   MTDFILL1:
                   MTDACP:
                   MTDFILL2:
                   MTDSAP:
                   MTDFILL3:
                   MTDNIN:
                   MTDFILL1:
                   MTDLR:
                   MTDFILL5:
                   MTDSTX:
                   MTDFILL6:
                   MTDCTX:
                   MTDFILL7:
                   MTDPOST:
                   MTDFILL8:
                   MTDCNTNO:
                   MTDFILL9:
                   MTDMO:
                   MTDDY:
                   MTDYR:
                   CTDap1:
                   CTDap2
         MOVE      C0 TO MTDACR
         move      c0 to mtdacp
         move      c0 to mtdsap
         MOVE      C0 TO MTDNIN
         MOVE      C0 TO MTDLR
         MOVE      C0 TO MTDSTX
         MOVE      C0 TO MTDCTX
         MOVE      C0 TO MTDPOST
WRITEMTD 
         WRITE     MTDFILE,RECNUM;P:
                   MTDACR:
                   MTDFILL1:
                   MTDACP:
                   MTDFILL2:
                   MTDSAP:
                   MTDFILL3:
                   MTDNIN:
                   MTDFILL1:
                   MTDLR:
                   MTDFILL5:
                   MTDSTX:
                   MTDFILL6:
                   MTDCTX:
                   MTDFILL7:
                   MTDPOST:
                   MTDFILL8:
                   *ZF,MTDCNTNO:
                   MTDFILL9:
                   MTDMO:
                   MTDDY:
                   MTDYR:
                   CTDap1:
                   CTDap2
         CLOSE     MTDFILE

         shutdown  "CLS"
         STOP

         INCLUDE   COMLOGIC.INC

