PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
;Patch2.3
                              Include   Compdd.inc
                              include   cntdd.inc
.         INCLUDE   NbrkDD.INC
.         INCLUDE   NMLRDD.INC
;Patch2.3
         INCLUDE   NSHPDD.inc
         INCLUDE   HP.INC
input     file
.Input file sorted by Broker/mailer. break on mailer - read broker file for
.each record to get contact name.
.
...................................................................................................................
.               "FAX SHIP INFO" TO STITLE
...................................................................................................................
RELEASE   INIT      "2.4"            09AUG2004 ASH Logo Conversion
.RELEASE   INIT      "2.3"            26MAY2004 DMB Malier Conversion
.RELEASE   INIT      "2.24"            12Jul2002 DLH  Use GetWinVer
;RELEASE   INIT      "2.23"            30Mar2001 DLH Replace c:\windows logic
.RELEASE   INIT      "2.22"            02OCT2000 ASH NEW SERVER ADDED
.RELEASE   INIT      "2.21"            16MAR2000 ASH REPLACED AREA CODE
.RELEASE   INIT      "2.2"            18FEB00 ASH SAMPLE DIRECTORY MOVED
.release   init      "2.1"            16Feb00 DLH hpio.inc
.RELEASE   INIT      "2.0"            10JUN99 ASH NINSHP Y2K, File expansion
.Release   init      "1.9"            05apr99 JD rename Facsys printer Facsys2
.Release   init      "1.8"           07Oct98 DLH add tracking number
.Release  init      "1.7"             03Sep98 DLH added ^[Slist Management info on fax
.                                   in embedded codes so sender is id'd by new server
.RELEASE  INIT      "1.6"             20JUL98 ASH Y2K changes reflecting NINBRK.DAT fields:  BRCOMP,BRREVDAT,BRCNTCT
.release  init      "1.6"             15jul98 DLH Y2 on ninshp.dat
.Release  init      "1.5"             13Apr98 Tighten code around Missing fax number
.                                    Have system send an EMail via outlook
.                                    and print out the list file.
.release  init      "1.2"             25sep97 re-written to use nprint to fax.
.release  init      "1.1"            11jan95 tightened up check of fax #.
.release  init      "1.0"           06jan95 DLH change broker read at print
.                                  time to have correct contact name.
.RELEASE  INIT      "PRE"           08DEC94 DLH
.
.PHONE DISPLAY VAR'S
LP       DIM       1
RP       DIM       1
EXT      DIM       3
ARCD     DIM       3
PHONE    DIM       4
MEDTYPE  DIM       1
.Increase var length to reflect changes to NINBRK.DAT
.faxname  dim       25
faxname  dim       45
faxtele  dim       10
faxattn  dim       25
TIME     DIM       8
DATE     DIM       8         'MM/DD/YY'.
spoolfl2 DIM       30                   .order  SPOOL FILEs
spoolfle DIM       30                   .order  SPOOL FILEs
recname  dim       60
faxflag  form      1                    .1=no, 2=yes.
holdbrk  dim        4                   .used for broker break
holdbrk1 dim        3                   .used for broker contact change
fhandle  dim        4                   .use to create fax files.
attchlst dim       1000
LPTCNT   FORM      4                 .LENGTH OF ATTCHLST
LONGDIST DIM       1
FORMFILE FILE      UNCOMP
copycmd  init      "y:command.com /C copy /b/y "
FORMNAME DIM       30
badfaxflag dim     1
badflag2 dim       1
.
.START PATCH 2.0 - REPLACED LOGIC
.qtymsk   dim       9
qtymsk   dim       11
.END PATCH 2.0 - REPLACED LOGIC
LINES    FORM      2
Logfile  file      .created at start of  job, delete if job finishes ok.
SHPFAX   FILE     .SUBSET OF ORDER FILE REPRESENTING NEW SHIPPING INFO ON
.                  LIST MANAGEMENT ORDERS.
;begin patch 2.24
.begin patch 2.23
;osflag   form   1          1=win 95,98, 2=NT
.end patch 2.23
;end patch 2.24

...............................................................................
.
         MOVE      "NSHP0004" TO PROGRAM
         MOVE      "NAMES IN THE NEWS CAL" TO COMPNME
         MOVE      "FAX SHIP INFO" TO STITLE
         MOVE      "EXIT" TO PF5
         TRAP      ABORT IF F5
         CALL      PAINT
         CALL      FUNCDISP
         move      c1 to nmlrpath
         move      c1 to nbrkpath
         CLEAR     HOLDBRK
;begin patch 2.24
.begin patch 2.23
                    call                GetWinVer
;        getinfo  system,str6
;        unpack   str6 into str1,str2
;        unpack   str2 into str1
;        move     c0 to osflag
;..0 = unknown
;..1 = Windows NT
;..2 = WIN32s Windows 3.1x (obsolete)
;..3 = Window 95
;..4 = Window 98
;..5 = Windows 2000
;..8 = Windows CE
;        if       (str1 = "3" or str1 = "4")
;        move     c1 to osflag
;        endif
;        if       (str1 = "1" or str1 = "5")
;        move     c2 to osflag
;        endif
;.end patch 2.23
;end patch 2.24
.START PATCH 2.22 REPLACED LOGIC
.         open      input,"g:\data\shipfax.srt",read
.         prepare   logfile,"g:\data\shipfax.log"
         PACK      STR35,NTWKPATH1,"shipfax.srt"
         open      input,STR35,read
         PACK      STR35,NTWKPATH1,"shipfax.log"
         prepare   logfile,STR35
.END PATCH 2.22 REPLACED LOGIC
         write     logfile,seq;"If this file is here nshp0004 is running or bombed"
         weof      logfile,seq
         close     logfile
.
READ     clear     olrn
         read      input,seq;ordvars
         GOTO      EOJ IF OVER
         ADD       C1 TO N4
         DISPLAY   *P10:12,"NUMBER OF ORDERS READ : ",N4
         MOVE      OLRN TO NSHPFLD
         match     "      " to olrn
         goto      read if equal
         match     "      " to olrn
         goto      read if eos
         type      olrn
         goto      read if not equal
         rep       zfill in nshpfld
         CALL      NSHPKEY
         GOTO      error IF OVER
         MATCH     obrknum TO holdbrk
         CALL      brkbREAK IF NOT EQUAL
         MATCH     obrkcnt TO holdbrk1
         CALL      cntbREAK IF NOT EQUAL
         PACK      MKEY FROM OMLRNUM,Z3
         CALL      NMLRKEY

.DO THE PRINT THING
         compare   "60" to lines
         call      header if not less
         UNPACK    SDATE INTO cc,yy,MM,DD
.START PATCH 2.0 - REPLACED LOGIC
.         move      "Z,ZZZ,ZZ0" to qtymsk
.         move      c0 to n7 
.         move      squant to n7
.         edit      n7 to qtymsk
         move      "ZZZ,ZZZ,ZZ0" to qtymsk
         move      c0 to n9 
         move      squant to n9
         edit      n9 to qtymsk
.END PATCH 2.0 - REPLACED LOGIC
         PRINT     *N,*1,hptmsr06,"PO## ",OMLRPON,HPT100,hptmsr10,"ATTN: ",BRCNTCT:
                   hptmsr08,HPT425,MM,SLASH,DD,SLASH,cc,YY:
                   HPT500,qtymsk:
                   HPT700,SINFO:
                   *N,hptmsr08,"LR## ",SLRNUM:
                   HPT100,hptmsr10,"MAILER: ",MCOMP:
                   hpt650,hptmsr08,strack,hptmsr10:
                   *N,HPT100,"LIST: ",O1DES
         ADD       C4 TO LINES 
         goto      read
.         
cntbreak  
         move      obrkcnt TO holdbrk1
         pack      nbrkfld from obrknum,obrkcnt
         call      nbrkkey
         return
.         
brkbreak display   *p1:24,*el,"Broker break"
         compare   N4 to c1
         if        equal
         move      obrknum to holdbrk
         move      obrkcnt TO holdbrk1
         pack      nbrkfld from obrknum,obrkcnt
         call      nbrkkey
         ELSE
         SPLCLOSE
         release
         endif
.....................................
.create new spool file
         pack      nbrkfld from obrknum,z3
         rep       zfill in nbrkfld
         call      nbrkkey             .make sure we have new brk info
         move      obrknum to fhandle
         move      brcomp to faxname
         MOVE      brFAX TO FAXTELE
         display   *p1:24,*el,"checking Broker ",brcomp
         match     "0000000000" to brfax
         if        equal
         goto      nofaxnum
         else
         TYPE      brFAX                             .valid fax #?
          if        not equal
.printing  not faxing - no or invalid fax number
.START PATCH 2.22 REPLACED LOGIC
.nofaxnum MOVE      "g:\DATA\SHiPfax.LST" TO spoolfle
nofaxnum PACK      spoolfle,NTWKPATH1,"SHiPfax.LST"
.END PATCH 2.22 REPLACED LOGIC
         DISPLAY   *P1:24,*EL,"The output file name for this fax is: ",spoolfle,*w4
         move      yes to badflag2
          SPLOPEN   spoolfle,"Q"
          display   *p1:24,*el,*b,"no fax number",*w,*b
          move       c1 to faxflag        .
          else
          move      c2 to faxflag        .do fax.
          display   *p1:24,*el,"fax IT "
          call      prepFAX
          endif
         endif
         move      obrknum to holdbrk
         move      obrkcnt TO holdbrk1
         pack      nbrkfld from obrknum,obrkcnt
         call      nbrkkey
         call      Header
         return
*******************************************************************************
. Either create batch to send files or Execute here & submit for NWsend.
.
.
.
prepfax     COUNT     N2,faxtele
            COMPARE   C10 TO N2
            IF        EQUAL
            MOVE      C1 TO LONGDIST
            UNPACK    faxtele INTO STR3,STR7
.START PATCH 2.21 REPLACED LOGIC
.            MATCH     "415" TO STR3           .LOCAL ?
            MATCH     "510" TO STR3           .LOCAL ?
.END PATCH 2.21 REPLACED LOGIC
            IF         EQUAL
            MOVE       STR7 TO faxtele
            CLEAR      LONGDIST
            else
            MATCH      B3 TO STR3           .LOCAL ?
            IF         EQUAL
            MOVE       STR7 TO faxtele
            CLEAR      LONGDIST
            ENDIF
            ENDIF
            ENDIF
.            move      c1 to n3
FILENAME CLEAR     formname
         APPEND    "ship",formNAME
         APPEND    fhandle,formname
         RESET     formname TO 8
         RESET     formname
         clear     spoolfle
.START PATCH 2.2 REPLACED LOGIC
.         append    "f:\data\samples\ship",spoolfle
.START PATCH 2.22 REPLACED LOGIC
.         append    "G:\data\samples\ship",spoolfle
         append    NTWKPATH1,spoolfle
         append    "samples\ship",spoolfle
.END PATCH 2.22 REPLACED LOGIC
.END PATCH 2.2 REPLACED LOGIC
         append    fhandle to spoolfle
         APPEND    ".lst" TO spoolfle
         reset     spoolfle
         clear     badfaxflag
          trap     badfax if spool
.will this work???
.         splopen   "Facsys2","A"
         splopen   "Faxcore IP Printer","A"
         cmatch     yes to badfaxflag
         if         equal
         SPLOPEN    spoolfle
         move       yes to badflag2
         endif
.         PREPARE   formfile,RECNAME,CREATE
         reset    faxname to 25
blankc   cmatch   b1 to faxname
         if       equal
         bump     faxname,-1
         goto     blankc
         else
         lenset   faxname
         reset    faxname,1
         endif
         unpack    date into mm,str1,dd,str1,yy
         clock     time to time
         clear     str5
         append    time to str5
         reset     str5
         print     "^[D",longdist,faxtele,"^[N",brcomp:
                   "^[SList Management"," ^]"
.         splclose   
         return

...............................................................................
.
header
.begin patch 2.1
.         print     hpreset:
.                   hpttray:
.                   hpport:
.                   033,"&l66P":               page length
..                   033,"&l65F":
.                   033,"&l1E",033,"&a0c0R":     top margin * print position
.                   hpltrhd:
.                   *n,*n,*n:
         print     hpreset:
                   hpttray:
                   hpport:
                   033,"&l66P":               page length
                   033,"&l65F";
         call       PortraitLTRHEAD
.START PATCH 2.4 REPLACED LOGIC
.         print     *n,*n,*n,*n,*n:
.                   *N,*N,*n:
.                   *n,*rptchar "_":200:
.                   *n,*n,*n,hpt250,hptmsr14,hpbon,"SHIPPING STATUS REPORT":
.                   *L,hpboff:
.                   *N,"     TO: ",BRCOMP,hptmsr08,hpt350,"VIA FACSIMILE":
.                   *N,*N,hptmsr14,"FROM: ","NAMES IN THE NEWS/CA  LIST":
.                   " MANAGEMENT DEPARTMENT":
.                   *N,*N,hptmsr10,*RPTCHAR ".":200:
.                   *N,hptmsr08,HPT425,"DATE":
.                   HPT500,"QTY":
.                   *N,HPT425,"SHIPPED":
.                   HPT500,"SHIPPED":
.                   HPT625,"METHOD",*n
         print     *n,*n,*n,*n,*n:
                   *N,*N,*n:
                   *n,*rptchar "_":200:
                   *n,*n,*n,hpt250,hptmsr14,hpbon,"SHIPPING STATUS REPORT":
                   *L,hpboff:
                   *N,"     TO: ",BRCOMP,hptmsr08,hpt350,"VIA FACSIMILE":
                   *N,*N,hptmsr14,"FROM: ","NAMES IN THE NEWS  LIST":
                   " MANAGEMENT DEPARTMENT":
                   *N,*N,hptmsr10,*RPTCHAR ".":200:
                   *N,hptmsr08,HPT425,"DATE":
                   HPT500,"QTY":
                   *N,HPT425,"SHIPPED":
                   HPT500,"SHIPPED":
                   HPT625,"METHOD",*n
.END PATCH 2.4 REPLACED LOGIC
.end patch 2.1
         MOVE      "22" TO LINES
         RETURN          
...............................................................................
ABORT    DISPLAY   *P1:24,*EL,*HON,"JOB ABORTED BY OPERATOR",*B,*W;
         shutdown
         stop
error    DISPLAY   *P1:24,*EL,*HON,"can't find shipping record",*B,*W;
         goto      read
.
EOJ      splclose
         release
         cmatch   yes to badflag2
         if       equal
.         .execute "F:\PUBLIC\NPRINT f:\DATA\samples\ship*.LST Q=facsys S=ns1 "
.begin patch 2.23
.         path      exist,"c:\windows"
.         if        not over
;begin patch 2.24
;          if        (osflag = c1)
                    If                  (osflag = c3 | osflag = c4)
.end patch 2.23
          execute  "!c:\command.com /C \\nins1\winbatch\senderr.exe"
.START PATCH 2.22 REPLACED LOGIC
.          execute  "!c:\command.com /C copy g:\data\shipfax.lst \\nts0\laser6"
.          else
.          execute  "!c:\winnt\system32\cmd.exe /C f:\apps\winbatch\senderr.exe"
.          execute  "!c:\winnt\system32\cmd.exe /C copy g:\data\shipfax.lst \\nts0\laser6"
.
          PACK     TASKNAME,"!c:\command.com /C copy ",NTWKPATH1,"shipfax.lst \\NINs2\Laser3"
          execute  TASKNAME
;          else
                    ElseIf              (osflag = c1 | osflag = c5)
          execute  "!c:\winnt\system32\cmd.exe /C \\nins1\winbatch\senderr.exe"
          PACK     TASKNAME,"!c:\winnt\system32\cmd.exe /C copy ",NTWKPATH1,"shipfax.lst \\NINs2\Laser3"
          execute  TASKNAME
                    ElseIf              (osflag = c6)
          execute  "!c:\windows\system32\cmd.exe /C \\nins1\winbatch\senderr.exe"
          PACK     TASKNAME,"!c:\windows\system32\cmd.exe /C copy ",NTWKPATH1,"shipfax.lst \\NINs2\Laser3"
          execute  TASKNAME
;end patch 2.24
.END PATCH 2.22 REPLACED LOGIC
         endif
.         .display  *p2:23,"Please wait I'm Sending to Fax !!!!!"

.         .pause     "20"
         endif
         display    *p2:23,*el,"Job Done",*w3
.START PATCH 2.22 REPLACED LOGIC
.         erase      "G:\data\shipfax.log"
         PACK       STR35,NTWKPATH1,"shipfax.log"
         erase      str35
.END PATCH 2.22 REPLACED LOGIC
         shutdown
         stop
BADLR    COMPARE   C1 TO N4   1ST REC?
         IF        EQUAL
         MOVE      C0 TO N4
         ENDIF
         GOTO      READ
badfax   trapclr   spool
         move      yes to badfaxflag
         return
                  
         INCLUDE   NORDIO.inc
         INCLUDE   NSHPIO.inc
;Patch2.3
                              Include   Compio.inc
                              Include   cntio.inc
.         INCLUDE   NbrkIO.INC
.         INCLUDE   NMLRIO.INC
;Patch2.3
.begin patch 2.1
         include   hpio.inc
.end patch 2.1
         INCLUDE   COMLOGIC.inc
