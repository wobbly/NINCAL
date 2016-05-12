pc       equ       0
         inc       common.inc
         inc       cons.inc
.patch1.3
                                            include    compdd.inc
                                            include    cntdd.inc
.         INC       NMLRDD.INC
.patch1.3
.        include   nbrkdd.inc
         inc       NMOADD.INC
         inc       gnxtdd.inc
         INCLUDE   NMOBDD.INC
release  init      "1.3"                  JD           26MAY2004  Mailer Conversion
.;release  init      "1.2"         ASH 02OCT2000 NEW SERVER ADDED
.                                               REMOVED DUPE VAR FOUND IN CONS.INC
.release  init      "1.1"         ASH 27Oct98 NINMOA Y2K,File expansion
.release  init      "1.0"         JD05dec96  fixed extra record getting written
.                                 to detail file. (transnum check)
.release  init      "pre"
OUTPUT   FILE      
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
input    form      6
count    form      6
ques     init      "???????????????"
quotcom  init      "#","
comquot  init      ",#""
.START PATCH 1.2 REMOVED DUPE VAR
.comma    init      ","
.END PATCH 1.2 REMOVED DUPE VAR
check    form       6
lastmoa  dim        6
.
         clock     date to today
         move      "exit" to pf5
         trap      EXIT if f5
         move      c3 to nmoapath
         call      paint
         call      funcdisp
         move      c1 to nmlrpath
         display   *p1:24,*el,"preparing output";
         PREPARE    OUTPUT,"c:\work\moadet"
         display   *p1:24,*el,"Lets get to it";
.START PATCH 1.2 REPLACED LOGIC
.         open      nmoafle3,"g:\data\text\ninmoa.eom"
         PACK      STR35,NTWKPATH1,"TEXT\ninmoa.eom"
         open      nmoafle3,STR35
.END PATCH 1.2 REPLACED LOGIC
         move      c1 to nmoaflg3
loop     
.call      nmoaseq
.Start Patch #1.1 - remmed and replaced lines
.         READ      NMOAFLE3,SEQ;MLR:
.                                MCNT:
.                                ENTRY:
.                                MBILLTO:
.                                TRANDATE:
.                                control:
.        INVOICE:
.        LRNUM:
.        INVDATE,ONAMOUNT,RECDATE,INAMOUNT,ONACOM,REASON,LIST,CHECKNUM:
.        TRANSNUM,NMOABRK
         READ      NMOAFLE3,SEQ;MLR:
                                MCNT:
                                ENTRY:
                                MBILLTO:
                                TRANDATE:
                                control:
        INVOICE:
        LRNUM:
.        INVDATE,ONAMOUNT,RECDATE,INAMOUNT,ONACOM,REASON,LIST,CHECKNUM:
        INVDATE,ONAMOUNT,RECDATE,INAMOUNT,MoaCOmp,MoaFill,REASON,LIST,CHECKNUM:
        TRANSNUM,NMOABRK,NMOAINIT
.End Patch #1.1 - remmed and replaced lines
         goto      eoj if over
.         PACK      CONTROL FROM B1,STR2
         add       c1 to input
         display   *p1:10,"input : ",input
         move      transnum to n7
. enter first number in range of 'new trans' in moa lst file
.         compare   "23824" to n7
.         compare   "25416" to n7          .july ?
.         compare    "25964" to n7          .august 95
.          compare    "26320" to n7          .october 95
.          compare    "26629" to n7          .November 95
.          compare    "26997" to n7          . December 95
.          compare    "27310" to n7          . January 96
.          compare    "27754" to n7          . February 96 
         MOVE      "NONALAST" TO GNXTFLD
         CALL      GNXTKEY
         MOVE      GNXTNUM TO check
          compare   check to n7         
         goto       loop if equal
         goto       loop if less
         goto       write
.skip date check.         
.Start Patch #1.1 - remmed and replaced line
.         unpack    recdate into mm,dd,yy
         unpack    recdate into str2,yy,mm,dd
.End Patch #1.1 - remmed and replaced line
         move      c0 to n2
         move      yy to n2
         compare   c0 to n2
         goto      write if equal
         compare   "96" to n2
         goto      loop if not equal
        move      c0 to n2
        move      mm to n2
        compare   c3 to n2
        goto      write if equal
        goto      loop
chkdte2 
.Start Patch #1.1 - remmed and replaced line
.         unpack    trandate into mm,dd,yy
         unpack    trandate into str2,yy,mm,dd
.End Patch #1.1 - remmed and replaced line
         move      c0 to n2
         move      yy to n2
         compare   c0 to n2
         goto      write if equal
         compare   "96" to n2
         goto      loop if not equal
        move      c0 to n2
        move      mm to n2
        compare   c3 to n2
        goto      write if equal
        goto      loop
....................................................

write    move      c0 to mm
         move      c0 to dd
         move      c0 to yy
.Start Patch #1.1 - remmed and replaced line
.         unpack    trandate into mm,dd,yy
         unpack    trandate into str2,yy,mm,dd
.End Patch #1.1 - remmed and replaced line
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO trandate
         move      c0 to mm
         move      c0 to dd
         move      c0 to yy
         cmatch    b1 to lrnum
         if        eos
         move      b6 to lrnum
         endif
         rep       zfill in transnum
.Start Patch #1.1 - remmed and replaced line
.         unpack    recdate into mm,dd,yy
         unpack    recdate into str2,yy,mm,dd
.End Patch #1.1 - remmed and replaced line
         CALL       CVTJULTS                   .CONVERT TO JULIAN FOR LOTUS
         MOVE       JULDAYS TO recdate
         rep       zfill in transnum
         WRITE     OUTPUT,SEQ;transnum,comma,lrnum,comma:
                   control,comma,trandate,comma,onamount,comma:
                   nmoabrk,comma,mlr,comma,recdate,comma,reason
         add       c1 to count
         display   *p1:12,"written: ",count,b1,mm,slash,dd,slash,yy
         goto     loop
eoj      WEOF     OUTPUT,SEQ
         CLOSE    OUTPUT
         move      yes to str1
         KEYIN     *P1:24,*EL,*B,*B,*B,"OK TO UPDATE NONALAST NOW  ?",*RV,*t120,STR1;
         cmatch    no to str1
         goto      exit if equal
         CMATCH    YES TO STR1
         IF         EQUAL
         MOVE      "NONANXT" TO GNXTFLD
         CALL      GNXTKEY
         MOVE      GNXTNUM TO lastmoa
         MOVE     "NONALAST" TO GNXTFLD
          CALL     GNXTKEY
          MOVE     lastMOA TO GNXTNUM
          CALL     GNXTUPD
         display   *p1:24,*el,"Record Updated",*b,*w5
         endif
EXIT     stop
         include   nMOAIO.INC
         INCLUDE   NMOBIO.INC
.patch1.3
                                            include    compio.inc
                                            include    cntio.inc
.         INCLUDE   NMLRIO.INC
.patch1.3
.         include   nmlrio.inc
         include   gnxtio.inc
         inc       comlogic.inc                 
