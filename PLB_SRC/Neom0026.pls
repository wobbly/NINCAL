pc       equ       0
         INCLUDE   COMMON.inc
         INCLUDE   ndatdd.inc
         INCLUDE   nowndd.inc
         include   cons.inc
NUM      FORM      1
Release   Init      "1.31"     DLH  change logic some non managed lists still make it in IE CElco
Reldate   Init      "2015 February 2"
.Note app is much faster now, Also CELCO issues was a list manager accidently marking a CELCO file as NIN Managed

.Release   Init      "1.3"     DLH  Clear some variables
.Reldate   Init      "02 November 2010"
.release  init      "1.2"     28JAN2004  ASH  DATACARD CONVERSION
.release  init      "1.1"     05Sep2003 skip owner # 33. NINCA
.release  init      "1.0"
parFILE  iFILE      keylen=4
.
         DISPLAY   *P1:1,*ES
.         move     c1 to ndatpath
         move     c3 to ndatlock
         move     c3 to nownlock
.begin patch 1.31
         move     c4 to ndatpath            .read only managed files
          move      "000000",ndatfld
         call       ndattst
.end patch 1.31
PREP     open    parFILE,"pareom|NINS1:502"
         
INPUT    
.begin patch 1.3 DH Nov 2010 clear Elstcde & status before the read
          Clear     Elstcde
          Clear     Status
.end patch 1.3         
.begin patch 1.31
.          call      ndatseq
          call      ndatks
.end patch 1.31
         GOTO      EOJ IF OVER
         add       c1 to n6
         display   *p10:15,"records read = ",n6
         cmatch    "W" to status
         goto      input if equal
CHECK
.          CMATCH    "C" TO ELSTCDE
.         goto      input if not equal
         
         if        (elstcde = "C" | elstcde = "P")
         goto      ownchk
         else
         goto      input
         endif
         
.START PATCH 1.2 REPLACED LOGIC
.         match     "2191" to ownnum
.         goto      input if equal
.         match     "4911" to ownnum
.         goto      input if equal
.         match     "0033" to ownnum
.         goto      input if equal
.nocheck  MOVE      Ownnum TO nownfld
ownchk
          unpack    OWNNUM,str2,str4
.begin patch 1.31
           if         (str4 = "1866")
           call       debug
           goto       input
           endif
           if         (str4 = "2191" or str4 = "4911" or str4 = "0033")
           goto       input
           endif
.          match     "2191" to str4
.          goto input if equal
.          match     "4911" to str4
.          goto input if equal
.          match     "0033" to str4
.          goto input if equal
.end patch 1.31
nocheck
          move      str4,nownfld
.END PATCH 1.2 REPLACED LOGIC
         rep       zfill in nownfld
          call     nownkey
         read       PARfile,nownfld;;
         goto       input if not over
WRITE    WRITE      parFILE,nownfld;nownfld,b1,b5,ownocpy
         add       c1 to n5
         display   *p10:16,"records written = ",n5
         GOTO      INPUT
EOJ
         CLOSE     parFILE
         STOP
         include   ndatio.inc
         include   nownio.inc
         include   comlogic.inc

