* *****************************************************************************
* ROLODEX
* *****************************************************************************
PC       EQU     0
         INCLUDE   COMMON.INC
         INCLUDE   CONS.INC
         INCLUDE   NROLDD.INC
.
release  init    "2.41"      29Jun99 DLH lock out add mod
.beginning to phase out
.release  init    "2.4"      10Nov98 ASH ROLODEX Y2K, File expansion
.release  init    "2.3"      06jun95 DLH email field
.release  init    "2.2"      18nov94 DLH search prob.
.RELEASE  INIT    "2.1"      02MAR93 DLH  *EDIT.
.RELEASE         INIT    "2.0"      TOTAL REWRITE TO USE AGENDA DIRECTORY FILE FORMAT
.                           08JAN93 DLH.
.
. .................
. VARIABLES
. .................

VTAB     FORM      2
HIT      FORM      1
VALKEY   FORM      1
. ....................
. MAINLINE FOLLOWS
. .............................................................................
         TRAP      STOP IF F5
         MOVE      "ROLODEX" TO PROGRAM
         MOVE      "Names In The News Ca. Inc" TO COMPNME
         MOVE      "PHONE DIRECTORY" TO STITLE
         MOVE      "EXIT" TO PF5
         clock     date to today
         CALL      PAINT
         CALL      FUNCDISP
         OPEN      NROLFILE,NROLNAME
START
.    DISPLAY   *P25:1,*ES,"NIN TELEPHONE DIRECTORY":
         CALL       PAINT
         DISPLAY   *P35:05,"1) ADD ",*red,"Disabled ",*white:
                   *P35:06,"2) SEARCH":
                   *P35:07,"3) END":
                   *P30:09,"YOUR CHOICE ? _";
GETIT    CLEAR     STR1
         KEYIN     *P44:09,*T254,STR1;
.
         TYPE      STR1
         GOTO      GETIT IF NOT EQUAL
         MOVE      STR1 TO N1
         BRANCH    N1 OF ADD,SEARCH,STOP
.
         GOTO      GETIT
.
ADD      beep
         goto       start
	 CALL       PAINT
         move       c0 to hit
         CLEAR      NROLNAM
         CLEAR      NROLNOTE
         DISPLAY   *bgcolor=7,*red,*P1:4,"ADD",*bgcolor=1,*white:
                   *P20:04,"NAME      -  ",*RPTCHAR "_":30:
                   *P19:24,"ENTER '<' TO BACKUP  or * TO CLEAR SCREEN":
                   *dioff,*white;
GETIT0   DISPLAY   *P33:04,*RPTCHAR "_":30,*dioff,*white
         MOVE      B1,NROLNAM
         CLEAR     NROLNAM
         KEYIN     *P33:04,*rv,*edit,NROLNAM,*P33:04,*EL,*P33:04,*DV,NROLNAM;
         MATCH     "<",NROLNAM
         GOTO      START IF EQUAL
GETIT00  CMATCH    star TO NROLNAM
         GOTO      START IF EQUAL
         CMATCH    B1 TO NROLNAM
         IF        EOS
         DISPLAY   *P33:04,*EL,"REQUIRED",*B
         GOTO      GETIT0
         ENDIF
READ1
         CLEAR     NROLFLD
         APPEND    "01L",NROLFLD
         APPEND    NROLNAM,NROLFLD
         RESET     NROLFLD
.
READ1A   CLEAR     NROLFLD2
.
READ1B
         DISPLAY   *P01:24,*EL,"S-E-A-R-C-H-I-N-G  FOR DUPLICATES !!";
         MOVE      C0,HIT
         MOVE      "11",VTAB
         CALL      KEYEDIT
         COMPARE   C2,VALKEY
         GOTO      READ1C IF NOT EQUAL
         DISPLAY   *P1:24,*EL,*B,"NOT ENOUGH INFORMATION TO SEARCH ON !!!",*W3;
         GOTO      START
.
READ1C
         READ      NROLFILE,NROLFLD,NROLFLD2;NROLVARS
         GOTO      NOMORE IF OVER
         CALL      PAINT
         BRANCH    HIT OF READ1E
         DISPLAY   *P1:24,*EL,"PLEASE MAKE SURE THAT RECORD IS NOT ALREADY ON":
                   "FILE";
         MOVE      C1,HIT
         GOTO      READ1E
.
READ1F
         READKG    NROLFILE;NROLVARS
         GOTO      NOMORE IF OVER
.
READ1E
         ADD       C1,VTAB
         COMPARE   "21",VTAB
         CALL      SRESET IF NOT LESS
.Start Patch #2.4 - remmed and replaced logic
.         unpack    nroldate into mm,dd,yy
.         DISPLAY   *P1:VTAB,*EL,*RED,"> ",*WHITE,NROLNAM,B1,NROLPHON:
.                   b1,*green,mm,slash,dd,slash,yy,*white
         unpack    nroldate into str2,yy,mm,dd
         DISPLAY   *P1:VTAB,*EL,*RED,"> ",*WHITE,NROLNAM,B1,NROLPHON:
                   b1,*green,mm,slash,dd,slash,str2,yy,*white
.End Patch #2.4 - remmed and replaced logic
         ADD       C1,VTAB
         DISPLAY   *P1:VTAB,NROLADR1,B1,NROLADR2,B1,NROLADR3
         ADD       C1 TO VTAB
         DISPLAY   *P1:VTAB,*EL,"FAX## ",NROLFAX," NOTES":
                   " - ",NROLNOTE,B1,NROLZIP;
         ADD       C1 TO VTAB
         DISPLAY   *P1:VTAB,*CYAN,*RPTCHAR I196:80,*WHITE
         GOTO      READ1F
.
SRESET   MOVE      "07",VTAB
         KEYIN     *P1:24,*EL,"There are more hit #"ENTER#" to see the rest":
                   *T254,STR1;
         DISPLAY   *P1:VTAB,*EF:
                   "FILE";
         RETURN
NOMORE   BRANCH    HIT OF NOMORE1
         DISPLAY   *P1:24,*EL," NEW ENTRY!!";
         GOTO      GETIT2B1
NOMORE1  KEYIN    *P1:24,*EL,"That's all of them ...hit #"Y#" to add ":
                   "another",*T254,STR1;
         MATCH     YES,STR1
         GOTO      START IF NOT EQUAL
* ********
GETIT2B1 CALL      PAINT
         DISPLAY   *HON,*P1:4,"ADD",*HOFF:
                   *P20:04,"NAME      -  ",*dioff,*white,NROLNAM:
                   *P20:06,"ADDRESS   -  ",*RPTCHAR "_":30:
                   *P20:08,"ADDRESS   -  ",*RPTCHAR "_":30:
                   *P20:10,"CITY/ST   -  ",*RPTCHAR "_":30:
                   *P20:12,"ZIP       -  ",*RPTCHAR "_":10:
                   *P20:14,"PHONE##   -  ",*RPTCHAR "_":20:
                   *P20:16,"FAX##     -  ",*RPTCHAR "_":20:
                   *P20:18,"NOTES     -  ",*RPTCHAR "_":30:
                   *P20:20,"Email addr-  ",*RPTCHAR "_":30:
                   *P19:24,"ENTER '<' TO BACKUP  or * TO CLEAR SCREEN",*dioff,*white;
GETIT1   DISPLAY   *P33:04,*RPTCHAR "_":30,*dioff,*white
         CLEAR     NROLNAM
         KEYIN     *P33:04,*edit,NROLNAM
         CMATCH    "<" TO NROLNAM
         GOTO      START IF EQUAL
GETIT1B  CMATCH    star TO NROLNAM
         GOTO      START IF EQUAL
GETIT2   DISPLAY   *P33:06,*RPTCHAR "_":30
         CLEAR     NROLADR1
         KEYIN     *P33:06,*edit,NROLADR1
         CMATCH    "<" TO NROLADR1
         GOTO      GETIT1 IF EQUAL
GETIT2B  CMATCH    star TO NROLADR1
         GOTO      START IF EQUAL
GETIT3   DISPLAY   *P33:08,*RPTCHAR "_":30
         CLEAR     NROLADR2
         KEYIN     *P33:08,*edit,NROLADR2;
         MATCH     "<",NROLADR2
         GOTO      GETIT2B1 IF EQUAL
GETIT3C  CMATCH    star TO NROLADR2
         GOTO      START IF EQUAL
GETIT4   DISPLAY   *P33:10,*RPTCHAR "_":30
         CLEAR     NROLADR3
         KEYIN     *P33:10,*+,*edit,NROLADR3;
         CMATCH    "<" TO NROLADR3
         GOTO      GETIT3 IF EQUAL
GETIT4C  CMATCH      star TO NROLADR3
         GOTO      START IF EQUAL
GETIT5   DISPLAY   *P33:12,*RPTCHAR "_":10
         CLEAR     NROLZIP
         KEYIN     *P33:12,*edit,NROLZIP;
         CMATCH    "<" TO NROLZIP
         GOTO      GETIT4 IF EQUAL
GETIT5C  CMATCH    star TO NROLZIP
         GOTO      START IF EQUAL
GETIT6   DISPLAY   *P33:14,*RPTCHAR "_":20
         CLEAR     NROLPHON
         KEYIN     *P33:14,*edit,NROLPHON;
         CMATCH    "<" TO NROLPHON
         GOTO      GETIT5 IF EQUAL
GETIT6C  CMATCH    star TO NROLPHON
         GOTO      START IF EQUAL
GETIT7   DISPLAY   *P33:16,*RPTCHAR "_":10
         CLEAR     NROLFAX
         KEYIN     *P33:16,*edit,NROLFAX;
         CMATCH    "<" TO NROLFAX
         GOTO      GETIT4 IF EQUAL
GETIT7C  CMATCH    star TO NROLFAX
         GOTO      START IF EQUAL
NOTES    DISPLAY   *P33:18,*RPTCHAR "_":30
         KEYIN     *P33:18,*edit,NROLNOTE;
         MATCH     "<",NROLNOTE
         GOTO      GETIT7 IF EQUAL
         CMATCH    star TO NROLNOTE
         GOTO      START IF EQUAL
Email    DISPLAY   *P33:20,*RPTCHAR "_":30
         KEYIN     *P33:20,*edit,NROLadr4;
         MATCH     "<",NROLadr4
         GOTO      notes IF EQUAL
         CMATCH    star TO NROLadr4
         GOTO      START IF EQUAL
NEXT     KEYIN     *P20:22,"IS RECORD INFORMATION CORRECT ? ",STR1;
         MATCH     YES,STR1
         GOTO      NOTES IF NOT EQUAL
WRITE    unpack    today into mm,str1,dd,str1,yy
.Start Patch #2.4 - remmed and replaced logic
.         pack      nroldate from mm,dd,yy
         pack      nroldate from cc,yy,mm,dd
.End Patch #2.4 - remmed and replaced logic
         FILEPI    3;NROLFILE
         DISPLAY   *P1:23,"RECORD NOW BEING WRITTEN TO NROLFILE";
         WRITE     NROLFILE;NROLVARS
         GOTO      START
. ................................................
SEARCH   CALL       PAINT
         DISPLAY   *P1:4,*bgcolor=7,*red,"SEARCH",*bgcolor=1:
                   *P20:04,"NAME      -  ",*RPTCHAR "_":30:
                   *P20:06,"NOTES     -  ",*RPTCHAR "_":30:
                   *P19:24,"ENTER '<' TO BACKUP  or * TO CLEAR SCREEN",*dioff,*white;
SERIT0   DISPLAY   *P33:04,*RPTCHAR "_":30,*dioff,*white
         MOVE      B1,NROLNAM
         CLEAR     NROLNAM
         KEYIN     *P33:04,*rv,*edit,NROLNAM,*P33:04,*EL,*P33:04,*DV,NROLNAM;
         cMATCH     "<",NROLNAM
         GOTO      START IF EQUAL
SERIT00  SCAN      STAR IN  NROLNAM
         GOTO      START IF EQUAL
SERIT1   RESET     NROLNAM,1
         DISPLAY   *P33:06,*RPTCHAR "_":30,*dioff,*white
         MOVE      B1,NROLNOTE
         CLEAR     NROLNOTE
         KEYIN     *P33:06,*rv,*edit,NROLNOTE,*P33:06,*EL,*P33:06,*DV,NROLNOTE;
         MATCH     "<",NROLNOTE
         GOTO      SERIT0 IF EQUAL
SERIT1A  SCAN      STAR IN  NROLNOTE
         GOTO      START IF EQUAL
SERIT2   RESET     NROLNOTE
NEXT1
         CLEAR     NROLFLD
         TYPE      NROLNAM
         GOTO      NEXT1AA IF EOS
         CMATCH    B1,NROLNAM
         GOTO      NEXT1AA IF EQUAL
         APPEND    "01F",NROLFLD
         APPEND    NROLNAM,NROLFLD
         RESET     NROLFLD
         GOTO      NEXT1A
NEXT1AA  CLEAR     NROLFLD
.
NEXT1A
         CLEAR     NROLFLD2
         TYPE      NROLNOTE
         GOTO      NEXT1BB IF EOS
         CMATCH    B1,NROLNOTE
         GOTO      NEXT1BB IF EQUAL
         APPEND    "02F",NROLFLD2
         APPEND    NROLNOTE,NROLFLD2
         RESET     NROLFLD2
         GOTO      NEXT1B
NEXT1BB  CLEAR     NROLFLD2
.
NEXT1B
         CALL      KEYEDIT
         COMPARE   C2,VALKEY
         GOTO      NG IF EQUAL
         GOTO      OK
NG       DISPLAY   *P1:24,*EL,*B,"NOT ENOUGH INFORMATION TO SEARCH ON !!!",*W3;
         GOTO      START
.
OK       DISPLAY   *P01:24,*EL,"S-E-A-R-C-H-I-N-G";
         MOVE      C0,HIT
         MOVE      "07",VTAB
.
READ2C
         READ      NROLFILE,NROLFLD,NROLFLD2;NROLVARS
         GOTO      NOMORE2 IF OVER
         BRANCH    HIT OF READ1E
         MOVE      C1,HIT
         GOTO      READ2E
.
READ2F
         READKG    NROLFILE;NROLVARS
         GOTO      NOMORE3 IF OVER
.
READ2E
         DISPLAY   *P1:24,*EL,"FOUND";
         ADD       C1,VTAB
         COMPARE   "21",VTAB
         CALL      SRESET1 IF NOT LESS
.Start Patch #2.4 - remmed and replaced logic
.         unpack    nroldate into mm,dd,yy
.         DISPLAY   *P4:VTAB,*EL,*dion,*RED,"> ",*dioff,*WHITE,NROLNAM,B1,NROLPHON:
.                   b1,*green,mm,slash,dd,slash,yy,*white
         unpack    nroldate into str2,yy,mm,dd
         DISPLAY   *P4:VTAB,*EL,*dion,*RED,"> ",*dioff,*WHITE,NROLNAM,B1,NROLPHON:
                   b1,*green,mm,slash,dd,slash,str2,yy,*white
.End Patch #2.4 - remmed and replaced logic
         ADD       C1,VTAB
         DISPLAY   *P1:VTAB,NROLADR1,B1,NROLADR2,B1,NROLADR3
         ADD       C1 TO VTAB
         DISPLAY   *P1:VTAB,*EL,"FAX## ",*white,*dioff,NROLFAX:
                   " NOTES",*dioff,*white:
                   " - ",NROLNOTE,B1,NROLZIP;
         ADD       C1 TO VTAB
         cmatch    b1 to nroladr4
         goto      read2ex if equal
         goto      read2ex if eos
         display   *p1:vtab,"Email address ",nroladr4
         add       c1 to vtab
read2ex  DISPLAY   *P1:VTAB,*CYAN,*RPTCHAR I196:80,*WHITE
         GOTO      READ2F
.
SRESET1
         KEYIN     *P01:24,*EL,"There are more hit ENTER to see the rest":
                   *T254,STR1;
         CMATCH    star,STR1
         GOTO      START IF EQUAL
         MOVE      "07",VTAB
         DISPLAY   *P1:VTAB,*EF;
         RETURN
.no more2 no hit on free float lets try left justify.
nomore2  
         goto      nomore3                 .weird bug skip this section
.                                          09sep96 DLH         
         clear     nrolfld
         APPEND    "01L",NROLFLD
         APPEND    NROLNAM,NROLFLD
         RESET     NROLFLD
         READ      NROLFILE,NROLFLD,NROLFLD2;NROLVARS
         GOTO      NOMORE3 IF OVER
         BRANCH    HIT OF READ1E
         MOVE      C1,HIT
         GOTO      READ2E
.        
NOMORE3   BRANCH    HIT OF NOMORE4
         KEYIN     *P1:24,"No records found hit ENTER ",*T254,STR1;
         GOTO      START
NOMORE4  KEYIN     *P1:24,*EL,"Thats all I can find, hit ENTER",*T254,STR1;
         CMATCH     "9",STR1
         GOTO      DELETE IF EQUAL
         GOTO      START
* ********
STOP     CLOSE     NROLFILE
         DISPLAY   *ES,*P35:12,"GOOD-BYE !!!";
         STOP
NOHIT    DISPLAY   *P1:24,*B,"NO HIT",*W1;
         GOTO      START
KEYEDIT
         MOVE      C0,VALKEY
.
         CMATCH    B1,NROLNAM
         CALL      CUME IF EQUAL
.
         CMATCH    B1,NROLNOTE
         CALL      CUME IF EQUAL
.
         RETURN
CUME     ADD       C1,VALKEY
         RETURN
DELETE
         DISPLAY   *P01:24,*EL,"D-E-L-E-T-I-N-G ";
         DELETE    NROLFILE
         DISPLAY   *P01:24,*EL,"G O N E";
         GOTO      START
         INCLUDE   COMLOGIC.inc

