*nrev0003.DBS reads revenue.dat and updates mailer/owner names, output 
* TO REVENUE.new
.
. First revenue.dbf must be copied as a flat file (Named revenue.txt)
. ---> run nrev0003
. go back into dbase delete all revenue.dbf records and import the records
. from the revenue.new (save a backup copy)
.
.....................................
pc       equ      0

	 include  common.inc
	 include  cons.inc
;Patch1.4
	include	compdd.inc
	include	cntdd.inc
.	 include  nmlrdd.inc
;patch1.4
	 include  ndatdd.inc
release  init     "1.4"       DMB 26MAY2004 Mailer Conversion
;release  init     "1.3"       DLH 22Sep98 increase client field add 98,99
.RELEASE  INIT     "1.2"       dlh 11/04/96 add 97 & UNBILLED DATA
.RELEASE  INIT     "1.1"       dlh 11/13/95 add 96 & f: drive  
.RELEASE  INIT     "1.0"       dlh 10/20/93  
.
.INPUT    FILE     fix=9592
.INPUT    FILE     fix=1334      INCREASES BY 132 EVERY YEAR
INput     file     fix=1498
.SEE FILE DESC IN NREV5
FILL1   DIM     1
TYPE     DIM      1
SOURCE	DIM 	1
MONTH	FORM	2
YEAR	FORM	2
AR	FORM	8.2
AP	FORM	8.2
LR	FORM	8.2
QTY	FORM	8
ADJAR	FORM	8.2
ADJAP	FORM	8.2
ADJLR	FORM	8.2
JUNK	DIM	2
...........................
.OUTPUT 	IFILE     KEYLEN=6,FIX=695 increases by 132 ever year
.OUTPUT 	FILE     FIX=1334
OUTPUT 	FILE     FIX=1520
.
.TYPE     DIM      1   1-1   ---\
.SOURCE	DIM 	1      2-2------->  KEY=nrevfld
CLIENTID DIM    6      3-8------/
............................
revvars  list
nrevfld DIM     8      1-8
.patch 5.1
.CLIENT  DIM     25     7-31
CLIENT  DIM     45     9-53
.end patch 5.1
PERCENT  FORM   1.2   54-57      PROJECTED CHANGE
unbilled form   8.2   58-68
.
JAN89   FORM    8.2   69-79     HOLDS ADJUSTED LR INC FOR MONTH
FEB89   FORM    8.2   80-90
MAR89   FORM    8.2   91-101
APR89   FORM    8.2   102-112
MAY89   FORM    8.2   113-123
JUN89   FORM    8.2   124-134
JUL89   FORM    8.2   135-145
AUG89   FORM    8.2   146-156
SEP89   FORM    8.2   157-167
OCT89   FORM    8.2   168-178
NOV89   FORM    8.2   179-189
DEC89   FORM    8.2   190-200
.       
JAN     FORM    8.2   201-211      HOLDS ADJUSTED LR INC FOR MONTH
FEB     FORM    8.2  
MAR     FORM    8.2  
APR     FORM    8.2  
MAY     FORM    8.2  
JUN     FORM    8.2  
JUL     FORM    8.2  
AUG     FORM    8.2  
SEP     FORM    8.2  
OCT     FORM    8.2  
NOV     FORM    8.2   
DEC     FORM    8.2      -332
.       
JAN1    FORM    8.2  333-343      HOLDS ADJUSTED LR INC FOR MONTH
FEB1    FORM    8.2  
MAR1    FORM    8.2  
APR1    FORM    8.2  
MAY1    FORM    8.2  
JUN1    FORM    8.2  
JUL1    FORM    8.2  
AUG1    FORM    8.2  
SEP1    FORM    8.2   
OCT1    FORM    8.2  
NOV1    FORM    8.2  
DEC1    FORM    8.2     -464
.       
JAN92   FORM    8.2  465-475      HOLDS ADJUSTED LR INC FOR MONTH
FEB92   FORM    8.2  
MAR92   FORM    8.2  
APR92   FORM    8.2  
MAY92   FORM    8.2  
JUN92   FORM    8.2  
JUL92   FORM    8.2  
AUG92   FORM    8.2  
SEP92   FORM    8.2   
OCT92   FORM    8.2  
NOV92   FORM    8.2  
DEC92   FORM    8.2     -596
.
JAN93   FORM    8.2  597-      HOLDS ADJUSTED LR INC FOR MONTH
FEB93   FORM    8.2  
MAR93   FORM    8.2  
APR93   FORM    8.2  
MAY93   FORM    8.2  
JUN93   FORM    8.2  
JUL93   FORM    8.2  
AUG93   FORM    8.2  
SEP93   FORM    8.2   
OCT93   FORM    8.2  
NOV93   FORM    8.2  
DEC93   FORM    8.2     -728
.
JAN94   FORM    8.2  729-739      HOLDS ADJUSTED LR INC FOR MONTH
FEB94   FORM    8.2  
MAR94   FORM    8.2  
APR94   FORM    8.2  
MAY94   FORM    8.2  
JUN94   FORM    8.2  
JUL94   FORM    8.2  
AUG94   FORM    8.2  
SEP94   FORM    8.2   
OCT94   FORM    8.2  
NOV94   FORM    8.2  
DEC94   FORM    8.2     -860
.       
JAN95   FORM    8.2  861-871      HOLDS ADJUSTED LR INC FOR MONTH
FEB95   FORM    8.2  
MAR95   FORM    8.2  
APR95   FORM    8.2  
MAY95   FORM    8.2  
JUN95   FORM    8.2  
JUL95   FORM    8.2  
AUG95   FORM    8.2  
SEP95   FORM    8.2   
OCT95   FORM    8.2  
NOV95   FORM    8.2  
DEC95   FORM    8.2      -992
.       
JAN96   FORM    8.2   993-994      HOLDS ADJUSTED LR INC FOR MONTH
FEB96   FORM    8.2     
MAR96   FORM    8.2     
APR96   FORM    8.2     
MAY96   FORM    8.2     
JUN96   FORM    8.2     
JUL96   FORM    8.2     
AUG96   FORM    8.2     
SEP96   FORM    8.2     
OCT96   FORM    8.2     
NOV96   FORM    8.2     
DEC96   FORM    8.2      -1124
.       
JAN97   FORM    8.2            HOLDS ADJUSTED LR INC FOR MONTH
FEB97   FORM    8.2  1125-1126
MAR97   FORM    8.2      
APR97   FORM    8.2      
MAY97   FORM    8.2      
JUN97   FORM    8.2      
JUL97   FORM    8.2      
AUG97   FORM    8.2      
SEP97   FORM    8.2      
OCT97   FORM    8.2      
NOV97   FORM    8.2      -
DEC97   FORM    8.2      -1256
.       
JAN98   FORM    8.2  1257-1267  HOLDS ADJUSTED LR INC FOR MONTH
FEB98   FORM    8.2     -
MAR98   FORM    8.2      
APR98   FORM    8.2      
MAY98   FORM    8.2      
JUN98   FORM    8.2      
JUL98   FORM    8.2      
AUG98   FORM    8.2      
SEP98   FORM    8.2      
OCT98   FORM    8.2      
NOV98   FORM    8.2      
DEC98   FORM    8.2      -1388
.       
JAN99	FORM	8.2 1389-      HOLDS ADJUSTED LR INC FOR MONTH
FEB99	FORM	8.2 
MAR99	FORM	8.2 
APR99	FORM	8.2 
MAY99	FORM	8.2 
JUN99	FORM	8.2 
JUL99	FORM	8.2 
AUG99	FORM	8.2 
SEP99	FORM	8.2 
OCT99	FORM	8.2 
NOV99	FORM	8.2 
DEC99	FORM	8.2      -1520
.	
         listend
UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    8.2
MO       FORM    2
.
.
	MOVE	 "nREV0003" TO PROGRAM
	 MOVE	 "UPDATE REVENUE FILE" TO STITLE
	 MOVE    "Names In The News Ca" TO COMPNME
 	 CALL	  PAINT
 	 move     c1 to ndatpath
 	 move     c1 to nmlrpath
	OPEN	INPUT,"f:\data\revenue",READ
 	prepare    OUTPUT,"f:\data\REVENUE.new"
.
INPUT   TRAP     FORMAT1 IF FORMAT
	READ     INPUT,SEQ;revvars
	GOTO 	EOJ IF OVER
	TRAPCLR  FORMAT
	SUB      SEQ FROM INCOUNT
	DISPLAY  *P10:10,"RECORDS IN ",INCOUNT
        unpack   nrevfld into type,source,clientid
.
	CMATCH   "M" TO SOURCE
        IF        EQUAL
        clear     ndatfld
        pack      ndatfld from clientid
        rep       zfill in ndatfld
        call      ndatkey
        if        over
        move      "List not found" to client
        else 
        move     Olstname to client
        endif
        MOVE      " " TO TYPE
        goto      write
        ENDIF
.
	CMATCH   "B" TO SOURCE
        IF        EQUAL
        clear     mkey
        clear     str4
        unpack    clientid into str2,str4
        pack      mkey from str4,z3
        rep      zfill in mkey
        call      nmlrkey
        if        over
        move      "mailer not found" to client
        else 
        move     mcomp to client
        endif
        goto      write
        ENDIF
	display *p1:24,*el,"missing source ",clientid
         goto    input
.
.
.
write	WRITE    OUTPUT,seq;revvars
.
	 ADD     C1 TO  WRTCOUNT
	 DISPLAY   *P10:14,"RECORDS WRITTEN : ",WRTCOUNT

	GOTO	INPUT
EOJ     CLOSE   INPUT
        weof    output,seq
  	CLOSE	OUTPUT
	STOP

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
;Patch1.4
	include	compio.inc
	include	cntio.inc
.  include   nmlrio.inc
;Patch1.4
         include   ndatio.inc
  	 INCLUDE  COMLOGIC.INC

