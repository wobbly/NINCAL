*nrev0005.DBS reads revenue.dat and creats output 
* TO projtot.dat 
.
. First revenue.dbf must be copied as a flat file (Named revenue.dat)
. ---> run nrev0005
.
.....................................
pc       equ      0
         include  common.inc
         include  cons.inc
;Patch2.5
.         include  nmlrdd.inc
          include   compdd.inc
          include   cntdd.inc
;Patch2.5
         include  ndatdd.inc
release   init     "2.6"       DLH Sendmail
reldate   Init      "19 February 2010"
.release   init     "2.5"       26MAY2004 DMB Mailer Conversion
.release   init     "2.4"       04OCT2000 ASH NEW SERVER ADDED
.release   init     "2.3"       22Nov99 DLH add 99 old year 2000 revenue file changes etc.
.release  init     "2.2"       22Feb99 DLH use new err message
.Release  init     "2.1"       DLH 22Sep98 name 25 to 45 add 99
.release  init     "2.0"       DLH 14Jul98 updated to use list number not lo as key

.RELEASE  INIT     "1.2"       dlh 04NOV96 add 97'
.RELEASE  INIT     "1.1"       dlh 11/13/95 add 96'  
.RELEASE  INIT     "1.0"       dlh 10/20/93  
.
INPUT    FILE     fix=380
.OUTPUT         IFILE     keylen=12,FIX=380
.increases in size by 132 bytes per year .1999 nov no longer increases
.
total    form      11     32-42
rent     form      11     43-53
exch     form      11     54-64
.
total1   form      11
rent1    form      11
.
FILL1   DIM     1
TYPE     DIM      1
SOURCE  DIM     1
MONTH   FORM    2
YEAR    FORM    2
AR      FORM    8.2
AP      FORM    8.2
LR      FORM    8.2
QTY     FORM    8
ADJAR   FORM    8.2
ADJAP   FORM    8.2
ADJLR   FORM    8.2
JUNK    DIM     2
...........................
OUTPUT  IFILE     keylen=8,FIX=66
.
outkey  dim       8
.TYPE     DIM      1   1-1   ---\
.SOURCE DIM     1      2-2------->  KEY=nrevfld
CLIENTID DIM    6      3-8------/
.total    form      11     9-19
.rent     form      11    20-30

............................
revvars  list
nrevfld DIM     12       1-12       type,source,clientid,ccyy
CLIENT  DIM     45      13-57
.
JANlr   FORM    10.2    58-70     HOLDS ADJUSTED LR INC FOR MONTH
FEBlr   FORM    10.2    71-83
MARlr   FORM    10.2    84-96
APRlr   FORM    10.2    97-109
MAYlr   FORM    10.2   110-122
JUNlr   FORM    10.2   123-135
JULlr   FORM    10.2   136-148
AUGlr   FORM    10.2   149-161
SEPlr   FORM    10.2   162-174
OCTlr   FORM    10.2   175-187
NOVlr   FORM    10.2   188-200
DEClr   FORM    10.2   201-213
.       
JANnin     FORM    10.2   214-226      HOLDS ADJUSTED NIN INcome
FEBnin     FORM    10.2   227-239
MARnin     FORM    10.2   240-252
APRnin     FORM    10.2   253-265
MAYnin     FORM    10.2   266-278
JUNnin     FORM    10.2   279-291
JULnin     FORM    10.2   292-304
AUGnin     FORM    10.2   305-317
SEPnin     FORM    10.2   318-330
OCTnin     FORM    10.2   331-343
NOVnin     FORM    10.2   344-356
DECnin     FORM    10.2   357-369
unbilled   form   8.2     370-380
        listend
UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    8.2
MO       FORM    2
.
.
        MOVE     "NREV0005" TO PROGRAM
         MOVE    "Create Projtot FILE" TO STITLE
         MOVE    "Names In The News" TO COMPNME
         CALL     PAINT
         move     c1 to ndatpath
         move     c1 to nmlrpath
        trap      io giving error if io
.START PATCH 2.4 REPLACED LOGIC
.        OPEN    INPUT,"\\nins1\e\data\dbase\revenue",READ
.        prepare    OUTPUT,"\\nins1\e\data\projtot","\\nins1\e\data\projtot","8","66"
        pack       str35,NTWKPATH1,"dbase\revenue"
        OPEN       INPUT,STR35,READ
        pack       str35,NTWKPATH1,"projtot"
        pack       str45,NTWKPATH1,"projtot"
        prepare    OUTPUT,str35,str45,"8","66"
.END PATCH 2.4 REPLACED LOGIC
.
INPUT   TRAP     FORMAT1 giving error IF FORMAT
        READ     INPUT,SEQ;revvars
        GOTO    EOJ IF OVER
        TRAPCLR  FORMAT
        SUB      SEQ FROM INCOUNT
        DISPLAY  *P10:10,"RECORDS IN ",INCOUNT
        unpack   nrevfld into type,source,clientid,str4
.
.       CMATCH   "M" TO SOURCE
.        IF        EQUAL
        if        (source = "M")
                  IF           (str4 = "1997" or str4 = "1998" or str4 = "1999") 
        move      c0 to total
        move      c0 to rent
        move      c0 to exch
        add       janlr,total
        add       feblr,total
        add       marlr to total
        add       aprlr,total
        add       maylr to total
        add       junlr to total
        add       jullr to total
        add       auglr to total
        add       seplr to total
        add       octlr to total
        add       novlr to total
        add       declr to total
        MOVE      " " TO TYPE
        goto      write
        ENDIF
        endif
.
.       CMATCH   "B" TO SOURCE
.        IF        EQUAL
        if        (source = "B")
                  IF           (str4 = "1997" or str4 = "1998" or str4 = "1999") 
        move      c0 to total
        move      c0 to rent
        move      c0 to exch
        add       janlr,total
        add       feblr,total
        add       marlr to total
        add       aprlr,total
        add       maylr to total
        add       junlr to total
        add       jullr to total
        add       auglr to total
        add       seplr to total
        add       octlr to total
        add       novlr to total
        add       declr to total
         cmatch    "R" to type
         if         equal
        add       janlr,rent
        add       feblr,rent
        add       marlr to rent
        add       aprlr,rent
        add       maylr to rent
        add       junlr to rent
        add       jullr to rent
        add       auglr to rent
        add       seplr to rent
        add       octlr to rent
        add       novlr to rent
        add       declr to rent
        endif
        goto      update
        ENDIF
        endif
         goto    input
.
.
update   clear    outkey
         
         pack     outkey from b1,source,clientid
         move     c0 to total1
         move     c0 to rent1
         read     output,outkey;outkey,total1,rent1
         goto     write if over
         add      total1 to total
         add      rent1 to rent
         update   output;outkey,total,rent
         goto     input
.
write   
         clear    outkey
         pack     outkey from b1,source,clientid
         WRITE    OUTPUT,outkey;outkey,total,rent
.
         ADD     C1 TO  WRTCOUNT
         DISPLAY   *P10:14,"RECORDS WRITTEN : ",WRTCOUNT

        GOTO    INPUT
EOJ     CLOSE   INPUT
.        weof    output,seq
        CLOSE   OUTPUT
        STOP

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT,*w4;
.begin patch 2.2
.         call       Errmess
      Move    "This is a Error e-mail from NREV0005",MailSubjct

          Clear     Mailbody
          append    "This is an error message",Mailbody
          append    CRLF,Mailbody
          append    error,Mailbody
          append    CRLF,Mailbody
          append    "Input file format error  ",mailbody    
          append    CRLF,Mailbody
          append    Source,Mailbody
          append    B1,Mailbody
          append    Type,Mailbody
          append    B1,Mailbody
          append    Client,Mailbody          
          append    CRLF,Mailbody
          reset     Mailbody
          move      "Creques@nincal.com",mailfrom
          move      "Creques@nincal.com",mailto
          call      SendMail
.       call     errmess
.end patch 2.2
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",outkey,*w4;
.        call    errmess
.begin patch 2.2
.         call       Errmess
      Move    "This is a Error e-mail from NREV0005",MailSubjct

          Clear     Mailbody
          append    "This is an error message",Mailbody
          append    CRLF,Mailbody
          append    error,Mailbody
          append    CRLF,Mailbody
          append    "Output file format error  ",mailbody    
          append    CRLF,Mailbody
          append    Outkey,Mailbody
          append    CRLF,Mailbody
          reset     Mailbody
          move      "Creques@nincal.com",mailfrom
          move      "Creques@nincal.com",mailto
          call      SendMail


.       call     errmess
.end patch 2.2
        STOP
IO      
.begin patch 2.2
.         call       Errmess
      Move    "This is a Error e-mail from NREV0005",MailSubjct

          Clear     Mailbody
          append    "This is an error message",Mailbody
          append    CRLF,Mailbody
          append    error,Mailbody
          append    CRLF,Mailbody
          append    "I/O file format error  ",mailbody    
          append    CRLF,Mailbody
          reset     Mailbody
          move      "Creques@nincal.com",mailfrom
          move      "Creques@nincal.com",mailto
          call      SendMail

.       call     errmess
.end patch 2.2
.call    errmess
        stop
;Patch2.5
          include   compio.inc
          include   cntio.inc
.         include   nmlrio.inc
;Patch2.5
         include   ndatio.inc
         INCLUDE  COMLOGIC.INC

