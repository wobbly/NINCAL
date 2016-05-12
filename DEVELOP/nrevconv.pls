*nrev0001.DBS converts INCOME7.dbf(DAT) format TO REVENUE.DBF(DAT) FORMAT
* and adds income7 records to revenue file.
.
. First revenue.dbf must be copied as a flat file (Named revenue.DAT)
. and indexed on type,source,client,ccyy 1-12
. then income7 copied to a flat file (Named income7.DAT).
. ---> run revenue
. go back into dbase delete all revenue.dbf records and import the records
. from the flat file.
.
.....................................
pc       equ      0
         include  common.inc
         include  cons.inc
release  init      "5.31"       DLH 14DEC2007 PLI
.release  init      "5.30"       JD 28DEC2001 Revenue master no longer DBF. \\nts1\e\data\text
.release  init      "5.22"       ASH 02OCT2000 NEW SERVER ADDED
.release  init      "5.21"       JD    28feb00 changed year to (dim) for write.
.release  init      "5.2"       DLH   6May99 NININV Y2k, New vars
.proposed new format for revenue file.
.release  init      "5.1"       DLH   22Sep98 client name 25 to 45 add 1999
.RELEASE  INIT     "5.0"        dlh 18mAY98  CLIENT ID FROM 4 TO 6
.                              LIST OWNER REPLACED BY LIST NUMBER
.RELEASE  INIT     "4.3"        JD 01/27/98  prepped for  1998. RECS
.RELEASE  INIT     "4.2"       dlh 11/04/96  prepped for  1997. RECS
.RELEASE  INIT     "4.2"       dlh 11/04/96  prepped for  19.97. RECS
.RELEASE  INIT     "4.1"       dlh 11/13/95  prepped for  1996. RECS
.release  init      "4.0"          DLH 05sep95 include unbilled.
.RELEASE  INIT     "3.4"       dlh 10/19/94  prepped for  1995. RECS
.RELEASE  INIT     "3.3"       dlh 10/19/93  prepped for  1994. RECS
.RELEASE  INIT     "3.2"       JD 1/26/93  ADDED 1993. RECS
.RELEASE  INIT     "3.1"       DLH 1/27/92  ADDED 1992. RECS
..RELEASE  INIT     "3.0"       DLH 12/18/91  ADDED 1989. RECS
.
.begin patch 5.2
.INPUT    FILE     VAR=163
.begin patch 5.31
.INPUT    FILE     VAR=178
INPUT    FILE     VAR=192
.end patch 5.31
.end patch 5.2
.
TYPE     DIM      1          1-1
SOURCE  DIM     1            2-2
RevCFlag Dim       1                    P or N       -KEY as of Dec 07
.id num         6            3-8
owner   dim     4
.CC
YEAR    DIM     2           11-12
MONTH   FORM    2            9-10
.dd
mohold  form    3
FILL1   DIM     1           13-13
AR      FORM    10.2         14-49
AP      FORM    10.2         50-60
LR      FORM    10.2         61-71
.begin patch 5.2
NINTOT  form    10.2
.end patch 5.2
QTY     FORM    8           72-79
ADJAR   FORM    10.2         80-90
ADJAP   FORM    10.2         91-101
ADJLR   FORM    10.2        102-112
.begin patch 5.2
ADJNINTOT  form    10.2
.end patch 5.2
unbilinc form      8.2     113-123
JUNK    DIM     2
...........................
.
.OUTPUT         IFILE     KEYLEN=6,FIX=695 increases by 132 ever year
.OUTPUT         IFILE     KEYLEN=6,FIX=959        for 95'
.OUTPUT         IFILE     KEYLEN=6,FIX=970        for 95 with unbilled
.OUTPUT         IFILE     KEYLEN=6,FIX=1102        96'
.OUTPUT         IFILE     KEYLEN=6,FIX=1235        97'
.OUTPUT  IFILE     KEYLEN=8,FIX=1369               98
.begin patch 5.2
.OUTPUT         IFILE     keylen=8,FIX=1520                ..99
.begin patch 3.51
.OUTPUT  IFILE     keylen=12,FIX=380
OUTPUT  IFILE     keylen=13,FIX=381
.end patch 3.51
.end patch 5.2

.begin patch 5.2
.TYPE     DIM      1   1-1   ---\
.SOURCE DIM     1      2-2------->  KEY=nrevfld
CLIENTID DIM    6      3-8------/
............................
revvars  list
.begin patch 3.51
nrevfld DIM     13       1-13       type,source,clientid,revcflag,ccyy
.nrevfld DIM     12       1-12       type,source,clientid,ccyy
.end patch 3.51
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

..TYPE     DIM      1   1-1   ---\
.SOURCE DIM     1      2-2------->  KEY=nrevfld
.CLIENTID DIM    6      3-8------/
............................
.revvars  list
.nrevfld DIM     8      1-8
..patch 5.1
.CLIENT  DIM     25     7-31
.CLIENT  DIM     45     9-53
.end patch 5.1
.PERCENT  FORM   1.2   54-57      PROJECTED CHANGE
.unbilled form   8.2   58-68
.
.JAN89   FORM    8.2   69-79     HOLDS ADJUSTED LR INC FOR MONTH
.FEB89   FORM    8.2   80-90
.MAR89   FORM    8.2   91-101
.APR89   FORM    8.2   102-112
.MAY89   FORM    8.2   113-123
.JUN89   FORM    8.2   124-134
.JUL89   FORM    8.2   135-145
.AUG89   FORM    8.2   146-156
.SEP89   FORM    8.2   157-167
.OCT89   FORM    8.2   168-178
.NOV89   FORM    8.2   179-189
.DEC89   FORM    8.2   190-200
..       
.JAN     FORM    8.2   201-211      HOLDS ADJUSTED LR INC FOR MONTH
.FEB     FORM    8.2  
.MAR     FORM    8.2  
.APR     FORM    8.2  
.MAY     FORM    8.2  
.JUN     FORM    8.2  
.JUL     FORM    8.2  
.AUG     FORM    8.2  
.SEP     FORM    8.2  
.OCT     FORM    8.2  
.NOV     FORM    8.2   
.DEC     FORM    8.2      -332
..       
.JAN1    FORM    8.2  333-343      HOLDS ADJUSTED LR INC FOR MONTH
.FEB1    FORM    8.2  
.MAR1    FORM    8.2  
.APR1    FORM    8.2  
.MAY1    FORM    8.2  
.JUN1    FORM    8.2  
.JUL1    FORM    8.2  
.AUG1    FORM    8.2  
.SEP1    FORM    8.2   
.OCT1    FORM    8.2  
.NOV1    FORM    8.2  
.DEC1    FORM    8.2     -464
..       
.JAN92   FORM    8.2  465-475      HOLDS ADJUSTED LR INC FOR MONTH
.FEB92   FORM    8.2  
.MAR92   FORM    8.2  
.APR92   FORM    8.2  
.MAY92   FORM    8.2  
.JUN92   FORM    8.2  
.JUL92   FORM    8.2  
.AUG92   FORM    8.2  
.SEP92   FORM    8.2   
.OCT92   FORM    8.2  
.NOV92   FORM    8.2  
.DEC92   FORM    8.2     -596
..
.JAN93   FORM    8.2  597-      HOLDS ADJUSTED LR INC FOR MONTH
.FEB93   FORM    8.2  
.MAR93   FORM    8.2  
.APR93   FORM    8.2  
.MAY93   FORM    8.2  
.JUN93   FORM    8.2  
.JUL93   FORM    8.2  
.AUG93   FORM    8.2  
.SEP93   FORM    8.2   
.OCT93   FORM    8.2  
.NOV93   FORM    8.2  
.DEC93   FORM    8.2     -728
..
.JAN94   FORM    8.2  729-739      HOLDS ADJUSTED LR INC FOR MONTH
.FEB94   FORM    8.2  
.MAR94   FORM    8.2  
.APR94   FORM    8.2  
.MAY94   FORM    8.2  
.JUN94   FORM    8.2  
.JUL94   FORM    8.2  
.AUG94   FORM    8.2  
.SEP94   FORM    8.2   
.OCT94   FORM    8.2  
.NOV94   FORM    8.2  
.DEC94   FORM    8.2     -860
..       
.JAN95   FORM    8.2  861-871      HOLDS ADJUSTED LR INC FOR MONTH
.FEB95   FORM    8.2  
.MAR95   FORM    8.2  
.APR95   FORM    8.2  
.MAY95   FORM    8.2  
.JUN95   FORM    8.2  
.JUL95   FORM    8.2  
.AUG95   FORM    8.2  
.SEP95   FORM    8.2   
.OCT95   FORM    8.2  
.NOV95   FORM    8.2  
.DEC95   FORM    8.2      -992
..       
.JAN96   FORM    8.2   993-994      HOLDS ADJUSTED LR INC FOR MONTH
.FEB96   FORM    8.2     
.MAR96   FORM    8.2     
.APR96   FORM    8.2     
.MAY96   FORM    8.2     
.JUN96   FORM    8.2     
.JUL96   FORM    8.2     
.AUG96   FORM    8.2     
.SEP96   FORM    8.2     
.OCT96   FORM    8.2     
.NOV96   FORM    8.2     
.DEC96   FORM    8.2      -1124
..       
.JAN97   FORM    8.2            HOLDS ADJUSTED LR INC FOR MONTH
.FEB97   FORM    8.2  1125-1126
.MAR97   FORM    8.2      
.APR97   FORM    8.2      
.MAY97   FORM    8.2      
.JUN97   FORM    8.2      
.JUL97   FORM    8.2      
.AUG97   FORM    8.2      
.SEP97   FORM    8.2      
.OCT97   FORM    8.2      
.NOV97   FORM    8.2      -
.DEC97   FORM    8.2      -1256
..       
.JAN98   FORM    8.2  1257-1267  HOLDS ADJUSTED LR INC FOR MONTH
.FEB98   FORM    8.2     -                                    .
.MAR98   FORM    8.2                                          .
.APR98   FORM    8.2                                          .
.MAY98   FORM    8.2      
.JUN98   FORM    8.2                                         .
.JUL98   FORM    8.2      
.AUG98   FORM    8.2      
.SEP98   FORM    8.2      
.OCT98   FORM    8.2      
.NOV98   FORM    8.2      
.DEC98   FORM    8.2      -1388
.                                  .
.JAN99  FORM    8.2 1389-      HOLDS ADJUSTED LR INC FOR MONTH
.FEB99  FORM    8.2 
.MAR99  FORM    8.2 
.APR99  FORM    8.2 
.MAY99  FORM    8.2 
.JUN99  FORM    8.2 
.JUL99  FORM    8.2 
.AUG99  FORM    8.2 
.SEP99  FORM    8.2 
.OCT99  FORM    8.2 
.NOV99  FORM    8.2 
.DEC99  FORM    8.2      -1520
.       
.       listend

.end patch 5.2

UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    10.2
MO       FORM    2
.
.
        MOVE     "REVENUE" TO PROGRAM
         MOVE    "UPDATE REVENUE FILE" TO STITLE
         MOVE    "Names In The News Ca" TO COMPNME
         CALL     PAINT
        display   *p1:24,*el,"opening income7";
.START PATCH 5.22 REPLACED LOGIC
.        OPEN    INPUT,"g:\data\INCOME7.DAT",READ
        PACK    STR35,NTWKPATH1,"INCOME7.DAT"
	MOve	"c:\work\Income7.dat",str35
        OPEN    INPUT,STR35,READ
.END PATCH 5.22 REPLACED LOGIC
        display   *p1:24,*el,"income7 opened",*w2;
        display   *p1:24,*el,"opening Revenue";
.        OPEN    OUTPUT,"\\nts1\e\data\dbase\REVENUE"
.START PATCH 5.30
.        OPEN    OUTPUT,"REVENUE"
	OPen 	INput,"c:\work\revenue.old"
        Prepare    OUTPUT,"c:\work\REVENUE.dat","c:\work\REVENUE.isi","13","381"
.End PATCH 5.30
        display   *p1:24,*el,"Revenue Open",*w2;
        
Super        
        read	Input,seq;Type,Source,ClientID,Str4: 
		CLIENT:
		JANlr:  
		FEBlr:  
		MARlr:  
		APRlr:  
		MAYlr:  
		JUNlr:  
		JULlr:  
		AUGlr:  
		SEPlr:  
		OCTlr:  
		NOVlr:  
		DEClr:  
		JANnin:
		FEBnin: 
		MARnin: 
		APRnin: 
		MAYnin: 
		JUNnin: 
		JULnin: 
		AUGnin: 
		SEPnin: 
		OCTnin: 
		NOVnin: 
		DECnin: 
		unbilled

        goto	Neweoj if over
        pack	Nrevfld from Type,Source,ClientID,"N",Str4
        Write	Output,Nrevfld;NrevFld:
		CLIENT:
		JANlr:  
		FEBlr:  
		MARlr:  
		APRlr:  
		MAYlr:  
		JUNlr:  
		JULlr:  
		AUGlr:  
		SEPlr:  
		OCTlr:  
		NOVlr:  
		DEClr:  
		JANnin:
		FEBnin: 
		MARnin: 
		APRnin: 
		MAYnin: 
		JUNnin: 
		JULnin: 
		AUGnin: 
		SEPnin: 
		OCTnin: 
		NOVnin: 
		DECnin: 
		unbilled

        goto	SUper
NewEOj
	WEof	output,seq
	stop
.
INPUT   TRAP     FORMAT1 IF FORMAT
        READ     INPUT,SEQ;TYPE,SOURCE,CLIENTID,REVCFlag,CC,year,MONth,FILL1,CLIENT:
                 owner,ar,AP,LR,nintot,QTY,ADJAR,ADJAP,ADJLR,adjnintot,unbilinc
        GOTO    EOJ IF OVER
        TRAPCLR  FORMAT
        CMATCH   " " TO CLIENT
        GOTO     INPUT IF EOS
        SUB      SEQ FROM INCOUNT
        DISPLAY  *P10:10,"RECORDS IN ",INCOUNT
        ADD      ADJLR TO LR
.begin patch 5.2
        add      adjnintot to NINtot
.end patch 5.2
.
        CMATCH   "M" TO SOURCE
        IF        EQUAL
        MOVE      " " TO TYPE
        ENDIF
.
        add     month to mohold
        PACK    nrevfld FROM TYPE,SOURCE,CLIENTID,REVCFlag,cc,year
        TRAP    FORMAT2 IF FORMAT
.        display *p1:24,*el,"key = ",nrevfld,b1,lr
        READ    OUTPUT,nrevfld;;
        GOTO    WRITE IF OVER
.
        READ    OUTPUT,nrevfld;revvars
        TRAPCLR FORMAT
********************************************************************
.begin patch 5.2
.       Compare  "90" to year
.        if      equal
.        add     "12" to mohold
.        endif
.        Compare  "91" to year
.        if      equal
.        add     "24" to mohold
.        endif
..        Compare  "92" to year
. .       if      equal
. .       add     "36" to mohold
. .       endif
.        Compare  "93" to year
.        if      equal
.        add     "48" to mohold
.        endif
.        Compare  "94" to year
.        if      equal
.        add     "60" to mohold
.        endif
.        Compare  "95" to year
.        if      equal
.        add     "72" to mohold
.        endif
.        Compare  "96" to year
.        if      equal
.        add     "84" to mohold
.        endif
.        Compare  "97" to year
.        if      equal
.        add     "96" to mohold
.        endif
.        Compare  "98" to year
.        if      equal
.        add     "108" to mohold
.        endif
.        Compare  "99" to year
.        if      equal
.        add     "120" to mohold
.        endif
..       "72" = 1995
..       "84" = 1996
..       "96" = 1997
..       ADD     "108" TO mohold         .increase by 12 every year
********************************************************************
        LOAD    CALC FROM MOhold OF JANLR:
                FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR,SEPLR,OCTLR:
                NOVLR,DECLR
.               

        ADD     LR TO CALC
.start PATCH 5.30
        move    c0 to unbilled
.end PATCH 5.30
        add     unbilinc to unbilled
.
        STORE   CALC INTO mohold OF JANLR:
                FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR,SEPLR,OCTLR:
                NOVLR,DECLR

        LOAD    CALC FROM MOhold OF JANNIN:
                FEBNIN,MARNIN,APRNIN,MAYNIN,JUNNIN,JULNIN,AUGNIN,SEPNIN,OCTNIN:
                NOVNIN,DECNIN

        add   nintot to calc

        Store   CALC into MOhold OF JANNIN:
                FEBNIN,MARNIN,APRNIN,MAYNIN,JUNNIN,JULNIN,AUGNIN,SEPNIN,OCTNIN:
                NOVNIN,DECNIN

         UPDATE  OUTPUT;revvars

         ADD     C1 TO  UPDCOUNT
         DISPLAY   *P10:12,"RECORDS UPDATED : ",UPDCOUNT
         move    c0 to mohold
         GOTO    INPUT
.
WRITE   PACK    nrevfld FROM TYPE,SOURCE,CLIENTID,REVCFlag,cc,year
.begin patch 5.2
.        MOVE    C0 TO JAN89
.        MOVE    C0 TO FEB89
.        MOVE    C0 TO MAR89
.        MOVE    C0 TO APR89
.        MOVE    C0 TO MAY89
.        MOVE    C0 TO JUN89
.        MOVE    C0 TO JUL89
.        MOVE    C0 TO AUG89
.        MOVE    C0 TO SEP89
.        MOVE    C0 TO OCT89
.        MOVE    C0 TO NOV89
.        MOVE    C0 TO DEC89

        MOVE    C0 TO JANLR
        MOVE    C0 TO FEBLR
        MOVE    C0 TO MARLR
        MOVE    C0 TO APRLR
        MOVE    C0 TO MAYLR
        MOVE    C0 TO JUNLR
        MOVE    C0 TO JULLR
        MOVE    C0 TO AUGLR
        MOVE    C0 TO SEPLR
        MOVE    C0 TO OCTLR
        MOVE    C0 TO NOVLR
        MOVE    C0 TO DECLR

        MOVE    C0 TO JANNIN
        MOVE    C0 TO FEBNIN
        MOVE    C0 TO MARNIN
        MOVE    C0 TO APRNIN
        MOVE    C0 TO MAYNIN
        MOVE    C0 TO JUNNIN
        MOVE    C0 TO JULNIN
        MOVE    C0 TO AUGNIN
        MOVE    C0 TO SEPNIN
        MOVE    C0 TO OCTNIN
        MOVE    C0 TO NOVNIN
        MOVE    C0 TO DECNIN

.        MOVE    C0 TO JAN1
.        MOVE    C0 TO FEB1
.        MOVE    C0 TO MAR1
.        MOVE    C0 TO APR1
.        MOVE    C0 TO MAY1
.        MOVE    C0 TO JUN1
.        MOVE    C0 TO JUL1
.        MOVE    C0 TO AUG1
.        MOVE    C0 TO SEP1
.        MOVE    C0 TO OCT1
.        MOVE    C0 TO NOV1
.        MOVE    C0 TO DEC1
.        MOVE    C0 TO JAN
.        MOVE    C0 TO FEB
.        MOVE    C0 TO MAR
.        MOVE    C0 TO APR
.        MOVE    C0 TO MAY
.        MOVE    C0 TO JUN
.        MOVE    C0 TO JUL
.        MOVE    C0 TO AUG
.        MOVE    C0 TO SEP
.        MOVE    C0 TO OCT
.        MOVE    C0 TO NOV
.        MOVE    C0 TO DEC
.        MOVE    C0 TO JAN92
.        MOVE    C0 TO FEB92
.        MOVE    C0 TO MAR92
.        MOVE    C0 TO APR92
.        MOVE    C0 TO MAY92
.        MOVE    C0 TO JUN92
.        MOVE    C0 TO JUL92
.        MOVE    C0 TO AUG92
.        MOVE    C0 TO SEP92
.        MOVE    C0 TO OCT92
.        MOVE    C0 TO NOV92
.        MOVE    C0 TO DEC92
.        MOVE    C0 TO JAN93
.        MOVE    C0 TO FEB93
.        MOVE    C0 TO MAR93
.        MOVE    C0 TO APR93
.        MOVE    C0 TO MAY93
.        MOVE    C0 TO JUN93
.        MOVE    C0 TO JUL93
.        MOVE    C0 TO AUG93
.        MOVE    C0 TO SEP93
.        MOVE    C0 TO OCT93
.        MOVE    C0 TO NOV93
.        MOVE    C0 TO DEC93
.        MOVE    C0 TO JAN94
.        MOVE    C0 TO FEB94
.        MOVE    C0 TO MAR94
.        MOVE    C0 TO APR94
.        MOVE    C0 TO MAY94
.        MOVE    C0 TO JUN94
.        MOVE    C0 TO JUL94
.        MOVE    C0 TO AUG94
.        MOVE    C0 TO SEP94
.        MOVE    C0 TO OCT94
.        MOVE    C0 TO NOV94
.        MOVE    C0 TO DEC94
.        MOVE    C0 TO JAN95
.        MOVE    C0 TO FEB95
.        MOVE    C0 TO MAR95
.        MOVE    C0 TO APR95
.        MOVE    C0 TO MAY95
.        MOVE    C0 TO JUN95
.        MOVE    C0 TO JUL95
.        MOVE    C0 TO AUG95
.        MOVE    C0 TO SEP95
.        MOVE    C0 TO OCT95
.        MOVE    C0 TO NOV95
.        MOVE    C0 TO DEC95
.        MOVE    C0 TO JAN96
.        MOVE    C0 TO FEB96
.        MOVE    C0 TO MAR96
.        MOVE    C0 TO APR96
.        MOVE    C0 TO MAY96
.        MOVE    C0 TO JUN96
.        MOVE    C0 TO JUL96
.        MOVE    C0 TO AUG96
.        MOVE    C0 TO SEP96
.        MOVE    C0 TO OCT96
.        MOVE    C0 TO NOV96
.        MOVE    C0 TO DEC96
.        MOVE    C0 TO JAN97
.        MOVE    C0 TO FEB97
.        MOVE    C0 TO MAR97
.        MOVE    C0 TO APR97
.        MOVE    C0 TO MAY97
.        MOVE    C0 TO JUN97
.        MOVE    C0 TO JUL97
.        MOVE    C0 TO AUG97
.        MOVE    C0 TO SEP97
.        MOVE    C0 TO OCT97
.        MOVE    C0 TO NOV97
.        MOVE    C0 TO DEC97
.        MOVE    C0 TO JAN98
.        MOVE    C0 TO FEB98
.        MOVE    C0 TO MAR98
.        MOVE    C0 TO APR98
.        MOVE    C0 TO MAY98
.        MOVE    C0 TO JUN98
.        MOVE    C0 TO JUL98
.        MOVE    C0 TO AUG98
.        MOVE    C0 TO SEP98
.        MOVE    C0 TO OCT98
.        MOVE    C0 TO NOV98
.        MOVE    C0 TO DEC98
.        MOVE    C0 TO JAN99
.        MOVE    C0 TO FEB99
.        MOVE    C0 TO MAR99
.        MOVE    C0 TO APR99
.        MOVE    C0 TO MAY99
.        MOVE    C0 TO JUN99
.        MOVE    C0 TO JUL99
.        MOVE    C0 TO AUG99
.        MOVE    C0 TO SEP99
.        MOVE    C0 TO OCT99
.        MOVE    C0 TO NOV99
.        MOVE    C0 TO DEC99
        move    c0 to unbilled
.end patch 5.2
.
        move    unbilinc to unbilled
.        MOVE    C0 TO PERCENT       *NEW  NO PROJECTED INCOME.
        MOVE    C0 TO MOHOLD
        MOVE    MONTH TO MOHOLD
.***************************************************************************
.begin patch 5.2
.Compare  "90" to year
.        if      equal
.        add     "12" to mohold
.        endif
.        Compare  "91" to year
.        if      equal
.        add     "24" to mohold
.        endif
.        Compare  "92" to year
.        if      equal
.        add     "36" to mohold
.        endif
.        Compare  "93" to year
.        if      equal
.        add     "48" to mohold
.        endif
.        Compare  "94" to year
.        if      equal
.        add     "60" to mohold
.        endif
.        Compare  "95" to year
.        if      equal
.        add     "72" to mohold
.        endif
.        Compare  "96" to year
.        if      equal
.        add     "84" to mohold
.        endif
.        Compare  "97" to year
.        if      equal
.        add     "96" to mohold
.        endif
.        Compare  "98" to year
.        if      equal
.        add     "108" to mohold
.        endif
.        Compare  "99" to year
.        if      equal
.        add     "120" to mohold
.        endif
.***************************************************************************

.       ADD     "96" TO mohold       .increase by 12 every year
.        ADD     "108" TO mohold       .increase by 12 every year
.
        STORE   LR INTO mohold OF JANLR:
                FEBLR,MARLR,APRLR,MAYLR,JUNLR,JULLR,AUGLR,SEPLR,OCTLR:
                NOVLR,DECLR
        STORE   NINTOT INTO mohold OF JANNIN:
                FEBNIN,MARNIN,APRNIN,MAYNIN,JUNNIN,JULNIN,AUGNIN,SEPNIN,OCTNIN:
                NOVNIN,DECNIN
.end patch 5.2
        WRITE    OUTPUT,nrevfld;revvars
.
         ADD     C1 TO  WRTCOUNT
         DISPLAY   *P10:14,"RECORDS WRITTEN : ",WRTCOUNT
         MOVE    C0 TO MOHOLD

        GOTO    INPUT
EOJ     CLOSE   INPUT
        CLOSE   OUTPUT
        STOP

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT:
                *b,*w10;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
         INCLUDE  COMLOGIC.INC

