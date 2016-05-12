*nrev0009.DBS
.
.make sure a revenue record for every projdolr record
.....................................
pc       equ      0
         include  common.inc
         include  cons.inc
;Patch1.2
			include	compdd.inc
			include	cntdd.inc
.         include  nmlrdd.inc
;Patch1.2
         include  nowndd.inc
RELEASE  INIT     "1.2"       DMB 26MAY2004 Mailer Conversion
;RELEASE  INIT     "1.1"       ASH 04OCT2000 NEW SERVER ADDED
.RELEASE  INIT     "1.0"       dlh for 1997 04nov96
.
projtot  IFILE     keylen=8,fix=64
.keyed on source,clientid 1-6
.
revenue  ifile     keylen=8,fixed=1520
revseq   file      fixed=1520
mode     form     1                  1=current year 2=next year
R        init    "R"
E        init    "E"
per      form    2.2
rental   form    11
exchange form    11
calcper  form    11.2
FILL1    DIM     1
TYPE     DIM      1
SOURCE   DIM    1
.CLIENT  DIM    25
MONTH    FORM   2
YEAR     FORM   2
AR       FORM   8.2
AP       FORM   8.2
LR       FORM   8.2
QTY      FORM   8
ADJAR    FORM   8.2
ADJAP    FORM   8.2
ADJLR    FORM   8.2
JUNK     DIM    2
keyin10  form   10
...........................
projdolr   iFILE     keylen=8,FIX=164
.TYPE     DIM      1   1-1   ---\
.SOURCE DIM     1      2-2------->  KEY=nrevfld
CLIENTID DIM    6      3-8------/
.CLIENT DIM     25     7-31
............................
.nrevfld         DIM       6      1-6
.CLIENT  DIM       25     7-31
projdol  form      11     32-42
proj93   form      11     43-53
proj94   form      11     54-64
proj95   form      11     65-75
proj95a  form      11     76-86
proj96   form      11     87-97
proj97   form      11     98-108
.......................................
tmpdol   form      11
olddol1   form      11               ,this year
olddol   form      11                .last
olddole   form      11               . 
olddolr   form      11               .year
proj98    form      11
proj98a   form      11
proj99    form      11
proj98r   form      11
proj97ea  form      11
proj97ra  form      11
proj97e   form      11     
proj97r   form      11     
proj96e   form      11     
proj96r   form      11     
proj95e   form      11     
proj95r   form      11     
proj95ea  form      11     
proj95ra  form      11     
.       
total    form      11     
rent     form      11     
exch     form      11     
UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    8.2
MO       FORM    2

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
JAN99   FORM    8.2 1389-      HOLDS ADJUSTED LR INC FOR MONTH
FEB99   FORM    8.2 
MAR99   FORM    8.2 
APR99   FORM    8.2 
MAY99   FORM    8.2 
JUN99   FORM    8.2 
JUL99   FORM    8.2 
AUG99   FORM    8.2 
SEP99   FORM    8.2 
OCT99   FORM    8.2 
NOV99   FORM    8.2 
DEC99   FORM    8.2      -1520
.       
         listend
crntyr  form    8.2
.
.
        MOVE     "NREV0009" TO PROGRAM
         MOVE    "Insure Projections have revenue record" TO STITLE
         MOVE    "Names In The News Ca" TO COMPNME
         CALL     PAINT
.START PATCH 1.1 REPLACED LOGIC
.        open     projdolr,"\\nts0\c\DATA\projdolr"
.        open     revenue,"\\nins1\e\data\dbase\revenue"
        PACK     STR35,NTWKPATH1,"projdolr"
        open     projdolr,STR35
        PACK     STR35,NTWKPATH1,"dbase\revenue"
        open     revenue,STR35
.END PATCH 1.1 REPLACED LOGIC

main    read     projdolr,seq;nrevfld,client,tmpdol,tmpdol:
                 olddole,proj96e,proj95ea,proj96e,proj97e,proj97ea,proj98:
                 proj98a,proj99
        goto     eoj if over
        add      c1 to n5
        display  *p10:12,"records in ",n5
        
        read     revenue,nrevfld;;
        goto     main if not over
        add      c1 to n6
        display  *p10:14,"records Added ",n6
        move     c0 to jan99
        move     c0 to feb99
        move     c0 to mar99
        move     c0 to apr99
        move     c0 to may99
        move     c0 to jun99
        move     c0 to jul99
        move     c0 to aug99
        move     c0 to sep99
        move     c0 to oct99
        move     c0 to nov99
        move     c0 to dec99
        move     c0 to jan98
        move     c0 to feb98
        move     c0 to mar98
        move     c0 to apr98
        move     c0 to may98
        move     c0 to jun98
        move     c0 to jul98
        move     c0 to aug98
        move     c0 to sep98
        move     c0 to oct98
        move     c0 to nov98
        move     c0 to dec98
        move     c0 to jan97
        move     c0 to feb97
        move     c0 to mar97
        move     c0 to apr97
        move     c0 to may97
        move     c0 to jun97
        move     c0 to jul97
        move     c0 to aug97
        move     c0 to sep97
        move     c0 to oct97
        move     c0 to nov97
        move     c0 to dec97
        move     c0 to jan96
        move     c0 to feb96
        move     c0 to mar96
        move     c0 to apr96
        move     c0 to may96
        move     c0 to jun96
        move     c0 to jul96
        move     c0 to aug96
        move     c0 to sep96
        move     c0 to oct96
        move     c0 to nov96
        move     c0 to dec96
        move     c0 to jan95
        move     c0 to feb95
        move     c0 to mar95
        move     c0 to apr95
        move     c0 to may95
        move     c0 to jun95
        move     c0 to jul95
        move     c0 to aug95
        move     c0 to sep95
        move     c0 to oct95
        move     c0 to nov95
        move     c0 to dec95
        move     c0 to jan94
        move     c0 to feb94
        move     c0 to mar94
        move     c0 to apr94
        move     c0 to may94
        move     c0 to jun94
        move     c0 to jul94
        move     c0 to aug94
        move     c0 to sep94
        move     c0 to oct94
        move     c0 to nov94
        move     c0 to dec94
        move     c0 to jan93
        move     c0 to feb93
        move     c0 to mar93
        move     c0 to apr93
        move     c0 to may93
        move     c0 to jun93
        move     c0 to jul93
        move     c0 to aug93
        move     c0 to sep93
        move     c0 to oct93
        move     c0 to nov93
        move     c0 to dec93
        move     c0 to jan92
        move     c0 to feb92
        move     c0 to mar92
        move     c0 to apr92
        move     c0 to may92
        move     c0 to jun92
        move     c0 to jul92
        move     c0 to aug92
        move     c0 to sep92
        move     c0 to oct92
        move     c0 to nov92
        move     c0 to dec92
        move     c0 to jan1
        move     c0 to feb1
        move     c0 to mar1
        move     c0 to apr1
        move     c0 to may1
        move     c0 to jun1
        move     c0 to jul1
        move     c0 to aug1
        move     c0 to sep1
        move     c0 to oct1
        move     c0 to nov1
        move     c0 to dec1
        move     c0 to jan
        move     c0 to feb
        move     c0 to mar
        move     c0 to apr
        move     c0 to may
        move     c0 to jun
        move     c0 to jul
        move     c0 to aug
        move     c0 to sep
        move     c0 to oct
        move     c0 to nov
        move     c0 to dec
        move     c0 to jan89
        move     c0 to feb89
        move     c0 to mar89
        move     c0 to apr89
        move     c0 to may89
        move     c0 to jun89
        move     c0 to jul89
        move     c0 to aug89
        move     c0 to sep89
        move     c0 to oct89
        move     c0 to nov89
        move     c0 to dec89
         move    c0 to percent
         move    c0 to unbilled
        filepi   1;revenue
        write    revenue,nrevfld;revvars
        goto     main

EOJ     stop

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
;Patch1.2
			include	compio.inc
			include	cntio.inc
.         include  nmlrio.inc
;Patch1.2
         include  nownio.inc
         INCLUDE  COMLOGIC.INC

