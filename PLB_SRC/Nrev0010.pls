*nrev0010
.make sure a projdolr record for every revenue record
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
RELEASE  INIT     "1.2"       DMB 24MAY2004 Mailer Conversion
;RELEASE  INIT     "1.1"       ASH 04OCT2000 NEW SERVER ADDED
.RELEASE  INIT     "1.0"       dlh for 1997 04nov96
.
projtot  IFILE     keylen=5,fix=64
.keyed on source,clientid 2-6
.
revenue  ifile     keylen=6,fixed=1334 
revseq   file      fixed=1334
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
projdolr   iFILE     keylen=6,FIX=108
.
.TYPE     DIM      1   1-1   ---\
.SOURCE DIM     1      2-2------->  KEY=nrevfld
CLIENTID DIM    4      3-6------/
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
nrevfld DIM     6      1-6
CLIENT  DIM     25     7-31
PERCENT  FORM   1.2   32-35      PROJECTED CHANGE
unbilled form   8.2   36-46
.
JAN89   FORM    8.2   47-57     HOLDS ADJUSTED LR INC FOR MONTH
FEB89   FORM    8.2   58-68
MAR89   FORM    8.2  
APR89   FORM    8.2  
MAY89   FORM    8.2  
JUN89   FORM    8.2  
JUL89   FORM    8.2  
AUG89   FORM    8.2  
SEP89   FORM    8.2  
OCT89   FORM    8.2  
NOV89   FORM    8.2  
DEC89   FORM    8.2  168-178
.       
JAN     FORM    8.2  179-189      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC     FORM    8.2  300-310
.       
JAN1    FORM    8.2  311-321      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC1    FORM    8.2  432-442
.       
JAN92   FORM    8.2  443-453      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC92   FORM    8.2  564-574
.
JAN93   FORM    8.2  575-585      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC93   FORM    8.2  696-706
.
JAN94   FORM    8.2  707-717      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC94   FORM    8.2      838
.
JAN95   FORM    8.2  839-849      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC95   FORM    8.2      970
.       
JAN96   FORM    8.2  971-981      HOLDS ADJUSTED LR INC FOR MONTH
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
DEC96   FORM    8.2      1202
.
JAN97   FORM    8.2  1203-1213      HOLDS ADJUSTED LR INC FOR MONTH
FEB97   FORM    8.2  
MAR97   FORM    8.2  
APR97   FORM    8.2  
MAY97   FORM    8.2  
JUN97   FORM    8.2  
JUL97   FORM    8.2  
AUG97   FORM    8.2  
SEP97   FORM    8.2  
OCT97   FORM    8.2  
NOV97   FORM    8.2  
DEC97   FORM    8.2       1334
.       
         listend
crntyr  form    8.2
.
.
        MOVE     "NREV0010" TO PROGRAM
         MOVE    "Insure Revenue records have Proj" TO STITLE
         MOVE    "Names In The News Ca" TO COMPNME
         CALL     PAINT
.START PATCH 1.1 REPLACED LOGIC
.        open     projdolr,"\\nins1\e\DATA\projdolr"
.        open     revenue,"\\nins1\e\data\dbase\revenue"
        PACK     STR35,NTWKPATH1,"projdolr"
        open     projdolr,STR35
        PACK     STR35,NTWKPATH1,"dbase\revenue"
        open     revenue,STR35
.END PATCH 1.1 REPLACED LOGIC

main    read     revenue,seq;revvars
        goto     eoj if over
        read     projdolr,nrevfld;;
        goto     main if not over
        move     c0 to tmpdol
        move     c0 to olddole
        move     c0 to proj95e
        move     c0 to proj95ea
        move     c0 to proj97e
        filepi   1;projdolr
        write    projdolr,nrevfld;nrevfld,client,tmpdol,tmpdol,olddole,proj95e,proj95ea,proj96e,proj97e
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
