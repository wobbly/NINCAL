*nrev0007.DBS asks for clientid # then reads projtot.dat,revenue.dat 
. and updates projdolr.dat
.
. go back into dbase use projdolr index projdolr, delete all projdolr.dbf 
. records and import the records from the projdolr.dat (save a backup copy)
.
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
;RELEASE  INIT     "1.1"       ash 04OCT2000 NEW SERVER ADDED
.RELEASE  INIT     "1.0"       dlh for 1997 04nov96
.
projtot  IFILE     keylen=5,fix=64
.keyed on source,clientid 2-6
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
        MOVE     "NREV0007" TO PROGRAM
         MOVE    "UPDATE 1997 projdolr FILE" TO STITLE
         MOVE    "Names In The News Ca" TO COMPNME
         CALL     PAINT
.START PATCH 1.1 REPLACED LOGIC
.        OPEN     projtot,"\\nts0\c\DATA\projtot",READ
.        open     projdolr,"\\nts0\c\DATA\projdolr"
.        open     revenue,"\\nins1\e\data\dbase\revenue"
.        open     revseq,"\\nins1\e\data\dbase\revenue"
.
        PACK     STR35,NTWKPATH1,"projtot"
        OPEN     projtot,STR35,READ
        PACK     STR35,NTWKPATH1,"projdolr"
        open     projdolr,STR35
        PACK     STR35,NTWKPATH1,"dbase\revenue"
        open     revenue,STR35
        open     revseq,STR35
.END PATCH 1.1 REPLACED LOGIC
        move     c1 to nmlrpath
mode   
. keyin    *p10:12,"(C)urrent year 1996 or (N)ew year 1997 ",*uc,str1
        move     c2 to mode
        branch   mode of old,new
        goto     mode
old    display   *p20:4,*Ef,*red,"Modify Year (96)",*hoff,*white
.        goto      keyin
new    display   *p20:4,*Ef,*red,"Modify Year (97)",*hoff,*white
       goto      passone
passone read     revseq,seq;revvars
         goto    eoj if over
        unpack    nrevfld into str1,source,clientid
        cmatch   "B" to source
        if       equal
        pack     mkey from str4,z3
        call     nmlrkey
        display  *p10:13,*el,mcomp
        endif
        cmatch   "M" to source
        if       equal
        pack     nownfld from str4
        call     nownkey
        display  *p10:13,*el,ownocpy
        endif
        cmatch   "B" to source
        goto     lm if not equal
.
        pack     nrevfld from E,source,clientid
        rep      zfill in client id
        move     c0 to olddol1
        move     c0 to olddol
        move     c0 to olddolr
        move     c0 to olddole
        move     c0 to proj95r
        move     c0 to proj95e
        move     c0 to proj95ra
        move     c0 to proj95ea
        move     c0 to proj96r
        move     c0 to proj96e
        move     c0 to proj97r
        move     c0 to proj97e
        move     c0 to jan96
        move     c0 to feb96
        move     c0 to mar96
        move     c0 to apr96
        move     c0 to may96
        move     c0 to jun97
        move     c0 to jul96
        move     c0 to aug96
        move     c0 to sep96
        move     c0 to oct96
        move     c0 to nov96
        move     c0 to dec96
        move     c0 to crntyr
        filepi   1;projdolr
        read     projdolr,nrevfld;str6,client,tmpdol,tmpdol,olddole,proj96e,proj95ea,proj96e,proj97e
        READ     revenue,nrevfld;revvars
        add      jan96 to crntyr
        add      feb96 to crntyr
        add      mar96 to crntyr
        add      apr96 to crntyr
        add      may96 to crntyr
        add      jun96 to crntyr
        add      jul96 to crntyr
        add      aug96 to crntyr
        add      sep96 to crntyr
        add      oct96 to crntyr
        add      nov96 to crntyr
        add      dec96 to crntyr
        
        pack     nrevfld from R,source,clientid
        filepi   1;projdolr
        read     projdolr,nrevfld;str6,client,tmpdol,tmpdol,olddolr,proj95r,proj95ra,proj96r,proj97r
        READ     revenue,nrevfld;revvars
        add      jan96 to crntyr
        add      feb96 to crntyr
        add      mar96 to crntyr
        add      apr96 to crntyr
        add      may96 to crntyr
        add      jun96 to crntyr
        add      jul96 to crntyr
        add      aug96 to crntyr
        add      sep96 to crntyr
        add      oct96 to crntyr
        add      nov96 to crntyr
        add      dec96 to crntyr
.        add     olddole to olddol
.        add     olddolr to olddol
        add     proj96e to olddol
        add     proj96r to olddol
        add     proj97e to olddol1
        add     proj97r to olddol1
        move    c0 to keyin10
        move    crntyr to keyin10
        display *p10:16,*el,"old ammounts: last year ",olddol," This year ",olddol1
amount  display *p10:14,*el,"new amount ",*p21:14,keyin10:
                *p65:14,"ok? ",yes
.
        move     c0 to n10
        move     keyin10 to n10
        compare  c0 to n10                  .new amount 0?
        if       equal                       yes
        move     c0 to exchange               "
        move     c0 to rental                 "
        goto     updbrk                       "
        endif
.
        compare  c0 to total                  .previous amount 0?
        if       equal                        .yes
        mult     ".5" by n10
        move      n10 to exchange
        move      n10 to rent
        goto      updbrk
        endif
.        
        compare  total to rent               .100 per rental?
        if       equal                        .yes
        move     n10 to rental                .yes
        move     c0 to exchange               .yes
        goto     updbrk                       .yes
        endif
. 
        compare  c0 to rent               .100 per exchange?
        if       equal                        .yes
        move     n10 to exchange              .yes
        move     c0 to rent                   .yes
        goto     updbrk                       .yes
        endif
.
        move     c0 to calcper
        move     rent to calcper
        div      total into calcper
        move     n10 to rental
        mult     calcper by rental
        sub      rental from n10
        move     n10 to exchange        
        goto       updbrk
.        
.lm - list management - currently handled elsewhere.
lm      move     "A" to str1
.        keyin    *p10:14,*el,"(P)erc change, (",*cyan,"A",*white,")mount ",*rv,str1
.        cmatch    "P" to str1
.        goto      lm1 if equal
.        cmatch    star to str1
.        goto      keyin if equal                
.        cmatch    "A" to str1
.        goto      lm if not equal
        pack     nrevfld from b1,source,clientid
        READ     revenue,nrevfld;revvars
        add      jan96 to crntyr
        add      feb96 to crntyr
        add      mar96 to crntyr
        add      apr96 to crntyr
        add      may96 to crntyr
        add      jun96 to crntyr
        add      jul96 to crntyr
        add      aug96 to crntyr
        add      sep96 to crntyr
        add      oct96 to crntyr
        add      nov96 to crntyr
        add      dec96 to crntyr
        mult     "1.05" by crntyr
        move     crntyr to n10
amount1 display  *p10:14,*el,"new amount ",*p21:14,n10:
                 *p65:14,"ok? ",yes
.        cmatch   no to str1
.        goto     amount1 if equal
.         cmatch  star to str1
.         goto    keyin if equal
         goto    update
.
lm1      keyin    *p10:14,*el,"perc change ",per,*p65:14,"ok? ",str1
        cmatch    no to str1
        goto      lm if equal
        cmatch    star to str1
.        goto      keyin if equal                
        goto      update
.
updbrk  TRAP     FORMAT1 IF FORMAT
        pack     nrevfld from E,source,clientid
        rep      zfill in client id
        filepi   1;projdolr
        read     projdolr,nrevfld;str6,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97
        if       over
        move     c0 to proj97
        move     c0 to proj96
        move     c0 to proj95a
        move     c0 to proj95
        move     c0 to proj93
        move     c0 to projdol
        move     c0 to proj94
         compare  c2 to mode
         if       equal
         move     exchange to proj97
         else
         move     exchange to proj96
         endif
        pack     mkey from clientid,z3
        call     nmlrkey
        move     mcomp to client
        filepi   1;projdolr
        write     projdolr,nrevfld;nrevfld,client,projdol,proj93:
                  proj94,proj95,proj95a,proj96,proj97
      else  
         compare  c2 to mode
         if       equal
         move     exchange to proj97
         else
         move     exchange to proj96
         endif
         filepi   1;projdolr
         update   projdolr;nrevfld,client,projdol,proj93:
                  proj94,proj95,proj95a,proj96,proj97
         endif
        pack     nrevfld from R,source,clientid
        rep      zfill in client id
        filepi   1;projdolr
        read     projdolr,nrevfld;str6,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97
        if       over
        move     c0 to proj97
        move     c0 to proj96
        move     c0 to proj95
        move     c0 to proj95a
        move     c0 to proj93
        move     c0 to projdol
        move     c0 to proj94
        compare  c2 to mode
        if       equal
        move     rental to proj97
        else
        move     rental to proj96
        endif
        pack     mkey from clientid,z3
        call     nmlrkey
        move     mcomp to client
        filepi   1;projdolr
        write     projdolr,nrevfld;nrevfld,client,projdol,proj93:
                  proj94,proj95,proj95a,proj96,proj97
        else  
        compare  c2 to mode
        if       equal
        move     rental to proj97
        else
        move     rental to proj96
        endif
        filepi   1;projdolr
        update   projdolr;nrevfld,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97
        endif
        display  *p1:23,*el,"Total ",total," Rent ",rent," % ",calcper:
                 *p1:24,"client ",client," rental ",rental," exchange ",exchange
        goto     passone
.
update
        TRAP     FORMAT1 IF FORMAT
        pack     nrevfld from b1,source,clientid
        rep      zfill in clientid
        filepi   1;projdolr
        read     projdolr,nrevfld;str6,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97
        if       over
        move     c0 to proj97
        move     c0 to proj96
        move     c0 to proj95
        move     c0 to proj95a
        move     c0 to proj93
        move     c0 to projdol
        move     c0 to proj94
        compare  c2 to mode
        if       equal
        move     n10 to proj97
        else
        move     n10 to proj96
        endif
        pack     nownfld from clientid
        call     nownkey
        move     ownocpy to client
        filepi   1;projdolr
        write    projdolr,nrevfld;nrevfld,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97
        else  
        compare  c2 to mode
        if       equal
        move     n10 to proj97
        else
        move     n10 to proj96
        endif
        filepi   1;projdolr
        update   projdolr;nrevfld,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97
        endif
        goto     passone
EOJ     stop

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
;Patch1.2
.         include  nmlrio.inc
			include	compio.inc
			include	cntio.inc
;Patch1.2
         include  nownio.inc
         INCLUDE  COMLOGIC.INC

