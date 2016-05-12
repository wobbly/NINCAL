*nrev0006.DBS asks for clientid # then reads projtot.dat and updates projdolr.dat
.
. go back into dbase use projdolr index projdolr, delete all projdolr.dbf 
. records and import the records from the projdolr.dat (save a backup copy)
.
.....................................
pc       equ      0
           include  common.inc
           include  cons.inc
;Patch1.4
			include	compdd.inc
			include	cntdd.inc
.         include  nmlrdd.inc
;Patch1.4
.         include  nowndd.inc
         include  ndatdd.inc
release       init            "1.4"           26JAN2006 DMB added code for data folder restructure         
.Release  init      "1.4"         26MAY2004	Mailer Conversion
.release  init      "1.3"         .DLH add 99old and 2000
.release  init     "1.2"          DLH owner replaced by list, name field from 25 to 45
.                                add 1998a & 1999
.release  init     "1.13"         DLH add revised 1997 04aug97
.RELEASE  INIT     "1.12"       dlh for 1997 04nov96
.RELEASE  INIT     "1.1"       dlh for 1996 11/13/95  
.RELEASE  INIT     "1.0"       dlh 10/20/93  
.
projtot  IFILE     keylen=8,fix=66
.keyed on b1,source,clientid 1-8
.
revenue  ifile     keylen=12,fixed=380
mode     form     1                  1=current year 2=next year
R        init    "R"
E        init    "E"
per      form    2.2
rental   form    11
exchange form    11
calcper  form    11.2
FILL1    DIM     1
TYPE     DIM      1
SOURCE     DIM      1
.CLIENT    DIM      45
MONTH      FORM     2
YEAR       FORM     2
AR         FORM     8.2
AP         FORM     8.2
LR         FORM     8.2
QTY        FORM     8
ADJAR      FORM     8.2
ADJAP      FORM     8.2
ADJLR      FORM     8.2
JUNK       DIM      2
keyin10  form   10
...........................
projdolr   iFILE     keylen=8,FIX=207
.
projfld   dim      8
.TYPE     DIM      1       1-1   ---\
.SOURCE   DIM       1      2-2------->  KEY=nrevfld
CLIENTID DIM    6          3-8------/
.CLIENT   DIM       45     9-53
............................
.nrevfld   DIM         8      1-8
.CLIENT    DIM         45     9-53
projdol  form      11          -64
proj93   form      11          -75
proj94   form      11          -86
proj95   form      11          -97
proj95a  form      11          -108
proj96   form      11          -119
proj97   form      11          -130
proj97a  form      11          -141
proj98   form      11          -152
proj98a  form      11          -164
proj99   form      11          -175
proj99old   form      11          -186
proj2000   form      11          -197
nin2000    form      11           208
.......................................
tmpdol   form      11
olddol1   form      11               ,this year
olddol   form      11                .last.
olddole   form      11               . 
olddolr   form      11               .year.
proj2000e   FOrm      11
Proj2000r   FOrm      11
proj99ex   FOrm      11                .old
Proj99rx   FOrm      11                .old
proj99e   FOrm      11
Proj99r   FOrm      11
proj98ea  form      11               .2nd projection for 1998
proj98ra  form      11               .2nd projection for 1998     
proj98e   FOrm      11
Proj98r   FOrm      11
proj97ea  form      11               .2nd projection for 1997
proj97ra  form      11               .2nd projection for 1997     
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
crntyr  form    8.2
.
.
          MOVE       "NREV0006" TO PROGRAM
           MOVE      "UPDATE 2000 projdolr FILE" TO STITLE
           MOVE    "Names In The News Ca" TO COMPNME
           CALL       PAINT
         move      c1 to ndatpath
         move      c3 to ndatlock
         move      c3 to nmlrlock
        OPEN       projtot,"f:\DATA\projtot",READ
         open     projdolr,"f:\DATA\projdolr"
.Patch 1.5         
.        open     revenue,"\\nins1\e\data\dbase\revenue"
        open     revenue,"\\nins1\e\storage\dbase\revenue"
.Patch 1.5        
.        open     revenue,"\\nins1\e\data\dbase\revenue"
        move     c1 to nmlrpath
mode
.    keyin    *p10:12,"(C)urrent year 1998 or (N)ew year 1999 ",*uc,str1
        rep      "C1N2" in str1
        move     c0 to mode
        move     str1 to mode
        move     c2 to mode
        branch   mode of old,new
        goto     mode
old    display   *p20:4,*Ef,*red,"Modify Year (98)",*hoff,*white
        goto      keyin
new    display   *p20:4,*Ef,*red,"Modify Year (2000)",*hoff,*white
        goto      keyin
.
keyin   keyin    *p10:12,*el,"source & id ",*p22:12,*dv,source,"/",*dv,str6:
                 *p22:12,*+,*rv,source,"/",*zf,*jr,str6,*-
        trap     oops if f5
        clear    str7
        scan     "?" in str6
        if       equal
        call     mlrhelp
        clear    str6
        move     "00" to str2

         pack    str6 from str2,mnum
.        move     mnum to str6
        endif
        pack     str7 from source,str6
        scan     star in str7
        stop     if equal
        reset     str7
        cmatch   "B" to source
        if       equal
        clear    str4
        unpack   str6 into str2,str4
        pack     mkey from str4,z3
        call     nmlrkey
        display  *p10:13,*el,mcomp
        endif
        cmatch   "M" to source
        if       equal
        pack     ndatfld from str6
        call     ndatkey
        display  *p10:13,*el,olstname
        reset    str7
        move     c0 to total
        move     c0 to rent
        pack     projfld from b1,str7
        unpack    str7 into source,clientid
        goto      LM
        endif
        reset    str7
        move     c0 to total
        move     c0 to rent
        pack     projfld from b1,str7
        filepi   1;projtot
        read     projtot,projfld;b1,source,clientid,total,rent
        if       over
        display  *p1:24,*el,*blinkon,str6," not found!!!, lm amount only pls",*blinkoff
        unpack    str7 into source,clientid
        cmatch    "M" to source
        goto      lm if equal
        endif
        cmatch   "B" to source
        goto     lm if not equal
        move     "1999" to str4
        pack     nrevfld from E,source,clientid,str4
        rep      zfill in clientid
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
        move     c0 to janlr
        move     c0 to feblr
        move     c0 to marlr
        move     c0 to aprlr
        move     c0 to maylr
        move     c0 to junlr
        move     c0 to jullr
        move     c0 to auglr
        move     c0 to seplr
        move     c0 to octlr
        move     c0 to novlr
        move     c0 to declr
        move     c0 to crntyr
        pack     projfld from E,source,clientid
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,tmpdol,tmpdol,olddole,proj95e:
                 proj95ea,proj96e,proj97e,proj97ea,proj98e,proj98ea,proj99e,proj99ex,proj2000e
        READ     revenue,nrevfld;revvars
        move     c0 to crntyr
        add      janlr to crntyr
        add      feblr to crntyr
        add      marlr to crntyr
        add      aprlr to crntyr
        add      maylr to crntyr
        add      junlr to crntyr
        add      jullr to crntyr
        add      auglr to crntyr
        add      seplr to crntyr
        add      octlr to crntyr
        add      novlr to crntyr
        add      declr to crntyr
        
        move     "1999" to str4
        pack     nrevfld from R,source,clientid,str4
        pack     projfld from R,source,clientid
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,tmpdol,tmpdol,olddolr,proj95r:
                 proj95ra,proj96r,proj97r,proj97ra,proj98r,proj98ra,proj99r,proj99rx,proj2000r
        READ     revenue,nrevfld;revvars
        add      janlr to crntyr
        add      feblr to crntyr
        add      marlr to crntyr
        add      aprlr to crntyr
        add      maylr to crntyr
        add      junlr to crntyr
        add      jullr to crntyr
        add      auglr to crntyr
        add      seplr to crntyr
        add      octlr to crntyr
        add      novlr to crntyr
        add      declr to crntyr
.        add     olddole to olddol
.        add     olddolr to olddol
        add     proj99e to olddol
        add     proj99r to olddol
.        add     proj98e to olddol1
.        add     proj98r to olddol1
        move    c0 to keyin10
        move    crntyr to keyin10
        display *p10:16,*el,"old ammounts:  ",olddol:
                " ",crntyr
.                " year98  ",olddol1," YTD ",crntyr
amount  keyin    *p10:14,*el,"new amount ",*p21:14,*dv,keyin10:
                 *p21:14,*rv,keyin10
        keyin    *p21:14,*dv,keyin10,*p65:14,"ok? ",str1
        cmatch   no to str1
        goto     amount if equal
         cmatch  star to str1
         goto    keyin if equal
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
        move      n10 to rental
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
        compare  total to rent               .100 per rental?
        if       equal                        .yes
        move     n10 to rental                .yes
        move     c0 to exchange               .yes
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
        keyin    *p10:14,*el,"(P)erc change, (",*cyan,"A",*white,")mount ",*rv,str1
        cmatch    "P" to str1
        goto      lm1 if equal
        cmatch    star to str1
        goto      keyin if equal                
        cmatch    "A" to str1
        goto      lm if not equal
amount1 keyin    *p10:14,*el,"new amount ",*p21:14,*dv,n10:
                 *p21:14,*rv,n10,*p65:14,"ok? ",str1
        cmatch   no to str1
        goto     amount1 if equal
         cmatch  star to str1
         goto    keyin if equal
        TRAP     FORMAT1 IF FORMAT
        pack     projfld from b1,source,clientid
        rep      zfill in clientid
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
         goto    update
.
lm1      keyin    *p10:14,*el,"perc change ",per,*p65:14,"ok? ",str1
        cmatch    no to str1
        goto      lm if equal
        cmatch    star to str1
        goto      keyin if equal 
        TRAP     FORMAT1 IF FORMAT
        pack     projfld from b1,source,clientid
        rep      zfill in clientid
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
.        if       over
        move      proj98 to n10               
        mult      per by n10
        add       proj98 to n10
        goto      update
.
updbrk  TRAP     FORMAT1 IF FORMAT
        pack     projfld from E,source,clientid
        rep      zfill in client id
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        if       over
        move     c0 to proj99old
        move     c0 to proj99
        move     c0 to proj98
        move     c0 to proj97a
        move     c0 to proj97
        move     c0 to proj96
        move     c0 to proj95a
        move     c0 to proj95
        move     c0 to proj93
        move     c0 to projdol
        move     c0 to proj94
         compare  c2 to mode
         if       equal
         move     exchange to proj2000
         else
.         move     exchange to proj98a
         endif
        unpack   clientid into str2,str4
        pack     mkey from str4,z3
        call     nmlrkey
        move     mcomp to client
        filepi   1;projdolr
        write     projdolr,projfld;projfld,client,projdol,proj93:
                  proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
      else  
         compare  c2 to mode
         if       equal
         move     exchange to proj2000
         else
.         move     exchange to proj98a
         endif
         filepi   1;projdolr
         update   projdolr;projfld,client,projdol,proj93:
                  proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
         endif
        pack     projfld from R,source,clientid
        rep      zfill in client id
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        if       over
        move     c0 to proj99old
        move     c0 to proj99
        move     c0 to proj98
        move     c0 to proj97a
        move     c0 to proj97
        move     c0 to proj96
        move     c0 to proj95
        move     c0 to proj95a
        move     c0 to proj93
        move     c0 to projdol
        move     c0 to proj94
        compare  c2 to mode
        if       equal
        move     rental to proj2000
        else
.        move     rental to proj98a
        endif
        unpack   clientid into str2,str4
        pack     mkey from str4,z3
        call     nmlrkey
        move     mcomp to client
        filepi   1;projdolr
        write     projdolr,projfld;projfld,client,projdol,proj93:
                  proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        else  
        compare  c2 to mode
        if       equal
        move     rental to proj2000
        else
.        move     rental to proj98a
        endif
        filepi   1;projdolr
        update   projdolr;projfld,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        endif
        display  *p1:23,*el,"Total ",total," Rent ",rent," % ",calcper:
                 *p1:24,"client ",client," rental ",rental," exchange ",exchange
        goto     keyin
.
update
        TRAP     FORMAT1 IF FORMAT
        pack     projfld from b1,source,clientid
        rep      zfill in clientid
        filepi   1;projdolr
        read     projdolr,projfld;str8,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        if       over
        move     c0 to proj99
        move     c0 to proj99old
        move     c0 to proj98
        move     c0 to proj97a
        move     c0 to proj97
        move     c0 to proj96
        move     c0 to proj95
        move     c0 to proj95a
        move     c0 to proj93
        move     c0 to projdol
        move     c0 to proj94
        compare  c2 to mode
        if       equal
        move     n10 to proj2000
        else
.        move     n10 to proj98a
        endif
.       pack     ndatfld from clientid
.       call     ndatkey
        move     olstname to client
        filepi   1;projdolr
        write    projdolr,projfld;projfld,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        else  
        compare  c2 to mode
        if       equal
        move     n10 to proj2000
        else
.        move     n10 to proj98a
        endif
        move     olstname to client
        filepi   1;projdolr
        update   projdolr;projfld,client,projdol,proj93:
                 proj94,proj95,proj95a,proj96,proj97,proj97a,proj98,proj98a,proj99,proj99old,proj2000
        endif
        goto     keyin
EOJ     stop
oops    trapclr  f5
        noreturn
        trap     oops if f5
        goto     keyin
.        
FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
;Patch1.4
			include	compio.inc
			include	cntio.inc
.         include  nmlrio.inc
;Patch1.4
         include  mlrhelp.inc
         include  ndatio.inc
           INCLUDE  COMLOGIC.INC

