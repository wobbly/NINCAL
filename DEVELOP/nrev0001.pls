.*nrev0001.DBS converts INCOME7 format TO REVENUE
.* and adds income7 records to revenue file.
.
.....................................
pc       equ      0
         include  common.inc
         include  cons.inc
Release   Init      "6.0"     DLH add AR & AP  use new company number for mailers
Reldate   Init      "28 September 2010"
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
.release  init      "4.0"          DLH 05sep95 include REVunbld.
.RELEASE  INIT     "3.4"       dlh 10/19/94  prepped for  1995. RECS
.RELEASE  INIT     "3.3"       dlh 10/19/93  prepped for  1994. RECS
.RELEASE  INIT     "3.2"       JD 1/26/93  ADDED 1993. RECS
.RELEASE  INIT     "3.1"       DLH 1/27/92  ADDED 1992. RECS
..RELEASE  INIT     "3.0"       DLH 12/18/91  ADDED 1989. RECS
.
.begin patch 5.2
.INPUT    FILE     VAR=163
INPUT    FILE     VAR=178
.end patch 5.2
.
.TYPE     DIM      1          1-1
SOURCE  DIM     1            2-2
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
.OUTPUT         IFILE     KEYLEN=6,FIX=970        for 95 with REVunbld
.OUTPUT         IFILE     KEYLEN=6,FIX=1102        96'
.OUTPUT         IFILE     KEYLEN=6,FIX=1235        97'
.OUTPUT  IFILE     KEYLEN=8,FIX=1369               98
.begin patch 5.2
.OUTPUT         IFILE     keylen=8,FIX=1520                ..99
.begin patch 6.0
          Include   \\nts1\e\library\develop\Nrevdd.inc
.OUTPUT  IFILE     keylen=12,FIX=380
..end patch 5.2
.
..begin patch 5.2
..TYPE     DIM      1   1-1   ---\
..SOURCE DIM     1      2-2------->  KEY=nrevfld
.CID DIM    6      3-8------/
.............................
.revvars  list
.nrevfld DIM     12       1-12       type,source,CID,ccyy
.CLIENT  DIM     45      13-57
..
.JanAR   FORM    10.2    58-70     HOLDS ADJUSTED LR INC FOR MONTH
.FebAR   FORM    10.2    71-83
.MarAR   FORM    10.2    84-96
.AprAR   FORM    10.2    97-109
.MayAR   FORM    10.2   110-122
.JunAR   FORM    10.2   123-135
.JulAR   FORM    10.2   136-148
.AugAR   FORM    10.2   149-161
.SepAR   FORM    10.2   162-174
.OctAR   FORM    10.2   175-187
.NOVAR   FORM    10.2   188-200
.DecAR   FORM    10.2   201-213
..       
.JanAP     FORM    10.2   214-226      HOLDS ADJUSTED NIN INcome
.FebAP     FORM    10.2   227-239
.MarAP     FORM    10.2   240-252
.AprAP     FORM    10.2   253-265
.MayAP     FORM    10.2   266-278
.JunAP     FORM    10.2   279-291
.JulAP     FORM    10.2   292-304
.AugAP     FORM    10.2   305-317
.SepAp     FORM    10.2   318-330
.OctAP     FORM    10.2   331-343
.NovAP     FORM    10.2   344-356
.DecAP     FORM    10.2   357-369
.REVunbld   form   8.2     370-380
.        listend
.end patch 6.0


UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    10.2
MO       FORM    2
.
.
        MOVE     "REVENUE" TO PROGRAM
         MOVE    "UPDATE REVENUE FILE" TO STITLE
         MOVE    "Names In The News" TO COMPNME
         CALL     PAINT
        display   *p1:24,*el,"opening income7";
.START PATCH 5.22 REPLACED LOGIC
.        OPEN    INPUT,"g:\data\INCOME7.DAT",READ
        PACK    STR35,NTWKPATH1,"INCOME7.DAT"
        OPEN    INPUT,STR35,READ
.END PATCH 5.22 REPLACED LOGIC
        display   *p1:24,*el,"income7 opened",*w2;
        display   *p1:24,*el,"opening Revenue";
.        OPEN    OUTPUT,"\\nts1\e\data\dbase\REVENUE"
.START PATCH 5.30
.begin patch 6.0
.        OPEN    OUTPUT,"REVENUE",exclusive
.end patch 6.0
.End PATCH 5.30
        display   *p1:24,*el,"Revenue Open",*w2;
.
INPUT   TRAP     FORMAT1 IF FORMAT
.begin patch 6.0
.        READ     INPUT,SEQ;TYPE,SOURCE,CID,CC,year,MONth,FILL1,CLIENT:
        READ     INPUT,SEQ;TYPE,SRC,CID,YR0,MONth,FILL1,CLIENT:
                 owner,ar,AP,LR,nintot,QTY,ADJAR,ADJAP,ADJLR,adjnintot,unbilinc
.end patch 6.0
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
.begin patch 6.0
.        CMATCH   "M" TO SOURCE
        CMATCH   "M" TO SrC
.end patch 6.0
        IF        EQUAL
        MOVE      " " TO TYPE
        ENDIF
.
        add     month to mohold
.begin patch 6.0
.        PACK    nrevfld FROM TYPE,SOURCE,CID,cc,year
        PACK    nrevfld FROM TYPE,SRC,CID,YR0
.end patch 6.0
        TRAP    FORMAT2 IF FORMAT
.        display *p1:24,*el,"key = ",nrevfld,b1,lr
.begin patch 6.0
          call      Nrevtst         
.        READ    OUTPUT,nrevfld;;
        GOTO    WRITE IF OVER
.
          call      Nrevkey
.        READ    OUTPUT,nrevfld;revvars
.end patch 6.0
        TRAPCLR FORMAT
********************************************************************
        LOAD    CALC FROM MOhold OF JanAR:
                FebAR,MarAR,AprAR,MayAR,JunAR,JulAR,AugAR,SepAR,OctAR:
                NOVAR,DecAR
.               

.begin patch 6.0
.        ADD     LR TO CALC
          Add       AR,Calc
          Add       AdjAR,calc
.end patch 6.0
.start PATCH 5.30
        move    c0 to REVunbld
.end PATCH 5.30
        add     unbilinc to REVunbld
.
        STORE   CALC INTO mohold OF JanAR:
                FebAR,MarAR,AprAR,MayAR,JunAR,JulAR,AugAR,SepAR,OctAR:
                NOVAR,DecAR

        LOAD    CALC FROM MOhold OF JanAP:
                FebAP,MarAP,AprAP,MayAP,JunAP,JulAP,AugAP,SepAp,OctAP:
                NovAP,DecAP

.begin patch 6.0
.        add   nintot to calc
          Add       AP,Calc
          Add       AdjAP,calc
.end patch 6.0

        Store   CALC into MOhold OF JanAP:
                FebAP,MarAP,AprAP,MayAP,JunAP,JulAP,AugAP,SepAp,OctAP:
                NovAP,DecAP
.begin patch 6.0
          add       AR,RevAR
          add       AP,RevAP
          add       AdjAR,RevAR
          add       AdjAP,REvAP
          call      Nrevupd
.         UPDATE  OUTPUT;revvars
.end patch 6.0

         ADD     C1 TO  UPDCOUNT
         DISPLAY   *P10:12,"RECORDS UPDATED : ",UPDCOUNT
         move    c0 to mohold
         GOTO    INPUT
.
WRITE   PACK    nrevfld FROM TYPE,SRC,CID,YR0

        MOVE    C0 TO JanAR
        MOVE    C0 TO FebAR
        MOVE    C0 TO MarAR
        MOVE    C0 TO AprAR
        MOVE    C0 TO MayAR
        MOVE    C0 TO JunAR
        MOVE    C0 TO JulAR
        MOVE    C0 TO AugAR
        MOVE    C0 TO SepAR
        MOVE    C0 TO OctAR
        MOVE    C0 TO NOVAR
        MOVE    C0 TO DecAR

        MOVE    C0 TO JanAP
        MOVE    C0 TO FebAP
        MOVE    C0 TO MarAP
        MOVE    C0 TO AprAP
        MOVE    C0 TO MayAP
        MOVE    C0 TO JunAP
        MOVE    C0 TO JulAP
        MOVE    C0 TO AugAP
        MOVE    C0 TO SepAp
        MOVE    C0 TO OctAP
        MOVE    C0 TO NovAP
        MOVE    C0 TO DecAP

        move    c0 to REVunbld
.
        move    unbilinc to REVunbld
.        MOVE    C0 TO PERCENT       *NEW  NO PROJECTED INCOME.
        MOVE    C0 TO MOHOLD
        MOVE    MONTH TO MOHOLD
.***************************************************************************
.
.begin patch 6.0
          Move      C0,Calc
          Add       AR,Calc
          Add       AdjAR,calc
.        STORE   LR INTO mohold OF JanAR:

        STORE   Calc INTO mohold OF JanAR:
                FebAR,MarAR,AprAR,MayAR,JunAR,JulAR,AugAR,SepAR,OctAR:
                NOVAR,DecAR

          Move      C0,Calc
          Add       AP,Calc
          Add       AdjAP,calc

.          STORE   NINTOT INTO mohold OF JanAP:
        STORE   Calc INTO mohold OF JanAP:
.end patch 6.0
                FebAP,MarAP,AprAP,MayAP,JunAP,JulAP,AugAP,SepAp,OctAP:
                NovAP,DecAP
.begin patch 6.0
          move      C0,REvAR
          move      C0,REvAP
          Move      AR,RevAR
          Move      AP,RevAP
          add       AdjAR,RevAR
          add       AdjAP,REvAP

          call      Nrevwrt
.        WRITE    OUTPUT,nrevfld;revvars
.end patch 6.0
.
         ADD     C1 TO  WRTCOUNT
         DISPLAY   *P10:14,"RECORDS WRITTEN : ",WRTCOUNT
         MOVE    C0 TO MOHOLD

        GOTO    INPUT
EOJ     CLOSE   INPUT
.begin patch 6.0
.        CLOSE   OUTPUT
.end patch 6.0
        STOP

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SRC,TYPE,CLIENT:
                *b,*w10;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
.begin patch 6.0
          Include   Nrevio.inc
.end patch 6.0
         INCLUDE  COMLOGIC.INC

