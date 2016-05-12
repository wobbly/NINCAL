.....................................
pc       equ      0
           include  common.inc
           include  cons.inc
RELEASE  INIT     "3.6"       dlh 11/04/96  prepped for  1997. RECS
.VARIABLES ADDED FILE DESC UPDATED ONLY STILL CALCING 96
.RELEASE  INIT     "3.5"       dlh 11/13/95  prepped for  1996. RECS
.RELEASE  INIT     "3.4"       dlh 10/17/94  prepped for  1995. RECS
.RELEASE  INIT     "3.3"       dlh 10/19/93  prepped for  1994. RECS
.RELEASE  INIT     "3.2"       JD 1/26/93  ADDED 1993. RECS
.RELEASE  INIT     "3.1"       DLH 1/27/92  ADDED 1992. RECS
..RELEASE  INIT     "3.0"       DLH 12/18/91  ADDED 1989. RECS
.
INPUT    FILE     VAR=110
.
FILL1   DIM     1
TYPE     DIM      1
SOURCE    DIM       1
.CLIENT   DIM       25
MONTH     FORM      2
YEAR      FORM      2
AR        FORM      8.2
AP        FORM      8.2
LR        FORM      8.2
QTY       FORM      8
ADJAR     FORM      8.2
ADJAP     FORM      8.2
ADJLR     FORM      8.2
JUNK      DIM       2
...........................
.OUTPUT   IFILE     KEYLEN=6,FIX=695 increases by 132 ever year
.OUTPUT   IFILE     KEYLEN=6,FIX=969
.OUTPUT   IFILE     KEYLEN=6,FIX=1101
OUTPUT    IFILE     KEYLEN=6,FIX=1233
.
.TYPE     DIM      1   1-1   ---\
.SOURCE   DIM       1      2-2------->  KEY=nrevfld
CLIENTID DIM    4      3-6------/
.CLIENT   DIM       25     7-31
............................
revvars  list
nrevfld   DIM       6      1-6
CLIENT    DIM       25     7-31
PERCENT  FORM       1.2   32-35      PROJECTED CHANGE
unbilled form   8.2
JAN89     FORM      8.2   36-46     HOLDS ADJUSTED LR INC FOR MONTH
FEB89     FORM      8.2   47-57
MAR89     FORM      8.2   58-68
APR89     FORM      8.2   69-79
MAY89     FORM      8.2   80-90
JUN89     FORM      8.2   91-101
JUL89     FORM      8.2  102-112
AUG89     FORM      8.2  113-123
SEP89     FORM      8.2  124-134
OCT89     FORM      8.2  135-145
NOV89     FORM      8.2  146-156
DEC89     FORM      8.2  157-167
.         
JAN       FORM      8.2  168-178      HOLDS ADJUSTED LR INC FOR MONTH
FEB       FORM      8.2  179-189
MAR       FORM      8.2  190-200
APR       FORM      8.2  201-211
MAY       FORM      8.2  212-222
JUN       FORM      8.2  223-233
JUL       FORM      8.2  234-244
AUG       FORM      8.2  245-255
SEP       FORM      8.2  256-266
OCT       FORM      8.2  267-277
NOV       FORM      8.2  278-288 
DEC       FORM      8.2  289-299
.         
JAN1      FORM      8.2  300-310      HOLDS ADJUSTED LR INC FOR MONTH
FEB1      FORM      8.2  311-321
MAR1      FORM      8.2  322-332
APR1      FORM      8.2  333-343
MAY1      FORM      8.2  344-354
JUN1      FORM      8.2  355-365
JUL1      FORM      8.2  366-376
AUG1      FORM      8.2  377-387
SEP1      FORM      8.2  388-398 
OCT1      FORM      8.2  399-409
NOV1      FORM      8.2  410-420
DEC1      FORM      8.2  421-431
.         
JAN92     FORM      8.2  432-442      HOLDS ADJUSTED LR INC FOR MONTH
FEB92     FORM      8.2  443-453
MAR92     FORM      8.2  454-464
APR92     FORM      8.2  465-475
MAY92     FORM      8.2  476-486
JUN92     FORM      8.2  487-497
JUL92     FORM      8.2  498-508
AUG92     FORM      8.2  509-519
SEP92     FORM      8.2  520-530 
OCT92     FORM      8.2  531-541
NOV92     FORM      8.2  542-552
DEC92     FORM      8.2  553-563
.
JAN93     FORM      8.2  564-574      HOLDS ADJUSTED LR INC FOR MONTH
FEB93     FORM      8.2  575-585
MAR93     FORM      8.2  586-596
APR93     FORM      8.2  597-607
MAY93     FORM      8.2  608-618
JUN93     FORM      8.2  619-629
JUL93     FORM      8.2  630-640
AUG93     FORM      8.2  641-651
SEP93     FORM      8.2  652-662 
OCT93     FORM      8.2  663-673
NOV93     FORM      8.2  674-684
DEC93     FORM      8.2  685-695
.
JAN94     FORM      8.2  696-706      HOLDS ADJUSTED LR INC FOR MONTH
FEB94     FORM      8.2  707-717
MAR94     FORM      8.2      728
APR94     FORM      8.2      739
MAY94     FORM      8.2      750
JUN94     FORM      8.2      761
JUL94     FORM      8.2      772
AUG94     FORM      8.2      783
SEP94     FORM      8.2      794
OCT94     FORM      8.2      805
NOV94     FORM      8.2      816
DEC94     FORM      8.2      827
.
JAN95     FORM      8.2  828-838      HOLDS ADJUSTED LR INC FOR MONTH
FEB95     FORM      8.2     -849
MAR95     FORM      8.2      860
APR95     FORM      8.2      872
MAY95     FORM      8.2      883
JUN95     FORM      8.2      894
JUL95     FORM      8.2      905
AUG95     FORM      8.2      916
SEP95     FORM      8.2      927
OCT95     FORM      8.2      938
NOV95     FORM      8.2      949
DEC95     FORM      8.2      960
.         
JAN96     FORM      8.2  961-971      HOLDS ADJUSTED LR INC FOR MONTH
FEB96     FORM      8.2     -982
MAR96     FORM      8.2      993
APR96     FORM      8.2      1004
MAY96     FORM      8.2      1015
JUN96     FORM      8.2      1026
JUL96     FORM      8.2      1037
AUG96     FORM      8.2      1048
SEP96     FORM      8.2      1059
OCT96     FORM      8.2      1070
NOV96     FORM      8.2      1081
DEC96     FORM      8.2      1092
.         
JAN97     FORM      8.2  1093-1112      HOLDS ADJUSTED LR INC FOR MONTH
FEB97     FORM      8.2       1123
MAR97     FORM      8.2       1134
APR97     FORM      8.2       1145
MAY97     FORM      8.2       1156
JUN97     FORM      8.2       1167
JUL97     FORM      8.2       1178
AUG97     FORM      8.2       1189
SEP97     FORM      8.2       1200
OCT97     FORM      8.2       1211
NOV97     FORM      8.2       1222
DEC97     FORM      8.2       1233
.         
         listend
UPDCOUNT FORM     5
INCOUNT  FORM     5
WRTCOUNT FORM     5
CALC     FORM    8.2
MO       FORM    2
.
.
          MOVE       "nprj0001" TO PROGRAM
           MOVE      "UPDATE REVENUE FILE" TO STITLE
           MOVE    "Names In The News" TO COMPNME
           CALL       PAINT
          OPEN      INPUT,"INCOME7",READ
          OPEN    OUTPUT,"REVENUE"
.
INPUT   TRAP     FORMAT1 IF FORMAT
          READ     INPUT,SEQ;TYPE,SOURCE,CLIENTID,MONth,YEAR,FILL1,CLIENT,AR:
                     AP,LR,QTY,ADJAR,ADJAP,ADJLR,JUNK
          GOTO      EOJ IF OVER
          TRAPCLR  FORMAT
          CMATCH   " " TO CLIENT
          GOTO     INPUT IF EOS
          SUB      SEQ FROM INCOUNT
          DISPLAY  *P10:10,"RECORDS IN ",INCOUNT
          ADD      ADJLR TO LR
.
          CMATCH   "M" TO SOURCE
        IF        EQUAL
        MOVE      " " TO TYPE
        ENDIF
.
          PACK    nrevfld FROM TYPE,SOURCE,CLIENTID
          TRAP    FORMAT2 IF FORMAT
          display *p1:24,*el,"key = ",nrevfld
        READ    OUTPUT,nrevfld;;
          GOTO    WRITE IF OVER
.
        READ    OUTPUT,nrevfld;revvars
          TRAPCLR FORMAT
******************************************************************
.       currently set for 1996
          ADD       "60" TO MONTH         .increase by 12 every year
******************************************************************
        LOAD    CALC FROM MONTH OF JAN89:
                    FEB89,MAR89,APR89,MAY89,JUN89,JUL89,AUG89,SEP89,OCT89:
                    NOV89,DEC89,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP:
                    OCT,NOV,DEC,JAN1:
                    FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1:
                JAN92:
                    FEB92,MAR92,APR92,MAY92,JUN92,JUL92,AUG92,SEP92,OCT92,NOV92:
                    DEC92:
                JAN93:
                    FEB93,MAR93,APR93,MAY93,JUN93,JUL93,AUG93,SEP93,OCT93,NOV93:
                    DEC93:
                JAN94:
                    FEB94,MAR94,APR94,MAY94,JUN94,JUL94,AUG94,SEP94,OCT94,NOV94:
                    DEC94:
                JAN95:
                    FEB95,MAR95,APR95,MAY95,JUN95,JUL95,AUG95,SEP95,OCT95,NOV95:
                    DEC95:
                JAN96:
                    FEB96,MAR96,APR96,MAY96,JUN96,JUL96,AUG96,SEP96,OCT96,NOV96:
                    DEC96:
                JAN97:
                    FEB97,MAR97,APR97,MAY97,JUN97,JUL97,AUG97,SEP97,OCT97,NOV97:
                    DEC97

          ADD     LR TO CALC
.
        STORE   CALC INTO MONTH OF JAN89:
                    FEB89,MAR89,APR89,MAY89,JUN89,JUL89,AUG89,SEP89,OCT89:
                    NOV89,DEC89,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP:
                    OCT,NOV,DEC,JAN1:
                    FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1:
                JAN92:
                    FEB92,MAR92,APR92,MAY92,JUN92,JUL92,AUG92,SEP92,OCT92,NOV92:
                    DEC92:
                JAN93:
                    FEB93,MAR93,APR93,MAY93,JUN93,JUL93,AUG93,SEP93,OCT93,NOV93:
                    DEC93:
                JAN94:
                    FEB94,MAR94,APR94,MAY94,JUN94,JUL94,AUG94,SEP94,OCT94,NOV94:
                    DEC94:
                JAN95:
                    FEB95,MAR95,APR95,MAY95,JUN95,JUL95,AUG95,SEP95,OCT95,NOV95:
                    DEC95:
                JAN96:
                    FEB96,MAR96,APR96,MAY96,JUN96,JUL96,AUG96,SEP96,OCT96,NOV96:
                    DEC96:
                JAN97:
                    FEB97,MAR97,APR97,MAY97,JUN97,JUL97,AUG97,SEP97,OCT97,NOV97:
                    DEC97
.
                    UPDATE  OUTPUT;revvars
.
           ADD     C1 TO  UPDCOUNT
           DISPLAY   *P10:12,"RECORDS UPDATED : ",UPDCOUNT
           GOTO    INPUT
.
WRITE     PACK      nrevfld FROM TYPE,SOURCE,CLIENTID
          MOVE    C0 TO JAN89
          MOVE      C0 TO FEB89
          MOVE    C0 TO MAR89
          MOVE      C0 TO APR89
          MOVE    C0 TO MAY89
          MOVE      C0 TO JUN89
          MOVE    C0 TO JUL89
          MOVE      C0 TO AUG89
          MOVE    C0 TO SEP89
          MOVE      C0 TO OCT89
          MOVE    C0 TO NOV89
          MOVE      C0 TO DEC89
          MOVE    C0 TO JAN1
          MOVE      C0 TO FEB1
          MOVE    C0 TO MAR1
          MOVE      C0 TO APR1
          MOVE    C0 TO MAY1
          MOVE      C0 TO JUN1
          MOVE    C0 TO JUL1
          MOVE      C0 TO AUG1
          MOVE    C0 TO SEP1
          MOVE      C0 TO OCT1
          MOVE    C0 TO NOV1
          MOVE      C0 TO DEC1
          MOVE    C0 TO JAN
          MOVE      C0 TO FEB
          MOVE    C0 TO MAR
          MOVE      C0 TO APR
          MOVE    C0 TO MAY
          MOVE      C0 TO JUN
          MOVE    C0 TO JUL
          MOVE      C0 TO AUG
          MOVE    C0 TO SEP
          MOVE      C0 TO OCT
          MOVE    C0 TO NOV
          MOVE      C0 TO DEC
          MOVE    C0 TO JAN92
          MOVE      C0 TO FEB92
          MOVE    C0 TO MAR92
          MOVE      C0 TO APR92
          MOVE    C0 TO MAY92
          MOVE      C0 TO JUN92
          MOVE    C0 TO JUL92
          MOVE      C0 TO AUG92
          MOVE    C0 TO SEP92
          MOVE      C0 TO OCT92
          MOVE    C0 TO NOV92
          MOVE      C0 TO DEC92
          MOVE    C0 TO JAN93
          MOVE      C0 TO FEB93
          MOVE    C0 TO MAR93
          MOVE      C0 TO APR93
          MOVE    C0 TO MAY93
          MOVE      C0 TO JUN93
          MOVE    C0 TO JUL93
          MOVE      C0 TO AUG93
          MOVE    C0 TO SEP93
          MOVE      C0 TO OCT93
          MOVE    C0 TO NOV93
          MOVE      C0 TO DEC93
          MOVE    C0 TO JAN94
          MOVE      C0 TO FEB94
          MOVE    C0 TO MAR94
          MOVE      C0 TO APR94
          MOVE    C0 TO MAY94
          MOVE      C0 TO JUN94
          MOVE    C0 TO JUL94
          MOVE      C0 TO AUG94
          MOVE    C0 TO SEP94
          MOVE      C0 TO OCT94
          MOVE    C0 TO NOV94
          MOVE      C0 TO DEC94
          MOVE    C0 TO JAN95
          MOVE      C0 TO FEB95
          MOVE    C0 TO MAR95
          MOVE      C0 TO APR95
          MOVE    C0 TO MAY95
          MOVE      C0 TO JUN95
          MOVE    C0 TO JUL95
          MOVE      C0 TO AUG95
          MOVE    C0 TO SEP95
          MOVE      C0 TO OCT95
          MOVE    C0 TO NOV95
          MOVE      C0 TO DEC95
.
          MOVE    C0 TO JAN96
          MOVE      C0 TO FEB96
          MOVE    C0 TO MAR96
          MOVE      C0 TO APR96
          MOVE    C0 TO MAY96
          MOVE      C0 TO JUN96
          MOVE    C0 TO JUL96
          MOVE      C0 TO AUG96
          MOVE    C0 TO SEP96
          MOVE      C0 TO OCT96
          MOVE    C0 TO NOV96
          MOVE      C0 TO DEC96
.
.
          MOVE    C0 TO JAN97
          MOVE      C0 TO FEB97
          MOVE    C0 TO MAR97
          MOVE      C0 TO APR97
          MOVE    C0 TO MAY97
          MOVE      C0 TO JUN97
          MOVE    C0 TO JUL97
          MOVE      C0 TO AUG97
          MOVE    C0 TO SEP97
          MOVE      C0 TO OCT97
          MOVE    C0 TO NOV97
          MOVE      C0 TO DEC97
.
          MOVE    C0 TO PERCENT       *NEW  NO PROJECTED INCOME.
          ADD     "60" TO MONTH       .increase by 12 every year
.
        STORE   LR INTO MONTH OF JAN89:
                    FEB89,MAR89,APR89,MAY89,JUN89,JUL89,AUG89,SEP89,OCT89:
                    NOV89,DEC89,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP:
                    OCT,NOV,DEC,JAN1:
                    FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1:
                JAN92:
                    FEB92,MAR92,APR92,MAY92,JUN92,JUL92,AUG92,SEP92,OCT92,NOV92:
                    DEC92:
                JAN93:
                    FEB93,MAR93,APR93,MAY93,JUN93,JUL93,AUG93,SEP93,OCT93,NOV93:
                    DEC93:
                JAN94:
                    FEB94,MAR94,APR94,MAY94,JUN94,JUL94,AUG94,SEP94,OCT94,NOV94:
                    DEC94:
                JAN95:
                    FEB95,MAR95,APR95,MAY95,JUN95,JUL95,AUG95,SEP95,OCT95,NOV95:
                    DEC95:
                JAN96:
                    FEB96,MAR96,APR96,MAY96,JUN96,JUL96,AUG96,SEP96,OCT96,NOV96:
                    DEC96:
                JAN97:
                    FEB97,MAR97,APR97,MAY97,JUN97,JUL97,AUG97,SEP97,OCT97,NOV97:
                    DEC97
                    
.
          WRITE    OUTPUT,nrevfld;revvars
.
           ADD     C1 TO  WRTCOUNT
           DISPLAY   *P10:14,"RECORDS WRITTEN : ",WRTCOUNT

          GOTO      INPUT
EOJ     CLOSE   INPUT
          CLOSE     OUTPUT
          STOP

FORMAT1 DISPLAY *P1:24,*EL,"INPUT FILE FORMAT ERROR ",SOURCE,TYPE,CLIENT;
        STOP
FORMAT2 DISPLAY *P1:24,*EL,"OUTPUT FILE FORMAT ERROR ",nrevfld;
        STOP
           INCLUDE  COMLOGIC.INC

