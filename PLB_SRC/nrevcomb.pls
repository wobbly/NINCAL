.
PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         include   nrevdd.inc
.............................................................................
................................................

release  init      "1.0"       03October2000 DLH first implementation.
input    file
output2  file
output   ifile     keylen=6,NODUPLICATES
input2   file
in       form      4
key15    dim       15   *FILE KEY.
key14    dim       14
key6     dim       6
ANS      DIM       1
TIPE     DIM       1
CHECK    FORM      1
SOURCE   DIM       1
cid2     dim       6
M        INIT      "M"
B        INIT      "B"
R        INIT      "R"
owner    dim       4
INCNAME  DIM       45        11-55
ARTOT    FORM      10.2        56-68
APTOT    FORM      10.2        69-81
LRTOT    FORM      10.2        82-94
NINTOT    FORM      10.2       95-107
QTYTOT   FORM      8          108-115
ADJARTOT FORM      10.2       116-128
ADJAPTOT FORM      10.2       129-141
ADJLRTOT FORM      10.2       142-154
ADJNINTOT FORM      10.2      155-167
unbill   form      8.2        168-190
unbilinc form      8.2       110-120
.
LRcalc   FORM      10.2
NINcalc  FORM      10.2
.output record
.byte 1 nothing
.source   dim      1                2-2
.ID       dim      6                3-8
.name     dim      45               9-53
.owner    dim      4               54-57
jan1     FORM      10.2            58-70
feb1     FORM      10.2
mar1     FORM      10.2
apr1     FORM      10.2
may1     FORM      10.2
jun1     FORM      10.2
jul1     FORM      10.2
aug1     FORM      10.2
sep1     FORM      10.2
oct1     FORM      10.2
nov1     FORM      10.2
dec1     FORM      10.2
janNIN1  FORM      10.2
febNIN1  FORM      10.2
marNIN1  FORM      10.2
aprNIN1  FORM      10.2
mayNIN1  FORM      10.2
junNIN1  FORM      10.2
julNIN1  FORM      10.2
augNIN1  FORM      10.2
sepNIN1  FORM      10.2
octNIN1  FORM      10.2
novNIN1  FORM      10.2
decNIN1  FORM      10.2
janx1    FORM      10.2           308-
febx1    FORM      10.2
marx1    FORM      10.2
aprx1    FORM      10.2
mayx1    FORM      10.2
junx1    FORM      10.2
julx1    FORM      10.2
augx1    FORM      10.2
sepx1    FORM      10.2
octx1    FORM      10.2
novx1    FORM      10.2
decx1    FORM      10.2
janNINx1 FORM      10.2
febNINx1 FORM      10.2
marNINx1 FORM      10.2
aprNINx1 FORM      10.2
mayNINx1 FORM      10.2
junNINx1 FORM      10.2
julNINx1 FORM      10.2
augNINx1 FORM      10.2
sepNINx1 FORM      10.2
octNINx1 FORM      10.2
novNINx1 FORM      10.2
decNINx1 FORM      10.2               681
.unbilled form      8.2            682-692

Total    form      10.2
RTotal   form      10.2
ETotal   form      10.2
NINTotal    form      10.2
RNINTotal   form      10.2
ENINTotal   form      10.2
Mpercent form      10.4
rpercent form      10.4
Epercent form      10.4
NINperc  form      10.4
percent  dim       16
...................................................................
projdolr   iFILE     keylen=14
.
.projvars  list
projfld   dim      8
.TYPE     DIM      1       1-1   ---\
.SOURCE   DIM       1      2-2------->  KEY=nrevfld
.CLIENTID DIM    6          3-8------/
.CLIENT   DIM       45     9-53
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
proj2002 dim    11
nin2002  dim    11
.         listend
.
.hold vars
proj2000X  form      11          hold exchange portion  LR
NIN2000x   form      11          hold exchange portion NIN
proj2001X  form      11
nin2001x   form      11
client2    dim       45
hold2002   form      10
holdn202   form      10
hold2004   form      10
holdn204   form     10
hold2003   form      10
holdn203   form      10
hold2001   form     10
holdn201   form      10
h2002      dim      10
hn202      dim      10
h2001      dim      10
hn201      dim      10
projfld2   dim       8
.......................................................................
.SORTVAR	 INIT	   "c:\data\average7.dat,c:\data\average7.srt;2,9-53"
SORTVAR	 INIT	   "c:\work\average7.dat,c:\work\average7.srt;2,9-53"
countout form      5
sheetno    form      2
Hmany1   form      9
Hmany2   form      9
Hmany3   form      9
Hmany4   form      9
Hmany5   form      9
Hmany6   form      9
Hmany7   form      9
Hmany8   form      9
NumberofSheets Integer	4,"0x00000000"     
endrow   form      9
.............................................................................................
.some excel goodies
         open      input,"c:\work\revenue.0201"
.output the above info will be compiled in the form I want it
         open      output,"c:\work\revnew"
			open      output2,"c:\work\revnew2"
input
         READ      INPUT,SEQ;cid,client,h2002,hn202,h2001,hn201
         goto      eoj if over
           ADD         C1 TO IN
           DISPLAY   *P10:10,"RECORDS READ : ",IN,cid
.         packkey   key14 from b1,"M",key6,"200201"
         pack      key6 from Cid

.         clear     projfld
.         move      key8 to projfld2
.         move      key8 to projfld
         read      output,key6;cid2,client2,hold2004,holdn204,hold2003,holdn203,hold2002,holdn202,hold2001,holdn201
         goto      write1 if over

.         add       unbill to unbilled
.         clear     projfld
.         move      key8 to projfld
         move      h2001 to hold2001
         move      hn201 to holdn201
         move      h2002 to hold2002
         move      hn202 to holdn202
.         update    output;key6,cid,client,hold2004,holdn204,hold2003,holdn203,hold2002,holdn202,hold2001,holdn201
         write    output2,seq;cid,client,hold2004,holdn204,hold2003,holdn203,hold2002,holdn202,hold2001,holdn201
write
         goto      input
write1
         move      client2 to client
         move      c0 to hold2004
         move      c0 to holdn204
         move      c0 to hold2003
         move      c0 to holdn203
         move      h2001 to hold2001
         move      hn201 to holdn201
         move      h2002 to hold2002
         move      hn202 to holdn202
.         move      b1 to projtype
.         move      "M" to projsrc
.         move      key6 to projclient
.         move      "2002" to projyr
.         move      "01" to projkey
.         move      hold2002 to projlr
.         move      holdn202 to projnin
.        write      output,key6;cid,client,hold2004,holdn204,hold2003,holdn203,hold2002,holdn202,hold2001,holdn201
        write      output2,seq;cid,client,hold2004,holdn204,hold2003,holdn203,hold2002,holdn202,hold2001,holdn201
.         write      outuput;projvars
         goto      input
eoj
        shutdown
        stop
.debug   return
        include     nrevio.inc
        include     comlogic.inc
