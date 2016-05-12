PC             EQU       0
               INC       COMMON.INC
               INC       CONS.INC
Release        Init           "NO"
               INC       CONSACCT.inc
.
               INCLUDE   NINVDD.inc
               INCLUDE   NINVACDDD.inc
               Include            Nmrgdd.inc
               include            nShpdd.inc
SHIPSW         DIM       1    "Y" IF SHIPPING INFO
MRGSW          DIM       1    "Y" IF MERGE INFO
count          Form           6
NINVAcdbad     FILE     FIXED=49
NINVAcdDupes     FILE     FIXED=49
writes         form           6
str1a          dim            1
;
           Move               C1 to Ninvpath
           open               NINVAcdbad,"c:\work\nininvacd.dat",Read
           Prepare            NINvAcdFile,"\\nts1\e\data\text\NINInvAcd.dat","\\nts1\e\data\index\NINInvAcd.isi","9","49",exclusive
           Prepare            NinvACDDupes,"c:\work\nininvacd.dupe"
           move               c1 to NInvAcdflag
Looper         Read           Ninvacdbad,seq;INVACDVARS
               goto           eoj if over
breakit        add            c1 to count
               display        *p10:10,"Records read ",count,b1,NInvAcdINV
               rep            zfill in NinvAcdNum
               packkey         NINVACDFLD from ninvacdinv,ninvacdnum
               Call           NInvAcdwrt
               add            c1 to Writes
               display        *p10:12,"Invoices Written ",writes
               Goto           Looper
Eoj
               stop
           Include   Comlogic.inc
           Include            Nmrgio.inc
           include            nShpio.inc
           INCLUDE   NINVio.inc
           INCLUDE   NINVACDio.inc

