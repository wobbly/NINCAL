PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
.patch2.72
                              include   compdd.inc
                              include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.patch2.72
         INCLUDE   NORDDD.inc
         INCLUDE   NDATDD.INC
.
.OUTPUT   FILE      FIX=344,STATIC=6
.Start Patch #2.6 - increased file size
.OUTPUT   FILE      FIX=384,STATIC=6
.OUTPUT   FILE      FIX=498,STATIC=6
OUTPUT   FILE      FIX=500,STATIC=6
.End Patch #2.6 - increased file size
COUNTIN  FORM      6
FOUND    FORM      6
ORDCNAME dim       45
ordmname dim       45
.oRDCNAME dim       25
.ordmname dim       25
KEY      DIM       6      
release  init      "2.76"        DLH use data manager
Reldate   Init      "03 February 2011"
.release  init      "2.75"        JD        25Sep2007        Updated starting LR.
.release  init      "2.74"        JD       03Feb2006        Updated starting LR.
.release  init      "2.73"        JD       14Jan2005        Updated starting LR.
.release  init      "2.72"        DMB   26MAY2004 Mailer Conversion
.release  init      "2.71"     04SEP2001 JD updated starting lr #
.release  init      "2.7"     02OCT2000 ASH NEW SERVER ADDED
.release  init      "2.6"     05JAN99 ASH NINORD Y2K, File expansion
.Release  init      "2.51"     12jan99 jd changed starting lr # for search
.Release  init      "2.5"     28Sep98 DLH added code to handle pending orders
.                            See norddd.inc patch 5
.Release  init      "2.4"     17Aug98 ASH Increased OUTPUT FILE size as well as ordmname to reflect change to NORD2DD.INC
.Release  init      "2.3"     31Mar98 DLH change to no locking on order,Mailer, Datacard files
.release  init      "2.2"     11jan95 DLH change record size to match master
.RELEASE INIT       "2.1"     12jul94    JD change order stat comp logic.
.RELEASE INIT       "2.0"    06MAY94    DLH  ADD LIST ORDERNAME LOOK UP.
.                                      CHANGE BRK/MLR ORDER IN OUTPUT FILE
.                                      TO MATCH NORD2DD.INC
.RELEASE         INIT      "1.0"       FEB92.
.RELEASE  INIT      "PRE"
holdmlr   dim      7
holdlist  dim      6
         CLOCK     DATE TO TODAY
         MOVE      "NORD013A" TO PROGRAM
         MOVE      "Names In The News" TO COMPNME
         MOVE      "FIND UNBILLED ORDERS" TO STITLE
         MOVE      "EXIT" TO PF5
         TRAP      EOJ IF F5
         MOVE      C1 TO NDATPATH
         CALL       PAINT
         CALL       FUNCDISP
         move       c3 to nmlrlock
         move       c3 to nordlock
         move       c3 to ndatlock
.DLH 31Mar98 no locks on the above 3 files.         
         trap       oops giving error if io
.   
         GOTO       PREP
OOPS     MOVE       C0 TO NORDFLAG    
         display     *p1:24,*el,"io error ",error,*b,*b,*w4
         noreturn
         trapclr    io
         trap       oops giving error if io
         GOTO       PREP
PREP     
.START PATCH 2.7 REPLACED LOGIC
.         PREPARE    OUTPUT,"g:\DATA\UNBILLED.DAT"
         PACK      STR35,NTWKPATH1,"UNBILLED.DAT"
         Pack       Str55,"e:\data\unbilled.dat|NINS1:502"
.         PREPARE    OUTPUT,STR35
         PREPARE    OUTPUT,STR55
.END PATCH 2.7 REPLACED LOGIC
.patch2.74
.         MOVE      "550000" TO NORDFLD
.patch2.74
.patch2.75
         MOVE      "600000" TO NORDFLD
.patch2.75
.patch2.73
.         MOVE      "450000" TO NORDFLD
.patch2.73
.         MOVE      "375000" TO NORDFLD
.         MOVE      "324000" TO NORDFLD
         DISPLAY   *P12:10,"ORDERS READ : ":
                   *P12:12,"UNBILLED    : ":
                   *p12:14,"LR Number   : ",nordfld

         MOVE      C1 TO NORDPATH
         CALL      NORDTST
         stop      if over
read     DISPLAY   *P12:10,"ORDERS READ : ":
                   *P12:12,"UNBILLED    : ":
                   *p12:14,"LR Number   : ",olrn
getrec
         call      rotdial
         CALL      NORDks
         GOTO      EOJ IF OVER
         ADD       C1 TO COUNTIN
         DISPLAY   *P26:10,COUNTIN
.
         CMATCH    "B" TO OSTAT         not BILLED ?
         GOTO      READ IF EQUAL     YES.
.begin patch 2.5
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      READ IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       LCR order ?
         GOTO      READ IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       cancelled LCR order ?
         GOTO      GETREC IF EQUAL     YES, skip.
.note cancodes also updated to skip cancelled pending orders.
.end patch 2.5
.     
         RESET     CANCODES
         SCAN      OSTAT IN CANCODES
         GOTO      READ IF EQUAL
.
         PACK      MKEY FROM OMLRNUM,Z3
         MATCH     MKEY TO HOLDMLR
         IF        NOT EQUAL
         move      mkey to holdmlr
         CALL      NMLRKEY
         MOVE      MCOMP TO ORDCNAME
         ENDIF
         MATCH     OLNUM TO HOLDLIST
         IF        NOT EQUAL
         move      olnum to holdlist
         MOVE      OLNUM TO NDATFLD
         CALL      NDATKEY
         MOVE      olstname TO O1DES          .in case name has changed.
         ENDIF
.
WRITE    WRITE     OUTPUT,SEQ;ORDVARS:
                                ORDMNAME:   
                                ORDCNAME:
                                compbillcde
.
         ADD       C1 TO FOUND
         DISPLAY   *P26:12,FOUND
         GOTO      READ
EOJ      WEOF      OUTPUT,SEQ
         CLOSE     OUTPUT
         STOP
.
.patch2.72
                                        include   compio.inc
                                        include   cntio.inc         
.         INCLUDE   NMLRIO.inc
.patch2.72
         INCLUDE   NORDIO.inc
         include   ndatio.inc
         INCLUDE   COMLOGIC.inc

