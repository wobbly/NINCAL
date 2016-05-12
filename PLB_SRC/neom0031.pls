.
. PROGRAM    : NEOM0031
. DATE       : 03/27/00
. AUTHOR     : D.L. Herrick
. DESCRIPTION: PRODUCES Flat file for Vantage Group Discount reconciliation
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   CONSacct.inc
         include   norddd.inc
;begin patch 1.3
               include        ninvdd.inc
               include        NInvAcddd.inc
;         include   ninvdd.inc
;end patch 1.3
         include   nshpdd.inc
         include   nmrgdd.inc
         include   nowndd.inc
         include   ndatdd.inc
         include   ndat3dd.inc
         include   nacddd.inc
         include   nadjdd.inc
.patch1.2
				include	compdd.inc
				include	cntdd.inc
.         INC       NMLRDD.INC
.patch1.2
release        init           "1.3"         DLH	08March2005	Invoice Conversion
;release  init      "1.2"         JD	26MAY2004	Mailer Conversion
;Release  Init      "1.1"        ASH 02OCT2000 NEW SERVER ADDED
.Release  Init      "pre"
output    file
str10a   dim        10
str10I   dim        10
str10M   dim        10
str10R   dim        10
str10C   dim        10
rent     form       10.2
shipsw   dim         1
mrgsw    dim         1
formula1 dim         250
Rqty     form         10
Eqty     form         10
n10a     form         10
.output file csv
.
.quarter.      dim    10         filled in later                            A
.po                                                                         B 
.key                                                                        C 
.mailer name   dim    35                                                    D
.mlr number    dim     4                                                    E
.lr number     dim     6                                                    F
.order date    dim     10                                                   G
.rtn date      dim     10                                                   H
.mail date     dim     10                                                   I
.invoice date  dim     10                                                   J
.chk recd date dim     10                                                   K
.rent /exch    dim      4     value of rent or exch                         L
.rent qty      dim     10     value of oqty-oexqty                          M
.excg qty      dim     10     value of either oqty or oexqty                N
outputqty      dim     10                                                   O
.cumm qty      dim      ?     =O(current row)+P(previous row)               P

.
excelrow dim    5      
excelrowb dim    5      
count   form       9
.START PATCH 1.1 REPLACED LOGIC
.        prepare    output,"g:\data\Vantage.csv"
        PACK      STR35,NTWKPATH1,"Vantage.csv"
        prepare    output,STR35
.END PATCH 1.1 REPLACED LOGIC
        move       c3 to nordlock
        move       c3 to nshplock
        move       c3 to nmlrlock
        move       c1 to nordpath
        move       c1 to ndatpath
        move       c1 to ninvpath
        move       c4 to excelrow
        rep        zfill in excelrow
        move       c3 to excelrowb
        rep        zfill in excelrowb

         write     output,seq;*cdfon,"       ","Mailer ","Mailer ","Mailer ","         ","Order","Order","Return","Mail":
                                     "Invoice","Payment   ","Rent ","Net","Rent","Exch":
                                     "Shipped","Cumm. ","Rental","Payment","Estimated","Quantity ":
                                     "$5.00 Per/M"
         write     output,seq;*cdfon,"Quarter","P.O.   ","Key    ","Name   ","Mailer ##","Number","Date ","Date   ","Date":
                                     "Date   ","Rec'd Date","Exch ","Percent","Qty ","Qty ":
                                     "Qty    ","Volume","Volume","Timely?","Refund","refundable":
                                     "Per/M"
        write     output,seq;*cdfon,b12
.  305233

        move      "361990" to nordfld               ..1st Vantage lr of 2000
        call       nordtst
        trap       exit if f5
looper  call       nordks
        goto       exit if over
         move      c6 to mm          .oct 25
         move      "30" to dd         . last day
         move      "00" to yy         . be
         call      cvtjul                .fore 
         move      juldays to n6         .new obildrct
         move      oodtem to mm           .code
         move      oodted to dd           .check
         move      oodtey to yy           .it 
         call      cvtjul                .out
         sub       juldays from n6       .what is diff?
         compare   c0 to n6
         goto      looper if not less
         CMATCH    "p" TO OSTAT       Pending order ?
         GOTO      looper IF EQUAL     YES, skip.
         CMATCH    "x" TO OSTAT       Cancelled Pending order ?
         GOTO      looper IF EQUAL     YES, skip.
         CMATCH    "l" TO OSTAT       lcr order ?
         GOTO      looper IF EQUAL     YES, skip.
         CMATCH    "z" TO OSTAT       lcr order ?
         GOTO      looper IF EQUAL     YES, skip.

        call       rotdial
        match      "0586" to obrknum             .Vantage
        goto       looper if not equal
        match      "7667" to omlrnum             .Vantage
        goto       looper if equal
        add        c1 to count
        display    *p10:10,"vantage records ",count
        pack       mkey from omlrnum,b3
        rep        zfill in mkey
        call       nmlrkey
        move       olrn to ninvfld
        rep        zfill in ninvfld
        call       ninvkey
.        goto       looper if over
         call      wipecvars
         move      c1 to ndatpath
         move      olnum to ndatfld
         call      ndatkey
         move      olon to nownfld
         call      nownkey
         MOVE      NO TO SHIPSW
         MOVE      NO TO MRGSW
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         MOVE      olrn TO NMRGFLD
         REP       ZFILL IN NMRGFLD
         CALL      NMRGKEY
         if        not over
         move      yes to mrgsw
         endif
         move      c0 to squant
         MOVE      olrn TO NSHPFLD
         CALL      NSHPKEY
         if        not over
         move      yes to shipsw
         endif
         GOTO      process
process
         call      compute
         clear     str10
         pack      str10 from OODTEm,slash,OODTEd,slash,OODTEc,OODTEy
         rep       zfill in str10
         clear     str10I
         pack      str10I from invdtem,slash,invdted,slash,invdtec,invdtey
         rep       zfill in str10I
         sub       cc from cc
         clear     mm
         clear     dd
         clear     yy
         clear     str10m
         pack      str10M from OMDTEm,slash,OMDTED,slash,OMDTEc,OMDTEy                .mail date
         rep       zfill in str10m
         clear     mm
         clear     dd
         clear     yy
         clear     str10c
         unpack    MLRPAYD into cc,yy,mm,dd
         type      mm
         if        equal
         clear     str10c
         pack      str10c from mm,slash,dd,slash,cc,yy                .payment rec'd date
         rep       zfill in str10c
         endif
         clear     str10R
         pack      str10R from ORTNDTEm,slash,ORTNDTED,slash,ORTNDTEc,ORTNDTEy      .return date
         rep       zfill in str10R
.check for empty dates
         scan      "00/" in str10R
         if         equal
         reset      str10R
         clear      str10R
         endif
         reset      str10R
         scan      "//" in str10R
         if         equal
         reset      str10R
         clear      str10R
         endif
         reset      str10R
.
         scan      "00/" in str10C
         if         equal
         reset      str10C
         clear      str10C
         endif
         reset      str10C
         scan      "//" in str10C
         if         equal
         reset      str10C
         clear      str10C
         endif
         reset      str10C
.
         scan      "00/" in str10I
         if         equal
         reset      str10I
         clear      str10I
         endif
         reset      str10I
         scan      "//" in str10I
         if         equal
         reset      str10I
         clear      str10I
         endif
         reset      str10I
.
         scan      "00/" in str10M
         if         equal
         reset      str10M
         clear      str10M
         endif
         reset      str10M
         scan      "//" in str10M
         if         equal
         reset      str10M
         clear      str10M
         endif
         reset      str10M
.
         move     c0 to rent
         move     gross to rent
         sub      lrinc from rent
         clear    formula1
         append   "=O",formula1
         append   excelrow,formula1
         append   "+p",formula1
         append   excelrowb,formula1
         reset    formula1

.need rent/exchange logic
         clear    str4
         RESET     EXCODES
         SCAN      OELCODE IN EXCODES
.         GOTO      RENT IF NOT EQUAL
         GOTO      EXCHANGE IF EQUAL
         GOTO      RENT
EXCHANGE MOVE      "Exch" TO str4
         move       oqty to Eqty
         goto      process1
rent     MOVE      "Rent" TO str4
         move       oqty to n10
         move       oexqty to N10a
         move       oexqty to eqty
         sub        N10a from n10
         move       n10 to Rqty
         goto      process1
.        if rent move rent to str4 if exch ....
process1 reset     cancodes
         scan      ostat in cancodes
         goto      process2 if not equal
         move      "CANC" to str4
         clear     str10c
process2
         move       c0 to n9
         move       qtybild to outputqty

         move      c0 to n2
         move      irnetper to n2
         compare   c0 to n2                    .net name order?
         if        not equal                   .yes
         goto      write
         ENDIF
         if        ((c2 = netflag or c3 = netflag) & lstmsw = "N" & paycode <> "4")         brokerage ,net order 
        CMATCH     YES TO MRGSW
        IF         EQUAL
        MOVE       NMRGRQTY TO outputqty    
        ELSE
        CMATCH     YES TO SHIPSW
        IF         EQUAL
        MOVE       SQUANT TO outputqty    
        ELSE
        MOVE       OQTY TO outputqty    
        ENDIF
        ENDIF
        endif

write
         MOVE      oLRn TO NADJFLD
. 
         CALL      NADJKEY
         GOTO      writeit IF OVER
.
          add       asrecadj to ar
writeit  write     output,seq;*cdfon,b10:              quarter  user supplied  col A
                                     OMLRPON:          Mailer po               col b
                                     OMLRKY:           Mailer Key              col c
                                     mcomp:            Mailer name             col d
                                     omlrnum:          Mailer ##               col e
                                     olrn:             Order number            col f
                                     str10:            Order Date              col g
                                     str10R:           return Date                 h
                                     str10M:           Mail Date                   i
                                     str10I:           invoice date                j
                                     str10c:           chk recd date               k
                                     str4:             rent/exch                   l
                                     irnetper:         net name percentage
                                     Rqty:                                         M
                                     eqty:                                         N
                                     outputqty:                                      O
                                     formula1:
                                     ar                                      P

         sub        rqty from rqty
         sub        eqty from eqty
         clear      irnetper
         move      excelrow to excelrowb
         move      excelrow to n5      .increment row counter
         add       c1 to n5
         move      n5 to excelrow
         rep       zfill in excelrow
         rep       zfill in excelrowb
         goto      looper

exit      weof     output,seq
         close     output
         stop
         include   nordio.inc
;begin patch 1.3
;         include   ninvio.inc
               include        ninvio.inc
               include        NInvAcdio.inc
;         include   compute.inc
               include        compute.inc
;end patch 1.3
         include   nshpio.inc
         include   nmrgio.inc
         include   nownio.inc
         include   ndatio.inc
         include   comlogic.inc
         include   nacdio.inc
         include   ndat3io.inc
.patch1.2
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.INC
.patch1.2
         include   nadjio.inc


