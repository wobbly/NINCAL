.patch         10.11         29 March 2011           DLH code117  Credit Card fee
.patch         10.1          04 Januar 2010          DLH code162 25% of our commission kick back SC
.patch         10.01          05 March 2009          DLH code160 & 161 2% rebate
.patch         10.005          05Oct 2007          JD     Lstmgmt switch PL
.patch         10.004          01Oct 2007          DLH    Codes 159
.patch         10.003          30August 2007          DLH    Codes 115, 157, 158
.patch         10.002          29August 2006          DLH    No longer discounted clients
.;patch         10.001          30June 2005          DLH    acd code 156
.;Patch         10.0            30July 2004          DLH    proposed slice and dice - two modes already billed (default)  & new bill 
. if previously billed  IE ostat = "B" or "Q" then use numbers (qty's, nets, etc) off the invoice record can be overridden by FrcCompFlag = Y
.
."REQUIRES" that you read all additional charge records and Load the NinvAcdRec  (record)
.    Last change: DLH 6/11/2002 10:23:27 AM
.notes via dave 5nov99 don't understand why cmpt94x should ever be added to nininc.
.????????? think it is the current prob.
.For previous releases see archives

.REQUIRED FOR ENTRY:
.valid invoice read
.  if creating a new invoice you need a valid
.      ninmrge read
.      ninshp read
. for all other accesses those reads are not necessary as qtyin will be used
. from the invoice record however the following are required.
.      ninord read    .need
.      nindat  read
.      ninown  read
.      nacddd & io includes
.      consacct include
.Returns: partial list
.         Formar
.         Ap
.         Formap2
:         Lrinc
.         nininc
.********************************************************************************************************************************
COMPUTE  
         MOVE      C0 TO SELECT
         move      c0 to nacd1flag
               IF             (Ostat = "B" or Ostat = "Q")
                              IF             (FrcCompFlag = Yes)
                              MOve           No to FrcCompFlag             .reset flag
                              goto           COmputeNew
                              endif
               goto           Computed          
               else
               goto           ComputeNew
               endif
.order date - pre 07/1/99 do it the old way.
.COmputed - to make it easy we wiil use all the same subroutines as the old compute HOWEVER we will populate fields like 
.shipping and merge qtys from fields stored in the invoice record
.NOTE - this will entail Forcing flag to call computenew  if we are modifying an invoice and need to pick up corrected/new 
.variables from secondary files
Computed
.Variable list:   Squant, nmrgrqty, nmrgnet
               Move           Qtyin to Squant
               move           qtyin to nmrgrqty
               move           qtybild to nmrgnet
. I belv we are moving qtyin to squant and to nmrgrqty. qtybild to nmrgnet need to look at the exchange qtys as well
ComputeNew
         move      "07" to mm
         move      "01" to dd
         move      "99" to yy
         call      cvtjul
         move      juldays to str5      .date we start billing commission on nets the new way
         move      c0 to mm
         move      c0 to dd
         move      c0 to yy
         move      oodtem to mm
         move      oodted to dd
         move      oodtey to yy
         call      cvtjul                 .order date
         move      str5 to n5
         if        (juldays >= n5)        .order date after 7/1/99
          move      yes to ordteflag
          RESET     DISCMLRS                  .DISCOUNTED MLR?
          UNPACK    OMLRNUM INTO STR4
          SCAN      STR4 IN DISCMLRS
          IF         EQUAL
                    move      no to ordteflag           .YES
          ENDIF
         else
          move      no to ordteflag
         endif
.patch 10.002  29August2006
         move      "07" to mm
         move      "12" to dd
         move      "06" to yy
         call      cvtjul
         move      juldays to str5      .date we start billing commission on nets the new way
         move      str5 to n5
         move      c0 to mm
         move      c0 to dd
         move      c0 to yy
         move      INVDTEM to mm
         move      INVDTED to dd
         move      INVDTEY to yy
         call      cvtjul                 .order date
//         call     debug
         if        (juldays < n5)        .INV date before 7/12/06
          RESET     DISCMLRSNOT                  .previously DISCOUNTED MLR?
          UNPACK    OMLRNUM INTO STR4
          SCAN      STR4 IN DISCMLRSNot
          IF         EQUAL
                    move      no to ordteflag           .YES
          ENDIF
         Endif      
.end patch  10.002
         MOVE      C0 TO SHIP
         MOVE      C0 TO CMREFLAG
         MOVE      C0 TO CMREDOLR
         CLEAR     PREPAYSW
         CLEAR     PPSW
         MOVE      C0 TO FORMAP2
         MOVE      C0 TO FORMAP3
         MOVE      C0 TO SAVEAP
         MOVE      C0 TO TAXES
         MOVE      C0 TO BRKCOM
         MOVE      C0 TO CMPCOM
         MOVE      C0 TO LRINC
         move    c0 to lrincx
         move      c0 to grossbase
         move      c0 to netsavins
         move      c0 to netsavfee
         MOVE      C0 TO ninINC
         MOVE      C0 TO AP
         MOVE      C0 TO SVECOM
         MOVE      C0 TO PREPAY
         MOVE      C0 TO POST
         MOVE      C0 TO GROSS
         MOVE      C0 TO FORMAR
         MOVE      C0 TO PRICE
         MOVE      C0 TO PRICEx
         MOVE      C0 TO AMOUNT
         MOVE      C0 TO AMOUNTx
         MOVE      C0 TO SVEACR
         MOVE      C0 TO CMPT92
         MOVE      C0 TO FORM72x
         MOVE      C0 TO FORM73
         MOVE      C0 TO CMPT94
         MOVE      C0 TO CMPT94x
         MOVE      C0 TO FORM32
         Move      C0 to form34
         MOVE      NO TO LSTMSW
.         
              Call            SetNetFlag
.
              call            calcrqty
         MOVE      QTYbild TO CMPT94
         COMPARE   C0 TO CMPT94
         GOTO      FNINCD IF EQUAL
         DIVIDE    THOUS INTO CMPT94
         MOVE      CMPT94 TO AMOUNT              QUANTITY BILLED
.lets get net goodies
..1st is there a minimum & do we meet it?
         compare   c0 to onetmin
         goto      nets if equal           .no min
         goto      nets if less
         goto      chkmin
nets     
               cmatch         "F" to onetfm               .flat ?
               if             equal                       .yes
               move           c3 to netflag                .set flag
               cmatch         yes to lstmsw                 .Lstmgmt its ok! 1/29/97.
                              if        equal
                              goto      flat                        .yes list man go calc that way
                              endif      
               endif      
.
               cmatch         yes to lstmsw                    .Lstmgmt its ok! 1/29/97.
               if             equal
               move           c3 to netflag
               goto           flat
               endif      
.
               move           c0 to net94n
               move           c0 to net94
               move           c0 to CMPT92 
               if             (OLnum = "002948" OR OLNUM = "015108")         .lifestyle fundraising     .temp fix dlh
               MOVE           QTYbild to NET94
               DIV            THOUS INTO NET94
               GOTO           SPLTQTY
               ENDIF
               move      c0 to net94
               if        (qtyin > 0)           .previously billed
               move      qtyin to net94
               else
                              if        (nmrgrqty > 0)        .not previously billed merge info?
                              move      nmrgrqty to net94
                              else
                              move      c0 to n9
                              move      squant to n9
                                           if        (n9 > 0)          .not previously billed shipping info?
                                            move      n9 to net94
                                            endif
                              endif
               endif
               move      irnetper to CMPT92
               div       hund into CMPT92
               mult      CMPT92 by net94
               compare   net94 to nmrgnet      .which is bigger net or actual
               if        less                      .use net
               move      net94 to net94n           10/31/95 save qty
               move      net94 to n10              .force round to whole # of names
               move      n10 to net94              .ditto         
               div       thous into net94
               goto      spltqty                    .use net
               endif
               move      nmrgnet to net94n         10/31/95 save qty
               move      nmrgNET to net94      .we got more names
               div       thous into net94
               goto      spltqty
.
chkmin
               move      c0 to net94
               if        (qtyin > 0)           .previously billed
               move      qtyin to net94
               else
                              if        (nmrgrqty > 0)        .not previously billed merge info?
                              move      nmrgrqty to net94
                              else
                              move      c0 to n9
                              move      squant to n9
                                           if        (n9 > 0)          .not previously billed shipping info?
                                            move      n9 to net94
                                            endif
                              endif
               endif
               move      irnetper to CMPT92
               div       hund into CMPT92
               mult      CMPT92 by net94
.? use what amount to qualify for min         10Aug99 DLH
               move           oqty to n10
               compare        onetmin to n10         
               if             less           .we don't qualify
                              if        (OLnum = "002948" OR OLNUM = "015108")         .lifestyle fundraising     .temp fix dlh
                              goto      nets
                              endif
               move      c1 to netflag
               goto      spltqty
               else
               goto      nets           .we pass
               endif

.
spltqty
         MOVE      iRexqty TO CMPT94x              .split qty
         DIVIDE    THOUS INTO CMPT94x
         MOVE      CMPT94x TO AMOUNTX            .split QUANTITY BILLED
.
         MOVE      C0 TO CMPT92
          compare   c0 to ppm
          goto      fnincd if equal
          move      ppm to price
.
         MOVE      C0 TO FORM72x                  exchange
.testing  24 Feb 2010 DH
          move      c0,pricex
.testing  24 Feb 2010 DH
          move      iexppm to pricex
.
         branch    netflag of grosbase,netbase,netbase
.
flat          
         move      c0 to net94
         move      c0 to CMPT94
         move      QTYbild  to net94
         move      net94 to n10              .force round to whole # of names
         move      n10 to net94              .ditto         
         MOVE      N10 TO CMPT94
         div       thous into net94        added 10/13/95
         move      net94 to n10         
         goto       spltqty

grosbase 
         MULT      PRICE BY CMPT94
         MOVE      CMPT94 TO GROSS               GROSS BILLING.
         MOVE      CMPT94 TO FORMAR                  ACCOUNTS RECEIVABLE.
         MOVE      CMPT94 TO SVEACR              WORKING AR.
         move      c0 to GrossBaseSR
         add       CMPT94 to GrossBaseSR
         goto      spltbase
.
netbase 
         move      c0 to argh94
..09Aug99 add c3 check for flats on brokerage ie lifestyle
         if        ((c2 = netflag or c3 = netflag) & lstmsw = "N" & ordteflag = "Y" & paycode <> "4")         brokerage ,net order and orderdate 7/1/99 or later
         MULT      PRICE BY NEt94
         MOVE      net94 TO GROSS               GROSS "NEt" BILLING.
        CMATCH     YES TO MRGSW
        IF         EQUAL
        MOVE       NMRGRQTY TO argh94
        ELSE
        CMATCH     YES TO SHIPSW
        IF         EQUAL
        MOVE       SQUANT TO argh94
        ELSE
        MOVE       OQTY TO argh94
        ENDIF
        ENDIF

         divide    thous into argh94
         mult      price by argh94              .
         Move      argh94 TO grossbase           .save it for print/display
         move      c0 to GrossBaseSR
         add       Argh94 to GrossBaseSR

               sub       net94 from argh94              .sub net in
               MOVE      argh94 TO  netsavins           .save for later
.begin patch 10.001               
.               MOVE      c0 TO FORM32
.               MOVE      COMMPCT TO FORM32
.               mult       ".01" by form32
.               mult      form32 by argh94              .extra commission ie savings fee
               MOVE      c0 TO FORM34
               MOVE      COMMPCT TO FORM34
               mult       ".01" by form34
               mult      form34 by argh94              .extra commission ie savings fee
.end patch 10.001               
         MOVE      argh94 TO  netsavfee           .save for later
         add       argh94 to gross
         MOVE      argh94 TO LRINCx
         MOVE      gross TO FORMAR                  ACCOUNTS RECEIVABLE.
         MOVE      gross TO SVEACR              WORKING AR.
         goto      spltbase
         else

         MULT      PRICE BY NEt94
         MOVE      net94 TO GROSS               GROSS BILLING.
         MOVE      net94 TO FORMAR                  ACCOUNTS RECEIVABLE.
         MOVE      net94 TO SVEACR              WORKING AR.
         move      c0 to GrossBaseSR
         add       Net94 to GrossBaseSR
         goto      spltbase

         endif







.
spltbase 
         MULT      PRICEx BY CMPT94x             .split portion
         add       CMPT94x TO GROSS               GROSS BILLING.
         add       CMPT94x TO SVEACR              WORKING AR.
         move      c0 to GrossBaseSE
         add       CMPT94x to GrossBaseSE           
.
FNINCD   MOVE      C0 TO N1                      CLEAR BRANCH VAR.
         MOVE      PAYCODE TO N1                     PAYABLE CODE
         BRANCH    N1 TO CHKCHRGS,FNINCD2,CHKCHRGS,FNINCD4
FNINCD2  
.begin patch 10.001               
.         MOVE      C0 TO FORM32
.         MOVE      COMMPCT TO FORM32
.         COMPARE   c0 TO FORM32                    C0 COMMISSION?
         MOVE      COMMPCT TO FORM34
         COMPARE   c0 TO FORM34                    C0 COMMISSION?
         GOTO      CHKCHRGS IF EQUAL
.         DIVIDE    HUND INTO FORM32
         DIVIDE    HUND INTO FORM34
         if        ((c2 = netflag or c3 = netflag) & lstmsw = "N" & ordteflag = "Y" & paycode <> "4")         brokerage ,net order and orderdate 7/1/99 or later
         MULT      PRICE BY CMPT94
         MOVE      CMPT94 TO GROSSar              GROSS BILLING.
         MOVE      grossAR TO LRINC
.         MULT      form32 BY LRINC               LR INCOME based on qty it.
         MULT      form34 BY LRINC               LR INCOME based on qty it.
         MOVE      LRINC TO CMPCOM
         else
         MOVE      FORMAR TO LRINC
.         MULT      form32 BY LRINC               LR INCOME based on qty it.
         MULT      FORM34 BY LRINC               LR INCOME.
         MOVE      LRINC TO CMPCOM
.end patch 10.001               
         endif
         GOTO      CHKCHRGS
FNINCD4
               rep       zfill in olnum
               if             (olnum = "005051" or olnum = "016909" or olnum = "021302" or olnum = "016898" or olnum = "014477" or olnum = "009766")
               move      formar to nininc
               else
               MOVE      FORMAR TO LRINC
               endif
CHKCHRGS
               For            Index,"1" to "15"
               CLEAR          PREPAYSW
               move           c0 to nacdfld
DebugWhat
               packkey        Nacdfld from NinvAcdREc(INdex).NinvAcdCodeRec
               if             (Nacdfld <> "   " & Nacdfld <> "")
               call           NextCharge
               endif
               repeat
               goto           endchrgs
NextCharge
         match     "001" TO nacdfld
         if         equal
         move      c1 to nacd1flag         .we had a code 1
         else
         move      c0 to nacd1flag
         endif
               if             (ninvoutflag = c1 or NINVOutFlag = c2)
. 1= printing logic 2 = display logic
.end nininv patch 10.0
               clear          nacdtext
               call           nacdkey
               Return         if over
               endif
               MOVE           C0 TO ACAMT
               MOVE           C0 TO NewACCMPR
               MOVE           C0 TO ANINCD
               Move           NinvAcdREc(INdex).NinvAcdRateRec to acamt
               Move           NinvAcdREc(INdex).NinvAcdPercRec to NewACCMPR
               
               MOVE           C0 TO ANINCD             CLEAR BRANCH
               If             (index = c4 or index = c5)
.               call           debug
               endif
               move           NinvAcdREc(INdex).NINVAcdANINCDRec to str3
               call           trim using str3
               move           str3 to Anincd
               move           NinvAcdREc(INdex).NINVAcdANINCDRec to NINVAcdANINCD
               move           NinvAcdREc(INdex).NINVAcdQTYRec to NINVAcdQTY
               MOve           NInvAcdRec(Index).NinvAcdAextcdRec to NinvAcdAextcd
               Move           NinvAcdREc(INdex).NinvAcdRateTRec to NinvAcdRateT

.
              IF              (nacdfld = "00A")
.          match     "00A" to nacdfld
.          if        equal
              move      "100" to codenum
              ElseIf          (Nacdfld = "00B")
.          endif
.          match     "00B" to nacdfld
.          if        equal
              move      "101" to codenum
              Else
.          endif
              MOVE      nacdfld TO CODENUM
              endif
         BRANCH    CODENUM TO CD01:      01    BROKER COMMISION
                              CD02:      02    BROKER COMMISSION
                              CD03:      03    Broker commssion   5/94
                              CD04:      04    TAXES canadian use
                              CD05:      05    TAXES
                              CD06:      06    TAXES
                              CD07:      07    TAXES
                              CD08:      08    TAXES
                              CD09:      09    TAXES
                              CD10:      10    TAXES
                              CD11:      11    TAXES
                              SLCTPERM:  12    INDUSTRY SELECT (NY=BROKER COMM)
                              SLCTPERM:  13    SELECTIONS
                              SLCTPERM:  14    SELECTIONS
                              SLCTPERM:  15    sex SELECTION
                              SLCTPERM:  16    SELECTIONS
                              SLCTPERM:  17    key labels SELECTIONS
                              SLCTPERM:  18    SELECTIONS
                              SLCTPERM:  19    split fee SELECTIONS
                              SLCTPERM:  20    SELECTIONS
                              SLCTPERM:  21    SELECTIONS
                              SLCTPERM:  22    SELECTIONS
                              SLCTPERM:  23    SELECTIONS
                              SLCTPERM:  24    SELECTIONS
                              SLCTPERM:  25    SELECTIONS
                              SLCTPERM:  26    SELECTIONS
                              SLCTPERM:  27    SELECTIONS
                              SLCTPERM:  28    SELECTIONS
                              SLCTPERM:  29    SELECTIONS
                              SLCTPERM:  30    SELECTIONS
                              SLCTPERM:  31    SELECTIONS
                              SLCTPERM:  32    SELECTIONS
                              SLCTPERM:  33    SELECTIONS
                              SLCTPERM:  34    recency SELECTIONS
                              SLCTRUNC:  35    running charge SELECTIONS
                              SLCTPERM:  36    SELECTIONS
                              SLCTPERM:  37    SELECTIONS
                              SLCTPERM:  38    SELECTIONS
                              SLCTPERM:  39    SELECTIONS
                              SLCTPERM:  40    SELECTIONS
                              SLCTPERM:  41    SELECTIONS
                              SLCTPERM:  42    ps labels SELECTIONS
                              SLCTPERM:  43    SELECTIONS
                              SLCTPERM:  44    SELECTIONS
                              SLCTPERM:  45    SELECTIONS
                              SLCTPERM:  46    SELECTIONS
                              SHIPPERM:  47    SHIPPING
                              SHIPPERM:  48    10% COMM RECAP SEP97
                              SLCTPERM:  49    SELECTIONS
                              SLCTPERM:  50    SELECTIONS
                              SLCTFLAT:  51    MIN FLAT. (NY=GROSS BILLING)
                              SLCTFLAT:  52    SELECTIONS
                              SLCTFLAT:  53    SELECTIONS
                              SHIPFLAT:  54    SHIPPING
                              SLCTFLAT:  55    SELECTIONS
                              SLCTFLAT:  56    SELECTIONS
                              SLCTFLAT:  57    SELECTIONS
                              TAXFLAT:   58    TAXES
                              SLCTFLAT:  59    SELECTIONS
                              SHIPFLAT:  60    SHIPPING
                              SLCTFLAT:  61    SELECTIONS
                              SHIPFLAT:  62    SHIPPING
                              SLCTFLAT:  63    SELECTIONS
                              SLCTFLAT:  64    SELECTIONS
                              SLCTFLAT:  65    SELECTIONS
                              SLCTFLAT:  66    SELECTIONS
                              SLCTFLAT:  67    EXCHANGE FEE
                              SLCTFLAT:  68    SELECTIONS
                              SHIPFLAT:  69    SHIPPING
                              SLCTFLAT:  70    SELECTIONS
                              SHIPFLAT:  71    SHIPPING
                              CD72:      72    OUR POSTAGE
                              SHIPFLAT:  73    SHIPPING
                              SHIPFLAT:  74    SHIPPING
                              SHIPFLAT:  75    SHIPPING
                              SHIPFLAT:  76    SHIPPING
                              SHIPFLAT:  77    SHIPPING
                              SHIPFLAT:  78    SHIPPING
                              SHIPFLAT:  79    SHIPPING
                              SHIPFLAT:  80    SHIPPING
                              SLCTFLAT:  81    10% cOMM RECAP 9/97
                              SHIPFLAT:  82    SHIPPING
                              SHIPFLAT:  83    SHIPPING
                              SLCTFLAT:  84    SELECTIONS
                              SLCTFLAT:  85    SELECTIONS
                              SLCTFLAT:  86    SELECTIONS
                              SLCTFLAT:  87    SELECTIONS
                              SLCTFLAT:  88    SELECTIONS
                              SLCTFLAT:  89    SELECTIONS
                              SLCTFLAT:  90    SELECTIONS
                              SLCTFLAT:  91    SELECTIONS
                              TAXFLAT:   92    TAXES
                              TAXFLAT:   93    TAXES
                              TAXFLAT:   94    TAXES
                              CD95:      95    BROKER COMMISSION
                              CD96:      96    PRE-PAYMENTS
                              CD97:      97    BROKER COMMISSION
                              cd98:      98    TAXES FLAT pass thru
                              CREDIT:     99    CREDIT
                              cd100:      100 (00A) running charges on net
                             SLCTFLAT:  101 (00b)   Flat Email SELECTION
                              SLCTFLAT:  102   Flat Diskette SELECTION
                              SLCTFLAT:  103   Flat bELOW MINIMUM
                              SLCTFLAT:  104   Rush Processing fee
                              SLCTFLAT:   105   Processing fee
                              SLCTFLAT:   106   MULTIPLY Nth
                              SLCTFLAT:   107   COMMISSION on negotiated savings
                              SLCTFLAT:   108   NON refundable admin fee
                              SLCTFLAT:   109   Conversion Ascii, comma delimited
                              SLCTFLAT:  110        ....note all selects Up to 150 are flat
                              SlctFlat:  111     Flat FTP Fee
                              SlctFlat:  112     Flat CDROM Fee
                              SlctFlat:  113     Flat Escrow Fee
                              SlctFlat:  114                ;Generic media/delivery fee
.begin patch 10.003
.                              NotActive:  115
                              SlctFlat:  115                .non-reciprocal fee     30 Aug 07
.end patch 10.003
.begin patch 10.1
                              cd116:  116
.end patch 10.1
.begin patch 10.11
.                              NotActive:  117
                              SlctFlat:  117                .Credit card fee
.begin patch 10.11
                              NotActive:  118
                              NotActive:  119
                              NotActive:  120
                              NotActive:  121
                              NotActive:  122
                              NotActive:  123
                              NotActive:  124
                              NotActive:  125
                              NotActive:  126
                              NotActive:  127
                              NotActive:  128
                              NotActive:  129
                              NotActive:  130
                              NotActive:  131
                              NotActive:  132
                              NotActive:  133
                              NotActive:  134
                              NotActive:  135
                              NotActive:  136
                              NotActive:  137
                              NotActive:  138
                              NotActive:  139
                              NotActive:  140
                              NotActive:  141
                              NotActive:  142
                              NotActive:  143
                              NotActive:  144
                              NotActive:  145
                              NotActive:  146
                              NotActive:  147
                              NotActive:  148
                              NotActive:  149
                              NotActive:  150  ...all the above to 109 are flat
                              SLCTPERM:   151
                              SlctRunPerM:  152
                              SLCTPERM:  153
                              Slct10Per:  154 
.begin patch 10.001
.                              SLCTPERM  155                     ;Generic media/delivery Fee
                              SLCTPERM:  155                     ;Generic media/delivery Fee
.begin patch 10.003
.                              SLCT5PER   156                     ;commision 5%
                              SLCT5PER:   156                     ;commision 5%
                              SLCTPERM:   157                     .non-reciprocal fee    30 Aug 07
.begin patch 10.004
                              CD158:       158                     .Less 15% commission    30 Aug 07
.BEGIN PATCH 10.01           .2% REBATE ON COMMISSION TO MLR
                              CD159:       159                     .Less 8% commission  
.                              CD159       159                     .Less 8% commission  
                              CD160:        160                      .2% commission rebate to Mailer
.begin patch 10.1
                              Slct2per:       161                      .2% from owner
                              CD162        162                      .25% LR commission rebate to SC
.end patch 10.1
.End PATCH 10.01           .2% REBATE ON COMMISSION TO MLR
.end patch 10.004
.end patch 10.003
.end patch 10.001
NotActive
               GOTO           CD00
...............................................................................
.
SLCTPERM
               if             (NinvAcdRateT = "f")
               goto           SLCTFLAT
               endif
.begin Ninv0001 Patch 10.0
.               move      c0 to n2
.               move      irnetper to n2
.               if        (mrgsw = Yes | n2 != 0)
.               move      qtyin to cmpt94
.               else
.               move      QTYbild  to cmpt94
.               endif
               Move           NinvAcdQty to Cmpt94
.end Ninv0001 Patch 10.0
               compare        "41" to codenum               .broker comm?
               if             equal                         .yes, calc it
               move           QTYbild  to cmpt94
               endif
. another perm for minimums.arcggggggggggggggggggggg
.
.begin Ninv0001 Patch 10.0
.         if        (qtyin < 5000)
.         move      QTYbild  to cmpt94
.         endif
.end Ninv0001 Patch 10.0
         DIVIDE    THOUS BY CMPT94
         MULTIPLY  ACAMT BY CMPT94
         MOVE      CMPT94 TO AMOUNT
         ADD       AMOUNT TO SELECT
         CMATCH    YES TO MCOPIES                .REGIONAL
         GOTO      SLCTPRMX IF NOT EQUAL         .NO
         RESET     FULHOUSE
         SCAN      OWNCTN IN FULHOUSE
         if        equal                         .yes, lets check for affected selects.
         compare   "15" to codenum               .sex select?
         goto       slctprmx if equal            .yes, calc it
         compare   "17" to codenum               .key  select?
         goto       slctprmx if equal            .yes, calc it
         compare   "19" to codenum               .split  select?
         goto       slctprmx if equal            .yes, calc it
         compare   "34" to codenum               .recency  select?
         goto       slctprmx if equal            .yes, calc it
         compare   "42" to codenum               .ps labels  select?
         goto       slctprmx if equal            .yes, calc it
         endif
         goto      sumamt               .none of the above - exit.
slctprmx
         RESET     FULHOUSE
         SCAN      OWNCTN IN FULHOUSE
         if        equal               .yes, lets check for affected selects.
         move      c0 to CMPT94X
         MOVE      iRexqty TO CMPT94X
         DIVIDE    THOUS BY CMPT94X
         MULTIPLY  ACAMT BY CMPT94X
         add       CMPT94X TO AMOUNT
         ADD       CMPT94X TO SELECT
         endif
         GOTO      SUMAMT
................................................................................
SLCTrunc 
               if             (NinvAcdRateT = "f")
               goto           SLCTFLAT
               endif
               if             (NinvAcdAextcd = "1" & QtyAdjFlag = "Y")
               move           OrigQtybild to cmpt94
               else
               move           QTYbild  to cmpt94
               endif

               DIVIDE         THOUS BY CMPT94
               compare        c2 to netflag              .net order?
               if             equal                      .yes
               move           net94n to CMPT94
               divide         thous by CMPT94
               endif
               MULTIPLY       ACAMT BY CMPT94
               MOVE           CMPT94 TO AMOUNT
               ADD            AMOUNT TO SELECT
               GOTO           SUMAMT
.
SLCTrunPerM
         move      chgrqty  to cmpt94
         DIVIDE    THOUS BY CMPT94
         MULTIPLY  ACAMT BY CMPT94
         MOVE      CMPT94 TO AMOUNT
         ADD       AMOUNT TO SELECT
         GOTO      SUMAMT
..............................................................................................
SHIPPERM 
.begin Ninv0001 Patch 10.0
               Move           NinvAcdQty to Cmpt94
.         move      QTYbild  to cmpt94
.end Ninv0001 Patch 10.0
         DIVIDE    THOUS BY CMPT94
         MULTIPLY  ACAMT BY CMPT94
         MOVE      CMPT94 TO AMOUNT
         ADD       AMOUNT TO SHIP
         MOVE      iRexqty TO CMPT94X
         DIVIDE    THOUS BY CMPT94X
         MULTIPLY  ACAMT BY CMPT94X
         add       CMPT94X TO AMOUNT
         ADD       CMPT94X TO ship
         COMPARE   "48" TO CODENUM
         IF        EQUAL
         ADD       AMOUNT TO CMREDOLR
         MOVE      C1 TO CMREFLAG
         ENDIF
         GOTO      SUMAMT
...............................................................................
SLCTFLAT MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO SELECT
         COMPARE   "81" TO CODENUM
         IF        EQUAL
         ADD       AMOUNT TO CMREDOLR
         MOVE      C1 TO CMREFLAG
         ENDIF
.
         GOTO      SUMAMT
.
SHIPFLAT MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO SHIP
         GOTO      SUMAMT
.
TAXFLAT  MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CREDIT
         MOVE      ACAMT TO AMOUNT
         MULT      "-1" BY AMOUNT
         GOTO      SUMAMT
.
.
.
CD00     MATCH     "0331" TO MLRN
         GOTO      CD00CONT IF NOT EQUAL
         PACK      STR4 WITH INVDTEY,INVDTEM
CD00CONT MOVE      STR7 TO FORM73
         DIVIDE    THOUS BY FORM73
         MULTIPLY  PRICE BY FORM73
         MULTIPLY  NewACCMPR BY FORM73
         MULTIPLY  "-1" BY FORM73
         MOVE      FORM73 TO AMOUNT
         ADD       AMOUNT TO FORMAR
         ADD       AMOUNT TO LRINC
         GOTO      ADDAMT
.
CD01     MOVE      SVEACR TO AMOUNT
         sub       CMPT94X from amount             .don't give comm on exchange
         SUB       netsavfee FROM amount
         MULTIPLY  ".10" BY AMOUNT 
         ADD       AMOUNT TO BRKCOM
         IF        (obrknum = "0192")             .epsilon ?
         RESET     epsimlrs
         SCAN      omlrnum in epsimlrs           .Is epsilon is it a rebate client?
                   IF not equal
.begin patch 10.001
.                   MOVE      COMMPCT TO FORM32    .yes continue
.                   mult       ".01" by form32
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
                   endif
         RESET     covemlr
         SCAN      omlrnum in covemlr            .Is this Covenant House??
                   IF        equal
.                   MOVE      COMMPCT TO FORM32    .yes continue
.                   mult       ".01" by form32
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
         move      brkcom to amount
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC
         GOTO      SUMAMT
.
CD02     MOVE      SVEACR TO AMOUNT
         sub       CMPT94X from amount             .don't give comm on exchange
         SUB       netsavfee FROM amount
         MULTIPLY  ".05" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         IF        (obrknum = "0192")             .epsilon ?
         RESET     epsimlrs
         SCAN      omlrnum in epsimlrs           .Is epsilon is it a rebate client?
                   IF not equal
.                   MOVE      COMMPCT TO FORM32    .yes continue
.                   mult       ".01" by form32
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
              CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
                   endif
         RESET     covemlr
         SCAN      omlrnum in covemlr            .Is this Covenant House??
                   IF        equal
.                   MOVE      COMMPCT TO FORM32    .yes continue
.                   mult       ".01" by form32
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
         move      brkcom to amount
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC
         GOTO      SUMAMT
.
CD03     MOVE      SVEACR TO AMOUNT
         sub       CMPT94X from amount             .don't give comm on exchange
         SUB       netsavfee FROM amount
         MULTIPLY  ".20" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         IF        (obrknum = "0192")             .epsilon ?
         RESET     epsimlrs
         SCAN      omlrnum in epsimlrs           .Is epsilon is it a rebate client?
                   IF not equal
.                   MOVE      COMMPCT TO FORM32    .yes continue
.                   mult       ".01" by form32
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
                   endif
         RESET     covemlr
         SCAN      omlrnum in covemlr            .Is this Covenant House??
                   IF        equal
.                   MOVE      COMMPCT TO FORM32    .yes continue
.                   mult       ".01" by form32
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
.end patch 10.001                   
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC       
         GOTO      SUMAMT

..begin patch    30Aug2007                     .less 15% commission
CD158     MOVE      SVEACR TO AMOUNT
         sub       CMPT94X from amount             .don't give comm on exchange
         SUB       netsavfee FROM amount
         MULTIPLY  ".15" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         IF        (obrknum = "0192")             .epsilon ?
         RESET     epsimlrs
         SCAN      omlrnum in epsimlrs           .Is epsilon is it a rebate client?
                   IF not equal
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
                   endif
         RESET     covemlr
         SCAN      omlrnum in covemlr            .Is this Covenant House??
                   IF        equal
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC       
         GOTO      SUMAMT
..end patch    30Aug2007
..begin patch    10.004

CD159     MOVE      SVEACR TO AMOUNT
         sub       CMPT94X from amount             .don't give comm on exchange
         SUB       netsavfee FROM amount
         MULTIPLY  ".08" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         IF        (obrknum = "0192")             .epsilon ?
         RESET     epsimlrs
         SCAN      omlrnum in epsimlrs           .Is epsilon is it a rebate client?
                   IF not equal
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
                   endif
         RESET     covemlr
         SCAN      omlrnum in covemlr            .Is this Covenant House??
                   IF        equal
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC       
         GOTO      SUMAMT
.end patch    10.004
.BEGIN PATCH 10.01           .2% REBATE ON COMMISSION TO MLR
CD160    MOVE      SVEACR TO AMOUNT
         sub       CMPT94X from amount             .don't give comm on exchange
         SUB       netsavfee FROM amount
         MULTIPLY  ".02" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         IF        (obrknum = "0192")             .epsilon ?
         RESET     epsimlrs
         SCAN      omlrnum in epsimlrs           .Is epsilon is it a rebate client?
                   IF not equal
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
                   endif
         RESET     covemlr
         SCAN      omlrnum in covemlr            .Is this Covenant House??
                   IF        equal
                   MOVE      COMMPCT TO FORM34    .yes continue
                   mult       ".01" by form34
                   CALL      calcrebate
                   ADD       rebate to brkcom
                   endif
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC       
         GOTO      SUMAMT
.END PATCH 10.01
.BEGIN PATCH 10.1           .25% REBATE of our COMMISSION TO MLR
CD162    MOVE      LRinc TO AMOUNT
          MOVE      SVEACR TO AMOUNT
          sub       CMPT94X from amount             .don't give comm on exchange
          MOVE      c0 TO FORM34
          MOVE      COMMPCT TO FORM34
          mult       ".01" by form34
          mult      form34 by Amount
.         SUB       netsavfee FROM amount          .we do give on netsavfee
         MULTIPLY  ".25" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC       
         GOTO      SUMAMT
.END PATCH 10.1

CD04     MOVE      SVEACR TO AMOUNT
         MOVE      AMOUNT TO CANUSE        *CANADIAN USE TAX
         SUB       LRINC FROM CANUSE
.rate change entered 04 June 2012 - don't know when it took effect
         MULT      ".13" BY CANUSE
.         MULT      ".10" BY CANUSE
.         MULT      "-1" BY CANUSE
         MOVE      CANUSE TO AMOUNT
         ADD       AMOUNT TO TAXES                          .????
         GOTO      SUMAMT
.
CD05     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0500" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD06     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0600" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD07     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0800" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD08     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0700" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD09     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0625" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD10     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0725" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD11     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".0825" BY AMOUNT
         ADD       AMOUNT TO TAXES
         GOTO      SUMAMT
.
CD12     MOVE      SVEACR TO AMOUNT
         MULTIPLY  ".15" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC
         GOTO      SUMAMT
.
CD51     MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO GROSS
         GOTO      SUMAMT
.
CD72     MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO SHIP
         ADD       AMOUNT TO FLOAMT
         ADD       AMOUNT TO POST
         GOTO      SUMAMT
.
CD95     ADD       ACAMT TO BRKCOM
         MOVE      ACAMT TO AMOUNT
         MULTIPLY  "-1" BY AMOUNT
         GOTO      SUMAMT
.
CD96     MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO PREPAY
         MOVE      YES TO PREPAYSW
         MOVE      YES TO PPSW
         GOTO      OUTCHRGS
.
CD97     ADD       ACAMT TO BRKCOM
         MOVE      ACAMT TO AMOUNT
        MULTIPLY  "-1" BY AMOUNT
         GOTO      SUMAMT
.
cd98     MOVE      ACAMT TO AMOUNT        .flat pass thru tax
.         ADD       AMOUNT TO TAXES       .we just pass thru to the  owner
         GOTO      SUMAMT
................................................................................
.cd100- for net name orders calc additional running charges on unused names.
.note I do not believe this was ever implemented. DLH 05Apr99
CD100
         move      QTYin  to cmpt94
         DIVIDE    THOUS BY CMPT94
         move       qtybild to CMPT94a
         divide     thous by CMPT94a
         sub        CMPT94a from CMPT94
         MULTIPLY  ACAMT BY CMPT94
         MOVE      CMPT94 TO AMOUNT
         ADD       AMOUNT TO SELECT
         GOTO      SUMAMT
.......................................................................................
CD116
         MOVE      ACAMT TO AMOUNT
         MULTIPLY  "-1" BY AMOUNT
         add        Amount to LRinc
         GOTO      SUMAMT

...............................................................................
.
SUMAMT   MOVE      AMOUNT TO SVECOM
         COMPARE   "0" TO NewACCMPR
         GOTO      ADDAMT IF EQUAL
         MULTIPLY  NewACCMPR BY SVECOM
               if        (codenum = 35 or Codenum = 91)
               add            svecom,runchrgtot
               endif
         if        (codenum > 0 & codenum < 12)
          ADD       SVECOM TO LRINC
         GOTO      SUMAMT1
         endif
         if        (codenum = 48 | codenum = 58 | codenum = 72 | codenum = 81 | codenum = 41 | codenum = 85)
         ADD       SVECOM TO LRINC
         GOTO      SUMAMT1
         endif
         if        (codenum > 91 & codenum < 96)
         ADD       SVECOM TO LRINC
         GOTO      SUMAMT1
         endif
         if        (codenum = 67 or Codenum = 45)           .exchange fee
         ADD       SVECOM TO LRINC
         GOTO      SUMAMT1
         endif
         if        (codenum > 96 & codenum <= 99)
         ADD       SVECOM TO LRINC
         GOTO      SUMAMT1
         endif
         if        (codenum = 153)
         ADD       SVECOM TO LRINC
         GOTO      SUMAMT1
         endif

         ADD       SVECOM TO NININC
         GOTO      SUMAMT1
SUMAMT1
         MOVE      C0 TO CALCRUN                 .tdmc stuff
         MOVE      SVECOM TO CALCRUN             .tdmc stuff
         MOVE      AMOUNT TO CMPT92
         SUB       SVECOM FROM CMPT92
         MOVE      CMPT92 TO SVECOM
******TDMC GOODIES.
         branch    tdmcflag to addamt,dotdmc          .checking tdmc this time ?
.                  
         goto      addamt                             .if no goto addamt
dotdmc   COMPARE   C0 TO CALCRUN
         GOTO      ADDAMT IF EQUAL       *NOTHING TO CALC FOR TDMC.
         MOVE      OLNUM TO NDAT3FLD     
         rep       zfill in ndat3fld
         CALL      NDAT3KEY
         GOTO      ADDAMT IF OVER        *NOT IN TDMC BILLING FILE, SKIP.
         CMATCH    B1 TO NDAT3cde
         GOTO      ADDAMT IF not EQUAL       *not TDMC, SKIP.
         CMATCH    B1 TO NDATTDMC
         GOTO      ADDAMT IF EQUAL       *WE DON'T BILL, SKIP.
         MOVE      STAR TO RUNFLAG
         ADD       C1 TO RUNRCNT
         COMPARE   "3.90" TO ACAMT       *RUNNING CHARGE
         GOTO      TDMCRUN IF EQUAL
         ADD        CALCRUN TO RUNRAR
         ADD        CALCRUN TO RUNRFLAT
         GOTO       ADDAMT
TDMCRUN  MOVE       C0 TO N1
         MOVE       OFOCODE TO N1
         BRANCH     N1 OF disket,LABEL,LABEL,LABEL,LABEL,pkgins,TAPE,TAPE,LABEL

TAPE     MOVE       CALCRUN TO FORM82
         ADD        CALCRUN TO RUNRAR
         MULT       ".40" BY FORM82
         ADD         FORM82 TO RUNRLR
         SUB         FORM82 FROM CALCRUN
         ADD        CALCRUN TO RUNRPASS
         GOTO       ADDAMT
LABEL     MOVE       CALCRUN TO FORM82
         ADD         CALCRUN TO RUNRAR
         MULT       ".10" BY FORM82
         ADD         FORM82 TO RUNRLR
         SUB         FORM82 FROM CALCRUN
         ADD        CALCRUN TO RUNRPASS
         GOTO       ADDAMT
disket
         ADD        CALCRUN TO RUNRAR
         ADD        CALCRUN TO RUNRFLAT
         GOTO       ADDAMT
PKGINS
         ADD        CALCRUN TO RUNRAR
         ADD        CALCRUN TO RUNRFLAT
         GOTO       ADDAMT
********END TDMC GOODIES         
******
.
ADDAMT
               BRANCH         ANINCD TO ANINCD1,ANINCD2,anincd3,Anincd4
ANINCD0        ADD            AMOUNT TO FORMAR
               branch         ninvoutflag of outchrgs,outchrgs
               return
ANINCD1        CMATCH         "2" TO PAYCODE
               GOTO           ADDCOM IF EQUAL
               ADD            AMOUNT TO AP
               branch         ninvoutflag of outchrgs,outchrgs
               return
ADDCOM         ADD            SVECOM TO AP
               branch         ninvoutflag of outchrgs,outchrgs
               return
ANINCD2  ADD       AMOUNT TO FORMAR
         GOTO      ANINCD1
ANINCD3  ADD       AMOUNT TO FORMAR
          branch    ninvoutflag of outchrgs,outchrgs
               return
ANINCD4   Subtract  AMOUNT from NININC
          ADD       AMOUNT TO AP
          branch    ninvoutflag of outchrgs,outchrgs
          GOTO      NEXTCHRG
.
OUTCHRGS       
.             call           debug
.begin patch 10.0
.               IF             (NINVOutFlag = 1 & (NINVFrmFlag = c1 or NINVFrmFlag = c2))
.;               if        (ninvoutflag = 1)
.;          compare   c1 to NINVFRMFLAG        * mailer copy ?  9/21/94 dlh
.;          if        equal                 * yes, so if charge is marked hidden
.          BRANCH              ANINCD TO NEXTCHRG,chrgok,nextchrg,nextchrg    * DON'T PRINT CHARGE if code = 1,3,4
.;          endif                           *
.;
.;          compare   c2 to NINVFRMFLAG        * mailer remit copy ?  1/24/95 dlh
.;          if        equal                 * yes, so if charge is marked hidden
.;          BRANCH              ANINCD TO NEXTCHRG,chrgok,nextchrg,nextchrg    * DON'T PRINT CHARGE
.;          endif                           *
.        endif
chrgok
.               if        (ninvoutflag = 1 & Ninvfrmflag = c3 & nacdfld = "041")
.               Goto           Nextchrg
.               endif                           *
.;.
.               if        (ninvoutflag = 1 & Ninvfrmflag = c3 & nacdfld = "085")
.               Goto           Nextchrg
.               endif                           *
.
.               if        (ninvoutflag = 1 & (Anincd = c3 or Anincd = c4))
.               Goto           Nextchrg
.               endif                           *
.
.               if        (ninvoutflag = 1 & Elstcde = "C" & NinvFrmflag = c3 & nacdfld = "003")
.               Goto           Nextchrg
.               endif                           *
.

.               MOVE      c0 TO N3
.               MOVE      nacdfld TO N3
.               COMPARE   N3 TO c11
.           GOTO      CLEARAT IF NOT LESS
.           COMPARE   N3 TO c51
.           GOTO      LOADAT IF NOT LESS
.           if        (n3= 151 | n3 = 152| n3=153)
.           goto      loadat
.           endif
.           GOTO      CLEARAT
.LOADAT     MOVE      AT TO ATPRT      *LOAD PRICE PER M INTO PRINTLINE.
           CLEAR     addqty
           move      c0 to n2
           move      irnetper to n2
               Move           NINVAcdQTY to addqty
           if        (codenum = 152)
           clear     addqty
           MOVE      chgrqty TO addqty
           endif
           MOVE      MASK32 TO ADD$RTE
           MOVE      ACAMT TO FORM32
           EDIT      FORM32 TO ADD$RTE
.           GOTO      OUTCHRG1
.CLEARAT    MOVE      B1 TO ATPRT    *FLAT RATE CLEAR PRICE PER M IN PRINTLINE.
.           clear     addqty
.           CLEAR     ADD$RTE
.           GOTO      OUTCHRG1
.End Patch 10.0
OUTCHRG1 
           MOVE      C0 TO CMPT92
           ADD       AMOUNT TO CMPT92
           CMATCH    YES TO PREPAYSW        *PREPAYMENT ?
           CALL      SUBPRE IF EQUAL          *YES
.................................................................................
           MOVE      MASK92 TO ADD$1
           EDIT      CMPT92 TO ADD$1           ;total
           MOve               Add$1 to NInvAcdRec(Index).NinvAcdTotalRec
           MOVE               addqty to NInvAcdRec(Index).NinvAcdQtyRec
           MOVE               Nacdtext to NInvAcdRec(Index).NinvAcdDescRec
.BEGIN PATCH 10.0
.           BRANCH    INDEX OF CHARGE1,CHARGE2,CHARGE3,CHARGE4,CHARGE5,CHARGE6:
.                       CHARGE7,CHARGE8,CHARGE9,CHARGE10
            return
SUBPRE   
           MULT      "-1" BY CMPT92
           RETURN
.
.CHARGE1
.           MOVE      MASK92 TO ADD$1
.           EDIT      CMPT92 TO ADD$1
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp1
.           move      NinvAcdAextcd to addAext1
.           move      anincd to addncd1
.           move      nacdfld to addcd1
.           MOVE      NACDTEXT TO ADDESC1
.           MOVE      ATPRT TO AT1
.;begin patch 2.0
.           MOVE      addqty TO addqty1
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT1
.           GOTO      NEXTCHRG
.CHARGE2
.           MOVE      MASK92 TO ADD$2
.           EDIT      CMPT92 TO ADD$2
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp2
.;begin patch 2.6
 ;          move      NinvAcdAextcd to addAext2
.;end patch 2.6
.;           move      NewACCMPR to addp2
.          move      anincd to addncd2
.          move      nacdfld to addcd2
.          MOVE      NACDTEXT TO ADDESC2
.           MOVE      ATPRT TO AT2
.;begin patch 2.0
.           MOVE      addqty TO addqty2
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT2
.           GOTO      NEXTCHRG
.CHARGE3
.           MOVE      MASK92 TO ADD$3
.           EDIT      CMPT92 TO ADD$3
.           move      anincd to addncd3
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp3
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext3
.;end patch 2.6
.;           move      NewACCMPR to addp3
.           move      nacdfld to addcd3
.           MOVE      NACDTEXT TO ADDESC3
.           MOVE      ATPRT TO AT3
.;begin patch 2.0
.           MOVE      addqty TO addqty3
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT3
.           GOTO      NEXTCHRG
.CHARGE4
.           MOVE      MASK92 TO ADD$4
.           EDIT      CMPT92 TO ADD$4
.           move      anincd to addncd4
.            clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp4
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext4
.;end patch 2.6
.;          move      NewACCMPR to addp4
.           move      nacdfld to addcd4
.           MOVE      NACDTEXT TO ADDESC4
.;begin patch 2.0
.           MOVE      addqty TO addqty4
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT4
.           MOVE      ATPRT TO AT4
.           GOTO      NEXTCHRG
.CHARGE5
.           MOVE      MASK92 TO ADD$5
.           EDIT      CMPT92 TO ADD$5
.           clear     str6
.          move      NewACCMPR to str6
.          bump      str6 by 2
.           move      str6 to addp5
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext5
.;end patch 2.6
.;           move      NewACCMPR to addp5
.           move      anincd to addncd5
.           move      nacdfld to addcd5
.           MOVE      NACDTEXT TO ADDESC5
.           MOVE      ADD$RTE TO ADD$RT5
.;begin patch 2.0
.           MOVE      addqty TO addqty5
.;end patch 2.0
.           MOVE      ATPRT TO AT5
.           GOTO      NEXTCHRG
.CHARGE6
.           MOVE      MASK92 TO ADD$6
.           EDIT      CMPT92 TO ADD$6
.           move      anincd to addncd6
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp6
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext6
.;end patch 2.6
.;           move      NewACCMPR to addp6
.           move      nacdfld to addcd6
.           MOVE      NACDTEXT TO ADDESC6
.           MOVE      ATPRT TO AT6
.;begin patch 2.0
.           MOVE      addqty TO addqty6
.;end patch 2.0
.          MOVE      ADD$RTE TO ADD$RT6
.          GOTO      NEXTCHRG
.CHARGE7
.           MOVE      MASK92 TO ADD$7
.           EDIT      CMPT92 TO ADD$7
.           move      anincd to addncd7
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp7
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext7
.;end patch 2.6
.;           move      NewACCMPR to addp7
.           move      nacdfld to addcd7
.           MOVE      NACDTEXT TO ADDESC7
.           MOVE      ADD$RTE TO ADD$RT7
.;begin patch 2.0
.           MOVE      addqty TO addqty7
.;end patch 2.0
.           MOVE      ATPRT TO AT7
           GOTO      NEXTCHRG
.CHARGE8
.           MOVE      MASK92 TO ADD$8
.           EDIT      CMPT92 TO ADD$8
.           move      anincd to addncd8
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp8
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext8
.;end patch 2.6
.;           move      NewACCMPR to addp8
.           move      nacdfld to addcd8
.           MOVE      NACDTEXT TO ADDESC8
.           MOVE      ATPRT TO AT8
.;begin patch 2.0
.           MOVE      addqty TO addqty8
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT8
.           GOTO      NEXTCHRG
.CHARGE9
.           MOVE      MASK92 TO ADD$9
.           EDIT      CMPT92 TO ADD$9
.           move      anincd to addncd9
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp9
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext9
.;end patch 2.6
.;           move      NewACCMPR to addp9
.           MOVE      ATPRT TO AT9
.           move      nacdfld to addcd9
.           MOVE      NACDTEXT TO ADDESC9
.;begin patch 2.0
.           MOVE      addqty TO addqty9
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT9
.           GOTO      NEXTCHRG
.CHARGE10
.           MOVE      MASK92 TO ADD$10
.           EDIT      CMPT92 TO ADD$10
.           move      anincd to addncd10
.           MOVE      ATPRT TO AT10
.           clear     str6
.           move      NewACCMPR to str6
.           bump      str6 by 2
.           move      str6 to addp10
.;begin patch 2.6
.           move      NinvAcdAextcd to addAext10
.;end patch 2.6
.;           move      NewACCMPR to addp10
.           move      nacdfld to addcd10
.           MOVE      NACDTEXT TO ADDESC10
.;begin patch 2.0
.           MOVE      addqty TO addqty10
.;end patch 2.0
.           MOVE      ADD$RTE TO ADD$RT10
.           GOTO      NEXTCHRG
.;END PATCH #6.8 - INCREASED VAR
.;
ENDCHRGS MOVE      C0 TO N1              CLEAR BRANCH VAR.
         MOVE      PAYCODE TO N1
         BRANCH    N1 TO PAYCD1,PAYCD2,PAYCD3
.PAYCD0 - PAYCODE = '0' OR ' ', SO GROSS=A/P
PAYCD0   MOVE      FORMAR TO AP
..bad patch
         add        LrincX to LRINC
..bad patch
         SUBTRACT  LRINC FROM AP
         SUBTRACT  TAXES FROM AP
         subtract  nininc from ap
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         add       CMPT94x to formar
         if        (codenum > 0 & codenum < 12)
         goto     addtolr
         endif
         if        (codenum = 48 | codenum = 58 | codenum = 72 | codenum = 81  | codenum = 41 | codenum = 85)
         goto     addtolr
         endif
         if        (codenum = 67 or Codenum = 45)           .exchange fee
         GOTO      addtolr
         endif
         if        (codenum > 91 & codenum < 96)
         goto     addtolr
          endif
         if        (codenum > 96 & codenum <= 99)
         goto     addtolr
         endif
.begin Patch 10.003
         if        (codenum = 154 | codenum = 113 | Codenum = 156)
.end Patch 10.003
         goto     addtolr
         endif
addtoNIN add       CMPT94x to nininc
         return
addtolr  add       CMPT94x to lrinc
         RETURN

         RETURN
.PAYCD1 - PAYCODE = '1', NININCOME = GROSS - A/P - TAXES.
PAYCD1   MOVE      FORMAR TO NININC
         SUBTRACT  AP FROM NININC
         SUBTRACT  TAXES FROM NININC
         SUBTRACT  POST FROM NININC
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         add       CMPT94x to formar
.bad patch
               ADD       LRINCX TO LRINC
.bad patch
         if        (codenum >0 & codenum < 12)
         goto     addtolr
         endif
         if        (codenum = 48 | codenum = 58 | codenum = 72 | codenum = 81 | codenum = 41 | codenum = 85)
         goto     addtolr
         endif
         if        (codenum > 91 & codenum < 96)
         goto     addtolr
         endif
         if        (codenum = 67 or Codenum = 45)           .exchange fee
         goto     addtolr
         endif
         if        (codenum > 96 & codenum <= 99)
         goto     addtolr
         endif
.begin Patch 10.003
         if        (codenum = 154 | codenum = 113 | Codenum = 156)
.end Patch 10.003
         goto     addtolr
         endif
         goto     addtonin
         RETURN
.PAYCD2 - PAYCODE = '2', 2- A/P'S,
.         A/P'S AS DEFINED IN INVOICE RECORD.
PAYCD2   
         MOVE      C0 TO CMPT92
         MOVE      AP1 TO CMPT92
         move      ap1 to saveap
         MOVE      SAVEAP TO AP
         MOVE      FORMAR TO CMPT92
         SUBTRACT  AP FROM CMPT92
         MOVE      AP2 TO FORMAP2
         SUB       FORMAP2 FROM CMPT92
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         MOVE      CMPT92 TO LRINC
         SUB       TAXES FROM LRINC
         SUB       POST FROM LRINC
.bad patch
               ADD       LRINCX TO LRINC
.bad patch
         add       CMPT94x to formar
         if        (codenum >0 & codenum < 12)
         goto     addtolr
         endif
         if        (codenum = 48 | codenum =58 | codenum =72 | codenum =81  | codenum = 41 | codenum = 85)
         goto     addtolr
         endif
         if        (codenum > 91 & codenum < 96)
         goto     addtolr
         endif
         if        (codenum > 96 & codenum <= 99)
         goto     addtolr
         endif
.begin Patch 10.003
         if        (codenum = 154 | codenum = 113 | Codenum = 156)
.end Patch 10.003
         goto     addtolr
         endif
         goto     addtonin
         RETURN
PAYCD3   
         MOVE      C0 TO CMPT92
         MOVE      AP1 TO CMPT92
         MOVE      C0 TO AP
         MOVE      CMPT92 TO AP
         MOVE      FORMAR TO LRINC
..bad patch
.         add       LrincX to  LRINC  .10/07/99 DLH do not need for paycd3
..bad patch

         SUBTRACT  AP FROM LRINC
         SUBTRACT  TAXES FROM LRINC
         SUBTRACT  POST FROM LRINC
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         add       CMPT94X TO formar
         add       CMPT94X to lrinc
         Sub       NININC from LRinc
         RETURN
wipeCvars
           clear     addP1
           clear     addP2
           clear     addP3
           clear     addP4
           clear     addP5
           clear     addP6
           clear     addP7
           clear     addP8
           clear     addP9
           clear     addP10
           clear     addcd1
           clear     addcd2
           clear     addcd3
           clear     addcd4
           clear     addcd5
           clear     addcd6
           clear     addcd7
               clear     addcd8
               clear     addcd9
               clear     addcd10
               CLEAR     ADDESC1
           CLEAR     ADDESC2
           CLEAR     ADDESC3
           CLEAR     ADDESC4
           CLEAR     ADDESC5
           CLEAR     ADDESC6
           CLEAR     ADDESC7
           CLEAR     ADDESC8
           CLEAR     ADDESC9
           CLEAR     ADDESC10
           CLEAR     ADD$1
           CLEAR     ADD$2
           CLEAR     ADD$3
           CLEAR     ADD$4
           CLEAR     ADD$5
           CLEAR     ADD$6
           CLEAR     ADD$7
           CLEAR     ADD$8
           CLEAR     ADD$9
           CLEAR     ADD$10
           CLEAR     ADD$RTE
           CLEAR     ADD$RT1
           CLEAR     ADD$RT2
           CLEAR     ADD$RT3
           CLEAR     ADD$RT4
           CLEAR     ADD$RT5
           CLEAR     ADD$RT6
           CLEAR     ADD$RT7
           CLEAR     ADD$RT8
           CLEAR     ADD$RT9
           CLEAR     ADD$RT10
           CLEAR     addqty
           CLEAR     addqty1
           CLEAR     addqty2
           CLEAR     addqty3
           CLEAR     addqty4
           CLEAR     addqty5
           CLEAR     addqty6
           CLEAR     addqty7
           CLEAR     addqty8
           CLEAR     addqty9
           CLEAR     addqty10
           CLEAR     AT1
           CLEAR     AT2
           CLEAR     AT3
           CLEAR     AT4
           CLEAR     AT5
           CLEAR     AT6
           CLEAR     AT7
           CLEAR     AT8
           CLEAR     AT9
           CLEAR     AT10
         MOVE        C0 TO TOTREJ
               move               c0,runchrgtot
               return
.enter with commission percentage in form34
.exit  with calced rebate in rebate
calcrebate
.series of if statements to calc what if any percentage of the savings fee gets rebated
                   MOVE      c0 TO rebate
.begin patch 10.001
.                   IF        (form32 = .20)     ..20% comm
                   IF        (form34 = .20)     ..20% comm
                   move      netsavfee to rebate
                   MULT      ".5" by rebate
                   endif
.                   IF        (form32 = .25)     ..25% comm
                   IF        (form34 = .25)     ..25% comm
                   move      netsavfee to rebate
                   MULT      ".4" by rebate
                   endif
.                   IF        (form32 = .30)     ..30% comm
                   IF        (form34 = .30)     ..30% comm
                   move      netsavfee to rebate
                   MULT      ".3333" by rebate
                   endif
.                   IF        (form32 = .40)     ..40% comm
                   IF        (form34 = .40)     ..40% comm
                   move      netsavfee to rebate
                   MULT      ".2957" by rebate
                   endif
.                   IF        (form32 = .45)     ..45% comm
                   IF        (form34 = .45)     ..45% comm
                   move      netsavfee to rebate
                   MULT      ".2222" by rebate
                   endif
           return
calcrqty
         cmatch    yes to lstmsw
         if        equal
         MOVE      squant TO CALCPER
         move      c0 to per85                  .DLH may95 use order net 
         move      irnetper to per85             .info if avail.
         mult      ".01" by per85               .
         compare   c0 to per85                  .
         if        equal                        .  
         move      ".85" to per85               .
         endif                                  .
         MULT      PER85 BY CALCPER
         move      calcper to qtybildn
         move      squant to chgrqty
         MOVE      CALCPER TO CHGQTY1
         SUB       chgqty1 from chgrqty
         return
         ENDIF
         MOVE      NMRGRQTY TO CALCPER
         move      c0 to per85                  .DLH may95 use order net 
         move      irnetper to per85             .info if avail.
         mult      ".01" by per85               .
         compare   c0 to per85                  .
         if        equal                        .  
         move      ".85" to per85               .
         endif                                  .
         MULT      PER85 BY CALCPER
         move      NMRGRQTY to chgrqty
         MOVE      NMRGNET TO CHGQTY1
         MOVE      CALCPER TO CHGQTY2
           COMPARE   CHGQTY1 TO CHGQTY2
         IF        LESS
         SUB       chgqty1 from chgrqty
         ELSE
         SUB       chgqty2 from chgrqty
         ENDIF
         return
Slct10Per
          move      QTYbild  to cmpt94
         DIVIDE    THOUS BY CMPT94
         compare   c2 to netflag              .net order?
         if        equal                      .yes
         move       net94n to CMPT94
         divide     thous by CMPT94
         endif
         MULTIPLY  price BY CMPT94
         mult      ".10" by cmpt94
         MOVE      CMPT94 TO AMOUNT
         ADD       AMOUNT TO SELECT
         ADD       AMOUNT TO lrinc
         GOTO      SUMAMT
              return
.begin patch 10.01
Slct2Per
          move      QTYbild  to cmpt94
         DIVIDE    THOUS BY CMPT94
         compare   c2 to netflag              .net order?
         if        equal                      .yes
         move       net94n to CMPT94
         divide     thous by CMPT94
         endif
         MULTIPLY  price BY CMPT94
         mult      ".02" by cmpt94
         MOVE      CMPT94 TO AMOUNT
         ADD       AMOUNT TO SELECT
         ADD       AMOUNT TO lrinc
         GOTO      SUMAMT
              return
.end patch 10.01

.begin patch 10.001
Slct5Per
              move            QTYbild  to cmpt94
              DIVIDE          THOUS BY CMPT94
              compare         c2 to netflag              .net order?
                              if        equal                      .yes
                              move       net94n to CMPT94
                              divide     thous by CMPT94
                              endif
              MULTIPLY        price BY CMPT94
              mult            ".05" by cmpt94
              MOVE            CMPT94 TO AMOUNT
              ADD             AMOUNT TO SELECT
              ADD             AMOUNT TO lrinc
              GOTO            SUMAMT
              return
.end patch 10.001
NEXTCHRG
               return
.
SetNetFlag
               PACK      STR2 FROM OSALES10,OSALES
               REP       ZFILL IN STR2
.start patch 10.???
.                                             IF             (str2 = "06" or str2 = "19")
               IF             (str2 = "06" or str2 = "19" or str2 = "27" or str2 = "28")
.end patch 10.???
                                             MOVE           YES TO LSTMSW            *LIST MANAGEMENT.
                                             endif
               move      c1 to netflag               .clear net flag
               move      c0 to n2
               move      irnetper to n2
               compare   c0 to n2                    .net name order?
                                             if        not equal                   .yes
.check date -  pre 10/1/95 do it the old way.
                                             move      "10" to mm
                                             move      "01" to dd
                                             move      "95" to yy
                                             call      cvtjul
                                             move      juldays to str5
                                             move      invdtem to mm
                                             move      invdted to dd
                                             move      invdtey to yy
                                             call      cvtjul
                                             move      str5 to n5
                                             sub       juldays from n5
                                             compare   c0 to n5
                                                            if        less
                                                            move      c2 to netflag             . do it the new way.
                                                            endif
                              
                                                            if        equal
                                                            move      c2 to netflag             . do it the new way.
                                                            endif
.         move      c2 to netflag               .yes
                                             endif
               Return