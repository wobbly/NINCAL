.need to add code to handle different codes & list owners in ndat3dd. 22aug95
.revised cd01-cd03 broker commission - make sure no commission on exchange
.portion of a split order 05june95 DLH
.added date check for change in net billing - 3/1/95 started billing gross
. on net name order selects.
.Revised 17mar95 finalize net
.REVISED 17JAN95 BUB WITH NEG ADDITIONAL CHARGES
.revised 12/30/94 fixed bub with rounding.
.revised 11/94 for splits.
.ADDED SUBPPSW TO ELIMINATE THE NEED FOR COMPUTEP.INC
.ADDED tdmcflag TO ELIMINATE THE NEED FOR COMPUTE.tri
.Need the above to flags set before entry 
.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.(note: ninv002l.dbs still has hardwired copy)
.%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.
COMPUTE  
         MOVE      C0 TO SELECT
         MOVE      C0 TO SHIP
         CLEAR     PREPAYSW
         CLEAR     PPSW
         MOVE      C0 TO FORMAP2
         MOVE      C0 TO SAVEAP
         MOVE      C0 TO TAXES
         MOVE      C0 TO BRKCOM
         MOVE      C0 TO CMPCOM
         MOVE      C0 TO LRINC
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
         MOVE      C0 TO FORM72
         MOVE      C0 TO FORM72x
         MOVE      C0 TO FORM73
         MOVE      C0 TO FORM74
         MOVE      C0 TO FORM74x
         MOVE      C0 TO FORM32
         move      c1 to netflag               .clear net flag
         move      c0 to n2
         move      onetper to n2
         compare   c0 to n2                    .net name order?
         if        not equal                   .yes
.check date - pre 10/1/95 do it the old way.
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
.
         MOVE      QTYSHP TO FORM74
         COMPARE   C0 TO FORM74
         GOTO      FNINCD IF EQUAL
         DIVIDE    THOUS INTO FORM74
         MOVE      FORM74 TO AMOUNT              QUANTITY BILLED
.
.         compare   c1 to netflag
.         if        equal            .not a net
.         MOVE      QTYSHP TO FORM74
.         COMPARE   C0 TO FORM74
.         GOTO      FNINCD IF EQUAL
.         DIVIDE    THOUS INTO FORM74
.         MOVE      FORM74 TO AMOUNT              QUANTITY BILLED
.         else
.lets get net goodies
.1st is there a minimum & do we meet it?
         compare   c0 to onetmin
         goto      nets if equal           .no min
         goto      nets if less
         goto      chkmin
nets     
         cmatch    "F" to onetfm
         if        equal
         move      c3 to netflag
         goto      flat
         endif      
         move      c0 to net74
         move      c0 to form72 
         move      NMRGrQTY to net74
         move      onetper to form72
         div       hund into form72
         mult      form72 by net74
.         compare   net74 to nmrgrqty      .which is bigge3 net or actual
         compare   net74 to nmrgnet      .which is bigge3 net or actual
         if        less        .use net
         move      net74 to n8              .force round to whole # of names
         move      n8 to net74              .ditto         
         div       thous into net74
         goto      spltqty                    .use net
         endif
         move      nmrgNET to net74      .we got more names
         div       thous into net74
         goto      spltqty
.         else                      .there is a min
.         move      NMRGrQTY to n8
.         move      nmrgrqty  to net74       
.         move      onetper to form72
.         div       hund into form72
.         mult      form72 by net74
.         move      net74 to n8
.           compare   onetmin to n8     
.           if        less           .we don't qualify
.           move      c1 to netflag
.           goto      spltqty
.           else
.           goto      nets           .we pass
.           endif
.           endif
.           endif
.
chkmin
         move      nmrgrqty  to net74
         move      onetper to form72
         div       hund into form72
         mult      form72 by net74
         move      net74 to n8
           compare   onetmin to n8     
           if        less           .we don't qualify
           move      c1 to netflag
           goto      spltqty
           else
           goto      nets           .we pass
           endif

.
spltqty
         MOVE      iRexqty TO FORM74x              .split qty
         DIVIDE    THOUS INTO FORM74x
         MOVE      FORM74x TO AMOUNTX            .split QUANTITY BILLED
.
         MOVE      C0 TO FORM72
         MOVE      PPM TO FORM72
         COMPARE   C0 TO FORM72
         GOTO      FNINCD IF EQUAL
         DIVIDE    HUND INTO FORM72
         MOVE      FORM72 TO PRICE               PRICE PER M
.
         MOVE      C0 TO FORM72x                  exchange
         MOVE      iexppm TO FORM72x              portion
         DIVIDE    HUND INTO FORM72x              of the
         MOVE      FORM72x TO PRICEX              split PRICE PER M
.
         branch    netflag of grosbase,netbase,netbase
.
flat          
         move      c0 to net74
         move      c0 to form74
         move      QTYSHP  to net74
         move      net74 to n8              .force round to whole # of names
         move      n8 to net74              .ditto         
         MOVE      N8 TO FORM74
         div       thous into net74        added 10/13/95
         move      net74 to n8
         goto       spltqty

grosbase MULT      PRICE BY FORM74
         MOVE      FORM74 TO GROSS               GROSS BILLING.
         MOVE      FORM74 TO FORMAR                  ACCOUNTS RECEIVABLE.
         MOVE      FORM74 TO SVEACR              WORKING AR.
         goto      spltbase
.
netbase 
         MULT      PRICE BY NEt74
         MOVE      net74 TO GROSS               GROSS BILLING.
         MOVE      net74 TO FORMAR                  ACCOUNTS RECEIVABLE.
         MOVE      net74 TO SVEACR              WORKING AR.
         goto      spltbase
.
spltbase MULT      PRICEx BY FORM74x             .split portion
         add       FORM74x TO GROSS               GROSS BILLING.
         add       FORM74x TO SVEACR              WORKING AR.
.
FNINCD   MOVE      C0 TO N1                      CLEAR BRANCH VAR.
         MOVE      PAYCODE TO N1                     PAYABLE CODE
         BRANCH    N1 TO CHKCHRGS,FNINCD2,CHKCHRGS,FNINCD4
FNINCD2  MOVE      C0 TO FORM32
         MOVE      COMMPCT TO FORM32
         COMPARE   c0 TO FORM32                    C0 COMMISSION?
         GOTO      CHKCHRGS IF EQUAL
         DIVIDE    HUND INTO FORM32
         MOVE      FORMAR TO LRINC
         MULT      FORM32 BY LRINC               LR INCOME.
         MOVE      LRINC TO CMPCOM
         GOTO      CHKCHRGS
FNINCD4  MOVE      FORMAR TO LRINC
.
CHKCHRGS MOVE      c0 TO INDEX
NEXTCHRG ADD       c1 TO INDEX
         CLEAR     PREPAYSW
         COMPARE   "11" TO INDEX
         GOTO      ENDCHRGS IF EQUAL
         LOAD      STR14 FROM INDEX OF ADDCHG1,ADDCHG2,ADDCHG3,ADDCHG4,ADDCHG5:
                   ADDCHG6,ADDCHG7,ADDCHG8,ADDCHG9,ADDCHG10
         UNPACK    STR14 INTO ADDCODE,STR12
         UNPACK    STR12 TO STR7,AEXTCD,STR3,STR1
         MATCH     "  " TO ADDCODE
         GOTO      ENDCHRGS IF EQUAL
.
         MOVE      ADDCODE TO ADDKEY
.
.
         MOVE      C0 TO ACAMT
         MOVE      C0 TO ACCMPR
         MOVE      C0 TO ANINCD
.
.         MOVE      STR7 TO FORM72
         move       c0 to cvtfld        \
         move       str7 to cvtfld       \ 
         call      cvt                     >dh 17jan95
         move      cvtfld to form72      /
         DIVIDE    HUND BY FORM72      /
         MOVE      FORM72 TO ACAMT
.
         MOVE      STR3 TO FORM32
         DIVIDE    HUND BY FORM32      
         MOVE      C0 TO ACCMPR
         MOVE      FORM32 TO ACCMPR
.
         MOVE      C0 TO ANINCD             CLEAR BRANCH
         MOVE      STR1 TO ANINCD
.
         MOVE      ADDCODE TO CODENUM
         BRANCH    CODENUM TO CD01:      01    BROKER COMMISION
                              CD02:      02    BROKER COMMISSION
                              CD03:      03    Broker commssion   5/94
                              CD04:      04    TAXES
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
                              SHIPPERM:  48    SHIPPING
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
                              SLCTFLAT:  81    SELECTIONS
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
                              SLCTFLAT:  98    SELECTIONS
                              CREDIT     99    CREDIT
         GOTO      CD00
...............................................................................
.
SLCTPERM 
         MOVE      QTYSHP TO FORM74
         DIVIDE    THOUS BY FORM74
         MULTIPLY  ACAMT BY FORM74
         MOVE      FORM74 TO AMOUNT
         ADD       AMOUNT TO SELECT
.lets handle a split
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
         move      c0 to form74x
         MOVE      iRexqty TO FORM74x
         DIVIDE    THOUS BY FORM74x
         MULTIPLY  ACAMT BY FORM74x
         add       FORM74x TO AMOUNT
         ADD       form74x TO SELECT
         endif
         GOTO      SUMAMT
................................................................................
SLCTrunc 
         MOVE      QTYSHP TO FORM74
         DIVIDE    THOUS BY FORM74
         compare   c2 to netflag              .net order?
         if        equal                      .yes
.lets get the names we owe on.          
         move       net74 from form74
         endif
         compare   c3 to netflag              .net order?
         if        equal                      .yes
         MOVE      QTYSHP TO FORM74
         DIVIDE    THOUS BY FORM74
         endif
         MULTIPLY  ACAMT BY FORM74
         MOVE      FORM74 TO AMOUNT
         ADD       AMOUNT TO SELECT
         GOTO      SUMAMT
...............................................................................
.
SHIPPERM MOVE      QTYSHP TO FORM74
         DIVIDE    THOUS BY FORM74
         MULTIPLY  ACAMT BY FORM74
         MOVE      FORM74 TO AMOUNT
         ADD       AMOUNT TO SHIP
.lets handle a split
         MOVE      iRexqty TO FORM74x
         DIVIDE    THOUS BY FORM74x
         MULTIPLY  ACAMT BY FORM74x
         add       FORM74x TO AMOUNT
         ADD       form74x TO ship
.
         GOTO      SUMAMT
...............................................................................
SLCTFLAT MOVE      ACAMT TO AMOUNT
         ADD       AMOUNT TO SELECT
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
         MULT      SEQ BY AMOUNT
         GOTO      SUMAMT
.
.
. ACAMT IS NOT IN CORRECT FORM, IT SHOULD HAVE 3 DEC PLACES.
. USE ITS ALPHA EQUIVLENT FROM THE UNPACK ABOVE.
.
CD00     MATCH     "0331" TO MLRN
         GOTO      CD00CONT IF NOT EQUAL
         PACK      STR4 WITH INVDTEY,INVDTEM
         MATCH     "8601" TO STR4
         GOTO      NEXTCHRG IF EQUAL
CD00CONT MOVE      STR7 TO FORM73
         DIVIDE    THOUS BY FORM73
         MULTIPLY  PRICE BY FORM73
         MULTIPLY  ACCMPR BY FORM73
         MULTIPLY  "-1" BY FORM73
         MOVE      FORM73 TO AMOUNT
         ADD       AMOUNT TO FORMAR
         ADD       AMOUNT TO LRINC
         GOTO      ADDAMT
.
CD01     MOVE      SVEACR TO AMOUNT
         sub       form74x from amount             .don't give comm on exchange
.                                                  .portion dlh 05jun95
         MULTIPLY  ".10" BY AMOUNT 
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC
         GOTO      SUMAMT
.
CD02     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".20" BY AMOUNT
         sub       form74x from amount             .don't give comm on exchange
.                                                  .portion dlh 05jun95
         MULTIPLY  ".05" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC
         GOTO      SUMAMT
.
CD03     MOVE      SVEACR TO AMOUNT
         sub       form74x from amount             .don't give comm on exchange
.                                                  .portion dlh 05jun95
         MULTIPLY  ".20" BY AMOUNT
         ADD       AMOUNT TO BRKCOM
         move      brkcom to amount
         MULTIPLY  "-1" BY AMOUNT
         ADD       AMOUNT TO LRINC       
         GOTO      SUMAMT
.         MULTIPLY  ".0525" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
.
.CD04     MOVE      SVEACR TO AMOUNT
.         MULTIPLY  ".0400" BY AMOUNT
.         ADD       AMOUNT TO TAXES
.         GOTO      SUMAMT
.
CD04     MOVE      SVEACR TO AMOUNT
         MOVE      AMOUNT TO CANUSE        *CANADIAN USE TAX
         SUB       LRINC FROM CANUSE
         MULT      ".10" BY CANUSE
         MULT      "-1" BY CANUSE
         MOVE      CANUSE TO AMOUNT
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
         GOTO      NEXTCHRG
.         GOTO      SUMAMT
.
CD97     ADD       ACAMT TO BRKCOM
         MOVE      ACAMT TO AMOUNT
         MULTIPLY  "-1" BY AMOUNT
         GOTO      SUMAMT
.
SUMAMT   MOVE      AMOUNT TO SVECOM
         COMPARE   "0" TO ACCMPR
         GOTO      ADDAMT IF EQUAL
         MULTIPLY  ACCMPR BY SVECOM
         ADD       SVECOM TO LRINC
	 MOVE	   C0 TO CALCRUN                 .tdmc stuff
	 MOVE      SVECOM TO CALCRUN             .tdmc stuff
         MOVE      AMOUNT TO FORM72
         SUB       SVECOM FROM FORM72
         MOVE      FORM72 TO SVECOM
******TDMC GOODIES.
         branch    tdmcflag to addamt,dotdmc          .checking tdmc this time ?
.                  
         goto      addamt                             .if no goto addamt
dotdmc	 COMPARE   C0 TO CALCRUN
	 GOTO	   ADDAMT IF EQUAL       *NOTHING TO CALC FOR TDMC.
	 MOVE      OLNUM TO NDAT3FLD     
	 CALL      NDAT3KEY
	 GOTO      ADDAMT IF OVER        *NOT IN TDMC BILLING FILE, SKIP.
	 CMATCH    B1 TO NDAT3cde
	 GOTO      ADDAMT IF EQUAL       *not TDMC, SKIP.
	 CMATCH    B1 TO NDATTDMC
	 GOTO      ADDAMT IF EQUAL       *WE DON'T BILL, SKIP.
	 MOVE      STAR TO RUNFLAG
	 ADD       C1 TO RUNRCNT
	 COMPARE   "3.90" TO ACAMT       *RUNNING CHARGE
	 GOTO      TDMCRUN IF EQUAL
	 ADD        CALCRUN TO RUNRAR
	 ADD        CALCRUN TO RUNRFLAT
	 GOTO	    ADDAMT
TDMCRUN  MOVE	    C0 TO N1
	 MOVE       OFOCODE TO N1
	 BRANCH     N1 OF ADDAMT,LABEL,LABEL,LABEL,LABEL,ADDAMT,TAPE,TAPE,LABEL
TAPE     MOVE       CALCRUN TO FORM82
	 ADD        CALCRUN TO RUNRAR
	 MULT       ".40" BY FORM82
	 ADD         FORM82 TO RUNRLR
	 SUB         FORM82 FROM CALCRUN
	 ADD        CALCRUN TO RUNRPASS
	 GOTO       ADDAMT
LABEL     MOVE       CALCRUN TO FORM82
	 ADD	     CALCRUN TO RUNRAR
	 MULT       ".10" BY FORM82
	 ADD         FORM82 TO RUNRLR
	 SUB         FORM82 FROM CALCRUN
	 ADD        CALCRUN TO RUNRPASS
	 GOTO       ADDAMT
********END TDMC GOODIES	 
******
.
ADDAMT   BRANCH    ANINCD TO ANINCD1,ANINCD2
.ANINCD0 - ADD. CHG CODE = ' ', ADD. CHG. TO AR.
ANINCD0  ADD       AMOUNT TO FORMAR
         GOTO      NEXTCHRG
.ANINCD1 - ADD. CHG. CODE - '1', ADD. CHG. TO A/P
ANINCD1  CMATCH    "2" TO PAYCODE
         GOTO      ADDCOM IF EQUAL
         ADD       AMOUNT TO AP
         GOTO      NEXTCHRG
ADDCOM   ADD       SVECOM TO AP
         GOTO      NEXTCHRG
ANINCD2  ADD       AMOUNT TO FORMAR
         GOTO      ANINCD1
.
.
ENDCHRGS MOVE      C0 TO N1              CLEAR BRANCH VAR.
         MOVE      PAYCODE TO N1
         BRANCH    N1 TO PAYCD1,PAYCD2,PAYCD3
.PAYCD0 - PAYCODE = '0' OR ' ', SO GROSS=A/P
PAYCD0   MOVE      FORMAR TO AP
         SUBTRACT  LRINC FROM AP
         SUBTRACT  TAXES FROM AP
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         add       form74x to lrinc
         add       form74x to formar
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
         add       form74x to lrinc
         add       form74x to formar
         RETURN
.PAYCD2 - PAYCODE = '2', 2- A/P'S,
.         A/P'S AS DEFINED IN INVOICE RECORD.
PAYCD2   MOVE      C0 TO FORM72
         MOVE      AP1 TO FORM72
         DIV       HUND INTO FORM72
         MOVE      FORM72 TO SAVEAP
         MOVE      SAVEAP TO AP
         MOVE      FORMAR TO FORM72
         SUBTRACT  AP FROM FORM72
         MOVE      AP2 TO FORMAP2
         DIV       HUND INTO FORMAP2
         SUB       FORMAP2 FROM FORM72
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         MOVE      FORM72 TO LRINC
         SUB       TAXES FROM LRINC
         SUB       POST FROM LRINC
         add       form74x to lrinc
         add       form74x to formar
         RETURN
PAYCD3   MOVE      C0 TO FORM72
         MOVE      AP1 TO FORM72
         DIVIDE    HUND INTO FORM72
         MOVE      C0 TO AP
         MOVE      FORM72 TO AP
         MOVE      FORMAR TO LRINC
         SUBTRACT  AP FROM LRINC
.         MOVE      AR TO CMPCOM
.        SUBTRACT  FORM72 FROM CMPCOM
         SUBTRACT  TAXES FROM LRINC
         SUBTRACT  POST FROM LRINC
         CMATCH    YES TO SUBPPSW
         IF        EQUAL
         SUBTRACT  PREPAY FROM FORMAR
         ENDIF
         add       form74x to lrinc
         add       form74x to formar
         RETURN

