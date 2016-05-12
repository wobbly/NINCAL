;----------------------------------------------------------------
;
;   PromoLstio.inc -
;
;
;   Author: David L Herrick
;----------------------------------------------------------------

..............................................................................
.
. PromoIO INCLUSION
.
..............................................................................
.
. ENTRY POINT : PromoSEQ
. REQUIRED    :
. RETURNED    : SCHEDULE RECORD
. DESCRIPTION : SEQUENTIAL FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
PromoSEQ    BRANCH    PromoFLAG TO Promo4
         CALL      PromoOPEN
Promo4    FILEPI    1;PromoFile
         READ      PromoFile,SEQ;Promovars

         RETURN
..............................................................................
.
. ENTRY POINT : PromoWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION :
.
PromoWRT  BRANCH    PromoFLAG TO Promo5
         CALL      PromoOPEN
Promo5    FILEPI    1;PromoFile
         WRITE     PromoFile,seq;Promovars

         RETURN
...............................................................................
.
. ENTRY POINT : PromoOPEN
. REQUIRED    : 'PromoFLAG'
. RETURNED    : 'PromoFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN  FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
PromoOPEN TRAP      PromoGONE IF IO
         OPEN      PromoFile,"\\nins1\e\data\PromoList.dat|NINS1:502"  
         TRAPCLR   IO
         MOVE      C1 TO PromoFLAG
         RETURN
.
PromoGONE MOVE      PromoNAME1 TO FILENAME
         CALL      FILEGONE
.
..............................................................................

