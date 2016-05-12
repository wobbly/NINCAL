......................................
.nord6DD INCLUSION
.NIN supplimental Cancelled LCR FILE DEFINITION
.
.FILE NAME:  NInord6
.REC LENGTH: 26 FIXED
.INDEX KEY:  (1) 1-6  (LR#)
.
. Patch 1.1 Changed IP Address of File Manager DMB 18JUN05
.
.nord6NAME       INIT    "NINORD6"
.>Patch 1.1 Begin
.nord6NAME       INIT    "NINORD6.ISI|20.20.30.103:502"
nord6NAME       INIT    "NINORD6.ISI|NINS1:502"
.>Patch 1.1 End
nord6FILE       IFILE   KEYLEN=6,FIXED=27
nord6FLD        DIM     6
nord6FLAG       FORM    1
nord6PATH       FORM    1
nord6LOCK       FORM    1
.
.
ORD6VARS        LIST
nord6LR         DIM     6       001-006 ORDER LR
nord6STAT       DIM     2       007-008 APPROVAL STATUS
.
.0 OR " " = Cancelled			4 = Cancelled
.1 = Mail Date over 6 months old	5 = Cancelled
.2 = Cancelled				6 = Cancelled
.3 = Cancelled				7 = Cancelled
NORD6PDTE       DIM     8       009-016 DATE ENTERED CCYYMMDD
.note actual order date changes to date approved (if approved)
nord6STA1       DIM     1       017-017 OLD OSTAT - "l", "p"
nord6STA2       DIM     2       018-019 CLEARANCE STATUS
.see notes from sales
.Following field was designed to hold date of last change.  Now holds date the record was automatically Cancelled,
.if applicable.
nord6CDTE       DIM     8       020-027 DATE ENTERED CCYYMMDD
                LISTEND
