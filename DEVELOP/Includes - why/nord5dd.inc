......................................
.nord5DD INCLUSION
.NIN supplimental Pending order Approval/Clearance ORDER FILE DEFINITION
.
.FILE NAME:  NInord5
.REC LENGTH: 26 FIXED
.INDEX KEY:  (1) 1-6  (LR#)
.
. Patch 1.2 Changed IP Address of File Manager DMB 18JUN05
. Patch 1.1 Added file to File Manager  ASH 07AUG01
.
.START PATCH 1.1 REPLACED LOGIC
.nord5NAME       INIT    "NInord5"
.nord5NAME       INIT    "NINORD5.ISI|20.20.30.103:502"
nord5NAME       INIT    "NINORD5.ISI|NINS1:502"
.END PATCH 1.1 REPLACED LOGIC
nord5FILE       IFILE   KEYLEN=6,FIXED=26
nord5FLD        DIM     6
nord5FLAG       FORM    1
nord5PATH       FORM    1
nord5LOCK       FORM    1
.
.
ORD5VARS        LIST
nord5LR         DIM     6       001-006 ORDER LR
nord5STAT       DIM     2       007-008 APPROVAL STATUS
.
.0 OR " " =             4 = Approved
.1 = 1rst Request       5 = Cancelled
.2 = 2nd Request        6 = Pending
.3 = Revised Request    7 = Denied
.8 = Pending Internal
.9 = Tentative Approval
NORD5PDTE       DIM     8       009-016 DATE ENTERED CCYYMMDD
.note actual order date changes to date approved (if approved)
nord5STA2       DIM     2       017-018 CLEARANCE STATUS
.see notes from sales
nord5CDTE       DIM     8       019-026 DATE ENTERED CCYYMMDD
.note actual order date chages to date apporved (if approved)
                LISTEND
