..............................................................................
.
. nord4D3D INCLUSION
. NIN supplimental Pending order Approval/Clearance ORDER FILE DEFINITION
.
. FILE NAME : NInord4xxxxx???
. REC LENGTH: 26 FIXED
. INDEX KEY : (1)  1-6       (LR#)
.
. Release 1.3 DLH add "Waiting for Pricing Approval"
. PATCH 1.2 Changed IP of File Manager  DMB 18JUN05
. Patch 1.1 Added file to File Manager  ASH 07AUG01
.
.START PATCH 1.1 REPLACED LOGIC
.nord4NAME Init       "NInord4"
.Patch 1.2 Begin
.nord4NAME Init       "NInord4.ISI|20.20.30.103:502"
nord4NAME Init       "NInord4.ISI|NINS1:502"
.Patch 1.2 End
.END PATCH 1.1 REPLACED LOGIC
nord4FILE IFILE     KEYLEN=6,FIXED=26
nord4FLD  DIM       6
nord4FLAG FORM      1
nord4PATH FORM      1
nord4lock form      1
.
.
ORd4VARS  LIST
nord4lr   DIM       6      001-006      ORDER LR
nord4STAT DIM       2      007-008      Pending STATUS:
. 0 OR " " = pending              5 = at serice bereau
. 1 = waiting for guar            6 = Denied
. 2 = waiting for brk call        7 = cancelled
. 3 = waiting for prepayment      8 = approved
. 4 = waiting for revision        9 = received
.10 = Accounting Hold            11 = 2nd Request
.12 = Revised Request            13 = Waiting for List Usage Agreement
.14 = Waiting for Sample         15 = Waiting for Counts
.begin patch 1.3
.16 = Waiting Pricing Approval
.end patch 1.3
nordpdte DIM       8      009-016     DATE entered ccyymmdd
.note actual order date changes to date approved (if approved)
nord4STA2 DIM       2     017-018      Clearance STATUS:
.see notes from sales
.
nord4Cdte DIM       8      019-026     DATE entered ccyymmdd
.note actual order date changes to date approved (if approved)


         listend
* .......................................................................

