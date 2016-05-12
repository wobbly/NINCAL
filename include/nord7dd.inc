......................................
.nord7DD INCLUSION
.NIN supplimental Order Receipt Acknowledgment
.
.FILE NAME:  NInord7
.REC LENGTH: 2000 FIXED
.INDEX KEY:  (1) 1-6  (LR#)
. Patch 1.1 Changed IP Address of File Manager DMB 18JUN05
.
.
.nord7NAME       INIT    "NInord7"
.>Patch 1.1 Begin
.nord7NAME       INIT    "NInord7.ISI|20.20.30.103:502"
nord7NAME       INIT    "NInord7.ISI|NINS1:502"
.>Patch 1.1 End
nord7FILE       IFILE   KEYLEN=6,FIXED=2000
nord7FLD        DIM     6
nord7FLAG       FORM    1
nord7PATH       FORM    1
nord7LOCK       FORM    1
.
.
ORD7VARS        LIST
nord7LR	DIM     	6       001-006  ORDER LR
Nord7TDate	Dim	17      007-023  transmission date
nord7Lname	dim	255     024-278  List Name
nord7MName	Dim 	255     279-533  Mailer name	
nord7Qshp	dim	8       534-541  estimated
nord7Via	dim	50      542-592  
nord7Sdate	dim	17      593-609  estimated
nord7AirB	Dim	50      610-659
nord7Media	Dim	50      660-709
nord7Ser	Dim	50      710-759 
nord7Chrg	dim	8       760-767
nord7Mdate	dim	17      768-784   maildate
nord7SEl	dim	255     785-1039  
nord7Oqty	dim	8      1040-1047 order qty 
nord7PO	dim	50     1048-1097
nord7ODate	dim	17     1098-1114
nord7cnt	dim	255    1115-1369
nord7Phone	dim	255    1370-1624
nord7Email	dim	255    1625-1879
Nord7Fill	dim	121    1880-2000
                LISTEND
