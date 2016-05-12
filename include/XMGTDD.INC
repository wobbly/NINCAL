..............................................................................
.
. xmgtDD INCLUSION
.
. FILE NAME : exchmgmt
. REC LENGTH:  FIXED
. INDEX KEY : 1-6 (lr#)
.patch 1.2
.2015 June Minimum and  room added. DLH
.New rate 01JUl04 - JD  PATCH # 1.1
.Expanded 03Sep99 - ASH increase Qty:  PATCH # 1.0
.created  13Feb98
..............................................................................
.
.START PATCH #1.0 -INCREASE VAR
.xmgtFILE IFILE     KEYLEN=6,fixed=49
.xmgtFILE IFILE     KEYLEN=6,fixed=51,NoDUPES
.patch 1.2
.xmgtFILE IFILE     KEYLEN=6,fixed=51,NoDUPES
xmgtFILE IFILE     KEYLEN=6,fixed=100,NoDUPES
.end patch 1.2
.END PATCH #1.0 -INCREASE VAR
xmgtNAME INIT      "EXCHMGNT "
xmgtfld  DIM       6
xmgtFLAG FORM      1
.
xmgtvars list
xmgtlr   dim       6           1-006   Order number Key Reqired
.START PATCH #1.0 -INCREASE VAR
.xmgtQty  form      7           7-013   Order qty
xmgtQty  form      9          07-015   Order qty or shipped qty
.END PATCH #1.0 -INCREASE VAR
xmgtDate dim       8          16-023   Date entered CCYYMMDD
xmgtOrd  dim       6          24-029   Dummy lr number
xmgtInv  dim       6          30-035   Invoice #
xmgtRate form      3.2        36-041   Rate charged usually $4 or $10  was $2 or $10.  patch 1.1
xmgtown  dim       4         042-045
xmgtlist dim       6         046-051
XmgtMin  form      3.2       052-057
Xmgtfill dim       53        058-100
         listend
...............................................................................
.END OF INCLUDE
.
..............................................................................

