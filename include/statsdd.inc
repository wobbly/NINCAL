..............................................................................
.
. statsDD INCLUSION
. Stats FILE DEFINITION
.
. FILE NAME :  NINSTATs
. REC LENGTH:     FIXED
. INDEX KEY :
.
.Patch 01/25/2005 - ASH Converted Mailer/Broker fields
.Patch 04/17/2002 - ASH Added Select Universe field
.Patch 02/22/2002 - ASH Added Busy Byte
.Patch 02/20/2002 - ASH Added Aamdex for History Screen in Program 1 
..............................................................................
.
StatFLst Filelist
.Start Patch 01/25/2005 - Replaced logic
.statFILE IFILE     KEYLEN=29,FIXED=501,UNCOMP,Name="NINSTATS.ISI|20.20.30.103:502"
.statFLE2 IFILE     KEYLEN=6,FIXED=501,UNCOMP,Name="NINSTAT2.ISI|20.20.30.103:502"
..Start Patch 02/20/2002
.statFLE3 AFILE     FIXED=501,UNCOMP,Name="NINSTATS.AAM|20.20.30.103:502"
..End Patch 02/20/2002
....................................
.Patch 06/18/2005 Begin
.statFILE IFILE     KEYLEN=31,FIXED=501,UNCOMP,Name="NINSTATS.ISI|20.20.30.103:502"
statFILE IFILE     KEYLEN=31,FIXED=501,UNCOMP,Name="NINSTATS.ISI|NINS1:502"
.statFLE2 IFILE     KEYLEN=6,FIXED=501,UNCOMP,Name="NINSTAT2.ISI|20.20.30.103:502"
statFLE2 IFILE     KEYLEN=6,FIXED=501,UNCOMP,Name="NINSTAT2.ISI|NINS1:502"
.statFLE3 AFILE     FIXED=501,UNCOMP,Name="NINSTATS.AAM|20.20.30.103:502"
statFLE3 AFILE     FIXED=501,UNCOMP,Name="NINSTATS.AAM|NINS1:502"
.End Patch 01/25/2005 - Replaced logic
.statFILE IFILE     KEYLEN=29,FIXED=501,UNCOMP,Name="NINSTATS.ISI"
.statFLE2 IFILE     KEYLEN=6,FIXED=501,UNCOMP,Name="NINSTAT2.ISI"
.statFILE IFILE     KEYLEN=29,FIXED=501,UNCOMP,Name="NINSTATS.ISI"
.statFLE2 IFILE     KEYLEN=6,FIXED=501,UNCOMP,Name="NINSTAT2.ISI"
         Filelistend
statNAME INIT      "NINSTATS"
statNME2 INIT      "NINSTAT2"
.PROJECTION FILE
Stat2FLst Filelist
.stat2FILE IFILE     KEYLEN=9,FIXED=501,UNCOMP,Name="NIN2STATS.ISI|20.20.30.103:502"
stat2FILE IFILE     KEYLEN=9,FIXED=501,UNCOMP,Name="NIN2STATS.ISI|NINS1:502"
.stat2FLE2 AFILE     FIXED=501,UNCOMP,Name="NIN2STATS.AAM|20.20.30.103:502"
stat2FLE2 AFILE     FIXED=501,UNCOMP,Name="NIN2STATS.AAM|NINS1:502"
.stat2FILE IFILE     KEYLEN=9,FIXED=501,UNCOMP,Name="NIN2STATS.ISI"
.stat2FLE2 AFILE     FIXED=501,UNCOMP,Name="NIN2STATS.AAM"
         Filelistend
.Patch 06/18/2005 End         
stat2NAME INIT      "NIN2STATS"
.stat2NME2 INIT      "NIN2STAT2"

.PROJECTION FILE END
.Start Patch 01/25/2005 - Replaced logic
.statFLD   dim      29               ninmlr+source       1-4,117-141
statFLD   dim      31               ninmlr+source       1-6,119-143
.End Patch 01/25/2005 - Replaced logic
statfld2  dim      6                nin LR 254-259
.Start Patch 02/20/2002
.Start Patch 01/25/2005 - Replaced logic
.statfld4  dim	   7		    statmlr (AAMKEY)
statfld4  dim	   9		    statmlr (AAMKEY)
.End Patch 01/25/2005 - Replaced logic
statfld5  dim	   28		    statpckcde (AAMKEY)
.End Patch 02/20/2002
STAT2FLD  DIM      9		    NIN LR + STATNUM
STAT2FLD2 DIM      9                NIN LR (AAMKEY)
STAT2FLD3 DIM      4                STATLOL (AAMKEY)
.Start Patch 02/20/2002
.Start Patch 01/25/2005 - Replaced logic
.stat2fld4 dim	   7		    NINMLR (AAMKEY)	.NOT YET IMPLEMENTED
stat2fld4 dim	   9		    NINMLR (AAMKEY)	.NOT YET IMPLEMENTED
.End Patch 01/25/2005 - Replaced logic
.End Patch 02/20/2002

statFLAG  FORM      1
statFLAg2  FORM      1
stat2flag FORM      1
statpath  form      1
StatLOCK  FORM      1         0 & 1=FILEPI, 2=REcord lock, 3=no lock
.
statvars  list
.Start Patch 01/25/2005 - Replaced logic
.statmlr   dim       4          1-4     NINCAL's mlr number for client
statmlr   dim       6          1-6     NINCAL's mlr number for client
.End Patch 01/25/2005 - Replaced logic
statcampn dim       30         7-36    Campaign description
statmdate dim       8         37-44     mm,dd,cc,yy   maildate 
statwkso  form      6         45-50     weeks out
statpdate dim       8         51-58     mail processed thru
statpanel dim       60        59-118    package description
statsrce  dim       25       119-143    mlr source code
statldes  dim       30       144-173    Mlr's list desc
statsel   dim       30       174-203    list select
Statlist  dim       6        204-209    ninca's list number
stattype  dim       1        210-210    list type Continuation or Test, or Rent, Exchange, Split
statmqty  form      8        211-218    quantity mailed
statresp  form      7        219-225    number of responses
statrev   form      9        226-234	gross revenue
statlcpm  form      4.2      235-241	list cost per m
statImcst form      6.2      242-250	In mail cost per M = all costs.
.Start Patch 01/25/2005
.Added padding for future LR increase
statlr2	  dim	    3        251-253
.End Patch 01/25/2005
statlr    dim       6        254-259	nin List Rental number
.Following field currently only used by TNC.  Field was added in 1999 and is the TNC List Code.  They currently
.only use part of the designated space - 4 bytes	ASH 27NOV2001
statkycd  dim       25       260-284	Mailers key code
Statpack  form      5.2      285-292	total package cost
Statpckm  form      5.2      293-300	total package cost per thous
statLVal  FORM      4.2      301-307	Lifetime Value
statpckcde DIM      25       308-332	.changged 19June2001 to facilitate tracking TNC packages.
statpcknum dim	    6	     333-338	.Our Package Number
statLOL    dim	    1	     339-339    .0=LR Record, 1=LOL Record
statgift  form      4.2      340-346	.Average Gift
statresp2 form	    3.2	     347-352	.Response Rate
statnetreq form     3.2      353-358	.Net Requested
statnetrec form     3.2      359-364	.Net Received
statexbase form     6.2      365-373	.Exchange Base
statrbase  form     6.2      374-382	.Rent Base
statrun    form     6.2      383-391	.Running Charge
statselfee form     6.2      392-400	.Select Fee
statship   form     6.2      401-409	.Ship/Tape
statpckprem form    6.2	     410-418    .Package Premium Price
statrecqty form	    9	     419-427	.Recommended Qty
statavgnet form	    3.2      428-433	.Net Qty
statnum	   dim      3        434-436
.START PATCH 03/22/2002
statcode   dim	    1	     437-437	.Busy byte
.END PATCH 03/22/2002
.START PATCH 03/22/2002
statseluni form	    9	     438-446	.Select Universe
.END PATCH 03/22/2002
statfiller dim	    55	     447-501    .Filler for future fields
         listend

.Following Variables used for Calculation of derived fields - NOT INCLUDED IN STATVARS!!!!!!
CALCSTATEXTOT	FORM	6.2
CALCSTATRTOT	FORM	6.2
CALCSTATNETREQ	FORM	3.2
CALCSTATCOSTMEM	FORM	6.2
.
CALCSTATNETP	FORM	9.4
CALCSTATNETNAME	FORM	9.4
CALCSTATTOTCOST FORM	9.4
CALCSTATLSTCOST	FORM	9.4
CALCSTATPROCOST	FORM	9.4

.END OF INCLUDE
.
..............................................................................
