..............................................................................
.
. NWFDD INCLUSION
. NWF Stats FILE DEFINITION
.
. FILE NAME : NWFSTATs
. REC LENGTH: 478 FIXED
. INDEX KEY : 
..............................................................................
.
NWFFILE IFILE     KEYLEN=50,FIXED=478,UNCOMP
NWFFLE2 IFILE     KEYLEN=6,FIXED=478,UNCOMP
NWFNAME INIT      "NWFSTATS"
NWFNME2 INIT      "NWFSTAT1"
NWFFLD   dim      50
nwffld2  dim      6
NWFFLAG  FORM      1
NWFFLAg2  FORM      1
nwfpath  form      1
.
NWFvars  list
nwfcampn dim       30         1-30    Campaign
nwfmdate dim       8         31-38     mm,dd,cc,yy   maildate 
nwfudate dim       8         39-46     Last update of stats
nwfwkso  form      6         47-52     weeks out
nwfpdate dim       8         53-60     mail processed thru
nwfpanel dim       60        61-120
nwfsrce  dim       6        121-126
nwflist  dim       30       127-156
nwfsel   dim       30       157-186
ninlist  dim       6        187-192
nwftype  dim       1        193-193    list type Continuation or Test
nwfmqty  form      8        194-201
nwfresp  form      7        202-208
nwfrr    form      1.6      209-216
nwf96z   form      1.2      217-220              .nwf only?
nwfP     form      1.6      221-228                   .nwf only?
nwfsqrtP form      1.6      229-236                   .nwf only?
nwfpci   form      1.6      237-244
nwfnci   form      1.6      245-252
nwfrev   form      9        253-261       gross revenue
nwfavgft form      2.2      262-266
nwflstcpm form     4.2      267-273       list cost per m 
nwftlst$  form     5.2      274-281
nwfpckcpm form     3.2      282-287
nwfTpck   form     5.2      288-295       total package cost
NWFTCPM  FORM      3.2      296-301     total cost per M
Nwftmcst form      6.2      302-310     total mail cost
nwfupcst form      4.2      311-317     unit premium cost
nwftpcst form      5.2      318-325     total premium cost
nwftotcst form     6.2      326-334     total cost
nwfnetrev form     6.2      335-343
nwfnrpci  form     6.2      344-352
nwfnrnci  form     6.2      353-361
nwfCstA   form     4.2      362-368        cost to aquire
nwfpcaci  form     4.2      369-375
nwfncaci  form     4.2      376-382
nwfCTA    form     4.2      383-389
nwfcost$  form     3.2      390-395
nwfnqty   form     7        396-402
nwfinv    form     5.2      403-410
nwflcpm   form     3.2      411-416
nwfkeycd  dim      6        417-422
nwfasresp form     7        423-429  Associate responses
nwfasrev  form     9        430-438     "       revenue
nwfasrr   form     1.6      439-446     "       response rate
nwfbasresp form     7       447-453   Basic    reponses
nwfbasrev form     9        454-462     "       revenue
nwfbasrr  form     1.6      463-470     "       response rate
nwfCI     form      1.6     471-478
         listend
.END OF INCLUDE
.
..............................................................................

