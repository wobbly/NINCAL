******************************************************
*  DATACARD FILE DESCRIPTOR.
. ....................................................
* ****************************************************
* NAMES IN THE NEWS MASTER DATACARD FILE.
* ****************************************************
.
.      FILE:      NINDAT
.    LENGTH:      600
.  COMPRESS:      NONE
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      2-7     LSTNUM
.                 64-138  MLSTNAME
.AIMDEX KEY:      2-7     LSTNUM
.                 64-138  MLSTNAME
.                 14-19   DATFUL
.                      1-1      STATUS
.                     32-32     NEW BYTE
.                     33-33   EXCLUSIVE BYTE
.Begin patch 1.83
.                        8-13    List owner
.                       14-19    Service B.
.end patch 1.83
......................................................
. LAST MODIFIED
.patch 1.83  2014 July add  owner and SB as aam keys
.patch 1.82  2014 July add flags counts,blocked dates,gender
.patch 1.81  2014 April  add LRA month day of renewal
.patch 1.8  2013 Oct 18  track if LRA required
.Patch 1.7  28August2012 DLH - add Ifile Nindat5   key=list # only if elstcde <> " "
.Patch 1.6  23Feb2009 DLH - List clearance delivery preference " " = none use presence of fax/email to determine '1'=fax, '2'=email 
.                                       Note if ' ' and email address is present that is the default
. patch 1.5 30May2008 DLH - Quick reco file for WEb
. patch 1.4 12September2006 DLH - Fulfillment & Min Changes
. patch 1.3 29JUNE2006 DMS - Add search, as per 6/12/2006 CTF Meeting
. patch 1.2 21JUNE2006 ASH - Added aamdex for Fulfillment Number
.         - 06Jul04 DLH Added NDatLUsage
.         - 29Jul02 ASH Began conversion Process
.Secondary FIles See
.         include   ntxtdd.inc - DATACARD TEXT FILE
.         include   nadddd.inc - address codes
.         include   narrdd.inc - Arrangement codes
.         include   ncatdd.inc - CATEGORY FILE DESCRIPTOR.
.         include   NSLTdd.inc - SELECTION CODE FILE DESCRIPTOR.
.         include   nsrcdd.inc - SOURCE CODE FILE DESCRIPTOR.
.         include   nmoddd.inc - PRICE MODIFIER FILE DESCRIPTOR.
.         include   nusedd.inc -  VARIaBLES MODULE FOR DATABUS PORT/USER IDENTIFICATION.
.         INclude   NMDCMsc.inc - additional info (currently from MIN) need to incorporate
.         INClude   NMDCCAT.inc - Min Category
.begin patch 1.5
.         INclude   NQRCdd.inc          - quick reco for web 
.end patch 1.5
.
.       - 18JUN2005  DMB IP Address changed for File Manager
.       - 06Apr2005  ASH  Modifed COMMPER
.         - 15Nov95 DLH add key by name
.         - 12/10/85 ADDED ONE BYTE TO EACH CATEGORY CODE, DELETED
.         BLANK8. RECORD SIZE NOW VARIABLE TO MAX 2813.
......................................................
NDATNAME  DIM       35
NDATNME1  INIT      "NINDAT|NINS1:502                            "
NDATNME2  INIT      "NINDAT|NINS1:502                            "
NDATNME3  INIT      "NINDAT4|NINS1:502                           "   nindat3 is used for tdmc info (argh)
NdatNME4  Init      "NINDAT5|NINS1:502                           "   (argh) see above only Manage files
NDATFLIST FILELIST
NDATFILE  IFILE     KEYLEN=6,FIXED=600,Name="NINDAT.ISI|NINS1:502"
NDATFLE2  AFILE     FIXED=600,Name="NINDAT.AAM|NINS1:502"
NDATFLE3  IFILE     KEYLEN=75,FIXED=600,Name="NINDAT4.ISI|NINS1:502"
NDATFLE4  Ifile     KEYLEN=6,FIXED=600,Name="NINDAT5.ISI|NINS1:502"           .only Managed lists
.NDATFILE IFILE     KEYLEN=6,FIXED=600,Name="NINDAT.ISI|20.20.30.103:502"
.NDATFILE IFILE     KEYLEN=6,FIXED=600,Name="NINDAT.ISI"
.NDATFLE3 IFILE     KEYLEN=55,FIXED=600,Name="NINDAT4.ISI|20.20.30.103:502"
.NDATFLE3 IFILE     KEYLEN=55,FIXED=600,Name="NINDAT4.ISI"
.NDATFLE2 AFILE     FIXED=600,Name="NINDAT.AAM|20.20.30.103:502"
.NDATFLE2 AFILE     FIXED=600,Name="NINDAT.AAM"
          FILELISTEND
NDATLOCK  FORM      1                   0 OR 1=filepi,2=recordlock, 3=no lock
NDATFLD             DIM       6
NDATFLD1  DIM       9
NDATFLD2  DIM       78
NDATFLD3  DIM       75
.START PATCH 1.2 ADDED LOGIC
NDATFLD4  DIM       9
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.3 ADDED LOGIC
NDATFLD5  DIM       4
NDATFLD6  DIM       4
.Begin patch 1.83
.NDATFLD7  DIM       4
.END PATCH 1.3 ADDED LOGIC
NDATFLD7  DIM       9
NDATFLD8  DIM       9
.end patch 1.83

NDATFLAG  FORM      1
NDATPATH  FORM      1
.
DATVARS   LIST
STATUS                DIM       1       1-1    'W' FOR WITHDRAWN.
LSTNUM                DIM       6       2-7    ZERO FILLED KEY.
OWNNUM                DIM       6       8-13   OWNER NUMBER (KEY FOR NINOWN  FILES)
DATFUL                DIM       6      14-19   FULFILLMENT NUMBER
DATMAN                DIM       6      20-25   MANAGER
DATMLR                DIM       6      26-31   MAILER  THIS WOULD NEGATE THE NEED FOR NINXRF.DAT!!!!!!!!!!
NLSTCDE               DIM       1      32-32   NEW LIST CODE (Y or N).
ELSTCDE               DIM       1      33-33   Management CODE (N, C, P, or  ).
.                              'N' = NEW YORK, 'C'=CALIFORNIA, 'P'=Pacific Lists, ' ' NOT Managed in house
OLDCOMMPER            DIM       3      34-36   COMMISSION.  OBSOLETE FOR ALL NEWLY COMPILED PROGRAMS AS OF 4/6/2005
HOTLINE               DIM       1      37-37   HOTLINE CODE    (Y or N).
NEWDATE               DIM       8      38-45   DATE CARD PUT UP CCYYMMDD FORMAT
REVDATE               DIM       8      46-53   REVISION DATE CCYYMMDD FORMAT - PRINTED ON DATACARD  data update date
PASSWORD              DIM       10     54-63   WHOM LAST UPDATED CARD  - PRINTED ON DATACARD
MLSTNAME              DIM       75     64-138  MASTER LIST NAME.
OLSTNAME              DIM       35    139-173  ORDER LIST NAME.
CLEANCDE              DIM       4     174-177  CLEANED CODE (Cxxx).
CLNINFO               DIM       38    178-215  CLEAN INFORMATION.
NETNAME               DIM       4     216-219  NET NAME CODE (Nxxx).
NETINFO               DIM       38    220-257  NET NAME INFORMATION.
DELCODE               DIM       4     258-261  DELIVERY CODE (Dxxx).
SAMPLE                DIM       4     262-265  SAMPLE CODE (Pxxx).
SEX                   DIM       15    266-280  SEX TEXT.   OFF 1 BYTE
MIN                   DIM       11    281-291  MINIMUM TEXT.
UNIVERSE              DIM       10    292-301  UNIVERSE QUANTITY.
DATPAY                DIM       6     302-307  PAY-TO NUMBER.
NDATCONV              DIM       1     308-308  CONVERSION BYTE
NDATEXCH              DIM       1     309-309  1 = EITHER, 2 = EXCHANGE ONLY, 3 = RENTAL ONLY -- OBSOLETE!!! USING BYTE IN NINSEL INSTEAD
UNITDATA              DIM       188   310-497
NDATWEB               DIM       1     498-498  ' ' =ALLOWED ON WEBSITE  1=Do not post
NDATOFF               DIM       1     499-499  1=OFFICE USE ONLY
NDATUPDDATE           DIM       8     500-507  UPDATE DATE          touched date
NDATUPDINIT           DIM       10    508-517  UPDATE INITS
NDATBUSY              DIM       1     518-518
NDatLUSAGE            DIM       1     519-519   ; 'F' If we cannot share list usage info
COMMPER               DIM       6     520-525  COMMISSION.
NDATVerf              Dim       8     526-533  Date info last confirmed data 
NDATNUPD              Dim       8     534-541       
NdatMen               form      3.2   542-547       percent men
NdatFem               Form      3.2   548-553       percent female
NdatOldOwn            Dim       6     554-559      Holds old manager / owner number if change occured 
.begin patch 1.6
NDatLCFlag            Dim       1         560-560   how to send lcr's " " use presence of email/fax# 1=fax, 2 = email
.begin patch 1.8
.NDatFill  Dim       40    561-600       
NDATLRA               Dim       1         561-561
.begin patch 1.81
NDatLRADte            Dim      2     562-563       .month of renewal
.begin patch 1.82
ndatFlag1             Dim        1          564-564    .y=blocked dates
ndatflag2             dim        1          565-565     .y=Gender
Ndatflag3             dim        1          566-566     .y=counts
Ndatflag4             dim        1          567-567     .y=mailer restrictions
NDatFill              Dim       33    568-600       
.NDatFill  Dim       37    564-600       
.NDatFill  Dim       39    562-600       
.end patch 1.81
.end patch 1.8
.end patch 1.6
.NDatFill  Dim       41    560-600       
.NDATFILL DIM       47    553-600  FILLER
.Eventually add following 2 fields:
.         Verification Date - Data that List Owner last verified our datacard info
.         Verification Schedule - Time frame when we start pestering List Owner about verifying our info.
           LISTEND
