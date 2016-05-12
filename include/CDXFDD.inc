;----------------------------------------------------------------
;
;   CDXFDD.INC -
;
;   Copyright(c) Names in the News 2005
;
;   Author: David L Herrick
;----------------------------------------------------------------
;   Release         1.1                2005 June 18 Changed IP of File Manager
;   Release         1.0                2005 May
;   Secondary Company file.
;   COntains data regarding electronic information transfer.
;..............................................
.>Patch 1.1 Begin
CdxfFLIST	Filelist
;CDXFFILE	IFILE	keylen=6,fix=437,uncomp,Name="CompCDXF.ISI|20.20.30.103:502"
CDXFFILE	IFILE	keylen=6,fix=437,uncomp,Name="CompCDXF.ISI|NINS1:502"
	FileListEnd
.>Patch 1.1 End	
CDXFNAME	INIT	"CompCDXF       "
CDXFPATH	FORM	1
CDXFFLAG	FORM	1
CDXFFLD		DIM	6
;
CDXFLOCK	FORM	1

CDXFVARS	LIST
CDXFNUM		DIM	6         1-6      ;COMPANY NUMBER. **KEY**
;dataflag 0=No 1=yes
CDXFDFlag1	Form	1	  7-7      ;Data type flag 1  Shipping information
CDXFDFlag2	Form	1 	  8-8      ;Data type flag 2  Order Confirmation
CDXFDFlag3	Form	1	  9-9      ;Data type flag 3  Merge Information
CDXFDFlag4	Form	1        10-0      ;Data type flag
CDXFDFlag5	Form	1        11-11     ;Data type flag
CDXFDFlag6	Form	1	 12-12     ;Data type flag
CDXFDFlag7	Form	1	 13-13     ;Data type flag
CDXFDFlag8	Form	1	 14-14     ;Data type flag
CDXFDFlag9	Form	1	 15-15     ;Data type flag
CDXFFill1	Dim	20	 16-35
CDXFXType	Form	2	 36-37     ;Exchange type  current codes 1= custom 2= DMEXCHANGE
CDXFSite	Dim	50       38-87     ;web or ftp site
CDXFEmail	Dim	50	 88-137    ;Email address info sent to
CDXFCOMP	Dim	50	138-187    ;Name ie for DMEXCHANGE 3 byte acronym is required
;				           ;current ALC, MGI = Millard, = Direct Media
CDXFNotes	Dim	200	198-387    ;notes
cdxfFill2	Dim	50	388-437
		LISTEND