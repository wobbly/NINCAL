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
.		   1-1	  STATUS
.		  32-32	  NEW BYTE
.		  33-33   EXCLUSIVE BYTE
......................................................
. LAST MODIFIED
. patch 1.4 12September2006 DLH - Fulfillment & Min Changes
. patch 1.3 29JUNE2006 DMS - Add search, as per 6/12/2006 CTF Meeting
. patch 1.2 21JUNE2006 ASH - Added aamdex for Fulfillment Number
.	- 06Jul04 DLH Added NDatLUsage
.	- 29Jul02 ASH Began conversion Process
.Secondary FIles See
.	include	ntxtdd.inc - DATACARD TEXT FILE
.	include	nadddd.inc - address codes
.	include	narrdd.inc - Arrangement codes
.	include	ncatdd.inc - CATEGORY FILE DESCRIPTOR.
.	include	NSLTdd.inc - SELECTION CODE FILE DESCRIPTOR.
.	include	nsrcdd.inc - SOURCE CODE FILE DESCRIPTOR.
.	include	nmoddd.inc - PRICE MODIFIER FILE DESCRIPTOR.
.	include	nusedd.inc -  VARIaBLES MODULE FOR DATABUS PORT/USER IDENTIFICATION.
.	INclude	NMDCMsc.inc - additional info (currently from MIN) need to incorporate
.	INClude	NMDCCAT.inc - Min Category
.
.       - 18JUN2005  DMB IP Address changed for File Manager
.       - 06Apr2005  ASH  Modifed COMMPER
.	- 15Nov95 DLH add key by name
.	- 12/10/85 ADDED ONE BYTE TO EACH CATEGORY CODE, DELETED
.	BLANK8. RECORD SIZE NOW VARIABLE TO MAX 2813.
......................................................
NDATBNAME	DIM	35
NDATBNME1	INIT	"NINDATB                             "
NDATBNME2	INIT	"NINDATB                             "
NDATBNME3	INIT	"NINDATB4                            "	nindat3 is used for tdmc info (argh)
NDATBFLIST	FILELIST
NDATBFILE	IFILE	KEYLEN=6,FIXED=617,Name="NINDATB.ISI|NINS1:502"
NDATBFLE3	IFILE	KEYLEN=23,FIXED=617,Name="NINDATB4.ISI|NINS1:502"
.
NDATBFLE2	AFILE	FIXED=617,Name="NINDATB.AAM|NINS1:502"
	FILELISTEND
NDATBLOCK	FORM	1		0 OR 1=filepi,2=recordlock, 3=no lock
NDATBFLD		DIM	6
NDATBFLD1	DIM	9
NDATBFLD2	DIM	78
NDATBFLD3	DIM	75
NDATBFLD4	DIM	9
NDATBFLD5	DIM	4
NDATBFLD6	DIM	4
NDATBFLD7	DIM	4

NDATBFLAG	FORM	1
NDATBPATH	FORM	1
.
DATBVARS	LIST
DatVars
.datvars - see Ndatdd.inc	
NDatBStamp	Dim	17
	 LISTEND
