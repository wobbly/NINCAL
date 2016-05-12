PC	EQU	1
.......................................................
.......................................................
.	INT001A.PLS
.	Campaign/LOL/LR Cross Reference file update Routines
.	Andrew Harkins
.	August 12, 2004
.
.	Holds routines responsible for updating the Cross
.	Reference files used by Integral/Program1
.
.	NOTE:	This program is NOT a stand-alone program.
.		This program only contains called routines.
.......................................................
.......................................................
	include	common.inc
	include	cons.inc
	include	norddd.inc
	include	nloldd.inc
	include	nsel2dd.inc
	include	nsel3dd.inc
	include	statsdd.inc
.	include	f:\library\develop\backups\norddd.inc
.	include	f:\library\develop\backups\nloldd.inc
.	include	f:\library\develop\backups\nsel2dd.inc
.	include	f:\library\develop\backups\nsel3dd.inc
.	include	f:\library\develop\backups\statsdd.inc
	include	ncmpdd.inc
	include	ncmp2dd.inc
	include	nlol2dd.inc

release		init	"1.1"	01DEC2004	ASH	Campaign file conversion
.release		init	"1.0"	12AUG2004	ASH	Initial Release

DimPtr		dim	^
DimPtr1		dim	^
FrmPtr		form	^
FrmPtr1		form	^

.LR fields
OSTATbak	dim	1
OMLRNUMbak	dim	4
OLRNbak		dim	6
OLNUMbak	dim	6
OLONbak		dim	4
OMLRPONbak	dim	12
OQTYbak		dim	9
OMLRKYbak	dim	12
ORTNDTECbak	dim	2
ORTNDTEYbak	dim	2
ORTNDTEMbak	dim	2
ORTNDTEDbak	dim	2
OMDTECbak	dim	2
OMDTEYbak	dim	2
OMDTEMbak	dim	2
OMDTEDbak	dim	2
OELCODEbak	dim	1
ONETQTYbak	dim	9
OCAMPbak	dim	6
OXPPMbak	dim	5
ORTNNUMbak	dim	4
OCOCODEbak	dim	2
OCO2CODEbak	dim	2
OODTECbak	dim	2
OODTEYbak	dim	2
OODTEMbak	dim	2
OODTEDbak	dim	2
OEXQTYbak	dim	9
OBRKNUMbak	dim	4
OBRKCNTbak	dim	3

.LOL Fields
NLOLLOLbak	dim	6
NLOLCNumbak	dim	6
NLOLListbak	dim	6
NLOLOwnerbak	dim	4
NLOLQtybak	dim	9
NLOLNetQtybak	dim	9
NLOLTestbak	dim	1
NLOLRentbak	dim	1
NLOLNetbak	dim	6
NLOLNetReqbak	dim	6
NLOLNetAppbak	dim	6
NLOLMDatebak	dim	8
NLOLCommentbak	dim	150

.Billing/Accounting/Merge Fields
.onetper
.str10
.DupeRate
.nmrgiqty
.nmrgnet
.AR

.Select Fields
NSEL2PRICEbak	form	5.2
NSEL2SPRICEbak	form	5.2
NSEL2DESCbak	dim	3
NSEL2NUMbak	dim	4
NSEL2NAMEbak	dim	75

.Campaign Fields
NCMPNUMbak	dim	6
NCMPCNAMEbak	dim	45
.START PATCH 1.1 REPLACED LOGIC
.NCMPMLRbak	dim	4
.NCMPSHIPTObak	dim	4
NCMPMLRbak	dim	6
NCMPSHIPTObak	dim	6
.END PATCH 1.1 REPLACED LOGIC
NCMPCNTbak	dim	2
NCMPPLANNERbak	dim	2
NCMPQTYbak	dim	13
NCMPNETQTYbak	dim	13
NCMPMDATEbak	dim	8
NCMPRDATEbak	dim	8
NCMPCDATEbak	dim	8
NCMPCOMMENTbak	dim	300

mss1	plform	Error
	formload mss1

	shutdown

.Before saving LR/LOL record program 1 will test to see if cross-reference record exists and if it has not already been
.flagged to update integral.  In that case it will call IntegralStoreDetail.  Once record is saved, program 1 will perform
.same test and then call IntegralTestDetail.  IntegralTestDetail will update cross-reference record if necessary.
integralsetup Routine
	move	str1,str1
	return

IntegralStoreDetail Routine DimPtr,FrmPtr
.DimPtr  = LR/LOL Record
.FrmPtr  = '1' - LR, '0' - LOL
	call	IntegralPreTestDetail using DimPtr,FrmPtr,N1
	if (N1 <> C0)
		return
	endif
	if (FrmPtr = C1)	.LR Record
		move	C1,NORDPATH
		move	DimPtr,NORDFLD
		move	"Int-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
.
		move	OSTAT,OSTATbak
		move	OMLRNUM,OMLRNUMbak
		move	OLRN,OLRNbak
		move	OLNUM,OLNUMbak
		move	OLON,OLONbak
		move	OMLRPON,OMLRPONbak
		move	OQTY,OQTYbak
		move	OMLRKY,OMLRKYbak
		move	ORTNDTEC,ORTNDTECbak
		move	ORTNDTEY,ORTNDTEYbak
		move	ORTNDTEM,ORTNDTEMbak
		move	ORTNDTED,ORTNDTEDbak
		move	OMDTEC,OMDTECbak
		move	OMDTEY,OMDTEYbak
		move	OMDTEM,OMDTEMbak
		move	OMDTED,OMDTEDbak
		move	OELCODE,OELCODEbak
		move	ONETQTY,ONETQTYbak
		move	OCAMP,OCAMPbak
		move	OXPPM,OXPPMbak
		move	ORTNNUM,ORTNNUMbak
		move	OCOCODE,OCOCODEbak
		move	OCO2CODE,OCO2CODEbak
		move	OODTEC,OODTECbak
		move	OODTEY,OODTEYbak
		move	OODTEM,OODTEMbak
		move	OODTED,OODTEDbak
		move	OEXQTY,OEXQTYbak
		move	OBRKNUM,OBRKNUMbak
		move	OBRKCNT,OBRKCNTbak
.
		packkey	NSEL2FLD,"1",OLRN
	else			.LOL Record
		move	C1,NLOLPATH
		move	DimPtr,NLOLFLD
		move	"Int-NLOLKEY",Location
		pack	KeyLocation,"Key: ",NLOLFLD
		call	NLOLKEY
.
		move	NLOLLOL,NLOLLOLbak
		move	NLOLCNum,NLOLCNumbak
		move	NLOLList,NLOLListbak
		move	NLOLOwner,NLOLOwnerbak
		move	NLOLQty,NLOLQtybak
		move	NLOLNetQty,NLOLNetQtybak
		move	NLOLTest,NLOLTestbak
		move	NLOLRent,NLOLRentbak
		move	NLOLNet,NLOLNetbak
		move	NLOLNetReq,NLOLNetReqbak
		move	NLOLNetApp,NLOLNetAppbak
		move	NLOLMDate,NLOLMDatebak
		move	NLOLComment,NLOLCommentbak
.
		packkey	NSEL2FLD,"2",NLOLLOL
	endif
	move	"Int-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
.
	move	NSEL2PRICE,NSEL2PRICEbak
	move	NSEL2SPRICE,NSEL2SPRICEbak
	move	NSEL2DESC,NSEL2DESCbak
	move	NSEL2NUM,NSEL2NUMbak
	move	NSEL2NAME,NSEL2NAMEbak
	return

IntegralTestDetail Routine DimPtr,FrmPtr
.DimPtr  = LR/LOL Record
.FrmPtr  = '1' - LR, '0' - LOL
	call	IntegralPreTestDetail using DimPtr,FrmPtr,N1
	if (N1 <> C0)
		if (N1 = C2)			.No Detail Cross Ref record - test for Campaign Cross Ref
			if (FrmPtr = C1)	.LR Record
				move	C1,NORDPATH
				move	DimPtr,NORDFLD
				move	"Int3-NORDKEY",Location
				pack	KeyLocation,"Key: ",NORDFLD
				call	NORDKEY
				if not over
					call	Trim using OCAMP
					if (OCAMP <> "")
						move	C1,NCMP2PATH
						pack	NCMP2FLD,OCAMP
						move	"Int2-NCMP2KEY",Location
						pack	KeyLocation,"Key: ",NCMP2FLD
						call	NCMP2KEY
						if not over
.Create a New Detail Record
							call	IntegralCreateNewDetail using DimPtr,C1
						endif
					endif
				endif
			else			.LOL Record
				move	C1,NLOLPATH
				move	DimPtr,NLOLFLD
				move	"Int3-NLOLKEY",Location
				pack	KeyLocation,"Key: ",NLOLFLD
				call	NLOLKEY
				if not over
					call	Trim using NLOLCNum
					if (NLOLCNum <> "")
						move	C1,NCMP2PATH
						pack	NCMP2FLD,NLOLCNum
						move	"Int3-NCMP2KEY",Location
						pack	KeyLocation,"Key: ",NCMP2FLD
						call	NCMP2KEY
						if not over
.Create a New Detail Record
							call	IntegralCreateNewDetail using DimPtr,C0
						endif
					endif
				endif
			endif
		endif
		return
	endif
	move	C0,N1
	if (FrmPtr = C1)	.LR Record
		move	C1,NORDPATH
		move	DimPtr,NORDFLD
		move	"Int2-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
.
		if (OSTAT <> OSTATbak)
			move	C1,N1
		elseif (OMLRNUM <> OMLRNUMbak)
			move	C1,N1
		elseif (OLRN <> OLRNbak)
			move	C1,N1
		elseif (OLNUM <> OLNUMbak)
			move	C1,N1
		elseif (OLON <> OLONbak)
			move	C1,N1
		elseif (OMLRPON <> OMLRPONbak)
			move	C1,N1
		elseif (OQTY <> OQTYbak)
			move	C1,N1
		elseif (OMLRKY <> OMLRKYbak)
			move	C1,N1
		elseif (ORTNDTEC <> ORTNDTECbak)
			move	C1,N1
		elseif (ORTNDTEY <> ORTNDTEYbak)
			move	C1,N1
		elseif (ORTNDTEM <> ORTNDTEMbak)
			move	C1,N1
		elseif (ORTNDTED <> ORTNDTEDbak)
			move	C1,N1
		elseif (OMDTEC <> OMDTECbak)
			move	C1,N1
		elseif (OMDTEY <> OMDTEYbak)
			move	C1,N1
		elseif (OMDTEM <> OMDTEMbak)
			move	C1,N1
		elseif (OMDTED <> OMDTEDbak)
			move	C1,N1
		elseif (OELCODE <> OELCODEbak)
			move	C1,N1
		elseif (ONETQTY <> ONETQTYbak)
			move	C1,N1
		elseif (OCAMP <> OCAMPbak)
			move	C1,N1
		elseif (OXPPM <> OXPPMbak)
			move	C1,N1
		elseif (ORTNNUM <> ORTNNUMbak)
			move	C1,N1
		elseif (OCOCODE <> OCOCODEbak)
			move	C1,N1
		elseif (OCO2CODE <> OCO2CODEbak)
			move	C1,N1
		elseif (OODTEC <> OODTECbak)
			move	C1,N1
		elseif (OODTEY <> OODTEYbak)
			move	C1,N1
		elseif (OODTEM <> OODTEMbak)
			move	C1,N1
		elseif (OODTED <> OODTEDbak)
			move	C1,N1
		elseif (OEXQTY <> OEXQTYbak)
			move	C1,N1
		elseif (OBRKNUM <> OBRKNUMbak)
			move	C1,N1
		elseif (OBRKCNT <> OBRKCNTbak)
			move	C1,N1
		endif
.
		packkey	NSEL2FLD,"1",OLRN
	else			.LOL Record
		move	C1,NLOLPATH
		move	DimPtr,NLOLFLD
		move	"Int2-NLOLKEY",Location
		pack	KeyLocation,"Key: ",NLOLFLD
		call	NLOLKEY
.
		if (NLOLLOL <> NLOLLOLbak)
			move	C1,N1
		elseif (NLOLCNum <> NLOLCNumbak)
			move	C1,N1
		elseif (NLOLList <> NLOLListbak)
			move	C1,N1
		elseif (NLOLOwner <> NLOLOwnerbak)
			move	C1,N1
		elseif (NLOLQty <> NLOLQtybak)
			move	C1,N1
		elseif (NLOLNetQty <> NLOLNetQtybak)
			move	C1,N1
		elseif (NLOLTest <> NLOLTestbak)
			move	C1,N1
		elseif (NLOLRent <> NLOLRentbak)
			move	C1,N1
		elseif (NLOLNet <> NLOLNetbak)
			move	C1,N1
		elseif (NLOLNetReq <> NLOLNetReqbak)
			move	C1,N1
		elseif (NLOLNetApp <> NLOLNetAppbak)
			move	C1,N1
		elseif (NLOLMDate <> NLOLMDatebak)
			move	C1,N1
		elseif (NLOLComment <> NLOLCommentbak)
			move	C1,N1
		endif
.
		packkey	NSEL2FLD,"2",NLOLLOL
	endif
	if (N1 <> C1)
		move	"Int2-NSEL2KEY",Location
		pack	KeyLocation,"Key: ",NSEL2FLD
		call	NSEL2KEY
.
		if (NSEL2PRICE <> NSEL2PRICEbak)
			move	C1,N1
		elseif (NSEL2SPRICE <> NSEL2SPRICEbak)
			move	C1,N1
		elseif (NSEL2DESC <> NSEL2DESCbak)
			move	C1,N1
		elseif (NSEL2NUM <> NSEL2NUMbak)
			move	C1,N1
		elseif (NSEL2NAME <> NSEL2NAMEbak)
			move	C1,N1
		endif
	endif
	if (N1 = C1)
		call	IntegralUpdateDetailCrossRef using DimPtr,FrmPtr
	endif
	return

IntegralStoreCampaign Routine DimPtr
.DimPtr  = Campaign Number
	call	IntegralPreTestCampaign using DimPtr,N1
	if (N1 = C1)
		return
	endif
	move	C1,NCMPPATH
	move	DimPtr,NCMPFLD
	move	"Int-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
.
	move	NCMPNUM,NCMPNUMbak
	move	NCMPCNAME,NCMPCNAMEbak
.START PATCH 1.1 REPLACED LOGIC
.	move	NCMPMLR,NCMPMLRbak
.	move	NCMPSHIPTO,NCMPSHIPTObak
	move	NCMPMLR,NCMPMLRbak
	move	NCMPSHIPTO,NCMPSHIPTObak
.END PATCH 1.1 REPLACED LOGIC
	move	NCMPCNT,NCMPCNTbak
	move	NCMPPLANNER,NCMPPLANNERbak
	move	NCMPQTY,NCMPQTYbak
	move	NCMPNETQTY,NCMPNETQTYbak
	move	NCMPMDATE,NCMPMDATEbak
	move	NCMPRDATE,NCMPRDATEbak
	move	NCMPCDATE,NCMPCDATEbak
	move	NCMPCOMMENT,NCMPCOMMENTbak
	return

IntegralTestCampaign Routine DimPtr
.DimPtr  = Campaign Number
	call	IntegralPreTestCampaign using DimPtr,N1
	if (N1 = C1)
		return
	endif
	move	C1,NCMPPATH
	move	DimPtr,NCMPFLD
	move	"Int2-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
	if (NCMPNUM <> NCMPNUMbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPCNAME <> NCMPCNAMEbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPMLR <> NCMPMLRbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPSHIPTO <> NCMPSHIPTObak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPCNT <> NCMPCNTbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPPLANNER <> NCMPPLANNERbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPQTY <> NCMPQTYbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPNETQTY <> NCMPNETQTYbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPMDATE <> NCMPMDATEbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPRDATE <> NCMPRDATEbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPCDATE <> NCMPCDATEbak)
		call	IntegralUpdateCampaignCrossRef
	elseif (NCMPCOMMENT <> NCMPCOMMENTbak)
		call	IntegralUpdateCampaignCrossRef
	endif
	return

IntegralUpdateDetailCrossRef LRoutine DimPtr,FrmPtr
.DimPtr  = LR/LOL Record
.FrmPtr  = '1' - LR, '0' - LOL
	pack	NLOL2FLD1,"01X",DimPtr
	clear	NLOL2FLD2
	if (FrmPtr = C1)	.LR Record
		pack	NLOL2FLD3,"03X1"
	else
		pack	NLOL2FLD3,"03X0"
	endif
	move	"Int-NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD3
	call	NLOL2AIM
	if not over
		move	C1,NLOL2Upd
		clock	timestamp,NLOL2UDate
.		clear	NLOL2Type2
		move	"Int-NLOL2UPD",Location
		call	NLOL2UPD
	endif
	return

IntegralUpdateCampaignCrossRef LRoutine DimPtr
.DimPtr  = Campaign
	move	C1,NCMP2PATH
	pack	NCMP2FLD,DimPtr
	move	"Int-NCMP2KEY",Location
	pack	KeyLocation,"Key: ",NCMP2FLD
	call	NCMP2KEY
	if not over
		move	C1,NCMP2Upd
		clock	timestamp,NCMP2UDate
	endif
	move	"Int-NCMP2UPD",Location
	call	NCMP2UPD
	return

IntegralPreTestDetail LRoutine DimPtr,FrmPtr,FrmPtr1
	move	C0,FrmPtr1
	pack	NLOL2FLD1,"01X",DimPtr
	clear	NLOL2FLD2
	pack	NLOL2FLD3,"03X",FrmPtr
	move	"Int-NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD3
	call	NLOL2AIM
	if over
		move	C2,FrmPtr1
	elseif (NLOL2Upd = "1" | NLOL2Type2 = "2" | NLOL2Type2 = "3")
.NLOL2Upd   = "1"  - Already flagged to Update Integral
.NLOL2Type2 = "2"  - Conversion from LOL -> LR    OBSOLETE
.NLOL2Type2 = "3"  - Marked as Deleted
		move	C1,FrmPtr1
	endif
	return

IntegralPreTestCampaign LRoutine DimPtr,FrmPtr1
	move	C0,FrmPtr1
	move	C1,NCMP2PATH
	pack	NCMP2FLD,DimPtr
	move	"Int-NCMP2KEY",Location
	pack	KeyLocation,"Key: ",NCMP2FLD
	call	NCMP2KEY
	if over
		move	C1,FrmPtr1
	elseif (NCMP2Upd = "1")
		move	C1,FrmPtr1
	endif
	return

IntegralDeleteDetail Routine DimPtr,FrmPtr
.DimPtr  = LR/LOL Number
.FrmPtr  = Record Type
.
.Notifies Integral when a record is no longer in Merge Manager Campaign.
.After Integral is updated, Cross Reference record will be deleted.
.
.When converting from LOL - > LR this routine is implicitly called.  However,
.as the NINLOL2 Cross Reference record has already been updated to use the newly
.created LR record, this routine should not get a hit.
	pack	NLOL2FLD1,"01X",DimPtr
	clear	NLOL2FLD2
	pack	NLOL2FLD3,"03X",FrmPtr
	move	"Int2-NLOL2AIM",Location
	pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD3
	call	NLOL2AIM
	if not over
		move	C3,NLOL2Type2	."3" Indicates to Integral that record is deleted.
		move	C1,NLOL2Upd
		clock	timestamp,NLOL2UDate
		move	"Int2-NLOL2UPD",Location
		call	NLOL2UPD
	endif
	return

IntegralCreateNewDetail Routine DimPtr,FrmPtr
.DimPtr  = LR/LOL Number
.FrmPtr  = Record Type
	move	DimPtr,NLOL2Num
	move	"***********************",NLOL2INum
	move	FrmPtr,NLOL2Type
	clear	NLOL2Type2
	move	C1,NLOL2Upd
	clock	timestamp,NLOL2UDate
	pack	NLOL2FLD,NLOL2Num,NLOL2INum,NLOL2Type
	move	"Int-NLOL2WRT",Location
	pack	KeyLocation,"Key: ",NLOL2FLD
	call	NLOL2WRT
	return

IntegralMoveCampaign Routine DimPtr,DimPtr1,FrmPtr
.DimPtr = LR/LOL Number
.DimPtr1 = New Campaign Number
.FrmPtr = Record Type:  "0"-LOL, "1"-LR
	move	C1,NCMP2PATH
	pack	NCMP2FLD,DimPtr1		.New Campaign
	move	"I.M.C.-NCMP2KEY",Location
	pack	KeyLocation,"Key: ",NCMP2FLD
	call	NCMP2KEY
	if not over
.Make sure LR is in Cross Ref
		pack	NLOL2FLD1,"01X",DimPtr
		clear	NLOL2FLD2
		pack	NLOL2FLD3,"03X",FrmPtr
		move	"I.M.C.-NLOL2AIM",Location
		pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD3
		call	NLOL2AIM
		if over
			call	IntegralCreateNewDetail using DimPtr,FrmPtr
		else
			pack	NLOL2FLD,NLOL2Num,NLOL2INum,FrmPtr
			move	"I.M.C.-NLOL2TST",Location
			pack	KeyLocation,"Key: ",NLOL2FLD
			call	NLOL2TST
			if over		.Should never happen!!
				call	IntegralCreateNewDetail using DimPtr,FrmPtr
			else
				move	C1,NLOL2Upd
				clear	NLOL2Type2
				clock	timestamp,NLOL2UDate
				move	"I.M.C.-NLOL2UPD",Location
				pack	KeyLocation,"Key: ",NLOL2FLD
				call	NLOL2UPD
			endif
		endif
	else
		pack	NLOL2FLD1,"01X",DimPtr
		clear	NLOL2FLD2
		pack	NLOL2FLD3,"03X",FrmPtr
		move	"I.M.C.2-NLOL2AIM",Location
		pack	KeyLocation,"Key: ",NLOL2FLD1,COMMA,NLOL2FLD3
		call	NLOL2AIM
		if not over
			pack	NLOL2FLD,NLOL2Num,NLOL2INum,FrmPtr
			move	"I.M.C.2-NLOL2TST",Location
			pack	KeyLocation,"Key: ",NLOL2FLD
			call	NLOL2TST
			if not over
.				move	FrmPtr,NLOL2Type
				move	C3,NLOL2Type2
				move	C1,NLOL2Upd
				clock	timestamp,NLOL2UDate
				move	"I.M.C.2-NLOL2UPD",Location
				pack	KeyLocation,"Key: ",NLOL2FLD
				call	NLOL2UPD
			endif
		endif
	endif
	return

	include nordio.inc
	include nlolio.inc
	include nsel2io.inc
	include nsel3io.inc
	include statsio2.inc
	include ncmpio.inc
	include ncmp2io.inc
	include nlol2io.inc
	include comlogic.inc
