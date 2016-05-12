	include	common.inc
	include	cons.inc
	include	nloldd.inc
	include	nlol2dd.inc
	include	ncmpdd.inc
	include	ncmp2dd.inc
	include	gnxtdd.inc
	include	compdd.inc
	include	cntdd.inc
	include	ncntdd.inc
	include	ndatdd.inc
	include	nseldd.inc
	include	norddd.inc
.START PATCH 1.1 ADDED LOGIC
	include	nsel2dd.inc
.END PATCH 1.1 ADDED LOGIC

Release	init	"1.4"	14JUN2005 ASH	Added:  NLOLMDATE, NLOLDate
.Release	init	"1.3"	08MAR2005 ASH	Rewrite:  Cleaned code alignment, added traps (undocumented), moved logic around (undocumented)
.					I did leave old patches for reference
.					Added patch to fix issues with Dates  3/21/05  ASH
.Release	init	"1.2"	22DEC2004 ASH	Temporary patch until Campaign Mailers come over from Integral using 6 byte Company number
.Release	init	"1.1"	19OCT2004 ASH	Added logic to properly add Detail records
.Release	init	"1.0" Initial Release of integral integration file

.START PATCH 1.1 ADDED LOGIC
.EXTERNAL ROUTINES FROM	NDAT001a.PLC
SelectTestBase4 external "NDAT001a;SelectTestBase4"
N52		form	5.2
.END PATCH 1.1 ADDED LOGIC

.CAMPAIGN(SCENARIO) + LOL(DETAIL) FILE
...First record is Campaign record:  length=425, Subsequent records are LOL records:  length=297
...One Campaign record for each file, followed by 0+ LOL records.
INTFILE		FILE VAR=425
INTNAME		INIT	"INTSCEN.DAT"
TMPFILE		FILE
FAILFILE	FILE
.
.CAMPAIGN(SCENARIO) VARS FOR INPUT FILE
INT1VARS	LIST
INTSID		DIM	18
INTSNAME	DIM	45
INTMLRNUM	DIM	4
INTSHIPTO	DIM	4
INTCNTCDE	DIM	2
INTPLCDE	DIM	2
INTGROSSQTY	DIM	13
INTNETQTY	DIM	13
INTMAILDTE	DIM	8
INTRETDTE	DIM	8
INTCUT		DIM	8
INTCOMM		DIM	300
		LISTEND
.
.LOL(DETAIL) VARS FOR INPUT FILE
INT2VARS	LIST
INT2DID		DIM	18
INT2LIST	DIM	6
INT2OWN		DIM	4
INT2SEL		DIM	4
INT2SELNAME	DIM	75
INT2PKG		DIM	6
INT2QTY		DIM	9
INT2NETQTY	DIM	9
INT2CONTFLG	DIM	1
INT2EXCHFLG	DIM	1
INT2MERGEP	DIM	6
INT2NETREq	DIM	6
INT2NOTES	DIM	150
.Taken out 8-11-04 after talking with jim - MM does not allow input of maildates
.INT2MAILDTE	DIM	8
		LISTEND
.
KEY2		INIT	"02X"
N34		form 3.4
TotRec		form	9

.HOLD VAR FOR CAMPAIGN NAME/NUMBER
HOLDCAMPAIGNNAME DIM	45
HOLDCAMPAIGNNUM	DIM	6

	CALL	PAINT
.Set up I/O Path Vars
	MOVE	C1,NDATPATH
	MOVE	C1,NCMPPATH
	move    C1,NCNTPATH
	MOVE	C1,NLOLPATH
	MOVE	C2,NCMP2PATH
.Default EMail Vars
	move	"NTS4",SmtpEmailServer			.Address of email serverc
	CLEAR	SmtpLogFile				.'Clear' disables the LogFile
	MOVE	"1",SmtpProgress			.Enable progress bars
.
.TEMPORARY FILE TO KEEP TRACK OF CAMP/DETAILS PROCESSED.
	DISPLAY	*P10:12,*EL,"Preparing Temporary Files"
	PREPARE	TMPFILE,"\\nins1\e\DATA\INTADD.DAT"
.FAILED FILE TO KEEP TRACK OF LOL RECORDS THAT ARE NOT ADDED TO OUR DATABASE.
	erase	"\\nins1\e\DATA\FAILEDADD.DAT"
	PREPARE	FAILFILE,"\\nins1\e\DATA\FAILEDADD.DAT"
.
	CLOCK	TIMESTAMP,TIMESTAMP
	DISPLAY	*P10:12,*EL,"Opening Integral File"
.
	OPEN	INTFILE,INTNAME
	READ	INTFILE,SEQ;INT1VARS
	STOP IF OVER
	ADD	C1,N9
	DISPLAY	*P10:14,*EL,"INTEGRAL RECORD COUNT READ ",N9
.Test for Mailer validity
	MOVE	INTMLRNUM,COMPFLD3
	CALL	TRIM using COMPFLD3
	CALL	ZFILLIT USING COMPFLD3
	move	"COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	CALL	COMPKEY3
	IF OVER
		pack	SmtpSubject,"INT0001.PLS - Error (1) Adding Campaign Record"
		pack	SmtpTextMessage(1),"Could not read Company File with Key: ",COMPFLD3,".  No Campaign or LOL Records were Created/Added!"
		move	"1",SmtpTextIndexLast
      		GOTO EXITINTEGRAL
	ENDIF
.Test for Integral ID validity
	IF (INTSID = "" OR INTSID = "                  ")
		pack	SmtpSubject,"INT0001.PLS - Error (2) Adding Campaign Record"
		pack	SmtpTextMessage(1),"No Unique ID from Integral for this Campaign.  No Campaign or LOL Records were Created/Added!"
		move	"1",SmtpTextIndexLast
		GOTO EXITINTEGRAL
	ENDIF
.
	MOVE	INTSID,NCMP2FLD1
	CALL	ZFILLIT USING NCMP2FLD1
	MOVE	"NCMP2KEY",Location
	PACK	KeyLocation,"Key: ",NCMP2FLD1
	CALL	NCMP2KEY
	IF NOT OVER
		pack	SmtpSubject,"INT0001.PLS - Error (3) Adding Campaign Record"
		pack	SmtpTextMessage(1),"Campaign for Mailer ##",INTMLRNUM," has already been imported from Integral!"
		pack	SmtpTextMessage(2)," The Campaign was not updated on the NIN system."
		pack	SmtpTextMessage(3),"Attempted Integral ID ##",NCMP2FLD1
		move	"3",SmtpTextIndexLast
		GOTO EXITINTEGRAL
	ELSE
.Load Campaign Variables
		clear	NCMPVARS
.Default Value for Media Type
		move	"20",NCMPMEDIA
.Default Value for Status
		move	"0",NCMPSTAT
.Campaign Name
		MOVE	INTSNAME,NCMPCNAME
.Mailer Number
.START PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH!!!
.		MOVE	INTMLRNUM,NCMPMLR
		MOVE	COMPNUM,NCMPMLR
.END PATCH 1.2 REPLACED LOGIC - TEMPORARY PATCH!!!
.Ship-To Number
		CALL	TRIM using INTSHIPTO
		CALL	ZFILLIT using INTSHIPTO
		MOVE	INTSHIPTO,NCMPSHIPTO
.NIN Contact
		CALL	TRIM using INTCNTCDE
		CALL	ZFILLIT using INTCNTCDE
		MOVE	INTCNTCDE,NCMPCNT
.NIN Planner
		CALL	TRIM using INTPLCDE
		CALL	ZFILLIT using INTPLCDE
.Remove code for the time being - Integral is using Contact file and NOT our Salesperson file!
		MOVE	INTPLCDE,N2
		if (n2 > 0)
			if (INTPLCDE = "50")		.Sue Adams
				move	"01",INTPLCDE
.			elseif (INTPLCDE = "11")	.Bonnie Olson
.				move	"02",INTPLCDE
			elseif (INTPLCDE = "12")	.Susan Anstrand
				move	"03",INTPLCDE
			elseif (INTPLCDE = "07")	.Ann Lovi
				move	"04",INTPLCDE
			elseif (INTPLCDE = "09")	.Phoebe Fearing
				move	"05",INTPLCDE
.			elseif (INTPLCDE = "10")	.Steve Kehrli
.				move	"19",INTPLCDE
.			elseif (INTPLCDE = "04")	.Jeanette Cassano
.				move	"08",INTPLCDE
			elseif (INTPLCDE = "15")	.Mac McIntosh
				move	"10",INTPLCDE
.			elseif (INTPLCDE = "14")	.Brian Schulz
.				move	"11",INTPLCDE
			elseif (INTPLCDE = "01")	.Suzie McGuire
				move	"14",INTPLCDE
			elseif (INTPLCDE = "46")	.Kattt Sammon
				move	"16",INTPLCDE
			else
				move	"00",INTPLCDE
			endif
		else
			move	"00",INTPLCDE
		endif
		MOVE	INTPLCDE,NCMPPLANNER
.Gross Qty
test
		CALL	TRIM using INTGROSSQTY
		CALL	ZFILLIT using INTGROSSQTY
		MOVE	INTGROSSQTY,NCMPQTY
.Net Qty
		CALL	TRIM using INTNETQTY
		CALL	ZFILLIT using INTNETQTY
		MOVE	INTNETQTY,NCMPNetQty
.Date Fields
.START PATCH 1.3 REPLACED LOGIC
.		MOVE	INTMAILDTE,NCMPMDATE	.Mail Date
.		MOVE	INTRETDTE,NCMPRDATE	.Return Date
.		MOVE	INTCUT,NCMPCDATE	.Cut Off Date
		unpack	INTMAILDTE,str4,MM,DD
		call	Trim using MM
		move	C0,N2
		move	MM,N2
		move	N2,MM
		rep	zfill,MM
		call	Trim using DD
		move	C0,N2
		move	DD,N2
		move	N2,DD
		rep	zfill,DD
		pack	NCMPMDATE,str4,MM,DD	.Mail Date
.
		unpack	INTRETDTE,str4,MM,DD
		call	Trim using MM
		move	C0,N2
		move	MM,N2
		move	N2,MM
		rep	zfill,MM
		call	Trim using DD
		move	C0,N2
		move	DD,N2
		move	N2,DD
		rep	zfill,DD
		pack	NCMPRDATE,str4,MM,DD	.Return Date
.
		unpack	INTCUT,str4,MM,DD
		call	Trim using MM
		move	C0,N2
		move	MM,N2
		move	N2,MM
		rep	zfill,MM
		call	Trim using DD
		move	C0,N2
		move	DD,N2
		move	N2,DD
		rep	zfill,DD
		pack	NCMPCDATE,str4,MM,DD	.Cut Off Date
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.1 ADDED LOGIC
		move	timestamp,NCMPDate	.Campaign Date
.END PATCH 1.1 ADDED LOGIC
.Comments
		MOVE	INTCOMM,NCMPCOMMENT
.Campaign Cross Reference Vars
		MOVE	NCMP2FLD1,NCMP2INUM
		MOVE	TIMESTAMP,NCMP2UDATE	.Update Date
		MOVE	C0,NCMP2UPD
.Get next Campagin Number
		MOVE	"NCMPNXT",GNXTFLD
		MOVE	"GNXTKEY",Location
		PACK	KeyLocation,"Key: ",GNXTFLD
		CALL	GNXTKEY
		IF OVER
			pack	SmtpSubject,"INT0001.PLS - Error (4) Adding Campaign Record"
			PACK	SmtpTextMessage(1),"Could not read GNXT File.  Key:  ",GNXTFLD,".  No Campaign or LOL records were Created/Added!"
			move	"1",SmtpTextIndexLast
			GOTO EXITINTEGRAL
		ELSE
			MOVE    GNXTNUM,N6
			LOOP
				ADD	C1,N6
				MOVE	N6,GNXTNUM
				REP	zfill,GNXTNUM
				MOVE	"GNXTUPD",Location
				PACK	KeyLocation,"Key: ",GNXTFLD
				CALL	GNXTUPD
				MOVE	N6,HOLDCAMPAIGNNUM
				REP	zfill,HOLDCAMPAIGNNUM
				MOVE	HOLDCAMPAIGNNUM,NCMPFLD
				MOVE	HOLDCAMPAIGNNUM,NCMP2NUM
				MOVE	"Save-NCMPTST",Location
				PACK	KeyLocation,"Key: ",NCMPFLD
				CALL	NCMPTST
				UNTIL OVER
			REPEAT
.Write to NINCMP.DAT
.make sure CAMP # is new number and not the last currently accessed one
			MOVE	HOLDCAMPAIGNNUM,NCMPNUM
			MOVE	"NCMPWRT",Location
			PACK	KeyLocation,"Key: ",NCMPFLD
			CALL	NCMPWRT
.
			MOVE	"NCMP2WRT",Location
			PACK	KeyLocation,"Key: ",NCMP2FLD1
			CALL	NCMP2WRT
.
			WRITE	TMPFILE,SEQ;TIMESTAMP
			WRITE	TMPFILE,SEQ;HOLDCAMPAIGNNUM,B1,NCMPCNAME,B1,NCMPMLR,B1,NCMP2INUM
			DISPLAY	*P10:16,*EL,"Campain Number For Integral Scenario ",HOLDCAMPAIGNNUM
.Set up beginnings of email message.
.Must be done here in case there are either no LOL records, or job bombs after this point.
			PACK	SmtpSubject,"Campaign for Mailer ##",NCMPMLR," was imported from Integral."
			pack	SmtpTextMessage(1),"Campaign ##",HOLDCAMPAIGNNUM
			pack	SmtpTextMessage(2),"Campaign Name:  ",NCMPCNAME
.Following value will be reset as more text lines are added to message in below logic
			move	"2",SmtpTextIndexLast
		ENDIF
	ENDIF
.LOL Section
. First Reread Campaign vars
	PACK	NCMPFLD,HOLDCAMPAIGNNUM
	MOVE	"NCMPKEY",Location
	PACK	KeyLocation,"Key: ",NCMPFLD
	CALL	NCMPKEY
	IF NOT OVER
		LOOP
READAGAIN
			READ	INTFILE,SEQ;INT2VARS
			UNTIL OVER
			ADD	C1,TotRec
			DISPLAY	*P10:14,*EL,"INTEGRAL RECORD COUNT READ ",TotRec
			CALL	TRIM using INT2LIST
			CALL	ZFILLIT using INT2LIST
			MOVE	INT2LIST,NDATFLD
			MOVE	"NDATKEY",Location
			PACK	KeyLocation,"Key: ",NDATFLD
			CALL	NDATKEY
			IF OVER
.Write out error record - FailedAdd.dat will be emailed to I.S. at end of program!
				WRITE	FAILFILE,SEQ;TIMESTAMP
				WRITE	FAILFILE,SEQ;"Campaign Number: ",NCMPNUM,", List: ",NDATFLD," - INVALID LIST - "
				GOTO READAGAIN
			ELSE
				IF (INT2DID = "" OR INT2DID = "                  ")
.Write out error record - FailedAdd.dat will be emailed to I.S. at end of program!
					WRITE	FAILFILE,SEQ;TIMESTAMP
					WRITE	FAILFILE,SEQ;"Campaign Number: ",NCMPNUM,", List: ",NDATFLD,", ID: ","BLANK"," - Added to system with Integral ID of '******************'.  Contact Integral!!!"
.Default value, will update to Integral at a later point
.Note:  Integral will need to be notified about this bogus value
					MOVE	"******************",INT2DID
				else
					CLEAR	NLOL2FLD1
					PACK	NLOL2FLD2,KEY2,INT2DID
					MOVE	"NLOL2AIM",Location
					PACK	KeyLocation,"Key: ",NLOL2FLD2
					CALL	NLOL2AIM
					IF NOT OVER
.Write out error record - FailedAdd.dat will be emailed to I.S. at end of program!
						WRITE	FAILFILE,SEQ;TIMESTAMP
						WRITE	FAILFILE,SEQ;"Campaign Number: ",NCMPNUM,", List: ",NDATFLD,", ID: ",INT2DID," - Integral ID Already Exists!!!"
						GOTO READAGAIN
					ENDIF
				ENDIF
.LOAD XREF VARS
				MOVE	INT2DID,NLOL2INUM
				MOVE	C0,NLOL2TYPE
				MOVE	C0,NLOL2UPD
				MOVE	TIMESTAMP,NLOL2UDATE
.LOAD LOL VARS
				clear	NLOLVARS
.List
				MOVE	INT2LIST,NLOLLIST
.Owner
				CALL	TRIM using INT2OWN
				CALL	ZFILLIT using INT2OWN
				MOVE	INT2OWN,NLOLOWNER
.List Select
				CALL	TRIM using INT2SEL
				CALL	ZFILLIT using INT2SEL
.START PATCH 1.1 REPLACED LOGIC
.				IF (INT2SEL <> "XXXX")
.					PACK NSELFLD,NLOLLIST,INT2SEL
.					CALL NSELKEY
.					IF NOT OVER
.							MOVE NSELSNAME,NLOLSELECT
.					ELSE
..						CALL TRIM using INT2SELNAME
..						MOVE INT2SELNAME,NLOLSELECT
.						CLEAR NLOLSELECT
.					ENDIF
.				ELSE
.						CLEAR NLOLSELECT
..						CALL TRIM using INT2SELNAME
..						MOVE INT2SELNAME,NLOLSELECT
.				ENDIF
..............................................................
				CLEAR	NLOLSELECT		.Obsolete Variable!!!!!
				CLEAR	NSEL2VARS
				move	"2",NSEL2CODE
				move	"XXXX",NSEL2NUM		.Default Value
				MOVE	INT2SELNAME,NSEL2NAME	.Default Value
				move	timestamp,NSEL2DATE
				IF (INT2SEL <> "XXXX")
					PACK	NSELFLD,NLOLLIST,INT2SEL
					CALL	NSELKEY
					IF NOT OVER
						call	LoadSelectVars
					ELSE
						call	FindSelectName
					ENDIF
				ELSE
					call	FindSelectName
				ENDIF
.END PATCH 1.1 REPLACED LOGIC
.Package - Currently not used
.				CALL	TRIM using INT2PKG
.				MOVE	INT2PKG,NLOLPackage
.Quantity
				CALL	TRIM using INT2QTY
				CALL	ZFILLIT using INT2QTY
				MOVE	INT2QTY,NLOLQTY
.Net Quantity
				CALL	TRIM using INT2NETQTY
				CALL	ZFILLIT using INT2NETQTY
				MOVE	INT2NETQTY,NLOLNETQTY
.Test/Continuation
				CALL	TRIM using INT2CONTFLG
				CALL	ZFILLIT using INT2CONTFLG
				MOVE	INT2CONTFLG,NLOLTEST
.Exchange/Rent
				MOVE	INT2EXCHFLG,NLOLRENT
.Merge Percentage
				call	trim using int2mergep
				if (int2mergep <> "1")
					MOVE	INT2MERGEP,N34
					MULT	"100",N34
					if (n34 <> c0)
						MOVE	N34,NLOLNET
					else
						clear	NLOLNET
					endif
				else
					clear	NLOLNET
				endif
.Net Requested
				call	trim using INT2NETREQ
				if (INT2NETREQ <> "1")
					MOVE	c0,N34
					MOVE	INT2NETREQ,N34
					MULT	"100",N34
					if (n34 <> c0)
						MOVE	N34,NLOLNETREQ
					else
						clear	NLOLNETREQ
					endif
				else
					clear	nlolnetreq
				endif
.Comments
				MOVE	INT2NOTES,NLOLCOMMENT
.Taken out 8-11-04 after talking with jim - MM does not allow input of maildates
.				MOVE INT2MAILDTE,NLOLMDATE
.START PATCH 1.4 ADDED LOGIC
.Pull Mail Date from Campaign Mail Date
				move	NCMPMDATE,NLOLMDATE
				move	timestamp,NLOLDate
.END PATCH 1.4 ADDED LOGIC
.Get next available NLOLLOL - a hidden value
				MOVE	"NLOLNXT",GNXTFLD
				MOVE	"GNXTKEY-2",Location
				PACK	KeyLocation,"Key: ",GNXTFLD
				CALL	GNXTKEY
				IF OVER
					call	SendContactEmail	.Send off what was successful
					pack	SmtpSubject,"INT0001.PLS - Error (1) Adding LOL Record"
					PACK	SmtpTextMessage(1),"Could not read GNXT File.  Key:  ",GNXTFLD,".  LOL record Creatin has been halted!"
					move	"1",SmtpTextIndexLast
					GOTO EXITINTEGRAL
				ELSE
					MOVE	GNXTNUM,N6
					LOOP
						ADD	C1,N6
						MOVE	N6,GNXTNUM
						REP	zfill,GNXTNUM
						MOVE	"GNXTUPD-2",Location
						PACK	KeyLocation,"Key: ",GNXTFLD
						CALL	GNXTUPD
						MOVE	N6,NLOLFLD
						REP	zfill,NLOLFLD
						MOVE	"NLOLTST",Location
						PACK	KeyLocation,"Key: ",NLOLFLD
						CALL	NLOLTST
						UNTIL OVER
					REPEAT
				ENDIF
				MOVE	NLOLFLD,NLOL2NUM
				CALL	ZFILLIT USING NLOL2NUM
				MOVE	NLOLFLD,NLOLLOL
				CALL	ZFILLIT USING NLOLLOL
.make sure LOL # is new number and not the last accessed one
				MOVE	HOLDCAMPAIGNNUM,NLOLCNUM
				MOVE	"NLOLWRT",Location
				PACK	KeyLocation,"Key: ",NLOLFLD
				CALL	NLOLWRT
				MOVE	"NLOL2WRT",Location
				PACK	KeyLocation,"Key: ",NLOL2FLD2
				CALL	NLOL2WRT
.START PATCH 1.1 ADDED LOGIC
				MOVE	NLOLLOL,NSEL2LR
				pack	NSEL2FLD,"2",NLOLLOL
				MOVE	"NSEL2WRT",Location
				PACK	KeyLocation,"Key: ",NSEL2FLD
				CALL	NSEL2WRT
				ADD	C1,howmany
.END PATCH 1.1 ADDED LOGIC
.TEMPFILE IN CASE OF ERROR
				WRITE	TMPFILE,SEQ;TIMESTAMP
				WRITE	TMPFILE,SEQ;NLOLLOL,B1,NLOLLIST,B1,NLOLSELECT,B1,NLOL2INUM
.
				MOVE	"7",SmtpTextIndexLast				.Index to last entry in TextMessage array
				pack	SmtpTextMessage(3),"LOL records were added to the system for Campaign ##",HOLDCAMPAIGNNUM
				move	NCMPCNAME to SmtpTextMessage(4)
				pack	SmtpTextMessage(5),"For Mailer: ",NCMPMLR
				pack	SmtpTextMessage(6),"Total LOL Read: ",TotRec	.Array <Text message >
				pack	SmtpTextMessage(7),"Total LOL Added: ",howmany	.Array <Text message >
			ENDIF
		REPEAT
	ELSE
		call	SendContactEmail	.Send off what was successful
		pack	SmtpSubject,"INT0001.PLS - Error (2) Adding LOL Record"
		PACK	SmtpTextMessage(1),"LOL Campaign Number Corrupted.  Key:  ",NCMPFLD,".  LOL record Creatin has been halted!"
		move	"1",SmtpTextIndexLast
		GOTO EXITINTEGRAL
	ENDIF
	CLOSE TMPFILE
	CLOSE FAILFILE
	call	SendContactEmail
.Send out Email if any LOL records failed!!
	FindFile "\\nins1\e\DATA\FAILEDADD.DAT"
	if zero		.File Exists - send Email
		pack	SmtpSubject,"INT0001.PLS - Failed File Attached"
		pack	SmtpTextMessage(1),"There were Detail records from Integral that Failed to download!"
		move	"1",SmtpTextIndexLast
		move	"InformationServices@nincal.com",SmtpEmailAddress
		move	"InformationServices",SmtpUserName		.User name
		move	"InformationServices",SmtpUserFullName		.User Full Name
		move	"InformationServices@nincal.com",SmtpDestinations(1,1)
		move	"1",SmtpDestIndexLast				.Index to last entry	in Dest	array
		move    "FAILEDADD.DAT",SmtpAttachments(1,1)		.Attached file name
		move    "\\nins1\e\DATA",SmtpAttachments(1,2)		.Path to attached file name
		move    "1",SmtpAttIndexLast				.Index to last entry - Only 1 entry
		move    "C:\work\eMail.Log",SmtpLogFile			.Path/filename to Log all socket read/writes
		move    "1",SmtpProgress				.Enable progress bars
		call    SmtpSend					.'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
		if not equal
			pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
				"Status Code ",SmtpStatus," - ",SmtpStatusText
			move    "FAILEDADD.DAT not found!",SmtpSubject Subject
			move    "0",SmtpAttIndexLast			.Index to last entry - Only 1 entry
			call    SmtpSend				.'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
.	                Alert  Stop,Mess,F1,"PL/B emailer - ERROR"
		else
.	                Alert  Stop,"eMail transmitted OK",F1,"PL/B emailer"
		endif
	endif
	stop

SendContactEmail
	clear	smtpemailaddress
	MOVE    "INFORMATIONSERVICES@NINCAL.COM",SMTPEMAILADDRESS
	MOVE	smtpemailaddress,SmtpDestinations(1,1)
	MOVE	"1",SmtpDestIndexLast				.Default
.
	pack    NCNTFLD,INTCNTCDE
	move    "Verify-NCNTTST",Location
	call    NCNTKEY
	if not over
		clear	SmtpDestinations(2,1)
		MOVE	CNTNAME,SmtpDestinations(2,2)
		SQUEEZE	CNTNAME,CNTNAME
		APPEND	CNTNAME,SmtpDestinations(2,1)
		APPEND	"@NINCAL.COM",SmtpDestinations(2,1)
		RESET	SmtpDestinations(2,1)
		MOVE	"2",SmtpDestIndexLast
	endif
	CALL    SmtpSend					.'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	return

EXITINTEGRAL
.SmtpTextMessage(#) established in calling routines
	move	"InformationServices@nincal.com",SmtpEmailAddress
	move	"InformationServices",SmtpUserName		.User name
	move	"InformationServices",SmtpUserFullName		.User Full Name
	move	"InformationServices@nincal.com",SmtpDestinations(1,1)
	move	"1",SmtpDestIndexLast				.Index to last entry	in Dest	array
	move	"0",SmtpAttIndexLast				.Index to last entry	- Only 1 entry
	call	SmtpSend					.'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	STOP

LoadSelectVars
	MOVE	NSELNUM,NSEL2NUM
	MOVE	NSELSNAME,NSEL2NAME	.Replaces any name they may have keyed-in in Merge Manager
	move	NSELQTY,NSEL2QTY
	move	NSELQTY,NSEL2QTY2
	move	NSELDESC,NSEL2DESC
	move	NSELDESC,NSEL2DESC2
	if (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
		move	NSELPRICE,NSEL2SPRICE
		move	NSELPRICE,NSEL2SPRICE2
		call	SelectTestBase4 using NSELLIST,NSELBASE,N52
		move	N52,NSEL2PRICE
		move	N52,NSEL2PRICE2
	else
		move	NSELPRICE,NSEL2PRICE
		move	NSELPRICE,NSEL2PRICE2
	endif
	return

FindSelectName
	move	INT2SELNAME,taskname
	call	Trim using taskname
	if (taskname <> "")
		packkey	NSELFLD1,"01X",INT2LIST
		clear	NSELFLD2
		packkey	NSELFLD3,"03X",taskname
		rep	lowup,taskname
		move	"NSELAIM",Location
		pack	KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD3
		call	NSELAIM
		loop
			until over
.The following is an extreme double-check!!
			call	Trim using NSELSNAME
			rep	lowup,NSELSNAME
			if (taskname = NSELSNAME)
				move	NSELNUM,NSEL2NUM
				call	LoadSelectVars
				break
			endif
			move	"NSELKG",Location
			call	NSELKG
		repeat
	endif
	return

	include	nlolio.inc
	include	nlol2io.inc
	include	ncmpio.inc
	include	ncmp2io.inc
	include	gnxtio.inc
	include	compio.inc
	include	cntio.inc
	include	ncntio.inc
	include	ndatio.inc
	include	nselio.inc
.START PATCH 1.1 ADDED LOGIC
	include	nsel2io.inc
.END PATCH 1.1 ADDED LOGIC
	include	comlogic.inc
