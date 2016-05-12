.ORDER PROGRAM
........................................
. Program:	Nord0001.PLS
. Function:	Order Report Program (development)
. Author:	Andrew Harkins
. Orig.	Date:	July 14,1999
. Release:	1.0
........................................
PC	EQU	1
.Include Files
	include	common.inc
	include	cons.inc
	include	norddd.inc
.	include	f:\library\develop\backups\norddd.inc
	include	nord4dd.inc
	include	nord5dd.inc
.	include	f:\library\develop\backups\nord4dd.inc
.	include	f:\library\develop\backups\nord5dd.inc
.START PATCH 3.6 ADDED LOGIC
	include	nord6dd.inc
.	include	f:\library\develop\backups\nord6dd.inc
.END PATCH 3.6 ADDED LOGIC
	include	nordpdd.inc
.	include	f:\library\develop\backups\nordpdd.inc
.START PATCH 3.74 REPLACED LOGIC
.	include	nmlrdd.inc
.	include	nmlr2dd.inc
.	include	nbrkdd.inc
.	include	nbrk2dd.inc
............
	include	compdd.inc
	include	cntdd.inc
.	include	f:\library\develop\backups\compdd.inc
.	include	f:\library\develop\backups\cntdd.inc
.END PATCH 3.74 REPLACED LOGIC
	include	nshpdd.inc
.	include	f:\library\develop\backups\nshpdd.inc
	include	npnddd.inc
	include	nrtndd.inc
	include	ninvdd.inc
.	include	f:\library\develop\backups\ninvdd.inc
	include	nowndd.inc
.	include	f:\library\develop\backups\nowndd.inc
	include	media.inc
	include	nsmpdd.inc
	include	nofrdd.inc
	include	nonodd.inc
.	include	f:\library\develop\backups\nonodd.inc
	include	ncrcdd.inc
.	include	f:\library\develop\backups\ncrcdd.inc
	include	ndatdd.inc
	include	npasdd.inc
	include	nxrfdd.inc
	include	nxngdd.inc
.	include	f:\library\develop\backups\nxngdd.inc
	include	gnxtdd.inc
	include	nxchdd.inc
.	include	f:\library\develop\backups\nxchdd.inc
	include	nspedd.inc
	include	nspe2dd.inc
.	include	f:\library\develop\backups\nspedd.inc
.	include	f:\library\develop\backups\nspe2dd.inc
	include	nspidd.inc
	include	tinvdd.inc
	include	npaydd.inc
	include	oslspern.inc
	include	nmdldd.inc
	include	hp.inc
	include	nusedd.inc
	include	ncntdd.inc
	include	ncmpdd.inc
	include	nloldd.inc
.	include	f:\library\develop\backups\nloldd.inc
	include	npkgdd.inc
	include	statsdd.inc
.	include	f:\library\develop\backups\statsdd.inc
	include	statndd.inc
	include	nprcdd.inc
	include	nprc2dd.inc
	include	nmrgdd.inc
.START PATCH 3.51 REPLACED LOGIC
.	include	fulfill.inc
.START PATCH 3.78.4 REMOVED LOGIC
.	include	nfuldd.inc
.END PATCH 3.78.4 REMOVED LOGIC
.	include	f:\library\develop\backups\nfuldd.inc
.END PATCH 3.51 REPLACED LOGIC
	include	winapi.inc
.START PATCH 11FEB2002 ADDED LOGIC
	include	nmlddd.inc
.	include	f:\library\develop\backups\nmlddd.inc
.END PATCH 11FEB2002 ADDED LOGIC
.START PATCH 3.72.1  30SEP2003 ADDED LOGIC
	include	compnotesdd.inc
.END PATCH  3.72.1 30SEP2003
.START PATCH 3.72 ADDED LOGIC
	include	nseldd.inc
.	include	f:\library\develop\backups\nseldd.inc
	include	nrefdd.inc
	include	nsel2dd.inc
	include	nsel3dd.inc
.	include	f:\library\develop\backups\nsel2dd.inc
.	include	f:\library\develop\backups\nsel3dd.inc
	include	nmoddd.inc
	include	nadddd.inc
	include	narrdd.inc
	include	ncatdd.inc
	include	NSLTdd.inc
	include	nsrcdd.inc
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.75 ADDED LOGIC
	include	nlol2dd.inc
	include	ncmp2dd.inc
.END PATCH 3.75 ADDED LOGIC
.START PATCH 3.76.9 ADDED LOGIC
.	include	f:\library\develop\backups\nord8dd.inc
	include	nord8dd.inc
.END PATCH 3.76.9 ADDED LOGIC
Release	init	"3.79.4"	 DLH Change LM contact from Agnes to Joey
Reldate	Init	"02May2007"
.Release	init	"3.79.3"	 DLH Return to inactive byte
.Reldate	Init	"12April2007"
.Release	init	"3.79.2"	 DLH Company Code and expand oslspern.inc
.Reldate	Init	"8March2007"
.Release	init	"3.79.1"	 DLH Change LM contact from Joey to Agnes & Modify RevTyps --- who is allowed to do corrections
.Reldate	Init	"11January2007"
.Release	init	"3.79"			DMB 19OCT06 Code Modified for bug fix which was not clearing a fullfillment field in nord001a (editlist_lostfocus)nor was a msg box displaying correctly when the datacard fulfill and order fulfill were diff.
.Release	init	"3.78.9"			DMB 18OCT06 Code Modified to not have this message pop up if there is not a fulfillment associated with the datacard.
.Release	init	"3.78.8"			DMB 18OCT06 Integrated Company/fulfillment Number into the order file and out of the owner file.  Fulfillment number will now be associated withe the datacard.
.Release	init	"3.78.7"			DLH 10Oct06 New NINspi 3 byte key
.Reldate	Init	"19Oct2006"
.Release	init	"3.78.6"			DMB 27SEP06 Replaced Logic in nord001a.plf in regards to adding frontline's status of orders like triplex - used to be tdmcnotify is now ninnot.dat
.Release	init	"3.78.5"			DMB 22AUG06 Replaced Logic in nord001a.plf in regards to the mutual exlusivity of test,retest an entire checkboxes.
.Release	init	"3.78.4"			ASH Fulfillment File conversion
.Release	init	"3.78.3"			DMS 13JUN06 replaced NLINE1-6 with NLINE in NORD001G.plf
.								convert ordnotes file
.Release	init	"3.78.2.5"			DMS renamed all plf vars to reflect plf names in the following plfs:
.	nord0001, nord001b, nord01a2, nord01a1, nord08a1, nord001t, nord01ea,
.	nord01ec, nord001g, nord001i, nordmsk1, nordmsk2, nordmsk3, nord001d,
.	nord01eb, nsta0001, STATS, nsta001a, nsta001b, nsta001c, nsta001d,
.	nsta001e
.Release		init	"3.78.2"			DMS added mailer notes to screen 5 - nord001D.plf streamline
.Release		init	"3.78.1"			DMS moved HoldMDate logic to after nordupd
.Release		init	"3.78"				DMS added VerifyDate calls to comlogic for date processing
.Release		init	"3.77.9"			DMS  4Mar06   tightened date processing
.Release		init	"3.77.8"			ASH  17JAN06  Modified notification when deleting LOL records
.Release		init	"3.77.7"			ASH  05JAN06  Added 'Pending Internal' code for LCRs
.Release		init	"3.77.5"			ASH  22DEC05  Small patch to thwart s04 (subcode 106) Errors
.Release		init	"3.77.5"			JD  08DEC05  Added new Ship-to number for Donnelley/InfoUSA
.Release		init	"3.77.4"			ASH 18NOV05  Added logic to thwart speed issues with PLB90
.Release		init	"3.77.3"			ASH 10NOV05  Added new Ship-to number for Donnelley/InfoUSA
.Release		init	"3.77.2"			ASH 17OCT05  Small Bug Fix:  Verify Temp file exists when using XLS button on Screen 7
.Release		init	"3.77.1"			ASH 12SEP05  Small Bug Fixes:  Verify Existence of XRef record before Writing out
.Release		init	"3.77.0"			ASH 26AUG05  Small Bug Fixes:
.									Retain Selected Format Charges when changing Select name
.Release		init	"3.76.9"			ASH 04AUG05  Added logic for New file to track 'New' Orders/LCRs/Pending
.Release		init	"3.76.8"			ASH 30JUN05  Added logic for DMExchange
.Release		init	"3.76.7"			ASH 22JUN05  Corrected logic around flags used to determine if TDMC should be notified of record changes
.								This patch is in addition to current logic.  I am still going to use the default Change_Event logic.
.								However, as the 'F5' function does not trigger the Change_Event, I am adding this logic.  I do not
.								want to rip out existing logic as Verify logic is currently correctly coded to notify Triplex of what
.								type of change they should be expecting.  Consequently, there will be some redundant code.
.Release		init	"3.76.6"			DMB 18JUN05  File Manager IP Change
.Release		init	"3.76.5"			ASH 13JUN05  PCL2PDF Retirement - commented out old LCR Print code
.Release		init	"3.76.4"			ASH 10JUN05  Added Code to display new codes in shipping file screen 4
.Release		init	"3.76.3"			DMB 01JUN05  Added Code to display new codes in shipping file screen 4
.Release		init	"3.76.2"			DMB 26MAY05  Added Code to display fufillment info in info.pls dialog box (see nord001a.plf for nordtest mod)
.Release	init	"3.76.1"				ASH 29APR05  Added option for PDF files.
.Release	init	"3.76"				ASH 14MAR05  Exchange File Conversion
.Release	init	"3.75.9"				ASH 27JAN05  NINSTAT Conversion
.Release	init	"3.75.8"				ASH 08DEC04 Fixes:  Net buttons, MailerLostFocus & associated Brokers
.								Could not find error with net buttons, but did move some code under Click_Nord01EBButtonNet
.Release	init	"3.75.7"				ASH 23NOV04 Converted Campaign File
.Release	init	"3.75.6"				ASH 12NOV04 CTF Items
.								361 - Notification when faxing LCRs if marked as "Sample to Follow"
.								327 - Mailer to Mailer Notes
.Release	init	"3.75.5"				ASH 01NOV04 Sample Conversion
.Release	init	"3.75.4"				ASH 18OCT04 Package Conversion
.Release	init	"3.75.3"				ASH 04OCT04 CTF Items
.								Work Orders:  53 & 469
.Release	init	"3.75.2"				ASH 02SEP04 Logo Conversion
.Release	init	"3.75.1"				ASH 25AUG04 Added patches to thwart bug associated with turning an LCR into an "Approved" Pending
.Release	init	"3.75"				ASH 13AUG04 Added logic to update Integral Merge Manager
.Release	init	"3.74.2"				ASH 20JUN04 Added patches to thwart bugs associated with Copy button
.								tempfile now found in cons.inc - removed from this program
.Release	init	"3.74.1"				DMB 17JUN04 Reloaded Search str after unpacking of new company number
.Release	init	"3.74"				ASH 27MAY04 MAILER CONVERSION
.							6/3/2004 Corrected logic around Reference Prices
.							6/2/2004 Added Dummy Orders to Search function
.							6/2/2004 Removed Select Total lines
.Release	init	"3.73"				ASH 18MAY04 Added logic to prevent Orders without prior clearance for Lists marked as requiring In-House LCR process in NMDL0001.PLS
.								Added logic to prevent Net Verification for LCRs/Pending Orders
.Release	init	"3.72.9"				ASH 06MAY04 Small Fixes
.								Added logic to allow Live Orders in LM Cancelled Report
.								Added button to allow viewing of Searchform
.								Added logic to thwart I44 errors when using LCR loop on Screen 7
.								Replaced Order2StatClearStat with an EditText box so Copying is possible.
.Release	init	"3.72.8"				ASH 19APR04 Small Fixes
.								Corrected erroneous Campaign Revision Date/inits on New Campaigns
.								Corrected incorrect prints when updating Orders
.								Corrected problem with NSELAIM being called w/o valid List Number
.								Corrected problem with OPPM displays
.Release	init	"3.72.7"				ASH 15JAN04 Added Code for new LM Cancelled/Denied Report
.							ASH 02FEB04 Fixed bug with Sample Verification
.Release	init	"3.72.6"				DMB 08JAN04 Added Code to Use Mailer Sales person instead of broker unless mailer sales is null then use broker in-house sales person
.							ASH ADDED CODE FOR MORE INFO ON SCREEN 10, SUBSCREEN 2 - CALC INFO AND XSTAT
.Release	init	"3.72.5"				ASH 04DEC03 Small fixes:  Moved Stop button on Screen 7.  Added logic to ensure Campaign fields were refreshed when using loop on Screen 7.
.Release	init	"3.72.4"				ASH 03NOV03 Addition of new Search Screen logic
.Release	init	"3.72.3"				ASH 16OCT03 Work Order #95 - Campaign number required for LW Robbins Orders
.							Work Order # 49, 50, 66
.Release	init	"3.72.2"				ASH 08OCT03	SMALL REQUESTS SUBMITTED TO CTF
.Release	init	"3.72.1"				DMB 30SEP03 Added Code to Replace Broker Notes with COMPANY Notes For Coloring of Broker label

.release	init	"3.72"			ASH 19SEP03 NEW LOGIC FOR DATACARD CONVERSION
.								CHANGED BACKGROUND COLOR DISABLED EDITTEXT BOXES ON SCREEN 10 (SUB-SCREENS)
.release	init	"3.71.9"			ASH 16SEP03 BROKER FILE CONVERSION
.release	init	"3.71.8.1"			ASH 18SEP03 SMALL FIX TO PREVENT DUPE KEYS IN NINORD4
.release	init	"3.71.8"			ASH 15SEP03 SAMPLE FORMAT CONVERSION
.release	        init	"3.71.7"			DMB 12SEP03 MOdified LM Changes from patch 3.71.5 - forcing contact to Joey Gamache Contact #17 on LM Orders
.release	init	"3.71.6"			ASH 27AUG03 FIXES FOR LM
.release	init	"3.71.5"			ASH 12AUG03 ENHANCEMENTS FOR LM + SMALL FIXES
.							REMOVE TIMER PATCH LOGIC NOW THAT PL/B HAS BEEN PATCHED
.							LIST MANAGEMENT ENHANCEMENTS
.release	init	"3.71.4"			ASH 14JUL03 SMALL FIXES
.							CORRECTED DISPLAY ISSUE WITH NET MINIMUMS
.							CORRECTED DEFAULT LCR STATUS PROBLEM
.							REPLACED ALL REFERENCES TO "F:\" WITH UNC NAME
.							AS PER DLH - FORCE ALL NEW LCRS TO HAVE SUBSTATUS OF "01" - FIRST REQUEST
.release	init	"3.71.3"			ASH 15MAY03 SMALL FIXES
.							ADDED PHONE NUMBER TO LCR REQUEST FORM
.							ADDED OMIT RESEARCH LOGIC
.							ADDED LOGIC TO ALLOW PRINTING OF 'ORDER PLACEMENT' SPREADSHEET 6/24/2003
.release	init	"3.71.2"			ASH 05MAY03 SMALL FIXES
.							ADDED LOGIC TO PREVENT INADVERTENT DELETION OF RECORDS FROM NINPRINTL WHEN CREATING A NEW LCR/PENDING RECORD AND APPROVING AT SAME TIME
.							ADDED LOGIC TO PREVENT FAX OPTION FOR ALL REPORTS ON SCREEN 5 EXCEPT LM BROKER REPORT
.							FIXED INCORRECT LOGIC IN ORDERLOADHISTSCREEN
.							RE-CENTERED Nord01eaStatBrkComp SO THAT IT DID NOT BLOCK VIEWING OF Nord001eaStatMlrComp (UNDOCUMENTED)
.							REMOVED TEST LANGUAGE FROM NSTA001EStatRecords (UNDOCUMENTED)
.release	init	"3.71.1"			ASH 21APR03 SMALL FIXES
.							ADDED 'EPSILON' EXCHANGE LOGIC TO FOLLOWING MAILERS:  5317(KCPT), 2598(KUHT), 1440(KPBS)
.							ADDED FIX TO DELETE ORDERS FROM LISTVIEWS ON SCREEN 7 WHEN ORDERS/LCRS ARE DELETED FROM CAMPAIGN
.							ADDED FIX TO VERIFY ROUTINE FOR INTERNAL/MAILER NOTES
.release	init	"3.71"			ASH 14APR03 SMALL FIXES
.							ADDED OPTION FOR FAX NUMBER KEY IN FOR GUARANTEE LETTERS
.							REMOVED ALL FORCING OF SHIPPING INFO PER JD
.							MODIFIED PATCH 3.68.8 TO FILTER OUT EXCLUSIVE LIST
.							ADDED OPTION TO ENTER 'REVISED REQUEST' LOOP IN SCREEN 7 - COMBINED WITH '2ND REQUEST' BUTTON
.							ADDED LOGIC TO TRAP I40 ERRORS WHEN LOADING SAMPLES
.							ADDED LOGIC TO ALLOW A YES-TO-ALL MESSAGE BOX FOR UPDATING OF NINPRINT WHEN USING PRINT BUTTON ON SCREEN 5
.release	init	"3.7"			ASH 09APR03 MAJOR OVERHAUL
.							ADDED LOGIC TO FILTER OUT CONTROL CHARACTERS IN ALL EDITTEXT BOXES
.							ADDED LOGIC TO PREVENT XML VERIFICATION TESTING OF ORDERS OLDER THAN 3/21/2003 (DATE XML LOGIC WAS INSTATED)
.							ADDED IDEA OF THE MONTH - EXCHANGE STATUS ON SCREEN 3
.release	init	"3.69.0"	ASH 31MAR03 SMALL PATCHES:  XML/FAX, TEST LOGIC FOR SAMPLE READS
.release	init	"3.68.9"	ASH 18MAR03 ADDED XML OPTION FOR TRIPLEX RECORDS
.release	init	"3.68.8"	ASH 13FEB03 IDEA OF THE MONTH - ALTER LCR FORM
.					PLUS SMALL PATCH IN SCREEN 5 EMAIL BUTTON TO ERADICATE I31 ERRORS
.release	init	"3.68.7"	ASH 09JAN03 SMALL FIXES
.					CTF 205 - MESSAGE BOXES FOR LIVE ORDERS OVER 50K W/O NETS
.					ADDED LOGIC TO PEEL OFF OFFER NAME FROM RTN COMPANY NAME WHERE APPLICABLE
.release	init	"3.68.6"	ASH 02JAN03 SMALL FIXES
.					REMOVED PATCH 3.68.4
.					ADDED MUTUAL EXCLUSIVITY TO SAMPLE NUMBERS AND STATUS OF "SAMPLE TO FOLLOW"
.release	init	"3.68.5"	ASH 26NOV02 CHANGED CALCULATIONS ON NSTA001DListViewTotal
.release	init	"3.68.4"	ASH 16OCT02 ENHANCMENT:  ADDED NEW EXCEPTION FOR XSTAT EXCLUSIONS
.release	init	"3.68.3"	ASH 10OCT02 ENHANCMENT:  ADDED SELECT LINE TO SCREEN 3
.release	init	"3.68.2"	ASH 09OCT02 FIX:  TDMCORD STUFF
.release	init	"3.68.1"	ASH 30SEP02 FIX:  SCREEN 10, SUB-SCREEN NOTES IS WRITING OUT INTERNAL/MAILER NOTES INCORRECTLY
.release	init	"3.68"	ASH 25SEP02 ENHANCEMENTS:  ADDED LOGIC TO ALLOW DELETION OF MAILDATE RECORDS FROM NINMDL
.					    ADDED FIX TO CLEAR HOLDMDATE WHEN CREATING NEW RECORDS!!
.					    MODIFIED LOGIC FOR CHECKING IF SENT TO TRIPLEX
.release	init	"3.67"	ASH 19SEP02 ENHANCEMENTS:  ADDED STOP BUTTON TO SCREEN 10, CAMPAIGN SUB-SCREEN
.					    ADDED LOGIC TO ALLOW USER TO DETERMINE PRINT FORMAT OF OUTSIDE LCRS - CTF CASE 180
.					    ADDED LOGIC TO ALLOW DYNAMIC DELETION OF LOL RECORDS FROM SCREEN 10, CAMPAIGN SUB-SCREEN, WHEN DELETING LAST ASSOCIATED PROJECTION RECORD
.					    ADDED LOGIC TO ALLOW USING EXCHANGE/RENT CHECKBOXES IN SEARCHES - CTF CASE 203
.release	init	"3.66"	ASH 16SEP02 ENHANCEMENTS:  ADDED LISTVIEW TO SCREEN 7 TO VIEW PROJECTION BREAKDOWNS
.				            FIXES:  CLEANED UP UNIVERSE FORMATTING ON NSTA001DListView
.release	init	"3.65"	ASH 09SEP02 ENHANCEMENTS:  ADDED INACTIVE BYTE TO SAMPLE FILE
.release	init	"3.64"	ASH 03SEP02 FIXES:  CORRECTED BUG WITH EXTRACTION OF CAMPAIGN GIFT CHANGE
.					    CORRECTED BUG WHICH RETURNED USER TO WRONG SCREEN WHEN RECO. QTY IS COMPARED ON SCREEN 10, SUB-SCREEN 2
.					    ADDED PACKAGE SELECTION CAPABILITY TO SCREEN 10 - SUBSCREEN CAMPAIGN COPY BUTTON
.					    ALLOW SEARCHES BY BROKER NUMBER ONLY
.release	init	"3.63"	ASH 14AUG02 FIXES:  Added logic to TIMEOUT to catch Projection records
.release	init	"3.62"	ASH 26JUL02 FIXES:  Corrected problem with Modify button on Screen 10 - Campaign
.					    Corrected logic so that LCR Form has Contact Name with appropriate spaces 07/29/2002
.					    Corrected logic so that View button on Screen 8 actually pulls correct value 07/31/2002
.release	init	"3.61"	ASH 22JUL02 FIXES
.release	init	"3.6"	ASH 31MAY02 ADDED LOGIC TO INCLUDE NINORD6 - CANCELLED LCR/PENDING RECORD FILE
.					ADDED LOGIC FOR NEW NORD002L PRTPAGE DOCUMENT
.release	init	"3.54"	 ASH 11JUN02 CTF CASE 129 - CHANGE DEFAULT OF PENDING TO LCR WHEN LCR IS ALREADY APPROVED
.release	init	"3.53.1"	 ASH 29MAY02 FIXES
.release	init	"3.53"	 ASH 29MAY02 TDMCORD FIX, LOGIC ADDED TO ALLOW PRINTING OF ALL COPIES FOR HOTORD
.release	init	"3.52"	 ASH 16MAY02 TDMCORD FIX, ENHANCEMENT TO DENIED PENDING ORDERS
.release	init	"3.51"	 ASH 01MAY02 NINFUL CONVERSION, ADDED BETA LOGIC, BUG FIXES
.release	init	"3.50"	ASH 03MAY2002 ADDED LOGIC TO ALLOW PDF OF HOTPRINTS
.release	init	"3.49.2"	 ASH 29APR02 ADDED BETA LOGIC, BUG FIXES
.release	init	"3.49.1"	 ASH 24APR02 ADDED BETA LOGIC, BUG FIXES
.release	init	"3.49"	 ASH 23APR02 ADDED LOGIC TO BATCH SPREADSHEET CREATION
.release	init	"3.48"	 ASH 17APR02 ADDED BETA LOGIC FOR JC, BUG FIXES
.release	init	"3.47"	 ASH 16APR02 ADDED BETA LOGIC FOR JC, BUG FIXES
.release	init	"3.46"	 ASH 10APR02 ADDED BETA LOGIC FOR JC
.release	init	"3.45"	 ASH 04APR02 LOGIC ADDED TO CORRECT PROBLEMS WITH TDMCORD RECORDS
.				      ADDED BETA LOGIC FOR JC - ALL STATS BETA LOGIC PRIOR TO THIS WAS FOR SK
.release	init	"3.44"	 ASH 02APR02 MORE BETA LOGIC ADDED
.				      MODIFIED HOW INFO.PLC DEALS WITH OBTAINING LIST # (INFO.PLS UPDATED AS WELL)
.				      ADDED F2 FUNCTION FOR LIST TYPE FIELD ON SCREEN 10, SUB-SCREEN 1
.				      ADDED DOUBLE-CLICK OPTION FOR NSTA0002ListView2
.				      ADDED DEFAULT VALUE FOR NET REQUESTED FIELD WHEN CREATING NEW PROJECTION RECORDS
.release	init	"3.43"	 ASH 22MAR02 MORE BETA LOGIC ADDED
.release	init	"3.42"	 ASH 22MAR02 ADDED LOGIC FOR BUSY BYTE FOR STATS FILE
.			  BUG FIXES/ENHANCEMENTS FOR BETA TESTING
.			  ALLOW	SELECTION OF MASTER PACKAGES
.release init	 "3.41"	  ASH 20MAR02 ADDED MORE STATS ENHANCMENTS BETA	TESTING	BEGINS
.			  REMOVE ANIMATE LOGIC TO SPEED	LOADS
.release init	 "3.4"	 ASH 27FEB02 ADDED MORE	STATS LOGIC, ADDED MASTER PACKAGE LOGIC, NUMEROUS SMALL	FIXES
.release init	 "3.3"	 ASH 20FEB01 Fixes and Enhancements
.release init	 "3.2"	 ASH 07SEP00 Inclusion of Data Entry into BETA testing
.release init	 "3.1"	 ASH 03AUG00 Enhancements to Campaign logic, Fixes, BETA testing of Campaign logic
.release init	 "3.0"	 ASH 06MAR00  Inclusion	of Campaign logic
.release init	 "2.0"	 ASH 01DEC99  Inclusion	of In-House LCR	logic
.release init	 "1.0"	 ASH 02DEC98  DEVELOPMENT RELEASE
.Set Vars used for About Box
	move	"NORDTEST.PLS",Wprognme
	move	"Order Program",Wfunction
	move	"Andrew	Harkins",Wauthor
	move	release,Wrelease
.begin patch 3.78.7
	Move	Reldate,Wreldate
.	move	"January 15, 2004",Wreldate
.end patch 3.78.7
	liston

.START PATCH 3.75 ADDED LOGIC
IntegralStoreDetail external "INT001A;IntegralStoreDetail"
IntegralTestDetail external "INT001A;IntegralTestDetail"
IntegralStoreCampaign external "INT001A;IntegralStoreCampaign"
IntegralTestCampaign external "INT001A;IntegralTestCampaign"
IntegralDeleteDetail external "INT001A;IntegralDeleteDetail"
IntegralCreateNewDetail external "INT001A;IntegralCreateNewDetail"
IntegralMoveCampaign external "INT001A;IntegralMoveCampaign"
.BELOW USED FOR TESTING ONLY!!!!
.integralsetup external "INT001A;integralsetup"
.END PATCH 3.75 ADDED LOGIC
.START PATCH 3.75.2 ADDED LOGIC
.EXTERNAL ROUTINES FROM	NLCR002L.PLC
PrintSingleLCR external "NLCR002L;PrintSingleLCR"
.END PATCH 3.75.2 ADDED LOGIC
.START PATCH 3.72 ADDED LOGIC
.EXTERNAL ROUTINES FROM	NDAT001a.PLC
SelectTestBase4 external "NDAT001a;SelectTestBase4"
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.68.9 ADDED LOGIC
.EXTERNAL ROUTINES FROM	NCMP0002.PLC
ExternalXMLCreate external "NORD0024;ExternalXMLCreate"
.END PATCH 3.68.9 ADDED LOGIC
.EXTERNAL ROUTINES FROM	NCMP0002.PLC
OrderCreateCampaign external "NCMP0002;CreateCampaign"
OrderCreateCampaignA external "NCMP002A;CreateCampaignA"
OrderCreateCampaignB external "NCMP002B;CreateCampaignB"
.START PATCH 3.71.3 ADDED LOGIC
.EXTERNAL ROUTINES FROM	NCMP0003.PLC
OrderCreateOrderSheet external "NCMP0003;CreateOrderSheet"
.END PATCH 3.71.3 ADDED LOGIC
.EXTERNAL ROUTINES FROM	INFO.PLC
OrderLoadForm external "INFO;LoadForm"
OrderDisplayMailer external "INFO;DisplayMailer"
OrderDisplayBroker external "INFO;DisplayBroker"
OrderDisplayShipto external "INFO;DisplayShipto"
OrderDisplayOwner external "INFO;DisplayOwner"
OrderDisplayList external "INFO;DisplayList"
OrderDisplayXstat external "INFO;DisplayXstat"
OrderDisplayMessage external "INFO;DisplayMessage"
OrderInfoClose external	"INFO;InfoClose"
.START PATCH 3.71.3 ADDED LOGIC
DisplayOmit external	"INFO;DisplayOmit"
.END PATCH 3.71.3 ADDED LOGIC
.START PATCH 3.72.3 ADDED LOGIC
OrderDisplaySearchTitle external "INFO;DisplaySearchTitle"
.END PATCH 3.72.3 ADDED LOGIC
.START PATCH 3.76.2 ADDED LOGIC
OrderDisplayFulfillment external "INFO;DisplayFulfillment"
.END PATCH 3.76.2 ADDED LOGIC
.EXTERNAL ROUTINES FROM	SPELLCHECK.PLC
OrderSpellCheck	external "SPELLCHECK;SpellCheck"
.EXTERNAL ROUTINES FROM	NPKG0001.PLC
OrderPackageLoadListViews external "NPKG0001;PackageLoadListViews"
OrderPackageDisableForm	external "NPKG0001;PackageDisableForm"
OrderPackageEnableForm external	"NPKG0001;PackageEnableForm"
OrderPackageLoadOK external "NPKG0001;PackageLoadOK"
OrderPackageSetListView	external "NPKG0001;PackageSetListView"
.EXTERNAL ROUTINES FROM	STATSCALC.PLC
OrderCalcStatNetNames external "STATSCALC;CalcStatNetNames"
OrderCalcStatNetNames2 external	"STATSCALC;CalcStatNetNames2"
OrderCalcStatExTotal external "STATSCALC;CalcStatExTotal"
OrderCalcStatRentTotal external	"STATSCALC;CalcStatRentTotal"
OrderCalcListCost external "STATSCALC;CalcListCost"
OrderCalcProdCost external "STATSCALC;CalcProdCost"
OrderCalcTotalCost external "STATSCALC;CalcTotalCost"
OrderCalcReturns external "STATSCALC;CalcReturns"
OrderCalcRevenue external "STATSCALC;CalcRevenue"
OrderCalcNetPlusMinus external "STATSCALC;CalcNetPlusMinus"
OrderCalcCostMember external "STATSCALC;CalcCostMember"
OrderCalcStatQty external "STATSCALC;CalcStatQty"
OrderCalcStatSetValues external	"STATSCALC;CalcStatSetValues"
OrderCalcStatGetValues external	"STATSCALC;CalcStatGetValues"
.OrderCalcPrepExcel external "STATSCALC;CalcPrepExcel"
OrderCalcCleanUpExcel external "STATSCALC;CalcCleanUpExcel"
.START PATCH 3.4 ADDED LOGIC
OrderCalcStatSetReturnValues external "STATSCALC;CalcStatSetReturnValues"
OrderCalcStatGetReturnValues external "STATSCALC;CalcStatGetReturnValues"
.END PATCH 3.4 ADDED LOGIC

.LCR Printing Variables
M01	INIT	"January"
M02	INIT	"February"
M03	INIT	"March"
M04	INIT	"April"
M05	INIT	"May"
M06	INIT	"June"
M07	INIT	"July"
M08	INIT	"August"
M09	INIT	"September"
M010	INIT	"October"
M011	INIT	"November"
M012	INIT	"December"
QTYMASK	INIT	"ZZZ,ZZZ,ZZ9"
QTYPRNT	DIM	11
.START PATCH 3.68.8 ADDED LOGIC
QTYPRNT2 DIM	50
.END PATCH 3.68.8 ADDED LOGIC
.Produces alert	box with 'Yes',	'No' buttons.  'No' button set as default
STYLE	INTEGER	1,"0x000104"
.Produces alert	box with 'Yes',	'No' buttons.  'Yes' button set	as default
STYLE1	INTEGER	1,"0x000004"
line1	dim	55
sampl	dim	14	  holds	sample file name if included
faxflag	form	1
hotflag	dim	1	  y=print now no = batch print/fax
.START PATCH 3.67 ADDED LOGIC
DevFlag form	1
ReprintFlag form 1
.END PATCH 3.67 ADDED LOGIC
LONGDIST DIM	1
recname	dim	50
FORMNAME DIM	30
FORMFILE FILE	UNCOMP
.START PATCH 3.71.8 REPLACED LOGIC
.dcx	init	".dcx"
dcx	init	".tif"
.END PATCH 3.71.8 REPLACED LOGIC
userinfo dim	500
userlogn dim	7
userlogw dim	7
Timer	Timer
.timetester dim	16
.START PATCH	3.78.2	ADDED LOGIC
NotesFlag init	"N"  // see if any notes were selected.  If not, do default
ClickFlag init	"N"    // used to see if columns have been reordered or not
.following variables used to scroll listview back to leftmost position - nord001D
LVM_SCROLL		INTEGER   2,"4116"  // LVM_FIRST + 20, 4116  // nord001D
SENDMSG   PROFILE   user32,SendMessageA,INT4,INT4,INT4,INT4,INT4  // nord001D
TOP	INTEGER	4   // nord001D
OBJHANDL  INTEGER   4   // nord001D
RET	INTEGER	4  // nord001D
DX	INTEGER	4   // nord001D
P_LV	ListView	^   // nord001D
RememberCol	form	2   // nord001D
RememberSort	form	2   // nord001D
.END PATCH	3.78.2	ADDED LOGIC
.Files to open
prfile	pfile
.tempfile file
preffile file
ORDPRINT ifile	keylen=6,fixed=696
ORDPRNTA afile	fixed=696
TDMCORD	IFILE	KEYLEN=6
TDMCDEL	IFILE	KEYL=6,FIX=76	      *HOLDS LR	NOT T/C'D TO TDMC.
TDMCsave IFILE	KEYL=6,var=43,dup	      *HOLDS LR	previously T/C'D TO TDMC.
LCRFAXFILE IFILE KEYL=6
.START PATCH 3.76.8 ADDED LOGIC
DMXFILE	IFILE	KEYL=6
.END PATCH 3.76.8 ADDED LOGIC
LRFILE	FILE
LOLFILE	FILE
CMPFILE	FILE
PTRFILE	FILE	^
LRINIT	FORM	"0"	.USED FOR TESTING, GIVING ABILITY TO TURN OFF ALL LOGIC	CONNECTED WITH LRFILE
height	form	9
width	form	9
.Used to keep track of tabs during Updating and	Saving
TabNum	form   "01"
.Tab6Num form	"01"
Tab10Num form	"01"
.TabTESTNum form  "01"

.FLAGS
ExitFlag init	"Y"	.For Orders
ExitFlag2 init	"Y"	.For Campaigns
ExitFlag3 init	"Y"	.For LOL Records
ExitFlag4 DIM	%1	.For Packages
	MOVE	YES,EXITFLAG4
ExitFlag5 init	"Y"	.For STATS Records
NewFlag	init	"N"	.For Orders
NewFlag2 init	"N"	.For Campaigns
NewFlag3 init	"N"	.For List of Lists
NewFlag5 init	"N"	.For STATS Records
CanFlag	init	"N"
PrtFlag	init	"N"
ReturnFlag init	"N"
ReturnFlag2 init "N"	.For Campaigns
ReturnFlag3 init "N"	.For LOL
ReturnFlag5 init "N"	.For STATS Records
GuarFlag init	"N"
GuarFlag2 init	"N"
OStatFlag init	 "BQXx"
HoldFlag form	"00"	.Holds value for Password clearance
SecFlag	init	"N"	.Allows	Order fixes - needs Password
SecFlag3 init	"N"	.Allows	LOL Record fixes - needs Password
.osflag	 form	1	.1=win 95,98, 2=NT
UPSW	INIT	"N"	.UPDATE	SWITCH IF CHANGES WHERE	MADE
.			.USED TO CHANGE	TYPIST INITIALS.
.LCRFlag form	 1
View3Flag form	"1"	.used to determine which ListView object on Screen 3 has focus
.START PATCH	3.78.2	REMOVED LOGIC
.View5Flag form	"3"	.used to determine which ListView object on Screen 5 has focus - Default is ListView3 as per Time Saviour meeting 12/16/1999 ASH
.END PATCH	3.78.2	REMOVED LOGIC
View7Flag form	"1"	.used to determine which ListView object on Screen 7 has focus - Default is ListView
View10Flag form	"1"	.used to determine which ListView object on Screen 10/4	has focus - Default is ListView
StopFlag init	"N"	.used to break out of a	Query on Screen	5
.START PATCH 3.67 ADDED LOGIC
Stop3Flag init	"N"	.used to break out of a	Query on Screen	10, Campaign sub-screen
.END PATCH 3.67 ADDED LOGIC
InitFlag init	"N"	.used to determine which Initials to place on LCR Request Form - "N" defaults to ODOWJ
HistFlag form	"0"	.used to determine if LCR/Pending has already been flagged as "NO EXCHANGE HISTORY"
PrintFlag form	"01"	.used to determine which Printer is used, default to Laser 3
MaskFlag form	"01"	.used to determine which File Maintenance program you are in
Order7Flag form	"0"	.used to determine if Save routine was completed when creating NINORD records off of NINLOL records on Screen 7
.START PATCH 3.66 ADDED LOGIC
.I must have following flag independent of View7Flag, as this ListView does not interact with ListViews associated with View7Flag
Proj7Flag form	"0"	.used to determine if viewing via Projection ListView on Screen 7
taskname2 dim	200
colornum form	24
.END PATCH 3.66 ADDED LOGIC
.START PATCH 3.72.2 ADDED LOGIC
colordim dim	8
colordim2 dim	8
.END PATCH 3.72.2 ADDED LOGIC
.START PATCH 3.49.1 ADDED LOGIC
Stats10Flag form "0"	.used to determine if Save routine was completed when modifying Projection records en masse on Screen 10, Campaign sub-screen
.END PATCH 3.49.1 ADDED LOGIC
.FIELD FLAGS
.Flags are set when ChangeEvent	occurs for particular field, thus setting
.flag to write to TDMC LOL file.  QtyFlag has a	secondary feature in that
.it can	prevent	unnecessary processing for OQTY, thus saving time.
.START PATCH 3.76.7 REPLACED LOGIC
.I am keeping QtyFlag as it has secondary uses.
.KeyFlag	init	"N"	.OMLRKEY
.ListFlag init	"N"	.O2DES
OMLRKYHold dim	12	.OMLRKY Hold value
SelectHold dim	75	.Select Hold value
OQTYHold form	13	.OQTY Hold value - Note that this field reflects possible field increase!!
.END PATCH 3.76.7 REPLACED LOGIC
QtyFlag	init	"N"	.OQTY/OEXQTY
QtyFlag2 init	"N"	.NCMPQTY,NCMPNETQTY
.QtyFlag3 init	 "N"	 .NLOLQTY,NLOLNETQTY
.Flags for fields other	than those affecting TDMC LOL file
POFlag	init	"N"	.OMLRPON
SpecFlag init	"N"	.Used to signify there are External Special Instructions
SpecFlag2 init	"N"	.Used to signify External Special Instructions Changed
Spec2Flag init	"N"	.Used to signify there are Internal Notes
Spec3Flag init	"N"	.Used to signify there are Mailer Notes
XFlag	init	"N"	.Used to signify XStat changed and therefore Update/Quit buttons are active
ListFlag2 init	"N"	.LIST-tripped by List_LostFocusEvent
CampFlag init	"N"	.Used to determine if Campaign Number changed
CampOp	form	"0"	.Used to determine if a	Copy or	a Move is desired
NetFlag	init	"N"	.Used to determine if Net Qty/Per has changed on Screen	1/2
NetFlag3 init	"N"	.Used to determine if Net Qty/Per has changed on Screen	8
CampPFlag init	"N"	.Used to determine if Campaign has Associated Projections
.ADDED 10.01.2001 FOR RENT TO EXCHANGE FIASCO
Rent2ExFlag dim	1
.START PATCH 3.4 ADDED VAR
LOLGrossFlag	form	9
.END PATCH 3.4 ADDED VAR
.START PATCH 3.46 ADDED VAR
StatNoteFlag	init	"N"
StatNoteFlag2	init	"N"
.END PATCH 3.46 ADDED VAR
.START PATCH 3.77 ADDED LOGIC
LRefFlag form	1
.END PATCH 3.77 ADDED LOGIC
.AAMKEY	CRITERIA
AKey1	init	"01X"
AKey2	init	"01F"
AKey3	init	"02F"
AKey1A	init	"01L"
AKey2A	init	"02L"
AKey4A	init	"04R"
AKey5A	init	"05X"
filler	init	"000000000"
editmsk	init	"ZZZ.99"
.START PATCH 3.72 ADDED LOGIC
editmsk2 init	"ZZ,ZZZ.99"
.END PATCH 3.72 ADDED LOGIC
F32	form	3.2
.START PATCH 3.72.4 ADDED LOGIC
TempTop	form	9
TempLeft form	9
SerTop	form	9
SerLeft form	9
.Parameters for Search Screen
MaxTHeight form	"600.0000"
MinTHeight form	"88.0000"
MaxTWidth form	"2000.0000"
MinTWidth form	"360.0000"
SrchWinFlag form 1
.END PATCH 3.72.4 ADDED LOGIC
.START PATCH 3.72 ADDED LOGIC
N52	form	5.2
N52A	form	5.2
N52B	form	5.2
N42	FORM	4.2
.Parameters for Parent Screen
.MaxHeight form	"804.0000"
.MinHeight form	"438.0000"
.MaxWidth form	"905.0000"
MinWidth form	"640.0000"
.Parameters for Select Screen
MaxSHeight form	"300.0000"
MinSHeight form	"85.0000"
MaxSCHeight form "280.0000"	.Max Height with Caption
MinSCHeight form "68.0000"	.Min Height with Caption
MaxSWidth form	"500.0000"
MinSWidth form	"405.0000"
MinSIWidth form	"200.0000"	.Min Width in Inquiry Mode
.Parameters for ListView Object on Select Screen
..MaxLVHeight form "600.0000"
..MinLVHeight form "100.0000"
.MinLVHeight form "160.0000"
..MaxLVWidth form	"600.0000"
.MinLVWidth form	"480.0000"
.Vars used for floating Select Screen
SelLeft	form	9
SelTop	form	9
Sel2Left form	9
Sel2Top	form	9
Sel8Left form	9
Sel8Top	form	9
SelTopC	form	9
SelLeftC form	9
Sel8TopC form	9
Sel8LeftC form	9
SuiteFlag form	1
Suite8Flag form	1
Suite10Flag form 1
SrchSelFlag form 1
HoldSelName	dim	75
.END PATCH 3.72 ADDED LOGIC
badstat	init	"B*PIN"
GoodStat init	"xp"
.revtyps init	 "AA FU	CF"	       ALLOWED REVISION	?
.start patch 3.79.1
.revtyps	init	"DH DM DMB JD "	     ALLOWED REVISION ?
revtyps	init	"DH CF AA JD JG "	     ALLOWED REVISION ?
.End patch 3.79.1
ListMgmt init	"SK AA KE JG JF"	.Used to determine List	Managment
Carr	init	0x7f
.hexeight integer 4,"4294967295"
typeNo	init	0x104
JDate	form	5
MailDate form	5
newdate1 dim	10	mm/dd/ccyy
newnum	dim	13
area	dim	3
SrchLast form	5	.When searching	start with End/Beginning of File??
.Length	of record plus space for B1 and	space for "/"
hold	dim	408	.FOR ORDER RECORD - WHEN IN MODIFY MODE, THIS VAR CAN ONLY BE USED FOR CURRENTLY MODIFIED RECORD!!!!!
hold2	dim	752	.FOR SPECIAL INSTRUCTIONS , 674	of which is modifiable,	78 of which is for XSTAT
.START PATCH	3.78.3	REPLACED LOGIC
.hold3	dim	390	.FOR NOTES
hold3	dim	530	.FOR NOTES
.END PATCH	3.78.3	REPLACED LOGIC
.START PATCH	3.78.3	ADDED LOGIC
NDATE2	dim	8
NTIME2	dim	4
.END PATCH	3.78.3	ADDED LOGIC
hold6	dim	535	.FOR CAMPAIGN
hold7	dim	516	.FOR LOL
hold8	dim	516	.FOR LOL
hold2p	dim	800	.FOR PACKAGES
hold10	dim	519	.FOR STATS
HoldGuar dim	1	.FOR GUARCODE
HoldStat dim	1	.FOR OSTAT
HoldMlr	dim	4	.For modification of AamKey, NORDFLD1 -	Mailer
HoldList dim	6	.For modification of AamKey, NORDFLD2 -	List
HoldPO	dim	12	.For modification of AamKey, NORDFLD3 -	Mlr PO
HoldBrk	dim	4	.For modification of AamKey, NORDFLD4 -	Broker
HoldCamp dim	6	.For modification of Secondary ISAM key	- Camp.	Number
.START PATCH 3.72 ADDED LOGIC
HoldLR	dim	6	.For loading of Select Prices
.END PATCH 3.72 ADDED LOGIC
HoldOCLRSTAT dim 1
HoldOCLRINIT dim 3
HoldOCLRDTEC dim 2
HoldOCLRDTEY dim 2
HoldOCLRDTEM dim 2
HoldOCLRDTED dim 2
HoldOHIST dim	1
HoldORCODE dim	1
.START PATCH 11FEB2002 ADDED LOGIC
HoldMDate dim	8
.END PATCH 11FEB2002 ADDED LOGIC
.START PATCH 3.49.1 ADDED LOGIC
HoldLCRDate dim	8
.END PATCH 3.49.1 ADDED LOGIC
.START PATCH 3.75.7 REMOVED LOGIC
.Hold2Mlr dim	4	.For modification of AamKey, NCMPFLD2 -	Camp. Mailer
.Hold2Brk dim	7	.For modification of AamKey, NCMPFLD3 -	Camp. Broker/Cnt
.Hold2Name dim	45	.For modification of AamKey, NCMPFLD1 -	Camp. Name
.Hold2PO	dim	12	.For modification of AamKey, NCMPFLD4 -	Camp. Mlr PO
.Hold2Plan dim	2	.For modification of AamKey, NCMPFLD5 -	Camp. Planner
.END PATCH 3.75.7 REMOVED LOGIC
Hold3Camp dim	6	.For modification of AamKey, NLOLFLD2 -	Camp. Num
Hold3List dim	6	.For modification of AamKey, NLOLFLD2 -	List Num
Hold3LOL dim	6	.FOR Hidden LOL	Num
Hold3LCR  dim	6	.FOR LR/LCR Num
Hold3Mlr dim	4	.For LOL Searches - Mailer
Hold3PO	dim	12	.For LOL Searches - PO
Hold3Pack dim	2	.For LOL Searches - Package
Hold3Plan dim	2	.For LOL Searches - Planner
Hold3Code dim	1	.For Busy Byte
Hold5From dim	8	.For Screen 5 From Date
Hold5To	dim 	8	.For Screen 5 To Date
Hold5Date form	"0"	.For Screen 5 Date Selection - Record Date versus Clearance Date
.START PATCH 3.4 ADDED LOGIC
HistCamp	dim	6
HistPack	dim	6
.START PATCH 3.64 ADDED LOGIC
HistPkg		dim	6
.END PATCH 3.64 ADDED LOGIC
HistFatigue	dim	6
HistApply	form	1
HistRec		form	1
HistFatForm	form	1.6
.START PATCH 3.48 REPLACED LOGIC
.HistFatResult	form	3.6
HistFatResult	form	4.6
HistFatResult2	form	4.6
.END PATCH 3.48 REPLACED LOGIC
.Campaign Projection Totals
StatsCostTotal	form	13.2
StatsUnivTotal	form	13
StatsRecoTotal	form	13
StatsRecoTotalB	form	13.4
StatsRecdTotal	form	13
StatsRtnsTotal	form	13
StatsLCPMTotal	form	13.2
StatsNetTotal	form	13
StatsNetTotalB	form	13.4
StatsRevTotal	form	13
StatsProdTotal	form	13
StatsLCstTotal	form	13
StatsTotCstTotal form	13
StatsNetPlusTotal form	13
StatsRRTotal	form	3.4
StatsHoldTotal	form	13.2
.END PATCH 3.4 ADDED LOGIC
.Vars used to calculate	Campaign Totals
CampCont form	13
CampTests form	13
CampTotal form	13
.Net Totals
CampNetCont form 13
CampNetTests form 13
CampNetTotal form 13
.key holds longest possible Aam
key	dim	45
holdkey	dim	6
key2	dim	45
holdkey2 dim	6
key3	dim	45
holdkey3 dim	12
.START PATCH 3.42 ADDED	VAR
holdkey4 dim	9
.END PATCH 3.42	ADDED VAR
.START PATCH 3.48 ADDED VAR
HoldPackage dim	6
.END PATCH 3.48 ADDED VAR
temmlr	dim	4
temmlr2	dim	4
xrefflag dim	2
.Vars used for printing
page	form	9
EditMask init	"ZZZ,ZZZ,ZZZ"
EditQuan dim	11
Quan	form	9
.Vars used for Report Screen
RptCan	dim	1
.Print button on Screen	5 will dynamically produce Report selection form.
.These vars hold values	of last	report/printer requested,
.allowing users	to retain previous values.
RptType	form	"001"
FromBrk	dim	4
ToBrk	dim	4
Preview	form	1
Default	form	1
Select	form	1
FromDate dim	8
ToDate	dim	8
.
date	dim	8
.Vars used to determine	MailerStatSalesMssg
salesper dim	15
sqty	form	9
sentry	form	5
.OrderInfoString dim	 47
MDLTEXT1 dim	70
MDLTEXT2 dim	68
MDLTEXT3 dim	70
MDLTEXT4 dim	70
str45a	dim	45
TDMC0	INIT	"TDMC"
TDMC1	INIT	"TRIPLEX"
form122	form	12.2	      used with	tinvdd
TDMCAMT	FORM	8.2	     used with tinvdd
.START PATCH 3.75.4 ADDED LOGIC
str6a	dim	6
.END PATCH 3.75.4 ADDED LOGIC

...EXTRA EXCHANGE VARS
.----------------------
.
.START PATCH 3.76 REPLACED LOGIC
.MALR1	 DIM	   4		MAILER1	HOLDS MAILER NUMBER FOR	MAILER READ &
.MALR2	 DIM	   4		MAILER2	MAILER NUMBER COMPARISON.
MALR1	 DIM	   6		MAILER1	HOLDS MAILER NUMBER FOR	MAILER READ &
MALR2	 DIM	   6		MAILER2	MAILER NUMBER COMPARISON.
.END PATCH 3.76 REPLACED LOGIC
EFLAG	dim	1	     mlr switch	CONSTANT EXCHANGE
FMESG	dim	22
BLANK5	dim	5
BLANK9	form	9
WORKX1	form	10
WORKX2	form	10
.START PATCH 3.79.2
HOldCompExcl	Dim	1                     .save company exclusive byte
.End PATCH 3.79.2
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.WORKX3	dim	4
.WORKX4	dim	4
WORKX3	dim	6
WORKX4	dim	6
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
EXSW	 DIM	   1		HOLDS "Y" IF THIS IS AN	EXCHANGE ORDER USED
.				IF ERASE ACTIVATED DURING ORDER	ENTRY TO
.				CORRECT	THE ENTRY NUMBER IN NINXNUM FILE.
lolsw	dim	1	 used tdmc lol file.
loltype	dim	1	 identify correction/canc.   7/19/95
mod	form	2	.used for Order	File Maintenance
LCRMod	form	"00"
ORPCODE	dim	1		  .1st byte of print file
rprtflag dim	1
rprtcode init	"RXQ"
.
. NIN SPECIAL INSTRUCTION CODES, NET NAME%, RUNNING CHARGES
.
nnet0	 init	   "00 00  0.00	      0"
NNET3	 INIT	   "03 85  0.00	      0"
NNET5	 INIT	   "05 85  4.00	  50000"
NNET6	 INIT	   "06 85  5.00	  50000"
NNET7	 INIT	   "07 85  6.00	  50000"
NNET8	 INIT	   "08 85  4.50	  50000"
NNET9	 INIT	   "09 85  4.00	 100000"
NNET10	 INIT	   "10 85  3.50	  50000"
NNET11	 INIT	   "11 85  7.50	  50000"
NNET12	 INIT	   "12 85  6.00	  50000"
NNET13	 INIT	   "13 85  6.00	  50000"
NNET14	 INIT	   "14 85  5.00	 100000"   .volume discount
NNETS	 FORM	   "12"
NnINFO	 DIM	   19
nnmin	 dim	   7
nnprc	 dim	   2
mfp	 form	   2
mll	 form	   2

CALCQTY	 FORM	   15
CALCNET	 FORM	   15
CALCPER	 FORM	   7.4
WORSTPER FORM	   7.4
BESTPER	 FORM	   7.4
GUARNAME DIM	   25			    CONTACT NAME FOR GUARANTY LETTER
.START PATCH 3.71 ADDED LOGIC
GUARFAX	 DIM	   11
.END PATCH 3.71 ADDED LOGIC
.Vars used by Nord001f - OrderSamples
.dcxpath init	 "G:\data\samples\"
dcxpath	init	"\\nins1\e\data\samples\"	."
SmpPage	form	5
SmpPage2 form	5
SmpFile	pfile
SmpScale form	3

.Vars used for Search function
ordsel	form	1
readflag form	1
.START PATCH 3.67 ADDED LOGIC
ExcSel	form	1	.0 = No Pref. 1 = Exchange, 2 = Rent, 3 = Split
.END PATCH 3.67 ADDED LOGIC

.Masked	fields
MaskOMLRNUM	dim	4
MaskOBRKNUM	dim	4
MaskOBRKCNT	dim	3
MaskOMLRPON	dim	12
MaskOMLRKY	dim	12
MaskOFOCODE	dim	2
MaskORTNDTEC	dim	2
MaskORTNDTEY	dim	2
MaskORTNDTEM	dim	2
MaskORTNDTED	dim	2
MaskOMDTEC	dim	2
MaskOMDTEY	dim	2
MaskOMDTEM	dim	2
MaskOMDTED	dim	2
MaskOODNUM	dim	7
MaskORTNNUM	dim	4
MaskOSHP	dim	2
MaskDESC002	dim	674
MaskOCOCODE	dim	2
.Added 11/15/02	as per Idea of the Month - ASH
MaskSubStat	form	2	.holds Sub-Status:  NORD4FLD,NORD5FLD
.End Add
.START PATCH 3.72.3 ADDED LOGIC
MaskOLNUM	dim	6
.END PATCH 3.72.3 ADDED LOGIC
.START PATCH 3.78	ADDED LOGIC
ReturnValue	form	1
.END PATCH 3.78	ADDED LOGIC
.START PATCH 3.78.4 ADDED LOGIC
.VARIABLES USEFUL FOR HOLDING FULFILLMENT VALUES - PULLED FROM ORIGINAL NFULDD.INC
NFULNUM		dim	6
NFULCOMP	dim	55
.END PATCH 3.78.4 ADDED LOGIC
.Pointers for accessing	ComboBoxes
ComboPtr ComboBox	^
DimPtr	Dim	^
.Pointers for Update NINPRINTL
DimPtr1	Dim	^
DimPtr2	Dim	^
DimPtr3	Dim	^
.
DimPtr4	Dim	^
DimPtr5	Dim	^
.Pointers used for updating objects
FrmPtr	form	^
FrmPtr1	form	^
EditPtr	EditText	^
EditPtr1 EditText      ^
EditPtr2 EditText      ^
LstVwPtr ListView	^
.Objects that are created and destroyed	dynamically
.Note that this	EditText, generally, fills spot	for ErrorMssgStat4!!
EditTextBoxes	EditText (4)
Buttons		Button	(5)
.START PATCH	3.78.2	REPLACED LOGIC
.CheckBoxes	CheckBox (2)
CheckBoxes	CheckBox (3)
.END PATCH	3.78.2	REPLACED LOGIC
ComboBoxes	ComboBox (4)
StatTextBoxes	StatText (8)
ListViews	ListView (2)
.START PATCH 3.41 ADDED	VARS
DataLists	DataList (2)
.END PATCH 3.41	ADDED VARS
.START PATCH 11FEB2002 ADDED LOGIC
MDateWindow	Window
.END PATCH 11FEB2002 ADDED LOGIC
.OrderInfoListView ListView
.OrderInfoEditText EditText
.Following Collection is used to dynamically destroy objects listed above
.Each time a form is loaded with those objects,	dump them into this collection
.and then destroy whole	collection when	needed
ObjectColl	Collection
.Objects used by OptionsOrd.plf	(a generic form)
.OptionsArrayEx	dim	x(y,z)
.Following Array will allow:  y	rows - representing each Screen
.			      z	columns	- representing # of preferances	for each Screen
.			      x	length of each field in	each Screen
.
OptionsArr1 CONST	"10"
OptionsArr2 CONST	"5"
OptionsArrSize CONST	"5"
OptionsArray dim	OptionsArrSize(OptionsArr1,OptionsArr2)
.Screen	1
Options1Coll Collection
OptionsScreenInit ComboBox
OptionsFaxLONo	CheckBox
.Screen	2
Options2Coll Collection
.Screen	3
Options3Coll Collection
.Screen	4
Options4Coll Collection
.Screen	5
Options5Coll Collection
OptionsScreen5BrkFilter	CheckBox
OptionsScreen5EMailOption CheckBox
OptionsScreen5FileDefault CheckBox
.Screen	6
Options6Coll Collection
.OptionsScreen6Init CheckBox
.Screen	7
Options7Coll Collection
.START PATCH 3.66 ADDED LOGIC
OptionsScreen7Proj CheckBox
.END PATCH 3.66 ADDED LOGIC
.Screen	8
Options8Coll Collection
.OptionsScreen8OrderSearch CheckBox
.Screen	9
Options9Coll Collection
OptionsScreen9View CheckBox
.Screen	10
Options10Coll Collection
OptionsScreen10View CheckBox
OptionsScreen10Usage CheckBox

ScrRight form	4
ScrBottom form	4
MouseForm form	10
FarRight form	4
FarBottom form	4
T1	form	4
L1	form	4
.Variables used	for Timestamp Loops with HotOrd
HotFax	form	"0"	.Used to limit Fax option to selected users
timestamp1 dim	16
timestamp2 dim	16
time1	form	16
time2	form	16
time3	form	16
.
.START PATCH 3.41 REMOVED LOGIC
..Constants for	Animate	Icon
..These	coordinates will place Icon directly below the New button on both NORDMSK1, NORDMSK2, NORDMSK3
.AniH	 init	 "72"
.AniV	 init	 "122"
.END PATCH 3.41	REMOVED	LOGIC

.Colors
white	color
grey	color
RED	color
BLACK	color
blue	color
green	color
yellow	color
orange	color

.Define	Fonts to be used
font1	font
font2	font
font3	font
font4	font
font5	font

.sfile	 sndfile
.Set Up	Menu Bar
mFile	menu
mEdit	menu
mOptions menu
mReports menu
mSecurity menu
mHelp	menu
.Menu Bar for Report Screen
mRSearch menu
.Set Up	SubMenu	for Options
sSearch	submenu

.Present Data for Menu Bar
FData	init	"&File;&Print;-;E&xit"
EData	init	"&Edit;<1&Undo;-;<2&Cut	Ctrl+X;<3&Copy Ctrl+C;<4&Paste Ctrl+V;<5&Delete;-;<6&Select All"
OData	init	"&Options;&Search-F2;-;&View Samples;View &Nets;-;&Preferences"
.START PATCH 3.72.7 REPLACED LOGIC
.RData	init	"&Reports;&Campaign;&LCR"
RData	init	"&Reports;&Campaign;&LCR;L&M Denied"
.END PATCH 3.72.7 REPLACED LOGIC
SEData	init	"Security;FIXORD mode;FIXBUSY"
HData	init	"&Help;LCR Help;-;&About"

.Present Data for SubMenu
.START PATCH 3.66 REPLACED LOGIC
.SData	init	";&Broker;&List;&Mailer;&Ship-To;&Campaign"
SData	init	";&Broker;&List;&Mailer;&Ship-To;&Campaign;&Owner"
.END PATCH 3.66 REPLACED LOGIC

VT_BOOL	EQU 11
OTRUE	variant
OFALSE	variant
OBOOL	variant
IntIndex integer 4
IntIndex2 integer 4
IntIndex3 integer 4
IntIndex4 integer 4
IntIndex5 integer 4
IntIndex6 integer 4
IntIndex7 integer 4
ColHeads automation
ColHead	automation
ListIts	automation
.START PATCH 3.66 ADDED LOGIC
ListIts2 automation
.END PATCH 3.66 ADDED LOGIC
ListIt	automation
SubIt	automation

formstuff
.Declare forms,	Always declare child forms first
.START PATCH 3.41 REMOVED LOGIC
.animicon plform Animate      	 .CONTAINS ALL THE ICONS
.END PATCH 3.41	REMOVED	LOGIC
brk	plform	Search
.inf	 plform	 Nord001h	 .OrderInfo
rpt	plform	Report
rpt2	plform	Report2
opt	plform	OptionsOrd
smp	plform	nord001f
pass	plform	Passwrd
mss1	plform	Error
abt	plform	About

x	plform	nord0001
Nord001b plform	Nord001b 	.Must be loaded	before Nord001a	as Nord001a references objects on Nord001b - ASH
.START PATCH 3.72 ADDED LOGIC
Nord001a2 plform Nord01a2	.Must be loaded	before Nord001a	as Nord001a references objects on Nord01a2 - ASH
.END PATCH 3.72 ADDED LOGIC
Nord001a plform	Nord001a
.Nord001a plform	Nord001aNEW
.START PATCH 3.72 ADDED LOGIC
Nord001a1 plform Nord01a1
Nord008a1 plform Nord08a1
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.72.4 ADDED LOGIC
SearchForm plform Nord001t
.END PATCH 3.72.4 ADDED LOGIC
Nord001c plform	Nord001c
Nord01ea plform	Nord01ea
.START PATCH 3.75 TEMP PATCH FOR TESTING!!!!
Nord01ec plform	Nord01ec
.Nord01ec plform	Nord01ecNEW
.END PATCH 3.75 TEMP PATCH FOR TESTING!!!!
Nord001g plform	Nord001g
Nord001i plform	Nord001i	.OrderXRef
Nord001k plform	Nord001k	.OrderXBal
.Nord001m plform Nord001m
.x	 plform	 nord0001
.These forms reference nord0001	and so must be loaded last
.START PATCH 3.75 TEMP PATCH FOR TESTING!!!!
NordMSK1 plform	NordMSK1
NordMSK2 plform	NordMSK2
NordMSK3 plform	NordMSK3
Nord001d plform	Nord001d
Nord01eb plform	Nord01eb	.References NORDMSK3 and so must be defined afterwards
.NordMSK1 plform	NordMSK1NEW
.NordMSK2 plform	NordMSK2NEW
.NordMSK3 plform	NordMSK3NEW
.Nord001d plform	Nord001dNEW
.Nord01eb plform	Nord01ebNEW	.References NORDMSK3 and so must be defined afterwards
.END PATCH 3.75 TEMP PATCH FOR TESTING!!!!
Nsta0001 plform	Nsta0001
Nsta0002 plform	Nsta0002
Nsta001a plform	Nsta001a
Nsta001b plform	Nsta001b
Nsta001c plform	Nsta001c
Nsta001d plform	Nsta001d
Nsta001e plform	Nsta001e
	winhide

.Load Forms, Always load parent	form first
	formload x
	formload NordMSK1,nord0001     .Mask for Order File Maintenance
	formload NordMSK2,nord0001     .Mask for Campaign File Maintenance
	formload NordMSK3,nord0001     .Mask for LOL File Maintenance
	formload Nord001a,nord0001
.START PATCH 3.72 ADDED LOGIC
	formload Nord001a2,nord0001
	formload Nord001a1,nord0001
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.72.4 ADDED LOGIC
	formload SearchForm,nord0001
.END PATCH 3.72.4 ADDED LOGIC
	formload Nord001b,nord0001
	formload Nord001c,nord0001
	formload Nord001d,nord0001
	formload Nord01ea,nord0001
	formload Nord01eb,nord0001
	formload Nord01ec,nord0001
.START PATCH 3.72 ADDED LOGIC
	formload Nord008a1,nord0001
.END PATCH 3.72 ADDED LOGIC
	formload Nord001g,nord0001
	formload Nord001i,nord0001
.	 formload Nord001j,nord0001
	formload Nord001k,nord0001
.	 formload Nord001l,nord0001
.	 formload Nord001m,nord0001
	formload Nsta0001,nord0001
	formload NSta0002,nord0001
	formload Nsta001a,nord0001
	formload Nsta001b,nord0001
	formload Nsta001c,nord0001
	formload Nsta001d,nord0001
	formload Nsta001e,nord0001
	formload abt
	formload mss1
	formload pass
	formload smp
	formload opt
	formload rpt2
	formload rpt
.	 formload inf
	formload brk
.START PATCH 3.41 REMOVED LOGIC
.	 formload animicon
.END PATCH 3.41	REMOVED	LOGIC
.BELOW USED FOR TESTING ONLY!!!!
.tester
.	call	integralsetup
.Load OrderInfo
	call	OrderLoadForm
.Load Package Screen
	call	OrderPackageLoadListViews using	NORD0001,C1
.START PATCH 3.72.4 ADDED LOGIC
	call	OrderSetSearchDefault
.END PATCH 3.72.4 ADDED LOGIC
.
.START PATCH 3.7 ADDED LOGIC
	setprop	NORD01EA,CauseValid=1
	setprop	brk,CauseValid=1
	setprop	rpt,CauseValid=1
	setprop	rpt2,CauseValid=1
	setprop	opt,CauseValid=1
	setprop	smp,CauseValid=1
	setprop	pass,CauseValid=1
	setprop	mss1,CauseValid=1
	setprop	abt,CauseValid=1
	setprop	x,CauseValid=1
	setprop	Nord001b,CauseValid=1
	setprop	Nord001a,CauseValid=1
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord001a2,CauseValid=1
	setprop	Nord001a1,CauseValid=1
	setprop	Nord008a1,CauseValid=1
.END PATCH 3.72 ADDED LOGIC
	setprop	Nord001c,CauseValid=1
	setprop	Nord01ea,CauseValid=1
	setprop	Nord01ec,CauseValid=1
	setprop	Nord001g,CauseValid=1
	setprop	Nord001i,CauseValid=1
	setprop	Nord001k,CauseValid=1
	setprop	NordMSK1,CauseValid=1
	setprop	NordMSK2,CauseValid=1
	setprop	NordMSK3,CauseValid=1
	setprop	Nord001d,CauseValid=1
	setprop	Nord01eb,CauseValid=1
	setprop	Nsta0001,CauseValid=1
	setprop	NSTA0002,CauseValid=1
	setprop	Nsta001a,CauseValid=1
	setprop	Nsta001b,CauseValid=1
	setprop	Nsta001c,CauseValid=1
	setprop	Nsta001d,CauseValid=1
	setprop	Nsta001e,CauseValid=1
.END PATCH 3.7 ADDED LOGIC
.Remove	objects	associated solely with Search function from the	plform collection
.so that when tabbing between screens they do not appear
	listdel	Nord001a,Nord001AEditRange,Nord001AComboEOF,Nord001AComboType,Nord001AStatRange
.START PATCH 3.41 REMOVED LOGIC
..Dynamically reset Animate as same size as nord0001
.	 getprop nord0001,height=H
.	 setprop Animate,height=H
.	 getprop nord0001,width=V
.	 setprop Animate,width=V
.END PATCH 3.41	REMOVED	LOGIC

.Must clear Resize Event which was tiggered when Animate was resized,
.as this is where the AnimateIt	subroutine sits.
	clearevent
.Trap for Errors
	trap	NOFILE giving ERROR if RANGE
	trap	INT giving ERROR if INT
.Load Media ComboBox
	move	C1,N2
	MOVE	C0,N3
	move	"  ",MEDIA
.Must delete blank items entered to ensure adequate space
	deleteitem Nord001bComboMedia,0
	deleteitem Nord01eaComboMedia,0
	loop
		LOAD	MEDIA FROM N2 OF MED20,MED0,MED1,MED2,MED3,MED4,MED5:
			MED6,MED7,MED8,MED9,MED10,MED11,MED12,MED13,MED14:
			MED15,MED16,MED17,MED18,MED19,MED21,MED22:
			MED23,MED24,MED25,MED26,MED27,MED28,MED29
		insertitem Nord001bComboMedia,N3,MEDIA
		insertitem Nord01eaComboMedia,N3,MEDIA
		add	C1,N2
		add	C1,N3
		until	(N2 > 30)	.Directly corresponds to number	of items in MEDIA.INC!!
	repeat
	setitem	Nord001bComboMedia,0,1
	setitem	Nord01eaComboMedia,0,1

.Load Contact &	Caller ComboBox
	move	C1,N2
	move	"  ",str45
.Must delete blank items entered to ensure adequate space
	deleteitem Nord001bComboContact,0
	deleteitem Nord001bComboCaller,0
	deleteitem Nord001DComboCallerSearch,0
	deleteitem Nord001DComboContactSearch,0
.	 deleteitem Nord01eaComboPlanner,0
	deleteitem Nord01eaComboContact,0
.	 deleteitem Nord01ECComboPlanner,0
	deleteitem Nord01ECComboContact,0
	insertitem Nord001bComboContact,N2,str45
	insertitem Nord001bComboCaller,N2,str45
	insertitem Nord001DComboCallerSearch,N2,str45
	insertitem Nord001DComboContactSearch,N2,str45
.	 insertitem Nord01eaComboPlanner,N2,str45
	insertitem Nord01eaComboContact,N2,str45
.	 insertitem Nord01ECComboPlanner,N2,str45
	insertitem Nord01ECComboContact,N2,str45
	loop
		move	C1,NCNTPATH
		move	"Load-NCNTSEQ",Location
		call	NCNTSEQ
		until over
		if (CNTCNT = "1")
			pack	str45,CNTNAME,B1,CNTNUM
			add	C1,N2
			insertitem Nord001bComboContact,N2,str45
			insertitem Nord001bComboCaller,N2,str45
			insertitem Nord001DComboCallerSearch,N2,str45
			insertitem Nord001DComboContactSearch,N2,str45
.			 insertitem Nord01eaComboPlanner,N2,str45
			insertitem Nord01eaComboContact,N2,str45
.			 insertitem Nord01ECComboPlanner,N2,str45
			insertitem Nord01ECComboContact,N2,str45
		endif
	repeat
	setitem	Nord001bComboContact,0,1
	setitem	Nord001bComboCaller,0,1
	setitem	Nord001DComboCallerSearch,0,1
	setitem	Nord001DComboContactSearch,0,1
.	 setitem Nord01eaComboPlanner,0,1
	setitem	Nord01eaComboContact,0,1
.	 setitem Nord01ECComboPlanner,0,1
	setitem	Nord01ECComboContact,0,1

.Load Planner ComboBoxes
	move	C1,N2
	move	C1,N3
	move	"  ",str45
.Must delete blank items entered to ensure adequate space
	deleteitem Nord01eaComboPlanner,0
	deleteitem Nord01ECComboPlanner,0
	insertitem Nord01eaComboPlanner,N3,str45
	insertitem Nord01ECComboPlanner,N3,str45
	loop
		move	osls0,salesper
		load	salesper from N2 of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
			osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
			osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
			osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
		if (salesper <>	OSLS0)
			call	Trim using salesper
			if (salesper <>	"")
				pack	str35,salesper,B55
				move	N2,str2
				rep	zfill,str2
				pack	str45,str35,B1,str2
				add	C1,N3
				insertitem Nord01eaComboPlanner,N3,str45
				insertitem Nord01ECComboPlanner,N3,str45
			endif
		endif
		add	C1,N2
		until (N2 > 22)
	repeat
	setitem	Nord01eaComboPlanner,0,1
	setitem	Nord01ECComboPlanner,0,1

.Load Package ListViews
.Create	Nord01eaListView Columns
	Nord001bListViewPackages.InsertColumn using "##",0,1
	Nord01eaListViewPackage.InsertColumn using "##",0,1
	Nord01ECListViewPackages.InsertColumn using "##",0,1
	Nord001bListViewMlrPackages.InsertColumn using "##",0,1
	Nord01ECListViewMlrPackages.InsertColumn using "##",0,1
	Nord001bListViewPackages.InsertColumn using "Record Packages",115,2
	Nord01eaListViewPackage.InsertColumn using "Package Name",165,2
	Nord01ECListViewPackages.InsertColumn using "Record Packages",115,2
	Nord001bListViewMlrPackages.InsertColumn using "Mailer Packages",155,2
	Nord01ECListViewMlrPackages.InsertColumn using "Mailer Packages",155,2
	Nord001bListViewPackages.InsertColumn using "ID",60,3
	Nord01eaListViewPackage.InsertColumn using "ID",55,3
	Nord01ECListViewPackages.InsertColumn using "ID",60,3
	Nord001bListViewMlrPackages.InsertColumn using "ID",80,3
	Nord01ECListViewMlrPackages.InsertColumn using "ID",80,3
	Nord001bListViewPackages.InsertColumn using "Qty",60,4
	Nord01ECListViewPackages.InsertColumn using "Qty",60,4
	Nord001bListViewPackages.InsertColumn using "Other Detail",0,5
	Nord01ECListViewPackages.InsertColumn using "Other Detail",0,5
	Nord001bListViewMlrPackages.InsertColumn using "Other Detail",0,4
	Nord01ECListViewMlrPackages.InsertColumn using "Other Detail",0,4
	Nord001bListViewPackages.SetColumnFormat using 3,1
	Nord01ECListViewPackages.SetColumnFormat using 3,1
.Load Special Instructions ListView
.Column	Clicking
.Check out notes under nord001CListView_ColumnClick for other options.
.{To implement option #2 references to nord001CListView2 could be	remmed or removed.} ASH
	nord001CListView.InsertColumn using "Code",40,1
	nord001CListView.InsertColumn using "Description",480,2
	nord001CListView2.InsertColumn using "Description",480,1
	nord001CListView2.InsertColumn using "Code",40,2
	move	"Driver	Prep-NSPISEQ",Location
	loop
		call	NSPISEQ
		until over
		pack	hold,INST1,INST2
.begin patch 3.78.7 ninspi updated to 3 byte id/key
.		type SPINO
.		if not equal
.			if (SPINO = "0A")
.				move	"100",str3
.			elseif (SPINO =	"0B")
.      				move	"101",str3
.			elseif (SPINO =	"0C")
.				move	"102",str3
.			elseif (SPINO =	"0D")
.				move	"103",str3
.			elseif (SPINO =	"0E")
.				move	"104",str3
.			elseif (SPINO =	"0F")
.				move	"105",str3
.			elseif (SPINO =	"0G")
.				move	"106",str3
.			elseif (SPINO =	"0H")
.				move	"107",str3
.			endif
.		else
.end patch 3.78.7
		pack	str3,SPINO
.		endif

		nord001CListView.InsertItem giving N9 using str3
		nord001CListView.SetItemText using N9,hold,1
		nord001CListView2.InsertItem giving N9 using hold
		nord001CListView2.SetItemText using N9,str3,1
	repeat
	nord001CListView.EnsureVisible using 0,0
	nord001CListView2.EnsureVisible using 0,0
	nord001CListView.SetItemState giving N9 using 0,2,2
	nord001CListView2.SetItemState giving N9 using 0,2,2
	clear	hold
.Create	Nord001TListView Columns
.Column	Clicking
.Check out notes under nord001CListView_ColumnClick for other options.
.{To implement option #2 references to nord001CListView2 could be	remmed or removed.} ASH
.START PATCH 3.72.4 REPLACED LOGIC
.	Nord001TListView.InsertColumn using "LR",50,1
.	Nord001TListView.InsertColumn using "Mailer P.O.",80,2
.	Nord001TListView.InsertColumn using "Mailer",40,3
.	Nord001TListView.InsertColumn using "List",55,4
.	Nord001TListView.InsertColumn using "Broker",50,5
.	Nord001TListView.InsertColumn using "Mailer Key",80,6
.	Nord001TListView.InsertColumn using "Other	Detail",100,7
........................
	Nord001TListView.InsertColumn using "LR",50,0
	Nord001TListView.InsertColumn using "Status",80,1
	Nord001TListView.InsertColumn using "Mailer P.O.",80,2
	Nord001TListView.InsertColumn using "Mailer",55,3
	Nord001TListView.InsertColumn using "Name",75,4
	Nord001TListView.InsertColumn using "Offer",75,5
	Nord001TListView.InsertColumn using "R/E",55,6
	Nord001TListView.InsertColumn using "List",55,7
	Nord001TListView.InsertColumn using "Name",75,8
	Nord001TListView.InsertColumn using "Select",75,9
	Nord001TListView.InsertColumn using "Comslct",50,10
	Nord001TListView.InsertColumn using "Order Date",60,11
	Nord001TListView.InsertColumn using "Mail Date",60,12
	Nord001TListView.InsertColumn using "Qty",55,13
	Nord001TListView.InsertColumn using "Broker",50,14
	Nord001TListView.InsertColumn using "Name",75,15
	Nord001TListView.InsertColumn using "Mailer Key",80,16
	Nord001TListView.InsertColumn using "Other	Detail",0,17
.Key Values
.	Nord001TListView.InsertColumn using "Order Date Key",0,18
.	Nord001TListView.InsertColumn using "Mail Date Key",0,19
	Nord001TListView.InsertColumn using "Qty Key",0,18
.
	Nord001TListView.SetColumnFormat using 13,1
.END PATCH 3.72.4 REPLACED LOGIC
.START PATCH	3.78.2	REPLACED LOGIC
..Create Column Headers for Nord001DListView
..Sorted	by LR
.	Nord001DListView.InsertColumn using "LR",60,1
.	Nord001DListView.InsertColumn using "List/Owner",80,2
.	Nord001DListView.InsertColumn using "Mailer",45,3
.	Nord001DListView.InsertColumn using "Quantity",70,4
.	Nord001DListView.InsertColumn using "Exc./Rent",60,5
.	Nord001DListView.InsertColumn using "Mail	Date",70,6
.	Nord001DListView.InsertColumn using "LCR Date",70,7
.	Nord001DListView.InsertColumn using "Clear Stat",60,8
.	Nord001DListView.InsertColumn using "Clear Inits",60,9
.	Nord001DListView.InsertColumn using "Clr./Den./Fax Date",70,10
.	Nord001DListView.InsertColumn using "Contact",100,11
.	Nord001DListView.InsertColumn using "Caller",100,12
.	Nord001DListView.InsertColumn using "Sample",50,13
.	Nord001DListView.InsertColumn using "Status",80,14
.	Nord001DListView.InsertColumn using "List	Name",80,15
.	Nord001DListView.InsertColumn using "Owner Name",80,16
.	Nord001DListView.InsertColumn using "Mailer Name",80,17
.	Nord001DListView.InsertColumn using "Offer",80,18
.	Nord001DListView.InsertColumn using "Xstat",80,19
..Sorted	by List
.	Order5ListView2.InsertColumn using "List Name",80,1
.	Order5ListView2.InsertColumn using "Mailer Name",80,2
.	Order5ListView2.InsertColumn using "LR",60,3
.	Order5ListView2.InsertColumn using "Quantity",70,4
.	Order5ListView2.InsertColumn using "Exc./Rent",60,5
.	Order5ListView2.InsertColumn using "Mail Date",70,6
.	Order5ListView2.InsertColumn using "LCR	Date",70,7
.	Order5ListView2.InsertColumn using "Clear Stat",60,8
.	Order5ListView2.InsertColumn using "Clear Inits",60,9
.	Order5ListView2.InsertColumn using "Clr./Den./Fax Date",70,10
.	Order5ListView2.InsertColumn using "Contact",100,11
.	Order5ListView2.InsertColumn using "Caller",100,12
.	Order5ListView2.InsertColumn using "Sample",50,13
.	Order5ListView2.InsertColumn using "Status",80,14
.	Order5ListView2.InsertColumn using "Owner Name",80,15
.	Order5ListView2.InsertColumn using "Offer",80,16
.	Order5ListView2.InsertColumn using "Xstat",80,17
.	Order5ListView2.InsertColumn using "List/Owner",80,18
.	Order5ListView2.InsertColumn using "Mailer",45,19
..Sorted	by Mailer
.	Order5ListView3.InsertColumn using "Mailer Name",80,1
.	Order5ListView3.InsertColumn using "List Name",80,2
.	Order5ListView3.InsertColumn using "LR",60,3
.	Order5ListView3.InsertColumn using "Quantity",70,4
.	Order5ListView3.InsertColumn using "Exc./Rent",60,5
.	Order5ListView3.InsertColumn using "Mail Date",70,6
.	Order5ListView3.InsertColumn using "LCR	Date",70,7
.	Order5ListView3.InsertColumn using "Clear Stat",60,8
.	Order5ListView3.InsertColumn using "Clear Inits",60,9
.	Order5ListView3.InsertColumn using "Clr./Den./Fax Date",70,10
.	Order5ListView3.InsertColumn using "Contact",100,11
.	Order5ListView3.InsertColumn using "Caller",100,12
.	Order5ListView3.InsertColumn using "Sample",50,13
.	Order5ListView3.InsertColumn using "Status",80,14
.	Order5ListView3.InsertColumn using "Owner Name",80,15
.	Order5ListView3.InsertColumn using "Offer",80,16
.	Order5ListView3.InsertColumn using "Xstat",80,17
.	Order5ListView3.InsertColumn using "List/Owner",80,18
.	Order5ListView3.InsertColumn using "Mailer",45,19
..Sorted	by Approval Date
..Top item is used for SORT
.	Order5ListView4.InsertColumn using "",0,1
.	Order5ListView4.InsertColumn using "Clr./Den./Fax Date",70,2
.	Order5ListView4.InsertColumn using "List Name",80,3
.	Order5ListView4.InsertColumn using "Mailer Name",80,4
.	Order5ListView4.InsertColumn using "LR",60,5
.	Order5ListView4.InsertColumn using "Clear Stat",60,6
.	Order5ListView4.InsertColumn using "Quantity",70,7
.	Order5ListView4.InsertColumn using "Exc./Rent",60,8
.	Order5ListView4.InsertColumn using "Mail Date",70,9
.	Order5ListView4.InsertColumn using "LCR	Date",70,10
.	Order5ListView4.InsertColumn using "Clear Inits",60,11
.	Order5ListView4.InsertColumn using "Contact",100,12
.	Order5ListView4.InsertColumn using "Caller",100,13
.	Order5ListView4.InsertColumn using "Sample",50,14
.	Order5ListView4.InsertColumn using "Status",80,15
.	Order5ListView4.InsertColumn using "Owner Name",80,16
.	Order5ListView4.InsertColumn using "Offer",80,17
.	Order5ListView4.InsertColumn using "Xstat",80,18
.	Order5ListView4.InsertColumn using "List/Owner",80,19
.	Order5ListView4.InsertColumn using "Mailer",45,20
..Sorted	by Mail	Date
..Top item is used for SORT
.	Order5ListView5.InsertColumn using "",0,1
.	Order5ListView5.InsertColumn using "Mail Date",70,2
.	Order5ListView5.InsertColumn using "List Name",80,3
.	Order5ListView5.InsertColumn using "Mailer Name",80,4
.	Order5ListView5.InsertColumn using "LR",60,5
.	Order5ListView5.InsertColumn using "Clear Stat",60,6
.	Order5ListView5.InsertColumn using "Quantity",70,7
.	Order5ListView5.InsertColumn using "Exc./Rent",60,8
.	Order5ListView5.InsertColumn using "LCR	Date",70,9
.	Order5ListView5.InsertColumn using "Clear Inits",60,10
.	Order5ListView5.InsertColumn using "Clr./Den./Fax Date",70,11
.	Order5ListView5.InsertColumn using "Contact",100,12
.	Order5ListView5.InsertColumn using "Caller",100,13
.	Order5ListView5.InsertColumn using "Sample",50,14
.	Order5ListView5.InsertColumn using "Status",80,15
.	Order5ListView5.InsertColumn using "Owner Name",80,16
.	Order5ListView5.InsertColumn using "Offer",80,17
.	Order5ListView5.InsertColumn using "Xstat",80,18
.	Order5ListView5.InsertColumn using "List/Owner",80,19
.	Order5ListView5.InsertColumn using "Mailer",45,20
..Sorted	by LCR Date
..Top item is used for SORT
.	Order5ListView6.InsertColumn using "",0,1
.	Order5ListView6.InsertColumn using "LCR	Date",70,2
.	Order5ListView6.InsertColumn using "List Name",80,3
.	Order5ListView6.InsertColumn using "Mailer Name",80,4
.	Order5ListView6.InsertColumn using "LR",60,5
.	Order5ListView6.InsertColumn using "Clear Stat",60,6
.	Order5ListView6.InsertColumn using "Quantity",70,7
.	Order5ListView6.InsertColumn using "Exc./Rent",60,8
.	Order5ListView6.InsertColumn using "Mail Date",70,9
.	Order5ListView6.InsertColumn using "Clear Inits",60,10
.	Order5ListView6.InsertColumn using "Clr./Den./Fax Date",70,11
.	Order5ListView6.InsertColumn using "Contact",100,12
.	Order5ListView6.InsertColumn using "Caller",100,13
.	Order5ListView6.InsertColumn using "Sample",50,14
.	Order5ListView6.InsertColumn using "Status",80,15
.	Order5ListView6.InsertColumn using "Owner Name",80,16
.	Order5ListView6.InsertColumn using "Offer",80,17
.	Order5ListView6.InsertColumn using "Xstat",80,18
.	Order5ListView6.InsertColumn using "List/Owner",80,19
.	Order5ListView6.InsertColumn using "Mailer",45,20
.
	Nord001DListView.InsertColumn using "Mailer Name",80,1
	Nord001DListView.InsertColumn using "List	Name",80,2
	Nord001DListView.InsertColumn using "LR",60,3
	Nord001DListView.InsertColumn using "Quantity",70,4
	Nord001DListView.InsertColumn using "Exc./Rent",60,5
	Nord001DListView.InsertColumn using "Mail	Date",70,6
	Nord001DListView.InsertColumn using "LCR Date",70,7
	Nord001DListView.InsertColumn using "Clear Stat",60,8
	Nord001DListView.InsertColumn using "Clear Inits",60,9
	Nord001DListView.InsertColumn using "Clr./Den./Fax Date",70,10
	Nord001DListView.InsertColumn using "Contact",100,11
	Nord001DListView.InsertColumn using "Caller",100,12
	Nord001DListView.InsertColumn using "Sample",50,13
	Nord001DListView.InsertColumn using "Status",80,14
	Nord001DListView.InsertColumn using "Owner Name",80,15
	Nord001DListView.InsertColumn using "Offer",80,16
	Nord001DListView.InsertColumn using "Xstat",80,17
	Nord001DListView.InsertColumn using "List/Owner",80,18
	Nord001DListView.InsertColumn using "Mailer",45,19
	Nord001DListView.InsertColumn using "",0,20   // hold yyyymmdd for mail date sort
	Nord001DListView.InsertColumn using "",0,21   // yyyymmdd for lcr date
	Nord001DListView.InsertColumn using "",0,22   // yyyymmdd for clr/den/fax date
.
.END PATCH	3.78.2	REPLACED LOGIC
.Create	NORDMSK2ListView Columns
	NORDMSK2ListView.InsertColumn using "Number",50,1
	NORDMSK2ListView.InsertColumn using "Name",140,2
.START PATCH 3.75.7 REPLACED LOGIC
.	NORDMSK2ListView.InsertColumn using "Mailer",40,3
	NORDMSK2ListView.InsertColumn using "Mailer",50,3
.END PATCH 3.75.7 REPLACED LOGIC
	NORDMSK2ListView.InsertColumn using "Broker",50,4
	NORDMSK2ListView.InsertColumn using "Campaign Date",80,5
	NORDMSK2ListView.InsertColumn using "Other Detail",100,6

.Create	NORDMSK3ListView Columns
	NORDMSK3ListView.InsertColumn using "List",100,1
	NORDMSK3ListView.InsertColumn using "Camp. ##",50,2
	NORDMSK3ListView.InsertColumn using "Select",100,3
	NORDMSK3ListView.InsertColumn using "Package",50,4
	NORDMSK3ListView.InsertColumn using "Other Detail",100,5

.Create	Nord01EBListView Columns
	Nord01EBListView.InsertColumn using "Test	Byte",0,1
	Nord01EBListView.InsertColumn using "LR ##",50,2
	Nord01EBListView.InsertColumn using "Status",50,3
	Nord01EBListView.InsertColumn using "Sub-Status",50,4
	Nord01EBListView.InsertColumn using "List	##",55,5
	Nord01EBListView.InsertColumn using "List	Name",120,6
	Nord01EBListView.InsertColumn using "Select",100,7
	Nord01EBListView.InsertColumn using "Quantity",70,8
	Nord01EBListView.InsertColumn using "Net Qty",70,9
	Nord01EBListView.InsertColumn using "Net %",50,10
	Nord01EBListView.InsertColumn using "Universe",70,11
	Nord01EBListView.InsertColumn using "Rent/Exc",70,12
	Nord01EBListView.InsertColumn using "History",70,13
	Nord01EBListView.InsertColumn using "Clear Stat",60,14
	Nord01EBListView.InsertColumn using "Clear Inits",60,15
	Nord01EBListView.InsertColumn using "Clr./Den./Fax Date",70,16
	Nord01EBListView.InsertColumn using "Caller",70,17
	Nord01EBListView.InsertColumn using "Test/Cont.",70,18
	Nord01EBListView.InsertColumn using "Xstat",100,19
	Nord01EBListView.InsertColumn using "Other Detail",100,20
.
	Nord01EBListView2.InsertColumn using "List Name",120,1
	Nord01EBListView2.InsertColumn using "List ##",55,2
	Nord01EBListView2.InsertColumn using "Select",100,3
	Nord01EBListView2.InsertColumn using "LR ##",50,4
	Nord01EBListView2.InsertColumn using "Status",50,5
	Nord01EBListView2.InsertColumn using "Sub-Status",50,6
	Nord01EBListView2.InsertColumn using "Quantity",70,7
	Nord01EBListView2.InsertColumn using "Net	Qty",70,8
	Nord01EBListView2.InsertColumn using "Net	%",50,9
	Nord01EBListView2.InsertColumn using "Universe",70,10
	Nord01EBListView2.InsertColumn using "Rent/Exc",70,11
	Nord01EBListView2.InsertColumn using "History",70,12
	Nord01EBListView2.InsertColumn using "Clear Stat",60,13
	Nord01EBListView2.InsertColumn using "Clear Inits",60,14
	Nord01EBListView2.InsertColumn using "Clr./Den./Fax Date",70,15
	Nord01EBListView2.InsertColumn using "Caller",70,16
	Nord01EBListView2.InsertColumn using "Test/Cont.",70,17
	Nord01EBListView2.InsertColumn using "Xstat",100,18
	Nord01EBListView2.InsertColumn using "Other Detail",100,19
.START PATCH 3.66 ADDED LOGIC
.Create	Column Headers for NSTA001EListView
	getprop	Nord01EBListViewProj,*ColumnHeaders=ColHeads
.I hide	the first item as I have not yet figured out if	I can change the ForeColor of that item, since it does not appear to be	a sub-item.
	ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
	ColHeads.Add using *Index=2,*Key="one",*Text="LR ##",*Width=50
	ColHeads.Add using *Index=3,*Key="two",*Text="Status",*Width=50
.	ColHeads.Add using *Index=4,*Key="",*Text="Sub-Status",*Width=50
	ColHeads.Add using *Index=4,*Key="three",*Text="List ##",*Width=55
	ColHeads.Add using *Index=5,*Key="four",*Text="List & Select/Package",*Width=170
.	ColHeads.Add using *Index=7,*Key="",*Text="Select",*Width=100
	ColHeads.Add using *Index=6,*Key="five",*Text="Quantity",*Width=70,*Alignment=1
	ColHeads.Add using *Index=7,*Key="six",*Text="Net/Rec.",*Width=70,*Alignment=1
	ColHeads.Add using *Index=8,*Key="seven",*Text="Net %",*Width=50,*Alignment=1
	ColHeads.Add using *Index=9,*Key="eight",*Text="Proj. Qty.",*Width=70,*Alignment=1
	ColHeads.Add using *Index=10,*Key="nine",*Text="Uni.\Sel. Uni.",*Width=75,*Alignment=1
	ColHeads.Add using *Index=11,*Key="ten",*Text="Other Detail",*Width=0
.	ColHeads.Add using *Index=11,*Key="ten",*Text="Rent/Exc",*Width=70
.	ColHeads.Add using *Index=12,*Key="eleven",*Text="History",*Width=70
.	ColHeads.Add using *Index=13,*Key="twelve",*Text="Clear Stat",*Width=60
.	ColHeads.Add using *Index=14,*Key="thirteen",*Text="Clear Inits",*Width=60
.	ColHeads.Add using *Index=15,*Key="fourteen",*Text="Clr./Den./Fax Date",*Width=70
.	ColHeads.Add using *Index=16,*Key="fifteen",*Text="Caller",*Width=70
.	ColHeads.Add using *Index=17,*Key="sixteen",*Text="Test/Cont.",*Width=70
.	ColHeads.Add using *Index=18,*Key="seventeen",*Text="Xstat",*Width=100
.	ColHeads.Add using *Index=19,*Key="eighteen",*Text="Other Detail",*Width=0

.Set some properties for ListView object
	create	OTRUE,VarType=VT_BOOL,VarValue=1
	create	OFALSE,VarType=VT_BOOL,VarValue=0
	create	OBOOL,VarType=VT_BOOL,VarValue=0
.
	setprop	Nord01EBListViewProj,*HideColumnHeaders=OFALSE
	setprop	Nord01EBListViewProj,*HideSelection=OFALSE
.	setprop	Nord01EBListViewProj,*HotTracking=OTRUE
	setprop	Nord01EBListViewProj,*FullRowSelect=OTRUE
	setprop	Nord01EBListViewProj,*MultiSelect=OTRUE
	setprop	Nord01EBListViewProj,*Sorted=OTRUE
	setprop	Nord01EBListViewProj,*SortOrder=0
	setprop	Nord01EBListViewProj,*AllowColumnReorder=OTRUE
	setprop	Nord01EBListViewProj,*LabelEdit=1
	setprop	Nord01EBListViewProj,*View=3
	setprop	Nord01EBListViewProj,*Font=font2
.
	getprop	Nord01EBListViewProj,*ListItems=ListIts2
.END PATCH 3.66 ADDED LOGIC
.Load STATS ListViews
	NSTA0002ListView.InsertColumn using "Source Code",100,1
	NSTA0002ListView.InsertColumn using "Package",120,2
	NSTA0002ListView.InsertColumn using "Qty",60,3
	NSTA0002ListView.InsertColumn using "Other	Detail",0,4
	NSTA0002ListView.SetColumnFormat using 2,1
.
	NSTA0002ListView2.InsertColumn using "Source Code",100,1
	NSTA0002ListView2.InsertColumn using "Package",120,2
	NSTA0002ListView2.InsertColumn using "Qty",60,3
	NSTA0002ListView2.InsertColumn using "Key",0,4
	NSTA0002ListView2.InsertColumn using "Other Detail",0,5
	NSTA0002ListView2.SetColumnFormat using 2,1
.
	NSTA001DListView.InsertColumn using "List	Name/PackageNum",0,1
	NSTA001DListView.InsertColumn using "LR",50,2
	NSTA001DListView.InsertColumn using "L0L",35,3
.START PATCH 3.49.1 REPLACED LOGIC
.	NSTA001DListView.InsertColumn using "List	Name",120,4
	NSTA001DListView.InsertColumn using "List	Name/Select",120,4
.END PATCH 3.49.1 REPLACED LOGIC
.	 NSTA001DListView.InsertColumn using "List ##",50,5
	NSTA001DListView.InsertColumn using "Package",100,5
	NSTA001DListView.InsertColumn using "Pack. ##",50,6
	NSTA001DListView.InsertColumn using "P. Cst/M",60,7
	NSTA001DListView.InsertColumn using "Mbr Cost",60,8
	NSTA001DListView.InsertColumn using "Req.	Qty",60,9
	NSTA001DListView.InsertColumn using "Rec.	Qty",60,10
	NSTA001DListView.InsertColumn using "R/E",35,11
	NSTA001DListView.InsertColumn using "Lst Cst/M",60,12
	NSTA001DListView.InsertColumn using "Avg Net",70,13
	NSTA001DListView.InsertColumn using "Net Names",70,14
	NSTA001DListView.InsertColumn using "Resp. Rate",70,15
	NSTA001DListView.InsertColumn using "Returns",60,16
	NSTA001DListView.InsertColumn using "Gift",60,17
	NSTA001DListView.InsertColumn using "Revenue",70,18
	NSTA001DListView.InsertColumn using "Prod. Cost",70,19
	NSTA001DListView.InsertColumn using "List	Cost",70,20
     	NSTA001DListView.InsertColumn using "Tot.	Cost",100,21
	NSTA001DListView.InsertColumn using "Net +-",100,22
	NSTA001DListView.InsertColumn using "Other Detail",0,23
.START PATCH 3.48 ADDED LOGIC
	NSTA001DListView.InsertColumn using "Universe",60,24
.END PATCH 3.48 ADDED LOGIC
	NSTA001DListView.SetColumnFormat using 6,1
	NSTA001DListView.SetColumnFormat using 7,1
	NSTA001DListView.SetColumnFormat using 8,1
	NSTA001DListView.SetColumnFormat using 9,1
	NSTA001DListView.SetColumnFormat using 11,1
	NSTA001DListView.SetColumnFormat using 12,1
	NSTA001DListView.SetColumnFormat using 13,1
	NSTA001DListView.SetColumnFormat using 14,1
	NSTA001DListView.SetColumnFormat using 15,1
	NSTA001DListView.SetColumnFormat using 16,1
	NSTA001DListView.SetColumnFormat using 17,1
	NSTA001DListView.SetColumnFormat using 18,1
	NSTA001DListView.SetColumnFormat using 19,1
	NSTA001DListView.SetColumnFormat using 20,1
	NSTA001DListView.SetColumnFormat using 21,1
.START PATCH 3.66 ADDED LOGIC
	NSTA001DListView.SetColumnFormat using 23,1
.END PATCH 3.66 ADDED LOGIC
.
	NSTA001DListView2.InsertColumn using "Package",100,1
	NSTA001DListView2.InsertColumn using "Pack. ##",50,2
	NSTA001DListView2.InsertColumn using "LR",50,3
	NSTA001DListView2.InsertColumn using "L0L",35,4
.START PATCH 3.49.1 REPLACED LOGIC
.	NSTA001DListView2.InsertColumn using "List Name",120,5
	NSTA001DListView2.InsertColumn using "List Name/Select",120,5
.END PATCH 3.49.1 REPLACED LOGIC
	NSTA001DListView2.InsertColumn using "P. Cst/M",60,6
	NSTA001DListView2.InsertColumn using "Mbr	Cost",60,7
	NSTA001DListView2.InsertColumn using "Req. Qty",60,8
	NSTA001DListView2.InsertColumn using "Rec. Qty",60,9
	NSTA001DListView2.InsertColumn using "R/E",35,10
	NSTA001DListView2.InsertColumn using "Lst	Cst/M",60,11
	NSTA001DListView2.InsertColumn using "Avg	Net",70,12
	NSTA001DListView2.InsertColumn using "Net	Names",70,13
	NSTA001DListView2.InsertColumn using "Resp. Rate",70,14
	NSTA001DListView2.InsertColumn using "Returns",60,15
	NSTA001DListView2.InsertColumn using "Gift",60,16
	NSTA001DListView2.InsertColumn using "Revenue",70,17
	NSTA001DListView2.InsertColumn using "Prod. Cost",70,18
	NSTA001DListView2.InsertColumn using "List Cost",70,19
	NSTA001DListView2.InsertColumn using "Tot. Cost",100,20
	NSTA001DListView2.InsertColumn using "Net	+-",100,21
	NSTA001DListView2.InsertColumn using "Other Detail",0,22
	NSTA001DListView2.SetColumnFormat	using 5,1
	NSTA001DListView2.SetColumnFormat	using 6,1
	NSTA001DListView2.SetColumnFormat	using 7,1
	NSTA001DListView2.SetColumnFormat	using 8,1
	NSTA001DListView2.SetColumnFormat	using 10,1
	NSTA001DListView2.SetColumnFormat	using 11,1
	NSTA001DListView2.SetColumnFormat	using 12,1
	NSTA001DListView2.SetColumnFormat	using 13,1
	NSTA001DListView2.SetColumnFormat	using 14,1
	NSTA001DListView2.SetColumnFormat	using 15,1
	NSTA001DListView2.SetColumnFormat	using 16,1
	NSTA001DListView2.SetColumnFormat	using 17,1
	NSTA001DListView2.SetColumnFormat	using 18,1
	NSTA001DListView2.SetColumnFormat	using 19,1
	NSTA001DListView2.SetColumnFormat	using 20,1
.
	NSTA001DListView3.InsertColumn using "Record Type/List Name/PackageNum",0,1
	NSTA001DListView3.InsertColumn using "LR",50,2
	NSTA001DListView3.InsertColumn using "L0L",35,3
.START PATCH 3.49.1 REPLACED LOGIC
.	NSTA001DListView3.InsertColumn using "List Name",120,4
	NSTA001DListView3.InsertColumn using "List Name/Select",120,4
.END PATCH 3.49.1 REPLACED LOGIC
.	 NSTA001DListView3.InsertColumn using "List ##",50,5
	NSTA001DListView3.InsertColumn using "Package",100,5
	NSTA001DListView3.InsertColumn using "Pack. ##",50,6
	NSTA001DListView3.InsertColumn using "P. Cst/M",60,7
	NSTA001DListView3.InsertColumn using "Mbr	Cost",60,8
	NSTA001DListView3.InsertColumn using "Req. Qty",60,9
	NSTA001DListView3.InsertColumn using "Rec. Qty",60,10
	NSTA001DListView3.InsertColumn using "R/E",35,11
	NSTA001DListView3.InsertColumn using "Lst	Cst/M",60,12
	NSTA001DListView3.InsertColumn using "Avg	Net",70,13
	NSTA001DListView3.InsertColumn using "Net	Names",70,14
	NSTA001DListView3.InsertColumn using "Resp. Rate",70,15
	NSTA001DListView3.InsertColumn using "Returns",60,16
	NSTA001DListView3.InsertColumn using "Gift",60,17
	NSTA001DListView3.InsertColumn using "Revenue",70,18
	NSTA001DListView3.InsertColumn using "Prod. Cost",70,19
	NSTA001DListView3.InsertColumn using "List Cost",70,20
	NSTA001DListView3.InsertColumn using "Tot. Cost",100,21
	NSTA001DListView3.InsertColumn using "Net	+-",100,22
	NSTA001DListView3.InsertColumn using "Other Detail",0,23
	NSTA001DListView3.SetColumnFormat	using 6,1
	NSTA001DListView3.SetColumnFormat	using 7,1
	NSTA001DListView3.SetColumnFormat	using 8,1
	NSTA001DListView3.SetColumnFormat	using 9,1
	NSTA001DListView3.SetColumnFormat	using 11,1
	NSTA001DListView3.SetColumnFormat	using 12,1
	NSTA001DListView3.SetColumnFormat	using 13,1
	NSTA001DListView3.SetColumnFormat	using 14,1
	NSTA001DListView3.SetColumnFormat	using 15,1
	NSTA001DListView3.SetColumnFormat	using 16,1
	NSTA001DListView3.SetColumnFormat	using 17,1
	NSTA001DListView3.SetColumnFormat	using 18,1
	NSTA001DListView3.SetColumnFormat	using 19,1
	NSTA001DListView3.SetColumnFormat	using 20,1
	NSTA001DListView3.SetColumnFormat	using 21,1
.START PATCH 3.45 ADDED LOGIC
	NSTA001DListView4.InsertColumn using "Mbr	Cost",0,1
	NSTA001DListView4.InsertColumn using "Mbr	Cost",60,2
	NSTA001DListView4.InsertColumn using "LR",50,3
	NSTA001DListView4.InsertColumn using "L0L",35,4
.START PATCH 3.49.1 REPLACED LOGIC
.	NSTA001DListView4.InsertColumn using "List Name",120,5
	NSTA001DListView4.InsertColumn using "List Name/Select",120,5
.END PATCH 3.49.1 REPLACED LOGIC
	NSTA001DListView4.InsertColumn using "Package",100,6
	NSTA001DListView4.InsertColumn using "Pack. ##",50,7
	NSTA001DListView4.InsertColumn using "P. Cst/M",60,8
	NSTA001DListView4.InsertColumn using "Req. Qty",60,9
	NSTA001DListView4.InsertColumn using "Rec. Qty",60,10
	NSTA001DListView4.InsertColumn using "R/E",35,11
	NSTA001DListView4.InsertColumn using "Lst	Cst/M",60,12
	NSTA001DListView4.InsertColumn using "Avg	Net",70,13
	NSTA001DListView4.InsertColumn using "Net	Names",70,14
	NSTA001DListView4.InsertColumn using "Resp. Rate",70,15
	NSTA001DListView4.InsertColumn using "Returns",60,16
	NSTA001DListView4.InsertColumn using "Gift",60,17
	NSTA001DListView4.InsertColumn using "Revenue",70,18
	NSTA001DListView4.InsertColumn using "Prod. Cost",70,19
	NSTA001DListView4.InsertColumn using "List Cost",70,20
	NSTA001DListView4.InsertColumn using "Tot. Cost",100,21
	NSTA001DListView4.InsertColumn using "Net	+-",100,22
	NSTA001DListView4.InsertColumn using "Other Detail",0,23
	NSTA001DListView4.SetColumnFormat	using 1,1
	NSTA001DListView4.SetColumnFormat	using 7,1
	NSTA001DListView4.SetColumnFormat	using 8,1
	NSTA001DListView4.SetColumnFormat	using 9,1
	NSTA001DListView4.SetColumnFormat	using 11,1
	NSTA001DListView4.SetColumnFormat	using 12,1
	NSTA001DListView4.SetColumnFormat	using 13,1
	NSTA001DListView4.SetColumnFormat	using 14,1
	NSTA001DListView4.SetColumnFormat	using 15,1
	NSTA001DListView4.SetColumnFormat	using 16,1
	NSTA001DListView4.SetColumnFormat	using 17,1
	NSTA001DListView4.SetColumnFormat	using 18,1
	NSTA001DListView4.SetColumnFormat	using 19,1
	NSTA001DListView4.SetColumnFormat	using 20,1
	NSTA001DListView4.SetColumnFormat	using 21,1
.END PATCH 3.45 ADDED LOGIC
	NSTA001DListViewTotal.InsertColumn using "Cost Mbr.",60,1
	NSTA001DListViewTotal.InsertColumn using "Reco Qty",60,2
	NSTA001DListViewTotal.InsertColumn using "Rec'd Qty",60,3
	NSTA001DListViewTotal.InsertColumn using "Diff",60,4
	NSTA001DListViewTotal.InsertColumn using "Lst CPM",60,5
	NSTA001DListViewTotal.InsertColumn using "Avg Net",60,6
	NSTA001DListViewTotal.InsertColumn using "Net Names",60,7
	NSTA001DListViewTotal.InsertColumn using "R. Rate",50,8
	NSTA001DListViewTotal.InsertColumn using "Rtns",50,9
	NSTA001DListViewTotal.InsertColumn using "Gift",60,10
	NSTA001DListViewTotal.InsertColumn using "Revenue",60,11
	NSTA001DListViewTotal.InsertColumn using "Prod Cost",70,12
	NSTA001DListViewTotal.InsertColumn using "Lst Cost",70,13
	NSTA001DListViewTotal.InsertColumn using "Tot Cost",70,14
	NSTA001DListViewTotal.InsertColumn using "Net +/-",70,15
	NSTA001DListViewTotal.InsertColumn using "Universe",60,16
	NSTA001DListViewTotal.SetColumnFormat using 0,1
	NSTA001DListViewTotal.SetColumnFormat using 1,1
	NSTA001DListViewTotal.SetColumnFormat using 2,1
	NSTA001DListViewTotal.SetColumnFormat using 3,1
	NSTA001DListViewTotal.SetColumnFormat using 4,1
	NSTA001DListViewTotal.SetColumnFormat using 5,1
	NSTA001DListViewTotal.SetColumnFormat using 6,1
	NSTA001DListViewTotal.SetColumnFormat using 7,1
	NSTA001DListViewTotal.SetColumnFormat using 8,1
	NSTA001DListViewTotal.SetColumnFormat using 9,1
	NSTA001DListViewTotal.SetColumnFormat using 10,1
	NSTA001DListViewTotal.SetColumnFormat using 11,1
	NSTA001DListViewTotal.SetColumnFormat using 12,1
	NSTA001DListViewTotal.SetColumnFormat using 13,1
	NSTA001DListViewTotal.SetColumnFormat using 14,1
	NSTA001DListViewTotal.SetColumnFormat using 15,1
.
	NSTA001BListViewPackage.InsertColumn using	"Sort Date",0,1
	NSTA001BListViewPackage.InsertColumn using	"",20,2
	NSTA001BListViewPackage.InsertColumn using	"Date",70,3
	NSTA001BListViewPackage.InsertColumn using	"Total",60,4
	NSTA001BListViewPackage.SetColumnFormat using 3,1

.Create	Column Headers for NSTA001EListView
	getprop	NSTA001EListView,*ColumnHeaders=ColHeads
.I hide	the first item as I have not yet figured out if	I can change the ForeColor of that item, since it does not appear to be	a sub-item.
	ColHeads.Add using *Index=1,*Key="key",*Text="",*Width=0
	ColHeads.Add using *Index=2,*Key="one",*Text="Source Code",*Width=80
	ColHeads.Add using *Index=3,*Key="two",*Text="Mailer Name",*Width=80
	ColHeads.Add using *Index=4,*Key="three",*Text="List Name",*Width=80
	ColHeads.Add using *Index=5,*Key="four",*Text="Select",*Width=80
	ColHeads.Add using *Index=6,*Key="five",*Text="LR",*Width=60
	ColHeads.Add using *Index=7,*Key="six",*Text="Qty Mailed",*Width=70,*Alignment=1
	ColHeads.Add using *Index=8,*Key="seven",*Text="Mail Date",*Width=70
	ColHeads.Add using *Index=9,*Key="eight",*Text="Package",*Width=80
	ColHeads.Add using *Index=10,*Key="nine",*Text="Other Info",*Width=0
	ColHeads.Add using *Index=11,*Key="ten",*Text="QTY KEY",*Width=0
	ColHeads.Add using *Index=12,*Key="eleven",*Text="MAIL DATE KEY",*Width=0

.START PATCH 3.66 ADDED LOGIC - REMOVE THIS, WE DID IT EARLIER
..Set some properties for ListView object
.	create	OTRUE,VarType=VT_BOOL,VarValue=1
.	create	OFALSE,VarType=VT_BOOL,VarValue=0
.	create	OBOOL,VarType=VT_BOOL,VarValue=0
.END PATCH 3.66 ADDED LOGIC
.
	setprop	NSTA001EListView,*HideColumnHeaders=OFALSE
	setprop	NSTA001EListView,*HideSelection=OFALSE
.	setprop	NSTA001EListView,*HotTracking=OTRUE
	setprop	NSTA001EListView,*FullRowSelect=OTRUE
.	setprop	NSTA001EListView,*MultiSelect=OTRUE
	setprop	NSTA001EListView,*Sorted=OTRUE
	setprop	NSTA001EListView,*SortOrder=0
	setprop	NSTA001EListView,*AllowColumnReorder=OTRUE
	setprop	NSTA001EListView,*LabelEdit=1
	setprop	NSTA001EListView,*View=3
	setprop	NSTA001EListView,*Font=font2
.
	getprop	NSTA001EListView,*ListItems=ListIts

.Set Following extended	properties for all ListView objects:
.  FullRowSelect
.  DragAndDrop
.  OneClickActivate
hexer	integer	1,"0x0070"

	Nord001bListViewPackages.SetExtendedStyle	giving N9 using	0,hexer
	Nord01eaListViewPackage.SetExtendedStyle giving N9 using 0,hexer
	Nord01ECListViewPackages.SetExtendedStyle	giving N9 using	0,hexer
	nord001CListView.SetExtendedStyle	giving N9 using	0,hexer
	nord001CListView2.SetExtendedStyle giving	N9 using 0,hexer
	Nord001bListViewMlrPackages.SetExtendedStyle giving N9 using 0,hexer
	Nord01ECListViewMlrPackages.SetExtendedStyle giving N9 using 0,hexer
	Nord001TListView.SetExtendedStyle giving N9 using 0,hexer
	NORDMSK2ListView.SetExtendedStyle	giving N9 using	0,hexer
	NORDMSK3ListView.SetExtendedStyle	giving N9 using	0,hexer
	Nord001DListView.SetExtendedStyle	giving N9 using	0,hexer
.START PATCH	3.78.2	REMOVED LOGIC
.	Order5ListView2.SetExtendedStyle giving	N9 using 0,hexer
.	Order5ListView3.SetExtendedStyle giving	N9 using 0,hexer
.	Order5ListView4.SetExtendedStyle giving	N9 using 0,hexer
.	Order5ListView5.SetExtendedStyle giving	N9 using 0,hexer
.	Order5ListView6.SetExtendedStyle giving	N9 using 0,hexer
.END PATCH	3.78.2	REMOVED LOGIC
	Nord01EBListView.SetExtendedStyle	giving N9 using	0,hexer
	Nord01EBListView2.SetExtendedStyle giving	N9 using 0,hexer
	NSTA0002ListView.SetExtendedStyle giving N9 using 0,hexer
	NSTA0002ListView2.SetExtendedStyle	giving N9 using	0,hexer
	NSTA001DListView.SetExtendedStyle	giving N9 using	0,hexer
	NSTA001DListView2.SetExtendedStyle giving	N9 using 0,hexer
	NSTA001DListView3.SetExtendedStyle giving	N9 using 0,hexer
.START PATCH 3.45 ADDED LOGIC
	NSTA001DListView4.SetExtendedStyle giving	N9 using 0,hexer
.END PATCH 3.45 ADDED LOGIC
	NSTA001BListViewPackage.SetExtendedStyle giving N9	using 0,hexer

.START PATCH 3.72 ADDED LOGIC
	Nord01A2ListViewSelect.InsertColumn using "##",0,0
	Nord01A2ListViewSelect.InsertColumn using "Name",175,1
	Nord01A2ListViewSelect.InsertColumn using "Price",60,2
	Nord01A2ListViewSelect.InsertColumn using "Qty",57,3
	Nord01A2ListViewSelect.InsertColumn using "Details",0,4
	Nord01A2ListViewSelect.InsertColumnFgClr using *Index=5
	Nord01A2ListViewSelect.InsertColumnBgClr using *Index=6
	Nord01A2ListViewSelect.SetColumnFormat using 3,1
.
	Nord01ECListViewSelect.InsertColumn using "##",0,0
	Nord01ECListViewSelect.InsertColumn using "Name",175,1
	Nord01ECListViewSelect.InsertColumn using "Price",60,2
	Nord01ECListViewSelect.InsertColumn using "Qty",57,3
	Nord01ECListViewSelect.InsertColumn using "Details",0,4
	Nord01ECListViewSelect.InsertColumnFgClr using *Index=5
	Nord01ECListViewSelect.InsertColumnBgClr using *Index=6
	Nord01ECListViewSelect.SetColumnFormat using 3,1
.
	NSTA001AListViewSelect.InsertColumn using "##",0,0
	NSTA001AListViewSelect.InsertColumn using "Name",175,1
	NSTA001AListViewSelect.InsertColumn using "Price",60,2
	NSTA001AListViewSelect.InsertColumn using "Qty",57,3
	NSTA001AListViewSelect.InsertColumn using "Details",0,4
	NSTA001AListViewSelect.InsertColumnFgClr using *Index=5
	NSTA001AListViewSelect.InsertColumnBgClr using *Index=6
	NSTA001AListViewSelect.SetColumnFormat using 3,1
.
        Nord01A1ListViewRef1.InsertColumn using "",0,1
        Nord01A1ListViewRef1.InsertColumn using "",100,2
        Nord01A1ListViewRef1.InsertColumn using "",75,3
	Nord01A1ListViewRef1.InsertColumn using "",60,4
	Nord01A1ListViewRef1.InsertColumn using "Details",0,5
	Nord01A1ListViewRef1.SetColumnFormat using 2,1
	Nord01A1ListViewRef1.SetColumnFormat using 3,1
.
        Nord01A1ListViewRef2.InsertColumn using "",0,1
        Nord01A1ListViewRef2.InsertColumn using "",100,2
        Nord01A1ListViewRef2.InsertColumn using "",75,3
	Nord01A1ListViewRef2.InsertColumn using "",60,4
	Nord01A1ListViewRef2.InsertColumn using "Details",0,5
	Nord01A1ListViewRef2.SetColumnFormat using 2,1
	Nord01A1ListViewRef2.SetColumnFormat using 3,1
.
        Nord08A1ListViewRef1.InsertColumn using "",0,1
        Nord08A1ListViewRef1.InsertColumn using "",100,2
        Nord08A1ListViewRef1.InsertColumn using "",75,3
	Nord08A1ListViewRef1.InsertColumn using "",60,4
	Nord08A1ListViewRef1.InsertColumn using "Details",0,5
	Nord08A1ListViewRef1.SetColumnFormat using 2,1
	Nord08A1ListViewRef1.SetColumnFormat using 3,1
.
        Nord08A1ListViewRef2.InsertColumn using "",0,1
        Nord08A1ListViewRef2.InsertColumn using "",100,2
        Nord08A1ListViewRef2.InsertColumn using "",75,3
	Nord08A1ListViewRef2.InsertColumn using "",60,4
	Nord08A1ListViewRef2.InsertColumn using "Details",0,5
	Nord08A1ListViewRef2.SetColumnFormat using 2,1
	Nord08A1ListViewRef2.SetColumnFormat using 3,1
.
.	call	LoadDataListViewRefHeaders
	call	OrderSetSelectDefault
	call	OrderSetSelectDefault8
	call	OrderSetSelectDefault2
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.72.4 ADDED LOGIC
	getprop	NORD0001,top=TempTop,left=TempLeft		.Get Default
.END PATCH 3.72.4 ADDED LOGIC

.START PATCH 3.72.2 ADDED LOGIC
	Nord001bListViewSamples.InsertColumn using "Key",0,0
	Nord001bListViewSamples.InsertColumn using "Name",150,1
	Nord001bListViewSamples.InsertColumn using "Date",75,2
	Nord001bListViewSamples.InsertColumn using "Number",40,3
	Nord001bListViewSamples.InsertColumn using "Status",20,4
	Nord001bListViewSamples.InsertColumnFgClr using *Index=5
.END PATCH 3.72.2 ADDED LOGIC

.Timer creation
.	 CREATE	 TIMER,80     .8 second	- FOR TESTING
	CREATE	TIMER,18000	.30 minutes
	ACTIVATE TIMER,Timeout,RESULT

.Create	Menus
	create	nord0001;mFile,FData
	create	nord0001;mEdit,EData,mFile
	create	nord0001;mOptions,OData,mEdit
	create	nord0001;mReports,RData,mOptions
	reset	revtyps
.	move	"AH",INITS
	scan	INITS,revtyps
	if equal
		create	nord0001;mSecurity,SEData,mReports
		create	nord0001;mHelp,HData,mSecurity
		setprop	NSTA001DEditSearchCamp,EditType=1		.Allow access to ALL records in	Projection File
	else
		create	nord0001;mHelp,HData,mReports
	endif
.	 move	 "AH JN	AA JD DH DB JS",str45
.	 scan	 INITS,str45
.	 if equal
		move	C1,HotFax
.	 endif
.Create	SubMenu
	create	nord0001;sSearch,SData,mOptions,1

.Activate Menus
.FileGo	leads to stop
	activate mFile,FileGo,result
.Need this when	it works
	activate mEdit,EditGo,result
.Only a	SubMenu	under this one
	activate mOptions,OptionsGo,result
	activate mReports,ReportGo,result
	reset	revtyps
	scan	INITS,revtyps
	if equal
		activate mSecurity,SecurityGo,result
	endif
	activate mHelp,HelpGo,result

.Activate SubMenus
	activate sSearch,SearchGo,result

.Create	Colors for EditText Inquiry
	create	white=*white
	create	grey=220:220:220
	create	RED=*RED
	create	black=*black
	create	blue=*blue
	create	green=*green
	create	yellow=*yellow
	create	orange=255:128:64

.Create	fonts to be used
	create	font1,"Arial",size=12,bold
	create	font2,"Arial",size=8
	create	font3,"Helvetica",size=9
	create	font4,"Arial",size=14,italic
	create	font5,"Arial",size=10
.Create/Activate Objects on OptionsOrd.plf
.Screen	1
	create	Options;OptionsScreenInit=80:100:40:255,"",";O)pen on Screen1;)Open on Screen2;)Open on	Screen3;)Open on Screen4;)Open on Screen5;)Open	on Screen6;)Open on Screen7;)Open on Screen8;)Open on Screen9;)Open on Screen10",zorder=100
	create	Options;OptionsFaxLONo=60:80:40:255,"'Fax To List Owner' Default = 'Cancel'",zorder=100
	listins	Options1Coll,OptionsScreenInit,OptionsFaxLONo
	setprop	Options1Coll,visible=1	.always	start with the first tab visible

.Screen	5
	create	Options;OptionsScreen5BrkFilter=80:100:40:250,"Filter Broker Report Records",zorder=100
	create	Options;OptionsScreen5EMailOption=100:120:40:250,"Display EMail	Option Message",zorder=100
	create	Options;OptionsScreen5FileDefault=120:140:40:250,"Use Default Notes File",zorder=100
	listins	Options5Coll,OptionsScreen5BrkFilter,OptionsScreen5EMailOption,OptionsScreen5FileDefault

.Screen	6
.	 create	 Options;OptionsScreen6Init=80:100:40:250,"Open	Program	on Campaign Screen",zorder=100
.	 listins Options6Coll,OptionsScreen6Init

.START PATCH 3.66 ADDED LOGIC
.Screen	7
	 create	 Options;OptionsScreen7Proj=80:100:40:250,"Allow Viewing of Projection Breakdown",zorder=100
	 listins Options7Coll,OptionsScreen7Proj
.END PATCH 3.66 ADDED LOGIC
.Screen	8
.	 create	 Options;OptionsScreen8OrderSearch=80:100:40:250,"Include Order	File in	Searches",zorder=100
.	 listins Options8Coll,OptionsScreen8OrderSearch

.Screen	9
	create	Options;OptionsScreen9View=80:100:40:250,"Disable Viewing of Screen 9",zorder=100
	listins	Options9Coll,OptionsScreen9View

.Screen	10
	create	Options;OptionsScreen10View=80:100:40:250,"Disable Viewing of Screen 10",zorder=100
	create	Options;OptionsScreen10Usage=100:120:40:250,"Automatically Calculate Usage/Orders",zorder=100
	listins	Options10Coll,OptionsScreen10View,OptionsScreen10Usage

.Main Loop
.Set Error Message Stat	Text Boxes
	call	SetOrderErrorMssgDefault
.Set tab index
	clock	timestamp,timestamp
	unpack	timestamp,CC,YY,MM,DD
	move	C1,NXRFPATH
	if (INITS = "  ")
		alert	caution,"Typist	Initials Error,	Inform I.S.",result
		goto FileGo3
.		 stop
	endif
	move	C0,NUSEFLD
	move	C1,NUSEPATH
	move	PORTN,NUSEFLD
	rep	zfill,NUSEFLD
	call	NUSEKEY
.START PATCH 3.76.9 ADDED LOGIC
	move	NUSEUSER,NORD8USER
.END PATCH 3.76.9 ADDED LOGIC
.	 goto userng if	over
	scan	"INVALID",NUSEUSER
.	 goto userng if	equal
	reset	NUSEUSER
.Find out system information
.START PATCH 3.61 REPLACED LOGIC
.	getinfo	system,str6
.	unpack	str6 into str1,str1
.	move	C0,osflag
.	if (str1 = "3" or str1 = "4")		.95/98
.		move	C1,osflag
.	elseif (str1 = "1"or str1 = "5")	.NT4/NT5
.		move	C2,osflag
.	endif
	call	GetWinVer
.END PATCH 3.61 REPLACED LOGIC
	call	Trim using NUSEUSER
	scan	"BILLING",NUSEUSER
	if not equal
		move	NUSEUSER,str1
		loop
			bump	NUSEUSER,1
			cmatch	B1,NUSEUSER
			until equal
			until eos
		repeat
		if not eos
			bump	NUSEUSER,1
			move	NUSEUSER,str6
			clear	userlogn
			pack	userlogn,str1,str6
		endif
	endif
	reset	NUSEUSER
.Open Files
.Trap if files are not there
	trap	NOFILE giving ERROR noreset if IO
.
	move	"NIN PRINT FILE	      ",FMESG
	TRAP	IOMssg Giving Error if IO
	move	"OPEN-ninprint.isi",Location
.	open	ORDPRINT,"NINPRINT"
	open	ORDPRINT,"NINPRINT.isi|10.10.30.103:502"
	move	"OPEN-ninprint.aam",Location
	open	ordprnta,"ninprint.aam|10.10.30.103:502"
.	open	ordprnta, "ninprint"
.	open	ORDPRINT,"NINPRINT"
.	move	"OPEN-ninprint.aam",Location
.	open	ordprnta,"ninprint"
................. Used for backup database..................
.	 open	 ORDPRINT,"NINPRINT.isi|30.2:502"
.	 move	 "OPEN-ninprint.aam",Location
.	 open	 ordprnta,"ninprint.aam|30.2:502"
............................................................
.	open	ORDPRINT,"NINPRINT"
.	open	ordprnta,"ninprint"

	move	"TDMCORD FILE	  ",FMESG
	move	"OPEN-TDMCORD",Location
.	open	 TDMCORD,"TDMCORD"
	open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
	move	"TCdelete FILE	   ",FMESG
	move	"OPEN-TCDELETE",Location
	open	TDMCDEL,"TCDELETE|10.10.30.103:502"
	move	"TDMCsave FILE	   ",FMESG
	move	"OPEN-TdmCords",Location
	open	TDMCsave,"TdmCords|10.10.30.103:502"
	move	"OPEN-LCRFAX",Location
	open	LCRFAXFILE,"LCRFAX|10.10.30.103:502"
.START PATCH 3.76.8 ADDED LOGIC
	move	"OPEN-DMXFILE",Location
	open	DMXFILE,"DMXFILE|10.10.30.103:502"
.END PATCH 3.76.8 ADDED LOGIC
	trapclr	IO
	trap	IOMssg giving ERROR NORESET if IO
.Open Preferences File
openpref
	pack	APIFileName,"c:\progra~1\nincal\nord0001.pre",hexzero
	call	FindFirstFile
	if (APIResult <> 0 & APIResult <> hexeight)
.		 trap	 Preferror if IO
		open	preffile,"c:\progra~1\nincal\nord0001.pre"
		move	C0,N9
		loop
			add	C1,N9
			move	C0,N8
			loop
				add	C1,N8
				read	preffile,seq;OptionsArray(N9,N8)
				until	over
				until (N8 = OptionsArr2)
			repeat
			until (N9 = OptionsArr1)
		repeat
		close	preffile
.		 trapclr io
		move	C0,N9
.Screen1
openpref2
		move	OptionsArray(1,1),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreenInit,0,N5
.
		move	OptionsArray(1,2),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsFaxLONo,0,N5
.Screen	5
		move	OptionsArray(5,1),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreen5BrkFilter,0,N5
.
		move	OptionsArray(5,2),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreen5EMailOption,0,N5
.
		move	OptionsArray(5,3),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreen5FileDefault,0,N5
.Screen	6
.		 move	 OptionsArray(6,1),str5
.		 call	 Trim using str5
.		 move	 str5,N5
.		 setitem OptionsScreen6Init,0,N5
.START PATCH 3.66 ADDED LOGIC
.Screen	7
		move	 OptionsArray(7,1),str5
		call	 Trim using str5
		move	 str5,N5
		setitem OptionsScreen7Proj,0,N5
		if (N5 = C0)
			setprop	Nord01EBButtonViewProj,height=0
		endif
.END PATCH 3.66 ADDED LOGIC
.Screen	8
.		 move	 OptionsArray(8,1),str5
.		 call	 Trim using str5
.		 move	 str5,N5
.		 setitem OptionsScreen8OrderSearch,0,N5
.Screen9
		move	OptionsArray(9,1),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreen9View,0,N5
.Screen10
		move	OptionsArray(10,1),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreen10View,0,N5
.
		move	OptionsArray(10,2),str5
		call	Trim using str5
		move	str5,N5
		setitem	OptionsScreen10Usage,0,N5
	endif
.START PATCH 3.67 ADDED LOGIC
	call	CleanUpLCRFaxFiles
.END PATCH 3.67 ADDED LOGIC
.Reset Color for Disabled items
	call	OrderDisableLower
	call	OrderDisableCampLower
	call	OrderDisableLOLLower
.START PATCH 3.72 ADDED LOGIC
.Nord01ECStatRevise is actually an Edittext Box that is mimicking a StaticText Box
	getprop	Nord01ECStatRtnComp,bgcolor=IntIndex
	setprop	Nord01ECStatRevise,bgcolor=IntIndex
.START PATCH 3.72.9 ADDED LOGIC
	setprop	Nord001bStatClearStat,bgcolor=IntIndex
.END PATCH 3.72.9 ADDED LOGIC
	call	OrderStatDisableLower
.END PATCH 3.72 ADDED LOGIC
	setprop	NORDMSK1ButtonModify,enabled=0
	setprop	NORDMSK1ButtonReprint,enabled=0
.START PATCH 3.47 ADDED LOGIC
	setprop	NORDMSK2ButtonModify,enabled=0
	setprop	NORDMSK3ButtonModify,enabled=0
	setprop	NORDMSK3ButtonCopy,enabled=0
	setprop	NORDMSK3ButtonOrder,enabled=0
	setprop	NORDMSK3ButtonLCR,enabled=0
.START PATCH 3.48 ADDED LOGIC
	setprop	NORDMSK3ButtonProj,enabled=0
.END PATCH 3.48 ADDED LOGIC
	setprop	Nord01EBButtonLCR,enabled=0
	setprop	Nord01EBButtonOrder,enabled=0
	setprop	Nord01EBButtonDelete,enabled=0
	setprop	Nord01EBButtonMove,enabled=0
	setprop	Nord01EBButtonNet,enabled=0
	setprop	Nord01EBButtonSecond,enabled=0
.START PATCH 3.71.3 ADDED LOGIC
	setprop	Nord01EBButtonPrint,enabled=0
.END PATCH 3.71.3 ADDED LOGIC
	setprop	Nord01EBButtonProjection,enabled=0
.END PATCH 3.47 ADDED LOGIC
.
	getitem	OptionsScreenInit,0,N5
	if (N5 = 10)
		call	OrderSwitchTab using C10
	elseif (N5 = 9)
		call	OrderSwitchTab using C9
.		 setfocus PackageEditMlr
	elseif (N5 = 8)
		call	OrderSwitchTab using C8
.START PATCH 3.72 ADDED LOGIC
		setprop	Nord08A1ListViewRef2,left=0
		setprop	NORD008A1,visible=1
.END PATCH 3.72 ADDED LOGIC
		setfocus NORDMSK2EditSearchKey
	elseif (N5 = 7)
		call	OrderSwitchTab using C7
		setfocus NORDMSK2EditSearchKey
	elseif (N5 = 6)
		call	OrderSwitchTab using C6
		setfocus NORDMSK2EditSearchKey
	elseif (N5 = 5)
		call	OrderSwitchTab using C5
		setfocus NORDMSK2EditSearchKey
	elseif (N5 = 4)
		call	OrderSwitchTab using C4
		setfocus NORDMSK1EditSearchKey
	elseif (N5 = 3)
		call	OrderSwitchTab using C3
		setfocus NORDMSK1EditSearchKey
	elseif (N5 = 2)
		call	OrderSwitchTab using C2
		setfocus NORDMSK1EditSearchKey
	else
.START PATCH 3.72 ADDED LOGIC
		setprop	Nord01A1ListViewRef2,left=0
		setprop	NORD001A1,visible=1
.END PATCH 3.72 ADDED LOGIC
		setfocus NORDMSK1EditSearchKey
	endif
	setprop	NORDMSK1ButtonOk,default=1
.Set Locking variables
	move	C3,NORD5LOCK
	move	C3,NCRCLOCK
	setmode	*GRAYSCALE=0
	loop
		eventwait
.		 setitem TIMER,0,80	.8 second - FOR	TESTING
.		setitem	timer,0,18000	.reset to 30 minutes
.START TESTER LOGIC
		deactivate TIMER
		ACTIVATE TIMER,Timeout,RESULT
.END TESTER LOGIC
	repeat

Timeout
.Test to make sure nothing is left open	in Modify Mode
	getprop	Nord001AEditMlr,enabled=N9
	if (N9 = 1)
		call	Quit
	endif
	getprop	Nord01eaEditMlr,enabled=N9
	if (N9 = 1)
		call	Quit2
	endif
	getprop	Nord01ECEditCamp,enabled=N9
	if (N9 = 1)
		call	Quit3
	endif
.START PATCH 3.63 ADDED LOGIC
	getprop	NSTA001AEditPackNum2,enabled=N9
	if (N9 = 1)
		call	Quit4
	endif
.END PATCH 3.63 ADDED LOGIC
	beep
	beep
	beep
	call	OrderCalcCleanUpExcel
	shutdown

SetOrderErrorMssgDefault
.Called	several	sub-routines
.Set Default for OrderFile Maintenance
	setprop	ErrorMssgStat1,visible=0
	setprop	ErrorMssgStat2,visible=0
	setprop	ErrorMssgStat3,visible=0
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"To Search By Record Number:"
	setitem	ErrorMssgStat2,0,"	Enter 6	Digit Number"
	setitem	ErrorMssgStat5,0,"No Records found with	that Criteria!"
	setitem	ErrorMssgOK,0,"O&K"
	setprop	ErrorMssgOK,default=1
	setfocus ErrorMssgOK
	return

.......................................
.............MESSAGE BOXES.............
.Following are dynamically created Message Boxes using Report2,	which only contains a couple of	objects.
.Each time a new Message box is	created:  1)the	Objects	from the previous incarnation must be destroyed
.2)the Form must receive a new Title  3)the Objects must be dumped into	the ObjectColl collection in order
.to facilitate easy destruction.
.Each Message Box may have several associated sub-routines, triggered by object	events,	ie Lost_Focus,
.Click,	etc.
.Keep all dynamic use of Report	2 in this section in order to better monitor its use.  ASH
.......................................
OrderSetScreen5Reports
.Allows	selection of different reports for In-House LCRs
.Called	by:  Nord001DPrint
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Report"
	create	Report2;StatTextBoxes(2)=50:70:10:110,"Report Type","'>MS Sans Serif'(8)"
	create	Report2;StatTextBoxes(3)=70:90:10:110,"Printer","'>MS Sans Serif'(8)"
	create	Report2;ComboBoxes(1)=50:71:80:310,"",";L)CR Request Form-Individual Format;)LCR Request Form-Galley Format;)Broker Clearance Report;)Planner Report"
	move	NO,str1
	trap	OrderSetScreen5ReportsSpoolTrap	if Spool
	PRTOPEN	prfile,"FAXFILE","FAXFILE.PRN"
	PRTCLOSE prfile
	if (str1 = NO)
.START PATCH 3.76.1 REPLACED LOGIC
.		create	Report2;ComboBoxes(2)=70:91:80:310,"",";L)aser 3;).PDF File;)Laser 2;)Fax"
		create	Report2;ComboBoxes(2)=70:91:80:310,"",";L)aser 3;).PDF Email;)Laser 2;).PDF File Dump;)Fax"
.END PATCH 3.76.1 REPLACED LOGIC
	else
.START PATCH 3.76.1 REPLACED LOGIC
.		create	Report2;ComboBoxes(2)=70:91:80:310,"",";L)aser 3;).PDF File;)Laser 2"
		create	Report2;ComboBoxes(2)=70:91:80:310,"",";L)aser 3;).PDF Email;)Laser 2;).PDF File Dump"
.END PATCH 3.76.1 REPLACED LOGIC
	endif
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate ComboBoxes(1)
	activate ComboBoxes(2)
	listins	ObjectColl,StatTextBoxes(2),StatTextBoxes(3),ComboBoxes(1),ComboBoxes(2)
	setfocus ComboBoxes(1)
	return
OrderSetScreen5ReportsSpoolTrap
	move	YES,str1
	return

.START PATCH 3.67 ADDED LOGIC
OrderSetOutsideFaxReport
.Allows	selection of different Printer devices for Outside LCRs
.Called	by:  LETHEAD
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Outside LCRs"
	create	Report2;StatTextBoxes(1)=70:90:10:110,"Device","'>MS Sans Serif'(8)"
	create	Report2;ComboBoxes(1)=70:91:80:310,"",";N)ightly Fax;)Laser 3;).PDF File;)Laser 2;)Fax"
	activate StatTextBoxes(1)
	activate ComboBoxes(1)
	listins	ObjectColl,StatTextBoxes(1),ComboBoxes(1)
	setfocus ComboBoxes(1)
	return
.END PATCH 3.67 ADDED LOGIC

OrderSetCampaignReports
.Allows	selection of Campaign for printing
.Called	by Reports menu, Campaign submenu
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Campaign Report"
	move	NO,RptCan
	create	Report2;mRSearch,"&Search;&Campaign"
	create	Report2;StatTextBoxes(1)=50:70:10:110,"Campaign",""
	create	Report2;StatTextBoxes(2)=70:90:10:110,"Printer",""
	create	Report2;StatTextBoxes(5)=90:110:10:310,"Report Type",""
	create	Report2;StatTextBoxes(3)=130:150:10:310,"",""
	create	Report2;StatTextBoxes(4)=150:170:10:310,"","",fgcolor=red
	create	Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;ComboBoxes(1)=70:91:80:310,"",";P)rint to File;)Laser 3;).PDF File;)Laser 2"
	create	Report2;ComboBoxes(2)=90:111:80:310,"",";S)tandard;)TNC;)NWF//UNICEF"
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	create	Report2;Buttons(2)=180:205:140:190,"&Finish",enabled=0
	activate mRSearch,Report2SearchGo,result
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
	activate StatTextBoxes(5)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
	eventreg EditTextBoxes(1),10,OrderCampaignKeyPress,RESULT=N9
	activate EditTextBoxes(1),OrderSetCampaignEditChange,result
	activate ComboBoxes(1)
	activate ComboBoxes(2)
	activate Buttons(1),OrderSetCampaignOK,result
	activate Buttons(2),OrderSetCampaignFinish,result
	listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4),StatTextBoxes(5):
		EditTextBoxes(1),ComboBoxes(1),ComboBoxes(2),Buttons(1),Buttons(2)
	setfocus EditTextBoxes(1)
	return
Report2SearchGo
	goto SearchGo5
OrderSetCampaignEditChange
	if (result = C2)	.Lost Focus + Change
		setprop	Buttons(2),enabled=0
	endif
	return
OrderSetCampaignOK
	getitem	EditTextBoxes(1),0,str6
	call	Trim using str6
	if (str6 = "")
		setitem	StatTextBoxes(4),0,"Not	a valid	Campaign Number!!"
		setprop	Buttons(2),enabled=0
		setfocus EditTextBoxes(1)
	else
		move	str6,NCMPFLD
		call	ZFILLIT	using NCMPFLD,C0
		setitem	StatTextBoxes(4),0,NCMPFLD
		move	C1,NCMPPATH
		move	"Nord01EBButtonMove-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if over
			setitem	StatTextBoxes(4),0,"Not	a valid	Campaign Number!!"
			setprop	Buttons(2),enabled=0
			setfocus EditTextBoxes(1)
		else
			pack	str25,"Campaign: ",NCMPNUM
			setitem	StatTextBoxes(3),0,str25
			setitem	StatTextBoxes(4),0,NCMPCNAME
			setprop	Buttons(2),enabled=1
			setfocus Buttons(2)
		endif
	endif
	return
OrderSetCampaignFinish
.	 move	 C1,result
	setprop	Report2,visible=0
	return

OrderCampaignKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo5
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return
.START PATCH 3.64 REPLACED LOGIC
.OrderMoveRecord	LRoutine FrmPtr
..Following is some ugly	code!!!
..I am using the	generic	Error message form, hiding the default OK button.
..An Edit Textbox and 3 Buttons are created and placed on the forms.
..The Click event(default) for the Buttons calls	subroutines listed at end.
..MoveEditChange	is the subroutine for the Edit Textbox.	 This subroutine uses
..the return value to determine what type of event was called.  In this case, I
..use the Lost Focus/Change event.  There is no way to access the Change	event by itself.  ASH
.	call	Report2DestroyObjects
.	setprop	Report2,title="LOL Record Copy/Move"
.	create	Report2;mRSearch,"&Search;&Campaign"
.	move	C0,result
.	create	Report2;StatTextBoxes(1)=50:70:10:110,"Campaign",""
.	create	Report2;StatTextBoxes(2)=130:150:10:310,"",""
.	create	Report2;StatTextBoxes(3)=150:170:10:310,"","",fgcolor=red
.	if (FrmPtr = C1)
.		create	Report2;StatTextBoxes(4)=70:90:10:110,"Operation",""
.	endif
.	create	Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
.	if (FrmPtr = C1)
.		create	Report2;ComboBoxes(1)=70:91:80:150,"",";C)opy;)Move"
.	endif
.	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
.	create	Report2;Buttons(2)=180:205:140:190,"&Finish",enabled=0
.	create	Report2;Buttons(3)=180:205:230:280,"C&ancel",zorder=500
.	activate mRSearch,Report2SearchGo,result
.	activate StatTextBoxes(1)
.	activate StatTextBoxes(2)
.	activate StatTextBoxes(3)
.	if (FrmPtr = C1)
.		activate StatTextBoxes(4)
.	endif
..When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
..Any other events must be registered manually.
..Below we register a KeyPress event.
.	eventreg EditTextBoxes(1),10,OrderCampaignKeyPress,RESULT=N9
.	activate EditTextBoxes(1),OrderMoveEditChange,result
.	if (FrmPtr = C1)
.		activate ComboBoxes(1)
.	endif
.	activate Buttons(1),OrderMoveOK,result
.	activate Buttons(2),OrderMoveFinish,result
.	activate Buttons(3),OrderMoveCancel,result
.	listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3):
.		EditTextBoxes(1),Buttons(1),Buttons(2),Buttons(3)
.	if (FrmPtr = C1)
.		listins	ObjectColl,ComboBoxes(1),StatTextBoxes(4)
.	endif
.	setfocus EditTextBoxes(1)
.	setprop	Report2,visible=1
.	return
.....................
OrderMoveRecord	LRoutine FrmPtr
.Following is some ugly	code!!!
.I am using the	generic	Error message form, hiding the default OK button.
.An Edit Textbox and 3 Buttons are created and placed on the forms.
.The Click event(default) for the Buttons calls	subroutines listed at end.
.MoveEditChange	is the subroutine for the Edit Textbox.	 This subroutine uses
.the return value to determine what type of event was called.  In this case, I
.use the Lost Focus/Change event.  There is no way to access the Change	event by itself.  ASH
	call	Report2DestroyObjects
	setprop	Report2,title="LOL Record Copy/Move"
	create	Report2;mRSearch,"&Search;&Campaign"
	move	C0,result
	if (FrmPtr = C2)
		create	Report2;StatTextBoxes(1)=30:50:10:110,"Campaign",""
		create	Report2;StatTextBoxes(2)=90:110:10:310,"",""
		create	Report2;StatTextBoxes(3)=110:130:10:310,"","",fgcolor=red
		create	Report2;StatTextBoxes(4)=70:90:10:110,"Operation",""
		create	Report2;StatTextBoxes(5)=50:70:10:110,"Package",""
		create	Report2;StatTextBoxes(6)=50:70:135:310,"No Selection will use Default.",""
		create	Report2;StatTextBoxes(7)=130:150:10:310,"",""
		create	Report2;StatTextBoxes(8)=150:170:10:310,"","",fgcolor=red
		create	Report2;EditTextBoxes(1)=30:50:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
		create	Report2;EditTextBoxes(2)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
		create	Report2;ComboBoxes(1)=70:91:80:150,"",";C)opy;)Move"
	else
		create	Report2;StatTextBoxes(1)=50:70:10:110,"Campaign",""
		create	Report2;StatTextBoxes(2)=130:150:10:310,"",""
		create	Report2;StatTextBoxes(3)=150:170:10:310,"","",fgcolor=red
		if (FrmPtr = C1)
			create	Report2;StatTextBoxes(4)=70:90:10:110,"Operation",""
		endif
		create	Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
		if (FrmPtr = C1)
			create	Report2;ComboBoxes(1)=70:91:80:150,"",";C)opy;)Move"
		endif
	endif
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	create	Report2;Buttons(2)=180:205:140:190,"&Finish",enabled=0
	create	Report2;Buttons(3)=180:205:230:280,"C&ancel",zorder=500
	activate mRSearch,Report2SearchGo,result
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	if (FrmPtr = 2)
		activate StatTextBoxes(4)
		activate StatTextBoxes(5)
		activate StatTextBoxes(6)
		activate StatTextBoxes(7)
		activate StatTextBoxes(8)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
		eventreg EditTextBoxes(1),10,OrderCampaignKeyPress,RESULT=N9
		activate EditTextBoxes(1),OrderMoveEditChange,result
		activate EditTextBoxes(2)
		activate ComboBoxes(1)
		activate Buttons(1),OrderMoveOK,result
		activate Buttons(2),OrderMoveFinish,result
		activate Buttons(3),OrderMoveCancel,result
		listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3):
			StatTextBoxes(4),StatTextBoxes(5),StatTextBoxes(6),StatTextBoxes(7),StatTextBoxes(8):
			EditTextBoxes(1),EditTextBoxes(2),Buttons(1),Buttons(2),Buttons(3),ComboBoxes(1)
	else
		if (FrmPtr = C1)
			activate StatTextBoxes(4)
		endif
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
		eventreg EditTextBoxes(1),10,OrderCampaignKeyPress,RESULT=N9
		activate EditTextBoxes(1),OrderMoveEditChange,result
		if (FrmPtr = C1)
			activate ComboBoxes(1)
		endif
		activate Buttons(1),OrderMoveOK,result
		activate Buttons(2),OrderMoveFinish,result
		activate Buttons(3),OrderMoveCancel,result
		listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3):
			EditTextBoxes(1),Buttons(1),Buttons(2),Buttons(3)
		if (FrmPtr = C1)
			listins	ObjectColl,ComboBoxes(1),StatTextBoxes(4)
		endif
	endif
	setfocus EditTextBoxes(1)
	setprop	Report2,visible=1
	return
.END PATCH 3.64 REPLACED LOGIC
OrderMoveEditChange
	if (result = C2)	.Lost Focus + Change
		setprop	Buttons(2),enabled=0
	endif
	return
OrderMoveOK
	getitem	EditTextBoxes(1),0,str6
	call	Trim using str6
	if (str6 = "")
		setitem	StatTextBoxes(2),0,""
		setitem	StatTextBoxes(3),0,"Not	a valid	Campaign Number!!"
		setprop	Buttons(2),enabled=0
		setfocus EditTextBoxes(1)
	elseif (str6 = str9)
		setitem	StatTextBoxes(2),0,""
		setitem	StatTextBoxes(3),0,"You	cannot use existing Campaign!!"
		setprop	Buttons(2),enabled=0
		setfocus EditTextBoxes(1)
	else
		move	str6,NCMPFLD
		call	ZFILLIT	using NCMPFLD,C0
		setitem	EditTextBoxes(1),0,NCMPFLD
		move	C1,NCMPPATH
		move	"Nord01EBButtonMove-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if over
			setitem	StatTextBoxes(2),0,""
			setitem	StatTextBoxes(3),0,"Not	a valid	Campaign Number!!"
			setprop	Buttons(2),enabled=0
			setfocus EditTextBoxes(1)
		else
			pack	str25,"Campaign: ",NCMPNUM
			setitem	StatTextBoxes(2),0,str25
			setitem	StatTextBoxes(3),0,NCMPCNAME
.START PATCH 3.64 REPLACED LOGIC
			if (FrmPtr = C2)
				clear	NPKGNUM
				getitem	EditTextBoxes(2),0,str6
				call	Trim using str6
				if (str6 <> "")
					call	ZFillIt using str6,C0
.START PATCH 3.75.7 REPLACED LOGIC - REMOVE TEMPORARY PATCH
..START PATCH 3.75.4 REPLACED LOGIC
..					pack	NPKGFLD,NCMPMLR,str6
.					move	"Nord01EBButtonMove-COMPKEY3",Location
.					pack	COMPFLD3,NCMPMLR
.					pack	KeyLocation,"Key: ",COMPFLD3
.					call	COMPKEY3
.					if over
.						clear	COMPNUM
.					endif
.					pack	NPKGFLD,COMPNUM,str6
..END PATCH 3.75.4 REPLACED LOGIC
					pack	NPKGFLD,NCMPMLR,str6
.END PATCH 3.75.7 REPLACED LOGIC - REMOVE TEMPORARY PATCH
					setitem	EditTextBoxes(2),0,str6
					move	C1,NPKGPATH
					move	"Nord01EBButtonMove-NPKGKEY",Location
					pack	KeyLocation,"Key: ",NPKGFLD
					call	NPKGKEY
					if over
						setitem	StatTextBoxes(7),0,""
						setitem	StatTextBoxes(8),0,"Not	a valid	Package for Campaign Mailer!!"
						setprop	Buttons(2),enabled=0
						setfocus EditTextBoxes(2)
						return
					endif
					pack	str25,"Package: ",NPKGNUM
					setitem	StatTextBoxes(7),0,str25
					setitem	StatTextBoxes(8),0,NPKGPNAME
				else
					setitem	StatTextBoxes(7),0,""
					setitem	StatTextBoxes(8),0,""
				endif
			endif
.END PATCH 3.64 REPLACED LOGIC
			setprop	Buttons(2),enabled=1
			setfocus Buttons(2)
		endif
	endif
	return
OrderMoveFinish
	move	C1,result
	setprop	Report2,visible=0
	if (FrmPtr = C1)
		getitem	ComboBoxes(1),0,CampOp
	endif
	call	OrderSetMouseBusy
	return
OrderMoveCancel
	move	C2,result
	setprop	Report2,visible=0
	return

OrderSetLCRReports
.Allows	selection of Campaign for printing
.Called	by Reports menu, Campaign submenu
	call	Report2DestroyObjects
	setprop	Report2,title="NIN LCR Report"
	move	NO,RptCan
	create	Report2;StatTextBoxes(1)=10:30:10:110,"From Date",""
	create	Report2;StatTextBoxes(2)=30:50:10:110,"To Date",""
	create	Report2;StatTextBoxes(3)=50:70:10:310,"Group",""
	create	Report2;StatTextBoxes(4)=90:110:10:310,"Type",""
	create	Report2;StatTextBoxes(5)=110:130:10:310,"Printer",""
	create	Report2;EditTextBoxes(1)=10:30:80:160,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;EditTextBoxes(2)=30:50:80:160,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;ComboBoxes(1)=50:71:80:310,"",";L)CR & Pending Order;)LCR Only;)Pending	Order Only;)Contact-select below;)Caller-select	below"
	create	Report2;ComboBoxes(2)=70:91:80:310,"",";)  ;)  ;)  ;)  ;)  ;)  ;)  ;)  ;)  "
	create	Report2;ComboBoxes(3)=90:111:80:310,"",";S)ummary;)Detail"
	create	Report2;ComboBoxes(4)=110:131:80:310,"",";L)aser 3 [Sales Area];)Laser 2 [List Management Area];)Laser 6 [Information Services];).PDF File"
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
	activate StatTextBoxes(5)
	activate EditTextBoxes(1),OrderSetLCRFromEditChange,result
	activate EditTextBoxes(2),OrderSetLCRToEditChange,result
	activate ComboBoxes(1)
	activate ComboBoxes(2)
	activate ComboBoxes(3)
	activate ComboBoxes(4)
	deleteitem ComboBoxes(2),0
	insertitem ComboBoxes(2),N2," "
	move	"-4",SEQ
	loop
		move	"SetLCRback-NCNTSEQ",Location
		call	NCNTSEQ
		until over
	repeat
	move	"-1",SEQ
	loop
		move	C1,NCNTPATH
		move	"SetLCR-NCNTSEQ",Location
		call	NCNTSEQ
		until over
		if (CNTCNT = "1")
			pack	str45,CNTNAME,B1,CNTNUM
			add	C1,N2
			insertitem ComboBoxes(2),N2,str45
		endif
	repeat
	setitem	ComboBoxes(4),0,1
	activate Buttons(1),OrderSetLCROK,result
	listins	ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4):
		StatTextBoxes(5),EditTextBoxes(1),EditTextBoxes(2):
		ComboBoxes(1),ComboBoxes(2),ComboBoxes(3),ComboBoxes(4),Buttons(1)
	setfocus EditTextBoxes(1)
	return
OrderSetLCRFromEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(1),0,str10
		call	Trim using str10
		count	N9,str10
		if (N9 = 8)
			unpack	str10,MM,DD,CC,YY
			pack	str10,MM,SLASH,DD,SLASH,CC,YY
			setitem	EditTextBoxes(1),0,str10
		endif
	endif
	return
OrderSetLCRToEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(2),0,str10
		call	Trim using str10
		count	N9,str10
		if (N9 = 8)
			unpack	str10,MM,DD,CC,YY
			pack	str10,MM,SLASH,DD,SLASH,CC,YY
			setitem	EditTextBoxes(2),0,str10
		endif
	endif
	return
OrderSetLCROK
	getitem	EditTextBoxes(1),0,str10
	call	Trim using str10
	count	N9,str10
	if (N9 <> 0)
		if (N9 <> 8 AND	N9 <> 10)
			alert	caution,"From Date must	be in MMDDYYYY format!",result
			setfocus EditTextBoxes(1)
			goto	OrderLCRReportReturn
		elseif (N9 = 10)
			unpack	str10,MM,str1,DD,str1,CC,YY
		else
			unpack	str10,MM,DD,CC,YY
		endif
		pack	str10,CC,YY,MM,DD
	else
		pack	str10,"00000000"
	endif
.
	getitem	EditTextBoxes(2),0,str11
	call	Trim using str11
	count	N9,str11
	if (N9 <> 0)
		if (N9 <> 8 AND	N9 <> 10)
			alert	caution,"To Date must be in MMDDYYYY format!",result
			setfocus EditTextBoxes(2)
			goto	OrderLCRReportReturn
		elseif (N9 = 10)
			unpack	str11,MM,str1,DD,str1,CC,YY
		else
			unpack	str11,MM,DD,CC,YY
		endif
		pack	str11,CC,YY,MM,DD
	else
		pack	str11,"99999999"
	endif
.
	getitem	ComboBoxes(1),0,N1
	sub	C1,N1
	move	N1,str1
	if (N1 > 2)
		getitem	ComboBoxes(2),0,N2
		if (N2 <= 1)
			alert	caution,"You must supply Contact/Caller!",result
			setfocus ComboBoxes(2)
			goto	OrderLCRReportReturn
		endif
		getitem	ComboBoxes(2),N2,str45
		unpack	  str45,CNTNAME,B1,CNTNUM
	else
		pack	CNTNUM,"  "
		setitem	ComboBoxes(2),0,1
	endif
.START PATCH 3.4 REPLACED LOGIC
.	getitem	ComboBoxes(3),0,N3
.	if (N3 = 4)
.		alert	caution,"PDF format is currently not an	option!",result
.		setfocus ComboBoxes(3)
.		goto	OrderLCRReportReturn
.	endif
.	move	N3,str3
.	call	Trim using str3
.	getitem	ComboBoxes(4),0,N4
.	move	N4,str4
.	call	Trim using str4
.....................
	getitem	ComboBoxes(3),0,N3
	move	N3,str3
	call	Trim using str3
	getitem	ComboBoxes(4),0,N4
	if (N4 = 4)
		alert	caution,"PDF format is currently not an	option!",result
		setfocus ComboBoxes(4)
		goto	OrderLCRReportReturn
	endif
	move	N4,str4
	call	Trim using str4
.END PATCH 3.4 REPLACED	LOGIC
	setprop	Report2,visible=0
	return
OrderLCRReportReturn
	setprop	Report2,visible=1
	return

OrderLCRMlrKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo3
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return
OrderLCRListKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo2
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return

.START PATCH 3.72.7 ADDED LOGIC
OrderSetLMLCRReports
.Allows	selection of Campaign for printing
.Called	by Reports menu, Campaign submenu
	call	Report2DestroyObjects
	setprop	Report2,title="NIN LM Cancelled/Denied Report"
	move	NO,RptCan
	create	Report2;StatTextBoxes(1)=10:30:10:110,"From Date",""
	create	Report2;StatTextBoxes(2)=10:30:165:265,"To Date",""
	create	Report2;StatTextBoxes(3)=30:50:10:310,"Type 1",""
	create	Report2;StatTextBoxes(4)=50:70:10:310,"Type 2",""
	create	Report2;StatTextBoxes(5)=70:90:10:310,"Group",""
	create	Report2;StatTextBoxes(6)=90:110:10:110,"Lists",""
	create	Report2;StatTextBoxes(7)=90:110:165:265,"Mailers",""
	create	Report2;EditTextBoxes(1)=10:30:80:160,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;EditTextBoxes(2)=10:30:230:310,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
.START PATCH 3.72.9 REPLACED LOGIC
.	create	Report2;ComboBoxes(1)=30:51:80:310,"",";L)CR & Pending Order;)LCR Only;)Pending	Order Only"
	create	Report2;ComboBoxes(1)=30:51:80:310,"",";L)CR & Pending Order;)LCR Only;)Pending	Order Only;)Live & Billed Orders"
.END PATCH 3.72.9 REPLACED LOGIC
	create	Report2;ComboBoxes(2)=50:71:80:310,"",";C)ancelled & Denied;)Cancelled Only;)Denied Only"
	create	Report2;ComboBoxes(3)=70:91:80:310,"",";A)ll Teams;)LM Only;)Sales Only"
	create	Report2;EditTextBoxes(3)=90:110:60:110,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;Buttons(2)=90:110:110:130,"+"
	create	Report2;ListViews(1)=110:180:10:160,MultiSelect=1,HideColHdr=1,FullRow=1
	create	Report2;Buttons(3)=90:110:130:150,"-"
	create	Report2;EditTextBoxes(4)=90:110:210:260,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;Buttons(4)=90:110:260:280,"+"
	create	Report2;ListViews(2)=110:180:165:315,MultiSelect=1,HideColHdr=1,FullRow=1
	create	Report2;Buttons(5)=90:110:280:300,"-"
	create	Report2;CheckBoxes(1)=210:230:10:300,"Use Denied/Approval Date as Default"
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
	activate StatTextBoxes(5)
	activate StatTextBoxes(6)
	activate StatTextBoxes(7)
	activate EditTextBoxes(1),OrderSetLMLCRFromEditChange,result
	activate EditTextBoxes(2),OrderSetLMLCRToEditChange,result
.Register LostFocus Event on EditTextBoxes so that corresponding "+/-" Buttons will have default
	eventreg EditTextBoxes(3),9,OrderSetLMLCRGF1
	eventreg EditTextBoxes(4),9,OrderSetLMLCRGF2
.Register LostFocus Event on EditTextBoxes so that OK Button will have default
	eventreg EditTextBoxes(3),11,OrderSetLMLCRLF
	eventreg EditTextBoxes(4),11,OrderSetLMLCRLF
	activate EditTextBoxes(3)
	activate EditTextBoxes(4)
	activate ComboBoxes(1)
	activate ComboBoxes(2)
	activate ComboBoxes(3)
.Register LostFocus Event on Buttons so that OK Button will have default
	eventreg Buttons(1),11,OrderSetLMLCRLF
	eventreg Buttons(2),11,OrderSetLMLCRLF
	eventreg Buttons(3),11,OrderSetLMLCRLF
	eventreg Buttons(4),11,OrderSetLMLCRLF
	activate Buttons(1),OrderSetLMLCROK,result
	activate Buttons(2),OrderLMLCRAddList,result
	activate Buttons(3),OrderLMLCRSubList,result
	activate Buttons(4),OrderLMLCRAddMailer,result
	activate Buttons(5),OrderLMLCRSubMailer,result
.Register LostFocus Event on ListViews so that corresponding "+/-" Buttons will have default
	eventreg ListViews(1),9,OrderSetLMLCRGF3
	eventreg ListViews(2),9,OrderSetLMLCRGF4
.Register LostFocus Event on ListViews so that OK Button will have default
	eventreg ListViews(1),11,OrderSetLMLCRLF
	eventreg ListViews(2),11,OrderSetLMLCRLF
	activate ListViews(1)
	activate ListViews(2)
	activate CheckBoxes(1)
	ListViews(1).InsertColumn using "",50,0
	ListViews(1).InsertColumn using "",80,1
	ListViews(2).InsertColumn using "",50,0
	ListViews(2).InsertColumn using "",80,1
	setitem	CheckBoxes(1),0,1
	listins	ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4),StatTextBoxes(5):
		StatTextBoxes(6),StatTextBoxes(7),EditTextBoxes(1),EditTextBoxes(2),EditTextBoxes(3),EditTextBoxes(4):
		ComboBoxes(1),ComboBoxes(2),ComboBoxes(3),Buttons(1),Buttons(2),Buttons(3),Buttons(4),Buttons(5):
		ListViews(1),ListViews(2),CheckBoxes(1)
	setfocus EditTextBoxes(1)
	return
OrderSetLMLCRFromEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(1),0,str10
		call	Trim using str10
		count	N9,str10
		if (N9 = 8)
			unpack	str10,MM,DD,CC,YY
			pack	str10,MM,SLASH,DD,SLASH,CC,YY
			setitem	EditTextBoxes(1),0,str10
		endif
	endif
	return
OrderSetLMLCRToEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(2),0,str10
		call	Trim using str10
		count	N9,str10
		if (N9 = 8)
			unpack	str10,MM,DD,CC,YY
			pack	str10,MM,SLASH,DD,SLASH,CC,YY
			setitem	EditTextBoxes(2),0,str10
		endif
	endif
	return
OrderSetLMLCRGF1
.Got Focus
	setprop	Buttons(2),default=1
	return
OrderSetLMLCRGF2
.Got Focus
	setprop	Buttons(4),default=1
	return
OrderSetLMLCRGF3
.Got Focus
	setprop	Buttons(3),default=1
	return
OrderSetLMLCRGF4
.Got Focus
	setprop	Buttons(5),default=1
	return
OrderSetLMLCRLF
.Lost Focus
	setprop	Buttons(1),default=1
	return
OrderSetLMLCROK
	getitem	EditTextBoxes(1),0,str10
	call	Trim using str10
	count	N9,str10
	if (N9 <> 0)
		if (N9 <> 8 AND	N9 <> 10)
			alert	caution,"From Date must	be in MMDDYYYY format!",result
			setfocus EditTextBoxes(1)
			goto	OrderLMLCRReportReturn
		elseif (N9 = 10)
			unpack	str10,MM,str1,DD,str1,CC,YY
		else
			unpack	str10,MM,DD,CC,YY
		endif
		pack	str10,CC,YY,MM,DD
	else
		pack	str10,"00000000"
	endif
.
	getitem	EditTextBoxes(2),0,str11
	call	Trim using str11
	count	N9,str11
	if (N9 <> 0)
		if (N9 <> 8 AND	N9 <> 10)
			alert	caution,"To Date must be in MMDDYYYY format!",result
			setfocus EditTextBoxes(2)
			goto	OrderLMLCRReportReturn
		elseif (N9 = 10)
			unpack	str11,MM,str1,DD,str1,CC,YY
		else
			unpack	str11,MM,DD,CC,YY
		endif
		pack	str11,CC,YY,MM,DD
	else
		pack	str11,"99999999"
	endif
.
	getitem	EditTextBoxes(3),0,str6
	call	Trim using str6
	if (str6 <> "")
		call	OrderLMLCRAddList
	endif
	getitem	EditTextBoxes(4),0,str4
	call	Trim using str4
	if (str4 <> "")
		call	OrderLMLCRAddMailer
	endif
.
	erase	"c:\work\LCRDeny.dat"
	prepare	tempfile,"c:\work\LCRDeny.dat"
	write	tempfile,SEQ;str10
	write	tempfile,SEQ;str11
.
	getitem	ComboBoxes(1),0,N1
	write	tempfile,SEQ;N1
.START PATCH 3.72.9 REPLACED LOGIC
.	getitem	ComboBoxes(2),0,N1
	getitem	ComboBoxes(2),0,N2
	if (N1 = 4 & N2 <> 2)
		pack	taskname,"Denied Records are not an option for Live Orders!",newline,"You will only receive Cancelled Records."
		alert	note,taskname,result
		move	C2,N1
	else
		move	N2,N1
	endif
.END PATCH 3.72.9 REPLACED LOGIC
	write	tempfile,SEQ;N1
	getitem	ComboBoxes(3),0,N1
	write	tempfile,SEQ;N1
	getitem	CheckBoxes(1),0,N1
	write	tempfile,SEQ;N1
.
	move	SEQ,result
	loop
		move	result,N9
		ListViews(1).GetNextItem giving result using 0,N9
 		until (result = SEQ)
		ListViews(1).GetItemText giving str6 using result
		call	Trim using str6		.Extra Precaution
		if (str6 <> "")
			pack	str12,"LIST  ",str6
			write	tempfile,SEQ;str12
		endif
	repeat
.
	move	SEQ,result
	loop
		move	result,N9
		ListViews(2).GetNextItem giving result using 0,N9
 		until (result = SEQ)
		ListViews(2).GetItemText giving str6 using result
		call	Trim using str6		.Extra Precaution
		if (str6 <> "")
			pack	str12,"MAILER",str6
			write	tempfile,SEQ;str12
		endif
	repeat
	close	tempfile
.
	setprop	Report2,visible=0
	return
OrderLMLCRAddList
	setprop	Report2,visible=1
	getitem	EditTextBoxes(3),0,str6
	call	Trim using str6
	call	ZFillIt using str6
	packkey	NDATFLD,str6
	move	"OLMLCRAdd-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	move	C1,NDATPATH
	call	NDATKEY
	if not over
		ListViews(1).GetItemCount giving howmany
		sub	C1,howmany
		for result,C0,howmany
			ListViews(1).GetItemText giving str6 using result
			if (str6 = NDATFLD)
				return
			endif
		repeat
		ListViews(1).InsertItem giving N9 using NDATFLD,9999
		ListViews(1).SetItemText using N9,OLSTNAME,1
	endif
	setitem	EditTextBoxes(3),0,""
	setfocus EditTextBoxes(3)
	return
OrderLMLCRSubList
.Test first to make sure there are records highlighted
	move	SEQ,result
	move	result,N9
	ListViews(1).GetNextItem giving result using C2,N9
	if (result = SEQ)
		return
	endif
	move	SEQ,result
	loop
		move	result,N9
		ListViews(1).GetNextItem giving result using C2,N9
 		until (result = SEQ)
		ListViews(1).DeleteItem using result
		sub	C1,result
	repeat
	return
OrderLMLCRAddMailer
	getitem	EditTextBoxes(4),0,str4
	call	Trim using str4
	call	ZFillIt using str4
	packkey	MKEY,str4,"000"
	move	"OLMLCRAdd-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	move	C1,NMLRPATH
	call	NMLRKEY
	if not over
		ListViews(2).GetItemCount giving howmany
		sub	C1,howmany
		for result,C0,howmany
			ListViews(2).GetItemText giving str4 using result
			if (str4 = MNUM)
				return
			endif
		repeat
		ListViews(2).InsertItem giving N9 using MNUM,9999
		ListViews(2).SetItemText using N9,MCOMP,1
	endif
	setitem	EditTextBoxes(4),0,""
	setfocus EditTextBoxes(4)
	return

OrderLMLCRSubMailer
.Test first to make sure there are records highlighted
	move	SEQ,result
	move	result,N9
	ListViews(2).GetNextItem giving result using C2,N9
	if (result = SEQ)
		return
	endif
	move	SEQ,result
	loop
		move	result,N9
		ListViews(2).GetNextItem giving result using C2,N9
 		until (result = SEQ)
		ListViews(2).DeleteItem using result
		sub	C1,result
	repeat
	return
OrderLMLCRReportReturn
	setprop	Report2,visible=1
	return
.END PATCH 3.72.7 ADDED LOGIC

OrderXRefVerify
.Allows	selection of Cross Reference Mailers
.Called	by OrderExchangeStatusB
	call	Report2DestroyObjects
	setprop	Report2,title="NIN XREF File Maintenance"
	move	NO,RptCan
	create	Report2;mRSearch,"&Search;&Mailer"
	create	Report2;StatTextBoxes(1)=50:70:10:110,"Mailer",""
	create	Report2;StatTextBoxes(2)=130:150:10:310,"",""
	create	Report2;StatTextBoxes(3)=150:170:10:310,"",""
	create	Report2;StatTextBoxes(4)=70:90:10:310,"",""
	create	Report2;EditTextBoxes(1)=50:70:80:130,MaxChars=4,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	create	Report2;Buttons(2)=180:205:140:190,"Finish",enabled=0
	activate mRSearch,OrderXRefSearchGo,result
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
	eventreg EditTextBoxes(1),10,OrderXRefKeyPress,RESULT=N9
	activate EditTextBoxes(1),OrderXRefEditChange,result
	activate Buttons(1),OrderXRefOK,result
	activate Buttons(2),OrderXRefFinish,result
	listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3),StatTextBoxes(4):
		EditTextBoxes(1),Buttons(1),Buttons(2)
	setfocus EditTextBoxes(1)
	return
OrderXRefSearchGo
	goto SearchGo3
OrderXRefEditChange
	if (result = C2)	.Lost Focus + Change
		setprop	Buttons(2),enabled=0
		setitem	StatTextBoxes(2),0,""
		setitem	StatTextBoxes(3),0,""
		setitem	StatTextBoxes(4),0,""
		getitem	EditTextBoxes(1),0,TEMMLR2
		call	Trim using TEMMLR2
		call	ZFILLIT	using TEMMLR2,C0
		setitem	EditTextBoxes(1),0,TEMMLR2
	endif
	return
OrderXRefOK
	getitem	EditTextBoxes(1),0,TEMMLR2
	call	Trim using TEMMLR2
	call	ZFILLIT	using TEMMLR2,C0
	if (TEMMLR2 = "" | TEMMLR2 = "0000")
		setitem	StatTextBoxes(2),0,""
		setitem	StatTextBoxes(4),0,""
		setitem	StatTextBoxes(3),0,"Not	a valid	Mailer Number!!"
		setprop	Buttons(2),enabled=0
		setfocus EditTextBoxes(1)
	else
		pack	MKEY,TEMMLR2,"000"
		setitem	StatTextBoxes(3),0,MKEY
		move	C1,NMLRPATH
		move	"OrderXRefOK-NMLRKEY",Location
		pack	KeyLocation,"Key: ",MKEY
		call	NMLRKEY
		if over
			setitem	StatTextBoxes(2),0,""
			setitem	StatTextBoxes(4),0,""
			setitem	StatTextBoxes(3),0,"Not	a valid	Mailer Number!!"
			setprop	Buttons(2),enabled=0
			setfocus EditTextBoxes(1)
		else
			if (TEMMLR2 <> TEMMLR)
				scan	YES,XrefFlag
				if equal
					move	"B",XrefFlag	    .Mailer XRef has changed
				endif
				reset	XrefFlag
			endif
			pack	str25,"Mailer: ",MNUM
			setitem	StatTextBoxes(2),0,str25
			setitem	StatTextBoxes(3),0,MCOMP
			if (MRCODE = "R")
				setitem	StatTextBoxes(4),0,"Rental Only!"
			else
				setitem	StatTextBoxes(4),0,"If Mailer Info is Correct, click 'Finish'"
			endif
			setprop	Buttons(2),enabled=1
			setfocus Buttons(2)
		endif
	endif
	pack	MKEY,OMLRNUM,OCOBN
	move	C1,NMLRPATH
	move	"OrderXRefOK2-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	call	NMLRKEY
	return
OrderXRefFinish
	setprop	Buttons(2),enabled=0
	setprop	Report2Cancel,enabled=0
.START PATCH 3.6 REPLACED LOGIC
.	if (mod	= 3 OR (mod = 7	AND NORD5STAT <> "04"))
	if (mod = 8 | mod = 3 OR (mod = 7 AND NORD5STAT <> "04"))
.END PATCH 3.6 REPLACED LOGIC
		goto OrderXRefFinishXRef
	endif
	move	TEMMLR2,TEMMLR	   .refresh TEMMLR in order to update or write to file
	reset	XrefFlag
	scan	NO,XrefFlag
	if equal	*WAS NOT IN LIST MLR FILE
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.		move	TEMMLR,NXRFMLR
		pack	COMPFLD3,TEMMLR
		move	"XRefF-COMPKEY3",Location
		pack	KeyLocation,"Key: ",COMPFLD3
		call	COMPKEY3
		move	COMPNUM,NXRFMLR
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
		move	OLNUM,NXRFLIST
.START PATCH 3.77.1 REPLACED LOGIC
.		call	NXRFWRT
.		IF (LRINIT = 1)
.		move	"NINXRF	- OrderXRefFinish",str45
.		call	OrderWriteLRFile using str45
.		ENDIF
		move	C1,NXRFPATH
		pack	NXRFFLD,NXRFLIST
		move	"XRefF-NXRFTST",Location
		pack	KeyLocation,"Key: ",NXRFFLD
		call	NXRFTST
		if over
			move	"XRefF-NXRFWRT",Location
			call	NXRFWRT
			IF (LRINIT = 1)
			move	"NINXRF	- OrderXRefFinish",str45
			call	OrderWriteLRFile using str45
			ENDIF
		endif
.END PATCH 3.77.1 REPLACED LOGIC
	endif
	reset	XrefFlag
	scan	"B",XrefFlag
	if equal	*LISTMLR FILE WAS WRONG
		alert	plain,"Update List/Mlr XREF File?",result
		if (result = C1)
.Who is	doing an update, and what are they trying to update?!?
			move	"This is a message from	NORDTEST",SmtpSubject Subject
.   Set	the text message that is send with the attachments
			clear	str45
			append	INITS,str45
			append	" tried	to update XREF file.",str45
			reset	str45
			move	str45,SmtpTextMessage(1)   Array <Text message >
			clear	str45
			append	"List: ",str45
			append	OLNUM,str45
			reset	str45
			move	str45,SmtpTextMessage(2)   Array <Text message >
			clear	str45
			append	"Old Mailer: ",str45
			append	NXRFMLR,str45
			reset	str45
			move	str45,SmtpTextMessage(3)   Array <Text message >
			clear	str45
			append	"New Mailer: ",str45
			append	TEMMLR,str45
			reset	str45
			move	str45,SmtpTextMessage(4)   Array <Text message >
			clear	str45
			append	"LR: ",str45
			append	OLRN,str45
			reset	str45
			move	str45,SmtpTextMessage(5)   Array <Text message >
			move	"5",SmtpTextIndexLast				    Index to last entry	in TextMessage array
			move	"NTS4",SmtpEmailServer			 Address of email serverc
			move	"InformationServices@nincal.com",SmtpEmailAddress
			move	"InformationServices",SmtpUserName				  User name
			move	"InformationServices",SmtpUserFullName		    User Full Name
			move	smtpemailaddress,SmtpDestinations(1,1)
			move	"1",SmtpDestIndexLast				    Index to last entry	in Dest	array
			move	"0",SmtpAttIndexLast				    Index to last entry	- Only 1 entry
			clear	SmtpLogFile					    'Clear' disables the LogFile
			call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
.
.			 move	 TEMMLR,NXRFMLR
.			 call	 NXRFUPD				 10/23/00 *turned off jd/dlh.				IF (LRINIT = 1)
.			 move	 "NINXRF - Update,OrderXRefFinish",str45
.			 call	 OrderWriteLRFile using	str45
		elseif (result = C3)
			setitem	EditTextBoxes(1),0,TEMMLR
			goto OrderXRefOK
		endif
	endif
OrderXRefFinishXRef
	setprop	Report2,visible=0
	setprop	Report2Cancel,enabled=1
	cmatch	"R",MRCODE	*EXCHANGES OK?
	goto OrderExchangeStatus if equal
	return

OrderXRefKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo3
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return

..Routine which	destroys all objects created from above	routines
Report2DestroyObjects
	destroy	ObjectColl
	return
Order5SelectInstructions
.START PATCH	3.78.2	REPLACED LOGIC7
	move	NO,RptCan
.	move	C0,howmany
.	call	Report2DestroyObjects
.	setprop	Report2,title="NIN File Select"
.	create	Report2;StatTextBoxes(1)=50:70:50:310,"Select Internal Notes or	Special	Instructions.",""
.	create	Report2;Buttons(1)=180:205:50:150,"Internal Notes",zorder=500
.	create	Report2;Buttons(2)=180:205:180:280,"Spec. Instructions",zorder=500,default=1
.	activate StatTextBoxes(1)
.	activate Buttons(1),Nord001DSelectInternal,result
.	activate Buttons(2),Nord001DSelectExternal,result
.	listins	ObjectColl,StatTextBoxes(1),Buttons(1),Buttons(2)
.	setfocus Buttons(2)
.	setprop	Report2,visible=1
.	move	C0,howmany
	call	Report2DestroyObjects
	setprop	Report2,title="NIN File Select"
	create	Report2;StatTextBoxes(1)=50:70:50:325,"Select one or more categories in which to save notes:",""
	create	Report2;CheckBoxes(1)=120:150:25:100,"Internal Notes", zorder=500
	create	Report2;CheckBoxes(2)=120:150:125:200,"Spec. Instr.", zorder=500
	create	Report2;CheckBoxes(3)=120:150:225:300,"Mailer Notes", zorder=500
	setitem	CheckBoxes(1),0,C1  // default
	setitem	CheckBoxes(3),0,C1  // default
	activate StatTextBoxes(1)
	activate CheckBoxes(1)
	activate CheckBoxes(2)
	activate CheckBoxes(3)
	setfocus Report2OK
	setprop Report2OK, default=1
	listins	ObjectColl,StatTextBoxes(1),CheckBoxes(1),CheckBoxes(2),CheckBoxes(3)
	setprop	Report2,visible=1
.END PATCH	3.78.2	REPLACED LOGIC
	return
.START PATCH	3.78.2	REMOVED LOGIC
.Nord001DSelectInternal
.	move	C1,howmany
.	setprop	Report2,visible=0
.	return
.Nord001DSelectExternal
.	move	C2,howmany
.	setprop	Report2,visible=0
.	return
.END PATCH	3.78.2	REPLACED LOGIC
Order5SelectRecipient
	move	NO,RptCan
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Select Recipient"
	create	Report2;StatTextBoxes(1)=50:70:100:310,"Select Email Recipient(s)",""
	create	Report2;Buttons(1)=180:205:50:110,"&Contact(s)",zorder=500,default=1
	create	Report2;Buttons(2)=180:205:140:200,"C&aller(s)"
	activate StatTextBoxes(1)
	activate Buttons(1),Nord001DSelectContact,result
	activate Buttons(2),Nord001DSelectCaller,result
	listins	ObjectColl,StatTextBoxes(1),Buttons(1),Buttons(2)
	setfocus Buttons(1)
	setprop	Report2,visible=1
	return

Nord001DSelectContact
	move	C1,howmany
	setprop	Report2,visible=0
	return
Nord001DSelectCaller
	move	C2,howmany
	setprop	Report2,visible=0
	return

OrderSetGetNetScreen
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Net Calculations"
	move	NO,RptCan
	create	Report2;StatTextBoxes(1)=10:30:10:70,"Mailer ##",""
	create	Report2;StatTextBoxes(2)=10:30:130:320,"",""
	create	Report2;StatTextBoxes(3)=30:50:10:70,"List ##",""
	create	Report2;StatTextBoxes(4)=30:50:130:320,"",""
	create	Report2;StatTextBoxes(5)=50:70:10:70,"From Date",""
	create	Report2;StatTextBoxes(6)=50:70:170:230,"To Date",""
	create	Report2;StatTextBoxes(7)=210:230:10:280,"","",fgcolor=red
	create	Report2;EditTextBoxes(1)=10:30:70:120,MaxChars=4,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;EditTextBoxes(2)=30:50:70:120,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;EditTextBoxes(3)=50:70:70:160,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;EditTextBoxes(4)=50:70:230:320,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;ListViews(1)=70:179:10:319,fullrow=1
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
	activate StatTextBoxes(5)
	activate StatTextBoxes(6)
	activate StatTextBoxes(7)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
.Below we register a KeyPress event.
	eventreg EditTextBoxes(1),10,OrderNetMailerKeyPress,RESULT=N9
	eventreg EditTextBoxes(2),10,OrderNetListKeyPress,RESULT=N9
	activate EditTextBoxes(1),OrderNetMlrEditChange,result
	activate EditTextBoxes(2),OrderNetListEditChange,result
	activate EditTextBoxes(3)
	activate EditTextBoxes(4)
	eventreg ListViews(1),6,OrderNetListViewDouble
	eventreg ListViews(1),10,OrderNetListViewKeyPress,CHAR=str1,RESULT=N9
	activate ListViews(1)
	ListViews(1).DeleteAllContents
	ListViews(1).InsertColumn using	"Rank",60,1
	ListViews(1).InsertColumn using	"LR",50,2
	ListViews(1).InsertColumn using	"Gross",70,3
	ListViews(1).InsertColumn using	"Net",70,4
	ListViews(1).InsertColumn using	"%",50,5
	ListViews(1).InsertColumn using	"Date",70,6
	ListViews(1).SetColumnFormat using 2,1
	ListViews(1).SetColumnFormat using 3,1
	ListViews(1).SetColumnFormat using 4,1
	activate Buttons(1),OrderNetOK,result
	listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3):
		StatTextBoxes(4),StatTextBoxes(5),StatTextBoxes(6),StatTextBoxes(7):
		EditTextBoxes(1),EditTextBoxes(2),EditTextBoxes(3),EditTextBoxes(4):
		ListViews(1),Buttons(1)
	setfocus EditTextBoxes(1)
	return
OrderNetOK
	setprop	Buttons(1),enabled=0
	setitem	StatTextBoxes(7),0,""
.
	getitem	EditTextBoxes(1),0,str4
	call	Trim using str4
	if (str4 <> "")
		call	ZFILLIT	using str4,C0
		setitem	EditTextBoxes(1),0,str4
	else
		setitem	StatTextBoxes(2),0,""
		setitem	StatTextBoxes(7),0,"Not	a valid	Mailer Number!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(1)
		return
	endif
	pack	MKEY,str4,"000"
	rep	zfill,MKEY
	move	"OrderNetOK-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	call	NMLRKEY
	if over
		setitem	StatTextBoxes(2),0,""
		setitem	StatTextBoxes(7),0,"Not	a valid	Mailer Number!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(1)
		return
	else
		setitem	StatTextBoxes(2),0,MCOMP
	endif
.
	getitem	EditTextBoxes(2),0,str6
	call	Trim using str6
	if (str6 <> "")
		call	ZFILLIT	using str6,C0
		setitem	EditTextBoxes(2),0,str6
	else
		setitem	StatTextBoxes(4),0,""
		setitem	StatTextBoxes(7),0,"Not	a valid	List Number!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(2)
		return
	endif
	pack	NDATFLD,str6
	rep	zfill,NDATFLD
	move	C1,NDATPATH
	move	"OrderNetOK-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATPATH
	call	NDATKEY
	if over
		setitem	StatTextBoxes(4),0,""
		setitem	StatTextBoxes(7),0,"Not	a valid	List Number!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(2)
		return
	else
		setitem	StatTextBoxes(4),0,OLSTNAME
	endif
.
	getitem	EditTextBoxes(3),0,str10
	call	Trim using str10
	count	N9,str10
	if (N9 = C10)
		unpack	str10,MM,str1,DD,str1,str2,YY
		pack	str10,str2,YY,MM,DD
	elseif (N9 = C8)
		unpack	str10,MM,DD,str2,YY
		pack	str10,str2,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,str2,YY
		setitem	EditTextBoxes(3),0,newdate1
	elseif (N9 <> C0)
		setitem	StatTextBoxes(7),0,"From Date must be in MM/DD/YYYY format!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(3)
		return
	endif
	if (str10 <> "")
		type	str10
		if not equal
			setitem	StatTextBoxes(7),0,"From Date must be in MM/DD/YYYY format!!"
			setprop	Buttons(1),enabled=1
			setfocus EditTextBoxes(3)
			return
		endif
	else
		move	C0,str10	.Set Up	Default
	endif
.
	getitem	EditTextBoxes(4),0,str11
	call	Trim using str11
	count	N9,str11
	if (N9 = C10)
		unpack	str11,MM,str1,DD,str1,str2,YY
		pack	str11,str2,YY,MM,DD
	elseif (N9 = C8)
		unpack	str11,MM,DD,str2,YY
		pack	str11,str2,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,str2,YY
		setitem	EditTextBoxes(4),0,newdate1
	elseif (N9 <> C0)
		setitem	StatTextBoxes(7),0,"To Date must be in MM/DD/YYYY format!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(4)
		return
	endif
	if (str11 <> "")
		type	str11
		if not equal
			setitem	StatTextBoxes(7),0,"To Date must be in MM/DD/YYYY format!!"
			setprop	Buttons(1),enabled=1
			setfocus EditTextBoxes(4)
			return
		endif
	else
		move	"99999999999",str11	.Set Up	Default
	endif
	call	OrderSetMouseBusy
.START PATCH 3.62 REPLACED LOGIC
.	move	C5,str1
	clear	str1
	move	C5,str2
.END PATCH 3.62 REPLACED LOGIC
	move	str10,N10
	move	str11,howmany
.START PATCH 3.62 REPLACED LOGIC
.	call	OrderGetNetInfo	Using str4,str6,str1,N10,howmany,str1,str1
	call	OrderGetNetInfo	Using str4,str6,str1,N10,howmany,str2,str1
.END PATCH 3.62 REPLACED LOGIC
	call	OrderSetMouseFree
	setprop	Buttons(1),enabled=1
	setfocus ListViews(1)
	ListViews(1).SetItemState giving N9 using 0,2,2
	return

OrderNetMlrEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(1),0,str4
		call	ZFILLIT	using str4,C0
		setitem	EditTextBoxes(1),0,str4
		setitem	StatTextBoxes(2),0,""
	endif
	return
OrderNetListEditChange
	if (result = C2)	.Lost Focus + Change
		getitem	EditTextBoxes(2),0,str6
		call	ZFILLIT	using str6,C0
		setitem	EditTextBoxes(2),0,str6
		setitem	StatTextBoxes(4),0,""
	endif
	return
OrderNetMailerKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo3
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return

OrderNetListKeyPress
	if (N9 = 113) .F2 Key calls Search Function
.Virtual Key Value
		goto SearchGo2
	elseif (N9 = 120)     .F9 Key closes Search Function
		setprop	Search,visible=0
	endif
	return

OrderNetListViewDouble
	getitem	nord0001TabControlTop,0,howmany
	if (howmany = C8)
		getprop	Nord01ECEditNetPer,enabled=howmany
		if (howmany = C1)
			ListViews(1).GetNextItem giving	N9 using C2
			ListViews(1).GetItemText giving	str6 using N9,4
			setitem	Nord01ECEditNetPer,0,str6
			call	Order8CalculateNet
.			 call	 NetPerChange
		endif
	elseif (howmany	= C2)
		getprop	Nord001bEditNetPer,enabled=howmany
		if (howmany = C1)
			ListViews(1).GetNextItem giving	N9 using C2
			ListViews(1).GetItemText giving	str6 using N9,4
			setitem	Nord001bEditNetPer,0,str6
			call	Order2CalculateNet using C1
		endif
	endif
	return

OrderNetListViewKeyPress
	if (N9 = 120)	  .F9 Key closes Net Screen
		setprop	Report2,visible=0
	elseif (N9 = 0 AND str1	= B1)
		goto OrderNetListViewDouble
	endif
	return

OrderGetNetInfo	LRoutine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1,DimPtr3,DimPtr4
.This Routine is called	for two	different scenarios:
.1) DimPtr3 = "5"  - Will populate the Net Screen with ALL calculations
.2) DimPtr3 <> "5" - Will calculate ONE	Net, based on value of DimPtr3
.The branches prevent code maintenance for two separate	Routines with almost identical code.
.DimPtr	= Mailer, DimPtr1 = List, DimPtr2 = LR,	DimPtr4	= Return value (Net Percent)
.FrmPtr	= From Date, FrmPtr1 = To Date
.
.START PATCH 3.42 ADDED	LOGIC
	clear	DimPtr4		.Initialize Return Value
.END PATCH 3.42	ADDED LOGIC
	call	Trim using DimPtr3
	if (DimPtr3 = "5")
		packkey	hold,ORDVARS
		ListViews(1).DeleteAllItems giving N9
		clear	str40
		clear	str45
		clear	str50
	endif
	move	C0,N9			.Order Date
	move	C0,N8			.Last Merge Date
	move	C0,CampTotal		.Average Gross Total - Var stolen from Campaign	logic
	move	C0,CampNetTotal		.Average Net Total - Var stolen	from Campaign logic
	move	C0,N32
	move	"9999999.9999",WORSTPER	.Worst Net Percentage
	move	C0,BESTPER		.Best Net Percentage
	pack	NORDFLD1,AKey1,DimPtr
	pack	NORDFLD2,"02X",DimPtr1
	clear	NORDFLD3
	clear	NORDFLD4
	move	"GetNetInfo-NORDAIM",Location
	pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
	call	NORDAIM
	if over
		if (DimPtr3 <> "5")
			clear	taskname
			append	"There are no Order records using",taskname
			append	carr,taskname
			append	"Mailer: ",taskname
			append	DimPtr,taskname
			append	carr,taskname
			append	"List: ",taskname
			append	DimPtr1,taskname
			append	carr,taskname
			append	"Record: ",taskname
			append	DimPtr2,taskname
			append	" will not be updated!",taskname
			reset	taskname
			alert	caution,taskname,N7
		else
			setitem	StatTextBoxes(7),0,"No Order Records Found!!"
		endif
	else
		loop
			pack	newdate,OODTEC,OODTEY,OODTEM,OODTED
			move	newdate,N9
			if (N9 >= FrmPtr AND N9	<= FrmPtr1)
				move	OLRN,NMRGFLD
				move	"GetNetInfo-NMRGKEY",Location
				pack	KeyLocation,"Key: ",NMRGFLD
				call	NMRGKEY
				if not over
					if (DimPtr3 = "1" | DimPtr3 = "5")		.Average
						add	NMRGNET,CampNetTotal
						add	NMRGIQTY,CampTotal
					endif
					if (DimPtr3 = "2")  .Last
						if (N9 > N8)
							move	N9,N8
							move	C0,CampNetTotal
							move	C0,CampTotal
							move	NMRGNET,CampNetTotal
							move	NMRGIQTY,CampTotal
						endif
					else
						compare	C0,NMRGNET
						if not equal
							move	NMRGIQTY,str9
							move	NMRGNET,str8
							call	OrderCalcNetPer	using str9,str8,str7
							if (DimPtr3 = "5")	 .Last
								if (N9 > N8)
									move	N9,N8
									pack	str16,NMRGIQTY,NMRGNET
									pack	str40,newdate,OLRN,str16,str7
								endif
							endif
							if (N32	> BESTPER)
								move	N32,BESTPER
								if (DimPtr3 = "5")
									pack	str16,NMRGIQTY,NMRGNET
									pack	str45,newdate,OLRN,str16,str7
								endif
							endif
							if (N32	< WORSTPER)
								move	N32,WORSTPER
								if (DimPtr3 = "5")
									pack	str16,NMRGIQTY,NMRGNET
									pack	str50,newdate,OLRN,str16,str7
								endif
							endif
						endif
					endif
				endif
			endif
			move	"GetNetInfo-NORDKG",Location
			pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
			call	NORDKG
			until over
		repeat
		if (DimPtr3 = "5")
.Load ListView Object
			if (CampNetTotal > 0)
				move	CampTotal,str15
				move	CampNetTotal,str13
				call	OrderCalcNetPer	using str15,str13,str7
				ListViews(1).InsertItem	giving N9 using	"Average"
				ListViews(1).SetItemText using N9,str7,4
			else
				setitem	StatTextBoxes(7),0,"No Records Found which match your criteria!!"
			endif
.Worst Merge Percentage
			if (str50 <> "")
				unpack	str50,str4,MM,DD,OLRN,str16,str7
				pack	newdate1,MM,SLASH,DD,SLASH,str4
				unpack	str16,str8,str9
				ListViews(1).InsertItem	giving N9 using	"Worst"
				ListViews(1).SetItemText using N9,OLRN,1
				call	FormatNumeric using str8,str12
				ListViews(1).SetItemText using N9,str12,2
				call	FormatNumeric using str9,str12
				ListViews(1).SetItemText using N9,str12,3
				ListViews(1).SetItemText using N9,str7,4
				ListViews(1).SetItemText using N9,newdate1,5
			endif
.Best Merge Percentage
			if (str45 <> "")
				unpack	str45,str4,MM,DD,OLRN,str16,str7
				pack	newdate1,MM,SLASH,DD,SLASH,str4
				unpack	str16,str8,str9
				ListViews(1).InsertItem	giving N9 using	"Best"
				ListViews(1).SetItemText using N9,OLRN,1
				call	FormatNumeric using str8,str12
				ListViews(1).SetItemText using N9,str12,2
				call	FormatNumeric using str9,str12
				ListViews(1).SetItemText using N9,str12,3
				ListViews(1).SetItemText using N9,str7,4
				ListViews(1).SetItemText using N9,newdate1,5
			endif
.Last Record
			if (str40 <> "")
				unpack	str40,str4,MM,DD,OLRN,str16,str7
				pack	newdate1,MM,SLASH,DD,SLASH,str4
				unpack	str16,str8,str9
				ListViews(1).InsertItem	giving N9 using	"Last"
				ListViews(1).SetItemText using N9,OLRN,1
				call	FormatNumeric using str8,str12
				ListViews(1).SetItemText using N9,str12,2
				call	FormatNumeric using str9,str12
				ListViews(1).SetItemText using N9,str12,3
				ListViews(1).SetItemText using N9,str7,4
				ListViews(1).SetItemText using N9,newdate1,5
			endif
			unpack	hold,ORDVARS
		else
.Average/Last Merge Percentage
			if (DimPtr3 = "1" | DimPtr3 = "2")
				if (CampNetTotal > 0)
					move	CampTotal,str13
					move	CampNetTotal,str15
					clear	str7
					call	OrderCalcNetPer	using str13,str15,str7
					move	str7,N32
				endif
			elseif (DimPtr3	= "3")
.Best Merge Percentage
				move	BESTPER,N32
			elseif (DimPtr3	= "4")
.Worst Merge Percentage
				move	WORSTPER,N32
			endif
			if (N32	> C0)
				move	N32,DimPtr4
			endif
		endif
	endif
	return

OrderGetNetLast	Routine DimPtr,DimPtr1,DimPtr3
.Currently Not Used - Used by INTEGRAL.PLS as an External Routine as of August 20, 2003 (ASH)
.A variation on	OrderGetNetInfo	which only finds Last Merge record, and	without	the object references.
.First,	retain values of original ORDVARS - This must happen!!
	packkey	hold,ORDVARS
.
	move	C0,N9			.Last Merge Date
	pack	NORDFLD1,AKey1,DimPtr
	pack	NORDFLD2,"02X",DimPtr1
	clear	NORDFLD3
	clear	NORDFLD4
	move	"GetNetLast-NORDAIM",Location
	pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
	call	NORDAIM
	if not over
		loop
			pack	newdate,OODTEC,OODTEY,OODTEM,OODTED
			move	newdate,N10
			if (N10	> N9)
				move	OLRN,NMRGFLD
				move	"GetNetLast-NMRGKEY",Location
				pack	KeyLocation,"Key: ",NMRGFLD
				call	NMRGKEY
				if not over
					compare	C0,NMRGNET
					if not equal
						move	NMRGIQTY,str9
						move	NMRGNET,str8
						call	OrderCalcNetPer	using str9,str8,str10
						if (N10	> N9)	 .Last Record
							move	N9,N8
							move	N32,DimPtr3
						endif
					endif
				endif
			endif
			move	"GetNetLast-NORDKG",Location
			pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
			call	NORDKG
			until over
		repeat
	endif
.Refresh original ORDVARS - This must happen!!
	unpack	hold,ORDVARS
	return

OrderGetNetAverage LRoutine DimPtr,DimPtr1,DimPtr2
.A variation on	OrderGetNetInfo	which only finds Average Net based on todays date to
.todays	date minus 1 year, and without the object references.
.First,	retain values of original ORDVARS - This must happen!!
	packkey	hold,ORDVARS
.
	move	C0,N9			.Todays	Date
	move	C0,N10			.One year ago today
	move	C0,CampNetTotal		.Average Net Total - Var stolen	from Campaign logic
	move	C0,CampTotal		.Average Gross Total - Var stolen from Campaign	logic
	unpack	timestamp,str8
	move	str8,N9
	sub	"10000",N9,N10
	pack	NORDFLD1,AKey1,DimPtr
	pack	NORDFLD2,"02X",DimPtr1
	clear	NORDFLD3
	clear	NORDFLD4
	move	"GetNetLast-NORDAIM",Location
	pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
	call	NORDAIM
	if not over
		loop
			pack	newdate,OODTEC,OODTEY,OODTEM,OODTED
			move	newdate,N8
			if (N8 <= N9 AND N8 >= N10)
				move	OLRN,NMRGFLD
				move	"GetNetLast-NMRGKEY",Location
				pack	KeyLocation,"Key: ",NMRGFLD
				call	NMRGKEY
				if not over
					add	NMRGNET,CampNetTotal
					add	NMRGIQTY,CampTotal
				endif
			endif
			move	"GetNetLast-NORDKG",Location
			pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
			call	NORDKG
			until over
		repeat
		if (CampNetTotal > 0)
			move	CampTotal,str15
			move	CampNetTotal,str13
			call	OrderCalcNetPer	using str15,str13,DimPtr2
		endif
	endif
.Refresh original ORDVARS - This must happen!!
	unpack	hold,ORDVARS
	return

OrderGetNetOptions Routine FrmPtr,FrmPtr1,DimPtr
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Net Calculation Options"
	move	NO,RptCan
	create	Report2;StatTextBoxes(1)=50:70:10:70,"From Date",""
	create	Report2;StatTextBoxes(2)=70:90:10:70,"To Date",""
	create	Report2;StatTextBoxes(3)=90:110:10:70,"Net Select",""
	create	Report2;StatTextBoxes(4)=210:230:10:280,"","",fgcolor=red
	create	Report2;EditTextBoxes(1)=50:70:70:160,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;EditTextBoxes(2)=70:90:70:160,MaxChars=10,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;ComboBoxes(1)=90:110:70:160,"",";A)verage Net;)Last Net;)Best Net;)Worst Net"
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
	activate StatTextBoxes(4)
	activate EditTextBoxes(1)
	activate EditTextBoxes(2)
	activate ComboBoxes(1)
	activate Buttons(1),OrderGetNetOK,result
	listins	ObjectColl,mRSearch,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3):
		StatTextBoxes(4),EditTextBoxes(1),EditTextBoxes(2),ComboBoxes(1),Buttons(1)
	setfocus EditTextBoxes(1)
	setprop	Report2,visible=1
	return
OrderGetNetOK Routine FrmPtr,FrmPtr1,DimPtr
	setitem	StatTextBoxes(4),0,""
	getitem	EditTextBoxes(1),0,str10
	call	Trim using str10
	count	N9,str10
	if (N9 = C10)
		unpack	str10,MM,str1,DD,str1,str2,YY
		pack	str10,str2,YY,MM,DD
	elseif (N9 = C8)
		unpack	str10,MM,DD,str2,YY
		pack	str10,str2,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,str2,YY
		setitem	EditTextBoxes(1),0,newdate1
	elseif (N9 <> C0)
		setitem	StatTextBoxes(4),0,"From Date must be in MM/DD/YYYY format!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(1)
		return
	endif
	if (str10 <> "")
		type	str10
		if not equal
			setitem	StatTextBoxes(4),0,"From Date must be in MM/DD/YYYY format!!"
			setprop	Buttons(1),enabled=1
			setfocus EditTextBoxes(1)
			return
		endif
	endif
	move	str10,N10
.
	getitem	EditTextBoxes(2),0,str10
	call	Trim using str10
	count	N9,str10
	if (N9 = C10)
		unpack	str10,MM,str1,DD,str1,str2,YY
		pack	str10,str2,YY,MM,DD
	elseif (N9 = C8)
		unpack	str10,MM,DD,str2,YY
		pack	str10,str2,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,str2,YY
		setitem	EditTextBoxes(2),0,newdate1
	elseif (N9 <> C0)
		setitem	StatTextBoxes(4),0,"To Date must be in MM/DD/YYYY format!!"
		setprop	Buttons(1),enabled=1
		setfocus EditTextBoxes(2)
		return
	endif
	if (str10 <> "")
		type	str10
		if not equal
			setitem	StatTextBoxes(4),0,"To Date must be in MM/DD/YYYY format!!"
			setprop	Buttons(1),enabled=1
			setfocus EditTextBoxes(2)
			return
		endif
	endif
	move	str10,FrmPtr1
.
	getitem	ComboBoxes(1),0,N3
	move	N3,DimPtr
	setprop	Report2,visible=0
	call	OrderSetMouseBusy
	return

OrderTestForNet
	getitem	Nord001AEditMlr,0,str4
	call	Trim using str4
	if (str4 <> "")
		call	ZFILLIT	using str4,C0
		getitem	Nord001AEditList,0,str6
		call	Trim using str6
		if (str6 <> "")
			call	ZFILLIT	using str6,C0
			clear	str7
			call	OrderGetNetAverage using str4,str6,str7
.			 call	 OrderGetNetLast using str4,str6,str7
			if (str7 <> "")
				setitem	Nord001bEditNetPer,0,str7
				call	OrderNetPerChange
			endif
		endif
	endif
	return

Order2CalculateNet LRoutine FrmPtr
.Used to calculate Net Percents	and Net	Quantities
	call	OrderCalculateNet using	Nord001AEditOrderQty,Nord001bEditNetPer,Nord001bEditNetQty,FrmPtr
	return

OrderCalculateNet LRoutine EditPtr,EditPtr1,EditPtr2,FrmPtr
.Gross Qty = EditPtr, Net Per =	EditPtr1, Net Qty = EditPtr2
.FrmPtr	= C1, Calculates NetQty	using Gross and	NetPer
.FrmPtr	<> C1, Calculates NetPer using Gross and NetQty
	getitem	EditPtr,0,str17
	call	RemoveChar using str17,COMMA
	call	Trim using str17
	if (FrmPtr = C1)
		clear	str8
		getitem	EditPtr1,0,str8
		call	Trim using str8
		if (str8 <> "")
			call	OrderCalcNet using str17,str8,str10
			call	FormatNumeric using str10,str13
			setitem	EditPtr2,0,str13
		endif
	else
		getitem	EditPtr2,0,str12
		call	RemoveChar using str12,COMMA
		call	Trim using str12
		if (str17 <> ""	AND str12 <> "")
			call	OrderCalcNetPer	using str17,str12,str10
			setitem	EditPtr1,0,str10
		endif
	return
	endif
	return

OrderCalcNet LRoutine DimPtr,DimPtr1,DimPtr2
.DimPtr	= Qty
.DimPtr1 = Net Per
.DimPtr2 = Net	Qty
	move	C0,CALCPER
	move	C0,result
	move	C0,DimPtr2
	move	DimPtr,result
	move	DimPtr1,CALCPER
	div	"100",CALCPER
	mult	CALCPER,result
	move	result,DimPtr2
	return

OrderCalcNetPer	LRoutine DimPtr,DimPtr1,DimPtr2
.DimPtr	= Qty
.DimPtr1 = Net Qty
.DimPtr2 = Net	Per
	move	C0,CALCPER
	move	C0,CALCQTY
	move	C0,CALCNET
	move	C0,N32
	move	C0,DimPtr2
	move	DimPtr,CALCQTY
	move	DimPtr1,CALCNET
	div	CALCQTY,CALCNET,CALCPER
	mult	"100",CALCPER
	move	CALCPER,N32
	move	N32,DimPtr2
	return

OrderGetNetValues1
	getitem	Nord001AEditMlr,0,str4
	getitem	Nord001AEditList,0,str6
	call	OrderGetNetValues using	str4,str6
	return
......................................................
OrderGetPackage	Routine	DimPtr3,DimPtr4
.DimPtr3 = Mailer Number
.DimPtr4 = Mailer Name
	call	Report2DestroyObjects
	setprop	Report2,title="NIN Projection Package Select"
	move	NO,RptCan
	pack	taskname,"Mailer:  ",DimPtr4
	create	Report2;StatTextBoxes(1)=50:70:10:310,taskname,""
	create	Report2;StatTextBoxes(2)=70:90:10:110,"Package",""
	create	Report2;StatTextBoxes(3)=90:110:80:310,"","",fgcolor=blue
	create	Report2;EditTextBoxes(1)=70:90:80:130,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
	create	Report2;Buttons(1)=180:205:50:100,"O&K",zorder=500,default=1
	create	Report2;Buttons(2)=180:205:140:190,"&Finish",enabled=0
	activate StatTextBoxes(1)
	activate StatTextBoxes(2)
	activate StatTextBoxes(3)
.When dynamically creating an EditTextBox, you are only	given three default events: GotFocus,LostFocus,LostFocus+Change.
.Any other events must be registered manually.
	activate EditTextBoxes(1),OrderGetPackageEditChange,result
	activate Buttons(1),OrderGetPackageOK,result
	activate Buttons(2),OrderGetPackageFinish,result
	listins	ObjectColl,StatTextBoxes(1),StatTextBoxes(2),StatTextBoxes(3):
		EditTextBoxes(1),Buttons(1),Buttons(2)
	setfocus EditTextBoxes(1)
	setprop	Report2,visible=1
	return
OrderGetPackageEditChange
	if (result = C2)	.Lost Focus + Change
		setprop	Buttons(2),enabled=0
	endif
	return
OrderGetPackageOK
	getitem	EditTextBoxes(1),0,str6
	call	Trim using str6
	if (str6 = "")
		setitem	StatTextBoxes(3),0,"Not	a valid	Package	Number!!"
		setprop	Buttons(2),enabled=0
		setfocus EditTextBoxes(1)
	else
		call	ZFILLIT	using str6,C0
		setitem	EditTextBoxes(1),0,str6
.START PATCH 3.75.7 REPLACED LOGIC
..START PATCH 3.75.4 REPLACED LOGIC
..		pack	NPKGFLD,DimPtr3,str6
.		move	"Nord01EBPKG-COMPKEY3",Location
.		pack	COMPFLD3,DimPtr3
.		pack	KeyLocation,"Key: ",COMPFLD3
.		call	COMPKEY3
.		if over
.			clear	COMPNUM
.		endif
.		pack	NPKGFLD,COMPNUM,str6
..END PATCH 3.75.4 REPLACED LOGIC
		pack	NPKGFLD,DimPtr3,str6
.END PATCH 3.75.7 REPLACED LOGIC
		call	ZFILLIT	using NPKGFLD,C0
		move	C1,NPKGPATH
		move	"Nord01EBPKG-NPKGKEY",Location
		pack	KeyLocation,"Key: ",NPKGFLD
		call	NPKGKEY
		if over
			setitem	StatTextBoxes(3),0,"Not	a valid	Package	Number!!"
			setprop	Buttons(2),enabled=0
			setfocus EditTextBoxes(1)
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.		elseif (NPKGMaster = "1")
.			 setitem StatTextBoxes(3),0,"Cannot select a Master Package!!"
.			 setprop Buttons(2),enabled=0
.			 setfocus EditTextBoxes(1)
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
		else
			setitem	StatTextBoxes(3),0,NPKGPNAME
			setprop	Buttons(2),enabled=1
			setfocus Buttons(2)
		endif
	endif
	return
OrderGetPackageFinish
	setprop	Report2,visible=0
	return
......................................................
FileGo
.Flag set to "N" if in Modify or New mode
	branch result to FileGo1,FileGo2,FileGo2
FileGo1
	if (PrtFlag = Yes)
		return
	else
		return
	endif
FileGo2
	if (ExitFlag = "Y" & ExitFlag2 = "Y" & ExitFlag3 = "Y" & ExitFlag4 = "Y" & ExitFlag5 = "Y")
		winshow
.		 EventSend	*Client, 2
FileGo3
		call	OrderCalcCleanUpExcel
		stop
	endif
	return
EditGo
HelpGo
	branch	result to HelpGo1,HelpGo1,HelpGo2
HelpGo1
	clear	taskname
.START PATCH 3.61 REPLACED LOGIC
.	Path	Exist,"c:\windows"
.	if over
.		append	"!c:\winnt\system32\cmd.exe /c ",taskname
.	else
.		append	"!c:\command.com /c c:\windows\winhlp32.exe ",taskname
.	endif
.
	Path	Exist,"c:\windows"
	if over			.nt/2000
		append	"!c:\winnt\system32\cmd.exe /c ",taskname
	elseif (osflag = c6)	.XP
		append	"!c:\windows\system32\cmd.exe /c ",taskname
	else			.95/98
		append	"!c:\command.com /c c:\windows\winhlp32.exe ",taskname
	endif
.END PATCH 3.61 REPLACED LOGIC
	append	"\\nts0\c\library\develop\LCRHelp.hlp",taskname
	reset	taskname
	execute	taskname
	return
HelpGo2
	setprop	AboutMssg,visible=1
	return

SearchGo
.START PATCH 3.66 REPLACED LOGIC
.	branch	result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5
	branch	result to SearchGo1,SearchGo2,SearchGo3,SearchGo4,SearchGo5,SearchGo6
.END PATCH 3.66 REPLACED LOGIC
SearchGo1
.BROKER
	move	C1,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return
SearchGo2
.LIST
	move	C2,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return
SearchGo3 Routine
.MAILER
.Needs to be a Routine as NPKG0001 will call it!
	move	C3,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return
SearchGo4
.SHIP-TO
	move	C4,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return
SearchGo5
.CAMPAIGN
	move	C5,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return

.START PATCH 3.66 ADDED LOGIC
SearchGo6
.OWNER
	move	C6,SrchFlag
	call	SearchSetTitle
	call	SearchSetVisible
	return
.END PATCH 3.66 ADDED LOGIC

SearchLoad
.Called	by SearchDataList_DoubleClick
.Only load if in modify	mode
	getprop	Nord001AEditMlr,enabled=howmany
	getprop	Nord01eaEditMlr,enabled=result
.START PATCH 3.66 REPLACED LOGIC
.	branch SrchFlag	to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5
	branch SrchFlag	to SearchLoad1,SearchLoad2,SearchLoad3,SearchLoad4,SearchLoad5,SearchLoad6
.END PATCH 3.66 REPLACED LOGIC
SearchLoad1
.BROKER
	if (TabNum = C6)
		if (result = C1)	.Screen	6
.START PATCH 3.75.7 REPLACED LOGIC
.			unpack	Srchstr,str4,str1,str3,str1,str45,str55
.			setitem	Nord01eaEditBrk,0,str4
.			setitem	Nord01eaStatBrkComp,0,str45
.			setfocus Nord01eaEditBrk
.			setitem	Nord01eaEditBrkContact,0,str3
.			move	str55,str45		.This is done so that BRCREDIT does not	appear!
.			setitem	Nord01eaStatBrkCntName,0,str45
..START PATCH 3.71.9 ADDED LOGIC
..START PATCH 3.74 REPLACED LOGIC
..			unpack	str55,str35,str10,str1,COMPNUM,CNCTID
..			call	Trim using COMPNUM
..			if (COMPNUM <> "")
..				call	Trim using CNCTID
..				if (CNCTID <> "")
..					pack	str10,COMPNUM,SLASH,CNCTID
..				else
..					pack	str10,COMPNUM
..				endif
..			else
..				clear	str10
..			endif
..			BRKNUM,SLASH,BRKCNT,B1,BRCOMP,BRCNTCT,BRCREDIT,COMPNUM,SLASH,CNCTID
.			unpack	str55,str35,str10,str1,str10
..END PATCH 3.74 REPLACED LOGIC
.			setitem	Nord01eaStatBrkNew,0,str10
..END PATCH 3.71.9 ADDED LOGIC
.............................................................
			unpack	Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6,str1,str3
			setitem	Nord01eaEditBrk,0,str6
			setitem	Nord01eaStatBrkComp,0,str45
			setfocus Nord01eaEditBrk
			setitem	Nord01eaEditBrkContact,0,str3
			pack	str45,str35,str10
			setitem	Nord01eaStatBrkCntName,0,str45
.END PATCH 3.75.7 REPLACED LOGIC
		endif
	elseif (TabNum = C8)
		getprop	Nord01ECEditBrk,enabled=result
		if (result = C1)
			unpack	Srchstr,str4,str1,str3,str1,str45,str55
			setitem	Nord01ECEditBrk,0,str4
			setitem	Nord01ECStatBrkComp,0,str45
			setfocus Nord01ECEditBrk
			setitem	Nord01ECEditBrkContact,0,str3
			move	str55,str45		.This is done so that BRCREDIT does not	appear!
			setitem	Nord01ECStatBrkCntName,0,str45
.START PATCH 3.71.9 ADDED LOGIC
.START PATCH 3.74 REMOVED LOGIC
.			unpack	str55,str35,str10,str1,COMPNUM,CNCTID
.			call	Trim using COMPNUM
.			if (COMPNUM <> "")
.				call	Trim using CNCTID
.				if (CNCTID <> "")
.					pack	str10,COMPNUM,SLASH,CNCTID
.				else
.					pack	str10,COMPNUM
.				endif
.			else
.				clear	str10
.			endif
			unpack	str55,str35,str10,str1,str10
.END PATCH 3.74 REMOVED LOGIC
			setitem	Nord01ECStatBrkNew,0,str10
.END PATCH 3.71.9 ADDED LOGIC
		endif
	elseif (TabNum = C1 & howmany =	C1)
		unpack	Srchstr,str4,str1,str3,str1,str45,str55
		setitem	Nord001AEditBrk,0,str4
		setitem	Nord001AStatBrkComp,0,str45
.START PATCH 3.71.9 ADDED LOGIC
.START PATCH 3.74 REMOVED LOGIC
.		unpack	str55,str35,str10,str1,COMPNUM,CNCTID
.		call	Trim using COMPNUM
.		if (COMPNUM <> "")
.			call	Trim using CNCTID
.			if (CNCTID <> "")
.				pack	str10,COMPNUM,SLASH,CNCTID
.			else
.				pack	str10,COMPNUM
.			endif
.		else
.			clear	str10
.		endif
		unpack	str55,str35,str10,str1,str10
.END PATCH 3.74 REMOVED LOGIC
		setitem	Nord001AStatBrkNew,0,str10
.END PATCH 3.71.9 ADDED LOGIC
		if (NewFlag <> "S")	.NOT Search mode
			setitem	Nord001AEditBrkContact,0,str3
			move	str55,str45		.This is done so that BRCREDIT does not	appear!
			setitem	Nord001AStatBrkCnt,0,str45
			type	BRSALES
			if equal
				rep	zfill,BRSALES
				unpack	BRSALES,OSALES10,OSALES
			endif
.START PATCH 3.72.6 REPLACED LOGIC
.			call	OrderLoadSalesperson using C1
			getitem	Nord001AEditMlr,0,str4
			pack	MKEY,str4,"000"
			move	"O.MailerSalesContactRead-NMLRKEY",Location
			pack	KeyLocation,"Key: ",MKEY
			call	NMLRKEY
			type	MSLSPER
			if Equal
				if (MSLSPER <> "00")
					unpack	MSLSPER,OSALES10,OSALES
				endif
			endif
			call	OrderLoadSalesperson using C1
.END PATCH 3.72.6 REPLACED LOGIC
		endif
		setfocus Nord001AEditBrk
	endif
	return
SearchLoad2
.LIST
	getprop	Report2,visible=N6
	if (N6 = C1)
		unpack	Srchstr,str6
		setitem	EditTextBoxes(2),0,str6
		return
	endif
	if (TabNum = 5)
.Only Load if OK button	is activated
		getprop	Nord001DButtonOk,enabled=Howmany
		if (Howmany = C1)
			unpack	Srchstr,str6
			setitem	Nord001DEditListSearch,0,str6
			setfocus Nord001DEditListSearch
		endif
	elseif (TabNum = C8)
		getprop	Nord01ECEditList,enabled=result
		if (result = C1)
			unpack	Srchstr,str6,str1,str35
			setitem	Nord01ECEditList,0,str6
			setitem	Nord01ECStatListName,0,str35
			setfocus Nord01ECEditList
		endif
	elseif (TabNum = C1 & Howmany =	C1)
		unpack	Srchstr,str6,str1,str35
		setitem	Nord001AEditList,0,str6
		setitem	Nord001AStatListName,0,str35
		setfocus Nord001AEditList
		if (NewFlag <> "S")	.NOT Search mode
.The file must be reread using current value in	order to set default NET info.
.If not	reread than all	fields will be nulled from loading of Search Data List.
.(SearchDataList uses over flag	to terminate population.  All fields nulled at
.that point.)
			pack	NDATFLD,str6
			move	C1,NDATPATH
			move	"SearchLoad2-NDATKEY",Location
			pack	KeyLocation,"Key: ",NDATFLD
			call	NDATKEY		.assumption made that there will not be	an over
.START PATCH 3.6 REPLACED LOGIC
.			if (mod	<> C5 AND mod <> C6 AND	mod <> C3 AND NewFlag <> "S")	       .Not LCR,Pending,OrderSearch
			if (mod <> 8 AND mod <> C5 AND mod <> C6 AND mod <> C3 AND NewFlag <> "S")	       .Not LCR,Pending,OrderSearch
.END PATCH 3.6 REPLACED LOGIC
				call	OrderVerifyList
				call	OrderDisableNINGuar
			endif
			call	Order1EditList_Change
		endif
	endif
	return
SearchLoad3
.MAILER
	getprop	Report2,visible=N6
	if (N6 = C1)
		unpack	Srchstr,str4
		setitem	EditTextBoxes(1),0,str4
		return
	endif
	if (TabNum = 5)
		getprop	Nord001DButtonOk,enabled=Howmany
		if (Howmany = C1)
			unpack	Srchstr,str4
			setitem	Nord001DEditMlrSearch,0,str4
			setfocus Nord001DEditMlrSearch
		endif
	elseif (TabNum = C6)
		if (result = C1)
.START PATCH 3.74 REPLACED LOGIC
.			unpack	Srchstr,str4,str1,str3,str1,str45
.START PATCH 3.75.7 REPLACED LOGIC
.			unpack	Srchstr,str4,str1,str3,str1,str45,str55
..END PATCH 3.74 REPLACED LOGIC
.			setitem	Nord01eaEditMlr,0,str4
..................................................
			unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
			setitem	Nord01eaEditMlr,0,str6
.END PATCH 3.75.7 REPLACED LOGIC
			setitem	Nord01eaStatMlrComp,0,str45
			setfocus Nord01eaEditMlr
.START PATCH 3.74 ADDED LOGIC
.START PATCH 3.75.7 REMOVED LOGIC
.			unpack	str55,str45,str1,str10
.			setitem	Nord01eaStatMlrNew,0,str10
.END PATCH 3.75.7 REMOVED LOGIC
.END PATCH 3.74 ADDED LOGIC
			if (NewFlag2 <>	"S")	 .NOT Search mode
				clear	str7	.prep before loading offers
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.				if (str4 = NCMPMLR)
.					pack	str7,NCMPMLR,NCMPOFFER	.Reload	and highlight original offers
				pack	COMPFLD,NCMPMLR
				move	"SL3a-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPKEY
				if (str4 = COMPOLDMLR)
					pack	str7,COMPOLDMLR,NCMPOFFER	.Reload	and highlight original offers
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
				endif
				call	OrderLoadOffer using C6
				call	OrderLoadSamples using C6
.START PATCH 3.75.8 ADDED LOGIC
				call	 Trim using COMPBROKER
				if (COMPBROKER = "")
					setitem	Nord01eaEditBrk,0,COMPCONSULT
					setitem	Nord01eaEditBrkContact,0,COMPCONSULT1
				else
					setitem	Nord01eaEditBrk,0,COMPBROKER
					setitem	Nord01eaEditBrkContact,0,COMPBROKER1
				endif
				call	BrokerLostFocus	using Nord01eaEditBrk,Nord01eaEditBrkContact,C6
.END PATCH 3.75.8 ADDED LOGIC
			endif
		endif
	elseif (TabNum = C8)
		getprop	Nord01ECEditMlr,enabled=result
		if (result = C1)
.START PATCH 3.74 REPLACED LOGIC
.			unpack	Srchstr,str4,str1,str3,str1,str45
.START PATCH 3.75.7 REPLACED LOGIC - PREPPING FOR FUTURE CONVERSION OF LOL FILE
.			unpack	Srchstr,str4,str1,str3,str1,str45,str55
			unpack  Srchstr,str4,str1,str3,str1,str45,str35,str10,str1,str6
.END PATCH 3.75.7 REPLACED LOGIC
.			unpack	str55,str35,str10,str1,COMPNUM,CNCTID
.			call	Trim using COMPNUM
.			if (COMPNUM <> "")
.				call	Trim using CNCTID
.				if (CNCTID <> "")
.					pack	str10,COMPNUM,SLASH,CNCTID
.				else
.					pack	str10,COMPNUM
.				endif
.			else
.				clear	str10
.			endif
.			setitem	Nord01ECStatMlrNew,0,str10
.END PATCH 3.74 REPLACED LOGIC
			setitem	Nord01ECEditMlr,0,str4
			setitem	Nord01ECStatMlrComp,0,str45
			setfocus Nord01ECEditMlr
			if (NewFlag3 <>	"S")	 .NOT Search mode
				clear	str7	.prep before loading offers
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.				if (str4 = NCMPMLR)
.					pack	str7,NCMPMLR,NCMPOFFER	.Reload	and highlight original offers
				pack	COMPFLD,NCMPMLR
				move	"SL3b-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPKEY
				if (str4 = COMPOLDMLR)
					pack	str7,COMPOLDMLR,NCMPOFFER	.Reload	and highlight original offers
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
				endif
				call	OrderLoadOffer using C8
				call	OrderLoadSamples using C8
			endif
		endif
	elseif (TabNum = C1 and	howmany	= C1)
.START PATCH 3.74 REPLACED LOGIC
.		unpack	Srchstr,str4,str1,str3,str1,str45
		unpack	Srchstr,str4,str1,str3,str1,str45,str55
.END PATCH 3.74 REPLACED LOGIC
		setitem	Nord001AEditMlr,0,str4
		setitem	Nord001AStatMlrComp,0,str45
		setfocus Nord001AEditMlr
.START PATCH 3.74 ADDED LOGIC
		unpack	str55,str45,str1,str10
		setitem	Nord001AStatMlrNew,0,str10
.END PATCH 3.74 ADDED LOGIC
		if (NewFlag <> "S")	.NOT Search mode
.Added following 04/19/2001 to ensure Screens 2+ get loaded with correct Mailer	Name
;BEGIN PATCH 3.74.1 Code Added
			unpack	Srchstr,str4,str1,str3,str1,str45,str55
;END PATCH 3.74.1
			setitem	Nord001bStatMlrComp,0,str45
			setitem	nord001CStatMlrComp,0,str45
.
			setitem	Nord001AEditMlrContact,0,str3
			clear	str7	.prep before loading offers
			if (str4 = OMLRNUM)
				move	OODNUM,str7	.Reload	and highlight original offers
			endif
			call	OrderLoadOffer using C1
			call	OrderLoadSamples using C1
			unpack	MSLSPER,OSALES10,OSALES
			call	OrderLoadSalesperson using C1
.START PATCH 3.75.8 ADDED LOGIC
			unpack	MBRKNUM,str4,str3
			setitem	Nord001AEditBrk,0,str4
			setitem	Nord001AEditBrkContact,0,str3
			call	BrokerLostFocus	using Nord001AEditBrk,Nord001AEditBrkContact,C1
.END PATCH 3.75.8 ADDED LOGIC
		endif
	endif
	return
SearchLoad4
.SHIP-TO
	if (TabNum = C6)
		if (result = C1)
			if (NewFlag2 <>	"S")	 .NOT Search mode
				unpack	Srchstr,str4,str1,str45
				setitem	Nord01eaEditRtn,0,str4
				setitem	Nord01eaStatRtnComp,0,str45
			endif
			setfocus Nord01eaEditRtn
		endif
	elseif (TabNum = C8)
		getprop	Nord01ECEditMlr,enabled=result
		if (result = C1)
			if (NewFlag3 <>	"S")	 .NOT Search mode
				unpack	Srchstr,str4,str1,str45
				setitem	Nord01ECEditRtn,0,str4
				setitem	Nord01ECStatRtnComp,0,str45
			endif
			setfocus Nord01ECEditRtn
		endif
	elseif (TabNum = C1 & howmany =	C1)
		if (NewFlag <> "S")	.NOT Search mode
			unpack	Srchstr,str4,str1,str45,str25,str30
			setitem	Nord001AEditRtn,0,str4
			setitem	Nord001AStatRtnComp,0,str45
			setitem	Nord001AStatRtnComp2,0,""
			setitem	Nord001AStatRtnAdd1,0,str25
			unpack	str30,str15,str2,str10
			call	Trim using str15
			if (str15 <> "")
				pack	str45,str15,COMMA,B1,str2,B1,str10
			else
				clear	str45
			endif
			setitem	Nord001AStatRtnAdd2,0,str45
			setfocus Nord001AEditRtn
		endif
	endif
	return
SearchLoad5
.CAMPAIGN
	getprop	Report2,visible=result
	if (result = C1)
		unpack	Srchstr,str6
		setitem	EditTextBoxes(1),0,str6
		return
	endif
	if (TabNum = C8)
		getprop	Nord01ECEditCamp,enabled=result
		if (result = C1)
			unpack	Srchstr,str6,str1,str45
			setitem	Nord01ECEditCamp,0,str6
			setitem	Nord01ECStatCampName,0,str45
			setfocus Nord01ECEditCamp
			call	CampaignLostFocus1 using C8
		endif
	elseif (TabNum = C1)
		getprop	Nord001AEditCampaign,enabled=result
		if (result = C1)
			if (NewFlag = "S")     .Search mode
				unpack	Srchstr,str6,str1,str45
				setitem	Nord001AEditCampaign,0,str6
				setitem	Nord001AStatCamp1,0,str45
			endif
			setfocus Nord001AEditCampaign
		endif
	endif
	return
.START PATCH 3.66 ADDED LOGIC
SearchLoad6
.OWNER
	getprop	Report2,visible=result
	if (result = C1)
		unpack	Srchstr,str6
		setitem	EditTextBoxes(1),0,str6
		return
	endif
	if (TabNum = C8)
		getprop	Nord01ECEditOwner,enabled=result
		if (result = C1)
			unpack	Srchstr,str4,str1,str25
			setitem	Nord01ECEditOwner,0,str4
			setitem	Nord01ECStatOwnerComp,0,str25
			call	SetOwner3
			setfocus Nord01ECEditOwner
		endif
	elseif (TabNum = C1)
		getprop	Nord001AEditOwner,enabled=result
		if (result = C1)
			unpack	Srchstr,str4,str1,str25
			setitem	Nord001AEditOwner,0,str4
			setitem	Nord001AStatOwnerComp,0,str25
			call	SetOwner
			setfocus Nord001AEditOwner
		endif
	endif
	return
.END PATCH 3.66 ADDED LOGIC

Optionsgo
	if (result < 4)
		if (TabNum > C5)
			call	ViewGo using C6
		else
			call	ViewGo using C1
		endif
		return
	else
		branch	result to ViewGo,ViewGo,ViewGo,NetGo,NetGo,OptionsGo1
	endif
ViewGo LRoutine	FrmPtr
.	 setitem SamplesEditMailer,0,OMLRNUM
.	 setitem SamplesEditSample,0,OSAMCDE
	if (FrmPtr = C1)
.START PATCH 3.75.5 REPLACED LOGIC - TEMPORARY PATCH!
.		getitem	Nord001AEditMlr,0,str6
		getitem	Nord001AStatMlrNew,0,str6
.END PATCH 3.75.5 REPLACED LOGIC - TEMPORARY PATCH!
.START PATCH 3.72.2 REPLACED LOGIC
.		getitem	Nord001bComboSample,0,N9
.		getitem	Nord001bComboSample,N9,str35
		getitem	Nord001bEditSample,0,str3
.END PATCH 3.72.2 REPLACED LOGIC
	elseif (FrmPtr = C6)
.START PATCH 3.75.7 REPLACED LOGIC
..START PATCH 3.75.5 REPLACED LOGIC - TEMPORARY PATCH!
..		getitem	Nord01eaEditMlr,0,str6
.		getitem	Nord01eaStatMlrNew,0,str6
..END PATCH 3.75.5 REPLACED LOGIC - TEMPORARY PATCH!
		getitem	Nord01eaEditMlr,0,str6
.END PATCH 3.75.7 REPLACED LOGIC
		getitem	Nord01eaComboSample,0,N9
.START PATCH 3.72.2 REPLACED LOGIC
.		getitem	Nord01eaComboSample,N9,str35
		getitem	Nord01eaComboSample,N9,str55
.END PATCH 3.72.2 REPLACED LOGIC
	endif
.START PATCH 3.72.2 REPLACED LOGIC
.	unpack	str35,str30,str1,str3
	if (FrmPtr <> C1)
		unpack	str55,str30,str1,str10,str1,str3
	endif
.END PATCH 3.72.2 REPLACED LOGIC
	setitem	SamplesEditMailer,0,str6
	setitem	SamplesEditSample,0,str3
	setfocus SamplesEditMailer
	setprop	nord001f,visible=1
	return
NetGo
	clear	str4
	clear	str6
	call	OrderGetNetValues using	str4,str6
	return
OptionsGo1
	setprop	Options,visible=1
	return
OptionsWritePref
.Pull values from OptionsOrd.plf
.Screen	1
	getitem	OptionsScreenInit,0,N5
	move	N5,OptionsArray(1,1)
	getitem	OptionsFaxLONo,0,N5
	move	N5,OptionsArray(1,2)
.Screen	5
	getitem	OptionsScreen5BrkFilter,0,N1
	move	N1,OptionsArray(5,1)
	getitem	OptionsScreen5EMailOption,0,N1
	move	N1,OptionsArray(5,2)
	getitem	OptionsScreen5FileDefault,0,N1
	move	N1,OptionsArray(5,3)
.Screen	6
.	 getitem OptionsScreen6Init,0,N1
.	 move	 N1,OptionsArray(6,1)
.START PATCH 3.66 ADDED LOGIC
.Screen	7
	getitem OptionsScreen7Proj,0,N1
	move	 N1,OptionsArray(7,1)
	if (N1 = C0)
		setprop	Nord01EBButtonViewProj,height=0
	else
		setprop	Nord01EBButtonViewProj,height=20
	endif
.END PATCH 3.66 ADDED LOGIC
.Screen	8
.	 getitem OptionsScreen8OrderSearch,0,N1
.	 move	 N1,OptionsArray(8,1)
.Screen9
	getitem	OptionsScreen9View,0,N1
	move	N1,OptionsArray(9,1)
.Screen10
	getitem	OptionsScreen10View,0,N1
	move	N1,OptionsArray(10,1)
	getitem	OptionsScreen10Usage,0,N1
	move	N1,OptionsArray(10,2)

.Write to the file
	pack	APIFileName,"c:\progra~1\nincal\nord0001.pre",hexzero
	call	DeleteFile
	prep	preffile,"c:\progra~1\nincal\nord0001.pre"
	move	C0,N9
	loop
		add	C1,N9
		move	C0,N8
		loop
			add	C1,N8
			write	preffile,seq;OptionsArray(N9,N8)
			until	over
			until (N8 = OptionsArr2)
		repeat
		until (N9 = OptionsArr1)
	repeat
	close	preffile
	return
ReportGo
.START PATCH 3.72.7 REPLACED LOGIC
.	branch	result to ReportGo1,ReportGo2
	branch	result to ReportGo1,ReportGo2,ReportGo3
.END PATCH 3.72.7 REPLACED LOGIC
ReportGo1
.START PATCH 3.49 REPLACED LOGIC
.	call	OrderSetCampaignReports
.	setprop	Report2,visible=1
.	if (RptCan = NO)
..NCMPKEY already called	in Report2OK routine
.		getitem	ComboBoxes(1),0,N9
.		if (N9 = 1)	.Need File Name.
.			 call	 OrderSetMouseBusy
.			 getitem ComboBoxes(2),0,N8
.			 if (N8	= 1)
.				 call	 OrderCreateCampaign using NCMPFLD,Nord01eaComboShip,userlogn
.			 elseif	(N8 = 2)
.				 call	 OrderCreateCampaignA using NCMPFLD,Nord01eaComboShip,userlogn
.			 elseif	(N8 = 3)
.				 call	 OrderCreateCampaignB using NCMPFLD,Nord01eaComboShip,userlogn
.			 endif
..The above routines contain logic to set the mouse cursor free.
..However, if they bomb, you must ensure user has use of mouse, so following routine
..must be called.
.			 call	 OrderSetMouseFree
.		endif
.	endif
.
	prepare	tempfile,"c:\work\combo.dat"
	for N9,C1,"50"
		getitem	Nord01eaComboShip,N9,str55
		write	tempfile,SEQ;str55
	repeat
	close	tempfile
	batch	"\\nts0\c\apps\plb\code\plbwin.exe \\nts0\c\apps\plb\code\ncmp0001"
.	batch	"\\nts0\c\apps\plb\code\plbwin.exe \\nts0\c\library\develop\ncmp0001"
.END PATCH 3.49 REPLACED LOGIC
	return
ReportGo2
	call	OrderSetLCRReports
	setprop	Report2,visible=1
	if (RptCan = NO)
		pack	str30,str10,str11,str1,CNTNUM,str3,str4
		rep	zfill,str30
		call	OrderSetMouseBusy
		clear	taskname
		if (str1 >= "3")
.START PATCH 3.71.4 REPLACED LOGIC
.			append	"!f:\apps\winbatch\butil job=LCRRPT ",taskname
			append	"!\\nts0\c\apps\winbatch\butil job=LCRRPT ",taskname
.END PATCH 3.71.4 REPLACED LOGIC
		else
.START PATCH 3.71.4 REPLACED LOGIC
.			append	"!f:\apps\winbatch\butil job=LCRRPT2 ",taskname
			append	"!\\nts0\c\apps\winbatch\butil job=LCRRPT2 ",taskname
.END PATCH 3.71.4 REPLACED LOGIC
		endif
		append	" infile=",taskname
		append	str30,taskname
.		append	" F=default C=1",taskname
		append	" B=",taskname
		append	user,taskname
		reset	taskname
		batch	taskname
		call	OrderSetMouseFree
	endif
	return
.START PATCH 3.72.7 ADDED LOGIC
ReportGo3
	call	OrderSetLMLCRReports
	setprop	Report2,visible=1
	if (RptCan = NO)
		call	OrderSetMouseBusy
		batch	"\\nts0\c\apps\plb\code\plbwin.exe \\nts0\c\apps\plb\code\nord0045"
		call	OrderSetMouseFree
	endif
	return
.END PATCH 3.72.7 ADDED LOGIC
SecurityGo
	branch	result to SecurityGo1,SecurityGo2
SecurityGo1
	move	"(",progcode
	move	NO,SecFlag
	pack	str55,"		 To Enter FIXORD mode"
	setitem	PasswordStatMssg1,0,str55
	setprop	PasswordStatMssg1,visible=1
	setitem	PasswordEdit,0,""
	setfocus PasswordEdit
	clear	NPASFLD
	setprop	Passwrd,visible=1
	if (NPASFLD <> "(COSMO")
		return
	endif
.............................
	alert	note,"FIXORD mode permitted!",result
.	 SNDOPEN sfile,"\\nins1\d\users\aharkin\ohmigod.wav"
.	 SNDPLAY sfile
.	 SNDCLOSE sfile
	call	SecurityRecordSelection
	if (result = C1)
		if (N9 = 1)
			move	YES,SecFlag
			if (ORCODE <> STAR)
				call	StartModify
				call	OrderEnableLowerSecurity
			endif
		else
			move	YES,SecFlag3
			if (NLOLCODE <>	STAR)
				call	StartModify3
				call	OrderEnableLOLLowerSearch
			endif
		endif
	endif
	return
SecurityGo2
	call	SecurityRecordSelection
	return
SecurityRecordSelection
.Called	by: SecurityGo1,SecurityGo2
.Following is some ugly	code!!!
.I am using the	generic	Error message form, hiding the default OK button.
.An Edit Textbox, ComboBox and 2 Buttons are created and placed	on the forms.
	setprop	ErrorMssgOK,default=0
.START PATCH 3.42 REPLACED LOGIC
.	 create	 ErrorMssg;ComboBoxes(1)=60:80:50:150,"",";O)rder File;)LOL File;)Campaign File"
	create	ErrorMssg;ComboBoxes(1)=60:80:50:150,"",";O)rder File;)LOL File;)Campaign File;)Projection File"
.END PATCH 3.42	REPLACED LOGIC
	if (result = 2)
.START PATCH 3.42 REPLACED LOGIC
.		 create	 ErrorMssg;EditTextBoxes(1)=100:120:50:100,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
		create	ErrorMssg;EditTextBoxes(1)=100:120:50:125,MaxChars=6,EditType=2,SelectAll=1,Style=1,Border=1
.END PATCH 3.42	REPLACED LOGIC
	endif
	create	ErrorMssg;Buttons(1)=140:165:30:80,"O&K",default=1
	create	ErrorMssg;Buttons(2)=140:165:170:220,"C&ancel"
	activate ComboBoxes(1)
	if (result = 2)
		activate EditTextBoxes(1)
	endif
	activate Buttons(1),Sec2OK,howmany
	activate Buttons(2),Sec2Cancel,howmany
	setprop	ErrorMssgStat1,visible=1,fgcolor=red
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=0
	getprop	ErrorMssg,title=str55
	setitem	ErrorMssgStat1,0,""
	setitem	ErrorMssgStat2,0,"Select Record	Type"
	if (result = 2)
		setitem	ErrorMssgStat3,0,"Enter	Record to Update"
		setprop	ErrorMssg,title="FIXBUSY Routine"
	else
		setitem	ErrorMssgStat3,0,""
		setprop	ErrorMssg,title="FIXORD	Routine"
	endif
.Essentially disable default button
	getprop	ErrorMssgOK,width=N10
	setprop	ErrorMssgOK,width=0
	if (TabNum = C8)
		setitem	ComboBoxes(1),0,2
		if (result = 2)
			setitem	EditTextBoxes(1),0,NLOLLOL
		endif
	elseif (TabNum = C6 | TabNum = C7)
		setitem	ComboBoxes(1),0,3
		if (result = 2)
			setitem	EditTextBoxes(1),0,NCMPNUM
		endif
.START PATCH 3.42 ADDED	LOGIC
	elseif (TabNum = 10)
		setitem	ComboBoxes(1),0,4
		if (result = 2)
			setitem	EditTextBoxes(1),0,stat2fld
			setprop	EditTextBoxes(1),MaxChars=9
		endif
.END PATCH 3.42	ADDED LOGIC
	elseif (result = 2)
		if (TabNum < C5)
			setitem	EditTextBoxes(1),0,OLRN
		else
			setitem	EditTextBoxes(1),0,""
		endif
	endif
	setfocus ComboBoxes(1)
	setprop	ErrorMssg,visible=1
.Reset defaults	for this screen
	call	SetOrderErrorMssgDefault
	setprop	ErrorMssg,title=str55
	setprop	ErrorMssgStat1,fgcolor=black
	setprop	ErrorMssgOK,width=N10
	destroy	EditTextBoxes(1)
	destroy	Buttons(1)
	destroy	Buttons(2)
	destroy	ComboBoxes(1)
	return
Sec2OK
	if (result = 2)
.START PATCH 3.42 REPLACED LOGIC
.		 getitem EditTextBoxes(1),0,str6
.		 call	 Trim using str6
.		 if (str6 = "")
.			 setitem ErrorMssgStat1,0,"That	record does not	exist!!"
.			 setfocus EditTextBoxes(1)
.		 else
.			 call	 ZFILLIT using str6,C0
.			 setitem EditTextBoxes(1),0,str6
.			 getitem ComboBoxes(1),0,N9
.			 if (N9	= C1)
.				 move	 C1,NORDPATH
.				 move	 str6,NORDFLD
.				 move	 "Sec2OK-NORDTST",Location
.				 pack	 KeyLocation,"Key: ",NORDFLD
.				 call	 NORDTST
.			 elseif	(N9 = C2)
.				 move	 C1,NLOLPATH
.				 move	 str6,NLOLFLD
.				 move	 "Sec2OK-NLOLTST",Location
.				 pack	 KeyLocation,"Key: ",NLOLFLD
.				 call	 NLOLTST
.			 elseif	(N9 = C3)
.				 move	 C1,NCMPPATH
.				 move	 str6,NCMPFLD
.				 move	 "Sec2OK-NCMPTST",Location
.				 pack	 KeyLocation,"Key: ",NCMPFLD
.				 call	 NCMPTST
..........................
		getitem	EditTextBoxes(1),0,str9
		call	Trim using str9
		if (str9 = "")
			setitem	ErrorMssgStat1,0,"That record does not exist!!"
			setfocus EditTextBoxes(1)
		else
			if (N9 = C4)
				call	ZFILLIT	using str9,C0
				setitem	EditTextBoxes(1),0,str9
			else
				move	str9,str6
				call	ZFILLIT	using str6,C0
				setitem	EditTextBoxes(1),0,str6
			endif
			getitem	ComboBoxes(1),0,N9
			if (N9 = C1)
				move	C1,NORDPATH
				move	str6,NORDFLD
				move	"Sec2OK-NORDTST",Location
				pack	KeyLocation,"Key: ",NORDFLD
				call	NORDTST
			elseif (N9 = C2)
				move	C1,NLOLPATH
				move	str6,NLOLFLD
				move	"Sec2OK-NLOLTST",Location
				pack	KeyLocation,"Key: ",NLOLFLD
				call	NLOLTST
			elseif (N9 = C3)
				move	C1,NCMPPATH
				move	str6,NCMPFLD
				move	"Sec2OK-NCMPTST",Location
				pack	KeyLocation,"Key: ",NCMPFLD
				call	NCMPTST
			elseif (N9 = C4)
				move	C1,STATPATH
				move	str9,STAT2FLD
				move	"Sec2OK-STAT2TST",Location
				pack	KeyLocation,"Key: ",STAT2FLD
				call	STAT2TST
.END PATCH 3.42	REPLACED LOGIC
			endif
			if over
				setitem	ErrorMssgStat1,0,"That record does not exist!!"
				setfocus EditTextBoxes(1)
			else
				if (N9 = C1)
					move	"Sec2OK-NORDRELEASE",Location
					call	NORDRELEASE
				elseif (N9 = C2)
					move	"Sec2OK-NLOLRELEASE",Location
					call	NLOLRELEASE
				elseif (N9 = C3)
					move	"Sec2OK-NCMPRELEASE",Location
					call	NCMPRELEASE
.START PATCH 3.42 ADDED	LOGIC
				elseif (N9 = C4)
					move	"Sec2OK-STAT2RELEASE",Location
					call	STAT2RELEASE
.END PATCH 3.42	ADDED LOGIC
				endif
				setprop	ErrorMssg,visible=0
			endif
		endif
	else
		getitem	ComboBoxes(1),0,N9
		setprop	ErrorMssg,visible=0
		move	C1,result
	endif
	return
Sec2Cancel
	setprop	ErrorMssg,visible=0
	if (result = 1)
		move	C0,result
	endif
	return

....CLEAR SCREENS....

.Order File Maintenance	Screens
OrderClear
.Called	by:  RefreshOrderIndexDataList,Click_NORDMSK1ButtonNew,Click_NORDMSK1ButtonSearch
	setitem	NORDMSK1StatLR,0,""
	setitem	Nord001ACheckEntire,0,0
	setitem	Nord001ACheckExchange,0,0
	setitem	Nord001ACheckRent,0,0
	setitem	Nord001ACheckTest,0,0
	setitem	Nord001ACheckRetest,0,0
	setitem	Nord001ACheckMode,0,0
.	 setitem NordMsk1CheckApprove,0,0
	setitem	NORDMSK1CheckApprove,0,0
	setprop	NORDMSK1ButtonApprove,height=0
	setitem	Nord001AComboOffer,0,0
	setitem	Nord001AComboStatus,0,0
	setitem	Nord001AComboPending,0,0
	setitem	Nord001AEditBrk,0,""
	setitem	Nord001AEditBrkContact,0,""
	setitem	Nord001AEditCampaign,0,""
	setitem	Nord001AEditExchangeQty,0,""
	setitem	Nord001AEditExchangePrice,0,""
	setitem	Nord001AEditList,0,""
.START PATCH 3.72 REPLACED LOGIC
.	setitem	Nord001AEditListUniverse,0,""
	setitem	Nord001AEditSelUniverse,0,""
.END PATCH 3.72 REPLACED LOGIC
	setitem	Nord001AEditListSel,0,""
.START PATCH 3.72 ADDED LOGIC
	setitem	Nord001AEditListSelPrice,0,""
	setitem	Nord001AEditListSelCharge,0,""
	setitem	Nord001AComboListSelMod,0,1
	setitem	Nord001AStatListSelUniverse,0,""
	setitem	Nord001AStatListSelPrice,0,""
	setitem	Nord001AStatListSelCharge,0,""
	setitem	Nord001AStatRefPrice,0,""
	setitem	Nord001AStatTotPrice,0,""
	Nord01A2ListViewSelect.DeleteAllItems giving result
	Nord01A1ListViewRef1.DeleteAllItems giving result
	Nord01A1ListViewRef2.DeleteAllItems giving result
.END PATCH 3.72 ADDED LOGIC
	setitem	Nord001AEditMailDate,0,""
	setitem	Nord001AEditMlr,0,""
	setitem	Nord001AEditMlrContact,0,""
	setitem	Nord001AEditOrderDate,0,""
	setitem	Nord001AEditOrderQty,0,""
	setitem	Nord001AEditOwner,0,""
.Start patch 3.78.8
	setitem	Nord001AEditFulfillment,0,""
.End Patch 3.78.8 
	
	setitem	Nord001AEditPO,0,""
	setitem	Nord001AEditPrice,0,""
	setitem	Nord001AEditRtn,0,""
	setitem	Nord001AEditRtnDate,0,""
	setitem	Nord001AStatBrkCnt,0,""
	setitem	Nord001AStatBrkComp,0,""
	setitem	Nord001AStatCamp1,0,""
	setitem	Nord001AStatExchangeMssg,0,""
	setitem	Nord001AStatListName,0,""
	setitem	Nord001AStatMssg,0,""
	setitem	Nord001AStatMlrComp,0,""
	setitem	Nord001AStatOwnerAdd1,0,""
	setitem	Nord001AStatOwnerAdd2,0,""
	setitem	Nord001AStatOwnerComp,0,""
.Start Patch 3.78.8 	Object has been replace with fulfillment static text box
.	setitem	Nord001AStatOwnerComp2,0,""
	setitem	Nord001AStatFulfillmentComp,0,""
.End Patch 3.78.8 	
	setitem	Nord001AStatRtnComp,0,""
	setitem	Nord001AStatRtnComp2,0,""
	setitem	Nord001AStatRtnAdd1,0,""
	setitem	Nord001AStatRtnAdd2,0,""
	setitem	Nord001AStatUniverse,0,""
	setitem	Nord001AStatRevise,0,""
	setitem	Nord001TStatTotalNames,0,""
.START PATCH 3.71.9 ADDED LOGIC
	setitem	Nord001AStatBrkNew,0,""
.END PATCH 3.71.9 ADDED LOGIC
.START PATCH 3.74 ADDED LOGIC
	setitem	Nord001AStatMlrNew,0,""
.END PATCH 3.74 ADDED LOGIC
.nord001CListView	 Load this field when the program starts and never alter it???????
	setitem	nord001CEditSpecial,0,""
	setitem	nord001CEditSpecial1,0,""
	setitem	nord001CEditSpecial2,0,""
	setitem	nord001CEditSpecial3,0,""
	setitem	nord001CStatListName,0,""
	setitem	nord001CStatMlrComp,0,""
	setitem	nord001CStatOrderCode,0,""
	setitem	Nord001bCheckDirect,0,0
	setitem	Nord001bComboBRKGuar,0,1
	setitem	Nord001bComboCaller,0,1
	setitem	Nord001bComboCont,0,1
	setitem	Nord001bComboContact,0,1
	setitem	Nord001bComboMedia,0,0
	setitem	Nord001bComboNINGuar,0,1
	setitem	Nord001bComboNet,0,1
	setitem	Nord001bComboSam,0,1
.START PATCH 3.72.2 REPLACED LOGIC
.	deleteitem Nord001bComboSample,0
.	setitem	Nord001bComboSample,0,1
	setitem	Nord001bEditSample,0,""
	Nord001bListViewSamples.DeleteAllItems giving result
	setprop	Nord001bListViewSamples,height=0
.END PATCH 3.72.2 REPLACED LOGIC
	setitem	Nord001bComboShip,0,0
	setitem	Nord001bComboTape,0,0
	setitem	Nord001bComboTestCode,0,1
	setitem	Nord001bComboZipScreen,0,1
	setitem	Nord001bEditAttn,0,""
.START PATCH 3.71 ADDED LOGIC
	setitem	Nord001bEditFax,0,""
.END PATCH 3.71 ADDED LOGIC
	setitem	Nord001bEditContDate,0,""
	setitem	Nord001bEditContLR,0,""
	setitem	Nord001bEditContQty,0,""
	setitem	Nord001bEditKeyInfo,0,""
	setitem	Nord001bEditNetCharge,0,""
	setitem	Nord001bEditNetMin,0,""
	setitem	Nord001bEditNetPercent,0,""
	setitem	Nord001bEditNetPer,0,""
	setitem	Nord001bEditNetQty,0,""
	setitem	Nord001bEditSales,0,""
	setitem	Nord001bStatHistory,0,""
	setitem	Nord001bStatClearStat,0,""
	setitem	Nord001bStatListName,0,""
	setitem	Nord001bStatMlrComp,0,""
	setitem	Nord001bStatOrderCode,0,""
	setitem	Nord001bStatSales2,0,""
	setitem	Nord001bStatTypist2,0,""
	setitem	Nord001bStatRevise,0,""
	setitem	Nord001bStatOrderQty,0,""
.START PATCH 3.65 ADDED LOGIC
	setitem	Nord001bStatSamplesInactive,0,""
.END PATCH 3.65 ADDED LOGIC
.START PATCH 3.68.3 ADDED LOGIC
	setitem	nord001CStatListSel,0,""
.END PATCH 3.68.3 ADDED LOGIC
.START PATCH 3.7 ADDED LOGIC
	setitem	nord001CStatExc,0,""
.END PATCH 3.7 ADDED LOGIC
	return

.Campaign File Maintenance Screens
OrderCampaignClear
	setitem	NORDMSK2StatCamp,0,""
	setitem	Nord01eaCheckBillDirect,0,0
	setitem	Nord01eaComboStatus,0,1
	setitem	Nord01eaComboContact,0,1
	setitem	Nord01eaComboMedia,0,1
	setitem	Nord01eaComboOffer,0,1
.	 setitem Nord01eaComboPackage,0,1
	Nord01eaListViewPackage.DeleteAllItems giving N9
	setitem	Nord01eaComboPlanner,0,1
	setitem	Nord01eaComboReport,0,1
	setitem	Nord01eaComboSample,0,1
	setitem	Nord01eaComboShip,0,1
	setitem	Nord01eaEditBrkContact,0,""
	setitem	Nord01eaEditBrk,0,""
	setitem	Nord01eaEditCampDate,0,""
	setitem	Nord01eaEditCampName,0,""
	setitem	Nord01eaEditCutOffDate,0,""
	setitem	Nord01eaEditGiftChange,0,""
	setitem	Nord01eaEditGrossQty,0,""
	setitem	Nord01eaEditKey,0,""
	setitem	Nord01eaEditMailDate,0,""
	setitem	Nord01eaEditMlr,0,""
    	setitem	Nord01eaEditNetQty,0,""
	setitem	Nord01eaEditPO,0,""
	setitem	Nord01eaEditRespChange,0,""
	setitem	Nord01eaEditRtnDate,0,""
	setitem	Nord01eaEditRtn,0,""
	setitem	Nord01eaEditSpecial,0,""
	setitem	Nord01eaStatBrkCntName,0,""
	setitem	Nord01eaStatBrkComp,0,""
	setitem	Nord01eaStatMlrComp,0,""
	setitem	Nord01eaStatProjections,0,""
	setitem	Nord01eaStatRevise,0,""
	setitem	Nord01eaStatRtnComp,0,""
	setitem	Nord01eaStatUniverse,0,""
.START PATCH 3.75.7 REMOVED LOGIC
..START PATCH 3.71.9 ADDED LOGIC
.	setitem	Nord01eaStatBrkNew,0,""
..END PATCH 3.71.9 ADDED LOGIC
..START PATCH 3.74 ADDED LOGIC
.	setitem	Nord01eaStatMlrNew,0,""
..END PATCH 3.74 ADDED LOGIC
.END PATCH 3.75.7 REMOVED LOGIC
OrderCampaignClear1
.This portion used for 'New' Campaign -	clear previous totals
	setitem	Nord01eaStatTestsNum,0,""
	setitem	Nord01eaStatTestsNetNum,0,""
	setitem	Nord01eaStatCampaignNum,0,""
	setitem	Nord01eaStatCampaignNetNum,0,""
	setitem	Nord01eaStatContNum,0,""
	setitem	Nord01eaStatContNetNum,0,""
.
	setitem	Nord01EBStatCampName2,0,""
	setitem	Nord01EBStatMlrComp,0,""
	call	OrderLOLDetailClear
	return

.List Of List File Maintenance Screen
OrderLOLClear
	setitem	NORDMSK3StatLOL,0,""
	setitem	Nord01ECCheckExchange,0,0
	setitem	Nord01ECCheckRegional,0,0
	setitem	Nord01ECCheckRent,0,0
	setitem	Nord01ECCheckReTest,0,0
	setitem	Nord01ECCheckTest,0,0
	setitem	Nord01ECComboContact,0,1
	setitem	Nord01ECComboOffer,0,1
.TESTING
.	 setitem Nord01ECComboPackage,0,1
	setitem	Nord01ECComboPlanner,0,1
	setitem	Nord01ECComboSample,0,1
	setitem	Nord01ECComboStatus,0,1
	setitem	Nord01ECEditBrk,0,""
	setitem	Nord01ECEditBrkContact,0,""
	setitem	Nord01ECEditLR,0,""
	setitem	Nord01ECEditCamp,0,""
	setitem	Nord01ECEditLOLDate,0,""
	setitem	Nord01ECStatCampName,0,""
.TESTING
.	 setitem Nord01ECEditGift,0,""
	setitem	Nord01ECEditGrossQty,0,""
	setitem	Nord01ECEditList,0,""
	setitem	Nord01ECEditListSel,0,""
	setitem	Nord01ECEditMailDate,0,""
	setitem	Nord01ECEditMlr,0,""
	setitem	Nord01ECEditNetQty,0,""
	setitem	Nord01ECEditNetPer,0,""
	setitem	Nord01ECEditOwner,0,""
	setitem	Nord01ECEditPO,0,""
.TESTING
.	 setitem Nord01ECEditResp,0,""
	setitem	Nord01ECEditRtn,0,""
	setitem	Nord01ECEditSpecial,0,""
	setitem	Nord01ECEditSpecial1,0,""
	setitem	Nord01ECEditUniverse,0,""
.START PATCH 3.72 ADDED LOGIC
	setitem	Nord01ECEditPrice,0,""
	setitem	Nord01ECEditSelPrice,0,""
	setitem	Nord01ECStatSelPrice,0,""
	setitem	Nord01ECComboMod,0,1
	setitem	Nord01ECStatUniverse2,0,""
	setitem	Nord01ECStatPrice2,0,""
	setitem	Nord01ECStatRefPrice,0,""
	setitem	Nord01ECStatTotPrice,0,""
	Nord01ECListViewSelect.DeleteAllItems giving result
	Nord08A1ListViewRef1.DeleteAllItems giving result
	Nord08A1ListViewRef2.DeleteAllItems giving result
.END PATCH 3.72 ADDED LOGIC
	setitem	Nord01ECStatBrkCntName,0,""
	setitem	Nord01ECStatBrkComp,0,""
.START PATCH 3.71.9 ADDED LOGIC
	setitem	Nord01ECStatBrkNew,0,""
.END PATCH 3.71.9 ADDED LOGIC
.START PATCH 3.74 ADDED LOGIC
.	setitem	Nord01ECStatMlrNew,0,""
.END PATCH 3.74 ADDED LOGIC
.TESTING
.	 setitem Nord01ECStatCost,0,""
.	 setitem Nord01ECStatCostMember,0,""
.	 setitem Nord01ECStatExcTot,0,""
.	 setitem Nord01ECStatGift,0,""
.	 setitem Nord01ECStatListCost,0,""
	setitem	Nord01ECStatListName,0,""
	setitem	Nord01ECStatMlrComp,0,""
.TESTING
.	 setitem Nord01ECStatNetAdj,0,""
	setitem	Nord01ECStatOwnerComp,0,""
.TESTING
.	 setitem Nord01ECStatRate,0,""
.	 setitem Nord01ECStatRentTot,0,""
.	 setitem Nord01ECStatRtn,0,""
.	 setitem Nord01ECStatRevenue,0,""
	setitem	Nord01ECStatRevise,0,""
	setitem	Nord01ECStatRtnComp,0,""
.TESTING
.	 setitem Nord01ECStatTotalCost,0,""
	setitem	Nord01ECStatTotalNames,0,""
	setitem	Nord01ECStatXstat,0,""
.START PATCH 3.4 ADDED LOGIC
	Nord01ECListViewPackages.DeleteAllItems giving N9
	Nord01ECListViewMlrPackages.DeleteAllItems giving	N9
.END PATCH 3.4 ADDED LOGIC
.START PATCH 3.65 REPLACED LOGIC
	setitem	Nord01ECStatSamplesInactive,0,""
.END PATCH 3.65 REPLACED LOGIC
	return

OrderLOLDetailClear
	Nord01EBListView.DeleteAllItems giving N9
	Nord01EBListView2.DeleteAllItems giving N9
.START PATCH 3.66 ADDED LOGIC
	Nord01EBListViewProj.ListItems.Clear
.END PATCH 3.66 ADDED LOGIC
	setitem	Nord01EBEditSpecial,0,""
	setitem	Nord01EBEditSpecial1,0,""
	setitem	Nord01EBEditSpecial2,0,""
	setitem	Nord01EBStatTotalNames,0,""
	return

OrderStatClear
	setitem	NSTA001AEditMlr,0,""
	setitem	NSTA001BEditSource,0,""
	setitem	NSTA001AEditLR,0,""
	setitem	NSTA001BEditLRB,0,""
	setitem	NSTA001AEditCampaign,0,""
	setitem	NSTA001AStatMlrName,0,""
	setitem	NSTA001BEditKeyCode,0,""
	setitem	NSTA001AEditMlrList,0,""
	setitem	NSTA001BEditListCost,0,""
	setitem	NSTA001AEditListNum,0,""
	setitem	NSTA001BEditLValue,0,""
	setitem	NSTA001BEditMailCost,0,""
	setitem	NSTA001AEditMailDate,0,""
	setitem	NSTA001AEditMailQty,0,""
	setitem	NSTA001BEditPDate,0,""
	setitem	NSTA001AEditPackage,0,""
	setitem	NSTA001BEditPackageB,0,""
	setitem	NSTA001AEditPackID,0,""
	setitem	NSTA001BEditPackIDB,0,""
	setitem	NSTA001BEditPackageCost,0,""
	setitem	NSTA001BEditPackageCostThou,0,""
	setitem	NSTA001BEditResponse,0,""
	setitem	NSTA001BEditRevenue,0,""
	setitem	NSTA001AEditSelect,0,""
	setitem	NSTA001BEditSelectB,0,""
.START PATCH 3.4 ADDED LOGIC
	setitem	NSTA001AEditAvgGift,0,""
	setitem	NSTA001AEditCostMember,0,""
	setitem	NSTA001AEditRespRate,0,""
	setitem	NSTA001BEditTotalCost,0,""
.END PATCH 3.4 ADDED LOGIC
	setitem	NSTA001AEditType,0,""
	setitem	NSTA001AEditWeeks,0,""
	setitem	NSTA001AStatList,0,""
	setitem	NSTA001BStatListB,0,""
	setitem	NSTA0002StatRecords,0,""
OrderStatClear2
	setitem	NSTA001ACheckLOL2,0,0
	setitem	NSTA001BCheckLOL2B,0,0
	setitem	NSTA001AEditAvgGift2,0,""
	setitem	NSTA001AEditLR2,0,""
	setitem	NSTA001BStatLR2C,0,""
	setitem	NSTA001AEditStatNum2,0,""
	setitem	NSTA001BEditListCostM2,0,""
	setitem	NSTA001BEditMailCost2,0,""
	setitem	NSTA001BEditPackageCost2,0,""
	setitem	NSTA001AStatCostMember2B,0,""
	setitem	NSTA001BStatUsage2B,0,""
	setitem	NSTA001BStatOrders2B,0,""
.START PATCH 3.72.6 ADDED LOGIC
	setitem	NSTA001AStatUsage2A,0,""
	setitem	NSTA001AStatOrders2A,0,""
.END PATCH 3.72.6 ADDED LOGIC
	setitem	NSTA001AEditRespRate2,0,""
	setitem	NSTA001BEditLValue2,0,""
	setitem	NSTA001BStatReturns2B,0,""
	setitem	NSTA001BStatRevenue2B,0,""
	setitem	NSTA001AEditAvgNet2,0,""
	setitem	NSTA001AEditNetReq2,0,""
	setitem	NSTA001AEditNetRec2,0,""
	setitem	NSTA001AEditMailQty2,0,""
	setitem	NSTA001BEditExBase2,0,""
	setitem	NSTA001BStatExTotal2B,0,""
	setitem	NSTA001BEditRentBase2,0,""
	setitem	NSTA001BEditRunCharge2,0,""
	setitem	NSTA001BEditSelectFee2,0,""
	setitem	NSTA001BEditShipTape2,0,""
	setitem	NSTA001BStatRentTotal2B,0,""
.	setitem	STATSEditPackageCostThou2,0,""
.	setitem	NSTA001BListViewPackage,0,1
	setitem	NSTA0002StatRecords2,0,""
.	setitem	NSTA001AStatDiff2B,0,""
	setitem	NSTA001BStatTotCost2B,0,""
	setitem	NSTA001AStatNetNames2B,0,""
	setitem	NSTA001BStatNet2B,0,""
	setitem	NSTA001BStatListCost2B,0,""
	setitem	NSTA001BStatProCost2B,0,""
	setitem	NSTA001CEditNotes2,0,""
.START PATCH 3.43 ADDED LOGIC
	setitem	NSTA001AEditType2B,0,""
	setitem	NSTA001BStatRecInfo2,0,""
.END PATCH 3.43 ADDED LOGIC
.START PATCH 3.45 ADDED LOGIC
	setitem	NSTA001AEditSelect2,0,""
	setitem	NSTA001AEditLRQty2,0,""
.END PATCH 3.45 ADDED LOGIC
.START PATCH 3.47 ADDED LOGIC
	setitem	NSTA001AEditSelUniverse2,0,""
	setitem	NSTA001AEditSelPrice2,0,""
	setitem	NSTA001AEditSelPrice3,0,""
.END PATCH 3.47 ADDED LOGIC
.START PATCH 3.72 ADDED LOGIC
	NSTA001AListViewSelect.DeleteAllItems giving result
.END PATCH 3.72 ADDED LOGIC
OrderStatClear2B
.Clears	fields which are useful	for display only.
.These are fields which	are not	stored in the Projection file, but instead
.are pulled from the LR	fields
	setitem	NSTA001AStatMlr2B,0,""
	setitem	NSTA001AEditMlr2B,0,""
	setitem	NSTA001AStatMlrName,0,""
	setitem	NSTA001AStatCampaign2B,0,""
	setitem	NSTA001AStatListNum2B,0,""
	setitem	NSTA001AEditListNum2B,0,""
	setitem	NSTA001AStatMailDate2B,0,""
.START PATCH 3.45 REMOVED LOGIC
.	setitem	NSTA001AStatSelect2B,0,""
.END PATCH 3.45 REMOVED LOGIC
.START PATCH 3.43 REMOVED LOGIC
.	setitem	NSTA001AStatType2B,0,""
.END PATCH 3.43 REMOVED LOGIC
	setitem	NSTA001AStatList2,0,""
	setitem	NSTA001AStatLRCamp2,0,""
	setitem	NSTA001AEditRecoQty2,0,""
.START PATCH 3.45 REMOVED LOGIC
.	setitem	NSTA001AStatLRQty2B,0,""
.END PATCH 3.45 REMOVED LOGIC
	setitem	NSTA001AStatLRNetQty2B,0,""
.START PATCH 3.4 ADDED LOGIC
	setitem	NSTA001CEditNotes3,0,""
	setitem	NSTA001CEditNotes4,0,""
	setitem	NSTA001CEditNotes5,0,""
.END PATCH 3.4 ADDED LOGIC
.START PATCH 3.72.6 ADDED LOGIC
	setitem	NSTA001AStatXstat2,0,""
.END PATCH 3.72.6 ADDED LOGIC
OrderStatClear2C
	setitem	NSTA001AEditPackNum2,0,""
	setitem	NSTA001AStatPackage2B,0,""
	setitem	NSTA001AStatPackID2B,0,""
	setitem	NSTA001BStatPackNum2C,0,""
	setitem	NSTA001BStatPackName2,0,""
	return

OrderStatClear2D
	setitem	NSTA001AEditStatNum2,0,""
	return

OrderHistClear
	setitem	NSTA001EEditCamp,0,""
	setitem	NSTA001EEditGift,0,""
	setitem	NSTA001EEditGrossRev,0,""
	setitem	NSTA001EEditInMail,0,""
	setitem	NSTA001EEditKey,0,""
	setitem	NSTA001EEditLCost,0,""
	setitem	NSTA001EEditLR,0,""
	setitem	NSTA001EEditLType,0,""
	setitem	NSTA001EEditLValue,0,""
	setitem	NSTA001EEditList,0,""
	setitem	NSTA001EEditMDate,0,""
	setitem	NSTA001EEditMailer,0,""
	setitem	NSTA001EEditMbrCost,0,""
	setitem	NSTA001EEditMlrList,0,""
	setitem	NSTA001EEditPCost,0,""
	setitem	NSTA001EEditPCostM,0,""
	setitem	NSTA001EEditPDate,0,""
	setitem	NSTA001EEditPID,0,""
	setitem	NSTA001EEditPName,0,""
	setitem	NSTA001EEditQtyMailed,0,""
	setitem	NSTA001EEditResp,0,""
	setitem	NSTA001EEditRespRate,0,""
	setitem	NSTA001EEditSelect,0,""
	setitem	NSTA001EEditSource,0,""
	setitem	NSTA001EEditTotCost,0,""
	setitem	NSTA001EEditWeeks,0,""
	setitem	NSTA001EStatListName,0,""
	setitem	NSTA001EStatMlrName,0,""
	setitem	NSTA001EStatRecords,0,""
	return

....UPPER SCREENS....

.Order File Maintenance	Screens
OrderDisableUpperButtons
	setprop	NORDMSK1ButtonOk,enabled=0
	setprop	nord0001ButtonExit,enabled=0
.	 setprop NORDMSK1ButtonPrint,enabled=0
	setprop	NORDMSK1ButtonNew,enabled=0
	setprop	NORDMSK1ButtonModify,enabled=0
	setprop	NORDMSK1ButtonSearch,enabled=0
	setprop	NORDMSK1ButtonReprint,enabled=0
	setprop	NORDMSK1ButtonFind,enabled=0
	setprop	NORDMSK1ButtonCopy,enabled=0
.START PATCH 3.71.3 ADDED LOGIC
	setprop	NORDMSK1ButtonPrint,enabled=0,height=0
.END PATCH 3.71.3 ADDED LOGIC
	setprop	NORDMSK1ButtonHotPrint,enabled=0
	setprop	NORDMSK1ButtonSave,enabled=0
	setprop	NORDMSK1ButtonQuit,enabled=0
	setprop	NORDMSK1ButtonCancel,enabled=0
	return
OrderDisableUpper
.Called	by:  Click_NORDMSK1ButtonModify,Click_NORDMSK1ButtonNew,Click_NORDMSK1ButtonSearch
	setprop	NORDMSK1EditSearchKey,enabled=0
	call   OrderDisableUpperButtons
	setprop	Nord001TListView,enabled=0,bgcolor=grey
	setprop	NORDMSK1VScrollLR,enabled=0
	setprop	NORDMSK1ButtonSearch,enabled=0
OrderDisableScreen5All
	setprop	Nord001DButtonOk,enabled=0
	setprop	Nord001DListView,enabled=0
.START PATCH	3.78.2	REMOVED LOGIC
.	setprop	Order5ListView2,enabled=0
.	setprop	Order5ListView3,enabled=0
.	setprop	Order5ListView4,enabled=0
.	setprop	Order5ListView5,enabled=0
.	setprop	Order5ListView6,enabled=0
.END PATCH	3.78.2	REMOVED LOGIC
OrderDisableScreen5
	setprop	Nord001DButtonClearExchange,enabled=0
	setprop	Nord001DButtonClearRent,enabled=0
	setprop	Nord001DButtonClearExcRent,enabled=0
	setprop	Nord001DButtonEmail,enabled=0
	setprop	Nord001DButtonFax,enabled=0
	setprop	Nord001DButtonDelete,enabled=0
	setprop	Nord001DButtonDeny,enabled=0
	setprop	Nord001DButtonPrint,enabled=0
	setprop	Nord001DEditSpecial1,enabled=0
	setitem	Nord001DEditSpecial1,0,""
	return

OrderEnableUpper
.Called	by:  Click_NORDMSK1ButtonCancel,Click_NORDMSK1ButtonFind,Click_NORDMSK1ButtonQuit,Click_NORDMSK1ButtonSave
	move	"Y",ExitFlag
	setprop	NORDMSK1EditSearchKey,enabled=1
	setprop	NORDMSK1ButtonOk,enabled=1
.	 setprop NORDMSK1ButtonPrint,enabled=1
	setprop	NORDMSK1ButtonNew,enabled=1
	setprop	Nord001TListView,enabled=1,bgcolor=white
	setprop	NORDMSK1VScrollLR,enabled=1
	setprop	NORDMSK1ButtonSearch,enabled=1
	setprop	NORDMSK1ButtonCopy,enabled=1
.START PATCH 3.71.3 ADDED LOGIC
	setprop	NORDMSK1ButtonPrint,enabled=1,height=23
.END PATCH 3.71.3 ADDED LOGIC
	call	Order5OkEnd
	call	OrderEnableExit
	return

OrderEnableScreen5
	setprop	Nord001DButtonFax,enabled=1
	setprop	Nord001DButtonClearRent,enabled=1
	setprop	Nord001DButtonClearExchange,enabled=1
	setprop	Nord001DButtonClearExcRent,enabled=1
	setprop	Nord001DButtonDeny,enabled=1
	setprop	Nord001DEditSpecial1,enabled=1
OrderEnableScreen5A
	setprop	Nord001DListView,enabled=1
.START PATCH	3.78.2	REMOVED LOGIC
.	setprop	Order5ListView2,enabled=1
.	setprop	Order5ListView3,enabled=1
.	setprop	Order5ListView4,enabled=1
.	setprop	Order5ListView5,enabled=1
.	setprop	Order5ListView6,enabled=1
.END PATCH	3.78.2	REMOVED LOGIC
OrderEnableScreen5B
	setprop	Nord001DButtonPrint,enabled=1
	setprop	Nord001DButtonEmail,enabled=1
.START PATCH	3.78.2	ADDED LOGIC
	setprop Nord001DEditSpecial1, enabled=1
.END PATCH	3.78.2	ADDED LOGIC
	scan	INITS,revtyps
	if equal
		setprop	Nord001DButtonDelete,enabled=1
	endif
OrderEnableScreen5C
	setprop	Nord001DButtonOk,enabled=1
	setprop	Nord001DButtonStop,enabled=0
	move	NO,StopFlag
	return

.Campaign File Maintenance Screens
OrderDisableCampUpperButtons
	setprop	NORDMSK2ButtonOK,enabled=0
	setprop	nord0001ButtonExit,enabled=0
.	 setprop Nordmsk2Print2,enabled=0
	setprop	NORDMSK2ButtonNew,enabled=0
	setprop	NORDMSK2ButtonModify,enabled=0
	setprop	NORDMSK2ButtonSearch,enabled=0
	setprop	NORDMSK2ButtonFind,enabled=0
	setprop	NORDMSK2ButtonSave,enabled=0
	setprop	NORDMSK2ButtonQuit,enabled=0
	setprop	NORDMSK2ButtonCancel,enabled=0
	return
OrderDisableCampUpper
.Called	by:  Click_NORDMSK2ButtonModify,Click_NORDMSK2ButtonNew,Click_NORDMSK2ButtonSearch
	setprop	NORDMSK2EditSearchKey,enabled=0
	call	OrderDisableCampUpperButtons
	setprop	NORDMSK2ListView,enabled=0,bgcolor=grey
	setprop	NORDMSK2VScrollLR,enabled=0
	setprop	NORDMSK2ButtonSearch,enabled=0
	call	OrderDisableScreen5All
OrderDisableScreen7All
	setprop	Nord01EBListView,enabled=0
	setprop	Nord01EBListView2,enabled=0
	setprop	Nord01EBButtonOrder,enabled=0
	setprop	Nord01EBButtonLCR,enabled=0
	setprop	Nord01EBButtonDelete,enabled=0
	setprop	Nord01EBButtonMove,enabled=0
	setprop	Nord01EBButtonNet,enabled=0
	setprop	Nord01EBButtonSecond,enabled=0
.START PATCH 3.71.3 ADDED LOGIC
	setprop	Nord01EBButtonPrint,enabled=0
.END PATCH 3.71.3 ADDED LOGIC
	setprop	Nord01EBButtonProjection,enabled=0
.	 setitem Nord01EBEditSpecial1,0,""
	return

OrderEnableCampUpper
.Called	by:  Click_NORDMSK2ButtonCancel,Click_NORDMSK2ButtonFind,Click_NORDMSK2ButtonQuit,Click_OrderSave 2
	move	"Y",ExitFlag2
	setprop	NORDMSK2EditSearchKey,enabled=1
	setprop	NORDMSK2ButtonOK,enabled=1
.	 setprop nordmsk2Print2,enabled=1
	setprop	NORDMSK2ButtonNew,enabled=1
	setprop	NORDMSK2ListView,enabled=1,bgcolor=white
	setprop	NORDMSK2VScrollLR,enabled=1
	setprop	NORDMSK2ButtonSearch,enabled=1
	call	Order5OKEnd
	call	OrderEnableExit
OrderEnableScreen7All
	setprop	Nord01EBListView,enabled=1
	setprop	Nord01EBListView2,enabled=1
	setprop	Nord01EBButtonOrder,enabled=1
	setprop	Nord01EBButtonLCR,enabled=1
	setprop	Nord01EBButtonDelete,enabled=1
	setprop	Nord01EBButtonMove,enabled=1
	setprop	Nord01EBButtonNet,enabled=1
	setprop	Nord01EBButtonSecond,enabled=1
.START PATCH 3.71.3 ADDED LOGIC
	setprop	Nord01EBButtonPrint,enabled=1
.END PATCH 3.71.3 ADDED LOGIC
	setprop	Nord01EBButtonProjection,enabled=1
.	 setitem Nord01EBEditSpecial1,0,""
	return

.List Of List File Maintenance Screens
OrderDisableLOLUpperButtons
	setprop	NORDMSK3ButtonOK,enabled=0
	setprop	nord0001ButtonExit,enabled=0
.	 setprop Nordmsk3Print3,enabled=0
	setprop	NORDMSK3ButtonNew,enabled=0
	setprop	NORDMSK3ButtonModify,enabled=0
	setprop	NORDMSK3ButtonSearch,enabled=0
	setprop	NORDMSK3ButtonFind,enabled=0
	setprop	NORDMSK3ButtonCopy,enabled=0
	setprop	NORDMSK3ButtonSave,enabled=0
	setprop	NORDMSK3ButtonQuit,enabled=0
	setprop	NORDMSK3ButtonCancel,enabled=0
	setprop	NORDMSK3ButtonDelete,enabled=0
.
	setprop	NORDMSK3ButtonOrder,enabled=0
	setprop	NORDMSK3ButtonLCR,enabled=0
.START PATCH 3.48 ADDED LOGIC
	setprop	NORDMSK3ButtonProj,enabled=0
.END PATCH 3.48 ADDED LOGIC
	return
OrderDisableLOLUpper
.Called	by:  Click_NORDMSK3ButtonModify,Click_NORDMSK3ButtonNew,Click_NORDMSK3ButtonSearch,Click_NORDMSK3ButtonCopy
	setprop	NORDMSK3EditSearchKey,enabled=0
	call	OrderDisableLOLUpperButtons
	setprop	NORDMSK3ListView,enabled=0,bgcolor=grey
	setprop	NORDMSK3VScrollLR,enabled=0
	setprop	NORDMSK3ButtonSearch,enabled=0
	call	OrderDisableScreen5All
	return

OrderEnableLOLUpper
.Called	by:  Click_NORDMSK3ButtonCancel,Click_NORDMSK3ButtonFind,Click_NORDMSK3ButtonQuit,Click_NORDMSK3ButtonSave,Click_NORDMSK3ButtonCopy
	move	"Y",ExitFlag3
	setprop	NORDMSK3EditSearchKey,enabled=1
	setprop	NORDMSK3ButtonOK,enabled=1
.	 setprop nordmsk3print3,enabled=1
	setprop	NORDMSK3ButtonNew,enabled=1
	setprop	NORDMSK3ListView,enabled=1,bgcolor=white
	setprop	NORDMSK3VScrollLR,enabled=1
	setprop	NORDMSK3ButtonSearch,enabled=1
	call	Order5OKEnd
	call	OrderEnableExit
	return
OrderEnableExit
	if (ExitFlag = YES AND ExitFlag2 = YES AND ExitFlag3 = YES AND ExitFlag4 = YES AND ExitFlag5 = YES)
		setprop	nord0001ButtonExit,enabled=1
	endif
	return

OrderStatDisableUpperButton
	setprop	NSTA0002ButtonOk,enabled=0
	setprop	NSTA0002ButtonNew,enabled=0
	setprop	NSTA0002ButtonModify,enabled=0
	setprop	NSTA0002ButtonDelete,enabled=0
	setprop	nord0001ButtonExit,enabled=0
	return

OrderStatEnableUpperButton
	move	"Y",ExitFlag5
	setprop	NSTA0002ButtonOk,enabled=1
	setprop	NSTA0002ButtonNew,enabled=1
	NSTA0002ListView2.GetItemCount giving N9
	if (N9 > 0)
		setprop	NSTA0002ButtonModify,enabled=1
		setprop	NSTA0002ButtonDelete,enabled=1
	endif
	call	OrderEnableExit
	return

OrderStatDisableLowerButton
	setprop	NSTA0002ButtonQuit,enabled=0
	setprop	NSTA0002ButtonSave,enabled=0
	return

OrderStatEnableLowerButton
	setprop	NSTA0002ButtonQuit,enabled=1
	setprop	NSTA0002ButtonSave,enabled=1
	return

OrderStatDisableUpper
.Called	by:
	setprop	NSTA0002EditSearchLR,enabled=0,bgcolor=grey
	setprop	NSTA0002EditSearchMLR,enabled=0,bgcolor=grey
	setprop	NSTA0002EditSearchSource,enabled=0,bgcolor=grey
	setprop	NSTA0002ButtonOk,enabled=0
	setprop	NSTA0002ButtonNew,enabled=0
	setprop	NSTA0002ListView,enabled=0,bgcolor=grey
	return

OrderStatEnableUpper
.Called	by:
	setprop	NSTA0002EditSearchLR,enabled=1,bgcolor=white
	setprop	NSTA0002EditSearchMLR,enabled=1,bgcolor=white
	setprop	NSTA0002EditSearchSource,enabled=1,bgcolor=white
	setprop	NSTA0002ButtonOk,enabled=1
	setprop	NSTA0002ButtonNew,enabled=1
	setprop	NSTA0002ListView,enabled=1,bgcolor=white
	return

OrderStatDisable2
	setprop	NSTA001DButtonOK,enabled=0
	setprop	NSTA001DButtonDelete,enabled=0
	setprop	NSTA001DButtonCopy,enabled=0
.START PATCH 3.49.1 ADDED LOGIC
	setprop	NSTA001DButtonModify,enabled=0
.END PATCH 3.49.1 ADDED LOGIC
.START PATCH 3.49.2 ADDED LOGIC
	setprop	NSTA001DListView,enabled=0
	setprop	NSTA001DListView2,enabled=0
	setprop	NSTA001DListView3,enabled=0
	setprop	NSTA001DListView4,enabled=0
.END PATCH 3.49.2 ADDED LOGIC
	return

OrderStatEnable2
	setprop	NSTA001DButtonOK,enabled=1
	setprop	NSTA001DButtonDelete,enabled=1
	setprop	NSTA001DButtonCopy,enabled=1
.START PATCH 3.49.1 ADDED LOGIC
	setprop	NSTA001DButtonModify,enabled=1
.END PATCH 3.49.1 ADDED LOGIC
.START PATCH 3.49.2 ADDED LOGIC
	setprop	NSTA001DListView,enabled=1
	setprop	NSTA001DListView2,enabled=1
	setprop	NSTA001DListView3,enabled=1
	setprop	NSTA001DListView4,enabled=1
.END PATCH 3.49.2 ADDED LOGIC
	return

....LOWER SCREENS....

.Order File Maintenance	Screens
OrderDisableLower
.Called	by:  Click_NORDMSK1ButtonCancel,Click_NORDMSK1ButtonFind,Click_NORDMSK1ButtonQuit,Click_NORDMSK1ButtonSave
	move	C0,mod
	setprop	Nord001ACheckEntire,enabled=0
	setprop	Nord001ACheckExchange,enabled=0
	setprop	Nord001ACheckRent,enabled=0
	setprop	Nord001ACheckTest,enabled=0
	setprop	Nord001ACheckRetest,enabled=0
	setprop	Nord001ACheckMode,enabled=0
.Order Search fields
	setprop	Nord001AComboEOF,visible=0
	setprop	Nord001AComboType,visible=0
	setprop	Nord001AEditRange,visible=0
	setprop	Nord001AStatRange,visible=0
.START PATCH 3.72 ADDED LOGIC
.In case we want to force the Minimum Height on these items
.	setprop	Nord01A1ListViewRef1,height=MinSHeight
.	setprop	Nord01A1ListViewRef2,height=MinSHeight
	getprop	NORD01A1,height=height
	setprop	Nord01A1ListViewRef1,height=height
	setprop	Nord01A1ListViewRef2,height=height
.END PATCH 3.72 ADDED LOGIC
...................
	setprop	Nord001AComboOffer,enabled=0,bgcolor=grey
	setprop	Nord001AComboStatus,enabled=0,bgcolor=grey
	setprop	Nord001AComboPending,enabled=0,bgcolor=grey
	setprop	Nord001AButtonPending,visible=0
	setprop	Nord001AEditBrk,enabled=0,bgcolor=grey
	setprop	Nord001AEditBrkContact,enabled=0,bgcolor=grey
	setprop	Nord001AEditCampaign,enabled=0,bgcolor=grey
.	 setprop Order1EditCutOffDate,enabled=0,bgcolor=grey
	setprop	Nord001AEditExchangeQty,enabled=0,bgcolor=grey
	setprop	Nord001AEditExchangePrice,enabled=0,bgcolor=grey
	setprop	Nord001AEditList,enabled=0,bgcolor=grey
.START PATCH 3.72 REPLACED LOGIC
.	setprop	Nord001AEditListUniverse,enabled=0,bgcolor=grey
	setprop	Nord001AEditSelUniverse,enabled=0,bgcolor=grey
.END PATCH 3.72 REPLACED LOGIC
	setprop	Nord001AEditListSel,enabled=0,bgcolor=grey
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord001AButtonArrow,enabled=0
	setprop	NORD001A2,visible=0
	move	C0,SrchSelFlag
	setprop	Nord001AEditListSelPrice,enabled=0,bgcolor=grey
	setprop	Nord001AEditListSelCharge,enabled=0,bgcolor=grey
	setprop	Nord001AComboListSelMod,enabled=0,bgcolor=grey
.END PATCH 3.72 ADDED LOGIC
	setprop	Nord001AEditMailDate,enabled=0,bgcolor=grey
	setprop	Nord001AEditMlr,enabled=0,bgcolor=grey
	setprop	Nord001AEditMlrContact,enabled=0,bgcolor=grey
	setprop	Nord001AEditOrderDate,enabled=0,bgcolor=grey
	setprop	Nord001AEditOrderQty,enabled=0,bgcolor=grey
	setprop	Nord001AEditOwner,enabled=0,bgcolor=grey
.Start Patch 3.78.8 Added Object
	setprop	Nord001AEditFulfillment,enabled=0,bgcolor=grey
.End Patch 3.78.8
	
	setprop	Nord001AEditPO,enabled=0,bgcolor=grey
	setprop	Nord001AEditPrice,enabled=0,bgcolor=grey
	setprop	Nord001AEditRtn,enabled=0,bgcolor=grey
	setprop	Nord001AEditRtnDate,enabled=0,bgcolor=grey
	setprop	nord001CEditSpecial,readonly=1,bgcolor=grey
	setprop	nord001CEditSpecial1,enabled=1
	setprop	nord001CEditSpecial2,readonly=1,bgcolor=grey
	setprop	nord001CEditSpecial3,readonly=1,bgcolor=grey
	setprop	Nord001bCheckDirect,enabled=0
	setprop	Nord001bComboBRKGuar,enabled=0,bgcolor=grey
	setprop	Nord001bComboCaller,enabled=0,bgcolor=grey
	setprop	Nord001bComboCont,enabled=0,bgcolor=grey
	setprop	Nord001bComboContact,enabled=0,bgcolor=grey
	setprop	Nord001bComboMedia,enabled=0,bgcolor=grey
	setprop	Nord001bComboNINGuar,enabled=0,bgcolor=grey
	setprop	Nord001bComboNet,enabled=0,bgcolor=grey
	setprop	Nord001bComboSam,enabled=0,bgcolor=grey
.START PATCH 3.72.2 REPLACED LOGIC
.	setprop	Nord001bComboSample,enabled=0,bgcolor=grey
	setprop	Nord001bEditSample,enabled=0,bgcolor=grey
	setprop	Nord001bListViewSamples,enabled=0,bgcolor=grey
	setprop	Nord001bListViewSamples,height=0
	setprop	Nord001bButtonArrow,enabled=0
.END PATCH 3.72.2 REPLACED LOGIC
	setprop	Nord001bComboShip,enabled=0,bgcolor=grey
	setprop	Nord001bComboTape,enabled=0,bgcolor=grey
	setprop	Nord001bComboTestCode,enabled=0,bgcolor=grey
	setprop	Nord001bComboZipScreen,enabled=0,bgcolor=grey
	setprop	Nord001bEditAttn,enabled=0,bgcolor=grey
.START PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditFax,enabled=0,bgcolor=grey
.END PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditContDate,enabled=0,bgcolor=grey
	setprop	Nord001bEditContLR,enabled=0,bgcolor=grey
	setprop	Nord001bEditContQty,enabled=0,bgcolor=grey
	setprop	Nord001bEditKeyInfo,enabled=0,bgcolor=grey
	setprop	Nord001bEditNetPer,enabled=0,bgcolor=grey
	setprop	Nord001bEditNetQty,enabled=0,bgcolor=grey
	setprop	Nord001bEditSales,enabled=0,bgcolor=grey
	setprop	Nord001bVScrollSales,enabled=0
	setprop	Nord001bButtonExchange,enabled=0
	setprop	Nord001bButtonRent,enabled=0
	setprop	Nord001bButtonExcRent,enabled=0
	setprop	NORDMSK1ButtonModify,enabled=1
	setprop	NORDMSK1ButtonCancel,enabled=0
	setprop	NORDMSK1ButtonSave,enabled=0
	setprop	NORDMSK1ButtonQuit,enabled=0
	setprop	NORDMSK1ButtonHotPrint,enabled=0
.START PATCH 3.68.5 REPLACED LOGIC
..	 TRAP	 IOMssg	Giving Error if	IO
.	TRAP	IOMssg2	Giving Error if	IO
.	move	"O.Dis.Low.-NINPRINT",Location
.	pack	KeyLocation,"Key: ",OLRN
.	read	ORDPRINT,OLRN;;
.	if over
.		setprop	NORDMSK1ButtonReprint,enabled=1
.	else
.		setprop	NORDMSK1ButtonReprint,enabled=0
.	endif
.	TRAPCLR	IO
	call	Trim using OLRN
	if (OLRN = "")
		setprop	NORDMSK1ButtonReprint,enabled=0
	else
		TRAP	IOMssg2	Giving Error if	IO
		move	"O.Dis.Low.-NINPRINT",Location
		pack	KeyLocation,"Key: ",OLRN
		read	ORDPRINT,OLRN;;
		if over
			setprop	NORDMSK1ButtonReprint,enabled=1
		else
			setprop	NORDMSK1ButtonReprint,enabled=0
		endif
		TRAPCLR	IO
	endif
.END PATCH 3.68.5 REPLACED LOGIC
.Clear tab of Asterisks	to indicate processing is over
	getitem	nord0001TabControlTop,1,str45
	pack	str2,STAR,B1
	rep	str2,str45
	call	Trim using str45
	setitem	nord0001TabControlTop,1,str45
	setprop	Nord001bEditLCRHist,height=0
	setprop	Nord001bEditLCRInit,height=0
	setprop	Nord001bEditLCRStat,height=0
	setprop	Nord001bEditLCRDate,height=0
OrderDisableNet
.Subset, called	by:  LostFocus_Nord001bComboNet,Click_NORDMSK1ButtonModify,OrderSetPendingDisable
	setprop	Nord001bEditNetCharge,enabled=0,bgcolor=grey
	setprop	Nord001bEditNetMin,enabled=0,bgcolor=grey
	setprop	Nord001bEditNetPercent,enabled=0,bgcolor=grey
	return

IOMssg2
	scan	"I44",Error
	if equal
		create	ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
		activate EditTextBoxes(1)
		clear	ErrMssg
		append	"ERROR = ",ErrMssg
		append	ERROR,ErrMssg
		reset	ErrMssg
		setprop	ErrorMssgStat1,visible=1
		setprop	ErrorMssgStat2,visible=1
		setprop	ErrorMssgStat3,visible=1
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
		setitem	ErrorMssgStat1,0,"* I44	Error !! *"
		setitem	ErrorMssgStat2,0,"Please Leave this Information	on"
		setitem	ErrorMssgStat5,0,"    Screen and Inform	I.S.!"
		setitem	ErrorMssgStat3,0,ErrMssg
		setitem	ErrorMssgOK,0,"&Stop"
		loop
			setfocus EditTextBoxes(1)
			setprop	ErrorMssg,visible=1
			getitem	EditTextBoxes(1),0,str1
			until (str1 = "*")
		repeat
		destroy	EditTextBoxes(1)
	endif
	goto	IOMssg

OrderDisableNINGuar
. Called by:  SearchLoad2,Click_NORDMSK1ButtonModify,LostFocus_Nord001AEditList
.List must be EXCLUSIVE, and GUARCODE must be:	30 day,	45 day,	60 day or no date.
	getitem	Nord001bComboNINGuar,0,N2
	if (((N2 > 1) AND (N2 <	5)) AND	(ELSTCDE = "C"))
		setprop	Nord001bComboNINGuar,enabled=0,bgcolor=grey
	else
		setprop	Nord001bComboNINGuar,enabled=1,bgcolor=white
	endif
	return

OrderDisableOrderAll4
.called	by:  OrderLoadNotes
	setprop	Nord001GButtonCancel,enabled=0
	setprop	Nord001GButtonSave,enabled=0
	setprop	Nord001GButtonNew,enabled=1
OrderDisableOrder4
.called	by:  Nord001GButtonCancel_Click, Nord001GButtonSave_Click
.START PATCH	3.78.3	REPLACED LOGIC
.	setprop	Nord001GEditNote1,enabled=0
.	setprop	Nord001GEditNote2,enabled=0
.	setprop	Nord001GEditNote3,enabled=0
.	setprop	Nord001GEditNote4,enabled=0
.	setprop	Nord001GEditNote5,enabled=0
.	setprop	Nord001GEditNote6,enabled=0
	setprop Nord001GEditNote, enabled=1  // enable it so can use scroll bar
	setprop	Nord001GEditNote, readonly=C1  // but don't let user change field!
.END PATCH	3.78.3	REPLACED LOGIC
	return

OrderEnableLower
.Called	by:  Click_NORDMSK1ButtonModify,Click_NORDMSK1ButtonNew
	move	"N",ExitFlag
	setprop	NORDMSK2ButtonModify,enabled=0
	setprop	NORDMSK3ButtonModify,enabled=0
.
	setprop	NORDMSK3ButtonOrder,enabled=0
	setprop	NORDMSK3ButtonLCR,enabled=0
.START PATCH 3.48 ADDED LOGIC
	setprop	NORDMSK3ButtonProj,enabled=0
.END PATCH 3.48 ADDED LOGIC
	setprop	Nord001ACheckEntire,enabled=1
	setprop	Nord001ACheckExchange,enabled=1
	if (OSTAT = "l"	OR OSTAT = "z" OR OSTAT	= "p" OR OSTAT = "x") .LCR/Pending
		setprop	Nord001ACheckRent,enabled=1
	endif
	setprop	Nord001ACheckTest,enabled=1
	setprop	Nord001ACheckRetest,enabled=1
	setprop	Nord001ACheckMode,enabled=1
	setprop	Nord001AComboOffer,enabled=1,bgcolor=white
	setprop	Nord001AComboStatus,bgcolor=white		.Unable	to actually modify values
	setprop	Nord001AComboPending,bgcolor=white	.Unable	to actually modify values
	setprop	Nord001AEditBrk,enabled=1,bgcolor=white
	setprop	Nord001AEditBrkContact,enabled=1,bgcolor=white
.Disable Nord001AEditExchangePrice,Nord001AEditExchangeQty if not an Exchange Order
	setprop	Nord001AEditCampaign,enabled=1,bgcolor=white
.	 setprop Order1EditCutOffDate,enabled=1,bgcolor=white
	getitem	Nord001ACheckExchange,0,result
	if (result = 1)
		setprop	Nord001AEditExchangeQty,enabled=1,bgcolor=white
		setprop	Nord001AEditExchangePrice,enabled=1,bgcolor=white
	endif
	setprop	Nord001AEditList,enabled=1,bgcolor=white
.START PATCH 3.72 REPLACED LOGIC
.	setprop	Nord001AEditListUniverse,enabled=1,bgcolor=white
	setprop	Nord001AEditSelUniverse,enabled=1,bgcolor=white
.END PATCH 3.72 REPLACED LOGIC
	setprop	Nord001AEditListSel,enabled=1,bgcolor=white
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord001AButtonArrow,enabled=1
	setprop	Nord001AEditListSelPrice,enabled=1,bgcolor=white
	setprop	Nord001AEditListSelCharge,enabled=1,bgcolor=white
	setprop	Nord001AComboListSelMod,enabled=1,bgcolor=white
.END PATCH 3.72 ADDED LOGIC
     	setprop	Nord001AEditMailDate,enabled=1,bgcolor=white
	setprop	Nord001AEditMlr,enabled=1,bgcolor=white
	setprop	Nord001AEditMlrContact,enabled=1,bgcolor=white
.ORDER DATE SHOULD NEVER BE MODIFIABLE!!!!!!
	setprop	Nord001AEditOrderDate,enabled=0,bgcolor=white
	setprop	Nord001AEditOrderQty,enabled=1,bgcolor=white
	setprop	Nord001AEditOwner,enabled=1,bgcolor=white
.Start patch 3.78.8
	setprop	Nord001AEditFulfillment,enabled=1,bgcolor=white
.End Patch 3.78.8 	
	setprop	Nord001AEditPO,enabled=1,bgcolor=white
.START PATCH 3.72 REMOVED LOGIC
.	setprop	Nord001AEditPrice,enabled=1,bgcolor=white
.END PATCH 3.72 REMOVED LOGIC
	setprop	Nord001AEditRtn,enabled=1,bgcolor=white
	setprop	Nord001AEditRtnDate,enabled=1,bgcolor=white
	setprop	Nord001AEditRtnDate,fgcolor=BLACK
.Approve button	visible	only when record is not	being modified
.	 setprop NORDMSK1ButtonApprove,visible=0
	setprop	NORDMSK1ButtonApprove,height=0
	setprop	nord001CEditSpecial,readonly=0,bgcolor=white
	setprop	nord001CEditSpecial1,enabled=0
	setprop	nord001CEditSpecial2,readonly=0,bgcolor=white
	setprop	nord001CEditSpecial3,readonly=0,bgcolor=white
.Buttons used for XSTAT
	setprop	nord001CButtonUpdate,visible=0
	setprop	nord001CButtonQuit,visible=0
.START PATCH REMOVED 02/04/2002
.	 setprop Nord001bCheckDirect,enabled=1
.END PATCH REMOVED 02/04/2002
	setprop	Nord001bComboBRKGuar,enabled=1,bgcolor=white
	setprop	Nord001bComboCaller,enabled=1,bgcolor=white
	setprop	Nord001bComboCont,enabled=1,bgcolor=white
	setprop	Nord001bComboContact,enabled=1,bgcolor=white
	setprop	Nord001bComboMedia,enabled=1,bgcolor=white
	setprop	Nord001bComboNINGuar,enabled=1,bgcolor=white
	setprop	Nord001bComboNet,enabled=1,bgcolor=white
	setprop	Nord001bComboSam,enabled=1,bgcolor=white
.This condition	taken out to see if this will clear up some problems
.	 if (OSCODE = "1")
.START PATCH 3.72.2 REPLACED LOGIC
.		setprop	Nord001bComboSample,enabled=1,bgcolor=white
		setprop	Nord001bEditSample,enabled=1,bgcolor=white
		setprop	Nord001bListViewSamples,enabled=1,bgcolor=white
		setprop	Nord001bButtonArrow,enabled=1
.END PATCH 3.72.2 REPLACED LOGIC
.	 endif
.Nord001bComboSample & Nord001bComboTape enabled by	LostFocus events of Nord001bComboSam & Nord001bComboMedia
	setprop	Nord001bComboShip,enabled=1,bgcolor=white
	setprop	Nord001bComboTestCode,enabled=1,bgcolor=white
	setprop	Nord001bComboZipScreen,enabled=1,bgcolor=white
	setprop	Nord001bEditAttn,enabled=1,bgcolor=white
.START PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditFax,enabled=1,bgcolor=white
.END PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditContDate,enabled=1,bgcolor=white
	setprop	Nord001bEditContLR,enabled=1,bgcolor=white
	setprop	Nord001bEditContQty,enabled=1,bgcolor=white
	setprop	Nord001bEditKeyInfo,enabled=1,bgcolor=white
	setprop	Nord001bEditNetCharge,enabled=1,bgcolor=white
	setprop	Nord001bEditNetMin,enabled=1,bgcolor=white
	setprop	Nord001bEditNetPercent,enabled=1,bgcolor=white
	setprop	Nord001bEditNetPer,enabled=1,bgcolor=white
	setprop	Nord001bEditNetQty,enabled=1,bgcolor=white
	setprop	Nord001bEditSales,enabled=1,bgcolor=white
	setprop	Nord001bVScrollSales,enabled=1
	if (NewFlag <> YES)
		setprop	NORDMSK1ButtonModify,enabled=0
.Hide button if	Cancelled, Cancelled/Billed, Cancelled LCR
.For Cancelled & Cancelled/Billed - there is no	need to	cancel
.an order which	has already been cancelled.
		if (OSTAT = "X"	OR OSTAT = "Q" OR OSTAT	= "z" OR OSTAT = "x")
			setprop	NORDMSK1ButtonCancel,enabled=0
		else
			setprop	NORDMSK1ButtonCancel,enabled=1
		endif
		setprop	NORDMSK1ButtonHotPrint,enabled=1
		if (OSTAT = "l"	& OHIST	<> "E" & OHIST <> "z" &	(OCO2CODE = "" | OCO2CODE = "  "))
			setprop	Nord001bButtonExchange,enabled=1
			setprop	Nord001bButtonRent,enabled=1
			setprop	Nord001bButtonExcRent,enabled=1
		endif
	endif
	setprop	NORDMSK1ButtonSave,enabled=1
	setprop	NORDMSK1ButtonQuit,enabled=1
	setprop	NORDMSK1ButtonReprint,enabled=0
.Mark tab with Asterisks to indicate processing	is occuring
	getitem	nord0001TabControlTop,1,str45
	pack	str55,STAR,str45,STAR
	setitem	nord0001TabControlTop,1,str55
OrderEnableNet
.Subset, called	by:  LostFocus_Nord001bComboNet,"VERIFY NET",
	setprop	Nord001bEditNetCharge,enabled=1,bgcolor=white
	setprop	Nord001bEditNetMin,enabled=1,bgcolor=white
	setprop	Nord001bEditNetPercent,enabled=1,bgcolor=white
	return

OrderEnableLowerSecurity
.called	by SecurityGo -	used to	enable ALL fields for modification
	setprop	Nord001AEditOrderDate,enabled=1,bgcolor=white
	setprop	Nord001AComboStatus,enabled=1,bgcolor=white
	setprop	Nord001AComboPending,enabled=1,bgcolor=white
	setprop	Nord001bEditNetCharge,enabled=1,bgcolor=white
	setprop	Nord001bEditNetMin,enabled=1,bgcolor=white
	setprop	Nord001bEditNetPercent,enabled=1,bgcolor=white
	setprop	Nord001AEditExchangeQty,enabled=1,bgcolor=white
	setprop	Nord001AEditExchangePrice,enabled=1,bgcolor=white
	setprop	Nord001bComboSam,enabled=1,bgcolor=white
.START PATCH 3.72.2 REPLACED LOGIC
.	setprop	Nord001bComboSample,enabled=1,bgcolor=white
	setprop	Nord001bEditSample,enabled=1,bgcolor=white
	setprop	Nord001bListViewSamples,enabled=1,bgcolor=white
	setprop	Nord001bButtonArrow,enabled=1
.END PATCH 3.72.2 REPLACED LOGIC
	setprop	Nord001bComboTape,enabled=1,bgcolor=white
	setprop	Nord001bComboNINGuar,enabled=1,bgcolor=white
	setprop	Nord001bEditLCRHist,height=20
	setprop	Nord001bEditLCRInit,height=20
	setprop	Nord001bEditLCRStat,height=20
	setprop	Nord001bEditLCRDate,height=20
	setitem	Nord001bEditLCRHist,0,OHIST
	setitem	Nord001bEditLCRInit,0,OCLRINIT
	setitem	Nord001bEditLCRStat,0,OCLRSTAT
	pack	str10,OCLRDTEM,OCLRDTED,OCLRDTEC,OCLRDTEY
	setitem	Nord001bEditLCRDate,0,str10
	return
OrderEnableOrder4
.Called	by Nord001GButtonNew_Click
.START PATCH 3.78.3 REPLACED LOGIC
.	setprop	Nord001GEditNote1,enabled=1
.	setprop	Nord001GEditNote2,enabled=1
.	setprop	Nord001GEditNote3,enabled=1
.	setprop	Nord001GEditNote4,enabled=1
.	setprop	Nord001GEditNote5,enabled=1
.	setprop	Nord001GEditNote6,enabled=1
	setprop Nord001GEditNote,enabled=1
	setprop Nord001GEditNote,readonly=C0  // let them edit field
.END PATCH 3.78.3 REPLACED LOGIC
	return

OrderDisableSave
	setprop	NORDMSK1ButtonSave,enabled=0
	return
OrderEnableSave
	setprop	NORDMSK1ButtonSave,enabled=1
	return

OrderDisableSaveB
	setitem	NORDMSK1ButtonSave,0,"Save"
	return
OrderEnableSaveB
	setitem	NORDMSK1ButtonSave,0,"&Save"
	return

.Campaign File Maintenance Screens
OrderDisableCampLower
.Called	by:  Click_NORDMSK2ButtonCancel,Click_NORDMSK2ButtonFind,Click_NORDMSK2ButtonQuit,Click_NORDMSK2ButtonSave
	setprop	Nord01eaCheckBillDirect,enabled=0
	setprop	Nord01eaComboStatus,enabled=0,bgcolor=grey
	setprop	Nord01eaComboContact,enabled=0,bgcolor=grey
	setprop	Nord01eaComboMedia,enabled=0,bgcolor=grey
	setprop	Nord01eaComboOffer,enabled=0,bgcolor=grey
.	 setprop Nord01eaComboPackage,enabled=0,bgcolor=grey
	setprop	Nord01eaComboPlanner,enabled=0,bgcolor=grey
	setprop	Nord01eaComboReport,enabled=0,bgcolor=grey
	setprop	Nord01eaComboSample,enabled=0,bgcolor=grey
	setprop	Nord01eaComboShip,enabled=0,bgcolor=grey
	setprop	Nord01eaEditBrk,enabled=0,bgcolor=grey
	setprop	Nord01eaEditBrkContact,enabled=0,bgcolor=grey
	setprop	Nord01eaEditCampDate,bgcolor=grey
	setprop	Nord01eaEditCampName,enabled=0,bgcolor=grey
	setprop	Nord01eaEditCutOffDate,enabled=0,bgcolor=grey
	setprop	Nord01eaEditGiftChange,enabled=0,bgcolor=grey
	setprop	Nord01eaEditGrossQty,enabled=0,bgcolor=grey
	setprop	Nord01eaEditKey,enabled=0,bgcolor=grey
	setprop	Nord01eaEditMailDate,enabled=0,bgcolor=grey
	setprop	Nord01eaEditMlr,enabled=0,bgcolor=grey
	setprop	Nord01eaEditNetQty,enabled=0,bgcolor=grey
	setprop	Nord01eaEditPO,enabled=0,bgcolor=grey
	setprop	Nord01eaEditRespChange,enabled=0,bgcolor=grey
	setprop	Nord01eaEditRtnDate,enabled=0,bgcolor=grey
	setprop	Nord01eaEditRtn,enabled=0,bgcolor=grey
	setprop	Nord01eaEditSpecial,readonly=1,bgcolor=grey
	setprop	NORDMSK2ButtonModify,enabled=1
	setprop	NORDMSK2ButtonCancel,enabled=0
	setprop	NORDMSK2ButtonSave,enabled=0
	setprop	NORDMSK2ButtonQuit,enabled=0
.Clear tab of Asterisks	to indicate processing is over
	getitem	nord0001TabControlTop,6,str45
	pack	str2,STAR,B1
	rep	str2,str45
	call	Trim using str45
	setitem	nord0001TabControlTop,6,str45
	return

OrderEnableCampLower
.Called	by:  Click_NORDMSK2ButtonModify,Click_NORDMSK2ButtonNew
	move	"N",ExitFlag2
	setprop	NORDMSK1ButtonModify,enabled=0
	setprop	NORDMSK3ButtonModify,enabled=0
.
	setprop	NORDMSK3ButtonOrder,enabled=0
	setprop	NORDMSK3ButtonLCR,enabled=0
.START PATCH 3.48 ADDED LOGIC
	setprop	NORDMSK3ButtonProj,enabled=0
.END PATCH 3.48 ADDED LOGIC
.	 setprop Nord01eaCheckBillDirect,enabled=1
	setprop	Nord01eaComboStatus,enabled=1,bgcolor=white
	setprop	Nord01eaComboContact,enabled=1,bgcolor=white
	setprop	Nord01eaComboMedia,enabled=1,bgcolor=white
	setprop	Nord01eaComboOffer,enabled=1,bgcolor=white
.	 setprop Nord01eaComboPackage,enabled=1,bgcolor=white
	setprop	Nord01eaComboPlanner,enabled=1,bgcolor=white
	setprop	Nord01eaComboReport,enabled=1,bgcolor=white
	setprop	Nord01eaComboSample,enabled=1,bgcolor=white
	setprop	Nord01eaComboShip,enabled=1,bgcolor=white
	setprop	Nord01eaEditBrk,enabled=1,bgcolor=white
	setprop	Nord01eaEditBrkContact,enabled=1,bgcolor=white
	setprop	Nord01eaEditCampDate,bgcolor=white
	setprop	Nord01eaEditCampName,enabled=1,bgcolor=white
	setprop	Nord01eaEditCutOffDate,enabled=1,bgcolor=white
	setprop	Nord01eaEditGiftChange,enabled=1,bgcolor=white
	setprop	Nord01eaEditGrossQty,enabled=1,bgcolor=white
	setprop	Nord01eaEditKey,enabled=1,bgcolor=white
	setprop	Nord01eaEditMailDate,enabled=1,bgcolor=white
	setprop	Nord01eaEditMlr,enabled=1,bgcolor=white
	setprop	Nord01eaEditNetQty,enabled=1,bgcolor=white
	setprop	Nord01eaEditPO,enabled=1,bgcolor=white
	setprop	Nord01eaEditRespChange,enabled=1,bgcolor=white
	setprop	Nord01eaEditRtnDate,enabled=1,bgcolor=white
	setprop	Nord01eaEditRtn,enabled=1,bgcolor=white
	setprop	Nord01eaEditSpecial,readonly=0,bgcolor=white
	setprop	NORDMSK2ButtonSave,enabled=1
	setprop	NORDMSK2ButtonQuit,enabled=1
.Mark tab with Asterisks to indicate processing	is occuring
	getitem	nord0001TabControlTop,6,str45
	pack	str55,STAR,str45,STAR
	setitem	nord0001TabControlTop,6,str55
	return

OrderEnableCampLowerSecurity
.called	by SecurityGo -	used to	enable ALL fields for modification
	return

OrderDisableSave3
	setitem	NORDMSK3ButtonSave,0,"Save"
	return
OrderEnableSave3
	setitem	NORDMSK3ButtonSave,0,"&Save"
	return

OrderDisableLOLLower
	setprop	Nord01ECCheckExchange,enabled=0
	setprop	Nord01ECCheckRegional,enabled=0
	setprop	Nord01ECCheckRent,enabled=0
	setprop	Nord01ECCheckReTest,enabled=0
	setprop	Nord01ECCheckTest,enabled=0
	setprop	Nord01ECComboContact,enabled=0,bgcolor=grey
	setprop	Nord01ECComboOffer,enabled=0,bgcolor=grey
.TESTING
.	 setprop Nord01ECComboPackage,enabled=0,bgcolor=grey
	setprop	Nord01ECComboPlanner,enabled=0,bgcolor=grey
	setprop	Nord01ECComboSample,enabled=0,bgcolor=grey
	setprop	Nord01ECComboStatus,enabled=0,bgcolor=grey
	setprop	Nord01ECEditBrk,enabled=0,bgcolor=grey
	setprop	Nord01ECEditBrkContact,enabled=0,bgcolor=grey
	setprop	Nord01ECEditCamp,enabled=0,bgcolor=grey
	setprop	Nord01ECEditLR,enabled=0,bgcolor=grey
	setprop	Nord01ECEditLOLDate,enabled=0,bgcolor=grey
.TESTING
.	 setprop Nord01ECEditGift,enabled=0,bgcolor=grey
	setprop	Nord01ECEditGrossQty,enabled=0,bgcolor=grey
	setprop	Nord01ECEditList,enabled=0,bgcolor=grey
	setprop	Nord01ECEditListSel,enabled=0,bgcolor=grey
	setprop	Nord01ECEditMailDate,enabled=0,bgcolor=grey
	setprop	Nord01ECEditMlr,enabled=0,bgcolor=grey
	setprop	Nord01ECEditNetQty,enabled=0,bgcolor=grey
	setprop	Nord01ECEditNetPer,enabled=0,bgcolor=grey
	setprop	Nord01ECEditOwner,enabled=0,bgcolor=grey
	setprop	Nord01ECEditPO,enabled=0,bgcolor=grey
.TESTING
.	 setprop Nord01ECEditResp,enabled=0,bgcolor=grey
	setprop	Nord01ECEditRtn,enabled=0,bgcolor=grey
	setprop	Nord01ECEditSpecial,readonly=1,bgcolor=grey
	setprop	Nord01ECEditSpecial1,readonly=1,bgcolor=grey
	setprop	Nord01ECEditUniverse,enabled=0,bgcolor=grey
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord01ECListViewSelect,height=0
	setprop	Nord01ECEditPrice,enabled=0,bgcolor=grey
	setprop	Nord01ECEditSelPrice,enabled=0,bgcolor=grey
	setprop	Nord01ECComboMod,enabled=0,bgcolor=grey
	setprop	Nord01ECButtonCalcNet,top=227
	setprop	Nord01ECButtonViewNet,top=227
	setprop	Nord01ECButtonArrow,enabled=0
.In case we want to force the Minimum Height on these items
.	setprop	Nord08A1ListViewRef1,height=MinSHeight
.	setprop	Nord08A1ListViewRef2,height=MinSHeight
	getprop	NORD08A1,height=height
	setprop	Nord08A1ListViewRef1,height=height
	setprop	Nord08A1ListViewRef2,height=height
.END PATCH 3.72 ADDED LOGIC
	setprop	Nord01ECButtonCalcNet,height=0
	setprop	Nord01ECButtonViewNet,height=0
	setprop	NORDMSK3ButtonModify,enabled=1
	setprop	NORDMSK3ButtonCopy,enabled=1
	setprop	NORDMSK3ButtonCancel,enabled=0
	setprop	NORDMSK3ButtonSave,enabled=0
	setprop	NORDMSK3ButtonQuit,enabled=0
	setprop	NORDMSK3ButtonDelete,enabled=0
.
	setprop	NORDMSK3ButtonOrder,enabled=1
	setprop	NORDMSK3ButtonLCR,enabled=1
.START PATCH 3.48 ADDED LOGIC
	setprop	NORDMSK3ButtonProj,enabled=1
.END PATCH 3.48 ADDED LOGIC
.	 setprop NORDMSK3ButtonOrder,enabled=0
.	 setprop NORDMSK3ButtonLCR,enabled=0
.Clear tab of Asterisks	to indicate processing is over
	getitem	nord0001TabControlTop,8,str45
	pack	str2,STAR,B1
	rep	str2,str45
	call	Trim using str45
	setitem	nord0001TabControlTop,8,str45
	return

OrderEnableLOLLower
	move	"N",ExitFlag3
	setprop	NORDMSK1ButtonModify,enabled=0
	setprop	NORDMSK2ButtonModify,enabled=0
.
	setprop	NORDMSK3ButtonOrder,enabled=0
	setprop	NORDMSK3ButtonLCR,enabled=0
.START PATCH 3.48 ADDED LOGIC
	setprop	NORDMSK3ButtonProj,enabled=0
.END PATCH 3.48 ADDED LOGIC
	setprop	Nord01ECCheckExchange,enabled=1
	setprop	Nord01ECCheckRegional,enabled=1
	setprop	Nord01ECCheckRent,enabled=1
	setprop	Nord01ECCheckReTest,enabled=1
	setprop	Nord01ECCheckTest,enabled=1
	setprop	Nord01ECComboOffer,enabled=1,bgcolor=white
.TESTING
.	 setprop Nord01ECComboPackage,enabled=1,bgcolor=white
	setprop	Nord01ECComboSample,enabled=1,bgcolor=white
	setprop	Nord01ECComboStatus,enabled=1,bgcolor=white
	setprop	Nord01ECEditCamp,enabled=1,bgcolor=white
	if (SecFlag3 = YES)
		setprop	Nord01ECEditLOLDate,enabled=1,bgcolor=white
	else
		setprop	Nord01ECEditLOLDate,bgcolor=white
	endif
.TESTING
.	 setprop Nord01ECEditGift,enabled=1,bgcolor=white
	setprop	Nord01ECEditGrossQty,enabled=1,bgcolor=white
	setprop	Nord01ECEditList,enabled=1,bgcolor=white
	setprop	Nord01ECEditListSel,enabled=1,bgcolor=white
	setprop	Nord01ECEditMailDate,enabled=1,bgcolor=white
	setprop	Nord01ECEditNetQty,enabled=1,bgcolor=white
	setprop	Nord01ECEditNetPer,enabled=1,bgcolor=white
	setprop	Nord01ECEditOwner,enabled=1,bgcolor=white
.TESTING
.	 setprop Nord01ECEditResp,enabled=1,bgcolor=white
	setprop	Nord01ECEditSpecial,readonly=0,bgcolor=white
	setprop	Nord01ECEditSpecial1,readonly=0,bgcolor=white
	setprop	Nord01ECEditUniverse,enabled=1,bgcolor=white
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord01ECEditPrice,enabled=1,bgcolor=white
	setprop	Nord01ECEditSelPrice,enabled=1,bgcolor=white
	setprop	Nord01ECComboMod,enabled=1,bgcolor=white
	setprop	Nord01ECButtonArrow,enabled=1
.END PATCH 3.72 ADDED LOGIC
	setprop	Nord01ECButtonCalcNet,height=15
	setprop	Nord01ECButtonViewNet,height=15
	setprop	NORDMSK3ButtonSave,enabled=1
	setprop	NORDMSK3ButtonQuit,enabled=1
.Mark tab with Asterisks to indicate processing	is occuring
	getitem	nord0001TabControlTop,8,str45
	pack	str55,STAR,str45,STAR
	setitem	nord0001TabControlTop,8,str55
	return

OrderEnableLOLLowerSearch
	setprop	Nord01ECComboContact,enabled=1,bgcolor=white
	setprop	Nord01ECComboPlanner,enabled=1,bgcolor=white
	setprop	Nord01ECEditMlr,enabled=1,bgcolor=white
	setprop	Nord01ECEditPO,enabled=1,bgcolor=white
	setprop	Nord01ECEditBrk,enabled=1,bgcolor=white
	setprop	Nord01ECEditBrkContact,enabled=1,bgcolor=white
	setprop	Nord01ECEditRtn,enabled=1,bgcolor=white
	return

OrderStatDisableLower
	setprop	NSTA001ACheckLOL2,enabled=0
	setprop	NSTA001BCheckLOL2B,enabled=0
	setprop	NSTA001AEditLR2,enabled=0,bgcolor=grey,readonly=1
	setprop	NSTA001AEditStatNum2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditListCostM2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditMailCost2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditMailQty2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditPackNum2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditPackageCost2,enabled=0,bgcolor=grey
.	setprop	STATSEditPackageCostThou2,enabled=0,bgcolor=grey
	setprop	NSTA001BListViewPackage,bgcolor=grey
	setprop	NSTA001AEditRecoQty2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditAvgNet2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditRespRate2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditAvgGift2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditLValue2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditNetReq2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditNetRec2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditExBase2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditRentBase2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditRunCharge2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditSelectFee2,enabled=0,bgcolor=grey
	setprop	NSTA001BEditShipTape2,enabled=0,bgcolor=grey
.START PATCH 3.45 ADDED LOGIC
	setprop	NSTA001AEditSelect2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditLRQty2,enabled=0,bgcolor=grey
.END PATCH 3.45 ADDED LOGIC
.START PATCH 3.47 ADDED LOGIC
	setprop	NSTA001AEditSelUniverse2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditSelPrice2,enabled=0,bgcolor=grey
	setprop	NSTA001AEditSelPrice3,enabled=0,bgcolor=grey
.END PATCH 3.47 ADDED LOGIC
.START PATCH 3.43 ADDED LOGIC
	setprop	NSTA001AEditType2B,enabled=0,bgcolor=grey
.END PATCH 3.43 ADDED LOGIC
	setprop	NSTA001CEditNotes2,readonly=1
.START PATCH 3.46 ADDED LOGIC
	setprop	NSTA001CEditNotes3,readonly=1
	setprop	NSTA001CEditNotes4,readonly=1
	setprop	NSTA001CEditNotes5,readonly=1
.END PATCH 3.46 ADDED LOGIC
	setprop	NSTA001BButtonCalcList,height=0
.START PATCH 3.72 ADDED LOGIC
	setprop	NSTA001AListViewSelect,height=0
	setprop	NSTA001AButtonArrow,enabled=0
.END PATCH 3.72 ADDED LOGIC
.Mark tab with Asterisks to indicate processing	is occuring
	getitem	nord0001TabControlTop,10,str45
	call	RemoveChar using str45,STAR
	setitem	nord0001TabControlTop,10,str45
	return

OrderStatEnableLowerA
	setprop	NSTA001ACheckLOL2,enabled=1
OrderStatEnableLower
	move	"N",ExitFlag5
	setprop	NSTA001AEditLR2,enabled=1,bgcolor=white
.	setprop	NSTA001AEditStatNum2,enabled=1,bgcolor=white
	setprop	NSTA001BEditListCostM2,enabled=1,bgcolor=white
	setprop	NSTA001BEditMailCost2,enabled=1,bgcolor=white
	setprop	NSTA001AEditMailQty2,enabled=1,bgcolor=white
	setprop	NSTA001AEditPackNum2,enabled=1,bgcolor=white
	setprop	NSTA001BEditPackageCost2,enabled=1,bgcolor=white
.	setprop	STATSEditPackageCostThou2,enabled=1,bgcolor=white
	setprop	NSTA001BListViewPackage,bgcolor=white
	setprop	NSTA001AEditRecoQty2,enabled=1,bgcolor=white
	setprop	NSTA001AEditAvgNet2,enabled=1,bgcolor=white
	setprop	NSTA001AEditRespRate2,enabled=1,bgcolor=white
	setprop	NSTA001AEditAvgGift2,enabled=1,bgcolor=white
	setprop	NSTA001BEditLValue2,enabled=1,bgcolor=white
	setprop	NSTA001AEditNetReq2,enabled=1,bgcolor=white
	setprop	NSTA001BEditExBase2,enabled=1,bgcolor=white
	setprop	NSTA001BEditRentBase2,enabled=1,bgcolor=white
	setprop	NSTA001BEditRunCharge2,enabled=1,bgcolor=white
	setprop	NSTA001BEditSelectFee2,enabled=1,bgcolor=white
	setprop	NSTA001BEditShipTape2,enabled=1,bgcolor=white
.START PATCH 3.43 ADDED LOGIC
	getitem	NSTA001BStatRecInfo2,0,str25
	if (str25 <> "Live Order")
		setprop	NSTA001AEditType2B,enabled=1,bgcolor=white
		setprop	NSTA001AEditNetRec2,enabled=1,bgcolor=white
.START PATCH 3.45 ADDED LOGIC
		setprop	NSTA001AEditSelect2,enabled=1,bgcolor=white
		setprop	NSTA001AEditLRQty2,enabled=1,bgcolor=white
.END PATCH 3.45 ADDED LOGIC
.START PATCH 3.46 ADDED LOGIC
		setprop	NSTA001CEditNotes3,readonly=0
		setprop	NSTA001CEditNotes4,readonly=0
.END PATCH 3.46 ADDED LOGIC
.START PATCH 3.72 ADDED LOGIC
		setprop	NSTA001AButtonArrow,enabled=1
		setprop	NSTA001AEditSelUniverse2,enabled=1,bgcolor=white
		setprop	NSTA001AEditSelPrice2,enabled=1,bgcolor=white
		setprop	NSTA001AEditSelPrice3,enabled=1,bgcolor=white
.END PATCH 3.72 ADDED LOGIC
	endif
.END PATCH 3.43 ADDED LOGIC
.START PATCH 3.47 ADDED LOGIC
.START PATCH 3.72 MOVED ABOVE
.	setprop	NSTA001AEditSelUniverse2,enabled=1,bgcolor=white
.END PATCH 3.72 MOVED ABOVE
.END PATCH 3.47 ADDED LOGIC
	setprop	NSTA001CEditNotes2,readonly=0
	setprop	NSTA001BButtonCalcList,height=20
.Mark tab with Asterisks to indicate processing	is occuring
	getitem	nord0001TabControlTop,10,str45
	pack	str55,STAR,str45,STAR
	setitem	nord0001TabControlTop,10,str55
	return

.START PATCH 3.46 ADDED LOGIC
StatsSetSpecial LRoutine DimPtr
	if (DimPtr = "1")
		setprop	NSTA001CEditNotes5,readonly=1
	else
		getitem	NSTA001BStatRecInfo2,0,str25
		if (str25 <> "Live Order")
			setprop	NSTA001CEditNotes5,readonly=0
			setprop	NSTA001CEditNotes3,readonly=0
			setprop	NSTA001CEditNotes4,readonly=0
		endif
	endif
	move	NO,StatNoteFlag
	move	NO,StatNoteFlag2
	return

StatsSetNewSpecial
	getitem	NSTA001ACheckLOL2,0,N1
	move	N1,str1
	call	StatsSetSpecial using str1
	return
.END PATCH 3.46 ADDED LOGIC
....FILE MAINTENANCE MODES....

.Order File Maintenance	Screens
.Mode/TabID set	- TABID's will need to be reset	if they	are reset on the forms!!!!!!!!
OrderSetOrderMode
.Called	by:  Click_NORDMSK1ButtonModify,Click_Nord001ACheckMode
	setprop	Nord001ACheckEntire,tabid=160
	setprop	Nord001ACheckExchange,tabid=180
	setprop	Nord001ACheckRent,tabid=185
	setprop	Nord001ACheckTest,tabid=170
	setprop	Nord001ACheckRetest,tabid=175
	setprop	Nord001AComboOffer,tabid=130
	setprop	Nord001AEditBrk,tabid=60
	setprop	Nord001AEditBrkContact,tabid=70
	setprop	Nord001AEditExchangePrice,tabid=200
	setprop	Nord001AEditExchangeQty,tabid=190
.Start Patch 3.78.8 Added Fulfillment Object
	setprop	Nord001AEditFulfillment,tabid=113
.End Patch 3.78.8
	setprop	Nord001AEditList,tabid=100
.START PATCH 3.72 REPLACED LOGIC
.	setprop	Nord001AEditListUniverse,tabid=105
	setprop	Nord001AEditSelUniverse,tabid=120
	setprop	Nord001AEditListSelPrice,tabid=125
	setprop	Nord001AEditListSelCharge,tabid=128
	setprop	Nord001AComboListSelMod,tabid=126
.END PATCH 3.72 REPLACED LOGIC
	setprop	Nord001AEditListSel,tabid=115
	setprop	Nord001AEditMailDate,tabid=220
	setprop	Nord001AEditMlr,tabid=40
	setprop	Nord001AEditMlrContact,tabid=50
	setprop	Nord001AEditOrderQty,tabid=140
	setprop	Nord001AEditOwner,tabid=110
	setprop	Nord001AEditPO,tabid=80
	setprop	Nord001AEditPrice,tabid=150
	setprop	Nord001AEditRtn,tabid=90
	setprop	Nord001AEditRtnDate,tabid=210
	setprop	Nord001bCheckDirect,tabid=116
	setprop	Nord001bComboBRKGuar,tabid=170
	setprop	Nord001bComboCaller,tabid=30
	setprop	Nord001bComboCont,tabid=40
	setprop	Nord001bComboContact,tabid=20
	setprop	Nord001bComboMedia,tabid=200
	setprop	Nord001bComboNINGuar,tabid=150
	setprop	Nord001bComboNet,tabid=80
	setprop	Nord001bComboSam,tabid=120
.START PATCH 3.72.2 REPLACED LOGIC
.	setprop	Nord001bComboSample,tabid=130
	setprop	Nord001bEditSample,tabid=130
	setprop	Nord001bButtonArrow,tabid=132
.END PATCH 3.72.2 REPLACED LOGIC
	setprop	Nord001bComboShip,tabid=112
	setprop	Nord001bComboTape,tabid=220
	setprop	Nord001bComboTestCode,tabid=210
	setprop	Nord001bComboZipScreen,tabid=190
	setprop	Nord001bEditAttn,tabid=160
.START PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditFax,tabid=165
.END PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditContDate,tabid=60
	setprop	Nord001bEditContLR,tabid=50
	setprop	Nord001bEditContQty,tabid=70
	setprop	Nord001bEditKeyInfo,tabid=180
	setprop	Nord001bEditNetCharge,tabid=100
	setprop	Nord001bEditNetMin,tabid=110
	setprop	Nord001bEditNetPercent,tabid=90
	setprop	Nord001bEditSales,tabid=10
	return
OrderSetPendingMode
.called	by:  Click_Nord001ACheckMode,LostFocus_Nord001AComboStatus
	setprop	Nord001AEditRtn,tabid=0
.	 setprop Nord001AComboOffer,tabid=0
.START PATCH 3.72 REPLACED LOGIC
.	setprop	Nord001AEditListUniverse,tabid=0
	setprop	Nord001AEditSelUniverse,tabid=0
.END PATCH 3.72 REPLACED LOGIC
	setprop	Nord001AEditListSel,tabid=0
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord001AEditListSelPrice,tabid=0
	setprop	Nord001AEditListSelCharge,tabid=0
	setprop	Nord001AComboListSelMod,tabid=0
.END PATCH 3.72 ADDED LOGIC
	setprop	Nord001AEditPrice,tabid=0
	setprop	Nord001ACheckEntire,tabid=0
	setprop	Nord001ACheckTest,tabid=0
	setprop	Nord001ACheckRetest,tabid=0
	setprop	Nord001ACheckExchange,tabid=0
	setprop	Nord001AEditExchangeQty,tabid=0
	setprop	Nord001AEditExchangePrice,tabid=0
.Currently Pending Orders only use nord001bEditSales & nord001bComboContact	which are the first
.two items.  Do	not really need	to set tabs after that
	return
OrderSetLCRMode
	getitem	Nord001bEditSales,0,str2
    	reset	ListMgmt
	scan	INITS,ListMgmt
	if not equal
		setprop	Nord001AEditBrk,tabid=0
		setprop	Nord001AEditBrkContact,tabid=0
	endif
	setprop	Nord001AEditExchangeQty,tabid=0
	setprop	Nord001AEditExchangePrice,tabid=0
.	 setprop Nord001AEditListSel,tabid=0
	setprop	Nord001AEditPO,tabid=0
	setprop	Nord001AEditPrice,tabid=0
	setprop	Nord001AEditRtn,tabid=0
	setprop	Nord001AEditRtnDate,tabid=0
	setprop	Nord001bCheckDirect,tabid=0
	setprop	Nord001bComboBRKGuar,tabid=0
.	 setprop Nord001bComboCaller,tabid=0
	setprop	Nord001bComboCont,tabid=0
	setprop	Nord001bComboNINGuar,tabid=0
	setprop	Nord001bComboNet,tabid=0
	setprop	Nord001bComboShip,tabid=0
	setprop	Nord001bComboTape,tabid=0
	setprop	Nord001bComboTestCode,tabid=0
	setprop	Nord001bComboZipScreen,tabid=0
	setprop	Nord001bEditAttn,tabid=0
.START PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditFax,tabid=0
.END PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditContDate,tabid=0
	setprop	Nord001bEditContLR,tabid=0
	setprop	Nord001bEditContQty,tabid=0
	setprop	Nord001bEditKeyInfo,tabid=0
	setprop	Nord001bEditNetCharge,tabid=0
	setprop	Nord001bEditNetMin,tabid=0
	setprop	Nord001bEditNetPercent,tabid=0
	return
OrderSetNewLiveMode
	setprop	Nord001AComboOffer,tabid=0
	setprop	Nord001AEditBrk,tabid=0
	setprop	Nord001AEditBrkContact,tabid=0
	setprop	Nord001AEditMailDate,tabid=0
	setprop	Nord001AEditMlr,tabid=0
	setprop	Nord001AEditMlrContact,tabid=0
	setprop	Nord001AEditPO,tabid=0
	setprop	Nord001AEditRtn,tabid=0
	setprop	Nord001AEditRtnDate,tabid=0
	setprop	Nord001bCheckDirect,tabid=0
	setprop	Nord001bComboBRKGuar,tabid=0
	setprop	Nord001bComboCaller,tabid=0
	setprop	Nord001bComboContact,tabid=0
	setprop	Nord001bComboNINGuar,tabid=0
	setprop	Nord001bComboTape,tabid=0
	setprop	Nord001bComboTestCode,tabid=0
	setprop	Nord001bEditAttn,tabid=0
.START PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditFax,tabid=0
.END PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditKeyInfo,tabid=0
.This is logic that Data Entry requested and then discarded
.
.	 getitem Nord001ACheckTest,0,N2
.	 if (N2	= 1)	 .Test will not	allow Cont. fields
.		 setprop Nord001bComboCont,tabid=0
.		 setprop Nord001bEditContDate,tabid=0
.		 setprop Nord001bEditContLR,tabid=0
.		 setprop Nord001bEditContQty,tabid=0
.	 else
.		 setprop Nord001bComboCont,tabid=40
.		 setprop Nord001bEditContDate,tabid=60
.		 setprop Nord001bEditContLR,tabid=50
.		 setprop Nord001bEditContQty,tabid=70
.	 endif
..
.	 getitem Nord001ACheckExchange,0,N2
.	 if (N2	= 1)	 .Exchange will	not allow Net fields
.		 setprop Nord001bComboNet,tabid=0
.		 setprop Nord001bEditNetCharge,tabid=0
.		 setprop Nord001bEditNetMin,tabid=0
.		 setprop Nord001bEditNetPercent,tabid=0
.	 else
.		 setprop Nord001bComboNet,tabid=80
.		 setprop Nord001bEditNetCharge,tabid=100
.		 setprop Nord001bEditNetMin,tabid=110
.		 setprop Nord001bEditNetPercent,tabid=90
.	 endif
	return
OrderSetNewMode
	setprop	Nord001bComboBRKGuar,tabid=0
	setprop	Nord001bComboNINGuar,tabid=0
	setprop	Nord001bComboZipScreen,tabid=0
	setprop	Nord001bComboTestCode,tabid=0
	setprop	Nord001bEditAttn,tabid=0
.START PATCH 3.71 ADDED LOGIC
	setprop	Nord001bEditFax,tabid=0
.END PATCH 3.71 ADDED LOGIC
	return
OrderSetLCRScreen
.Called	by Nord001AComboStatus_LostFocus
	getitem	Nord001AStatPending,0,str3
.Do not	repopulate if already in proper	mode - you erase previous entry!!
	if (str3 = "LCR")
		return
	endif
	call	OrderLoadLCRCombo
	call	OrderSetDefaultRentTest
	return
OrderSetDefaultRentTest
.Called	by Nord001AComboStatus_LostFocus
	setprop	Nord001ACheckRent,enabled=1
	setprop	Nord001ACheckExchange,enabled=1
	return

OrderLoadLCRCombo
.Called	by OrderSetLCRScreen, OrderLoadOStats
	setitem	Nord001AStatPending,0,"LCR"
	deleteitem Nord001AComboPending,0
	insertitem Nord001AComboPending,1,""
	insertitem Nord001AComboPending,2,"1st Request"
	insertitem Nord001AComboPending,3,"2nd Request"
	insertitem Nord001AComboPending,4,"Revised Request"
	insertitem Nord001AComboPending,5,"Approved"
	insertitem Nord001AComboPending,6,"Cancelled"
	insertitem Nord001AComboPending,7,"Pending"
	insertitem Nord001AComboPending,8,"Denied"
.START PATCH 3.77.7 ADDED LOGIC
	insertitem Nord001AComboPending,9,"Pending Internal"
.END PATCH 3.77.7 ADDED LOGIC
	return
OrderSetDefaultScreen
.Called	by Nord001AComboStatus_LostFocus
	call	OrderSetDefaultRentTest
OrderSetDefaultScreen2
	getitem	Nord001AStatPending,0,str7
.Do not	repopulate if already in proper	mode - you erase previous entry!!
	if (str7 = "Pending")
		return
	endif
	call	OrderLoadPendingCombo
	return
.OrderSetDefaultTestRent
..Called by Nord001AComboStatus_LostFocus
.	 setprop Nord001ACheckRent,enabled=0
.	 setitem Nord001ACheckRent,0,0
.	 return
OrderLoadPendingCombo
.Called	by OrderSetDefaultScreen, OrderLoadOSTAT
	setitem	Nord001AStatPending,0,"Pending"
	deleteitem Nord001AComboPending,0
	insertitem Nord001AComboPending,1,""
	insertitem Nord001AComboPending,2,"Pending List Owner Approval"
	insertitem Nord001AComboPending,3,"Waiting for Guarantee"
	insertitem Nord001AComboPending,4,"Waiting for Broker Call Back"
	insertitem Nord001AComboPending,5,"Waiting for Pre-Payment"
	insertitem Nord001AComboPending,6,"Waiting for Revision"
	insertitem Nord001AComboPending,7,"At Service Bureau"
	insertitem Nord001AComboPending,8,"Denied"
	insertitem Nord001AComboPending,9,"Cancelled"
	insertitem Nord001AComboPending,10,"Approved!"
	insertitem Nord001AComboPending,11,"In Process"
	insertitem Nord001AComboPending,12,"Accounting Hold"
	insertitem Nord001AComboPending,13,"2nd Request"
	insertitem Nord001AComboPending,14,"Revised Request"
.START PATCH 3.71.5 ADDED LOGIC
	insertitem Nord001AComboPending,15,"Waiting for List Usage Agreement"
	insertitem Nord001AComboPending,16,"Waiting for Sample"
	insertitem Nord001AComboPending,17,"Waiting for Counts"
.END PATCH 3.71.5 ADDED LOGIC
	return
OrderSetPendingDisable
.called	by:  Nord001AComboStatus
.	 setprop Nord001ACheckTest,enabled=0
	setprop	Nord001ACheckEntire,enabled=0
	setprop	Nord001ACheckExchange,enabled=0
	setprop	Nord001AEditExchangeQty,enabled=0,bgcolor=grey
	setprop	Nord001AEditExchangePrice,enabled=0,bgcolor=grey
	setprop	Nord001bComboNet,enabled=0,bgcolor=grey
	call	OrderDisableNet
	setprop	Nord001bEditKeyInfo,enabled=0,bgcolor=grey
	return
OrderSetPendingDefault
.called	by:  Nord001AComboStatus
	setitem	Nord001ACheckTest,0,0
	setitem	Nord001ACheckRetest,0,0
	setitem	Nord001ACheckEntire,0,0
	setitem	Nord001ACheckExchange,0,0
	setitem	Nord001AEditPrice,0,"   .00"
	setitem	Nord001AEditRtn,0,"0000"
	setitem	Nord001AEditListSel,0,""
.START PATCH 3.72 ADDED LOGIC
	setprop	Nord001AEditListSelPrice,tabid=0
	setprop	Nord001AEditListSelCharge,tabid=0
	setprop	Nord001AComboListSelMod,tabid=0
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.68.3 ADDED LOGIC
	setitem	nord001CStatListSel,0,""
.END PATCH 3.68.3 ADDED LOGIC
.START PATCH 3.7 ADDED LOGIC
	setitem	nord001CStatExc,0,""
.END PATCH 3.7 ADDED LOGIC
	setitem	Nord001AComboOffer,0,2	.Offer #1
	move	C0,OSALES10
	move	C6,OSALES		.List Management
	call	OrderLoadSalesperson using C1
	setitem	Nord001bEditContDate,0,""
	setitem	Nord001bEditContLR,0,""
	setitem	Nord001bEditContQty,0,""
	setitem	Nord001bComboCont,0,1
	setitem	Nord001bEditNetCharge,0,""
	setitem	Nord001bEditNetMin,0,""
	setitem	Nord001bEditNetPercent,0,""
	setitem	Nord001bComboMedia,0,1	.blank
	setitem	Nord001bComboNet,0,1
.START PATCH 3.72.2 REPLACED LOGIC
.	setitem	Nord001bComboSample,0,1
	setitem	Nord001bEditSample,0,""
	Nord001bListViewSamples.DeleteAllItems giving result
.END PATCH 3.72.2 REPLACED LOGIC
	setitem	Nord001bComboSam,0,1
	setitem	Nord001bComboShip,0,1	.blank
	setitem	Nord001bComboTape,0,C1	.blank
	setitem	Nord001bComboBRKGuar,0,1
	setitem	Nord001bComboNINGuar,0,1
.START PATCH 3.71.5 REPLACED LOGIC
.START PATCH 3.71.7 COMMENTED BACK IN & MODIFIED CODE FROM CNT #2 TO CNT #17 JOEY G. LOGIC REMOVED IN PATCH 3.71.5
.	move	"02",str2	JANE NAGATOSHI
.Start patch 3.79.4
.Start patch 3.79.1
	move	"17",str2	Joey Gamache
.	move	"19",str2	Agnes Alvarez
.End patch 3.79.1
.Start patch 3.79.4
	call	OrderLoadCombo Using Nord001bComboContact,str2	 .Catherine Veyna
.SUB PATCH 3.71.7 COMMENTED OUT DEFAULT BLANK CONTACT,DEFAULT WIIL NOW BE JOEY G.
.	setitem	Nord001bComboContact,0,1
.END PATCH 3.71.7 COMMENTED BACK IN & MODIFIED FROM CNT #2 TO CNT #17 JOEY G. LOGIC REMOVED IN PATCH 3.71.5
.END PATCH 3.71.5 REPLACED LOGIC
	setitem	Nord001bEditKeyInfo,0,""
	setitem	Nord001bComboZipScreen,0,1
	return

.START PATCH 3.6 ADDED LOGIC
OrderSetCancelledScreen
	getitem	Nord001AStatPending,0,str9
.Do not	repopulate if already in proper	mode - you erase previous entry!!
	if (str7 = "Cancelled")
		return
	endif
	call	OrderLoadCancelledCombo
	return

OrderLoadCancelledCombo
.Called	by OrderSetDefaultScreen, OrderLoadOstat
	setitem	Nord001AStatPending,0,"Cancelled"
	deleteitem Nord001AComboPending,0
	insertitem Nord001AComboPending,1,""
	insertitem Nord001AComboPending,2,"Cancelled"
	insertitem Nord001AComboPending,3,"Mail Date over 6 Months old"
	insertitem Nord001AComboPending,4,"Cancelled"
	insertitem Nord001AComboPending,5,"Cancelled"
	insertitem Nord001AComboPending,6,"Cancelled"
	insertitem Nord001AComboPending,7,"Cancelled"
	insertitem Nord001AComboPending,8,"Cancelled"
	insertitem Nord001AComboPending,9,"Cancelled"
	return
.END PATCH 3.6 ADDED LOGIC

.This routine is no longer local.  NCAL0001 uses it.  12/12/2000 ASH
.OrderLoadCombo	 LRoutine ComboPtr,DimPtr
OrderLoadCombo	Routine	ComboPtr,DimPtr
	move	C2,N2		.Must start with first legitimate entry!!
	clear	str2
	loop
		getitem	ComboPtr,N2,str45
		unpack	str45,str35,str1,str2
		if (str2 = "" |	str2 = " " | str2 = "  ")
			move	C1,N2
			break
		endif
		until (str2 = DimPtr)
		add	C1,N2
.Extra protection
		if (N2 >= 98)
			move	C1,N2
			break
		endif
	repeat
	setitem	ComboPtr,0,N2
	return

OrderSwitchToPending
	setprop	Nord001AButtonPending,visible=0
	getitem	Nord001AComboStatus,0,N3
	getitem	Nord001bEditSales,0,str2
	getprop	Nord001ACheckMode,enabled=N9
	getitem	nord0001TabControlTop,0,howmany
	if (N3 = 2 & str2 = "06" & N9 =	1 & howmany = 1)
		setprop	Nord001AButtonPending,visible=1
	endif
	return
OrderClearFieldFlags
.START PATCH 3.76.7 REPLACED LOGIC
.	move	NO,KeyFlag
.	move	NO,ListFlag
	move	OMLRKY,OMLRKYHold
	getitem	Nord001AEditListSel,0,SelectHold
	move	OQTY,OQTYHold
.END PATCH 3.76.7 REPLACED LOGIC
	move	NO,QtyFlag
	move	NO,SpecFlag
	move	NO,SpecFlag2
	move	NO,Spec2Flag
	move	NO,Spec3Flag
	move	NO,XFlag
	move	B1,lolsw
	move	B1,loltype
.Fields	other than those affecting TDMC	LOL file
	move	NO,POFlag
	move	NO,GuarFlag2
	move	NO,ListFlag2
	move	C0,HistFlag
	move	NO,CampFlag
	return
OrderClearCampFieldFlags
	move	NO,QtyFlag2
	return
OrderClearLOLFieldFlags
.	 move	 NO,QtyFlag3
	return
OrderLoadScreens
.LR
	setitem	NORDMSK1StatLR,0,OLRN
.Reset Nord001AStatMssg.	If OSTAT = "B" then NININV is read.
.At that point Paid status is determined and Nord001AStatMssg may	be changed!
	setitem	Nord001AStatMssg,0,""
	setprop	Nord001AStatMssg,fgcolor=black
.I load	this object first as Nord001AStatMssg will be overwritten	if Order is Invoiced.
.CAMPAIGN
	setitem	Nord001AEditCampaign,0,OCAMP
	pack	NCMPFLD,OCAMP
	move	C1,NCMPPATH
	move	"O.LoadScreens-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
	setitem	Nord001AStatCamp1,0,NCMPCNAME
.HISTORY
	if (OHIST = "p")
		setitem	Nord001bStatHistory,0,"Originated as a List Management Order."
	elseif (OHIST =	"l")
		setitem	Nord001bStatHistory,0,"Originated as an LCR."
	elseif (OHIST =	"L")
		setitem	Nord001bStatHistory,0,"Originated as an In-House LCR."
	elseif (OHIST =	"e")
		if (OCO2CODE <>	"" & OCO2CODE <> "  ")
			setitem	Nord001bStatHistory,0,"LCR Waiting for Caller Response."
			setitem	Nord001AStatMssg,0,"Waiting for Caller Response."
		else
			setitem	Nord001bStatHistory,0,"Outside LCR Waiting for Contact Response."
			setitem	Nord001AStatMssg,0,"Outside LCR"
		endif
	elseif (OHIST =	"E")
		if (OCO2CODE <>	"" & OCO2CODE <> "  ")
			setitem	Nord001bStatHistory,0,"LCR Cleared by Caller."
			setitem	Nord001AStatMssg,0,"Cleared by Caller."
		else
			setitem	Nord001bStatHistory,0,"Outside LCR Cleared by Contact."
			setitem	Nord001AStatMssg,0,"Cleared Outside LCR."
		endif
	elseif (OHIST =	"*")
		if (OCO2CODE <>	"" & OCO2CODE <> "  ")
			setitem	Nord001bStatHistory,0,"LCR Waiting for Owner Response."
			setitem	Nord001AStatMssg,0,"Waiting for Owner Response."
		else
			setitem	Nord001bStatHistory,0,"Outside LCR Waiting for Owner Response."
			setitem	Nord001AStatMssg,0,"Outside LCR Waiting for Owner."
		endif
	elseif (OHIST =	"z")
		if (OCO2CODE <>	"" & OCO2CODE <> "  ")
			setitem	Nord001bStatHistory,0,"LCR Denied by Caller."
			setitem	Nord001AStatMssg,0,"Denied by Caller."
		else
			setitem	Nord001bStatHistory,0,"Denied Outside LCR."
			setitem	Nord001AStatMssg,0,"Denied Outside LCR."
		endif
	else
		setitem	Nord001bStatHistory,0,""
	endif
.CLEARANCE INFO
	call	OrderLoadClearanceStat2
.OSTAT COMBOBOX
	call	OrderLoadOSTAT
.MAILER
	setitem	Nord001AEditMlr,0,OMLRNUM
	setitem	Nord001AEditMlrContact,0,OCOBN
	clear	MKEY
	pack	MKEY,OMLRNUM,OCOBN
	rep	zfill,MKEY
	call	OrderLoadMailer	using C1
.BROKER
	setitem	Nord001AEditBrk,0,OBRKNUM
	setitem	Nord001AEditBrkContact,0,OBRKCNT
	clear	NBRKFLD
	call	TRIM using OBRKNUM
	count	N1,OBRKNUM
	if (N1 <> C0)
		pack	NBRKFLD,OBRKNUM,OBRKCNT
		call	OrderLoadBroker	using C1
	else
		setitem	Nord001AStatBrkComp,0,""
		setitem	Nord001AStatBrkCnt,0,""
.START PATCH 3.71.9 ADDED LOGIC
		setitem	Nord001AStatBrkNew,0,""
.END PATCH 3.71.9 ADDED LOGIC
	endif
.PO NUM
	setitem	Nord001AEditPO,0,OMLRPON
.OWNER
	setitem	Nord001AEditOwner,0,OLON
	move	OLON,NOWNFLD
	rep	ZFILL,NOWNFLD
	call	OrderLoadOwner using C1
.Start Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order - 
.FULFILLMENT
	count	N1,OFULLFIL
	if (N1 <> C0)
		Move OFullFil,Compfld
		Call Compkey
		If Over or (COMPSVBFLG <> "T")
			setitem NOrd001AStatFulfillmentComp,0,""					
		else
			setitem Nord001AEditFulFillment,0,OFULLFIL
			setitem NOrd001AStatFulfillmentComp,0,COMPCOMP		
.Patch Begin 3.78.8 Note - Moved this code from OrderLoadOwner routine 			
.			move	COMPCOMP,NFULCOMP			
.			call	Trim using NFULCOMP
.			if (COMPCOMP = "TDMC" OR COMPCOMP = "Tdmc" OR COMPCOMP = "tdmc" OR OFULLFIL = "009406")
			if (OFULLFIL = "009406")
				setprop	Nord001bStatListName,fgcolor=green
			else
				setprop	Nord001bStatListName,fgcolor=black
			endif		
.Patch End 3.78.8 Note - Moved this code from OrderLoadOwner routine 						
		endif		

	else
		setitem NOrd001AStatFulfillmentComp,0,""	
		setitem Nord001AEditFulfillment,0,""			

	Endif
.End Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order
	
list
.LIST AND LIST SEL
	setitem	Nord001AEditList,0,OLNUM
.START PATCH 3.72 REMOVED LOGIC
.	move	OUQTY,str9
.	call	FormatNumeric using str9,str11
.	setitem	Nord001AEditListUniverse,0,str11
.END PATCH 3.72 REMOVED LOGIC
	setitem	Nord001AStatListName,0,O1DES
	setitem	nord001CStatListName,0,O1DES
	setitem	Nord001bStatListName,0,O1DES
.START PATCH 3.72 REPLACED LOGIC
.	setitem	Nord001AEditListSel,0,O2DES
..START PATCH 3.68.3 ADDED LOGIC
.	setitem	nord001CStatListSel,0,O2DES
..END PATCH 3.68.3 ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"O.LoadScreens-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		setitem	Nord001AEditListSel,0,O2DES
		setitem	nord001CStatListSel,0,O2DES
		move	OUQTY,str9
		call	FormatNumeric using str9,str11
		setitem	Nord001AEditSelUniverse,0,str11
		move	OPPM,str3
		bump	OPPM BY	3
		move	OPPM,str2
		if (str2 = " 0"	OR str2	= "  ")
			pack	str2,"00"
		endif
		reset	OPPM
		pack	str6,str3,PERIOD,str2
.
		type	str6
		if not equal
			clear	str6
		endif
.
		setitem	Nord001AEditListSelPrice,0,str6
		setitem	Nord001AEditListSelCharge,0,""
		setitem	Nord001AComboListSelMod,0,1
		setitem	Nord001AStatListSelUniverse,0,""
		setitem	Nord001AStatListSelPrice,0,""
		setitem	Nord001AStatListSelCharge,0,""
		setitem	Nord001AStatRefPrice,0,""
		setitem	Nord001AStatTotPrice,0,""
.		setprop	Nord001AEditListSel,font=">MS Sans Serif'(8,BOLD)"
		setprop	Nord001AStatListSel,fgcolor=red
	else
		setitem	Nord001AEditListSel,0,NSEL2NAME
		setitem	nord001CStatListSel,0,NSEL2NAME
		if (NSEL2NUM = "XXXX")
.			setprop	Nord001AEditListSel,font=">MS Sans Serif'(8,BOLD)"
			setprop	Nord001AStatListSel,fgcolor=red
		else
.			setprop	Nord001AEditListSel,font=">MS Sans Serif'(8)"
			setprop	Nord001AStatListSel,fgcolor=black
		endif
		move	C0,N10
		move	NSEL2QTY,N10
		if (N10 = C0)
			clear	str13
		else
			move	N10,str10
			call	FormatNumeric using str10,str13
		endif
		setitem	Nord001AEditSelUniverse,0,str13
.
		move	C0,N10
		move	NSEL2QTY2,N10
		if (N10 = C0)
			clear	str13
		else
			move	N10,str10
			call	FormatNumeric using str10,str13
		endif
		setitem	Nord001AStatListSelUniverse,0,str13
.
		if (NSEL2PRICE = C0)
			clear	str9
		else
			unpack	NSEL2PRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
		endif
		setitem	Nord001AEditListSelPrice,0,str9
.
		move	C0,N3
		move	NSEL2DESC,N3
		add	C1,N3
		setitem	Nord001AComboListSelMod,0,N3
.
		if (NSEL2SPRICE = C0)
			clear	str9
		else
			unpack	NSEL2SPRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
		endif
		setitem	Nord001AEditListSelCharge,0,str9
.
		if (NSEL2SPRICE2 = C0)
			clear	str9
		else
			unpack	NSEL2SPRICE2,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
		endif
		setitem	Nord001AStatListSelCharge,0,str9
.
		if (NSEL2PRCD = "2")
			move	"Exc. Only",str25
		else
			if (NSEL2PRICE2 = C0)
				clear	str25
			else
				unpack	NSEL2PRICE2,str5,str3
				call	FormatNumeric using str5,str6
				pack	str9,str6,str3
				pack	NMODFLD,NSEL2DESC2
				rep	zfill,NMODFLD
				move	"O.LoadScreens-NMODKEY",Location
				pack	KeyLocation,"Key: ",NMODFLD
				call	NMODKEY
				call	Trim using NMODDESC
				pack	str25,str9,NMODDESC
			endif
		endif
		setitem	Nord001AStatListSelPrice,0,str25
	endif
.END PATCH 3.72 REPLACED LOGIC
.Call to text if List is EXCLUSIVE.
.If EXCLUSIVE then disable Order2ComboNINGuar -	done in	NORDMSK1ButtonModify_Click
	move	C3,NDATLOCK
	move	C1,NDATPATH
	pack	NDATFLD,OLNUM
	move	"O.LoadScreens-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	call	OrderLoadListColor using C1
.START PATCH 3.72 ADDED LOGIC
	call	OrderLoadSelectRef using C1
.
.START PATCH 3.77.4 ADDED LOGIC
	if (OLRN <> "")
.END PATCH 3.77.4 ADDED LOGIC
		pack	NSEL3FLD1,"01X1",OLRN
		move	"O.LoadScreens-NSEL3AIM",Location
		pack	KeyLocation,"Key: ",NSEL3FLD1
		call	NSEL3AIM
		loop
			until over
			if (NSEL3CODE = "A")
				pack	NADDFLD,OLNUM,NSEL3NUM
				move	"O.LoadScreens-NADDKEY",Location
				pack	KeyLocation,"Key: ",NADDFLD
				call	NADDKEY
				if not over
.START PATCH 3.74 ADDED LOGIC
					move	NSEL3PRICE,NADDPRICE
.END PATCH 3.74 ADDED LOGIC
					call	OrderLoadRefAddressing using Nord01A1ListViewRef2
				endif
			elseif (NSEL3CODE = "L")
				pack	NSLTFLD,OLNUM,NSEL3NUM
				move	"O.LoadScreens-NSLTKEY",Location
				pack	KeyLocation,"Key: ",NSLTFLD
				call	NSLTKEY
				if not over
.START PATCH 3.74 ADDED LOGIC
					move	NSEL3PRICE,NSLTPRICE
.END PATCH 3.74 ADDED LOGIC
					call	OrderLoadRefSelection using Nord01A1ListViewRef2
				endif
			endif
			move	"O.LoadScreens-NSEL3KG",Location
			call	NSEL3KG
		repeat
.START PATCH 3.77.4 ADDED LOGIC
	endif
.END PATCH 3.77.4 ADDED LOGIC
	call	OrderAddRefTotal
.END PATCH 3.72 ADDED LOGIC
.ORDER/EXCHANGE	QUANTITY & PRICE
PRICE
	move	C0,sqty
	move	oqty,sqty
	compare	sqty,"999999"
	if less
		setprop	Nord001AEditOrderQty,fgcolor=RED
	else
		setprop	Nord001AEditOrderQty,fgcolor=BLACK
	endif
	move	OQTY,str9
	call	FormatNumeric using str9,str11
	setitem	Nord001AEditOrderQty,0,str11
	setitem	Nord001bStatOrderQty,0,str11
	move	OEXQTY,str9
	call	FormatNumeric using str9,str11
	setitem	Nord001AEditExchangeQty,0,str11
........................
.START PATCH 3.72 REPLACED LOGIC
.	move	OPPM,str3
.	bump	OPPM BY	3
.	move	OPPM,str2
.	if (str2 = " 0"	OR str2	= "  ")
.		pack	str2,"00"
.	endif
.	reset	OPPM
.	pack	str6,str3,PERIOD,str2
.	setitem	Nord001AEditPrice,0,str6
.END PATCH 3.72 REPLACED LOGIC
.
	move	OXPPM,str3
	bump	OXPPM BY 3
	move	OXPPM,str2
	if (str2 = " 0"	OR str2	= "  ")
		pack	str2,"00"
	endif
	reset	OXPPM
	pack	str6,str3,PERIOD,str2
	setitem	Nord001AEditExchangePrice,0,str6
.RETURNDATE/MAILDATE/ORDERDATE
    	call	TRIM using ORTNDTEM
	count	N2,ORTNDTEM
	if (N2 <> 0 AND	ORTNDTEM <> "00")
		pack	newdate1,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
	else
		clear	newdate1
	endif
	setitem	Nord001AEditRtnDate,0,newdate1
	move	key,NSHPFLD
	move	"O.LoadScreens-NSHPTST",Location
	pack	KeyLocation,"Key: ",NSHPFLD
	call	NSHPTST
	if over
		setprop	Nord001AEditRtnDate,fgcolor=RED
	else
		setprop	Nord001AEditRtnDate,fgcolor=BLACK
	endif
.
.START PATCH 3.77.9 DREW - REPLACED LOGIC
.	call	TRIM using OMDTEM
.	count	N2,OMDTEM
.	if (N2 <> 0 AND	OMDTEM <> "00")
.		pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.	else
.		clear	newdate1
.	endif
.	setitem	Nord001AEditMailDate,0,newdate1
....................................................
	call	TRIM using OMDTEM
	call	TRIM using OMDTED
	call	TRIM using OMDTEC
	call	TRIM using OMDTEY
	pack	newdate1,OMDTEM,OMDTED,OMDTEC,OMDTEY
	if (newdate1 = "00000000" | newdate1 = "11111111" | newdate1 = "")
		clear	newdate1
	else
		pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
	endif
	setitem	Nord001AEditMailDate,0,newdate1
.END PATCH 3.77.9 DREW - REPLACED LOGIC
.
	call	TRIM using OODTEM
	count	N2,OODTEM
	if (N2 <> 0 AND	OODTEM <> "00")
		pack	newdate1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
	else
		clear	newdate1
	endif
	setitem	Nord001AEditOrderDate,0,newdate1
OFFER
	move	OMLRNUM,str4
	move	OODNUM,str7
	call	OrderLoadOffer using C1	.Offer reloaded	whenever mailer	field is changed
.Next Field Must be loaded after Offer is loaded - check out OrderLoadRtn
.SHIP TO
	if (ORTNNUM = "0001")	.OREUSE
		setitem	Nord001AEditRtn,0,OREUSE
		setitem	Nord001AStatRtn,0,"Re-use LR"
	else
		setitem	Nord001AStatRtn,0,"Ship-To ##"
		setitem	Nord001AEditRtn,0,ORTNNUM
	endif
	cmatch	B1,ORTNNUM
	if not EOS
		move	ORTNNUM,NRTNFLD
	endif
	move	"O.LoadScreens-NRTNKEY",Location
	pack	KeyLocation,"Key: ",NRTNFLD
	call	NRTNKEY
	if over
		call	OrderClearRtn using C1
	elseif (ORTNNUM	= B4)
		call	OrderClearRtn using C1
	else
		move	ORTNNUM,str6
		move	OMLRNUM,str4
		call	OrderLoadRtn using C1
	endif
.REVISE/CANCEL
	clear	str55
	move	key,NCRCFLD
	move	c1 to NCRCPATH
	move	"O.LoadScreens-NCRCKEY",Location
	pack	KeyLocation,"Key: ",NCRCFLD
	call	NCRCKEY
	if not over
		loop
			clear	str55
			match	"C" to NCRCCODE
			if equal
				append	"CANCELLED ",str55
			else
				append	"REVISED ",str55
			endif
			append	NCRCMM,str55
			append	SLASH,str55
			append	NCRCDD,str55
			append	SLASH,str55
			append	NCRCCC,str55
			append	NCRCYY,str55
			append	" By ",str55
			append	NCRCTYP,str55
			reset	str55
			move	"O.LoadScreens-NCRCKS",Location
			pack	KeyLocation,"Key: ",NCRCFLD
			call	NCRCKS
			until over
			match	NCRCFLD,NCRCKEY
			until not equal
		repeat
	endif
	setitem	Nord001bStatRevise,0,str55
	setitem	Nord001AStatRevise,0,str55

.RENTAL/EXCHANGE
.START PATCH 3.7 ADDED LOGIC
	clear	str8
.END PATCH 3.7 ADDED LOGIC
	if (OELCODE = "1")
		setitem	Nord001ACheckEntire,0,1
		setitem	Nord001ACheckExchange,0,0
	elseif (OELCODE	= "2")
		setitem	Nord001ACheckEntire,0,0
		setitem	Nord001ACheckExchange,0,1
		if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
			if (ORENT = "1")  .LCR Rental
				move	"Exc/Rent",str8
			else
				move	"Exchange",str8
			endif
		else
			call	Trim using OEXQTY
			move	C0,N9
			move	OEXQTY,N9
			if (N9 > 0)
				move	"Split",str8
			else
				move	"Exchange",str8
			endif
		endif
	elseif (OELCODE	= "3")
		setitem	Nord001ACheckEntire,0,1
		setitem	Nord001ACheckExchange,0,1
		if (OSTAT = "l" | OSTAT = "z" | OSTAT = "p" | OSTAT = "x")
			if (ORENT = "1")  .LCR Rental
				move	"Exc/Rent",str8
			else
				move	"Exchange",str8
			endif
		else
			call	Trim using OEXQTY
			move	C0,N9
			move	OEXQTY,N9
			if (N9 > 0)
				move	"Split",str8
			else
				move	"Exchange",str8
			endif
		endif
	else	.OELCODE = B1
		setitem	Nord001ACheckEntire,0,0
		setitem	Nord001ACheckExchange,0,0
	endif
.START PATCH 3.7 ADDED LOGIC
	setitem	nord001CStatExc,0,str8
.END PATCH 3.7 ADDED LOGIC
	cmatch	"1",OTOCODE
	if equal	.Order Test
		setitem	Nord001ACheckTest,0,1
		setitem	Nord001ACheckRetest,0,0
	elseif (OTOCODE	= "2")
		setitem	Nord001ACheckRetest,0,1
		setitem	Nord001ACheckTest,0,0
	else
		setitem	Nord001ACheckTest,0,0
		setitem	Nord001ACheckRetest,0,0
	endif
.RENT
	if (ORENT = "1")
		setitem	Nord001ACheckRent,0,1
	else
		setitem	Nord001ACheckRent,0,0
	endif
.Order3	Screen Special Instructions
.START PATCH 3.72.9 ADDED LOGIC
	call	Trim using OLRN
	if (OLRN <> "")
.END PATCH 3.72.9 ADDED LOGIC
		pack	NSPEFLD,OLRN
		rep	zfill,NSPEFLD
		move	C3,NSPELOCK
		move	"O.LoadScreens-NSPEKEY",Location
		pack	KeyLocation,"Key: ",NSPEFLD
		call	NSPEKEY
.
		call	Trim using DESC001
		setitem	nord001CEditSpecial1,0,DESC001
		call	Trim using DESC002
.If creating record off	of a NINLOL record do not retain Special Instructions from parent record in NINORD.DAT
		if (Order7Flag = C1)
			clear	DESC001
			clear	DESC002
			setitem	nord001CEditSpecial1,0,""
		endif
		setitem	nord001CEditSpecial,0,DESC002
		setitem	Nord001AStatExchangeMssg,0,DESC001
.START PATCH 3.72.9 ADDED LOGIC
	else
		setitem	nord001CEditSpecial1,0,""
		setitem	nord001CEditSpecial,0,""
		setitem	Nord001AStatExchangeMssg,0,""
	endif
.END PATCH 3.72.9 ADDED LOGIC.
.START PATCH 3.72.9 ADDED LOGIC
	if (OLRN <> "")
.END PATCH 3.72.9 ADDED LOGIC
		pack	NSPE2FLD,OLRN
		rep	zfill,NSPE2FLD
		move	C3,NSPE2LOCK
		move	"O.LoadScreens-NSPE2KEY",Location
		pack	KeyLocation,"Key: ",NSPE2FLD
		call	NSPE2KEY
.
		call	Trim using DESC003
		setitem	nord001CEditSpecial2,0,DESC003
.
		call	Trim using DESC004
		setitem	nord001CEditSpecial3,0,DESC004
.START PATCH 3.72.9 ADDED LOGIC
	else
		setitem	nord001CEditSpecial2,0,""
		setitem	nord001CEditSpecial3,0,""
	endif
.END PATCH 3.72.9 ADDED LOGIC
.Order2	Screen
.SALESPERSON
	call	OrderLoadSalesperson using C1
.CONTACT
.LOOK AT THIS AGAIN:  MODIFY - DISCONTC
	call	TRIM using OCOCODE
	call	OrderLoadCombo Using Nord001bComboContact,OCOCODE
.CALLER
	call	TRIM using OCO2CODE
	call	OrderLoadCombo Using Nord001bComboCaller,OCO2CODE
CONTINUATION
	if (OCCODE = " ")
		setitem	Nord001bComboCont,0,1
		setitem	Nord001bEditContDate,0,""
		setitem	Nord001bEditContLR,0,""
		setitem	Nord001bEditContQty,0,""
	elseif (OCCODE = "1")	.Omit
		setitem	Nord001bComboCont,0,2
		call	TRIM using OODTECOM
		count	N2,OODTECOM
		if (N2 <> 0 AND	OODTECOM <> "00")
			pack	newdate1,OODTECOM,SLASH,OODTECOD,SLASH,OODTECOC,OODTECOY
		else
			clear	newdate1
		endif
		setitem	Nord001bEditContDate,0,newdate1
		setitem	Nord001bEditContLR,0,OLRNCO
		move	OQTYCO,str9
		call	FormatNumeric using str9,str11
		setitem	Nord001bEditContQty,0,str11
	else	.No Omit
		setitem	Nord001bComboCont,0,3
		setitem	Nord001bEditContDate,0,""
		setitem	Nord001bEditContLR,0,""
		setitem	Nord001bEditContQty,0,""
	endif
NET
	if (ONETFM = NO)
		setitem	Nord001bComboNet,0,2	.GROSS BILLING NO DEDUCTS
		setitem	Nord001bEditNetCharge,0,"0"
		setitem	Nord001bEditNetMin,0,"0"
		setitem	Nord001bEditNetPercent,0,"0"
	elseif (ONETFM <> "M" AND ONETFM <> "F")
		setitem	Nord001bComboNet,0,1
		setitem	Nord001bEditNetCharge,0,"0"
		setitem	Nord001bEditNetMin,0,"0"
		setitem	Nord001bEditNetPercent,0,"0"
	else
		if (ONETFM = "M")
			setitem	Nord001bComboNet,0,3
		elseif (ONETFM = "F")
			setitem	Nord001bComboNet,0,4
		endif
		move	ONETRC,str6		.Must load EditTextBoxes with DIM var
		setitem	Nord001bEditNetCharge,0,str6
		move	ONETMIN,str7		.Must load EditTextBoxes with DIM var
		call	Trim using str7
		call	FormatNumeric using str7,str9
		setitem	Nord001bEditNetMin,0,str9
		setitem	Nord001bEditNetPercent,0,ONETPER
	endif
.NET QTY/PERCENTAGE
	call	Trim using ONETQTY
	move	C0,N9
	move	ONETQTY,N9
	if (ONETQTY = "" OR N9 = C0)
		setitem	Nord001bEditNetPer,0,""
		setitem	Nord001bEditNetQty,0,""
	else
		move	ONETQTY,str9
		call	FormatNumeric using str9,str11
		setitem	Nord001bEditNetQty,0,str11
		call	OrderCalculateNet using	Nord001AEditOrderQty,Nord001bEditNetPer,Nord001bEditNetQty,C2
	endif
.SAMPLE
	move	OMLRNUM,str4
	call	OrderLoadSamples using C1			.This routine will refresh DataList
	move	OSCODE,str3
	move	OSAMCDE,str4
	call	OrderLoadSamples2
.NIN GUARANTY
.NOTICE	THAT NULL WOULD	REFER TO C5, BUT TO KEEP COMBO BOX CLEAN I PLACED IT AS	ITEM #1
	setitem	Nord001bEditAttn,0,""
.START PATCH 3.71 ADDED LOGIC
	setitem	Nord001bEditFax,0,""
.END PATCH 3.71 ADDED LOGIC
	if (GUARCODE = "1")
		setitem	Nord001bComboNINGuar,0,2
	elseif (GUARCODE = "2")
		setitem	Nord001bComboNINGuar,0,3
	elseif (GUARCODE = "3")
		setitem	Nord001bComboNINGuar,0,4
	elseif (GUARCODE = "4")
		setitem	Nord001bComboNINGuar,0,5
	elseif (GUARCODE = "5")
		setitem	Nord001bComboNINGuar,0,1
	elseif (GUARCODE = "6")
		setitem	Nord001bComboNINGuar,0,6
	elseif (GUARCODE = "7")
		setitem	Nord001bComboNINGuar,0,7
	elseif (GUARCODE = "8")
		setitem	Nord001bComboNINGuar,0,8
	elseif (GUARCODE = "9")
		setitem	Nord001bComboNINGuar,0,9
	else
		setitem	Nord001bComboNINGuar,0,1
	endif
.BRK GUARANTY
	if (OBRKGUAR = "1")
		setitem	Nord001bComboBRKGuar,0,2
	elseif (OBRKGUAR = "2")
		setitem	Nord001bComboBRKGuar,0,3
	elseif (OBRKGUAR = "3")
		setitem	Nord001bComboBRKGuar,0,4
	elseif (OBRKGUAR = "4")
		setitem	Nord001bComboBRKGuar,0,5
	else
		setitem	Nord001bComboBRKGuar,0,1
	endif
.KEY INFO
	setitem	Nord001bEditKeyInfo,0,OMLRKY
MEDIA
.- COMBOBOX LOADED IMMEDIATELY AFTER FORMS ARE LOADED
.Logic here is a bit strange but here is the idea:  MED20 currently holds blank	record,	and that
.has been associated with item #1 of combobox.	Item #1	will equal MED20, Item #2 will equal MED00,
.Item #3 will equal MED01, etc.	 When you reach	Item #20 one of	those displacements has	been
.soaked	up by associating MED20	with Item #1 and so you	need one less incrementation.
	move	OFOCODE,N2
	move	N2,N3
	add	C1,N2	.File begins with '0', Combo begins with '1' and  first	item is	null
	if (N3 = 20)
		move	C0,N2	.First item has	blank filled string of MED20
	else
		if (N3 < 20)
			add	C1,N2
		endif
	endif
	setitem	Nord001bComboMedia,0,N2
.SHIPPING
	call	TRIM using OSHP
	if (OSHP = "")
		setitem	Nord001bComboShip,0,1
	else
		move	OSHP,N2
		add	C2,N2
		setitem	Nord001bComboShip,0,N2
	endif
.TAPE RETURN
	if (OTAPERET = YES)
		setitem	Nord001bComboTape,0,2
	elseif (OTAPERET = NO)
		setitem	Nord001bComboTape,0,3
	else
		setitem	Nord001bComboTape,0,1
	endif
.DJ
	setitem	Nord001bStatTypist2,0,ODOWJ
.TEST CODE SELECTION
	if (OSOTCODE = "1")
		setitem	Nord001bComboTestCode,0,2
	elseif (OSOTCODE = "2")
		setitem	Nord001bComboTestCode,0,3
	elseif (OSOTCODE = "3")
		setitem	Nord001bComboTestCode,0,4
	else
		setitem	Nord001bComboTestCode,0,1
	endif
.ZIP SCREEN
	if (OCOMSLCT = "C")
		setitem	Nord001bComboZipScreen,0,2
	elseif (OCOMSLCT = "L")
		setitem	Nord001bComboZipScreen,0,3
	elseif (OCOMSLCT = "I")
		setitem	Nord001bComboZipScreen,0,4
	else
		setitem	Nord001bComboZipScreen,0,1
	endif

.BILL DIRECT
	if (OBILDRCT = YES OR OBILDRCT = "y")
		setitem	Nord001bCheckDirect,0,1
	else
		setitem	Nord001bCheckDirect,0,0
	endif
.PACKAGES
	call	OrderLoadMlrPackageListView using Nord001bListViewMlrPackages,OMLRNUM
	call	OrderLoadPackageListView using Nord001bListViewPackages,OLRN,C0
.Order4	Screen
	call	OrderLoadNotes
	call	OrderLoadTDMC
	move	NORDFLD,NSHPFLD
	call	OrderLoadShip
.OrderApproveButton
	call	OrderApproveCheck
.............. end of LoadScreens ...............
	return

OrderLoadMlrPackageListView Routine LstVwPtr,DimPtr
	return
.
	LstVwPtr.DeleteAllItems	giving N9
	call	Trim using DimPtr
	if (DimPtr = "")
		return
	endif
	move	C1,NPKGPATH
	clear	NPKGFLD2
	clear	NPKGFLD3
	clear	NPKGFLD4
.START PATCH 3.4 ADDED LOGIC
	clear	NPKGFLD5
.END PATCH 3.4 ADDED LOGIC
.START PATCH 3.75.4 REPLACED LOGIC
.	pack	NPKGFLD1,"01X",DimPtr
	move	"LoadScreens-COMPKEY3",Location
	pack	COMPFLD3,DimPtr
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	if over
		clear	COMPNUM
	endif
	pack	NPKGFLD1,"01X",COMPNUM
.END PATCH 3.75.4 REPLACED LOGIC
	move	"LoadScreens-NPKGAIM",Location
	pack	KeyLocation,"Key: ",NPKGFLD1
	call	NPKGAIM
	loop
		until over
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.		if (NPKGMaster <> "1")
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
			pack	hold2p,NPKGVARS
			LstVwPtr.InsertItem giving N9 using NPKGNUM
			LstVwPtr.SetItemText using N9,NPKGPNAME,1
			LstVwPtr.SetItemText using N9,NPKGID,2
			LstVwPtr.SetItemText using N9,hold2p,3
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.		endif
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
		move	"LoadScreens-NPKGKG",Location
		call	NPKGKG
	repeat
	return

OrderLoadPackageListView Routine LstVwPtr,DimPtr,FrmPtr
.	return
.
	LstVwPtr.DeleteAllItems	giving N9
	call	Trim using DimPtr
	if (DimPtr = "")
		return
	endif
	move	C1,STATPATH
	pack	STAT2FLD2,"01X",DimPtr
	pack	STAT2FLD3,"02X",FrmPtr
	move	"LoadScreens-STAT2AIM",Location
	pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
	call	STAT2AIM
	loop
		until over
		pack	hold10,STATVARS
		move	C1,NPKGPATH
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.4 REPLACED LOGIC
..		pack	NPKGFLD,STATMLR,STATPCKNUM
.		move	"LoadScreens-COMPKEY3",Location
.		pack	COMPFLD3,STATMLR
.		pack	KeyLocation,"Key: ",COMPFLD3
.		call	COMPKEY3
.		if over
.			clear	COMPNUM
.		endif
.		pack	NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 3.75.4 REPLACED LOGIC
		pack	NPKGFLD,STATMLR,STATPCKNUM
.END PATCH 3.75.9 REPLACED LOGIC
		move	"LoadScreens-NPKGKEY",Location
		pack	KeyLocation,"Key: ",NPKGFLD
		call	NPKGKEY
		if over
			clear	NPKGPNAME
			clear	NPKGID
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.		elseif (NPKGMaster = "1")
.			clear	NPKGPNAME
.			clear	NPKGID
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
		endif
		LstVwPtr.InsertItem giving N9 using NPKGNUM
		LstVwPtr.SetItemText using N9,NPKGPNAME,1
		LstVwPtr.SetItemText using N9,NPKGID,2
.START PATCH 3.4 REPLACED LOGIC
.		move	STATMQTY,str8
.		call	FormatNumeric using str8,str11
		move	statrecqty,str9
		call	Trim using str9
		call	FormatNumeric using str9,str11
.END PATCH 3.4 REPLACED	LOGIC
		LstVwPtr.SetItemText using N9,str11,3
		LstVwPtr.SetItemText using N9,hold10,4
		move	"LoadScreens-STAT2KG",Location
		pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
		call	STAT2KG
	repeat
	return

OrderStatsCheckOK Routine FrmPtr
	getprop	NSTA001AEditLR2,enabled=FrmPtr
	return

OrderStatsLoadOK Routine DimPtr
	setitem	NSTA0002EditSearchLR,0,DimPtr
	call	Click_NSTA0002ButtonOk
	return

OrderStatsSetListView Routine DimPtr
	setprop	NSTA0002ListView2,MultiSelect=0
	move	C0,howmany
	NSTA0002ListView2.GetItemCount giving result
	sub	C1,result
	for N9 from "0"	to result
     		NSTA0002ListView2.GetItemText giving hold10 using N9,C4
		unpack	hold10,STATVARS
		pack	str10,STATLR,STATNUM
		if (DimPtr = str10)
			move	N9,howmany
			break
		endif
	repeat
	NSTA0002ListView2.SetItemState GIVING N9 USING *Index=howmany,*State=3,*Statemask=3
	NSTA0002ListView2.EnsureVisible using howmany,0
	call	Click_NSTA0002ListView2
	setprop	NSTA0002ListView2,MultiSelect=1
	return

OrderLoadNotes
	call	OrderDisableOrderAll4
	clear	hold3
	deleteitem Nord001GDataList,0
	move	OLRN,NONOFLD
	move	"O.LoadNotes-NONOKEY",Location
	pack	KeyLocation,"Key: ",NONOFLD
	call	NONOKEY
	if not over
		unpack	NTIME,str2,str3
		pack	str5,str2,COLON,str3
		unpack	NDATE,MM,DD,YY,STR2
.START PATCH 3.78.3 REPLACED LOGIC
.		pack	hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NLINE1:
.			NLINE2,NLINE3,NLINE4,NLINE5,NLINE6,NINITS
.		HERE PACK HOLD3 FOR THE DATA LIST
		pack	hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NINITS,NDATE,NTIME // str5 is time, str2 is cc
.END PATCH 3.78.3 REPLACED LOGIC
		insertitem Nord001GDataList,0,hold3
		loop
			move	"O.LoadNotes-NONOKS",Location
			pack	KeyLocation,"Key: ",NONOFLD
			call	NONOKS
			until over
			until (NOTEKEY <> NONOFLD)
			unpack	NTIME,str2,str3
			pack	str5,str2,COLON,str3
			unpack	NDATE,MM,DD,YY,STR2
.START PATCH 3.78.3 REPLACED LOGIC
.			pack	hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1:
.				NLINE1,NLINE2,NLINE3,NLINE4,NLINE5,NLINE6,NINITS
			pack	hold3,NOTEKEY,B1,MM,SLASH,DD,SLASH,STR2,YY,B2,str5,B1,NINITS,NDATE,NTIME // str5 is time, str2 is cc
.END PATCH 3.78.3 REPLACED LOGIC
			insertitem Nord001GDataList,0,hold3
		repeat
		setitem	Nord001GDataList,1,1
	endif
	call	OrderLoadNotes2
	return

OrderLoadNotes2
.START PATCH 3.78.3 ADDED LOGIC
	Nord001GDataList.GetCount giving result
	if (result = 0)
		setitem	Nord001GEditDate,0,""
		setitem	Nord001GEditTime,0,""
		setitem	Nord001GEditInits,0,""
		setitem	Nord001GEditNote,0,""
		return
	endif
.END PATCH 3.78.3 ADDED LOGIC
	getitem	Nord001GDataList,0,result
	getitem	Nord001GDataList,result,hold3
.START PATCH 3.78.3 REPLACED LOGIC
.	unpack	hold3,NOTEKEY,str1,str10,str2,str5,str1,NLINE1,NLINE2,NLINE3:
.		NLINE4,NLINE5,NLINE6,NINITS
	unpack	hold3,NOTEKEY,str1,str10,str2,str5,str1,NINITS,NDATE,NTIME
.END PATCH 3.78.3 REPLACED LOGIC
	call	Trim using str10
	count	N2,str10
	if (N2 <> 10)
		clear	str10
	endif
	setitem	Nord001GEditDate,0,str10
	setitem	Nord001GEditTime,0,str5
	setitem	Nord001GEditInits,0,NINITS
.START PATCH 3.78.3 REPLACED LOGIC
.	setitem	Nord001GEditNote1,0,NLINE1
.	setitem	Nord001GEditNote2,0,NLINE2
.	setitem	Nord001GEditNote3,0,NLINE3
.	setitem	Nord001GEditNote4,0,NLINE4
.	setitem	Nord001GEditNote5,0,NLINE5
.	setitem	Nord001GEditNote6,0,NLINE6
.  now do read on notekey, ndate, and ntime
.  save old vars
	move ndate, ndate2
	move ntime, ntime2
	move	notekey,NONOFLD   // notekey will change - old value still in nonofld
	move	"O.LoadNotes-NONOKEY",Location
	pack	KeyLocation,"Key: ",NONOFLD
	call	NONOKEY
	if not over
		if (nonofld=notekey && ndate2=ndate && ntime2=ntime)  // is found record the one I want?
			call 	TRIM using NLINE
			setitem	Nord001GEditNote,0,NLINE
		else
			loop
				move	"O.LoadNotes-NONOKS",Location
				pack	KeyLocation,"Key: ",NONOFLD
				call	NONOKS
				until over
				until (NOTEKEY <> NONOFLD)
				if (nonofld=notekey && ndate2=ndate && ntime2=ntime) // is found record the one I want?
					call 	TRIM using NLINE
					setitem	Nord001GEditNote,0,NLINE
				endif
			repeat
		endif
	endif
.END PATCH 3.78.3 REPLACED LOGIC
	setfocus Nord001GDataList
	return

OrderLoadTDMC
	clear	taskname
	deleteitem Nord001GTDMCList,0
	move	OLRN,TINVFLD
	move	"O.LoadTDMC-TINVKEY",Location
	pack	KeyLocation,"Key: ",TINVFLD
	call	TINVKEY
	if not over
		move	TINVDOLR,form122
		mult	".01",form122
		move	C0,TDMCAMT
		add	form122,TDMCAMT
		move	TDMCAMT,str11
		unpack	TINVDATE,str2,YY,MM,DD
		pack	taskname,TINVLR,B1,MM,SLASH,DD,SLASH,STR2,YY,B1,TINVDESC:
			TINVINV,str11
		insertitem Nord001GTDMCList,0,taskname
		loop
			move	"O.LoadTDMC-TINVKS",Location
			pack	KeyLocation,"Key: ",TINVFLD
			call	TINVKS
			until over
			until (TINVLR <> TINVFLD)
			move	TINVDOLR,form122
			mult	".01",form122
			move	C0,TDMCAMT
			add	form122,TDMCAMT
			move	TDMCAMT,str11
			unpack	TINVDATE,str2,YY,MM,DD
			pack	taskname,TINVLR,B1,MM,SLASH,DD,SLASH,STR2,YY,B1,TINVDESC:
				TINVINV,str11
			insertitem Nord001GTDMCList,0,taskname
		repeat
		setitem	Nord001GTDMCList,1,1
	endif
	call	OrderLoadTDMC2
	return

OrderLoadTDMC2
	getitem	Nord001GTDMCList,0,result
	getitem	Nord001GTDMCList,result,hold3
	unpack	taskname,TINVLR,str1,str10,str1,TINVDESC,TINVINV,str11
	call	Trim using str10
	count	N2,str10
	if (N2 <> 10)
		clear	str10
	endif
	setitem	Nord001GEditTDMCAmount,0,str11
	setitem	Nord001GEditTDMCDate,0,str10
	setitem	Nord001GEditTDMCID,0,TINVINV
	setitem	Nord001GEditTDMCName,0,TINVDESC
	setfocus Nord001GTDMCList
	return

OrderLoadScreen5
	clear	str8
	append	OLRN,str8
	if (OSTAT = "p")
		append	"p",str8
	endif
	if (LCRMod = 4)
.	 if (LCRMod = 3)
		TRAP	IOMssg Giving Error if IO
		move	"O.LoadScr.5-NINPRINT",Location
		pack	KeyLocation,"Key: ",OLRN
		if (OSTAT = "l")
			read	ordprint,OLRN;;
			if not over
				append	STAR,str8
			endif
		else	.OSTAT = "p"
			read	ordprint,OLRN;str55,str55,str55,str10,str1;
			if not over
				if (str1 = "*")
					append	STAR,str8
				endif
			endif
		endif
		TRAPCLR	IO
	endif
.START PATCH	3.78.2	REPLACED LOGIC
..ListView #1
.	reset	str8
.	Nord001DListView.InsertItem giving N9 using str8
.	pack	str15,OLNUM,SLASH,OLON
.	Nord001DListView.SetItemText using N9,str15,1
.	Nord001DListView.SetItemText using N9,OMLRNUM,2
.	Nord001DListView.SetItemText using N9,OQTY,3
. 	if (OELCODE = "2" | OELCODE = "3")
..		 if (OTOCODE = "R")  .LCR Rental
..			 move	 "Exc/Rent",str10
..		 else
..			 move	 "Exchange",str10
..		 endif
.........
.		if (ORENT = "1")  .LCR Rental
.			move	"Exc/Rent",str10
.		else
.			move	"Exchange",str10
.		endif
.	else
.		move	"Rental",str10
.	endif
.	Nord001DListView.SetItemText using N9,str10,4
.	call	TRIM using OMDTEM
.	count	N2,OMDTEM
.	if (N2 <> 0 AND	OMDTEM <> "00")
.		pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.	else
.		clear	newdate1
.	endif
.	Nord001DListView.SetItemText using N9,newdate1,5
.	call	TRIM using OODTEM
.	count	N2,OODTEM
.	if (N2 <> 0 AND	OODTEM <> "00")
.		pack	str11,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
.	else
.		clear	str11
.	endif
.	Nord001DListView.SetItemText using N9,str11,6
.	pack	NCNTFLD,OCOCODE
.	move	C1,NCNTPATH
.	move	"O.Load5-NCNTKEY",Location
.	pack	KeyLocation,"Key: ",NCNTFLD
.	call	NCNTKEY
.	if over
.		clear	str35
.	else
.		move	CNTNAME,str35
.	endif
.	clear	str12
.	if (OCLRSTAT = "1")
.		move	"Exchange",str12
.	elseif (OCLRSTAT = "2")
.		move	"Rental",str12
.	elseif (OCLRSTAT = "3")
.		move	"Exc/Rent",str12
.	elseif (OCLRSTAT = "4")
.		move	"Denied",str12
.	endif
.	Nord001DListView.SetItemText using N9,str12,7
.	Nord001DListView.SetItemText using N9,OCLRINIT,8
.	clear	str16
.	call	TRIM using OCLRDTEM
.	count	N2,OCLRDTEM
.	if (N2 <> 0 AND	OCLRDTEM <> "00")
.		append	OCLRDTEM,str16
.		append	SLASH,str16
.		append	OCLRDTED,str16
.		append	SLASH,str16
.		append	OCLRDTEC,str16
.		append	OCLRDTEY,str16
.		if (OBRKRPT = "1")
.			append	STAR,str16
.		endif
.		reset	str16
.	elseif (OBRKRPT	= "1")
.		append	STAR,str16
.		reset	str16
.	endif
.	Nord001DListView.SetItemText using N9,str16,9
.	Nord001DListView.SetItemText using N9,str35,10
.	pack	NCNTFLD,OCO2CODE
.	move	C1,NCNTPATH
.	move	"O.Load5-2-NCNTKEY",Location
.	pack	KeyLocation,"Key: ",NCNTFLD
.	call	NCNTKEY
.	if over
.		clear	CNTNAME
.	endif
.	Nord001DListView.SetItemText using N9,CNTNAME,11
.	Nord001DListView.SetItemText using N9,OSAMCDE,12
.	clear	NPNDDESC
.	if (OSTAT = "l"	| OSTAT	= "z")
.		move	OLRN,NORD5FLD
.		if (NORD5FLD <>	"")
.			rep	zfill in NORD5FLD
.			clear	NORD5STAT
.			move	"O.Load5-NORD5KEY",Location
.			pack	KeyLocation,"Key: ",NORD5FLD
.			call	NORD5KEY		.get LCR info
.			if not over			    .File is out of whack!
.				move	"l",str1
.				pack	NPNDFLD	from str1,NORD5STAT
.				rep	zfill in NPNDFLD
.				move	"O.PendingStatus-NPNDKEY",Location
.				pack	KeyLocation,"Key: ",NPNDFLD
.				call	NPNDKEY
.				if over
.					clear	NPNDDESC
.				endif
.			endif
.		endif
.	else
.		move	OLRN,NORD4FLD
.		if (NORD4FLD <>	"")
.			rep	zfill in NORD4FLD
.			clear	NORD4STAT
.			move	"O.Load5-NORD4KEY",Location
.			pack	KeyLocation,"Key: ",NORD4FLD
.			call	NORD4KEY		.get LCR info
.			if not over			    .File is out of whack!
.				if (OSTAT = "p"	| OSTAT	= "x")
.					move	"p",str1
.				endif
.				pack	NPNDFLD	from str1,NORD4STAT
.				rep	zfill in NPNDFLD
.				move	"O.PendingStatus-NPNDKEY",Location
.				pack	KeyLocation,"Key: ",NPNDFLD
.				call	NPNDKEY
.				if over
.					clear	NPNDDESC
.				endif
.			endif
.		endif
.	endif
.	Nord001DListView.SetItemText using N9,NPNDDESC,13
.	Nord001DListView.SetItemText using N9,O1DES,14
.	move	OLON,NOWNFLD
.	rep	ZFILL,NOWNFLD
.	move	"O.Load5-NOWNKEY",Location
.	pack	KeyLocation,"Key: ",NOWNFLD
.	call	NOWNKEY
.	if over
.		clear	OWNOCPY
.	endif
.	Nord001DListView.SetItemText using N9,OWNOCPY,15
.	clear	MKEY
.	pack	MKEY,OMLRNUM,"000"
.	rep	zfill,MKEY
.	move	"O.Load5-NMLRKEY",Location
.	pack	KeyLocation,"Key: ",MKEY
.	call	NMLRKEY
.	if over
.		clear	MCOMP
.	endif
.	Nord001DListView.SetItemText using N9,MCOMP,16
.	BUMP	OODNUM BY 4
.	pack	NOFRFLD,OMLRNUM,OODNUM
.	reset	OODNUM
.	rep	zfill in NOFRFLD
.	move	"O.Load5-NOFRKEY",Location
.	pack	KeyLocation,"Key: ",NOFRFLD
.	call	NOFRKEY
.	if over
.		clear	OFDESC
.	endif
.	Nord001DListView.SetItemText using N9,OFDESC,17
.	clear	DESC001
.	move	C3,NSPELOCK
.	pack	NSPEFLD,OLRN
.	rep	zfill,NSPEFLD
.	move	"O.Load5-NSPEKEY",Location
.	pack	KeyLocation,"Key: ",NSPEFLD
.	call	NSPEKEY
.	if over
.		clear	DESC001
.	endif
.	Nord001DListView.SetItemText using N9,DESC001,18

.ListView #1
	reset	str8  // from above
	clear	MKEY
		pack	MKEY,OMLRNUM,"000"
		rep	zfill,MKEY
		move	"O.Load5-NMLRKEY",Location
		pack	KeyLocation,"Key: ",MKEY
		call	NMLRKEY
		if over
			clear	MCOMP
		endif
	Nord001DListView.InsertItem giving N9 using MCOMP
	Nord001DListView.SetItemText using N9,str8,2
	pack	str15,OLNUM,SLASH,OLON
	Nord001DListView.SetItemText using N9,str15,17
	Nord001DListView.SetItemText using N9,OMLRNUM,18
	Nord001DListView.SetItemText using N9,OQTY,3
.begin patch 3.79.2
	If	(NewFlag = yes)
	Move	CompExcl,OCompID
	endif
.begin patch 3.79.2
 	if (OELCODE = "2" | OELCODE = "3")
		if (ORENT = "1")  .LCR Rental
			move	"Exc/Rent",str10
		else
			move	"Exchange",str10
		endif
	else
		move	"Rental",str10
	endif
	Nord001DListView.SetItemText using N9,str10,4
	call	TRIM using OMDTEM
	count	N2,OMDTEM
	if (N2 <> 0 AND	OMDTEM <> "00")
		pack newdate1, OMDTEC,OMDTEY,OMDTEM,OMDTED,"00000000"
		Nord001DListView.SetItemText using N9,newdate1,19
		clear newdate1
		pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
	else
		clear	newdate1
	endif
	Nord001DListView.SetItemText using N9,newdate1,5
	call	TRIM using OODTEM
	count	N2,OODTEM
	if (N2 <> 0 AND	OODTEM <> "00")
		pack newdate1, OODTEC,OODTEY,OODTEM,OODTED,"00000000"
		Nord001DListView.SetItemText using N9,newdate1,20
		clear newdate1
		pack	str11,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
	else
		clear	str11
	endif
	Nord001DListView.SetItemText using N9,str11,6
	pack	NCNTFLD,OCOCODE
	move	C1,NCNTPATH
	move	"O.Load5-NCNTKEY",Location
	pack	KeyLocation,"Key: ",NCNTFLD
	call	NCNTKEY
	if over
		clear	str35
	else
		move	CNTNAME,str35
	endif
	clear	str12
	if (OCLRSTAT = "1")
		move	"Exchange",str12
	elseif (OCLRSTAT = "2")
		move	"Rental",str12
	elseif (OCLRSTAT = "3")
		move	"Exc/Rent",str12
	elseif (OCLRSTAT = "4")
		move	"Denied",str12
	endif
	Nord001DListView.SetItemText using N9,str12,7
	Nord001DListView.SetItemText using N9,OCLRINIT,8
	clear	str16
	call	TRIM using OCLRDTEM
	count	N2,OCLRDTEM
	if (N2 <> 0 AND	OCLRDTEM <> "00")
		pack newdate1, OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,"00000000"
		Nord001DListView.SetItemText using N9,newdate1,21
		clear newdate1
		append	OCLRDTEM,str16
		append	SLASH,str16
		append	OCLRDTED,str16
		append	SLASH,str16
		append	OCLRDTEC,str16
		append	OCLRDTEY,str16
		if (OBRKRPT = "1")
			append	STAR,str16
		endif
		reset	str16
	elseif (OBRKRPT	= "1")
		append	STAR,str16
		reset	str16
	endif
	Nord001DListView.SetItemText using N9,str16,9
	Nord001DListView.SetItemText using N9,str35,10
	pack	NCNTFLD,OCO2CODE
	move	C1,NCNTPATH
	move	"O.Load5-2-NCNTKEY",Location
	pack	KeyLocation,"Key: ",NCNTFLD
	call	NCNTKEY
	if over
		clear	CNTNAME
	endif
	Nord001DListView.SetItemText using N9,CNTNAME,11
	Nord001DListView.SetItemText using N9,OSAMCDE,12
	clear	NPNDDESC
	if (OSTAT = "l"	| OSTAT	= "z")
		move	OLRN,NORD5FLD
		if (NORD5FLD <>	"")
			rep	zfill in NORD5FLD
			clear	NORD5STAT
			move	"O.Load5-NORD5KEY",Location
			pack	KeyLocation,"Key: ",NORD5FLD
			call	NORD5KEY		.get LCR info
			if not over			    .File is out of whack!
				move	"l",str1
				pack	NPNDFLD	from str1,NORD5STAT
				rep	zfill in NPNDFLD
				move	"O.PendingStatus-NPNDKEY",Location
				pack	KeyLocation,"Key: ",NPNDFLD
				call	NPNDKEY
				if over
					clear	NPNDDESC
				endif
			endif
		endif
	else
		move	OLRN,NORD4FLD
		if (NORD4FLD <>	"")
			rep	zfill in NORD4FLD
			clear	NORD4STAT
			move	"O.Load5-NORD4KEY",Location
			pack	KeyLocation,"Key: ",NORD4FLD
			call	NORD4KEY		.get LCR info
			if not over			    .File is out of whack!
				if (OSTAT = "p"	| OSTAT	= "x")
					move	"p",str1
				endif
				pack	NPNDFLD	from str1,NORD4STAT
				rep	zfill in NPNDFLD
				move	"O.PendingStatus-NPNDKEY",Location
				pack	KeyLocation,"Key: ",NPNDFLD
				call	NPNDKEY
				if over
					clear	NPNDDESC
				endif
			endif
		endif
	endif
	Nord001DListView.SetItemText using N9,NPNDDESC,13
	Nord001DListView.SetItemText using N9,O1DES,1
	move	OLON,NOWNFLD
	rep	ZFILL,NOWNFLD
	move	"O.Load5-NOWNKEY",Location
	pack	KeyLocation,"Key: ",NOWNFLD
	call	NOWNKEY
	if over
		clear	OWNOCPY
	endif
	Nord001DListView.SetItemText using N9,OWNOCPY,14

	BUMP	OODNUM BY 4
	pack	NOFRFLD,OMLRNUM,OODNUM
	reset	OODNUM
	rep	zfill in NOFRFLD
	move	"O.Load5-NOFRKEY",Location
	pack	KeyLocation,"Key: ",NOFRFLD
	call	NOFRKEY
	if over
		clear	OFDESC
	endif
	Nord001DListView.SetItemText using N9,OFDESC,15
	clear	DESC001
	move	C3,NSPELOCK
	pack	NSPEFLD,OLRN
	rep	zfill,NSPEFLD
	move	"O.Load5-NSPEKEY",Location
	pack	KeyLocation,"Key: ",NSPEFLD
	call	NSPEKEY
	if over
		clear	DESC001
	endif
	Nord001DListView.SetItemText using N9,DESC001,16
	return


.END PATCH	3.78.2 	REPLACE LOGIC
.START PATCH	3.78.2	REMOVED LOGIC
.ListView #2
.	Order5ListView2.InsertItem giving N9 using O1DES
.	Order5ListView2.SetItemText using N9,MCOMP,1
.	Order5ListView2.SetItemText using N9,str8,2
.	Order5ListView2.SetItemText using N9,OQTY,3
.	Order5ListView2.SetItemText using N9,str10,4
.	Order5ListView2.SetItemText using N9,newdate1,5
.	Order5ListView2.SetItemText using N9,str11,6
.	Order5ListView2.SetItemText using N9,str12,7
.	Order5ListView2.SetItemText using N9,OCLRINIT,8
.	Order5ListView2.SetItemText using N9,str16,9
.	Order5ListView2.SetItemText using N9,str35,10
.	Order5ListView2.SetItemText using N9,CNTNAME,11
.	Order5ListView2.SetItemText using N9,OSAMCDE,12
.	Order5ListView2.SetItemText using N9,NPNDDESC,13
.	Order5ListView2.SetItemText using N9,OWNOCPY,14
.	Order5ListView2.SetItemText using N9,OFDESC,15
.	Order5ListView2.SetItemText using N9,DESC001,16
.	Order5ListView2.SetItemText using N9,str15,17
.	Order5ListView2.SetItemText using N9,OMLRNUM,18
.
..ListView #3
.	Order5ListView3.InsertItem giving N9 using MCOMP
.	Order5ListView3.SetItemText using N9,O1DES,1
.	Order5ListView3.SetItemText using N9,str8,2
.	Order5ListView3.SetItemText using N9,OQTY,3
.	Order5ListView3.SetItemText using N9,str10,4
.	Order5ListView3.SetItemText using N9,newdate1,5
.	Order5ListView3.SetItemText using N9,str11,6
.	Order5ListView3.SetItemText using N9,str12,7
.	Order5ListView3.SetItemText using N9,OCLRINIT,8
.	Order5ListView3.SetItemText using N9,str16,9
.	Order5ListView3.SetItemText using N9,str35,10
.	Order5ListView3.SetItemText using N9,CNTNAME,11
.	Order5ListView3.SetItemText using N9,OSAMCDE,12
.	Order5ListView3.SetItemText using N9,NPNDDESC,13
.	Order5ListView3.SetItemText using N9,OWNOCPY,14
.	Order5ListView3.SetItemText using N9,OFDESC,15
.	Order5ListView3.SetItemText using N9,DESC001,16
.	Order5ListView3.SetItemText using N9,str15,17
.	Order5ListView3.SetItemText using N9,OMLRNUM,18
..ListView #4
..Top item is used for SORT
.	move	C0,JULDAYS
.	if (str16 <> ""	& str16	<> "*")
.		unpack	str16,MM,str1,DD,str1,CC,YY
.		call	cvtjul
.	endif
.	move	JULDAYS,str5
.	Order5ListView4.InsertItem giving N9 using str5
.	Order5ListView4.SetItemText using N9,str16,1
.	Order5ListView4.SetItemText using N9,O1DES,2
.	Order5ListView4.SetItemText using N9,MCOMP,3
.	Order5ListView4.SetItemText using N9,str8,4
.	Order5ListView4.SetItemText using N9,str12,5
.	Order5ListView4.SetItemText using N9,OQTY,6
.	Order5ListView4.SetItemText using N9,str10,7
.	Order5ListView4.SetItemText using N9,newdate1,8
.	Order5ListView4.SetItemText using N9,str11,9
.	Order5ListView4.SetItemText using N9,OCLRINIT,10
.	Order5ListView4.SetItemText using N9,str35,11
.	Order5ListView4.SetItemText using N9,CNTNAME,12
.	Order5ListView4.SetItemText using N9,OSAMCDE,13
.	Order5ListView4.SetItemText using N9,NPNDDESC,14
.	Order5ListView4.SetItemText using N9,OWNOCPY,15
.	Order5ListView4.SetItemText using N9,OFDESC,16
.	Order5ListView4.SetItemText using N9,DESC001,17
.	Order5ListView4.SetItemText using N9,str15,18
.	Order5ListView4.SetItemText using N9,OMLRNUM,19
..ListView #5
..Top item is used for SORT
.	move	C0,JULDAYS
.	if (newdate1 <>	"" & newdate1 <> "*")
.		unpack	newdate1,MM,str1,DD,str1,CC,YY
.		call	cvtjul
.	endif
.	move	JULDAYS,str5
.	Order5ListView5.InsertItem giving N9 using str5
.	Order5ListView5.SetItemText using N9,newdate1,1
.	Order5ListView5.SetItemText using N9,O1DES,2
.	Order5ListView5.SetItemText using N9,MCOMP,3
.	Order5ListView5.SetItemText using N9,str8,4
.	Order5ListView5.SetItemText using N9,str12,5
.	Order5ListView5.SetItemText using N9,OQTY,6
.	Order5ListView5.SetItemText using N9,str10,7
.	Order5ListView5.SetItemText using N9,str11,8
.	Order5ListView5.SetItemText using N9,OCLRINIT,9
.	Order5ListView5.SetItemText using N9,str16,10
.	Order5ListView5.SetItemText using N9,str35,11
.	Order5ListView5.SetItemText using N9,CNTNAME,12
.	Order5ListView5.SetItemText using N9,OSAMCDE,13
.	Order5ListView5.SetItemText using N9,NPNDDESC,14
.	Order5ListView5.SetItemText using N9,OWNOCPY,15
.	Order5ListView5.SetItemText using N9,OFDESC,16
.	Order5ListView5.SetItemText using N9,DESC001,17
.	Order5ListView5.SetItemText using N9,str15,18
.	Order5ListView5.SetItemText using N9,OMLRNUM,19
..ListView #6
..Top item is used for SORT
.	move	C0,JULDAYS
.	if (str11 <> ""	& str11	<> "*")
.		unpack	str11,MM,str1,DD,str1,CC,YY
.		call	cvtjul
.	endif
.	move	JULDAYS,str5
.	Order5ListView6.InsertItem giving N9 using str5
.	Order5ListView6.SetItemText using N9,str11,1
.	Order5ListView6.SetItemText using N9,O1DES,2
.	Order5ListView6.SetItemText using N9,MCOMP,3
.	Order5ListView6.SetItemText using N9,str8,4
.	Order5ListView6.SetItemText using N9,str12,5
.	Order5ListView6.SetItemText using N9,OQTY,6
.	Order5ListView6.SetItemText using N9,str10,7
.	Order5ListView6.SetItemText using N9,newdate1,8
.	Order5ListView6.SetItemText using N9,OCLRINIT,9
.	Order5ListView6.SetItemText using N9,str16,10
.	Order5ListView6.SetItemText using N9,str35,11
.	Order5ListView6.SetItemText using N9,CNTNAME,12
.	Order5ListView6.SetItemText using N9,OSAMCDE,13
.	Order5ListView6.SetItemText using N9,NPNDDESC,14
.	Order5ListView6.SetItemText using N9,OWNOCPY,15
.	Order5ListView6.SetItemText using N9,OFDESC,16
.	Order5ListView6.SetItemText using N9,DESC001,17
.	Order5ListView6.SetItemText using N9,str15,18
.	Order5ListView6.SetItemText using N9,OMLRNUM,19
OrderLoadStatScreen5
	setitem	Nord001DStatListName,0,O1DES
	setitem	Nord001DStatOwnerName,0,OWNOCPY
	setitem	Nord001DStatMailerName,0,MCOMP
	setitem	Nord001DStatOfferName,0,OFDESC
	setitem	Nord001DStatSpecial1,0,DESC001
	pack	NSPEFLD,OLRN
	rep	zfill,NSPEFLD
	clear	DESC002
	if (NSPEFLD <> "")
		move	C3,NSPELOCK
		move	"O.Load5,2-NSPEKEY",Location
		pack	KeyLocation,"Key: ",NSPEFLD
		call	NSPEKEY
	endif
	pack	NSPE2FLD,OLRN
	rep	zfill,NSPE2FLD
	clear	DESC003
.START PATCH	3.78.2	ADDED LOGIC
	clear	DESC004
.END PATCH	3.78.2	ADDED LOGIC
	if (NSPE2FLD <>	"")
		move	C3,NSPE2LOCK
		move	"O.Load5,2-NSPE2KEY",Location
		pack	KeyLocation,"Key: ",NSPE2FLD
		call	NSPE2KEY
	endif
.START PATCH	3.78.2	REMOVED LOGIC
.	setitem	Nord001DEditSpecial3,0,DESC003
.	setitem	Nord001DEditSpecial4,0,DESC002
.END PATCH	3.78.2	REMOVED LOGIC
.START PATCH	3.78.2	ADDED LOGIC
GetRadioButtons
	getitem Nord001DRadioSpecInstr,0,N1
	if (N1=1)
		setitem Nord001DEditTextNotes,0,Desc002
		return
	endif
	getitem Nord001DRadioIntNotes,0,N1
	if (N1=1)
		setitem Nord001DEditTextNotes,0,Desc003
		return
	endif
	getitem Nord001DRadioMlrNotes,0,N1
		if (N1=1)
		setitem Nord001DEditTextNotes,0,Desc004  // mailer notes
		return
	endif
.END PATCH	3.78.2	ADDED LOGIC
	return
.
OrderLoadShip
	move	"O.LoadShip-NSHPKEY",Location
	pack	KeyLocation,"Key: ",NSHPFLD
	call	NSHPKEY
	setitem	Nord001GEditInfo,0,SINFO
	setitem	Nord001GEditShipQty,0,SQUANT
	call	Trim using SPOST
	if (SPOST <> "")
		move	SPOST,str2
		bump	SPOST BY 2
		move	SPOST,str3
		if (str3 = " 0"	OR str3	= "  ")
			pack	str3,"00"
		endif
		reset	SPOST
		pack	str5,str2,PERIOD,str3
	else
		clear	str5
	endif
	setitem	Nord001GEditPrice,0,str5
	call	TRIM using SDATE
	if (SDATE <> "")
		unpack	SDATE,str2,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,str2,YY
	else
		clear	newdate1
	endif
	setitem	Nord001GEditShipDate,0,newdate1
	setitem	Nord001GEditTrack,0,STRACK
	if (SCODE = "C")
		move	C2,N2
	elseif (SCODE =	"I")
		move	C3,N2
	elseif (SCODE =	"M")
		move	C4,N2
	elseif (SCODE =	"P")
		move	C5,N2
	elseif (SCODE =	"S")
		move	C6,N2
	elseif (SCODE =	"T")
		move	C7,N2
.>Patch 3.76.3 Code Added for new Shipping Codes
	elseif (SCODE = "E")
		move	C8,N2
	elseif (SCODE = "D")
		move	C9,N2
	elseif (SCODE = "A")
		move	C10,N2
.>Patch 3.76.3 End
	else
		move	C1,N2
	endif
	setitem	Nord001GComboCode,0,N2
	return

OrderLoadOSTAT
.OSTAT saved as	HoldStat in OrderModify
.Clear Nord001AComboPending value
	setitem	Nord001AComboPending,0,1
	clear	str55
.START PATCH 3.6 REPLACED LOGIC
.	if (OSTAT = "l"	OR OSTAT = "z")
.		call	OrderLoadLCRCombo
.		if (OSTAT = "l")
.			setitem	Nord001AComboStatus,0,2
..Eventually we'll simply throw away next line and use OSTAT
.			move	"l",str1	.used by following subroutine
.			call	OrderLCRStatus
.			append	"*LCR*",STR55
.			append	B1,STR55
.			append	NPNDDESC,STR55
.			reset	STR55
.		elseif (OSTAT =	"z")
.			setitem	Nord001AComboStatus,0,2
..Eventually we'll simply throw away next line and use OSTAT
.			move	"l",str1	.used by following subroutine, eventually will need to be "x"
.			call	OrderLCRStatus
.			append	"*CANCELLED LCR*",STR55
.			append	B1,STR55
.			append	NPNDDESC,STR55
.			reset	STR55
.		endif
.................................
	if (OSTAT = "l")
		call	OrderLoadLCRCombo
		setitem	Nord001AComboStatus,0,2
.Eventually we'll simply throw away next line and use OSTAT
		move	"l",str1	.used by following subroutine
		call	OrderLCRStatus
		append	"*LCR*",STR55
		append	B1,STR55
		append	NPNDDESC,STR55
		reset	STR55
	elseif (OSTAT = "z")
		call	OrderLoadCancelledCombo
		setitem	Nord001AComboStatus,0,9
.Eventually we'll simply throw away next line and use OSTAT
		move	"x",str1	.used by following subroutine, eventually will need to be "x"
		call	OrderCancelledStatus
		append	"*CANCELLED LCR*",STR55
		append	B1,STR55
		append	NPNDDESC,STR55
		reset	STR55
.END PATCH 3.6 REPLACED LOGIC
	else
		call	OrderLoadPendingCombo
		if (OSTAT = "p")
			setitem	Nord001AComboStatus,0,3
.Eventually we'll simply throw away next line and use OSTAT
			move	"p",str1	.used by following subroutine
			call	OrderPendingStatus
			append	"*PENDING*",STR55
			append	B1,STR55
			append	NPNDDESC,STR55
			reset	STR55
		elseif (OSTAT =	"x")
			setitem	Nord001AComboStatus,0,3
.Eventually we'll simply throw away next line and use OSTAT
			move	"p",str1	.used by following subroutine, eventually will need to be "x"
			call	OrderPendingStatus
			append	"*CANCELLED PENDING*",STR55
			append	B1,STR55
			append	NPNDDESC,STR55
			reset	STR55
		elseif (OSTAT =	"0")
			setitem	Nord001AComboStatus,0,5
			pack	str55,"Live Order"
		elseif (OSTAT =	"B")
			setitem	Nord001AComboStatus,0,6
			pack	str55,"Billed Order"
			call	OrderBilled
		elseif (OSTAT =	"Q")
			setitem	Nord001AComboStatus,0,7
			pack	str55,"Cancelled-Billed"
			call	OrderBilled
		elseif (OSTAT =	"X")
			setitem	Nord001AComboStatus,0,8
			pack	str55,"Cancelled Order"
		else
			setitem	Nord001AComboStatus,0,1
			clear	str55
		endif
	endif
	call	OrderLoadMessages
	return
OrderBilled
	move	key to NINVFLD
	move	c1 to NINVPATH
	move	"OrderBilled-NINVKEY",Location
	pack	KeyLocation,"Key: ",NINVFLD
	call	NINVKEY
	if not over
		setprop	Nord001AStatMssg,fgcolor=red
.Test if Order is paid
		if (STATB = "P")
			setitem	Nord001AStatMssg,0,"Order is Paid!"
.Test if Invoice is 60+	days old
		else
			move	OMDTEM,MM
			move	OMDTED,DD
			move	OMDTEY,YY
			call	CVTJUL
			move	JULDAYS,howmany
			unpack	timestamp,str2,YY,MM,DD
			call	CVTJUL
			sub	howmany,JULDAYS
			if (JULDAYS >= 120)
				setitem	Nord001AStatMssg,0,"Order Invoice over 120 days!"
			elseif (JULDAYS	>= 90)
				setitem	Nord001AStatMssg,0,"Order Invoice over 90 days!"
			elseif (JULDAYS	> 60)
				setitem	Nord001AStatMssg,0,"Order Invoice over 60 days!"
			endif
		endif
	endif
	return
OrderPendingStatus
	move	key to NORD4FLD
	rep	zfill in NORD4FLD
	clear	NORD4STAT
	move	"O.PendingStatus-NORD4KEY",Location
	pack	KeyLocation,"Key: ",NORD4FLD
	call	NORD4KEY		.get pending info
	if over				.File is out of	whack!
		return			.Do not	reset Nord001AComboPending!
	endif
	call	OrderStatusDesc
	return
OrderLCRStatus
	move	key to NORD5FLD
	rep	zfill in NORD5FLD
	clear	NORD5STAT
	move	"O.PendingStatus-NORD5KEY",Location
	pack	KeyLocation,"Key: ",NORD5FLD
	call	NORD5KEY		.get LCR info
	if over				.File is out of	whack!
		return			.Do not	reset Nord001AComboPending!
	endif
	call	OrderStatusDesc
	return
.START PATCH 3.6 ADDED LOGIC
OrderCancelledStatus
	move	key to NORD6FLD
	rep	zfill in NORD6FLD
	clear	NORD6STAT
	move	"O.PendingStatus-NORD6KEY",Location
	pack	KeyLocation,"Key: ",NORD6FLD
	call	NORD6KEY		.get LCR info
	if over				.File is out of	whack!
		return			.Do not	reset Nord001AComboPending!
	endif
	call	OrderStatusDesc
	return
.END PATCH 3.6 ADDED LOGIC
OrderStatusDesc
.START PATCH 3.6 ADDED LOGIC
	move	C0,N2
.END PATCH 3.6 ADDED LOGIC
	if (str1 = "p")
		pack	NPNDFLD	from str1,NORD4STAT
		rep	zfill in NPNDFLD
		move	"O.PendingStatus-NPNDKEY",Location
		pack	KeyLocation,"Key: ",NPNDFLD
		call	NPNDKEY
		move	NORD4STAT,N2
		add	C2,N2
		setitem	Nord001AComboPending,0,N2
	elseif (str1 = "x")	.not yet applied
.START PATCH 3.6 ADDED LOGIC
		pack	NPNDFLD	from str1,NORD6STAT
		rep	zfill in NPNDFLD
		move	"O.PendingStatus-NPNDKEY",Location
		pack	KeyLocation,"Key: ",NPNDFLD
		call	NPNDKEY
		move	NORD6STAT,N2
		add	C2,N2
		setitem	Nord001AComboPending,0,N2
.END PATCH 3.6 ADDED LOGIC
	elseif (str1 = "l")
		pack	NPNDFLD	from str1,NORD5STAT
		rep	zfill in NPNDFLD
		move	"O.PendingStatus-NPNDKEY",Location
		pack	KeyLocation,"Key: ",NPNDFLD
		call	NPNDKEY
		move	NORD5STAT,N2
		add	C1,N2
		setitem	Nord001AComboPending,0,N2
	elseif (str1 = "z")	.not yet applied
	endif
	return

OrderLoadSamples LRoutine FrmPtr
.Called	by:  Nord001AEditMlr_LostFocus, OrderLoadScreens,OrderLoadCampScreens,OrderLoadLOLScreen
.str4 must be filled with OMLRNUM prior	to calling this	subroutine!!!!
.This is used to dynamically change the	Samples	when the Mailer	is changed!!!
.Load Samples ComboBox
.START PATCH 3.69.0 REPLACED LOGIC
.	clear	NSMPFLD
.	clear	str3
.	move	C0,N3
.	move	C0,N4
.	move	C1,N5
.	move	B1,str35
..Must delete blank items entered to ensure adequate space
.	if (FrmPtr = C1)
.		deleteitem Nord001bComboSample,0
.		insertitem Nord001bComboSample,N3,str35	.Head item is blank
.	elseif (FrmPtr = C6)
.		deleteitem Nord01eaComboSample,0
.		insertitem Nord01eaComboSample,N3,str35
.	elseif (FrmPtr = C8)
.		deleteitem Nord01ECComboSample,0
.		insertitem Nord01ECComboSample,N3,str35
.	endif
.	loop
.		add	C1,N3
.		move	N3,str3
.		rep	zfill,str3
.		pack	NSMPFLD,str4,str3
.		move	"O.LoadSamples-NSMPKEY",Location
.		pack	KeyLocation,"Key: ",NSMPFLD
.		call	NSMPKEY
.		if not over
..START PATCH 3.65 REPLACED LOGIC
..			pack	str35,NSMPDES1,B1,str3
.			if (NSMPINACTIVE = "1")
.				move	"I",str1
.			else
.				clear	str1
.			endif
.			pack	str35,NSMPDES1,B1,str3,str1
..END PATCH 3.65 REPLACED LOGIC
.			if (FrmPtr = C1)
.				insertitem Nord001bComboSample,N3,str35
.			elseif (FrmPtr = C6)
.				add	C1,N5
.				if (str3 = NCMPSAMPLE)
..					 add	 C1,N3,N4
.					move	N5,N4
.				endif
..START PATCH 3.65 REPLACED LOGIC
..				insertitem Nord01eaComboSample,N3,str35
.				if (str1 = "I")
.					pack	str45,NSMPDES1,B1,str3,B1,"INACTIVE"
.				else
.					pack	str45,str35
.				endif
.				insertitem Nord01eaComboSample,N3,str45
..END PATCH 3.65 REPLACED LOGIC
.			elseif (FrmPtr = C8)
.				add	C1,N5
.  				if (str3 = NLOLSAMPLE)
..					 add	 C1,N3,N4
.					move	N5,N4
.				endif
.				insertitem Nord01ECComboSample,N3,str35
.			endif
.		endif
.		until (N3 > 50)	.THIS WILL NEED	TO BE INCREASED	IF MORE	THAN 50	SAMPLES	EXIST!!
.	repeat
.	if (FrmPtr = C6)
.		setitem	Nord01eaComboSample,0,N4
.	elseif (FrmPtr = C8)
.		setitem	Nord01ECComboSample,0,N4
..START PATCH 3.65 ADDED LOGIC
.		getitem	Nord01ECComboSample,N4,str35
.		unpack	str35,str30,str4,str1
.		if (str1 = "I")
.			setitem	Nord01ECStatSamplesInactive,0,"INACTIVE"
.		else
.			setitem	Nord01ECStatSamplesInactive,0,""
.		endif
..END PATCH 3.65 ADDED LOGIC
.	endif
........................
.START PATCH 3.75.5 ADDED LOGIC
	move	"O.LoadSamples-COMPKEY3",Location
	pack	COMPFLD3,str4
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	if over
		clear	COMPNUM
	endif
.END PATCH 3.75.5 ADDED LOGIC
	clear	NSMPFLD
	clear	str3
	move	C1,N3
	move	B1,str35
.Must delete blank items entered to ensure adequate space
	if (FrmPtr = C1)
.START PATCH 3.72.2 REPLACED LOGIC
.		deleteitem Nord001bComboSample,0
.		insertitem Nord001bComboSample,1,str35	.Head item is blank
		setitem	Nord001bEditSample,0,""
		Nord001bListViewSamples.DeleteAllItems giving result
		Nord001bListViewSamples.InsertItem giving N9 using "99999999"
.END PATCH 3.72.2 REPLACED LOGIC
	elseif (FrmPtr = C6)
		deleteitem Nord01eaComboSample,0
		insertitem Nord01eaComboSample,1,str35
	elseif (FrmPtr = C8)
		deleteitem Nord01ECComboSample,0
		insertitem Nord01ECComboSample,1,str35
	endif
.START PATCH 3.71 ADDED LOGIC
.START PATCH 3.75.5 ADDED LOGIC
.	call	Trim using str4
.	if (str4 = "")
.		return
.	endif
	call	Trim using COMPNUM
	if (COMPNUM = "")
		return
	endif
.END PATCH 3.75.5 ADDED LOGIC
.END PATCH 3.71 ADDED LOGIC
.START PATCH 3.75.5 ADDED LOGIC
.	pack	NSMPFLD1,"01X",str4
	pack	NSMPFLD1,"01X",COMPNUM
.END PATCH 3.75.5 ADDED LOGIC
	move	"O.LoadSamples-NSMPAIM",Location
	pack	KeyLocation,"Key: ",NSMPFLD1
	call	NSMPAIM
	loop
		until over
.START PATCH 3.75.5 ADDED LOGIC
.		until (str4 <> NSMPMLR)
		until (COMPNUM <> NSMPMLR)
.END PATCH 3.75.5 ADDED LOGIC
		if (NSMPINACTIVE = "1")
			move	"I",str1
		else
			clear	str1
		endif
.START PATCH 3.72.2 REPLACED LOGIC
.		pack	str35,NSMPDES1,B1,NSMPNUM,str1
		call	Trim using NSMPDATE
		if (NSMPDATE <> "")
			unpack	NSMPDATE,CC,YY,MM,DD
			pack	str55,NSMPDES1,B1,MM,SLASH,DD,SLASH,CC,YY,DASH,NSMPNUM,str1
			pack	str10,MM,SLASH,DD,SLASH,CC,YY
		else
			pack	str55,NSMPDES1,B1,"          ",DASH,NSMPNUM,str1
			clear	str10
		endif
.END PATCH 3.72.2 REPLACED LOGIC
		if (FrmPtr = C1)
.START PATCH 3.72.2 REPLACED LOGIC
.			insertitem Nord001bComboSample,1,str35
			Nord001bListViewSamples.InsertItem giving N9 using NSMPDATE
			Nord001bListViewSamples.SetItemText using N9,NSMPDES1,1
			Nord001bListViewSamples.SetItemText using N9,str10,2
			Nord001bListViewSamples.SetItemText using N9,NSMPNUM,3
			Nord001bListViewSamples.SetItemText using N9,str1,4
			if (NSMPINACTIVE = "1")
				move	"0xFF0000",colordim		.Red
			else
				move	"0x000000",colordim		.Black
			endif
			Nord001bListViewSamples.SetItemText using N9,colordim,5
.END PATCH 3.72.2 REPLACED LOGIC
		elseif (FrmPtr = C6)
			add	C1,N3
			move	NSMPNUM,str3
			if (str1 = "I")
.START PATCH 3.72.2 REPLACED LOGIC
.				pack	str45,NSMPDES1,B1,NSMPNUM,B1,"INACTIVE"
.			else
.				pack	str45,str35
.			endif
.			insertitem Nord01eaComboSample,1,str45
..............
				if (NSMPDATE <> "")
					unpack	NSMPDATE,CC,YY,MM,DD
					pack	str55,NSMPDES1,B1,MM,SLASH,DD,SLASH,CC,YY,DASH,NSMPNUM,B1,"INACTIVE"
				else
					pack	str55,NSMPDES1,B1,"          ",DASH,NSMPNUM,B1,"INACTIVE"
				endif
			endif
			insertitem Nord01eaComboSample,1,str55
.END PATCH 3.72.2 REPLACED LOGIC
		elseif (FrmPtr = C8)
			add	C1,N3
			move	NSMPNUM,str3
.START PATCH 3.72.2 REPLACED LOGIC
.			insertitem Nord01ECComboSample,1,str35
			insertitem Nord01ECComboSample,1,str55
.END PATCH 3.72.2 REPLACED LOGIC
		endif
		move	"O.LoadSamples-NSMPKG",Location
		call	NSMPKG
	repeat
	if (FrmPtr = C6)
		move	C1,N5
		for N4,"1",N3
.START PATCH 3.72.2 REPLACED LOGIC
.			getitem	Nord01eaComboSample,N4,str35
.			unpack	str35,str30,str4,str1
			getitem	Nord01eaComboSample,N4,str55
			unpack	str55,str30,str1,str10,str4,str1
.END PATCH 3.72.2 REPLACED LOGIC
			bump	str4
			if (str4 = NCMPSAMPLE)
				move	N4,N5
				break
			endif
		repeat
		setitem	Nord01eaComboSample,0,N5
	elseif (FrmPtr = C8)
		move	C1,N5
		clear	str1
		for N4,"1",N3
.START PATCH 3.72.2 REPLACED LOGIC
.			getitem	Nord01ECComboSample,N4,str35
.			unpack	str35,str30,str4,str1
			getitem	Nord01ECComboSample,N4,str55
			unpack	str55,str30,str1,str10,str4,str1
.END PATCH 3.72.2 REPLACED LOGIC
			bump	str4
			if (str4 = NLOLSAMPLE)
				move	N4,N5
				break
			endif
		repeat
		setitem	Nord01ECComboSample,0,N5
		if (str1 = "I")
			setitem	Nord01ECStatSamplesInactive,0,"INACTIVE"
		else
			setitem	Nord01ECStatSamplesInactive,0,""
		endif
	endif
.END PATCH 3.69.0 REPLACED LOGIC
	return
OrderLoadSamples2
.Called	by:  Nord001AEditMlr_LostFocus, OrderLoadScreens
.str4 must be filled with OMLRNUM prior	to calling this	subroutine!!!!
.This is used to dynamically change the	Samples	when the Mailer	is changed!!!
.START PATCH 3.72.2 REPLACED LOGIC
..START PATCH 3.61 REPLACED LOGIC
..	if (str3 = "1")
..		setitem	Nord001bComboSam,0,2		."Sample Enclosed"
.	if (str3 = "1" | str3 = "3")
.		move	C0,N1
.		move	str3,N1
.		add	C1,N1
.		setitem	Nord001bComboSam,0,N1		."Sample Enclosed" OR "Sample Previously Cleared"
..END PATCH 3.61 REPLACED LOGIC
.		call	TRIM using str4
.		if ((str4 <> "000") AND	(str4 <> ""))
..This logic must remain	as there are often sequentially	missing	Sample #'s
..but there may be a situation where Mailer was updated and Sample was not
..and so	Sample would refer to another Mailer.
..START PATCH 3.65 ADDED LOGIC
.			clear	str1
..END PATCH 3.65 ADDED LOGIC
.			move	C0,result
.			move	C0,N3
.			loop
.				add	C1,result
..START PATCH 3.72.2 REPLACED LOGIC
..				getitem	Nord001bComboSample,result,str35
..				unpack	str35,str30,str1,str5
.				getitem	Nord001bEditSample,result,str55
.				unpack	str55,str30,str1,str10,str1,str5
..END PATCH 3.72.2 REPLACED LOGIC
..START PATCH 3.65 REPLACED LOGIC
.				unpack	str5,str1,str1,str1,str1
..END PATCH 3.65 REPLACED LOGIC
.				setlptr	str5,3
.				if (str5 = str4)
.					move	result,N3
.					setitem	Nord001bComboSample,0,N3
.				endif
.				until (str5 = str4)
.				if (result > 50)	.if not	found yet then assume there is no SAmple for that Mailer!
..START PATCH 3.61 REPLACED LOGIC
..					insertitem Nord001bComboSample,1,"Sample NOT found!!"	.refreshed each	time mailer changed
..					deleteitem Nord001bComboSample,1				.decrement
..					setitem	Nord001bComboSample,0,1
.					if (str3 = "1")
.						insertitem Nord001bComboSample,1,"Sample NOT found!!"	.refreshed each	time mailer changed
.						deleteitem Nord001bComboSample,1				.decrement
.					endif
.					setitem	Nord001bComboSample,0,1
..END PATCH 3.61 REPLACED LOGIC
..START PATCH 3.65 ADDED LOGIC
.					clear	str1
..END PATCH 3.65 ADDED LOGIC
.				endif
.				until (result >	50)
.			repeat
..START PATCH 3.65 ADDED LOGIC
.			if (str1 = "I")
.				setitem	Nord001bStatSamplesInactive,0,"INACTIVE"
.			else
.				setitem	Nord001bStatSamplesInactive,0,""
.			endif
..END PATCH 3.65 ADDED LOGIC
.		else
..START PATCH 3.61 REPLACED LOGIC
..			insertitem Nord001bComboSample,1,"Sample NOT found!!"	.refreshed each	time mailer changed
..			deleteitem Nord001bComboSample,1				.decrement
..			setitem	Nord001bComboSample,0,1
.			if (str3 = "1")
.				insertitem Nord001bComboSample,1,"Sample NOT found!!"	.refreshed each	time mailer changed
.				deleteitem Nord001bComboSample,1				.decrement
.			endif
.			setitem	Nord001bComboSample,0,1
..END PATCH 3.61 REPLACED LOGIC
..START PATCH 3.65 ADDED LOGIC
.			setitem	Nord001bStatSamplesInactive,0,""
..END PATCH 3.65 ADDED LOGIC
.		endif
.	else
.		if (str3 = "2")
.			setitem	Nord001bComboSam,0,3	."Sample to Follow"
..START PATCH 3.61 REMOVED LOGIC
..		elseif (str3 = "3")
..			setitem	Nord001bComboSam,0,4	."Sample Previously Cleared"
..END PATCH 3.61 REMOVED LOGIC
.		else  .(str3 = B1)    assumed
.			setitem	Nord001bComboSam,0,1	."No Sample Enclosed"
.		endif
.		setitem	Nord001bComboSample,0,1		.Head item is blank
..START PATCH 3.65 ADDED LOGIC
.		setitem	Nord001bStatSamplesInactive,0,""
..END PATCH 3.65 ADDED LOGIC
.	endif
..................................................................
	clear	taskname
	if (str3 = "1" | str3 = "3")
		move	C0,N1
		move	str3,N1
		add	C1,N1
		setitem	Nord001bComboSam,0,N1		."Sample Enclosed" OR "Sample Previously Cleared"
		call	TRIM using str4
		if ((str4 <> "000") AND	(str4 <> ""))
.This logic must remain	as there are often sequentially	missing	Sample #'s
.but there may be a situation where Mailer was updated and Sample was not
.and so	Sample would refer to another Mailer.
			clear	str1
			move	C0,result
			move	C0,N3
			Nord001bListViewSamples.GetItemCount giving howmany
			for result,C1,howmany
				Nord001bListViewSamples.GetItemText giving NSMPNUM using result,3
				if (NSMPNUM = str4)
					Nord001bListViewSamples.GetItemText giving NSMPDES1 using result,1
					Nord001bListViewSamples.GetItemText giving str1 using result,4
					pack	taskname,NSMPNUM," - ",NSMPDES1,B1,str1
					break
				endif
			repeat
			if (str1 = "I")
				setitem	Nord001bStatSamplesInactive,0,"INACTIVE"
			else
				setitem	Nord001bStatSamplesInactive,0,""
			endif
		else
			setitem	Nord001bStatSamplesInactive,0,""
		endif
	else
		if (str3 = "2")
			setitem	Nord001bComboSam,0,3	."Sample to Follow"
		else  .(str3 = B1)    assumed
			setitem	Nord001bComboSam,0,1	."No Sample Enclosed"
		endif
		setitem	Nord001bStatSamplesInactive,0,""
	endif
	setitem	Nord001bEditSample,0,taskname
.END PATCH 3.72.2 REPLACED LOGIC
	return

.START PATCH 3.72.2 ADDED LOGIC
OrderLoadSamples3 Routine DimPtr,DimPtr1,DimPtr2
.DimPtr  = Sample Description
.DimPtr1 = Sample Number
.DimPtr2 = Inactive Status
	call	Trim using DimPtr1
	if (DimPtr1 <> "")
		getitem	Nord001bComboSam,0,N8
		if (N8 = 1 | N8 = 3)	.No Sample Enclosed/Sample to Follow
			setitem	Nord001bComboSam,0,2	.Sample Enclosed
		endif
		pack	taskname,DimPtr1," - ",DimPtr,B1,DimPtr2
		setitem	Nord001bEditSample,0,taskname
		if (DimPtr2 = "I")
			setitem	Nord001bStatSamplesInactive,0,"INACTIVE"
		else
			setitem	Nord001bStatSamplesInactive,0,""
		endif
	else
		setitem	Nord001bEditSample,0,""
		setitem	Nord001bStatSamplesInactive,0,""
	endif
	return
.END PATCH 3.72.2 ADDED LOGIC

OrderLoadMessages
	setitem	nord001CStatOrderCode,0,STR55
	setitem	Nord001bStatOrderCode,0,STR55
	return
OrderLoadSalesperson LRoutine FrmPtr
.Called	by:  OrderLoadScreens, Nord001AEditMlr_LostFocus,	Nord001AEditMlrContact_LostFocus,
.	     Nord001AEditBrk_LostFocus, Nord001AEditBrkContact_LostFocus,SearchLoad1,SearLoad3
.
	if (FrmPtr = C1)
.START PATCH 3.6 REPLACED LOGIC
.		if (mod	= 5 OR mod = 6 OR NewFlag = "S")
		if (mod = 8 OR mod = 5 OR mod = 6 OR NewFlag = "S")
.END PATCH 3.6 REPLACED LOGIC
			return
		endif
		pack	str2,OSALES10,OSALES
	elseif (FrmPtr = C7)
	endif
	call	OrderLoadSalespersonVerify using FrmPtr
	return
OrderLoadSalespersonVerify LRoutine FrmPtr
	if (FrmPtr = C1)
		setitem	Nord001bEditSales,0,str2
.have to reset this.  setitem will automatically set enabled to	TRUE, as per 8.3B - ASH
		getprop	Nord001bVScrollSales,enabled=result
		clear	HowMany
		move	str2,HowMany
		setitem	Nord001bVScrollSales,0,HowMany
		setprop	Nord001bVScrollSales,enabled=result
		move	osls0,salesper
		load	salesper from HowMany of osls1,osls2,osls3,osls4,osls5,osls6,osls7:
			osls8,osls9,osls10,osls11,osls12,osls13,osls14,osls15,osls16:
			osls17,osls18,osls19,osls20,osls21,osls22,osls23,osls24,osls25:
			osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
		setitem	Nord001bStatSales2,0,salesper
	elseif (FrmPtr = C7)
	endif
	return
OrderLoadBroker	LRoutine FrmPtr
.Called	by:  OrderLoadScreens,OrderLoadCampScreens,OrderLoadLOLScreen
.BrokerLostFocus, BrokerContactLostFocus,
	move	"O.LoadBroker-NBRKKEY",Location
	pack	KeyLocation,"Key: ",NBRKFLD
	call	NBRKKEY
	if over
.Do not	know if	above line is useful since Broker is not a required field.
		if (FrmPtr = "1")
			setitem	Nord001AStatBrkComp,0,""
			setitem	Nord001AStatBrkCnt,0,""
.START PATCH 3.71.9 ADDED LOGIC
			setitem	Nord001AStatBrkNew,0,""
.END PATCH 3.71.9 ADDED LOGIC
		elseif (FrmPtr = C6)
			setitem	Nord01eaStatBrkComp,0,""
			setitem	Nord01eaStatBrkCntName,0,""
.START PATCH 3.75.7 REMOVED LOGIC
..START PATCH 3.71.9 ADDED LOGIC
.			setitem	Nord01eaStatBrkNew,0,""
..END PATCH 3.71.9 ADDED LOGIC
.END PATCH 3.75.7 REMOVED LOGIC
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatBrkComp,0,""
			setitem	Nord01ECStatBrkCntName,0,""
.START PATCH 3.71.9 ADDED LOGIC
			setitem	Nord01ECStatBrkNew,0,""
.END PATCH 3.71.9 ADDED LOGIC
		endif
	else
.START PATCH 3.71.9 ADDED LOGIC
		call	Trim using COMPNUM
		if (COMPNUM <> "")
			call	Trim using CNCTID
			if (CNCTID <> "")
				pack	str10,COMPNUM,SLASH,CNCTID
			else
				pack	str10,COMPNUM
			endif
		else
			clear	str10
		endif
.END PATCH 3.71.9 ADDED LOGIC
		clear	str55
		pack	str55,"C/O ",BRCOMP
		if (FrmPtr = "1")
			setitem	Nord001AStatBrkComp,0,str55
			setitem	Nord001AStatBrkCnt,0,BRCNTCT
.START PATCH 3.71.9 ADDED LOGIC
			setitem	Nord001AStatBrkNew,0,str10
.END PATCH 3.71.9 ADDED LOGIC
		elseif (FrmPtr = C6)
			setitem	Nord01eaStatBrkComp,0,str55
			setitem	Nord01eaStatBrkCntName,0,BRCNTCT
.START PATCH 3.75.7 REMOVED LOGIC
..START PATCH 3.71.9 ADDED LOGIC
.			setitem	Nord01eaStatBrkNew,0,str10
..END PATCH 3.71.9 ADDED LOGIC
.END PATCH 3.75.7 REMOVED LOGIC
		elseif (FrmPtr = C8)
      			setitem	Nord01ECStatBrkComp,0,str55
			setitem	Nord01ECStatBrkCntName,0,BRCNTCT
.START PATCH 3.71.9 ADDED LOGIC
			setitem	Nord01ECStatBrkNew,0,str10
.END PATCH 3.71.9 ADDED LOGIC
		endif
		reset	badstat
		scan	BRCREDIT,badstat
		if equal
			if (FrmPtr = "1")
				setprop	Nord001AStatBrkComp,fgcolor=red
				setprop	Nord001AStatBrkCnt,fgcolor=red
			elseif (FrmPtr = C6)
				setprop	Nord01eaStatBrkComp,fgcolor=red
				setprop	Nord01eaStatBrkCntName,fgcolor=red
			elseif (FrmPtr = C8)
				setprop	Nord01ECStatBrkComp,fgcolor=red
				setprop	Nord01ECStatBrkCntName,fgcolor=red
			endif
		elseif (BRCREDIT = "W")
			if (FrmPtr = "1")
				setprop	Nord001AStatBrkComp,fgcolor=yellow
				setprop	Nord001AStatBrkCnt,fgcolor=yellow
			elseif (FrmPtr = C6)
				setprop	Nord01eaStatBrkComp,fgcolor=yellow
				setprop	Nord01eaStatBrkCntName,fgcolor=yellow
			elseif (FrmPtr = C8)
				setprop	Nord01ECStatBrkComp,fgcolor=yellow
				setprop	Nord01ECStatBrkCntName,fgcolor=yellow
			endif
		else
.START PATCH 3.72.1 REPLACED LOGIC
.			move	NBRKFLD,NBRK2FLD
.			move	"O.LoadBroker-NBRK2KEY",Location
.			pack	KeyLocation,"Key: ",NBRK2FLD
.			call	NBRK2KEY
.			if not over
.				call	Trim using BRK2NOTES
.			else
.				clear	BRK2NOTES
.			endif
.			if (BRK2NOTES <> "")
			move	COMPNUM to COMPNOTEFLD
			move	"O.LoadBroker-COMPNOTEKEY",Location
			pack	KeyLocation,"Key: ",COMPNOTEFLD
			call	COMPNOTEKEY
			if not	over
				call	Trim using COMPNOTES
			else
				clear	COMPNOTES
			endif
			if (COMPNOTES <> "")
.END PATCH 3.72.1 REPLACED LOGIC
				if (FrmPtr = "1")
					setprop	Nord001AStatBrkComp,fgcolor=orange
					setprop	Nord001AStatBrkCnt,fgcolor=orange
				elseif (FrmPtr = C6)
					setprop	Nord01eaStatBrkComp,fgcolor=orange
					setprop	Nord01eaStatBrkCntName,fgcolor=orange
				elseif (FrmPtr = C8)
					setprop	Nord01ECStatBrkComp,fgcolor=orange
					setprop	Nord01ECStatBrkCntName,fgcolor=orange
				endif
			else
				if (FrmPtr = "1")
					setprop	Nord001AStatBrkComp,fgcolor=black
					setprop	Nord001AStatBrkCnt,fgcolor=black
				elseif (FrmPtr = C6)
					setprop	Nord01eaStatBrkComp,fgcolor=black
					setprop	Nord01eaStatBrkCntName,fgcolor=black
				elseif (FrmPtr = C8)
					setprop	Nord01ECStatBrkComp,fgcolor=black
					setprop	Nord01ECStatBrkCntName,fgcolor=black
				endif
			endif
		endif
	endif
	return
OrderLoadList LRoutine FrmPtr
.START PATCH 3.72 ADDED LOGIC
	if (FrmPtr = C1)
		Nord01A2ListViewSelect.DeleteAllItems giving N9
		if (NDATFLD <> str7)
.str7 established in GotFocus_Nord001AEditList
.NDATFLD established in LostFocus_Nord001AEditList
.START PATCH 3.77 REPLACED LOGIC
.			Nord01A1ListViewRef1.DeleteAllItems giving N9
.			Nord01A1ListViewRef2.DeleteAllItems giving N9
			if (LRefFlag = C0)
				Nord01A1ListViewRef1.DeleteAllItems giving N9
				Nord01A1ListViewRef2.DeleteAllItems giving N9
			endif
.END PATCH 3.77 REPLACED LOGIC
			setitem	Nord001AStatListSelUniverse,0,""
			setitem	Nord001AStatListSelPrice,0,""
			setitem	Nord001AStatListSelCharge,0,""
			setitem	Nord001AStatRefPrice,0,""
			call	OrderAddTotTotal
		endif
	elseif (FrmPtr = C8)
		Nord01ECListViewSelect.DeleteAllItems giving N9
		if (NDATFLD <> str7)
.str7 established in GotFocus_Nord001AEditList
.NDATFLD established in LostFocus_Nord001AEditList
.START PATCH 3.77 REPLACED LOGIC
.			Nord08A1ListViewRef1.DeleteAllItems giving N9
.			Nord08A1ListViewRef2.DeleteAllItems giving N9
			if (LRefFlag = C0)
				Nord08A1ListViewRef1.DeleteAllItems giving N9
				Nord08A1ListViewRef2.DeleteAllItems giving N9
			endif
.END PATCH 3.77 REPLACED LOGIC
			setitem	Nord01ECStatUniverse2,0,""
			setitem	Nord01ECStatPrice2,0,""
			setitem	Nord01ECStatSelPrice,0,""
			setitem	Nord01ECStatRefPrice,0,""
			call	OrderAddTotTotal8
		endif
	elseif (FrmPtr = C10)
		NSTA001AListViewSelect.DeleteAllItems giving N9
	endif
.END PATCH 3.72 ADDED LOGIC
	move	"O.LoadList-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	move	C1,NDATPATH
	call	NDATKEY
	if over
		if (FrmPtr = C1)
			setitem	Nord001AStatListName,0,""
			setitem	nord001CStatListName,0,""
			setitem	Nord001bStatListName,0,""
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatListName,0,""
		endif
	else
		if (FrmPtr = C1)
			setitem	Nord001AStatListName,0,OLSTNAME
			setitem	nord001CStatListName,0,OLSTNAME
			setitem	Nord001bStatListName,0,OLSTNAME
.START PATCH 3.72 ADDED LOGIC
			if (NDATFLD <> str7)
.str7 established in GotFocus_Nord001AEditList
.NDATFLD established in LostFocus_Nord001AEditList
				call	OrderLoadSelectRef using C1
.START PATCH 3.77 9/27/05 ASH ADDED TEST LOGIC
				if (LRefFlag = C0)
.END PATCH 3.77 9/27/05 ASH ADDED TEST LOGIC
					if (LSTNUM = HoldList)
						pack	NSEL3FLD1,"01X1",HoldLR
						move	"O.LoadList1-NSEL3AIM",Location
						pack	KeyLocation,"Key: ",NSEL3FLD1
						call	NSEL3AIM
						loop
							until over
							if (NSEL3CODE = "A")
								pack	NADDFLD,LSTNUM,NSEL3NUM
								move	"O.LoadList1-NADDKEY",Location
								pack	KeyLocation,"Key: ",NADDFLD
								call	NADDKEY
								if not over
.START PATCH 3.74 ADDED LOGIC
									move	NSEL3PRICE,NADDPRICE
.END PATCH 3.74 ADDED LOGIC
									call	OrderLoadRefAddressing using Nord01A1ListViewRef2
								endif
							elseif (NSEL3CODE = "L")
								pack	NSLTFLD,LSTNUM,NSEL3NUM
								move	"O.LoadList1-NSLTKEY",Location
								pack	KeyLocation,"Key: ",NSLTFLD
								call	NSLTKEY
								if not over
.START PATCH 3.74 ADDED LOGIC
									move	NSEL3PRICE,NSLTPRICE
.END PATCH 3.74 ADDED LOGIC
									call	OrderLoadRefSelection using Nord01A1ListViewRef2
								endif
							endif
							move	"O.LoadList1-NSEL3KG",Location
							call	NSEL3KG
						repeat
						call	OrderAddRefTotal
					endif
.START PATCH 3.77 9/27/05 ASH ADDED TEST LOGIC
				endif
.END PATCH 3.77 9/27/05 ASH ADDED TEST LOGIC
			endif
.END PATCH 3.72 ADDED LOGIC
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatListName,0,OLSTNAME
.START PATCH 3.72 ADDED LOGIC
			if (NDATFLD <> str7)
.str7 established in GotFocus_Nord01ECEditList
.NDATFLD established in LostFocus_Nord01ECEditList
				call	OrderLoadSelectRef using C8
				if (LSTNUM = Hold3List)
					pack	NSEL3FLD1,"01X2",Hold3LOL
					move	"O.LoadList8-NSEL3AIM",Location
					pack	KeyLocation,"Key: ",NSEL3FLD1
					call	NSEL3AIM
					loop
						until over
						if (NSEL3CODE = "A")
							pack	NADDFLD,LSTNUM,NSEL3NUM
							move	"O.LoadList8-NADDKEY",Location
							pack	KeyLocation,"Key: ",NADDFLD
							call	NADDKEY
							if not over
.START PATCH 3.74 ADDED LOGIC
								move	NSEL3PRICE,NADDPRICE
.END PATCH 3.74 ADDED LOGIC
								call	OrderLoadRefAddressing using Nord08A1ListViewRef2
							endif
						elseif (NSEL3CODE = "L")
							pack	NSLTFLD,LSTNUM,NSEL3NUM
							move	"O.LoadList8-NSLTKEY",Location
							pack	KeyLocation,"Key: ",NSLTFLD
							call	NSLTKEY
							if not over
.START PATCH 3.74 ADDED LOGIC
								move	NSEL3PRICE,NSLTPRICE
.END PATCH 3.74 ADDED LOGIC
								call	OrderLoadRefSelection using Nord08A1ListViewRef2
							endif
						endif
						move	"O.LoadList8-NSEL3KG",Location
						call	NSEL3KG
					repeat
					call	OrderAddRefTotal8
				endif
			endif
		elseif (FrmPtr = C10)
			call	OrderLoadSelectRef using C10
.END PATCH 3.72 ADDED LOGIC
		endif
	endif
	return

.START PATCH 3.72 ADDED LOGIC
OrderLoadSelectRef Routine FrmPtr
.START PATCH 3.72.8 ADDED LOGIC
	call	Trim using LSTNUM
	if (LSTNUM = "")
		return
	endif
.END PATCH 3.72.8 ADDED LOGIC
	pack	NSELFLD1,"01X",LSTNUM
	clear	NSELFLD2
	clear	NSELFLD3
	move	"O.LoadRef-NSELAIM",Location
	pack	KeyLocation,"Key: ",NSELFLD1
	call	NSELAIM
	loop
		until over
		call	OrderLoadSelectListView using FrmPtr
		move	"O.LoadRef-NSELKG",Location
		pack	KeyLocation,"Key: ",NSELFLD1
		call	NSELKG
	repeat
.START PATCH 3.77 ADDED LOGIC
	if (LRefFlag = C1)
		return
	endif
.END PATCH 3.77 ADDED LOGIC
.
	if (FrmPtr = 1)
		Nord01A1ListViewRef1.DeleteAllItems giving N9		.Initialize object
		Nord01A1ListViewRef2.DeleteAllItems giving N9
	elseif (FrmPtr = 8)
		Nord08A1ListViewRef1.DeleteAllItems giving N9		.Initialize object
		Nord08A1ListViewRef2.DeleteAllItems giving N9
	else
		return		.Screen 10 does not load Pricing information yet
	endif
.	call	LoadDataListViewRefHeaders
.ADDRESSING
	pack	NADDFLD1,"01X",LSTNUM
	move	"O.LoadRef-NADDAIM",Location
	pack	KeyLocation,"Key: ",NADDFLD1
	call	NADDAIM
	loop
		until over
		if (FrmPtr = 1)
			call	OrderLoadRefAddressing using Nord01A1ListViewRef1
		elseif (FrmPtr = 8)
			call	OrderLoadRefAddressing using Nord08A1ListViewRef1
		endif
		move	"O.LoadRef-NADDKG",Location
		pack	KeyLocation,"Key: ",NADDFLD1
		call	NADDKG
	repeat
.SELECT
	pack	NSLTFLD1,"01X",LSTNUM
	move	"O.LoadRef-NSLTAIM",Location
	pack	KeyLocation,"Key: ",NSLTFLD1
	call	NSLTAIM
	loop
		until over
		if (FrmPtr = 1)
			call	OrderLoadRefSelection using Nord01A1ListViewRef1
		elseif (FrmPtr = 8)
			call	OrderLoadRefSelection using Nord08A1ListViewRef1
		endif
		move	"O.LoadRef-NSLTKG",Location
		pack	KeyLocation,"Key: ",NSLTFLD1
		call	NSLTKG
	repeat
..ARRANGEMENT
.			pack	NARRFLD1,"01X",LSTNUM
.			move	"O.LoadRef-NARRAIM",Location
.			pack	KeyLocation,"Key: ",NARRFLD1
.			call	NARRAIM
.			loop
.				until over
.				call	OrderLoadRefArrangement
.				move	"O.LoadRef-NARRKG",Location
.				pack	KeyLocation,"Key: ",NARRFLD1
.				call	NARRKG
.			repeat
..SOURCE
.			pack	NSRCFLD1,"01X",LSTNUM
.			move	"O.LoadRef-NSRCAIM",Location
.			pack	KeyLocation,"Key: ",NSRCFLD1
.			call	NSRCAIM
.			loop
.				until over
.				call	OrderLoadRefSource
.				move	"O.LoadRef-NSRCKG",Location
.				pack	KeyLocation,"Key: ",NSRCFLD1
.				call	NSRCKG
.			repeat
..CATEGORY
.			pack	NCATFLD1,"01X",LSTNUM
.			move	"O.LoadRef-NCATAIM",Location
.			pack	KeyLocation,"Key: ",NCATFLD1
.			call	NCATAIM
.			loop
.				until over
.				call	OrderLoadRefCategory
.				move	"O.LoadRef-NCATKG",Location
.				pack	KeyLocation,"Key: ",NCATFLD1
.				call	NCATKG
.			repeat
.Highlight top item
	if (FrmPtr = 1)
		Nord01A1ListViewRef1.GetItemCount giving result
	elseif (FrmPtr = 8)
		Nord08A1ListViewRef1.GetItemCount giving result
	endif
	sub	C1,result
	for howmany,C0,result
		if (FrmPtr = 1)
			Nord01A1ListViewRef1.GetItemText giving str4 using howmany
		elseif (FrmPtr = 8)
			Nord08A1ListViewRef1.GetItemText giving str4 using howmany
		endif
		call	Trim using str4
		count	N1,str4
		if (N1 > 3)
			break
		endif
	repeat
	if (howmany > result)
		move	C0,howmany
	endif
	if (FrmPtr = 1)
		Nord01A1ListViewRef1.SetItemState giving N9 using howmany,2,2
		Nord01A1ListViewRef1.EnsureVisible using howmany,0
	elseif (FrmPtr = 8)
		Nord08A1ListViewRef1.SetItemState giving N9 using howmany,2,2
		Nord08A1ListViewRef1.EnsureVisible using howmany,0
	endif
	return

OrderSelectCaption Routine FrmPtr1
	setprop	NORD01A1,caption=FrmPtr1
	return

Order8SelectCaption Routine FrmPtr1
	setprop	NORD08A1,caption=FrmPtr1
	return

OrderSetSelectDefault
.Position Select Form in Default Position
	getprop	Nord001AEditExchangeQty,top=T1
	getprop	Nord001AComboEOF,left=L1
	add	"50",T1		.height of this object
        setprop NORD01A1,winpos=1
        getprop NORD0001,top=H,left=V
        add     T1,H,SelTop
.        add     "50",SelTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
        add     L1,V,SelLeft
        setprop NORD01A1,top=SelTop,left=SelLeft		.Default
	move	SelTop,SelTopC
	move	SelLeft,SelLeftC
	return

OrderSetSelectDefault2
.Position Select Form in Default Position
	getprop	Nord001AEditListSel,top=T1,left=L1
	add	"20",T1		.height of this object
        setprop NORD01A2,winpos=1
        getprop NORD0001,top=H,left=V
        add     T1,H,Sel2Top
        add     "50",Sel2Top          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
        add     L1,V,Sel2Left
        setprop NORD01A2,top=Sel2Top,left=Sel2Left		.Default
	return

OrderSetSelectDefault8
.Position Select Form in Default Position
.	getprop	Nord01ECListViewPackages,top=T1,left=L1
	getprop	Nord01ECEditSpecial,top=T1,left=L1
	add	"50",T1		.height of this object
        setprop NORD08A1,winpos=1
        getprop NORD0001,top=H,left=V
        add     T1,H,Sel8Top
        add     L1,V,Sel8Left
        setprop NORD08A1,top=Sel8Top,left=Sel8Left		.Default
	move	Sel8Top,Sel8TopC
	move	Sel8Left,Sel8LeftC
	return

SetfocusOrder1EditListSel
	setfocus Nord001AEditListSel
	return

LostFocusOrder1EditListSel
	getitem	Nord001AEditListSel,0,taskname
	if (taskname <> "")
		rep	lowup,taskname
		move	C0,N1
		Nord01A2ListViewSelect.GetItemCount giving result
		if (result > 0)
			sub	C1,result
			for howmany,C0,result
				Nord01A2ListViewSelect.GetItemText giving taskname2 using howmany,1
				call	Trim using taskname2
				rep	lowup,taskname2
				if (taskname = taskname2)
					move	C1,N1
					break
				endif
			repeat
		endif
		if (N1 = C0)
			setprop	Nord001AStatListSel,fgcolor=red
		endif
	endif
	return

LostFocusOrder8EditListSel
	getitem	Nord01ECEditListSel,0,taskname
	if (taskname <> "")
		rep	lowup,taskname
		move	C0,N1
		Nord01ECListViewSelect.GetItemCount giving result
		if (result > 0)
			sub	C1,result
			for howmany,C0,result
				Nord01ECListViewSelect.GetItemText giving taskname2 using howmany,1
				call	Trim using taskname2
				rep	lowup,taskname2
				if (taskname = taskname2)
					move	C1,N1
					break
				endif
			repeat
		endif
		if (N1 = C0)
			setprop	Nord01ECStatListSel,fgcolor=red
		endif
	endif
	return

LostFocusStatsEditSelect2
	getitem	NSTA001AEditSelect2,0,taskname
	if (taskname <> "")
		rep	lowup,taskname
		move	C0,N1
		NSTA001AListViewSelect.GetItemCount giving result
		if (result > 0)
			sub	C1,result
			for howmany,C0,result
				NSTA001AListViewSelect.GetItemText giving taskname2 using howmany,1
				call	Trim using taskname2
				rep	lowup,taskname2
				if (taskname = taskname2)
					move	C1,N1
					break
				endif
			repeat
		endif
		if (N1 = C0)
			setprop	NSTA001AStatSelect2,fgcolor=red
		endif
	endif
	return
.END PATCH 3.72 ADDED LOGIC

.START PATCH 3.72.4 ADDED LOGIC
OrderSetSearchKey
	setitem NORDMSK1EditSearchKey,0,OLRN
	return

OrderSetSearchDefault
.Position Search Form in Default Position
	getprop	NORDMSK1ButtonSearch,top=T1,left=L1
	add	"52",L1
	setprop	SearchForm,winpos=1
	getprop	NORD0001,top=H,left=V
	add     T1,H,SerTop
	add     "50",SerTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
	add     L1,V,SerLeft
	setprop	SearchForm,top=SerTop,left=SerLeft		.Default
	return

OrderMasterMove
	getprop	NORD0001,top=T1,left=L1
	if (T1 <> TempTop)
		move	C0,N8
		if (TempTop > T1)
			calc	N8=(TempTop-T1)
			sub	N8,SelTop
			sub	N8,Sel2Top
			sub	N8,Sel8Top
			sub	N8,SerTop
		elseif (T1 > TempTop)
			calc	N8=(T1-TempTop)
			add	N8,SelTop
			add	N8,Sel2Top
			add	N8,Sel8Top
			add	N8,SerTop
		endif
.        	add     "54",SelTop          .Compensate for Menu Bar/Title Bar + some to allow second click to make invisible
		setprop NORD01A1,top=SelTop
		setprop NORD01A2,top=Sel2Top
		setprop NORD08A1,top=Sel8Top
		setprop NORD001T,top=SerTop
	endif
	if (L1 <> TempLeft)
		move	C0,N8
		if (TempLeft > L1)
			calc	N8=(TempLeft-L1)
			sub	N8,SelLeft
			sub	N8,Sel2Left
			sub	N8,Sel8Left
			sub	N8,SerLeft
		elseif (L1 > TempLeft)
			calc	N8=(L1-TempLeft)
			add	N8,SelLeft
			add	N8,Sel2Left
			add	N8,Sel8Left
			add	N8,SerLeft
		endif
		setprop NORD01A1,Left=SelLeft
		setprop NORD01A2,Left=Sel2Left
		setprop NORD08A1,Left=Sel8Left
		setprop NORD001T,Left=SerLeft
	endif
	move	T1,TempTop
	move	L1,TempLeft
	return
.END PATCH 3.72.4 ADDED LOGIC

.START PATCH 3.72 ADDED LOGIC
Order1ListRef1DoubleClick
	getprop	Nord001AEditOrderQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	Nord01A1ListViewRef1.GetNextItem giving result using C2,N9
	if (result <> SEQ)
		move	C0,N1
		Nord01A1ListViewRef1.GetItemText giving taskname using result,4
		unpack	taskname,str6,NREFDESC,str25,str11
		Nord01A1ListViewRef2.GetItemCount giving howmany
		if (howmany > 0)
			sub	C1,howmany
			for N4,"0",howmany
				Nord01A1ListViewRef2.GetItemText giving str10 using N4,0
				if (str10 = str6)
					move	C1,N1
					break
				endif
			repeat
		endif
		if (N1 = C0)
			call	Trim using str25
			call	Trim using str11
			Nord01A1ListViewRef2.InsertItem giving N9 using str6
			Nord01A1ListViewRef2.SetItemText using N9,NREFDESC,1
			Nord01A1ListViewRef2.SetItemText using N9,str25,2
			Nord01A1ListViewRef2.SetItemText using N9,str11,3
			Nord01A1ListViewRef2.SetItemText using N9,taskname,4
			call	OrderAddRefTotal
		endif
	endif
	return

Order1ListViewRef1MouseDown
        deleteitem Nord01A1DataList1,0
	getprop	Nord001AEditOrderQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	loop
		Nord01A1ListViewRef1.GetNextItem giving result using C2,N9
		until (result = SEQ)
		move	result,N9
		Nord01A1ListViewRef1.GetItemText giving taskname using result,4
		insertitem Nord01A1DataList1,9999,taskname
	repeat
	return

Order1ListViewRef1MouseUp
        deleteitem Nord01A1DataList1,0
	getprop	Nord001AEditOrderQty,enabled=result
	if (result =0)
		return
	endif
	Nord01A1DataList2.GetCount giving result
	if (result > 0)
		sub	C1,result
		for N9,"0",result
			move	C0,N1
			Nord01A1DataList2.GetText giving NREFSTR using N9
			Nord01A1ListViewRef2.GetItemCount giving howmany
			if (howmany > 0)
				sub	C1,howmany
				for N4,"0",howmany
					Nord01A1ListViewRef2.GetItemText giving str10 using N4,0
					if (str10 = NREFSTR)
						Nord01A1ListViewRef2.DeleteItem using N4
						sub	C1,N4
						sub	C1,howmany
					endif
				repeat
			endif
		repeat
	endif
	call	OrderAddRefTotal
	clear	NREFSTR
        deleteitem Nord01A1DataList2,0
	return

Order1ListViewRef2DoubleClick
	getprop	Nord001AEditOrderQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	Nord01A1ListViewRef2.GetNextItem giving result using C2,N9
	if (result <> SEQ)
		Nord01A1ListViewRef2.DeleteItem using result
		call	OrderAddRefTotal
	endif
	return

Order1ListViewRef2MouseDown
        deleteitem Nord01A1DataList2,0
	getprop	Nord001AEditOrderQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	loop
		Nord01A1ListViewRef2.GetNextItem giving result using C2,N9
		until (result = SEQ)
		move	result,N9
		Nord01A1ListViewRef2.GetItemText giving NREFSTR using result,0
		insertitem Nord01A1DataList2,9999,NREFSTR
	repeat
	return

Order1ListViewRef2MouseUp
        deleteitem Nord01A1DataList2,0
	getprop	Nord001AEditOrderQty,enabled=result
	if (result =0)
		return
	endif
	Nord01A1DataList1.GetCount giving result
	if (result > 0)
		sub	C1,result
		for N9,"0",result
			move	C0,N1
			Nord01A1DataList1.GetText giving NREFSTR using N9
			unpack	NREFSTR,str6,NREFDESC,str25,str11
			Nord01A1ListViewRef2.GetItemCount giving howmany
			if (howmany > 0)
				sub	C1,howmany
				for N4,"0",howmany
					Nord01A1ListViewRef2.GetItemText giving str10 using N4,0
					if (str10 = str6)
						move	C1,N1
						break
					endif
				repeat
			endif
			if (N1 = C0)
				call	Trim using str25
				call	Trim using str11
				Nord01A1ListViewRef2.InsertItem giving N9 using str6
				Nord01A1ListViewRef2.SetItemText using N9,NREFDESC,1
				Nord01A1ListViewRef2.SetItemText using N9,str25,2
				Nord01A1ListViewRef2.SetItemText using N9,str11,3
				Nord01A1ListViewRef2.SetItemText using N9,NREFSTR,4
				call	OrderAddRefTotal
			endif
		repeat
	endif
	clear	NREFSTR
        deleteitem Nord01A1DataList1,0
	return

OrderAddRefTotal
	move	C0,N52
	Nord01A1ListViewRef2.GetItemCount giving howmany
	if (howmany > 0)
		sub	C1,howmany
		for N4,"0",howmany
			Nord01A1ListViewRef2.GetItemText giving taskname using N4,4
			unpack	taskname,str7,str45,str1,str25,str11,str8
			call	Trim using str8
			type	str8
			if equal
				move	C0,N52B
				move	str8,N52B
				add	N52B,N52
			endif
		repeat
	endif
	unpack	N52,str5,str3
	call	FormatNumeric using str5,str6
	pack	str9,str6,str3
	setitem	Nord001AStatRefPrice,0,str9
OrderAddTotTotal
	move	C0,N52
	move	C0,N52A
	move	C0,N52B
	getitem	Nord001AEditListSelPrice,0,str11
	call	Trim using str11
	if (str11 <> "")
		call	RemoveChar using str11,COMMA
		move	str11,N52
	endif
	getitem	Nord001AEditListSelCharge,0,str11
	call	Trim using str11
	if (str11 <> "")
		call	RemoveChar using str11,COMMA
		move	str11,N52A
	endif
	add	N52A,N52
	getitem	Nord001AStatRefPrice,0,str11
	call	Trim using str11
	if (str11 <> "")
		call	RemoveChar using str11,COMMA
		move	str11,N52B
	endif
	add	N52,N52B
	unpack	N52B,str5,str3
	call	FormatNumeric using str5,str6
	pack	str9,str6,str3
	setitem	Nord001AStatTotPrice,0,str9
	return

Order8ListRef1DoubleClick
	getprop	Nord01ECEditGrossQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	Nord08A1ListViewRef1.GetNextItem giving result using C2,N9
	if (result <> SEQ)
		move	C0,N1
		Nord08A1ListViewRef1.GetItemText giving taskname using result,4
		unpack	taskname,str6,NREFDESC,str25,str11
		Nord08A1ListViewRef2.GetItemCount giving howmany
		if (howmany > 0)
			sub	C1,howmany
			for N4,"0",howmany
				Nord08A1ListViewRef2.GetItemText giving str10 using N4,0
				if (str10 = str6)
					move	C1,N1
					break
				endif
			repeat
		endif
		if (N1 = C0)
			call	Trim using str25
			call	Trim using str11
			Nord08A1ListViewRef2.InsertItem giving N9 using str6
			Nord08A1ListViewRef2.SetItemText using N9,NREFDESC,1
			Nord08A1ListViewRef2.SetItemText using N9,str25,2
			Nord08A1ListViewRef2.SetItemText using N9,str11,3
			Nord08A1ListViewRef2.SetItemText using N9,taskname,4
			call	OrderAddRefTotal8
		endif
	endif
	return

Order8ListViewRef1MouseDown
        deleteitem Nord08A1Datalist1,0
	getprop	Nord01ECEditGrossQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	loop
		Nord08A1ListViewRef1.GetNextItem giving result using C2,N9
		until (result = SEQ)
		move	result,N9
		Nord08A1ListViewRef1.GetItemText giving taskname using result,4
		insertitem Nord08A1Datalist1,9999,taskname
	repeat
	return

Order8ListViewRef1MouseUp
        deleteitem Nord08A1Datalist1,0
	getprop	Nord01ECEditGrossQty,enabled=result
	if (result =0)
		return
	endif
	Nord08A1Datalist2.GetCount giving result
	if (result > 0)
		sub	C1,result
		for N9,"0",result
			move	C0,N1
			Nord08A1Datalist2.GetText giving NREFSTR using N9
			Nord08A1ListViewRef2.GetItemCount giving howmany
			if (howmany > 0)
				sub	C1,howmany
				for N4,"0",howmany
					Nord08A1ListViewRef2.GetItemText giving str10 using N4,0
					if (str10 = NREFSTR)
						Nord08A1ListViewRef2.DeleteItem using N4
						sub	C1,N4
						sub	C1,howmany
					endif
				repeat
			endif
		repeat
	endif
	call	OrderAddRefTotal8
	clear	NREFSTR
        deleteitem Nord08A1Datalist2,0
	return

Order8ListViewRef2DoubleClick
	getprop	Nord01ECEditGrossQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	Nord08A1ListViewRef2.GetNextItem giving result using C2,N9
	if (result <> SEQ)
		Nord08A1ListViewRef2.DeleteItem using result
		call	OrderAddRefTotal8
	endif
	return

Order8ListViewRef2MouseDown
        deleteitem Nord08A1Datalist2,0
	getprop	Nord01ECEditGrossQty,enabled=result
	if (result =0)
		return
	endif
	move	SEQ,result
	move	result,N9
	loop
		Nord08A1ListViewRef2.GetNextItem giving result using C2,N9
		until (result = SEQ)
		move	result,N9
		Nord08A1ListViewRef2.GetItemText giving NREFSTR using result,0
		insertitem Nord08A1Datalist2,9999,NREFSTR
	repeat
	return

Order8ListViewRef2MouseUp
        deleteitem Nord08A1DataList2,0
	getprop	Nord01ECEditGrossQty,enabled=result
	if (result =0)
		return
	endif
	Nord08A1Datalist1.GetCount giving result
	if (result > 0)
		sub	C1,result
		for N9,"0",result
			move	C0,N1
			Nord08A1Datalist1.GetText giving NREFSTR using N9
			unpack	NREFSTR,str6,NREFDESC,str25,str11
			Nord08A1ListViewRef2.GetItemCount giving howmany
			if (howmany > 0)
				sub	C1,howmany
				for N4,"0",howmany
					Nord08A1ListViewRef2.GetItemText giving str10 using N4,0
					if (str10 = str6)
						move	C1,N1
						break
					endif
				repeat
			endif
			if (N1 = C0)
				call	Trim using str25
				call	Trim using str11
				Nord08A1ListViewRef2.InsertItem giving N9 using str6
				Nord08A1ListViewRef2.SetItemText using N9,NREFDESC,1
				Nord08A1ListViewRef2.SetItemText using N9,str25,2
				Nord08A1ListViewRef2.SetItemText using N9,str11,3
				Nord08A1ListViewRef2.SetItemText using N9,NREFSTR,4
				call	OrderAddRefTotal8
			endif
		repeat
	endif
	clear	NREFSTR
        deleteitem Nord08A1Datalist1,0
	return

OrderAddRefTotal8
	move	C0,N52
	Nord08A1ListViewRef2.GetItemCount giving howmany
	if (howmany > 0)
		sub	C1,howmany
		for N4,"0",howmany
			Nord08A1ListViewRef2.GetItemText giving taskname using N4,4
			unpack	taskname,str7,str45,str1,str25,str11,str8
			call	Trim using str8
			type	str8
			if equal
				move	C0,N52B
				move	str8,N52B
				add	N52B,N52
			endif
		repeat
	endif
	unpack	N52,str5,str3
	call	FormatNumeric using str5,str6
	pack	str9,str6,str3
	setitem	Nord01ECStatRefPrice,0,str9
OrderAddTotTotal8
	move	C0,N52
	move	C0,N52A
	move	C0,N52B
	getitem	Nord01ECEditPrice,0,str11
	call	Trim using str11
	if (str11 <> "")
		call	RemoveChar using str11,COMMA
		move	str11,N52
	endif
	getitem	Nord01ECEditSelPrice,0,str11
	call	Trim using str11
	if (str11 <> "")
		call	RemoveChar using str11,COMMA
		move	str11,N52A
	endif
	add	N52A,N52
	getitem	Nord01ECStatRefPrice,0,str11
	call	Trim using str11
	if (str11 <> "")
		call	RemoveChar using str11,COMMA
		move	str11,N52B
	endif
	add	N52,N52B
	unpack	N52B,str5,str3
	call	FormatNumeric using str5,str6
	pack	str9,str6,str3
	setitem	Nord01ECStatTotPrice,0,str9
	return

OrderLoadSelectListView Routine FrmPtr
	pack	taskname,NSELVARS
	call	FormatNumeric using NSELQTY,str13

	if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
		move	"Exc. Only",str25
	else
		unpack	NSELPRICE,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
		pack	NMODFLD,NSELDESC
		rep	zfill,NMODFLD
		move	"O.LoadList-NMODKEY",Location
		pack	KeyLocation,"Key: ",NMODFLD
		call	NMODKEY
		call	Trim using NMODDESC
		if (NSELBASE = "BASE" | NSELBASE = "SEC." | NSELBASE = "    ")
			clear	str1
		else			.if (NSELPRICE > 0)
			move	"+",str1
.		else
.			clear	str1
		endif
		pack	str25,str1,str9,NMODDESC
	endif
.Establish Foreground Color
	if (NSELINACTIVE = "1")
		move	"0xFF0000",colordim		.Red
	elseif (NSELSTATUS = "1")
		move	"0x0000FF",colordim		.Blue
	else
		move	"0x000000",colordim		.Black
	endif
.Establish Background Color
	if (NSELBASE = "BASE")
		move	"0xFFFF00",colordim2		.Yellow
	elseif (NSELBASE = "SEC.")
		move	"0xFFFFC0",colordim2		.Pale Yellow
	else
		move	"0xFFFFFF",colordim2		.White
	endif
	move	C0,N4
	move	NSELINDEX,N4
	if (N4 = C0)
		move	"9999",NSELINDEX
	else
		move	N4,NSELINDEX
		rep	zfill,NSELINDEX
	endif
	if (FrmPtr = 1)
		Nord01A2ListViewSelect.InsertItem giving N9 using NSELINDEX
		Nord01A2ListViewSelect.SetItemText using N9,NSELSNAME,1
		Nord01A2ListViewSelect.SetItemText using N9,str25,2
		Nord01A2ListViewSelect.SetItemText using N9,str13,3
		Nord01A2ListViewSelect.SetItemText using N9,taskname,4
		Nord01A2ListViewSelect.SetItemText using N9,colordim,5
		Nord01A2ListViewSelect.SetItemText using N9,colordim2,6
	elseif (FrmPtr = 8)
		Nord01ECListViewSelect.InsertItem giving N9 using NSELINDEX
		Nord01ECListViewSelect.SetItemText using N9,NSELSNAME,1
		Nord01ECListViewSelect.SetItemText using N9,str25,2
		Nord01ECListViewSelect.SetItemText using N9,str13,3
		Nord01ECListViewSelect.SetItemText using N9,taskname,4
		Nord01ECListViewSelect.SetItemText using N9,colordim,5
		Nord01ECListViewSelect.SetItemText using N9,colordim2,6
	elseif (FrmPtr = 10)
		NSTA001AListViewSelect.InsertItem giving N9 using NSELINDEX
		NSTA001AListViewSelect.SetItemText using N9,NSELSNAME,1
		NSTA001AListViewSelect.SetItemText using N9,str25,2
		NSTA001AListViewSelect.SetItemText using N9,str13,3
		NSTA001AListViewSelect.SetItemText using N9,taskname,4
		NSTA001AListViewSelect.SetItemText using N9,colordim,5
		NSTA001AListViewSelect.SetItemText using N9,colordim2,6
	endif
	return

OrderLoadRefAddressing Routine LstVwPtr
.	pack	taskname,NADDVARS
	pack	str7,"AAA",NADDNUM
	LstVwPtr.InsertItem giving N10 using str7
	pack	NREFFLD,"A",NADDNUM
	move	"D.Load1-NREFKEY",Location
	pack	KeyLocation,"Key: ",NREFFLD
	call	NREFKEY
	call	Trim using NREFDESC
	LstVwPtr.SetItemText using N10,NREFDESC,1
	if (NADDPRICE = C0)
		clear	str9
	else
		unpack	NADDPRICE,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
	endif
	pack	NMODFLD,NADDDESC
	rep	zfill,NMODFLD
	move	"D.Load2-NMODKEY",Location
	pack	KeyLocation,"Key: ",NMODFLD
	call	NMODKEY
	call	Trim using NMODDESC
	pack	str25,str9,NMODDESC
	LstVwPtr.SetItemText using N10,str25,2
	packkey	NREFDESC,NREFDESC
	packkey	str25,str25
	pack	taskname,str7,NREFDESC,str25,"           ",NADDPRICE
	LstVwPtr.SetItemText using N10,taskname,4
	return

OrderLoadRefSelection Routine LstVwPtr
.	pack	taskname,NSLTVARS
	pack	str7,"LLL",NSLTNUM
	LstVwPtr.InsertItem giving N10 using str7
	pack	NREFFLD,"L",NSLTNUM
	move	"D.Load2-NREFKEY",Location
	pack	KeyLocation,"Key: ",NREFFLD
	call	NREFKEY
	call	Trim using NREFDESC
	LstVwPtr.SetItemText using N10,NREFDESC,1
	if (NSLTPRICE = C0)
		clear	str9
	else
		unpack	NSLTPRICE,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
	endif
	pack	NMODFLD,NSLTDESC
	rep	zfill,NMODFLD
	move	"D.Load3-NMODKEY",Location
	pack	KeyLocation,"Key: ",NMODFLD
	call	NMODKEY
	call	Trim using NMODDESC
	pack	str25,str9,NMODDESC
	LstVwPtr.SetItemText using N10,str25,2
	call	FormatNumeric using NSLTQTY,str11
	LstVwPtr.SetItemText using N10,str11,3
	packkey	NREFDESC,NREFDESC
	packkey	str25,str25
	packkey	str11,str11
	pack	taskname,str7,NREFDESC,str25,str11,NSLTPRICE
	LstVwPtr.SetItemText using N10,taskname,4
	return

OrderLoadRefArrangement Routine LstVwPtr
.CURRENTLY INACTIVE
.	pack	taskname,NARRVARS
	pack	str7,"RRR",NARRNUM
	LstVwPtr.InsertItem giving N10 using str7
	pack	NREFFLD,"R",NARRNUM
	move	"D.Load4-NREFKEY",Location
	pack	KeyLocation,"Key: ",NREFFLD
	call	NREFKEY
	call	Trim using NREFDESC
	LstVwPtr.SetItemText using N10,NREFDESC,1
	packkey	NREFDESC,NREFDESC
	pack	taskname,str7,NREFDESC
	LstVwPtr.SetItemText using N10,taskname,4
	return

OrderLoadRefSource Routine LstVwPtr
.CURRENTLY INACTIVE
.	pack	taskname,NSRCVARS
	pack	str7,"SSS",NSRCNUM
	LstVwPtr.InsertItem giving N10 using str7
	pack	NREFFLD,"S",NSRCNUM
	move	"D.Load5-NREFKEY",Location
	pack	KeyLocation,"Key: ",NREFFLD
	call	NREFKEY
	call	Trim using NREFDESC
	LstVwPtr.SetItemText using N10,NREFDESC,1
	move	C0,N3
	move	NSRCPER,N3
	if (N3 > C0)
		move	N3,NSRCPER
		call	Trim using NSRCPER
		pack	str4,NSRCPER,PRC
		LstVwPtr.SetItemText using N10,str4,2
	else
		move	"    ",str4
	endif
	packkey	NREFDESC,NREFDESC
	packkey	str4,str4
	pack	taskname,str7,NREFDESC,str4
	LstVwPtr.SetItemText using N10,taskname,4
	return

OrderLoadRefCategory Routine LstVwPtr
.CURRENTLY INACTIVE
.	pack	taskname,NCATVARS
	pack	str7,"TTT",NCATCODE,NCATNUM
	LstVwPtr.InsertItem giving N10 using str7
	pack	NREFFLD,"T",NCATCODE,NCATNUM
	move	"D.Load6-NREFKEY",Location
	pack	KeyLocation,"Key: ",NREFFLD
	call	NREFKEY
	call	Trim using NREFDESC
	LstVwPtr.SetItemText using N10,NREFDESC,1
	if (NCATCODE = "B")
		move	"Business",str8
	elseif (NCATCODE = "C")
		move	"Consumer",str8
	elseif (NCATCODE = "E")
		move	"Enhanced",str8
	else
		clear	str8
	endif
	LstVwPtr.SetItemText using N10,str8,2
	packkey	NREFDESC,NREFDESC
	packkey	str8,str8
	pack	taskname,str7,NREFDESC,str8
	LstVwPtr.SetItemText using N10,taskname,4
	return

OrderLoadDefaultSelect
.Select Name
	setitem	Nord001AEditListSel,0,NSELSNAME
	setprop	Nord001AStatListSel,fgcolor=black
.Select Universe
	move	C0,N10
	move	NSELQTY,N10
	if (N10 = C0)
		clear	str13
	else
		move	N10,str10
		call	FormatNumeric using str10,str13
	endif
	setitem	Nord001AStatListSelUniverse,0,str13
	setitem	Nord001AEditSelUniverse,0,str13
.Select Price (Combined)
.	if (NSELEXC = "2"& (NSELBASE = "BASE" | NSELBASE = "SEC."))
.		move	"Exc. Only",str25
.		clear	str9
.		clear	str10
.	else
		if (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
.Select off of a Base/Secondary Base
			call	SelectTestBase4 using NSELLIST,NSELBASE,N52
.Format Bundled Select Price
			unpack	NSELPRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str10,str6,str3
		else
.Base/Secondary Base - clear bundled portion, move over Base Price.
			clear	str10
			move	C0,N52
			move	NSELPRICE,N52
		endif
		unpack	N52,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
..........................
		if (NSELEXC = "2"& (NSELBASE = "BASE" | NSELBASE = "SEC."))
			move	"Exc. Only",str25
		else
..........................
			pack	NMODFLD,NSELDESC
			rep	zfill,NMODFLD
			move	"O.LoadDefault-NMODKEY",Location
			pack	KeyLocation,"Key: ",NMODFLD
			call	NMODKEY
			call	Trim using NMODDESC
			pack	str25,str9,NMODDESC
		endif
.	endif
	setitem	Nord001AStatListSelPrice,0,str25
	setitem	Nord001AEditListSelPrice,0,str9
	setitem	Nord001AEditListSelCharge,0,str10
	setitem	Nord001AStatListSelCharge,0,str10
.
	move	C0,N3
	move	NSELDESC,N3
	add	C1,N3
	setitem	Nord001AComboListSelMod,0,N3
.
	call	OrderAddTotTotal
	return

OrderLoadDefaultSelect8
.Select Name
	setitem	Nord01ECEditListSel,0,NSELSNAME
	setprop	Nord01ECStatListSel,fgcolor=black
.Select Universe
	move	C0,N10
	move	NSELQTY,N10
	if (N10 = C0)
		clear	str13
	else
		move	N10,str10
		call	FormatNumeric using str10,str13
	endif
	setitem	Nord01ECStatUniverse2,0,str13
	setitem	Nord01ECEditUniverse,0,str13
.Select Price (Combined)
.	if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
.		move	"Exc. Only",str25
.		clear	str9
.		clear	str10
.	else
		if (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
.Select off of a Base/Secondary Base
			call	SelectTestBase4 using NSELLIST,NSELBASE,N52
.Format Bundled Select Price
			unpack	NSELPRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str10,str6,str3
		else
.Base/Secondary Base - clear bundled portion, move over Base Price.
			clear	str10
			move	C0,N52
			move	NSELPRICE,N52
		endif
		unpack	N52,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
..........................
		if (NSELEXC = "2"& (NSELBASE = "BASE" | NSELBASE = "SEC."))
			move	"Exc. Only",str25
		else
..........................
			pack	NMODFLD,NSELDESC
			rep	zfill,NMODFLD
			move	"O.LoadDefault8-NMODKEY",Location
			pack	KeyLocation,"Key: ",NMODFLD
			call	NMODKEY
			call	Trim using NMODDESC
			pack	str25,str9,NMODDESC
		endif
.	endif
	setitem	Nord01ECEditPrice,0,str9
	setitem	Nord01ECStatPrice2,0,str25
	setitem	Nord01ECEditSelPrice,0,str10
	setitem	Nord01ECStatSelPrice,0,str10
.
	move	C0,N3
	move	NSELDESC,N3
	add	C1,N3
	setitem	Nord01ECComboMod,0,N3
.
	call	OrderAddTotTotal8
	return

OrderLoadDefaultSelect10
.Select Name
	setitem	NSTA001AEditSelect2,0,NSELSNAME
	setprop	NSTA001AStatSelect2,fgcolor=black
	move	C0,N10
	move	NSELQTY,N10
	if (N10 > 0)
		move	NSELQTY,str10
		call	FormatNumeric using str10,str13
	else
		clear	str13
	endif
.	if (NSELEXC = "2" & (NSELBASE = "BASE" | NSELBASE = "SEC."))
.		clear	str9
.		clear	str10
.	else
		if (NSELBASE <> "BASE" & NSELBASE <> "SEC.")
.Select off of a Base/Secondary Base
			call	SelectTestBase4 using NSELLIST,NSELBASE,N52
.Format Bundled Select Price
			unpack	NSELPRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str10,str6,str3
		else
.Base/Secondary Base - clear bundled portion, move over Base Price.
			clear	str10
			move	C0,N52
			move	NSELPRICE,N52
		endif
		unpack	N52,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
.	endif
	setitem	NSTA001AEditSelUniverse2,0,str13
	setitem	NSTA001AEditSelPrice2,0,str9
	setitem	NSTA001AEditSelPrice3,0,str10
	return
.END PATCH 3.72 ADDED LOGIC

.START PATCH 3.72 REPLACED LOGIC
.OrderLoadListUniverse
.	move	UNIVERSE,str9
.	call	FormatNumeric using str9,str11
.	setitem	Nord001AEditListUniverse,0,str11
.	return
.END PATCH 3.72 REPLACED LOGIC
OrderLoadListColor LRoutine FrmPtr
	unpack	REVDATE,MM,str1,DD,str1,STR2,YY
	call	CVTJUL
	move	JULDAYS,howmany
	unpack	timestamp,str2,YY,MM,DD
	call	CVTJUL
	sub	howmany,JULDAYS
	if (JULDAYS > 90)
		if (FrmPtr = C1)
			setprop	Nord001AEditList,fgcolor=blue
		elseif (FrmPtr = C8)
			setprop	Nord01ECEditList,fgcolor=blue
		endif
	else
		if (FrmPtr = C1)
			setprop	Nord001AEditList,fgcolor=black
		elseif (FrmPtr = C8)
			setprop	Nord01ECEditList,fgcolor=black
		endif
	endif
	return

OrderLoadMailer	LRoutine FrmPtr
.Called	by:  OrderLoadScreens, Nord001AEditMlr_LostFocus,	Nord001AEditMlrContact_LostFocus,
.OrderLoadCampScreens, Nord01ECEditMlr_LostFocus,	OrderLoadLOLScreen, Nord01ECEditMlr_LostFocus
	clear	taskname
	move	"O.LoadMailer-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	call	NMLRKEY
	if over
		if (FrmPtr = "1")
			setitem	Nord001AStatMlrComp,0,""
			setitem	nord001CStatMlrComp,0,""
			setitem	Nord001bStatMlrComp,0,""
.START PATCH 3.74 ADDED LOGIC
			setitem	Nord001AStatMlrNew,0,""
.END PATCH 3.74 ADDED LOGIC
		elseif (FrmPtr = C6)
			setitem	Nord01eaStatMlrComp,0,""
			setitem	Nord01EBStatMlrComp,0,""
.START PATCH 3.75.7 REMOVED LOGIC
..START PATCH 3.74 ADDED LOGIC
.			setitem	Nord01eaStatMlrNew,0,""
..END PATCH 3.74 ADDED LOGIC
.END PATCH 3.75.7 REMOVED LOGIC
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatMlrComp,0,""
.START PATCH 3.74 ADDED LOGIC
.			setitem	Nord01ECStatMlrNew,0,""
.END PATCH 3.74 ADDED LOGIC
		endif
	else
.START PATCH 3.74 ADDED LOGIC
		call	Trim using COMPNUM
		if (COMPNUM <> "")
			call	Trim using CNCTID
			if (CNCTID <> "")
				pack	str10,COMPNUM,SLASH,CNCTID
			else
				pack	str10,COMPNUM
			endif
		else
			clear	str10
		endif
.END PATCH 3.74 ADDED LOGIC
.Prep for Screen3!!
		reset	badstat
		scan	MSTAT,badstat
		if equal
			if (FrmPtr = "1")
				setprop	Nord001AStatMlrComp,fgcolor=red
				setprop	nord001CStatMlrComp,fgcolor=red
				setprop	Nord001bStatMlrComp,fgcolor=red
			elseif (FrmPtr = C6)
				setprop	Nord01eaStatMlrComp,fgcolor=red
				setprop	Nord01EBStatMlrComp,fgcolor=red
       			elseif (FrmPtr = C8)
				setprop	Nord01ECStatMlrComp,fgcolor=red
			endif
		elseif (MSTAT =	"W")
			if (FrmPtr = "1")
				setprop	Nord001AStatMlrComp,fgcolor=yellow
				setprop	nord001CStatMlrComp,fgcolor=yellow
				setprop	Nord001bStatMlrComp,fgcolor=yellow
			elseif (FrmPtr = C6)
				setprop	Nord01eaStatMlrComp,fgcolor=yellow
				setprop	Nord01EBStatMlrComp,fgcolor=yellow
			elseif (FrmPtr = C8)
				setprop	Nord01ECStatMlrComp,fgcolor=yellow
			endif
		else
.START PATCH 3.74 REPLACED LOGIC
.			move	MKEY,NMLR2FLD
.			move	"O.LoadBroker-NMLR2KEY",Location
.			pack	KeyLocation,"Key: ",NMLR2FLD
.			call	NMLR2KEY
.			if not over
.				call	Trim using MLR2NOTES
.			else
.				clear	MLR2NOTES
.			endif
.			if (MLR2NOTES <> "")
			move	COMPNUM to COMPNOTEFLD
			move	"O.LoadMailer-COMPNOTEKEY",Location
			pack	KeyLocation,"Key: ",COMPNOTEFLD
			call	COMPNOTEKEY
			if not	over
				call	Trim using COMPNOTES
			else
				clear	COMPNOTES
			endif
			if (COMPNOTES <> "")
.END PATCH 3.74 REPLACED LOGIC
				if (FrmPtr = "1")
					setprop	Nord001AStatMlrComp,fgcolor=orange
					setprop	nord001CStatMlrComp,fgcolor=orange
					setprop	Nord001bStatMlrComp,fgcolor=orange
				elseif (FrmPtr = C6)
					setprop	Nord01eaStatMlrComp,fgcolor=orange
					setprop	Nord01EBStatMlrComp,fgcolor=orange
				elseif (FrmPtr = C8)
					setprop	Nord01ECStatMlrComp,fgcolor=orange
				endif
			else
				if (FrmPtr = "1")
					setprop	Nord001AStatMlrComp,fgcolor=black
					setprop	nord001CStatMlrComp,fgcolor=black
					setprop	Nord001bStatMlrComp,fgcolor=black
				elseif (FrmPtr = C6)
					setprop	Nord01eaStatMlrComp,fgcolor=black
					setprop	Nord01EBStatMlrComp,fgcolor=black
				elseif (FrmPtr = C8)
					setprop	Nord01ECStatMlrComp,fgcolor=black
				endif
			endif
		endif
		if (FrmPtr = "1")
			setitem	Nord001AStatMlrComp,0,MCOMP
			setitem	nord001CStatMlrComp,0,MCOMP
			setitem	Nord001bStatMlrComp,0,MCOMP
.START PATCH 3.74 ADDED LOGIC
			setitem	Nord001AStatMlrNew,0,str10
.END PATCH 3.74 ADDED LOGIC
		elseif (FrmPtr = C6)
			setitem	Nord01eaStatMlrComp,0,MCOMP
			setitem	Nord01EBStatMlrComp,0,MCOMP
.START PATCH 3.75.7 REMOVED LOGIC
..START PATCH 3.74 ADDED LOGIC
.			setitem	Nord01eaStatMlrNew,0,str10
..END PATCH 3.74 ADDED LOGIC
.END PATCH 3.75.7 REMOVED LOGIC
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatMlrComp,0,MCOMP
.START PATCH 3.74 ADDED LOGIC
.			setitem	Nord01ECStatMlrNew,0,str10
.END PATCH 3.74 ADDED LOGIC
		endif
.Removed as per	SA request - 10/19/00
.		 if (NewFlag <>	"S")
.			 clear	 NXRFFLD2
.			 clear	 NXRFLIST
.			 move	 MKEY,NXRFFLD2
.			 rep	 zfill,NXRFFLD2
.			 move	 C2,NXRFPATH
.			 call	 NXRFKEY
.			 if not	over
.				 move	 NXRFLIST,NDATFLD
.				 move	 C0,UNIVERSE
.				 move	 C1,NDATPATH
.				 call	 NDATKEY
.				 if not	over
.					 if (STATUS <> "W" & STATUS <> "T")
.						 append	 "Mailer has ",taskname
.						 move	 UNIVERSE,str10
.						 call	 FormatNumeric using str10,str13
.						 append	 str13,taskname
.						 append	 " names.",taskname
.						 reset	 taskname
.					 else
.						 append	 "Mailer List has been withdrawn!",taskname
.						 reset	 taskname
.					 endif
.				 endif
.				 clear	 NDATFLD
.			 endif
.		 endif
	endif
.	 if (FrmPtr = "1")
.		 setitem Nord001AStatUniverse,0,taskname
.	 elseif	(FrmPtr	= C6)
.		 setitem Nord01eaStatUniverse,0,taskname
.	 endif
	return

.Replaced as per SA request - 10/19/00
.OrderLoadUniverseInSpecial
.	 clear	 NXRFFLD2
.	 clear	 NXRFLIST
.	 getitem Nord001AEditMlr,0,str4
.	 if (str4 <> "")
.		 pack	 MKEY,str4,"000"
.		 move	 "O.LoadUniver.-NMLRKEY",Location
.		 pack	 KeyLocation,"Key: ",MKEY
.		 call	 NMLRKEY
.		 if not	over
.			 move	 MKEY,NXRFFLD2
.			 rep	 zfill,NXRFFLD2
.			 move	 C2,NXRFPATH
.			 call	 NXRFKEY
.			 if not	over
.				 move	 NXRFLIST,NDATFLD
.				 move	 C0,UNIVERSE
.				 move	 C1,NDATPATH
.				 call	 NDATKEY
.				 if not	over
.					 if (STATUS <> "W" & STATUS <> "T")
.						 clear	 taskname
.						 getitem nord001CEditSpecial,0,DESC002
.						 if (DESC002 <>	"")
.							 append	 carr,taskname
.						 endif
.						 call	 Trim using MCOMP
.						 append	 MCOMP,taskname
.						 append	 " has ",taskname
.						 move	 UNIVERSE,str10
.						 call	 FormatNumeric using str10,str13
.						 append	 str13,taskname
.						 append	 " names.",taskname
.						 reset	 taskname
.						 pack	 DESC002,DESC002,taskname
.						 setitem nord001CEditSpecial,0,DESC002
.					 endif
.				 endif
.				 clear	 NDATFLD
.			 endif
.		 endif
.	 endif
.	 return
OrderLoadUniverseInSpecial Routine DimPtr
.This routine is external, called by Info.plc.	Any modifications may affect Info.plc!!!!
.DimPtr	 = Mailer Universe
	getprop	nord001CEditSpecial,readonly=N9
	if (N9 = C0 & TabNum = 3)
 		clear	taskname
		getitem	nord001CEditSpecial,0,DESC002
		if (DESC002 <> "")
			append	carr,taskname
		endif
		getitem	nord001CStatMlrComp,0,MCOMP
		call	Trim using MCOMP
		append	MCOMP,taskname
		append	" has ",taskname
		append	DimPtr,taskname
		append	" names.",taskname
		reset	taskname
		pack	DESC002,DESC002,taskname
		setitem	nord001CEditSpecial,0,DESC002
		move	YES,SpecFlag2
	endif
	return

.START PATCH 3.71.3 ADDED LOGIC
OrderLoadOmit Routine DimPtr,DimPtr1,DimPtr2
.This routine is external, called by Info.plc.	Any modifications may affect Info.plc!!!!
.DimPtr	 = LR Number
.DimPtr1 = Order Date
.DimPtr2 = Qty
	getprop	Nord001bComboCont,enabled=N9
	if (N9 = C1)
		setitem Nord001bComboCont,0,2
		setitem	Nord001bEditContLR,0,DimPtr
		setitem	Nord001bEditContDate,0,DimPtr1
		setitem	Nord001bEditContQty,0,DimPtr2
	endif
	return
.END PATCH 3.71.3 ADDED LOGIC

OrderLoadRtn LRoutine FrmPtr
	clear	str55
	match	"0677",str4
	goto OREPMLR if	equal
	match	"0210",str4
	goto OREPMLR if	equal
	match	"1361",str4	"CUSTOM	LISTS"
	goto OREPMLR if	equal
	match	"0053",str4	"ANACAPA"
	goto OREPMLR if	equal
	match	"0702",str4
	goto OREPMLR if	equal
	match	"0965",str4
	goto OREPMLR if	equal
	match	"2531",str6
	goto OKEEPRTN if equal
	clear	str45
	clear	str45a
	move	mcomp to str45
	move	rtcomp to str45a
	rep	uplow in str45
	rep	uplow in str45a
	reset	str45
	reset	str45a
	match	str45 to str45a
	goto OKEEPRTN if equal
	if (FrmPtr = C1)
		setitem	Nord001AStatRtnComp,0,MCOMP
	elseif (FrmPtr = C6)
		setitem	Nord01eaStatRtnComp,0,MCOMP
	endif
	append	"C/O ",str55
	goto OLOADWINDOW

OKEEPRTN
	if (FrmPtr = C1)
		setitem	Nord001AStatRtnComp,0,RTCNTCT
	elseif (FrmPtr = C6)
		setitem	Nord01eaStatRtnComp,0,RTCNTCT
	endif
	goto OLOADWINDOW

OREPMLR	if (FrmPtr = C1)
		getitem	Nord001AComboOffer,0,N3
		getitem	Nord001AComboOffer,N3,str45
.START PATCH 3.68.7 ADDED LOGIC
.OFDESC - Offer Name has a length of 40.  Set Length to 41 so that Offer Number does not appear
		setlptr str45,41
.END PATCH 3.68.7 ADDED LOGIC
		setitem	Nord001AStatRtnComp,0,str45
	elseif (FrmPtr = C6)
		getitem	Nord01eaComboOffer,0,N3
		getitem	Nord01eaComboOffer,N3,str45
.START PATCH 3.68.7 ADDED LOGIC
.OFDESC - Offer Name has a length of 40.  Set Length to 41 so that Offer Number does not appear
		setlptr str45,41
.END PATCH 3.68.7 ADDED LOGIC
		setitem	Nord01eaStatRtnComp,0,str45
	endif
OLOADWINDOW
	append	RTCOMP,str55
	reset	str55
	count	N4,str55
	if (N4 = 4)	.only contains "C\0 "
		clear	str55
	endif
	if (FrmPtr = C1)
		setitem	Nord001AStatRtnComp2,0,str55
	endif
.Address field of record "0001"	says "Reuse of LR #"
	match	"0001",str6
	if equal
		clear	str55
		call	Trim using RTADDR
		append	RTADDR,str55
		append	B1,str55
		append	OREUSE,str55
		reset	str55
		if (FrmPtr = C1)
			setitem	Nord001AStatRtnAdd1,0,str55
		endif
	else
		if (FrmPtr = C1)
			setitem	Nord001AStatRtnAdd1,0,RTADDR
		endif
	endif
	call	TRIM using RTCITY
	if (RTCITY <> "")
		pack	str55,RTCITY,",	",RTSTATE,B1,RTZIP
	else
		pack	str55,RTSTATE,B1,RTZIP
	endif
	if (FrmPtr = C1)
		setitem	Nord001AStatRtnAdd2,0,str55
	endif
	return

OrderSetObildrct LRoutine FrmPtr
.Called	by:  Nord001AEditMlr_LostFocus, Nord001AEditMlrContact_LostFocus
	if (MBILDRCT = YES)
		if (FrmPtr = 1)
			setitem	Nord001bCheckDirect,0,1
		elseif (FrmPtr = 6)
			setitem	Nord01eaCheckBillDirect,0,1
		endif
	else
		if (FrmPtr = 1)
			setitem	Nord001bCheckDirect,0,0
		elseif (FrmPtr = 6)
			setitem	Nord01eaCheckBillDirect,0,0
		endif
	endif
	return
OrderLoadOffer LRoutine	FrmPtr
.Called	by:  OrderLoadScreens, SearchLoad3, Nord001AEditMlr_LostFocus,OrderLoadCampScreens,OrderLoadLOLScreen
.Must delete blank items entered to ensure adequate space
	if (FrmPtr = C1)
		deleteitem Nord001AComboOffer,0		.REFRESHED EACH	TIME MAILER IS CHANGED
		insertitem Nord001AComboOffer,0,"	  "	.FIRST ITEM ALWAYS NULL
	elseif (FrmPtr = C6)
		deleteitem Nord01eaComboOffer,0		.REFRESHED EACH	TIME MAILER IS CHANGED
		insertitem Nord01eaComboOffer,0,"	  "	.FIRST ITEM ALWAYS NULL
	elseif (FrmPtr = C8)
		deleteitem Nord01ECComboOffer,0		.REFRESHED EACH	TIME MAILER IS CHANGED
		insertitem Nord01ECComboOffer,0,"	  "	.FIRST ITEM ALWAYS NULL
	endif
.START PATCH 3.74 REPLACED LOGIC
.	move	C0,N4
.	move	C0,N3
.	BUMP	str7 BY	4
.	MOVE	str7 TO	str3
.	reset	str7
.	clear	str45
.	pack	NOFRFLD,str4,"001"
.	rep	zfill in NOFRFLD
.	move	"O.LoadOffer-NOFRKEY",Location
.	pack	KeyLocation,"Key: ",NOFRFLD
.	call	NOFRKEY
.	if not over
.		loop
.			if (str3 = OFNUM)
.				move	OFNUM,N3
.			endif
.			add	C1,N4
.			pack	str45,OFDESC,B1,OFNUM
.			if (FrmPtr = C1)
.				insertitem Nord001AComboOffer,N4,str45
.			elseif (FrmPtr = C6)
.				insertitem Nord01eaComboOffer,N4,str45
.			elseif (FrmPtr = C8)
.				insertitem Nord01ECComboOffer,N4,str45
.			endif
.			move	"O.LoadOffer-NOFRKS",Location
.			pack	KeyLocation,"Key: ",NOFRFLD
.			call	NOFRKS
.			until (OFMLR <>	str4)
.		repeat
.	endif
.	if (FrmPtr = C1	& NewFlag = "S")
.		move	C0,N3
.	elseif (FrmPtr = C6 & NewFlag2 = "S")
.		move	C0,N3
.	elseif (FrmPtr = C8 & NewFlag3 = "S")
.		move	C0,N3
.	endif
.	add	C1,N3				.FIRST ITEM ALWAYS NULL
.	if (FrmPtr = C1)
.		setitem	Nord001AComboOffer,0,N3
.	elseif (FrmPtr = C6)
.		setitem	Nord01eaComboOffer,0,N3
.	elseif (FrmPtr = C8)
.		setitem	Nord01ECComboOffer,0,N3
.	endif
................................................
	destroy ListViews(1)
	create	ListViews(1)=1:1:1:1,SortOrder=1,FullRow=1
	activate ListViews(1)
	ListViews(1).InsertColumn using "",0,0
	ListViews(1).InsertColumn using "",0,1
	move	C0,N4
	move	C0,N3
	BUMP	str7 BY	4
	MOVE	str7 TO	str3
	reset	str7
	clear	str45
	pack	NOFRFLD1,"01X",str4
	rep	zfill in NOFRFLD1
	move	"O.LoadOffer-NOFRAIM",Location
	pack	KeyLocation,"Key: ",NOFRFLD1
	call	NOFRAIM
	loop
		until over
		ListViews(1).InsertItem giving N9 using OFNUM
		ListViews(1).SetItemText using N9,OFDESC,1
		move	"O.LoadOffer-NOFRKG",Location
		call	NOFRKG
	repeat
	ListViews(1).GetItemCount giving howmany
	sub	C1,howmany
	for result,C0,howmany
		ListViews(1).GetItemText giving OFNUM using result
		if (str3 = OFNUM)
			move	OFNUM,N3
		endif
		ListViews(1).GetItemText giving OFDESC using result,C1
		add	C1,N4
		pack	str45,OFDESC,B1,OFNUM
		if (FrmPtr = C1)
			insertitem Nord001AComboOffer,N4,str45
		elseif (FrmPtr = C6)
			insertitem Nord01eaComboOffer,N4,str45
		elseif (FrmPtr = C8)
			insertitem Nord01ECComboOffer,N4,str45
		endif
	repeat
	destroy ListViews(1)
	if (FrmPtr = C1	& NewFlag = "S")
		move	C0,N3
	elseif (FrmPtr = C6 & NewFlag2 = "S")
		move	C0,N3
	elseif (FrmPtr = C8 & NewFlag3 = "S")
		move	C0,N3
	endif
	add	C1,N3				.FIRST ITEM ALWAYS NULL
	if (FrmPtr = C1)
		setitem	Nord001AComboOffer,0,N3
	elseif (FrmPtr = C6)
		setitem	Nord01eaComboOffer,0,N3
	elseif (FrmPtr = C8)
		setitem	Nord01ECComboOffer,0,N3
	endif
.END PATCH 3.74 REPLACED LOGIC
	return
OrderLoadOwner LRoutine	FrmPtr
.Called	by:  OrderLoadScreens,Nord001AEditOwner_LostFocus
	move	"O.LoadOwner-NOWNKEY",Location
	pack	KeyLocation,"Key: ",NOWNFLD
	call	NOWNKEY
	if over
		if (FrmPtr = C1)
			setitem	Nord001AStatOwnerComp,0,""
.Start Patch 3.78.8 	Object has been replace with fulfillment static text box			
.			setitem	Nord001AStatOwnerComp2,0,""
.End Patch 3.78.8 	
			setitem	Nord001AStatOwnerAdd1,0,""
			setitem	Nord001AStatOwnerAdd2,0,""
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatOwnerComp,0,""
		endif
	else
		if (FrmPtr = C1)
			setitem	Nord001AStatOwnerComp,0,OWNOCPY
.START PATCH 3.51 REPLACED LOGIC
.			pack	str55,"cc: ",OWNCTN
.START PATCH 3.78.4 REPLACED LOGIC
.			pack	NFULFLD,OWNCTN
.			if (NFULFLD = "    ")
.				pack	NFULFLD,"////"	.FORCE AN OVER
.			else
.				rep	zfill,NFULFLD
.			endif
.			move	C1,NFULPATH
.			move	"O.LoadOwner-NFULKEY",Location
.			pack	KeyLocation,NFULFLD
.			call	NFULKEY
.			pack	str55,"cc: ",NFULCOMP
..............................
.Start Patch 3.78.8 Comment Out Remove this logic and replace during list read.
.			pack	COMPFLD6,OWNCTN
.			if (COMPFLD6 = "    ")
.				pack	COMPFLD6,"////"	.FORCE AN OVER
.			else
.				rep	zfill,COMPFLD6
.			endif
.			move	"O.LoadOwner-COMPKEY6",Location
.			pack	KeyLocation,COMPFLD6
.			call	COMPKEY6
.			if not over
.				if (COMPSVBFLG <> "T")
.					clear	COMPNUM
.					clear	COMPCOMP
.				endif
.			endif
.			//Refresh the "Fulfillment Variables"
.			move	COMPNUM,NFULNUM
.			move	COMPCOMP,NFULCOMP
.			pack	str55,"cc: ",NFULCOMP
.END PATCH 3.78.4 REPLACED LOGIC
.END PATCH 3.51 REPLACED LOGIC
.			setitem	Nord001AStatOwnerComp2,0,str55
.End Patch 3.78.8 Comment Out Removed this logic and replace during Orderloadscreens Routine.			
			setitem	Nord001AStatOwnerAdd1,0,OWNLOSA
			call	TRIM using OWNLOCTY
			if (OWNLOCTY <>	"")
				pack	str55,OWNLOCTY,", ",OWNLOS,B1,OWNLOZC
			else
				pack	str55,OWNLOS,B1,OWNLOZC
			endif
			setitem	Nord001AStatOwnerAdd2,0,str55
.START PATCH 3.51 REPLACED LOGIC
.			call	Trim using OWNCTN
.			if (OWNCTN = "TDMC" OR OWNCTN =	"Tdmc" OR OWNCTN = "tdmc")
.				setprop	Nord001bStatListName,fgcolor=green
.			else
.				setprop	Nord001bStatListName,fgcolor=black
.			endif
..............
.START PATCH 3.78.4 REPLACED LOGIC
.			call	Trim using NFULCOMP
.			if (NFULCOMP = "TDMC" OR NFULCOMP = "Tdmc" OR NFULCOMP = "tdmc" OR NFULFLD = "0026")
.Start Patch 3.78.8 Comment Out Remove this logic and replace during list read.			
.			call	Trim using NFULCOMP
.			if (NFULCOMP = "TDMC" OR NFULCOMP = "Tdmc" OR NFULCOMP = "tdmc" OR NFULNUM = "009406")
					
.END PATCH 3.78.4 REPLACED LOGIC
.				setprop	Nord001bStatListName,fgcolor=green
.			else
.				setprop	Nord001bStatListName,fgcolor=black
.			endif
.End Patch 3.78.8 Comment Out this logic and replace during Orderloadscreens Routine.				
.END PATCH 3.51 REPLACED LOGIC
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatOwnerComp,0,OWNOCPY
		endif
	endif
	if (FrmPtr = C1)
		pack	NPAYFLD,NOWNFLD,"0"
		move	"O.LoadOwner-NPAYTST",Location
		pack	KeyLocation,"Key: ",NPAYFLD
		call	NPAYTST
		if over
			setprop	Nord001AEditOwner,fgcolor=black
		else
			setprop	Nord001AEditOwner,fgcolor=red
		endif
	endif
	return
OrderScreenSample
.Called	by:  Nord001AEditOwner_LostFocus because Nord001a	is loaded before Nord001b
.Special processing
	if (NOWNFLD = "0994" OR	NOWNFLD	= "4574")
.START PATCH 3.72.2 REPLACED LOGIC
.		setprop	Nord001bComboSample,enabled=0,bgcolor=grey
.		setprop	Nord001bComboSam,enabled=0,bgcolor=grey
.		setitem	Nord001bComboSample,0,1
.		setitem	Nord001bComboSam,0,1
............
		setprop	Nord001bEditSample,enabled=0,bgcolor=grey
		setprop	Nord001bListViewSamples,enabled=0,bgcolor=grey
		setprop	Nord001bButtonArrow,enabled=0
		setprop	Nord001bComboSam,enabled=0,bgcolor=grey
		setitem	Nord001bEditSample,0,""
		setitem	Nord001bComboSam,0,1
.END PATCH 3.72.2 REPLACED LOGIC
.START PATCH 3.65 ADDED LOGIC
		setitem	Nord001bStatSamplesInactive,0,""
.END PATCH 3.65 ADDED LOGIC
	else
.START PATCH 3.72.2 REPLACED LOGIC
.		setprop	Nord001bComboSample,enabled=1,bgcolor=white
		setprop	Nord001bEditSample,enabled=1,bgcolor=white
		setprop	Nord001bListViewSamples,enabled=1,bgcolor=white
		setprop	Nord001bButtonArrow,enabled=1
.END PATCH 3.72.2 REPLACED LOGIC
		setprop	Nord001bComboSam,enabled=1,bgcolor=white
	endif
	return

OrderLoadClearanceStat
	setitem	Nord001bStatHistory,0,"Outside LCR Cleared by Contact."
	setitem	Nord001AStatMssg,0,"Cleared Outside LCR."
OrderLoadClearanceStat2
	pack	str10,OCLRDTEM,SLASH,OCLRDTED,SLASH,OCLRDTEC,OCLRDTEY
	clear	str55
	if (OCLRSTAT <>	"" AND OCLRSTAT	<> " ")
		if (OCLRSTAT = "4")
			append	"Denied	",str55
		else
			append	"Cleared for ",str55
			if (OCLRSTAT = "1")
				append	"Exchange ",str55
			elseif (OCLRSTAT = "2")
				append	"Rent ",str55
			elseif (OCLRSTAT = "3")
				append	"Exc/Rent ",str55
			endif
		endif
		append	str10,str55
		append	" by ",str55
		append	OCLRINIT,str55
		reset	str55
	else
	endif
	setitem	Nord001bStatClearStat,0,str55
	return
OrderLoadCampaignScreens
	call	OrderLoadCampaignScreen7
	call	OrderLoadCampaignScreen8
	return
OrderLoadCampaignScreen7
	call	OrderSetMouseBusy
.SET UP	SCROLL BARS
	move	"50",N2
	setitem	NORDMSK2VScrollLR,0,N2
.CAMPAIGN STATUS
	move	NCMPSTAT,N1
	add	C1,N1
	setitem	Nord01eaComboStatus,0,N1
.CAMPAIGN NUM/NAME
	setitem	NORDMSK2StatCamp,0,NCMPNUM
	setitem	Nord01eaEditCampName,0,NCMPCNAME
.
	setitem	Nord01EBStatCampName2,0,NCMPCNAME
.MAILER
	setitem	Nord01eaEditMlr,0,NCMPMLR
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	clear	MKEY
.	pack	MKEY,NCMPMLR,"000"
	pack	COMPFLD,NCMPMLR
	move	"OLCS7a-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	clear	MKEY
	pack	MKEY,COMPOLDMLR,"000"
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	rep	zfill,MKEY
	call	OrderLoadMailer	using C6
.setitem Nord01eaStatUniverse,0,"" - DETERMINED IN ABOVE ROUTINE
.BROKER
	setitem	Nord01eaEditBrk,0,NCMPBRK
	setitem	Nord01eaEditBrkContact,0,NCMPBRKCNT
	clear	NBRKFLD
	call	TRIM using NCMPBRK
	count	N1,NCMPBRK
	if (N1 <> C0)
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.		pack	NBRKFLD,NCMPBRK,NCMPBRKCNT
		pack	CNCTFLD,NCMPBRK,NCMPBRKCNT
		rep	zfill,CNCTFLD
		move	"O.LoadC.Screens-CNCTKEY",Location
		pack	KeyLocation,"Key: ",CNCTFLD
		call	CNCTKEY
		if not over
			pack	NBRKFLD,CNCTCNT
		else
			pack	COMPFLD,NCMPBRK
			move	"O.LoadC.ScreensB-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			pack	NBRKFLD,COMPOLDBRK,"000"
		endif
.END PATCH 3.75.7 REPLACED LOGIC
		call	OrderLoadBroker	using C6
	else
		setitem	Nord01eaStatBrkComp,0,""
		setitem	Nord01eaStatBrkCntName,0,""
.START PATCH 3.75.7 REMOVED LOGIC
..START PATCH 3.71.9 ADDED LOGIC
.		setitem	Nord01eaStatBrkNew,0,""
..END PATCH 3.71.9 ADDED LOGIC
.END PATCH 3.75.7 REMOVED LOGIC
	endif
.PO NUM
	setitem	Nord01eaEditPO,0,NCMPPO
.SHIP-TO
	pack	NRTNFLD,NCMPSHIPTO
	move	"O.LoadC.Screens-NRTNKEY",Location
	pack	KeyLocation,"Key: ",NRTNFLD
	call	NRTNKEY
	if over
		setitem	Nord01eaEditRtn,0,""
	       	setitem	Nord01eaStatRtnComp,0,""
	else
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.		setitem	Nord01eaEditRtn,0,NCMPSHIPTO
		setitem	Nord01eaEditRtn,0,RTNUM
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		setitem	Nord01eaStatRtnComp,0,RTCOMP
	endif
.REPORT	DEFAULT
	move	C1,N1
	move	NCMPRpt,N1
	setitem	Nord01eaComboReport,0,N1
.CAMPAIGN DATE
	call	TRIM using NCMPDATE
	if (NCMPDATE <>	"")
		unpack	NCMPDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	setitem	Nord01eaEditCampDate,0,newdate1
.RETURN	DATE
	call	TRIM using NCMPRDATE
	count	N2,NCMPRDATE
	if (NCMPRDATE <> "")
		unpack	NCMPRDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	setitem	Nord01eaEditRtnDate,0,newdate1
.CUT-OFF DATE
	call	TRIM using NCMPCDATE
	count	N2,NCMPCDATE
	if (NCMPCDATE <> "")
		unpack	NCMPCDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	setitem	Nord01eaEditCutOffDate,0,newdate1
.MAIL DATE
	call	TRIM using NCMPMDATE
	if (NCMPMDATE <> "")
		unpack	NCMPMDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	setitem	Nord01eaEditMailDate,0,newdate1
.REVISE
	call	TRIM using NCMPMODDATE
	if (NCMPMODDATE	<> "")
		unpack	NCMPMODDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	call	Trim using NCMPINITS
	clear	str55
	append	"Last Revised:	",str55
	append	newdate1,str55
	append	" By ",str55
	append	NCMPINITS,str55
	reset	str55
	setitem	Nord01eaStatRevise,0,str55
.PLANNER
	call	TRIM using NCMPPLANNER
	call	OrderLoadCombo Using Nord01eaComboPlanner,NCMPPLANNER
.CONTACT
	call	TRIM using NCMPCNT
	call	OrderLoadCombo Using Nord01eaComboContact,NCMPCNT
.PACKAGE
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	call	OrderLoadMlrPackageListView using Nord01eaListViewPackage,NCMPMLR
	pack	COMPFLD,DimPtr
	move	"SL3c-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	call	OrderLoadMlrPackageListView using Nord01eaListViewPackage,COMPOLDMLR
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.KEY INFO
	setitem	Nord01eaEditKey,0,NCMPKEY
.BILL DIRECT
	move	C0,N1
	move	NCMPBill,N1
	setitem	Nord01eaCheckBillDirect,0,N1
.GROSS/NET QTY
	move	NCMPQTY,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01eaEditGrossQty,0,str17
	move	NCMPNETQTY,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01eaEditNetQty,0,str17
.MEDIA
	move	NCMPMEDIA,N2
	move	N2,N3
	add	C1,N2	.File begins with '0', Combo begins with '1' and  first	item is	null
	if (N3 = 20)
		move	C0,N2	.First item has	blank filled string of MED20
	else
		if (N3 < 20)
			add	C1,N2
		endif
	endif
	setitem	Nord01eaComboMedia,0,N2
.OFFER
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	move	NCMPMLR,str4
.	pack	str7,NCMPMLR,NCMPOFFER
	pack	COMPFLD,NCMPMLR
	move	"SL3a-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	move	COMPOLDMLR,str4
	pack	str7,COMPOLDMLR,NCMPOFFER
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	call	OrderLoadOffer using C6	 .Offer	reloaded whenever mailer field is changed
.SHIP
	call	TRIM using NCMPSHIP
	if (NCMPSHIP = "")
		setitem	Nord01eaComboShip,0,1
	else
		move	NCMPSHIP,N2
		add	C2,N2
		setitem	Nord01eaComboShip,0,N2
	endif
.SAMPLE
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	move	NCMPMLR,str4
	move	COMPOLDMLR,str4
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	call	OrderLoadSamples using C6
.RESPONSE
	setitem	Nord01eaEditRespChange,0,NCMPRATE
.GIFT
	setitem	Nord01eaEditGiftChange,0,NCMPGIFT
.SPECIAL INSTRUCTIONS
	setitem	Nord01eaEditSpecial,0,NCMPCOMMENT
	call	OrderSetMouseFree
	return

OrderLoadCampaignScreen8
	call	OrderSetMouseBusy
.Initialize Totals which are calculated	in following routine
	move	C0,CampCont
	move	C0,CampTests
	move	C0,CampTotal
	move	C0,CampNetCont
	move	C0,CampNetTests
	move	C0,CampNetTotal
	move	NO,CampPFlag
.RECORD	DETAILS
	call	OrderLoadLOLDetailScreen
.PROJECTIONS
	if (CampPFlag =	YES)
		setitem	Nord01eaStatProjections,0,"Associated Projection Records!"
	else
		setitem	Nord01eaStatProjections,0,""
	endif
.TESTS/CONT
	move	CampCont,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01eaStatContNum,0,str17
	move	CampTests,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01eaStatTestsNum,0,str17
	calc	CampTotal=(CampTests + CampCont)
	move	CampTotal,str15
	call	FormatNumeric using str15,str24
	setitem	Nord01eaStatCampaignNum,0,str24
.
	move	CampNetCont,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01eaStatContNetNum,0,str17
	move	CampNetTests,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01eaStatTestsNetNum,0,str17
	calc	CampNetTotal=(CampNetTests + CampNetCont)
	move	CampNetTotal,str15
	call	FormatNumeric using str15,str24
	setitem	Nord01eaStatCampaignNetNum,0,str24
	call	OrderSetMouseFree
	return

OrderLoadLOLDetailScreen
.Called	by:  OrderLoadLOLScreen
.Load LOL records corresponding	to Campaign record
	setprop	NORDMSK2ButtonModify,enabled=1
	move	C0,N7
	call	OrderLOLDetailClear
	move	NCMPFLD,NLOLFLD1
	move	C2,NLOLPATH
	move	"LoadLOLDetail-NLOLKEY",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
.Clear Order variables used in List View
	clear	OLRN
	clear	OCLRINIT
	clear	OSTAT
	call	NLOLKEY
	if over
		goto OrderLoadLOLDetailScreen2
	endif
	move	"LoadLOLDetail-NLOLKS",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	loop
		until (NLOLCNUM	<> NLOLFLD1)
		call	OrderLoadLOLDetail
		call	NLOLKS
		until over
	repeat
OrderLoadLOLDetailScreen2
	move	NCMPFLD,NORDFLDC
	move	C4,NORDPATH
	move	"LoadLOLDetail-NORDKEY",Location
	pack	KeyLocation,"Key: ",NORDFLDC
	call	NORDKEY
	if not over
		move	"LoadLOLDetail-NORDKS",Location
		pack	KeyLocation,"Key: ",NORDFLDC
		loop
			until (OCAMP <>	NORDFLDC)
			call	OrderLoadLOLPackLOL
			call	OrderLoadLOLDetail
.Need to reestablish NORDPATH at each iteration	as above routines call other routines which MAY	reset NORDPATH to C1
			move	C4,NORDPATH
			call	NORDKS
			until over
		repeat
	endif
.Make sure at least one	record is here
	Nord01EBListView.GetItemCount giving result
	if (result = C0)
		return
	endif
	move	N7,str7
	call	Trim using str7
	pack	str35,"Total Records:  ",str7
	setitem	Nord01EBStatTotalNames,0,str35
.Insert	Break point for	Test/Retest Records
	pack	str45,"2","BREAK FOR TEST RECORDS"
	Nord01EBListView.InsertItem giving N9 using str45
	Nord01EBListView.SetItemText using N9,"-----TESTS-----",5
	pack	str45,"4","BREAK FOR RETEST RECORDS"
	Nord01EBListView.InsertItem giving N9 using str45
	Nord01EBListView.SetItemText using N9,"-----RETESTS-----",5
	Nord01EBListView.EnsureVisible using 0,0
	Nord01EBListView2.EnsureVisible using 0,0
	Nord01EBListView.SetItemState giving N9 using 0,2,2
	Nord01EBListView2.SetItemState giving N9 using 0,2,2
	if (View7Flag =	1)
		setfocus Nord01EBListView
		goto Order7ListViewClick
	else	.ViewFlag = 2
		setfocus Nord01EBListView2
		goto Order7ListViewClick2
	endif

OrderLoadLOLDetail
.Called	by: OrderLoadLOLDetailScreen
	add	C1,N7
	move	C1,NDATPATH
	move	NLOLLIST,NDATFLD
	move	"LoadLOLDetail-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	if (NLOLTEST = "1")
		pack	str45,"3",OLSTNAME
	elseif (NLOLTEST = "2")
		pack	str45,"5",OLSTNAME
	else
		pack	str45,"1",OLSTNAME
	endif
	Nord01EBListView.InsertItem giving N9 using str45
	Nord01EBListView2.InsertItem giving N8 using OLSTNAME
	Nord01EBListView.SetItemText using N9,OLRN,1
	Nord01EBListView2.SetItemText using N8,OLRN,3
	clear	str25
	if (OSTAT = "0")
		move	"Order",str25
	elseif (OSTAT =	"p")
		move	"Pending Order",str25
	elseif (OSTAT =	"x")
		move	"Cancelled Pending Order",str25
	elseif (OSTAT =	"l")
		if (OCO2CODE <>	"" & OCO2CODE <> "  ")
			move	"*LCR",str25
		else
			move	"LCR",str25
		endif
	elseif (OSTAT =	"z")
		move	"Denied/Cancelled LCR",str25
	elseif (OSTAT =	"B")
		move	"Billed	Order",str25
	elseif (OSTAT =	"Q")
		move	"Cancelled Billed Order",str25
	elseif (OSTAT =	"X")
		move	"Cancelled Order",str25
	endif
	Nord01EBListView.SetItemText using N9,str25,2
	Nord01EBListView2.SetItemText using N8,str25,4
	clear	NPNDDESC
.NINORD5/NINORD4 File
	if (OSTAT = "l"	| OSTAT	= "z")
		move	OLRN,NORD5FLD
		rep	zfill,NORD5FLD
		move	"O.LoadLOLDet.-NORD5KEY",Location
		pack	KeyLocation,"Key: ",NORD5FLD
		call	NORD5KEY
		if over
.			 move	 "No Status Found!",NPNDDESC
		else
.NINPND	File
			pack	NPNDFLD,OSTAT,NORD5STAT
			rep	zfill,NPNDFLD
			move	"O.LoadLOLDet.-NPDNKEY",Location
			pack	KeyLocation,"Key: ",NPNDFLD
			call	NPNDKEY
			if over
.				 move	 "No Status Found!",NPNDDESC
			endif
		endif
	elseif (OSTAT =	"p" | OSTAT = "x")
		move	OLRN,NORD4FLD
		rep	zfill,NORD4FLD
		move	"O.LoadLOLDet.-NORD4KEY",Location
		pack	KeyLocation,"Key: ",NORD4FLD
		call	NORD4KEY
		if over
.			 move	 "No Status Found!",NPNDDESC
		else
.NINPND	File
			pack	NPNDFLD,OSTAT,NORD4STAT
			rep	zfill,NPNDFLD
			move	"O.LoadLOLDet.-NPDNKEY",Location
			pack	KeyLocation,"Key: ",NPNDFLD
			call	NPNDKEY
			if over
.				 move	 "No Status Found!",NPNDDESC
			endif
		endif
	endif
	Nord01EBListView.SetItemText using N9,NPNDDESC,3
	Nord01EBListView2.SetItemText using N8,NPNDDESC,5
	if (OLRN <> "")
		pack	STAT2FLD2,"01X",OLRN
		pack	STAT2FLD3,"02X0"
	else
		pack	STAT2FLD2,"01X",NLOLLOL
		pack	STAT2FLD3,"02X1"
	endif
	move	"O.LoadLOLDet.-STAT2AIM",Location
	pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
	call	STAT2AIM
	if not over
		move	"*",str1
		move	YES,CampPFlag
	else
		clear	str1
	endif
	pack	str8,NLOLLIST,str1
	Nord01EBListView.SetItemText using N9,str8,4
	Nord01EBListView2.SetItemText using N8,str8,1
	Nord01EBListView.SetItemText using N9,OLSTNAME,5
.START PATCH 3.72 REPLACED LOGIC
.	Nord01EBListView.SetItemText using N9,NLOLSELECT,6
.	Nord01EBListView2.SetItemText using N8,NLOLSELECT,2
	pack	NSEL2FLD,"2",NLOLLOL
	move	"O.LoadLOLDet.-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	NLOLSELECT,NSEL2NAME
		move	NLOLUNIVERSE,NSEL2QTY
	endif
	Nord01EBListView.SetItemText using N9,NSEL2NAME,6
	Nord01EBListView2.SetItemText using N8,NSEL2NAME,2
.END PATCH 3.72 REPLACED LOGIC
	move	NLOLQTY,str13
	call	FormatNumeric using str13,str17
	Nord01EBListView.SetColumnFormat using 7,1
	Nord01EBListView.SetItemText using N9,str17,7
	Nord01EBListView2.SetColumnFormat	using 6,1
	Nord01EBListView2.SetItemText using N8,str17,6
	move	NLOLNETQTY,str13
	call	FormatNumeric using str13,str17
	Nord01EBListView.SetColumnFormat using 8,1
	Nord01EBListView.SetItemText using N9,str17,8
	Nord01EBListView2.SetColumnFormat	using 7,1
	Nord01EBListView2.SetItemText using N8,str17,7
	Nord01EBListView.SetItemText using N9,NLOLNET,9
	Nord01EBListView2.SetItemText using N8,NLOLNET,8
.START PATCH 3.72 REPLACED LOGIC
.	move	NLOLUNIVERSE,str13
	move	NSEL2QTY,str13
.END PATCH 3.72 REPLACED LOGIC
	call	FormatNumeric using str13,str17
	Nord01EBListView.SetColumnFormat using 10,1
	Nord01EBListView.SetItemText using N9,str17,10
	Nord01EBListView2.SetColumnFormat	using 9,1
	Nord01EBListView2.SetItemText using N8,str17,9
	if (NLOLRENT = "1")
		move	"Exchange",str15
	elseif (NLOLRENT = "2")
		move	"Rent",str15
	elseif (NLOLRENT = "3")
		move	"Exc/Rent",str15
	else
		clear	str15
	endif
	Nord01EBListView.SetItemText using N9,str15,11
	Nord01EBListView2.SetItemText using N8,str15,10
	clear	str35
	clear	str12
	clear	str16
	clear	CNTNAME
	if (OLRN <> "")	.In Order File
		if (OHIST = "p")
			move	"Originated as a Pending Order.",str35
		elseif (OHIST =	"l")
			move	"Originated as an LCR.",str35
		elseif (OHIST =	"L")
			move	"Originated as an In-House LCR.",str35
		elseif (OHIST =	"e")
			if (OCO2CODE <>	"" & OCO2CODE <> "  ")
				move	"Waiting for Caller Response.",str35
			else
				move	"Waiting for Contact Response.",str35
			endif
		elseif (OHIST =	"E")
			if (OCO2CODE <>	"" & OCO2CODE <> "  ")
				move	"Cleared by Caller.",str35
			else
				move	"Cleared.",str35
			endif
		elseif (OHIST =	"*")
			move	"Waiting for Owner Response.",str35
		elseif (OHIST =	"z")
			if (OCO2CODE <>	"" & OCO2CODE <> "  ")
				move	"Denied	by Caller.",str35
			else
				move	"Denied.",str35
			endif
		endif
		if (OCLRSTAT = "1")
			move	"Exchange",str12
		elseif (OCLRSTAT = "2")
			move	"Rental",str12
		elseif (OCLRSTAT = "3")
			move	"Exc/Rent",str12
		elseif (OCLRSTAT = "4")
			move	"Denied",str12
		endif
		call	TRIM using OCLRDTEM
		count	N2,OCLRDTEM
		if (N2 <> 0 AND	OCLRDTEM <> "00")
			pack	str16,OCLRDTEM,SLASH,OCLRDTED,SLASH,OCLRDTEC,OCLRDTEY
		endif
		pack	NCNTFLD,OCO2CODE
		move	C1,NCNTPATH
		move	"O.Load5-NCNTKEY",Location
		pack	KeyLocation,"Key: ",NCNTFLD
		call	NCNTKEY
	endif
	Nord01EBListView.SetItemText using N9,str35,12
	Nord01EBListView2.SetItemText using N8,str35,11
	Nord01EBListView.SetItemText using N9,str12,13
	Nord01EBListView2.SetItemText using N8,str12,12
	Nord01EBListView.SetItemText using N9,OCLRINIT,14
	Nord01EBListView2.SetItemText using N8,OCLRINIT,13
	Nord01EBListView.SetItemText using N9,str16,15
	Nord01EBListView2.SetItemText using N8,str16,14
	Nord01EBListView.SetItemText using N9,CNTNAME,16
	Nord01EBListView2.SetItemText using N8,CNTNAME,15
.You must initialize with zero.	 If not	then garbage will remain in form vars if dim vars do not have valid numeric data!
	move	C0,N10
	move	C0,howmany
	if ((OSTAT <> "l" | (NORD5STAT <> "05" & NORD5STAT <> "07")) & OSTAT <>	"z")
.Do not	add Denied/Cancelled LCRs into Totals
		move	NLOLQTY,N10
		move	NLOLNETQTY,howmany
	endif
	if (NLOLTEST = "1" | NLOLTEST =	"2")
		if (NLOLTEST = "1")
			move	"Test",str6
		else
			move	"Retest",str6
		endif
		add	N10,CampTests
		add	howmany,CampNetTests
	else
		move	"Cont.",str6
		add	N10,CampCont
		add	howmany,CampNetCont
	endif
	Nord01EBListView.SetItemText using N9,str6,17
	Nord01EBListView2.SetItemText using N8,str6,16
	move	NLOLLIST,OLNUM
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	move	NCMPMLR,OMLRNUM
	pack	COMPFLD,NCMPMLR
	move	"OL5a-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	move	COMPOLDMLR,OMLRNUM
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	call	OrderTestXSTAT
	if (taskname = "")
.I can do the following	as taskname=NULL and is	not filled until after second pointer is used
.Check out subroutine and trace	DimPtr/DimPtr1 to see what I mean.
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.		pack	NORDFLD1,"01R",NCMPMLR
		pack	NORDFLD1,"01R",COMPOLDMLR
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		pack	NORDFLD2,"02R",NLOLLIST
		clear	NORDFLD3
		clear	NORDFLD4
		call	OrderGetHistory	using taskname,taskname,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C8
	endif
	Nord01EBListView.SetItemText using N9,taskname,18
	Nord01EBListView2.SetItemText using N8,taskname,17
	pack	hold7,NLOLVARS
	Nord01EBListView.SetItemText using N9,hold7,19
	Nord01EBListView2.SetItemText using N8,hold7,18
.START PATCH 3.66 ADDED LOGIC
	getitem OptionsScreen7Proj,0,N1
	if (N1 = 1)
		call	Trim using OLSTNAME
.START PATCH 3.72 REPLACED LOGIC
.		call	Trim using NLOLSELECT
		call	Trim using NSEL2NAME
.END PATCH 3.72 REPLACED LOGIC
		call	Trim using OLRN
		if (OLRN <> "")
			move	OLRN,str6
		else
			move	NLOLLOL,str6
		endif
.START PATCH 3.72 REPLACED LOGIC
.		pack	taskname,OLSTNAME,SLASH,NLOLSELECT,str6
		pack	taskname,OLSTNAME,SLASH,NSEL2NAME,str6
.END PATCH 3.72 REPLACED LOGIC
		ListIts2.Add giving ListIt using *Index=1,*Text=taskname
		setprop	ListIt,*SubItems(1)=OLRN
		setprop	ListIt,*SubItems(2)=str25
		setprop	ListIt,*SubItems(3)=NLOLLIST
		setprop	ListIt,*SubItems(4)=taskname
		move	NLOLQTY,str13
		call	FormatNumeric using str13,str17
		setprop	ListIt,*SubItems(5)=str17
		move	NLOLNETQTY,str13
		call	FormatNumeric using str13,str17
		setprop	ListIt,*SubItems(6)=str17
		setprop	ListIt,*SubItems(7)=NLOLNET
		move	C0,N10
		move	"O.LoadLOLDet.2-STAT2AIM",Location
		pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
		call	STAT2AIM
		loop
			until over
			if (statmqty > C0)
				add	statmqty,N10
			elseif (statrecqty > C0)
				add	statrecqty,N10
			endif
			move	"O.LoadLOLDet.2-STAT2KG",Location
			call	STAT2KG
		repeat
		getitem	red,0,colornum
		move	N10,str10
		call	FormatNumeric using str10,str13
		ListIt.ListSubItems.Add giving SubIt using *Text=str13
		call	Trim using NLOLQTY
		move	C0,N9
		move	NLOLQTY,N9
		if (N10 <> N9)
			setprop SubIt,*ForeColor=colornum
		endif
.		setprop	ListIt,*SubItems(8)=str13
.START PATCH 3.72 REPLACED LOGIC
.		move	NLOLUNIVERSE,str13
		move	NSEL2QTY,str13
.END PATCH 3.72 REPLACED LOGIC
		call	FormatNumeric using str13,str17
		setprop	ListIt,*SubItems(9)=str17
		setprop	ListIt,*SubItems(10)=hold7
.Add Package Breakdowns if applicable
		getitem	blue,0,colornum
		move	"O.LoadLOLDet.3-STAT2AIM",Location
		pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
		call	STAT2AIM
		loop
			until over
			pack	taskname2,taskname,STATNUM
			ListIts2.Add giving ListIt using *Index=1,*Text=taskname2
			setprop	ListIt,*SubItems(1)=""
			setprop	ListIt,*SubItems(2)=""
			setprop	ListIt,*SubItems(3)=""
.			setprop	ListIt,*SubItems(3)=STATPCKNUM
.START PATCH 3.75.4 REPLACED LOGIC
.			pack	NPKGFLD,OMLRNUM,STATPCKNUM
			move	"O.LoadLOLDet.-COMPKEY3",Location
			pack	COMPFLD3,OMLRNUM
			pack	KeyLocation,"Key: ",COMPFLD3
			call	COMPKEY3
			if over
				clear	COMPNUM
			endif
			pack	NPKGFLD,COMPNUM,STATPCKNUM
.END PATCH 3.75.4 REPLACED LOGIC
			move	C1,NPKGPATH
			move	"O.LoadLOLDet.-NPKGKEY",Location
			pack	KeyLocation,"Key: ",NPKGFLD
			call	NPKGKEY
			pack	taskname2,STATPCKNUM,B1,NPKGPNAME
			ListIt.ListSubItems.Add giving SubIt using *Text=taskname2
			setprop SubIt,*ForeColor=colornum
.			setprop	ListIt,*SubItems(4)=taskname2,*ForeColor=colornum
	 		move	statrecqty,str13
			call	FormatNumeric using str13,str17
			ListIt.ListSubItems.Add giving SubIt using *Text=str17
			setprop SubIt,*ForeColor=colornum
.			setprop	ListIt,*SubItems(5)=str17,*ForeColor=colornum
			move	statmqty,str13
			call	FormatNumeric using str13,str17
			ListIt.ListSubItems.Add giving SubIt using *Text=str17
			setprop SubIt,*ForeColor=colornum
.			setprop	ListIt,*SubItems(6)=str17,*ForeColor=colornum
			ListIt.ListSubItems.Add giving SubIt using *Text=""		.Blank filled
			ListIt.ListSubItems.Add giving SubIt using *Text=""		.Blank filled
.START PATCH 3.72 REPLACED LOGIC
.			move	statseluni,str9
.			call	FormatNumeric using str9,str11
.			ListIt.ListSubItems.Add giving SubIt using *Text=str11
.			setprop SubIt,*ForeColor=colornum
			move	NSEL2QTY,str13
			call	FormatNumeric using str13,str17
			ListIt.ListSubItems.Add giving SubIt using *Text=str17
			setprop SubIt,*ForeColor=colornum
.END PATCH 3.72 REPLACED LOGIC
			move	"O.LoadLOLDet.3-STAT2KG",Location
			call	STAT2KG
		repeat
	endif
.END PATCH 3.66 ADDED LOGIC
	return

OrderLoadLOLDetail2 LRoutine DimPtr1,DimPtr2,DimPtr3
	setitem	Nord01EBEditSpecial,0,DimPtr1
	setitem	Nord01EBEditSpecial1,0,DimPtr3
	setitem	Nord01EBEditSpecial2,0,DimPtr2
	return

OrderLoadLOLScreen
.LOAD UP KEY VALUE IN CASE YOU WANT TO VIEW IT VIA A CLICK ON AAMKEY
	pack	Hold3LOL,NLOLLOL
.SET UP	SCROLL BARS
	move	"50",N2
	setitem	NORDMSK3VScrollLR,0,N2
.RECORD	NUMBER
.	 pack	 str13,NLOLCNUM,DASH,NLOLLIST
.	 setitem NORDMSK3StatLOL,0,str13
	setitem	NORDMSK3StatLOL,0,NLOLLOL
.LOL STATUS
	move	C0,N1
	move	NLOLSTAT,N1
	add	C1,N1
	setitem	Nord01ECComboStatus,0,N1
.LOL MASTER LR/LCR
	setitem	Nord01ECEditLR,0,NLOLLR
.LOL NUM/NAME
	setitem	Nord01ECEditCamp,0,NLOLCNUM
	move	C1,NCMPPATH
	pack	NCMPFLD,NLOLCNUM
	move	"LoadLOL-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NLOLCNUM
	call	NCMPKEY
	setitem	Nord01ECStatCampName,0,NCMPCNAME
.Following Fields rely on NCMPKEY above	returning valid	values
.MAILER
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	setitem	Nord01ECEditMlr,0,NCMPMLR
.	clear	MKEY
.	pack	MKEY,NCMPMLR,"000"
	pack	COMPFLD,NCMPMLR
	move	"OLLOLS-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	setitem	Nord01ECEditMlr,0,COMPOLDMLR
	clear	MKEY
	pack	MKEY,COMPOLDMLR,"000"
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	rep	zfill,MKEY
	call	OrderLoadMailer	using C8
.BROKER
.START PATCH 3.75.7 REMOVED LOGIC - TEMPORARY LOGIC
.	setitem	Nord01ECEditBrk,0,NCMPBRK
.	setitem	Nord01ECEditBrkContact,0,NCMPBRKCNT
.END PATCH 3.75.7 REMOVED LOGIC - TEMPORARY LOGIC
	clear	NBRKFLD
	call	TRIM using NCMPBRK
	count	N1,NCMPBRK
	if (N1 <> C0)
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.		pack	NBRKFLD,NCMPBRK,NCMPBRKCNT
		pack	CNCTFLD,NCMPBRK,NCMPBRKCNT
		rep	zfill,CNCTFLD
		move	"O.LoadLOLScreens-CNCTKEY",Location
		pack	KeyLocation,"Key: ",CNCTFLD
		call	CNCTKEY
		if not over
			pack	NBRKFLD,CNCTCNT
			unpack	CNCTCNT,str4,str3
		else
			pack	COMPFLD,NCMPBRK
			move	"O.LoadLOLScreensB-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			pack	NBRKFLD,COMPOLDBRK,"000"
			unpack	NBRKFLD,str4,str3
		endif
.This logic has been temporarily moved, until LOL file has been converted
		setitem	Nord01ECEditBrk,0,str4
		setitem	Nord01ECEditBrkContact,0,str3
.END PATCH 3.75.7 REPLACED LOGIC
		call	OrderLoadBroker	using C8
	else
.START PATCH 3.75.7 ADDED LOGIC - TEMPORARY LOGIC
		setitem	Nord01ECEditBrk,0,""
		setitem	Nord01ECEditBrkContact,0,""
.END PATCH 3.75.7 ADDED LOGIC - TEMPORARY LOGIC
		setitem	Nord01ECStatBrkComp,0,""
		setitem	Nord01ECStatBrkCntName,0,""
.START PATCH 3.71.9 ADDED LOGIC
		setitem	Nord01ECStatBrkNew,0,""
.END PATCH 3.71.9 ADDED LOGIC
	endif
.PO NUM
	setitem	Nord01ECEditPO,0,NCMPPO
.SHIP-TO
	pack	NRTNFLD,NCMPSHIPTO
	move	"O.LoadLOLScreens-NRTNKEY",Location
	pack	KeyLocation,"Key: ",NRTNFLD
	call	NRTNKEY
	if over
		setitem	Nord01ECEditRtn,0,""
		setitem	Nord01ECStatRtnComp,0,""
	else
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.		setitem	Nord01ECEditRtn,0,NCMPSHIPTO
		setitem	Nord01ECEditRtn,0,RTNUM
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		setitem	Nord01ECStatRtnComp,0,RTCOMP
	endif
.PLANNER
	call	TRIM using NCMPPLANNER
	call	OrderLoadCombo Using Nord01ECComboPlanner,NCMPPLANNER
.CONTACT
	call	TRIM using NCMPCNT
	call	OrderLoadCombo Using Nord01ECComboContact,NCMPCNT
.PACKAGE
.TESTING
.	 setitem Nord01ECComboPackage,0,1
.OFFER
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	move	NCMPMLR,str4
.	pack	str7,NCMPMLR,NLOLOFFER
	pack	COMPFLD,NCMPMLR
	move	"OLLOLSB-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	move	COMPOLDMLR,str4
	pack	str7,COMPOLDMLR,NLOLOFFER
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	call	OrderLoadOffer using C8	 .Offer	reloaded whenever mailer field is changed
.SAMPLE
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	move	NCMPMLR,str4
	move	COMPOLDMLR,str4
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	call	OrderLoadSamples using C8
.End of	Campaign Vars
.LIST
	setitem	Nord01ECEditList,0,NLOLLIST
	pack	NDATFLD,NLOLLIST
	call	OrderLoadList using C8
	call	OrderLoadListColor using C8
.XSTAT
	move	NLOLLIST,OLNUM
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	move	NCMPMLR,OMLRNUM
	pack	COMPFLD,NCMPMLR
	move	"OLLOLSC-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	move	COMPOLDMLR,OMLRNUM
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
	call	OrderTestXSTAT
	if (taskname = "")
.I can do the following	as taskname=NULL and is	not filled until after second pointer is used
.Check out subroutine and trace	DimPtr/DimPtr1 to see what I mean.
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.		pack	NORDFLD1,"01R",NCMPMLR
		pack	NORDFLD1,"01R",COMPOLDMLR
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		pack	NORDFLD2,"02R",NLOLLIST
		clear	NORDFLD3
		clear	NORDFLD4
		call	OrderGetHistory	using taskname,taskname,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C8
	endif
	setitem	Nord01ECStatXSTAT,0,taskname
.OWNER
	setitem	Nord01ECEditOwner,0,NLOLOWNER
	move	NLOLOWNER,NOWNFLD
	rep	ZFILL,NOWNFLD
	call	OrderLoadOwner using C8
.START PATCH 3.72 REPLACED LOGIC - MOVED TO BELOW
..UNIVERSE
.	move	NLOLUNIVERSE,str13
..START PATCH 3.4 ADDED LOGIC
.	call	Trim using str13
..END PATCH 3.4 ADDED LOGIC
.	call	FormatNumeric using str13,str17
.	setitem	Nord01ECEditUniverse,0,str17
.END PATCH 3.72 REPLACED LOGIC - MOVED TO BELOW
......................................
.LIST SELECT/UNIVERSE
.START PATCH 3.72 REPLACED LOGIC
.	setitem	Nord01ECEditListSel,0,NLOLSELECT
..............................................
	packkey	NSEL2FLD,"2",NLOLLOL
	move	"O.LoadLOLScreens-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		setitem	Nord01ECEditListSel,0,NLOLSELECT
		move	NLOLUNIVERSE,str13
		call	Trim using str13
		call	FormatNumeric using str13,str17
		setitem	Nord01ECEditUniverse,0,str17
		setitem	Nord01ECEditPrice,0,""
		setitem	Nord01ECEditSelPrice,0,""
		setitem	Nord01ECStatSelPrice,0,""
		setitem	Nord01ECComboMod,0,1
		setitem	Nord01ECStatUniverse2,0,""
		setitem	Nord01ECStatPrice2,0,""
		setitem	Nord01ECStatRefPrice,0,""
		setitem	Nord01ECStatTotPrice,0,""
.		setprop	Nord01ECEditListSel,font=">MS Sans Serif'(8,BOLD)"
		setprop	Nord01ECStatListSel,fgcolor=red
	else
		setitem	Nord01ECEditListSel,0,NSEL2NAME
		if (NSEL2NUM = "XXXX")
.			setprop	Nord01ECEditListSel,font=">MS Sans Serif'(8,BOLD)"
			setprop	Nord01ECStatListSel,fgcolor=red
		else
.			setprop	Nord01ECEditListSel,font=">MS Sans Serif'(8)"
			setprop	Nord01ECStatListSel,fgcolor=black
		endif
		move	C0,N10
		move	NSEL2QTY,N10
		if (N10 = C0)
			clear	str13
		else
			move	N10,str10
			call	FormatNumeric using str10,str13
		endif
		setitem	Nord01ECEditUniverse,0,str13
.
		move	C0,N10
		move	NSEL2QTY2,N10
		if (N10 = C0)
			clear	str13
		else
			move	N10,str10
			call	FormatNumeric using str10,str13
		endif
		setitem	Nord01ECStatUniverse2,0,str13
.
		if (NSEL2PRICE = C0)
			clear	str9
		else
			unpack	NSEL2PRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
		endif
		setitem	Nord01ECEditPrice,0,str9
.
		if (NSEL2SPRICE = C0)
			clear	str9
		else
			unpack	NSEL2SPRICE,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
		endif
		setitem	Nord01ECEditSelPrice,0,str9
.
		if (NSEL2SPRICE2 = C0)
			clear	str9
		else
			unpack	NSEL2SPRICE2,str5,str3
			call	FormatNumeric using str5,str6
			pack	str9,str6,str3
		endif
		setitem	Nord01ECStatSelPrice,0,str9
.
		if (NSEL2PRCD = "2")
			move	"Exc. Only",str25
		else
			if (NSEL2PRICE2 = C0)
				clear	str25
			else
				unpack	NSEL2PRICE2,str5,str3
				call	FormatNumeric using str5,str6
				pack	str9,str6,str3
				pack	NMODFLD,NSEL2DESC2
				rep	zfill,NMODFLD
				move	"O.LoadLOLScreens-NMODKEY",Location
				pack	KeyLocation,"Key: ",NMODFLD
				call	NMODKEY
				call	Trim using NMODDESC
				pack	str25,str9,NMODDESC
			endif
		endif
		setitem	Nord01ECStatPrice2,0,str25
.
		move	C0,N3
		move	NSEL2DESC,N3
		add	C1,N3
		setitem	Nord01ECComboMod,0,N3
	endif
.Pricing
	call	OrderLoadSelectRef using C8
.
	pack	NSEL3FLD1,"01X2",NLOLLOL
	move	"O.LoadLOLScreens-NSEL3AIM",Location
	pack	KeyLocation,"Key: ",NSEL3FLD1
	call	NSEL3AIM
	loop
		until over
		if (NSEL3CODE = "A")
			pack	NADDFLD,OLNUM,NSEL3NUM
			move	"O.LoadLOLScreens-NADDKEY",Location
			pack	KeyLocation,"Key: ",NADDFLD
			call	NADDKEY
			if not over
.START PATCH 3.74 ADDED LOGIC
				move	NSEL3PRICE,NADDPRICE
.END PATCH 3.74 ADDED LOGIC
				call	OrderLoadRefAddressing using Nord08A1ListViewRef2
			endif
		elseif (NSEL3CODE = "L")
			pack	NSLTFLD,OLNUM,NSEL3NUM
			move	"O.LoadLOLScreens-NSLTKEY",Location
			pack	KeyLocation,"Key: ",NSLTFLD
			call	NSLTKEY
			if not over
.START PATCH 3.74 ADDED LOGIC
				move	NSEL3PRICE,NSLTPRICE
.END PATCH 3.74 ADDED LOGIC
				call	OrderLoadRefSelection using Nord08A1ListViewRef2
			endif
		endif
		move	"O.LoadLOLScreens-NSEL3KG",Location
		call	NSEL3KG
	repeat
	call	OrderAddRefTotal8
.END PATCH 3.72 REPLACED LOGIC
......................................
.RECORD	DATE
	call	TRIM using NLOLDATE
	if (NLOLDATE <>	"")
		unpack	NLOLDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	setitem	Nord01ECEditLOLDate,0,newdate1
.MAIL DATE
	call	TRIM using NLOLMDATE
	if (NLOLMDATE <> "")
		unpack	NLOLMDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	setitem	Nord01ECEditMailDate,0,newdate1
.REVISE
	call	TRIM using NLOLMODDATE
	if (NLOLMODDATE	<> "")
		unpack	NLOLMODDATE,CC,YY,MM,DD
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	newdate1
	endif
	call	Trim using NLOLINITS
	clear	str55
.START PATCH 3.4 ADDED LOGIC
	if (newdate1 <>	"" | NLOLINITS <> "")
.END PATCH 3.4 ADDED LOGIC
.START PATCH 3.72 REPLACED LOGIC - DONE BECAUSE OBJECT WIDTH WAS DECREASED DUE TO FORM REARRANGEMENT
.		append	"Last Revised:	",str55
		append	"Revised: ",str55
.END PATCH 3.72 REPLACED LOGIC
		append	newdate1,str55
.START PATCH 3.4 ADDED LOGIC
		if (NLOLINITS <> "")
.END PATCH 3.4 ADDED LOGIC
.START PATCH 3.72 REPLACED LOGIC
.			append	" By ",str55
			append	newline,str55
			append	"By ",str55
.END PATCH 3.72 REPLACED LOGIC
			append	NLOLINITS,str55
.START PATCH 3.4 ADDED LOGIC
		endif
.END PATCH 3.4 ADDED LOGIC
		reset	str55
.START PATCH 3.4 ADDED LOGIC
	endif
.END PATCH 3.4 ADDED LOGIC
	setitem	Nord01ECStatRevise,0,str55
.REGIONAL
.START PATCH 10/19/2004 ADDED LOGIC
	move	C0,N9
.END PATCH 10/19/2004 ADDED LOGIC
	move	NLOLREGIONAL,N9
	setitem	Nord01ECCheckRegional,0,N9
.GROSS/NET QTY
	move	NLOLQTY,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01ECEditGrossQty,0,str17
	move	NLOLNETQTY,str13
	call	FormatNumeric using str13,str17
	setitem	Nord01ECEditNetQty,0,str17
.NET PERCENTAGE
	setitem	Nord01ECEditNetPer,0,NLOLNET
.EXCHANGE/RENT
	setitem	Nord01ECCheckExchange,0,0
	setitem	Nord01ECCheckRent,0,0
	if (NLOLRENT = "1")
		setitem	Nord01ECCheckExchange,0,1
	elseif (NLOLRENT = "2")
		setitem	Nord01ECCheckRent,0,1
	elseif (NLOLRENT = "3")
		setitem	Nord01ECCheckExchange,0,1
		setitem	Nord01ECCheckRent,0,1
	endif
.TESTS/CONT
	setitem	Nord01ECCheckTest,0,0
	setitem	Nord01ECCheckReTest,0,0
	if (NLOLTEST = "1")
		setitem	Nord01ECCheckTest,0,1
	elseif (NLOLTEST = "2")
		setitem	Nord01ECCheckReTest,0,1
	endif
.RESPONSE
.TESTING
.	 setitem Nord01ECEditResp,0,NLOLRATE
.GIFT
.TESTING
.	 setitem Nord01ECEditGift,0,NLOLGIFT
.SPECIAL INSTRUCTIONS
	setitem	Nord01ECEditSpecial,0,NLOLCOMMENT
	setitem	Nord01ECEditSpecial1,0,NLOLCOMMENT1
.CALCULATED FIELDS
.TESTING
.	 setitem Nord01ECStatRate,0,""
.	 setitem Nord01ECStatRtn,0,""
.	 setitem Nord01ECStatGift,0,""
.	 setitem Nord01ECStatRevenue,0,""
.	 setitem Nord01ECStatCost,0,""
.	 setitem Nord01ECStatListCost,0,""
.	 setitem Nord01ECStatTotalCost,0,""
.	 setitem Nord01ECStatNetAdj,0,""
.	 setitem Nord01ECStatCostMember,0,""
.	 setitem Nord01ECStatExcTot,0,""
.	 setitem Nord01ECStatRentTot,0,""
.PACKAGES
	call	OrderLoadMlrPackageListView using Nord01ECListViewMlrPackages,OMLRNUM
	call	OrderLoadPackageListView using Nord01ECListViewPackages,NLOLLOL,C1
	return

OrderStatsLoadRecords
	call	OrderCalcCleanUpExcel
.Load Actual Returns
	NSTA0002ListView.DeleteAllItems giving N9
	move	"NSTA0002ButtonOk-STATKEY",Location
	pack	KeyLocation,"Key: ",STATFLD,COMMA,STATFLD2
	call	STATKEY
	if not over
		if (STATPATH <>	C2)
			move	C2,STATPATH
			packkey	STATFLD2,STATLR
			move	"STATSOK2-STATKEY",Location
			pack	KeyLocation,"Key: ",STATFLD2
			call	STATKEY
		endif
		loop
			until over
			until (STATLR <> STATFLD2)
			call	OrderLoadStatsListView
			move	"NSTA0002ButtonOk-STATKS",Location
			pack	KeyLocation,"Key: ",STATFLD2
			call	STATKS
		repeat
		NSTA0002ListView.EnsureVisible using 0,0
		NSTA0002ListView.SetItemState giving N9 using 0,2,2
		NSTA0002ListView.GetItemCount giving N9
		move	N9,str9
		call	FormatNumeric using str9,str11
		pack	str25,str11," Records Found."
		setitem	NSTA0002StatRecords,0,str25
		call	Click_NSTA0002ListView
	endif
.Load Projected	Returns
	NSTA0002ListView2.DeleteAllItems giving N9
	NSTA001BListViewPackage.DeleteAllItems giving N9
	if (STATFLD2 <>	"")
		move	STATFLD2,STATLR
	endif
	call	Trim using STATLR
	if (STATLR <> "")
		move	"NSTA0002ButtonOk-STAT2AIM",Location
		pack	STAT2FLD2,"01X",STATLR
		clear	STAT2FLD3
		pack	KeyLocation,"Key: ",STAT2FLD2
		call	STAT2AIM
		if not over
			loop
				until over
				until (STATLR <> STATFLD2)
				call	OrderLoadStatsListView2
				move	"NSTA0002ButtonOk-STAT2KG",Location
				pack	KeyLocation,"Key: ",STAT2FLD2
				call	STAT2KG
			repeat
			NSTA0002ListView2.EnsureVisible using 0,0
			NSTA0002ListView2.SetItemState giving N9 using 0,2,2
			NSTA0002ListView2.GetItemCount giving N9
			move	N9,str9
			call	FormatNumeric using str9,str11
			pack	str25,str11," Records Found."
			setitem	NSTA0002StatRecords2,0,str25
			call	Click_NSTA0002ListView2
		endif
	endif
	NSTA0002ListView2.GetItemCount giving N9
	if (N9 > 0)
		setprop	NSTA0002ButtonModify,enabled=1
		setprop	NSTA0002ButtonDelete,enabled=1
	else
		setprop	NSTA0002ButtonModify,enabled=0
		setprop	NSTA0002ButtonDelete,enabled=0
	endif
	return

OrderLoadStatsListView
	pack	hold10,STATVARS
	NSTA0002ListView.InsertItem giving	N9 using STATSRCE
	NSTA0002ListView.SetItemText using	N9,STATPANEL,1
	move	STATMQTY,str8
	call	Trim using str8
	call	FormatNumeric using str8,str10
	NSTA0002ListView.SetItemText using	N9,str10,2
	NSTA0002ListView.SetItemText using	N9,hold10,3
	return

OrderLoadStatsListView2
	pack	hold10,STATVARS
	NSTA0002ListView2.InsertItem giving N9 using STATSRCE
	move	C1,NPKGPATH
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.4 REPLACED LOGIC
..	pack	NPKGFLD,STATMLR,STATPCKNUM
.	move	"LoadListView-COMPKEY3",Location
.	pack	COMPFLD3,STATMLR
.	pack	KeyLocation,"Key: ",COMPFLD3
.	call	COMPKEY3
.	if over
.		clear	COMPNUM
.	endif
.	pack	NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 3.75.4 REPLACED LOGIC
	pack	NPKGFLD,STATMLR,STATPCKNUM
.END PATCH 3.75.9 REPLACED LOGIC
	move	"LoadListView-NPKGKEY",Location
	pack	KeyLocation,"Key: ",NPKGFLD
	call	NPKGKEY
	if over
		clear	NPKGPNAME
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.	elseif (NPKGMaster = "1")
.		clear	NPKGPNAME
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
	endif
	NSTA0002ListView2.SetItemText using N9,NPKGPNAME,1
	move	STATRECQTY,str9
	call	Trim using str9
	call	FormatNumeric using str9,str11
	NSTA0002ListView2.SetItemText using N9,str11,2
	pack	str9,STATLR,STATNUM
	NSTA0002ListView2.SetItemText using N9,str9,3
	NSTA0002ListView2.SetItemText using N9,hold10,4
	return

OrderLoadStatsScreen
	setitem	NSTA001AEditMlr,0,STATMLR
.START PATCH 3.75.9 REPLACED LOGIC
.	move	C1,NMLRPATH
.	pack	MKEY,STATMLR,"000"
.	move	"LoadSTATS-NMLRKEY",Location
.	pack	KeyLocation,"Key: ",MKEY
.	call	NMLRKEY
.	if over
.		clear	MCOMP
.	endif
.	setitem	NSTA001AStatMlrName,0,MCOMP
..........................
	pack	COMPFLD,STATMLR
	move	"LoadStats-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if over
		clear	COMPCOMP
	elseif (COMPMLRFLG <> "T")
		clear	COMPCOMP
	endif
	setitem	NSTA001AStatMlrName,0,COMPCOMP
.END PATCH 3.75.9 REPLACED LOGIC
	setitem	NSTA001BEditSource,0,STATSRCE
	setitem	NSTA001AEditLR,0,STATLR
	setitem	NSTA001BEditLRB,0,STATLR
	setitem	NSTA001AEditCampaign,0,STATCAMPN
	setitem	NSTA001BEditKeyCode,0,STATKYCD
	setitem	NSTA001AEditListNum,0,STATLIST
	move	C1,NDATPATH
	pack	NDATFLD,STATLIST
	move	"LoadStats-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	if over
		clear	OLSTNAME
	endif
	setitem	NSTA001AStatList,0,OLSTNAME
	setitem	NSTA001BStatListB,0,OLSTNAME
	setitem	NSTA001AEditMlrList,0,STATLDES
	unpack	STATLCPM,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	setitem	NSTA001BEditListCost,0,str10
	unpack	STATIMCST,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditMailCost,0,str10
	unpack	STATLVAL,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	setitem	NSTA001BEditLValue,0,str10
	call	Trim using STATPDATE
	if (STATPDATE <> "")
		unpack	STATPDATE,MM,DD,str4
		pack	str10,MM,SLASH,DD,SLASH,str4
	else
		clear	str10
	endif
	setitem	NSTA001BEditPDate,0,str10
	call	Trim using STATMDATE
	if (STATMDATE <> "")
		unpack	STATMDATE,MM,DD,str4
		pack	str10,MM,SLASH,DD,SLASH,str4
	else
		clear	str10
	endif
	setitem	NSTA001AEditMailDate,0,str10
	move	STATMQTY,str8
	call	FormatNumeric using str8,str11
	setitem	NSTA001AEditMailQty,0,str11
	setitem	NSTA001AEditPackID,0,STATPCKCDE
	setitem	NSTA001BEditPackIDB,0,STATPCKCDE
	setitem	NSTA001AEditPackage,0,STATPANEL
	setitem	NSTA001BEditPackageB,0,STATPANEL
	unpack	STATPACK,str5,str3
	call	FormatNumeric using str5,str6
	pack	str10,str6,str3
	setitem	NSTA001BEditPackageCost,0,str10
	unpack	STATPCKM,str5,str3
	call	FormatNumeric using str5,str6
	pack	str10,str6,str3
	setitem	NSTA001BEditPackageCostThou,0,str10
	move	STATRESP,str7
	call	FormatNumeric,str7,str9
	setitem	NSTA001BEditResponse,0,str9
	move	STATREV,str9
	call	FormatNumeric,str9,str11
	setitem	NSTA001BEditRevenue,0,str11
	setitem	NSTA001AEditSelect,0,STATSEL
	setitem	NSTA001BEditSelectB,0,STATSEL
	setitem	NSTA001AEditType,0,STATTYPE
	move	STATWKSO,str6
	call	FormatNumeric using str6,str7
	setitem	NSTA001AEditWeeks,0,str7
.START PATCH 3.4 ADDED LOGIC
.Calculated Fields
	move	C0,STATRESP2
	move	C0,STATGIFT
	call	OrderCalcStatsetReturnValues using STATRESP,STATMQTY,STATREV
	call	OrderCalcStatGetReturnValues using STATRESP2,STATGIFT
.
	move	STATRESP2,str6
	call	Trim using str6
	setitem	NSTA001AEditRespRate,0,str6
.
	unpack	STATGIFT,str4,str3
	call	FormatNumeric using str4,str5
	pack	str8,str5,str3
	setitem	NSTA001AEditAvgGift,0,str8
.
	call	Trim using statlr
	if (statlr <> "")
		pack	NORDFLD,statlr
		move	C1,NORDPATH
		move	"LoadStat-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		if not over
			call	Trim using OCAMP
			if (OCAMP <> "")
				pack	NCMPFLD,OCAMP
				move	C1,NCMPPATH
				move	"LoadStat-NCMPKEY",Location
				pack	KeyLocation,"Key: ",NCMPFLD
				call	NCMPKEY
				if over
					call	OrderStatsLookAtMailer
				else
					call	Trim using NCMPRPT
					if (NCMPRPT = "")
						call	OrderStatsLookAtMailer
					endif
				endif
			else
				call	OrderStatsLookAtMailer
			endif
		else
			call	OrderStatsLookAtMailer
		endif
	else
		call	OrderStatsLookAtMailer
	endif
	getitem	NSTA001AEditType,0,str1
	move	"100",statnetrec
	call	OrderCalcStatSetValues using statrecqty,statmqty,N1,statlcpm,statnetreq,statnetrec,statavgnet,statresp2,statgift,str1,statpckm,NPRCPremium,C0,statexbase,statrbase,statselfee,statrun,statship,NCMPRPT
	call	OrderCalcStatGetValues using CALCSTATNETNAME,statresp,statrev,CALCSTATPROCOST,CALCSTATLSTCOST,CALCSTATTOTCOST,CALCSTATNETP,CALCSTATCOSTMEM,N1,N1,N1,CALCSTATEXTOT,CALCSTATRTOT
.
	move	CALCSTATTOTCOST,N9
	move	N9,str9
	call	FormatNumeric using str9,str11
	setitem	NSTA001BEditTotalCost,0,str11
.
	unpack	CALCSTATCOSTMEM,str6,str3
	if (CALCSTATCOSTMEM < 0)
		call	RemoveChar using str6,DASH
		move	DASH,str1
	else
		clear	str1
	endif
	call	FormatNumeric using str6,str7
	pack	str11,str1,str7,str3
	setitem	NSTA001AEditCostMember,0,str11
.END PATCH 3.4 ADDED LOGIC
	return

.START PATCH 3.4 ADDED LOGIC
OrderStatsLookAtMailer
.START PATCH 3.75.9 REPLACED LOGIC
.	if (statmlr = "0170")
	if (statmlr = "000913")
.END PATCH 3.75.9 REPLACED LOGIC
		move	C3,NCMPRPT
	else
		clear	NCMPRPT
	endif
	return
.END PATCH 3.4 ADDED LOGIC

OrderLoadStatsScreen2
.First load the	fields that are	not tied to the	Order/LOL record
	setitem	NSTA001AStatMlr2B,0,STATMLR
	setitem	NSTA001AEditMlr2B,0,STATMLR
.START PATCH 3.75.9 REPLACED LOGIC
.	move	C1,NMLRPATH
.	pack	MKEY,STATMLR,"000"
.	move	"LoadStatsMlr-NMLRKEY",Location
.	pack	KeyLocation,"Key: ",MKEY
.	call	NMLRKEY
.	if over
.		clear	MCOMP
.	endif
.	setitem	NSTA001AStatMlrName,0,MCOMP
.............................
	pack	COMPFLD,STATMLR
	move	"LoadStatsMlr-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if over
		clear	COMPCOMP
	elseif (COMPMLRFLG <> "T")
		clear	COMPCOMP
	endif
	setitem	NSTA001AStatMlrName,0,COMPCOMP
.END PATCH 3.75.9 REPLACED LOGIC
.
	setitem	NSTA001AEditPackNum2,0,STATPCKNUM
	setitem	NSTA001BStatPackNum2C,0,STATPCKNUM
	move	C1,NPKGPATH
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.4 REPLACED LOGIC
..	pack	NPKGFLD,STATMLR,STATPCKNUM
.	move	"Load.Stats-COMPKEY3",Location
.	pack	COMPFLD3,STATMLR
.	pack	KeyLocation,"Key: ",COMPFLD3
.	call	COMPKEY3
.	if over
.		clear	COMPNUM
.	endif
.	pack	NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 3.75.4 REPLACED LOGIC
	pack	NPKGFLD,STATMLR,STATPCKNUM
.END PATCH 3.75.9 REPLACED LOGIC
	move	"Load.Stats-NPKGKEY",Location
	pack	KeyLocation,"Key: ",NPKGFLD
	call	NPKGKEY
	if over
		clear	NPKGPNAME
		clear	NPKGID
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.	elseif (NPKGMaster = "1")
.		clear	NPKGPNAME
.		clear	NPKGID
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
	endif
	call	StatsLoadMasterColor
	setitem	NSTA001AStatPackage2B,0,NPKGPNAME
	setitem	NSTA001BStatPackName2,0,NPKGPNAME
	setitem	NSTA001AStatPackID2B,0,NPKGID
.
	unpack	STATIMCST,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditMailCost2,0,str10
.
	unpack	STATPACK,str5,str3
	call	FormatNumeric using str5,str6
	pack	str10,str6,str3
	setitem	NSTA001BEditPackageCost2,0,str10
.
	call	OrderLoadStatsSTATPCKM
.
	move	STATREV,str9
	call	FormatNumeric using str9,str11
	setitem	NSTA001BStatRevenue2B,0,str11
.
	move	C0,N1
	move	STATLOL,N1
	setitem	NSTA001ACheckLOL2,0,N1
	setitem	NSTA001BCheckLOL2B,0,N1
.
	unpack	STATRESP2,str6
	setitem	NSTA001AEditRespRate2,0,str6
.
	unpack	STATGIFT,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	setitem	NSTA001AEditAvgGift2,0,str10
.
	move	STATMQTY,str8
	call	FormatNumeric using str8,str10
	setitem	NSTA001AEditMailQty2,0,str10
.
	move	statrecqty,str9
	call	FormatNumeric using str9,str11
	setitem	NSTA001AEditRecoQty2,0,str11
.
	unpack	statavgnet,str6
	setitem	NSTA001AEditAvgNet2,0,str6
.
	unpack	STATNETREQ,str6
	setitem	NSTA001AEditNetReq2,0,str6
.
	unpack	STATNETREC,str6
	setitem	NSTA001AEditNetRec2,0,str6
.
	unpack	STATLVAL,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	setitem	NSTA001BEditLValue2,0,str10
.
	unpack	STATEXBASE,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditExBase2,0,str10
.
	unpack	STATRBASE,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditRentBase2,0,str10
.
	unpack	STATRUN,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditRunCharge2,0,str10
.
	unpack	STATSELFEE,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditSelectFee2,0,str10
.
	unpack	STATSHIP,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	setitem	NSTA001BEditShipTape2,0,str10
.START PATCH 3.43 ADDED LOGIC
	setitem	NSTA001AEditType2B,0,STATTYPE
.END PATCH 3.43 ADDED LOGIC
.START PATCH 3.47 ADDED LOGIC
.START PATCH 3.72 MOVED LOGIC TO ANOTHER LOCATION
.	if (statseluni > 0)
.		move	statseluni,str9
.		call	FormatNumeric using str9,str11
.	else
.		clear	str11
.	endif
.	setitem	NSTA001AEditSelUniverse2,0,str11
.END PATCH 3.72 MOVED LOGIC TO ANOTHER LOCATION
.END PATCH 3.47 ADDED LOGIC
.START PATCH 3.45 ADDED LOGIC
.	setitem	NSTA001AEditSelect2,0,STATSEL
.END PATCH 3.45 ADDED LOGIC
.
.	if (STATLOL = "1")
.		pack	NCMPFLD,NLOLCNUM
.	else
.		pack	NCMPFLD,OCAMP
.	endif
.	move	C1,NCMPPATH
.	move	"LoadStats-NCMPKEY",Location
.	pack	KeyLocation,"Key: ",NCMPFLD
.	call	NCMPKEY
.	call	OrderCallCalcFields using C1
.
	move	C1,STATNPATH
	packkey	STATNFLD,STATLR,STATLOL,STATPCKNUM
	rep	zfill,STATNFLD
	move	"LoadStats-STATNKEY",Location
	pack	KeyLocation,"Key: ",STATNFLD
	call	STATNKEY
	setitem	NSTA001CEditNotes2,0,STATNNOTE
.START PATCH 3.46 REMOVED LOGIC
.	if (STATLOL <> "1")
.		pack	NSPEFLD,STATLR
.		rep	zfill,NSPEFLD
.		move	C3,NSPELOCK
.		move	"LoadStats-NSPEKEY",Location
.		pack	KeyLocation,"Key: ",NSPEFLD
.		call	NSPEKEY
.		setitem	NSTA001CEditNotes5,0,DESC002
.	else
.		setitem	NSTA001CEditNotes5,0,""
.	endif
.	pack	NSPE2FLD,STATLR
.	rep	zfill,NSPE2FLD
.	move	C3,NSPE2LOCK
.	move	"LoadStats-NSPE2KEY",Location
.	pack	KeyLocation,"Key: ",NSPE2FLD
.	call	NSPE2KEY
.	setitem	NSTA001CEditNotes3,0,DESC003
.	setitem	NSTA001CEditNotes4,0,DESC004
.END PATCH 3.46 REMOVED LOGIC
.
	call	OrderLoadStatsScreen2B using C0
	call	OrderCallCalcFields using C1
	getitem	NSTA001AStatListNum2B,0,str6
	getitem	OptionsScreen10Usage,0,N1
	if (N1 = 1)
		call	OrderLoadStatsScreenUsage
	else
		setitem	NSTA001BStatUsage2B,0,""
		setitem	NSTA001BStatOrders2B,0,""
.START PATCH 3.72.6 ADDED LOGIC
		setitem	NSTA001AStatUsage2A,0,""
		setitem	NSTA001AStatOrders2A,0,""
.END PATCH 3.72.6 ADDED LOGIC
	endif
	return

.START PATCH 3.49.2 ADDED LOGIC
StatsUpdateScreen10 LRoutine DimPtr,FrmPtr
	move	DimPtr,STAT2FLD
	move	C0,N1
	move	SEQ,result
	loop
		move	result,N9
		NSTA001DListView.GetNextItem giving result using C0,N9
		until (result =	SEQ)
		NSTA001DListView.GetItemText giving hold10 using result,22
.START PATCH 3.75.9 REPLACED LOGIC
.		unpack	hold10,str55,str55,str55,str55,str25,str3,str6,str55,str55,str55,str9,str3
		unpack	hold10,str55,str55,str55,str55,str25,str8,str6,str55,str55,str55,str9,str3
.END PATCH 3.75.9 REPLACED LOGIC
		pack	str9,str6,str3
		if (str9 = STAT2FLD)
			NSTA001DListView.DeleteItem giving N8 using result
			move	C1,N1
		endif
	repeat
	move	SEQ,result
	loop
		move	result,N9
		NSTA001DListView2.GetNextItem giving result using	C0,N9
		until (result =	SEQ)
		NSTA001DListView2.GetItemText giving hold10 using result,21
.START PATCH 3.75.9 REPLACED LOGIC
.		unpack	hold10,str55,str55,str55,str55,str25,str3,str6,str55,str55,str55,str9,str3
		unpack	hold10,str55,str55,str55,str55,str25,str8,str6,str55,str55,str55,str9,str3
.END PATCH 3.75.9 REPLACED LOGIC
		pack	str9,str6,str3
		if (str9 = STAT2FLD)
			NSTA001DListView2.DeleteItem giving N8 using result
			move	C1,N1
		endif
	repeat
	move	SEQ,result
	loop
		move	result,N9
		NSTA001DListView3.GetNextItem giving result using C0,N9
		until (result =	SEQ)
		NSTA001DListView3.GetItemText giving hold10 using result,22
.START PATCH 3.75.9 REPLACED LOGIC
.		unpack	hold10,str55,str55,str55,str55,str25,str3,str6,str55,str55,str55,str9,str3
		unpack	hold10,str55,str55,str55,str55,str25,str8,str6,str55,str55,str55,str9,str3
.END PATCH 3.75.9 REPLACED LOGIC
		pack	str9,str6,str3
		if (str9 = STAT2FLD)
			NSTA001DListView3.DeleteItem giving N8 using result
			move	C1,N1
		endif
	repeat
	move	SEQ,result
	loop
		move	result,N9
		NSTA001DListView4.GetNextItem giving result using	C0,N9
		until (result =	SEQ)
		NSTA001DListView4.GetItemText giving hold10 using result,22
.START PATCH 3.75.9 REPLACED LOGIC
.		unpack	hold10,str55,str55,str55,str55,str25,str3,str6,str55,str55,str55,str9,str3
		unpack	hold10,str55,str55,str55,str55,str25,str8,str6,str55,str55,str55,str9,str3
.END PATCH 3.75.9 REPLACED LOGIC
		pack	str9,str6,str3
		if (str9 = STAT2FLD)
			NSTA001DListView4.DeleteItem giving N8 using result
			move	C1,N1
		endif
	repeat
	if (FrmPtr = C1)
		return
	endif
.Clear Order variables used in List View
	if (N1 = C1)
		if (STATLOL <> "1")
			move	C0,N1
		endif
.Need to recall the variables.  Otherwise following routine will load them into hold10 after the Verify routine, which trimmed them all
		rep	zfill,STAT2FLD
		pack	KeyLocation,"Key: ",STAT2FLD
		move	"Update10-STAT2KEY",Location
		call	STAT2KEY
		if not over
			call	OrderLoadNSTA001DListViews using N1
			if (View10Flag = 1)
				call	Click_NSTA001DListView
			elseif (View10Flag = 2)
				call	StatsSortbyPackageA
				call	Click_NSTA001DListView2
			elseif (View10Flag = 3)
				call	Click_NSTA001DListView3
			elseif (View10Flag = 4)
				call	StatsSortbyCost
				call	Click_NSTA001DListView4
			endif
		endif
	endif
	return
.END PATCH 3.49.2 ADDED LOGIC

.START PATCH 3.42 ADDED	LOGIC
StatsLoadMasterColorA
	setitem	NSTA001BStatPackNum2C,0,NPKGNUM
	setitem	NSTA001BStatPackName2,0,NPKGPNAME
StatsLoadMasterColor
	if (NPKGMASTER = "1")
		setprop	NSTA001AStatPackage2B,fgcolor=red
		setprop	NSTA001BStatPackName2,fgcolor=red
	else
		setprop	NSTA001AStatPackage2B,fgcolor=black
		setprop	NSTA001BStatPackName2,fgcolor=black
	endif
	return
.END PATCH 3.42	ADDED LOGIC

OrderLoadStatsScreenUsage
	call	OrderCalcUsage using STATMLR,str6,howmany,N10
	move	howmany,str9
	call	FormatNumeric using str9,str11
	setitem	NSTA001BStatUsage2B,0,str11
	move	N10,str10
	call	FormatNumeric using str10,str15
	setitem	NSTA001BStatOrders2B,0,str15
.START PATCH 3.72.6 ADDED LOGIC
	setitem	NSTA001AStatUsage2A,0,str11
	setitem	NSTA001AStatOrders2A,0,str15
.END PATCH 3.72.6 ADDED LOGIC
	return

OrderLoadStatsSTATPCKM Routine FrmPtr
	call	OrderLoadStatsPackage using NPKGFLD
	move	C0,N8
	if (NCMPRPT <> "3")
		clear	str10
		unpack	STATPCKM,str5,str3
		call	Trim using str5
		NSTA001BListViewPackage.GetItemCount giving N9
		sub	C1,N9
		for	howmany,"0",N9
			NSTA001BListViewPackage.GetItemText giving	str6 using howmany,0
			call	Trim using str6
			if (str5 = str6)
				NSTA001BListViewPackage.SetItemText using howmany,"X",1
				NSTA001BListViewPackage.GetItemText giving	str10 using howmany,3
				move	howmany,N8
				break
			endif
		repeat
	else
.Select	Top Item
		NSTA001BListViewPackage.SetItemText using 0,"X",1
		NSTA001BListViewPackage.GetItemText giving	str10 using 0,3
	endif
	move	"$",str1
	call	RemoveChar using str10,str1
	call	Trim using str10
.This will be a	temporary value	for statpckm.  Used only for calculation purposes.
.Real value of statpckm	will be	Julian Date of Price
	move	C0,statpckm
	move	str10,statpckm
	NSTA001BListViewPackage.EnsureVisible using N8,0
	return

OrderDeleteStatsListView
	NSTA001BListViewPackage.DeleteAllItems giving N9
	return

OrderLoadStatsPackage Routine DimPtr
	call	Trim using DimPtr
	if (DimPtr = "")
		return
	endif
.START PATCH 3.75.4 REPLACED LOGIC
.	unpack	DimPtr,str4,str6
	unpack	DimPtr,str5,str1,str6
.END PATCH 3.75.4 REPLACED LOGIC
	NSTA001BListViewPackage.DeleteAllItems giving N9
	move	C1,NPRCPATH
.START PATCH 3.75.4 REPLACED LOGIC
.	pack	NPRCFLD1,"01X",str4
	pack	NPRCFLD1,"01X",str5,str1
.END PATCH 3.75.4 REPLACED LOGIC
	pack	NPRCFLD2,"02X",str6
	move	"Load.Stats-NPRCAIM",Location
	pack	KeyLocation,"Key: ",NPRCFLD1,COMMA,NPRCFLD2
	call	NPRCAIM
	loop
		until over
		move	C0,JULDAYS
		call	Trim using NPRCDATE
		if (NPRCDATE <>	"")
			unpack	NPRCDATE,CC,YY,MM,DD
			pack	str10,MM,SLASH,DD,SLASH,CC,YY
			call	cvtjul
		else
.This should never happen as NPRCDATE is part of ISAM
			clear	str10
		endif
		move	JULDAYS,str5
		move	NPRCTOTAL,str11
		unpack	str11,str8,str3
		call	Trim using str8
		if (str8 <> "")
			call	FormatNumeric using str8,str11
		else
			clear	str11
		endif
		pack	str25,"$",str11,str3
		NSTA001BListViewPackage.InsertItem	giving result using str5
		NSTA001BListViewPackage.SetItemText using result,str10,2
		NSTA001BListViewPackage.SetItemText using result,str25,3
		move	"Load.Stats-NPRCKG",Location
		call	NPRCKG
	repeat
	return

OrderLoadStatCampaign
	call	OrderCalcCleanUpExcel
.
	move	NCMPFLD,NLOLFLD1
	move	C2,NLOLPATH
	move	"LoadStatLOL-NLOLKEY",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	call	NLOLKEY
	if over
		goto OrderLoadStatCampaign2
	endif
.START PATCH 3.67 REPLACED LOGIC
.	move	"LoadStatLOL-NLOLKS",Location
.	pack	KeyLocation,"Key: ",NLOLFLD1
.END PATCH 3.67 REPLACED LOGIC
	loop
		until (NLOLCNUM	<> NLOLFLD1)
.START PATCH 3.67 ADDED LOGIC
		eventcheck
		until (Stop3Flag = YES)
.END PATCH 3.67 ADDED LOGIC
		pack	STAT2FLD2,"01X",NLOLLOL
		pack	STAT2FLD3,"02X1"
		call	OrderLoadStatDetail using C1
.START PATCH 3.67 REPLACED LOGIC
		move	"LoadStatLOL-NLOLKS",Location
		pack	KeyLocation,"Key: ",NLOLFLD1
.END PATCH 3.67 REPLACED LOGIC
		call	NLOLKS
		until over
	repeat
OrderLoadStatCampaign2
	move	NCMPFLD,NORDFLDC
	move	C4,NORDPATH
	move	"LoadStatLR-NORDKEY",Location
	pack	KeyLocation,"Key: ",NORDFLDC
	call	NORDKEY
	if not over
		move	"LoadStatLR-NORDKS",Location
		pack	KeyLocation,"Key: ",NORDFLDC
		loop
			until (OCAMP <>	NORDFLDC)
.START PATCH 3.67 ADDED LOGIC
			eventcheck
			until (Stop3Flag = YES)
.END PATCH 3.67 ADDED LOGIC
			pack	STAT2FLD2,"01X",OLRN
			pack	STAT2FLD3,"02X0"
			call	OrderLoadStatDetail using C0
			call	NORDKS
			until over
		repeat
	endif
.START PATCH 3.48 ADDED LOGIC
	if (View10Flag = 2)
		call	StatsSortbyPackageA
	elseif (View10Flag = 4)
		call	StatsSortbyCost
	endif
.END PATCH 3.48 ADDED LOGIC
	NSTA001DListView.EnsureVisible using 0,0
	NSTA001DListView.SetItemState giving N9 using 0,2,2
	NSTA001DListView2.EnsureVisible using 0,0
	NSTA001DListView2.SetItemState giving N9 using 0,2,2
	NSTA001DListView3.EnsureVisible using 0,0
	NSTA001DListView3.SetItemState giving N9 using 0,2,2
.START PATCH 3.45 ADDED LOGIC
	NSTA001DListView4.EnsureVisible using 0,0
	NSTA001DListView4.SetItemState giving N9 using 0,2,2
.END PATCH 3.45 ADDED LOGIC
	NSTA001DListView.GetItemCount giving N9
	if (N9 > C0)
		move	N9,str9
		call	FormatNumeric using str9,str11
.START PATCH 3.67 REPLACED LOGIC
.		pack	str25,str11," Records Found."
.		setitem	NSTA001DStatTotalNames,0,str25
		pack	taskname,str11," Records Found."
		if (Stop3Flag = YES)
			pack	taskname,taskname,"  Load was aborted-Campaign may be incomplete!"
		endif
		setitem	NSTA001DStatTotalNames,0,taskname
.END PATCH 3.67 REPLACED LOGIC
		setitem	NSTA001DStatCampNum,0,NCMPNum
		setitem	NSTA001DStatCampName2,0,NCMPCName
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
..		setitem	NSTA001DStatMlrNum,0,NCMPMlr
..		setitem	NSTA001DEditMlrNum,0,NCMPMlr
..		pack	MKEY,NCMPMLR,"000"
..		move	C1,NMLRPATH
..		move	"O.LoadStatCamp.-NMLRKEY",Location
..		pack	KeyLocation,"Key: ",MKEY
..		call	NMLRKEY
..		setitem	NSTA001DStatMlrComp,0,MCOMP
...................................................
.		pack	COMPFLD,NCMPMLR
.		move	"O.LoadStatCamp.-COMPKEY",Location
.		pack	KeyLocation,"Key: ",COMPFLD
.		call	COMPKEY
.		setitem	NSTA001DStatMlrNum,0,COMPOLDMLR
.		setitem	NSTA001DEditMlrNum,0,COMPOLDMLR
.		setitem	NSTA001DStatMlrComp,0,COMPCOMP
..END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		setitem	NSTA001DStatMlrNum,0,NCMPMlr
		setitem	NSTA001DEditMlrNum,0,NCMPMlr
		pack	COMPFLD,NCMPMLR
		move	"O.LoadStatCamp.-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		setitem	NSTA001DStatMlrComp,0,COMPCOMP
.END PATCH 3.75.9 REPLACED LOGIC
		call	OrderLoadStatTotal
	endif
.START PATCH 3.48 REPLACED LOGIC
.	call	Click_NSTA001DListView2
	if (View10Flag = 1)
		call	Click_NSTA001DListView
	elseif (View10Flag = 2)
		call	Click_NSTA001DListView2
	elseif (View10Flag = 3)
		call	Click_NSTA001DListView3
	elseif (View10Flag = 4)
		call	Click_NSTA001DListView4
	endif
.END PATCH 3.48 REPLACED LOGIC
	return

OrderLoadStatTotal
	move	C0,StatsCostTotal
	move	C0,StatsRecoTotal
	move	C0,StatsRecdTotal
	move	C0,StatsRtnsTotal
	move	C0,StatsLCPMTotal
	move	C0,StatsNetTotal
	move	C0,StatsRevTotal
	move	C0,StatsProdTotal
	move	C0,StatsLCstTotal
	move	C0,StatsTotCstTotal
	move	C0,StatsNetPlusTotal
	move	SEQ,result
.START PATCH 3.48 REPLACED LOGIC
.	loop
	move	C0,StatsUnivTotal
	move	C0,howmany3
	loop
StatTotalInnerLoop
.END PATCH 3.48 REPLACED LOGIC
		move	result,N9
		NSTA001DListView.GetNextItem giving result using C0,N9
		until (result =	SEQ)
.START PATCH 3.48 ADDED LOGIC
		NSTA001DListView.GetItemText giving str7 using result,1
		unpack	str7,str6,str1
		if (str1 = "x")
			goto StatTotalInnerLoop
		endif
		add	C1,howmany3
.END PATCH 3.48 ADDED LOGIC
.
		NSTA001DListView.GetItemText giving str13	using result,8
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsRecoTotal
.
		NSTA001DListView.GetItemText giving str13	using result,9
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsRecdTotal
.
		NSTA001DListView.GetItemText giving str13	using result,11
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,StatsHoldTotal
		move	str13,StatsHoldTotal
		add	StatsHoldTotal,StatsLCPMTotal
.
		NSTA001DListView.GetItemText giving str13	using result,13
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsNetTotal
.
		NSTA001DListView.GetItemText giving str13	using result,15
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsRtnsTotal
.
		NSTA001DListView.GetItemText giving str13	using result,17
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsRevTotal
.
		NSTA001DListView.GetItemText giving str13	using result,18
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsProdTotal
.
		NSTA001DListView.GetItemText giving str13	using result,19
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsLCstTotal
.
		NSTA001DListView.GetItemText giving str13	using result,20
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsTotCstTotal
.
.START PATCH 3.48 ADDED LOGIC
		NSTA001DListView.GetItemText giving str13	using result,23
		call	Trim using str13
		call	RemoveChar using str13,COMMA
		move	C0,N10
		move	str13,N10
		add	N10,StatsUnivTotal
.END PATCH 3.48 ADDED LOGIC
	repeat
	sub	StatsTotCstTotal,StatsRevTotal,StatsNetPlusTotal
	if (StatsNetTotal > 0)
		if (StatsNetPlusTotal <> 0)
			move	StatsNetPlusTotal,StatsCostTotal
			move	StatsRtnsTotal,StatsHoldTotal
			div	StatsHoldTotal,StatsCostTotal
		endif
	endif
	unpack	StatsCostTotal,str13,str3
	if (StatsCostTotal < 0)
		call	RemoveChar using str13,DASH
		move	DASH,str1
	else
		clear	str1
	endif
	call	FormatNumeric using str13,str25
	pack	str35,str1,str25,str3
	NSTA001DListViewTotal.InsertItem giving result using str35
.
	move	StatsRecoTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,1
.
	move	StatsRecdTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,2
.
	if (StatsRecdTotal > C0)
		sub	StatsRecoTotal,StatsRecdTotal,N10
	else
		move	C0,N10
	endif
	move	N10,str10
	call	FormatNumeric using str10,str15
	NSTA001DListViewTotal.SetItemText	using result,str15,3
.
.START PATCH 3.48 REPLACED LOGIC
.	NSTA001DListView.GetItemCount giving howmany
.	div	howmany,StatsLCPMTotal
.START PATCH 3.68.5 REPLACED LOGIC
.	div	howmany3,StatsLCPMTotal
	div	"1000",StatsNetTotal,StatsLCPMTotal
	calc	StatsLCPMTotal=(StatsLCstTotal/StatsLCPMTotal)
.END PATCH 3.68.5 REPLACED LOGIC
.END PATCH 3.48 REPLACED LOGIC
	unpack	StatsLCPMTotal,str13,str3
	call	FormatNumeric using str13,str25
	pack	str35,str25,str3
	NSTA001DListViewTotal.SetItemText	using result,str35,4
.
	if (StatsRecoTotal > 0 AND StatsNetTotal > 0)
		move	StatsNetTotal,StatsNetTotalB
		move	StatsRecoTotal,StatsRecoTotalB
		div	StatsRecoTotalB,StatsNetTotalB
		mult	"100",StatsNetTotalB
		unpack	StatsNetTotalB,str13,str3
		pack	str25,str13,str3
		call	Trim using str25
		call	RemoveChar using str25,DASH
	else
		clear	str25
	endif
	NSTA001DListViewTotal.SetItemText	using result,str25,5
.
	move	StatsNetTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,6
.
	if (StatsRtnsTotal > 0 AND StatsNetTotal > 0)
		div	StatsNetTotal,StatsRtnsTotal,StatsRRTotal
.		calc	StatsRRTotal=StatsRtnsTotal/StatsNetTotal
		mult	"100",StatsRRTotal,N32
		move	N32,str6
	else
		clear	str6
	endif
	NSTA001DListViewTotal.SetItemText	using result,str6,7
.
	move	StatsRtnsTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,8
.
	move	StatsRevTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,10
.
	if (StatsRtnsTotal > 0 AND StatsRevTotal > 0)
		div	StatsRtnsTotal,StatsRevTotal,StatsHoldTotal
	else
		move	C0,StatsHoldTotal
	endif
	unpack	StatsHoldTotal,str13,str3
	call	FormatNumeric using str13,str25
	pack	str35,str25,str3
	NSTA001DListViewTotal.SetItemText	using result,str35,9
.
	move	StatsProdTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,11
.
	move	StatsLCstTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,12
.
	move	StatsTotCstTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,13
.
	move	StatsNetPlusTotal,str13
	if (StatsNetPlusTotal < 0)
		call	RemoveChar using str13,DASH
		move	DASH,str1
	else
		clear	str1
	endif
	call	FormatNumeric using str13,str25
	pack	str35,str1,str25
	NSTA001DListViewTotal.SetItemText	using result,str35,14
.
	move	StatsUnivTotal,str13
	call	FormatNumeric using str13,str25
	NSTA001DListViewTotal.SetItemText	using result,str25,15
	return

OrderLoadStatCampaignIS
	loop
		move	"LoadStatCampIS-STAT2SEQ",Location
		call	STAT2SEQ
		until over
		call	OrderLoadNSTA001DListViews using C2
     	repeat
	NSTA001DListView.EnsureVisible using 0,0
	NSTA001DListView.SetItemState giving N9 using 0,2,2
	NSTA001DListView2.EnsureVisible using 0,0
	NSTA001DListView2.SetItemState giving N9 using 0,2,2
	NSTA001DListView3.EnsureVisible using 0,0
	NSTA001DListView3.SetItemState giving N9 using 0,2,2
.START PATCH 3.45 ADDED LOGIC
	NSTA001DListView4.EnsureVisible using 0,0
	NSTA001DListView4.SetItemState giving N9 using 0,2,2
.END PATCH 3.45 ADDED LOGIC
	NSTA001DListView.GetItemCount giving N9
	if (N9 > C0)
		move	N9,str9
		call	FormatNumeric using str9,str11
		pack	str25,str11," Records Found."
		setitem	NSTA001DStatTotalNames,0,str25
		setitem	NSTA001DStatCampNum,0,""
		setitem	NSTA001DStatCampName2,0,""
		setitem	NSTA001DStatMlrNum,0,""
		setitem	NSTA001DEditMlrNum,0,""
		setitem	NSTA001DStatMlrComp,0,""
	endif
.START PATCH 3.49.1 REPLACED LOGIC
.	call	Click_NSTA001DListView2
	if (View10Flag = 1)
		call	Click_NSTA001DListView
	elseif (View10Flag = 2)
		call	Click_NSTA001DListView2
	elseif (View10Flag = 3)
		call	Click_NSTA001DListView3
	elseif (View10Flag = 4)
		call	Click_NSTA001DListView4
	endif
.END PATCH 3.49.1 REPLACED LOGIC
	return

OrderLoadStats3Screen
	move	C1,STATNPATH
	packkey	STATNFLD,STATLR,STATLOL,STATPCKNUM
	call	Trim using STATNFLD
	if (STATNFLD <>	"")
		rep	zfill,STATNFLD
		move	"LoadStats3-STATNKEY",Location
		pack	KeyLocation,"Key: ",STATNFLD
		call	STATNKEY
		setitem	NSTA001DEditNotes,0,STATNNOTE
	else
		setitem	NSTA001DEditNotes,0,""
	endif
	if (STATLOL <> "1")
		call	Trim using STATLR
		if (STATLR <> "")
			pack	NSPE2FLD,STATLR
			rep	zfill,NSPE2FLD
			move	C3,NSPE2LOCK
			move	"LoadStats3-NSPE2KEY",Location
			pack	KeyLocation,"Key: ",NSPE2FLD
			call	NSPE2KEY
			setitem	NSTA001DEditNotes3,0,DESC003
			setitem	NSTA001DEditNotes4,0,DESC004
		else		.Should never happen!!
			setitem	NSTA001DEditNotes3,0,""
			setitem	NSTA001DEditNotes4,0,""
		endif
	else
		move	C1,NLOLPATH
		pack	NLOLFLD,STATLR
		rep	zfill,NLOLFLD
		move	C3,NLOLLOCK
		move	"LoadStats3-NLOLKEY",Location
		pack	KeyLocation,"Key: ",NLOLFLD
		call	NLOLKEY
		setitem	NSTA001DEditNotes3,0,NLOLComment
		setitem	NSTA001DEditNotes4,0,NLOLComment1
	endif
	return

OrderLoadStatDetail Routine FrmPtr
	move	"LoadStatsDet.-STAT2AIM",Location
	pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
	call	STAT2AIM
	if not over
		loop
			until over
.START PATCH 3.48 REPLACED LOGIC
.			call	OrderLoadNSTA001DListViews using FrmPtr
.			move	"NSTA0002ButtonOk-STAT2KG",Location
.			pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
.			call	STAT2KG
			if (HoldPackage = "" | HoldPackage = statpcknum)
				call	OrderLoadNSTA001DListViews using FrmPtr
			endif
			move	"NSTA0002ButtonOk-STAT2KG",Location
			pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
			call	STAT2KG
.END PATCH 3.48 REPLACED LOGIC
		repeat
	endif
	return

OrderLoadNSTA001DListViews Routine FrmPtr
	pack	hold10,STATVARS
	if (FrmPtr = C1)	.LOL Records
		pack	NDATFLD,NLOLLIST
	elseif (FrmPtr = C0)	.LR Records
		pack	NDATFLD,OLNUM
	else			.IS Sequential Search
		pack	NDATFLD,"******"
	endif
	rep	zfill,NDATFLD
	move	C1,NDATPATH
	move	"O.LoadStats3-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	pack	str45,OLSTNAME,STATPCKNUM
	pack	str55,STATLOL,OLSTNAME,STATPCKNUM
.START PATCH 3.49.1 ADDED LOGIC
	call	Trim using OLSTNAME
.END PATCH 3.49.1 ADDED LOGIC
.
.START PATCH 3.75.7 REPLACED LOGIC - REMOVED TEMPORARY PATCH
..START PATCH 3.75.4 REPLACED LOGIC
..	pack	NPKGFLD,NCMPMLR,STATPCKNUM
.	move	"O.LoadStats3-COMPKEY3",Location
.	pack	COMPFLD3,NCMPMLR
.	pack	KeyLocation,"Key: ",COMPFLD3
.	call	COMPKEY3
.	if over
.		clear	COMPNUM
.	endif
.	pack	NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 3.75.4 REPLACED LOGIC
	pack	NPKGFLD,NCMPMLR,STATPCKNUM
.END PATCH 3.75.7 REPLACED LOGIC - REMOVED TEMPORARY PATCH
	rep	zfill,NPKGFLD
	move	C1,NPKGPATH
	move	"O.LoadStats3-NPKGKEY",Location
	pack	KeyLocation,"Key: ",NPKGFLD
	call	NPKGKEY
.START PATCH 3.42 REMOVED LOGIC
..START	PATCH 3.4 ADDED	LOGIC
.	if (NPKGMaster = "1")
.		clear	NPKGPNAME
.	endif
..END PATCH 3.4	ADDED LOGIC
.END PATCH 3.42	REMOVED	LOGIC
	NSTA001DListView.InsertItem giving howmany using str45
	NSTA001DListView3.InsertItem giving howmany3 using str55
	NSTA001DListView2.InsertItem giving howmany2 using NPKGPNAME
.START PATCH 3.48 REPLACED LOGIC
.	NSTA001DListView.SetItemText using howmany,STATLR,1
.	NSTA001DListView3.SetItemText using howmany3,STATLR,1
.	NSTA001DListView2.SetItemText using howmany2,STATPCKNUM,1
.	NSTA001DListView2.SetItemText using howmany2,STATLR,2
.
	move	STATLR,str7	.Default value
	if (FrmPtr = C0)	.LR Records
		if (OSTAT = "z" | OSTAT = "X" | OSTAT = "Q")
			pack	str7,STATLR,"x"
		elseif (OSTAT = "l")
			move	OLRN,NORD5FLD
			if (NORD5FLD <>	"")
				rep	zfill in NORD5FLD
				clear	NORD5STAT
				move	"O.LoadStats3-NORD5KEY",Location
				pack	KeyLocation,"Key: ",NORD5FLD
				call	NORD5KEY		.get LCR info
				if not over
					if (NORD5STAT = "05" | NORD5STAT = "07")
						pack	str7,STATLR,"x"
					endif
				endif
			endif
		endif
	endif
	NSTA001DListView.SetItemText using howmany,str7,1
	NSTA001DListView3.SetItemText using howmany3,str7,1
	NSTA001DListView2.SetItemText using howmany2,STATPCKNUM,1
	NSTA001DListView2.SetItemText using howmany2,str7,2
.END PATCH 3.48 REPLACED LOGIC
	if (STATLOL = "1")
		move	YES,str1
	else
		clear	str1
	endif
	NSTA001DListView.SetItemText using howmany,str1,2
	NSTA001DListView3.SetItemText using howmany3,str1,2
	NSTA001DListView2.SetItemText using howmany2,str1,3
.START PATCH 3.49.1 REPLACED LOGIC
.	NSTA001DListView.SetItemText using howmany,OLSTNAME,3
.	NSTA001DListView3.SetItemText using howmany3,OLSTNAME,3
.	NSTA001DListView2.SetItemText using howmany2,OLSTNAME,4
	if (FrmPtr = C0)	.LR Records
.START PATCH 3.72 REPLACED LOGIC
.		pack	taskname,OLSTNAME,B5,DASH,B5,O2DES
.	else
.		pack	taskname,OLSTNAME,B5,DASH,B5,NLOLSelect
.	endif
..............................................................
		pack	NSEL2FLD,"1",OLRN
	else
		pack	NSEL2FLD,"2",NLOLLOL
	endif
	move	"O.LoadStats3-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		if (FrmPtr = C0)	.LR Records
			move	O2DES,NSEL2NAME
.			if (NCMPRPT = "3")	.NWF
.				move	statseluni,NSEL2QTY
.			else
.				move	OUQTY,NSEL2QTY
.			endif
		else
			move	NLOLSelect,NSEL2NAME
.			if (NCMPRPT = "3")	.NWF
.				move	statseluni,NSEL2QTY
.			else
.				move	NLOLUniverse,NSEL2QTY
.			endif
		endif
	endif
	pack	taskname,OLSTNAME,B5,DASH,B5,NSEL2NAME
.END PATCH 3.72 REPLACED LOGIC
	NSTA001DListView.SetItemText using howmany,taskname,3
	NSTA001DListView3.SetItemText using howmany3,taskname,3
	NSTA001DListView2.SetItemText using howmany2,taskname,4
.END PATCH 3.49.1 REPLACED LOGIC
	NSTA001DListView.SetItemText using howmany,NPKGPNAME,4
	NSTA001DListView3.SetItemText using howmany3,NPKGPNAME,4
	NSTA001DListView.SetItemText using howmany,STATPCKNUM,5
	NSTA001DListView3.SetItemText using howmany3,STATPCKNUM,5
	if (FrmPtr <> C2)
		if (NCMPRPT = "3")
.NWF template uses last	Price Total always
			move	C0,N6
			move	C0,STATPCKM
.Cannot	use N5 as that is what CVTJUL uses!!
.START PATCH 3.75.7 REPLACED LOGIC - REMOVED TEMPORARY PATCH
..START PATCH 3.75.4 REPLACED LOGIC
..			pack	NPRCFLD1,"01X",NCMPMLR
.			move	"O.LoadStats3-COMPKEY3",Location
.			pack	COMPFLD3,NCMPMLR
.			pack	KeyLocation,"Key: ",COMPFLD3
.			call	COMPKEY3
.			if over
.				clear	COMPNUM
.			endif
.			pack	NPRCFLD1,"01X",COMPNUM
..END PATCH 3.75.4 REPLACED LOGIC
			pack	NPRCFLD1,"01X",NCMPMLR
.END PATCH 3.75.7 REPLACED LOGIC - REMOVED TEMPORARY PATCH
			pack	NPRCFLD2,"02X",STATPCKNUM
			move	"O.LoadStats3-NPRCAIM",Location
			pack	KeyLocation,"Key: ",NPRCFLD1,COMMA,NPRCFLD2
			call	NPRCAIM
			loop
				until over
				move	C0,JULDAYS
				call	Trim using NPRCDATE
				if (NPRCDATE <>	"")
					unpack	NPRCDATE,CC,YY,MM,DD
					pack	str10,MM,SLASH,DD,SLASH,CC,YY
					call	cvtjul
				else
.This should never happen as NPRCDATE is part of ISAM
					clear	str10
				endif
				if (JULDAYS > N6)
					move	JULDAYS,N6
					move	NPRCTOTAL,STATPCKM
				endif
				move	"O.LoadStats3-NPRCKG",Location
				call	NPRCKG
			repeat
		else
			move	C0,JULDAYS
			unpack	STATPCKM,str5,str3
			call	Trim using str5
			move	str5,JULDAYS
			call	CVTGREG
			if (YY < "80")
				move	"20",CC
			else
				move	"19",CC
			endif
			move	C1,NPRCPATH
.START PATCH 3.75.7 REPLACED LOGIC - REMOVED TEMPORARY PATCH
..START PATCH 3.75.4 REPLACED LOGIC
..			pack	NPRCFLD,NCMPMLR,STATPCKNUM,CC,YY,MM,DD
.			move	"O.LoadStats3B-COMPKEY3",Location
.			pack	COMPFLD3,NCMPMLR
.			pack	KeyLocation,"Key: ",COMPFLD3
.			call	COMPKEY3
.			if over
.				clear	COMPNUM
.			endif
.			pack	NPRCFLD,COMPNUM,STATPCKNUM,CC,YY,MM,DD
..END PATCH 3.75.4 REPLACED LOGIC
			pack	NPRCFLD,NCMPMLR,STATPCKNUM,CC,YY,MM,DD
.END PATCH 3.75.7 REPLACED LOGIC - REMOVED TEMPORARY PATCH
			move	"O.LoadStats3-NPRCKEY",Location
			pack	KeyLocation,"Key: ",NPRCFLD
			call	NPRCKEY
			if not over
				move	NPRCTOTAL,STATPCKM
			else
				move	C0,STATPCKM
			endif
		endif
	else
		move	C0,STATPCKM
	endif
	unpack	STATPCKM,str5,str3
	call	FormatNumeric using str5,str6
	pack	str10,str6,str3
	NSTA001DListView.SetItemText using howmany,str10,6
	NSTA001DListView3.SetItemText using howmany3,str10,6
	NSTA001DListView2.SetItemText using howmany2,str10,5
	move	STATRECQTY,str9
	call	FormatNumeric using str9,str11
	NSTA001DListView.SetItemText using howmany,str11,8
	NSTA001DListView3.SetItemText using howmany3,str11,8
	NSTA001DListView2.SetItemText using howmany2,str11,7
	move	STATMQTY,str8
	call	FormatNumeric using str8,str10
	NSTA001DListView.SetItemText using howmany,str10,9
	NSTA001DListView3.SetItemText using howmany3,str10,9
	NSTA001DListView2.SetItemText using howmany2,str10,8
.START PATCH 3.45 REPLACED LOGIC
.	if (FrmPtr = C1)
.		if (NLOLRENT = "1")
.			move	"E",str1
.		elseif (NLOLRENT = "2")
.			move	"R",str1
.		elseif (NLOLRENT = "3")
.			move	"S",str1
.		else
.			clear	str1
.		endif
.	elseif (FrmPtr = C0)
.		if (OELCODE = "2" | OELCODE = "3")
.			if (ORENT = "1")  .LCR Rental
.				move	"S",str1
.			else
.				move	"E",str1
.			endif
.		else
.			move	"R",str1
.		endif
.	else
.		clear	str1
.	endif
.	NSTA001DListView.SetItemText using howmany,str1,10
.	NSTA001DListView3.SetItemText using howmany3,str1,10
.	NSTA001DListView2.SetItemText using howmany2,str1,9
	NSTA001DListView.SetItemText using howmany,STATTYPE,10
	NSTA001DListView3.SetItemText using howmany3,STATTYPE,10
	NSTA001DListView2.SetItemText using howmany2,STATTYPE,9
.END PATCH 3.45 REPLACED LOGIC
	unpack	statavgnet,str6
	NSTA001DListView.SetItemText using howmany,str6,12
	NSTA001DListView3.SetItemText using howmany3,str6,12
	NSTA001DListView2.SetItemText using howmany2,str6,11
	unpack	STATRESP2,str6
	NSTA001DListView.SetItemText using howmany,str6,14
	NSTA001DListView3.SetItemText using howmany3,str6,14
	NSTA001DListView2.SetItemText using howmany2,str6,13
	unpack	STATGIFT,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	NSTA001DListView.SetItemText using howmany,str10,16
	NSTA001DListView3.SetItemText using howmany3,str10,16
	NSTA001DListView2.SetItemText using howmany2,str10,15
.
	NSTA001DListView.SetItemText using howmany,hold10,22
	NSTA001DListView3.SetItemText using howmany3,hold10,22
	NSTA001DListView2.SetItemText using howmany2,hold10,21
.START PATCH 3.48 ADDED LOGIC
.START PATCH 3.72 REPLACED LOGIC
.	clear	str9
.	if (NCMPRPT = "3")	.NWF
.		move	statseluni,str9
.	else
.		if (FrmPtr = C0)	.LR Records
.			move	OUQTY,str9
.		else
.			move	NLOLUniverse,str9
.		endif
.	endif
.	call	Trim using str9
..START PATCH 3.66 REPLACED LOGIC
..	NSTA001DListView.SetItemText using howmany,str9,23
.	call	FormatNumeric using str9,str11
.	NSTA001DListView.SetItemText using howmany,str11,23
...........................
	clear	str10
	move	NSEL2QTY,str10
	call	Trim using str10
	call	FormatNumeric using str10,str13
	NSTA001DListView.SetItemText using howmany,str13,23
.END PATCH 3.72 REPLACED LOGIC
.END PATCH 3.66 REPLACED LOGIC
.END PATCH 3.48 ADDED LOGIC
.
	if (STATLOL = "1")
		pack	NCMPFLD,NLOLCNUM
	else
		pack	NCMPFLD,OCAMP
	endif
	move	C1,NCMPPATH
	move	"LoadStats-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
	call	OrderCallCalcFields using C2
.START PATCH 3.45 ADDED LOGIC
.START PATCH 3.48 REPLACED LOGIC
.	NSTA001DListView4.SetItemText using howmany4,STATLR,2
	move	STATLR,str7	.Default value
	if (FrmPtr = C0)	.LR Records
		if (OSTAT = "z" | OSTAT = "X" | OSTAT = "Q")
			pack	str7,STATLR,"x"
		elseif (OSTAT = "l")
			move	OLRN,NORD5FLD
			if (NORD5FLD <>	"")
				rep	zfill in NORD5FLD
				clear	NORD5STAT
				move	"O.LoadStats3-NORD5KEY",Location
				pack	KeyLocation,"Key: ",NORD5FLD
				call	NORD5KEY		.get LCR info
				if not over
					if (NORD5STAT = "05" | NORD5STAT = "07")
						pack	str7,STATLR,"x"
					endif
				endif
			endif
		endif
	endif
	NSTA001DListView4.SetItemText using howmany4,str7,2
.END PATCH 3.48 REPLACED LOGIC
	if (STATLOL = "1")
		move	YES,str1
	else
		clear	str1
	endif
	NSTA001DListView4.SetItemText using howmany4,str1,3
.START PATCH 3.49.1 REPLACED LOGIC
.	NSTA001DListView4.SetItemText using howmany4,OLSTNAME,4
.START PATCH 3.72 REPLACED LOGIC
.	if (FrmPtr = C0)	.LR Records
.		pack	taskname,OLSTNAME,B5,DASH,B5,O2DES
.	else
.		pack	taskname,OLSTNAME,B5,DASH,B5,NLOLSelect
.	endif
	pack	taskname,OLSTNAME,B5,DASH,B5,NSEL2NAME
.END PATCH 3.72 REPLACED LOGIC
	NSTA001DListView4.SetItemText using howmany4,taskname,4
.START PATCH 3.49.1 REPLACED LOGIC
	NSTA001DListView4.SetItemText using howmany4,NPKGPNAME,5
	NSTA001DListView4.SetItemText using howmany4,STATPCKNUM,6
	unpack	STATPCKM,str5,str3
	call	FormatNumeric using str5,str6
	pack	str10,str6,str3
	NSTA001DListView4.SetItemText using howmany4,str10,7
	move	STATRECQTY,str9
	call	FormatNumeric using str9,str11
	NSTA001DListView4.SetItemText using howmany4,str11,8
	move	STATMQTY,str8
	call	FormatNumeric using str8,str10
	NSTA001DListView4.SetItemText using howmany4,str10,9
	NSTA001DListView4.SetItemText using howmany4,STATTYPE,10
	unpack	statavgnet,str6
	NSTA001DListView4.SetItemText using howmany4,str6,12
	unpack	STATRESP2,str6
	NSTA001DListView4.SetItemText using howmany4,str6,14
	unpack	STATGIFT,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	NSTA001DListView4.SetItemText using howmany4,str10,16
	NSTA001DListView4.SetItemText using howmany4,hold10,22
.END PATCH 3.45 ADDED LOGIC
	return

.OrderCallCalcFields Routine FrmPtr1
..Calculated Fields
.	unpack	STATLCPM,str4,str3
.	call	FormatNumeric using str4,str5
.	pack	str10,str5,str3
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BEditListCostM2,0,str10
.	else
.		NSTA001DListView.SetItemText using howmany,str10,11
.		NSTA001DListView3.SetItemText using howmany3,str10,11
.		NSTA001DListView2.SetItemText using howmany2,str10,10
.	endif
..
..	move	C0,N9
..	sub	statrecqty,statmqty,N10
..	move	N10,str10
..	if (N10	< C0)
..		move	DASH,str1
..		call	RemoveChar using str10,DASH
..	else
..		clear	str1
..	endif
..	call	FormatNumeric using str10,str11
..	pack	str12,str1,str11
..	setitem	StatsStatDiff2B,0,str12
..
.	call	OrderCalcStatExTotal using statavgnet,statexbase,CALCSTATEXTOT
.	unpack	CALCSTATEXTOT,str6,str3
.	call	FormatNumeric using str6,str7
.	pack	str10,str7,str3
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatExTotal2B,0,str10
.	endif
..
.	if (NCMPRpt = "3")
.		call	OrderCalcStatNetNames2 using statmqty,statnetrec,statnetreq,statavgnet,statrecqty,CALCSTATNETNAME
.	else
.		call	OrderCalcStatNetNames using statavgnet,statmqty,CALCSTATNETNAME
.	endif
.	move	CALCSTATNETNAME,N9
.	move	N9,str9
.	call	FormatNumeric using str9,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001AStatNetNames2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,13
.		NSTA001DListView3.SetItemText using howmany3,str11,13
.		NSTA001DListView2.SetItemText using howmany2,str11,12
.	endif
..
.	call	OrderCalcStatRentTotal using statrecqty,statrbase,statnetreq,statrun,statselfee,statavgnet,statship,CALCSTATNETNAME,CALCSTATRTOT
.	unpack	CALCSTATRTOT,str6,str3
.	call	FormatNumeric using str6,str7
.	pack	str10,str7,str3
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatRentTotal2B,0,str10
.	endif
..
.	call	OrderCalcReturns using CALCSTATNETNAME,statresp2,statresp
.	move	statresp,str9
.	call	FormatNumeric using str9,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatReturns2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,15
.		NSTA001DListView3.SetItemText using howmany3,str11,15
.		NSTA001DListView2.SetItemText using howmany2,str11,14
.	endif
..
.	call	OrderCalcRevenue using statresp,statgift,statrev
.	move	statrev,str9
.	call	FormatNumeric using str9,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatRevenue2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,17
.		NSTA001DListView3.SetItemText using howmany3,str11,17
.		NSTA001DListView2.SetItemText using howmany2,str11,16
.	endif
..
.	call	OrderCalcProdCost using	CALCSTATNETNAME,statpckm,statresp,statpckprem,C0,CALCSTATPROCOST
.	move	CALCSTATPROCOST,N9
.	move	N9,str9
.	call	FormatNumeric using str9,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatProCost2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,18
.		NSTA001DListView3.SetItemText using howmany3,str11,18
.		NSTA001DListView2.SetItemText using howmany2,str11,17
.	endif
..
.	call	OrderCalcListCost using	CALCSTATNETNAME,statlcpm,CALCSTATLSTCOST
.	move	CALCSTATLSTCOST,N9
.	move	N9,str9
.	call	FormatNumeric using str9,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatListCost2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,19
.		NSTA001DListView3.SetItemText using howmany3,str11,19
.		NSTA001DListView2.SetItemText using howmany2,str11,18
.	endif
..
.	call	OrderCalcTotalCost using CALCSTATPROCOST,CALCSTATLSTCOST,CALCSTATTOTCOST
.	move	CALCSTATTOTCOST,N9
.	move	N9,str9
.	call	FormatNumeric using str9,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatTotCost2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,20
.		NSTA001DListView3.SetItemText using howmany3,str11,20
.		NSTA001DListView2.SetItemText using howmany2,str11,19
.	endif
..
.	call	OrderCalcNetPlusMinus using statrev,CALCSTATTOTCOST,CALCSTATNETP
.	move	CALCSTATNETP,N9
.	move	N9,str9
.	if (CALCSTATNETP < 0)
.		call	RemoveChar using str9,DASH
.		move	DASH,str1
.	else
.		clear	str1
.	endif
.	call	FormatNumeric using str9,str11
.	pack	str12,str1,str11
.	if (FrmPtr1 = C1)
.		setitem	NSTA001BStatNet2B,0,str12
.	else
.		NSTA001DListView.SetItemText using howmany,str12,21
.		NSTA001DListView3.SetItemText using howmany3,str12,21
.		NSTA001DListView2.SetItemText using howmany2,str12,20
.	endif
..
.	call	OrderCalcCostMember using CALCSTATNETNAME,CALCSTATNETP,statresp,CALCSTATCOSTMEM
.	unpack	CALCSTATCOSTMEM,str6,str3
.	if (CALCSTATCOSTMEM < 0)
.		call	RemoveChar using str6,DASH
.		move	DASH,str1
.	else
.		clear	str1
.	endif
.	call	FormatNumeric using str6,str7
.	pack	str11,str1,str7,str3
.	if (FrmPtr1 = C1)
.		setitem	NSTA001AStatCostMember2B,0,str11
.	else
.		NSTA001DListView.SetItemText using howmany,str11,7
.		NSTA001DListView3.SetItemText using howmany3,str11,7
.		NSTA001DListView2.SetItemText using howmany2,str11,6
.	endif
.	return

OrderCallCalcFields Routine FrmPtr1
.Calculated Fields
	unpack	STATLCPM,str4,str3
	call	FormatNumeric using str4,str5
	pack	str10,str5,str3
	if (FrmPtr1 = C1)
		setitem	NSTA001BEditListCostM2,0,str10
	else
		NSTA001DListView.SetItemText using howmany,str10,11
		NSTA001DListView3.SetItemText using howmany3,str10,11
		NSTA001DListView2.SetItemText using howmany2,str10,10
	endif
.
.	move	C0,N9
.	sub	statrecqty,statmqty,N10
.	move	N10,str10
.	if (N10	< C0)
.		move	DASH,str1
.		call	RemoveChar using str10,DASH
.	else
.		clear	str1
.	endif
.	call	FormatNumeric using str10,str11
.	pack	str12,str1,str11
.	setitem	StatsStatDiff2B,0,str12
.
...............................
.START PATCH 3.43 REPLACED LOGIC
.	getitem	StatsStatType2B,0,str1
	getitem	NSTA001AEditType2B,0,str1
.END PATCH 3.43 REPLACED LOGIC
	call	OrderCalcStatSetValues using statrecqty,statmqty,N1,statlcpm,statnetreq,statnetrec,statavgnet,statresp2,statgift,str1,statpckm,NPRCPremium,C0,statexbase,statrbase,statselfee,statrun,statship,NCMPRPT
	call	OrderCalcStatGetValues using CALCSTATNETNAME,statresp,statrev,CALCSTATPROCOST,CALCSTATLSTCOST,CALCSTATTOTCOST,CALCSTATNETP,CALCSTATCOSTMEM,N1,N1,N1,CALCSTATEXTOT,CALCSTATRTOT
.START PATCH 3.45 ADDED LOGIC
	if (FrmPtr1 <> C1)
		move	CALCSTATCOSTMEM,str25
		NSTA001DListView4.InsertItem giving howmany4 using str25
		NSTA001DListView4.SetItemText using howmany4,str10,11
	endif
.END PATCH 3.45 ADDED LOGIC
	unpack	CALCSTATEXTOT,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatExTotal2B,0,str10
	endif
.
	move	CALCSTATNETNAME,N9
	move	N9,str9
	call	FormatNumeric using str9,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001AStatNetNames2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,13
		NSTA001DListView3.SetItemText using howmany3,str11,13
		NSTA001DListView2.SetItemText using howmany2,str11,12
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,13
.END PATCH 3.45 ADDED LOGIC
	endif
.
	unpack	CALCSTATRTOT,str6,str3
	call	FormatNumeric using str6,str7
	pack	str10,str7,str3
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatRentTotal2B,0,str10
	endif
.
	move	statresp,str9
	call	FormatNumeric using str9,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatReturns2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,15
		NSTA001DListView3.SetItemText using howmany3,str11,15
		NSTA001DListView2.SetItemText using howmany2,str11,14
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,15
.END PATCH 3.45 ADDED LOGIC
	endif
.
	move	statrev,str9
	call	FormatNumeric using str9,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatRevenue2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,17
		NSTA001DListView3.SetItemText using howmany3,str11,17
		NSTA001DListView2.SetItemText using howmany2,str11,16
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,17
.END PATCH 3.45 ADDED LOGIC
	endif
.
	move	CALCSTATPROCOST,N9
	move	N9,str9
	call	FormatNumeric using str9,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatProCost2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,18
		NSTA001DListView3.SetItemText using howmany3,str11,18
		NSTA001DListView2.SetItemText using howmany2,str11,17
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,18
.END PATCH 3.45 ADDED LOGIC
	endif
.
	move	CALCSTATLSTCOST,N9
	move	N9,str9
	call	FormatNumeric using str9,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatListCost2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,19
		NSTA001DListView3.SetItemText using howmany3,str11,19
		NSTA001DListView2.SetItemText using howmany2,str11,18
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,19
.END PATCH 3.45 ADDED LOGIC
	endif
.
	move	CALCSTATTOTCOST,N9
	move	N9,str9
	call	FormatNumeric using str9,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatTotCost2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,20
		NSTA001DListView3.SetItemText using howmany3,str11,20
		NSTA001DListView2.SetItemText using howmany2,str11,19
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,20
.END PATCH 3.45 ADDED LOGIC
	endif
.
	move	CALCSTATNETP,N9
	move	N9,str9
	if (CALCSTATNETP < 0)
		call	RemoveChar using str9,DASH
		move	DASH,str1
	else
		clear	str1
	endif
	call	FormatNumeric using str9,str11
	pack	str12,str1,str11
	if (FrmPtr1 = C1)
		setitem	NSTA001BStatNet2B,0,str12
	else
		NSTA001DListView.SetItemText using howmany,str12,21
		NSTA001DListView3.SetItemText using howmany3,str12,21
		NSTA001DListView2.SetItemText using howmany2,str12,20
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str12,21
.END PATCH 3.45 ADDED LOGIC
	endif
.
	unpack	CALCSTATCOSTMEM,str6,str3
	if (CALCSTATCOSTMEM < 0)
		call	RemoveChar using str6,DASH
		move	DASH,str1
	else
		clear	str1
	endif
	call	FormatNumeric using str6,str7
	pack	str11,str1,str7,str3
	if (FrmPtr1 = C1)
		setitem	NSTA001AStatCostMember2B,0,str11
	else
		NSTA001DListView.SetItemText using howmany,str11,7
		NSTA001DListView3.SetItemText using howmany3,str11,7
		NSTA001DListView2.SetItemText using howmany2,str11,6
.START PATCH 3.45 ADDED LOGIC
		NSTA001DListView4.SetItemText using howmany4,str11,1
.END PATCH 3.45 ADDED LOGIC
	endif
	return

OrderCalcStatListCost
.START PATCH 3.43 REPLACED LOGIC
.	getitem	StatsStatType2B,0,str1
	getitem	NSTA001AEditType2B,0,str1
.END PATCH 3.43 REPLACED LOGIC
.STATTYPE is now a value only associated with Returns
.	if (stattype = "R")
	if (str1 = "R")
		move	CALCSTATRTOT,statlcpm
	else
		move	CALCSTATEXTOT,statlcpm
	endif
	return

.START PATCH 3.47 ADDED LOGIC
StatsCalcUsage
	getitem	NSTA001AStatMlr2B,0,STATMLR
	call	Trim using STATMLR
	if (STATMLR <> "")
		getitem	NSTA001AStatListNum2B,0,str6
		call	Trim using str6
		if (str6 <> "")
			call OrderLoadStatsScreenUsage
		endif
	endif
	return
.END PATCH 3.47 ADDED LOGIC

OrderLoadStatsScreen2B Routine FrmPtr
.Fields	that are not stored, but instead tied to the Order/LOL record
	setitem	NSTA001AEditLR2,0,STATLR
	setitem	NSTA001BStatLR2C,0,STATLR
	if (STATLOL = "1")
.START PATCH 3.43 ADDED LOGIC
		setitem	NSTA001BStatRecInfo2,0,"LOL Record"
.END PATCH 3.43 ADDED LOGIC
		move	C1,NLOLPATH
		pack	NLOLFLD,STATLR
		move	"LoadStats2-NLOLKEY",Location
		pack	KeyLocation,"Key: ",NLOLFLD
		call	NLOLKEY
		if over
			call	OrderStatClear2B
			return
		endif
		setitem	NSTA001AStatCampaign2B,0,NLOLCNUM
		pack	NCMPFLD,NLOLCNUM
		move	C1,NCMPPATH
		move	"LoadStats2-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if over
			clear	NCMPCNAME
		endif
		setitem	NSTA001AStatLRCamp2,0,NCMPCNAME
		if (FrmPtr = C1)
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
..			setitem	NSTA001AStatMlr2B,0,NCMPMLR
..			setitem	NSTA001AEditMlr2B,0,NCMPMLR
..			move	C1,NMLRPATH
..			pack	MKEY,NCMPMLR,"000"
..			move	"LoadStats2-NMLRKEY",Location
..			pack	KeyLocation,"Key: ",MKEY
..			call	NMLRKEY
..			if over
..				clear	MCOMP
..			endif
..			setitem	NSTA001AStatMlrName2,0,MCOMP
.............................................................
.			pack	COMPFLD,NCMPMLR
.			move	"LoadStats2-COMPKEY",Location
.			pack	KeyLocation,"Key: ",COMPFLD
.			call	COMPKEY
.			if over
.				clear	COMPCOMP
.			endif
.			setitem	NSTA001AStatMlr2B,0,COMPOLDMLR
.			setitem	NSTA001AEditMlr2B,0,COMPOLDMLR
.			setitem	NSTA001AStatMlrName2,0,COMPCOMP
..END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
			setitem	NSTA001AStatMlr2B,0,NCMPMLR
			setitem	NSTA001AEditMlr2B,0,NCMPMLR
			pack	COMPFLD,NCMPMLR
			move	"LoadStats2-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			if over
				clear	COMPCOMP
			endif
			setitem	NSTA001AStatMlrName2,0,COMPCOMP
.END PATCH 3.75.9 REPLACED LOGIC
			clear	STATNUM
.START PATCH 3.43 ADDED LOGIC
			if (NLOLRENT = "1")
				move	"E",str1
			elseif (NLOLRENT = "2")
				move	"R",str1
			elseif (NLOLRENT = "3")
				move	"S",str1
			else
				clear	str1
			endif
			setitem	NSTA001AEditType2B,0,str1
.END PATCH 3.43 ADDED LOGIC
		endif
		setitem	NSTA001AEditStatNum2,0,STATNUM
		setitem	NSTA001AStatListNum2B,0,NLOLLIST
		setitem	NSTA001AEditListNum2B,0,NLOLLIST
		move	C1,NDATPATH
		pack	NDATFLD,NLOLLIST
		move	"LoadStats2-NDATKEY",Location
		pack	KeyLocation,"Key: ",NDATFLD
		call	NDATKEY
		if over
			clear	OLSTNAME
		endif
		setitem	NSTA001AStatList2,0,OLSTNAME
		call	Trim using NLOLMDATE
		if (NLOLMDATE <> "")
			unpack	NLOLMDATE,str4,MM,DD
			pack	str10,MM,SLASH,DD,SLASH,str4
		else
			clear	str10
		endif
		setitem	NSTA001AStatMailDate2B,0,str10
.START PATCH 3.45 REPLACED LOGIC
.		setitem	StatsStatSelect2B,0,NLOLSELECT
.START PATCH 3.72 REPLACED LOGIC
.		setitem	NSTA001AEditSelect2,0,NLOLSELECT
		pack	NSEL2FLD,"2",NLOLLOL
		move	"LoadStats2_a-NSEL2KEY",Location
		pack	KeyLocation,"Key: ",NSEL2FLD
		call	NSEL2KEY
		if over
			move	NLOLSELECT,NSEL2NAME
			setprop	NSTA001AStatSelect2,fgcolor=red
			if (statseluni > 0)
				move	statseluni,str10
				call	FormatNumeric using str10,str13
			else
				clear	str13
			endif
			clear	str9
			clear	str10
		else
			if (NSEL2NUM = "XXXX")
				setprop	NSTA001AStatSelect2,fgcolor=red
			else
				setprop	NSTA001AStatSelect2,fgcolor=black
			endif
			if (NSEL2QTY > 0)
				move	NSEL2QTY,str10
				call	FormatNumeric using str10,str13
			else
				clear	str13
			endif
			if (NSEL2PRICE = C0)
				clear	str9
			else
				unpack	NSEL2PRICE,str5,str3
				call	FormatNumeric using str5,str6
				pack	str9,str6,str3
			endif
			if (NSEL2SPRICE = C0)
				clear	str10
			else
				unpack	NSEL2SPRICE,str5,str3
				call	FormatNumeric using str5,str6
				pack	str10,str6,str3
			endif
		endif
		setitem	NSTA001AEditSelect2,0,NSEL2NAME
		setitem	NSTA001AEditSelUniverse2,0,str13
		setitem	NSTA001AEditSelPrice2,0,str9
		setitem	NSTA001AEditSelPrice3,0,str10
.END PATCH 3.72 REPLACED LOGIC
.END PATCH 3.45 REPLACED LOGIC
.START PATCH 3.43 REMOVED LOGIC
.		if (NLOLRENT = "1")
.			move	"E",str1
.		elseif (NLOLRENT = "2")
.			move	"R",str1
.		elseif (NLOLRENT = "3")
.			move	"S",str1
.		else
.			clear	str1
.		endif
.		setitem	StatsStatType2B,0,str1
.END PATCH 3.43 REMOVED LOGIC
		call	FormatNumeric using NLOLQTY,str11
.START PATCH 3.45 REPLACED LOGIC
.		setitem	StatsStatLRQty2B,0,str11
		setitem	NSTA001AEditLRQty2,0,str11
.END PATCH 3.45 REPLACED LOGIC
		call	FormatNumeric using NLOLNetQTY,str11
		setitem	NSTA001AStatLRNetQty2B,0,str11
.START PATCH 3.46 ADDED LOGIC
		setitem	NSTA001CEditNotes3,0,NLOLComment
		setitem	NSTA001CEditNotes4,0,NLOLComment1
		setitem	NSTA001CEditNotes5,0,""
.END PATCH 3.46 ADDED LOGIC
.START PATCH 3.72.6 ADDED LOGIC
		move	NLOLLIST,OLNUM
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.		move	NCMPMLR,OMLRNUM
...........................................................
		pack	COMPFLD,NCMPMLR
		move	"LoadStats2B-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		move	COMPOLDMLR,OMLRNUM
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		call	OrderTestXSTAT
		if (taskname = "")
.I can do the following	as taskname=NULL and is	not filled until after second pointer is used
.Check out subroutine and trace	DimPtr/DimPtr1 to see what I mean.
			pack	NORDFLD1,"01R",OMLRNUM
			pack	NORDFLD2,"02R",NLOLLIST
			clear	NORDFLD3
			clear	NORDFLD4
			call	OrderGetHistory	using taskname,taskname,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C8
		endif
		setitem	NSTA001AStatXSTAT2,0,taskname
.END PATCH 3.72.6 ADDED LOGIC
	else
		move	C1,NORDPATH
		pack	NORDFLD,STATLR
		move	"LoadStats2-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		if over
			call	OrderStatClear2B
			return
		endif
.START PATCH 3.43 ADDED LOGIC
		if (OSTAT = "l" | OSTAT = "z")
			setitem	NSTA001BStatRecInfo2,0,"LCR Record"
		elseif (OSTAT = "p" | OSTAT = "x")
			setitem	NSTA001BStatRecInfo2,0,"Pending Record"
		else
			setitem	NSTA001BStatRecInfo2,0,"Live Order"
		endif
.END PATCH 3.43 ADDED LOGIC
		if (FrmPtr = C1)
.START PATCH 3.75.9 REPLACED LOGIC - TEMPORARY PATCH
.			setitem	NSTA001AStatMlr2B,0,OMLRNUM
.			setitem	NSTA001AEditMlr2B,0,OMLRNUM
.			move	C1,NMLRPATH
.			pack	MKEY,OMLRNUM,"000"
.			move	"LoadStats2-NMLRKEY",Location
.			pack	KeyLocation,"Key: ",MKEY
.			call	NMLRKEY
.			if over
.				clear	MCOMP
.			endif
.			setitem	NSTA001AStatMlrName2,0,MCOMP
....................................
			pack	COMPFLD3,OMLRNUM
			move	"LoadStats2-COMPKEY3",Location
			pack	KeyLocation,"Key: ",COMPFLD3
			call	COMPKEY3
			setitem	NSTA001AStatMlr2B,0,COMPNUM
			setitem	NSTA001AEditMlr2B,0,COMPNUM
			if over
				clear	COMPCOMP
			endif
			setitem	NSTA001AStatMlrName2,0,COMPCOMP
.END PATCH 3.75.9 REPLACED LOGIC - TEMPORARY PATCH
			clear	STATNUM
.START PATCH 3.43 ADDED LOGIC
			if (OELCODE = "2" | OELCODE = "3")
				if (ORENT = "1")  .LCR Rental
					move	"S",str1
				else
					call	Trim using OEXQTY
					move	C0,N9
					move	OEXQTY,N9
					if (N9 > 0)
						move	"S",str1
					else
						move	"E",str1
					endif
				endif
			else
				move	"R",str1
			endif
			setitem	NSTA001AEditType2B,0,str1
.END PATCH 3.43 ADDED LOGIC
		endif
		setitem	NSTA001AEditStatNum2,0,STATNUM
		setitem	NSTA001AStatCampaign2B,0,OCAMP
		pack	NCMPFLD,OCAMP
		move	C1,NCMPPATH
		move	"LoadStats2-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NCMPFLD
		call	NCMPKEY
		if over
			clear	NCMPCNAME
		endif
		setitem	NSTA001AStatLRCamp2,0,NCMPCNAME
		setitem	NSTA001AStatListNum2B,0,OLNUM
		setitem	NSTA001AEditListNum2B,0,OLNUM
		move	C1,NDATPATH
		pack	NDATFLD,OLNUM
		move	"LoadStats2-NDATKEY",Location
		pack	KeyLocation,"Key: ",NDATFLD
		call	NDATKEY
		if over
			clear	OLSTNAME
		endif
		setitem	NSTA001AStatList2,0,OLSTNAME
		call	Trim using OMDTEM
		if (OMDTEM <> "")
			pack	str10,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
		else
			clear	str10
		endif
		setitem	NSTA001AStatMailDate2B,0,str10
.START PATCH 3.45 REPLACED LOGIC
.		setitem	StatsStatSelect2B,0,O2DES
.START PATCH 3.72 REPLACED LOGIC
.		setitem	NSTA001AEditSelect2,0,O2DES
		pack	NSEL2FLD,"1",OLRN
		move	"LoadStats_b-NSEL2KEY",Location
		pack	KeyLocation,"Key: ",NSEL2FLD
		call	NSEL2KEY
		if over
			move	O2DES,NSEL2NAME
			setprop	NSTA001AStatSelect2,fgcolor=red
			if (statseluni > 0)
				move	statseluni,str10
				call	FormatNumeric using str10,str13
			else
				clear	str13
			endif
			clear	str9
			clear	str10
		else
			if (NSEL2NUM = "XXXX")
				setprop	NSTA001AStatSelect2,fgcolor=red
			else
				setprop	NSTA001AStatSelect2,fgcolor=black
			endif
			if (NSEL2QTY > 0)
				move	NSEL2QTY,str10
				call	FormatNumeric using str10,str13
			else
				clear	str13
			endif
			if (NSEL2PRICE = C0)
				clear	str9
			else
				unpack	NSEL2PRICE,str5,str3
				call	FormatNumeric using str5,str6
				pack	str9,str6,str3
			endif
			if (NSEL2SPRICE = C0)
				clear	str10
			else
				unpack	NSEL2SPRICE,str5,str3
				call	FormatNumeric using str5,str6
				pack	str10,str6,str3
			endif
		endif
		setitem	NSTA001AEditSelect2,0,NSEL2NAME
		setitem	NSTA001AEditSelUniverse2,0,str13
		setitem	NSTA001AEditSelPrice2,0,str9
		setitem	NSTA001AEditSelPrice3,0,str10
.END PATCH 3.72 REPLACED LOGIC
.END PATCH 3.45 REPLACED LOGIC
.START PATCH 3.43 REMOVED LOGIC
.		if (OELCODE = "2" | OELCODE = "3")
.			if (ORENT = "1")  .LCR Rental
.				move	"S",str1
.			else
.				move	"E",str1
.			endif
.		else
.			move	"R",str1
.		endif
.		setitem	StatsStatType2B,0,str1
.END PATCH 3.43 REMOVED LOGIC
		call	FormatNumeric using OQTY,str11
.START PATCH 3.45 REPLACED LOGIC
.		setitem	StatsStatLRQty2B,0,str11
		setitem	NSTA001AEditLRQty2,0,str11
.END PATCH 3.45 REPLACED LOGIC
		call	FormatNumeric using ONetQTY,str11
		setitem	NSTA001AStatLRNetQty2B,0,str11
.START PATCH 3.46 ADDED LOGIC
.START PATCH 3.72.9 ADDED LOGIC
		call	Trim using STATLR
		if (STATLR <> "")
.END PATCH 3.72.9 ADDED LOGIC
			pack	NSPE2FLD,STATLR
			rep	zfill,NSPE2FLD
			move	C3,NSPE2LOCK
			move	"LoadStats-NSPE2KEY",Location
			pack	KeyLocation,"Key: ",NSPE2FLD
			call	NSPE2KEY
			setitem	NSTA001CEditNotes3,0,DESC003
			setitem	NSTA001CEditNotes4,0,DESC004
.START PATCH 3.72.9 ADDED LOGIC
		else
			setitem	NSTA001CEditNotes3,0,""
			setitem	NSTA001CEditNotes4,0,""
		endif
.END PATCH 3.72.9 ADDED LOGIC
.
.START PATCH 3.72.9 ADDED LOGIC
		if (STATLR <> "")
.END PATCH 3.72.9 ADDED LOGIC
			pack	NSPEFLD,STATLR
			rep	zfill,NSPEFLD
			move	C3,NSPELOCK
			move	"LoadStats-NSPEKEY",Location
			pack	KeyLocation,"Key: ",NSPEFLD
			call	NSPEKEY
			setitem	NSTA001CEditNotes5,0,DESC002
.START PATCH 3.72.9 ADDED LOGIC
		else
			setitem	NSTA001CEditNotes5,0,""
		endif
.END PATCH 3.72.9 ADDED LOGIC
.END PATCH 3.46 ADDED LOGIC
.START PATCH 3.72.6 ADDED LOGIC
		call	OrderTestXSTAT
		if (taskname = "")
.I can do the following	as taskname=NULL and is	not filled until after second pointer is used
.Check out subroutine and trace	DimPtr/DimPtr1 to see what I mean.
			pack	NORDFLD1,"01R",OMLRNUM
			pack	NORDFLD2,"02R",OLNUM
			clear	NORDFLD3
			clear	NORDFLD4
			call	OrderGetHistory	using taskname,taskname,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C8
		endif
		setitem	NSTA001AStatXSTAT2,0,taskname
.END PATCH 3.72.6 ADDED LOGIC
	endif
	return

.START PATCH 3.4 ADDED LOGIC
OrderCalcUsage Routine	DimPtr,DimPtr1,FrmPtr,FrmPtr1
.DimPtr	 = Mailer Number
.DimPtr1 = List	Number
.FrmPtr	 = Number of Names Retrieved (Return Value)
.FrmPtr1 = Number of Orders (Return Value)
.
.Initialize return values
	move	C0,FrmPtr
	move	C0,FrmPtr1
.Locate	Mailer of List being used
	move	C1,NXRFPATH
	clear	NXRFFLD2
	pack	NXRFFLD,DimPtr1
	move	"O.CalcUsageA-NXRFKEY",Location
	pack	KeyLocation,"Key: ",NXRFFLD
	call	NXRFKEY
	if not over
.Load Order Aim	key value as NXRFMLR will be lost with subsequent reads	of NINXRF
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.		pack	NORDFLD1,"01X",NXRFMLR
		pack	COMPFLD,NXRFMLR
		move	"O.CalcUsageA-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		pack	NORDFLD1,"01X",COMPOLDMLR
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
		clear	NORDFLD3
		clear	NORDFLD4
.START PATCH 3.45 ADDED IN CASE OUTSIDE PROGRAMS ARE CALLING - FOUND DURING TESTING
		clock	timestamp,timestamp
.END PATCH 3.45 ADDED IN CASE OUTSIDE PROGRAMS ARE CALLING - FOUND DURING TESTING
		unpack	timestamp,CC,YY,MM,DD
		call	CVTJUL
		move	C0,N5
		move	C0,N6
		move	C0,N7
		move	JULDAYS,N5
		move	"365",N3
		sub	N3,N5		.We want records within	last year
		sub	N3,N5,N6	.Set up	criteria of when to stop searching - 2 years back!
.Locate	all Lists associated with Mailer that is having	usage calculated
		move	C2,NXRFPATH
		clear	NXRFFLD
.START PATCH 3.76 REPLACED LOGIC
..START PATCH 3.75.9 REPLACED LOGIC - TEMPORARY PATCH
..		pack	NXRFFLD2,DimPtr
.		pack	COMPFLD,DimPtr
.		move	"O.CalcUsageA-COMPKEY",Location
.		pack	KeyLocation,"Key: ",COMPFLD
.		call	COMPKEY
.		pack	NXRFFLD2,COMPOLDMLR
..END PATCH 3.75.9 REPLACED LOGIC - TEMPORARY PATCH
		pack	NXRFFLD2,DimPtr
.END PATCH 3.76 REPLACED LOGIC
		move	"O.CalcUsage-NXRFKEY",Location
		pack	KeyLocation,"Key: ",NXRFFLD2
		call	NXRFKEY
		loop
			until over
			until (NXRFFLD2	<> NXRFMLR)
.
			pack	NORDFLD2,"02X",NXRFLIST
			move	"O.CalcUsage-NORDLAST",Location
			pack	KeyLocation,"Key: ",NORDFLD1,NORDFLD2
			call	NORDLAST
			loop
				until over
				move	OODTEC,CC
				move	OODTEY,YY
				move	OODTEM,MM
				move	OODTED,DD
				call	CVTJUL
				move	C0,N7
				move	JULDAYS,N7
				until (N7 < N6)		.Until Order Date is older than	2 years	back from todays date -	a one year cushion
				if (N7 >= N5)		.Pull Records which are	within past year
					if (OSTAT = "0"	| OSTAT	= "B")
						move	C0,N9
						move	OQTY,N9
						add	N9,FrmPtr
						add	C1,FrmPtr1
					endif
				endif
				move	"O.CalcUsage-NORDKGP",Location
				call	NORDKGP
			repeat
			move	"O.CalcUsage-NXRFKS",Location
			call	NXRFKS
		repeat
	endif
	return

OrderHistLoadRecords LRoutine DimPtr,DimPtr1
.Load Actual Returns
	getprop	NSTA001EListView,*ListItems=ListIts
	if (STATFLD <> "")	.Read via ISAM
		move	"O.HistLoad-STATKEY",Location
		pack	KeyLocation,"Key: ",STATFLD
		call	STATKEY
		loop
			until over
			pack	str35,STATMLR,STATSRCE
			until (STATFLD <> str35)
			unpack	statmdate,MM,DD,str4
			pack	str8,str4,MM,DD
			if (str8 >= DimPtr & str8 <= DimPtr1)
				call	OrderLoadNSTA001EListView
			endif
			eventcheck
			until (NewFlag5	= "S")
			move	"O.HistLoad-STATKS",Location
			pack	KeyLocation,"Key: ",STATFLD
			call	STATKS
		repeat
	else
		if (STATFLD5 <>	"")
			move	STATFLD5,str3
			if (str3 = "MAS")	.Master	Package	Designation
				unpack	STATFLD5,str3,str4,str6
				clear	NPKGFLD2
				clear	NPKGFLD3
				clear	NPKGFLD4
				clear	NPKGFLD5
.START PATCH 3.75.4 REPLACED LOGIC
.				pack	NPKGFLD1,"01X",str4
				move	"HistOK-COMPKEY3",Location
				pack	COMPFLD3,str4
				pack	KeyLocation,"Key: ",COMPFLD3
				call	COMPKEY3
				if over
					clear	COMPNUM
				endif
				pack	NPKGFLD1,"01X",COMPNUM
.END PATCH 3.75.4 REPLACED LOGIC
				pack	NPKGFLD5,"05X",str6
				move	C1,NPKGPATH
				move	"HistOK-NPKGAIM",Location
				pack	KeyLocation,"Key: ",NPKGFLD1,COMMA,NPKGFLD5
				call	NPKGAIM
				loop
					until over
					pack	STATFLD5,"02X",NPKGID
					call	OrderLoadHistRecordsAIM	using DimPtr,DimPtr1
					move	"HistOK-NPKGKG",Location
					call	NPKGKG
				repeat
			else
				call	OrderLoadHistRecordsAIM	using DimPtr,DimPtr1
			endif
		else
			call	OrderLoadHistRecordsAIM	using DimPtr,DimPtr1
		endif
	endif
	getprop	ListIts,*Count=result
	move	result,str9
	call	FormatNumeric using str9,str11
	pack	str25,str11," Records Found."
	setitem	NSTA001EStatRecords,0,str25
	if (result > 0)
		setprop	NSTA001EButtonCopy,enabled=1
		setprop	NSTA001EListView,*MultiSelect=OFALSE
		getprop	ListIts,*Item(1)=ListIt
		setprop	NSTA001EListView,*SelectedItem=ListIt
		setprop	NSTA001EListView,*MultiSelect=OTRUE
		call	Click_NSTA001EListView
	else		.Safety	measure
		setprop	NSTA001EButtonCopy,enabled=0
	endif
	return

OrderLoadHistRecordsAIM	Routine	DimPtr,DimPtr1
	move	"O.HistLoad-STATAIM",Location
	pack	KeyLocation,"Key: ",STATFLD4,COMMA,STATFLD5
	call	STATAIM
	loop
		until over
		unpack	statmdate,MM,DD,str4
		pack	str8,str4,MM,DD
		if (str8 >= DimPtr & str8 <= DimPtr1)
			call	OrderLoadNSTA001EListView
		endif
		eventcheck
		until (NewFlag5	= "S")
		move	"O.HistLoad-STATKG",Location
		pack	KeyLocation,"Key: ",STATFLD4,COMMA,STATFLD5
		call	STATKG
	repeat
	return

OrderLoadNSTA001EListView
	pack	hold10,STATVARS
	pack	taskname,MCOMP,STATSRCE
	ListIts.Add giving ListIt using	*Index=1,*Text=taskname
	setprop	ListIt,*SubItems(1)=STATSRCE
	setprop	ListIt,*SubItems(2)=MCOMP
	move	C1,NDATPATH
	pack	NDATFLD,STATLIST
	move	"LoadHistList-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	setprop	ListIt,*SubItems(3)=OLSTNAME
	setprop	ListIt,*SubItems(4)=STATSEL
	setprop	ListIt,*SubItems(5)=STATLR
	move	STATMQTY,str9
	rep	zfill,str9
	setprop	ListIt,*SubItems(10)=str9
	move	STATMQTY,str9
	call	FormatNumeric using str9,str11
	setprop	ListIt,*SubItems(6)=str11
	unpack	STATMDATE,MM,DD,CC,YY
	call	Trim using MM
	if (MM <> "")
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setprop	ListIt,*SubItems(7)=str10
	setprop	ListIt,*SubItems(8)=STATPANEL
	setprop	ListIt,*SubItems(9)=hold10
	call	CVTJUL
	move	JULDAYS,str5
	setprop	ListIt,*SubItems(11)=str5
	return

OrderLoadHistScreen
	setitem	NSTA001EEditCamp,0,statcampn
	move	statrev,str9
	call	FormatNumeric using str9,str11
	setitem	NSTA001EEditGrossRev,0,str11
	if (statImcst <> C0)
		unpack	statImcst,str6,str3
		call	FormatNumeric using str6,str7
		pack	str10,str7,str3
	else
		clear	str10
	endif
	setitem	NSTA001EEditInMail,0,str10
	setitem	NSTA001EEditKey,0,statkycd
	setitem	NSTA001EEditLR,0,statlr
	if (statLVal <>	C0)
		unpack	statLVal,str4,str3
		call	FormatNumeric using str4,str5
		pack	str8,str5,str3
	else
		clear	str8
	endif
	setitem	NSTA001EEditLValue,0,str8
	setitem	NSTA001EEditList,0,Statlist
.START PATCH 3.71.2 REPLACED LOGIC
.	move	C1,STATPATH
	move	C1,NDATPATH
.END PATCH 3.71.2 REPLACED LOGIC
	pack	NDATFLD,STATLIST
	move	"LoadHistScreen-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	setitem	NSTA001EStatListName,0,OLSTNAME
	if (statlcpm <>	C0)
		unpack	statlcpm,str4,str3
		call	FormatNumeric using str4,str5
		pack	str8,str5,str3
	else
		clear	str8
	endif
	setitem	NSTA001EEditLCost,0,str8
	unpack	statmdate,MM,DD,CC,YY
	call	Trim using MM
	if (MM <> "")
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setitem	NSTA001EEditMDate,0,str10
	setitem	NSTA001EEditMailer,0,statmlr
.START PATCH 3.75.9 REPLACED LOGIC
.	pack	MKEY,statmlr,"000"
.	move	C1,NMLRPATH
.	move	"LoadHistScreen-NMLRKEY",Location
.	pack	KeyLocation,"Key: ",MKEY
.	call	NMLRKEY
.	setitem	NSTA001EStatMlrName,0,MCOMP
...............................
	pack	COMPFLD,statmlr
	move	"LoadHistScreen-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	setitem	NSTA001EStatMlrName,0,COMPCOMP
.END PATCH 3.75.9 REPLACED LOGIC
	setitem	NSTA001EEditMlrList,0,statldes
	if (Statpack <>	C0)
		unpack	Statpack,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
	else
		clear	str9
	endif
	setitem	NSTA001EEditPCost,0,str9
	if (Statpckm <>	C0)
		unpack	Statpckm,str5,str3
		call	FormatNumeric using str5,str6
		pack	str9,str6,str3
	else
		clear	str9
	endif
	setitem	NSTA001EEditPCostM,0,str9
	unpack	statpdate,MM,DD,CC,YY
	call	Trim using MM
	if (MM <> "")
		pack	str10,MM,SLASH,DD,SLASH,CC,YY
	else
		clear	str10
	endif
	setitem	NSTA001EEditPDate,0,str10
	setitem	NSTA001EEditPID,0,statpckcde
	setitem	NSTA001EEditPName,0,statpanel
	move	statmqty,str8
	call	FormatNumeric using str8,str10
	setitem	NSTA001EEditQtyMailed,0,str10
	move	statresp,str7
	call	FormatNumeric using str7,str9
	setitem	NSTA001EEditResp,0,str9
	setitem	NSTA001EEditSelect,0,statsel
	setitem	NSTA001EEditSource,0,statsrce
	move	statwkso,str6
	call	FormatNumeric using str6,str7
	setitem	NSTA001EEditWeeks,0,str7
.Calculated Fields
	move	C0,STATRESP2
	move	C0,STATGIFT
	call	OrderCalcStatSetReturnValues using STATRESP,STATMQTY,STATREV
	call	OrderCalcStatGetReturnValues using STATRESP2,STATGIFT
.
	move	STATRESP2,str6
	call	Trim using str6
	setitem	NSTA001EEditRespRate,0,str6
.
	unpack	STATGIFT,str4,str3
	call	FormatNumeric using str4,str5
	pack	str8,str5,str3
	setitem	NSTA001EEditGift,0,str8
.
	call	Trim using statlr
	if (statlr <> "")
		pack	NORDFLD,statlr
		move	C1,NORDPATH
		move	"LoadStat-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		if not over
			call	Trim using OCAMP
			if (OCAMP <> "")
				pack	NCMPFLD,OCAMP
				move	C1,NCMPPATH
				move	"LoadStat-NCMPKEY",Location
				pack	KeyLocation,"Key: ",NCMPFLD
				call	NCMPKEY
				if over
					call	OrderStatsLookAtMailer
				else
					call	Trim using NCMPRPT
					if (NCMPRPT = "")
						call	OrderStatsLookAtMailer
					endif
				endif
			else
				call	OrderStatsLookAtMailer
			endif
		else
			call	OrderStatsLookAtMailer
		endif
	else
		call	OrderStatsLookAtMailer
	endif
	getitem	NSTA001EEditLType,0,str1
	move	"100",statnetrec
	call	OrderCalcStatSetValues using statrecqty,statmqty,N1,statlcpm,statnetreq,statnetrec,statavgnet,statresp2,statgift,str1,statpckm,NPRCPremium,C0,statexbase,statrbase,statselfee,statrun,statship,NCMPRPT
	call	OrderCalcStatGetValues using CALCSTATNETNAME,statresp,statrev,CALCSTATPROCOST,CALCSTATLSTCOST,CALCSTATTOTCOST,CALCSTATNETP,CALCSTATCOSTMEM,N1,N1,N1,CALCSTATEXTOT,CALCSTATRTOT
.
	move	CALCSTATTOTCOST,N9
	move	N9,str9
	call	FormatNumeric using str9,str11
	setitem	NSTA001EEditTotCost,0,str11
.
	unpack	CALCSTATCOSTMEM,str6,str3
	if (CALCSTATCOSTMEM < 0)
		call	RemoveChar using str6,DASH
		move	DASH,str1
	else
		clear	str1
	endif
	call	FormatNumeric using str6,str7
	pack	str11,str1,str7,str3
	setitem	NSTA001EEditMbrCost,0,str11
	return
.END PATCH 3.4 ADDED LOGIC

....DATA VERIFICATION ROUTINES...

....OrderScreens

OrderVerifyData
.SET MOD
.Establish the MOD, Prep for Exchange write in case of Pending -> Live
	getitem	Nord001AComboStatus,0,N2
	if (N2 <= 1)		 .Force	decision before	we continue as MOD needs to be set!!!!
		 alert	 caution,"Please Select	Order Status!!",result
		 call	 OrderSwitchTab	using C1
		 setfocus Nord001AComboStatus
		 move	 "Y",ReturnFlag
		 return
	elseif (N2 = 2)		.LCR Order
.START PATCH 3.71.4 ADDED LOGIC
		if (NewFlag = YES)
			setitem	Nord001AComboPending,0,2
		endif
.END PATCH 3.71.4 ADDED LOGIC
		getitem	Nord001AComboPending,0,N3
		if (N3 <= 1)
			alert	caution,"Please	Select LCR Status!!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AComboPending
			move	"Y",ReturnFlag
			return
		endif
.NORD5STAT must	be set up in order to write to Exchange	file
		sub	C1,N3
		move	N3,str3
		rep	zfill,str3
		bump	str3
		if (str3 = "06")
.Pending LCR's cannot be modified!!
			move	OLRN,NORD5FLD
			move	"V.OSTAT1-NORD5KEY",Location
			pack	KeyLocation,"Key: ",NORD5FLD
			call	NORD5KEY
			if not over
.Need to find real NORD5STAT to	see if already Pending!
				if (NORD5STAT =	"06")
					clear	taskname
					append	"Pending LCR's cannot be modified!!",taskname
					append	carr,taskname
					append	"Select	another	Status to Save changes.",taskname
					reset	taskname
					alert	caution,taskname,result
					call	OrderSwitchTab using C1
					setfocus Nord001AComboPending
					move	"Y",ReturnFlag
					return
				endif
			endif
		endif
		move	str3,NORD5STAT
		if (NORD5STAT =	"04")	.Approved
			getitem	Nord001ACheckRent,0,N1
			if (N1 = 1)
				alert	plain,"Is this Approved	LCR a Rental?",result
				if (result = 1)
					setitem	Nord001ACheckExchange,0,0
					setitem	Nord001ACheckRent,0,0
				elseif (result = 2)	.No = Exchange or Split
					setitem	Nord001ACheckExchange,0,1
					setitem	Nord001ACheckRent,0,0
				elseif (result = 3)	.Cancel
					call	OrderSwitchTab using C1
					setfocus Nord001AComboPending
					move	"Y",ReturnFlag
					return
				endif
			endif
			if (OHIST = "e"	| OHIST	= "z")
				getitem	Nord001bComboCaller,0,N2
				if (N2 <> C1)
					clear	taskname
					append	"This is an LCR with an Exclusive List",taskname
					append	carr,taskname
					append	"which has not been approved by	the Caller!",taskname
					append	carr,taskname
					append	"You cannot turn this LCR into a Live Order!!",taskname
					reset	taskname
					alert	caution,taskname,result
					call	OrderSwitchTab using C1
					setfocus Nord001AComboPending
					move	"Y",ReturnFlag
					return
				endif
			endif
.begin patch 3.79.2
			If	(CompExcl <> OCompId)
				if	(CompExcl = "P")
				pack	taskname,"MLR is a PL Client Make record match ?"
				Alert	Plain,taskname,result
					IF (result = C1)
					move	CompExcl,OCompId
					endif
				Elseif	(OCompid = "P")
				pack	taskname,"Record is marked as PL Client.",Carr,"Mlr is not, Make order match ?"
				Alert	Plain,taskname,result
					IF (result = C1)
					move	CompExcl,OCompId
					endif
				endif	
			endif
.add code for Management side :(			
.end patch 3.79.2
.START PATCH 3.73 ADDED LOGIC
			if (OHIST <> "E")
				move	NDATFLD,NMDLFLD
				call	NMDLKEY
				if not over
					if (mdllcrcd2 = YES)
						clear	taskname
						append	"This LCR uses a List that requires the In-House",taskname
						append	carr,taskname
						append	"Clearance Process.  Make sure to include a Caller,",taskname
						append	carr,taskname
						append	"and then clear through Screen 5.",taskname
						reset	taskname
						alert	caution,taskname,result
						call	OrderSwitchTab using C1
						setfocus Nord001AComboPending
						move	"Y",ReturnFlag
						return
					endif
				endif
			endif
.END PATCH 3.73 ADDED LOGIC
.START PATCH 10-16-2001	BUG FIX	WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
.START PATCH 3.49.1 ADDED LOGIC
			pack	HoldLCRDate,OODTEC,OODTEY,OODTEM,OODTED
.END PATCH 3.49.1 ADDED LOGIC
			unpack	timestamp,OODTEC,OODTEY,OODTEM,OODTED
.Modified to only refresh OrderDate fields.  Routine will be called later.  02/11/2002
.			call	OrderVerifyReturnDate
.END PATCH 10-16-2001 BUG FIX WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
		endif
		if (NewFlag = YES)
			move   C3,mod
		else
			move   C7,mod
		endif
.START PATCH 3.6 ADDED LOGIC
	elseif (N2 = 9 | N2 = 4)		.Cancelled LCR/Pending
.We are trying to dupe the system so that we skip any unnecessary processing
		move   C8,mod
.END PATCH 3.6 ADDED LOGIC
	elseif (N2 = 3)		.Pending Order
SwitchToPending
		getitem	Nord001AComboPending,0,N3
		if (N3 <= 1)
			alert	caution,"Please	Select Pending Status!!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AComboPending
			move	"Y",ReturnFlag
			return
		endif
.NORD4STAT must	be set up in order to write to Exchange	file
		sub	C2,N3
		move	N3,str3
		rep	zfill,str3
		bump	str3
		move	str3,NORD4STAT
		if (NORD4STAT =	"08")	 .Approved - make a live Order!!
			getitem	Nord001ACheckRent,0,N1
			if (N1 = 1)
				alert	plain,"Is this Approved	Pending	a Rental?",result
				if (result = 1)
					setitem	Nord001ACheckExchange,0,0
					setitem	Nord001ACheckRent,0,0
				elseif (result = 2)	.No = Exchange or Split
					setitem	Nord001ACheckExchange,0,1
					setitem	Nord001ACheckRent,0,0
				elseif (result = 3)	.Cancel
					call	OrderSwitchTab using C1
					setfocus Nord001AComboPending
					move	"Y",ReturnFlag
					return
				endif
			endif
			if (OHIST = "e"	| OHIST	= "z")
				clear	taskname
				append	"This is a Pending LCR with an Exclusive List",taskname
				append	carr,taskname
				append	"which has not been approved by	the Caller!",taskname
				append	carr,taskname
				append	"You cannot turn this LCR into a Live Order!!",taskname
				reset	taskname
				alert	caution,taskname,result
				call	OrderSwitchTab using C1
				setfocus Nord001AComboPending
				move	"Y",ReturnFlag
				return
			endif
.START PATCH 3.73 ADDED LOGIC
			if (OHIST <> "E")
				move	NDATFLD,NMDLFLD
				call	NMDLKEY
				if not over
					if (mdllcrcd2 = YES)
						clear	taskname
						append	"This Pending Order uses a List that requires the In-House",taskname
						append	carr,taskname
						append	"Clearance Process.  Make sure to include a Caller,",taskname
						append	carr,taskname
						append	"and then clear through Screen 5.",taskname
						reset	taskname
						alert	caution,taskname,result
						call	OrderSwitchTab using C1
						setfocus Nord001AComboPending
						move	"Y",ReturnFlag
						return
					endif
				endif
			endif
.END PATCH 3.73 ADDED LOGIC
.START PATCH 10-16-2001	BUG FIX	WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
.START PATCH 3.49.1 ADDED LOGIC
			pack	HoldLCRDate,OODTEC,OODTEY,OODTEM,OODTED
.END PATCH 3.49.1 ADDED LOGIC
			unpack	timestamp,OODTEC,OODTEY,OODTEM,OODTED
.Modified to only refresh OrderDate fields.  Routine will be called later.  02/11/2002
.			call	OrderVerifyReturnDate
.END PATCH 10-16-2001 BUG FIX WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
		endif
		if (NewFlag = YES)
			move	C5,mod
		else
			move	C6,mod
		endif
	else			.Live Order, includes -	Billed,Cancelled,Billed/Cancelled
		if (NewFlag = YES)
.START PATCH 3.73 ADDED LOGIC
			move	NDATFLD,NMDLFLD
			call	NMDLKEY
			if not over
				if (mdllcrcd2 = YES)
					clear	taskname
					append	"This Order uses a List that requires the In-House",taskname
					append	carr,taskname
					append	"Clearance Process.  Start creation as an LCR.",taskname
					append	carr,taskname
					append	"Make sure to include a Caller,",taskname
					append	carr,taskname
					append	"and then clear through Screen 5.",taskname
					reset	taskname
					alert	caution,taskname,result
					call	OrderSwitchTab using C1
					setfocus Nord001AComboPending
					move	"Y",ReturnFlag
					return
				endif
			endif
.END PATCH 3.73 ADDED LOGIC
			move	C1,mod
		else
			move	C2,mod
		endif
	endif
.VERIFY	OCAMP
	getitem	Nord001AEditCampaign,0,OCAMP
	call	Trim using OCAMP
	if (NewFlag <> YES)
		if (OCAMP <> HoldCamp)
			clear	taskname
			pack	taskname,"Are you sure you wish",carr,"to change Associated Campaign?"
			alert	plain,taskname,result
			if (result = 2)		.NO
				move	HoldCamp,OCAMP
				setitem	Nord001AEditCampaign,0,HoldCamp
			elseif (result = 3)	.CANCEL
				setitem	Nord001AEditCampaign,0,HoldCamp
				call	OrderSwitchTab using C1
				setfocus Nord001AEditCampaign
				move	"Y",ReturnFlag
				return
.Take care of YES branch at very end of	routine, after all possible returns
			endif
			if (OCAMP <> "")
.Make sure Campaign Number exists
				move	OCAMP,NCMPFLD
				call	ZFILLIT	using NCMPFLD,C1
				move	C1,NCMPPATH
				move	"Ver.D.-NCMPKEY",Location
				pack	KeyLocation,"Key: ",NCMPFLD
				call	NCMPKEY
				if over
					alert	caution,"Campaign Number does not exist!!",result
					setitem	Nord001AEditCampaign,0,OCAMP
					call	OrderSwitchTab using C1
					setfocus Nord001AEditCampaign
					move	"Y",ReturnFlag
					return
				endif
			endif
		endif
	endif

.VERIFY	MAILER
.ZFILL occurs at Nord001AEditMlr_LostFocus & Nord001AEditMlrContact_LostFocus
	getitem	Nord001AEditMlr,0,str4
	call	Trim using str4		.Nord001AEditMlr_LostFocus will allow null entry
	count	N1,str4
	if (N1 <> 4)
		alert	caution,"4 Digit Mailer	Required!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditMlr
		move	"Y",ReturnFlag
		return
	endif
.Removed as per	JD October 2000
.	 getitem Nord001AEditMlrContact,0,str3
.	 call	 Trim using str3	 .Nord001AEditMlrContact_LostFocus will allow null entry
.	 count	 N1,str3
.	 if (N1	<> 3)
.		 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) .LCR does not require contact to be entered, so use default
.			 move	 "000",str3
.		 else
.			 alert	 caution,"3 Digit Mlr Contact Required!",result
.			 call	 OrderSwitchTab	using C1
.			 setfocus Nord001AEditMlrContact
.			 move	 "Y",ReturnFlag
.			 return
.		 endif
.	 endif
	move	"000",str3
.
	pack	MKEY,str4,str3
	move	"O.VerifyData-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	call	NMLRKEY
	if over
		alert	caution,"Mailer	Not Found!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditMlr
		move	"Y",ReturnFlag
		return
	else
.scary biz

		If	(Newflag = Yes)
		Move	CompExcl,OCOmpId
		endif
		reset	badstat
		scan	MSTAT,badstat
		if equal
.Pending/LCR/Modified Orders do	not need to return for valid Credit
			if (mod	= C1 OR	(mod = C6 AND NORD4STAT	= "08")	OR (mod	= 7 AND	NORD5STAT = "04"))	     .Live/New Order only
				if (CanFlag <> YES)
.START PATCH 11FEB2002 FOR JD -	REPLACED LOGIC
.					 alert	 caution,"No Orders Allowed for	this Client!",result
.					 if (SecFlag = NO)
.						 call	 OrderSwitchTab	using C1
.						 setfocus Nord001AEditMlr
.						 move	 "Y",ReturnFlag
.						 return
.					 endif
............
					getitem	Nord001ACheckExchange,0,result
					getitem	Nord001AEditExchangeQty,0,str11
					call	Trim using str11
					call	RemoveChar using str11,COMMA
					if (result = 1 & str11 = "")
					else
.START PATCH 3.4 ADDED LOGIC FOR JD - ASH 19MAR2002
						if (MSTAT <> "N")
.END PATCH 3.4 ADDED LOGIC FOR JD - ASH	19MAR2002
							alert	caution,"No Orders Allowed for this Client!",result
							if (SecFlag = NO)
								call	OrderSwitchTab using C1
								setfocus Nord001AEditMlr
								move	"Y",ReturnFlag
								return
							endif
.START PATCH 3.4 ADDED LOGIC FOR JD - ASH 19MAR2002
						endif
.END PATCH 3.4 ADDED LOGIC FOR JD - ASH	19MAR2002
					endif
.END PATCH 11FEB2002 FOR JD - REPLACED LOGIC
				endif
			endif
		endif
	endif
..Following retained for LCR's per request by SM - 8/17/99
.	 if ((mod = 3 OR (mod =	7 AND NORD5STAT	<> "04")) AND MCOPIES =	YES)
.		 getprop Nord001h,visible=N1
.		 if (N1	= C1)
.			 setprop Nord001h,visible=0
.		 endif
.		 setitem OrderInfoStatText1,0,""
.		 setitem OrderInfoStatText2,0,""
.		 setitem OrderInfoStatText3,0,"This is a Regional Mailer!!!"
.		 setitem OrderInfoStatText4,0,""
.		 setitem OrderInfoStatText5,0,""
.		 setprop Nord001h,title="Mailer	Update Information"
.		 setprop Nord001h,visible=1
.		 setitem timer2,0,10	 .reset	to 1 second
..		  pause	  "1"
.		 setprop Nord001h,visible=0
.	 endif
	if (OCAMP <> "")
		call	OrderMoveTestMailer using NCMPMLR,str4,OLRN,N1
		if (N1 = C0)
			call	OrderSwitchTab using C1
			setfocus Nord001AEditMlr
			move	"Y",ReturnFlag
			return
		endif
	endif
	move	str4,OMLRNUM
	move	str3,OCOBN
.VERIFY	BROKER
.ZFILL & Trim occurs at	Nord001AEditBrk_LostFocus	& Nord001AEditBrkContact_LostFocus
.Check status of OSALES	& OSALES10
.START PATCH - REPLACED	LOGIC
.	 pack	 str2,OSALES10,OSALES
	getitem	Nord001bEditSales,0,str2
.END PATCH - REPLACED LOGIC
	getitem	Nord001AEditBrk,0,str4
	call	Trim using str4		.Nord001AEditBrk_LostFocus will allow null entry
	count	HowMany,str4
.	 if (str2 = "06" AND HowMany = C0 AND mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04")))
	if (str2 = "06"	AND HowMany = C0)
		if (CanFlag <> YES)
			alert	caution,"Broker	Required for List Management!",result
			if (SecFlag = NO)
				call	OrderSwitchTab using C1
				setfocus Nord001AEditBrk
				move	"Y",ReturnFlag
				return
			endif
		endif
	else
		getitem	Nord001AEditBrkContact,0,str3
.START PATCH 3.6 REPLACED LOGIC
.		if (str2 = "06"	OR (mod	<> C3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04"))))  .Not Brokerage LCR
		if (str2 = "06"	OR (mod <> 8 AND mod <> C3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))))  .Not Brokerage LCR
.END PATCH 3.6 REPLACED LOGIC
			call	Trim using str3	.Nord001AEditBrkContact_LostFocus	will allow null	entry
			count	result,str3
			if (HowMany <> C0 AND result = C0)
				alert	caution,"Broker	Contact	Required!",result
				call	OrderSwitchTab using C1
				setfocus Nord001AEditBrkContact
				move	"Y",ReturnFlag
				return
			elseif (HowMany	= C0 AND result	<> C0)
				setitem	Nord001AEditBrkContact,0,""
			elseif (HowMany	<> C0 AND result <> C0)	 .PROCESS FOR VALIDITY -- FINALLY!!!!
				call	ZFILLIT	using str3,C1
				pack	NBRKFLD,str4,str3
				move	"O.VerifyData-NBRKKEY",Location
				pack	KeyLocation,"Key: ",NBRKFLD
				call	NBRKKEY
				if over
					alert	caution,"Broker	Not Found!",result
					call	OrderSwitchTab using C1
					setfocus Nord001AEditBrk
					move	"Y",ReturnFlag
					return
				elseif (str4 = "0000")
					alert	caution,"Invalid Broker!",result
					call	OrderSwitchTab using C1
					setfocus Nord001AEditBrk
					move	"Y",ReturnFlag
				else
					if (mod = C1 OR	(mod = C6 AND NORD4STAT	= "08")	OR (mod = 7 AND	NORD5STAT = "04"))	     .Live/New Order only
						if (CanFlag <> YES)
.START PATCH 3.71.3 REPLACED LOGIC
.							if (BRCREDIT = "B" OR BRCREDIT = "*")
.								alert	caution,"Broker	Credit on Hold!",result
.								if (SecFlag = NO)
.									call	OrderSwitchTab using C1
.									setfocus Nord001AEditBrk
.		       							move	"Y",ReturnFlag
.									return
.								endif
..START PATCH 3.68.6 REPLACED LOGIC
..							elseif (BRCREDIT = "I")
.							elseif (BRCREDIT = "I" | BRINACTIVE = "T")
..END PATCH 3.68.6 REPLACED LOGIC
.								alert	caution,"Broker	is Inactive!",result
.								if (SecFlag = NO)
.									call	OrderSwitchTab using C1
.									setfocus Nord001AEditBrk
.									move	"Y",ReturnFlag
.									return
.								endif
.							endif
							getitem	Nord001ACheckExchange,0,result
							getitem	Nord001AEditExchangeQty,0,str11
							call	Trim using str11
							call	RemoveChar using str11,COMMA
							if (result = 1 & str11 = "")
							else
								if (BRCREDIT = "B" OR BRCREDIT = "*")
									alert	caution,"Broker	Credit on Hold!",result
									if (SecFlag = NO)
										call	OrderSwitchTab using C1
										setfocus Nord001AEditBrk
			       							move	"Y",ReturnFlag
										return
									endif
								elseif (BRCREDIT = "I" | BRINACTIVE = "T")
									alert	caution,"Broker	is Inactive!",result
									if (SecFlag = NO)
										call	OrderSwitchTab using C1
										setfocus Nord001AEditBrk
										move	"Y",ReturnFlag
										return
									endif
								endif
							endif
.END PATCH 3.71.3 REPLACED LOGIC
						endif
					endif
				endif
			endif
		endif
	endif
	move	str4,OBRKNUM
	move	str3,OBRKCNT
.START PATCH 3.72.3 ADDED LOGIC
.LW Robbins Orders have to have valid Campaign Number
	if (OBRKNUM = "0638" & OCAMP = "")
		if (OSTAT = "0" OR mod = C1 OR (mod = C6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))	     .Live/New Order only
			alert	caution,"Valid Campaign Number Required for LW Robbins Orders!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AEditCampaign
			move	"Y",ReturnFlag
			return
		endif
	endif
.END PATCH 3.72.3 ADDED LOGIC
.VERIFY	PO NUM
	getitem	Nord001AEditPO,0,str15
	call	Trim using str15
.START PATCH 3.6 REPLACED LOGIC
.	if (str2 = "06"	AND str15 = "" AND (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))))
	if (str2 = "06"	AND str15 = "" AND (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))))
.END PATCH 3.6 REPLACED LOGIC
		alert	caution,"You must enter	a PO Number!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditPO
		move	"Y",ReturnFlag
		return
	endif
..Check	this field if Nord001AEditPO_ChangeEvent occured OR if this is a New Order
	if (POFlag = YES OR NewFlag = YES)
.		 if (mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	  .Not LCR
.			 if (mod = 5 | mod = 6 | str2 =	"06")  .List Management	Pending	Order
.		 if (str2 = "06")	 .List Management
		if (str2 = "06"	| (mod = 5 | mod = 6))	      .List Management
			if (str15 <> "")	.Do not	test LCRs w/o valid PO entries
.				 clear	 NORDFLD2
.				 clear	 NORDFLD4
.				 clear	 NORDFLD3
.				 clear	 NORDFLD1
.				 pack	 NORDFLD1,"01R",OMLRNUM
.				 pack	 NORDFLD3,"03L",str15
.				 move	 C2,NORDPATH
.				 move	 "O.VerifyData-NORDAIMT",Location
.				 pack	 KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
.				 call	 NORDAIMT
.				 if not	over
..str12	gets loaded with value of OMLRPON in NORDAIMT
.					 call	 Trim using str12
.					 if (str12 = str15)
.						 alert	 caution,"This PO ## Previously	Used!",result
.						 if (SecFlag = NO)
.							 call	 OrderSwitchTab	using C1
.							 setfocus Nord001AEditPO
.							 move	 "Y",ReturnFlag
.							 return
.						 endif
.					 endif
.				 endif
				if (NewFlag = YES)
					clear	str7
				else
					move	OLRN,str7
				endif
				clear	NORDFLD2
				clear	NORDFLD4
				clear	NORDFLD3
				clear	NORDFLD1
				pack	NORDFLD1,"01R",OMLRNUM
				pack	NORDFLD3,"03L",str15
				move	C2,NORDPATH
				move	"O.VerifyData-NORDAIMT",Location
				pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
				call	NORDAIMT
				loop
					until over
.str12 gets loaded with	value of OMLRPON in NORDAIMT
					call	Trim using str12
					if (str12 = str15 AND str6 <> str7)
						alert	caution,"This PO ## Previously Used!",result
						if (SecFlag = NO)
							call	OrderSwitchTab using C1
							setfocus Nord001AEditPO
							move	"Y",ReturnFlag
							return
						endif
					endif
					move	"O.VerifyData-NORDKGAT",Location
					call	NORDKGAT
				repeat
			endif
		endif
	endif
..Check	this field if Nord001AEditPO_ChangeEvent occured OR if this is a New Order
..	  if (POFlag = YES OR NewFlag =	YES)
.
..		  if (mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04")))   .Not	LCR
..			  if (mod = 5 |	mod = 6	| str2 = "06")	.List Management Pending Order
.		 if (str2 = "06")	 .List Management
.			 if (str15 <> "")	 .Do not test LCRs w/o valid PO	entries
.				 clear	 NORDFLD2
.				 clear	 NORDFLD4
.				 clear	 NORDFLD3
.				 clear	 NORDFLD1
.				 pack	 NORDFLD1,"01R",OMLRNUM
.				 pack	 NORDFLD3,"03L",str15
.				 move	 C2,NORDPATH
.				 move	 "O.VerifyData-NORDAIMT",Location
.				 pack	 KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
.				 call	 NORDAIMT
.				 if not	over
.					 move	 "O.VerifyData-NORDKGAT",Location
.					 loop
..str12	gets loaded with value of OMLRPON in NORDAIMT
.					 if (str6 <> OLRN)
.					 call	 Trim using str12
.					 if (str12 = str15)
.						 alert	 caution,"This PO ## Previously	Used!",result
.						 if (SecFlag = NO)
.							 call	 OrderSwitchTab	using C1
.							 setfocus Nord001AEditPO
.							 move	 "Y",ReturnFlag
.							 return
.						 endif
.					 endif
.					 endif
.					 call	 NORDKGAT
.					 until over
.					 repeat
.				 endif
.			 endif
.		 endif
..	  endif
	move	str15,OMLRPON
.VERIFY	SHIP TO/OREUSE
.ZFILL occurs at Nord001AEditRtn_LostFocus
	clear	OREUSE
	getitem	Nord001AEditRtn,0,str6
.This branch was commented out as DE may be fleshing out LCR's prior to	Sales turning them
.into Live Orders and may need to put OREUSE in	place
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))   .LCR
.		 move	 str6,ORTNNUM
.	 else
		count	N1,str6
		if (N1 = 4)		.ORTNNUM
			pack	NRTNFLD,str6
			move	"O.VerifyData-NRTNTST",Location
			pack	KeyLocation,"Key: ",NRTNFLD
			call	NRTNTST
			if over
				alert	caution,"Ship- To ## Not Found!",result
				call	OrderSwitchTab using C1
				setfocus Nord001AEditRtn
				move	"Y",ReturnFlag
				return
			endif
			move	str6,ORTNNUM
		elseif (N1 = 6)		.OREUSE
.Must SAVE original value of NORDFLD
			pack	str7,NORDFLD
			pack	NORDFLD,str6
			rep	zfill,NORDFLD
			move	C1,NORDPATH
			move	"O.VerifyData-NORDTST",Location
			pack	KeyLocation,"Key: ",NORDFLD
			call	NORDTST
			if over
				alert	caution,"Re-Use	LR Not Valid!",result
				if (SecFlag = NO)
					call	OrderSwitchTab using C1
					setfocus Nord001AEditRtn
					move	"Y",ReturnFlag
					return
				endif
			endif
.Must refresh original value of	NORDFLD
			pack	NORDFLD,str7
			move	str6,OREUSE
			move	"0001",ORTNNUM
		elseif (mod = 5	OR (mod = C6 AND NORD4STAT <> "08"))	.Pending does not require ORTNNUM
			clear	ORTNNUM
.This branch was added when top	branch was removed
.START PATCH 3.6 REPLACED LOGIC
.		elseif (mod = 3	OR (mod = 7 AND	NORD5STAT <> "04"))	.LCR does not require ORTNNUM
		elseif (mod = 8 | mod = 3 OR (mod = 7 AND NORD5STAT <> "04"))	.LCR does not require ORTNNUM
.END PATCH 3.6 REPLACED LOGIC
			clear	ORTNNUM
		else
			alert	caution,"Ship-To/Re-Use	Not Valid!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AEditRtn
			move	"Y",ReturnFlag
			return
		endif
.	 endif
.VERIFY	OWNER
	getitem	Nord001AEditOwner,0,str4
	call	Trim using str4
	count	N1,str4
	if (N1 < 4)
		alert	caution,"4 Digit Owner ## Required!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditOwner
		move	"Y",ReturnFlag
		return
	endif
	move	str4,NOWNFLD
	rep	ZFILL,NOWNFLD
.START PATCH 3.51 REPLACED LOGIC
.	move	"O.VerifyData-NOWNTST",Location
.	pack	KeyLocation,"Key: ",NOWNFLD
.	call	NOWNTST
	move	"O.VerifyData-NOWNKEY",Location
	pack	KeyLocation,"Key: ",NOWNFLD
	call	NOWNKEY
.END PATCH 3.51 REPLACED LOGIC
	if over
		alert	caution,"Owner Not Found!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditOwner
		move	"Y",ReturnFlag
		return
	endif
	move	str4,OLON
.START PATCH 3.51 ADDED LOGIC
.Done here for Verify Routines which scan for TDMC records
.START PATCH 3.78.4 REPLACED LOGIC
.	pack	NFULFLD,OWNCTN
.	if (NFULFLD = "    ")
.		pack	NFULFLD,"////"	.FORCE AN OVER
.	else
.		rep	zfill,NFULFLD
.	endif
.	move	C1,NFULPATH
.	move	"O.VerifyData-NFULKEY",Location
.	pack	KeyLocation,NFULFLD
.	call	NFULKEY
.....................
.Start Patch 3.78.8 Comment Out Replace Owner Association of Fulfillment Company to Fulfillment field on Order or List Association
.	pack	COMPFLD6,OWNCTN
.	if (COMPFLD6 = "    ")
.		pack	COMPFLD6,"////"	.FORCE AN OVER
.	else
.		rep	zfill,COMPFLD6
.	endif
.	move	"O.VerifyData-COMPKEY6",Location
.	pack	KeyLocation,COMPFLD6
.	call	COMPKEY6
.	if not over
.		if (COMPSVBFLG <> "T")
.			clear	COMPNUM
.			clear	COMPCOMP
.		endif
.	endif
.	//Refresh the "Fulfillment Variables"
.	move	COMPNUM,NFULNUM
.	move	COMPCOMP,NFULCOMP
.End Patch 3.78.8 Comment Out Replace Owner Association of Fulfillment Company to Fulfillment field on Order	
.END PATCH 3.78.4 REPLACED LOGIC
.END PATCH 3.51 ADDED LOGIC
.Start Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order or List Association
.VERIFY	FULFILLMENT - Not a Required Field
	clear   str6
	getitem	Nord001AEditFulfillment,0,str6
	count	N1,str6
	if (N1 <> C0)	
		call	Trim using str6
		move	str6,COMPFLD
		rep	ZFILL,COMPFLD
		move	"O.VerifySBData-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		If	Not Over
			if (COMPSVBFLG <> "T")
				alert	caution,"Fulfillment Not Found!",result
				call	OrderSwitchTab using C1
				setfocus Nord001AEditFulfillment
				move	"Y",ReturnFlag
				return
			endif			
		else
			alert	caution,"Fulfillment Not Found!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AEditFulfillment
			move	"Y",ReturnFlag
			return
		endif
		move	str6,OFULLFIL	
	else
		move    str6,OFULLFIL
	endif
.End Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order or List Association
.VERIFY LIST, LIST UNIVERSE AND	LIST SEL
	getitem	Nord001AEditList,0,NDATFLD
	call	Trim using NDATFLD		.Nord001AEditList_LostFocus will allow null entry
	count	N1,NDATFLD
	if (N1 <> 6)
		alert	caution,"6 Digit List ## Required!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditList
		move	"Y",ReturnFlag
		return
	endif
	move	C1,NDATPATH
	move	"O.VerifyData-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
	if over
		alert	caution,"List Not Found!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AEditList
		move	"Y",ReturnFlag
		return
	else
		if (mod	= C3 OR	(mod = 7 AND NORD5STAT <> "04"))   .LCR
.Removed as per	Muffin Meeting 9/28/2000 - ASH
.			 reset	 WHITNEY
.			 scan	 OWNNUM,WHITNEY
.			 if equal
.				 alert	 plain,"This is	a Whitney list,	Are you	Sure?",result
.				 if (result <> C1)
.					 call	 OrderSwitchTab	using C1
.					 setfocus Nord001AEditList
.					 move	 "Y",ReturnFlag
.					 return
.				 endif
.			 endif
			reset	PERLOWIN
.START PATCH 3.72 REPLACED LOGIC
.			scan	OWNNUM,PERLOWIN
			move	C0,N4
			move	OWNNUM,N4
			move	N4,str4
			rep	zfill,str4
			scan	str4,PERLOWIN
.END PATCH 3.72 REPLACED LOGIC
			if equal
				alert	plain,"This is a Perlowin list,	Are you	Sure?",result
				if (result <> C1)
					call	OrderSwitchTab using C1
					setfocus Nord001AEditList
					move	"Y",ReturnFlag
					return
				endif
			endif
		endif
....................
.		 if (ELSTCDE <>	"C")  .OUTSIDE LIST
.			 getitem Nord001bComboCaller,0,N2
.			 if (N2	<> C1)
.				 clear	 taskname
.				 append	 "You wish to associate	a Caller",taskname
.				 append	 carr,taskname
.				 append	 "with this Outside List??",taskname
.				 reset	 taskname
.				 alert	 plain,taskname,result
.				 if (result = 3)	 .Cancel - Go back to field
.					 call	 OrderSwitchTab	using C2
.					 setfocus Nord001bComboCaller
.					 move	 "Y",ReturnFlag
.					 return
.				 elseif	(result	= 2)	 .No - Clear field
.					 setitem Nord001bComboCaller,0,1
.				 endif
.			 endif
.		 endif
.		 if (STATUS = "W" OR STATUS = "T")
.			 if (CanFlag <>	YES)
.				 alert	 caution,"This List Has	Been Withdrawn!",result
.				 if (SecFlag = NO)
..Temporary Patch for:	KCET,WETA,WTTW.	 07-27-1999 ASH	per SM
..Removed per SM 11-17-1999
..					  if (NDATFLD =	"001495" OR NDATFLD = "004493" OR NDATFLD = "005578")
...Verify Password for Public Television stations!!!!!
..						  if (HoldFlag < C2)	  .have	they supplied password previously?
..							  clear	  NPASFLD
..							  getitem PasswordStatMssg,0,str55		  .Save	it
..							  setitem PasswordStatMssg,0,"	 Enter Password	to Use this List."
..							  getitem PasswordStatMssg2,0,str25		  .Save	it
...							   setitem PasswordStatMssg2,0,""
..							  setprop PasswordStatMssg1,visible=0
..							  setitem PasswordEdit,0,""
..							  setfocus PasswordEdit
..							  move	  "T",progcode		  .Password = "TIME"
..							  setprop Passwrd,visible=1
..							  if (mod = 6 AND NORD4STAT = "08")
..								  if (NPASFLD <> "TCOSMO")
..									  call	  OrderSwitchTab using C1
..									  setfocus Nord001AEditList
..									  move	  "Y",ReturnFlag
..									  return
..								  endif
..							  else
..								  if (NPASFLD <> "TLIST")
..									  if (PassFlag = NO)
..										  call	  OrderSwitchTab using C1
..										  setfocus Nord001AEditList
..										  move	  "Y",ReturnFlag
..										  return
..									  else
..										  move	  C2,HoldFlag
..									  endif
..								  endif
..							  endif
..						   endif
..					  else
...LCR can enter Withdrawn List!
.					 if ((mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04"))) & (mod	<> 5 AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08"))))	     .Pending/LCR allows entry of Withdrawn Lists
.						 call	 OrderSwitchTab	using C1
.						 setfocus Nord001AEditList
.						 move	 "Y",ReturnFlag
.						 return
.					 endif
..					  endif
.				 endif
.			 endif
.		 elseif	(ELSTCDE = "C")	 .EXCLUSIVE LIST
.			 if (mod = 3 OR	mod = 7	OR mod = 5 OR mod = 6)
.				 if ((mod = 3 OR mod = 7) AND NORD5STAT	<> "04")
.					 alert	 plain,"This List is EXCLUSIVE,	OK?",result
.					 if (result <> 1)
.						 call	 OrderSwitchTab	using C1
.						 setfocus Nord001AEditList
.						 move	 "Y",ReturnFlag
.						 return
.					 endif
.				 endif
.				 move	 NDATFLD,NMDLFLD
.				 call	 NMDLKEY
.				 if not	over
.					 if ((mod = 3 OR mod = 7) AND NORD5STAT	<> "04")
.						 if (MDLLCRCD =	"N")
.							 move	 "G",progcode
.							 setitem PasswordStatMssg1,0,"		  To LCR This List"
.							 setprop PasswordStatMssg1,visible=1
.							 setitem PasswordEdit,0,""
.							 setfocus PasswordEdit
.							 setprop Passwrd,visible=1
.							 if (PassFlag =	NO)
.								 call	 OrderSwitchTab	using C1
.								 setfocus Nord001AEditList
.								 move	 "Y",ReturnFlag
.								 return
.							 endif
.						 endif
.					 endif
.					 if (MDLCALL <>	"" AND MDLCALL <> "  ")
.						 getitem Nord001bComboCaller,0,N2
.						 if (N2	<= C1)
.							 clear	 taskname
.							 append	 "This List requires a Caller!",taskname
.							 append	 carr,taskname
.							 append	 "Suggested Caller:  ",taskname
.							 append	 MDLCALL,taskname
.							 reset	 taskname
.							 alert	 caution,taskname,result
.							 call	 OrderSwitchTab	using C2
.							 setfocus Nord001bComboCaller
.							 move	 "Y",ReturnFlag
.							 return
.						 endif
.					 endif
.				 endif
.			 endif
.
.....................
		move	NDATFLD,NMDLFLD
		call	NMDLKEY
.		 if (mod = 3 OR	mod = 7	OR mod = 5 OR mod = 6)
			if (MDLCALL <> "" AND MDLCALL <> "  ")
				getitem	Nord001bComboCaller,0,N2
				if (N2 <= C1)
					clear	taskname
					append	"This List requires a Caller!",taskname
					append	carr,taskname
					append	"Suggested Caller:  ",taskname
					move	C2,NUSEPATH
					move	MDLCALL,NUSEFLD2
					call	NUSEKEY
					if not over
						append	NUSEUSER,taskname
					else
						append	MDLCALL,taskname
					endif
					reset	taskname
					alert	caution,taskname,result
					call	OrderSwitchTab using C2
					setfocus Nord001bComboCaller
					move	"Y",ReturnFlag
					return
				endif
			elseif (ELSTCDE	<> "C")	 .OUTSIDE LIST
				getitem	Nord001bComboCaller,0,N2
				if (N2 <> C1)
					clear	taskname
					append	"You wish to associate a Caller",taskname
					append	carr,taskname
					append	"with this Outside List??",taskname
					reset	taskname
					alert	plain,taskname,result
					if (result = 3)		.Cancel	- Go back to field
						call	OrderSwitchTab using C2
						setfocus Nord001bComboCaller
						move	"Y",ReturnFlag
						return
					elseif (result = 2)	.No - Clear field
						setitem	Nord001bComboCaller,0,1
					endif
				endif
			endif
.		 endif
		if (STATUS = "W" OR STATUS = "T")
			if (CanFlag <> YES AND OSTAT <>	"X" AND	OSTAT <> "Q")
				alert	caution,"This List Has Been Withdrawn!",result
				if (SecFlag = NO)
.Temporary Patch for:  KCET,WETA,WTTW.	07-27-1999 ASH per SM
.Removed per SM	11-17-1999
.					 if (NDATFLD = "001495"	OR NDATFLD = "004493" OR NDATFLD = "005578")
..Verify Password for Public Television	stations!!!!!
.						 if (HoldFlag <	C2)	 .have they supplied password previously?
.							 clear	 NPASFLD
.							 getitem PasswordStatMssg,0,str55		 .Save it
.							 setitem PasswordStatMssg,0,"	Enter Password to Use this List."
.							 getitem PasswordStatMssg2,0,str25		 .Save it
.							 setitem PasswordStatMssg2,0,""
.							 setprop PasswordStatMssg1,visible=0
.							 setitem PasswordEdit,0,""
.							 setfocus PasswordEdit
.							 move	 "T",progcode		 .Password = "TIME"
.							 setprop Passwrd,visible=1
.							 if (mod = 6 AND NORD4STAT = "08")
.								 if (NPASFLD <>	"TCOSMO")
.									 call	 OrderSwitchTab	using C1
.									 setfocus Nord001AEditList
.									 move	 "Y",ReturnFlag
.									 return
.								 endif
.							 else
.								 if (NPASFLD <>	"TLIST")
.									 if (PassFlag =	NO)
.										 call	 OrderSwitchTab	using C1
.										 setfocus Nord001AEditList
.										 move	 "Y",ReturnFlag
.										 return
.									 else
.										 move	 C2,HoldFlag
.									 endif
.								 endif
.							 endif
.						  endif
.					 else
..LCR can enter	Withdrawn List!
.START PATCH 3.6 REPLACED LOGIC
.					if ((mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	& (mod <> 5 AND	(mod <>	6 OR (mod = 6 AND NORD4STAT = "08"))))	    .Pending/LCR allows	entry of Withdrawn Lists
					if ((mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) & (mod <> 5 AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08"))))	    .Pending/LCR allows	entry of Withdrawn Lists
.END PATCH 3.6 REPLACED LOGIC
						call	OrderSwitchTab using C1
						setfocus Nord001AEditList
						move	"Y",ReturnFlag
						return
					endif
.					 endif
				endif
			endif
		elseif (ELSTCDE	= "C")	.EXCLUSIVE LIST
			if (mod = 3 OR mod = 7 OR mod =	5 OR mod = 6)
.				 if ((mod = 3 OR mod = 7) AND NORD5STAT	<> "04")
.					 alert	 plain,"This List is EXCLUSIVE,	OK?",result
.					 if (result <> 1)
.						 call	 OrderSwitchTab	using C1
.						 setfocus Nord001AEditList
.						 move	 "Y",ReturnFlag
.						 return
.					 endif
.				 endif
..................
.				 if ((mod = 3 OR mod = 7) AND NORD5STAT	<> "04")
.					 if (MDLLCRCD =	"N")
.						 move	 "G",progcode
.						 setitem PasswordStatMssg1,0,"		  To LCR This List"
.						 setprop PasswordStatMssg1,visible=1
.						 setitem PasswordEdit,0,""
.						 setfocus PasswordEdit
.						 setprop Passwrd,visible=1
.						 if (PassFlag =	NO)
.							 call	 OrderSwitchTab	using C1
.							 setfocus Nord001AEditList
.							 move	 "Y",ReturnFlag
.							 return
.						 endif
.					 endif
.				 endif
			endif
.START PATCH 3.49.1 ADDED LOGIC
			if ((mod = 7 AND NORD5STAT = "04") | (mod = 6 AND NORD4STAT = "08"))
.Check REVDATE if Approving an LCR/Pending Order
				unpack	REVDATE,MM,str1,DD,str1,CC,YY
				move	C0,JULDAYS
				call	CVTJUL
				move	C0,N5
				move	JULDAYS,N5
				unpack	HoldLCRDate,CC,YY,MM,DD
				move	C0,JULDAYS
				call	CVTJUL
				if (N5 > JULDAYS)
					alert	caution,"This List has been Updated since your LCR/Pending Order was created!",result
				endif
			endif
.END PATCH 3.49.1 ADDED LOGIC
		else
.Check for List	Update Age
			unpack	date,mm,str1,dd,str1,yy
			call	CVTJUL
			move	JULDAYS,MailDate
			move	mm,N2
			compare	C4,N2
			if less
				unpack	REVDATE,str8,str2
				if (str2 <> yy)
.
.					 getprop OrderInfo,visible=N1
.					 if (N1	= C1)
.						 call	 OrderInfoClose
.					 endif
.					 setitem OrderInfoStatText1,0,""
.					 setitem OrderInfoStatText2,0,""
.					 setitem OrderInfoStatText3,0,"List Update NOT within 1st Quarter!"
.					 setitem OrderInfoStatText4,0,""
.					 setitem OrderInfoStatText5,0,""
.					 setprop OrderInfo,title="List Update Information"
.					 setprop OrderInfo,visible=1
.					 pause	 "1"
.					 call	 OrderInfoClose
					pack	str45,"List Update Information"
					pack	str55,"List Update NOT within 1st Quarter!"
					clear	str1
					call	OrderDisplayMessage using Nord0001,str45,str1,str1,str55,str1,str1,C0,C0,C0,C0
					pause	"1"
					call	OrderInfoClose
.
				endif
.			 else
.				 unpack	 REVDATE,mm,STR1,dd,STR1,str2,yy
.				 call	 CVTJUL
.				 move	 JULDAYS,JDate
.				 sub	 JDate,MailDate
.				 if (MailDate >	90)
.					 getprop Nord001h,visible=N1
.					 if (N1	= C1)
.						 setprop Nord001h,visible=0
.					 endif
.					 setitem OrderInfoStatText1,0,""
.					 setitem OrderInfoStatText2,0,""
.					 setitem OrderInfoStatText3,0,"List Update is 90+ Days Old!"
.					 setitem OrderInfoStatText4,0,""
.					 setitem OrderInfoStatText5,0,""
.					 setprop Nord001h,title="List Update Information"
.					 setprop Nord001h,visible=1
.					 pause	 "2"
.					 setprop Nord001h,visible=0
.				 endif
			endif
		endif
.Removed 02/15/2000 - ASH
..Check	to see if record is marked as Rent and if Data Card says "EXCHANGE ONLY"
.		 move	 C0,result	 .Used as a Flag
.		 if ((mod = 3 OR (mod =	7 AND NORD5STAT	<> "04")) | (mod = 5 OR	(mod = 6 AND NORD4STAT <> "08")))
.			 getitem Nord001ACheckRent,0,result
.		 else
.			 getitem Nord001ACheckExchange,0,N8
.			 if (N8	<> 1)
.				 move	 C1,result
.			 endif
.		 endif
.		 if (result = 1)     .Marked as	Rental
.			 rep	 lowup,TextData	 .Just in case text entered in lower case
.			 scan	 "EXCHANGE ONLY",TextData
.			 if equal
.				 clear	 taskname
.				 append	 "This List is marked as 'EXCHANGE ONLY'.",taskname
.				 append	 carr,taskname
.				 append	 "Do you really	want this on Rental?",taskname
.				 reset	 taskname
.				 alert	 plain,taskname,result
.				 if (result = 1)	 .Yes
..Do nothing, allow the	processing to happen
.				 elseif	(result	= 2)	 .No
.					 if ((mod = 3 OR (mod =	7 AND NORD5STAT	<> "04")) | (mod = 5 OR	(mod = 6 AND NORD4STAT <> "08")))
.						 setitem Nord001ACheckRent,0,0
.					 else
.						 call	 OrderSwitchTab	using C1
.						 setfocus Nord001ACheckExchange
.						 move	 "Y",ReturnFlag
.						 return
..						  setitem Nord001ACheckExchange,0,1
.					 endif
.				 else			 .Cancel - return to List
.					 call	 OrderSwitchTab	using C1
.					 setfocus Nord001AEditList
.					 move	 "Y",ReturnFlag
.					 return
.				 endif
.			 endif
.		 endif
	endif
	move	NDATFLD,OLNUM
	move	OLSTNAME,O1DES
.Safety	Check to see if	Owner has changed between LCR/Live Order stage.
.START PATCH 3.72 REPLACED LOGIC
.	if ((mod = 7 AND NORD5STAT = "04") AND OLON <> OWNNUM)
	move	C0,N4
	move	OWNNUM,N4
	move	N4,str4
	rep	zfill,str4
	if ((mod = 7 AND NORD5STAT = "04") AND OLON <> str4)
.END PATCH 3.72 REPLACED LOGIC
		clear	taskname
		append	"The Owner Number on DataCard ## ",taskname
		append	OLNUM,taskname
		append	carr,taskname
		append	"is listed as Owner ## ",taskname
.START PATCH 3.72 REPLACED LOGIC
.		append	OWNNUM,taskname
		append	str4,taskname
.END PATCH 3.72 REPLACED LOGIC
		append	PERIOD,taskname
		append	carr,taskname
		append	"Do you	want this record to use	that Number?",taskname
		reset	taskname
		alert	plain,taskname,result
		if (result = 1)		.YES
.START PATCH 3.72 REPLACED LOGIC
.			move	OWNNUM,OLON
			move	str4,OLON
.END PATCH 3.72 REPLACED LOGIC
			setitem	Nord001AEditOwner,0,OLON
		elseif (result = 2)	.NO
.NADA -	let it be
		else			.CANCEL
			call	OrderSwitchTab using C1
			setfocus Nord001AEditOwner
			move	"Y",ReturnFlag
			return
		endif
	endif
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to List/Fulfillment Association 
.Checking to see if fulfillment company has changed
.Patch 3.78.9 Code Modified to not have this message pop up if there is not a fulfillment associated with the datacard.
.	move	C0,N6
.	move	DATFUL,N6
.	move	N6,str6
.	rep	zfill,str6
.Patch 3.79 Bug Fix - Datfull must be trimmed to do a real comparison
	call trim using datful
.Patch 3.79 Bug Fix	
.	if ((mod = 7 AND NORD5STAT = "04") AND OFULLFIL <> str6)
	if ((mod = 7 AND NORD5STAT = "04") AND OFULLFIL <> DATFUL)
	
.Patch 3.78.9 	
		clear	taskname
		append	"The Fulfillment Number on DataCard ## ",taskname
		append	OLNUM,taskname
		append	carr,taskname

.Start Patch 3.79 Bug Fix				
		if (DATFUL <> "")
			append	"is listed as Fulfillment ## ",taskname	
			append  DATFUL,taskname
		else
			append	"is listed as Fulfillment ## ",taskname		
			append	"Nothing",taskname		
		endif
.End Patch 3.79 Bug Fix	
		append	PERIOD,taskname
		append	carr,taskname
		append	"Do you	want this record to use	that Number?",taskname
		reset	taskname
		alert	plain,taskname,result
		if (result = 1)		.YES
.Start Patch 3.79 Bug Fix						
			move	DATFUL,OFULLFIL
.End Patch 3.79 Bug Fix							
			setitem	Nord001AEditFulfillment,0,OFULLFIL
		elseif (result = 2)	.NO
.NADA -	let it be
		else			.CANCEL
			call	OrderSwitchTab using C1
			setfocus Nord001AEditFulfillment
			move	"Y",ReturnFlag
			return
		endif
	endif
.End Patch 3.78.8 Replace Owner/Fulfillment Association to List/Fulfillment Association 
.
.START PATCH 3.72 REPLACED LOGIC - MOVED LOGIC TO ORDERSAVE BUTTON
.	getitem	Nord001AEditListUniverse,0,str11
.	call	RemoveChar using str11,COMMA
.	call	Trim using str11
.	move	str11,OUQTY
.	call	ZFILLIT	using OUQTY
	clear	OUQTY
.END PATCH 3.72 REPLACED LOGIC - MOVED LOGIC TO ORDERSAVE BUTTON
.

.START PATCH 3.72 REPLACED LOGIC - MOVED LOGIC TO ORDERSAVE BUTTON
.	getitem	Nord001AEditListSel,0,O2DES
.	call	Trim using O2DES
.	if (O2DES = "")
.		move	"				    ",O2DES
.	endif
......................................................................
.Note that I am NOT currently testing for the Exchange Only/Rent Only
.status of either Lists or Selects.  I am allowing users to choose
.a List/Select that is Exchange Only, and apply a price.  This would
.be necessary for Mailer/List combinations where the List/Select
.is Exchange Only, the Mailer does not have a list, and the List
.Owner will allow a rent for a small surcharge.
.
.Nor am I comparing the Exchange Only/Rent Only status of Lists/Selects
.with the Exchange status of the Order.
......................................................................
	clear	O2DES
	if (mod = 1 OR (mod = 6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))	     .Live/New Order only
.Test for Select Eligibility
		getitem	Nord001AEditListSel,0,taskname
		if (taskname <> "")
			packkey	NSELFLD1,"01X",LSTNUM
			clear	NSELFLD2
			packkey	NSELFLD3,"03X",taskname
			rep	lowup,taskname
			move	"Ver.Data-NSELAIM",Location
			pack	KeyLocation,"Key: ",NSELFLD1,COMMA,NSELFLD3
			call	NSELAIM
			loop
				until over
.The following is an extreme double-check!!
				call	Trim using NSELSNAME
				rep	lowup,NSELSNAME
				if (taskname = NSELSNAME)
					if (NSELINACTIVE = "1")
						alert	caution,"This Select is Inactive!",result
						call	OrderSwitchTab using C1
						setfocus Nord001AEditListSel
						move	"Y",ReturnFlag
						return
					elseif (NSELSTATUS = "1")	.Special
						pack	taskname,"This Select is tagged as 'Special',",newline,"Do you wish to continue?"
						alert	plain,taskname,result
						if (result <> 1)
							call	OrderSwitchTab using C1
							setfocus Nord001AEditListSel
							move	"Y",ReturnFlag
							return
						endif
					elseif (NSELSTATUS = "2")	.Office use Only
						pack	taskname,"This Select is tagged as 'Office Use Only',",newline,"Do you wish to continue?"
						alert	plain,taskname,result
						if (result <> 1)
							call	OrderSwitchTab using C1
							setfocus Nord001AEditListSel
							move	"Y",ReturnFlag
							return
						endif
					endif
					getitem	Nord001AEditOrderQty,0,str11
					call	RemoveChar using str11,COMMA
					call	Trim using str11
					move	C0,howmany
					move	str11,howmany
					move	C0,N10
					move	NSELQTY,N10
					if (howmany > N10)
						pack	taskname,"The Order Qty is Greater than the Select Qty,",newline,"Do you wish to continue?"
						alert	plain,taskname,result
						if (result <> 1)
							call	OrderSwitchTab using C1
							setfocus Nord001AEditListSel
							move	"Y",ReturnFlag
							return
						endif
					elseif (N10 > howmany)
						getitem	Nord001ACheckEntire,0,N1
						if (N1 = 1)
							pack	taskname,"The Order Qty is Less than the Select Qty,",newline,"and the Entire checked box is flagged,",newline,"Do you wish to continue?"
							alert	plain,taskname,result
							if (result <> 1)
								call	OrderSwitchTab using C1
								setfocus Nord001AEditOrderQty
								move	"Y",ReturnFlag
								return
							endif
						endif
					endif
					break
				endif
				move	"Ver.Data-NSELKG",Location
				call	NSELKG
			repeat
		endif
	endif
	if (mod = 1 OR mod = 2 OR (mod = 6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))	.Live Order Only
.Test to see if supplying Base Price for a straight Exchange Order
		getitem	Nord001ACheckExchange,0,result
		getitem	Nord001AEditExchangeQty,0,str11
		call	Trim using str11
		call	RemoveChar using str11,COMMA
		if (result = 1 & str11 = "")
			getitem	Nord001AEditListSelPrice,0,str9
			call	Trim using str9
			call	RemoveChar using str9,COMMA
			move	C0,N52
			move	str9,N52
			if (N52 <> 0)
				pack	taskname,"This Exchange Order has an associated Base Price!",newline,"Do you wish to keep it?"
				alert	plain,taskname,result
				if (result <> 1)
					call	OrderSwitchTab using C1
					setfocus Nord001AEditListSelPrice
					move	"Y",ReturnFlag
					return
				endif
			endif
		endif
	endif
.END PATCH 3.72 REPLACED LOGIC - MOVED LOGIC TO ORDERSAVE BUTTON
.START PATCH 3.6 REPLACED LOGIC
.	if (ListFlag = YES AND mod <> 5	AND mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
.START PATCH 3.76.7 REPLACED LOGIC
.	if (ListFlag = YES AND mod <> 5	AND mod	<> 3 AND mod <> 8 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
	getitem	Nord001AEditListSel,0,taskname
	call	Trim using taskname
	call	Trim using SelectHold
	if (taskname <> SelectHold)
		if (mod <> 5	AND mod	<> 3 AND mod <> 8 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
.END PATCH 3.76.7 REPLACED LOGIC
.END PATCH 3.6 REPLACED LOGIC
			if (mod	<> C6)
				move	yes,lolsw		       *flag for tdmc lol file.
				move	"L",loltype
			elseif (NORD4STAT = "08")
				move	yes,lolsw		       *flag for tdmc lol file.
				move	"L",loltype
			endif
		endif
.START PATCH 3.76.7 ADDED LOGIC
	endif
.END PATCH 3.76.7 ADDED LOGIC
.START PATCH 3.78 ADDED LOGIC
..VERIFY	RETURNDATE/MAILDATE/ORDERDATE
.	if (SecFlag = YES)
.		getitem	Nord001AEditOrderDate,0,str10
.		call	TRIM using str10
.		count	N2,str10
.		if (N2 = 0)
.			clear	OODTEM
.			clear	OODTED
.			clear	OODTEC
.			clear	OODTEY
.      		else
.			if (N2 = 10)
.				unpack	str10,OODTEM,str1,OODTED,str1,OODTEC,OODTEY
.			elseif (N2 = 8)
.				unpack	str10,OODTEM,OODTED,OODTEC,OODTEY
.			elseif (N2 <> 0)
.				alert	caution,"Order Date Must be in MMDDCCYY	Format",result
.				goto OrderReturnOrderDate
.			endif
.			move	OODTEM,N2
.			if (N2 > "12")
.				alert	caution,"Invalid Month!",result
.				goto OrderReturnOrderDate
.			else
.				move	OODTED,N2
.				if (N2 > "31")
.					alert	caution,"Invalid Day!",result
.					goto OrderReturnOrderDate
.				else
.					move	OODTEC,N2
.					if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
.						alert	caution,"Invalid Year!",result
.						goto OrderReturnOrderDate
.					elseif (N2 = "19")
.						move	OODTEY,N2
.						if (N2 < "80")
.							alert	caution,"Invalid Year!",result
.							goto OrderReturnOrderDate
.						endif
.					endif
.				endif
.			endif
.		endif
.		call	TRIM using OODTEM
.		count	N2,OODTEM
.		if (N2 <> 0 AND	OODTEM <> "00")
.			pack	newdate1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
.		else
.			clear	newdate1
.		endif
.		setitem	Nord001AEditOrderDate,0,newdate1
.	endif
..
.	getitem	Nord001AEditRtnDate,0,str10
.	call	TRIM using str10
.	count	N2,str10
.	if (N2 = 0)	.Should	apply if OREUSE
.		clear	ORTNDTEM
.		clear	ORTNDTED
.		clear	ORTNDTEC
.		clear	ORTNDTEY
..START PATCH   3.77.9	ADDED LOGIC
.		clear	newdate1
.		setitem	Nord001AEditRtnDate,0,newdate1  // BLANK DATE
..END PATCH   3.77.9	ADDED LOGIC
.	else  // return date not blank
.
.
..START PATCH   3.77.9	REPLACED LOGIC
.		call	Trim using str10  // str10 is return date
.		call	RemoveChar, str10, SLASH
.		count	result,str10
.		if (result = 0)	// return date may be left blank
.			//do nothing
.		elseif (result <> 8)
.			alert note, "Return date must be in this format: DDMMYYYY", result
.			goto OrderReturnRtnDate
.		else
.			unpack	str10,ORTNDTEM,ORTNDTED,ORTNDTEC,ORTNDTEY
.			pack str10, ORTNDTEM, "/", ORTNDTED, "/", ORTNDTEC, ORTNDTEY
.			setitem	Nord001AEditRtnDate,0,str10
.		endif
..
..		if (N2 = 10)
..			unpack	str10,ORTNDTEM,str1,ORTNDTED,str1,ORTNDTEC,ORTNDTEY
..		elseif (N2 = 8)
..			unpack	str10,ORTNDTEM,ORTNDTED,ORTNDTEC,ORTNDTEY
..		elseif (N2 <> 0)
..			alert	caution,"Return	Date Must be in	MMDDCCYY Format",result
..			goto OrderReturnRtnDate
..		endif
..END PATCH 3.77.9 REPLACED LOGIC  // now whatever's in return text box is 12/12/1234 or blank
.		move	ORTNDTEM,N2
..START PATCH 3.77.9 REPLACED LOGIC
..		if (N2 > "12")
.		if (N2 > "12" OR N2 < 1)
..END PATCH 3.77.9 REPLACED LOGIC
.			alert	caution,"Invalid Return Date month!",result
.			goto OrderReturnRtnDate
.		else
.			move	ORTNDTED,N2
..START PATCH 3.77.9 REPLACED LOGIC
..			if (N2 > "31")
.			if (N2 > "31" OR N2 < 1)
..END PATCH 3.77.9 REPLACED LOGIC
.				alert	caution,"Invalid Return Date day!",result
.				goto OrderReturnRtnDate
.			else
.				move	ORTNDTEC,N2
..START PATCH 3.77.9 REPLACED LOGIC
..				if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
.				if (N2 < "19" OR N2 > "25")
..					alert	caution,"Invalid Year!",result
.					alert	caution,"Invalid Return Date century!",result
..END PATCH 3.77.9 REPLACED LOGIC
.					goto OrderReturnRtnDate
.				elseif (N2 = "19")
.					move	ORTNDTEY,N2
..START PATCH 3.77.9 REPLACED LOGIC
..					if (N2 < "80")
.					if (N2 < "80" OR N2 < 1)
..END PATCH 3.77.9 REPLACED LOGIC
.						alert	caution,"Invalid Return DateYear!",result
.						goto OrderReturnRtnDate
.					endif
.				endif
.			endif
.		endif
.		call	TRIM using ORTNDTEM
.		count	N2,ORTNDTEM
..START PATCH 3.77.9 REMOVED LOGIC
..		if (N2 <> 0 AND	ORTNDTEM <> "00")
.			pack	newdate1,ORTNDTEM,SLASH,ORTNDTED,SLASH,ORTNDTEC,ORTNDTEY
..		else  // return date is blank
..			clear	newdate1
..		endif
..END PATCH 3.77.9 REMOVED LOGIC
.		setitem	Nord001AEditRtnDate,0,newdate1
..START PATCH 10-16-2001	BUG FIX	WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
..		 move	 ORTNDTEM,MM
..		 move	 ORTNDTED,DD
..		 move	 ORTNDTEY,YY
..		 call	 CVTJUL
..		 move	 JULDAYS,howmany
...Order	Date must be written to	before this - ASH
..		 move	 OODTEM,MM
..		 move	 OODTED,DD
..		 move	 OODTEY,YY
..		 call	 CVTJUL
..		 if (howmany < JULDAYS)
..			 alert	 caution,"Return Date Must be After Order Date!",result
..			 goto	 OrderReturnRtnDate
..		 endif
..START PATCH 3.77.9 REMOVED LOGIC
..		call	OrderVerifyReturnDate
..END PATCH 3.77.9 REMOVED LOGIC
..END PATCH 10-16-2001 BUG FIX WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
.	endif
..
.	getitem	Nord001AEditMailDate,0,str10  // now work on mail date
..START PATCH 3.77.9 REPLACED LOGIC
..	getitem	Nord001AEditMailDate,0,str10
..	call	TRIM using str10
..	count	N2,str10
..	if (N2 = 10 | N2 = 8)
..		if (N2 = 10)
..			unpack	str10,OMDTEM,str1,OMDTED,str1,OMDTEC,OMDTEY
..		elseif (N2 = 8)
..			unpack	str10,OMDTEM,OMDTED,OMDTEC,OMDTEY
..		endif
..		move	OMDTEC,N2
..		if (N2 = "19")
..			move	OMDTEY,N2
..			if (N2 < "80")
..				alert	caution,"Invalid Mail Date year!",result
..				goto OrderReturnMailDate
..			endif
..		endif
..	elseif (N2 <> 0)
..		alert	caution,"Mail Date Must	be in MMDDCCYY Format",result
..		call	OrderReturnMailDate
..	else	.No Mail Date!!	 LCR would have	to enter "00000000" for	no Mail	Date
..		alert	caution,"Invalid Mail Date!",result
..		goto OrderReturnMailDate
..	endif
........................
.	call	Trim using str10
.	call	RemoveChar, str10, SLASH
.	count	result,str10
.	if (result = 8)
.		unpack	str10,OMDTEM,OMDTED,OMDTEC,OMDTEY
.		pack str10, OMDTEM, "/", OMDTED, "/", OMDTEC, OMDTEY
.		setitem	Nord001AEditMailDate,0,str10
.
.			move	OMDTEC,N2
.			if (N2 = "19")
.				move	OMDTEY,N2
.				if (N2 < "80")
.					alert	caution,"Invalid Mail Date year!",result
.					goto OrderReturnMailDate
.				endif
.			endif
.
.	elseif (result = 0)
..No Mail Date!!	 LCR would have	to enter "00000000" for	no Mail	Date
.				alert	caution,"Invalid Mail Date!",result
.				goto OrderReturnMailDate
.	elseif (result <> 8)
.		alert note, "Mail date must be in this format: MMDDCCYY", result
.		goto OrderReturnMailDate
.	endif
..END PATCH 3.77.9 	REPLACED LOGIC
..START PATCH 11FEB2002 ADDED LOGIC
..Poorly placed - can create erroneous Mail Date records w/o consideration of verification return
..Should be moved to area right before NORDUPD.
.	if (HoldMDate <> "")
.		pack	str8,OMDTEC,OMDTEY,OMDTEM,OMDTED
.		if (HoldMDate <> str8)
.			pack	NMLDLR,OLRN
.			call	Trim using NMLDLR
.			if (NMLDLR <> "")
.				clock	timestamp,NMLDTIME
.				move	HoldMDate,NMLDDATE
.				move	INITS,NMLDINIT
.				move	"V.DataMDate-NMLDWRT", Location
.				pack	KeyLocation,"Key: ",NMLDFLD
.				call	NMLDWRT
.			endif
.		endif
.	endif
..END PATCH 11FEB2002 ADDED LOGIC
..Following logic will allow no entry for Mail Date even	though above logic will	return
..you if	you had	no data	in field.  I leave it here it case we eventually want to allow
..no data to be entered in field.
..START PATCH 3.77.9  - REPLACED LOGIC
.	move	OMDTEM,N2
.	if (N2 > "12")
.		alert	caution,"Invalid Mail Date month!",result
.		goto OrderReturnMailDate
.	else
.		move	OMDTED,N2
.		if (N2 > "31")
.			alert	caution,"Invalid Mail Date day!",result
.			goto OrderReturnMailDate
.		else
.			move	OMDTEC,N2
..			if (N2 <> C0 AND (N2 < "19" OR N2 > "25"))
.			if (N2 < "19" OR N2 > "25")
..END PATCH 3.77.9  - REPLACED LOGIC
..START PATCH 3.77.9 DREW - REPLACED LOGIC
..				if (N2 = "11")
.				//Test for Special values available to only LCR records
.				getitem	Nord001AEditMailDate,0,str10
.				if (str10 = "00/00/0000" | str10 = "11/11/1111")
..END PATCH 3.77.9 DREW - REPLACED LOGIC
..START PATCH 3.6 REPLACED LOGIC
..					if (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
.					if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
..END PATCH 3.6 REPLACED LOGIC
..START PATCH 3.77.9 DREW - REPLACED LOGIC
..						alert	caution,"Invalid Year!",result
.						alert	caution,"Invalid Mail Date!",result
..END PATCH 3.77.9 DREW - REPLACED LOGIC
.						goto OrderReturnMailDate
.					endif
.				elseif (N2 = "19")
.					move	OMDTEY,N2
.					if (N2 < "80")
.						alert	caution,"Invalid Mail Date year!",result
.						goto OrderReturnMailDate
.					endif
.				else
.					alert	caution,"Invalid Mail Date year!",result
.					goto OrderReturnMailDate
.				endif
.			endif
.		endif
.	endif
.	call	TRIM using OMDTEM
.	count	N2,OMDTEM
.	if (N2 <> 0 AND	OMDTEM <> "00" AND OMDTEC <> "11")
.		pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.	else
.		clear	newdate1
.	endif
.	setitem	Nord001AEditMailDate,0,newdate1
..
..START PATCH 	3.77.9 	REPLACED LOGIC
..	pack	str4,ORTNDTEC,ORTNDTEY
.	pack	str4, OMDTEC, OMDTEY  // mailer date, not return date, is what can be all 0s or 1s
..END PATCH 	3.77.9 	REPLACED LOGIC
..
..START PATCH 3.61 REPLACED LOGIC
..	if (str4 <> "0000")
..		move	OMDTEM,MM
..		move	OMDTED,DD
..		move	OMDTEY,YY
..		call	CVTJUL
..		if (JULDAYS < howmany)
..			alert	caution,"Mail Date Must	be After Return	Date!",result
..			goto	OrderReturnMailDate
..		endif
..	endif
..START PATCH 	3.77.9 	ADDED LOGIC
.	getitem	Nord001AEditRtnDate,0,str10
.	if (str10 <> "")
.		move	ORTNDTEM,MM
.		move	ORTNDTED,DD
.		move	ORTNDTEC, CC
.		move	ORTNDTEY,YY
.		call	CVTJUL
.		move	JULDAYS,howmany   // howmany is return date
.	else
.		alert note, "Return Date is blank!", result
.		move C0, howmany
.	endif
..END PATCH 	3.77.9 	ADDED LOGIC
.	move	OODTEM,MM
.	move	OODTED,DD
.	move	OODTEC,CC
.	move	OODTEY,YY
.	call	CVTJUL
.	move	JULDAYS,N7    // N7 is order date
..START PATCH 	3.77.9 	ADDED LOGIC
.	if (str4 <> "0000" & str4 <> "1111")
..END PATCH 	3.77.9 	ADDED LOGIC
.		move	OMDTEM,MM
.		move	OMDTED,DD
.		move	OMDTEC,CC
.		move	OMDTEY,YY
.		call	CVTJUL         // JULDAYS is mail date
..START PATCH 	3.77.9 	ADDED LOGIC
.	else
.		alert note, "Special entry for Mail Date", result
.		move C0, JULDAYS
.	endif
..END PATCH 	3.77.9 	ADDED LOGIC
..START PATCH 	3.77.9 	REPLACED LOGIC
..	if (str4 <> "0000")
.	if (JULDAYS <> C0 AND howmany <> C0)  // we have both dates
..END PATCH 	3.77.9 	REPLACED LOGIC
.		if (JULDAYS < howmany)
.			alert	caution,"Mail Date Must	be After Return	Date!",result
.			goto	OrderReturnMailDate
..START PATCH 3.77.9	ADDED LOGIC
.		endif
.	endif
.	if (howmany <> C0) // howmany is return date, N7 is order date
.		sub 	N7, howmany, result  // subtract return from order date
.		if (result > 365)
.		 	alert plain,"Return date is more than a year after Order date.  Do you wish to continue?", result
.			if (result = 2)  // no
.				goto OrderReturnRtnDate
.			elseif (result = 1) // yes
.				// do nothing
.			else	// cancel
.				goto OrderReturnRtnDate
.			endif
.		endif
..END PATCH 	3.77.9	ADDED LOGIC
.	endif
..Compare to Order Date
..START PATCH 3.77.9 REPLACED LOGIC
..	if (JULDAYS < N7)
.	if (JULDAYS <> C0 AND JULDAYS < N7)
..END PATCH 3.77.9 REPLACED LOGIC
.			alert	caution,"Mail Date Must	be After Order Date!",result
.			goto	OrderReturnMailDate
..START PATCH 	3.77.9	ADDED LOGIC
.	else   // JULDAYS is mail date, N7 is order date
.		sub 	N7, JULDAYS, result  // subtract mail from order date
.		if (result > 365)
.		 	alert plain,"Mail date is more than a year after Order date.  Do you wish to continue?", result
.				if (result = 2)  // no
.					goto OrderReturnMailDate
.				elseif (result = 1) // yes
.					// do nothing
.				else	// cancel
.					goto OrderReturnMailDate
.				endif
.		endif
..END PATCH 	3.77.9	ADDED LOGIC
.	endif
..END PATCH 3.61 REPLACED LOGIC
.....................................................
.VERIFY	RETURNDATE/MAILDATE/ORDERDATE
	if (SecFlag = YES)
		call VerifyDate giving returnValue using Nord001AEditOrderDate
		if (returnValue = C2) // if order date is empty
			clear	OODTEM
			clear	OODTED
			clear	OODTEC
			clear	OODTEY
		elseif (returnValue = C1) // order date valid
			getitem	Nord001AEditOrderDate,0,str10
			call	Trim using str10
			call	RemoveChar, str10, SLASH
			unpack	str10,OODTEM,OODTED,OODTEC,OODTEY
		elseif (returnValue = C0) // order date invalid
			alert	caution,"Order Date Must be in MMDDCCYY	Format",result
			goto OrderReturnOrderDate
		endif
		pack	newdate1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
		setitem	Nord001AEditOrderDate,0,newdate1 // either blank or date
	endif
.
	call VerifyDate giving returnValue using Nord001AEditRtnDate
	if (returnValue = C2) // if order date is empty, Should	apply if OREUSE
		clear	ORTNDTEM
		clear	ORTNDTED
		clear	ORTNDTEC
		clear	ORTNDTEY
		setitem	Nord001AEditRtnDate,0,""  // BLANK DATE
	elseif (returnValue = C0)  // invalid date
		alert	caution,"Invalid Return Date",result
		goto OrderReturnRtnDate
	else  // valid date
		getitem	Nord001AEditRtnDate,0,str10
		call	Trim using str10
		call	RemoveChar, str10, SLASH
		unpack	str10,ORTNDTEM,ORTNDTED,ORTNDTEC,ORTNDTEY
		pack str10, ORTNDTEM, "/", ORTNDTED, "/", ORTNDTEC, ORTNDTEY
		setitem	Nord001AEditRtnDate,0,str10
	endif
.  now do mail date
	getitem	Nord001AEditMailDate,0,str10
	call	Trim using str10
	call	RemoveChar, str10, SLASH
	call VerifyDate giving returnValue using Nord001AEditMailDate
	if (returnValue = C2)  // mail date is empty
		alert	caution,"Empty Mail Date!",result
		goto OrderReturnMailDate
	elseif (returnValue = C0)  // invalid mail date
		if (str10 <> "00000000" AND str10 <> "11111111") // special codes are exceptions to invalid dates!
			alert	caution,"Invalid Mail Date!",result
			goto OrderReturnMailDate
		// pack 0s and 1s with slash and set text box
		else
			unpack	str10,OMDTEM,OMDTED,OMDTEC,OMDTEY
			pack str10, OMDTEM, "/", OMDTED, "/", OMDTEC, OMDTEY
			setitem	Nord001AEditMailDate,0,str10
		endif
	else   // valid mail date
		unpack	str10,OMDTEM,OMDTED,OMDTEC,OMDTEY
		pack str10, OMDTEM, "/", OMDTED, "/", OMDTEC, OMDTEY
		setitem	Nord001AEditMailDate,0,str10
	endif
.
.START PATCH 3.78.1	REMOVED LOGIC
.Poorly placed - can create erroneous Mail Date records w/o consideration of verification return
.Should be moved to area right before NORDUPD.
.	if (HoldMDate <> "")
.		pack	str8,OMDTEC,OMDTEY,OMDTEM,OMDTED
.		if (HoldMDate <> str8)
.			pack	NMLDLR,OLRN
.			call	Trim using NMLDLR
.			if (NMLDLR <> "")
.				clock	timestamp,NMLDTIME
.				move	HoldMDate,NMLDDATE
.				move	INITS,NMLDINIT
.				move	"V.DataMDate-NMLDWRT", Location
.				pack	KeyLocation,"Key: ",NMLDFLD
.				call	NMLDWRT
.			endif
.		endif
.	endif
.END PATCH 3.78.1	REMOVED LOGIC
.
	//Test for Special values available to only LCR records
	getitem	Nord001AEditMailDate,0,str10
	if (str10 = "00/00/0000" | str10 = "11/11/1111")
		if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
			alert	caution,"Invalid Mail Date!",result
			goto OrderReturnMailDate
		endif
	endif
.
	pack	str4, OMDTEC, OMDTEY  // will use to check if mailer date is 0s or 1s
.Calculate julian days for calculations
	getitem	Nord001AEditRtnDate,0,str10
	if (str10 <> "")
		move	ORTNDTEM,MM
		move	ORTNDTED,DD
		move	ORTNDTEC, CC
		move	ORTNDTEY,YY
		call	CVTJUL
		move	JULDAYS,howmany   // howmany is return date
	else // return date is blank
		move C0, howmany
	endif
	move	OODTEM,MM  // order date should never be blank
	move	OODTED,DD
	move	OODTEC,CC
	move	OODTEY,YY
	call	CVTJUL
	move	JULDAYS,N7    // N7 is order date
.
	if (str4 <> "0000" & str4 <> "1111")  // if mail date not special case
		move	OMDTEM,MM
		move	OMDTED,DD
		move	OMDTEC,CC
		move	OMDTEY,YY
		call	CVTJUL         // JULDAYS is mail date
	else  // special entry for mail date
		move C0, JULDAYS
	endif
.Check for correct sequencing
	if (SecFlag <> YES)	.I.S. can do whatever
		if (N7 > C0 & howmany > C0)
			if (howmany < N7)
				alert	caution,"Return Date Must be After Order Date!",result
				goto	OrderReturnRtnDate
			endif
		endif
		if (howmany > C0 & JULDAYS > C0)
			if (JULDAYS < howmany)
				alert	caution,"Mail Date Must	be After Return	Date!",result
				goto	OrderReturnMailDate
			endif
		endif
	endif
.Compare Return Date to Order Date
	if (howmany <> C0) // howmany is return date, N7 is order date
		sub 	N7, howmany, result  // subtract return from order date
		if (result > 365)
		 	alert plain,"Return date is more than a year after Order date.  Do you wish to continue?", result
			if (result = 2)  // no
				goto OrderReturnRtnDate
			elseif (result = 1) // yes
				// do nothing
			else	// cancel
				goto OrderReturnRtnDate
			endif
		endif
	endif
.Compare Mail Date to Order Date
	if (JULDAYS <> C0) // JULDAYS is mail date, N7 is order date
		sub 	N7, JULDAYS, result  // subtract mail from order date
		if (result > 365)
		 	alert plain,"Mail date is more than a year after Order date.  Do you wish to continue?", result
			if (result = 2)  // no
				goto OrderReturnMailDate
			elseif (result = 1) // yes
				// do nothing
			else	// cancel
				goto OrderReturnMailDate
			endif
		endif
.begin patch DH  check for maildate prior to orderdate test
		if	(juldays < N7)
		 	alert plain,"Mail date prior to the Order date.  Do you wish to continue?", result
			if (result = 2)  // no
				goto OrderReturnMailDate
			elseif (result = 1) // yes
				// do nothing
			else	// cancel
				goto OrderReturnMailDate
			endif
		endif
.end patch DH
	endif
.END PATCH 3.78 ADDED LOGIC
.
.VERIFY	OFFER
	clear	OODNUM
	getitem	Nord001AComboOffer,0,result
	getitem	Nord001AComboOffer,result,str45
	unpack	str45,str35,str6,str3
	call	Trim using str3
	if (str3 = "")
.Susan amended again so	that all records require OFFER 12/17/1999 - ASH
.		 if (mod < 5 OR	(mod = 6 AND NORD4STAT = "08") OR mod =	7)	.Pending Orders	do not require Offers
.Susan took this out first thing 12/13/1999 - day logic	went live - ASH
..No Offer required for	LM LCR's!!
.			 getitem Nord001bEditSales,0,str2
.			 if (str2 <> "06")
		alert	caution,"Offer Required!",result
		call	OrderSwitchTab using C1
		setfocus Nord001AComboOffer
		move	"Y",ReturnFlag
		return
.			 endif
.		 endif
	else
		pack	OODNUM,OMLRNUM,str3
	endif
.VERIFY	MEDIA
.- COMBOBOX LOADED IMMEDIATELY AFTER FORMS ARE LOADED
.Logic here is a bit strange but here is the idea:  MED20 currently holds blank	record,	and that
.has been associated with item #1 of combobox.	Item #1	will equal MED20, Item #2 will equal MED00,
.Item #3 will equal MED01, etc.	 When you reach	Item #20 one of	those displacements has	been
.soaked	up by associating MED20	with Item #1 and so you	need one less incrementation.
.SKIP ALL THIS FOR PENDING RECORDS!!!!
.	 if (mod = C5)
.		 move	 "20",OFOCODE
.		 setitem Nord001bComboMedia,0,C1
.	 else
		getitem	Nord001bComboMedia,0,N2
		if (N2 <= C1)
			move	"20",OFOCODE
		else
			sub	C1,N2
			if (N2 < "21")
				sub	C1,N2
			endif
			move	N2,str3
			call	TRIM using str3
			count	result,str3
.Pack number with preceding zeroes
.Have not used ZFILLIT as this is a special field
			if (result < 2)
				sub	result from "2"	giving N1
				setlptr	filler,	N1
				pack	str2,filler,str3
			else
				move	str3,str2
			endif
			move	str2,OFOCODE
		endif
.	 endif

.VERIFY	TAPE RETURN
	getitem	Nord001bComboTape,0,N1
	if (N2 <> 7 AND	N2 <> 8	AND N2 <> 0)	  .Pending Order
		move	B1,OTAPERET
		setitem	Nord001bComboTape,0,C1
	else
		if (N1 <= 1)
			move	B1,OTAPERET
		elseif (N1 = 2)
			move	YES,OTAPERET
		elseif (N1 = 3)
			move	NO,OTAPERET
		endif
	endif
.VERIFY	SHIPPING
.START PATCH 3.71 REPLACED LOGIC
..check to see if list Pathfinder (1049) or Fairness in accuracy 7 Reporting (16065)
..START PATCH 3.6 REPLACED LOGIC
..	if (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")) AND (OLNUM = "001049" OR OLNUM = "016065"))
..START PATCH 07/12/2002 JD REQUEST
..	if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")) AND (OLNUM = "001049" OR OLNUM = "016065"))
.	if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")) AND (OLNUM = "016065"))
..END PATCH 07/12/2002 JD REQUEST
..END PATCH 3.6 REPLACED LOGIC
..START PATCH 10-26-01 ADDED FOR	JD - ASH
..		 if (ORTNNUM = "0040")
.		if (ORTNNUM = "0040" | ORTNNUM = "5224")
..END PATCH 10-26-01 ADDED FOR JD - ASH
.			move	B2,OSHP
.			setitem	Nord001bComboShip,0,1
.		else
.			pack	OSHP,"06"
.			setitem	Nord001bComboShip,0,8
.		endif
.	else
.		getitem	Nord001bComboShip,0,N2
.		if (N2 <= 1)
.			move	B2,OSHP
.		else
.			sub	C2,N2
.			move	N2,str3
.     			call	TRIM using str3
.			count	result,str3
..Pack number with preceding zeroes
..Have not used ZFILLIT as this is a special field
.			if (result < 2)
.				sub	result from "2"	giving N1
.				setlptr	filler,	N1
.				pack	str2,filler,str3
.			else
.				move	str3,str2
.			endif
.			move	str2,OSHP
.		endif
.	endif
..................................................................................
	getitem	Nord001bComboShip,0,N2
	if (N2 <= 1)
		move	B2,OSHP
	else
		sub	C2,N2
		move	N2,str3
		call	TRIM using str3
		count	result,str3
.Pack number with preceding zeroes
.Have not used ZFILLIT as this is a special field
		if (result < 2)
			sub	result from "2"	giving N1
			setlptr	filler,	N1
			pack	str2,filler,str3
		else
			move	str3,str2
		endif
		move	str2,OSHP
	endif
.END PATCH 3.71 REPLACED LOGIC
.VERIFY	Order3 Screen
.DESC001 established in	OrderExchangeStatus2B
.If an XSTAT currently exists it will be overwritten later if
.this Order is an Exchange!!!
	getitem	nord001CEditSpecial1,0,DESC001
	call	Trim using DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	getprop	nord001CEditSpecial,multiline=result
	getitem	nord001CEditSpecial,0,hold2
	count	N7,hold2
	if (N7 > 0 AND NewFlag = YES)
		move	YES,SpecFlag2
	endif
	clear	HowMany
	loop
		movefptr hold2,N9
		movelptr hold2,N8
		until (N8 = N9)
		call	PARSITUP using str55,hold2,C1
		add	C1,HowMany
	repeat
	if (HowMany > result)
		clear	taskname
		append	"You may only have up to ",taskname
		move	result,str9
		call	Trim using str9
		append	str9,taskname
		append	" lines",taskname
		append	carr,taskname
		append	"for Special Instructions!",taskname
		reset	taskname
		alert	caution,taskname,result
		call	OrderSwitchTab using C3
		setfocus nord001CEditSpecial
		move	YES,ReturnFlag
		return
	endif
	if (SpecFlag2 =	YES | NewFlag =	YES)		A change was made to Special Instructions
		move	NO,SpecFlag
		getitem	nord001CEditSpecial,0,DESC002
		call	Trim using DESC002
.Make sure they	didn't just enter in Carriage Returns
.Essentially code below	is a version of	Trim
		if (DESC002 <> "")
			clear	N9
			clear	N8
			movelptr DESC002,N9
			loop
				cmatch	Carr,DESC002
				if not equal
					move	YES,SpecFlag
					goto Order3End
				endif
				bump	DESC002
				movefptr DESC002,N8
				until	(N8 = N9)
			repeat
.must do one last compare
			cmatch	Carr,DESC002
			if not equal
				move	YES,SpecFlag
			endif
		endif
Order3End
		if (SpecFlag = YES)
			reset	DESC002
		endif
	endif
	if (Spec2Flag =	YES | NewFlag =	YES)
		getitem	nord001CEditSpecial2,0,DESC003
		call	Trim using DESC003
		setitem	nord001CEditSpecial2,0,DESC003
.START PATCH 3.71.1 MOVED LOGIC
.moved from spot below
		clear	str1
.END PATCH 3.71.1 MOVED LOGIC
.Make sure they	didn't just enter in Carriage Returns
.Essentially code below	is a version of	Trim
		if (DESC003 <> "")
			clear	N9
			clear	N8
.START PATCH 3.71.1 MOVED LOGIC
.moved to spot above
.			clear	str1
.END PATCH 3.71.1 MOVED LOGIC
			movelptr DESC003,N9
			loop
				cmatch	Carr,DESC003
				if not equal
					move	YES,str1
					goto Order3End2
				endif
				bump	DESC003
				movefptr DESC003,N8
				until	(N8 = N9)
			repeat
.must do one last compare
			cmatch	Carr,DESC003
			if not equal
				move	YES,str1
			endif
		endif
Order3End2
		if (str1 = YES)
			reset	DESC003
		endif
	endif
	if (Spec3Flag =	YES | NewFlag =	YES)
		getitem	nord001CEditSpecial3,0,DESC004
		call	Trim using DESC004
		setitem	nord001CEditSpecial3,0,DESC004
.START PATCH 3.71.1 MOVED LOGIC
.moved from spot below
		clear	str1
.END PATCH 3.71.1 MOVED LOGIC
.Make sure they	didn't just enter in Carriage Returns
.Essentially code below	is a version of	Trim
		if (DESC004 <> "")
			clear	N9
			clear	N8
.START PATCH 3.71.1 MOVED LOGIC
.moved to spot above
.			clear	str1
.END PATCH 3.71.1 MOVED LOGIC
			movelptr DESC004,N9
			loop
				cmatch	Carr,DESC004
				if not equal
					move	YES,str1
					goto Order3End3
				endif
				bump	DESC004
				movefptr DESC004,N8
				until	(N8 = N9)
			repeat
.must do one last compare
			cmatch	Carr,DESC004
			if not equal
				move	YES,str1
			endif
		endif
Order3End3
		if (str1 = YES)
			reset	DESC004
		endif
	endif
.VERIFY	SALESPERSON
	getitem	Nord001bEditSales,0,str2
	if (mod	= 5 | mod = 6)
		move	"06",str2
		call	OrderLoadSalespersonVerify using C1
	endif
	count	HowMany,str2
	if (HowMany = "1")
		getitem	Nord001bEditSales,0,str1
		pack	OSALES10,C0
		pack	OSALES,str1
	elseif (HowMany	= "0")
		alert caution,"Sales Person Required!",result
		call	OrderSwitchTab using C2
		setfocus Nord001bEditSales
		move	"Y",ReturnFlag
		return
	else
		unpack	str2,OSALES10,OSALES
	endif
	if (str2 > "22")
		move	B1,OSALES10
		move	B1,OSALES
	elseif (str2 = "06")		.List Management
.START PATCH 3.6 REPLACED LOGIC
.		if (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
		if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
.END PATCH 3.6 REPLACED LOGIC
			if (OBRKNUM = "")	.Trimmed already
				alert	caution,"Broker	Required for List Management!",result
				call	OrderSwitchTab using C1
				setfocus Nord001AEditBrk
				move	"Y",ReturnFlag
				return
			elseif (OBRKCNT	= "")	.Trimmed already
				alert	caution,"Contact Required for List Management!",result
				call	OrderSwitchTab using C1
				setfocus Nord001AEditBrkContact
				move	"Y",ReturnFlag
				return
			endif
		endif
	endif
	setitem	Nord001bEditSales,0,str2
.VERIFY	CONTACT
.	 if (str2 = "06" & (mod	= 5 | mod = 3 |	mod = 7))    .Pending Order/Pending LCR	with List Management
.	 if (str2 = "06" & mod = 5)			      .Pending Order
.As of 07/21/2000 JN wanted following code to catch ALL	Pending	Orders and throw in CV as Contact
.	 if (str2 = "06" & (mod	= 5 | (mod = 6 & NORD4STAT = "08") | (mod = 7 &	NORD5STAT = "04"))) .New Pending Order OR Pending ORder	being turned into Live Order OR	LCR being turned into Live Order
.START PATCH 3.71.5 REMOVED LOGIC
.START PATCH 3.71.7 COMMENTED BACK IN CODE FROM LOGIC REMOVED IN PATCH 3.71.5
	if (str2 = "06"	& (mod = 5 | mod = 6 | (mod = 7	& NORD5STAT = "04"))) .Pending Order OR	LCR being turned into Live Order
.END PATCH 3.71.7 COMMENTED BACK IN CODE FROM LOGIC REMOVED IN PATCH 3.71.5
.END PATCH 3.71.5 REMOVED LOGIC
.THIS CODE MAY NEED TO CHANGE.	IT WILL	NOT ALLOW MODIFICATION OF OCOCODE WHILE	IN PENDING MODE!!!!!!!!!
.IF MODE SET PRIOR TO ALLOWING ANY ENTRY AT ALL	THEN THIS CODE CAN BE PLACED IN	LOSTFOCUS EVENT
.FOR OSALES10,OSALES
.		 clear	 str2
.		 pack	 str2,OSALES10,OSALES
.		 move	 C0,N2
.		 move	 str2,N2
.		 if (N2	= C6)
.START PATCH 3.71.5 REMOVED LOGIC
.START PATCH 3.71.7 COMMENTED BACK IN & MODIFIED CODE FROM CNT #2 TO CNT #17 JOEY G. LOGIC REMOVED IN PATCH 3.71.5
.Start patch 3.79.4
	move	"17",OCOCODE	.Joey Gamache
.	move	"19",OCOCODE	Agnes Alvarez
.End patch 3.79.4
			call	OrderLoadCombo Using Nord001bComboContact,OCOCODE	    .Catherine Veyna
.END PATCH 3.71.7 COMMENTED BACK IN & MODIFIED CODE FROM CNT #2 TO CNT #17 JOEY G. LOGIC REMOVED IN PATCH 3.71.5
.END PATCH 3.71.5 REMOVED LOGIC
.		 endif
.START PATCH 3.71.5 REMOVED LOGIC
.START PATCH 3.71.7 COMMENTED BACK IN CODE FROM LOGIC REMOVED IN PATCH 3.71.5
	else
.END PATCH 3.71.7 COMMENTED BACK IN CODE FROM LOGIC REMOVED IN PATCH 3.71.5
.END PATCH 3.71.5 REMOVED LOGIC
		getitem	Nord001bComboContact,0,N2
		if (N2 <= 1)
			alert	caution,"Valid Contact Required!",result
			call	OrderSwitchTab using C2
			setfocus Nord001bComboContact
			move	"Y",ReturnFlag
			return
		endif
		getitem	Nord001bComboContact,N2,str45
		unpack	str45,str35,str1,OCOCODE
.START PATCH 3.71.5 REMOVED LOGIC
.START PATCH 3.71.7 COMMENTED BACK IN CODE FROM LOGIC REMOVED IN PATCH 3.71.5
	endif
.START PATCH 3.71.7 COMMENTED BACK IN CODE FROM LOGIC REMOVED IN PATCH 3.71.5
.END PATCH 3.71.5 REMOVED LOGIC
.VERIFY	CALLER
	getitem	Nord001bComboCaller,0,N2
	if (N2 <= 1)
		clear	OCO2CODE
	else
		getitem	Nord001bComboCaller,N2,str45
		unpack	str45,str35,str1,OCO2CODE
	endif
.VERIFY	CONTINUATION
	 getitem Nord001bEditContDate,0,newdate1
	 call	 Trim using newdate1
	 getitem Nord001bEditContLR,0,OLRNCO
	 call	Trim using OLRNCO
	 getitem Nord001bEditContQty,0,str11
	 call	 RemoveChar using str11,COMMA
	 call	 Trim using str11
	 move	 str11,OQTYCO
	 getitem Nord001bComboCont,0,N1
	 if ((N1 <= 1) OR (N1 =	3))
		if (N1 <= 1)
			move	B1,OCCODE
		elseif (N1 = 3)
			move	C2,OCCODE
		endif
		if ((newdate1 <> "") OR	(OLRNCO	<> "") OR (OQTYCO <> ""))
			alert	plain,"Should I	clear the Cont.	fields?!?",result
			if (result <> 1)
				call	OrderSwitchTab using C2
				setfocus Nord001bComboCont
				move	"Y",ReturnFlag
				return
			endif
			move	"      ",OLRNCO
			move	"	  ",OQTYCO
			clear	OODTECOD
			clear	OODTECOM
			clear	OODTECOC
			clear	OODTECOY
			setitem	Nord001bEditContDate,0,""
			setitem	Nord001bEditContLR,0,""
			setitem	Nord001bEditContQty,0,""
		endif
	 elseif	(N1 = 2)   .Omit
		call   ZFILLIT using OQTYCO
		move	C1,OCCODE
		count	N2,newdate1
		if (N2 = 10)
			unpack	newdate1,OODTECOM,str1,OODTECOD,str1,OODTECOC,OODTECOY
		elseif (N2 = 8)
			unpack	newdate1,OODTECOM,OODTECOD,OODTECOC,OODTECOY
		else
			alert	caution,"Date must be entered in MMDDCCYY Format!",result
			call	OrderSwitchTab using C2
			setfocus Nord001bEditContDate
			move	"Y",ReturnFlag
			return
		endif
		pack	newdate,OODTECOM,slash,OODTECOD,slash,OODTECOC,OODTECOY
		setitem	Nord001bEditContDate,0,newdate1
		setitem	Nord001bEditContQty,0,OQTYCO
.OLRNCO	already	processed
	 endif
.VERIFY	NET
.If in pending mode then these fields should be	disabled	THEY ARE!!!
.There should be other verbage to keep users from modifying these fields
.depending on OSTAT.
	getitem	Nord001bEditNetCharge,0,str6
	move	C0,N32
	move	str6,N32
	if (N32	= C0)
		clear	str6
		setitem	Nord001bEditNetCharge,0,str6
	endif
	move	N32,ONETRC
	getitem	Nord001bEditNetMin,0,str9
	call	RemoveChar using str9,COMMA
	call	Trim using str9
	move	C0,N7
	move	str9,N7
	if (N7 = C0)
		clear	str9
		setitem	Nord001bEditNetMin,0,str9
	endif
	move	N7,ONETMIN
	getitem	Nord001bEditNetPercent,0,ONETPER
	move	C0,N2
	move	ONETPER,N2
	if (N2 = C0)
	       	clear	ONETPER
		setitem	Nord001bEditNetPercent,0,ONETPER
	endif
	getitem	Nord001bComboNet,0,N2
.Test if List Management
	pack	str2,OSALES10,OSALES
	move	str2,N3
.START PATCH 3.6 REPLACED LOGIC
.	if (N3 = C6 AND	mod <> C5 AND (mod <> C6 OR (mod = 6 AND NORD4STAT = "08")) AND	mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) .Pending Orders should be List Management but no net	fields!
	if (N3 = C6 AND	mod <> C5 AND (mod <> C6 OR (mod = 6 AND NORD4STAT = "08")) AND	mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) .Pending Orders should be List Management but no net	fields!
.END PATCH 3.6 REPLACED LOGIC
		if (N2 <> 4 AND	N2 <> 1)
			clear	taskname
			append	"List Management requires either Flat Discount",taskname
			append	carr,taskname
			append	"or blank entry	for Net	field!",taskname
			reset	taskname
			alert	note,taskname,result
			move	"F",ONETFM
			setitem	Nord001bComboNet,0,4
			call	OrderEnableNet
			call	OrderSwitchTab using C2
			setfocus Nord001bComboNet
			move	"Y",ReturnFlag
			return
		endif
	endif
	if (N2 > 2)
		if (ONETPER = "" | ONETPER = "0")
.START PATCH 3.73 ADDED LOGIC
			if (mod <> C5 AND (mod <> C6 OR (mod = 6 AND NORD4STAT = "08")) AND mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
.END PATCH 3.73 ADDED LOGIC
				clear	taskname
				append	"This Net Option requires a Percentage!",taskname
				append	carr,taskname
				append	"Do you wish to clear the Net Option Field?",taskname
				reset	taskname
				alert	plain,taskname,result
				if (result <> 1)
					setfocus Nord001bEditNetPercent
					move	"Y",ReturnFlag
					return
				endif
				move	C1,N2
				setitem	Nord001bComboNet,0,N2
.START PATCH 3.73 ADDED LOGIC
			endif
.END PATCH 3.73 ADDED LOGIC
		endif
	endif
	if (N2 = 2)
		move	"N",ONETFM
		call	NetSetZero
	elseif (N2 = 3)
		move	"M",ONETFM
	elseif (N2 = 4)
		move	"F",ONETFM
	else   .N2 =1	NULL
		move	B1,ONETFM
		call	NetSetZero
	endif
	goto	VERIFYSAMPLE
NetSetZero
.Give user option of going back	to change Nord001BComboNet and retain previous Net values
	if ((ONETPER <>	"0" AND	ONETPER	<> "") OR (ONETRC > C0)	OR (ONETMIN > C0))
		call	OrderSwitchTab using C2
		alert	plain,"Should I	clear Net Fields?!?",result
		if (result <> C1)
			setfocus Nord001bComboNet
			move	"Y",ReturnFlag
.Following must be here in case going from LCR to Live.  We must escape from Verify routine now!!
			noreturn
			return
		else
			setitem	Nord001bEditNetCharge,0,""
			setitem	Nord001bEditNetMin,0,""
			setitem	Nord001bEditNetPercent,0,""
			clear	ONETPER
			clear	ONETRC
			clear	ONETMIN
		endif
	endif
.Clear Numeric Net fields if necessary,	so that	Billing	program	runs smoothly.
	if ((ONETPER = "0" OR ONETPER =	"") AND	(ONETRC	= C0) AND (ONETMIN = C0))
		setitem	Nord001bEditNetCharge,0,""
		setitem	Nord001bEditNetMin,0,""
		setitem	Nord001bEditNetPercent,0,""
		clear	ONETPER
		clear	ONETRC
		clear	ONETMIN
	endif
	return
VERIFYSAMPLE
.START PATCH 3.72.2 REPLACED LOGIC
.	getitem	Nord001bComboSam,0,N2
.	getitem	Nord001bComboSample,0,N3
..If Manager is All American List do not	allow sample - per BLO	  04Dec96 DLH
..If LCR	and if Manager is Whats-Her-Name do not	allow sample - per MG/SA 13Jan97 DLH
.	if (OLON = "0994" OR OLON = "4574" OR (((mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5	OR (mod	= 6 AND	NORD4STAT <> "08"))) AND (OLON = "4799"	OR OLON	= "4801")))
.		if (N2 = 2)	.Sample	Enclosed
.			clear	taskname
.			append	"List Owner does not allow Samples.",taskname
.			append	carr,taskname
.			append	"Clearing Sample Field now.",taskname
.			reset	taskname
.			alert	caution,taskname,result
.		endif
.		move	Z3,OSAMCDE
.		clear	OSCODE
.		setprop	Nord001bComboSample,enabled=0,bgcolor=grey
.		setprop	Nord001bComboSam,enabled=0,bgcolor=grey
.		setitem	Nord001bComboSample,0,1
.		setitem	Nord001bComboSam,0,1
..START PATCH 3.65 ADDED LOGIC
.		setitem	Nord001bStatSamplesInactive,0,""
..END PATCH 3.65 ADDED LOGIC
.	else
..START PATCH 3.65 ADDED LOGIC
.		if ((mod = 7 AND NORD5STAT = "04") | (mod = 6 AND NORD4STAT = "08") | NewFlag = YES)
.			if (N2 = 2 | N2 = 4)		.Sample Enclosed/Sample Previously Cleared
..START PATCH 3.72.2 REPLACED LOGIC
..				getitem	Nord001bComboSample,N3,str35
..				unpack	str35,str30,str4,str1
.				getitem	Nord001bComboSample,N3,str55
.				unpack	str55,str30,str1,str10,str4,str1
..END PATCH 3.72.2 REPLACED LOGIC
.				if (str1 = "I")
.					alert	caution,"Sample is Inactive!",result
.					move	YES,ReturnFlag
.					call	OrderSwitchTab using C2
.					setfocus Nord001bComboSample
.					return
.				endif
.			endif
.		endif
.		setitem	Nord001bStatSamplesInactive,0,""
..END PATCH 3.65 ADDED LOGIC
.		if (N2 = 2)
..LCR to	Live should reset Sample Combo Box
.			if ((mod = 7 AND NORD5STAT = "04") | (mod = 6 AND NORD4STAT = "08"))
.				alert	plain,"Was Sample Previously Cleared?",result
.				if (result = 1)		.YES
.					move	C3,OSCODE
.					setitem	Nord001bComboSam,0,4
.					move	Z3,OSAMCDE
.					setitem	Nord001bComboSample,0,1
.				elseif (result = 2)	.NO
.					move	C1,OSCODE
..START PATCH 3.72.2 REPLACED LOGIC
..					getitem	Nord001bComboSample,N3,str35
..					unpack	str35,str30,str1,str3
.					getitem	Nord001bComboSample,N3,str55
.					unpack	str55,str30,str1,str10,str1,str3
..END PATCH 3.72.2 REPLACED LOGIC
.					type	str3
.					if not equal
.						move	Z3,OSAMCDE
.						setitem	Nord001bComboSample,0,1
.					else
.						move	str3,OSAMCDE
.					endif
.				else			.CANCEL
.					move	YES,ReturnFlag
.					call	OrderSwitchTab using C2
.					setfocus Nord001bComboSam
.					return
.				endif
.			else
.				move	C1,OSCODE
..START PATCH 3.72.2 REPLACED LOGIC
..				getitem	Nord001bComboSample,N3,str35
..				unpack	str35,str30,str1,str3
.				getitem	Nord001bComboSample,N3,str55
.				unpack	str55,str30,str1,str10,str1,str3
..END PATCH 3.72.2 REPLACED LOGIC
.				type	str3
.				if not equal
.					alert	caution,"Invalid Sample, I will	clear the field!!",result
.					move	B1,OSCODE
.					move	Z3,OSAMCDE
.					setitem	Nord001bComboSample,0,1
.					setitem	Nord001bComboSam,0,1
.				else
.					move	str3,OSAMCDE
.				endif
.			endif
.		else
..START PATCH 3.61 REPLACED LOGIC
..			if (N2 <= 1)
..				move	B1,OSCODE
..			elseif (N2 = 3)
..				move	C2,OSCODE
..			elseif (N2 = 4)
..				move	C3,OSCODE
..			endif
..			move	Z3,OSAMCDE
..			setitem	Nord001bComboSample,0,1
.			if (N2 = 4)		.Sample Previously Cleared
.				move	C3,OSCODE
..START PATCH 3.72.2 REPLACED LOGIC
..				getitem	Nord001bComboSample,N3,str35
..				unpack	str35,str30,str1,str3
.				getitem	Nord001bComboSample,N3,str55
.				unpack	str55,str30,str1,str10,str1,str3
..END PATCH 3.72.2 REPLACED LOGIC
.				call	Trim using str3
.				if (str3 <> "")
.					type	str3
.					if not equal
.						alert	caution,"Invalid Sample, I will	clear the field!!",result
.						move	Z3,OSAMCDE
.						setitem	Nord001bComboSample,0,1
.					else
.						move	str3,OSAMCDE
.					endif
.				else
.					move	Z3,OSAMCDE
.					setitem	Nord001bComboSample,0,1
.				endif
.			else
.				if (N2 <= 1)
.					move	B1,OSCODE
.				elseif (N2 = 3)
.					move	C2,OSCODE
.				endif
.				move	Z3,OSAMCDE
.				setitem	Nord001bComboSample,0,1
.			endif
..END PATCH 3.61 REPLACED LOGIC
.		endif
.	endif
................................................................
	getitem	Nord001bComboSam,0,N2
	getitem	Nord001bEditSample,0,str55
.START PATCH 3.76.4 ADDED LOGIC
.Eventually will be following
.	if (COMPOSAMP = "T")
	if (OWNBLK = "1")
		if (N2 = 2)	.Sample	Enclosed
			clear	taskname
			append	"List Owner does not allow Samples!",taskname
			append	carr,taskname
			append	"Clearing Sample Field now.",taskname
			reset	taskname
			alert	note,taskname,result
.			alert	plain,taskname,result
.			if (result <> 1)
.				move	YES,ReturnFlag
.				call	OrderSwitchTab using C2
.				setfocus Nord001bEditSample
.				return
.			endif
		endif
		move	Z3,OSAMCDE
		clear	OSCODE
		setprop	Nord001bEditSample,enabled=0,bgcolor=grey
		setprop	Nord001bListViewSamples,enabled=0,bgcolor=grey
		setprop	Nord001bButtonArrow,enabled=0
		setprop	Nord001bComboSam,enabled=0,bgcolor=grey
		setitem	Nord001bEditSample,0,""
		setitem	Nord001bComboSam,0,1
		setitem	Nord001bStatSamplesInactive,0,""
		clear	str55
		move	C0,N2
	endif
.END PATCH 3.76.4 ADDED LOGIC
.If Manager is All American List do not	allow sample - per BLO	  04Dec96 DLH
.If LCR	and if Manager is Whats-Her-Name do not	allow sample - per MG/SA 13Jan97 DLH
	if (OLON = "0994" OR OLON = "4574" OR (((mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5	OR (mod	= 6 AND	NORD4STAT <> "08"))) AND (OLON = "4799"	OR OLON	= "4801")))
		if (N2 = 2)	.Sample	Enclosed
			clear	taskname
			append	"List Owner does not allow Samples.",taskname
			append	carr,taskname
			append	"Clearing Sample Field now.",taskname
			reset	taskname
			alert	caution,taskname,result
		endif
		move	Z3,OSAMCDE
		clear	OSCODE
		setprop	Nord001bEditSample,enabled=0,bgcolor=grey
		setprop	Nord001bListViewSamples,enabled=0,bgcolor=grey
		setprop	Nord001bButtonArrow,enabled=0
		setprop	Nord001bComboSam,enabled=0,bgcolor=grey
		setitem	Nord001bEditSample,0,""
		setitem	Nord001bComboSam,0,1
		setitem	Nord001bStatSamplesInactive,0,""
	else
		if ((mod = 7 AND NORD5STAT = "04") | (mod = 6 AND NORD4STAT = "08") | NewFlag = YES)
			if (N2 = 2 | N2 = 4)		.Sample Enclosed/Sample Previously Cleared
				unpack	str55,OSAMCDE,str3,str30,str1,str1
				if (str1 = "I")
					alert	caution,"Sample is Inactive!",result
					move	YES,ReturnFlag
					call	OrderSwitchTab using C2
					setfocus Nord001bEditSample
					return
				endif
			endif
		endif
		setitem	Nord001bStatSamplesInactive,0,""
		if (N2 = 2)
.LCR to	Live should reset Sample Combo Box
			if ((mod = 7 AND NORD5STAT = "04") | (mod = 6 AND NORD4STAT = "08"))
				alert	plain,"Was Sample Previously Cleared?",result
				if (result = 1)		.YES
					move	C3,OSCODE
...Begin Patch 24Apr2007   DO NOT CLEAR INFO
					setitem	Nord001bComboSam,0,4          .leave alone for now
.					move	Z3,OSAMCDE
.					setitem	Nord001bEditSample,0,""
...end Patch 24Apr2007   DO NOT CLEAR INFO
				elseif (result = 2)	.NO
					move	C1,OSCODE
					unpack	str55,OSAMCDE
.START PATCH 3.72.7 REPLACED LOGIC
.					type	str3
.					if not equal
.						move	Z3,OSAMCDE
.						setitem	Nord001bEditSample,0,""
.					else
.						move	str3,OSAMCDE
.					endif
					type	OSAMCDE
					if not equal
						move	Z3,OSAMCDE
						setitem	Nord001bEditSample,0,""
					endif
.END PATCH 3.72.7 REPLACED LOGIC
				else			.CANCEL
					move	YES,ReturnFlag
					call	OrderSwitchTab using C2
					setfocus Nord001bComboSam
					return
				endif
			else
				move	C1,OSCODE
				unpack	str55,OSAMCDE
				type	OSAMCDE
				if not equal
					alert	caution,"Invalid Sample, I will	clear the field!!",result
					move	B1,OSCODE
					move	Z3,OSAMCDE
					setitem	Nord001bEditSample,0,""
					setitem	Nord001bComboSam,0,1
				endif
			endif
		else
			if (N2 = 4)		.Sample Previously Cleared
				move	C3,OSCODE
				unpack	str55,OSAMCDE
				call	Trim using OSAMCDE
				if (OSAMCDE <> "")
					type	OSAMCDE
					if not equal
						alert	caution,"Invalid Sample, I will	clear the field!!",result
						move	Z3,OSAMCDE
						setitem	Nord001bEditSample,0,""
					endif
				else
					move	Z3,OSAMCDE
					setitem	Nord001bEditSample,0,""
				endif
			else
				if (N2 <= 1)
					move	B1,OSCODE
				elseif (N2 = 3)
					move	C2,OSCODE
				endif
				move	Z3,OSAMCDE
				setitem	Nord001bEditSample,0,""
			endif
		endif
	endif
.END PATCH 3.72.2 REPLACED LOGIC

.VERIFY	NIN GUARANTY
.NOTICE	THAT NULL WOULD	REFER TO C5, BUT TO KEEP COMBO BOX CLEAN I PLACED IT AS	ITEM #1
.Following logic pulled	from old code.	If you are in pending mode you should not even
.be able to modify the object
	if (mod	= 5)	   .pending order add
.		 move	 b1 to guarcode
.		 setitem Nord001bComboNINGuar,0,1
	else
		move	YES,GuarFlag		.DEFAULT
		getitem	Nord001bComboNINGuar,0,N2
		if (N2 = 2)
			move	C1,GUARCODE
		elseif (N2 = 3)
			move	C2,GUARCODE
		elseif (N2 = 4)
			move	C3,GUARCODE
		elseif (N2 = 5)
			move	C4,GUARCODE
		elseif (N2 = 6)
			move	C6,GUARCODE
		elseif (N2 = 7)
			move	C7,GUARCODE
		elseif (N2 = 8)
			move	C8,GUARCODE
		elseif (N2 = 9)
			move	C9,GUARCODE
		else
			move	B1,GUARCODE
			move	NO,GuarFlag
		endif
	endif
.Only send out letter of Guarantee if:
.	Valid Guarantee	code AND (GUARCODE changed OR OPPPM changed OR GUARNAME	Changed	OR New Order)
.
.START PATCH 3.6 REPLACED LOGIC
.	if (GuarFlag = YES AND (mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))))
	if (GuarFlag = YES AND (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))))
.END PATCH 3.6 REPLACED LOGIC
		if (GuarFlag2 =	YES OR NewFlag = YES)
			getitem	Nord001bEditAttn,0,GUARNAME
			call	Trim using GUARNAME
.START PATCH 3.71 ADDED LOGIC
			getitem	Nord001bEditFax,0,GUARFAX
			call	Trim using GUARFAX
.END PATCH 3.71 ADDED LOGIC
			call	OrderGuarLetter
		endif
	endif
.VERIFY	BRK GUARANTY
.START PATCH 3.71.6 REPLACED LOGIC
.	if (mod	= 5)	   .pending order add
..		 move	 B1,OBRKGUAR
..		 setitem Nord001bComboBRKGuar,0,1
.	else
.		getitem	Nord001bComboBRKGuar,0,N2
.		if (N2 <= 1)
.			move	B1,OBRKGUAR
.		elseif (N2 = 2)
.			move	C1,OBRKGUAR
.		elseif (N2 = 3)
.			move	C2,OBRKGUAR
.		elseif (N2 = 4)
.			move	C3,OBRKGUAR
.		elseif (N2 = 5)
.			move	C4,OBRKGUAR
.		endif
.	endif
................................
	getitem	Nord001bComboBRKGuar,0,N2
	if (N2 <= 1)
		move	B1,OBRKGUAR
	elseif (N2 = 2)
		move	C1,OBRKGUAR
	elseif (N2 = 3)
		move	C2,OBRKGUAR
	elseif (N2 = 4)
		move	C3,OBRKGUAR
	elseif (N2 = 5)
		move	C4,OBRKGUAR
	endif
.END PATCH 3.71.6 REPLACED LOGIC
.VERIFY	KEY INFO & ZIP SCREEN
	pack	str2,OSALES10,OSALES
	getitem	Nord001bEditKeyInfo,0,OMLRKY
	call	Trim using OMLRKY
	if (str2 <> "06" AND mod <> C5 AND (mod	<> C6 OR (mod =	6 AND NORD4STAT	= "08"))) .Pending
.START PATCH 3.6 REPLACED LOGIC
.		if (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) .LCR
		if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) .LCR
.END PATCH 3.6 REPLACED LOGIC
.START PATCH 10-26-01 ADDED FOR	JD - ASH
.			 if (OMLRKY = "" AND ORTNNUM = "0040")
.START PATCH 3.77.3 REPLACED LOGIC
.			if (OMLRKY = ""	AND (ORTNNUM = "0040" |	ORTNNUM	= "5224"))
.			if (OMLRKY = ""	AND (ORTNNUM = "0040" |	ORTNNUM	= "5224" | ORTNNUM = "5318"))
.			if (OMLRKY = ""	AND (ORTNNUM = "0040" |	ORTNNUM	= "5224" | ORTNNUM = "5318" | ORTNNUM = "5316"))
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
			if (OMLRKY = ""	AND (ORTNNUM = "0040" |	ORTNNUM	= "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319"))
.End PATCH 3.77.5 REPLACED LOGIC
.END PATCH 10-26-01 ADDED FOR JD - ASH
				alert	caution,"Mailer	Key Must Be Entered!",result
				call	OrderSwitchTab using C2
				setfocus Nord001bEditKeyInfo
				move	"Y",ReturnFlag
				return
			endif
		endif
	endif
.START PATCH 3.76.7 REPLACED LOGIC
.	if (KeyFlag = YES AND mod <> C5	AND (mod <> C6 OR (mod = 6 AND NORD4STAT = "08")))  .Pending Orders should not write to	TDMC LOL file
	call	Trim using OMLRKYHold
	if (OMLRKY <> OMLRKYHold)
		if (mod <> C5 AND (mod <> C6 OR (mod = 6 AND NORD4STAT = "08")))  .Pending Orders should not write to	TDMC LOL file
.END PATCH 3.76.7 REPLACED LOGIC
.START PATCH 3.6 REPLACED LOGIC
.		if (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) .LCR
			if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) .LCR
.END PATCH 3.6 REPLACED LOGIC
				move	YES,lolsw
				move	"R",loltype
			endif
		endif
.START PATCH 3.76.7 ADDED LOGIC
	endif
.END PATCH 3.76.7 ADDED LOGIC
	getitem	Nord001bComboZipScreen,0,N2
	if (N2 = C1)
		move	B1,OCOMSLCT
	elseif (N2 = C2)
		move	"C",OCOMSLCT
	elseif (N2 = C3)
	       move    "L",OCOMSLCT
	else   .N2 = C4
	       move    "I",OCOMSLCT
	endif

.VERIFY	BILL DIRECT
.START PATCH ADDED 02/04/2002
	if (NewFlag = YES)
		call	OrderSetObildrct using C1
	endif
.END PATCH ADDED 02/04/2002
	getitem	Nord001bCheckDirect,0,result
	if (result = 1)
		move	YES,OBILDRCT
	else
		move	NO,OBILDRCT
	endif
.Patch added 9/11/2001 as per DH - ASH
	if (OSALES10 = "0" & OSALES = "6")
		move	NO,OBILDRCT
.Patch added to locate problems where Bill Direct Code is inappropriately set 1/6/05 - ASH
	elseif (OBILDRCT <> YES & MBILDRCT = YES)
		pack	taskname,"The Mailers Bill Direct Code is set to 'Y'!",newline,"Do you want this record's Bill Direct Code to reflect the Mailer?"
		alert	plain,taskname,result
		if (result = 1)
			move	YES,OBILDRCT
		elseif (result = 2)	.No
.Do nothing, leave it alone
		elseif (result = 3)	.Cancel
			call	OrderSwitchTab using C2
			setfocus Nord001bCheckDirect
			move	"Y",ReturnFlag
			return
		endif
.End of Patch added to locate problems where Bill Direct Code is inappropriately set 1/6/05 - ASH
	endif
.End of	Patch

.VERIFY	TEST/RENT
....Screen for Pending...
.	 move	 B1,OTOCODE
..	  if (mod <> 5)	.Currently (mod	= C6) will allow modification and so might call	subroutines below
.	 getitem Nord001ACheckTest,0,N1
..New logic for	Pending	orders
..	  if (mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04")))
.	 if ((mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04"))) & (mod	<> 5 AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08"))))
.		 if (N1	= C0)
.			 move	 B1,OTOCODE
.		 else
.			 move	 C1,OTOCODE
.		 endif
.	 else
.		 getitem Nord001ACheckRent,0,N1
.		 if (N1	= C0)
.			 getitem Nord001ACheckExchange,0,N8
.			 if (N8	= 0)
.				 alert	 caution,"Rental? Exchange? Both?",result
.				 call	 OrderSwitchTab	using C1
.				 setfocus Nord001ACheckExchange
.				 move	 "Y",ReturnFlag
.				 return
.			 endif
.			 move	 B1,OTOCODE
.		 else
.			 move	 "R",OTOCODE
.		 endif
.	  endif
...............
RENT2
...Screen for Pending...
	getitem	Nord001ACheckTest,0,N1
.New logic for Pending orders
..	  if (mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04")))
.	 if ((mod <> 3 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT = "04"))) & (mod	<> 5 AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08"))))
	if (N1 = C0)
		getitem	Nord001ACheckReTest,0,N1
		if (N1 = C0)
			move	B1,OTOCODE
		else
			move	C2,OTOCODE
		endif
	else
		move	C1,OTOCODE
	endif
	getitem	Nord001ACheckRent,0,N1
	if (N1 = C0)
		getitem	Nord001ACheckExchange,0,N8
		if (N8 = 0)
			if (mod	= 3 OR mod = 5 OR (mod = 7 AND NORD5STAT <> "04") OR (mod = 6 AND NORD4STAT <> "08"))
				alert	caution,"Rental? Exchange? Both?",result
				call	OrderSwitchTab using C1
				setfocus Nord001ACheckExchange
				move	"Y",ReturnFlag
				return
			endif
		endif
		move	B1,ORENT
	else
		move	C1,ORENT
	endif
.Nord001ACheckTest, Nord001ACheckRetest & Nord001ACheckEntire	are mutually exclusive so if OTOCODE >=	C1 then
.Nord001ACheckEntire will	always be unchecked!!!
.VERIFY	RENTAL/EXCHANGE
	getitem	Nord001ACheckEntire,0,N9
	getitem	Nord001ACheckExchange,0,N8
.START PATCH 3.68.7 MOVED LOGIC - ADDED POSSIBLE RETURN
	if (N8 = 0)	.Rental
		getitem	Nord001AEditOrderQty,0,str11
		call	RemoveChar using str11,COMMA
		call	Trim using str11
		move	str11,N10
		if (N10 = C0)
		else
			move	C0,N2
			move	ONETPER,N2
			if (N2 = C0 AND N10 >= 50000)
				if (mod = 1 | mod = 2 | (mod = 7 AND NORD5STAT = "04") | (mod = 6 AND NORD4STAT = "08"))
					alert	plain,"No Net on Order! Is this OK?",result
					if (result <> 1)
						call	OrderSwitchTab using C2
						setfocus Nord001bEditNetPercent
						move	"Y",ReturnFlag
						return
					endif
				endif
			endif
		endif
	endif
.END PATCH 3.68.7 MOVED LOGIC - ADDED POSSIBLE RETURN
.START PATCH 10-10-01
.Flag for Rent to Exchange
	if (mod	= 2)
		if (N8 = 1 AND (OELCODE	= "1" OR OELCODE = B1))
			move	YES,Rent2ExFlag
		else
			clear	Rent2ExFlag
		endif
	endif
.END PATCH 10-10-01
.Do not	permit Exchange	to Rental without Password
	if (N8 <> C1 & (OELCODE	= "2" |	OELCODE	= "3"))
.		 if (mod = 2 | (mod = 7	AND NORD5STAT =	"04") |	(mod = 6 AND NORD4STAT = "08"))
.		 if (mod = 2 & HoldFlag	< C2)
		if (mod	= 2)
			if (HoldFlag < C2)
				setitem	PasswordStatMssg1,0,"		 Exchange To Rental!!!"
				setprop	PasswordStatMssg1,visible=1
				setitem	PasswordEdit,0,""
				setfocus PasswordEdit
				move	"E",progcode
				setprop	Passwrd,visible=1
				if (PassFlag = NO)
					call	OrderSwitchTab using C1
					setfocus Nord001ACheckExchange
					move	"Y",ReturnFlag
					return
				else
					move	C2,HoldFlag
				endif
			endif
.This is new logic to allow dynamic deletion of	Exchange accounts when the only
.record	has been changed to a Rental.  ASH  07/07/2000
.Stolen	from OrderCancelOrder routine
			move	C2,NXCHPATH
			pack	NXCHFLD2,OLRN
			rep	zfill,NXCHFLD2
			move	"Ver.Rental-NXCHKEY",Location
			pack	KeyLocation,"Key: ",NXCHFLD2
			call	NXCHKEY
			if over
				goto RentEnd
			endif
			pack	str8,OODTEC,OODTEY,OODTEM,OODTED
			match	str8,DAT
			if not equal
				move	C2,NXCHPATH
				move	"Ver.Rental-NXCHKS",Location
				pack	KeyLocation,"Key: ",NXCHFLD2
				loop
					call	NXCHKS
					goto RentEnd if	over
					match	OLRN,LR
					goto RentEnd if	not equal    *NOT SAME ORDER
					match	str8,DAT	  *SAME	ORDER?
					until equal
				repeat
			endif
.START PATCH 3.76 REPLACED LOGIC
.			unpack	EXKEY,str4,str3,str1,str5
.			move	str5,SENTRY
.			pack	NXNGFLD1,AKEY1A,str4
.			pack	NXNGFLD2,AKEY2A,str3,str1
..			 pack	 ACKEY,str4,str3,str1
............................
			unpack	EXKEY,str6,str6a,str5
			move	str5,SENTRY
			pack	NXNGFLD1,AKEY1A,str6
			pack	NXNGFLD2,AKEY2A,str6a
.END PATCH 3.76 REPLACED LOGIC
			move	"Ver.Rental-NXNGAIM",Location
			pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
			call	NXNGAIM
			goto RentEnd if	over
			compare	C1,ENTRY		*IS THIS LR THE	ONLY ONE?
			if equal
				clear	taskname
				append	"This is the only Exchange for these",taskname
				append	carr,taskname
				append	"Clients.  May it be Deleted?",taskname
				reset	taskname
				alert	plain,taskname,result
				if (result = 1)
.					 unpack	 EXKEY,str4,str5
.					 pack	 NXNGFLD1,AKEY1A,str4
.					 pack	 NXNGFLD2,AKEY2A,str5
.					 move	 "Ver.Rental-NXNGTST",Location
.					 pack	 KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
.					 call	 NXNGTST
					move	"Ver.Rental-NXNGDEL",Location
					pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
					call	NXNGDEL
					move	C1,NXCHPATH
					move	EXKEY,NXCHFLD1
					move	"Ver.Rental-NXCHTST",Location
					pack	KeyLocation,"Key: ",NXCHFLD1
					call	NXCHTST
					move	"Ver.Rental-NXCHDEL",Location
					pack	KeyLocation,"Key: ",NXCHFLD1
					call	NXCHDEL
.START PATCH 3.76 REMOVED LOGIC
.					deletek	NXCHFLE2,NXCHFLD2
.END PATCH 3.76 REMOVED LOGIC
.Delete	Beginning Balance record
					move	C1,NXCHPATH
.START PATCH 3.76 REPLACED LOGIC
.					unpack	EXKEY,str8
.					pack	NXCHFLD1,str8,"00000"
					unpack	EXKEY,str12
					pack	NXCHFLD1,str12,"00000"
.END PATCH 3.76 REPLACED LOGIC
					move	"Ver.Rental-NXCHTST,2",Location
					pack	KeyLocation,"Key: ",NXCHFLD1
					call	NXCHTST
					move	"Ver.Rental-NXCHDEL,2",Location
					pack	KeyLocation,"Key: ",NXCHFLD1
					call	NXCHDEL
					alert	note,"Exchange Status is Deleted",result
				else
					goto RentUpdateExchange
				endif
			else	.Dynamically go	and change the Exchange	File
RentUpdateExchange
				if (STAT <> "C"	AND STAT <> "R"	AND STAT <> "X")
.Update	record in question to a	Rental
					move	QTY,N9
					if (ENTRY = SENTRY)
.If record in question IS last record then update Qty fields as	well
						if (MLRSW = "1")
							sub	N9,USAGE1
						else
							sub	N9,USAGE2
						endif
					else
.Branch	added 9-24-2001	in order to save MLRSW value before final record is read and updated.  ASH
						move	MLRSW,str1
					endif
					move	"R",STAT
					move	C2,NXCHPATH
					move	"Ver.Rental-NXCHUPD",Location
					pack	KeyLocation,"Key: ",NXCHFLD2
					call	NXCHUPD
					IF (LRINIT = 1)
					move	"NINXCH	- Update,RentUpdateExchange",str45
					call	OrderWriteLRFile using str45
					ENDIF
					if (ENTRY <> SENTRY)
.If record in question is NOT last record then update last Exchange record to reflect new Quantity
						unpack	EXKEY,ACCKEY
						pack	NXCHFLD1,ACCKEY,ENTRY
						rep	zfill,NXCHFLD1
						move	C1,NXCHPATH
						move	"Ver.Rental-NXCHKEY,2",Location
						pack	KeyLocation,"Key: ",NXCHFLD1
						call	NXCHKEY
						if not over
.START PATCH ADDED 9-24-01  ASH
.							 if (MLRSW = "1")
.								 sub	 N9,USAGE1
.							 else
.								 sub	 N9,USAGE2
.							 endif
.MLRSW has been	refreshed.  We want value from record associated with the LR we	are updating.  ASH
							if (str1 = "1")
								sub	N9,USAGE1
							else
								sub	N9,USAGE2
							endif
.END PATCH ADDED 9-24-01  ASH
							move	"Ver.Rental-NXCHUPD,2",Location
							call	NXCHUPD
							IF (LRINIT = 1)
							move	"NINXCH	- 2,Update,RentUpdateExchange",str45
							call	OrderWriteLRFile using str45
							ENDIF
						endif
					endif
				endif
			endif
		endif
RentEnd
.........................
	endif
	move	B1,OSOTCODE
	move	B1,OELCODE
	if (N8 = C1)	.YES for Exchange
.New logic for Pending orders
.		 if (mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))
.START PATCH 3.6 REPLACED LOGIC
.		if ((mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	& (mod <> 5 AND	(mod <>	6 OR (mod = 6 AND NORD4STAT = "08"))))
		if ((mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04"))) & (mod <> 5 AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08"))))
.END PATCH 3.6 REPLACED LOGIC
			clear	NXRFFLD2
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.			move	OMLRNUM,NXRFFLD2
			pack	COMPFLD3,OMLRNUM
			move	"O.VerifyData-COMPKEY3",Location
			pack	KeyLocation,"Key: ",COMPFLD3
			call	COMPKEY3
			move	COMPNUM,NXRFFLD2
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
			move	C2,NXRFPATH
			move	"O.VerifyData-NXRFKEY",Location
			pack	KeyLocation,"Key: ",NXRFFLD2
			call	NXRFKEY
			if over
				clear	taskname
				append	"There is no Cross Reference Record",taskname
				append	carr,taskname
				append	"for Mailer ## ",taskname
				append	OMLRNUM,taskname
				append	" and List ## ",taskname
				append	OLNUM,taskname
				reset	taskname
				alert	caution,taskname,result
				call	OrderSwitchTab using C1
				setfocus Nord001ACheckExchange
				move	"Y",ReturnFlag
				return
			endif
		endif
		if (N9 = C1)
			move	C3,OELCODE
		else
			move	C2,OELCODE
		endif
.Make sure flag	is set so that OrderExchangeStatus2 will be called!!!
		if ((mod = 6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))
			move	yes,QtyFlag
		endif
		call	OrderExchangeStatus
		if (ReturnFlag = YES)
			call	OrderSwitchTab using C1
			setfocus Nord001ACheckExchange
			return
		endif
	else
.Ensure	XSTAT is calculated if LCR or Pending!!!!
.This is a change applied to Pending logic as of 11/16/1999 - ASH
.		 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
		if ((mod = 5 OR	(mod = 6 AND NORD4STAT <> "08")) | (mod	= 3 OR (mod = 7	AND NORD5STAT <> "04")))
.You need to bypass message that will prevent some Mailers from	being Exchanges	as ALL
.LCR's and Pending Orders need to come here
			call	OrderExchangeStatus
		endif
		if (N9 = C1)
			move	C1,OELCODE
		else
			move	B1,OELCODE
		endif
.Not a split
		setitem	Nord001AEditExchangeQty,0,""
		setitem	Nord001AEditExchangePrice,0,""
	endif
.	 endif

.VERIFY TEST CODE SELECTION
.START PATCH 3.47 ADDED LOGIC PER IDEA OF THE MONTH
	getitem Nord001bComboTestCode,0,N1
	if (N1 > 0)
		sub	C1,N1
	endif
	if (N1 = 0)
		clear	OSOTCODE
	else
		move	N1,OSOTCODE
	endif
.END PATCH 3.47 ADDED LOGIC PER IDEA OF THE MONTH
.VERIFY	ORDER/EXCHANGE QUANTITY
	getitem	Nord001AEditOrderQty,0,str11
	call	RemoveChar using str11,COMMA
	call	Trim using str11
	move	str11,OQTY
	call	ZFILLIT	using OQTY
	if (OQTY = "")
		move	"000000000",OQTY
	endif
.START PATCH 3.76.7 ADDED LOGIC
	move	C0,N9
	move	OQTY,N9
	if (N9 <> OQTYHold)
		move	YES,QtyFlag
	endif
.END PATCH 3.76.7 ADDED LOGIC
	getitem	Nord001AEditExchangeQty,0,STR11
	call	RemoveChar using str11,COMMA
	call	Trim using str11
	move	str11,OEXQTY
	call	ZFILLIT	using OEXQTY
	move	OEXQTY,N9
	if (N9 = 0)
		move	B1,OEXQTY
	endif
.STOP IF OQTY >	UNIVERSE
.START PATCH 3.72 REPLACED LOGIC
.	if (OQTY > UNIVERSE)
	move	C0,howmany
	move	OQTY,howmany
	move	C0,N10
	call	Trim using UNIVERSE
	move	UNIVERSE,N10
	if (howmany > N10)
.END PATCH 3.72 REPLACED LOGIC
.
.		 getprop OrderInfo,visible=N1
.		 if (N1	= C1)
.			 call	 OrderInfoClose
.		 endif
.		 setitem OrderInfoStatText1,0,""
.		 setitem OrderInfoStatText2,0,""
.		 setitem OrderInfoStatText3,0,"Quantity	is Greater than	Universe!"
.		 setitem OrderInfoStatText4,0,""
.		 setitem OrderInfoStatText5,0,""
.		 setprop OrderInfo,title="Update Information"
.		 setprop OrderInfo,visible=1
..		  setitem timer2,0,10	  .reset to 1 second
.		 pause	 "1"
.		 call	 OrderInfoClose
		pack	str45,"Update Information"
		pack	str55,"Quantity	is Greater than	Universe!"
		clear	str1
		call	OrderDisplayMessage using Nord0001,str45,str1,str1,str55,str1,str1,C0,C0,C0,C0
		pause	"1"
		call	OrderInfoClose
.
	endif
.LCRs do not have minimum calculated!!
.NEW LOGIC FOR PENDINGS
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
	if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
.		 if (QtyFlag = YES AND (OELCODE	= "2" OR OELCODE = "3"))
			call	OrderExchangeStatus2
			if (ReturnFlag = YES)
				call	OrderSwitchTab using C1
				setfocus Nord001ACheckExchange
				return
			endif
.		 endif
	elseif (QtyFlag	= YES)
		clear	str9
		scan	"$",MIN
		goto K21D if equal
		reset	MIN
		lenset	MIN
		move	C1,N2
LOOP		reset	MIN,N2
		type	MIN
		call	APPMIN if equal
		compare	"11",N2
		goto DONE if equal
		add	C1,N2
		goto LOOP
APPMIN		cmatch	B1,MIN
		return if equal
		append	MIN,str9
		return
DONE		reset	str9
		reset	MIN
		move	C0,BLANK9
		move	str9,BLANK9
		compare	C0,BLANK9
		goto K21N if equal
		move	OQTY,SQTY
		compare	C0,SQTY	    ORDER QTY PRESENT?
		goto K21D if equal	NO!
		cmatch	"C",ELSTCDE	EXCLUSIVE LIST???
		goto EXCLCHK if	equal	 YES!
		compare	SQTY,BLANK9	NO, CHECK FOR MEETING MIN!
		goto K21N if equal     OK!
		goto K21N if less	OK!
		goto BELMIN	   *****NO GOOD****
EXCLCHK
		compare	"5000",BLANK9	 MIN 5M?
		goto EXCLCHK1 if equal	  YES
		goto EXCLCHK1 if less	  MIN BELOW 5M
.						NO
		move	"5000",BLANK9	 EXCLUSIVE USE 5M AS MIN.
EXCLCHK1	compare	SQTY,BLANK9	 CHECK FOR MEETING MIN!
		goto K21N if equal     OK!
		goto K21N if less	OK!
BELMIN
.Skip Below Minimum section entirely as	per Managers Meeting 11-17-1999
		goto k21N
.
.NEW LOGIC FOR PENDINGS
.		 if (mod = 5 OR	(mod = C6 AND NORD4STAT	<> "08"))
.			 goto k21n
.		 endif
		if (NewFlag = NO AND QtyFlag = NO)
			goto k21n
		endif
		create	ErrorMssg;EditTextBoxes(1)=100:120:10:100,MaxChars=7,EditType=5,SelectAll=1,Style=1,Border=1,Password=1
		activate EditTextBoxes(1)
		setprop	ErrorMssgStat1,visible=0
		setprop	ErrorMssgStat2,visible=1
		setprop	ErrorMssgStat3,visible=0
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
		setitem	ErrorMssgStat2,0,"    Below Minimum!"
		setitem	ErrorMssgStat5,0,"    Enter Password"
		setitem	ErrorMssgOK,0,"O&K"
		setfocus EditTextBoxes(1)
		setprop	ErrorMssg,visible=1
		getitem	EditTextBoxes(1),0,str7
		destroy	EditTextBoxes(1)
		if (str7 <> "MINPASS")
			alert	caution,"Incorrect Password!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AEditOrderQty
			move	"Y",ReturnFlag
			return
		endif
.check net min
k21N		move	C0,N2
		match	"NN",ONETPER	.net/net?
		goto K21NQ if equal	.yes check min.
		move	ONETPER,N2
		compare	C0,N2			 .is order marked net?
		goto K21D if equal		 .no
k21nq		compare	SQTY,ONETMIN
		goto K21D if equal     OK!
		goto K21D if less	OK!
		clear	taskname
		append	"Below Net Minimum!  ",taskname
.START PATCH 3.71.4 REPLACED LOGIC
.		append	BLANK9,taskname
		move	ONETMIN,str9
		call	FormatNumeric using str9,str11
		append	str11,taskname
.END PATCH 3.71.4 REPLACED LOGIC
		append	carr,taskname
		append	"Was Net Minimum Waived?",taskname
		reset	taskname
		alert	plain,taskname,result
		if	(result	= 1)
			move	c0,onetmin
		else
			move	yes,returnflag
			call	OrderSwitchTab using C1
			setfocus Nord001AEditOrderQty
			return
		endif
K21D		move	C0,SQTY
		move	OQTY,SQTY
		compare	SQTY,"999999"
		if less
			setprop	Nord001AEditOrderQty,fgcolor=RED
		else
			setprop	Nord001AEditOrderQty,fgcolor=BLACK
		endif
.VERIFY	EXCHANGE
		if (mod	<> C5 AND (mod <> C6 OR	(mod = 6 AND NORD4STAT = "08")))
.START PATCH 3.6 REPLACED LOGIC
.			if (mod	<> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
			if (mod <> 8 AND mod <> 3 AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")))
.END PATCH 3.6 REPLACED LOGIC
				move	YES,lolsw		      *flag tdmc lol file.
				move	"Q",loltype
			endif
		endif
		if (OELCODE = "1" OR OELCODE = B1)	 .NOT AN EXCHANGE ORDER
			clear	OEXQTY
		elseif (OELCODE	= "2" OR OELCODE = "3")
			call	OrderExchangeStatus2
			if (ReturnFlag = YES)
				call	OrderSwitchTab using C1
				setfocus Nord001AEditExchangeQty
				return
			endif
		endif
	endif
.Following needs to remain a label as OrderExchangeStatus2 uses	it
VERIFYPRICE
.START PATCH 3.72 REPLACED LOGIC
.	getitem	Nord001AEditPrice,0,str6
.	unpack	str6,str3,str1,str2
.	pack	OPPM,str3,str2
	clear	OPPM
.END PATCH 3.72 REPLACED LOGIC
.
	getitem	Nord001AEditExchangePrice,0,str6
	unpack	str6,str3,str1,str2
	pack	OXPPM,str3,str2
.VERIFY NET QTY
	move	C0,N9
	move	OQTY,N9
	if (N9 = C0)
		clear	ONETQTY
	else
		getitem	Nord001bEditNetQty,0,str11
		call	RemoveChar using str11,COMMA
		call	Trim using str11
		move	str11,ONETQTY
		call	ZFILLIT	using ONETQTY
	endif
.This field needs to be	verified last!!
.VERIFY OSTAT
.Added 11/15/02	as per Idea of the Month - ASH
	move	C0,MaskSubStat
.End Add
	clear	str55
	getitem	Nord001AComboStatus,0,N2
	if (N2 <= 1)
		 alert	 caution,"Please Select	Order Status!!",result
		 call	 OrderSwitchTab	using C1
		 setfocus Nord001AComboStatus
		 move	 "Y",ReturnFlag
		 return
	elseif (N2 = 2)		.LCR
		move	"l",OSTAT
		getitem	Nord001AComboPending,0,N3
.Added 11/15/02	as per Idea of the Month - ASH
		move	N3,MaskSubStat
.End Add
		if (OCO2CODE <>	"" AND OCO2CODE	<> "  ")
."e" = No Caller Approval, "E" = Caller	Approval, "*" =	Faxed To Owner,	"z" = Denied,Cancelled
			if (NewFlag = YES)
				move	"e",OHIST
			elseif (N3 = 3 | N3 = 4)    .3 = 2nd Request, 4	= Revised Request
				sub	C1,N3,N4
				move	OLRN,NORD5FLD
				move	"V.OSTAT-NORD5KEY",Location
				pack	KeyLocation,"Key: ",NORD5FLD
				call	NORD5KEY
				if not over
					move	NORD5STAT,N5
					if (N5 <> N4)
						move	"e",OHIST
						move	" ",OBRKRPT
					endif
				else	.This should never happen
					move	"e",OHIST
				endif
			else
				if (OHIST <> "E" AND OHIST <> "*" AND OHIST <> "z")
					move	"e",OHIST
				endif
			endif
		else
			if (NewFlag = YES)
				move	"e",OHIST
				clear	OCLRSTAT
				clear	OCLRINIT
				clear	OCLRDTEC
				clear	OCLRDTEY
				clear	OCLRDTEM
				clear	OCLRDTED
			elseif (N3 = 3 | N3 = 4)    .3 = 2nd Request, 4	= Revised Request
				sub	C1,N3,N4
				move	OLRN,NORD5FLD
				move	"V.OSTAT-NORD5KEY",Location
				pack	KeyLocation,"Key: ",NORD5FLD
				call	NORD5KEY
				if not over
					move	NORD5STAT,N5
					if (N5 <> N4)
						move	"e",OHIST
					endif
				else	.This should never happen
					move	"e",OHIST
				endif
			else
				if (OHIST <> "E" AND OHIST <> "*" AND OHIST <> "z")
					move	"e",OHIST
				endif
			endif
		endif
.I cannot subtract here	as I would have	no way of determining if no selection was made!!
		if (N3 = 5)    .Approved - make	a live Order!!
.			 if (OHIST = "e" | OHIST = "z")
.				 clear	 taskname
.				 append	 "This is an LCR with an Exclusive List",taskname
.				 append	 carr,taskname
.				 append	 "which	has not	been approved by the Caller!",taskname
.				 append	 carr,taskname
.				 append	 "You cannot turn this LCR into	a Live Order!!",taskname
.				 reset	 taskname
.				 alert	 caution,taskname,result
.				 call	 OrderSwitchTab	using C1
.				 setfocus Nord001AComboPending
.				 move	 "Y",ReturnFlag
.				 return
.			 endif
.START PATCH 3.71.2 ADDED LOGIC
			if (NewFlag <> YES)
.END PATCH 3.71.2 ADDED LOGIC
.Place pointer on appropriate record
				move	C1,NORDPPATH
				move	OLRN,NORDPFLD
				move	"Verify'Live'-NORDPTST",Location
				pack	KeyLocation,"Key: ",NORDPFLD
				call	NORDPTST
				if not over
					move	"Verify'Live'-NORDPDEL",Location
					pack	KeyLocation,"Key: ",NORDPFLD
					call	NORDPDEL
				endif
.START PATCH 3.71.2 ADDED LOGIC
			endif
.END PATCH 3.71.2 ADDED LOGIC
			if (OHIST = "E")
				move	"L",OHIST
			else
				move	"l",OHIST
			endif
			move	"0",OSTAT	.make order live
			move	"S",ORPCODE	.make order get	sent, used by NINPRINT.DAT
			pack	str55,"Live Order"
			setitem	Nord001AComboStatus,0,5
			setitem	Nord001AComboPending,0,1
.Reset Order Date once turned into Live	Order
			unpack	timestamp,OODTEC,OODTEY,OODTEM,OODTED
			pack	newdate1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
			setitem	Nord001AEditOrderDate,0,newdate1
.START PATCH 10-16-2001	BUG FIX	WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
.Should	never occur!
			call	OrderVerifyReturnDate
.END PATCH 10-16-2001 BUG FIX WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
		endif
		if (N3 <= 1)
			alert	caution,"Please	Select LCR Status!!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AComboPending
			move	"Y",ReturnFlag
			return
		else
.Subtraction must occur	here
			sub	C1,N3
			move	N3,str3
			bump	str3
			rep	zfill,str3
.START PATCH 3.71.8.1 REMOVED LOGIC
.			move	str3,NORD5STAT
.END PATCH 3.71.8.1 REMOVED LOGIC
			if (mod	= 7)	.LCR modify mode
.START PATCH 3.71.8.1 REPLACED LOGIC
.				move	"UpdateNord5-NORD5TST",Location
.				pack	KeyLocation,"Key: ",NORD5FLD
.				call	NORD5TST
				move	OLRN,NORD5FLD
				move	"UpdateNord5-NORD5KEY",Location
				pack	KeyLocation,"Key: ",NORD5FLD
				call	NORD5KEY
.END PATCH 3.71.8.1 REPLACED LOGIC
				if over
					move	"UpdateNord5-NORD5WRT",Location
.ADDED 07AUG01
					unpack	timestamp,NORD5PDTE
.START PATCH 3.71.8.1 ADDED LOGIC
					move	str3,NORD5STAT
.END PATCH 3.71.8.1 ADDED LOGIC
					move	OLRN,NORD5LR
					call	NORD5WRT
				else
					move	"UpdateNord5-NORD5UPD",Location
.START PATCH 3.71.8.1 ADDED LOGIC
					move	str3,NORD5STAT
.END PATCH 3.71.8.1 ADDED LOGIC
					call	NORD5UPD
				endif
				IF (LRINIT = 1)
				move	"NINORD5 - 2,Update,VerifyData",str45
				call	OrderWriteLRFile using str45
				ENDIF
			endif
			if (N3 = 5 OR N3 = 7)	   .Cancelled/Denied LCR
.Delete	from NINPRINTL if Cancelled or if Denied AND it	is no longer In-House
.If Denied and record is In-House keep it in file until	report is run, then delete it.
				TRAP	IOMssg Giving Error if IO
				move	"VER.OSTAT-NINPRINT",Location
				pack	KeyLocation,"Key: ",holdkey
				filepi	3;ORDPRINT
				read	ORDPRINT,holdkey;;
				if not over
					delete	ORDPRINT,holdkey
				endif
.				 move	 "z",OSTAT
				TRAPCLR	IO
				if (N3 = 5)	.Cancelled should produce a different message
					move	"z",OSTAT
					if (OHIST = "")
						move	"l",OHIST
					else
						move	"L",OHIST
					endif
				else	.Denied
					move	"z",OHIST
				endif
				append	"*CANCELLED LCR*",str55
				reset	str55
				setprop	Nord001AComboPending,enabled=0,bgcolor=grey
			elseif (N3 <> 4)
				move	"l",OSTAT
				append	"*LCR* ",str55
				pack	NPNDFLD,OSTAT,str3
				move	"Verify	OSTAT-NPNDKEY",Location
				pack	KeyLocation,"Key: ",NPNDFLD
				call	NPNDKEY
				append	NPNDDESC,str55
				reset	str55
			endif
		endif
	elseif (N2 = 3)		.Pending Order
		getitem	Nord001AComboPending,0,N3
.Added 11/15/02	as per Idea of the Month - ASH
		move	N3,MaskSubStat
.End Add
		if (OCO2CODE <>	"" AND OCO2CODE	<> "  ")
."e" = No Caller Approval, "E" = Caller	Approval, "*" =	Faxed To Owner,	"z" = Denied,Cancelled
			if (NewFlag = YES)
				move	"e",OHIST
			elseif (N3 = 13	| N3 = 14)    .13 = 2nd	Request, 14 = Revised Request
				sub	C2,N3,N4
				move	OLRN,NORD4FLD
				move	"V.OSTAT-NORD4KEY",Location
				pack	KeyLocation,"Key: ",NORD4FLD
				call	NORD4KEY
				if not over
					move	NORD4STAT,N5
					if (N5 <> N4)
						move	"e",OHIST
						move	" ",OBRKRPT
					endif
				else	.This should never happen
					move	"e",OHIST
				endif
			else
				if (OHIST <> "E" AND OHIST <> "*" AND OHIST <> "z")
					move	"e",OHIST
 				endif
			endif
		else
			clear	OHIST
		endif
.I cannot subtract here	as I would have	no way of determining if no selection was made!!
		if (N3 = 10)	.Approved - make a live	Order!!
.			 if (OHIST = "e" | OHIST = "z")
.				 clear	 taskname
.				 append	 "This is a Pending LCR	with an	Exclusive List",taskname
.				 append	 carr,taskname
.				 append	 "which	has not	been approved by the Caller!",taskname
.				 append	 carr,taskname
.				 append	 "You cannot turn this LCR into	a Live Order!!",taskname
.				 reset	 taskname
.				 alert	 caution,taskname,result
.				 call	 OrderSwitchTab	using C1
.				 setfocus Nord001AComboPending
.				 move	 "Y",ReturnFlag
.				 return
.			 endif
.START PATCH 3.71.2 ADDED LOGIC
			if (NewFlag <> YES)
.END PATCH 3.71.2 ADDED LOGIC
.Place pointer on appropriate record
				move	C1,NORDPPATH
				move	OLRN,NORDPFLD
				move	"Verify'Live'-NORDPTST",Location
				pack	KeyLocation,"Key: ",NORDPFLD
				call	NORDPTST
				if not over
					move	"Verify'Live'-NORDPDEL",Location
					pack	KeyLocation,"Key: ",NORDPFLD
					call	NORDPDEL
				endif
.START PATCH 3.71.2 ADDED LOGIC
			endif
.END PATCH 3.71.2 ADDED LOGIC
			move	"p",OHIST
			move	"0",OSTAT	.make order live
			move	"S",ORPCODE	.make order get	sent, used by NINPRINT.DAT
			pack	str55,"Live Order"
			setitem	Nord001AComboStatus,0,5
			setitem	Nord001AComboPending,0,1
.Reset Order Date once turned into Live	Order
			unpack	timestamp,OODTEC,OODTEY,OODTEM,OODTED
			pack	newdate1,OODTEM,SLASH,OODTED,SLASH,OODTEC,OODTEY
			setitem	Nord001AEditOrderDate,0,newdate1
.START PATCH 10-16-2001	BUG FIX	WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
.Should	never occur!
			call	OrderVerifyReturnDate
.END PATCH 10-16-2001 BUG FIX WHEN APPROVING LCR/PENDING AFTER RETURN/MAIL DATE
		endif
		if (N3 <= 1)
			alert	caution,"Please	Select Pending Status!!",result
			call	OrderSwitchTab using C1
			setfocus Nord001AComboPending
			move	"Y",ReturnFlag
			return
		else
.Subtraction must occur	here
			sub	C2,N3
			move	N3,str3
			bump	str3
			rep	zfill,str3
.START PATCH 3.71.8.1 REMOVED LOGIC
.			move	str3,NORD4STAT
.END PATCH 3.71.8.1 REMOVED LOGIC
			if (mod	= 6)	.Pending modify	mode
				move	OLRN,NORD4FLD
.START PATCH 3.71.8.1 REPLACED LOGIC
.				move	"UpdateNord4-NORD4TST",Location
.				pack	KeyLocation,"Key: ",NORD4FLD
.				call	NORD4TST
				move	"UpdateNord4-NORD4KEY",Location
				pack	KeyLocation,"Key: ",NORD4FLD
				call	NORD4KEY
.END PATCH 3.71.8.1 REPLACED LOGIC
.This is a protective measure for List Managment LCR's that are	turned to Pending Orders
				if over
					move	"UpdateNord4-NORD4WRT",Location
					pack	KeyLocation,"Key: ",NORD4FLD
					unpack	timestamp,NORDPDTE
.START PATCH 3.71.8.1 ADDED LOGIC
					move	str3,NORD4STAT
.END PATCH 3.71.8.1 ADDED LOGIC
					move	OLRN,NORD4LR
					call	NORD4WRT
					IF (LRINIT = 1)
					move	"NINORD4 - Verify OSTAT",str45
					call	OrderWriteLRFile using str45
					ENDIF
				else
					move	"UpdateNord4-NORD4UPD",Location
.START PATCH 3.71.8.1 ADDED LOGIC
					move	str3,NORD4STAT
.END PATCH 3.71.8.1 ADDED LOGIC
					call	NORD4UPD
					IF (LRINIT = 1)
					move	"NINORD4 - Update, Verify OSTAT",str45
					call	OrderWriteLRFile using str45
					ENDIF
				endif
			endif
			if (N3 = 7 OR N3 = 6)		.Cancelled Pending & Denied Pending
				move	"x",OSTAT
				if (N3 = 7)		.Cancelled should produce a different message
					move	"p",OHIST
				else			.Denied
					move	"z",OHIST
				endif
				append	"*CANCELLED PENDING* ",str55
				reset	str55
				setprop	Nord001AComboPending,enabled=0,bgcolor=grey
			elseif (N3 <> 8)	.not approved
				move	"p",OSTAT
				append	"*PENDING* ",str55
				pack	NPNDFLD,OSTAT,str3
				move	"Verify	OSTAT-NPNDKEY",Location
				pack	KeyLocation,"Key: ",NPNDFLD
				call	NPNDKEY
				append	NPNDDESC,str55
				reset	str55
			endif
		endif
.START PATCH 3.6 ADDED LOGIC
	elseif (N2 = 9)		.Cancelled LCR
		move	"z",OSTAT
		pack	str55,"*CANCELLED LCR*"
.END PATCH 3.6 ADDED LOGIC
	elseif (N2 = 4)
		move	"x",OSTAT
		pack	str55,"*CANCELLED PENDING*"
	elseif (N2 = 5)
		move	"0",OSTAT
		pack	str55,"Live Order"
	elseif (N2 = 6)
		move	"B",OSTAT
		pack	str55,"Billed Order"
	elseif (N2 = 7)
		move	"Q",OSTAT
		pack	str55,"Cancelled-Billed"
	elseif (N2 = 8)
		move	"X",OSTAT
		pack	str55,"Cancelled Order"
	endif
	call	OrderLoadMessages
.WRITE TO NINORDC
	if (NewFlag <> YES AND OCAMP <>	HoldCamp)
		call	OrderMoveCampaign using	HoldCamp,OCAMP,OLRN,C0
.		 move	 C3,NORDLOCK
.		 move	 C4,NORDPATH
.		 clear	 NORDFLDC
.		 move	 HoldCamp,NORDFLDC
.		 if (NORDFLDC =	"")	 .Force	an Over	condition
.			 move	 "))))))",NORDFLDC
.		 endif
.		 move	 "VerifyData-NORDTST(4)",Location
.		 pack	 KeyLocation,"Key: ",NORDFLDC
.		 call	 NORDTST
.		 if over
.			 move	 OLRN,NORDFLD
.			 move	 C1,NORDPATH
.			 call	 NORDTST
.			 if not	over
.				 if (OCAMP <> "")
.					 move	 "VerifyD.-INSERT NORDFLE4",Location
.					 call	 NORDWRT4
.				 endif
.			 endif
.		 else
.			 if (NORDFLDC <> "")
.				 loop
.					 until over
.					 until (str6 = OLRN)
.					 move	 "VerifyData-NORDKS(4)",Location
.					 move	 C5,NORDPATH
.					 call	 NORDKS
.				 repeat
.				 if (str6 = OLRN)
.					 TRAP	 IOMssg	Giving Error if	IO
.					 filepi	 2;NORDFLE4
.					 move	 "Ver.D.-DELETEDK NORDFLE4",Location
.					 DELETEDK NORDFLE4,NORDFLDC
.					 trapclr IO
.				 endif
.			 endif
.			 if (OCAMP <> "")
.				 move	 "VerifyD.-INSERT NORDFLE4",Location
.				 call	 NORDWRT4
.			 endif
.		 endif
	endif

.VERIFY	LCR FIELDS
.This option is	only available if in FIXORD mode
	if (SecFlag = YES)
		getitem	Nord001bEditLCRHist,0,OHIST
		getitem	Nord001bEditLCRInit,0,OCLRINIT
		getitem	Nord001bEditLCRStat,0,OCLRSTAT
		getitem	Nord001bEditLCRDate,0,str10
		unpack	str10,OCLRDTEM,OCLRDTED,OCLRDTEC,OCLRDTEY
	endif
	return
...End of Verify...

OrderVerifyReturnDate
	call	Trim using ORTNDTEY
	if (ORTNDTEY = "")
		return
	endif
	move	ORTNDTEM,MM
	move	ORTNDTED,DD
	move	ORTNDTEY,YY
	call	CVTJUL
	if (JULDAYS = 0)
		return
	endif
	move	JULDAYS,howmany
.Order Date must be written to before this - ASH
	move	OODTEM,MM
	move	OODTED,DD
	move	OODTEY,YY
	call	CVTJUL
.START PATCH 3.77.9 REPLACED LOGIC
.	if (howmany < JULDAYS)
	if (howmany <> C0 and JULDAYS <> C0 and howmany < JULDAYS)
.END PATCH 3.77.9 REPLACED LOGIC
		alert	caution,"Return	Date Must be After Order Date!",result
		noreturn
		goto	OrderReturnRtnDate
	endif
	return
....Campaign Screens....

OrderVerifyCampaignData
.VERFIY	CAMPAIGN STATUS
	getitem	Nord01eaComboStatus,0,N1
	sub	C1,N1
	move	N1,NCMPSTAT
.VERIFY	CAMPAIGN NAME
	getitem	Nord01eaEditCampName,0,NCMPCNAME
	call	Trim using NCMPCNAME
	if (NCMPCNAME =	"")
		alert	caution,"Campaign Name Required!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaEditCampName
		move	"Y",ReturnFlag2
		return
	endif
.VERIFY	CAMPAIGN MAILER
.START PATCH 3.75.7 REPLACED LOGIC
.	getitem	Nord01eaEditMlr,0,str4
.	call	Trim using str4		.Nord001AEditMlr_LostFocus will allow null entry
.	count	N1,str4
.	if (N1 <> 4)
.		alert	caution,"4 Digit Mailer	Required!",result
.		call	OrderSwitchTab using C6
.		setfocus Nord01eaEditMlr
.		move	"Y",ReturnFlag2
.		return
.	endif
.	pack	MKEY,str4,"000"
.	move	"O.VerifyC.Data-NMLRKEY",Location
.	pack	KeyLocation,"Key: ",MKEY
.	call	NMLRKEY
.	if over
.		alert	caution,"Mailer	Not Found!",result
.		call	OrderSwitchTab using C6
.		setfocus Nord01eaEditMlr
.		move	"Y",ReturnFlag2
.		return
.	endif
.	move	str4,NCMPMLR
...........................................................
	getitem	Nord01eaEditMlr,0,str6
	call	Trim using str6		.Nord001AEditMlr_LostFocus will allow null entry
	count	N1,str6
	if (N1 <> 6)
		alert	caution,"6 Digit Mailer	Required!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaEditMlr
		move	"Y",ReturnFlag2
		return
	endif
	pack	COMPFLD,str6
	move	"O.VerifyC.Data-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if over
		alert	caution,"Mailer	Not Found!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaEditMlr
		move	"Y",ReturnFlag2
		return
	elseif (COMPMLRFLG <> "T")
		alert	caution,"Valid Mailer Required!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaEditMlr
		move	"Y",ReturnFlag2
		return
	endif
	move	str6,NCMPMLR
.Temporary patch follows
	move	COMPOLDMLR,str4
.END PATCH 3.75.7 REPLACED LOGIC
	call	OrderLoadMailer	using C6
.setitem Nord01eaStatUniverse,0,"" - DETERMINED IN ABOVE ROUTINE
.VERIFY	CAMPAIGN BROKER
.START PATCH 3.75.7 REPLACED LOGIC
.	getitem	Nord01eaEditBrk,0,str4
.	call	Trim using str4		.Nord01eaEditBrk_LostFocus will allow null entry
.	if (str4 = "")
.		clear	str3
..		 alert	 caution,"Broker Required!",result
..		 call	 OrderSwitchTab	using C6
..		 setfocus Nord01eaEditBrk
..		 move	 "Y",ReturnFlag2
..		 return
.	else
.		getitem	Nord01eaEditBrkContact,0,str3
.		call	Trim using str3
.		count	result,str3
.		if (str3 = "")
.			alert	caution,"Broker	Contact	Required!",result
.			call	OrderSwitchTab using C6
.			setfocus Nord01eaEditBrkContact
.			move	"Y",ReturnFlag2
.			return
.		else
.			pack	NBRKFLD,str4,str3
.			move	"O.VerifyC.Data-NBRKKEY",Location
.			pack	KeyLocation,"Key: ",NBRKFLD
.			call	NBRKKEY
.			if over
.				alert	caution,"Broker	Not Found!",result
.				call	OrderSwitchTab using C6
.				setfocus Nord01eaEditBrk
.				move	"Y",ReturnFlag2
.				return
.			endif
.		endif
.	endif
.	move	str4,NCMPBRK
.	move	str3,NCMPBRKCNT
...................................................
	getitem	Nord01eaEditBrk,0,str6
	call	Trim using str6		.Nord01eaEditBrk_LostFocus will allow null entry
	if (str6 = "")
		clear	str3
	else
		getitem	Nord01eaEditBrkContact,0,str3
		call	Trim using str3
		if (str3 = "")
.ALLOW INSTANCE OF NO CONTACT AS THERE ARE MANY COMPANY RECORDS OUT THERE WITHOUT ANY CONTACT RECORDS
.			alert	caution,"Broker	Contact	Required!",result
.			call	OrderSwitchTab using C6
.			setfocus Nord01eaEditBrkContact
.			move	"Y",ReturnFlag2
.			return
		endif
.Check to see if 'Broker' is a valid Company, and if flagged as Broker
		pack	COMPFLD,str6
		move	"O.VerifyC.Data-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		if over
			alert	caution,"Broker	Not Found!",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditBrk
			move	"Y",ReturnFlag2
			return
		elseif (COMPBRKFLG <> "T" & COMPCLRFLG <> "T")
			alert	caution,"Valid Broker Required!",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditBrk
			move	"Y",ReturnFlag2
			return
		elseif (str3 <> "")
.Now test to make sure the Broker/Contact number is a valid entry
			pack	CNCTFLD,str6,str3
			move	"O.VerifyC.Data-CNCTKEY",Location
			pack	KeyLocation,"Key: ",CNCTFLD
			call	CNCTKEY
			if over
				alert	caution,"Valid Broker Contact Required!",result
				call	OrderSwitchTab using C6
				setfocus Nord01eaEditBrkContact
				move	"Y",ReturnFlag2
				return
			endif
			pack	NBRKFLD,CNCTCNT
		else
			pack	NBRKFLD,COMPOLDBRK,"000"
		endif
	endif
	move	str6,NCMPBRK
	move	str3,NCMPBRKCNT
.END PATCH 3.75.7 REPLACED LOGIC
	call	OrderLoadBroker	using C6
.VERIFY	CAMPAIGN PO NUM
	getitem	Nord01eaEditPO,0,NCMPPO
	call	Trim using NCMPPO
.VERIFY	CAMPAIGN SHIP-TO
	getitem	Nord01eaEditRtn,0,str6
	call	Trim using str6
	if (str6 <> "")		    .NCMPSHIPTO
		pack	NRTNFLD,str6
		move	"O.VerifyC.Data-NRTNTST",Location
		pack	KeyLocation,"Key: ",NRTNFLD
		call	NRTNTST
		if over
			alert	caution,"Ship- To ## Not Found!",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditRtn
   			move	"Y",ReturnFlag2
			return
		endif
	endif
	move	str6,NCMPSHIPTO
.VERIFY	REPORT DEFAULT
	getitem	Nord01eaComboReport,0,N1
	move	N1,NCMPRpt
.VERIFY	CAMPAIGN DATE
	getitem	Nord01eaEditCampDate,0,str10
	call	TRIM using str10
	count	N2,str10
	if (N2 = 10)
		unpack	str10,MM,str1,DD,str1,str2,YY
	elseif (N2 = 8)
		unpack	str10,MM,DD,str2,YY
	else
		alert	caution,"Campaign Date Must be in MMDDCCYY Format",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaEditCampDate
		move	"Y",ReturnFlag2
		return
	endif
	move	str2,N2
	move	YY,N3
	if ((N2	= "19" AND N3 <	"80") OR N2 < "19" OR N2 > "25")
		alert	caution,"Invalid Year!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaEditCampDate
		move	"Y",ReturnFlag2
		return
	endif
	pack	NCMPDATE,str2,YY,MM,DD
.VERIFY	CAMPAIGN RETURN	DATE
	getitem	Nord01eaEditRtnDate,0,str10
	call	TRIM using str10
	count	N2,str10
	if (N2 <> 0)
		if (N2 = 10)
			unpack	str10,MM,str1,DD,str1,str2,YY
		elseif (N2 = 8)
			unpack	str10,MM,DD,str2,YY
		else
			alert	caution,"Return	Date Must be in	MMDDCCYY Format",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditRtnDate
			move	"Y",ReturnFlag2
			return
		endif
		move	str2,N2
		move	YY,N3
		if ((N2	= "19" AND N3 <	"80") OR N2 < "19" OR N2 > "25")
			alert	caution,"Invalid Year!",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditRtnDate
			move	"Y",ReturnFlag2
			return
		endif
		pack	NCMPRDATE,str2,YY,MM,DD
	else
		clear	NCMPRDATE
	endif
.VERIFY	CAMPAIGN CUT-OFF DATE
	getitem	Nord01eaEditCutOffDate,0,str10
	call	TRIM using str10
	count	N2,str10
	if (N2 <> 0)
		if (N2 = 10)
			unpack	str10,MM,str1,DD,str1,str2,YY
		elseif (N2 = 8)
			unpack	str10,MM,DD,str2,YY
		else
			alert	caution,"Cut-Off Date Must be in MMDDCCYY Format",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditCutOffDate
			move	"Y",ReturnFlag2
			return
		endif
		move	str2,N2
		move	YY,N3
		if ((N2	= "19" AND N3 <	"80") OR N2 < "19" OR N2 > "25")
			alert	caution,"Invalid Year!",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditCutOffDate
			move	"Y",ReturnFlag2
			return
		endif
		pack	NCMPCDATE,str2,YY,MM,DD
	else
		clear	NCMPCDATE
	endif
.VERIFY	CAMPAIGN MAIL DATE
	getitem	Nord01eaEditMailDate,0,str10
	call	TRIM using str10
	count	N2,str10
.As per	Time Saviour meeting 2/16/01, SM wants to make this a required field - ASH
.	 if (N2	<> 0)
		if (N2 = 10)
			unpack	str10,MM,str1,DD,str1,str2,YY
		elseif (N2 = 8)
			unpack	str10,MM,DD,str2,YY
		else
			alert	caution,"Mail Date Must	be in MMDDCCYY Format",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditMailDate
			move	"Y",ReturnFlag2
			return
		endif
		move	str2,N2
		move	YY,N3
		if ((N2	= "19" AND N3 <	"80") OR N2 < "19" OR N2 > "25")
			alert	caution,"Invalid Year!",result
			call	OrderSwitchTab using C6
			setfocus Nord01eaEditMailDate
			move	"Y",ReturnFlag2
			return
		endif
		pack	NCMPMDATE,str2,YY,MM,DD
.	 else
.		 clear	 NCMPMDATE
.	 endif
.VERIFY	CAMPAIGN PLANNER
	getitem	Nord01eaComboPlanner,0,N2
	if (N2 <= 1)
		alert	caution,"Valid Planner Required!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaComboPlanner
		move	"Y",ReturnFlag2
		return
	endif
	getitem	Nord01eaComboPlanner,N2,str45
	unpack	str45,str35,str1,NCMPPLANNER
.VERIFY	CAMPAIGN CONTACT
	getitem	Nord01eaComboContact,0,N2
	if (N2 <= 1)
		alert	caution,"Valid Contact Required!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaComboContact
		move	"Y",ReturnFlag2
		return
	endif
	getitem	Nord01eaComboContact,N2,str45
	unpack	str45,str35,str1,NCMPCNT
..VERIFY CAMPAIGN PACKAGE
.	 setitem Nord01eaComboPackage,0,1
.VERIFY	CAMPAIGN KEY INFO
	getitem	Nord01eaEditKey,0,NCMPKEY
	call	Trim using NCMPKEY
.BILL DIRECT
	getitem	Nord01eaCheckBillDirect,0,N1
	move	N1,NCMPBill
.VERIFY	CAMPAIGN QTY - GROSS/NET
	getitem	Nord01eaEditGrossQty,0,str17
	call	RemoveChar using str17,COMMA
	call	Trim using str17
	move	str17,NCMPQTY
	call	ZFILLIT	using NCMPQTY
	if (NCMPQTY = "")
		move   	"000000000",NCMPQTY
	endif
	getitem	Nord01eaEditNetQty,0,STR17
	call	RemoveChar using str17,COMMA
	call	Trim using str17
	move	str17,NCMPNETQTY
	call	ZFILLIT	using NCMPNETQTY
	if (NCMPNETQTY = "")
		move	"000000000",NCMPNETQTY
	endif
.VERIFY	CAMPAIGN MEDIA
.- COMBOBOX LOADED IMMEDIATELY AFTER FORMS ARE LOADED
.Logic here is a bit strange but here is the idea:  MED20 currently holds blank	record,	and that
.has been associated with item #1 of combobox.	Item #1	will equal MED20, Item #2 will equal MED00,
.Item #3 will equal MED01, etc.	 When you reach	Item #20 one of	those displacements has	been
.soaked	up by associating MED20	with Item #1 and so you	need one less incrementation.
	getitem	Nord01eaComboMedia,0,N2
	if (N2 <= C1)
		move	"20",NCMPMEDIA
	else
		sub	C1,N2
		if (N2 < "21")
			sub	C1,N2
		endif
		move	N2,str3
		call	TRIM using str3
		count	result,str3
.Pack number with preceding zeroes
.Have not used ZFILLIT as this is a special field
		if (result < 2)
			sub	result from "2"	giving N1
			setlptr	filler,	N1
			pack	str2,filler,str3
		else
			move	str3,str2
		endif
		move	str2,NCMPMEDIA
	endif
.VERIFY	CAMPAIGN OFFER
	clear	NCMPOFFER
	getitem	Nord01eaComboOffer,0,result
	getitem	Nord01eaComboOffer,result,str45
	unpack	str45,str35,str6,str3
	call	Trim using str3
	if (str3 = "")
		alert	caution,"Offer Required!",result
		call	OrderSwitchTab using C6
		setfocus Nord01eaComboOffer
		move	"Y",ReturnFlag2
		return
	else
		pack	NCMPOFFER,str3
	endif
.VERIFY	CAMPAIGN SHIPPING
	getitem	Nord01eaComboShip,0,N2
	if (N2 <= 1)
		move	B2,NCMPSHIP
	else
		sub	C2,N2
		move	N2,str3
		call	TRIM using str3
		count	result,str3
.Pack number with preceding zeroes
.Have not used ZFILLIT as this is a special field
		if (result < 2)
			sub	result from "2"	giving N1
			setlptr	filler,	N1
			pack	str2,filler,str3
		else
			move	str3,str2
		endif
		move	str2,NCMPSHIP
	endif
.VERIFY	CAMPAIGN SAMPLE
	getitem	Nord01eaComboSample,0,N2
.START PATCH 3.72.2 REPLACED LOGIC
.	getitem	Nord01eaComboSample,N2,str35
.	unpack	str35,str30,str1,str3
	getitem	Nord01eaComboSample,N2,str55
	unpack	str55,str30,str1,str10,str1,str3
.END PATCH 3.72.2 REPLACED LOGIC
	type	str3
	if not equal
		move	B1,NCMPSAMPLE
		setitem	Nord01eaComboSample,0,1
	else
		move	str3,NCMPSAMPLE
	endif
.VERIFY	CAMPAIGN RESPONSE
	getitem	Nord01eaEditRespChange,0,NCMPRATE
.VERIFY	CAMPAIGN GIFT
	getitem	Nord01eaEditGiftChange,0,NCMPGIFT
.VERIFY	CAMPAIGN SPECIAL INSTRUCTIONS
	getitem	Nord01eaEditSpecial,0,NCMPCOMMENT
	call	Trim using NCMPCOMMENT
	return		...End of Verify...

.START PATCH 3.49 ADDED LOGIC
.OrderSetShipToInfo Routine ComboPtr
.	moveaddr Nord01eaComboShip,ComboPtr
.	formload nord01ea
.	for N9,C1,"100"
.		getitem	Nord01eaComboShip,N9,str55
.		setitem	ComboPtr,N9,str55
.	repeat
.	return
.END PATCH 3.49 ADDED LOGIC

OrderVerifyLOLData
.VERIFY	LOL STATUS
	getitem	Nord01ECComboStatus,0,N1
	sub	C1,N1
	if (N1 <> C0)
		move	N1,NLOLSTAT
	else
		move	B1,NLOLSTAT
	endif
..VERIFY LOL ASSOCIATED	LR/LCR
.	 getitem Nord01ECEditLCR,0,str6
.	 call	 Trim using str6
.	 if (str6 <> Hold3LCR AND Hold3LCR <> "" AND NewFlag3 <> YES)
.		 alert	 plain,"You wish to change associated LR/LCR ##?",result
.		 if (result <> 1)
.			 call	 OrderSwitchTab	using C8
.			 setfocus Nord01ECEditLCR
.			 move	 "Y",ReturnFlag3
.			 return
.		 endif
.	 endif
.	 if (str6 <> "")
.		 pack	 NORDFLD,str6
.		 move	 C1,NORDPATH
.		 move	 "VerifyLOL-NORDTST",Location
.		 pack	 KeyLocation,"Key: ",str6
.		 call	 NORDTST
.		 if over
.			 alert	 caution,"This LR/LCR does not exist!!",result
.			 call	 OrderSwitchTab	using C8
.			 setfocus Nord01ECEditLCR
.			 move	 "Y",ReturnFlag3
.			 return
.		 endif
.	 endif
.	 move	 str6,NLOLLCR
.VERIFY	LOL LOL	NUM/NAME
	getitem	Nord01ECEditCamp,0,str6
	call	Trim using str6
	if (str6 = "")
		alert	caution,"6 digit Campaign Number required!!",result
		call	OrderSwitchTab using C8
		setfocus Nord01ECEditCamp
		move	"Y",ReturnFlag3
		return
	endif
	if (str6 <> Hold3Camp AND Hold3Camp <> "" AND NewFlag3 <> YES)
		alert	plain,"You wish	to change associated Camp. ##?",result
		if (result <> 1)
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditCamp
			move	"Y",ReturnFlag3
			return
		endif
	endif
	move	C1,NCMPPATH
	pack	NCMPFLD,str6
	move	"VerifyLOL-NCMPKEY",Location
	pack	KeyLocation,"Key: ",str6
	call	NCMPKEY
	if over
		alert	caution,"Invalid Campaign Number!!",result
      		call	OrderSwitchTab using C8
		setfocus Nord01ECEditCamp
		move	"Y",ReturnFlag3
		return
	endif
	move	str6,NLOLCNUM
.FOLLOWING FIELDS ONLY RETRIEVED IF IN SECURITY	MODE!!
	if (SecFlag3 = YES)
.Following fields must match Campaign vars queried above!!!
.VERIFY	LOL MAILER
		getitem	Nord01ECEditMlr,0,str4
		call	Trim using str4
.START PATCH 3.75.7 REPLACED LOGIC
.		if (str4 <> NCMPMLR)
		pack	COMPFLD,NCMPMLR
		move	"VerifyLOLa-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		if (str4 <> COMPOLDMLR)
.END PATCH 3.75.7 REPLACED LOGIC
			alert	caution,"LOL Mailer does not match Campaign Mailer!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditMlr
			move	"Y",ReturnFlag3
			return
		endif
.VERIFY	LOL BROKER
		getitem	Nord01ECEditBrk,0,str4
		call	Trim using str4
		call	Trim using NCMPBRK
.START PATCH 3.75.7 REPLACED LOGIC
.		if (str4 <> NCMPBRK)
		pack	COMPFLD,NCMPBRK
		move	"VerifyLOLB-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		call	Trim using COMPOLDBRK
		if (str4 <> COMPOLDBRK)
.END PATCH 3.75.7 REPLACED LOGIC
			alert	caution,"LOL Broker does not match Campaign Broker!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditBrk
			move	"Y",ReturnFlag3
			return
		endif
		getitem	Nord01ECEditBrkContact,0,str3
		call	Trim using str3
		call	Trim using NCMPBRKCNT
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.		if (str3 <> NCMPBRKCNT)
		pack	CNCTFLD,NCMPBRK,NCMPBRKCNT
		move	"VerifyLOLB-CNCTKEY",Location
		pack	KeyLocation,"Key: ",CNCTFLD
		call	CNCTKEY
		unpack	CNCTCNT,str4,str4
		if (str3 <> str4)
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
			alert	caution,"LOL Brk/Cnt. does not match Campaign Brk/Cnt.!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditBrkContact
			move	"Y",ReturnFlag3
			return
		endif
.VERIFY	LOL PO NUM
		getitem	Nord01ECEditPO,0,str12
		call	Trim using str12
		call	Trim using NCMPPO
		if (str12 <> NCMPPO)
			alert	caution,"LOL PO	does not match Campaign	PO!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditPO
			move	"Y",ReturnFlag3
			return
		endif
.VERIFY	LOL SHIP-TO
		getitem	Nord01ECEditRtn,0,str4
		call	Trim using str4
		call	Trim using NCMPSHIPTO
		if (str4 <> NCMPSHIPTO)
			alert	caution,"LOL ShipTo does not match Campaign ShipTo!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditRtn
			move	"Y",ReturnFlag3
			return
		endif
.VERIFY	LOL PLANNER
		getitem	Nord01ECComboPlanner,0,N2
		getitem	Nord01ECComboPlanner,N2,str45
		unpack	str45,str35,str1,str2
		if (str2 <> NCMPPLANNER)
			alert	caution,"LOL Planner does not match Campaign Planner!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECComboPlanner
			move	"Y",ReturnFlag3
			return
		endif
.VERIFY	LOL CONTACT
		getitem	Nord01ECComboContact,0,N2
		getitem	Nord01ECComboContact,N2,str45
		unpack	str45,str35,str1,str2
		if (str2 <> NCMPCNT)
			alert	caution,"LOL Contact does not match Campaign Contact!!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECComboContact
			move	"Y",ReturnFlag3
			return
		endif
.END OF	SECURITY MODE RETRIEVAL
	endif
.VERIFY	LOL PACKAGE
.	 getitem Nord01ECComboPackage,0,N2
.	 getitem Nord01ECComboPackage,N2,str45
.	 unpack	 str45,str35,str1,str2
.	 move	 str2,NLOLPACKAGE
.VERIFY	LOL OFFER
	clear	NLOLOFFER
	getitem	Nord01ECComboOffer,0,result
	getitem	Nord01ECComboOffer,result,str45
	unpack	str45,str35,str6,str3
	call	Trim using str3
	if (str3 = "")
		alert	caution,"Offer Required!",result
		call	OrderSwitchTab using C8
		setfocus Nord01ECComboOffer
		move	"Y",ReturnFlag3
		return
	else
		pack	NLOLOFFER,str3
	endif
.VERIFY	LOL SAMPLE
	getitem	Nord01ECComboSample,0,N2
.START PATCH 3.72.2 REPLACED LOGIC
.	getitem	Nord01ECComboSample,N2,str35
.	unpack	str35,str30,str1,str3
	getitem	Nord01ECComboSample,N2,str55
	unpack	str55,str30,str1,str10,str1,str3,str1
.END PATCH 3.72.2 REPLACED LOGIC
	type	str3
	if not equal
		move	B1,NLOLSAMPLE
		setitem	Nord01ECComboSample,0,1
	else
		move	str3,NLOLSAMPLE
	endif
.VERIFY	LOL LIST
	getitem	Nord01ECEditList,0,str6
	call	Trim using str6
	move	C1,NDATPATH
	pack	NDATFLD,str6
	move	"VerifyLOL-NDATTST",Location
	pack	KeyLocation,"Key: ",str6
	call	NDATTST
	if over
		alert	caution,"List Required!",result
		call	OrderSwitchTab using C8
		setfocus Nord01ECEditList
		move	"Y",ReturnFlag3
		return
	endif
	move	str6,NLOLLIST
.VERIFY	LOL OWNER
	getitem	Nord01ECEditOwner,0,str4
	call	Trim using str4
	move	C1,NOWNPATH
	pack	NOWNFLD,str4
	move	"VerifyLOL-NOWNTST",Location
	pack	KeyLocation,"Key: ",str4
	call	NOWNTST
	if over
		alert	caution,"Owner Required!",result
		call	OrderSwitchTab using C8
		setfocus Nord01ECEditOwner
		move	"Y",ReturnFlag3
		return
	endif
	move	str4,NLOLOWNER
.VERIFY	UNIVERSE
.START PATCH 3.72 REPLACED LOGIC - MOVED LOGIC TO Nordmsk3SAVE BUTTON
.	getitem	Nord01ECEditUniverse,0,str17
.	call	RemoveChar using str17,COMMA
.	call	Trim using str17
.	move	str17,NLOLUNIVERSE
.	call	ZFILLIT	using NLOLUNIVERSE
.	if (NLOLUNIVERSE = "")
.		move	"000000000",NLOLUNIVERSE
.	endif
.
..VERIFY	LOL LIST SELECT
.	getitem	Nord01ECEditListSel,0,str35
.	call	Trim using str35
.	move	str35,NLOLSELECT
	clear	NLOLUNIVERSE
	clear	NLOLSELECT
.END PATCH 3.72 REPLACED LOGIC - MOVED LOGIC TO NORDMSK3SAVE BUTTON
.VERIFY	LOL RECORD DATE
	if (SecFlag3 = YES)
		getitem	Nord01ECEditLOLDate,0,str10
		call	TRIM using str10
		count	N2,str10
		if (N2 = C10)
			unpack	str10,MM,str1,DD,str1,CC,YY
		elseif (N2 = C8)
			unpack	str10,MM,DD,CC,YY
		else
			alert	caution,"Record	Date must be in	MMDDCCYY Format!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditLOLDate
			move	"Y",ReturnFlag3
			return
		endif
		pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
		setitem	Nord01ECEditLOLDate,0,newdate1
		move	CC,N2
		move	YY,N3
		if ((N2	= "19" AND N3 <	"80") OR N2 < "19" OR N2 > "25")
			alert	caution,"Invalid Year!",result
			call	OrderSwitchTab using C8
			setfocus Nord01ECEditLOLDate
			move	"Y",ReturnFlag3
			return
		endif
		pack	NLOLDATE,CC,YY,MM,DD
	endif
.VERIFY	LOL MAIL DATE
	getitem	Nord01ECEditMailDate,0,str10
	call	TRIM using str10
	count	N2,str10
	if (N2 = C10)
		unpack	str10,MM,str1,DD,str1,CC,YY
	elseif (N2 = C8)
		unpack	str10,MM,DD,CC,YY
	else
		alert	caution,"Mail Date must	be in MMDDCCYY Format!",result
		call	OrderSwitchTab using C8
		setfocus Nord01ECEditMailDate
		move	"Y",ReturnFlag3
		return
	endif
	pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
	setitem	Nord01ECEditMailDate,0,newdate1
	move	CC,N2
	move	YY,N3
	if ((N2	= "19" AND N3 <	"80") OR N2 < "19" OR N2 > "25")
		alert	caution,"Invalid Year!",result
		call	OrderSwitchTab using C8
		setfocus Nord01ECEditMailDate
		move	"Y",ReturnFlag3
		return
	endif
	pack	NLOLMDATE,CC,YY,MM,DD
.VERIFY	LOL REGIONAL
	getitem	Nord01ECCheckRegional,0,N9
	if (N9 = C1)
		move	C1,NLOLREGIONAL
	else
		move	B1,NLOLREGIONAL
	endif
.VERIFY	LOL GROSS/NET QTY
	getitem	Nord01ECEditGrossQty,0,str17
	call	RemoveChar using str17,COMMA
	call	Trim using str17
	move	str17,NLOLQTY
	call	ZFILLIT	using NLOLQTY
	if (NLOLQTY = "")
		move	"000000000",NLOLQTY
		move	"000000000",NLOLNETQTY
....................................................
.		 clear	 NLOLNET
		getitem	Nord01ECEditNetPer,0,str6
		move	C0,N32
		move	str6,N32
		if (N32	> C0)
			move	N32,NLOLNET
		else
			clear	NLOLNET
		endif
		setitem	Nord01ECEditNetPer,0,NLOLNET
....................................................
	else
.Force a calculation
		getitem	Nord01ECEditNetQty,0,STR17
		getitem	Nord01ECEditNetPer,0,str6
		if (STR17 <> ""	AND str6 = "")
			call	OrderCalculateNet using	Nord01ECEditGrossQty,Nord01ECEditNetPer,Nord01ECEditNetQty,C2
		elseif (STR17 =	"" AND str6 <> "")
			call	Order8CalculateNet
		endif
.
		getitem	Nord01ECEditNetQty,0,STR17
		call	RemoveChar using str17,COMMA
		call	Trim using str17
		move	str17,NLOLNETQTY
		call	ZFILLIT	using NLOLNETQTY
		if (NLOLNETQTY = "")
			move	"000000000",NLOLNETQTY
		endif
.VERIFY	LOL NET	PERCENTAGE
		getitem	Nord01ECEditNetPer,0,str6
		move	C0,N32
		move	str6,N32
		if (N32	> C0)
			move	N32,NLOLNET
		else
			clear	NLOLNET
		endif
		setitem	Nord01ECEditNetPer,0,NLOLNET
	endif
.VERIFY	LOL EXCHANGE/RENT
	getitem	Nord01ECCheckExchange,0,N9
	getitem	Nord01ECCheckRent,0,N8
	move	C0,N1
	if (N9 = C1)
		add	C1,N1
	endif
	if (N8 = C1)
		add	C2,N1
	endif
	move	N1,NLOLRENT
.VERIFY	LOL TESTS/CONT
	getitem	Nord01ECCheckTest,0,N9
	if (N9 = C1)
		move	C1,NLOLTEST
	else
		getitem	Nord01ECCheckReTest,0,N9
		if (N9 = C1)
			move	C2,NLOLTEST
		else
			move	B1,NLOLTEST
		endif
	endif

.VERIFY	LOL RESPONSE
.TESTING
.	 getitem Nord01ECEditResp,0,NLOLRATE
	call	Trim using NLOLRATE
.VERIFY	LOL GIFT
.TESTING
.	 getitem Nord01ECEditGift,0,NLOLGIFT
	call	Trim using NLOLGIFT
.VERIFY	LOL SPECIAL INSTRUCTIONS
	getitem	Nord01ECEditSpecial,0,NLOLCOMMENT
	call	Trim using NLOLCOMMENT
	setitem	Nord01ECEditSpecial,0,NLOLCOMMENT
	getitem	Nord01ECEditSpecial1,0,NLOLCOMMENT1
	call	Trim using NLOLCOMMENT1
	setitem	Nord01ECEditSpecial1,0,NLOLCOMMENT1
	return

OrderVerifyStats
.Start by clearing all fields
	clear	STATVARS
.LOL INDICATOR
	getitem	NSTA001ACheckLOL2,0,N1
	move	N1,STATLOL
.LOL/LR	NUMBER
	getitem	NSTA001AEditLR2,0,STATLR
	call	Trim using STATLR
	if (STATLR = "")
		alert	caution,"Valid LR/LOL Record Required!",result
		call	OrderSwitchStatTab using C2
		setfocus NSTA001AEditLR2
		move	YES,ReturnFlag5
		return
	endif
	if (STATLOL = "1")
		move	C1,NLOLPATH
		pack	NLOLFLD,STATLR
		move	"Ver.Stats-NLOLKEY",Location
		pack	KeyLocation,"Key: ",NLOLFLD
		call	NLOLKEY
		if over
			alert	caution,"Valid LOL Record Required!",result
			call	OrderSwitchStatTab using C2
			setfocus NSTA001AEditLR2
			move	YES,ReturnFlag5
			return
		endif
		packkey	NCMPFLD,NLOLCNUM
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.7 REPLACED LOGIC
..		move	NCMPMLR,STATMLR
.		pack	COMPFLD,NCMPMLR
.		move	"Ver.Stats-COMPKEY",Location
.		pack	KeyLocation,"Key: ",COMPFLD
.		call	COMPKEY
.		move	COMPOLDMLR,STATMLR
..END PATCH 3.75.7 REPLACED LOGIC
		move	NCMPMLR,STATMLR
.END PATCH 3.75.9 REPLACED LOGIC
.START PATCH 3.43 ADDED LOGIC
		getitem	NSTA001AEditType2B,0,STATTYPE
		call	Trim using STATTYPE
		if (STATTYPE <> "" & STATTYPE <> "E" & STATTYPE <> "R" & STATTYPE <> "S")
			alert	caution,"Valid Record Type Required!",result
			call	OrderSwitchStatTab using C2
			setfocus NSTA001AEditType2B
			move	YES,ReturnFlag5
			return
		endif
.END PATCH 3.43 ADDED LOGIC
.START PATCH 3.45 ADDED LOGIC
.START PATCH 3.72 REMOVED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
.		getitem	NSTA001AEditSelect2,0,STATSEL
.		call	Trim using STATSEL
.END PATCH 3.72 REMOVED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
		getitem NSTA001AEditLRQty2,0,NLOLQty
		call	Trim using NLOLQty
		call	RemoveChar using NLOLQty,COMMA
		call	ZFillIt using NLOLQty,C0
.END PATCH 3.45 ADDED LOGIC
	else
		move	C1,NORDPATH
		pack	NORDFLD,STATLR
		move	"Ver.Stats-NORDKEY",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDKEY
		if over
			alert	caution,"Valid LR Record Required!",result
			call	OrderSwitchStatTab using C2
			setfocus NSTA001AEditLR2
			move	YES,ReturnFlag5
			return
.START PATCH 3.43 ADDED LOGIC
		elseif (OSTAT = "l" | OSTAT = "p" | OSTAT = "x" | OSTAT = "z")
			getitem	NSTA001AEditType2B,0,STATTYPE
			call	Trim using STATTYPE
			if (STATTYPE <> "" & STATTYPE <> "E" & STATTYPE <> "R" & STATTYPE <> "S")
				alert	caution,"Valid Record Type Required!",result
				call	OrderSwitchStatTab using C2
				setfocus NSTA001AEditType2B
				move	YES,ReturnFlag5
				return
			endif
.START PATCH 3.45 ADDED LOGIC
.START PATCH 3.72 REMOVED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
.			getitem	NSTA001AEditSelect2,0,STATSEL
.			call	Trim using STATSEL
.END PATCH 3.72 REMOVED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
			getitem NSTA001AEditLRQty2,0,OQTY
			call	Trim using OQTY
			call	RemoveChar using OQTY,COMMA
			call	ZFillIt using OQTY,C0
.END PATCH 3.45 ADDED LOGIC
		else
			if (OELCODE = "2" | OELCODE = "3")
				if (ORENT = "1")  .LCR Rental
					move	"S",STATTYPE
				else
					call	Trim using OEXQTY
					move	C0,N9
					move	OEXQTY,N9
					if (N9 > 0)
						move	"S",STATTYPE
					else
						move	"E",STATTYPE
					endif
				endif
			else
				move	"R",STATTYPE
			endif
.END PATCH 3.43 ADDED LOGIC
.START PATCH 3.45 ADDED LOGIC
.START PATCH 3.72 REMOVED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
.			move	O2DES,STATSEL
.			setitem	NSTA001AEditSelect2,0,STATSEL
.END PATCH 3.72 REMOVED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
.END PATCH 3.45 ADDED LOGIC
		endif
		packkey	NCMPFLD,OCAMP
.START PATCH 3.75.9 REPLACED LOGIC
.		move	OMLRNUM,STATMLR
		pack	COMPFLD3,OMLRNUM
		move	"Ver.Stats-COMPKEY3",Location
		pack	KeyLocation,"Key: ",COMPFLD3
		call	COMPKEY3
		move	COMPNUM,STATMLR
.END PATCH 3.75.9 REPLACED LOGIC
	endif
.STAT NUMBER (KEY VALUE)
	getitem	NSTA001AEditStatNum2,0,STATNUM
	move	C0,N3
	move	STATNUM,N3
	move	N3,STATNUM
	rep	zfill,STATNUM
.CAMPAIGN
	move	C1,NCMPPATH
	move	"Ver.Stats-NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
	if over
		alert	caution,"This Record does not contain a	valid Campaign Number!",result
		call	OrderSwitchStatTab using C2
		setfocus NSTA001AEditLR2
		move	YES,ReturnFlag5
		return
	endif
.Not really a possibility, but what the	hay.
	call	Trim using STATMLR
	if (STATMLR = "")
		alert	caution,"This Record does not contain a	valid Mailer!",result
		call	OrderSwitchStatTab using C2
		setfocus NSTA001AEditLR2
		move	YES,ReturnFlag5
		return
	else
.START PATCH 3.75.9 REPLACED LOGIC
.		move	C1,NMLRPATH
.		pack	MKEY,STATMLR,"000"
.		move	"Ver.Stats-NMLRTST",Location
.		pack	KeyLocation,"Key: ",MKEY
.		call	NMLRTST
.		if over
.			alert	caution,"This Record does not contain a	valid Mailer!",result
.			call	OrderSwitchStatTab using C2
.			setfocus NSTA001AEditLR2
.			move	YES,ReturnFlag5
.			return
.		endif
......................................
		pack	COMPFLD,STATMLR
		move	"Ver.Stats-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		if over
			alert	caution,"This Record does not contain a	valid Mailer!",result
			call	OrderSwitchStatTab using C2
			setfocus NSTA001AEditLR2
			move	YES,ReturnFlag5
			return
		elseif (COMPMLRFLG <> "T")
			alert	caution,"This Record does not contain a	valid Mailer!",result
			call	OrderSwitchStatTab using C2
			setfocus NSTA001AEditLR2
			move	YES,ReturnFlag5
			return
		endif
.END PATCH 3.75.9 REPLACED LOGIC
	endif
.PACKAGE
	getitem	NSTA001AEditPackNum2,0,STATPCKNUM
	call	Trim using STATPCKNUM
	if (STATPCKNUM = "")
		alert	caution,"Valid Package Number Required!",result
		call	OrderSwitchStatTab using C2
		setfocus NSTA001AEditPackNum2
		move	YES,ReturnFlag5
		return
	endif
	call	ZFillIt	using STATPCKNUM,C0
	move	C1,NPKGPATH
.START PATCH 3.75.9 REPLACED LOGIC
..START PATCH 3.75.4 REPLACED LOGIC
..	pack	NPKGFLD,STATMLR,STATPCKNUM
.	move	"Ver.Stats-COMPKEY3",Location
.	pack	COMPFLD3,STATMLR
.	pack	KeyLocation,"Key: ",COMPFLD3
.	call	COMPKEY3
.	if over
.		clear	COMPNUM
.	endif
.	pack	NPKGFLD,COMPNUM,STATPCKNUM
..END PATCH 3.75.4 REPLACED LOGIC
	pack	NPKGFLD,STATMLR,STATPCKNUM
.END PATCH 3.75.9 REPLACED LOGIC
	move	"Ver.Stats-NPKGTST",Location
	pack	KeyLocation,"Key: ",NPKGFLD
	call	NPKGTST
	if over
		alert	caution,"Valid Package Number Required!",result
		call	OrderSwitchStatTab using C2
		setfocus NSTA001AEditPackNum2
		move	YES,ReturnFlag5
		return
.We will assume	they cannot select a Master Package
	endif
.LIST CPM
	getitem	NSTA001BEditListCostM2,0,str8
	call	RemoveChar using str8,COMMA
	move	C0,STATLCPM
	move	str8,STATLCPM
.PACKAGE COST
	getitem	NSTA001BEditPackageCost2,0,str13
	call	RemoveChar using str13,COMMA
	move	C0,STATPACK
	move	str13,STATPACK
.
.	getitem	StatsEditPackageCostThou2,0,str13
.	call	RemoveChar using str13,COMMA
.	move	C0,STATPCKM
.	move	str13,STATPCKM
.............................
.	move	C0,STATPCKM
.	getitem	StatsComboPackageCostThou,0,N9
.	getitem	StatsComboPackageCostThou,N9,str10
.	call	Trim using str10
.	if (str10 <> "")
.		unpack	str10,MM,str1,DD,str1,CC,YY
.		move	C0,JULDAYS
.		call	cvtjul
.		move	JULDAYS,STATPCKM
.	endif
.
	move	C0,STATPCKM
	if (NCMPRPT <> "3")
		move	C0,N8
		NSTA001BListViewPackage.GetItemCount giving N9
		sub	C1,N9
		for	howmany,"0",N9
			NSTA001BListViewPackage.GetItemText giving	str1 using howmany,1
			if (str1 = "X")
				NSTA001BListViewPackage.GetItemText giving	str5 using howmany,0
				move	str5,STATPCKM
				break
			endif
		repeat
	endif
.SELECT UNIVERSE
.START PATCH 3.47 ADDED LOGIC
.START PATCH 3.72 REPLACED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
.	getitem	NSTA001AEditSelUniverse2,0,str11
.	call	RemoveChar using str11,COMMA
.	move	C0,STATSELUNI
.	move	str11,STATSELUNI
.	call	FormatNumeric using str11,str13
.	setitem	NSTA001AEditSelUniverse2,0,str13
	move	C0,STATSELUNI
.END PATCH 3.72 REPLACED LOGIC - VERIFICATION MOVED TO CLICK_NSTA0002ButtonSave
.END PATCH 3.47 ADDED LOGIC
.RESPONSE RATE
	getitem	NSTA001AEditRespRate2,0,str6
	call	RemoveChar using str6,COMMA
	move	C0,STATRESP2
	move	str6,STATRESP2
.AVG GIFT
	getitem	NSTA001AEditAvgGift2,0,str8
	call	RemoveChar using str8,COMMA
	move	C0,STATGIFT
	move	str8,STATGIFT
.RECO QTY
.START PATCH 3.45 ADDED LOGIC
	move	C0,LOLGrossFlag
.END PATCH 3.45 ADDED LOGIC
	getitem	NSTA001AEditRecoQty2,0,str11
       	call	RemoveChar using str11,COMMA
	move	C0,STATRECQTY
	move	str11,STATRECQTY
	if (STATRECQTY > 0)
		if (STATLOL = "1")
			move	NLOLQTY,howmany
		else
			move	OQTY,howmany
		endif
.START PATCH 3.4 REPLACED LOGIC
.		if (STATRECQTY > howmany)
.			alert	caution,"Requested Quantity must be <= LR/LOL Gross Quantity!",result
.			call	OrderSwitchStatTab using C1
.			setfocus NSTA001AEditRecoQty2
.			move	YES,ReturnFlag5
.			return
.		else
..Test all Projection records that have	this LR/LOLNUM
.			call	OrderCalcStatQty using STATLR,STATLOL,STATNUM,N10,C2
.			add	STATRECQTY,N10
.			if (N10	> howmany)
.				alert	caution,"Total Req. Qty. for all Packages must be <= LR/LOL Gross Quantity!",result
.				call	OrderSwitchStatTab using C1
.				setfocus NSTA001AEditRecoQty2
.				move	YES,ReturnFlag5
.				return
.			endif
.			move	ONETQTY,howmany
.			if (STATRECQTY > howmany)
.				pack	taskname,"Requested Quantity is	> LR/LOL Net Quantity!",carr,"Do you wish to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditRecoQty2
.					move	YES,ReturnFlag5
.					return
.				endif
.			elseif (N10 > howmany)
.				pack	taskname,"Total	Req. Qty. for all Packages is >	LR/LOL Net Quantity!",carr,"Do you wish	to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditRecoQty2
.					move	YES,ReturnFlag5
.					return
.				endif
.			endif
.		endif
......................................
		if (STATRECQTY > howmany)
			if (STATLOL = "1")
				goto LOLQtyAfterFirstTest
			else
				alert	caution,"Requested Quantity must be <= LR/LCR Gross Quantity!",result
				call	OrderSwitchStatTab using C2
				setfocus NSTA001AEditRecoQty2
				move	YES,ReturnFlag5
				return
			endif
		else
LOLQtyAfterFirstTest
.Test all Projection records that have this LR/LOLNUM
			call	OrderCalcStatQty using STATLR,STATLOL,STATNUM,N10,C2
			add	STATRECQTY,N10
			if (N10	> howmany)
				if (STATLOL = "1")
					clear	taskname
					append	"Total Requested Quantity is Greater than LOL Gross Quantity!",taskname
					append	newline,taskname
					append	"Do you	want to	update LOL Gross Qty?",taskname
					reset	taskname
					alert	plain,taskname,result
					if (result = 1)		.YES
						move	N10,LOLGrossFlag
					elseif (result = 3)	.CANCEL
						call	OrderSwitchStatTab using C2
						setfocus NSTA001AEditRecoQty2
						move	YES,ReturnFlag5
						return
					else			.NO
						move	C0,LOLGrossFlag
					endif
				else
					alert	caution,"Total Req. Qty. for all Packages must be <= LR/LOL Gross Quantity!",result
					call	OrderSwitchStatTab using C2
					setfocus NSTA001AEditRecoQty2
					move	YES,ReturnFlag5
					return
				endif
			endif
.START PATCH 3.4 REMOVED LOGIC AS PER MEETING WITH SK 19MAR2002	- ASH
.			if (STATLOL = "1")
.				move	NLOLNETQTY,howmany
.			else
.				move	ONETQTY,howmany
.			endif
.			if (STATRECQTY > howmany)
.				pack	taskname,"Requested Quantity is	> LR/LOL Net Quantity!",carr,"Do you wish to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditRecoQty2
.					move	YES,ReturnFlag5
.					return
..FOLLOWING DOES NOT MAKE SENSE	- SO I CUT IT
..				elseif (result = 1 & STATLOL = "1")
..					alert	plain,"Do you want to update LOL Net Qty?",result
..					if (result =1)		.YES
..						move	N10,LOLNetFlag
..					elseif (result = 3)	.CANCEL
..						call	OrderSwitchStatTab using C1
..						setfocus NSTA001AEditRecoQty2
..						move	YES,ReturnFlag5
..						return
..					else
..						move	C0,LOLNetFlag
..					endif
.				endif
.			elseif (N10 > howmany)
.				pack	taskname,"Total	Req. Qty. for all Packages is >	LR/LOL Net Quantity!",carr,"Do you wish	to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditRecoQty2
.					move	YES,ReturnFlag5
.					return
..FOLLOWING DOES NOT MAKE SENSE	- SO I CUT IT
..				elseif (result = 1 & STATLOL = "1")
..					alert	plain,"Do you want to update LOL Net Qty?",result
..					if (result =1)		.YES
..						move	N10,LOLNetFlag
..					elseif (result = 3)	.CANCEL
..						call	OrderSwitchStatTab using C1
..						setfocus NSTA001AEditRecoQty2
..						move	YES,ReturnFlag5
..						return
..					else
..						move	C0,LOLNetFlag
..					endif
.				endif
.			endif
.END PATCH 3.4 REMOVED LOGIC AS	PER MEETING WITH SK 19MAR2002 -	ASH
		endif
.END PATCH 3.4 REPLACED	LOGIC
	endif
.RECEIVED QTY
	getitem	NSTA001AEditMailQty2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,STATMQTY
	move	str10,STATMQTY
	if (STATMQTY > C0)
		if (STATLOL = "1")
			move	NLOLQTY,howmany
		else
			move	OQTY,howmany
		endif
.START PATCH 3.4 REPLACED LOGIC
.		if (STATMQTY > howmany)
.			alert	caution,"Received Quantity must	be <= LR/LOL Gross Quantity!",result
.			call	OrderSwitchStatTab using C1
.			setfocus NSTA001AEditMailQty2
.			move	YES,ReturnFlag5
.			return
.		else
..Test all Projection records that have	this LR/LOLNUM
.			call	OrderCalcStatQty using STATLR,STATLOL,STATNUM,N10,C1
.			add	STATMQTY,N10
.			if (N10	> howmany)
.				alert	caution,"Total Rec. Qty. for all Packages must be <= LR/LOL Gross Quantity!",result
.				call	OrderSwitchStatTab using C1
.				setfocus NSTA001AEditMailQty2
.				move	YES,ReturnFlag5
.				return
.			endif
.			move	ONETQTY,howmany
.			if (STATMQTY > howmany)
.				pack	taskname,"Received Quantity is > LR/LOL	Net Quantity!",carr,"Do	you wish to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditMailQty2
.					move	YES,ReturnFlag5
.					return
.				endif
.			elseif (N10 > howmany)
.				pack	taskname,"Total	Rec. Qty. for all Packages is >	LR/LOL Net Quantity!",carr,"Do you wish	to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditMailQty2
.					move	YES,ReturnFlag5
.					return
.				endif
.			endif
.		endif
.......................................
		if (STATMQTY > howmany)
			if (STATLOL = "1")
				goto LOLQtyAfterSecondTest
			else
				alert	caution,"Received Quantity must	be <= LR/LOL Gross Quantity!",result
				call	OrderSwitchStatTab using C2
				setfocus NSTA001AEditMailQty2
				move	YES,ReturnFlag5
				return
			endif
		else
LOLQtyAfterSecondTest
.Test all Projection records that have this LR/LOLNUM
			call	OrderCalcStatQty using STATLR,STATLOL,STATNUM,N10,C1
			add	STATMQTY,N10
			if (N10	> howmany)
				if (STATLOL = "1")
					clear	taskname
					append	"Total Received	Quantity is Greater than LOL Gross Quantity!",taskname
					append	newline,taskname
					append	"Do you	want to	update LOL Gross Qty?",taskname
					reset	taskname
					alert	plain,taskname,result
					if (result = 1)		.YES
						move	N10,LOLGrossFlag
					elseif (result = 3)	.CANCEL
						call	OrderSwitchStatTab using C2
						setfocus NSTA001AEditMailQty2
						move	YES,ReturnFlag5
						return
					else			.NO
						move	C0,LOLGrossFlag
					endif
				else
					alert	caution,"Total Rec. Qty. for all Packages must be <= LR/LOL Gross Quantity!",result
					call	OrderSwitchStatTab using C2
					setfocus NSTA001AEditMailQty2
					move	YES,ReturnFlag5
					return
				endif
			endif
.START PATCH 3.4 REMOVED LOGIC AS PER MEETING WITH SK 19MAR2002	- ASH
.			if (STATLOL = "1")
.				move	NLOLNETQTY,howmany
.			else
.				move	ONETQTY,howmany
.			endif
.			if (STATMQTY > howmany)
.				pack	taskname,"Received Quantity is > LR/LOL	Net Quantity!",carr,"Do	you wish to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditMailQty2
.					move	YES,ReturnFlag5
.					return
..FOLLOWING DOES NOT MAKE SENSE	- SO I CUT IT
..				elseif (result = 1 & STATLOL = "1")
..					alert	plain,"Do you want to update LOL Net Qty?",result
..					if (result =1)		.YES
..						move	N10,LOLNetFlag
..					elseif (result = 3)	.CANCEL
..						call	OrderSwitchStatTab using C1
..						setfocus NSTA001AEditMailQty2
..						move	YES,ReturnFlag5
..						return
..					else
..						move	C0,LOLNetFlag
..					endif
.				endif
.			elseif (N10 > howmany)
.				pack	taskname,"Total	Rec. Qty. for all Packages is >	LR/LOL Net Quantity!",carr,"Do you wish	to continue?"
.				alert	plain,taskname,result
.				if (result <> 1)
.					call	OrderSwitchStatTab using C1
.					setfocus NSTA001AEditMailQty2
.					move	YES,ReturnFlag5
.					return
..FOLLOWING DOES NOT MAKE SENSE	- SO I CUT IT
..				elseif (result = 1 & STATLOL = "1")
..					alert	plain,"Do you want to update LOL Net Qty?",result
..					if (result =1)		.YES
..						move	N10,LOLNetFlag
..					elseif (result = 3)	.CANCEL
..						call	OrderSwitchStatTab using C1
..						setfocus NSTA001AEditMailQty2
..						move	YES,ReturnFlag5
..						return
..					else
..						move	C0,LOLNetFlag
..					endif
.				endif
.			endif
.END PATCH 3.4 REMOVED LOGIC AS	PER MEETING WITH SK 19MAR2002 -	ASH
		endif
.END PATCH 3.4 REPLACED	LOGIC
	endif
.AVERAGE NET
	getitem	NSTA001AEditAvgNet2,0,str6
	call	RemoveChar using str6,COMMA
	move	C0,STATAVGNET
	move	str6,STATAVGNET
.NET REQUESTED
	getitem	NSTA001AEditNetReq2,0,str6
	call	RemoveChar using str6,COMMA
	move	C0,statnetreq
	move	str6,statnetreq
.NET RECEIVED
	getitem	NSTA001AEditNetRec2,0,str6
	call	RemoveChar using str6,COMMA
	move	C0,statnetrec
	move	str6,statnetrec
.IN MAIL COST
	getitem	NSTA001BEditMailCost2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,statImcst
	move	str10,statImcst
.LIFE VALUE
	getitem	NSTA001BEditLValue2,0,str8
	call	RemoveChar using str8,COMMA
	move	C0,statLVal
	move	str8,statLVal
.EXCHANGE BASE
	getitem	NSTA001BEditExBase2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,statexbase
	move	str10,statexbase
.RENT BASE
	getitem	NSTA001BEditRentBase2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,statRbase
	move	str10,statRbase
.RUNNING CHARGE
	getitem	NSTA001BEditRunCharge2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,statrun
	move	str10,statrun
.SELECT	FEE
	getitem	NSTA001BEditSelectFee2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,statselfee
	move	str10,statselfee
.SHIP FEE
	getitem	NSTA001BEditShipTape2,0,str10
	call	RemoveChar using str10,COMMA
	move	C0,statship
	move	str10,statship
.NOTES
	getitem	NSTA001CEditNotes2,0,STATNNOTE
	call	Trim using STATNNOTE
	move	C1,STATNPATH
	packkey	STATNFLD,STATLR,STATLOL,STATPCKNUM
	move	"LoadStats-STATNTST",Location
	pack	KeyLocation,"Key: ",STATNFLD
	call	STATNTST
	if over
		if (STATNNOTE <> "")
			move	STATLR,STATNLR
			move	STATLOL,STATNLOL
			move	STATPCKNUM,STATNPACK
			move	"LoadStats-STATNWRT",Location
			call	STATNWRT
		endif
	else
		if (STATNNOTE <> "")
			move	"LoadStats-STATNUPD",Location
			call	STATNUPD
		else
			move	"LoadStats-STATNDEL",Location
			call	STATNDEL
		endif
	endif
	return

.START PATCH 3.72 ADDED LOGIC
OrderVerifyStats2
	getitem	NSTA001AEditSelect2,0,HoldSelName
.
	getitem	NSTA001AEditSelUniverse2,0,str13
	call	Trim using str13
	call	RemoveChar using str13,COMMA
.
	getitem	NSTA001AEditSelPrice2,0,str9
	call	Trim using str9
	call	RemoveChar using str9,COMMA
.
	getitem	NSTA001AEditSelPrice3,0,str10
	call	Trim using str10
	call	RemoveChar using str10,COMMA
	return
.END PATCH 3.72 ADDED LOGIC

.START PATCH 3.46 ADDED LOGIC
StatVerifyNotes LRoutine FrmPtr
.FrmPtr  = Indicates if LOL or Order file
	if (FrmPtr = C1)	.LOL record
.START PATCH 3.68.1 REPLACED LOGIC
.		getitem	NSTA001CEditNotes3,0,NLOLCOMMENT1
.		getitem	NSTA001CEditNotes4,0,NLOLCOMMENT
		getitem	NSTA001CEditNotes3,0,NLOLCOMMENT
		getitem	NSTA001CEditNotes4,0,NLOLCOMMENT1
.END PATCH 3.68.1 REPLACED LOGIC
	else			.Order record
		move	NO,StatNoteFlag
		getitem	NSTA001CEditNotes3,0,DESC003
		call	Trim using DESC003
.Make sure they	didn't just enter in Carriage Returns
.Essentially code below	is a version of	Trim
		if (DESC003 <> "")
			clear	N9
			clear	N8
			movelptr DESC003,N9
			loop
				cmatch	Carr,DESC003
				if not equal
					move	YES,StatNoteFlag
					goto Stat3End2
				endif
				bump	DESC003
				movefptr DESC003,N8
				until	(N8 = N9)
			repeat
.must do one last compare
			cmatch	Carr,DESC003
			if not equal
				move	YES,StatNoteFlag
			endif
		endif
Stat3End2
		if (StatNoteFlag = YES)
			reset	DESC003
		endif
.
		getitem	NSTA001CEditNotes4,0,DESC004
		call	Trim using DESC004
.Make sure they	didn't just enter in Carriage Returns
.Essentially code below	is a version of	Trim
		if (DESC004 <> "")
			clear	N9
			clear	N8
			clear	str1
			movelptr DESC004,N9
			loop
				cmatch	Carr,DESC004
				if not equal
					move	YES,str1
					goto Stat3End3
				endif
				bump	DESC004
				movefptr DESC004,N8
				until	(N8 = N9)
			repeat
.must do one last compare
			cmatch	Carr,DESC004
			if not equal
				move	YES,str1
			endif
		endif
Stat3End3
		if (str1 = YES)
			reset	DESC004
			move	YES,StatNoteFlag
		endif
	endif
	return

StatVerifyNotes2
.A change was made to Special Instructions
	move	NO,StatNoteFlag2
	getprop	NSTA001CEditNotes5,multiline=result
	getitem	NSTA001CEditNotes5,0,hold2
	count	N7,hold2
	if (N7 > 0 AND NewFlag = YES)
		move	YES,SpecFlag2
	endif
	clear	HowMany
	loop
		movefptr hold2,N9
		movelptr hold2,N8
		until (N8 = N9)
		call	PARSITUP using str55,hold2,C1
		add	C1,HowMany
	repeat
	if (HowMany > result)
		clear	taskname
		append	"You may only have up to ",taskname
		move	result,str9
		call	Trim using str9
		append	str9,taskname
		append	" lines",taskname
		append	carr,taskname
		append	"for Special Instructions!",taskname
		reset	taskname
		alert	caution,taskname,result
		call	OrderSwitchTab using C10
		setfocus NSTA001CEditNotes5
		move	YES,ReturnFlag
		return
	endif
	getitem	NSTA001CEditNotes5,0,DESC002
	call	Trim using DESC002
.Make sure they	didn't just enter in Carriage Returns
.Essentially code below	is a version of	Trim
	if (DESC002 <> "")
		clear	N9
		clear	N8
		movelptr DESC002,N9
		loop
			cmatch	Carr,DESC002
			if not equal
				move	YES,StatNoteFlag2
				goto Stat3End4
			endif
			bump	DESC002
			movefptr DESC002,N8
			until	(N8 = N9)
		repeat
.must do one last compare
		cmatch	Carr,DESC002
		if not equal
			move	YES,StatNoteFlag2
		endif
	endif
Stat3End4
	if (StatNoteFlag2 = YES)
		reset	DESC002
	endif
	return
.END PATCH 3.46 ADDED LOGIC

.END OF	VERIFICATION ROUTINES

OrderDeleteLOL
.Called	by:  NORDMSK3ButtonDelete
.Delete	Secondary ISAM
	move	C2,NLOLPATH
	pack	NLOLFLD1,NLOLCNUM
	move	"OrderDel.LOL-NLOLTST",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	call	NLOLTST
	if over
.START PATCH 3.77.8 REMOVED LOGIC
.RECORD WAS ALREADY DELETED- NOT NECESSARY TO SEND AN EMAIL
.		clear	taskname
.		append	NLOLFLD1,taskname
.		append	" DELETEDK not Found in	NINLOL File-NLOLTST!!",taskname
.		reset	taskname
.		alert	caution,taskname,result
.		move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
..   Set	the text message that is send with the attachments
.		move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
.		move	taskname,SmtpTextMessage(2)   Array <Text message >
.		move	"Subroutine Save3",SmtpTextMessage(3)	Array <Text message >
.		move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
.		call	errmesg
.END PATCH 3.77.8 REMOVED LOGIC
		return
	endif
	unpack	NLOL24,str6,str5,str1
	pack	str7,str5,str1
	if (str6 <> NLOLLOL AND	str7 = NLOLCNUM)
		loop
			until (str6 = NLOLLOL)
			until (str7 <> NLOLCNUM)
			call	NLOLKS2
			if over
.START PATCH 3.77.8 REMOVED LOGIC
.RECORD WAS ALREADY DELETED- NOT NECESSARY TO SEND AN EMAIL
.				clear	taskname
.				append	NLOLFLD1,taskname
.				append	SLASH,taskname
.				append	str6,taskname
.				append	" DELETEDK not Found in	NINLOL File-NLOLKS2!!",taskname
.				reset	taskname
.				alert	caution,taskname,result
.				move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
..   Set	the text message that is send with the attachments
.				move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
.				move	taskname,SmtpTextMessage(2)   Array <Text message >
.				move	"Subroutine Save3",SmtpTextMessage(3)	Array <Text message >
.				move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
.				call	errmesg
.END PATCH 3.77.8 REMOVED LOGIC
				return
			endif
			unpack	NLOL24,str6,str5,str1
			pack	str7,str5,str1
		repeat
	endif
	if (str7 <> NLOLCNUM)
.START PATCH 3.77.8 REMOVED LOGIC
.RECORD WAS ALREADY DELETED- NOT NECESSARY TO SEND AN EMAIL
.		clear	taskname
.		append	NLOLFLD1,taskname
.		append	SLASH,taskname
.		append	str6,taskname
.		append	" DELETEDK not Found in	NINLOL File-NLOLKS2-Final Test!!",taskname
.		reset	taskname
.		alert	caution,taskname,result
.		move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
..   Set	the text message that is send with the attachments
.		move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
.		move	taskname,SmtpTextMessage(2)   Array <Text message >
.		move	"Subroutine Save3",SmtpTextMessage(3)	Array <Text message >
.		move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
.		call	errmesg
		return
.END PATCH 3.77.8 REMOVED LOGIC
	endif
	move	"OrderDel.LOL-DELETEK NLOLFLE1",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	TRAP	IOMssg Giving Error if IO
	DELETEDK NLOLFLE1,NLOLFLD1
.Delete	AAMKEY
	move	C3,NLOLPATH
	pack	NLOLFLD2,"01X",NLOLCNUM,NLOLLIST
	clear	NLOLFLD3
	clear	NLOLFLD4
	move	"OrderDel.LOL-NLOLAIMA",Location
	pack	KeyLocation,"Key: ",NLOLFLD2
	call	NLOLAIMA
	if over
		clear	taskname
		append	NLOLFLD2,taskname
		append	" DELETEK not Found in NINLOL File-NLOLAIMA!!",taskname
		reset	taskname
		alert	caution,taskname,result
		move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
		move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
		move	taskname,SmtpTextMessage(2)   Array <Text message >
		move	"Subroutine Save3",SmtpTextMessage(3)	Array <Text message >
		move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
		call	errmesg
		return
	endif
	unpack	NLOL24,str6
	if (str6 <> NLOLLOL)
		loop
			until (str6 = NLOLLOL)
			call	NLOLKGA
			if over
				clear	taskname
				append	NLOLFLD2,taskname
				append	" DELETEK not Found in NINLOL File-NLOLKGA!!",taskname
				reset	taskname
				alert	caution,taskname,result
				move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
				move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
				move	taskname,SmtpTextMessage(2)   Array <Text message >
				move	"Subroutine Save3",SmtpTextMessage(3)	Array <Text message >
				move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
				call	errmesg
				return
			endif
			unpack	NLOL24,str6
		repeat
	endif
	move	"OrderDel.LOL-DELETEK NLOLFLE2",Location
	pack	KeyLocation,"Key: ",NLOLFLD2
	TRAP	IOMssg Giving Error if IO
	DELETEK	NLOLFLE2

.Delete	actual record
	move	C1,NLOLPATH
	pack	NLOLFLD,NLOLLOL
	move	"OrderDel.LOL-NLOLTST",Location
	call	NLOLTST
	if over
		clear	taskname
		append	NLOLFLD,taskname
		append	" Record not Found in NINLOL File-NLOLTST!!",taskname
		append	carr,taskname
		append	"Delete	will not occur!!!",taskname
		reset	taskname
		alert	caution,taskname,result
		return
	endif
	move	"OrderDel.LOL-NLOLDEL",Location
	pack	KeyLocation,"Key: ",NLOLFLD
	call	NLOLDEL
.START PATCH 3.72 ADDED LOGIC
	packkey	NSEL2FLD,"2",NLOLLOL
	move	"OrderDel.LOL-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if not over
		move	"OrderDel.LOL-NSEL2DEL",Location
		call	NSEL2DEL
	endif
.
	packkey	NSEL3FLD1,"01X2",NLOLLOL
	move	"OrderDel.LOL-NSEL3AIM",Location
	pack	KeyLocation,"Key: ",NSEL3FLD1
	call	NSEL3AIM
	loop
		until over
		packkey	NSEL3FLD,NSEL3LRCODE,NSEL3LR,NSEL3CODE,NSEL3NUM
		move	"OrderDel.LOL-NSEL3TST",Location
		call	NSEL3TST
		if not over
			move	"OrderDel.LOL-NSEL3DEL",Location
			call	NSEL3DEL
		endif
		move	"OrderDel.LOL-NSEL3KG",Location
		call	NSEL3KG
	repeat
.END PATCH 3.72 ADDED LOGIC
.START PATCH 3.75 ADDED LOGIC
	call	IntegralDeleteDetail using NLOLLOL,C0
.END PATCH 3.75 ADDED LOGIC
	return
....READ MASTER	FILES....
....Order Screens
RefreshOrderIndexDataList
	count	HowMany,key
	if (HowMany <> "6")
		setprop	ErrorMssg,visible=1
		setprop	NORDMSK1ButtonModify,enabled=0
		setfocus NORDMSK1EditSearchKey
		call	OrderClear
		return
	endif
	move	"OK-1rst Read ORDPRINT",Location
	pack	KeyLocation,"Key: ",KEY
	TRAP	IOMssg Giving Error if IO
	move	key,OLRN
	filepi	1;ORDPRINT
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL
.begin patch 3.79.2   OcompID Ocompid2
	read	ORDPRINT,key;ORpCODE,OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM:
		OLON,OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE:
		ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED:
		OMDTEC,OMDTEY,OMDTEM,OMDTED,OTOCODE,OSOTCODE,OCCODE,OLRNCO:
		OODTECOC,OODTECOY,OODTECOM,OODTECOD:
		OQTYCO,OSPI,OBILDRCT,OBRKGUAR,OELCODE:
		OODNUM,OODES,ONETQTY,OCAMP,OCLRSTAT,OCLRINIT,OBRKRPT,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,ORENT,OHIST,OXPPM,ORTNNUM,OTAPERET,OUQTY,OSALES10,OSALES,OCOCODE,OCO2CODE:
		OODTEC,OODTEY,OODTEM,OODTED,OSCODE,OCOMSLCT,OSHP,O1DES,O2DES:
		OREUSE,ODOWJ,OEXQTY,GUARCODE,OBRKNUM,OBRKCNT:
		OSAMCDE,ONETPER,ONETRC,ONETFM,ONETMIN,OFullFil:
		OCompID:
		OCompID2:
		OFILLER
.end patch 3.79.2   OcompID Ocompid2
.		OSAMCDE,ONETPER,ONETRC,ONETFM,ONETMIN,OFILLER
.End Patch 3.78.8 Modification - Addition of OFULLFIL		
	if not over
		setprop	NORDMSK1ButtonReprint,enabled=0
		move	"ORDER PRINT",FMESG
		move	key,NORDFLD
		match	OLRN,key
		goto	ISAMBAD	if not equal
	else
		setprop	NORDMSK1ButtonReprint,enabled=1
	endif
	TRAPCLR	IO
	pack	NORDFLD,key
	move	C3,NORDLOCK
	move	C1,NORDPATH
	move	"OK-1rst NORDKEY",Location
	pack	KeyLocation,"Key: ",NORDFLD
	call	NORDKEY
	if Over
		setprop	NORDMSK1ButtonReprint,enabled=0
.Change	StatText Boxes For Error Message
		setprop	ErrorMssgStat1,visible=0
		setprop	ErrorMssgStat2,visible=0
		setprop	ErrorMssgStat3,visible=0
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
.Display Error Message
		setprop	ErrorMssg,visible=1
.Reset StatText	Boxes
		call	SetOrderErrorMssgDefault
		call	OrderClear
		setfocus NORDMSK1EditSearchKey
	else
		move	"MASTER	ORDER",FMESG
		match	OLRN,key
		goto	ISAMBAD	if not equal
		call	OrderLoadScreens
.Set up	Spin Key for LR	# to allow quick access	of sequential LR #'s
		unpack	key,str5,str1
.		 move	 "50",N2
.		 setitem NORDMSK1VScrollLR,0,N2
.
		call	OrderEnableModify
	endif
	return
OrderEnableModify
	if (ExitFlag2 =	YES)
		if (ExitFlag3 =	YES)
			if (ExitFlag = YES)
				setprop	NORDMSK1ButtonModify,enabled=1
				setprop	NORDMSK1ButtonCopy,enabled=1
.START PATCH 3.71.3 ADDED LOGIC
				setprop	NORDMSK1ButtonPrint,enabled=1,height=23
.END PATCH 3.71.3 ADDED LOGIC
			endif
		endif
	endif
	return
OrderEnableModify2
	if (ExitFlag = YES)
		if (ExitFlag3 =	YES)
			if (ExitFlag2 =	YES)
				setprop	NORDMSK2ButtonModify,enabled=1
			endif
		endif
	endif
	return
OrderEnableModify3
	if (ExitFlag = YES)
		if (ExitFlag2 =	YES)
			if (ExitFlag3 =	YES)
				setprop	NORDMSK3ButtonModify,enabled=1
				setprop	NORDMSK3ButtonOrder,enabled=1
				setprop	NORDMSK3ButtonLCR,enabled=1
.START PATCH 3.48 ADDED LOGIC
				setprop	NORDMSK3ButtonProj,enabled=1
.END PATCH 3.48 ADDED LOGIC
				setprop	NORDMSK3ButtonCopy,enabled=1
			endif
		endif
	endif
	return
OrderReleaseRecord
.Releases record from Busy mode
.Called	by:  NORDMSK1ButtonQuit_Click, NORDMSK1ButtonCancel_Click
	move	"ORelRec-NORDRELEASE",Location
	pack	KeyLocation,"Key: ",NORDFLD
	call	NORDRELEASE
	return

OrderReleaseLOL
.Releases LOL record from Busy mode
.Called	by:  NORDMSK3ButtonQuit_Click
	move	"ORelRec-NLOLRELEASE",Location
	pack	KeyLocation,"Key: ",NLOLFLD
	call	NLOLRELEASE
	return

OrderReleaseCampaign
.Releases CMP record from Busy mode
.Called	by:  NORDMSK2ButtonQuit_Click
	move	"ORelRec-NCMPRELEASE",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPRELEASE
	return

OrderCloseTempFile LRoutine PTRFile
	IF (LRINIT = 1)
	close	PTRFILE,DELETE
	ENDIF
	return

.OrderCloseLRTempFile
.	 IF (LRINIT = 1)
.	 close	 LRFILE,DELETE
.	 clear	 APIFileName
.	 pack	 APIFileName,"\\NTS0\D\DATA\OPEN\LR",OLRN,".DAT",hexzero
.	 call	 FindFirstFile
.	 if (APIResult <> 0 & APIResult	<> hexeight)
.		 call	 DeleteFile
.		 if (APIResult = 0 | APIResult = hexeight)
.		 endif
.	 endif
.	 ENDIF
.	 return
.OrderCloseLOLTempFile
.	 IF (LRINIT = 1)
.	 close	 LOLFILE,DELETE
.	 clear	 APIFileName
.	 pack	 APIFileName,"\\NTS0\D\DATA\OPEN\LOL",NLOLLOL,".DAT",hexzero
.	 call	 FindFirstFile
.	 if (APIResult <> 0 & APIResult	<> hexeight)
.		 call	 DeleteFile
.		 if (APIResult = 0 | APIResult = hexeight)
.		 endif
.	 endif
.	 return
.	 ENDIF

.OrderCloseCMPTempFile
.	 IF (LRINIT = 1)
.	 close	 CMPFILE,DELETE
.	 clear	 APIFileName
.	 pack	 APIFileName,"\\NTS0\D\DATA\OPEN\CMP",NCMPNUM,".DAT",hexzero
.	 call	 FindFirstFile
.	 if (APIResult <> 0 & APIResult	<> hexeight)
.		 call	 DeleteFile
.		 if (APIResult = 0 | APIResult = hexeight)
.		 endif
.	 endif
.	 return
.	 ENDIF

RefreshOrderCampaign LRoutine FrmPtr
.Called	by:  NORDMSK2ButtonOK
	count	HowMany,key2
	if (HowMany <> "6")
		setprop	ErrorMssg,visible=1
		setprop	NORDMSK2ButtonModify,enabled=0
		setfocus NORDMSK2EditSearchKey
		call	OrderCampaignClear
		return
	endif
	move	"OK-1rst Read NINCMP",Location
	pack	NCMPFLD,key2
	move	C3,NCMPLOCK
	move	C1,NCMPPATH
	move	"OK-1rst NCMPKEY",Location
	pack	KeyLocation,"Key: ",NCMPFLD
	call	NCMPKEY
	if Over
.Change	StatText Boxes For Error Message
		setprop	ErrorMssgStat1,visible=0
		setprop	ErrorMssgStat2,visible=0
		setprop	ErrorMssgStat3,visible=0
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
.Display Error Message
		setprop	ErrorMssg,visible=1
.Reset StatText	Boxes
		call	SetOrderErrorMssgDefault
		call	OrderCampaignClear
		setfocus NORDMSK2EditSearchKey
	else
		if (FrmPtr = C1)
			call	OrderLoadCampaignScreens
		else
			call	OrderLoadCampaignScreen7
		endif
		call	OrderEnableModify2
	endif
	return

RefreshOrderCampaignAam
.Called	by:  NORDMSK2ButtonOK,NORDMSK2ButtonFind
.Clear DataList
	NORDMSK2ListView.DeleteAllItems giving N9
	call	OrderLOLDetailClear
	move	C0,N5
	move	"Driver-NCMPAIM",Location
	pack	KeyLocation,"Key: ",NCMPFLD1,NCMPFLD2,NCMPFLD3,NCMPFLD4
	call	NCMPAIM
	if Over
.Change	StatText Boxes For Error Message
		setprop	ErrorMssgStat1,visible=0
		setprop	ErrorMssgStat2,visible=0
		setprop	ErrorMssgStat3,visible=0
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
.Display Error Message
		setprop	ErrorMssg,visible=1
.Reset StatText	Boxes
		call	SetOrderErrorMssgDefault
		if (NewFlag2 = "F")	 .Search mode using Find button
			setprop	NORDMSK2ButtonFind,enabled=1
			setprop	NORDMSK2ButtonQuit,enabled=1
			setfocus Nord01eaEditCampName
			move	"S",NewFlag2
		else			.AamKey	Search using OK	button
			setprop	NORDMSK2ButtonOK,enabled=1
			setfocus NORDMSK2EditSearchKey
			call	OrderCampaignClear
		endif
		return
	endif
	call	CampaignFindPackIt
.START PATCH 3.41 REMOVED LOGIC
..Set up Icon to display while searching
.	 moveaddr nord0001,AnimateWindow
.	 move	 C1,AnimateCurIcon
.	 move	 C4,AnimateFrames
.	 move	 C0,AnimateIconID
..position Icon	to sit to left of FindIt button
.	 move	 AniH,H
.	 move	 AniV,V
.END PATCH 3.41	REMOVED	LOGIC
	if (NewFlag2 = "F")	 .Search mode
		setprop	NORDMSK2ButtonQuit,enabled=1
	endif
	loop
.START PATCH 3.41 REMOVED LOGIC
.		 call	 ANIMATEIT
.END PATCH 3.41	REMOVED	LOGIC
		move	"Driver-NCMPAIM",Location
		pack	KeyLocation,"Key: ",NCMPFLD1,NCMPFLD2,NCMPFLD3,NCMPFLD4
		call	NCMPKG
		until over
		call	CampaignFindPackIt
		eventcheck
		until (NewFlag2	= "S")
	repeat
.START PATCH 3.41 REMOVED LOGIC
.	 destroy  AnimateIcon
.END PATCH 3.41	REMOVED	LOGIC
	clear	 str55
	move	 N5,str5
	call	 Trim using str5
	append	 "Total	Campaigns:  ",str55
	append	 str5,str55
	reset	 str55
	setitem	 Nord01eaStatTotalNames,0,str55
	NORDMSK2ListView.SetItemState GIVING N9 USING *Index=0,*State=2,*Statemask=2
	NORDMSK2ListView.EnsureVisible using 0,0
	setfocus NORDMSK2ListView
.Modify	Screen
	call	 OrderDisableCampLower
	call	 OrderEnableCampUpper
.Reset NewFlag
	move	 "N",NewFlag2
.IMPORTANT - KEEP THIS!!! Clears acumulated LostFocus, GotFocus, etc. events
.that have gathered up from Enabling\Disabling Objects
	LOOP
		CLEAREVENT
		UNTIL OVER
	REPEAT
	goto	OrderListViewClick2

CampaignFindPackIt
	clear	hold6
	pack	hold6,NCMPVARS
	move	NCMPNUM,key2
	move	NCMPNUM,holdkey2
	move	NCMPNUM,NCMPFLD
	NORDMSK2ListView.InsertItem giving N9 using NCMPNUM
	NORDMSK2ListView.SetItemText using N9,NCMPCNAME,1
	NORDMSK2ListView.SetItemText using N9,NCMPMLR,2
	NORDMSK2ListView.SetItemText using N9,NCMPBRK,3
	unpack	NCMPDATE,str2,YY,MM,DD
	call	Trim using MM
	if (MM <> "")
		pack	newdate1,MM,SLASH,DD,SLASH,str2,YY
	else
		clear	newdate1
	endif
	NORDMSK2ListView.SetItemText using N9,newdate1,4
	NORDMSK2ListView.SetItemText using N9,hold6,5
	NORDMSK2ListView.EnsureVisible using N9,0
	add	C1,N5			.Campaign Total
	return

RefreshOrderLOL
.Called	by:  NORDMSK3ButtonOK
	count	HowMany,key3
	if (HowMany <> "12")
		setprop	ErrorMssg,visible=1
		setprop	NORDMSK3ButtonModify,enabled=0
.
		setprop	NORDMSK3ButtonOrder,enabled=0
		setprop	NORDMSK3ButtonLCR,enabled=0
.START PATCH 3.48 ADDED LOGIC
		setprop	NORDMSK3ButtonProj,enabled=0
.END PATCH 3.48 ADDED LOGIC
		setfocus NORDMSK3EditSearchKey
		call	OrderLOLClear
		return
	endif
	move	"OK-1rst Read NINLOL",Location
	pack	NLOLFLD1,key3
	move	C3,NLOLLOCK
	move	C2,NLOLPATH
	move	"OK-1rst NLOLKEY",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	call	NLOLKEY
	if Over
.Change	StatText Boxes For Error Message
		setprop	ErrorMssgStat1,visible=0
		setprop	ErrorMssgStat2,visible=0
		setprop	ErrorMssgStat3,visible=0
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
.Display Error Message
		setprop	ErrorMssg,visible=1
.Reset StatText	Boxes
		call	SetOrderErrorMssgDefault
		call	OrderLOLClear
		setfocus NORDMSK3EditSearchKey
	else
		call	OrderLoadLOLScreen
		call	OrderEnableModify3
	endif
	return

RefreshOrderLOLAam
.Called	by:  NORDMSK3ButtonOK,NORDMSK3ButtonFind
.Clear DataList
	NORDMSK3ListView.DeleteAllItems giving N9
	move	C0,N5
	move	"Driver-NLOLAIM",Location
	pack	KeyLocation,"Key: ",NLOLFLD2
	move	C3,NLOLPATH
	call	NLOLAIM
	if Over
.Change	StatText Boxes For Error Message
		setprop	ErrorMssgStat1,visible=0
		setprop	ErrorMssgStat2,visible=0
		setprop	ErrorMssgStat3,visible=0
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
.Display Error Message
		setprop	ErrorMssg,visible=1
.Reset StatText	Boxes
		call	SetOrderErrorMssgDefault
		if (NewFlag3 = "F")	 .Search mode using Find button
			setprop	NORDMSK3ButtonFind,enabled=1
			setprop	NORDMSK3ButtonQuit,enabled=1
			setfocus Nord01ECEditCamp
			move	"S",NewFlag3
		else			.AamKey	Search using OK	button
			setprop	NORDMSK3ButtonOK,enabled=1
			setfocus NORDMSK3EditSearchKey
			call	OrderLOLClear
		endif
		return
	endif
	call	LOLFindPackIt
.START PATCH 3.41 REMOVED LOGIC
..Set up Icon to display while searching
.	 moveaddr nord0001,AnimateWindow
.	 move	 C1,AnimateCurIcon
.	 move	 C4,AnimateFrames
.	 move	 C0,AnimateIconID
..position Icon	to sit to left of FindIt button
.	 move	 AniH,H
.	 move	 AniV,V
.END PATCH 3.41	REMOVED	LOGIC
.Commented out following decision statement as user should be able to stop a regular search if,	say, they search on "2700" and get thousands of	entries	which use Sierra Club
.	 if (NewFlag3 =	"F")	  .Search mode
		setprop	NORDMSK3ButtonQuit,enabled=1
.	 endif
	loop
.START PATCH 3.41 REMOVED LOGIC
.		 call	 ANIMATEIT
.END PATCH 3.41	REMOVED	LOGIC
		move	"Driver-NLOLKG",Location
		pack	KeyLocation,"Key: ",NLOLFLD2
		call	NLOLKG
		until over
		call	LOLFindPackIt
		eventcheck
		until (NewFlag3	= "S")
	repeat
.START PATCH 3.41 REMOVED LOGIC
.	 destroy AnimateIcon
.END PATCH 3.41	REMOVED	LOGIC
	clear	str55
	move	N5,str5
	call	Trim using str5
	append	"Total Records:	 ",str55
	append	str5,str55
	reset	str55
	setitem	Nord01ECStatTotalNames,0,str55
............
	move	C0,howmany
.This happens because List View	sorts itself and therefore index will change after loading.
	if (NLOLFLD <> "")
		move	SEQ,result
		NORDMSK3ListView.GetItemCount giving N10
		loop
			NORDMSK3ListView.GetNextItem giving N9 using C0,result
			until (result >	N10)	    .Essentially an over
			NORDMSK3ListView.GetItemText giving hold7	using N9,4
			unpack	hold7,str2,NLOLLOL
			if (NLOLLOL = NLOLFLD)
				move	N9,howmany
			endif
			until (NLOLLOL = NLOLFLD)
			add	C1,result
		repeat
	endif
	NORDMSK3ListView.SetItemState GIVING N9 USING *Index=howmany,*State=2,*Statemask=2
............
.	 NORDMSK3ListView.SetItemState GIVING N9 USING *Index=0,*State=2,*Statemask=2
	NORDMSK3ListView.EnsureVisible using howmany,0
	setfocus NORDMSK3ListView
.Modify	Screen
	call	OrderDisableLOLLower
	call	OrderEnableLOLUpper
.Reset NewFlag
	move	"N",NewFlag3
.IMPORTANT - KEEP THIS!!! Clears acumulated LostFocus, GotFocus, etc. events
.that have gathered up from Enabling\Disabling Objects
	LOOP
		CLEAREVENT
		UNTIL OVER
	REPEAT
	goto	OrderListViewClick3

LOLSetListView
.	 NORDMSK3ListView.FindItem giving	howmany	using 6,NLOLFLD
.	 if (howmany <>	SEQ)
.		 NORDMSK3ListView.SetItemState GIVING N9 USING *Index=howmany,*State=2,*Statemask=2
.	 endif
	move	SEQ,result
	move	C0,howmany
	NORDMSK3ListView.GetItemCount giving N10
.This happens because List View	sorts itself and therefore index will change after loading.
	loop
		NORDMSK3ListView.GetNextItem giving N9 using C0,result
		until (N9 > N10)	.Essentially an	over
		NORDMSK3ListView.GetItemText giving hold7	using N9,4
		unpack	hold7,str2,NLOLLOL
		if (NLOLLOL = NLOLFLD)
			move	N9,howmany
		endif
		until (NLOLLOL = NLOLFLD)
		add	C1,result
	repeat
	NORDMSK3ListView.SetItemState GIVING N9 USING *Index=howmany,*State=2,*Statemask=2
	return

LOLFindPackIt
	if (NewFlag3 = "F")	 .Search mode using Find button
		call	LOLFindLastTest	using str1
		if (str1 = NO)
			return
		endif
	endif
	clear	hold8
	pack	hold8,NLOLVARS
.Commented out following line as I think it is wreaking	havoc -	12/6/00, ASH
.	 pack	 key3,NLOLCNUM,NLOLLIST
	pack	holdkey3,NLOLCNUM,NLOLLIST
	pack	NDATFLD,NLOLLIST
	move	C1,NDATPATH
	move	"LOLFind-NDATKEY",Location
	pack	KeyLocation,"Key: ",NDATFLD
	call	NDATKEY
.	 NORDMSK3ListView.SetColumnWidth using 0,200
	NORDMSK3ListView.InsertItem giving N9 using OLSTNAME
	NORDMSK3ListView.SetItemText using N9,NLOLCNUM,1
.START PATCH 3.72 REPLACED LOGIC
.	NORDMSK3ListView.SetItemText using N9,NLOLSELECT,2
	pack	NSEL2FLD,"2",NLOLLOL
	move	"LOLFind-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	NLOLSELECT,NSEL2NAME
	endif
	NORDMSK3ListView.SetItemText using N9,NSEL2NAME,2
.END PATCH 3.72 REPLACED LOGIC
	NORDMSK3ListView.SetItemText using N9,NLOLPACKAGE,3
.	 NORDMSK3ListView.SetItemText using N9,NLOLLOL,5
	NORDMSK3ListView.SetItemText using N9,hold8,4
	NORDMSK3ListView.EnsureVisible GIVING n10	using N9,0
	add	C1,N5			.LOL Record Total
	return

LOLFindLastTest	LRoutine DimPtr
	move	YES,DimPtr	.Initialize value
	if (Hold3Pack <> "" & Hold3Pack	<> NLOLPACKAGE)
		move	NO,DimPtr
	else
		move	C1,NCMPPATH
		pack	NCMPFLD,NLOLCNUM
		move	"LOLFindLast-NCMPKEY",Location
		pack	KeyLocation,"Key: ",NLOLCNUM
		call	NCMPKEY
		if not over
			call	Trim using NCMPMLR
.START PATCH 3.75.7 REPLACED LOGIC
.			if (Hold3Mlr <>	"" & Hold3Mlr <> NCMPMLR)
			pack	COMPFLD,NCMPMLR
			move	"LOLFindLast-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
			call	Trim using COMPOLDMLR
			if (Hold3Mlr <>	"" & Hold3Mlr <> COMPOLDMLR)
.END PATCH 3.75.7 REPLACED LOGIC
				move	NO,DimPtr
			else
				call	Trim using NCMPPO
				if (Hold3PO <> "" & Hold3PO <> NCMPPO)
					move	NO,DimPtr
				else
					call	Trim using NCMPPLANNER
					if (Hold3Plan <> "" & Hold3Plan	<> NCMPPLANNER)
						move	NO,DimPtr
					endif
				endif
			endif
		else	.really	not possible, but might	as well
			if (Hold3Mlr <>	"" | Hold3PO <>	"" | Hold3Plan <> "")
				move	NO,DimPtr
			endif
		endif
	endif
	return
.....................................
.Universal Event Logic
.....................................

.......Click Events......

....Lost Focus Events....
CampaignLostFocus LRoutine FrmPtr
	if (str7 = str6)
		return
	endif
CampaignLostFocus1 LRoutine FrmPtr
	clear	NCMPFLD
.I want	the display to be blank	but I also want	the over flag triggered,
.so I use a key	I know will produce an over.
	if (str6 = "")
		move	badstat,str6
	endif
	move	C1,NCMPPATH
	pack	NCMPFLD,str6
	move	"Camp.LostFocus-NCMPKEY",Location
	pack	KeyLocation,"Key: ",str6
	call	NCMPKEY
	if (FrmPtr = C8)
.Campaign has changed, pull default values.  User has no choice!!!
		setitem	Nord01ECStatCampName,0,NCMPCNAME
.START PATCH 3.75.7 REPLACED LOGIC
.		setitem	Nord01ECEditMlr,0,NCMPMLR
..START PATCH 3.74.1 REPLACED LOGIC - ASH 11AUG2004
..		move	NCMPMLR,str4
..		move	"000",str3
.		pack	MKEY,NCMPMLR,"000"
..END PATCH 3.74.1 REPLACED LOGIC - ASH 11AUG2004
.....................................
		pack	COMPFLD,NCMPMLR
		move	"Camp.LostFocus-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		setitem	Nord01ECEditMlr,0,COMPOLDMLR
		pack	MKEY,COMPOLDMLR,"000"
.END PATCH 3.75.7 REPLACED LOGIC
		call	OrderLoadMailer	using C8
		setitem	Nord01ECEditPO,0,NCMPPO
		if (NewFlag3 <>	"S")
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.			setitem	Nord01ECEditBrk,0,NCMPBRK
.			setitem	Nord01ECEditBrkContact,0,NCMPBRKCNT
.			pack	NBRKFLD,NCMPBRK,NCMPBRKCNT
.....................................................
			pack	CNCTFLD,NCMPBRK,NCMPBRKCNT
			rep	zfill,CNCTFLD
			move	"Camp.LostFocus-CNCTKEY",Location
			pack	KeyLocation,"Key: ",CNCTFLD
			call	CNCTKEY
			if not over
				pack	NBRKFLD,CNCTCNT
				unpack	CNCTCNT,str4,str3
			else
				pack	COMPFLD,NCMPBRK
				move	"Camp.LostFocusB-COMPKEY",Location
				pack	KeyLocation,"Key: ",COMPFLD
				call	COMPKEY
				pack	NBRKFLD,COMPOLDBRK,"000"
				unpack	NBRKFLD,str4,str3
			endif
			setitem	Nord01ECEditBrk,0,str4
			setitem	Nord01ECEditBrkContact,0,str3
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
			call	OrderLoadBroker	using C8
		endif
		call	TRIM using NCMPPLANNER
		call	OrderLoadCombo Using Nord01ECComboPlanner,NCMPPLANNER
		if (NewFlag3 <>	"S")
			call	TRIM using NCMPCNT
			call	OrderLoadCombo Using Nord01ECComboContact,NCMPCNT
.START PATCH 3.75.7 ADDED LOGIC
			pack	COMPFLD,NCMPMLR
			move	"Camp.LostFocusB-COMPKEY",Location
			pack	KeyLocation,"Key: ",COMPFLD
			call	COMPKEY
.END PATCH 3.75.7 ADDED LOGIC
.Following fields have optional	masking	from new Campaign!!
			clear	taskname
			append	"Do you	wish the following values",taskname
			append	carr,taskname
			append	"masked	from the Campaign file?",taskname
			append	carr,taskname
			append	"Mail Date",taskname
			append	carr,taskname
			append	"Offer",taskname
			append	carr,taskname
			append	"Sample",taskname
			append	carr,taskname
			append	"Package",taskname
			reset	taskname
			alert	Type=style1,taskname,result
			if (result = 6)
.Following same	message	box with 'No' button set as default
.			 alert	 Type=style,taskname,result
.			 if (result = 6) .Yes
				call	Trim using NCMPMDATE
				if (NCMPMDATE <> "")
       					unpack	NCMPMDATE,CC,YY,MM,DD
					pack	newdate1,MM,SLASH,DD,SLASH,CC,YY
				else
					clear	newdate1
				endif
				setitem	Nord01ECEditMailDate,0,newdate1
.START PATCH 3.75.7 REPLACED LOGIC
.				pack	str7,NCMPMLR,NCMPOFFER
.				move	NCMPMLR,str4
				pack	str7,COMPOLDMLR,NCMPOFFER
				move	COMPOLDMLR,str4
.END PATCH 3.75.7 REPLACED LOGIC
				call	OrderLoadOffer using C8
				move	NCMPSAMPLE,NLOLSAMPLE
				call	OrderLoadSamples using C8
			else
.START PATCH 3.75.7 REPLACED LOGIC
.				pack	str7,NCMPMLR,"000"
.				move	NCMPMLR,str4
				pack	str7,COMPOLDMLR,"000"
				move	COMPOLDMLR,str4
.END PATCH 3.75.7 REPLACED LOGIC
				call	OrderLoadOffer using C8
				move	"000",NLOLSAMPLE
				call	OrderLoadSamples using C8
			endif
		endif
	endif
	return

MailerLostFocus	LRoutine FrmPtr
.Called	by: CampaignLostFocus
	clear	MKEY
.I want	the display to be blank	but I also want	the over flag triggered,
.so I use a key	I know will produce an over.
	if (str4 = "")
		move	badstat,str4
	endif
.Only do all this processing if	a change was made!
.str5 is gathered at GotFocus event, holds value prior to possible changes.
	if (str4 <> str5)	.value has changed
		pack	MKEY,str4,str3
		call	OrderLoadMailer	using FrmPtr
		if (FrmPtr = C1)
			if (NewFlag <> "S")	.NOT Search mode
				call	OrderSetObildrct using FrmPtr
				type	MSLSPER
				if equal
					rep	zfill,MSLSPER
					unpack	MSLSPER,OSALES10,OSALES
					call	OrderLoadSalesperson using C1
				endif
				clear	str7	.prep before loading offers
.If current value is equal to OMLRNUM, save Offer field.
.(Covers instances when	there is modification of a current record.)
.START PATCH 3.71.6 REPLACED LOGIC
.				if (str4 = OMLRNUM)
				if (str4 = OMLRNUM & NewFlag <> YES)
.END PATCH 3.71.6 REPLACED LOGIC
					move	OODNUM,str7	.Reload	and highlight original offers
					pack	MBRKNUM,OBRKNUM,OBRKCNT	.Reload	original Broker
				endif
				call	OrderLoadOffer using FrmPtr
.str4 already loaded with new OMLRNUM value
				call	OrderLoadSamples using FrmPtr
.START PATCH 3.71.6 REPLACED LOGIC
.				if (str4 = OMLRNUM)
				if (str4 = OMLRNUM & NewFlag <> YES)
.END PATCH 3.71.6 REPLACED LOGIC
					move	OSAMCDE,str4
					move	OSCODE,str3
				else	.necessary as str4 originally will hold	Mailer Number
					clear	str4	.OSAMCDE
					clear	str3	.OSCODE
				endif
				call	OrderLoadSamples2
.If not	in Search mode,	Load associated	Broker if there	is one,	otherwise don't	touch field!
				getitem	Nord001AEditBrk,0,str4
				getitem	Nord001AEditBrkContact,0,str3
				pack	str7,str4,str3
				call	Trim using MBRKNUM
				if (str7 <> MBRKNUM)
					unpack	MBRKNUM,str4,str3
					setitem	Nord001AEditBrk,0,str4
					setitem	Nord001AEditBrkContact,0,str3
.Subroutine is obviously found at Nord001AEditBrk_LostFocus!!
					call	BrokerLostFocus	using Nord001AEditBrk,Nord001AEditBrkContact,FrmPtr
				endif
				call	RtnToLostFocus using FrmPtr
			else
.Allow searching using Offer
				call	OrderLoadOffer using FrmPtr
			endif
		elseif (FrmPtr = C6)
			if (NewFlag2 <>	"S")	.NOT Search mode
				call	OrderSetObildrct using FrmPtr
				clear	str7	.prep before loading offers
.If current value is equal to NCMPMLR, save Offer field.
.(Covers instances when	there is modification of a current record.)
.START PATCH 3.75.7 REPLACED LOGIC
.				if (str4 = NCMPMLR)
TESTER
				pack	COMPFLD,NCMPMLR
.START PATCH 3.75.8 REPLACED LOGIC
.				move	"MLostFocus-COMPKEY",Location
.				pack	KeyLocation,"Key: ",COMPFLD
.				call	COMPKEY
				call	Trim using COMPFLD
				if (COMPFLD <> "")
					move	"MLostFocus-COMPKEY",Location
					pack	KeyLocation,"Key: ",COMPFLD
					call	COMPKEY
				else
					clear	COMPOLDMLR
				endif
.END PATCH 3.75.8 REPLACED LOGIC
				if (str4 = COMPOLDMLR)
.END PATCH 3.75.7 REPLACED LOGIC
					move	NCMPOFFER,str7			.Reload	and highlight original offers
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.					pack	MBRKNUM,NCMPBRK,NCMPBRKCNT	.Reload	original Broker
					pack	CNCTFLD,NCMPBRK,NCMPBRKCNT
					move	"MLostFocus-CNCTKEY",Location
					pack	KeyLocation,"Key: ",CNCTFLD
					call	CNCTKEY
					pack	MBRKNUM,CNCTCNT
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
				endif
				call	OrderLoadOffer using FrmPtr
.str4 already loaded with new NCMPMLR value
				call	OrderLoadSamples using FrmPtr
.If not	in Search mode,	Load associated	Broker if there	is one,	otherwise don't	touch field!
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.				getitem	Nord01eaEditBrk,0,str4
.				getitem	Nord01eaEditBrkContact,0,str3
.				pack	str7,str4,str3
				getitem	Nord01eaEditBrk,0,str6
				getitem	Nord01eaEditBrkContact,0,str3
				pack	CNCTFLD,str6,str3
.START PATCH 3.75.8 REPLACED LOGIC
.				move	"MLostFocusB-CNCTKEY",Location
.				pack	KeyLocation,"Key: ",CNCTFLD
.				call	CNCTKEY
				call	Trim using CNCTFLD
				if (CNCTFLD <> "")
					move	"MLostFocusB-CNCTKEY",Location
					pack	KeyLocation,"Key: ",CNCTFLD
					call	CNCTKEY
				else
					clear	CNCTCNT
				endif
.END PATCH 3.75.8 REPLACED LOGIC
				pack	str7,CNCTCNT
TESTER2
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
				call	Trim using MBRKNUM
				if (str7 <> MBRKNUM)
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
.					unpack	MBRKNUM,str4,str3
.					setitem	Nord01eaEditBrk,0,str4
.					setitem	Nord01eaEditBrkContact,0,str3
					pack	CNCTFLD4,MBRKNUM
					move	"MLostFocus-CNCTKEY2",Location
					pack	KeyLocation,"Key: ",CNCTFLD4
					call	CNCTKEY2
					setitem	Nord01eaEditBrk,0,CNCTCODE
					setitem	Nord01eaEditBrkContact,0,CNCTID
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY LOGIC
					call	BrokerLostFocus	using Nord01eaEditBrk, Nord01eaEditBrkContact, FrmPtr
				endif
				call	RtnToLostFocus using FrmPtr
			endif
		endif
	endif
	return

BrokerLostFocus	LRoutine EditPtr, EditPtr1, FrmPtr
.Mimics	Generic	Broker_LostFocus Event
.Called	by: Nord001AEditBrk_LostFocus, Nord01eaEditBrk_LostFocus, CampaignLostFocus
.START PATCH 3.75.7 REPLACED LOGIC
.	getitem	EditPtr,0,str4
.	call	ZFILLIT	using str4,C0
.	setitem	EditPtr,0,str4
.	getitem	EditPtr1,0,str3
.....................................
	if (FrmPtr = C6)
		getitem	EditPtr,0,str6
		call	ZFILLIT	using str6,C0
		setitem	EditPtr,0,str6
		getitem	EditPtr1,0,str3
		pack	CNCTFLD,str6,str3
		if (CNCTFLD = "")
			pack	CNCTFLD,badstat,badstat,badstat
		endif
		move	"B.LostFocus-CNCTKEY",Location
		pack	KeyLocation,"Key: ",CNCTFLD
		call	CNCTKEY
		unpack	CNCTCNT,str4,str3
	else
		getitem	EditPtr,0,str4
		call	ZFILLIT	using str4,C0
		setitem	EditPtr,0,str4
		getitem	EditPtr1,0,str3
	endif
.END PATCH 3.75.7 REPLACED LOGIC
	call	Trim using str3
	if (str3 = "")
		move	"000",str3
	endif
	clear	NBRKFLD
.I want	the display to be blank	but I also want	the over flag triggered
	if (str4 = "")
		move	badstat,str4
	endif
.Only do all this processing if	a change was made!
.str5 is gathered at GotFocus event, holds value prior to possible changes.
	if (str4 <> str5)	.value has changed
		pack	NBRKFLD,str4,str3
		call	OrderLoadBroker	using FrmPtr
		if (mod	<> C5 AND mod <> C6 AND	NewFlag	<> "S")
			type	BRSALES
			if equal
				rep	zfill,BRSALES
				unpack	BRSALES,OSALES10,OSALES
				if (FrmPtr = C1)
.START PATCH 3.72.6 REPLACED LOGIC
.					call	OrderLoadSalesperson using C1
					getitem	Nord001AEditMlr,0,str4
					pack	MKEY,str4,"000"
					move	"O.MailerSalesContactRead-NMLRKEY",Location
					pack	KeyLocation,"Key: ",MKEY
					call	NMLRKEY
					type	MSLSPER
.					if Equal
.						if (MSLSPER = "00")
.							call	OrderLoadSalesperson using C1
.						endif
.					else
.							call	OrderLoadSalesperson using C1
.					endif
					if Equal
						if (MSLSPER <> "00")
							unpack	MSLSPER,OSALES10,OSALES
						endif
					endif
					call	OrderLoadSalesperson using C1
.END PATCH 3.72.6 REPLACED LOGIC
				endif
			endif
		endif
	endif
	return

BrokerContactLostFocus LRoutine	EditPtr, EditPtr1, FrmPtr
.Mimics	Generic	BrokerContact_LostFocus	Event
.Called	by:  Nord001AEditBrkContact, Nord01eaEditBrkContact
.str4 set to Broker Contact at OrderXEditBrkContact_GotFocus
	if (str3 <> str4)
.START PATCH 3.75.7 REPLACED LOGIC
.		getitem	EditPtr,0,str4
.		call	Trim using str4
..I want the display to be blank	but I also want	the over flag triggered
.		if (str4 = "")
.			setitem	EditPtr1,0,""
.			move	badstat,str4
.		else
.			setitem	EditPtr1,0,str3
.		endif
.		clear	NBRKFLD
.		pack	NBRKFLD,str4,str3
.		call	OrderLoadBroker	using FrmPtr
.....................................
		setitem	EditPtr1,0,str3
		if (FrmPtr = C6)
			getitem	EditPtr,0,str6
			call	Trim using str6
			if (str6 <> "")
				pack	CNCTFLD,str6,str3
				rep	zfill,CNCTFLD
				move	"BC.LostFocus-CNCTKEY",Location
				pack	KeyLocation,"Key: ",CNCTFLD
				call	CNCTKEY
				call	Trim using CNCTCNT
				unpack	CNCTCNT,str4,str3
			endif
		else
			getitem	EditPtr,0,str4
		endif
		call	Trim using str4
.I want	the display to be blank	but I also want	the over flag triggered
		if (str4 = "")
			setitem	EditPtr1,0,""
			move	badstat,str4
		endif
		clear	NBRKFLD
		pack	NBRKFLD,str4,str3
		call	OrderLoadBroker	using FrmPtr
.END PATCH 3.75.7 REPLACED LOGIC
	endif
	return
RtnToLostFocus LRoutine	FrmPtr
	if (FrmPtr = C1)
		getitem	Nord001AEditRtn,0,str6
	elseif (FrmPtr = C6)
		getitem	Nord01eaEditRtn,0,str6
	elseif (FrmPtr = C8)
		getitem	Nord01ECEditRtn,0,str6
	endif
	call	Trim using str6
	count	result,str6
.Pack number with preceding zeroes
	if (result < 4 AND result <> C0)
		sub	result from "4"	giving N1
		setlptr	filler,	N1
		pack	str4,filler,str6
		move	str4,str6
		reset	str6
		if (FrmPtr = C1)
			setitem	Nord001AStatRtn,0,"Ship-To ##"
			setitem	Nord001AEditRtn,0,str6
		elseif (FrmPtr = C6)
			setitem	Nord01eaEditRtn,0,str6
		elseif (FrmPtr = C8)
			setitem	Nord01ECEditRtn,0,str6
		endif
	elseif (result = 6)	.OREUSE
		if (FrmPtr = C1)
			setitem	Nord001AStatRtn,0,"Re-use	LR"
			setitem	Nord001AEditRtn,0,str6
			move	"0001",str6
		endif
	elseif (result = 4)
		if (FrmPtr = C1)
			setitem	Nord001AStatRtn,0,"Ship-To ##"
			setitem	Nord001AEditRtn,0,str6
		elseif (FrmPtr = C6)
			setitem	Nord01eaEditRtn,0,str6
		elseif (FrmPtr = C8)
			setitem	Nord01ECEditRtn,0,str6
		endif
	endif
	clear	NRTNFLD
	pack	NRTNFLD,str6
	move	"Rtn.L.Focus-NRTNKEY",Location
	pack	KeyLocation,NRTNFLD
	call	NRTNKEY
	if over
		call	OrderClearRtn using FrmPtr
	elseif (NRTNFLD	= "")
		call	OrderClearRtn using FrmPtr
.begin patch xxx
	Elseif	(RTActive = "N")
		alert	caution,"This record is marked INACTIVE!!",result
		call	OrderClearRtn using FrmPtr
.end patch xxx
	else
		if (FrmPtr = C1)
			getitem	Nord001AEditMlr,0,str4
			call	OrderLoadRtn using FrmPtr
		elseif (FrmPtr = C6)
			setitem	Nord01eaStatRtnComp,0,RTCOMP
		elseif (FrmPtr = C8)
			setitem	Nord01ECStatRtnComp,0,RTCOMP
		endif

	endif
	return
OrderClearRtn LRoutine FrmPtr
	if (FrmPtr = C1)
		setitem	Nord001AStatRtnComp,0,""
		setitem	Nord001AStatRtnComp2,0,""
		setitem	Nord001AStatRtnAdd1,0,""
		setitem	Nord001AStatRtnAdd2,0,""
	elseif (FrmPtr = C6)
		setitem	Nord01eaStatRtnComp,0,""
.		 setitem Nord1eaStatRtnComp2,0,""
.		 setitem Nord1eaStatRtnAdd1,0,""
.		 setitem Nord1eaStatRtnAdd2,0,""
	elseif (FrmPtr = C8)
		setitem	Nord01ECStatRtnComp,0,""
	endif
	return

OrderDecimalLostFocus LRoutine EditPtr
	getitem	EditPtr,0,str6
.START PATCH 3.64 REPLACED LOGIC
.	move	str6,N32
.	move	N32,str6
	call	Trim using str6
	if (str6 <> "")
		move	C0,N32
		move	str6,N32
		move	N32,str6
	endif
.END PATCH 3.64 REPLACED LOGIC
	setitem	EditPtr,0,str6
	return

..............................................................
.HOUSEKEEPING FOR GUI
..............................................................
.OrderInfoClose
.	 setprop OrderInfo,visible=0
.	 setprop OrderInfo,winpos=3
.	 setprop OrderInfo,height=100
.	 setprop OrderInfoStatText1,fgcolor=black
.	 setprop OrderInfoStatText5,fgcolor=black
.	 destroy OrderInfoListView
.	 destroy OrderInfoEditText
.	 return

OrderSwitchTab LRoutine	FrmPtr
	if (TabNum <> FrmPtr)
		move	TabNum,N2
		call	OrderTabClick
		move	FrmPtr,N2
		call	OrderTabChange
		setitem	nord0001TabControlTop,0,FrmPtr
	endif
	return

OrderTabClick
.Force LostFocus event for fields when switching tabs.
.This is done so that fields found on other forms that require data
.established through LostFocus events will be set.
.Switching to another tab does not affect the focus on that
.particular form!  LostFocus events must be triggered!
..TEST PATCH
.	RESET	REVTYPS
.	SCAN	INITS,REVTYPS
.	IF NOT EQUAL
.      		MOVE	N2,TABTESTNUM
.	ENDIF
.
	if (N2 = C1)
		setfocus   Nord001ACheckMode	.Benign	yet important OBJECT
		Deactivate Nord001a
.START PATCH 3.72 ADDED LOGIC
		Deactivate Nord001a2
		Deactivate Nord001a1
.END PATCH 3.72 ADDED LOGIC
		setprop	   nord0001ButtonPrevious,visible=1
	elseif (N2 = C2)
		setfocus   Nord001bEditSales	.Not terribly important	field
		Deactivate Nord001b
	elseif (N2 = C3)
		Deactivate Nord001c
	elseif (N2 = C4)
		Deactivate Nord001g
	elseif (N2 = C5)
		Deactivate Nord001d
		setprop	Nord001DButtonOk,default=0
		setprop	NORDMSK1ButtonOk,default=1
	elseif (N2 = C6)
		Deactivate Nord01ea
	elseif (N2 = C7)
		Deactivate Nord01eb
	elseif (N2 = C8)
		Deactivate Nord01ec
.START PATCH 3.72 ADDED LOGIC
		Deactivate Nord008a1
.END PATCH 3.72 ADDED LOGIC
		setprop	nord0001ButtonPrevious,visible=1
		setprop	nord0001ButtonNext,visible=1
	elseif (N2 = C9)
		call	OrderPackageDisableForm
		setprop	nord0001ButtonPrevious,visible=1
		setprop	   nord0001ButtonNext,visible=1
	else	.(N2 = C10)
		Deactivate Nsta0001
		deactivate NSTA0002
		Deactivate Nsta001a
		Deactivate Nsta001b
		Deactivate Nsta001c
		Deactivate Nsta001d
		Deactivate Nsta001e
		setprop	nord0001ButtonPrevious,visible=1
		setprop	   nord0001ButtonNext,visible=1
	endif
	return

.OrderStatTabClick
.	 if (N3	= C1)
.		 Deactivate Nsta001a
.	 elseif	(N3 = C2)
.		 Deactivate Nsta001b
.	 endif
.	 return

OrderTabChange
..TEST PATCH
.	IF (N2 >= 9)
.		RESET	REVTYPS
.		SCAN	INITS,REVTYPS
.		IF NOT EQUAL
.			SETITEM	nord0001TabControlTop,0,TABTESTNUM
.			MOVE	TABTESTNUM,N2
.		ENDIF
.	ENDIF
.
	move	N2,TabNum
.Following section governs which File Maintenance Buttons will be used.
.START PATCH 3.72.4 ADDED LOGIC
	if (TabNum > 4)
		setprop	Nord001t,visible=0
	endif
.END PATCH 3.72.4 ADDED LOGIC
	if (TabNum <= 4)
		if (MaskFlag <>	1)
			if (MaskFlag = 2)
				Deactivate NordMSK2
			elseif (MaskFlag = 3)
				Deactivate NordMSK3
			endif
			Activate NordMSK1
			move	C1,MaskFlag
			getprop	NORDMSK1EditSearchKey,enabled=N3
			if (N3 = 1)
				setfocus NORDMSK1EditSearchKey
			endif
.START PATCH 3.72.4 ADDED LOGIC
			if (SrchWinFlag = 1)
				setprop	Nord001t,visible=1
			endif
.END PATCH 3.72.4 ADDED LOGIC
		endif
	elseif (TabNum = 5 | TabNum >= 9)
		if (MaskFlag = 1)
			Deactivate NordMSK1
		elseif (MaskFlag = 2)
			Deactivate NordMSK2
		elseif (MaskFlag = 3)
			Deactivate NordMSK3
		endif
		move	C0,MaskFlag
	elseif (TabNum > 5 & TabNum < 8)
		if (MaskFlag <>	2)
			if (MaskFlag = 1)
				Deactivate NordMSK1
			elseif (MaskFlag = 3)
				Deactivate NordMSK3
			endif
			Activate NordMSK2
			move	C2,MaskFlag
			getprop	NORDMSK2EditSearchKey,enabled=N3
			if (N3 = 1)
				setfocus NORDMSK2EditSearchKey
			endif
		endif
	elseif (TabNum = 8)
		if (MaskFlag <>	3)
			if (MaskFlag = 1)
				Deactivate NordMSK1
			elseif (MaskFlag = 2)
				Deactivate NordMSK2
			endif
			Activate NordMSK3
			move	C3,MaskFlag
			getprop	NORDMSK3EditSearchKey,enabled=N3
			if (N3 = 1)
				setfocus NORDMSK3EditSearchKey
			endif
		endif
	endif
.
	if (N2 = C1)
		Activate Nord001a
.START PATCH 3.72 ADDED LOGIC
		Activate Nord001a1
		eventclear				.Clear the event which sets the Window Caption to be seen
		call	OrderSelectCaption using C0	.Force the Select Window Caption to be unseen
		eventclear
.		if (SrchSelFlag = 1)
.			call	DetermineLVHeight
.		endif
		setprop	Nord001a2,height=0
		setprop	Nord001a2,visible=0
		move	C0,SrchSelFlag
.END PATCH 3.72 ADDED LOGIC
		setprop	nord0001ButtonPrevious,visible=0
		call	OrderSwitchToPending
	elseif (N2 = C2)
		Activate Nord001b
		setfocus Nord001bEditSales
	elseif (N2 = C3)
		Activate Nord001c
		if (XFlag = NO)
			setprop	nord001CButtonUpdate,visible=0
			setprop	nord001CButtonQuit,visible=0
		else
			setprop	nord001CButtonUpdate,visible=1
			setprop	nord001CButtonQuit,visible=1
		endif
.Following is done because when	you go back to a Screen, all objects receive their default
.properties established	when they were originally created using	the Designer.
		if (View3Flag =	1)
			setfocus nord001CListView
		else
			setprop	nord001CListView,visible=0
			setfocus nord001CListView2
		endif
	elseif (N2 = C4)
		Activate Nord001g
	elseif (N2 = C5)
		Activate Nord001d
		setprop	NORDMSK1ButtonOk,default=0
		setprop	Nord001DButtonOk,default=1
.Following is done because when	you go back to a Screen, all objects
.are made visible when collection (Nord001d is head of collection) is made visible.
.START PATCH	3.78.2	REMOVED LOGIC
.		if (View5Flag =	1)
.			setprop	Order5ListView2,visible=0
.			setprop	Order5ListView3,visible=0
.			setprop	Order5ListView4,visible=0
.			setprop	Order5ListView5,visible=0
.			setprop	Order5ListView6,visible=0
			setfocus Nord001DListView
.		elseif (View5Flag = 2)
.			setprop	Nord001DListView,visible=0
.			setprop	Order5ListView3,visible=0
.			setprop	Order5ListView4,visible=0
.			setprop	Order5ListView5,visible=0
.			setprop	Order5ListView6,visible=0
.			setfocus Order5ListView2
.		elseif (View5Flag = 3)
.			setprop	Nord001DListView,visible=0
.			setprop	Order5ListView2,visible=0
.			setprop	Order5ListView4,visible=0
.			setprop	Order5ListView5,visible=0
.			setprop	Order5ListView6,visible=0
.			setfocus Order5ListView3
.		elseif (View5Flag = 4)
.			setprop	Nord001DListView,visible=0
.			setprop	Order5ListView2,visible=0
.			setprop	Order5ListView3,visible=0
.			setprop	Order5ListView5,visible=0
.			setprop	Order5ListView6,visible=0
.			setfocus Order5ListView4
.		elseif (View5Flag = 5)
.			setprop	Nord001DListView,visible=0
.			setprop	Order5ListView2,visible=0
.			setprop	Order5ListView3,visible=0
.			setprop	Order5ListView4,visible=0
.			setprop	Order5ListView6,visible=0
.			setfocus Order5ListView5
.		elseif (View5Flag = 6)
.			setprop	Nord001DListView,visible=0
.			setprop	Order5ListView2,visible=0
.			setprop	Order5ListView3,visible=0
.			setprop	Order5ListView4,visible=0
.			setprop	Order5ListView5,visible=0
.			setfocus Order5ListView6
.		endif
.END PATCH	3.78.2	REMOVED LOGIC
	elseif (N2 = C6)
		Activate Nord01ea
	elseif (N2 = C7)
		Activate Nord01eb
.START PATCH 3.66 REPLACED LOGIC
.		if (View7Flag =	1)
.			setprop	Nord01EBListView2,visible=0
.			setfocus Nord01EBListView
.		elseif (View7Flag = 2)
.			setprop	Nord01EBListView,visible=0
.			setfocus Nord01EBListView2
.		endif
		if (Proj7Flag = 1)
			setprop	Nord01EBListView,visible=0
			setprop	Nord01EBListView2,visible=0
.			setprop	Nord01EBListViewProj,visible=1
			setfocus Nord01EBListViewProj
		else
			if (View7Flag =	1)
				setprop	Nord01EBListView2,visible=0
				setfocus Nord01EBListView
			elseif (View7Flag = 2)
				setprop	Nord01EBListView,visible=0
				setfocus Nord01EBListView2
			endif
		endif
.END PATCH 3.66 REPLACED LOGIC
	elseif (N2 = C8)
		Activate Nord01ec
.START PATCH 3.72 ADDED LOGIC
		Activate Nord008a1
		eventclear				.Clear the event which sets the Window Caption to be seen
		call	Order8SelectCaption using C0	.Force the Select Window Caption to be unseen
		eventclear
.END PATCH 3.72 ADDED LOGIC
		setprop	nord0001ButtonPrevious,visible=0
		setprop	nord0001ButtonNext,visible=0
	elseif (N2 = C9)
		call	OrderPackageEnableForm
		setprop	nord0001ButtonPrevious,visible=0
		setprop	 nord0001ButtonNext,visible=0
	else	.(N2 = C10)
		Activate Nsta0001
		move	Tab10Num,N3
		call	OrderStatTabChange
		setprop	nord0001ButtonPrevious,visible=0
		setprop	 nord0001ButtonNext,visible=0
	endif
.	 call	 OrderApproveCheck
	return

OrderSwitchStatTab LRoutine FrmPtr
	if (Tab10Num <>	FrmPtr)
		Deactivate Nsta001a
		Deactivate Nsta001b
		Deactivate Nsta001c
		Deactivate Nsta001d
		Deactivate Nsta001e
		move	FrmPtr,N3
		call	OrderStatTabChange
	endif
	return

OrderStatTabChange
	move	N3,Tab10Num
	getprop	NSTA001AEditPackNum2,enabled=result
.START PATCH 3.48 REPLACED LOGIC
.	if (N3 < C4)
.		activate NSTA0002
.	else
.		deactivate NSTA0002
.	endif
.	if (N3 = C1)
.		Activate Nsta001a
.		if (result = 1)
.			setfocus NSTA001AEditLR2
.		endif
.	elseif (N3 = C2)
.		Activate Nsta001b
.		if (result = 1)
.			setfocus NSTA001BEditListCostM2
.		endif
.	elseif (N3 = C3)
.		Activate Nsta001c
.		if (result = 1)
.			setfocus NSTA001CEditNotes2
.		endif
.	elseif (N3 = C4)
.		Activate Nsta001d
.		if (View10Flag = 1)
.			setprop	NSTA001DListView2,visible=0
.			setprop	NSTA001DListView3,visible=0
..START PATCH 3.45 ADDED LOGIC
.			setprop	NSTA001DListView4,visible=0
..END PATCH 3.45 ADDED LOGIC
.			setprop	NSTA001DListView,visible=1
.		elseif (View10Flag = 2)
.			setprop	NSTA001DListView,visible=0
.			setprop	NSTA001DListView3,visible=0
..START PATCH 3.45 ADDED LOGIC
.			setprop	NSTA001DListView4,visible=0
..END PATCH 3.45 ADDED LOGIC
.			setprop	NSTA001DListView2,visible=1
.		elseif (View10Flag = 3)
.			setprop	NSTA001DListView,visible=0
.			setprop	NSTA001DListView2,visible=0
..START PATCH 3.45 ADDED LOGIC
.			setprop	NSTA001DListView4,visible=0
..END PATCH 3.45 ADDED LOGIC
.			setprop	NSTA001DListView3,visible=1
..START PATCH 3.45 ADDED LOGIC
.		elseif (View10Flag = 4)
.			setprop	NSTA001DListView,visible=0
.			setprop	NSTA001DListView2,visible=0
.			setprop	NSTA001DListView3,visible=0
.			setprop	NSTA001DListView4,visible=1
..END PATCH 3.45 ADDED LOGIC
.		endif
.	elseif (N3 = C5	| N3 = C6)
.		Activate Nsta001e
.		setfocus NSTA001EListView
.	endif
............................
	if (N3 > 1 & N3 < C5)
		activate NSTA0002
	else
		deactivate NSTA0002
	endif
	if (N3 = C1)
		Activate Nsta001d
		if (View10Flag = 1)
			setprop	NSTA001DListView2,visible=0
			setprop	NSTA001DListView3,visible=0
			setprop	NSTA001DListView4,visible=0
			setprop	NSTA001DListView,visible=1
		elseif (View10Flag = 2)
			setprop	NSTA001DListView,visible=0
			setprop	NSTA001DListView3,visible=0
			setprop	NSTA001DListView4,visible=0
			setprop	NSTA001DListView2,visible=1
		elseif (View10Flag = 3)
			setprop	NSTA001DListView,visible=0
			setprop	NSTA001DListView2,visible=0
			setprop	NSTA001DListView4,visible=0
			setprop	NSTA001DListView3,visible=1
		elseif (View10Flag = 4)
			setprop	NSTA001DListView,visible=0
			setprop	NSTA001DListView2,visible=0
			setprop	NSTA001DListView3,visible=0
			setprop	NSTA001DListView4,visible=1
		endif
	elseif (N3 = C2)
		Activate Nsta001a
		if (result = 1)
			setfocus NSTA001AEditLR2
		endif
	elseif (N3 = C3)
		Activate Nsta001b
		if (result = 1)
			setfocus NSTA001BEditListCostM2
		endif
	elseif (N3 = C4)
		Activate Nsta001c
		if (result = 1)
			setfocus NSTA001CEditNotes2
		endif
	elseif (N3 = C5	| N3 = C6)
		Activate Nsta001e
		setfocus NSTA001EListView
	endif
.END PATCH 3.48 REPLACED LOGIC
	setitem	NSTA0001TabControl001,0,N3
	return

StatsSortbyCost
	NSTA001DListView4.SortColumn using *Column=1,*Type=4
	NSTA001DListView4.SetItemState giving N9 using 0,2,2
	return

StatsSortbyPackageA
.Secondary sort by List Name
	NSTA001DListView2.SortColumn using *Column=0,*Type=1,*Column1=4,*Type1=1
	NSTA001DListView2.SetItemState giving N9 using 0,2,2
	return

StatsSortbyPackageB
.Secondary sort by Cost per Member
	NSTA001DListView2.SortColumn using *Column=0,*Type=1,*Column1=6,*Type1=4
	NSTA001DListView2.SetItemState giving N9 using 0,2,2
	return

OrderStatTabClick
.START PATCH 3.48 REPLACED LOGIC
.	if (N3 = C1)
.		Deactivate Nsta001a
.	elseif (N3 = C2)
.		Deactivate Nsta001b
.	elseif (N3 = C3)
.		Deactivate Nsta001c
.	elseif (N3 = C4)
.		Deactivate Nsta001d
.	elseif (N3 = C5)
.		Deactivate Nsta001e
.	endif
	if (N3 = C1)
		Deactivate Nsta001d
	elseif (N3 = C2)
		Deactivate Nsta001a
	elseif (N3 = C3)
		Deactivate Nsta001b
	elseif (N3 = C4)
		Deactivate Nsta001c
	elseif (N3 = C5)
		Deactivate Nsta001e
	endif
.END PATCH 3.48 REPLACED LOGIC
	return

OptionsTabClick
	if (N2 = C1)
		setprop	Options1Coll,visible=0
	elseif (N2 = C2)
.		 setprop Options2Coll,visible=0
	elseif (N2 = C3)
.		 setprop Options3Coll,visible=0
	elseif (N2 = C4)
.		 setprop Options4Coll,visible=0
	elseif (N2 = C5)
		setprop	Options5Coll,visible=0
	elseif (N2 = C6)
.		 setprop Options6Coll,visible=0
	elseif (N2 = C7)
.START PATCH 3.66 ADDED LOGIC
		setprop Options7Coll,visible=0
.END PATCH 3.66 ADDED LOGIC
	elseif (N2 = C8)
.		 setprop Options8Coll,visible=0
	elseif (N2 = C9)
		setprop	Options9Coll,visible=0
	elseif (N2 = C10)
		setprop	Options10Coll,visible=0
	endif
	return

OptionsTabChange
	if (N2 = C1)
		setprop	Options1Coll,visible=1
	elseif (N2 = C2)
.		 setprop Options2Coll,visible=1
	elseif (N2 = C3)
.		 setprop Options3Coll,visible=1
	elseif (N2 = C4)
.		 setprop Options4Coll,visible=1
	elseif (N2 = C5)
		setprop	Options5Coll,visible=1
	elseif (N2 = C6)
.		 setprop Options6Coll,visible=1
	elseif (N2 = C7)
.START PATCH 3.66 ADDED LOGIC
		setprop Options7Coll,visible=1
.END PATCH 3.66 ADDED LOGIC
	elseif (N2 = C8)
.		 setprop Options8Coll,visible=1
	elseif (N2 = C9)
		setprop	Options9Coll,visible=1
	elseif (N2 = C10)
		setprop	Options10Coll,visible=1
	endif
	return

OrderSpinKeyUp
	getitem	Nord001bVScrollSales,0,N2
	sub	C1,N2
	call	Spin3
	return
OrderSpinKeyDown
	getitem	Nord001bVScrollSales,0,N2
	add	C1,N2
	call	Spin3
	return

OrderReturnRtnDate
	call	OrderSwitchTab using C1
	setfocus Nord001AEditRtnDate
	move	"Y",ReturnFlag
	return
OrderReturnMailDate
	call	OrderSwitchTab using C1
	setfocus Nord001AEditMailDate
	move	"Y",ReturnFlag
	return
OrderReturnOrderDate
.Called	only if	in FIXORD mode
	call	OrderSwitchTab using C1
.START PATCH 	3.77.9	REPLACED LOGIC
.	setfocus Nord001AEditMailDate
	setfocus Nord001AEditOrderDate  // this is the proper field
.END PATCH 	3.77.9	REPLACED LOGIC
	move	"Y",ReturnFlag
	return

CalcPseudoMouseForm LRoutine EditPtr
.Function used to give a psuedo	Mouse_Down_Event in order to display OrderInfo
.by hitting the	F3 key while sitting on	certain	Edit Text Boxes.
	getprop	EditPtr,top=T1,left=L1,height=N8,width=N9
	calc	MouseForm=((N9*10000)+N8)
	return

.OrderSetInfoScreen
..LOGIC	in this	section	broken down into following generalized equation:
..
..OrderInfo_Top=(TopCoordinateOfMouseClick + TopCoordinateOfObjectWhereClickOccurred + Cushion + TopCoordinateOfProgram1Screen
..If ((OrderInfo_Top + OrderInfo_Height) > ScreenHeight)
..	 OrderInfo_Top=(OrderInfo_Top -	TopCoordinateOfMouseClick - Cushion
..Endif
..
..OrderInfo_Left=(LeftCoordinateOfMouseClick + LeftCoordinateOfObjectWhereClickOccurred	+ Cushion + LeftCoordinateOfProgram1Screen
..If ((OrderInfo_Left +	OrderInfo_Width) > ScreenWidth)
..	 OrderInfo_Left=(OrderInfo_Left	- LeftCoordinateOfMouseClick - Cushion
..Endif
..
..
..Getinfo
..This is done each time in case the user changes their	screen dimensions in the middle	of
..using	this program
.	 clear	 str25
.	 getinfo system,str25
.	 bump	 str25,12
.	 move	 str25,str4
.	 move	 str4,ScrRight
.	 bump	 str25,4
.	 move	 str25,str4
.	 move	 str4,ScrBottom
..
.	 setprop OrderInfo,winpos=1
.	 getprop nord0001,top=H,left=V
..Break	down mouse coordinates - figured in terms of object where mouse	was clicked
..MouseForm established	at MouseDown_Event
.	 move	 "10000",N7
.	 div	 N7,MouseForm,N9 .N9=left
.	 mult	 N9,N7
.	 sub	 N7,MouseForm,N8 .N8=top
..Add to STATIC	coordinates of object where mouse was clicked
..T1/L1	established at MouseDown_Event
.	 add	 N9,L1		 .L1=left
.	 add	 N8,T1		 .T1=top
..Calulate totals for positions
.	 add	 T1,H
.	 add	 "44",H		 .Compensate for Menu Bar/Title	Bar + some to allow second click to make invisible
.	 add	 L1,V
.	 add	 "7",V		 .Compensate to	allow second click to make invisible
..Test to see if object	will fit on page
.	 Getprop OrderInfo,height=N9,width=N8
.	 add	 N9,H,FarBottom
.	 if (FarBottom > ScrBottom)
.		 sub	 N9,H
.		 sub	 "20",H	 .Compensate to	allow second click to make invisible
.	 endif
.	 add	 N8,V,FarRight
.	 if (FarRight >	ScrRight)
.		 sub	 N8,V
.		 sub	 "10",V	 .Compensate to	allow second click to make invisible
.	 endif
..Set coordinates
.	 setprop OrderInfo,top=H,left=V
.	 return

OrderSetMouseBusy
	setmode	*mcursor=*wait
	return
OrderSetMouseFree
	setmode	*mcursor=*arrow
	return

OrderUpdateScreen7 LRoutine DimPtr
	move	SEQ,result
	loop
		move	result,N9
		Nord01EBListView.GetNextItem giving result using C0,N9
		until (result =	SEQ)
		Nord01EBListView.GetItemText giving str8 using result,19
		unpack	str8,str2,str6
		if (str6 = DimPtr)
			Nord01EBListView.DeleteItem giving N8 using result
		endif
	repeat
	move	SEQ,result
	loop
		move	result,N9
		Nord01EBListView2.GetNextItem giving result using	C0,N9
		until (result =	SEQ)
		Nord01EBListView2.GetItemText giving str8	using result,18
		unpack	str8,str2,str6
		if (str6 = DimPtr)
			Nord01EBListView2.DeleteItem giving N8 using result
		endif
	repeat
.Clear Order variables used in List View
	clear	OLRN
	clear	OCLRINIT
	clear	OSTAT
	call	OrderLoadLOLDetail
	return
Order2ClearExcFields
	setitem	Nord001AEditExchangeQty,0,""
	setitem	Nord001AEditExchangePrice,0,""
	return

OrderSetStatsFocus
	setprop	NSTA001AEditLR2,readonly=0
	setprop	NSTA001ACheckLOL2,enabled=1
OrderSetStatsFocus1
	setfocus NSTA001AEditLR2
	return
*******************************************************************************
****************** NINCAL EXCHANGE STATUS SUBROUTINES **************************
*******************************************************************************
OrderExchangeStatus
.LCR's and Pending Orders need to skip this next check
.START PATCH 3.6 ADDED LOGIC
	if (mod = 8)
		return
	endif
.END PATCH 3.6 ADDED LOGIC
	if ((mod = 5 OR	(mod = 6 AND NORD4STAT <> "08")) | (mod	= 3 OR (mod = 7	AND NORD5STAT <> "04")))
		goto OrderExchangeStatusB
	endif
	cmatch	"R",MRCODE			   *EXCHANGES ALLOWED
	if equal
		clear	str55
		append	"Exchanges Not Allowed On Mailer ## ",str55
		append	OMLRNUM,str55
		reset	str55
		alert	caution,str55,result
		setfocus Nord001ACheckExchange
		move	"Y",ReturnFlag
		return
	endif
	goto OrderExchangeStatusB
.THIS CODE CURRENTLY NOT ACCESSED - LEFT FOR FUTURE USE
exchnet	move	C0 to N2
	move	onetper	to N2
	match	"NN" to	onetper			.net net?
	goto	exchneta if equal	      .yes
	compare	c0 to n2
	if	not equal
exchneta keyin	*p35:24,*el,*b,"Net on exchanges??? Wipe net info 'Y/n'	",str1;
	rep	"yYnN" in str1
	cmatch	Yes to str1
	if	equal
	move	c0 to onetper
	move	c0 to onetrc
	move	c0 to onetmin
	move	b1 to onetfm
	endif
	cmatch	no to str1
	goto	exchnet	if not equal
	endif
OrderExchangeStatusB
	move	OLNUM,NXRFFLD
	move	NO,XrefFlag	    .STR2 USED IN NORD001I
	clear	NXRFMLR
	clear	TEMMLR
	move	C1,NXRFPATH
	move	"O.XchngStatB-NXRFKEY",Location
	pack	KeyLocation,"Key: ",NXRFFLD
	call	NXRFKEY
	if not over
		move	YES,XrefFlag	    .STR2 USED IN NORD001I
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.		move	NXRFMLR,TEMMLR
.begin patch 2.79.2
		MOve	CompExcl,HoldCompExcl		.Save Mlr excl byte
.end patch 2.79.2
		move	NXRFMLR,COMPFLD
		move	"O.XchngStatB-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
.begin patch 2.79.2
		MOve	HOldCompExcl,CompExcl               .restore mailer excl byte
.end patch 2.79.2
		move	COMPOLDMLR,TEMMLR
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.	 elseif	(mod = 3 OR (mod = 7 AND NORD5STAT <> "04"))
	elseif ((mod = 5 OR (mod = 6 AND NORD4STAT <> "08")) | (mod = 3	OR (mod	= 7 AND	NORD5STAT <> "04")))
		goto OrderLCRNoHistory
	endif
.Only display if List or Mlr was changed, New Order, Pending to	Live ORder
	if (mod	= 1 OR (mod = 2	AND ((OMLRNUM <> HoldMlr | OLNUM <> HoldList) OR QtyFlag = YES)) OR (mod = C6 AND NORD4STAT = "08") OR (mod = 7	AND NORD5STAT =	"04"))
....................
..Trying out new logic,	which will erradicate the need for this	extra form..
....................
.		 clear	 str55
.		 append	 "Enter	Mlr ## for ",str55
.		 append	 O1DES,str55
.		 reset	 str55
.		 setitem Nord001iStatKey,0,str55
.		 setitem Nord001iStatMlr,0,""
.		 setitem Nord001iStatAdd1,0,""
.		 setitem Nord001iStatAdd2,0,""
.		 setitem Nord001iStatMR,0,""
.		 setitem Nord001iEditKey,0,TEMMLR
.		 setfocus Nord001iEditKey
.		 call	 XRefOK		 .Found	in NORD001I, OK_Click
.		 setprop Nord001i,visible=1
.		 if (ReturnFlag	= YES)	 .User cancelled transaction and thus cancelled	update!!
.			 return
.		 endif
		call	OrderXRefVerify
		setitem	EditTextBoxes(1),0,TEMMLR
		call	OrderXRefOK
		setprop	Report2,visible=1
		if (RptCan = YES)
			move	YES,ReturnFlag
			return
		endif
	endif

.I commented out the following code because 90%	you will want the checkpoint,
.however there is one scenario where the incorrect XSTAT will be calculated:
.If you	open a new instance of program,	call up	an existing Exchange Order,
.without Modifying it, Mask an LCR off of it, the XSTAT	will appear incorrectly,
.due to	the QtyFlag set	to NO.	ASH
.Prevent double	processing for New LCR's/Pendings
.NEW LOGIC FOR PENDINGS
..	  if (mod = 3 AND QtyFlag = NO)
.	 if ((mod = 3 |	mod = 5) AND QtyFlag = NO)
.		 return
.	 endif
.**************************************************************************
.Finish	Processing
.**************************************************************************
READAC
.	 if (mod <> C1 AND mod <> C3)
	if (mod	<> C1 AND mod <> C3 AND	mod <> C5)
		return
	endif
READAC1	clear	ACKEY
	clear	NXNGFLD1
	clear	NXNGFLD2
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.	pack	ACKEY,OMLRNUM,TEMMLR
.	pack	NXNGFLD1,AKey1A,OMLRNUM
.	pack	NXNGFLD2,AKey2A,TEMMLR
	pack	COMPFLD3,OMLRNUM
	move	"READAC1-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	move	COMPNUM,str6
.
	pack	COMPFLD3,TEMMLR
	move	"READAC1,2-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.
	pack	ACKEY,str6,COMPNUM
	pack	NXNGFLD1,AKey1A,str6
	pack	NXNGFLD2,AKey2A,COMPNUM
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
	rep	ZFILL,ACKEY
	move	"READAC1-NXNGAIM",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGAIM
	goto REV if over
	move	"NINXNUM",FMESG
	match	ACKEY,ACCKEY
	goto ISAMBAD IF	NOT EQUAL
	move	C1,MLRSW
	move	C1,EFLAG
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
.START PATCH 3.6 REPLACED LOGIC
.This is a safety measure, should never get here if mod=8
.	if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
	if ((mod = 8 OR mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
.END PATCH 3.6 REPLACED LOGIC
		goto ExacLoop
	endif
	GOTO	EXACC
.
. REV -	CHECKS TO SEE IF NXNGFILE EXISTS WITH CLIENT NUMBERS REVERSED.
REV	clear	ACKEY
	clear	NXNGFLD2
	clear	NXNGFLD1
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.	pack	ACKEY,TEMMLR,OMLRNUM
.	pack	NXNGFLD2,AKey2A,OMLRNUM
.	pack	NXNGFLD1,AKey1A,TEMMLR
	pack	COMPFLD3,OMLRNUM
	move	"REV-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	move	COMPNUM,str6
.
	pack	COMPFLD3,TEMMLR
	move	"REV,2-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.
	pack	ACKEY,COMPNUM,str6
	pack	NXNGFLD2,AKey2A,str6
	pack	NXNGFLD1,AKey1A,COMPNUM
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
	rep	ZFILL,ACKEY
	move	"REV-NXNGAIM",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGAIM
	goto NEWACC if over
	move	"NXNGAIM-REV",FMESG
	match	ACKEY,ACCKEY
.This patch added to circumvent	the following:
.  LCR with List that has no associated	Mailer.
.  LCR's will not hit Cross Reference section
.  and so code would bomb here.
.	 goto ISAMBAD if not equal
	if not equal
.START PATCH 3.6 REPLACED LOGIC
.This is a safety measure, should never get here if mod=8
.		if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
		if ((mod = 8 OR mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
.END PATCH 3.6 REPLACED LOGIC
			if (HistFlag <>	1)
				goto OrderLCRNoHistory
			else
				return
			endif
		endif
		goto ISAMBAD
	endif
	move	C2,MLRSW
	move	C2,EFLAG
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
.START PATCH 3.6 REPLACED LOGIC
.This is a safety measure, should never get here if mod=8
.	if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
	if ((mod = 8 OR mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
.END PATCH 3.6 REPLACED LOGIC
		goto ExacLoop
	endif
	goto EXACC
.
. NEWACC - NEW ACCOUNT.
NEWACC
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
.START PATCH 3.6 REPLACED LOGIC
.This is a safety measure, should never get here if mod=8
.	if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
	if ((mod = 8 OR mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
.END PATCH 3.6 REPLACED LOGIC
		goto OrderLCRNoHistory
	endif
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.	pack	ACKEY,OMLRNUM,TEMMLR
.................
	pack	COMPFLD3,OMLRNUM
	move	"NEWACC-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	move	COMPNUM,str6
.
	pack	COMPFLD3,TEMMLR
	move	"NEWACC,2-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.
	pack	ACKEY,str6,COMPNUM
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
	rep	ZFILL,ACKEY
	move	ACKEY,ACCKEY
	move	"00000",ENTRY
	move	"NEWACC-NXNGWRT",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGWRT
	IF (LRINIT = 1)
	move	"NINXNG	- NEWACC",str45
	call	OrderWriteLRFile using str45
	ENDIF
	move	C1,EFLAG
	move	C1,MLRSW
.
KEYNEW	clear	taskname
	append	"Beginning balance for ",taskname
.START PATCH 3.76 TEMPORARY LOGIC
.This will be removed when above logic in NEWACC is removed.
.the second read to the Company File is screwing up the value of MCOMP
	pack	COMPFLD3,OMLRNUM
	move	"KEYNEW-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.START PATCH 3.76 TEMPORARY LOGIC
	append	MCOMP,taskname
	append	COLON,taskname
	reset	taskname
	setitem	OrderXBalStat1,0,taskname
	setitem	OrderXBalEdit1,0,"0"
	clear	taskname
	append	"Beginning balance for ",taskname
	append	O1DES,taskname
	append	COLON,taskname
	reset	taskname
	setitem	OrderXBalStat2,0,taskname
	setitem	OrderXBalEdit2,0,"0"
	setprop	OrderXBalOK,enabled=1
	setprop	OrderXBalCancel,enabled=1
	setfocus OrderXBalOK
	setprop	Nord001k,visible=1
	if (ReturnFlag = YES)
		return
	endif
	move	"00000",ENTRY
	pack	EXKEY,ACKEY,ENTRY
	rep	ZFILL,EXKEY
KEYNEW1	move	"NXCHNXT",GNXTFLD
	move	"KEYNEW1-GNXTKEY",Location
	pack	KeyLocation,"Key: ",GNXTFLD
	call	GNXTKEY
	move	GNXTNUM,LR
	move	LR,str1
	bump	LR,1
	move	LR,N5
	reset	LR
	add	C1,N5
	clear	LR
	pack	LR,str1,N5
	rep	ZFILL,LR
	move	LR,GNXTNUM
	move	"KEYNEW1-GNXTUPD",Location
	pack	KeyLocation,"Key: ",GNXTFLD
	call	GNXTUPD
	IF (LRINIT = 1)
	move	"GNXT -	Update,KEYNEW1",str45
	call	OrderWriteLRFile using str45
	ENDIF
	move	C2,NXCHPATH
	pack	NXCHFLD2,LR
	rep	zfill,NXCHFLD2
	move	"KEYNEW1-NXCHTST",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHTST
	goto NEWEXLR if	over
	clear	taskname
	append	"This BEG/BAL LR## is already on file!",taskname
	append	carr,taskname
	append	"I will	try again.",taskname
	reset	taskname
	alert	note,taskname,result
	goto KEYNEW1
NEWEXLR	clear	XCHCOMNT
	if ((mod = 6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))
		move	timestamp,str8
	else
		pack	str8,OODTEC,OODTEY,OODTEM,OODTED
	endif
	move	str8,DAT
	move	C0,QTY
	move	OLNUM,LIST
	clear	STAT
	clear	MLRSW
	pack	NXCHFLD1,EXKEY
	rep	zfill,NXCHFLD1
.NXCHWRT sets value of NXCHPATH
	move	INITS,TYPE
	move	"NEWEXLR-NXCHWRT",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHWRT
	IF (LRINIT = 1)
	move	"NINXCH	- NEWEXLR",str45
	call	OrderWriteLRFile using str45
	ENDIF
	move	"000000000",USAGE1
	move	"000000000",USAGE2
	goto	EXACC
.
DELNEW
	move	"Y",ReturnFlag
	move	"DELNEW-NXNGTST",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGTST
	if over
.START PATCH 3.76 REPLACED LOGIC
.		unpack	NXNGFLD1,str3,str4
.		unpack	NXNGFLD2,str3,str5
.		pack	NXNGFLD1,AKey1A,str5
.		pack	NXNGFLD2,AKey2A,str4
		unpack	NXNGFLD1,str3,str6
		unpack	NXNGFLD2,str3,str7
		pack	NXNGFLD1,AKey1A,str7
		pack	NXNGFLD2,AKey2A,str6
.END PATCH 3.76 REPLACED LOGIC
		move	"DELNEW-NXNGTST-2nd",Location
		pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
		call	NXNGTST
		if over
			return
		endif
	endif
	move	"DELNEW-NXNGDEL",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGDEL
	return
.
EXACC
	move	YES,EXSW
.START PATCH 3.76 REPLACED LOGIC
.	unpack	ACKEY,str4,str5
.	pack	NXNGFLD1 FROM AKey1A,str4
.	pack	NXNGFLD2 FROM AKey2A,str5
	unpack	ACKEY,str6,str7
	pack	NXNGFLD1 FROM AKey1A,str6
	pack	NXNGFLD2 FROM AKey2A,str7
.END PATCH 3.76 REPLACED LOGIC
	move	"EXACC-NXNGAIM",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGAIM
	move	ENTRY,BLANK5
	move	"NINXNUM-EXACC",FMESG
	match	ACKEY,ACCKEY
	goto ISAMBAD if	not equal
.	 getitem Nord001AComboPending,0,N2
.	 if (mod = 6 & N2 = 10)		  .approved pending ?
.		 add	 C1,ENTRY			 .yes prep to write new	detail
.		 move	 "EXACC-NXNGUPD",Location
.		 pack	 KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
.		 call	 NXNGUPD
.	 elseif	(mod <>	6)		 .not approved pending,	add mode?
		add	C1,ENTRY	.yes do	it
		move	"EXACC-NXNGUPD",Location
		pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
		call	NXNGUPD
		IF (LRINIT = 1)
		move	"NINXNG	- Update,EXACC",str45
		call	OrderWriteLRFile using str45
		ENDIF
.	 endif
ExAcLoop
	move	C1,NXCHPATH
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
.START PATCH 3.6 REPLACED LOGIC
.This is a safety measure, should never get here if mod=8
.	if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
	if ((mod = 8 OR mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
.END PATCH 3.6 REPLACED LOGIC
		pack	NXCHFLD1,ACKEY,ENTRY
	else
		pack	NXCHFLD1,ACKEY,BLANK5
	endif
	rep	ZFILL,NXCHFLD1
	move	"ExAcLoop-NXCHKEY",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHKEY
.WORKX1	& WORKX2 used to calculate how many names are owed
.without altering values of USAGE1 & USAGE2 - ASH
	move	USAGE1,WORKX1
	move	USAGE2,WORKX2
	if over					.try to	reset counter
.		 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
.START PATCH 3.6 REPLACED LOGIC
.This is a safety measure, should never get here if mod=8
.		if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
		if ((mod = 8 OR mod = 3 OR (mod = 7 AND NORD5STAT <> "04")) | (mod = 5 OR (mod = 6 AND NORD4STAT <> "08")))
.END PATCH 3.6 REPLACED LOGIC
			goto COUNTERR
		else
			move	BLANK5,N5
			sub	C1,N5
			move	N5,BLANK5
			compare	C0,N5
			goto COUNTERR if equal		.reached what should have been begin bal - reject
			goto EXACLOOP
		endif
	endif
.  IF READ DOES	NOT FIND A VALID DETAIL	RECORD RESET ENTRY NUMBER , RETURN
. TO K18 AND INFORM TYPIST TO CONTACT COMPUTER PERSONNEL. 03/08/83
.
	pack	EXKEY,ACKEY,ENTRY
	rep	ZFILL,EXKEY
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.	clear	WORKX3
.	append	OMLRNUM,WORKX3
.	reset	WORKX3
.	clear	WORKX4
.	append	ACKEY,WORKX4
.	reset	WORKX4
.	match	WORKX3,WORKX4
..........................
	pack	COMPFLD3,OMLRNUM
	move	"ExAcLoop-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.
	clear	WORKX3
.Following line will be replaced once NINORD converted!
.	append	OMLRNUM,WORKX3
	append	COMPNUM,WORKX3
	reset	WORKX3
	clear	WORKX4
	append	ACKEY,WORKX4
	reset	WORKX4
	match	WORKX3,WORKX4
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
	goto FLAG if equal
	move	C2,EFLAG
	move	C2,MLRSW
	return
.
FLAG
	move	C1,EFLAG
	move	C1,MLRSW
	return
.
COUNTERR
	beep
	beep
	clear	taskname
	append	"Counter Error Running BAL Not Found ",taskname
	append	ackey,taskname
	append	B1,taskname
	append	NXCHFLD1,taskname
	append	Carr,taskname	      .Carriage	Return
	append	"Inform	Computer Personel!!!!!",taskname
	alert	note,taskname,result
.START PATCH 3.76 REPLACED LOGIC
.	unpack	ACKEY INTO str4,str5
.	pack	NXNGFLD1 FROM AKey1A,str4
.	pack	NXNGFLD2 FROM AKey2A,str5
	unpack	ACKEY INTO str6,str7
	pack	NXNGFLD1 FROM AKey1A,str6
	pack	NXNGFLD2 FROM AKey2A,str7
.END PATCH 3.76 REPLACED LOGIC
	clear	error
	append	"XCH Add counterr ",error
	append	ackey,error
	append	B1,error
	append	NXCHFLD1,error
	reset	error
	Move	"This is an Error e-mail from Nord0001",SmtpSubject Subject

.   Set	the text message that is send with the attachments

	Move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
	Move	error,SmtpTextMessage(2)   Array <Text message >
	Move	"Subroutine Counterr",SmtpTextMessage(3)   Array <Text message >
	Move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	call	errmesg
	move	"Y",ReturnFlag
	return

OrderExchangeStatus2
.START PATCH 3.6 ADDED LOGIC
	if (mod = 8)
		return
	endif
.END PATCH 3.6 ADDED LOGIC
.	 branch	 mod of	OrderExchangeStatus2B,OrderExchangeStatus2A,OrderExchangeStatus2A,OrderExchangeStatus2A,VERIFYPRICE,OrderExchangeStatus2A,OrderExchangeStatus2A
	branch	mod of OrderExchangeStatus2B,OrderExchangeStatus2A,OrderExchangeStatus2A,OrderExchangeStatus2A,OrderExchangeStatus2A,OrderExchangeStatus2A,OrderExchangeStatus2A
OrderExchangeStatus2A
.Refers	to K21EE in original code - ASH
.	 if (mod = 3 AND QtyFlag = YES)
.		 goto OrderExchangeStatus2B
.	 elseif	(mod = 3 AND HistFlag =	C1)
.		 goto OrderExchangeStatus2B
.	 endif
	if ((mod = 5 | mod = 3) AND QtyFlag = YES)
		goto OrderExchangeStatus2B
	elseif ((mod = 5 | mod = 3) AND	HistFlag = C1)
		goto OrderExchangeStatus2B
.Following Branch added	10/1/2001 - ASH
	elseif ((mod = 6 AND nord4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))
		call	ReadAC1
		if (ReturnFlag = YES)
			return
		endif
		call	OrderExchangeStatus2B
		goto OrderFinalExchange
.Following Branch added	10/1/2001 - ASH
.Following Branch added	9/7/2001 - ASH
	elseif ((mod = 6 AND nord4STAT <> "08")	OR (mod	= 7 AND	NORD5STAT <> "04") OR (mod = 3)	OR (mod	= 5))
		call	ReadAC1
		if (ReturnFlag = YES)
			return
		endif
		goto OrderExchangeStatus2B
.End of	New Branch - ASH
	endif
	move	C2,NXCHPATH
	move	OLRN,NXCHFLD2
	rep	zfill,NXCHFLD2
	move	"O.XchngStat2A-NXCHKEY",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHKEY
.This is where we might	write in a new Exchange	record if we are going from Rental to Exchange
.Exchange testing - remmed
	if over
.Rental	to Exchange / Pending to Live /	LCR to Live
		if ((mod = 2) OR (mod =	6 AND nord4stat	= "08")	OR (mod	= 7 AND	NORD5STAT = "04"))
			call	ReadAC1
			if (ReturnFlag = YES)
				return
			endif
			call	OrderExchangeStatus2B
			goto OrderFinalExchange
		elseif ((mod = 6 AND nord4stat <> "08")	OR (mod	= 7 AND	NORD5STAT <> "04") OR (mod = 3)	OR (mod	= 5))
.		 elseif	((mod =	6 AND nord4stat	<> "08") OR (mod = 7 AND NORD5STAT <> "04") OR (mod = 3))
			call	ReadAC1
			if (ReturnFlag = YES)
				return
			endif
			goto OrderExchangeStatus2B
		else
			goto KEYEXNG
		endif
	endif
.	 if (mod = 3 OR	(mod = 7 AND NORD5STAT <> "04"))
	if ((mod = 3 OR	(mod = 7 AND NORD5STAT <> "04")) | (mod	= 5 OR (mod = 6	AND NORD4STAT <> "08")))
		goto OrderExchangeStatus2B
	endif
	pack	str8,OODTEC,OODTEY,OODTEM,OODTED
	match	str8,DAT		*SAME ORDER?
	goto RXUP2 if equal		*YES
RXUP1	move	"RXUP1-NXCHKS",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHKS
.START New logic 10/1/2001
.	 goto	 KEYEXNG if over
	if over
		if (mod	= 2 & Rent2ExFlag = YES)
.Rent to Exchange
			call	ReadAC1
			if (ReturnFlag = YES)
				return
			endif
			call	OrderExchangeStatus2B
			goto OrderFinalExchange
		endif
		goto	KEYEXNG
	endif
.END New logic 10/1/2001
	match	OLRN,LR			*SAME ORDER?
.START New logic 10/1/2001
.	 goto KEYEXNG if not equal	 *NO
	if not equal
		if (mod	= 2 & Rent2ExFlag = YES)
.Rent to Exchange
			call	ReadAC1
			if (ReturnFlag = YES)
				return
			endif
			call	OrderExchangeStatus2B
			goto OrderFinalExchange
		endif
		goto	KEYEXNG
	endif
.END New logic 10/1/2001
	pack	str8,OODTEC,OODTEY,OODTEM,OODTED
	match	str8,DAT		*SAME ORDER?
	goto RXUP1 if not equal		*NO
RXUP2	pack	LIST,OLNUM
	move	QTY,SQTY
	move	OLRN,LR
.
	unpack	EXKEY,MALR1,MALR2
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY PATCH
.	match	OMLRNUM,MALR1
.	goto EXADJ if equal
.	match	OMLRNUM,MALR2
	pack	COMPFLD3,OMLRNUM
	move	"RXUP2-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.
	match	COMPNUM,MALR1
	goto EXADJ if equal
	match	COMPNUM,MALR2
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY PATCH
	goto EXADJ2 if equal
. IF MAILER NUMBERS DON'T MATCH	- ABORT
	alert	caution,"Mailer	Match Error - Exchange Not Adjusted!!",result
	move	YES,ReturnFlag
	return
EXADJ2
	move	C2,EFLAG
	move	C2,MLRSW
	sub	QTY,USAGE2
	move	"000000000",QTY
.OEXQTY	will be	filled with B1 if nothing is entered	ASH
	type	OEXQTY		    (EXCHANGE SPLIT?)
	goto EXADJ2A IF	NOT EQUAL   (NO)
	move	OEXQTY,QTY
	add	QTY,USAGE2
	goto WEXUP1
EXADJ2A
	move	OQTY,QTY
	add	QTY,USAGE2
	goto WEXUP1
.
EXADJ	sub	QTY,USAGE1
	move	C1,EFLAG
	move	C1,MLRSW
	move	"000000000",QTY
.OEXQTY	will be	filled with B1 if nothing is entered	ASH
	type	OEXQTY		   (EXCHANGE SPLIT?)
	goto EXADJ2B IF	NOT EQUAL      (NO)
	move	OEXQTY,QTY
	add	QTY,USAGE1
	goto WEXUP1
EXADJ2B
	 MOVE	   OQTY,QTY
	 ADD	   QTY,USAGE1
	 GOTO	   WEXUP1
WEXUP1
	move	C2,NXCHPATH
	move	INITS,TYPE
..START	PATCH 10-2-01
.	if (mod	= 2 & Rent2ExFlag = YES)
.		if (STAT = "R")
.			clear	STAT
.		else	.if (STAT <> "C" AND STAT <> "X")
.			clear	Rent2ExFlag
.		endif
.	endif
..END PATCH 10-2-01
	move	"WEXUP1-NXCHUPD",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHUPD
	IF (LRINIT = 1)
	move	"NINXCH	- Update,WEXUP1",str45
	call	OrderWriteLRFile using str45
	ENDIF
       	unpack	EXKEY,ACKEY,BLANK5
.START PATCH 3.76 REPLACED LOGIC
.	unpack	ACKEY,str4,str5
.	pack	NXNGFLD1,AKEY1A,str4
.	pack	NXNGFLD2,AKEY2A,str5
	unpack	ACKEY,str6,str7
	pack	NXNGFLD1,AKEY1A,str6
	pack	NXNGFLD2,AKEY2A,str7
.END PATCH 3.76 REPLACED LOGIC
	move	BLANK5,SENTRY
	move	"WEXUP1-NXNGAIM",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	move	"NINXNUM-WEXUP1",FMESG
	call	NXNGAIM
	goto ISAMBAD if	over
	compare	ENTRY,SENTRY
	goto UPEXIT if equal
	move	ENTRY,SENTRY
	move	BLANK5,ENTRY
	add	C1,ENTRY
PREPUP
	move	C1,NXCHPATH
	pack	NXCHFLD1,ACKEY,ENTRY
	rep	ZFILL,NXCHFLD1
	move	QTY,HowMany
	move	"PREPUP-NXCHKEY",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHKEY
.BLANK9	(used to be BLANK7) is used in order to	retain previous	value of QTY
	if over
		move	QTY,blank9
		move	HowMany,QTY
		goto WEXLOOP
	endif
.  IF RUNNING BALANCE IS ON LR JUST ADJUSTED, EXIT .
	move	QTY,blank9
	move	HowMany,QTY
	match	OLNUM,LR
	goto UPEXIT if equal
	cmatch	"1",EFLAG
	goto SUB2 if equal
	sub	SQTY,USAGE2
	add	QTY,USAGE2
	goto WEXUP2
SUB2	sub	SQTY,USAGE1
	add	QTY,USAGE1
WEXUP2	move	C1,NXCHPATH
	move	QTY,HowMany
	move	blank9,QTY
	move	INITS,TYPE
	move	"WEXUP2-NXCHUPD",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHUPD
	IF (LRINIT = 1)
	move	"NINXCH	- Update,WEXUP2",str45
	call	OrderWriteLRFile using str45
	ENDIF
	move	HowMany,QTY
WEXLOOP	compare	ENTRY,SENTRY
	goto UPEXIT if equal
	add	C1,ENTRY
	goto PREPUP
.
UPEXIT
..START	PATCH 10-2-01
.	if (mod	= 2 & Rent2ExFlag = YES)
..If going Rent	To Exchange add	subtracted amount back in to last detail record
.		if (EFLAG = "1")
.			add	SQTY,USAGE1
.		else
.			add	SQTY,USAGE2
.		endif
.		clear	Rent2ExFlag
.		move	"UPEXIT-NXCHUPD",Location
.		pack	KeyLocation,"Key: ",NXCHFLD2
.		call	NXCHUPD
.	endif
..END PATCH 10-2-01
	move	USAGE1,WORKX1
	move	USAGE2,WORKX2
UPEXIT1	goto UPCALC1
.
*******************************************************************************
. DO NOT CHANGE	THE LOGIC OF THIS SECTION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*******************************************************************************
OrderExchangeStatus2B
.First branch will allow XSTAT calculation for ALL LCR's
.	 if (QtyFlag = NO AND mod <> 3 AND mod <> 7)
.START PATCH 3.6 ADDED LOGIC
	if (mod = 8)
		return
	endif
.END PATCH 3.6 ADDED LOGIC
	if (QtyFlag = NO AND mod <> 3 AND mod <> 7 AND mod <> 5	AND mod	<> 6)
		return
.Second	branch will prevent XSTAT from being overwritten for LCR's w/o Exchange	History
.	 elseif	(HistFlag = C1 AND (mod	<> 7 OR	(mod = 7 AND NORD5STAT <> "04")))
	elseif (HistFlag = C1 AND ((mod	<> 7 OR	(mod = 7 AND NORD5STAT <> "04")) & (mod	<> 6 OR	(mod = 6 AND NORD4STAT <> "08"))))
		return
	endif
	cmatch	"1",EFLAG
	goto FLAG1 if equal
	type	OEXQTY
	goto FLAGA if not equal
	move	OEXQTY,USAGE2
.	 if (mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))
	if ((mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	& (mod <> 5 AND	(mod <>	6 OR (mod = 6 AND NORD4STAT = "08"))))
		add	USAGE2,WORKX2
	endif
	goto CALCX
.
FLAGA	move	OQTY,USAGE2
.	 if (mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))
	if ((mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	& (mod <> 5 AND	(mod <>	6 OR (mod = 6 AND NORD4STAT = "08"))))
		add	USAGE2,WORKX2
	endif
	goto CALCX
FLAG1
	type	OEXQTY
	goto FLAG1A if not equal
	move	OEXQTY,USAGE1
.	 if (mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))
	if ((mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	& (mod <> 5 AND	(mod <>	6 OR (mod = 6 AND NORD4STAT = "08"))))
		add	USAGE1,WORKX1
	endif
	goto CALCX
FLAG1A	move	OQTY,USAGE1
.	 if (mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))
	if ((mod <> 3 AND (mod <> 7 OR (mod = 7	AND NORD5STAT =	"04")))	& (mod <> 5 AND	(mod <>	6 OR (mod = 6 AND NORD4STAT = "08"))))
		add	USAGE1,WORKX1
	endif
	goto CALCX
.
CALCX	move	WORKX1,USAGE1
	move	WORKX2,USAGE2
.
UPCALC1
.Code added to prevent Epsilon Orders from having XSTAT	dumped into DESC001
.Code added to prevent Covenant	House Orders from having XSTAT dumped into DESC001 - Used to be	clients	of Epsilon
.	 if (OBRKNUM = "0192" AND (OCO2CODE = "	 " | OCO2CODE =	""))
.START PATCH 3.68.4 REPLACED LOGIC
.	if ((OBRKNUM = "0192" |	OMLRNUM	= "0396") AND (OCO2CODE	= "  " | OCO2CODE = ""))
.START PATCH 3.68.6 REPLACED LOGIC
.	if ((OBRKNUM = "0192" |	OMLRNUM	= "0396" | (OBRKNUM = "0638" &	OMLRNUM	= "1604")) AND (OCO2CODE = "  " | OCO2CODE = ""))
.START PATCH 3.71.1 REPLACED LOGIC
.	if ((OBRKNUM = "0192" |	OMLRNUM	= "0396") AND (OCO2CODE	= "  " | OCO2CODE = ""))
	if ((OBRKNUM = "0192" |	OMLRNUM	= "0396" | OMLRNUM = "5317" | OMLRNUM = "2598" | OMLRNUM = "1440") AND (OCO2CODE = "  " | OCO2CODE = ""))
.END PATCH 3.71.1 REPLACED LOGIC
.END PATCH 3.68.6 REPLACED LOGIC
.END PATCH 3.68.4 REPLACED LOGIC
		return
	endif
	move	EFLAG,N1
	compare	WORKX1,WORKX2
	goto CALCX3 if equal		  *WORKX1 = WORKX2
	goto CALCX1 if less		  *WORKX1 > WORKX2
	goto CALCX2 if not less		  *WORKX2 > WORKX1
.
CALCX1	sub	WORKX2,WORKX1
	branch	N1 OF CALCX1A,CALCX1B
CALCX1A
	clear	DESC001
	call	Trim using MCOMP
	append	MCOMP,DESC001
	append	" owes ",DESC001
	move	workx1,str10
.	 call	 LTrim using str10
.	 append	 str10,DESC001
	call	FormatNumeric using str10,str13
	append	str13,DESC001
	append	" names.",DESC001
	reset	DESC001
	setitem	Nord001AStatExchangeMssg,0,DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	move	YES,SpecFlag
	move	YES,SpecFlag2
	return
CALCX1B
	clear	DESC001
	call	Trim using O1DES
	append	O1DES,DESC001
	append	" owes ",DESC001
	move	workx1,str10
.	 call	 LTrim using str10
.	 append	 str10,DESC001
	call	FormatNumeric using str10,str13
	append	str13,DESC001
	append	" names.",DESC001
	reset	DESC001
	setitem	Nord001AStatExchangeMssg,0,DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	move	YES,SpecFlag
	move	YES,SpecFlag2
	return
CALCX2	sub	WORKX1,WORKX2
	branch	N1 OF CALCX2A,CALCX2B
	clear	str55
	append	"Branch	Error at CALCX2,",str55
	append	Carr,str55
	append	"Flag =	",str55
	append	EFLAG,str55
	reset	str55
	alert	caution,str55,result
CALCX2A
	clear	DESC001
	call	Trim using O1DES
	append	O1DES,DESC001
	append	" owes ",DESC001
	move	workx2,str10
.	 call	 LTrim using str10
.	 append	 str10,DESC001
	call	FormatNumeric using str10,str13
	append	str13,DESC001
	append	" names.",DESC001
	reset	DESC001
	setitem	Nord001AStatExchangeMssg,0,DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	move	YES,SpecFlag
	move	YES,SpecFlag2
	return
CALCX2B
	clear	DESC001
	call	Trim using MCOMP
	append	MCOMP,DESC001
	append	" owes ",DESC001
	move	workx2,str10
.	 call	 LTrim using str10
.	 append	 str10,DESC001
	call	FormatNumeric using str10,str13
	append	str13,DESC001
	append	" names.",DESC001
	reset	DESC001
	setitem	Nord001AStatExchangeMssg,0,DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	move	YES,SpecFlag
	move	YES,SpecFlag2
	return
CALCX3
	clear	DESC001
	move	"The exchange status is	even.",DESC001
	setitem	Nord001AStatExchangeMssg,0,DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	move	YES,SpecFlag
	move	YES,SpecFlag2
	return

OrderLCRNoHistory
.
.	 getprop OrderInfo,visible=N1
.	 if (N1	= C1)
.		 call	 OrderInfoClose
.	 endif
.	 setitem OrderInfoStatText1,0,""
.	 setitem OrderInfoStatText2,0,""
.	 setitem OrderInfoStatText3,0,"No Exchange History Found!"
.	 setitem OrderInfoStatText4,0,""
.	 setitem OrderInfoStatText5,0,""
.	 setprop OrderInfo,title="LCR Exchange History"
.	 setprop OrderInfo,visible=1
.	 pause	 "1"
.	 call	 OrderInfoClose
	pack	str45,"LCR Exchange History"
	pack	str55,"No Exchange History Found!"
	clear	str1
	call	OrderDisplayMessage using Nord0001,str45,str1,str1,str55,str1,str1,C0,C0,C0,C0
	pause	"1"
	call	OrderInfoClose
.
.Write out an XSTAT, flag it.
	pack	NORDFLD1,"01R",OMLRNUM
	pack	NORDFLD2,"02R",OLNUM
	clear	NORDFLD3
	clear	NORDFLD4
.	 if (OBRKNUM <>	"0192" OR (OCO2CODE <> "  " AND	OCO2CODE <> ""))
.START PATCH 3.68.4 REPLACED LOGIC
.	if ((OBRKNUM <>	"0192" & OMLRNUM <> "0396") OR (OCO2CODE <> "  " AND OCO2CODE <> ""))
.START PATCH 3.68.6 REPLACED LOGIC
.	if ((OBRKNUM <>	"0192" & OMLRNUM <> "0396" | (OBRKNUM = "0638" & OMLRNUM = "1604")) OR (OCO2CODE <> "  " AND OCO2CODE <> ""))
.START PATCH 3.71.1 REPLACED LOGIC
.	if ((OBRKNUM <>	"0192" & OMLRNUM <> "0396") OR (OCO2CODE <> "  " AND OCO2CODE <> ""))
	if ((OBRKNUM <> "0192" & OMLRNUM <> "0396" & OMLRNUM <> "5317" & OMLRNUM <> "2598" & OMLRNUM <> "1440") OR (OCO2CODE <> "  " AND OCO2CODE <> ""))
.END PATCH 3.71.1 REPLACED LOGIC
.END PATCH 3.68.6 REPLACED LOGIC
.END PATCH 3.68.4 REPLACED LOGIC
		call	OrderGetHistory	using DESC001,OLRN,NORDFLD1,NORDFLD2,NORDFLD3,NORDFLD4,C1
	endif
.	 move	 "NoHist-NORDAIMA",Location
.	 pack	 KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
.	 call	 NORDAIMA
.	 if over
.		 move	 "NO EXCHANGE HISTORY",DESC001
.	 else
.		 move	 C0,N1
..Modification should not test record currently	accessed.
..New will not have this problem.
..New may have str6 = OLRN, if record was masked after another record.
.		 if (str6 <> OLRN | NewFlag = YES)
.			 if (NORDFLAG <> C1)
.				 move	 C1,NORDPATH
.				 call	 NORDOPEN
.			 endif
.			 read	   NORDFILE,str6;str1,str1
.			 if not	over
.				 if (str1 = "B"	| str1 = "0")
.					 move	 C1,N1
.				 endif
.			 endif
.		 endif
.		 loop
.			 move	 "NoHist-NORDKGA",Location
.			 call	 NORDKGA
.			 until over
..Modification should not test record currently	accessed.
..New will not have this problem.
..New may have str6 = OLRN, if record was masked after another record.
.			 if (str6 <> OLRN | NewFlag = YES)
.				 read	   NORDFILE,str6;str1,str1
.				 if not	over
.					 if (str1 = "B"	| str1 = "0")
.						 move	 C1,N1
.					 endif
.				 endif
.			 endif
.		 repeat
.		 if (N1	= C1)
.			 move	 "NO EXCHANGE HISTORY -	RENTAL CONTINUATION",DESC001
.		 else
.			 move	 "NO EXCHANGE HISTORY",DESC001
.		 endif
.	 endif
	setitem	Nord001AStatExchangeMssg,0,DESC001
	setitem	nord001CEditSpecial1,0,DESC001
	move	YES,SpecFlag
	move	YES,SpecFlag2
.Prevent XSTAT from being overwritten with "The	Exchange Status	is Even"
	move	C1,HistFlag
	move	NO,Qtyflag
	return

OrderGetHistory	Routine	DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4,DimPtr5,FrmPtr
.Called	by: OrderLoadLOLScreen,OrderLCRNoHistory
.AamKeys need to be established	prior to calling this routine!!!
.
.This routine is called	externally by INFO.PLS.	 If any	changes	are made to this
.code, check for conflicts in INFO.PLS!!!
.
	move	DimPtr2,NORDFLD1
	move	DimPtr3,NORDFLD2
	move	DimPtr4,NORDFLD3
	move	DimPtr5,NORDFLD4
	move	"GetHist-NORDAIMA",Location
	pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
	call	NORDAIMA
	if over
		move	"NO EXCHANGE HISTORY",DimPtr
	else
		move	C0,N1
.Modification should not test record currently accessed.
.New will not have this	problem.
.New may have str6 = DimPtr1(LR), if record was	masked after another record.
		clear	str2
		if (FrmPtr = C1)
			move	NewFlag,str2
		elseif (FrmPtr = C8)

		endif
.START PATCH 3.64 REPLACED LOGIC
.		if (str6 <> DimPtr1 | str2 = YES)
.			if (NORDFLAG <>	C1)
.				move	C1,NORDPATH
.				call	NORDOPEN
.			endif
		if (NORDFLAG <>	C1)
			move	C1,NORDPATH
			call	NORDOPEN
		endif
		if (str6 <> DimPtr1 | str2 = YES)
.END PATCH 3.64 REPLACED LOGIC
			read	  NORDFILE,str6;str1,str1
			if not over
				if (str1 = "B" | str1 =	"0")
					move	C1,N1
				endif
			endif
		endif
		loop
			move	"NoHist-NORDKGA",Location
			call	NORDKGA
			until over
.Modification should not test record currently accessed.
.New will not have this	problem.
.New may have str6 = OLRN, if record was masked	after another record.
			if (str6 <> DimPtr1 | str2 = YES)
				read	  NORDFILE,str6;str1,str1
				if not over
					if (str1 = "B" | str1 =	"0")
						move	C1,N1
					endif
				endif
			endif
		repeat
		if (N1 = C1)
			move	"NO EXCHANGE HISTORY - RENTAL CONTINUATION",DimPtr
		else
			move	"NO EXCHANGE HISTORY",DimPtr
		endif
	endif
	return
*******************************************************************************
**************	PROCESSING FOR ORDERVERIFYDATA	*******************************
*******************************************************************************
OrderGuarLetter
	 move	 OLRN,str6
.Start patch added 12-14-01
	 move	 C0,N2
.End patch added 12-14-01
	 move	 GUARCODE,N2
CHKGUAR	 BRANCH	 N2 OF GUARWRT,GUARWRT,GUARWRT,GUARWRT:
		       GUAREND,GUAREND,GUARWRT,GUARWRT,GUARWRT
	 GOTO	 GUAREND
GUARWRT	 CLOSE	 TDMCORD
	 OPEN	 TDMCORD,"GUARANT"
.CANNOT	USE KEY	AS IT IS OF TOO	LARGE A	LENGTH SO MUST MOVE IT OVER
	 MOVE	 KEY,STR6
	 FILEPI	 1;TDMCORD
	 READ	 TDMCORD,STR6;;
	 GOTO	 GUARWRT1 IF OVER
	 FILEPI	 1;TDMCORD
	 DELETE	 TDMCORD,STR6
GUARWRT1 FILEPI	 1;TDMCORD
.START PATCH 3.71 REPLACED LOGIC
.	 WRITE	 TDMCORD,STR6;STR6,GUARCODE,INITs,GUARNAME
	 WRITE	 TDMCORD,STR6;STR6,GUARCODE,INITs,GUARNAME,GUARFAX
.END PATCH 3.71 REPLACED LOGIC
	 IF (LRINIT = 1)
	 move	 "GUARANT - 2, GUARWRT1",str45
	 call	 OrderWriteLRFile using	str45
	 ENDIF
	 GOTO	 GUAREXIT
GUAREXIT CLOSE	 TDMCORD
.could add batch job submit here.
	 call	 OrderRetrievePrinter
	 CLEAR	 TASKNAME
	 append	 "c:\progra~1\lanbatch\batch -X	-SA -Q\\nts0\c\lanbat~2	\\nts0\c\apps\winbatch\butil job=GUAR ",TASKNAME
	 append	 inits,taskname
	 append	 " F=default C=1",TASKNAME
	 APPEND	 " B=",TASKNAME
	 APPEND	 user TO TASKNAME
	 append	 " prin=",taskname
	 move	 CNTPRINT,str2
	 call	 Trim using str2
	 append	 str2,taskname
	 RESET	 TASKNAME
	 EXECUTE TASKNAME
.	OPEN	  TDMCORD,"TDMCORD"
	OPEN	 TDMCORD,"TDMCORD.isi|10.10.30.103:502"
GUAREND
	return

OrderVerifyList
.Sets default when List	# is changed.  Can be overridden.
.Called	by:  Nord001AEditList_LostFocus, SearchLoad2
	clear	onetfm
	clear	str1
	clear	str2
	unpack	netname,str1,str2
	move	str2,N2
	scan	"GROSS"	in netinfo
	if equal
		move	C1,N2
	endif
	if ((N2	= 1) OR	(N2 =4)	OR (netname = B3))
		reset	netinfo
		move	c0,onetper
		move	c0,onetrc
		move	c0,onetmin
	elseif (N2 = 2 OR N2 = 13)
		reset	netinfo
		scan	"NET DISC",netinfo
		if equal
			move	"13",N2
		endif
		if (N2 = 13)
			reset	netinfo
			move	"F" to onetfm		 .flat deduction
			move	c0 to onetper
			move	c0 to onetrc
			move	c0 to onetmin
		else
			reset	netinfo
			call	OrderNetParse
		endif
	elseif ((N2 > 4	AND N2 < 12) OR	(N2 = 3) OR (N2	= 14))
		LOAD	NniNFO USING N2	FROM NNET0,NNET0,NNET3,NNET0:
					     NNET5,NNET6,NNET7,NNET8:
					     NNET9,NNET10,NNET11,NNET12:
					     nnet13,nnet14
		UNPACK	NnINFO TO STR3,STR2,STR6,str1,STR7
		MOVE	STR2 TO	onetper		.SET NET NAME%
		MOVE	STR6 TO	onetrc		.SET RUNNING CHARGE PER	M
		MOVE	STR7 TO	onetmin		.SET MINIMUM QTY
		compare	"14" to	n2
		if equal			.volume	discount
			move	"F" to onetfm
		endif
	elseif (N2 = 12)
		move	NO,onetfm	     .no deducts allowed
		move	c0 to onetper
		move	c0 to onetrc
		move	c0 to onetmin
	endif
	if (ELSTCDE = "C")
		setprop	Nord001bComboNINGuar,enabled=0
	else
		setprop	Nord001bComboNINGuar,enabled=1
	endif
.Reset NET fields - assume processing is correct!
	if (ONETFM = NO)
		setitem	Nord001bComboNet,0,2	.GROSS BILLING NO DEDUCTS
	elseif (ONETFM = "M")
		setitem	Nord001bComboNet,0,3
	elseif (ONETFM = "F")
		setitem	Nord001bComboNet,0,4
	else
		setitem	Nord001bComboNet,0,1
	endif
	move	ONETRC,str6		.Must load EditTextBoxes with DIM var
	setitem	Nord001bEditNetCharge,0,str6
	move	ONETMIN,str7		.Must load EditTextBoxes with DIM var
...........
.	 setitem Nord001bEditNetMin,0,str7
	call	Trim using str7
	call	FormatNumeric using str7,str9
	setitem	Nord001bEditNetMin,0,str9
...........
	setitem	Nord001bEditNetPercent,0,ONETPER
combochange
	getitem	Nord001bComboNet,0,N1
	if ((N1	= 1) OR	(N1 = 2))
		call	OrderDisableNet
	else	.(N2 = 3) OR (N2 = 4)
		call	OrderEnableNet
	endif
	return

OrderNetParse
.Called	by OrderVerifyData - Code pulled from original.	  UGLY!
	scan	"%" in netinfo
	goto	nndone if not equal
	bump	netinfo	by -2
	move	netinfo	to onetper
.
	reset	netinfo
	setlptr	netinfo
	scan	"/M" in	netinfo
	goto	min if not equal
	bump	netinfo	by -1
	movefptr netinfo to mll
n1	bump	netinfo	by -1
	goto	min if eos
	cmove	netinfo	to str1
	type	str1
	goto	n1 if equal
	cmatch	"." to str1
	goto	n1 if equal
	movefptr netinfo to mfp
	add	c1 to mfp
	reset	netinfo	to mfp
	setlptr	netinfo	to mll
	move	netinfo	to onetrc
.
min	reset	netinfo
	setlptr	netinfo
	scan	"ENTIRE" in netinfo
	goto	m0 if not equal
	move	universe to onetmin
	goto	nndone
m0	reset	netinfo			   .DLH	03MAY94.
	scan	"INQUIRE" IN NETINFO
	GOTO	M1 IF NOT EQUAL
	MOVE	seq TO onetMIN
	GOTO	nnDONE
m1	rep	"( " in	netinfo
	rep	"; " in	netinfo
	scan	"$" in netinfo
	if equal
		bump	netinfo	by 1
		scan	"$" in netinfo
		if equal
			rep	"$ " in	netinfo
		endif
	endif
	reset	netinfo
	scan	"MIN" in netinfo
	goto	nndone if not equal
	bump	netinfo	by -2
	movefptr netinfo to mll
m2	bump	netinfo	by -1
	goto	nndone if eos
	cmatch	b1 to netinfo
	goto	m2 if not equal
	movefptr netinfo to mfp
	add	c1 to mfp
	reset	netinfo	to mfp
	setlptr	netinfo	to mll
	move	netinfo	to nnmin
.
nndone	reset	netinfo
	setlptr	netinfo
	scan	"M" in nnmin
	if	equal
		bump	nnmin by -1
		append	"000" to nnmin
		reset	nnmin
		move	nnmin to onetmin
	endif
	move	"M" to onetfm
	return
*******************************************************************************
*******************  FINAL PROCESSING FOR CANCEL  *****************************
*******************************************************************************
OrderCancelOrder
.Called	by OrderCancel
	if (OSTAT = "p")
		setitem	Nord001AComboPending,0,9
		goto SaveButton
.		 move	 "S",ORCODE	.Reset so no longer busy!!!
.		 move	 C1,NORDPATH
.		 move	 key,NORDFLD
.		 move	 "07",NORD4STAT
..Next two lines will prevent Re-Aamdexing and Deletion	of Special Instructions.
..Would	need to	be eliminated if we decide to allow modification with Cancel.
.		 move	 NO,SpecFlag2
.		 move	 C6,mod		 .Needed for following logic
.		 call	 OrderUpdateFiles
..		  move	  "O.CancelO.-NORDUPD,Pending",Location
..		  pack	  KeyLocation,"Key: ",NORDFLD
..		  call	  NORDUPD
..		  move	  key,str6
..		  read	  ORDPRINT,str6;;
..		  if not over
..			  call	  WRPRTR
..		  else
..			  clear	  taskname
..			  append  "Pending Order Not Found in NINPRINT!!",taskname
..			  append  carr,taskname
..			  append  "Contact I.S.	with this message!!",taskname
..			  alert	  note,taskname,result
..		  endif
..
.		 move	 "O.CancelO.-NORD4UPD,Pending",Location
.		 pack	 KeyLocation,"Key: ",NORD4FLD
.		 call	 NORD4UPD
.		 return
	elseif (OSTAT =	"l" | OSTAT = "z")
		setitem	Nord001AComboPending,0,6
		goto SaveButton
.		 move	 "z",OSTAT
.		 move	 "S",ORCODE	.Reset so no longer busy!!!
.		 move	 C1,NORDPATH
.		 move	 "O.CancelO.-NORDUPD,LCR",Location
.		 pack	 KeyLocation,"Key: ",NORDFLD
.		 call	 NORDUPD
.		 move	 "05",NORD5STAT
.		 move	 "O.CancelO.-NORD5UPD,LCR",Location
.		 pack	 KeyLocation,"Key: ",NORD5FLD
.		 call	 NORD5UPD
..Following section is not really necessary at this time.  It might be effective
..at some point	to dynamically delete from the NINPRINT.AAM file.  However, the
..NINPRINT.AAM file is currently only used by the Pending Order	Program	- NORD0037.
..
..If it	is decided to use this logic the following tests will need to be applied:
..	 Create	NEW order	 w/broker	 write to NINPRINT	 cancel	 {base 1}
..	 "			 "		 no write to NINPRINT	 cancel	 {base 2}
..	 "			 w/o broker	 write to NINPRINT	 cancel	 {base 3}
..	 "			 "		 no write to NINPRINT	 cancel	 {base 4}
..	 Modify	base 1		 w/broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 "			 w/o broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 Modify	base 2		 w/broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 "			 w/o broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 Modify	base 3		 w/broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 "			 w/o broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 Modify	base 4		 w/broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..	 "			 w/o broker	 write to NINPRINT	 cancel
..	 "			 "		 no write to NINPRINT	 cancel
..		  if (OBRKNUM <> "")
..			  pack	  str7,AKey1,OBRKNUM
..			  read	  ORDPRNTA,str7;str6,str6
..			  if not over
..				  if (holdkey <> str6)
..					  loop
..						  readkg  ORDPRNTA;str6,str6
..						  until	(holdkey = str6)
..						  until	over
..					  repeat
..				  endif
..				  filepi  1;ORDPRNTA
..				  delete  ORDPRNTA
..				  filepi  1;ORDPRINT
..				  deletek ORDPRINT,holdkey
..			  endif
..		  else
.		 filepi	 3;ORDPRINT
.		 read	 ORDPRINT,holdkey;;
.		 if not	over
.			 delete	 ORDPRINT,holdkey
.		 endif
..		  endif
.		 return
	endif
	move	YES,rprtflag		    7/15/94 added to force print
	move	"S",ORPCODE
	cmatch	"B",OSTAT
	goto UPOUTP if not equal
	move	"Q",OSTAT
	goto	WRTOUTP
UPOUTP	move	"X",OSTAT
.START PATCH 10-26-01 ADDED FOR	JD - ASH
.	 match	 "0040",ORTNNUM
.	 if equal
.START PATCH 3.77.3 REPLACED LOGIC
.	if (ORTNNUM = "0040" | ORTNNUM = "5224")
.	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.START PATCH 3.77.5 REPLACED LOGIC
.END PATCH 10-26-01 ADDED FOR JD - ASH
		reset	RUNCODES
		scan	OLNUM,RUNCODES
		if not equal
			move	"X",loltype
			FILEPI	1;TDMCORD
			read	tdmcord,holdkey;;
			if over
				filepi	1;tdmcord
				write	TDMCORD,holdkey;holdkey,loltype
				IF (LRINIT = 1)
				move	"TDMCORD - 5, UPOUTP",str45
				call	OrderWriteLRFile using str45
				ENDIF
			endif
		endif
	endif
	move	B1,loltype
WRTOUTP
	move	holdkey,OLRN	.This will affect every	instance of OLRN that follows!!!!!
	move	"S",ORCODE	.Reset so no longer busy!!!
	move	B1,OBILDRCT
	move	B1,OBRKGUAR
	move	C1,NORDPATH
	move	"WRTOUTP-NORDUPD",Location
	pack	KeyLocation,"Key: ",NORDFLD
.START PATCH 3.75 REPLACED LOGIC
.	call	NORDUPD
	call    IntegralStoreDetail using OLRN,C1
	call	NORDUPD
	call    IntegralTestDetail using OLRN,C1
.END PATCH 3.75 REPLACED LOGIC
.START PATCH 3.76.8 ADDED LOGIC
	call	OrderClearDMXFILE using OLRN
.END PATCH 3.76.8 ADDED LOGIC
	IF (LRINIT = 1)
	move	"NINORD	- Update, WRTOUTP",str45
	call	OrderWriteLRFile using str45
	ENDIF
	unpack	timestamp,NCRCCC,NCRCYY,NCRCMM,NCRCDD
	move	INITS,NCRCTYP
	move	"C",NCRCCODE
	move	holdkey,NCRCKEY
	rep	zfill,NCRCKEY
	clear	NCRCFLD
	pack	NCRCFLD,NCRCKEY
	pack	NCRCFLD2,NCRCCC,NCRCYY,NCRCMM,NCRCDD
	move	"WRTOUTP-NCRCWRT",Location
	pack	KeyLocation,"Key: ",NCRCFLD,COMMA,NCRCFLD2
	call	NCRCWRT
	IF (LRINIT = 1)
	move	"NINCRC	- 2, WRTOUTP",str45
	call	OrderWriteLRFile using str45
	ENDIF
.
	move	"WRTOUTP-NINPRINT",Location
	pack	KeyLocation,"Key: ",holdkey
	TRAP	IOMssg Giving Error if IO
	filepi	4;ORDPRINT,ORDPRNTA
	read	ORDPRINT,holdkey;;
	if over
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL	
.begin patch 3.79.2   OcompID Ocompid2
		write	ORDPRINT,holdkey;ORpCODE,OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM:
			OLON,OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE:
			ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED:
			OMDTEC,OMDTEY,OMDTEM,OMDTED,OTOCODE,OSOTCODE,OCCODE,OLRNCO:
			OODTECOC,OODTECOY,OODTECOM,OODTECOD:
			OQTYCO,OSPI,B2,OELCODE:
			OODNUM,OODES,ONETQTY,OCAMP,OCLRSTAT,OCLRINIT,OBRKRPT,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,ORENT,OHIST,OXPPM,ORTNNUM,OTAPERET,OUQTY,OSALES10,OSALES,OCOCODE,OCO2CODE:
			OODTEC,OODTEY,OODTEM,OODTED,OSCODE,OCOMSLCT,OSHP,O1DES,O2DES:
			OREUSE,ODOWJ,OEXQTY,GUARCODE,OBRKNUM,OBRKCNT:
			OSAMCDE:
			onetper,onetrc,onetfm,onetmin,OFULLFIL:
			OCompID:
			OCompID2:
			ofiller	
.end patch 3.79.2   OcompID Ocompid2
.			onetper,onetrc,onetfm,onetmin,OFULLFIL,ofiller	
			
.			onetper,onetrc,onetfm,onetmin,ofiller
.End patch 3.78.8 CODE Modification - Addition of OFULLFIL			
		insert	ORDPRNTA
		TRAPCLR	IO
		IF (LRINIT = 1)
		move	"NINPRINT - WRTOUTP",str45
		call	OrderWriteLRFile using str45
		ENDIF
	else
.Make sure OLRN	has not	been shat on
		if (holdkey <> OLRN)
			call	OrderBadNINPRINT using holdkey,OLRN
			goto WRTEXNG
		endif
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL			
.begin patch 3.79.2   OcompID Ocompid2

		update	ORDPRINT;ORpCODE,OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM:
			OLON,OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE:
			ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED:
			OMDTEC,OMDTEY,OMDTEM,OMDTED,OTOCODE,OSOTCODE,OCCODE,OLRNCO:
			OODTECOC,OODTECOY,OODTECOM,OODTECOD:
			OQTYCO,OSPI,B2,OELCODE:
			OODNUM,OODES,ONETQTY,OCAMP,OCLRSTAT,OCLRINIT,OBRKRPT,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,ORENT,OHIST,OXPPM,ORTNNUM,OTAPERET,OUQTY,OSALES10,OSALES,OCOCODE,OCO2CODE:
			OODTEC,OODTEY,OODTEM,OODTED,OSCODE,OCOMSLCT,OSHP,O1DES,O2DES:
			OREUSE,ODOWJ,OEXQTY,GUARCODE,OBRKNUM,OBRKCNT:
			OSAMCDE:
			onetper,onetrc,onetfm,onetmin,OFULLFIL:
			OcompId:
			OCompId2:
			ofiller			
.end patch 3.79.2   OcompID Ocompid2
.			onetper,onetrc,onetfm,onetmin,ofiller
.End patch 3.78.8 CODE Modification - Addition of OFULLFIL				
		TRAPCLR	IO
		IF (LRINIT = 1)
		move	"NINPRINT - Update,WRTOUTP",str45
		call	OrderWriteLRFile using str45
		ENDIF
	endif
WRTEXNG
	move	C2,NXCHPATH
	pack	NXCHFLD2,OLRN
	rep	zfill,NXCHFLD2
	move	"WRTEXNG-NXCHKEY",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHKEY
	if over
		goto NOEXLR
	endif
	pack	str8,OODTEC,OODTEY,OODTEM,OODTED
	match	str8,DAT
	goto WRTEXNG2 if equal	   *YES
WRTEXNG1
	move	C2,NXCHPATH
	move	"WRTEXNG1-NXCHKS",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHKS
	goto NOEXLR if over
	match	holdkey,LR
	goto NOEXLR if not equal    *NOT SAME ORDER
	PACK	STR8 FROM OODTEC,OODTEY,OODTEM,OODTED
	match	str8,DAT	  *SAME	ORDER?
	goto WRTEXNG1 if not equal  *NO
WRTEXNG2
	cmatch	"C",STAT	   *CHECK FOR PREVIOUS CANCELLATION
	goto SEEMOR if equal
	cmatch	"R",STAT
	goto SEEMOR if equal
	alert	plain,"Do you want Exchange balance adjusted?",result
	if (result = 1)		.yes
		goto YESADJ
	endif
NOADJ
	move	"BALANCE NOT ADJUSTED!",XCHCOMNT
	move	INITS,TYPE
	move	"X",STAT
	move	OLRN,LR
	move	C2,NXCHPATH
	move	"NOADJ-NXCHUPD",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHUPD
	IF (LRINIT = 1)
	move	"NINXCH	- Update,NOADJ",str45
	call	OrderWriteLRFile using str45
	ENDIF
	alert	note,"BALANCE WAS NOT ADJUSTED",result
	goto SEEMOR
YESADJ
	clear	XCHCOMNT		      .DLH 21Oct97
	append	"Cancelled ",XCHCOMNT
	append	today,XCHCOMNT
	append	ODOWJ,XCHCOMNT
	reset	XCHCOMNT
	move	QTY,SQTY	   SAVE	QTY FOR	UPDATE OF RUNNING BALANCE
.
	unpack	EXKEY,MALR1
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY PATCH
.	match	OMLRNUM,MALR1	   CHECKING TO SEE WHICH MAILER	PLACED ORDER
	pack	COMPFLD3,OMLRNUM
	move	"YESADJ-COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
.
	match	COMPNUM,MALR1	   CHECKING TO SEE WHICH MAILER	PLACED ORDER
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY PATCH
	goto CEXADJ if equal
	move	C2,EFLAG
	sub	QTY,USAGE2
	goto CANCUP
.
CEXADJ
	move	C1,EFLAG
	sub	QTY,USAGE1
CANCUP
	move	INITS,TYPE
	move	"C",STAT
	move	OLRN,LR
	move	C2,NXCHPATH
    	move	"CANCUP-NXCHUPD",Location
	pack	KeyLocation,"Key: ",NXCHFLD2
	call	NXCHUPD
	IF (LRINIT = 1)
	move	"NINXCH	- Update,CANCUP",str45
	call	OrderWriteLRFile using str45
	ENDIF
	move	C4,MOD		  *=======>SET MOD FOR XCHNG UPDATE
.START PATCH 3.76 REPLACED LOGIC
.	unpack	EXKEY,str4,str3,str1,blank5
.	move	blank5,SENTRY
.	pack	NXNGFLD1,AKEY1A,str4
.	pack	NXNGFLD2,AKEY2A,str3,str1
.	pack	ACKEY,str4,str3,str1
...................
	unpack	EXKEY,str6,str6A,blank5
	move	blank5,SENTRY
	pack	NXNGFLD1,AKEY1A,str6
	pack	NXNGFLD2,AKEY2A,str6A
	pack	ACKEY,str6,str6A
.END PATCH 3.76 REPLACED LOGIC
	clear	FMESG
	append	"NXNGFILE KEY-",FMESG
	append	NXNGFLD1,FMESG
	append	B1,FMESG
	append	NXNGFLD2,FMESG
	reset	FMESG
	move	"CANCUP-NXNGAIM",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGAIM
	goto ISAMBAD if	over
	compare	C1,ENTRY		*IS THIS LR THE	ONLY ONE?
	call	DELEX if equal		*YES CHECK WITH	OPERATOR IF OK TO DELETE
	compare	ENTRY,SENTRY		*IS LR ALSO LAST RECORD?
	goto XCANCEX if	equal		*IF YES	EXIT.
	move	ENTRY,SENTRY		*LAST ENTRY TO BE UPDATED.
	move	BLANK5,ENTRY
	add	C1,ENTRY
	match	ACKEY,ACCKEY
	goto PREP if equal
	move	"NXNGAIM-NXNGFILE",FMESG
	goto ISAMBAD
.
PREP	move	C1,NXCHPATH
	pack	NXCHFLD1,ACKEY,ENTRY
	rep	ZFILL,NXCHFLD1
	move	"PREP-NXCHKEY",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHKEY
	goto UPXLOOP if	over
	match	OLRN,LR
	goto XCANCEX if	equal
	cmatch	"1",EFLAG
	goto SUB1 if equal
	sub	SQTY,USAGE2
	goto UPXNG
.
SUB1	sub	SQTY,USAGE1
.
UPXNG
	move	C1,NXCHPATH
	move	INITS,TYPE
	move	"UPXNG-NXCHUPD",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHUPD
	IF (LRINIT = 1)
	move	"NINXCH	- Update,UPXNG",str45
	call	OrderWriteLRFile using str45
	ENDIF
UPXLOOP
	compare	ENTRY,SENTRY
	goto XCANCEX if	equal
	add	C1,ENTRY
	goto PREP
.
XCANCEX	move	USAGE1,WORKX1
	move	USAGE2,WORKX2
	move	B1,XCHCOMNT
	call	UPCALC1
.
	call	UPDATECANCELSPECIAL
	goto SEEMOR
.	 return
.
NOEXLR
	clear	taskname
	append	"No Exchange Record Found for LR## ",taskname
	append	holdkey,taskname
	reset	taskname
	alert	caution,taskname,result
	clear	DESC001
	call	UPDATECANCELSPECIAL
	goto SEEMOR
DELEX
	clear	taskname
	append	"This is the only Exchange for these",taskname
	append	carr,taskname
	append	"Clients.  May it be Deleted?",taskname
	reset	taskname
	alert	plain,taskname,result
	if (result <> 1)
		return
	endif
.START PATCH 3.76 REPLACED LOGIC
.	unpack	EXKEY,str4,str5
.	pack	NXNGFLD1,AKEY1A,str4
.	pack	NXNGFLD2,AKEY2A,str5
	unpack	EXKEY,str6,str6A
	pack	NXNGFLD1,AKEY1A,str6
	pack	NXNGFLD2,AKEY2A,str6A
.END PATCH 3.76 REPLACED LOGIC
	move	"DELEX-NXNGTST",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGTST
	move	"DELEX-NXNGDEL",Location
	pack	KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
	call	NXNGDEL
	move	C1,NXCHPATH
	move	EXKEY,NXCHFLD1
	move	"DELEX-NXCHTST",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHTST
	move	"DELEX-NXCHDEL",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHDEL
.START PATCH 3.76 REMOVED LOGIC
.	deletek	NXCHFLE2,holdkey
.END PATCH 3.76 REMOVED LOGIC
.Delete	Beginning Balance record
	move	C1,NXCHPATH
.START PATCH 3.76 REPLACED LOGIC
.	unpack	EXKEY,str8
.	pack	NXCHFLD1,str8,"00000"
	unpack	EXKEY,str12
	pack	NXCHFLD1,str12,"00000"
.END PATCH 3.76 REPLACED LOGIC
	move	"DELEX-NXCHTST,2",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHTST
	move	"DELEX-NXCHDEL,2",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHDEL
	noreturn
	clear	DESC001
	call	UPDATECANCELSPECIAL
	alert	note,"Exchange Status is Deleted",result
	goto SEEMOR
UPDATECANCELSPECIAL
.PREVENT I16 ERROR WHEN	SEARCHKEY IS EMPTY!!
.	 move	 KEY,NSPEFLD
.	 move	 KEY,NSPELR
	move	OLRN,NSPEFLD
	move	OLRN,NSPELR
	rep	zfill,NSPEFLD
	move	C1,NSPELOCK
	move	"XCANCEX-NSPEDEL",Location
	pack	KeyLocation,"Key: ",NSPEFLD
	call	NSPEDEL
	if (DESC002 <> "" OR DESC001 <>	"")		.There are special instructions
		move	C1,NSPELOCK
		move	"XCANCEX-NSPEWRT",Location
		call	NSPEWRT
		IF (LRINIT = 1)
		move	"NINSPEC - UPDATECANCELSPECIAL",str45
		call	OrderWriteLRFile using str45
		ENDIF
	endif
.I added this logic 8/11/00 when I added protection against I16	errors.	 ASH
	move	OLRN,NSPE2FLD
	move	OLRN,NSPE2LR
	rep	zfill,NSPE2FLD
	move	C1,NSPE2LOCK
	move	"XCANCEX-NSPE2DEL",Location
	pack	KeyLocation,"Key: ",NSPE2FLD
	call	NSPE2DEL
	if (DESC003 <> "" OR DESC004 <>	"")
		move	C1,NSPE2LOCK
		move	"XCANCEX-NSPE2WRT",Location
		call	NSPE2WRT
		IF (LRINIT = 1)
		move	"NINSPEC2 - UPDATECANCELSPECIAL",str45
      		call	OrderWriteLRFile using str45
		ENDIF
	endif
	return
OrderBadNINPRINT LRoutine DimPtr,DimPtr1
	clear	taskname
	append	"Key value and LR value	do not match!",taskname
	append	carr,taskname
	append	"Key: ",taskname
	append	DimPtr,taskname
	append	carr,taskname
	append	"LR: ",taskname
	append	DimPtr1,taskname
	append	carr,taskname
	append	"Contact I.S. immediately!",taskname
	reset	taskname
	alert	caution,taskname,result
.
	move	C0,NUSEFLD
	move	C1,NUSEPATH
	move	PORTN,NUSEFLD
	rep	zfill,NUSEFLD
	call	NUSEKEY
.
	move	"This is a message from	NORDTEST",SmtpSubject Subject
.   Set	the text message that is send with the attachments
	move	"Key value and LR value	do not match!",SmtpTextMessage(1)   Array <Text	message	>
	pack	SmtpTextMessage(2),"Key: ",DimPtr      Array <Text message >
	pack	SmtpTextMessage(3),"LR:	",DimPtr1	   Array <Text message >
	move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	move	"NTS4",SmtpEmailServer			 Address of email serverc
	move	"InformationServices@nincal.com",SmtpEmailAddress
	move	NUSEUSER,SmtpUserName				     User name
	move	NUSEUSER,SmtpUserFullName	       User Full Name
	move	smtpemailaddress,SmtpDestinations(1,1)
	move	"1",SmtpDestIndexLast				    Index to last entry	in Dest	array
	move	"0",SmtpAttIndexLast				    Index to last entry	- Only 1 entry
	clear	SmtpLogFile					    'Clear' disables the LogFile
	call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
	return
*******************************************************************************
*******************  FINAL PROCESSING FOR UPDATE  *****************************
*******************************************************************************
OrderUpdateFiles
.Called	by:  OrderSave
.begin patch 3.79.2
			If	(CompExcl <> OCompId)
				if	(CompExcl = "P")
				pack	taskname,"MLR is a PL Client Make record match ?"
				Alert	Plain,taskname,result
					IF (result = C1)
					move	CompExcl,OCompId
					endif
				Elseif	(OCompid = "P")
				pack	taskname,"Record is marked as PL Client.",Carr,"Mlr is not, Make order match ?"
				Alert	Plain,taskname,result
					IF (result = C1)
					move	CompExcl,OCompId
					endif
				endif	
			endif
.add code for Management side :(			
.end patch 3.79.2

.No revisions if in Pending/LCR	Modify modes
	compare	C6,mod
	goto writmodb if equal
	compare	C7,mod
	goto writmodb if equal
.START PATCH 3.6 ADDED LOGIC
	compare	C8,mod
	goto writmodb if equal
.END PATCH 3.6 ADDED LOGIC
REVISION
	move	INITS,NCRCTYP
	pack	str9,OODTEC,OODTEY,OODTEM,OODTED
	pack	str8,timestamp
.Allow changes within 24 hours to be applied without writing to	file
	if (str8 = str9)
		goto WRITMODB
	endif
	move	"REVISION-NINPRINT",Location
	pack	KeyLocation,"Key: ",holdkey
	TRAP	IOMssg Giving Error if IO
	filepi	1;ORDPRINT
	read	ORDPRINT,holdkey;str1,str1;
	if not over
		TRAPCLR	IO
		match	"0",str1	.OSTAT = Live Order
		goto writmodb if equal
	endif
	TRAPCLR	IO
.Removed as per	DLH 15JUL99 - ASH, ADDED 20JUL99 BY ASH	FOR I.S. USE
.revtyps now equal to I.S. initials
	reset	revtyps
	scan	INITS,revtyps
	if equal
		alert	plain,"Mark Revised?",result
		if (result <> 1)	.NO, Cancel
			goto WRITMODB
		endif
	endif
	unpack	timestamp,NCRCCC,NCRCYY,NCRCMM,NCRCDD
	move	key,NCRCKEY
	move	"R",NCRCCODE
	clear	NCRCFLD
	pack	NCRCFLD,key
	pack	NCRCFLD2,NCRCCC,NCRCYY,NCRCMM,NCRCDD
	move	"REVISION-NCRCWRT",Location
	pack	KeyLocation,"Key: ",NCRCFLD,COMMA,NCRCFLD2
	call	NCRCWRT
	IF (LRINIT = 1)
	move	"NINCRC	- REVISION",str45
	call	OrderWriteLRFile using str45
	ENDIF
WRITMODB
	move	"S",ORPCODE	.default  .approved
	if (mod	= 6 OR mod = 7)		     .pending/lcr modify mode????
		if (mod	= 6)
			move	"p",OSTAT    .DEFAULT PENDING STATUS
		else
			move	"l",OSTAT    .DEFAULT LCR STATUS
		endif
		move	"F",orpcode
		if ((mod = 6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04")) .APROVED?	      ..LEVEL 2
			move	"0",OSTAT		      .YES MARK	ORDER THE SAME
			move	"S",ORPCODE
.START PATCH 3.51 REPLACED LOGIC
.			reset	OWNCTN
.			scan	TDMC0,OWNCTN	      *TRIPLEX CCTO?
.			if equal
.				move	C0,loltype
.				goto WRITDMCP
.			endif
.			reset	OWNCTN
.			scan	TDMC1,OWNCTN	      *TRIPLEX CCTO?
.			if equal
.				move	C0,loltype
.				goto WRITDMCP
.			endif
......................
.START PATCH 3.78.4 REPLACED LOGIC
.			if (NFULFLD = "0026")			*TRIPLEX CCTO?
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.			if (NFULNUM = "009406")			*TRIPLEX CCTO?
			move 	OFULLFIL to COMPFLD
			call 	zfillit using COMPFLD
			CALL	COMPKEY			
			reset	COMPCOMP
			if (OFULLFIL = "009406")			*TRIPLEX CCTO?
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.78.4 REPLACED LOGIC
				move	C0,loltype
				goto WRITDMCP
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
			elseif (COMPCOMP <> "")
				scan	TDMC0,COMPCOMP		*TRIPLEX CCTO?
				if equal
					move	C0,loltype
					goto WRITDMCP
				endif
				reset	COMPCOMP
				scan	TDMC1,COMPCOMP		*TRIPLEX CCTO?
				if equal
					move	C0,loltype
					goto WRITDMCP
				endif
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.			elseif (NFULCOMP <> "")
.				reset	NFULCOMP
.				scan	TDMC0,NFULCOMP		*TRIPLEX CCTO?
.				if equal
.					move	C0,loltype
.					goto WRITDMCP
.				endif
.				reset	NFULCOMP
.				scan	TDMC1,NFULCOMP		*TRIPLEX CCTO?
.				if equal
.					move	C0,loltype
.					goto WRITDMCP
.				endif
			endif
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.51 REPLACED LOGIC
.START PATCH 10-26-01 ADDED FOR	JD - ASH
.			 match	 "0040",ORTNNUM
.			 if equal
.START PATCH 3.77.3 REPLACED LOGIC
.			if (ORTNNUM = "0040" | ORTNNUM = "5224")
.			if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.      	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.START PATCH 3.77.5 REPLACED LOGIC
.END PATCH 10-26-01 ADDED FOR JD - ASH
				reset	RUNCODES
				scan	OLNUM,RUNCODES
				if not equal
					move	"D",LOLTYPE
					goto WRITDMCP
				endif
			endif
			goto NOTDMCP
.
WRITDMCP
.Added Patch 12/12/00 -	Prevent	Reuse from being entered
.	       		 if (ORTNNUM <>	"0001")
				filepi	1;TDMCORD
				write	TDMCORD,holdkey;holdkey,loltype
				move	C0,OSTAT
				IF (LRINIT = 1)
				move	"TDMCORD - 1, WRITDMCP",str45
				call	OrderWriteLRFile using str45
				ENDIF
.			 endif
		endif
	endif
NOTDMCP
	if (mod	= 6)
		if (NORD4STAT =	"07" | NORD4STAT = "06")  .CANCELLED OR	DENIED	.LEVEL 3
................
			move	"x",OSTAT	.MARK ORDER
.			 if (NORD4STAT = "07")
.				 move	 "x",OSTAT	 .MARK ORDER
.			 endif
................
			move	"F",ORPCODE
		endif
	elseif (mod = 7)
		if (NORD5STAT =	"05" | NORD5STAT = "07")  .CANCELLED OR	DENIED	.LEVEL 3
................
.			 move	 "z",OSTAT	 .MARK ORDER
			if (NORD5STAT =	"05")
				move	"z",OSTAT	.MARK ORDER
			endif
................
		endif
	endif				.LEVEL 4
	move	ORPCODE,ORCODE
.Test to make sure ORCODE is a valid entry
	if (ORCODE <> "F" AND ORCODE <>	"S")
		clear	taskname
		append	"Invalid ORCODE	entry.	Call I.S. - NOW!!",taskname
		append	carr,taskname
		append	"ORCODE	= ",taskname
		append	ORCODE,taskname
		reset	taskname
		alert	caution,taskname,result
		Move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
		clear	str55
		append	"ORCODE	= ",str55
		append	ORCODE,str55
		reset	str55
		Move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
		Move	str55,SmtpTextMessage(2)   Array <Text message >
		Move	"Subroutine OrderUpdateFiles",SmtpTextMessage(3)   Array <Text message >
		Move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
		call	errmesg
	endif
.Test to make sure NORDFLD & OLRN match
	if (NORDFLD <> OLRN)
		clear	taskname
		append	"NORDFLD <> OLRN.  Call	I.S. - NOW!!",taskname
		append	carr,taskname
		append	"NORDFLD = ",taskname
		append	NORDFLD,taskname
		append	"  OLRN	= ",taskname
		append	OLRN,taskname
		reset	taskname
		alert	caution,taskname,result
		move	key,NORDFLD
		clear	taskname
		append	"NORDFLD now equals ",taskname
		append	NORDFLD,taskname
		append	carr,taskname
		append	"Continue?",taskname
		reset	taskname
		alert	plain,taskname,result
		if (result <> 1)
			goto FileGo3
.			 stop
		endif
		Move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
		clear	str55
		append	"NORDFLD = ",str55
		append	NORDFLD,str55
		append	"  OLRN	= ",str55
		append	OLRN,str55
		reset	str55
		Move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
		Move	str55,SmtpTextMessage(2)   Array <Text message >
		Move	"Subroutine OrderUpdateFiles",SmtpTextMessage(3)   Array <Text message >
		Move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
		call	errmesg
	endif
	if (HoldMlr <> OMLRNUM | HoldList <> OLNUM | HoldPO <> OMLRPON | HoldBrk <> OBRKNUM)
.AIM KEY CHANGE	OCCURED.
		CALL	INSERTA
		IF (LRINIT = 1)
		move	"NINORD	- INSERTA",str45
		call	OrderWriteLRFile using str45
		ENDIF
	endif
	move	"NOTDMCP-NORDUPD",Location
	pack	KeyLocation,"Key: ",NORDFLD
	call	NORDUPD
.START PATCH 3.78.1 ADDED LOGIC
	if (HoldMDate <> "")
		pack	str8,OMDTEC,OMDTEY,OMDTEM,OMDTED
		if (HoldMDate <> str8)
			pack	NMLDLR,OLRN
			call	Trim using NMLDLR
			if (NMLDLR <> "")
				clock	timestamp,NMLDTIME
				move	HoldMDate,NMLDDATE
				move	INITS,NMLDINIT
				move	"V.DataMDate-NMLDWRT", Location
				pack	KeyLocation,"Key: ",NMLDFLD
				call	NMLDWRT
			endif
		endif
	endif
.END PATCH 3.78.1 ADDED LOGIC
	IF (LRINIT = 1)
	move	"NINORD	- OrderUpdateFiles",str45
	call	OrderWriteLRFile using str45
	ENDIF
.START PATCH 3.6 REPLACED LOGIC
.	if (mod	= 6 OR mod = 7)		   .lcr/pending	modify?
	if (mod	= 6 OR mod = 7 OR mod = 8)		   .cancelled/lcr/pending modify?
.END PATCH 3.6 REPLACED LOGIC
		goto skiplol	.Yes, do not do	Modify LOL file	code
	endif
..................................................
.START PATCH 3.45 ADDED LOGIC
.Better solution
..................................................
.Check to see if this record has EVER been sent to Triplex
	read	TDMCsave,holdkey;;
	if over
		close	TDMCORD
		open	TDMCORD,"HotOrders"
		read	TDMCORD,holdkey;;
		if not over
			close	TDMCORD
.			open	TDMCORD,"TDMCORD"
			open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
		else
			close	TDMCORD
.			open	TDMCORD,"TDMCORD"
			open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
.START PATCH 3.51 REPLACED LOGIC
.			reset	OWNCTN
.			scan	TDMC0,OWNCTN	      *TRIPLEX CCTO?
.			if equal
.				move	C0,loltype
.				goto WRITDMCP2
.			endif
.			reset	OWNCTN
.			scan	TDMC1,OWNCTN	      *TRIPLEX CCTO?
.			if equal
.				move	C0,loltype
.				goto WRITDMCP2
.			endif
......................
.START PATCH 3.78.4 REPLACED LOGIC
.			if (NFULFLD = "0026")			*TRIPLEX CCTO?
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.			if (NFULNUM = "009406")			*TRIPLEX CCTO?
			move 	OFULLFIL to COMPFLD
			call 	zfillit using COMPFLD
			CALL	COMPKEY			
			reset	COMPCOMP
			if (OFULLFIL = "009406")			*TRIPLEX CCTO?
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.78.4 REPLACED LOGIC
				move	C0,loltype
				goto WRITDMCP2
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
			elseif (COMPCOMP <> "")
				move 	OFULLFIL to COMPFLD
				call 	zfillit using COMPFLD
				CALL	COMPKEY			
				reset	COMPCOMP
				scan	TDMC0,COMPCOMP		*TRIPLEX CCTO?
				if equal
					move	C0,loltype
					goto WRITDMCP2
				endif
				reset	COMPCOMP
				scan	TDMC1,COMPCOMP		*TRIPLEX CCTO?
				if equal
					move	C0,loltype
					goto	WRITDMCP2
				endif
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)


.Start Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.			elseif (NFULCOMP <> "")
.				reset	NFULCOMP
.				scan	TDMC0,NFULCOMP		*TRIPLEX CCTO?
.				if equal
.					move	C0,loltype
.					goto WRITDMCP2
.				endif
.				reset	NFULCOMP
.				scan	TDMC1,NFULCOMP		*TRIPLEX CCTO?
.				if equal
.					move	C0,loltype
.					goto WRITDMCP2
.				endif
			endif
.End Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.51 REPLACED LOGIC
.START PATCH 3.77.3 REPLACED LOGIC
.			if (ORTNNUM = "0040" | ORTNNUM = "5224")
.			if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.      	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.START PATCH 3.77.5 REPLACED LOGIC
				reset	RUNCODES
				scan	OLNUM,RUNCODES
				if not equal
					move	"D",LOLTYPE
					goto WRITDMCP2
				endif
			endif
			goto skiplol

WRITDMCP2
			read	TDMCORD,holdkey;;
			if over
.				pack	taskname,"This record has never been sent to Triplex.",newline,"Do you wish to do so now?"
.				alert	plain,taskname,result
.				if (result = 1)
.START PATCH 3.52 REPLACED LOGIC
.					filepi	1;TDMCORD
.					write	TDMCORD,holdkey;holdkey,loltype
.					goto skiplol
 				read	ORDPRINT,holdkey;str1
				if over
					filepi	1;TDMCORD
					write	TDMCORD,holdkey;holdkey,loltype
					goto skiplol
				elseif (str1 = "S")
					filepi	1;TDMCORD
					write	TDMCORD,holdkey;holdkey,loltype
					goto skiplol
				endif
.END PATCH 3.52 REPLACED LOGIC
.				endif
			endif
		endif
	endif
..................................................
.END PATCH 3.45 ADDED LOGIC
..................................................
	cmatch	YES,lolsw	.triplex List of list flag on ?
	if equal		.yes
.START PATCH 10-26-01 ADDED FOR	JD - ASH
.		 match	 "0040",ORTNNUM	 .return to triplex
.		 if equal		 .yes
.START PATCH 3.77.3 REPLACED LOGIC
.		if (ORTNNUM = "0040" | ORTNNUM = "5224")
.		if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.   	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.START PATCH 3.77.5 REPLACED LOGIC
.END PATCH 10-26-01 ADDED FOR JD - ASH
			reset	RUNCODES
			scan	OLNUM,RUNCODES		   .running charge list?
			if not equal			 .no
				filepi	1;TDMCORD
				read	TDMCORD,holdkey;;		   .already in file?
				if over
					cmatch	B1,loltype		.no check for valid LOL	type
					if equal
						alert	note,"NO LolType!!",result
						goto skiplol
					endif
.START PATCH 12-27-01 ADDED FOR	JD - ASH
					read	ORDPRINT,holdkey;str1
					if not over
						if (str1 <> "S")
							goto skiplol
						endif
					endif
.END PATCH 12-27-01 ADDED FOR JD - ASH
					filepi	1;tdmcord
					write	TDMCORD,holdkey;holdkey,loltype
					IF (LRINIT = 1)
					move	"TDMCORD - 2, OrderUpdateFiles",str45
					call	OrderWriteLRFile using str45
					ENDIF
				endif
			endif
		endif
.START PATCH 3.45 TEST LOGIC
.One possibility
.	elseif (LOLFlag = YES)
.		reset	OWNCTN
.		scan	TDMC0,OWNCTN	      *TRIPLEX CCTO?
.		if equal
.			move	C0,loltype
.			goto WRITDMCP2
.		endif
.		reset	OWNCTN
.		scan	TDMC1,OWNCTN	      *TRIPLEX CCTO?
.		if equal
.			move	C0,loltype
.			goto WRITDMCP2
.		endif
.		if (ORTNNUM = "0040" | ORTNNUM = "5224")
.			reset	RUNCODES
.			scan	OLNUM,RUNCODES
.			if not equal
.				move	"D",LOLTYPE
.				goto WRITDMCP2
.			endif
.		endif
.		goto skiplol
..
.WRITDMCP2
.		read	TDMCORD,holdkey;;
.		if over
..Need to check TDMCORD.SAV file.  We should not be writing to the file if it was written out at some earlier point.
.			filepi	1;TDMCORD
.			write	TDMCORD,holdkey;holdkey,loltype
.		endif
.END PATCH 3.45 TEST LOGIC
	endif
skiplol	move	B1,lolsw
	move	B1,loltype
	goto WRTOTHER
INSERTA
.
	clear	NORDFLD1
	append	"01R",NORDFLD1
	append	HoldMlr,NORDFLD1
	reset	NORDFLD1
.
	clear	NORDFLD2
	append	"02R",NORDFLD2
	append	HoldList,NORDFLD2
	reset	NORDFLD2
.
	clear	NORDFLD3
	if (HoldPO <> "")
		call	Trim using HoldPO
		append	"03L",NORDFLD3
		append	HoldPO,NORDFLD3
		reset	NORDFLD3
	endif
.
	clear	NORDFLD4
	if (HoldBrk <> "")
		append	"04R",NORDFLD4
		append	HoldBrk,NORDFLD4
		reset	NORDFLD4
	endif
	move	"INSERTA-NORDAIMA",Location
	pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
	call	NORDAIMA
	if over
		alert caution,"NORDAIMA,INSERTA-Unable to Update NINORD	AamKey!!!",result
		move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
		move	"NORDAIMA,INSERTA-Unable to Update NINORD AamKey!!!",SmtpTextMessage(1)	  Array	<Text message >
		clear	taskname
		append	"KEY:  ",taskname
		append	NORDFLD,taskname
		reset	taskname
		move	taskname,SmtpTextMessage(2)   Array <Text message >
		clear	taskname
		append	"AKEY:	",taskname
		append	NORDFLD1,taskname
		append	COMMA,taskname
		append	NORDFLD2,taskname
		append	COMMA,taskname
		append	NORDFLD3,taskname
		append	COMMA,taskname
		append	NORDFLD4,taskname
		reset	taskname
		move	taskname,SmtpTextMessage(3)   Array <Text message >
		move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
		call	errmesg
	else
		loop
			until (str6 = NORDFLD)
			call	NORDKGA
			if over
				clear	taskname
				append	"NORDKGA,INSERTA-Unable	to Update NINORD AamKey!!!",taskname
				append	carr,taskname
				append	"str6=",taskname
				append	str6,taskname
				append	"  NORDFLD=",taskname
				append	NORDFLD,taskname
				reset	taskname
				alert	caution,taskname,result
				move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
				move	"NORDKGA-Unable	to Update NINORD AamKey!!!",SmtpTextMessage(1)	 Array <Text message >
				clear	taskname
				append	"KEY:  ",taskname
				append	NORDFLD,taskname
				reset	taskname
				move	taskname,SmtpTextMessage(2)   Array <Text message >
				clear	taskname
				append	"AKEY:	",taskname
				append	NORDFLD1,taskname
				append	COMMA,taskname
				append	NORDFLD2,taskname
				append	COMMA,taskname
				append	NORDFLD3,taskname
				append	COMMA,taskname
				append	NORDFLD4,taskname
				reset	taskname
				move	taskname,SmtpTextMessage(3)   Array <Text message >
				move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
				call	errmesg
				return
			endif
		repeat
		move	"INSERTA-UPDATE	NORDFLE2",Location
		pack	KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2,COMMA,NORDFLD3,COMMA,NORDFLD4
		TRAP	IOMssg Giving Error if IO
		FILEPI	1;NORDFLE2
		UPDATE	NORDFLE2;ORDVARS
	endif
	RETURN
WRTOTHER
.LCR's always have XSTAT written out.
.XSTAT needs to	be deleted if going from LCR to	Live and it's not an Exchange
	if (SpecFlag2 =	YES OR (mod = 7	AND NORD5STAT <> "05" AND NORD5STAT <> "07") OR	(mod = 6 AND NORD4STAT <> "06" AND NORD4STAT <>	"07")) .Special	Instructions were modified
.PREVENT I16 ERROR WHEN	SEARCHKEY IS EMPTY!!
.		 move	 KEY,NSPEFLD
.		 move	 KEY,NSPELR
		move	OLRN,NSPEFLD
		rep	zfill,NSPEFLD
		move	C1,NSPELOCK
		move	"Save-NSPEDEL",Location
		pack	KeyLocation,"Key: ",NSPEFLD
		call	NSPEDEL
		if (SpecFlag = YES OR DESC001 <> "" OR DESC002 <> "")		  .There are special instructions
.Following code	is protection against situations where XSTAT is	written	to records that	are not	Exchanges.
.However, all LCR's should have	XSTAT written out and not deleted.
			if (DESC001 <> "" AND (OELCODE <> "2" AND OELCODE <> "3"))
.START PATCH 3.6 REPLACED LOGIC
.				if ((mod <> 7 OR (mod =	7 AND NORD5STAT	= "04")) AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08")))
				if ((mod <> 8) AND (mod <> 7 OR (mod = 7 AND NORD5STAT = "04")) AND (mod <> 6 OR (mod = 6 AND NORD4STAT = "08")))
.END PATCH 3.6 REPLACED LOGIC
					clear	DESC001
					if (DESC002 = "")
						goto UPDTR
					endif
				endif
			endif
			move	OLRN,NSPELR
			move	C1,NSPELOCK
			move	"Save-NSPEWRT",Location
			pack	KeyLocation,"Key: ",NSPEFLD
			call	NSPEWRT
			IF (LRINIT = 1)
			move	"NINSPEC - WRTOTHER",str45
			call	OrderWriteLRFile using str45
			ENDIF
		endif
	endif
	if (Spec2Flag =	YES | Spec3Flag	= YES |	NewFlag	= YES)
.PREVENT I16 ERROR WHEN	SEARCHKEY IS EMPTY!!
.		 move	 KEY,NSPE2FLD
		move	OLRN,NSPE2FLD
		move	C3,NSPE2LOCK
		rep	zfill,NSPE2FLD
		move	"Save-NSPE2TST",Location
		pack	KeyLocation,"Key: ",NSPE2FLD
		call	NSPE2TST
		move	"Save-NSPE2DEL",Location
		move	C1,NSPE2LOCK
		call	NSPE2DEL
		move	"Save-NSPE2WRT",Location
		move	C1,NSPE2LOCK
.PREVENT I16 ERROR WHEN	SEARCHKEY IS EMPTY!!
.		 move	 KEY,NSPE2LR
		move	OLRN,NSPE2LR
		call	NSPE2WRT
		IF (LRINIT = 1)
		move	"NINSPEC2 - WRTOTHER",str45
		call	OrderWriteLRFile using str45
		ENDIF
	endif
UPDTR
.START PATCH 3.72.8 ADDED LOGIC
	call	OrderUpdateSelectFiles
.END PATCH 3.72.8 ADDED LOGIC
	move	"UPDTR-NINPRINT",Location
	pack	KeyLocation,"Key: ",holdkey
	TRAP	IOMssg Giving Error if IO
	filepi	1;ORDPRINT		.DLH 26MAR93
	read	ORDPRINT,holdkey;orpcode,str1;			      09JUN92
	if not over
		TRAPCLR	IO
		reset	rprtcode
		scan	str1,rprtcode
		if equal
			alert	plain,"Print to	laser? ",result
			if (result = 1)	    .Yes
				goto hotord
			else
				move	NO,rprtflag
				goto updtr2a
			endif
		endif
UPDTR1
		if ((mod = 6 & nord4STAT = "08") OR (mod = 7 & NORD5STAT = "04"))    .LCR/pending approved ?
			move	YES,upsw
			goto updtr2a		      .yes, treat as live
		endif
	else
		TRAPCLR	IO
.START PATCH 3.75.1 ADDED LOGIC
		if (mod = 6 & nord4stat = "08")    .Pending approved ?
			move	YES,upsw
			goto updtr2a		      .yes, treat as live
		endif
.END PATCH 3.75.1 ADDED LOGIC
.................
.		 if (mod = 7 AND (NORD5STAT = "04" OR NORD5STAT	= "05" OR NORD5STAT = "07"))
		if (mod	= 7 AND	(NORD5STAT = "04" OR NORD5STAT = "05"))
.................
			goto updtr2a
		endif
		if ((OSTAT = "p" | OSTAT = "l")	AND OCO2CODE <>	"" AND OCO2CODE	<> "  ")	.Valid Caller
.			 move	 OLRN,NORDPFLD
.			 move	 C1,NORDPPATH
.			 move	 "reprint-NORDPTST",Location
.			 pack	 KeyLocation,"Key: ",NORDPFLD
.			 call	 NORDPTST
.			 if over
				goto updtr2a
.			 endif
		endif
.START PATCH IT	UP
.		 alert	 plain,"Do you want to reprint this order?",result
.		 if (result <> 1)	  .No, Cancel
.			 if (OSTAT = "l")
.				 if (OCO2CODE =	"" | OCO2CODE =	"  ")	 .No Caller
.					 move	 "reprint-NORDPDEL",Location
.					 pack	 KeyLocation,"Key: ",NORDPFLD
.					 call	 NORDPDEL
.				 endif
.				 goto	 HotOrd
.			 endif
.			 goto SEEMOR
.		 endif
................
		if (OSTAT <> "l" AND OSTAT <> "z")
			alert	plain,"Do you want to reprint this order?",result
			if (result <> 1)	 .No, Cancel
				goto SEEMOR
			endif
		endif
.END PATCH IT UP
UPDTR2		if (OSTAT <> "l" AND OSTAT <> "z")	.Don't trip flag if LCR!!!!
			move	YES,rprtflag
			move	YES,UPSW
		endif
	endif
UPDTR2A
.START PATCH 3.6 REPLACED LOGIC
.	if ((mod <> 7 OR (mod =	7 & NORD5STAT =	"04")) AND (mod	<> 6 OR	(mod = 6 & NORD4STAT = "08")))
	if (mod <> 8 AND (mod <> 7 OR (mod = 7 & NORD5STAT = "04")) AND (mod <> 6 OR (mod = 6 & NORD4STAT = "08")))
.END PATCH 3.6 REPLACED LOGIC
.START PATCH 3.51 REPLACED LOGIC
.		reset	OWNCTN
.		scan	TDMC0,OWNCTN	      *TRIPLEX CCTO?
.		goto WRITDMR if	equal	     *YES
.		reset	OWNCTN
.		scan	TDMC1,OWNCTN	      *TRIPLEX CCTO?
.		goto WRITDMR if	equal
......................
.START PATCH 3.78.4 REPLACED LOGIC
.		if (NFULFLD = "0026")			*TRIPLEX CCTO?
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.		if (NFULNUM = "009406")			*TRIPLEX CCTO?
		move 	OFULLFIL to COMPFLD
		call 	zfillit using COMPFLD
		CALL	COMPKEY			
		reset	COMPCOMP
		if (OFULLFIL = "009406")			*TRIPLEX CCTO?
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.78.4 REPLACED LOGIC
			goto WRITDMR
		elseif (COMPCOMP <> "")
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
			scan	TDMC0,COMPCOMP		*TRIPLEX CCTO?
			goto 	WRITDMR if equal
			reset	COMPCOMP
			scan	TDMC1,COMPCOMP		*TRIPLEX CCTO?
			goto 	WRITDMR if equal				
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.Start Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.		elseif (NFULCOMP <> "")
.			reset	NFULCOMP
.			scan	TDMC0,NFULCOMP		*TRIPLEX CCTO?
.			goto WRITDMR if equal
.			reset	NFULCOMP
.			scan	TDMC1,NFULCOMP		*TRIPLEX CCTO?
.			goto WRITDMR if equal
.End Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
		endif
.END PATCH 3.51 REPLACED LOGIC
	endif
	call	OrderWriteNINPrint
	return
.This section is skipped if an LCR
WRITDMR
	filepi	1;TDMCORD
	read	TDMCORD,holdkey;;
	if not over
		call	OrderWriteNINPrint
		return
.Added Patch 12/12/00 -	Prevent	Reuse from being entered
.	 elseif	(ORTNNUM = "0001")
.		 call	 OrderWriteNINPrint
.		 return
	endif
.START PATCH 3.68 REPLACED LOGIC
.This initialization needs to occur!!
.	move	C0,result
.	filepi	1;TDMCsave	       .DLH  06MAR96
.	read	TDMCsave,holdkey;;
.	goto tdmr2 if over
.	alert	plain,"Resend this TDMC	order to Triplex?",result
.	goto writdmr1
.Tdmr2	filepi	1;TDMCdel	      .DLH  06MAR96
.	read	TDMCdel,holdkey;;
.	goto writdmr1 if over
.	alert	plain,"Previously deleted from TDMC T/C, send now?",result
.writdmr1
.	if (result <> 1)
.		call	OrderWriteNINPrint
.		return
.	endif
.	clear	loltype
.	move	OSTAT,loltype
.	rep	"R0",loltype
.	filepi	1;TDMCORD
.	write	TDMCORD,holdkey;holdkey,loltype
.	IF (LRINIT = 1)
.     	move	"TDMCORD - 3, writdmr1",str45
.	call	OrderWriteLRFile using str45
.	ENDIF
..TDMCEXIT - IF RECORD HAS BEEN PREVIOUSLY DELETED FROM T/C FILE. REMOVE	FROM
..	    LOG	BECAUSE	IS NOW BEING SET UP FOR	T/C.
.EXITDMC
.	filepi	1;TDMCORD	      .DLH  26MAR93
.	read	TDMCDEL,holdkey;;
.	goto EXITDMC1 if over
.	filepi	1;TDMCDEL
.	delete	TDMCDEL,holdkey
.EXITDMC1
.	call	OrderWriteNINPrint
.	return
................
	filepi	1;TDMCsave	       .DLH  06MAR96
	read	TDMCsave,holdkey;;
	if over
		close	TDMCORD
		open	TDMCORD,"HotOrders"
		read	TDMCORD,holdkey;;
		if over
			close	TDMCORD
.			open	TDMCORD,"TDMCORD"
			open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
			pack	taskname,"There is no indication this LR was sent to Triplex!",newline,"Do you want to send it now?"
			alert	plain,taskname,result
			if (result = 1)
				clear	loltype
				move	OSTAT,loltype
				rep	"R0",loltype
				filepi	1;TDMCORD
				write	TDMCORD,holdkey;holdkey,loltype
				IF (LRINIT = 1)
			     	move	"TDMCORD - 3, writdmr1",str45
				call	OrderWriteLRFile using str45
				ENDIF
.TDMCEXIT - IF RECORD HAS BEEN PREVIOUSLY DELETED FROM T/C FILE. REMOVE	FROM
.	    LOG	BECAUSE	IS NOW BEING SET UP FOR	T/C.
				filepi	1;TDMCORD	      .DLH  26MAR93
				read	TDMCDEL,holdkey;;
				if not over
					filepi	1;TDMCDEL
					delete	TDMCDEL,holdkey
				endif
			endif
		else
			close	TDMCORD
.			open	TDMCORD,"TDMCORD"
			open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
		endif
	endif
	call	OrderWriteNINPrint
	return
.END PATCH 3.68 REPLACED LOGIC
*******************************************************************************
**************	FINAL PROCESSING FOR UPDATE/SAVE  *****************************
*******************************************************************************
OrderWriteTDMC
.Called	by:  NORDMSK1ButtonSave_Click
.Start patch added 12-14-01
	move	C0,N2
.End patch added 12-14-01
	move	GUARCODE,N2
	branch	N2 OF CALLGUAR,CALLGUAR,CALLGUAR,CALLGUAR:
		CHKTDMC,CHKTDMC,CALLGUAR,CALLGUAR,CALLGUAR
	goto CHKTDMC
CALLGUAR call	WRITGUAR
CHKTDMC
.START PATCH 3.51 REPLACED LOGIC
.	reset	OWNCTN
.	scan	TDMC0,OWNCTN	      *TRIPLEX CCTO?
.	if equal
.		move	C0,loltype
.		goto WRITDMC
.	endif
.	reset	OWNCTN
.	scan	TDMC1,OWNCTN	      *TRIPLEX CCTO?
.	if equal
.		move	C0,loltype
.		goto WRITDMC
.	endif
.START PATCH 3.78.4 REPLACED LOGIC
.	if (NFULFLD = "0026")			*TRIPLEX CCTO?
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.	if (NFULNUM = "009406")			*TRIPLEX CCTO?
	move 	OFULLFIL to COMPFLD
	call 	zfillit using COMPFLD
	CALL	COMPKEY			
	reset	COMPCOMP
	if (OFULLFIL = "009406")			*TRIPLEX CCTO?	
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.78.4 REPLACED LOGIC
		move	C0,loltype
		goto WRITDMC
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
	elseif (COMPCOMP <> "")
		scan	TDMC0,COMPCOMP		*TRIPLEX CCTO?
		if equal
			move	C0,loltype
			goto WRITDMC
		endif
		reset	COMPCOMP
		scan	TDMC1,COMPCOMP		*TRIPLEX CCTO?
		if equal
			move	C0,loltype
			goto WRITDMC
		endif			
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.Start Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.	elseif (NFULCOMP <> "")
.		reset	NFULCOMP
.		scan	TDMC0,NFULCOMP		*TRIPLEX CCTO?
.		if equal
.			move	C0,loltype
.			goto WRITDMC
.		endif
.		reset	NFULCOMP
.		scan	TDMC1,NFULCOMP		*TRIPLEX CCTO?
.		if equal
.			move	C0,loltype
.			goto WRITDMC
.		endif
.End Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
	endif
.END PATCH 3.51 REPLACED LOGIC
.START PATCH 10-26-01 ADDED FOR	JD - ASH
.	match	"0040",ORTNNUM	.return	to triplex
.	if equal		.yes
.START PATCH 3.77.3 REPLACED LOGIC
.	if (ORTNNUM = "0040" | ORTNNUM = "5224")
.	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.START PATCH 3.77.5 REPLACED LOGIC
.END PATCH 10-26-01 ADDED FOR JD - ASH
		reset	RUNCODES
		scan	OLNUM,RUNCODES
		if not equal
			move	"D",loltype
			goto WRITDMC
		endif
	endif
	return
WRITGUAR close	TDMCORD
	open	TDMCORD,"GUARANT"
	filepi	1;TDMCORD
.Cannot	use key	as its'	length is too long so move it over
	move	key,str6
.START PATCH 3.71 REPLACED LOGIC
.	write	TDMCORD,str6;str6,GUARCODE,INITs,GUARNAME
	write	TDMCORD,str6;str6,GUARCODE,INITs,GUARNAME,GUARFAX
.END PATCH 3.71 REPLACED LOGIC
	close	TDMCORD
	IF (LRINIT = 1)
	move	"GUARANT - WRITGUAR",str45
	call	OrderWriteLRFile using str45
	ENDIF
.	 open	 TDMCORD,"TDMCORD"
	open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
	return
.
WRITDMC
.Cannot	use key	as its'	length is too long so move it over
.Added Patch 12/12/00 -	Prevent	Reuse from being entered
.	 if (ORTNNUM <>	"0001")
		move	key,str6
.START PATCH ADDED TO TRAP FOR PENDING/LCRS BEING WRITTEN TO TDMC FILE 12/21/01
		move	"WRITDMC-ApproveButton",Location
		call	TestTDMCError
.END PATCH ADDED TO TRAP FOR PENDING/LCRS BEING	WRITTEN	TO TDMC	FILE 12/21/01
.START PATCH 12-27-01 ADDED FOR	JD - ASH
.		 filepi	 1;TDMCORD
.		 write	 TDMCORD,str6;str6,loltype
		read	TDMCORD,str6;;
		if over
			filepi	1;TDMCORD
			write	TDMCORD,str6;str6,loltype
		else
			filepi	1;TDMCORD
			update	TDMCORD;str6,loltype
		endif
.END PATCH 12-27-01 ADDED FOR JD - ASH
		IF (LRINIT = 1)
		move	"TDMCORD - 4, WRITDMC",str45
		call	OrderWriteLRFile using str45
		ENDIF
		move	C0,OSTAT	.redundant as you cannot ADD an	Order that is Billed,CAncelled,etc.
.	 endif
	return

.START PATCH ADDED TO TRAP FOR PENDING/LCRS BEING WRITTEN TO TDMC FILE 12/21/01
TestTDMCError
	if (NORDFLAG <>	1)
		call	NORDOPEN
	endif
	read	NORDFILE,str6;str2
	unpack	str2,str1,str1
	if (str1 = "l" | str1 =	"p")
		move	"This is a Error e-mail	from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
		move	"This is an error message",SmtpTextMessage(1)	.Array <Text message >
		move	"WRITDMC Error",SmtpTextMessage(2)		.Array <Text message >
		move	Location,SmtpTextMessage(3)			.Array <Text message >
		move	Location,SmtpTextMessage(4)			.Array <Text message >
		move	"4",SmtpTextIndexLast				.Index to last entry in	TextMessage array
		call	  errmesg
.
		create	ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
		activate EditTextBoxes(1)
		clear	ErrMssg
		append	Location,ErrMssg
		reset	ErrMssg
		setprop	ErrorMssgStat1,visible=1
		setprop	ErrorMssgStat2,visible=1
		setprop	ErrorMssgStat3,visible=1
		setprop	ErrorMssgStat4,visible=0
		setprop	ErrorMssgStat5,visible=1
		setitem	ErrorMssgStat1,0,"WRITDMC Error"
		setitem	ErrorMssgStat2,0,"Please Leave this Information	on"
		setitem	ErrorMssgStat5,0,"    Screen and Inform	I.S.!"
		setitem	ErrorMssgStat3,0,ErrMssg
		setitem	ErrorMssgOK,0,"&Stop"
		loop
      			setfocus EditTextBoxes(1)
			setprop	ErrorMssg,visible=1
			getitem	EditTextBoxes(1),0,str1
			until (str1 = "^")
		repeat
		destroy	EditTextBoxes(1)
	endif
	return
.END PATCH ADDED TO TRAP FOR PENDING/LCRS BEING	WRITTEN	TO TDMC	FILE 12/21/01

OrderFinalExchange
.Called	by:  NORDMSK1ButtonSave_Click
	unpack	EXKEY,ACKEY,str5	.SAVE KEY
	move	C1,NXCHPATH
	pack	NXCHFLD1,EXKEY
	rep	zfill,NXCHFLD1
	move	"O.FinalExchange-NXCHTST",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHTST
	if not over
		move	"EXCHANGE" TO FMESG
		goto DUPE
	endif
	pack	EXKEY,ACKEY,str5	.RESTORE KEY.
.Trap logic that pertains to other IO's	will be	useless	with Exchange file as we need to continue
.if we get a DUPE.  Location not necessary but will put	it in in case I	forget above and include
.error trapping	in IO.	That way I will	be able	to see my error	and correct it quickly.	 ASH
	trap	DUPE GIVING ERROR IF IO
	move	B1,XCHCOMNT
	if ((mod = 6 AND NORD4STAT = "08") OR (mod = 7 AND NORD5STAT = "04"))
		move	timestamp,str8
	else
		pack	str8,OODTEC,OODTEY,OODTEM,OODTED
	endif
	move	str8,DAT
	move	B1,STAT
	move	KEY,LR
	move	OLNUM,LIST
	type	OEXQTY
	if equal
		move	OEXQTY,QTY
	else
		move	OQTY,QTY
	endif
	move	INITS,TYPE
	move	"O.FinalExchange-NXCHWRT",Location
	pack	KeyLocation,"Key: ",NXCHFLD1
	call	NXCHWRT
	IF (LRINIT = 1)
	move	"NINXCH	- OrderFinalExchange",str45
	call	OrderWriteLRFile using str45
	ENDIF
	return

OrderWriteNINPRINT
.Approved LCR's	are taken care of at 'Verify OStat'
.START PATCH 3.6 ADDED LOGIC
	if (mod = 8)
.This record should not be in EITHER of these files, but double-check anyway, and delete if found.
		move	OLRN,NORDPFLD
		move	C1,NORDPPATH
		move	"NORDPTST-A",Location
		pack	KeyLocation,"Key: ",NORDPFLD
		call	NORDPTST
		if not over
			move	"NORDPDEL-A",Location
			call	NORDPDEL
		endif
		move	"NINPRINT-A",Location
		pack	KeyLocation,"Key: ",holdkey
		TRAP	IOMssg Giving Error if IO
		move	holdkey,str6
		rep	zfill,str6
		filepi	3;ORDPRINT
		read	ORDPRINT,str6;;
		if not over
			delete	ORDPRINT,str6
		endif
		trapclr	IO
		goto SEEMOR
	endif
.END PATCH 3.6 ADDED LOGIC
	if (mod	= 3 OR (mod = 7	& NORD5STAT <> "04"))
		if (NORD5STAT <> "05" AND NORD5STAT <> "07") .Cancelled/Denied
.START PATCH 3.67 REPLACED LOGIC
.			call	OrderLCRPrintIt
			call	OrderLCRPrintIt using C0
.END PATCH 3.67 REPLACED LOGIC
		else
.HotFlag set in	OrderLCRPrintIt.  If a Cancelled Outside LCR we	need to	prevent	it from	going out as an	Order.
			move	NO,hotflag
		endif
		move	OLRN,NORDPFLD
		move	C1,NORDPPATH
		move	"NORDPTST",Location
		pack	KeyLocation,"Key: ",NORDPFLD
		call	NORDPTST
		if over
.You should not	write Denied records back to file after	Contact	has been notified and record deleted.
			if (OCO2CODE <>	"" AND OCO2CODE	<> "  "	AND OSTAT <> "z" AND OHIST <> "z")	  .Valid Caller/Not Cancelled or Denied
				move	"F",ORCODE
				move	"O.Wrt.NPrint-NORDPWRT",Location
				pack	KeyLocation,"Key: ",NORDPFLD
				call	NORDPWRT
				IF (LRINIT = 1)
				move	"NINPRINTL - 1,	OrderWriteNINPRINT",str45
				call	OrderWriteLRFile using str45
				ENDIF
			endif
		else
			if (OCO2CODE <>	"" AND OCO2CODE	<> "  "	AND NORD5STAT <> "05")	      .Valid Caller/Not	Cancelled
				move	"F",ORCODE
				move	C2,NORDPPATH
				move	"O.W.N.PRINT-NORDPAIMA",Location
				pack	KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
				call	NORDPAIMA
				if over
					clear	taskname
					append	"NORDPAIMA,O.W.N.Print-Unable to Update	NINPRINTL AamKey!!!",taskname
					append	carr,taskname
					append	"Key: ",taskname
					append	NORDPFLD,taskname
					append	COMMA,taskname
					append	NORDPFLD1,taskname
					append	COMMA,taskname
					append	NORDPFLD2,taskname
					append	COMMA,taskname
					append	NORDPFLD3,taskname
					append	COMMA,taskname
					append	NORDPFLD4,taskname
					reset	taskname
					alert caution,taskname,result
				else
					move	"O.W.N.PRINT-NORDPKGA",Location
					pack	KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
					loop
						until (str6 = NORDPFLD)
						call	NORDPKGA
						if over
							clear	taskname
							append	"NORDPKGA,O.W.N.Print-Unable to	Update NINPRINTL AamKey!!!",taskname
							append	carr,taskname
							append	"str6=",taskname
							append	str6,taskname
							append	"  NORDPFLD=",taskname
							append	NORDPFLD,taskname
							append	carr,taskname
							append	"NORDPFLD1=",taskname
							append	NORDPFLD1,taskname
							append	",NORDPFLD2=",taskname
							append	NORDPFLD2,taskname
							append	",NORDPFLD3=",taskname
							append	NORDPFLD3,taskname
							append	",NORDPFLD4=",taskname
							append	NORDPFLD4,taskname
							reset	taskname
							alert	caution,taskname,result
						endif
					repeat
					move	"O.W.N.PRINT-NORDPFLE",Location
					pack	KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
					TRAP	IOMssg Giving Error if IO
					FILEPI	1;NORDPFLE
					UPDATE	NORDPFLE;ORDVARS
					TRAPCLR	IO
					IF (LRINIT = 1)
					move	"NINPRINTL - 2,	OrderWriteNINPRINT",str45
					call	OrderWriteLRFile using str45
					ENDIF
				endif
			else						.No Caller
				move	"O.W.N.PRINT-NORDPDEL",Location
				pack	KeyLocation,"Key: ",NORDPFLD
				call	NORDPDEL
			endif
		endif
		if (OCO2CODE <>	"" AND OCO2CODE	<> "  ")		.Valid Caller
.If found in NINPRINT and flagged to fax update	NINPRINT, otherwise delete
			move	"O.Wrt.NPrint-read ORDPRINT",Location
			pack	KeyLocation,"Key: ",NORDPFLD
			TRAP	IOMssg Giving Error if IO
			filepi	5;ORDPRINT
			read	ORDPRINT,NORDPFLD;;
			if not over
				if (OHIST = "*")
.Added in order	to ensure record does not get caught at	WRTPRNTR/WRTPRTR
					TRAPCLR	IO
					move	NORDPFLD,str6
					goto WRTPRNTR
				else
					move	"O.W.N.Print-DEL.O.PRINT",Location
					pack	KeyLocation,"Key: ",NORDPFLD
					delete	ORDPRINT,NORDPFLD
				endif
			endif
			TRAPCLR	IO
			return
		endif
		if (hotflag = NO)
			return
		endif
.Keeping the In-House List Management Pending Orders/LCR's separate in order to	prevent	extra confusion!!
	elseif (mod = 5	OR (mod	= 6 & NORD4STAT	<> "08"))
		move	OLRN,NORDPFLD
		move	C1,NORDPPATH
		move	"NORDPTST2",Location
		pack	KeyLocation,"Key: ",NORDPFLD
		call	NORDPTST
		if over
.You should not	write Denied records back to file after	Contact	has been notified and record deleted.
			if (OCO2CODE <>	"" AND OCO2CODE	<> "  "	AND OSTAT <> "x" AND OHIST <> "z" AND OHIST <> "p")	   .Valid Caller/Not Cancelled or Denied
				move	"F",ORCODE
				move	"O.Wrt.NPrint2-NORDPWRT",Location
				pack	KeyLocation,"Key: ",NORDPFLD
				call	NORDPWRT
				IF (LRINIT = 1)
				move	"NINPRINTL - 3,	OrderWriteNINPRINT",str45
				call	OrderWriteLRFile using str45
				ENDIF
			endif
		else
			if (OCO2CODE <>	"" AND OCO2CODE	<> "  "	AND NORD4STAT <> "07")	      .Valid Caller/Not	Cancelled
				move	"F",ORCODE
				move	C2,NORDPPATH
				move	"O.W.N.PRINT2-NORDPAIMA",Location
				pack	KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
				call	NORDPAIMA
				if over
					clear	taskname
					append	"NORDPAIMA2,O.W.N.Print-Unable to Update NINPRINTL AamKey!!!",taskname
					append	carr,taskname
					append	"Key: ",taskname
					append	NORDPFLD,taskname
					append	COMMA,taskname
					append	NORDPFLD1,taskname
					append	COMMA,taskname
					append	NORDPFLD2,taskname
					append	COMMA,taskname
					append	NORDPFLD3,taskname
					append	COMMA,taskname
					append	NORDPFLD4,taskname
					reset	taskname
					alert caution,taskname,result
				else
					move	"O.W.N.PRINT2-NORDPKGA",Location
					pack	KeyLocation,"Key: ",NORDPFLD1,COMMA,NORDPFLD2,COMMA,NORDPFLD3,COMMA,NORDPFLD4
					loop
						until (str6 = NORDPFLD)
						call	NORDPKGA
						if over
							clear	taskname
							append	"NORDPKGA2,O.W.N.Print-Unable to Update	NINPRINTL AamKey!!!",taskname
							append	carr,taskname
							append	"str6=",taskname
							append	str6,taskname
							append	"  NORDPFLD=",taskname
							append	NORDPFLD,taskname
							append	carr,taskname
							append	"NORDPFLD1=",taskname
							append	NORDPFLD1,taskname
							append	",NORDPFLD2=",taskname
							append	NORDPFLD2,taskname
							append	",NORDPFLD3=",taskname
							append	NORDPFLD3,taskname
							append	",NORDPFLD4=",taskname
							append	NORDPFLD4,taskname
							reset	taskname
							alert	caution,taskname,result
						endif
					repeat
					move	"O.W.N.PRINT-NORDPFLE",Location
					pack	KeyLocation,"Key: ",NORDPFLD
					TRAP	IOMssg Giving Error if IO
					FILEPI	1;NORDPFLE
					UPDATE	NORDPFLE;ORDVARS
					TRAPCLR	IO
					IF (LRINIT = 1)
					move	"NINPRINTL - 4,	OrderWriteNINPRINT",str45
					call	OrderWriteLRFile using str45
					ENDIF
				endif
			else	     					.No Caller
				move	"O.W.N.PRINT-NORDPDEL",Location
				pack	KeyLocation,"Key: ",NORDPFLD
				call	NORDPDEL
			endif
		endif
.Refresh OHIST as NINPRINT value may differ from NINPRINTL/NINORD.  If LM In-House LCR has
.been faxed to Owner already, you do not want it to go again, unless specified.
.NORD0008 will re-write	OHIST to B1 once it is faxed to	owner.	NINORD & NINPRINTL will	still
.have "*" as value of OHIST.
		if (OHIST = "*")
.			 move	 key,str6
			move	"O.W.N.2-NINPRINT",Location
			pack	KeyLocation,"Key: ",holdkey
			TRAP	IOMssg Giving Error if IO
			move	holdkey,str6
			read	ORDPRINT,str6;str55,str55,str55,str10,str1;
.This check needs to happen in case you	are dealing with a New Pending order, in which
.case you would	receive	an over, which would clear OHIST
			if not over
				move	str1,OHIST
			endif
			TRAPCLR	IO
		endif
	endif
	reset	RUNCODES
	scan	OLNUM,RUNCODES
	if equal
		move	"S",ORPCODE		*auto approve runcharge
		goto PRINTCDE
	endif
	move	C0,N9
	move	OQTY,N9
	if (N9 = C0)
		move	"S",ORPCODE		*auto approve 0	qty
		goto PRINTCDE
	endif
	move   "F",ORPCODE			*not approved yet
PRINTCDE
	move	"F",ORCODE			*NOT APPROVED YET.
	if (mod	= 1 OR mod = 3 OR mod =	5)	.Add New Order,LCR or Pending
		goto WRTPRNTB
	endif
	if (OSTAT <> "z")	.Check for Cancelled LCR - not in CANCODES!
		reset	CANCODES
		scan	OSTAT,CANCODES			.CANCELLED?
		if not equal				.LEVEL 0
.Cannot	use key	as its'	length is too long so move it over
.			 move	 key,str6
			move	"PRINTCDE-NINPRINT",Location
			pack	KeyLocation,"Key: ",holdkey
			TRAP	IOMssg Giving Error if IO
			move	holdkey,str6
			rep	zfill,str6
			filepi	1;ORDPRINT
			read	ORDPRINT,str6;ORPCODE,OSTAT;	 .09JUN92
.			 read	 ORDPRINT,str6;;		.08Oct99 - remmed and old line replaced	above
			if over
				TRAPCLR	IO
				move	"R",OSTAT	.it's a	reprint
				move	"S",ORPCODE
				goto WRTPRNTB
			else
				TRAPCLR	IO
				goto WRTPRNTR
			endif
		endif
	endif
.Cannot	use key	as its'	length is too long so move it over
.	 move	 key,str6
	move	"PRINTCDE2-NINPRINT",Location
	pack	KeyLocation,"Key: ",holdkey
	TRAP	IOMssg Giving Error if IO
	move	holdkey,str6
	rep	zfill,str6
	filepi	1;ORDPRINT
	read	ORDPRINT,str6;;			  09JUN92
	goto WRTPRNTR if not over
	TRAPCLR	IO
WRTPRNTB
	if (mod	= 5 OR mod = 6)			.Pending Add/Modify mode
		move	"F",ORPCODE
		move	"p",OSTAT
	elseif (mod = 3	OR mod = 7)		.LCR Add/Modify	mode
		move	"F",ORPCODE
		move	"l",OSTAT
	endif
	if (mod	= 6)
		if (NORD4STAT =	"08")		.APROVED?
			move	"0",OSTAT		.YES MARK ORDER	THE SAME
			move	"S",ORPCODE
		elseif (NORD4STAT = "07" | NORD4STAT = "06")  .CANCELLED OR DENIED  .LEVEL 4
................
			move	"x",OSTAT	.MARK ORDER
.			 if (NORD4STAT = "07")
.				 move	 "x",OSTAT	 .MARK ORDER
.			 endif
................
			move	"F",ORPCODE
		endif
	elseif (mod = 7)
		if (NORD5STAT =	"04")		.APPROVED?
			move	"0",OSTAT		.YES MARK ORDER	THE SAME
			move	"S",ORPCODE
		elseif (NORD5STAT = "05" OR NORD5STAT =	"07") .CANCELLED/DENIED	LCR  .LEVEL 4
................
.			 move	 "z",OSTAT	 .MARK ORDER
			if (NORD5STAT =	"05")
				move	"z",OSTAT	.MARK ORDER
			endif
................
		endif
	endif
.	 if (str6 <> OLRN)
.		 call	 OrderBadNINPRINT using	str6,OLRN
.		 goto SEEMOR
.	 endif
.Cannot	use key	as its'	length is too long
	move	"WRTPRNTB-NINPRINT",Location
	pack	KeyLocation,"Key: ",str6
	TRAP	IOMssg Giving Error if IO
	move	OLRN,str6
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL					
.begin patch 3.79.2   OcompID Ocompid2
	filepi	2;ORDPRINT,ORDPRNTA
	write	ORDPRINT,str6;ORPCODE,OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM,OLON:
		OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE,ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED:
		OMDTEC,OMDTEY,OMDTEM,OMDTED,OTOCODE,OSOTCODE,OCCODE,OLRNCO:
		OODTECOC,OODTECOY,OODTECOM,OODTECOD,OQTYCO,OSPI,OBILDRCT,OBRKGUAR:
		OELCODE,OODNUM,OODES,ONETQTY,OCAMP,OCLRSTAT,OCLRINIT,OBRKRPT,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,ORENT,OHIST,OXPPM,ORTNNUM,OTAPERET,OUQTY,OSALES10,OSALES:
		OCOCODE,OCO2CODE,OODTEC,OODTEY,OODTEM,OODTED,OSCODE,OCOMSLCT,OSHP:
		O1DES,O2DES,OREUSE,INITs,OEXQTY,GUARCODE,OBRKNUM,OBRKCNT:
	       	OSAMCDE,ONETPER,ONETRC,ONETFM,ONETMIN,OFULLFIL:
	       	OcompId:
	       	OCompid2:
	       	OFILLER		
.end patch 3.79.2   OcompID Ocompid2
.	       	OSAMCDE,ONETPER,ONETRC,ONETFM,ONETMIN,OFILLER
.End patch 3.78.8 CODE Modification - Addition of OFULLFIL					       	
	insert	ORDPRNTA
	TRAPCLR	IO
	IF (LRINIT = 1)
	move	"NINPRINT - 1, WRTPRNTB",str45
	call	OrderWriteLRFile using str45
	ENDIF
	GOTO	SEEMOR
.
WRTPRNTR
	if (mod	= 6)			.pending modify	mode????
		move	"p",OSTAT	.DEFAULT PENDING STATUS
		move	"F",ORPCODE
		if (NORD4STAT =	"08")	 .APPROVED?
			move	"0",OSTAT		      .YES MARK	ORDER THE SAME
			move	"S",ORPCODE
		elseif (NORD4STAT = "07" | NORD4STAT = "06")  .CANCELLED OR DENIED  .LEVEL 4
................
			move	"x",OSTAT	.MARK ORDER
.			 if (NORD4STAT = "07")
.				 move	 "x",OSTAT	 .MARK ORDER
.			 endif
................
			move	"F",ORPCODE
		endif
	elseif (mod = 7)
		move	"l",OSTAT	.DEFAULT PENDING STATUS
		move	"F",ORPCODE
		if (NORD5STAT =	"04")	 .APPROVED?
			move	"0",OSTAT		      .YES MARK	ORDER THE SAME
			move	"S",ORPCODE
		elseif (NORD5STAT = "05" | NORD5STAT = "07")  .CANCELLED/DENIED	 .LEVEL	4
................
.			 move	 "z",OSTAT	 .MARK ORDER
			if (NORD5STAT =	"05")
				move	"z",OSTAT	.MARK ORDER
			endif
................
		endif
	endif
WRTPRTR
	if (str6 <> OLRN)
		call	OrderBadNINPRINT using str6,OLRN
		goto SEEMOR
	endif
.Cannot	use key	as its'	length is too long
	move	"WRTPRTR-NINPRINT",Location
	pack	KeyLocation,"Key: ",str6
	TRAP	IOMssg Giving Error if IO
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL	
.begin patch 3.79.2   OcompID Ocompid2
	filepi	1;ORDPRINT
	update	ORDPRINT;ORPCODE,OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM,OLON:
		OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE:
		ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED,OMDTEC,OMDTEY,OMDTEM,OMDTED:
		OTOCODE,OSOTCODE,OCCODE,OLRNCO,OODTECOC,OODTECOY,OODTECOM,OODTECOD:
		OQTYCO,OSPI,OBILDRCT,OBRKGUAR,OELCODE,OODNUM,OODES,ONETQTY,OCAMP,OCLRSTAT,OCLRINIT,OBRKRPT,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,ORENT,OHIST,OXPPM,ORTNNUM:
		OTAPERET,OUQTY,OSALES10,OSALES,OCOCODE,OCO2CODE:
		OODTEC,OODTEY,OODTEM,OODTED,OSCODE,OCOMSLCT,OSHP,O1DES,O2DES:
		OREUSE,INITs,OEXQTY,GUARCODE,OBRKNUM,OBRKCNT,OSAMCDE:
		ONETPER,ONETRC,ONETFM,ONETMIN,OFULLFIL:
		OCompID:
		OCompId2:
		OFILLER		
.end patch 3.79.2   OcompID Ocompid2
.		ONETPER,ONETRC,ONETFM,ONETMIN,OFILLER
	TRAPCLR	IO
.End patch 3.78.8 CODE Modification - Addition of OFULLFIL	
	IF (LRINIT = 1)
	move	"NINPRINT - Update, WRTPTR",str45
	call	OrderWriteLRFile using str45
	ENDIF
.  SEE IF MORE INPUT
*******************************************************************************
.
SEEMOR
	move	NO,EXSW
	cmatch	YES,rprtflag
	goto hotord if equal
	return
*******************************************************************************
.HOTORD	- DO REAL TIME PRINT * NOW!
HOTORD
	if (OSTAT = "p"	| OSTAT	= "x")	      .LCR/Pending - skip it
		return
	elseif (OSTAT =	"l" OR OSTAT = "z")
.Force Owner fax copy to the Pit, then return
		call	OrderLCRPrintItRefresh
.Force printout	to go to the pit
		move	C1,PrintFlag
		call	SplOwn
		return
	endif
	clear	TASKNAME
	call	OrderRetrievePrinter
.Get Code
	create	ErrorMssg;EditTextBoxes(1)=120:140:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1
	activate EditTextBoxes(1)
	if (HotFax = C1)
		create	ErrorMssg;StatTextBoxes(1)=120:140:10:240,"		'F' = FULFILLMENT",">Arial(10,BOLD)"
.START PATCH 3.50 REPLACED LOGIC
.		create	ErrorMssg;CheckBoxes(1)=160:180:10:50,"Fax"
.		activate CheckBoxes(1)
		create	ErrorMssg;ComboBoxes(1)=160:180:10:75,"",";P)rint;)Fax;)PDF"
		activate ComboBoxes(1)
.END PATCH 3.50 REPLACED LOGIC
		activate StatTextBoxes(1)
	endif
	create	ErrorMssg;Buttons(1)=140:170:100:150,"O&K",zorder=500,default=1
	activate Buttons(1),HotOrdOK,result
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=1
	setprop	ErrorMssgStat5,visible=1
	getprop	ErrorMssg,title=str55
	setprop	ErrorMssg,title="NIN Select Option"
	setitem	ErrorMssgStat1,0,"Select:  'A' = ALL"
	setitem	ErrorMssgStat2,0,"	       'M' = MAILER"
	setitem	ErrorMssgStat5,0,"	       'L' = LO"
	setitem	ErrorMssgStat3,0,"	       'O' = OFFICE"
	setitem	ErrorMssgStat4,0,"	       'X' = EXIT"
	setitem	EditTextBoxes(1),0,"A"
	setitem	ErrorMssgOK,0,"O&K"
	loop
		clear	str1
		setfocus EditTextBoxes(1)
		setprop	ErrorMssg,visible=1
		if (HotFax <> C1 & str1	= "F")	.Override to prevent it
			move	"*",str1
		elseif (str1 = "")
			move	"X",str1
		endif
		until (str1 = "A" OR str1 = "M"	OR str1	= "L" OR str1 =	"O" OR str1 = "X" OR str1 = "F")
	repeat
	if (HotFax = C1)
.START PATCH 3.50 REPLACED LOGIC
.		getitem	CheckBoxes(1),0,N8
		getitem	ComboBoxes(1),0,N8
.END PATCH 3.50 REPLACED LOGIC
	else
.START PATCH 3.53.1 REPLACED LOGIC
.		move	C0,N8
		move	C1,N8
.END PATCH 3.53.1 REPLACED LOGIC
	endif
	setprop	ErrorMssg,title=str55
	destroy	EditTextBoxes(1)
	destroy	Buttons(1)
	call	SetOrderErrorMssgDefault
	if (HotFax = C1)
.START PATCH 3.50 REPLACED LOGIC
.		destroy	CheckBoxes(1)
		destroy	ComboBoxes(1)
.END PATCH 3.50 REPLACED LOGIC
		destroy	StatTextBoxes(1)
	endif
	if (str1 = "X")
.
.		 getprop OrderInfo,visible=N1
.		 if (N1	= C1)
.			 call	 OrderInfoClose
.		 endif
.		 setitem OrderInfoStatText1,0,""
.		 setitem OrderInfoStatText2,0,""
.		 setitem OrderInfoStatText3,0,"Hot Print Cancelled"
.		 clear	 taskname
.		 append	 "For LR ## ",taskname
.		 append	 NORDFLD,taskname
.		 reset	 taskname
.		 setitem OrderInfoStatText4,0,taskname
.		 setitem OrderInfoStatText5,0,""
.		 setprop OrderInfo,title="Hot Print Information"
.		 setprop OrderInfo,visible=1
.		 pause	 "2"
.		 call	 OrderInfoClose
		pack	str45,"Hot Print Information"
		pack	str55,"Hot Print Cancelled"
		clear	taskname
		append	"For LR	## ",taskname
		append	NORDFLD,taskname
		reset	taskname
		clear	str1
		call	OrderDisplayMessage using Nord0001,str45,str1,str1,str55,taskname,str1,C0,C0,C0,C0
		pause	"2"
		call	OrderInfoClose
.
		move	NO,rprtflag
		return
	endif
.Cannot	use key	as its'	length is too long so move it over
	move	key,str6
	rep	zfill,str6
.	 append	 "c:\progra~1\lanbatch\batch -X	-S#"<ANY>#" -Q\\nts0\c\lanbat~1	\\nts0\d\apps\winbatch\butil job=HOTORD	",TASKNAME
.As per	October	2000 Muffin Meeting - allow dynamic faxing
.START PATCH 3.50 REPLACED LOGIC
.	if (N8 <> 1)
	if (N8 = 1)
.Regular Print Run
.END PATCH 3.50 REPLACED LOGIC
.START PATCH 3.53.1 REMOVED PATCH 3.53 LOGIC
.START PATCH 3.53 REPLACED LOGIC
.		Path	Exist,"c:\windows"
.		if	over
.			append	"!f:\apps\winbatch\butil job=HOTORD ",taskname
.		else
.			append	"!f:\apps\winbatch\butil job=HOTORD ",TASKNAME
.		endif
..		 append	 "\\nts0\d\apps\winbatch\butil job=HOTORD ",TASKNAME
.		append	" infile=",taskname
.		append	str6,taskname
.		append	str1,taskname
.		append	inits,taskname
.		append	" F=default C=1",TASKNAME
.		append	" B=",TASKNAME
.		append	user,TASKNAME
.		append	" prin=",taskname
.		move	CNTPRINT,str2
.		call	Trim using str2
.		append	str2,taskname
.		reset	TASKNAME
.		execute	TASKNAME
..............................
		move	C1,N8
		if (str1 = "A")
			move	C5,N8
			loop
				clear	taskname
				if (N8 = 5)
					move	"M",str1
				elseif (N8 = 4)
					move	"L",str1
				elseif (N8 = 3)
					move	"F",str1
				elseif (N8 = 2)
					move	"O",str1
				endif
				Path	Exist,"c:\windows"
				if	over
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.					append	"!f:\apps\winbatch\butil job=HOTORD ",taskname
					append	"!\\nts0\c\apps\winbatch\butil job=HOTORD ",taskname
.END PATCH 3.71.4 REPLACED LOGIC
.					append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORD ",taskname
.END PATCH 3.6 REPLACED LOGIC
				else
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.					append	"!f:\apps\winbatch\butil job=HOTORD ",TASKNAME
					append	"!\\nts0\c\apps\winbatch\butil job=HOTORD ",TASKNAME
.END PATCH 3.71.4 REPLACED LOGIC
.					append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORD ",taskname
.END PATCH 3.6 REPLACED LOGIC
				endif
.				 append	 "\\nts0\d\apps\winbatch\butil job=HOTORD ",TASKNAME
				append	" infile=",taskname
				append	str6,taskname
				append	str1,taskname
				append	inits,taskname
				append	" F=default C=1",TASKNAME
				append	" B=",TASKNAME
				append	user,TASKNAME
				append	" prin=",taskname
				move	CNTPRINT,str2
				call	Trim using str2
				append	str2,taskname
				reset	TASKNAME
				execute	TASKNAME
				until (N8 <= 2)
				sub	C1,N8
			repeat
		else
.START PATCH 3.68.9 ADDED LOGIC
HotOrdPrintCopy
.END PATCH 3.68.9 ADDED LOGIC
			Path	Exist,"c:\windows"
			if	over
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.				append	"!f:\apps\winbatch\butil job=HOTORD ",taskname
				append	"!\\nts0\c\apps\winbatch\butil job=HOTORD ",taskname
.END PATCH 3.71.4 REPLACED LOGIC
.				append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORD ",taskname
.END PATCH 3.6 REPLACED LOGIC
			else
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.				append	"!f:\apps\winbatch\butil job=HOTORD ",TASKNAME
				append	"!\\nts0\c\apps\winbatch\butil job=HOTORD ",TASKNAME
.END PATCH 3.71.4 REPLACED LOGIC
.				append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORD ",taskname
.END PATCH 3.6 REPLACED LOGIC
			endif
.			 append	 "\\nts0\d\apps\winbatch\butil job=HOTORD ",TASKNAME
			append	" infile=",taskname
			append	str6,taskname
			append	str1,taskname
			append	inits,taskname
			append	" F=default C=1",TASKNAME
			append	" B=",TASKNAME
			append	user,TASKNAME
			append	" prin=",taskname
			move	CNTPRINT,str2
			call	Trim using str2
			append	str2,taskname
			reset	TASKNAME
			execute	TASKNAME
		endif
.END PATCH 3.53 REPLACED LOGIC
.END PATCH 3.53.1 REMOVED PATCH 3.53 LOGIC
.START PATCH 3.50 REPLACED LOGIC
.	else
	elseif (N8 = 2)
.Fax Run
		move	C1,N8
.END PATCH 3.50 REPLACED LOGIC
		if (str1 = "A")
			move	C5,N8
		endif
		loop
			if (N8 = 5)
				move	"M",str1
			elseif (N8 = 4)
				move	"L",str1
			elseif (N8 = 3)
				move	"F",str1
			elseif (N8 = 2)
				move	"O",str1
			endif
			move	C0,N4
			if (str1 = "M")
				if (OSALES10 = "0" AND OSALES =	"6")   .if List	Management then	send to	Broker,	not Mailer
					pack	NBRKFLD,OBRKNUM,OBRKCNT
					move	C1,NBRKPATH
					move	"HotOrd-NBRKKEY",Location
					pack	KeyLocation,"Key: ",NBRKFLD
					call	NBRKKEY
					if over
						move	"**********",MFAX
					else
						move	BRFAX,MFAX
						move	BRCOMP,MCOMP
					endif
				else
					pack	MKEY,OMLRNUM,"000"
					move	C1,NMLRPATH
					move	"HotOrd-NMLRKEY",Location
					pack	KeyLocation,"Key: ",MKEY
					call	NMLRKEY
					if over
						move	"**********",MFAX
					endif
				endif
				if (MFAX <> "**********")
					call	Trim using MFAX
					if (MFAX = "" |	MFAX = "0000000000")
						alert	note,"Invalid Fax Number - Mailer copy will be printed!",result
					else
						move	C1,N4
						count	N2,MFAX
						compare	C10,N2
						if equal
							move	C1,LONGDIST
							unpack	MFAX,str3,str7
							match	"510",str3
							if equal
								move	str7,MFAX
								clear	LONGDIST
							else
								match	B3,str3
								if equal
									move	str7,MFAX
									clear	LONGDIST
								endif
							endif
						endif
.Testing
.						 move	 "4154337796",mFAX
.						 MOVE	 C1,LONGDIST
.CNTNAME/CNTPHONE established via OrderRetrievePrinter
						SPLOPEN	"c:\work\HDRFILE.prn"
						print	"^[D",longdist,MFAX,"^[N",MCOMP:
							"^[S",CNTNAME,B2,CNTPHONE," ^]"
						SPLCLOSE
					endif
				endif
			elseif (str1 = "L" | str1 = "F")
				pack	NOWNFLD,OLON
				move	C1,NOWNPATH
				move	"HotOrd-NOWNKEY",Location
				pack	KeyLocation,"Key: ",NOWNFLD
				call	NOWNKEY
				if not over
					if (str1 = "L")
						call	Trim using OWNFAX
						if (OWNFAX = ""	| OWNFAX = "0000000000")
							alert	note,"Invalid Fax Number - Owner copy will be printed!",result
						else
							move	C1,N4
				       			count	N2,OWNFAX
							compare	C10,N2
							if equal
								move	C1,LONGDIST
								unpack	OWNFAX,str3,str7
								match	"510",str3
								if equal
									move	str7,OWNFAX
									clear	LONGDIST
								else
									match	B3,str3
									if equal
										move	str7,OWNFAX
										clear	LONGDIST
									endif
								endif
							endif
.Testing
.							 move	 "4154337796",OWNFAX
.							 MOVE	 C1,LONGDIST
.CNTNAME/CNTPHONE established via OrderRetrievePrinter
							SPLOPEN	"c:\work\HDRFILE.prn"
							print	"^[D",longdist,OWNFAX,"^[N",OWNOCPY:
								"^[S",CNTNAME,B2,CNTPHONE," ^]"
							SPLCLOSE
						endif
					elseif (str1 = "F")
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.						call	Trim using OWNCTN
						call	Trim using OFULLFIL
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.						 if (OWNCTN <> "" & OWNCTN <> "TDMC" & OWNCTN <> "Tdmc"	& ORTNNUM <> "0001")
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.						if (OWNCTN <> "" & ORTNNUM <> "0001")
						if (OFULLFIL <> "" & ORTNNUM <> "0001")
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
							move	C0,N10
							move	OQTY,N10
							if (N10	<> C0)
								reset	RUNCODES
								scan	OLNUM,RUNCODES
								if not equal
.START PATCH 3.51 REPLACED LOGIC
.									move	C1,N9
.									clear	str25
.									move	OWNCTN,str25
.									rep	LowUp,str25	      .make sure all caps for match
.									loop
.										clear	str15
.										load	str15 FROM N9 OF FUL1,FUL2,FUL3,FUL4,FUL5:
.											FUL6,FUL7,FUL8,FUL9,FUL10,FUL11,ful12,FUL13,ful14,ful15:
.											ful16,ful17,ful18,ful19,ful20,ful21,ful22,ful23,ful24,ful25,ful26
.										match	str15,str25
.										if equal
.											move	C1,N4
.											load	str10 FROM N9 OF FULTEL1,FULTEL2,FULTEL3,FULTEL4,FULTEL5:
.												FULTEL6,FULTEL7,FULTEL8,FULTEL9,FULTEL10,FULTEL11,fultel12:
.												FULTEL13,fultel14,fultel15,fultel16,fultel17,fultel18,fultel19:
.												fultel20,fultel21,fultel22,fultel23,fultel24,fultel25,fultel26
.											move	C1,LONGDIST
.											unpack	str10,str3,str7
.											match	"510",str3
.											if equal
.												move	str7,str10
.												clear	LONGDIST
.											else
.												match	B3,str3
.												if equal
.													move	str7,str10
.													clear	LONGDIST
.												endif
.											endif
..Testing
..											 move	 "4154337796",str10
.											SPLOPEN	"c:\work\HDRFILE.prn"
.											print	"^[D",longdist,str10,"^[N",str15
.											SPLCLOSE
.											break
.										else
.											add	C1,N9
.										endif
.										until (N9 > 26)
.									repeat
.................
.START PATCH 3.78.4 REPLACED LOGIC
.									pack	NFULFLD,OWNCTN
.									rep	zfill,NFULFLD
.									move	C1,NFULPATH
.									move	"HotOrd-NFULKEY",Location
.									pack	KeyLocation,NFULFLD
.									call	NFULKEY
.									if not over
.Start Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.									pack	COMPFLD6,OWNCTN
.									rep	zfill,COMPFLD6
.									move	"HotOrd-COMPKEY6",Location
.									pack	KeyLocation,COMPFLD6
.									call	COMPKEY6
.									if not over
.End Patch 3.78.8 Comment Out Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
									pack	COMPFLD,OFULLFIL
									call	zfillit using COMPFLD
									move	"HotOrd-COMPKEY",Location
									pack	KeyLocation,COMPFLD
									call	COMPKEY
									if not over									
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.78.4 REPLACED LOGIC
.START PATCH 3.68.9 REPLACED LOGIC
.										move	C1,N4
.										move	NFULFAX,str10
.										move	C1,LONGDIST
.										unpack	str10,str3,str7
.										if (str3 = "510")
.											move	str7,str10
.											clear	LONGDIST
.										else
.											match	B3,str3
.											if equal
.												move	str7,str10
.												clear	LONGDIST
.											endif
.										endif
..Testing
..										 move	 "4154337796",str10
..										 MOVE	 C1,LONGDIST
.										SPLOPEN	"c:\work\HDRFILE.prn"
.										print	"^[D",longdist,str10,"^[N",NFULCOMP
.										SPLCLOSE
.............................................................................
.START PATCH 3.7 REPLACED LOGIC
.										if (NFULNUM = "0026")		.Triplex
.											close	TDMCORD
.											open	TDMCORD,"tdmcnotify"
..											filepi	1;TDMCORD
.											read	TDMCORD,str6;;
.											if over
.												call	ExternalXMLCreate using OLRN
.												close	TDMCORD
..												open	TDMCORD,"TDMCORD"
.												open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
..START PATCH 3.69.0 REMOVED LOGIC
..Removing following will force a copy to also be faxed
..												goto HotOrdAfterCopy
..ADDED FOLLOWING
.												move	"F",str1
..Added following to send to Faxing logic
.												goto HOTORDAFTERXML
..END PATCH 3.69.0 REMOVED LOGIC
.											else
..START PATCH 3.69.0 REMOVED LOGIC
..												pack	taskname,"This LR has already been sent to Triplex!",newline,"To send again, hand fax.",newline,"I will print a copy now."
.												pack	taskname,"This LR has already been sent to Triplex!",newline,"I will Hot Fax a copy now."
.												alert	note,taskname,result
.												close	TDMCORD
..												open	TDMCORD,"TDMCORD"
.												open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
.												move	"F",str1
..												clear	taskname
..Removing following will force a copy to also be faxed
..												goto HotOrdPrintCopy
..Added following to send to Faxing logic
.												goto HOTORDAFTERXML
..END PATCH 3.69.0 REMOVED LOGIC
.											endif
....................................
.START PATCH 3.78.4 REPLACED LOGIC
.										if (NFULNUM = "0026")		.Triplex
.Start Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
										move 	OFULLFIL to COMPFLD
										call 	zfillit using COMPFLD
										CALL	COMPKEY	
.										if (NFULNUM = "009406")		.Triplex
										if (OFULLFIL = "009406")		.Triplex
.End Patch 3.78.8 Replace Owner/Fulfillment Association to Fulfillment field on order (List/Fulfillment Association)
.END PATCH 3.78.4 REPLACED LOGIC
											pack	str8,OODTEC,OODTEY,OODTEM,OODTED
											if (str8 > "20030321")
												close	TDMCORD
.Start Patch 3.78.6												
.												open	TDMCORD,"tdmcnotify"
												open	TDMCORD,"NINNOT"
.End Patch 3.78.6
.												filepi	1;TDMCORD
												read	TDMCORD,str6;;
												if over
													call	ExternalXMLCreate using OLRN
													close	TDMCORD
.													open	TDMCORD,"TDMCORD"
													open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
.START PATCH 3.69.0 REMOVED LOGIC
.Removing following will force a copy to also be faxed
.													goto HotOrdAfterCopy
.ADDED FOLLOWING
													move	"F",str1
.Added following to send to Faxing logic
													goto HOTORDAFTERXML
.END PATCH 3.69.0 REMOVED LOGIC
												else
.START PATCH 3.69.0 REMOVED LOGIC
.													pack	taskname,"This LR has already been sent to Triplex!",newline,"To send again, hand fax.",newline,"I will print a copy now."
													pack	taskname,"This LR has already been sent to Triplex!",newline,"I will Hot Fax a copy now."
														alert	note,taskname,result
													close	TDMCORD
.													open	TDMCORD,"TDMCORD"
													open	TDMCORD,"TDMCORD.isi|10.10.30.103:502"
													move	"F",str1
.													clear	taskname
.Removing following will force a copy to also be faxed
.													goto HotOrdPrintCopy
.Added following to send to Faxing logic
													goto HOTORDAFTERXML
.END PATCH 3.69.0 REMOVED LOGIC
												endif
											else
												goto HOTORDAFTERXML
											endif
.END PATCH 3.7 REPLACED LOGIC
										else
.START PATCH 3.69.0 REMOVED LOGIC
HOTORDAFTERXML
.END PATCH 3.69.0 REMOVED LOGIC
											move	C1,N4
.START PATCH 3.78.4 REPLACED LOGIC
.											move	NFULFAX,str10
											move	COMPFAX,str10
.END PATCH 3.78.4 REPLACED LOGIC
											move	C1,LONGDIST
											unpack	str10,str3,str7
											if (str3 = "510")
												move	str7,str10
												clear	LONGDIST
											else
												match	B3,str3
												if equal
													move	str7,str10
													clear	LONGDIST
												endif
											endif
											SPLOPEN	"c:\work\HDRFILE.prn"
.Start patch 3.78.8 Replace Code Replacing this read using ownctn with order read ofufill											
.											print	"^[D",longdist,str10,"^[N",NFULCOMP
											print	"^[D",longdist,str10,"^[N",COMPCOMP
.End patch 3.78.8 Replace Code Replacing this read using ownctn with order read ofufill											
											SPLCLOSE
										endif
.END PATCH 3.68.9 REPLACED LOGIC
									endif
.END PATCH 3.51 REPLACED LOGIC
								endif
							endif
						endif
					endif
				endif
			endif
			clear	taskname
			Path	Exist,"c:\windows"
			if	over
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.				append	"!f:\apps\winbatch\butil job=HOTORDFAX ",taskname
				append	"!\\nts0\c\apps\winbatch\butil job=HOTORDFAX ",taskname
.END PATCH 3.71.4 REPLACED LOGIC
.				append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORDFAX ",taskname
.END PATCH 3.6 REPLACED LOGIC
			else
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.				append	"!f:\apps\winbatch\butil job=HOTORDFAX ",TASKNAME
				append	"!\\nts0\c\apps\winbatch\butil job=HOTORDFAX ",TASKNAME
.END PATCH 3.71.4 REPLACED LOGIC
.				append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORDFAX ",taskname
.END PATCH 3.6 REPLACED LOGIC
			endif
			append	" infile=",taskname
.			 append	 " infile=c:\work\",taskname
			call	Trim using inits
			pack	str10,str6,str1,inits
			append	str10,taskname
			append	" F=default C=1",TASKNAME
			append	" B=",TASKNAME
			append	user,TASKNAME
			append	" prin=",taskname
			if (N4 = 1)
				move	"A",str2
			else
				move	CNTPRINT,str2
			endif
			call	Trim using str2
			append	str2,taskname
			reset	TASKNAME
			execute	TASKNAME
			clear	taskname
			if (N4 = 1)
				pack	APIFileName,str55,str55,str55,str55
................drew
.				pack	APIFileName,"\\nins1\e\data\",str10,".fax",HexZero
				pack	 APIFileName,"c:\work\",str10,".fax",HexZero
................drew
				clock	timestamp,timestamp1
				move	timestamp1,time1
				call	OrderSetMouseBusy
				loop
.START PATCH 3.6 ADDED LOGIC
					pack	 APIFileName,"c:\work\",str10,".fax",HexZero
.END PATCH 3.6 ADDED LOGIC
					call	FindFirstFile
					if (APIResult =	0 | APIResult =	hexeight)
.If .FAX file does not exist then do not attempt to copy
						clock	timestamp,timestamp2
						move	timestamp2,time2
						sub	time1,time2,time3
						if (time3 > 3000) ..30 Seconds Maximum
							if (str1 = "M")
								append	"Mailer",taskname
							elseif (str1 = "L")
								append	"List Owner",taskname
							elseif	(str1 =	"F")
								append	"Fulfillment",taskname
							endif
							append	" Copy Not Faxed!  Try again.",taskname
							reset	taskname
							alert	caution,taskname,result
							clear	taskname
							goto HotOrdAfterCopy
						endif
					endif
					until	(apiresult <> 0	& APIRESULT <> Hexeight)
			       	repeat
.START PATCH 3.61 REPLACED LOGIC
.				Path	Exist,"c:\windows"
.				if	over
.					append	"c:\winnt\system32\cmd.exe",taskname
.				else
.					append	"!c:\command.com",taskname
.				endif
				Path	Exist,"c:\windows"
				if over			.nt/2000
					append	"!c:\winnt\system32\cmd.exe",taskname
				elseif (osflag = c6)	.XP
					append	"!c:\windows\system32\cmd.exe",taskname
				else			.95/98
					append	"!c:\command.com",taskname
				endif
.END PATCH 3.61 REPLACED LOGIC

.				append	 " /c copy c:\work\hdrfile.prn /b + \\nts0\d\data\hotord.lst /b	c:\work\",taskname
................drew
.				append	" /c copy c:\work\hdrfile.prn /b + \\nins1\e\data\",taskname
				append	 " /c copy c:\work\hdrfile.prn /b + c:\work\",taskname
................drew
				append	str10,taskname
				append	".fax /b c:\work\",taskname
				append	str10,taskname
				append	".prn /b",taskname
				reset	taskname
				execute	taskname
				clear	taskname
.START PATCH 3.61 REPLACED LOGIC
.				Path	Exist,"c:\windows"
.				if	over
.					append	"c:\winnt\system32\cmd.exe",taskname
.				else
.					append	"!c:\command.com",taskname
.				endif
				Path	Exist,"c:\windows"
				if over			.nt/2000
					append	"!c:\winnt\system32\cmd.exe",taskname
				elseif (osflag = c6)	.XP
					append	"!c:\windows\system32\cmd.exe",taskname
				else			.95/98
					append	"!c:\command.com",taskname
				endif
.END PATCH 3.61 REPLACED LOGIC
				append	" /c copy c:\work\",taskname
				append	str10,taskname
				append	".prn \\nts2\fax",taskname
				reset	taskname
				execute	taskname
HotOrdAfterCopy
			endif
			call	OrderSetMouseFree
			pack	APIFileName,str55,str55,str55,str55
................drew
.			pack	APIFileName,"\\nins1\e\data\",str10,".fax",HexZero
			pack	 APIFileName,"c:\work\",str10,".fax",HexZero
................drew
			call	DeleteFile
			pack	APIFileName,str55,str55,str55,str55
			pack	APIFileName,"c:\work\",str10,".prn",HexZero
			call	DeleteFile
			until (N8 <= 2)
			sub	C1,N8
		repeat
.START PATCH 3.50 ADDED LOGIC
	else
.PDF Run
		move	C1,N8
		if (str1 = "A")
			move	C5,N8
		endif
.START PATCH 3.75.5 ADDED LOGIC
.Present message excusing time to run
		destroy	MDateWindow
		destroy StatTextBoxes(1)
		destroy StatTextBoxes(2)
		destroy StatTextBoxes(3)
		create	MDateWindow=380:480:70:320,title="NIN Order PDF Print",wintype=2,winpos=3,sysmenu=0,style=4
		create	MDateWindow;StatTextBoxes(1)=10:30:1:250,"PDF Files may take some time to Create.","",Center=1
		create	MDateWindow;StatTextBoxes(2)=40:60:1:250,"Please be Patient.","",Center=1
		create	MDateWindow;StatTextBoxes(3)=70:90:1:250,"","",Center=1
		setprop	StatTextBoxes(1),Font="'>MS Sans Serif'(8)"
		setprop	StatTextBoxes(2),Font="'>MS Sans Serif'(8)"
		setprop	StatTextBoxes(3),Font="'>MS Sans Serif'(8)"
		activate MDateWindow
		activate StatTextBoxes(1)
		activate StatTextBoxes(2)
		activate StatTextBoxes(3)
.END PATCH 3.75.5 ADDED LOGIC
		loop
			if (N8 = 5)
				move	"M",str1
.START PATCH 3.75.5 ADDED LOGIC
				setitem	StatTextBoxes(3),0,"Creating Mailer Copy"
.END PATCH 3.75.5 ADDED LOGIC
			elseif (N8 = 4)
				move	"L",str1
.START PATCH 3.75.5 ADDED LOGIC
				setitem	StatTextBoxes(3),0,"Creating List Owner Copy"
.END PATCH 3.75.5 ADDED LOGIC
			elseif (N8 = 3)
				move	"F",str1
.START PATCH 3.75.5 ADDED LOGIC
				setitem	StatTextBoxes(3),0,"Creating Fulfillment Copy"
.END PATCH 3.75.5 ADDED LOGIC
			elseif (N8 = 2)
				move	"O",str1
.START PATCH 3.75.5 ADDED LOGIC
				setitem	StatTextBoxes(3),0,"Creating Office Copy"
.END PATCH 3.75.5 ADDED LOGIC
			endif
			move	C0,N4
			clear	taskname
			Path	Exist,"c:\windows"
			if	over
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.				append	"!f:\apps\winbatch\butil job=HOTORDFAX ",taskname
				append	"!\\nts0\c\apps\winbatch\butil job=HOTORDFAX ",taskname
.END PATCH 3.71.4 REPLACED LOGIC
.				append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORDFAX ",taskname
.END PATCH 3.6 REPLACED LOGIC
			else
.START PATCH 3.6 REPLACED LOGIC
.START PATCH 3.71.4 REPLACED LOGIC
.				append	"!f:\apps\winbatch\butil job=HOTORDFAX ",TASKNAME
				append	"!\\nts0\c\apps\winbatch\butil job=HOTORDFAX ",TASKNAME
.END PATCH 3.71.4 REPLACED LOGIC
.				append	"!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=HOTORDFAX ",taskname
.END PATCH 3.6 REPLACED LOGIC
			endif
			append	" infile=",taskname
			call	Trim using inits
			pack	str10,str6,str1,inits
			append	str10,taskname
			append	" F=default C=1",TASKNAME
			append	" B=",TASKNAME
			append	userlogn,TASKNAME
			append	" prin=P",taskname
			reset	TASKNAME
			execute	TASKNAME
.START PATCH 3.75.5 ADDED LOGIC
			pack	APIFileName,B55,B55,B55,B55,B55,B55
			pack	APIFileName,"\\nins1\e\data\",str10,".pdf",hexzero
			call	FindFirstFile
			if (APIResult <> 0 & APIResult <> hexeight)
.Make sure PDF file was created!!!  Fulfillment copy will NOT print if no associated Fulfillment house!!
.END PATCH 3.75.5 ADDED LOGIC
.Email file
				clear	taskname
.START PATCH 3.71.4 REPLACED LOGIC
.				append  "!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 f:\apps\winbatch\butil job=killfile ",TASKNAME
				append  "!c:\progra~1\lanbatch\batch -X -SC -Q\\nts0\c\lanbat~1 \\nts0\c\apps\winbatch\butil job=killfile ",TASKNAME
.END PATCH 3.71.4 REPLACED LOGIC
				append  " infile=",taskname
				append	str10,taskname
				append  " B=",taskname
				append	userlogn,taskname
				append	" pdf=y",taskname
				reset   TASKNAME
				execute TASKNAME
.START PATCH 3.75.5 ADDED LOGIC
			endif
.END PATCH 3.75.5 ADDED LOGIC
.
			clear	taskname
			pack	APIFileName,str55,str55,str55,str55
.			pack	APIFileName,"\\nins1\e\data\",str10,".fax",HexZero
			pack	APIFileName,"c:\work\",str10,".fax",HexZero
			call	DeleteFile
			until (N8 <= 2)
			sub	C1,N8
		repeat
.START PATCH 3.75.5 ADDED LOGIC
		destroy	MDateWindow
		destroy StatTextBoxes(1)
		destroy StatTextBoxes(2)
		destroy StatTextBoxes(3)
.END PATCH 3.75.5 ADDED LOGIC
.END PATCH 3.50 ADDED LOGIC
	endif
.	 cmatch	 yes,rprtflag
.	 if equal
.		 move	 NO,rprtflag
.		 goto seemor
.	 endif
.Cannot	use key	as its'	length is too long so move it over
	move	"H.O.A.COPY-NINPRINT",Location
	pack	KeyLocation,"Key: ",key
	TRAP	IOMssg Giving Error if IO
	move	key,str6
	rep	zfill,str6
	filepi	1;ORDPRINT			 1 to 5	dlh 08jul96
	read	ORDPRINT,str6;orpcode,OSTAT;		      .	28jun94
	if not over						.DLH
		filepi	1;ordprint		  . reprint so will not	print later.
		updatab	Ordprint;*1,"SR"
		flush	ordprint
	endif
	TRAPCLR	IO
	move	NO,rprtflag
.
.START PATCH 3.68.2 REPLACED LOGIC
.	filepi	1;TDMCORD
.	read	TDMCORD,str6;;
.	if not over
.		filepi	1;TDMCORD
.		update	TDMCORD;str6,"D"
.	endif
	filepi	1;TDMCORD
	read	TDMCORD,str6;str6,str1
	if not over
		if (str1 = "0")
			filepi	1;TDMCORD
			update	TDMCORD;str6,"D"
		endif
	endif
.END PATCH 3.68.2 REPLACED LOGIC
	return

HotOrdOK
	setprop	ErrorMssg,visible=0
	getitem	EditTextBoxes(1),0,str1
	return
OrderRetrievePrinter
.Get default printer
	move	PORTN,NCNTFLD1
	rep	zfill,NCNTFLD1
	move	C3,NCNTPATH
	move	"HotOrd-NCNTKEY",Location
	pack	KeyLocation,"Key: ",NCNTFLD1
	call	NCNTKEY
	if over
		move	C2,CNTPRINT    .Laser 3
	endif
	return

OrderApprovePrinting
	if (OSTAT = "p"	OR OSTAT = "x" OR OSTAT	= "l" OR OSTAT = "z" OR	OSTAT =	"X" OR OSTAT = "Q")
		return
	elseif (ORPCODE	= "F")
		alert	plain,"Do you Approve this Order for printing?",result
		if (result = 1)
.If in Modify mode or New mode,	this is	a double check of Sample.
.If in Cancel mode then	this is	the only check for Sample.
.Return	flag is	then set so that you may change
			move	OSCODE,N2
			if (N2 = 1)
				if (OSAMCDE <> Z3 & OSAMCDE <> B3 & OSAMCDE <> "")
					clear	NSMPFLD
.START PATCH 3.75.5 ADDED LOGIC
.					pack	NSMPFLD,OMLRNUM,OSAMCDE
					pack	COMPFLD3,OMLRNUM
					move	"COMPKEY3-READ",Location
					pack	KeyLocation,"Key: ",COMPFLD3
					call	COMPKEY3
					pack	NSMPFLD,COMPNUM,OSAMCDE
.END PATCH 3.75.5 ADDED LOGIC
					rep	zfill,NSMPFLD
					move	"O.Approve-NSMPKEY",Location
					pack	KeyLocation,"Key: ",NSMPFLD
					call	NSMPKEY
					if over
						alert	caution,"Sample	Not Found, you cannot approve!!",result
						return
					endif
				endif
			endif
			if (holdkey <> "")
				move	"O.A.Printing-NINPRINT",Location
				pack	KeyLocation,"Key: ",holdkey
				TRAP	IOMssg Giving Error if IO
				filepi	1;ORDPRINT
				read	ORDPRINT,holdkey;;
				if over
					TRAPCLR	IO
					clear	taskname
					append	"You cannot approve, NINPRINT copy not found.",taskname
					append	carr,taskname
					append	"Please	inform I.S.",taskname
					reset	taskname
					alert	caution,taskname,result
					return
				else
					move	"S",ORPCODE
					filepi	1;ORDPRINT
					updatab	ORDPRINT;*1,ORPCODE
.START PATCH 3.53 ADDED LOGIC
.START PATCH 3.78.4 REPLACED LOGIC
.					if (NFULFLD = "0026")			*TRIPLEX CCTO?
.End Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order				
.					if (NFULNUM = "009406")			*TRIPLEX CCTO?
					move 	OFULLFIL to COMPFLD
					call 	zfillit using COMPFLD
					CALL	COMPKEY			
					reset	COMPCOMP
					if (OFULLFIL = "009406")			*TRIPLEX CCTO?
.End Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order									
.END PATCH 3.78.4 REPLACED LOGIC
.Start Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order				
						move	C0,loltype
						goto WRITDMCP3
					elseif (COMPCOMP <> "")
						reset	COMPCOMP
						scan	TDMC0,COMPCOMP		*TRIPLEX CCTO?
						if equal
							move	C0,loltype
							goto WRITDMCP3
						endif
						reset	COMPCOMP
						scan	TDMC1,COMPCOMP		*TRIPLEX CCTO?
						if equal
							move	C0,loltype
							goto WRITDMCP3
						endif
.End Patch 3.78.8 Replace Owner Association of Fulfillment Company to Fulfillment field on Order										
.Start Patch 3.78.8 Comment Out Replace Owner Association of Fulfillment Company to Fulfillment field on Order				
.					elseif (NFULCOMP <> "")
.						reset	NFULCOMP
.						scan	TDMC0,NFULCOMP		*TRIPLEX CCTO?
.						if equal
.							move	C0,loltype
.							goto WRITDMCP3
.						endif
.						reset	NFULCOMP
.						scan	TDMC1,NFULCOMP		*TRIPLEX CCTO?
.						if equal
.							move	C0,loltype
.							goto WRITDMCP3
.						endif
.End Patch 3.78.8 Comment Out Replace Owner Association of Fulfillment Company to Fulfillment field on Order				
					endif
.START PATCH 3.77.3 REPLACED LOGIC
.					if (ORTNNUM = "0040" | ORTNNUM = "5224")
.					if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318")
.             	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316")
.END PATCH 3.77.3 REPLACED LOGIC
.START PATCH 3.77.5 REPLACED LOGIC
            	if (ORTNNUM = "0040" | ORTNNUM = "5224" | ORTNNUM = "5318" | ORTNNUM = "5316" | ORTNNUM = "5319")
.START PATCH 3.77.5 REPLACED LOGIC
						reset	RUNCODES
						scan	OLNUM,RUNCODES
						if not equal
							move	"D",LOLTYPE
							goto WRITDMCP3
						endif
					endif
					goto skipWRITDMCP3
WRITDMCP3
					read	TDMCORD,holdkey;;
					if over
						filepi	1;TDMCORD
						write	TDMCORD,holdkey;holdkey,loltype
					endif
skipWRITDMCP3
.END PATCH 3.53 ADDED LOGIC
				endif
				TRAPCLR	IO
			else
				clear	taskname
				append	"You cannot approve, NULL key value!!",taskname
				append	carr,taskname
				append	"Please	inform I.S.",taskname
				reset	taskname
				alert	caution,taskname,result
				return
			endif
		endif
	endif
	return
.OrderApproveCheck
..Will hide both NORDMSK1ButtonApprove
.	 setprop NORDMSK1ButtonApprove,visible=0
..	  setprop Nord001AButtonPending,visible=0
.	 getprop Nord001ACheckMode,enabled=result
..Must be in Inquiry mode AND Order cannot be:	Pending/LCR/Cancelled/CancelledBilled AND cannot be in Search mode!
.	 if (result = 0	AND OSTAT <> "p" AND OSTAT <> "x" AND NewFlag <> "S" AND OSTAT <> "l" AND OSTAT	<> "z" AND OSTAT <> "X"	AND OSTAT <> "Q")
.		 if (ORPCODE = "F")		 .Order	must be	in NINPRINT and	not approved!
.			 setitem nordmsk1CheckApprove,0,0
.			 getitem nord0001TabControlTop,0,howmany
..Only load if on first	three screens
.			 if (howmany <=	3)
.				 setprop NORDMSK1ButtonApprove,visible=1
.			 endif
.		 else
.			 setitem nordmsk1CheckApprove,0,1
.			 setprop NORDMSK1ButtonApprove,visible=0
.		 endif
.	 else
.		 setitem nordmsk1CheckApprove,0,0
.		 setprop NORDMSK1ButtonApprove,visible=0
.	 endif
.	 return
OrderApproveCheck
	setprop	NORDMSK1ButtonApprove,height=0
	getprop	Nord001ACheckMode,enabled=result
.Must be in Inquiry mode AND Order cannot be:  Pending/LCR/Cancelled/CancelledBilled AND cannot	be in Search mode!
	if (result = 0 AND OSTAT <> "p"	AND OSTAT <> "x" AND NewFlag <>	"S" AND	OSTAT <> "l" AND OSTAT <> "z" AND OSTAT	<> "X" AND OSTAT <> "Q")
		if (ORPCODE = "F")		.Order must be in NINPRINT and not approved!
			setitem	NORDMSK1CheckApprove,0,0
			setprop	NORDMSK1ButtonApprove,height=20
		else
			setitem	NORDMSK1CheckApprove,0,1
			setprop	NORDMSK1ButtonApprove,height=0
		endif
	else
		setitem	NORDMSK1CheckApprove,0,0
		setprop	NORDMSK1ButtonApprove,height=0
	endif
	return

OrderMaskVariables
	pack	MaskOMLRNUM,OMLRNUM
	pack	MaskOBRKNUM,OBRKNUM
	pack	MaskOBRKCNT,OBRKCNT
	pack	MaskOMLRPON,OMLRPON
	pack	MaskOMLRKY,OMLRKY
	pack	MaskOFOCODE,OFOCODE
	pack	MaskORTNDTEC,ORTNDTEC
	pack	MaskORTNDTEY,ORTNDTEY
	pack	MaskORTNDTEM,ORTNDTEM
	pack	MaskORTNDTED,ORTNDTED
	pack	MaskOMDTEC,OMDTEC
	pack	MaskOMDTEY,OMDTEY
	pack	MaskOMDTEM,OMDTEM
	pack	MaskOMDTED,OMDTED
	pack	MaskOODNUM,OODNUM
	pack	MaskORTNNUM,ORTNNUM
	pack	MaskOSHP,OSHP
	pack	MaskDESC002,DESC002
	pack	MaskOCOCODE,OCOCODE
.START PATCH 3.72.3 ADDED LOGIC
	pack	MaskOLNUM,OLNUM
.END PATCH 3.72.3 ADDED LOGIC
	return

.START PATCH 3.72.3 ADDED LOGIC
OrderReverseSearch
	getitem	Nord001AEditList,0,NXRFFLD
	call	Trim using NXRFFLD
	if (NXRFFLD = "")
		move	MaskOLNUM,NXRFFLD
	endif
.
	getitem	Nord001AEditMlr,0,str4
	call	Trim using str4
	if (str4 = "")
		setitem	Nord001AEditMlr,0,MaskOMLRNUM
	endif
.
	setitem	Nord001AEditMlrContact,0,"000"
	move	"16",N4
	move	C0,MouseForm
	getprop Nord001AStatListName,top=T1,left=L1
	call	OrderInfoClose
	call	OrderDisplayMailer using Nord0001,Nord001AEditMlr,Nord001AEditMlrContact,N4,MouseForm,T1,L1
	call	OrderDisplaySearchTitle
	setitem	Nord001AEditMlrContact,0,""
.Setting of List Number is dependent upon a DoubleClick Event defined below
	move	C1,NXRFPATH
	clear	NXRFFLD2
	rep	zfill,NXRFFLD
	call	NXRFKEY
	if not over
.START PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
.		setitem	Nord001AEditMlr,0,NXRFMLR
		pack	COMPFLD,NXRFMLR
		move	"O.Rev.Search-COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		setitem	Nord001AEditMlr,0,COMPOLDMLR
.END PATCH 3.76 REPLACED LOGIC - TEMPORARY LOGIC
	else
		setitem	Nord001AEditMlr,0,""
	endif
	call	LostFocus_Nord001AEditMlr
	return

OrderLoadListNumber Routine DimPtr
	setitem	Nord001AEditList,0,DimPtr
	return
.END PATCH 3.72.3 ADDED LOGIC

.START PATCH 3.44 ADDED LOGIC
StatsLoadLRValue
	getprop	Nord001AEditMlr,enabled=N10
	if (N10 = 0)
		call	Trim using STATLR
		if (STATLR <> "")
			setitem	NORDMSK1EditSearchKey,0,STATLR
			call	OrderSwitchTab using C1
			goto Reload
		endif
	endif
	return

StatsLoadLOLValue
	getprop	Nord01ECEditList,enabled=N10
	if (N10 = 0)
		call	Trim using STATLR
		if (STATLR <> "")
			pack	NLOLFLD,STATLR
			move	C1,NLOLPATH
			move	"LoadLOL-NLOLKEY",Location
			pack	KeyLocation,"Key: ",NLOLFLD
			call	NLOLKEY
			if not over
				pack	str12,NLOLCNum,NLOLList
				setitem	NORDMSK3EditSearchKey,0,str12
				call	OrderSwitchTab using C8
				goto Reload3A
			endif
		endif
	endif
	return
.END PATCH 3.44 ADDED LOGIC

.OrderTestXSTAT
..Called by:  OrderLoadLOLScreen,OrderLoadLOLScreen2,Nord001AStatExchangeMssg
..OLNUM	& OMLRNUM need to be established beforehand
..returns taskname with	appropriate XSTAT/NULL
.	 clear	 taskname
.	 move	 C0,EFLAG
.	 move	 OLNUM,NXRFFLD
.	 clear	 NXRFMLR
.	 move	 C1,NXRFPATH
.	 move	 "O.TestXStat-NXRFKEY",Location
.	 pack	 KeyLocation,"Key: ",NXRFFLD
.	 call	 NXRFKEY
.	 if not	over
.		 clear	 ACKEY
.		 clear	 NXNGFLD1
.		 clear	 NXNGFLD2
.		 pack	 ACKEY,OMLRNUM,NXRFMLR
.		 pack	 NXNGFLD1,AKey1A,OMLRNUM
.		 pack	 NXNGFLD2,AKey2A,NXRFMLR
.		 rep	 ZFILL,ACKEY
.		 move	 "O.TestXStat-NXNGAIM",Location
.		 pack	 KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
.		 call	 NXNGAIM
.		 if over
..Check	to see if Mlr/List Mlr have been reversed
.			 clear	 ACKEY
.			 clear	 NXNGFLD2
.			 clear	 NXNGFLD1
.			 pack	 ACKEY,NXRFMLR,OMLRNUM
.			 pack	 NXNGFLD2,AKey2A,OMLRNUM
.			 pack	 NXNGFLD1,AKey1A,NXRFMLR
.			 rep	 ZFILL,ACKEY
.			 move	 "O.TestXStat-NXNGAIM",Location
.			 pack	 KeyLocation,"Key: ",NXNGFLD1,COMMA,NXNGFLD2
.			 call	 NXNGAIM
.			 return	if over
.			 match	 ACKEY,ACCKEY
.			 return	if not equal
.			 move	 C2,EFLAG
.		 else
.			 match	 ACKEY,ACCKEY
.			 return	if not equal
.			 move	 C1,EFLAG
.		 endif
..
.		 move	 C1,NXCHPATH
.		 pack	 NXCHFLD1,ACKEY,ENTRY
.		 rep	 ZFILL,NXCHFLD1
.		 move	 "ExAcLoop-NXCHKEY",Location
.		 pack	 KeyLocation,"Key: ",NXCHFLD1
.		 call	 NXCHKEY
.		 if over
.			 move	 C0,EFLAG
.			 return
.		 endif
.	 endif
.	 if (EFLAG <> "0")
.		 compare USAGE1,USAGE2
.		 if less
.			 sub	 USAGE2,USAGE1
.			 if (EFLAG = "1")
.				 call	 Trim using MCOMP
.				 append	 MCOMP,taskname
.				 append	 " owes	",taskname
.				 move	 USAGE1,str10
.				 call	 FormatNumeric using str10,str13
.				 append	 str13,taskname
.				 append	 " names.",taskname
.			 else
.				 call	 Trim using OLSTNAME
.				 append	 OLSTNAME,taskname
.				 append	 " owes	",taskname
.				 move	 USAGE1,str10
.				 call	 FormatNumeric using str10,str13
.				 append	 str13,taskname
.				 append	 " names.",taskname
.			 endif
.		 elseif	not equal	.Greater
.			 sub	 USAGE1,USAGE2
.			 if (EFLAG = "1")
.				 call	 Trim using OLSTNAME
.				 append	 OLSTNAME,taskname
.				 append	 " owes	",taskname
.				 move	 USAGE2,str10
.				 call	 FormatNumeric using str10,str13
.				 append	 str13,taskname
.				 append	 " names.",taskname
.			 else
.				 call	 Trim using MCOMP
.				 append	 MCOMP,taskname
.				 append	 " owes	",taskname
.				 move	 USAGE2,str10
.				 call	 FormatNumeric using str10,str13
.				 append	 str13,taskname
.				 append	 " names.",taskname
.			 endif
.		 else
.			 append	 "Exchange Status is Even",taskname
.		 endif
.		 reset	 taskname
.	 endif
.	 return

.OrderCalculateLastNet Routine DimPtr,DimPtr1,DimPtr2
.	 move	 C0,N8
.	 move	 C0,N9
.	 pack	 NORDFLD1,AKey1,DimPtr
.	 pack	 NORDFLD2,"02X",DimPtr1
.	 clear	 NORDFLD3
.	 clear	 NORDFLD4
.	 move	 "CalcNet-NORDAIM",Location
.	 pack	 KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
.	 call	 NORDAIM
.	 if over
.		 move	 "0",DimPtr2
.	 else
.		 loop
.			 pack	 newdate,OODTEC,OODTEY,OODTEM,OODTED
.			 move	 newdate,N9
.			 if (N9	> N8)
.				 move	 newdate,N8
.				 move	 OLRN,NMRGFLD
.				 move	 "CalcNet-NMRGKEY",Location
.				 pack	 KeyLocation,"Key: ",NMRGFLD
.				 call	 NMRGKEY
.				 if not	over
.					 compare C0,NMRGNET
.					 if equal
.						 move	 C0,DimPtr2
.					 else
.						 move	 C0,CALCPER
.						 move	 NMRGNET,CALCPER
.						 divide	 NMRGIQTY,CALCPER
.						 mult	 "100",CALCPER
.						 move	 C0,N32
.						 add	 CALCPER,N32
.						 move	 N32,DimPtr2
.					 endif
.				 endif
.			 endif
.			 move	 "CalcNet-NORDKG",Location
.			 pack	 KeyLocation,"Key: ",NORDFLD1,COMMA,NORDFLD2
.			 call	 NORDKG
.			 until over
.		 repeat
.	 endif
.	 return
OrderGetNetValues Routine DimPtr,DimPtr1
	call	OrderSetGetNetScreen
	setitem	EditTextBoxes(1),0,DimPtr
	setitem	EditTextBoxes(2),0,DimPtr1
	setprop	Report2,visible=1
	return

Order5TestBusy LRoutine	DimPtr
.str1 is used as NORDTST uses this variable to determine if Busy Byte is active
.Following Note	is obsolete as this routine was	pulled from NORD01D.PLF	in order
.to use	it in different	forms.
..NOTE:	 #result CANNOT	be used	in this	routine	as its'	value needs to remain integral in calling routine!!!
	move	C1,NORDPATH
	move	DimPtr,NORDFLD
	move	"O.5TestBusy-NORDTST",Location
	pack	KeyLocation,"Key: ",NORDFLD
	call	NORDTST
	if over
		clear	taskname
		append	NORDFLD,taskname
		append	" is not in Master Order File!!!",taskname
		reset	taskname
		alert	caution,taskname,N6
		move	STAR,str1
	elseif (str1 = STAR)
		clear	taskname
		append	NORDFLD,taskname
		append	" is currently in use.	Try again later!",taskname
		reset	taskname
		alert	note,taskname,N6
	endif
	return

OrderMoveCampaign LRoutine DimPtr1,DimPtr2,DimPtr3,FrmPtr
.DimPtr1 = current Campaign value
.DimPtr2 = new Campaign value
.DimPtr3 = current LR value
.FrmPtr  = Which routine is calling this sub-routine.
.This routine is called	by 3 different sub-routines:
.	OrderVerifyData, which does not	need to	test if	busy and does not need to update OCAMP
.	OrderCopy
.	Nord01EBButtonMove, which should produce error if record not found via NORDFLDC
	move	DimPtr1,NORDFLDC
	move	DimPtr2,OCAMP
	move	DimPtr3,NORDFLD
	call	Trim using NORDFLDC
	call	Trim using OCAMP
	call	Trim using NORDFLD
	if (NORDFLDC = "")	.Force an Over condition
.This string MUST match	string below, otherwise	you get	an ugly	infinite loop
		pack	NORDFLDC,"(OVER)"
	endif
	move	C3,NORDLOCK
	move	C4,NORDPATH
	move	"O.M.Cam.-NORDTST(4)",Location
	pack	KeyLocation,"Key: ",NORDFLDC
	call	NORDTST
	loop
		if over
.Don't call Order5TestBusy if called from VerifyData - I know the record will be busy!
			if (FrmPtr = C1)
				call	Order5TestBusy using NORDFLD
			elseif (FrmPtr = C2)	.Major fucking problem - record	should be here
				clear	taskname
				append	NORDFLDC,taskname
				append	B1,taskname
				append	NORDFLDC,taskname
				append	" Record not Found in NORDFLE4-NORDTST/NORDKS!!",taskname
				reset	taskname
				alert	caution,taskname,N10
				move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
				move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
				move	taskname,SmtpTextMessage(2)   Array <Text message >
				move	"Subroutine OrderMoveCampaign",SmtpTextMessage(3)   Array <Text	message	>
				move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
				call	errmesg
				goto FileGo3
.				 stop
			else
				move	PERIOD,str1
			endif
			if (str1 <> STAR)
				move	C1,NORDPATH
				move	"O.M.Cam.-NORDTST",Location
				pack	KeyLocation,"Key: ",NORDFLD
				call	NORDTST		.Safety	measure
				if not over
					if (OCAMP <> "")
						call	OrderMoveCampaignWriteIt using FrmPtr
.START PATCH 3.75 ADDED LOGIC
						call	IntegralMoveCampaign using DimPtr3,DimPtr2,C1
					else
						call	IntegralDeleteDetail using DimPtr3,C1
.END PATCH 3.75 ADDED LOGIC
					endif
				endif
				break
.This string MUST match	string above, otherwise	you get	an ugly	infinite loop
			elseif (NORDFLDC = "(OVER)")  .Record is Busy AND null Campaign	Number
				break
			endif
		endif
		until (NORDFLD = str6)
		move	"OrderMoveCampaign-NORDKS",Location
		move	C5,NORDPATH
		call	NORDKS
	repeat
	if (NORDFLD = str6)
.READKS	will not produce Over when the Secondary/AAM Key value changes,	it will	simply
.continue reading until	EOF.  So make sure that	the matched record actually contains the
.same Secondary/AAM Key	value.
		if (NORDFLAG <>	C1)
			move	C1,NORDPATH
			call	NORDOPEN
		endif
		readtab	NORDFILE,NORDFLD;*156,str6
		if (str6 = NORDFLDC)
.Don't call Order5TestBusy if called from VerifyData - I know the record will be busy!
			if (FrmPtr = C1)
				call	Order5TestBusy using DimPtr3
			else
				move	PERIOD,str1
			endif
			if (str1 <> STAR)
.Delete	Key for	old Campaign
				trap	IOMssg if IO
				move	"O.M.Cam.-DeleteK,NORDFLE4",Location
				pack	KeyLocation,"Key: ",NORDFLDC
				filepi	1;NORDFLE4
				deletedk NORDFLE4,NORDFLDC
.Establish new value to	update - Campaign
				if (OCAMP <> "")
					call	OrderMoveCampaignWriteIt using FrmPtr
.START PATCH 3.75 ADDED LOGIC
					call	IntegralMoveCampaign using DimPtr3,DimPtr2,C1
				else
					call	IntegralDeleteDetail using DimPtr3,C1
.END PATCH 3.75 ADDED LOGIC
				endif
				trapclr	IO
			endif
		endif
	endif
	return

OrderMoveCampaignWriteIt LRoutine FrmPtr
	move	"O.M.Cam.-NORDWRT4",Location
	pack	KeyLocation,"Key: ",OCAMP
	call	NORDWRT4
.Update	actual record
	if (FrmPtr <> C0)
.NORDFLD established in	calling	sun-routine
		move	C1,NORDPATH
		move	"O.M.C.-NORDTST",Location
		pack	KeyLocation,"Key: ",NORDFLD
		call	NORDTST		.Safety	measure
		if over	.Major Fucking Problem - not really possible!!!
		else
			move	"O.M.C.-updatab,nordfile",Location
			trap	IOMssg giving Error if IO
			filepi	1;NORDFILE
			updatab	NORDFILE;*156,OCAMP
			trapclr	IO
		endif
	endif
	return

OrderMoveLOLCampaign LRoutine DimPtr,DimPtr1,DimPtr2,DimPtr3,DimPtr4
.DimPtr  = current Campaign value
.DimPtr1 = new Campaign value
.DimPtr2 = current List value
.DimPtr3 = new List value
.DimPtr4 = NLOLLOL value
	move	DimPtr1,NLOLCNUM
	move	DimPtr3,NLOLLIST
	move	DimPtr4,NLOLLOL
	call	Trim using NLOLCNUM
	call	Trim using NLOLLIST
	call	Trim using NLOLLOL
	pack	NLOLFLD2,AKey1,DimPtr,DimPtr2
	clear	NLOLFLD3
	clear	NLOLFLD4
	move	C3,NLOLLOCK
	move	C3,NLOLPATH
	move	"O.M.LOL.C.-NLOLAIMA",Location
	pack	KeyLocation,"Key: ",NLOLFLD2
	call	NLOLAIMA
	loop
		if over
			clear	taskname
			append	NLOLFLD2,taskname
			append	" Record not Found in NINLOL File-NLOLAIMA!!",taskname
			reset	taskname
			alert	caution,taskname,result
			move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is sent with the attachments
			move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
			move	taskname,SmtpTextMessage(2)   Array <Text message >
			move	"Subroutine OrderMoveLOLCampaign",SmtpTextMessage(3)   Array <Text message >
			move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
			call	errmesg
			goto FileGo3
.			 stop
		endif
		unpack	NLOL24,str6
		until (str6 = NLOLLOL)
		move	"O.M.LOL.C.-NLOLKGA",Location
		call	NLOLKGA
	repeat
	move	"O.M.LOL.C.-UPDATE NLOLFLE2",Location
	pack	KeyLocation,"Key: ",NLOLFLD2
	TRAP	IOMssg Giving Error if IO
	filepi	1;NLOLFLE2
	UPDATE	NLOLFLE2;NLOLVARS
	trapclr	IO
	return

OrderMoveLOLCampaign2 LRoutine DimPtr,DimPtr1,DimPtr2
.DimPtr  = current Campaign value
.DimPtr1 = new Campaign value
.DimPtr2 = NLOLLOL value
	move	DimPtr1,NLOLCNUM
	move	DimPtr2,NLOLLOL
	call	Trim using NLOLCNUM
	call	Trim using NLOLLOL
	move	C2,NLOLPATH
	move	DimPtr,NLOLFLD1
	move	"O.M.LOL.C.2-NLOLTST",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	call	NLOLTST
	loop
		if over
			clear	taskname
			append	NLOLFLD1,taskname
			append	" Record not Found in NINLOL1 File-NLOLTST!!",taskname
			reset	taskname
			alert	caution,taskname,result
			move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
			move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
			move	taskname,SmtpTextMessage(2)   Array <Text message >
			move	"Subroutine OrderMoveLOLCampaign2",SmtpTextMessage(3)	Array <Text message >
			move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
			call	errmesg
			goto FileGo3
.			 stop
		endif
		unpack	NLOL24,str6,str5,str1
		pack	str7,str5,str1
		until (str6 = NLOLLOL)
		until (str7 <> DimPtr)
		call	NLOLKS2
	repeat
	if (str7 <> NLOLCNUM)
		clear	taskname
		append	NLOLFLD1,taskname
		append	" Record not Found in NINLOL1 File-NLOLKS2!!",taskname
		reset	taskname
		alert	caution,taskname,result
		move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
		move	"This is an error message",SmtpTextMessage(1)	Array <Text message >
		move	taskname,SmtpTextMessage(2)   Array <Text message >
		move	"Subroutine OrderMoveLOLCampaign2",SmtpTextMessage(3)	Array <Text message >
		move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
		call	errmesg
		goto FileGo3
.		 stop
	endif
	move	"O.M.L.C2-DELETEK NLOLFLE1",Location
	pack	KeyLocation,"Key: ",NLOLFLD1
	TRAP	IOMssg Giving Error if IO
	filepi	5;NLOLFLE1
	DELETEDK NLOLFLE1,NLOLFLD1
	move	"O.M.L.C2-INSERT NLOLFLE1",Location
	pack	KeyLocation,"Key: ",NLOLCNUM
	INSERT	NLOLFLE1,NLOLCNUM
	trapclr	IO
.START PATCH 3.75 ADDED LOGIC
	call	IntegralMoveCampaign using NLOLLOL,DimPtr1,C0
.END PATCH 3.75 ADDED LOGIC
	return

OrderMoveTestMailer LRoutine DimPtr,DimPtr1,DimPtr2,FrmPtr
.DimPtr	= Camp.	Mlr, DimPtr1 = LR Mlr, DimPtr2 = LR #, FrmPtr =	Flag
.START PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
.	if (DimPtr <> DimPtr1)
	pack	COMPFLD,DimPtr
	move	"OMTM-COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if (COMPOLDMLR <> DimPtr1)
.END PATCH 3.75.7 REPLACED LOGIC - TEMPORARY PATCH
		clear	taskname
		append	"Record: ",taskname
		append	DimPtr2,taskname
		append	carr,taskname
		append	"This record has a different Mailer than",taskname
		append	carr,taskname
		append	"the destination Campaign!",taskname
		append	carr,taskname
		append	"You must change the record first!!",taskname
		reset	taskname
		alert	caution,taskname,howmany
		move	C0,FrmPtr
	else
		move	C1,FrmPtr
	endif
	return
OrderListLostFocus
	call	GotFocus_Nord001AEditList
	call	LostFocus_Nord001AEditList
	return

.START PATCH 3.71.3 ADDED LOGIC
OrderDisplayOmit
	call	DisplayOmit using Nord0001,Nord001AEditMlr,Nord001AEditList,N4,MouseForm,T1,L1
	return
.END PATCH 3.71.3 ADDED LOGIC
*******************************************************************************
*********************************  Printing  **********************************
*******************************************************************************
.START PATCH 3.67 REPLACED LOGIC
.OrderLCRPrintIt
OrderLCRPrintIt LRoutine FrmPtr
	move	FrmPtr,ReprintFlag
.END PATCH 3.67 REPLACED LOGIC
.Default is to send report to the pit
	move	C1,PrintFlag
	call	OrderLCRPrintItRefresh
.
.BLANK STOCK
.
PRINTTOP
prtdate
.Removed as per	Time Saviour meeting 12/16/1999	ASH
.	 alert	 Type=style,"Print Office copy?",result
.	 if (result = 7) .NO
		goto LETHEAD
.	 endif
................
.Currently, users are not given	option of creating this	form in	PDF format
.unless	invoked	by Print button	on Screen 5, which does	not use	this branch.  ASH
.	 if (PrintFlag = 1)	 .Laser3
.START PATCH 3.61 REPLACED LOGIC
.		if (osflag = c2)	 .nt
.			splopen	  "\\NTS0\Laser3 Blankstock","A"
.		elseif (osflag = c1)	     .win 95 98
.			splopen	  "Laser3 Blankstock","A"
.		else   .(osflag	= c0)	      .Don't know prompt for printer
.			splopen	  "","A"
.		endif
		if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
			splopen	"\\NTS0\Laser3 Blankstock","A"
		elseif (osflag = c3 | osflag =c4)         .win 95 98
			splopen	"Laser3 Blankstock","A"
		else   .(osflag = c0)         .Don't know prompt for printer
			splopen	"","A"
		endif
.END PATCH 3.61 REPLACED LOGIC
.	 elseif	(PrintFlag = 2)	 .PDF File
.	 endif
.	 splopen "\\NTS0\LASER3","A"

.............
.	 print	 HPRESET:
.		 HPtTRAY:
.		 HPltrhd:		 .letterhead macro
.		 HPLETTER:
.		 HPDTCH12;
.............
	print	HPRESET;
	call	PortraitLTRHEAD
	print	HPDTCH12;
.
	if (NORD5STAT =	"02")
		print	*N,*N,*N,*N,*N,HPT325,hpdtch14,hpunon,hpitalic,hpbon,"Second Request":
			hpboff,hpunoff,hpuprght,hpdtch12;
	elseif (NORD5STAT = "03")
		print	*N,*N,*N,*N,*N,HPT325,hpdtch14,hpunon,hpitalic,hpbon,"Request Revision":
			hpboff,hpunoff,hpuprght,hpdtch12;
	else
		print	*N,*N,*N,*N,*N,b1;
	endif
	print	*N,b1;
	call	Trim using OWNLOCTY
	clear	taskname
	if (OWNLOCTY <>	"")
		pack	taskname,OWNlOCTY,COMMA,B1,OWNlOS,B1,OWNlOZC
	endif
.START PATCH 3.72 REPLACED LOGIC
.	print	*N,OWNlONM,HPT525,str25:
.		*N,OWNOCPY:
.		*N,OWNlOSA,HPT525,HPdtch85,"LR## ",OLRN:
.		*N,HPDTCH12,taskname:
.		*N:
.		*N:
.		*N:
.		*N,*26,HPDTCH12,HPBON,HPUNON:
.		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
.		*N:
.		*N:
.		*N,hptmsr10,"List:",HPT200,OLNUM,HPDTCH12,b2,O1DES:
.		*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
.		*N:
.		*N,hptmsr10,"Mailer:",HPT200,MNUM,HPDTCH12,b4,MCOMP:
.		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
.		*N,HPT200,HPDTCH12:
.		*N,HPDTCH12,HPT200,"Quantity",HPT325:
.		"Mail Period Requested":
.		HPT550,"Order Type"
	print	*N,OWNlONM,HPT525,str25:
		*N,OWNOCPY:
		*N,OWNlOSA,HPT525,HPdtch85,"LR## ",OLRN:
		*N,HPDTCH12,taskname:
		*N:
		*N:
		*N:
		*N,*26,HPDTCH12,HPBON,HPUNON:
		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
		*N:
		*N:
		*N,hptmsr10,"List:",HPT200,OLNUM,HPDTCH12,b2,O1DES:
		*N,hptmsr10,"Select:",HPT200,HPDTCH12,NSEL2NAME:
		*N:
		*N,hptmsr10,"Mailer:",HPT200,MNUM,HPDTCH12,b4,MCOMP:
		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
		*N,HPT200,HPDTCH12:
		*N,HPDTCH12,HPT200,"Quantity",HPT325:
		"Mail Period Requested":
		HPT550,"Order Type"
.END PATCH 3.72 REPLACED LOGIC
	pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
	if (newdate1 = "00/00/0000")
.START PATCH 3.68.8 REPLACED LOGIC
.		print	HPdtch12,HPT200,QTYPRNT,HPT325,"As Soon	As Possible":
.			HPT550,str18:
.			*N
.START PATCH 3.71 REPLACED LOGIC
.		print	HPdtch12,HPT050,QTYPRNT2,HPT325,"As Soon As Possible":
.			HPT550,str18:
.			*N
		if (OCO2CODE <> "" & OCO2CODE <> "  ")
			print	HPdtch12,HPT200,QTYPRNT2,HPT325,"As Soon As Possible":
				HPT550,str18:
				*N
		else
			print	HPdtch12,HPT050,QTYPRNT2,HPT325,"As Soon As Possible":
				HPT550,str18:
				*N
		endif
.END PATCH 3.71 REPLACED LOGIC
.END PATCH 3.68.8 REPLACED LOGIC
	elseif (newdate1 = "11/11/1111")
.START PATCH 3.68.8 REPLACED LOGIC
.		print	HPdtch12,HPT200,QTYPRNT,HPT325,"See Special Instructions":
.			HPT550,str18:
.			*N
.START PATCH 3.71 REPLACED LOGIC
.		print	HPdtch12,HPT050,QTYPRNT2,HPT325,"See Special Instructions":
.			HPT550,str18:
.			*N
		if (OCO2CODE <> "" & OCO2CODE <> "  ")
			print	HPdtch12,HPT200,QTYPRNT2,HPT325,"See Special Instructions":
				HPT550,str18:
				*N
		else
			print	HPdtch12,HPT050,QTYPRNT2,HPT325,"See Special Instructions":
				HPT550,str18:
				*N
		endif
.END PATCH 3.71 REPLACED LOGIC
.END PATCH 3.68.8 REPLACED LOGIC
	else
.START PATCH 3.68.8 REPLACED LOGIC
.		print	HPdtch12,HPT200,QTYPRNT,HPT325,newdate1:
.			HPT550,str18:
.			*N
.START PATCH 3.71 REPLACED LOGIC
.		print	HPdtch12,HPT050,QTYPRNT2,HPT325,newdate1:
.			HPT550,str18:
.			*N
		if (OCO2CODE <> "" & OCO2CODE <> "  ")
			print	HPdtch12,HPT200,QTYPRNT2,HPT325,newdate1:
				HPT550,str18:
				*N
		else
			print	HPdtch12,HPT050,QTYPRNT2,HPT325,newdate1:
				HPT550,str18:
				*N
		endif
.END PATCH 3.71 REPLACED LOGIC
.END PATCH 3.68.8 REPLACED LOGIC
	endif
	call	OrderPrintLCRSpecialInstructions
	print	*RPTCHAR "_":85:
		*N,HPdtch12,HPT200:
		*N,*34,HPDTCH12,HPBON,"CLEARANCE APPROVAL",HPBOFF,HPLIN8:
		*N:
		*N,*25,P72PT,"Please fill in below and fax to (415)433-7796":
		HPLIN6:
		*N,HPDTCH12,HPT150,"__",HPT175,"Approved":
		HPT450,"___Rental",HPT550,"___Exchange":
		*N:
		*N,HPT150,"__",HPT175,"Not Approved and	reason:	________":
		"_______________________________":
		*N:
		*N:
		*N,HPT175,"Signature: ____________________________________":
		"_________________"
.START PATCH FOR NEW EMAIL ADDRESSES
.PRTREST print	 HPDTCH12,*RPTCHAR "_":85,B1:
.		 *N:
.		 *N,"NIN Contact,":
.		 *N,CNTNAME,*94,MDLCALL:
.		 *n,hpdtch85,CNTPHONE:
.		 *n,str24:
.		 hpdtch12,hpreset
PRTREST	print	HPDTCH12,*RPTCHAR "_":85,B1:
		*N:
		*N,"NIN Contact,":
		*N,CNTNAME,*94,MDLCALL:
		*n,hpdtch85,CNTPHONE:
		*n,str55:
		hpdtch12,hpreset
.END PATCH FOR NEW EMAIL ADDRESSES
	splclose
	release

.
. LETTERHEAD  loaded from hpdl.pcl
. CHECK	FOR DIVOKY AND SUPPRESS	IF EQUAL.
LETHEAD
.Suppress Fax if In House LCR
	if (OCO2CODE <>	"" AND OCO2CODE	<> "  ")
		goto OUT
	endif
	scan	"DIVOKY",OWNOCPY
	goto OUT if equal
	reset	OWNOCPY
	scan	"DIVOKY",OWNLONM
	goto OUT if equal
	reset	OWNLONM
.CHECK FOR LIST	OWNER FAX NUMBER IF PRESENT ASK	IF FAX????
	move	C1,faxflag		   .reset flag
	type	OWNFAX			      .VALID PHONE?
	if equal
.Following alert box is	a 'Plain', but I want to have the option of selecting the default button.
.Because of this I need	to declare the type, so	that the result	values are identical in	either
.branch.  Otherwise, if	I declared the alert box as 'Plain' in the first branch, answering 'Yes'
.will return '1', versus '6' with alert	box in the second branch.
		getitem	OptionsFaxLONo,0,N5
.		 if (N5	= C0)
.			 alert	 type=3,"Fax To	List Owner?",result	 .Plain	alert box
.		 else
.			 alert	 type=515,"Fax To List Owner?",result	 .0x003	(3) = Three button style. 0x200	(512) =	Third button (Cancel) is default.  0x200 + 0x003 = 0x203 = 515
.		 endif
		if (N5 = C1 | (OCLRSTAT	<> "" AND OCLRSTAT <> "	"))
			alert	type=515,"Fax To List Owner?",result	.0x003 (3) = Three button style. 0x200 (512) = Third button (Cancel) is	default.  0x200	+ 0x003	= 0x203	= 515
		else
			alert	type=3,"Fax To List Owner?",result	.Plain alert box
		endif
		if (result = 7)		.NO
.START PATCH 3.67 ADDED LOGIC
			move	C2,DevFlag	.Initialize
LETHEADPrint
.END PATCH 3.67 ADDED LOGIC
.Do not	write to file but instead Print	it out
			move	C1,faxflag
			move	NO,hotflag
		elseif (result = 2)	.CANCEL
.START PATCH 3.67 ADDED LOGIC
LETHEADCancel
.END PATCH 3.67 ADDED LOGIC
.Do not	write to file.	Do not Print it.
			move	C2,faxflag
			move	NO,hotflag
		else			.YES
.Write to file.	 Do not	Print it.
			move	C2,faxflag
			move	YES,hotflag
.Following update is for Outside LCRs.	This logic mimics Fax To Owner button on Screen	5.
.Implemented when Campaign logic was included.
.Double	check -	just in	case
			if (OSTAT = "l"	& (OCO2CODE = "" | OCO2CODE = "	 "))
.START PATCH 3.67 REPLACED LOGIC
.				move	STAR,OHIST
.				if (NORDFLAG <>	C1)
.					call	NORDOPEN
.				endif
.				filepi	1;NORDFILE
.				updatab	NORDFILE;*176,OHIST
..				 IF (LRINIT = 1)
..				 write	 LRFILE,SEQ;"NINORD - Updatab,LETHEAD"
..				 ENDIF
..........................
				call	OrderSetOutsideFaxReport
				setitem	ComboBoxes(1),0,1
				setprop Report2,visible=1
				getitem	ComboBoxes(1),0,DevFlag
				if (RptCan = YES)
					goto LETHEADCancel
				endif
.START PATCH 3.75.6 ADDED LOGIC
				if (OSCODE = "2")
					pack	taskname,OLRN," is marked as 'Sample to Follow'!",newline,"Make sure you include a Sample if the List Owner",newline,"has not yet received it."
					alert	caution,taskname,N6
				endif
.END PATCH 3.75.6 ADDED LOGIC
.DevFlag = 1 - Nightly Fax
.DevFlag = 2 - Laser 3
.DevFlag = 3 - .PDF File
.DevFlag = 4 - Laser 2
.DevFlag = 5 - Fax
				if (DevFlag = C1)	.Nightly Fax
					move	STAR,OHIST
					if (NORDFLAG <>	C1)
						call	NORDOPEN
					endif
					filepi	1;NORDFILE
					updatab	NORDFILE;*176,OHIST
					if (ReprintFlag = C1)		.Called by Reprint button - not automatically written to NINPRINT
						trap	IOMssg2	Giving Error if	IO
						move	"LETHEAD-NINPRINT",Location
						pack	KeyLocation,"Key: ",OLRN
.Start patch 3.78.8 CODE Modification - Addition of OFULLFIL						
						read	ORDPRINT,OLRN;;
						if over
.begin patch 3.79.2   OcompID Ocompid2
							write	ORDPRINT,OLRN;"F",OSTAT,OMLRNUM,OLRN,OCOBN,OLNUM:
								OLON,OMLRPON,OQTY,OPPM,OMLRKY,OFOCODE:
								ORTNDTEC,ORTNDTEY,ORTNDTEM,ORTNDTED:
								OMDTEC,OMDTEY,OMDTEM,OMDTED,OTOCODE,OSOTCODE,OCCODE,OLRNCO:
								OODTECOC,OODTECOY,OODTECOM,OODTECOD:
								OQTYCO,OSPI,B2,OELCODE:
								OODNUM,OODES,ONETQTY,OCAMP,OCLRSTAT,OCLRINIT,OBRKRPT,OCLRDTEC,OCLRDTEY,OCLRDTEM,OCLRDTED,ORENT,OHIST,OXPPM,ORTNNUM,OTAPERET,OUQTY,OSALES10,OSALES,OCOCODE,OCO2CODE:
								OODTEC,OODTEY,OODTEM,OODTED,OSCODE,OCOMSLCT,OSHP,O1DES,O2DES:
								OREUSE,ODOWJ,OEXQTY,GUARCODE,OBRKNUM,OBRKCNT:
								OSAMCDE:
								onetper,onetrc,onetfm,onetmin,ofullfil:
								OCompID:
								OCompid2:
								ofiller								
.end patch 3.79.2   OcompID Ocompid2
.								onetper,onetrc,onetfm,onetmin,ofiller
						endif
.End patch 3.78.8 CODE Modification - Addition of OFULLFIL						
					endif
				else			.Standard Print/.PDF or Fax
					if (NewFlag <> YES)
						trap	IOMssg2	Giving Error if	IO
						move	"LETHEAD2-NINPRINT",Location
						pack	KeyLocation,"Key: ",OLRN
						read	ORDPRINT,OLRN;;
						if not over
							clear	taskname
							append	"This LCR is scheduled to be Faxed this evening!",taskname
							append	newline,taskname
							append	"Do you want it to go?",taskname
							reset	taskname
							alert	plain,taskname,result
							if (result = 2)
								delete	ORDPRINT,OLRN
							elseif (result = 3)
								goto LETHEADCancel
							endif
						endif
					endif
					sub	C1,DevFlag,PrintFlag
					goto LETHEADPrint
				endif
.END PATCH 3.67 REPLACED LOGIC
			endif
		endif
	else
		alert	type=4,"No fax ## for List owner, add it?",result
		if (result = 6)	.YES
			create	ErrorMssg;EditTextBoxes(1)=100:120:10:100,MaxChars=10,EditType=3,SelectAll=1,Style=1,Border=1
			activate EditTextBoxes(1)
			setprop	ErrorMssgStat1,visible=0
			setprop	ErrorMssgStat2,visible=1
			setprop	ErrorMssgStat3,visible=0
			setprop	ErrorMssgStat4,visible=0
			setprop	ErrorMssgStat5,visible=1
			setitem	ErrorMssgStat2,0,"    Enter 10 Digit Fax Number"
			setitem	ErrorMssgStat5,0,"10 zero's will skip Owner file update!"
			setitem	ErrorMssgOK,0,"O&K"
			setfocus EditTextBoxes(1)
			loop
				setprop	ErrorMssg,visible=1
				getitem	EditTextBoxes(1),0,str10
				count	N2,str10
				until (N2 = 10)
				until (str10 = "0000000000")
			repeat
			destroy	EditTextBoxes(1)
			if (str10 <> "0000000000")
				move	str10,OWNFAX
				move	USERNME,OWNPASS
				unpack	timestamp,str4,str3,str1
				pack	OWNRDTE,str3,str1,str4
				call	NOWNUPD
				IF (LRINIT = 1)
				move	"NINOWN	- Update,LETHEAD",str45
				call	OrderWriteLRFile using str45
				ENDIF
			endif
			goto	LETHEAD
		endif
		goto out
	endif
	branch	faxflag	to splown,out
splown
.START PATCH 3.75.2 ADDED LOGIC
	call	PrintSingleLCR using OLRN,PrintFlag,PORTN
	return
.END PATCH 3.75.2 ADDED LOGIC
.START PATCH 3.76.5 RETIRED LOGIC
..FOLLOWING CODE IS NO LONGER ACCESSED!!!!!
.	if (PrintFlag =	1)	.Laser3
..START PATCH 3.61 REPLACED LOGIC
..		if (osflag = c2)	 .nt
..			splopen	  "\\NTS0\Laser3 Blankstock","A"
..		elseif (osflag = c1)	     .win 95 98
..			splopen	  "Laser3 Blankstock","A"
..		else   .(osflag	= c0)	      .Don't know prompt for printer
..			splopen	  "","A"
..		endif
.		if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
.			splopen	"\\NTS0\Laser3 Blankstock","A"
.		elseif (osflag = c3 | osflag =c4)         .win 95 98
.			splopen	"Laser3 Blankstock","A"
.		else   .(osflag = c0)         .Don't know prompt for printer
.			splopen	"","A"
.		endif
..END PATCH 3.61 REPLACED LOGIC
..START PATCH 3.67 REPLACED LOGIC
..	elseif (PrintFlag = 2)	.PDF File
..		call	Trim using OLRN
..		if (OLRN = "")
..			alert	caution,"Invalid LR#!!!",result
..			return
..		else
..			pack	APIFileName,NTWKPATH1,OLRN,".PDF",HexZero
..			call	FindFirstFile
..			if (APIResult <> 0 & APIResult <> hexeight)
...File already exists, cannot create in PDF format!!
..				alert	caution,"This file already exists in PDF format!!!",result
..				return
..			else
..				pack	APIFileName,NTWKPATH1,OLRN,".LST"
..				splopen	APIFileName
..			endif
..		endif
.	elseif (PrintFlag = 2 | PrintFlag = 4)		.PDF/Fax
.		splopen	"Faxfile","A"
..END PATCH 3.67 REPLACED LOGIC
.	elseif (PrintFlag = 3)	    .Laser2
..END PATCH 3.61 REPLACED LOGIC
..		if (osflag = c2)	 .nt
..			splopen	  "\\NTS0\Laser2","A"
..		elseif (osflag = c1)	     .win 95 98
..			splopen	  "Laser2","A"
..		else   .(osflag	= c0)	      .Don't know prompt for printer
..			splopen	  "","A"
..		endif
.		if (osflag = c1 | osflag = C5 | osflag = c6)         .nt 2k xp
.			splopen	"\\NTS0\Laser2","A"
.		elseif (osflag = c3 | osflag =c4)         .win 95 98
.			splopen	"Laser2","A"
.		else   .(osflag = c0)         .Don't know prompt for printer
.			splopen	"","A"
.		endif
..END PATCH 3.61 REPLACED LOGIC
.	endif
..	  if (osflag = c2)	   .nt
..		  splopen   "\\NTS0\Laser3 Blankstock","A"
..	  elseif (osflag = c1)	       .win 95 98
..		  splopen   "Laser3 Blankstock","A"
..	  else	 .(osflag = c0)		.Don't know prompt for printer
..		  splopen   "","A"
..	  endif
..	 splopen "\\NTS0\LASER3","A"
.	goto prtown
.faxown
.	count	N2,OWNFAX
.	compare	C10,N2
.	if equal
.		move	C1,LONGDIST
.		unpack	OWNFAX,STR3,STR7
.		match	  "510",str3		. local?
.		if equal
.			move	str7,OWNFAX
.			clear	LONGDIST
.		endif
.	endif
.	move	C1,N2
.FILENAME
.	clear	formname
.	append	dcxpath,formname
.	append	"NLCR",formname
.	move	N2,str2
.	rep	zfill,str2
.	append	str2,formname
.	reset	formname,6
.	reset	formname
.	trap	GOODFILE GIVING	ERROR IF IO
.	open	formfile,formname
.	close	formfile
.ADDFILE
.	add	C1,N2
.	goto FILENAME
.GOODFILE
.	trapclr	IO
.	noreturn
.	reset	error
.	scan	"I * Y",error
.	goto ADDFILE if	equal
.	clear	formname
.	append	"NLCR",formname
.	move	N2,str2
.	rep	zfill,str2
.	append	str2,formname
.	reset	formname,6
.	reset	formname
.	clear	recname
.	append	"\\nts0\d\public\notework\",recname
.	append	formname,recname
.	append	".frm",recname
.	reset	recname
.	move	B1,error
.	prepare	formfile,recname,CREATE
.	clear	taskname
.	append	"The output file name for this fax is:",taskname
.	append	carr,taskname
.	append	formname,taskname
.	reset	taskname
.	alert	note,taskname,result
.	clear	recname
.       	append	dcxpath,recname
.	append	formname,recname
.	append	".dat",recname
.	reset	recname
.	clear	sampl
.	type	OSAMCDE
.	if equal
.		match	"000",OSAMCDE
.		if not equal
.			move	",S",str2
.			pack	sampl,str2,OMLRNUM,OSAMCDE,dcx
.		endif
.	else
.		clear	  sampl
.	endif
.	write	FORMFILE,SEQ;"SMF-70"
.	write	FORMFILE,SEQ;"o-nw-formtype: n,w, #"Datacard Update Request#""
.	write	FORMFILE,SEQ;"TO: Fax @	MHSNTFAX ( ) {FAX: ":
.		LONGDIST,B1,*LL,OWNFAX;
.	write	FORMFILE,SEQ;";	TO: ",*ll,OWNOCPY,"}"
.	write	FORMFILE,SEQ;"From: ",nuseuser
.	write	FORMFILE,SEQ;"Subject: Request for list	approval"
.	write	FORMFILE,SEQ;"ATTACHMENT: ",*LL,formname,".dat",sampl
.	write	FORMFILE,SEQ;"ATTACHMENT-NAME: ",*LL,formname,".dat",sampl
.	weof	FORMFILE,SEQ
.	close	formfile
.	splopen	recname
..............
..	 print	 HPRESET:
..		 HPtTRAY:
..		 HPltrhd:		 .letterhead macro
..		 HPLETTER:
..		 HPDTCH12;
..............
.	print	HPRESET;
.	call	PortraitLTRHEAD
.	print	HPDTCH12;
..
.	clear	area
.	clear	str3
.	clear	str4
.	clear	str5
.	unpack	ownfax into area,str3,str4
.	cmatch	B1,str4
.	if not equal
.		move	"Fax: ",str5
.	endif
.	if (NORD5STAT =	"02")
.		print	*N,*N,*N,*N,*N,HPT325,hpdtch14,hpunon,hpitalic,hpbon,"Second Request":
.			hpboff,hpunoff,hpuprght,hpdtch12;
.	elseif (NORD5STAT = "03")
.		print	*N,*N,*N,*N,*N,HPT325,hpdtch14,hpunon,hpitalic,hpbon,"Request Revision":
.			hpboff,hpunoff,hpuprght,hpdtch12;
.	else
.		print	*N,*N,*N,*N,*N,b1;
.	endif
.	print	*N,b1;
.	call	Trim using OWNLOCTY
.	clear	taskname
.	if (OWNLOCTY <>	"")
.		pack	taskname,OWNlOCTY,COMMA,B1,OWNlOS,B1,OWNlOZC
.	endif
..START PATCH 3.71.3 REPLACED LOGIC
..	print	*N,OWNlONM,HPT525,str25:
..		*N,OWNOCPY:
..		*N,OWNlOSA,HPT525,HPDTCH85,"LR## ",OLRN:
..		*N,HPDTCH12,taskname:
..		*N,str5,area,B1,str3,B1,str4:
..		*N:
..		*N:
..		*N,*26,HPDTCH12,HPBON,HPUNON:
..		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..		*N:
..		*N:
..		*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..		*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..		*N:
..		*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..		*N,HPT200,HPDTCH12:
..		*N,HPDTCH12,HPT200,"Quantity",HPT325:
..		"Mail Period Requested":
..		HPT550,"Order Type"
................
.	clear	str24
.	call	Trim using OWNTELE
.	if (OWNTELE <> "")
.		count	result,OWNTELE
.		if (result = 10)
.			unpack	OWNTELE,str2,str1
.			append	str2,str24
.			append	str1,str24
.			append	" ",str24
.			bump	OWNTELE,3
.			unpack	OWNTELE,str2,str1,str6
.			append	str2,str24
.			append	str1,str24
.			append	" ",str24
.			append	str6,str24
.			reset	str24
.		elseif (result = 7)
.			unpack	OWNTELE,str2,str1,str6
.			append	str2,str24
.			append	str1,str24
.			append	" ",str24
.			append	str6,str24
.			reset	str24
.		endif
.	endif
..START PATCH 3.72 REPLACED LOGIC
..	print	*N,OWNlONM,HPT525,str25:
..		*N,OWNOCPY:
..		*N,OWNlOSA,HPT525,HPDTCH85,"LR## ",OLRN:
..		*N,HPDTCH12,taskname:
..		*N,str24:
..		*N,str5,area,B1,str3,B1,str4:
..		*N:
..		*N,*26,HPDTCH12,HPBON,HPUNON:
..		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..		*N:
..		*N:
..		*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..		*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..		*N:
..		*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..		*N,HPT200,HPDTCH12:
..		*N,HPDTCH12,HPT200,"Quantity",HPT325:
..		"Mail Period Requested":
..		HPT550,"Order Type"
.	print	*N,OWNlONM,HPT525,str25:
.		*N,OWNOCPY:
.		*N,OWNlOSA,HPT525,HPDTCH85,"LR## ",OLRN:
.		*N,HPDTCH12,taskname:
.		*N,str24:
.		*N,str5,area,B1,str3,B1,str4:
.		*N:
.		*N,*26,HPDTCH12,HPBON,HPUNON:
.		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
.		*N:
.		*N:
.		*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
.		*N,hptmsr10,"Select:",HPT200,HPDTCH12,NSEL2NAME:
.		*N:
.		*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
.		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
.		*N,HPT200,HPDTCH12:
.		*N,HPDTCH12,HPT200,"Quantity",HPT325:
.		"Mail Period Requested":
.		HPT550,"Order Type"
..END PATCH 3.72 REPLACED LOGIC
..END PATCH 3.71.3 REPLACED LOGIC
.	pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.	if (newdate1 = "00/00/0000")
..START PATCH 3.68.8 REPLACED LOGIC
..		print	HPdtch12,HPT200,QTYPRNT,HPT325,"As Soon	As Possible":
..			HPT550,str18:
..			*N
..START PATCH 3.71 REPLACED LOGIC
..		print	HPdtch12,HPT050,QTYPRNT2,HPT325,"As Soon As Possible":
..			HPT550,str18:
..			*N
.		if (OCO2CODE <> "" & OCO2CODE <> "  ")
.			print	HPdtch12,HPT200,QTYPRNT2,HPT325,"As Soon As Possible":
.				HPT550,str18:
.				*N
.		else
.			print	HPdtch12,HPT050,QTYPRNT2,HPT325,"As Soon As Possible":
.				HPT550,str18:
.				*N
.		endif
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	elseif (newdate1 = "11/11/1111")
..START PATCH 3.68.8 REPLACED LOGIC
..		print	HPdtch12,HPT200,QTYPRNT,HPT325,"See Special Instructions":
..			HPT550,str18:
..			*N
..START PATCH 3.71 REPLACED LOGIC
..		print	HPdtch12,HPT050,QTYPRNT2,HPT325,"See Special Instructions":
..			HPT550,str18:
..			*N
.		if (OCO2CODE <> "" & OCO2CODE <> "  ")
.			print	HPdtch12,HPT200,QTYPRNT2,HPT325,"See Special Instructions":
.				HPT550,str18:
.				*N
.		else
.			print	HPdtch12,HPT050,QTYPRNT2,HPT325,"See Special Instructions":
.				HPT550,str18:
.				*N
.		endif
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	else
..START PATCH 3.68.8 REPLACED LOGIC
..		print	HPdtch12,HPT200,QTYPRNT,HPT325,newdate1:
..			HPT550,str18:
..			*N
..START PATCH 3.71 REPLACED LOGIC
..		print	HPdtch12,HPT050,QTYPRNT2,HPT325,newdate1:
..			HPT550,str18:
..			*N
.		if (OCO2CODE <> "" & OCO2CODE <> "  ")
.			print	HPdtch12,HPT200,QTYPRNT2,HPT325,newdate1:
.				HPT550,str18:
.				*N
.		else
.			print	HPdtch12,HPT050,QTYPRNT2,HPT325,newdate1:
.				HPT550,str18:
.				*N
.		endif
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	endif
.	call	OrderPrintLCRSpecialInstructions
.	print	*RPTCHAR "_":85:
.		*N,HPdtch12,HPT200:
.		*N,*34,HPDTCH12,HPBON,"CLEARANCE APPROVAL",HPBOFF,HPLIN8:
.		*N:
.		*N,*25,P72PT,"Please fill in below and fax to (415)433-7796":
.		HPLIN6:
.		*N,HPDTCH12,HPT150,"__",HPT175,"Approved":
.		HPT450,"___Rental",HPT550,"___Exchange":
.		*N:
.		*N,HPT150,"__",HPT175,"Not Approved and	reason:	________":
.		"_______________________________":
.		*N:
.		*N:
.		*N,HPT175,"Signature: ____________________________________":
.		"_________________"
..START PATCH FOR NEW EMAIL ADDRESSES
..	 print	 HPDTCH12,*RPTCHAR "_":85,b1:
..		 *N:
..		 *N,"NIN Contact,":
..		 *N,CNTNAME,*94,MDLCALL:
..		 *N,hpdtch85,CNTPHONE:
..		 *N,str24:
..		 hpdtch12
.	print	HPDTCH12,*RPTCHAR "_":85,b1:
.		*N:
.		*N,"NIN Contact,":
.		*N,CNTNAME,*94,MDLCALL:
.		*N,hpdtch85,CNTPHONE:
.		*N,str55:
.		hpdtch12
..END PATCH FOR NEW EMAIL ADDRESSES
.	splclose
.	release
.faxit	clear	taskname
.	append	"c:\progra~1\lanbatch\BATCH -X -N -PC:\WORK BB butil job=NLCRSEND INFILE=",taskname
.	append	formname,taskname
.	append	" F=default C=",taskname
.	append	C1,taskname
.	append	" B=",taskname
.	append	user,taskname
.	reset	taskname
..***********************************
..apperant problem with execute and the number of file handles we are using,
..therefore the file having problems (i*z) is being closed before execute and
..status	flag reset to match its	status.	This does take care of the problem.
..22feb94 DLH.
..*************************************
..	 close	 nlcrfle2
..	 move	 c0 to nlcrflg2
.	close	NXRFFILE
.	move	C0,NXRFFLAG
.	execute	taskname
..
..	 getprop OrderInfo,visible=N1
..	 if (N1	= C1)
..		 call	 OrderInfoClose
..	 endif
..	 clear	 OrderInfoString
..	 append	 "I'm sending ",OrderInfoString
..	 append	 formname,OrderInfoString
..	 append	 " now!",OrderInfoString
..	 reset	 OrderInfoString
..	 setitem OrderInfoStatText1,0,""
..	 setitem OrderInfoStatText2,0,""
..	 setitem OrderInfoStatText3,0,OrderInfoString
..	 setitem OrderInfoStatText4,0,""
..	 setitem OrderInfoStatText5,0,""
..	 setprop OrderInfo,title="LCR Fax Information"
..	 setprop OrderInfo,visible=1
..	 pause	 "1"
..	 call	 OrderInfoClose
.	pack	str45,"LCR Fax Information"
.	clear	str55
.	append	"I'm sending ",str55
.	append	formname,str55
.	append	" now!",str55
.	reset	str55
.	clear	str1
.	call	OrderDisplayMessage using Nord0001,str45,str1,str1,str55,str1,str1,C0,C0,C0,C0
.	pause	"1"
.	call	OrderInfoClose
..
.	return
.
.prtown
..START PATCH 3.67 REPLACED LOGIC
..	if (PrintFlag =	2)
................
...		 print	 033,"&l1E",033,"&a0c0R":
...			 033,"*p632x75Y":
...			 033,"(8U",033,"(s1p24.00v0s+3b5T",hpt250,"Names":
...			 b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
...			 *l,hpln4,*l:
...			 033,"(8U",033,"(s1p12.00v0s-2b5T",hpt250," C  A  L  I	F  O  R	 N  I  A	I  N  C	.":
...			 "*p600x3038Y":
...			 033,"(8U",033,"(s1p10.00v0s-2b5T",hpt150,"One Bush Street, San	Francisco, CA 94104 ":
...			 bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"*p0x0Y":
...			 HPLETTER,HPCOUR,HPDTCH12;
................
..		print	033,"&l1E",033,"&a0c0R":
..			033,"*p632x75Y":
..			033,"(8U",033,"(s1p24.00v0s+3b5T",hpt250,"Names":
..			b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
..			*l,hpln4,*l:
..			033,"(8U",033,"(s1p12.00v0s-2b5T",hpt250," C  A	 L  I  F  O  R	N  I  A	       I  N  C .":
..			"*p600x3038Y":
..			033,"(8U",033,"(s1p10.00v0s-2b5T",hpt150,"1300 Clay St., 11th Floor, Oakland, CA 94612-1429 ":
..			bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"*p0x0Y":
..			HPLETTER,HPCOUR,HPDTCH12;
..	else
...............
...		 print	 *N,*N,HPRESET:
...			 HPltrhd:		 .letterhead macro
...			 HPLETTER:
...			 HPCOUR,HPDTCH12;		 .letterhead macro
...............
..		print	HPRESET;
..		call	PortraitLTRHEAD
..		print	*N,*N,hpfixed,"&a13l",HPCOUR,HPDTCH12;
..	endif
....................
.	print	HPRESET;
.	call	PortraitLTRHEAD
.	print	*N,*N,hpfixed,"&a13l",HPCOUR,HPDTCH12;
..END PATCH 3.67 REPLACED LOGIC
.	clear	area
.	clear	str3
.	clear	str4
.	clear	str5
.	unpack	ownfax into area,str3,str4
.	cmatch	B1,str4
.	if not equal
.		move	"Fax: "	to str5
.	endif
.	if (NORD5STAT =	"02")	   .2nd	request?
.		print	*N,*N,*N,*N,*N,HPT325,hpdtch14,hpunon,hpitalic,hpbon,"Second Request":
.			hpboff,hpunoff,hpuprght,hpdtch12;
.	elseif (NORD5STAT = "03")      .Revised	request?
.		print	*N,*N,*N,*N,*N,HPT325,hpdtch14,hpunon,hpitalic,hpbon,"Request Revision":
.			hpboff,hpunoff,hpuprght,hpdtch12;
.	else
.		print	*N,*N,*N,*N,*N,b1;
.	endif
.	print	*N,B1;
.	call	Trim using OWNLOCTY
.	clear	taskname
.	if (OWNLOCTY <>	"")
.		pack	taskname,OWNlOCTY,COMMA,B1,OWNlOS,B1,OWNlOZC
.	endif
..START PATCH 3.68.8 REPLACED LOGIC
..	print	*N,OWNlONM,HPT525,str25:
..		*N,OWNOCPY:
..		*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
..		*N,HPDTCH12,taskname:
..		*N,str5,area,b1,str3,str4:
..		*N:
..		*N:
..		*N,*26,HPDTCH12,HPBON,HPUNON:
..		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..		*N:
..		*N:
..		*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..		*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..		*N:
..		*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..		*N,HPT200,HPDTCH12:
..		*N,HPDTCH12,HPT200,"Quantity",HPT325:
..		"Mail Period Requested":
..		HPT550,"Order Type"
..START PATCH 3.71 REPLACED LOGIC
..	print	*N,OWNlONM,HPT525,str25:
..		*N,OWNOCPY:
..		*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
..		*N,HPDTCH12,taskname:
..		*N,str5,area,b1,str3,str4:
..		*N:
..		*N:
..		*N,*26,HPDTCH12,HPBON,HPUNON:
..		"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..		*N:
..		*N:
..		*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..		*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..		*N:
..		*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..		*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..		*N,HPT200,HPDTCH12:
..		*N,HPDTCH12,HPT050,"Mail Period Requested",HPT275:
..		"Quantity":
..		HPT625,"Order Type"
..START PATCH 3.71.3 REPLACED LOGIC
..	if (OCO2CODE <> "" & OCO2CODE <> "  ")
..		print	*N,OWNlONM,HPT525,str25:
..			*N,OWNOCPY:
..			*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
..			*N,HPDTCH12,taskname:
..			*N,str5,area,b1,str3,str4:
..			*N:
..			*N:
..			*N,*26,HPDTCH12,HPBON,HPUNON:
..			"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..			*N:
..			*N:
..			*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..			*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..			*N:
..			*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..			*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..			*N,HPT200,HPDTCH12:
..			*N,HPDTCH12,HPT200,"Quantity",HPT325:
..			"Mail Period Requested":
..			HPT550,"Order Type"
..	else
..		print	*N,OWNlONM,HPT525,str25:
..			*N,OWNOCPY:
..			*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
..			*N,HPDTCH12,taskname:
..			*N,str5,area,b1,str3,str4:
..			*N:
..			*N:
..			*N,*26,HPDTCH12,HPBON,HPUNON:
..			"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..			*N:
..			*N:
..			*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..			*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..			*N:
..			*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..			*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..			*N,HPT200,HPDTCH12:
..			*N,HPDTCH12,HPT050,"Mail Period Requested",HPT275:
..			"Quantity":
..			HPT625,"Order Type"
..	endif
.....................
.	clear	str24
.	call	Trim using OWNTELE
.	if (OWNTELE <> "")
.		count	result,OWNTELE
.		if (result = 10)
.			unpack	OWNTELE,str2,str1
.			append	str2,str24
.			append	str1,str24
.			append	" ",str24
.			bump	OWNTELE,3
.			unpack	OWNTELE,str2,str1,str6
.			append	str2,str24
.			append	str1,str24
.			append	" ",str24
.			append	str6,str24
.			reset	str24
.		elseif (result = 7)
.			unpack	OWNTELE,str2,str1,str6
.			append	str2,str24
.			append	str1,str24
.			append	" ",str24
.			append	str6,str24
.			reset	str24
.		endif
.	endif
.	if (OCO2CODE <> "" & OCO2CODE <> "  ")
..START PATCH 3.72 REPLACED LOGIC
..		print	*N,OWNlONM,HPT525,str25:
..			*N,OWNOCPY:
..			*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
..			*N,HPDTCH12,taskname:
..			*N,str24:
..			*N,str5,area,b1,str3,B1,str4:
..			*N:
..			*N,*26,HPDTCH12,HPBON,HPUNON:
..			"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..			*N:
..			*N:
..			*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..			*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..			*N:
..			*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..			*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..			*N,HPT200,HPDTCH12:
..			*N,HPDTCH12,HPT200,"Quantity",HPT325:
..			"Mail Period Requested":
..			HPT550,"Order Type"
..	else
..		print	*N,OWNlONM,HPT525,str25:
..			*N,OWNOCPY:
..			*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
..			*N,HPDTCH12,taskname:
..			*N,str24:
..			*N,str5,area,b1,str3,B1,str4:
..			*N:
..			*N,*26,HPDTCH12,HPBON,HPUNON:
..			"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
..			*N:
..			*N:
..			*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
..			*N,hptmsr10,"Select:",HPT200,HPDTCH12,O2DES:
..			*N:
..			*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
..			*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
..			*N,HPT200,HPDTCH12:
..			*N,HPDTCH12,HPT050,"Mail Period Requested",HPT275:
..			"Quantity":
..			HPT625,"Order Type"
.		print	*N,OWNlONM,HPT525,str25:
.			*N,OWNOCPY:
.			*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
.			*N,HPDTCH12,taskname:
.			*N,str24:
.			*N,str5,area,b1,str3,B1,str4:
.			*N:
.			*N,*26,HPDTCH12,HPBON,HPUNON:
.			"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
.			*N:
.			*N:
.			*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
.			*N,hptmsr10,"Select:",HPT200,HPDTCH12,NSEL2NAME:
.			*N:
.			*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
.			*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
.			*N,HPT200,HPDTCH12:
.			*N,HPDTCH12,HPT200,"Quantity",HPT325:
.			"Mail Period Requested":
.			HPT550,"Order Type"
.	else
.		print	*N,OWNlONM,HPT525,str25:
.			*N,OWNOCPY:
.			*N,OWNlOSA,HPT525,HPDTCH85,"LR ## ",OLRN:
.			*N,HPDTCH12,taskname:
.			*N,str24:
.			*N,str5,area,b1,str3,B1,str4:
.			*N:
.			*N,*26,HPDTCH12,HPBON,HPUNON:
.			"REQUEST FOR LIST APPROVAL",HPBOFF,HPUNOFf:
.			*N:
.			*N:
.			*N,hptmsr10,"List:",HPT200,HPDTCH12,O1DES:
.			*N,hptmsr10,"Select:",HPT200,HPDTCH12,NSEL2NAME:
.			*N:
.			*N,hptmsr10,"Mailer:",HPT200,HPDTCH12,MCOMP:
.			*N,hptmsr10,"Offer:",HPT200,HPDTCH12,OFDESC:
.			*N,HPT200,HPDTCH12:
.			*N,HPDTCH12,HPT050,"Mail Period Requested",HPT275:
.			"Quantity":
.			HPT625,"Order Type"
..END PATCH 3.72 REPLACED LOGIC
.	endif
..END PATCH 3.71.3 REPLACED LOGIC
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	pack	newdate1,OMDTEM,SLASH,OMDTED,SLASH,OMDTEC,OMDTEY
.	if (newdate1 = "00/00/0000")
..START PATCH 3.68.8 REPLACED LOGIC
..		print	HPdtch12,HPT200,QTYPRNT,HPT325,"As Soon	As Possible":
..			HPT550,str18:
..			*N
..START PATCH 3.71 REPLACED LOGIC
..		print	HPdtch12,HPT050,"As Soon As Possible",HPT275,QTYPRNT2:
..			HPT625,str18:
..			*N
.		if (OCO2CODE <> "" & OCO2CODE <> "  ")
.			print	HPdtch12,HPT200,QTYPRNT2,HPT325,"As Soon As Possible":
.				HPT550,str18:
.				*N
.		else
.			print	HPdtch12,HPT050,"As Soon As Possible",HPT275,QTYPRNT2:
.				HPT625,str18:
.				*N
.		endif
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	elseif (newdate1 = "11/11/1111")
..START PATCH 3.68.8 REPLACED LOGIC
..		print	HPdtch12,HPT200,QTYPRNT,HPT325,"See Special Instructions":
..			HPT550,str18:
..			*N
..START PATCH 3.71 REPLACED LOGIC
..		print	HPdtch12,HPT050,"See Special Instructions",HPT275,QTYPRNT2:
..			HPT625,str18:
..			*N
.		if (OCO2CODE <> "" & OCO2CODE <> "  ")
.			print	HPdtch12,HPT200,QTYPRNT2,HPT325,"See Special Instructions":
.				HPT550,str18:
.				*N
.		else
.			print	HPdtch12,HPT050,"See Special Instructions",HPT275,QTYPRNT2:
.				HPT625,str18:
.				*N
.		endif
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	else
..START PATCH 3.68.8 REPLACED LOGIC
..		print	HPdtch12,HPT200,QTYPRNT,HPT325,newdate1:
..			HPT550,str18:
..			*N
..START PATCH 3.71 REPLACED LOGIC
..		print	HPdtch12,HPT050,newdate1,HPT275,QTYPRNT2:
..			HPT625,str18:
..			*N
.		if (OCO2CODE <> "" & OCO2CODE <> "  ")
.			print	HPdtch12,HPT200,QTYPRNT2,HPT325,newdate1:
.				HPT550,str18:
.				*N
.		else
.			print	HPdtch12,HPT050,newdate1,HPT275,QTYPRNT2:
.				HPT625,str18:
.				*N
.		endif
..END PATCH 3.71 REPLACED LOGIC
..END PATCH 3.68.8 REPLACED LOGIC
.	endif
.	call	OrderPrintLCRSpecialInstructions
.	print	*RPTCHAR "_":85:
.		*N,HPdtch12,HPT200:
.		*N,*34,HPDTCH12,HPBON,"CLEARANCE APPROVAL",HPBOFF,HPLIN8:
.		*N:
.		*N,*25,P72PT,"Please fill in below and fax to (415)433-7796":
.		HPLIN6:
.		*N,HPDTCH12,HPT150,"__",HPT175,"Approved":
.		HPT450,"___Rental",HPT550,"___Exchange":
.		*N:
.		*N,HPT150,"__",HPT175,"Not Approved and	reason:	________":
.		"_______________________________":
.		*N:
.		*N:
.		*N,HPT175,"Signature: ____________________________________":
.		"_________________"
..START PATCH FOR NEW EMAIL ADDRESSES
..PRTREST1
..	 print	 HPDTCH12,*RPTCHAR "_":85,B1:
..		 *N:
..		 *N,"NIN Contact,":
..		 *N,CNTNAME,*94,MDLCALL:
..		 *n,hpdtch85,CNTPHONE:
..		 *n,str24:
..		 hpdtch12
.PRTREST1
.	print	HPDTCH12,*RPTCHAR "_":85,B1:
.		*N:
.		*N,"NIN Contact,":
.		*N,CNTNAME,*94,MDLCALL:
.		*n,hpdtch85,CNTPHONE:
.		*n,str55:
.		hpdtch12
..END PATCH FOR NEW EMAIL ADDRESSES
.	print	HPRESET
.	splclose
.	release
..START PATCH 3.67 ADDED LOGIC
.	if (PrintFlag = 2 | PrintFlag = 4)		.PDF/Fax
..Clean up pre-existing files!
.		call	CleanUpLCRFaxFiles
..
.		pack	APIFileName,"c:\work\faxfile.prn",HexZero
.		call	FindFirstFile
.		if (APIResult = 0 | APIResult = hexeight)
..File not created!
.			if (PrintFlag = 2)
.				pack	taskname,"File Creation Failed!  .PDF File not created!"
.			elseif (PrintFlag = 4)
.				pack	taskname,"File Creation Failed!  Fax will not be sent!"
.			endif
.			alert	caution,taskname,result
.			return
.		elseif (PrintFlag = 2) 			.PDF
.			clear	taskname
.			append	"\\nts0\c\apps\pcl2pdf\pcl2pdf32 ",taskname
.			append	"c:\work\faxfile.prn ",taskname
.			append	"c:\work\lcrfax",taskname
.			append	OLRN,taskname
..pcl2pdf32 Options:  /S - Silent Mode(no display), /M:# - Maximum number of pages
..I set the second option as the converter is inherently adding a second page that I want to suppress
.			append	".PDF /S /M:1",taskname
.			reset	taskname
.			execute	taskname
..Mail it back to them
.			pack	str25,"lcrfax",OLRN,".PDF"
.			pack	taskname,"c:\work\",str25
..
.			pack	str45,userlogn,"@nincal.com"
..
.			move	"Here is your PDF File.",SmtpSubject			.Subject
..   Set	the text message that is senT with the attachments
.			pack	SmtpTextMessage(1),taskname
.			move	"1",SmtpTextIndexLast					.Index to last entry in TextMessage array
.			move	"NTS4",SmtpEmailServer					.Address of email serverc
.			move	str45,SmtpEmailAddress
.			move	str45,SmtpUserName					.User name
.			move	str45,SmtpUserFullName					.User Full Name
.			move	SmtpEmailAddress,SmtpDestinations(1,1)
.			move	"1",SmtpDestIndexLast					.Index to last entry in Dest array
.			move	str25,SmtpAttachments(1,1)				.Attached file name
.			move	"C:\Work",SmtpAttachments(1,2)				.Path to attached file name
.			move	"1",SmtpAttIndexLast					.Index to last entry	- Only 1 entry
.			clear	SmtpLogFile						.'Clear' disables the LogFile
.			call	SmtpSend   ( 'Send' is in Smtp.Pri which is included in	TestSmtp.Dbs )
.		elseif (PrintFlag = 4)			.Fax
.			call	Trim using OWNFAX
.			type	OWNFAX			      .VALID PHONE?
.			if not equal
.				alert	note,"Invalid Fax Number for List Owner, Fax will not be sent!",result
.			else
.				if (OWNFAX = "" | OWNFAX = "0000000000")
.					alert	note,"Invalid Fax Number for List Owner, Fax will not be sent!",result
.				else
.					move	C1,N4
.					count	N2,OWNFAX
.					compare	C10,N2
.					if equal
.						move	C1,LONGDIST
.						unpack	OWNFAX,str3,str7
.						match	"510",str3
.						if equal
.							move	str7,OWNFAX
.							clear	LONGDIST
.						else
.							match	B3,str3
.							if equal
.								move	str7,OWNFAX
.								clear	LONGDIST
.							endif
.						endif
.					endif
..Testing
..					move	 "4154337796",OWNFAX
..					move	 C1,LONGDIST
..CNTNAME/CNTPHONE established via OrderRetrievePrinter
.					SPLOPEN	"c:\work\HDRFILE.prn"
.					print	"^[D",longdist,OWNFAX,"^[N",OWNOCPY:
.						"^[S",CNTNAME,B2,CNTPHONE," ^]"
.					SPLCLOSE
..
.					clear	taskname
.					Path	Exist,"c:\windows"
.					if over			.nt/2000
.						append	"!c:\winnt\system32\cmd.exe",taskname
.					elseif (osflag = c6)	.XP
.						append	"!c:\windows\system32\cmd.exe",taskname
.					else			.95/98
.						append	"!c:\command.com",taskname
.					endif
.					append	 " /c copy c:\work\hdrfile.prn /b + c:\work\faxfile.prn",taskname
.					append	" /b c:\work\lcrfax",taskname
.					append	OLRN,taskname
.					append	".prn /b",taskname
.					reset	taskname
.					execute	taskname
..
.					clear	taskname
.					Path	Exist,"c:\windows"
.					if over			.nt/2000
.						append	"!c:\winnt\system32\cmd.exe",taskname
.					elseif (osflag = c6)	.XP
.						append	"!c:\windows\system32\cmd.exe",taskname
.					else			.95/98
.						append	"!c:\command.com",taskname
.					endif
.					append	" /c copy c:\work\lcrfax",taskname
.					append	OLRN,taskname
.					append	".prn \\nts2\fax",taskname
.					reset	taskname
.					execute	taskname
.				endif
.			endif
.		endif
.	endif
..END PATCH 3.67 ADDED LOGIC
..END PATCH 3.76.5 RETIRED LOGIC
OUT
	return

OrderLCRPrintItRefresh
.START PATCH 3.75.2 ADDED LOGIC - THIS ROUTINE IS NOW DEFUNCT!!
	return
.END PATCH 3.75.2 ADDED LOGIC - THIS ROUTINE IS NOW DEFUNCT!!
	clear	MKEY
	pack	MKEY,OMLRNUM,OCOBN
	move	"O.LCRPrintIt-NMLRKEY",Location
	pack	KeyLocation,"Key: ",MKEY
	call	NMLRKEY
	move	OLON,NOWNFLD
	move	"O.LCRPrintIt-NOWNKEY",Location
	pack	KeyLocation,"Key: ",NOWNFLD
	call	NOWNKEY
	getitem	Nord001AComboOffer,0,N3
	getitem	Nord001AComboOffer,N3,OFDESC
.
	move	QTYMASK,QTYPRNT
	move	C0,N9
	move	OQTY,N9
.START PATCH 3.68.8 REPLACED LOGIC
.	if (OELCODE = "1" OR OELCODE = "3")
.		move	"    ALL",QTYPRNT
.	else
.		edit	N9,QTYPRNT
.	endif
.START PATCH 3.71 REPLACED LOGIC
.	if (OELCODE = "1" OR OELCODE = "3")
.		if (N9 > 0)
.			edit	N9,QTYPRNT
.			call	Trim using QTYPRNT
.			pack	QTYPRNT2,"ALL/ ",QTYPRNT," Please advise actual quantity."
.		else
.			move	"    ALL",QTYPRNT2
.		endif
.	else
.		edit	N9,QTYPRNT
.		move	QTYPRNT,QTYPRNT2
.	endif
	if (OELCODE = "1" OR OELCODE = "3")
		if (N9 > 0 & (OCO2CODE = "" | OCO2CODE = "  "))
			edit	N9,QTYPRNT
			call	Trim using QTYPRNT
			pack	QTYPRNT2,"ALL/ ",QTYPRNT," Please advise actual quantity."
		else
			move	"    ALL",QTYPRNT2
		endif
	else
		edit	N9,QTYPRNT
		move	QTYPRNT,QTYPRNT2
	endif
.END PATCH 3.71 REPLACED LOGIC
.END PATCH 3.68.8 REPLACED LOGIC
	move	OODTEM,N2
	load	str9 Using N2 From m01,m02,m03,m04,m05,m06,m07,m08,m09,m010,m011,m012
	move	OODTED,str2
	reset	str2,1
	setlptr	str2,1
	rep	"0 ",str2
	setlptr	str2
	clear	str25
	append	str9,str25
	append	B1,str25
	append	str2,str25
	append	COMMA,str25
	append	B1,str25
	append	OODTEC,str25
	append	OODTEY,str25
	reset	str25
.GET CALLER
.Dave changed this logic 12/17/1999 -ASH
.	 move	 OLNUM,NMDLFLD
.	 call	 NMDLKEY
.	 if over
.		 clear	 MDLCALL
.	 endif
.
	move	ODOWJ,MDLCALL
	if (InitFlag = YES)
		move	C0,NUSEFLD
		move	C1,NUSEPATH
		move	PORTN,NUSEFLD
		rep	zfill,NUSEFLD
		call	NUSEKEY
		if not over
			move	NUSEINIT,MDLCALL
		endif
		move	NO,InitFlag
	endif
................
.Rental	or Exchange
.
	clear	str18
.	 if (OTOCODE = "R")
	if (ORENT = "1")
		if (OELCODE = "2" OR OELCODE = "3")
			append	"RENT/EXC",str18
		else
			append	"RENTAL",str18
		endif
	else
		append	"EXCHANGE",str18
	endif
	reset	str18
.START PATCH 3.72 ADDED LOGIC
	packkey	NSEL2FLD,"1",OLRN
	move	"O.LCRPrintIt-NSEL2KEY",Location
	pack	KeyLocation,"Key: ",NSEL2FLD
	call	NSEL2KEY
	if over
		move	O2DES,NSEL2NAME
	endif
.END PATCH 3.72 ADDED LOGIC
.
. DETERMINE  REP NAME.
.
.START PATCH FOR NEW EMAIL ADDRESSES
.SVCREP	 pack	 str24,B10,B10,B10
.	 getitem Nord001bComboCaller,0,N2
.	 if (N2	> 1)
.		 getitem Nord001bComboCaller,N2,str45
.	 else
.		 getitem Nord001bComboContact,0,N2
.		 getitem Nord001bComboContact,N2,str45
.	 endif
.	 unpack	 str45,CNTNAME,str1,NCNTFLD
.	 move	 "SVCREP-NCNTKEY",Location
.	 pack	 KeyLocation,"Key: ",NCNTFLD
.	 move	 C1,NCNTPATH
.	 call	 NCNTKEY
.	 if not	over
.SVCREP2
.		 scan	 "BILLING",CNTNAME
.		 goto cntexit if equal
.		 move	 CNTNAME,str1
.cntloopy
.		 bump	 CNTNAME,1
.		 cmatch	 B1,CNTNAME
.		 goto	 cntloopy if not equal
.		 goto	 cntexit if eos
.		 bump	 CNTNAME,1
.		 move	 CNTNAME,str7
.		 call	 RemoveChar using str7,B1
.		 move	 str7,str6
.		 clear	 str24
.		 pack	 str24,str1,str6,"@NINCAL.COM"
.cntexit	 reset	 CNTNAME
.	 endif
SVCREP	pack	str55,B10,B10,B10,B10,B10,B10
	getitem	Nord001bComboCaller,0,N2
	if (N2 > 1)
		getitem	Nord001bComboCaller,N2,str45
	else
		getitem	Nord001bComboContact,0,N2
		getitem	Nord001bComboContact,N2,str45
	endif
	unpack	str45,CNTNAME,str1,NCNTFLD
	move	"SVCREP-NCNTKEY",Location
	pack	KeyLocation,"Key: ",NCNTFLD
	move	C1,NCNTPATH
	call	NCNTKEY
	if not over
SVCREP2
.START PATCH 3.62 REPLACED LOGIC
.		scan	"BILLING",CNTNAME
.		goto cntexit if	equal
.		call	RemoveChar using CNTNAME,B1
.		call	Trim using CNTNAME
.		if (CNTNAME = "")
.			goto cntexit
.		endif
.		pack	str55,cntname,"@NINCAL.COM"
.cntexit		reset	CNTNAME
.
		move	CNTNAME,str55
		scan	"BILLING",str55
		if not equal
			call	RemoveChar using str55,B1
			call	Trim using str55
			if (str55 <> "")
				pack	str55,str55,"@NINCAL.COM"
			else
				pack	str55,B10,B10,B10,B10,B10,B10
			endif
		else
			pack	str55,B10,B10,B10,B10,B10,B10
		endif
.END PATCH 3.62 REPLACED LOGIC
	endif
.END PATCH FOR NEW EMAIL ADDRESSES
	return

OrderPrintLCRSpecialInstructions
	call	TRIM using DESC002
	move	C0,howmany
	loop
		call	PARSITUP using line1,DESC002,C1
		print	DTCH10FX,line1
		add	C1,howmany
		until	(howmany >= 14)
	repeat
.Print XSTAT last
	call	TRIM using DESC001
	call	PARSITUP using line1,DESC001,C1
	print	DTCH10FX,line1
	call	PARSITUP using line1,DESC001,C1
	print	DTCH10FX,line1
	return
OrderWriteLRFile LRoutine DimPtr
	TRAP	IOMssg Giving Error if IO
	move	"Write-LRFILE",Location
	write	LRFILE,SEQ;DimPtr
	TRAPCLR	IO
	return

.START PATCH 3.67 ADDED LOGIC
.START PATCH 3.75.2 REPLACED LOGIC
.CleanUpLCRFaxFiles
CleanUpLCRFaxFiles Routine
.END PATCH 3.75.2 REPLACED LOGIC
	clear	taskname
	Path	Exist,"c:\windows"
	if over			.nt/2000
		append	"!c:\winnt\system32\cmd.exe",taskname
	elseif (osflag = c6)	.XP
		append	"!c:\windows\system32\cmd.exe",taskname
	else			.95/98
		append	"!c:\command.com",taskname
	endif
	append	" /c del c:\work\lcrfax*.pdf",taskname
	reset	taskname
	execute	taskname
.
	pack	str4,".pdf"
	scan	str4,taskname
	if equal
		movefptr taskname,result
		reset	taskname
		setlptr	taskname,result
		pack	taskname,taskname,"prn"
		execute	taskname
	endif
	return
.END PATCH 3.67 ADDED LOGIC
.START PATCH 3.76.8 ADDED LOGIC
OrderClearDMXFILE Routine DimPtr
	call	Trim using DimPtr
	if (DimPtr <> "")
		read    DMXFILE,DimPtr;;
		if not over
			filepi  1;DMXFILE
			delete  DMXFILE,DimPtr
		endif
	endif
	return
.END PATCH 3.76.8 ADDED LOGIC
*******************************************************************************
**************************  ERROR DISPLAY SECTION   ***************************
*******************************************************************************
USERNG	clear	taskname
	append	"I'm sorry I've	lost track of who you are.",taskname
	append	carr,taskname
	append	"Please	exit program and try again.",taskname
	append	carr,taskname
	append	"Don't forget to tell I.S.!",taskname
	reset	taskname
	alert	caution,taskname,result
	goto FileGo3
.	 stop

NOFILE
	create	ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
	activate EditTextBoxes(1)
	clear	LocMssg
	append	"File Not On-Line: ",LocMssg
	append	FMESG,LocMssg
	reset	LocMssg
	clear	ErrMssg
	append	"Error = ",ErrMssg
	append	ERROR,ErrMssg
	reset	ErrMssg
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"File Not On-Line: "
	setitem	ErrorMssgStat2,0,FMESG
	setitem	ErrorMssgStat5,0,ErrMssg
	setitem	ErrorMssgStat3,0,"Contact IS With Error!"
	setitem	ErrorMssgOK,0,"&Stop"
	loop
		setfocus EditTextBoxes(1)
		setprop	ErrorMssg,visible=1
		getitem	EditTextBoxes(1),0,str1
		until (str1 = "Q")
	repeat
	destroy	EditTextBoxes(1)
	move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
	move	LocMssg,SmtpTextMessage(1)   Array <Text message >
	move	ErrMssg,SmtpTextMessage(2)   Array <Text message >
	move	"Subroutine NOFILE",SmtpTextMessage(3)	 Array <Text message >
	move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	call	errmesg
	goto FileGo3
.	 stop
..
ISAMBAD
	clear	LocMssg
	append	"Verify	ISAM of	",LocMssg
	append	FMESG,LocMssg
	append	" File",LocMssg
	reset	LocMssg
	clear	ErrMssg
	append	"Error:	",ErrMssg
	append	Error,ErrMssg
	reset	ErrMssg
	clear	taskname
	scan	"WEXUP1",FMESG
	if equal
		append	"NXNGFLD1 = ",taskname
		append	NXNGFLD1,taskname
		append	"  NXNGFLD2 = ",taskname
		append	NXNGFLD2,taskname
	else
		reset	FMESG
		scan	"CANCUP",FMESG
		if equal
			append	"NXNGFLD1 = ",taskname
			append	NXNGFLD1,taskname
			append	"  NXNGFLD2 = ",taskname
			append	NXNGFLD2,taskname
		else
			reset	FMESG
			scan	"NINXNUM",FMESG
			if equal
				append	"ACKEY = ",taskname
				append	ACKEY,taskname
				append	"  ACCKEY = ",taskname
				append	ACCKEY,taskname
			else
				reset	FMESG
				scan	"NXNGAIM",FMESG
				if equal
					append	"ACKEY = ",taskname
					append	ACKEY,taskname
					append	"  ACCKEY = ",taskname
					append	ACCKEY,taskname
				else
					append	"NORDFLD = ",taskname
					append	NORDFLD,taskname
					append	"  OLRN	= ",taskname
					append	OLRN,taskname
				endif
			endif
		endif
	endif
	reset	taskname
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=1
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"File I/O Error!"
	setitem	ErrorMssgStat2,0,LocMssg
	setitem	ErrorMssgStat5,0,ErrMssg
	setitem	ErrorMssgStat3,0,taskname
	setitem	ErrorMssgStat4,0,"Contact IS With Error!"
	setprop	ErrorMssg,visible=1
	match	"EXCHANGE",FMESG
	goto KEYEXNG if	equal
	move	"This is an Error e-mail from Nordtest",SmtpSubject Subject
.   Set	the text message that is send with the attachments
	move	LocMssg,SmtpTextMessage(1)   Array <Text message >
	move	ErrMssg,SmtpTextMessage(2)   Array <Text message >
	move	taskname,SmtpTextMessage(3)   Array <Text message >
	move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	call	errmesg
	goto CONTACT
.
* **************** NINCAL EXCHANGE  ERROR MESSAGES ****************************
KEYEXNG
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=0
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"File I/O Error!"
	setitem	ErrorMssgStat5,0,"  ****MODIFICATION ABORTED!!!!!!"
	setitem	ErrorMssgStat3,0,"No Record Found in Exchange File"
	setprop	ErrorMssg,visible=1
	FILEPI	2;NORDFILE
	READ	NORDFILE,holdkey;;
	UPDATAB	NORDFILE;*1,"S"
	move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
	move	"No Record Found in Exchange File",SmtpTextMessage(1)	Array <Text message >
	move	ErrMssg,SmtpTextMessage(2)   Array <Text message >
	move	taskname,SmtpTextMessage(3)   Array <Text message >
	move	"3",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	call	errmesg
	GOTO	CONTACT
*******************************************************************************
DUPE
	create	ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
	activate EditTextBoxes(1)
	clear	ErrMssg
	append	"ERROR = ",ErrMssg
	append	ERROR,ErrMssg
	append	B2,ErrMssg
	append	"KEY = ",ErrMssg
	append	EXKEY,ErrMssg
	reset	ErrMssg
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"THIS EXCHANGE	IS A DUPE "
	setitem	ErrorMssgStat2,0,"Please Leave this Information	on"
	setitem	ErrorMssgStat5,0,"    Screen and Inform	I.S.!"
	setitem	ErrorMssgStat3,0,ErrMssg
	setitem	ErrorMssgOK,0,"O&K"
	loop
		setfocus EditTextBoxes(1)
		setprop	ErrorMssg,visible=1
		getitem	EditTextBoxes(1),0,str1
		until (str1 = "Q")
	repeat
	destroy	EditTextBoxes(1)
	move	"This is an Error e-mail from Nord0001",SmtpSubject Subject
.   Set	the text message that is send with the attachments
	move	"Exchange is a Duplicate!!",SmtpTextMessage(1)	 Array <Text message >
	move	ErrMssg,SmtpTextMessage(2)   Array <Text message >
	move	"2",SmtpTextIndexLast				    Index to last entry	in TextMessage array
	call	errmesg
	goto OrderWriteNINPRINT

.* ***************************************************************************
.*  EXIT AND FERROR SUBROUTINES
.* ****************************************************************************
CONTACT
	create	ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
	activate EditTextBoxes(1)
	clear	ErrMssg
	append	"ERROR = ",ErrMssg
	append	ERROR,ErrMssg
	reset	ErrMssg
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"File I/O Error!"
	setitem	ErrorMssgStat2,0,"Please Leave this Information	on"
	setitem	ErrorMssgStat5,0,"    Screen and Inform	I.S.!"
	setitem	ErrorMssgStat3,0,ErrMssg
	setitem	ErrorMssgOK,0,"&Stop"
	loop
		setfocus EditTextBoxes(1)
		setprop	ErrorMssg,visible=1
		getitem	EditTextBoxes(1),0,str1
		until (str1 = "Q")
	repeat
	destroy	EditTextBoxes(1)
	goto FileGo3
.	 stop
*******************************************************************************
INT
	trapclr	INT
	trap	INT giving ERROR if INT
	move	"This is a Error e-mail	from Nord0001",SmtpSubject Subject

.   Set	the text message that is send with the attachments

	move	"This is an error message",SmtpTextMessage(1)	.Array <Text message >
	move	error,SmtpTextMessage(2)			.Array <Text message >
	move	"Interupt Error",SmtpTextMessage(3)		.Array <Text message >
	move	"3",SmtpTextIndexLast				.Index to last entry in	TextMessage array
	call	  errmesg
.
	create	ErrorMssg;EditTextBoxes(1)=100:120:10:50,MaxChars=1,EditType=5,SelectAll=1,Style=1,Border=1,FGColor=white
	activate EditTextBoxes(1)
	clear	ErrMssg
	append	"ERROR = ",ErrMssg
	append	ERROR,ErrMssg
	reset	ErrMssg
	setprop	ErrorMssgStat1,visible=1
	setprop	ErrorMssgStat2,visible=1
	setprop	ErrorMssgStat3,visible=1
	setprop	ErrorMssgStat4,visible=0
	setprop	ErrorMssgStat5,visible=1
	setitem	ErrorMssgStat1,0,"* Interupt Error !! *"
	setitem	ErrorMssgStat2,0,"Please Leave this Information	on"
	setitem	ErrorMssgStat5,0,"    Screen and Inform	I.S.!"
	setitem	ErrorMssgStat3,0,ErrMssg
	setitem	ErrorMssgOK,0,"&Stop"
	loop
		setfocus EditTextBoxes(1)
		setprop	ErrorMssg,visible=1
		getitem	EditTextBoxes(1),0,str1
		until (str1 = "I")
	repeat
	destroy	EditTextBoxes(1)
	goto FileGo3
.	 stop

.Include IO file
	include	nordio.inc
	include	nord4io.inc
	include	nord5io.inc
.START PATCH 3.6 ADDED LOGIC
	include	nord6io.inc
.END PATCH 3.6 ADDED LOGIC
	include	nordpio.inc
.START PATCH 3.74 REPLACED LOGIC
.	include	nmlrio.inc
.	include	nmlr2io.inc
.	include	nbrkio.inc
.	include	nbrk2io.inc
	include	compio.inc
	include	cntio.inc
.END PATCH 3.74 REPLACED LOGIC
	include	nshpio.inc
	include	npndio.inc
	include	nrtnio.inc
	include	ninvio.inc
	include	nownio.inc
	include	nsmpio.inc
	include	nofrio.inc
	include	nonoio.inc
	include	ncrcio.inc
	include	searchio.inc	  .contains logic for search.plf
	include	ndatio.inc
	include	npasio.inc
	include	nxrfio.inc
	include	nxngio.inc
	include	gnxtio.inc
	include	nxchio.inc
	include	nspeio.inc
	include	nspe2io.inc
	include	nspiio.inc
	include	tinvio.inc
	include	npayio.inc
	include	nmdlio.inc
	include	nuseio.inc
	include	ncntio.inc
	include	hpio.inc
	include	ncmpio.inc
	include	nlolio.inc
	include	nlolio2.inc
	include	npkgio.inc
	include	statsio.inc
	include	statsio2.inc
	include	statnio.inc
	include	nprcio.inc
	include	nprc2io.inc
	include	nmrgio.inc
.START PATCH 11FEB2002 ADDED LOGIC
	include	nmldio.inc
.END PATCH 11FEB2002 ADDED LOGIC
.START PATCH 3.51 ADDED LOGIC
.START PATCH 3.78.4 REMOVED LOGIC
.	include	nfulio.inc
.END PATCH 3.78.4 REMOVED LOGIC
.END PATCH 3.51 ADDED LOGIC
.START PATCH 3.72 ADDED LOGIC
	include	nselio.inc
	include	nrefio.inc
	include	nsel2io.inc
	include	nsel3io.inc
	include	nmodio.inc
	include	naddio.inc
	include	narrio.inc
	include	ncatio.inc
	include	NSLTio.inc
	include	nsrcio.inc
.END PATCH 3.72 ADDED LOGIC
.START 3.72.1 PATCH 30SEP2003 ADDED LOGIC
	include	compnotesio.inc
.END PATCH 3.72.1  30SEP2003
.START PATCH 3.75 ADDED LOGIC
	include	nlol2io.inc
	include	ncmp2io.inc
.END PATCH 3.75 ADDED LOGIC
.START PATCH 3.76.9 ADDED LOGIC
	include	nord8io.inc
.END PATCH 3.76.9 ADDED LOGIC
	include	comlogic.inc
