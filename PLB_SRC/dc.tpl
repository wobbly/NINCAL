	GOTO	#Start
*.................................................................
.
.Customer File Definitions
.
CustNum	FORM	10	
CoName	DIM	40		
Address	DIM 	40		
City	DIM	30		
State	DIM	2
Zipcode	DIM	5
ContactFN	DIM	20
ContactLN	DIM	20
.
%PREFIX_Data VARLIST CustNum,CoName,Address,City:
		 State,Zipcode,ContactFN,ContactLN		
.
FieldCnt	CONST	"8"		
CustKey	DIM	10
CustKeyA	DIM	43(FieldCnt)
CustSrch	DIM	1(FieldCnt),("X"),("F"),("F"),("F"),("X"),("X"),("F"),("F")
.
%PREFIX_List FILELIST
CustFile	   IFILE		NAME="CUSTOMER.ISI"
CustFilA	   AFILE		NAME="CUSTOMER.AAM"
	   FILELISTEND
.	
%PREFIX_Text COLLECTION
%PREFIX_Cmd  COLLECTION
*................................................................
.
.Local Data
.
#YesNo	 INTEGER	4,"0x24"	// Alert type	
#Result	 INTEGER	4	// Univeral variable
#Searching INTEGER	4	// True = Search in progress
#Adding	 INTEGER	4	// True = Addition in progress
#String	 DIM	175	// size of data + 1 for each field	
#FieldNo	 FORM	2	// AAM field number
#RecordCnt FORM	5	// Record count
*................................................................
.
.Open the files
.
. If the file is empty, all controls except the
.  New and Close buttons are disabled.  Otherwise, the 
.  first record is displayed.
.
%PREFIX_Open  
*
..Build a collection of the EditText objects
. and a collection of the command buttons except New and Close.
.
	LISTINS	%PREFIX_Text,%FIELDS
.
	LISTINS	%PREFIX_Cmd,%PREFIX_CmdDelete:
		%PREFIX_CmdFirst,%PREFIX_CmdPrevious:
		%PREFIX_CmdNext,%PREFIX_CmdLast	
*
.Open the files
.	
	TRAP	%PREFIX_Prep IF IO
	OPEN	%PREFIX_List
	TRAPCLR	IO
*
.Attempt to read the first record
.
	CALL	%PREFIX_First
	CALL	%PREFIX_Count
.	
	RETURN
*....................................................
.
.Create the Files
.
%PREFIX_Prep
.
	NORETURN
.
	ALERT	TYPE=#YESNO,"The Customer file "::
		"does not exist - Create It ?":
		#Result,"Warning"
	STOP	IF (#Result = 7)
.
	%PREPARE
.		
	RETURN
*....................................................
.
.Close the form
.
%PREFIX_Close 
.
	IF	(#Searching)
	SETPROP	%PREFIX_Cmd,ENABLED=1
	CLEAR	%PREFIX_Data,#Searching
	SETPROP	%PREFIX_CmdNew,ENABLED=1
	SETPROP	ednCustNum,READONLY=1
	SETPROP	%PREFIX_CmdFind,TITLE="&Find"
	SETPROP	%PREFIX_CmdDelete,TITLE="&Delete"
	CALL	%PREFIX_Put
	CALL	%PREFIX_Read
	CALL	%PREFIX_Count
	ELSE
	CALL	%PREFIX_Delete IF (#Adding)
	SETPROP	%FORMNAME,VISIBLE=0
	ENDIF
.	
	RETURN	
*....................................................
.
.Read for a specified key value
.
%PREFIX_Read
.
	CALL	%PREFIX_First IF (CustNum = 0)
.
	MOVE	CustNum,CustKey
	TRAP	%PREFIX_Read1 IF IO
	READ	CustFile,CustKey;CUSTDATA
	TRAPCLR	IO
	RETURN	IF OVER
.
	CALL	%PREFIX_Put
.
%PREFIX_Read1
.
	RETURN
*....................................................
.
.Read the first record
.
%PREFIX_First
.
	IF	(#Searching)
	READ	CustFilA,CustKeyA;CUSTDATA	
	ELSE
	FILL	" ",CustKey
	READ	CustFile,CustKey;;
	READKS	CustFile;CUSTDATA
.
	IF	OVER
	SETPROP	%PREFIX_Text,ENABLED=0
	SETPROP	%PREFIX_Cmd,ENABLED=0
	RETURN
	ENDIF
	ENDIF
.
	CALL	%PREFIX_Put
	RETURN
*....................................................
.
.Read the previous record
.
%PREFIX_Previous
.
	IF	(%PREFIX_Searching)
	READKGP	CustFilA;CUSTDATA
	ELSE
	READKP	CustFile;CUSTDATA
	ENDIF
	IF	OVER
	ALERT	NOTE,"Beginning of file.",#Result,"Move Previous"
	GOTO	%PREFIX_First
	ENDIF 
.
	CALL	%PREFIX_Put
	RETURN	
*....................................................
.
.Read the next record
.
%PREFIX_Next
.
	IF	(%PREFIX_SEARCHING)
	READKG	CustFilA;CUSTDATA
	ELSE
	READKS	CustFile;CUSTDATA
	ENDIF
	IF	OVER
	ALERT	NOTE,"End of file.",#Result,"Move Next"
	GOTO	%PREFIX_Last
	ENDIF
.
	CALL	%PREFIX_Put
	RETURN
*....................................................
.
.Read the last record
.
%PREFIX_Last
.
	IF	(%PREFIX_SEARCHING)
	READLAST	CustFilA,CustKeyA;CUSTDATA	
	ELSE
	FILL	"9",CustKey
	READ	CustFile,CustKey;;
	READKP	CustFile;CUSTDATA
	RETURN	IF OVER
	ENDIF
.
	CALL	%PREFIX_Put
	RETURN		
*....................................................
.
.Delete the current record
.
%PREFIX_Delete
.
	IF	(#Adding)
	SETPROP	%PREFIX_Cmd,ENABLED=1
	CLEAR	%PREFIX_Data,#Adding
	SETPROP	%PREFIX_CmdNew,ENABLED=1
	SETPROP	ednCustNum,READONLY=1
	CALL	%PREFIX_Put
	CALL	%PREFIX_Read
.	
	ELSE
	RETURN	IF (CustNum = 0)
	DELETE	%PREFIX_List
	CALL	%PREFIX_Next
	CALL	%PREFIX_Previous IF OVER 
	CALL	%PREFIX_Count
	ENDIF
.	
	RETURN
*....................................................
.
.Add a new record
.
%PREFIX_New	
.
	SET	#Adding			// Indicate Adding
	SETPROP	%PREFIX_Text,ENABLED=1	// Enable EditTexts
	SETPROP	%PREFIX_ednCustNum,READONLY=0	// Allow Number Entry
	SETPROP	%PREFIX_Cmd,ENABLED=0	// Disable Buttons
	SETPROP	%PREFIX_CmdNew,ENABLED=0	// Disable New 	
	SETPROP	%PREFIX_CmdDelete,ENABLED=1 	// Enable Delete
	SETFOCUS	%PREFIX_ednCustNum 		// Position Cursor
	DELETEITEM %PREFIX_Text,0		// Clear Fields
.	
	RETURN
*....................................................
.
.Save the current record
.
%PREFIX_Save
.
	CALL	%PREFIX_GET
.
	IF	(#Adding)			
	WRITE	CUSTLIST;CUSTDATA
	CALL	%PREFIX_Count
	CLEAR	#Adding
	SETPROP	%PREFIX_Cmd,ENABLED=1
	SETPROP	%PREFIX_CmdNew,ENABLED=1
	SETPROP	ednCustNum,READONLY=1
	ELSE
	UPDATE	CUSTLIST;CUSTDATA
	ENDIF
.
	SETPROP	%PREFIX_CmdSave,ENABLED=0
	RETURN
*....................................................
.
.Update the count of records
.
%PREFIX_Count
.
	IF	(%PREFIX_SEARCHING)
	SETITEM	%PREFIX_lblCount,0,"Search Results"
	ELSE
	GETFILE	CustFile,RECORDCOUNT=#RecordCnt
.
	IF	(#RecordCnt = 0)
	MOVE	"No Records",#String
	ELSEIF	(#RecordCnt = 1)
	MOVE	"1 Record",#String
	ELSE
	MOVE	#RecordCnt,#String
	SQUEEZE	#String,#String
	PACK	#String WITH #String," Records"
	ENDIF
.
	SETITEM	%PREFIX_lblCount,0,#String	
.
	IF	(#RecordCnt > 1)
	SETPROP	%PREFIX_Cmd,ENABLED=$TRUE
	ENDIF
.	
	ENDIF
.	
	RETURN
*....................................................
.
.Transfer Record Data to the Form Objects
.
%PREFIX_Put
.
	IF	(CustNum > 0)
 	MOVE	CustNum,CustKey
 	ELSE
 	CLEAR	CustKey
 	ENDIF
.
	SETITEM	%PREFIX_ednCustNum,0,CustKey
	SETITEM	%PREFIX_edtCoName,0,CoName
	SETITEM	%PREFIX_edtAddress,0,Address
	SETITEM	%PREFIX_edtCity,0,City
	SETITEM	%PREFIX_edtState,0,State
	SETITEM	%PREFIX_edtZipcode,0,Zipcode
	SETITEM	%PREFIX_edtContactFN,0,ContactFN
	SETITEM	%PREFIX_edtContactLN,0,ContactLN
	RETURN
*....................................................
.
.Transfer record data from the form objects
.
DC_Get
.
	GETITEM	%PREFIX_ednCustNum,0,#String
	IF	ZERO
	CLEAR	#String
	ELSE	
	GETITEM	%PREFIX_ednCustNum,0,#String
	MOVE	CustKey,CustNum
	ENDIF
.
	GETITEM	%PREFIX_edtCoName,0,CoName
	GETITEM	%PREFIX_edtAddress,0,Address
	GETITEM	%PREFIX_edtCity,0,City
	GETITEM	%PREFIX_edtState,0,State
	GETITEM	%PREFIX_edtZipcode,0,Zipcode
	GETITEM	%PREFIX_edtContactFN,0,ContactFN
	GETITEM	%PREFIX_edtContactLN,0,ContactLN
.	
	RETURN
*....................................................
.
.Enable the Save button when the required fields are input
.
%PREFIX_Verify
.
	SETPROP	%PREFIX_CmdSave,ENABLED=$FALSE
	GETITEM	%PREFIX_ednCustNum,0,#String		// Number is required
	TYPE	#String
	RETURN 	IF EOS
	GETITEM	%PREFIX_edtCoName,0,#String0		// Name is required
	TYPE	#String
	RETURN	IF EOS
.
	SETPROP	%PREFIX_CmdSave,ENABLED=$TRUE
	RETURN
*....................................................
.
.Locate a record
.
%PREFIX_Find
.
	IF	(%PREFIX_Searching)
	CALL	%PREFIX_GET
	CLEAR	CustKeyA
	IMPLODE	#String,";",ednCustNum:
		edtCoName,edtAddress:
		edtCity,edtState:
		edtZipcode,edtContactFN:
		edtContactLN
	EXPLODE	#String,";",CustKeyA
.	
	FOR	#FieldNo,"1",FieldCnt
	COUNT	#Result,CustKeyA(#FieldNo)
	IF	NOT ZERO
	IF	(CustSrch(#FieldNo) = "F" AND #Result < 3)
	ALERT	CAUTION,"At least three characters required for search":
		#Result,"Find"
	RETURN
	ENDIF
	PACK	#String WITH #FieldNo,CustSrch(FIELDNO)
	REP	" 0",#String
	SPLICE	#String,CustKeyA(#FieldNo)
	ENDIF
	REPEAT
.	
	READ	CustFilA,CustKeyA;CUSTDATA
	IF	OVER
	ALERT	NOTE,"No matching records found",#Result,"Find"
	SETFOCUS	ednCustNum
	ENDIF
.	
	CALL	%PREFIX_PUT
	SETPROP	%PREFIX_CmdDelete,ENABLED=1
	SETPROP	%PREFIX_CmdFirst,ENABLED=1
	SETPROP	%PREFIX_CmdPrevious,ENABLED=1
	SETPROP	%PREFIX_CmdNext,ENABLED=1
	SETPROP	%PREFIX_CmdLast,ENABLED=1
	RETURN
	
	ELSE
	SET	%PREFIX_SEARCHING		// Indicate Adding
	SETPROP	%PREFIX_Cmd,ENABLED=0	// Disable Navigation
	SETPROP	%PREFIX_CmdNew,ENABLED=0	// Disable New
	SETPROP	%PREFIX_Text,ENABLED=1	 // Enable EditTexts
	SETPROP	%PREFIX_ednCustNum,READONLY=0	// Allow Number Entry
	SETPROP	%PREFIX_CmdClose,ENABLED=1: 	// Enable Delete
		Title="Cancel"		// Change Caption
	SETPROP	%PREFIX_CmdFind,Title="Search"// Change Caption
	SETFOCUS	%PREFIX_ednCustNum 		// Position Cursor
	DELETEITEM DC_Text,0		// Clear Fields
	ENDIF
.
	CALL	%PREFIX_Count
	RETURN
.
#Start	
