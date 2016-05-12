//Program information
//Program Name:NSHP0002.PLS
//Program Description:This job will clean the shipping data for the various SB's that we receive shipping information from.
//Associated Winbatch Job:  N/A
//Programmer Name:David Baca
//Original Release Date:June 16, 2006



	Include Common.inc
	Include Cons.inc
	Include nftpdd.inc
	Include nftp2dd.inc
	Include nftplogdd.inc


PC EQU 0

Release Init "1.0" 16JUN2006	DMB	Program Initial Release
InShipFile   File
OutShipFile   File

dlFiles       datalist
FileString DIM 255
fileDir	   DIM 255
NDX        FORM 9

TEMPSLRNUM      DIM       6           LR NUMBER
TEMPSINFO       DIM       36          SHIPPING TEXT EXPLANATION
TEMPSCODE       DIM       1           HOW SHIP INFO WAS RECEIVED (C,P,S,I,R)
TEMPSDATE       DIM       8           SHIPMENT DATE     ccyymmdd
TEMPSPOST 	DIM       4           SHIPPING COST
TEMPSQUANT	DIM       9           SHIP QUANTITY
TEMPStrack      DIM       25          Tracking number
TEMPSINITS	DIM       3           Initials of person who created record
TEMPSRDATE	DIM       8           Date record was created
TEMPSPINITS	DIM       3           Initials of person who Printed record
TEMPSPDATE	DIM       8           Date record was Printed
TEMPSFILLER     DIM       25



Taskname1  DIM 100
Taskname2  DIM 100
Taskname3  DIM 100
Taskname4  DIM 510
Taskname5  DIM 510

	Open	NFTPFILE,NFTPNAME
	Open	OutShipFile,"\\nts1\e\data\ImportShipping.dat"

	Loop
		Read NFTPFILE,seq;NFTPVARS
		Pack "\\nts1\e\STORAGE\IMPORT\",NFTPCOMP
		Create	dlFiles=1:10:1:10,visible=0
		Pack	taskname,"\\Nts1\E\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING\",NFTP2WILDCARD
		Pack	fileDir,"\\Nts1\E\STORAGE\EXPORT\",NFTPCOMP,"\","SHIPPING\"								
		Pack	Taskname,NFTP2LocalDir,"\",NFTP2WILDCARD
		Pack	fileDir,NFTP2LocalDir,"\"								

                dlFiles.Dir giving result using *Filespec=Taskname,*flags=0
		If ( result <> -1 )
			For NDX from 0 to result
				dlFiles.GetText giving FileString using *Index=NDX
//   Set the text message that is sent with the attachments
				Pack taskname5,fileDir,taskname											
				Open InShipFile,Taskname5
								
				If (NFTPCOMP = "000001")
					Move	"A" to tempscode
				Elseif(NFTPCOMP = "000002")
					Move	"T" to tempscode				
				Elseif(NFTPCOMP = "000003")				
					Move	"T" to tempscode				. Must Create a PIDI Shipping Code				
				Elseif(NFTPCOMP = "000004")

				Else
				Endif
				
				Read InShipFile,
				
				
				Pack taskname,"SENT_",FileString
				Pack taskname4,fileDir,FileString
				Trap ErrorRename giving str50 if IO
				Rename taskname4,taskname5
				Trapclr IO
		Endif
		Destroy dlFiles		
		
		
		
	Until Over
	
	Repeat
	
	Include nftpio.inc
	Include nftp2io.inc
	Include nftplogio.inc
	Include Comlogic.inc	