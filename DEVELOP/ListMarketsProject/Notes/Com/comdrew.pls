PhoneFile	Afile
Name1		Dim	20
Phone1		Dim	20
Key		Dim	23

		Trap	DoPrep If IO
		Open 	PhoneFile, "TestPhone"
		Trapclr	IO
	EventReg	*Client,1,DoRead,ARG1=Name1
	EventReg	*Client,2,DoExit
	Loop
	EventWait
	Repeat
	Stop

DoPrep
	Prep	PhoneFile, "TestPhone.txt", "TestPhone.aam":
		"1-20","40",Share
	Write	PhoneFile;"Sam Smith           ","555-1234"
	Write	PhoneFile;"Fred Jones          ", "555-9988"
	Write	PhoneFile;"Tom Smith           ","555-1334"
		Return

DoRead
	Pack	Key, "01F", Name1
	Read	PhoneFile,Key;Name1,Phone1
	Loop
	  Break		If Over
 	  EventSend	*Client, 1, ARG1=Name1, ARG2=Phone1
	   ReadKG	PhoneFile;Name1,Phone1
	Repeat
	EventSend 	*Client, 2
	Return


DoExit
		Close	PhoneFile
	Stop
