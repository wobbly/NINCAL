PC       EQU       0
	INCLUDE	COMMON.INC
	INCLUDE	CONS.INC
	include	hp.inc
	include	compdd.inc
font1		font
Font4		font
font5		font
fontO8		font
fontO9I		font
fontO10		font
fontO10n	font
fontO10B	font
fontO12B	font
fontO14		font
FontO14B	font
FontO14BI	font
FontO18I	font
FontO7		font
FontO7dot5	font
FontO7dot5B	font
FontO7dot5I	font
FontO7dot5BI	font
FontO18B	font
FontO18BI	font
PRTPG24B	font
PRTPG24I	font
PRTPG10		font
font3   font
font6   font

.START PATCH 9.41 ADDED LOGIC
Font08I	font
Font08BI font
.END PATCH 9.41 ADDED LOGIC
.Create fonts to be used
sevenfive	form	"7.5"
Laser	pfile
externalmode	integer 1


	create	font1,"Times New Roman",size=14,bold
	create	fontO8,"Times New Roman",size=8
	create	font5,"Times New Roman",size=11
	create	fontO9I,"Times New Roman",size=9,Italic
	create	fontO10,"Times New Roman",size=10
	create	fontO10n,"Courier New",size=11
	create	fontO10B,"Times New Roman",size=10,Bold
	create	fontO12B,"Times New Roman",size=12,Bold
	create	fontO14,"Times New Roman",size=14
	create	fontO14B,"Times New Roman",size=14,Bold
	create	fontO14BI,"Times New Roman",size=14,Bold,Italic
	create	fontO18I,"Times New Roman",size=18,Italic
	create	fontO7,"Times New Roman",size=7
	create	fontO7dot5,"Times New Roman",size=sevenfive
	create	fontO7dot5I,"Times New Roman",size=sevenfive,Italic
	create	fontO7dot5b,"Times New Roman",size=sevenfive,Bold
	create	fontO7dot5bI,"Times New Roman",size=sevenfive,Bold,Italic
	create	fontO18B,"Times New Roman",size=18,Bold
	create	fontO18BI,"Times New Roman",size=18,Bold,Italic
.
	create	PRTpg24B,"Times New Roman",size=24,Bold
	create	PRTpg24I,"Times New Roman",size=24,Italic
	create	PRTpg10,"Times New Roman",size=10
.START PATCH 9.41 ADDED LOGIC
	create	font08I,"Times New Roman",size=8,Italic
	create	font08bI,"Times New Roman",size=8,Bold,Italic
	create	font3,"Helvetica",size=14,bold
	create	font6,"Arial",size=12
	
font7	font
font8	font
font9	font
NINLogo	PICT
	move	"750",column
	move	"1750",column1
	move	"3000",column2
	create	font7,"Helvetica",size=14,bold
	create	font8,"Helvetica",size=14,italic
	create	font9,"Arial",size=12
.START PATCH 9.38.5 REPLACED LOGIC
.	CREATE	NINLogo=3:13:30:50:
.		"\\nts0\c\netutils\NIN logo black outline.jpg"
	if (externalMode)
		CREATE  NINLogo=3:13:30:50,"..\images\NIN logo black outline.jpg"
	else
		CREATE	NINLogo=3:13:30:50:
			"\\nts0\c\netutils\NIN logo black outline.jpg"
	endif
	move	"P",compexcl
	Prtopen	Laser,"",""
	prtpage	Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon

	IF	(CompExcl = "P")
	prtpage	Laser;*p=93:25,*font=fontO18b,"Pacific Lists, Inc.":
		*p=500:343,*font=fontO7,"100 Tamal Plaza, Suite 50":
		*p=400:443,"Corte Madera, CA 94925-1182":
		*p=335:543,"415-945-9450 ","·"," Fax 415-945-9451":
		*p=335:643,"A Division of Names in the News"
	Else
	prtpage	Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
	endif
	move	"1500",row
	prtpage	Laser;*pcolumn7:row,*font=font3,"LIST CLEARANCES"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn7:row,*font=font4,"   VIA FACSIMILE"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,*font=font6,"Date:"
	prtpage	Laser;*pcolumn3:row,today
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,"To:"
	prtpage	Laser;*pcolumn3:row,"faxname"
		add	eightlpi,row
		add	eightlpi,row
		add	eightlpi,row
		prtpage	Laser;*pcolumn1:row,"Attn:"
		prtpage	Laser;*pcolumn3:row,"faxattn"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,"From:"
	prtpage	Laser;*pcolumn3:row,"Requests"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,"1"," Request(s) Enclosed"
		add	eightlpi,row
		add	eightlpi,row
		add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,"1"," Sample(s) Enclosed"
.	add	eightlpi,row
.	add	eightlpi,row
.	add	eightlpi,row
.	prtpage	Laser;*pcolumn1:row,ownpcnt," Total Pages (including cover)"
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
.START PATCH 2.41 ADDED LOGIC
	prtpage	Laser;*pcolumn1:row,"Please note the NEW fax number (510-302-4690) for all clearance responses." 
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,"We will continue to use 415-433-7796 as the general fax number."
.End PATCH 2.41 ADDED LOGIC
	add	eightlpi,row
	add	eightlpi,row
	add	eightlpi,row
	prtpage	Laser;*pcolumn1:row,"Please call if you do not receive all pages."
.	return
	prtpage	Laser;*NewPage

	prtclose	Laser
	stop
