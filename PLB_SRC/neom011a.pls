.
. PROGRAM    : NEOM011A
. DATE       : 01/03/05
. AUTHOR     : ANDREW HARKINS
. DESCRIPTION: CALLS COMPKEY FOR OLD MAILER/BROKER NUMBERS
.	       THIS PROGRAM IS TEMPORARY - WILL BE OBSOLETE WHEN NINSTE IS CONVERTED!!!!!
.
.	       USED BY:	NEOM0011
.			NEOM0036
.			NEOM0099
............................................................................
PC	Equ	0
	Include	common.inc
	include	cons.inc
	include	compdd.inc
	include	cntdd.inc

release	init	"1.2"	DLH 30Sep2008 Small Patch
.release	init	"1.1"	ASH 17AUG2005 Small Patch
.release	init	"1.0"	ASH 03JAN2005 Initial Release

DimPtr	dim	^
DimPtr1	dim	^
DimPtr2	dim	^

GetNewMlrBrk Routine DimPtr,DimPtr1,DimPtr2
.DimPtr  = Old Mailer Number
.DimPtr1 = Old Broker Number
.DimPtr2 = DTPFLD (Return value)
.
.begin patch 1.2
	Move	C3,CompLock
	Move	C3,CNCTLOCK
.end patch 1.2
.START PATCH 1.1 ADDED LOGIC
	call	Trim using DimPtr
	if (DimPtr = "")
.Force an OVER
		move	"OVERIT",DimPtr
	endif
.END PATCH 1.1 ADDED LOGIC
	pack	COMPFLD3,DimPtr
	move	"COMPKEY3",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	MOVE	COMPNUM,str6
.
.START PATCH 1.1 ADDED LOGIC
	call	Trim using DimPtr1
	if (DimPtr1 = "")
.Force an OVER
		move	"OVERIT",DimPtr1
	endif
.END PATCH 1.1 ADDED LOGIC
	pack	COMPFLD4,DimPtr1
	move	"COMPKEY2",Location
	pack	KeyLocation,"Key: ",COMPFLD4
	call	COMPKEY2
.
	packKey	DimPtr2 from COMPNUM,str6
	return

GetNewMlr Routine DimPtr,DimPtr1
.begin patch 1.2
	Move	C3,CompLock
	Move	C3,CNCTLOCK
.end patch 1.2
.DimPtr  = Old Mailer Number
.DimPtr1 = INDMNUM (Return value)
	pack	COMPFLD3,DimPtr
	move	"COMPKEY3-2",Location
	pack	KeyLocation,"Key: ",COMPFLD3
	call	COMPKEY3
	move	COMPNUM,DimPtr1
	return

	include	compio.inc
	include	cntio.inc
	include	comlogic.inc