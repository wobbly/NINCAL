	include	common.inc
	include	cons.inc
	include compdd.inc
	include cntdd.inc
Release	init	"1.1"		DMB 06APR04 Initial Release
;Release	init	"1.0"		ASH 15OCT02 Initial Release

DimPtr	dim	^
DimPtr2	dim	^
;patch1.1
DimPtr3	dim	^
;patch1.1
mss1	plform	Error
	formload mss1

CnctOldBrkKey Routine DimPtr,DimPtr2
	pack	CNCTFLD,DimPtr
	move	"CNCTKEY",Location
	pack	KeyLocation,"Key: ",CNCTFLD
	call	CNCTKEY
	if not over
		move	CNCTCNT,DimPtr2
	else
		clear	DimPtr2
		pack	COMPFLD,DimPtr
		move	"COMPKEY",Location
		pack	KeyLocation,"Key: ",COMPFLD
		call	COMPKEY
		if not over
			pack	DIMPTR2,COMPOLDBRK,z3
		else
			clear	DimPtr2
		endif
	endif
	return	
	include compio.inc
	include	cntio.inc
	include	comlogic.inc