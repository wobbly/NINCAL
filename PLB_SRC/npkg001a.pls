	include	common.inc
	include	cons.inc
	include	npkgdd.inc

release	init	"1.0"	ASH	25MAR2002	INITIAL RELEASE - USED TO TEST FOR DUPLICATES OF NPKGID
DimPtr	dim	^
DimPtr1	dim	^
DimPtr2	dim	^
DimPtr3	dim	^
FrmPtr	form	^

PackageTestforDupe Routine DimPtr,DimPtr1,DimPtr2,DimPtr3,FrmPtr
.DimPtr  = NPKGMLR
.DimPtr1 = NPKGNUM
.DimPtr2 = NPKGID
.DimPtr3 = NewFlag value
.FrmPtr  = Return Value
	move	C0,FrmPtr	.Initialize Return Value
	move    C1,NPKGPATH
	clear	NPKGFLD2
	clear	NPKGFLD3
	pack	NPKGFLD1,"01X",DimPtr
	pack	NPKGFLD4,"04X",DimPtr2
	packkey	NPKGFLD4,NPKGFLD4
	move	"P.Test4Dupe-NPKGAIM",Location
	pack	KeyLocation,"Key: ",DimPtr,DimPtr2
	call	NPKGAIM
	if (DimPtr3 = YES)
		if not over
			move	C1,FrmPtr
		endif
	else
		loop
			until over
			if (NPKGNUM <> DimPtr1)
				move	C1,FrmPtr
				break
			endif
			move	"P.Test4Dupe-NPKGKG",Location
			call	NPKGKG
		repeat
	endif
	return

	include	npkgio.inc
	include	comlogic.inc
