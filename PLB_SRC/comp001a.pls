.COMPANY FILE MAINTENANCE SUBSIDIARY ROUTINE PROGRAM
........................................
. Program:	COMP001A.PLS
. Function:	Order Report Program (development)
. Author:	Andrew Harkins
. Orig.	Date:	October 15, 2002
. Release:	1.0
........................................

PC	EQU	1

	include	common.inc
	include	cons.inc
.	include	convcompdd.inc
.	include	mlrcntdd.inc
;patchconversion
.	include	mlrnbrkdd.inc
;Patch 1.2
;	include compdd.inc
;	include cntdd.inc
	include compdd.inc
	include cntdd.inc
;Patch 1.2
;patch conversion
Release	init	"1.3"		ASH 29Jul05  Added functionality to allow viewing of Associated Mailers for Consultants/Brokers
.Release	init	"1.2"		DMB 13APR05  Add code to check contact active byte
.Release	init	"1.1"		DMB 06APR04 Initial Release
;Release	init	"1.0"		ASH 15OCT02 Initial Release

DimPtr	dim	^
DimPtr2	dim	^
;patch1.1
DimPtr3	dim	^
;patch1.1
;Patch1.2
DimPtr4	dim	^
;Patch 1.2
.START PATCH 1.3 ADDED LOGIC
FrmPtr	form	^
DataPtr	Datalist ^
.END PATCH 1.3 ADDED LOGIC
mss1	plform	Error
	formload mss1

	shutdown

CompCompKey Routine DimPtr,DimPtr2
	pack	COMPFLD,DimPtr
	move	"COMPKEY",Location
	pack	KeyLocation,"Key: ",COMPFLD
	call	COMPKEY
	if not over
		pack	DimPtr2,COMPVARS
	else
		clear	DimPtr2
	endif
	return
;Patch1.01
CnctCnctKey Routine DimPtr,DimPtr2
	pack	CNCTFLD,DimPtr
	move	"CNCTKEY",Location
	pack	KeyLocation,"Key: ",CNCTFLD
	call	CNCTKEY
	if not over
		pack	DimPtr2,CNCTVARS
	else
		clear	DimPtr2
	endif
	return
;Patch1.01
;Patch1.1
CnctCnctNameKey Routine DimPtr,DimPtr2,DimPtr3   ;ptr3 is the company num
	pack cnctfld3 with "02L",DIMPTR
	call CNCTAIM
	if not over
		if	(DIMPTR3 = CNCTCODE)
			pack	DimPtr2,CNCTVARS
			return
		endif
		loop
         call CNCTKG
      until over
		until	(DIMPTR3 = CNCTCODE)
         repeat
			if	(DIMPTR3 = CNCTCODE)
				pack	DimPtr2,CNCTVARS
			else
				clear DIMPTR2
			endif
	else
		clear	DimPtr2
	endif
	return
;Endpatch1.1
;Patch1.2
CnctCnctActiveKey Routine DimPtr,DimPtr2,DimPtr3,DimPtr4      ;DimPtr=CNCTCODE ,dimptr3=cncttype
	pack	CNCTFLD2,"01X",DimPtr
	clear   CNCTFLD3
	call    CNCTAIM
	if not over
		if (DIMPTR3 = CNCTTYPE)         ;Mailer?
			IF ("F" = CNCTINACTIVE)      ;Active?
				if (CNCTID = DimPtr4) ;Testing against itself
					clear DimPtr2
					return
				else
					pack	DimPtr2,CNCTVARS
					return
				endif
			ENDIF
		endif
	 	loop
        		call CNCTKG
	        	until over
			if (DIMPTR3 = CNCTTYPE)
				IF ("F" = CNCTINACTIVE)
					if (CNCTID = DimPtr4)
						clear DimPtr2
						return
					else
						clear DimPtr2
						pack	DimPtr2,CNCTVARS
						return
					endif
				ENDIF
			endif
	         repeat
	         clear  DimPtr2
	else
 		 clear	DimPtr2
	endif
	return
;Endpatch1.2
;patchconversion
;CnctOldBrkKey Routine DimPtr,DimPtr2
;	pack	CNCTFLD,DimPtr
;	move	"CNCTKEY",Location
;	pack	KeyLocation,"Key: ",CNCTFLD
;	call	CNCTKEY
;	if not over
;		move	CNCTCNT,DimPtr2
;	else
;		clear	DimPtr2
;		pack	COMPFLD,DimPtr
;		move	"COMPKEY",Location
;		pack	KeyLocation,"Key: ",COMPFLD
;		call	COMPKEY
;		if not over
;			pack	DIMPTR2,COMPOLDBRK,z3
;		else
;			clear	DimPtr2
;		endif
;	endif
;	return

;OldtoNewBrk Routine DimPtr,DimPtr2,DimPtr3
;	pack	cnctfld4 from DIMPTR
;	CALL	cnctkey2
;	if not over
;;		pack	DimPtr2,CNCTCODE,CNCTID
;		if (CNCTTYPE = "5")   .consult
;				move c1 to DimPtr3
;		elseif (CNCTTYPE = "2")		   .broker
;;				move c2 to DimPtr3
;		else
;			clear DimPtr3
;		endif
;	else
;		pack	compfld4 from cnctfld4
;;		move	"COMPKEY2-READ",Location
;		pack	KeyLocation,"Key: ",compfld4
;;		call	compkey2
;		if not over
;			pack	DimPtr2,COMPNUM,z3
;			if (COMPCLRFLG = "1")	 .COnsult
;					move c1 to DimPtr3
;			elseif (COMPBRKFLG = "1")  .Broker
;					move c2 to DimPtr3
;			else
;				clear DimPtr3
;			endif
;		else
;			clear	DimPtr2
;		endif
;	endif
;	return
OldtoNewMlr Routine DimPtr,DimPtr2
		pack	compfld3 from DIMPTR
		move	"COMPKEY3-READ",Location
		pack	KeyLocation,"Key: ",compfld3
		call	compkey3
		if not over
			pack	DimPtr2,COMPNUM
		else
			clear	DimPtr2
		endif
	return
ComptoOldMlr Routine DimPtr,DimPtr2
		pack	compfld from DIMPTR
		move	"COMPKEY-READ",Location
		pack	KeyLocation,"Key: ",compfld3
		call	compkey
		if not over
			pack	DimPtr2,COMPOLDMLR
		else
			clear	DimPtr2
		endif
	return
;Begin Patch 1.2
OldBrktoNewComp Routine DimPtr,DimPtr2
			pack	compfld4 from DimPtr
			move	"COMPKEY2-READ",Location
			if (compfld4  = "    ")
				clear DimPtr2
				return
			endif
			call	compkey2
			if not over
				pack	DimPtr2,COMPVARS
			else
				clear	DimPtr2
			endif
	return
OldBrktoNewContact Routine DimPtr,DimPtr2
			pack	cnctfld4 from DimPtr
			move	"CNCTKEY2-READ",Location
			if (cnctfld4  = "    ")
				clear DimPtr2
				return
			endif
			call	CNCTKEY2
			if not over
				pack	DimPtr2,CNCTVARS
			else
				clear	DimPtr2
			endif
	return
;End Patch 1.2
.START PATCH 1.3 ADDED LOGIC
CompLoadAssocMailer Routine DimPtr,FrmPtr,DataPtr
.Loads Mailers associated with a Consultant/Broker
.DimPtr  = Company Number (Consultant/Broker)
.FrmPtr  = Flag to determine if looking for Mailers for a Consultant(1) OR a Broker(2)
.DataPtr = DataList object in which to load associated Mailers
	call	Trim using DimPtr
	if (DimPtr <> "")
		deleteitem DataPtr,0
		if (FrmPtr = C1)	.Consultant
			clear	COMPFLD12
			pack	COMPFLD11 from "01X",DimPtr
			pack	KeyLocation,"Key: ",COMPFLD11
		elseif (FrmPtr = C2)	.Broker
			clear	COMPFLD11
			pack	COMPFLD12,"02X",DimPtr
			pack	KeyLocation,"Key: ",COMPFLD12
		else
			return
		endif
		move	"LoadM-COMPAIM2",Location
		call	COMPAIM2
		loop
			until over
			pack	taskname,COMPNUM,DASH,COMPCOMP
			insertitem DataPtr,9999,taskname
			move	"LoadM-COMPKG2",Location
			call	COMPKG2
		repeat
	endif
	return
.END PATCH 1.3 ADDED LOGIC

.	include	nbrkio.inc
	include compio.inc
	include	cntio.inc
	include	comlogic.inc