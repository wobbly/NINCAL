	include	common.inc
	include	cons.inc
	include	statsdd.inc
	include	nprcdd.inc

release		init	"1.31"	ASH 25NOV02	More Added features
.					I had to add NumberFormat property setting to several fields in order to prevent
.					real numbers.  This was done in order to ensure that numbers were identical to
.					the Campaign spreadsheets, which has the same values for NumberFormat property.
.					If not, fractional numbers were occasionally throwing off individual values, and therefore, Totals.
.release		init	"1.3"	ASH 14NOV02	Added features
.release		init	"1.2"	ASH 29MAR02	ADDED NET RECEIVED TO CALCULATIONS
.release		init	"1.1"	ASH 04MAR02	ADDED LOGIC TO MAKE RETURN CALCULATIONS
.release	init	"1.0"	Initial Release	ASH 08AUG01
DimPtr	dim	^
DimPtr1	dim	^
DimPtr2	dim	^
FrmPtr  form    ^
FrmPtr1 form    ^
FrmPtr2 form    ^
FrmPtr3 form    ^
FrmPtr4 form    ^
FrmPtr5 form    ^
FrmPtr6 form    ^
FrmPtr7 form    ^
FrmPtr8 form    ^
FrmPtr9 form    ^
FrmPtr10 form   ^
FrmPtr11 form   ^
FrmPtr12 form   ^
FrmPtr13 form   ^
FrmPtr14 form   ^
FrmPtr15 form   ^
FrmPtr16 form   ^
FrmPtr17 form   ^
Frm62	form	6.2
Frm64	form	6.4
Frm94	form	9.4
.
.Following are all new variables with new logic
.
ExcelFlag form	"0"
taskname2 dim	500
N35	form	3.5
.
books   automation
book    automation
sheets  automation
sheet1  automation
ex      automation      class="Excel.Application"
.Variant objects used to talk to outside applications
.See PL/B help in order to understand use of Variant objects.
.
.Booleans
.PL/B does not have a Boolean datatype, so we have to create our own.
VT_BOOL EQU 11
OTRUE   variant
OFALSE  variant

CalcPrepExcel Routine
.Create the Variant objects
.Booleans
        create  OTRUE,VarType=VT_BOOL,VarValue=1
        create  OFALSE,VarType=VT_BOOL,VarValue=0
.Open Excel application
        create  ex
.START PATCH 1.3 ADDED LOGIC
	setprop ex,*IgnoreRemoteRequests="True",*Interactive="False"
.END PATCH 1.3 ADDED LOGIC
	setprop ex,*Visible=OFALSE
.Suppress any alert boxes produced by Excel.  We want to close down this instance of Excel now!!
        setprop ex,*DisplayAlerts=OFALSE
.Create Workbooks collection
        getprop ex,*Workbooks=books
.Create/Add a single Workbook
        books.add
        books.item giving book using 1
.START PATCH 1.31 ADDED LOGIC
	setprop	book,*PrecisionAsDisplayed="True"
.END PATCH 1.31 ADDED LOGIC
.Create Worksheets collection
        getprop book,*Sheets=sheets
.Create a single Worksheet - we did not need to add it as we set the default above to
.add one new Worksheet each time a Workbook is created.
        sheets.item giving sheet1 using 1
.
.....ESTABLISH DERIVED FIELDS.....
.
..NET NAMES - NWF
.        setprop sheet1.range("O1"),*Formula="=IF(B1>0,IF(F1>0,B1*F1,IF(E1>0,B1*E1,B1*G1)),IF(F1>0,A1*F1,IF(E1>0,A1*E1,A1*G1)))"
..NET NAMES - TNC
.        setprop sheet1.range("P1"),*Formula="=IF(B1=0,A1*G1,B1*G1)"
.RETURNS
        setprop sheet1.range("Q1"),*Formula="=ROUND(O1*H1,0)"
.REVENUE
.START PATCH 1.31 ADDED LOGIC
.        setprop sheet1.range("R1"),*Formula="=Q1*I1"
        setprop sheet1.range("R1"),*Formula="=Q1*I1",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.PROD COST
.START PATCH 1.31 ADDED LOGIC
.        setprop sheet1.range("S1"),*Formula="=(O1*K1/1000+Q1*L1*M1)"
        setprop sheet1.range("S1"),*Formula="=(O1*K1/1000+Q1*L1*M1)",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.LIST COST
.START PATCH 1.31 ADDED LOGIC
.        setprop sheet1.range("T1"),*Formula="=IF(O1<>0,O1/1000*D1,0)"
        setprop sheet1.range("T1"),*Formula="=IF(O1<>0,O1/1000*D1,0)",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.TOTAL COST
.START PATCH 1.31 ADDED LOGIC
.        setprop sheet1.range("U1"),*Formula="=SUM(S1:T1)"
        setprop sheet1.range("U1"),*Formula="=SUM(S1:T1)",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.NET +/-
.START PATCH 1.31 ADDED LOGIC
.        setprop sheet1.range("V1"),*Formula="=R1-U1"
        setprop sheet1.range("V1"),*Formula="=R1-U1",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.COST PER MEMBER
.        setprop sheet1.range("W1"),*Formula="=IF(O1>0,IF(V1<>0,V1/Q1,0),0)"
        setprop sheet1.range("W1"),*Formula="=IF(O1>0,IF(V1<>0,IF(Q1<>0,V1/Q1,0),0),0)"
.INC M
        setprop sheet1.range("X1"),*Formula="=IF(Q1>0,IF(R1<>0,R1/O1*1000,0),0)"
.COST/M
        setprop sheet1.range("Y1"),*Formula="=IF(O1>0,IF(U1<>0,U1/O1*1000,0),0)"
.        setprop sheet1.range("Y1"),*Formula="=IF(O1>0,IF(T1<>0,T1/(O1/1000),0),0)"
.COST?$
        setprop sheet1.range("Z1"),*Formula="=IF(R1>0,IF(U1<>0,U1/R1,0),0)"
.EXCHANGE TOTAL
        setprop sheet1.range("AK1"),*Formula="=IF(ISERROR(AF1/G1),0,AF1/G1)"
.RENT TOTAL
.NOTE:  This calculation uses Net Names for TNC.  Code will need modification if NWF wants to calculate a Rental Total.  AH.
.START PATCH 1.2 REPLACED LOGIC
.	pack	taskname2,"=IF(ISERROR(IF(A1>0,(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5)/G1+(AJ1/(O1/1000)),(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5)/G1+(AJ1/1000))),0,IF(A1>0,(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5)/G1+(AJ1/(O1/1000)),(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5)/G1+(AJ1/1000)))"
	pack	taskname2,"=IF(ISERROR(IF(A1>0,IF(F1>0,(((AG1*0.8)*F1)+((1-F1)*AI1)+AH1+5),(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5))/G1+(AJ1/(O1/1000)),IF(F1>0,(((AG1*0.8)*F1)+((1-F1)*AI1)+AH1+5),(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5))/G1+(AJ1/1000))),0,IF(A1>0,IF(F1>0,(((AG1*0.8)*F1)+((1-F1)*AI1)+AH1+5),(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5))/G1+(AJ1/(O1/1000)),IF(F1>0,(((AG1*0.8)*F1)+((1-F1)*AI1)+AH1+5),(((AG1*0.8)*E1)+((1-E1)*AI1)+AH1+5))/G1+(AJ1/1000)))"
.END PATCH 1.2 REPLACED LOGIC
        setprop sheet1.range("AL1"),*Formula=taskname2
.START PATCH 1.1 ADDED LOGIC
        setprop sheet1.range("C2"),*Formula="=if(ISERROR(A2/B2),0,A2/B2*100)"
.	setprop sheet1.range("E2"),*Formula="=if(A2<>0,D2/A2,0)"
	setprop sheet1.range("E2"),*Formula="=if(ISERROR(D2/A2),0,D2/A2)"
.END PATCH 1.1 ADDED LOGIC
	move	C1,ExcelFlag
	return

CalcCleanUpExcel Routine
.Clean up after myself
.All created automation objects MUST be destroyed.  If not ex.quit will fail and
.Excel.exe will still be running.
	destroy	OTRUE
	destroy OFALSE
        destroy sheet1
        destroy sheets
.START PATCH 1.31 ADDED LOGIC
	if (ExcelFlag = C1)
		setprop	book,*PrecisionAsDisplayed="False"
	endif
.END PATCH 1.31 ADDED LOGIC
        destroy book
        destroy books
	if (ExcelFlag = C1)
.START PATCH 1.3 ADDED LOGIC
		setprop ex,*IgnoreRemoteRequests="False",*Interactive="True"
.END PATCH 1.3 ADDED LOGIC
		ex.quit
	endif
        destroy ex
	move	C0,ExcelFlag
        return

CalcStatSetValues Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3,FrmPtr4,FrmPtr5,FrmPtr6,FrmPtr7,FrmPtr8,DimPtr,FrmPtr9,FrmPtr10,FrmPtr11,FrmPtr12,FrmPtr13,FrmPtr14,FrmPtr15,FrmPtr16,DimPtr1
	if (ExcelFlag <> C1)
		call	CalcPrepExcel
	endif
.RECO QTY
.START PATCH 1.31 ADDED LOGIC
.	setprop sheet1.range("A1"),*Value=FrmPtr
	setprop sheet1.range("A1"),*Value=FrmPtr,*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.RECEIVED QTY
.START PATCH 1.31 ADDED LOGIC
.      	setprop sheet1.range("B1"),*Value=FrmPtr1
       	setprop sheet1.range("B1"),*Value=FrmPtr1,*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.LIST COST
.START PATCH 1.31 ADDED LOGIC
.      	setprop sheet1.range("C1"),*Value=FrmPtr2
       	setprop sheet1.range("C1"),*Value=FrmPtr2,*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
.AVG ?/M
       	setprop sheet1.range("D1"),*Value=FrmPtr3
.NET REQUESTED
	mult	".01",FrmPtr4,N35
       	setprop sheet1.range("E1"),*Value=N35
.NET RECEIVED
	mult	".01",FrmPtr5,N35
       	setprop sheet1.range("F1"),*Value=N35
.AVERAGE NET
	mult	".01",FrmPtr6,N35
       	setprop sheet1.range("G1"),*Value=N35
.RESPONSE RATE
	mult	".01",FrmPtr7,N35
       	setprop sheet1.range("H1"),*Value=N35
.GIFT
       	setprop sheet1.range("I1"),*Value=FrmPtr8
.R/E
        setprop sheet1.range("J1"),*Value=DimPtr
.PACKAGE COST
       	setprop sheet1.range("K1"),*Value=FrmPtr9
.PREMIUM
       	setprop sheet1.range("L1"),*Value=FrmPtr10
.PREMIUM %
	mult	".01",FrmPtr11,N35
       	setprop sheet1.range("M1"),*Value=N35
.NET NAMES - NWF
	if (DimPtr1 = "3")
.START PATCH 1.31 ADDED LOGIC
.	        setprop sheet1.range("O1"),*Formula="=IF(B1>0,IF(F1>0,B1*F1,IF(E1>0,B1*E1,B1*G1)),IF(F1>0,A1*F1,IF(E1>0,A1*E1,A1*G1)))"
	        setprop sheet1.range("O1"),*Formula="=IF(B1>0,IF(F1>0,B1*F1,IF(E1>0,B1*E1,B1*G1)),IF(F1>0,A1*F1,IF(E1>0,A1*E1,A1*G1)))",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
	else
.NET NAMES - TNC
.START PATCH 1.31 ADDED LOGIC
.	        setprop sheet1.range("O1"),*Formula="=IF(B1=0,A1*G1,B1*G1)"
	        setprop sheet1.range("O1"),*Formula="=IF(B1=0,A1*G1,B1*G1)",*NumberFormat="#0_)"
.END PATCH 1.31 ADDED LOGIC
	endif
.EXCHANGE BASE
        setprop sheet1.range("AF1"),*Value=FrmPtr12
.RENT BASE
        setprop sheet1.range("AG1"),*Value=FrmPtr13
.SELECT FEE
        setprop sheet1.range("AH1"),*Value=FrmPtr14
.RUN CHARGE
        setprop sheet1.range("AI1"),*Value=FrmPtr15
.SHIP/TAX
        setprop sheet1.range("AJ1"),*Value=FrmPtr16
	return

CalcStatGetValues Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3,FrmPtr4,FrmPtr5,FrmPtr6,FrmPtr7,FrmPtr8,FrmPtr9,FrmPtr10,FrmPtr11,FrmPtr12
	if (ExcelFlag <> C1)
		call	CalcPrepExcel
	endif
.NET NAMES
        getprop sheet1.range("O1"),*Value=FrmPtr
.        getprop sheet1.range("P1"),*Value=FrmPtr1
.RETURNS
        getprop sheet1.range("Q1"),*Value=FrmPtr1
.REVENUE
        getprop sheet1.range("R1"),*Value=FrmPtr2
.PROD COST
        getprop sheet1.range("S1"),*Value=FrmPtr3
.LIST COST
        getprop sheet1.range("T1"),*Value=FrmPtr4
.TOTAL COST
        getprop sheet1.range("U1"),*Value=FrmPtr5
.NET +/-
        getprop sheet1.range("V1"),*Value=FrmPtr6
.COST PER MEMBER
        getprop sheet1.range("W1"),*Value=FrmPtr7
.INC M
        getprop sheet1.range("X1"),*Value=FrmPtr8
.COST/M
        getprop sheet1.range("Y1"),*Value=FrmPtr9
.COST?$
        getprop sheet1.range("Z1"),*Value=FrmPtr10
.EXCHANGE TOTAL
        getprop sheet1.range("AK1"),*Value=FrmPtr11
.RENT TOTAL
        getprop sheet1.range("AL1"),*Value=FrmPtr12
	return

.START PATCH 1.1 ADDED LOGIC
CalcStatSetReturnValues Routine FrmPtr,FrmPtr1,FrmPtr2
	if (ExcelFlag <> C1)
		call	CalcPrepExcel
	endif
.RETURNS
        setprop sheet1.range("A2"),*Value=FrmPtr
.MAIL QTY
        setprop sheet1.range("B2"),*Value=FrmPtr1
.REVENUE
        setprop sheet1.range("D2"),*Value=FrmPtr2
	return

CalcStatGetReturnValues Routine FrmPtr,FrmPtr1
	if (ExcelFlag <> C1)
		call	CalcPrepExcel
	endif
.RESPONSE RATE
        getprop sheet1.range("C2"),*Value=FrmPtr
.AVERAGE GIFT
        getprop sheet1.range("E2"),*Value=FrmPtr1
	return
.END PATCH 1.1 ADDED LOGIC

.Following is all old logic
CalcStatNetNames Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = Net Names
.FrmPtr  = Average Net
.FrmPtr1 = Received Qty
	move	C0,FrmPtr2
	calc	FrmPtr2 = (FrmPtr * .01) * FrmPtr1
	return

CalcStatNetNames2 Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3,FrmPtr4,FrmPtr5
.=IF(F18>0,IF(M18>0,F18*M18,IF(L18>0,F18*L18,F18*N18)),IF(M18>0,E18*M18,IF(L18>0,E18*L18,E18*N18)))
.=IF(FrmPtr>0,IF(FrmPtr1>0,FrmPtr*FrmPtr1,IF(FrmPtr2>0,FrmPtr*FrmPtr2,FrmPtr*FrmPtr3)),IF(FrmPtr1>0,FrmPtr4*FrmPtr1,IF(FrmPtr2>0,FrmPtr4*FrmPtr2,FrmPtr4*FrmPtr3)))
.FrmPtr5 = Net Names
.FrmPtr  = Qty Approved 	F
.FrmPtr1 = Net Approved		M
.FrmPtr2 = Net Requested	L
.FrmPtr3 = Average Net		N
.FrmPtr4 = Reco Qty	        E
	if (FrmPtr > 0)
		if (FrmPtr1 > 0)
			calc	FrmPtr5 = FrmPtr * (FrmPtr1 * .01)
		else
			if (FrmPtr2 > 0)
				calc	FrmPtr5 = FrmPtr * (FrmPtr2 * .01)
			else			           
				calc	FrmPtr5 = FrmPtr * (FrmPtr3 * .01)
			endif
		endif
	else
		if (FrmPtr1 > 0)
			calc	FrmPtr5 = FrmPtr4 * (FrmPtr1 * .01)
		else
			if (FrmPtr2 > 0)
				calc	FrmPtr5 = FrmPtr4 * (FrmPtr2 * .01)
			else
				calc	FrmPtr5 = FrmPtr4 * (FrmPtr3 * .01)
			endif
		endif
	endif
	return

CalcStatExTotal Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = ExTotal
.FrmPtr  = Average Net
.FrmPtr1 = Exchange Base
	move	C0,FrmPtr2
	calc	FrmPtr2 = FrmPtr1/(FrmPtr * .01)
	return

CalcStatRentTotal Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3,FrmPtr4,FrmPtr5,FrmPtr6,FrmPtr7,FrmPtr8
.FrmPtr8 = RentTotal
.FrmPtr  = Reco. Qty.
.FrmPtr1 = Rent Base
.FrmPtr2 = Net Requested
.FrmPtr3 = Running Charge
.FrmPtr4 = Select Fee
.FrmPtr5 = Average Net
.FrmPtr6 = Ship Tape
.FrmPtr7 = Net Names
	move	C0,FrmPtr8
	move	C0,Frm64
	move	FrmPtr6,Frm64
.	call	CalcStatNetNames using FrmPtr5,FrmPtr,N9
	if (FrmPtr > 0)
.		calc	FrmPtr8 = (((FrmPtr1 * 0.8) * FrmPtr2) + ((1 - FrmPtr2) * FrmPtr3) + FrmPtr4 + 5)/(FrmPtr5 * .01) + (FrmPtr6/(FrmPtr7/1000))
.		move	C0,Frm94
.		move	FrmPtr7,Frm94
		calc	FrmPtr8 = (((FrmPtr1 * 0.8) * (FrmPtr2 * .01)) + ((1 - (FrmPtr2 * .01)) * FrmPtr3) + FrmPtr4 + 5)/(FrmPtr5 * .01) + (Frm64/(FrmPtr7/1000))
	else
.		calc	FrmPtr8 = (((FrmPtr1 * 0.8) * FrmPtr2) + ((1 - FrmPtr2) * FrmPtr3) + FrmPtr4 + 5)/(FrmPtr5 * .01) + (FrmPtr6/1000)
		calc	FrmPtr8 = (((FrmPtr1 * 0.8) * (FrmPtr2 * .01)) + ((1 - (FrmPtr2 * .01)) * FrmPtr3) + FrmPtr4 + 5)/(FrmPtr5 * .01) + (FrmPtr6/1000)
	endif
	return

CalcListCost Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = List Cost
.FrmPtr  = Net Names
.FrmPtr1 = List Cost per M
	move	C0,FrmPtr2
	move	C0,Frm94
	div	"1000",FrmPtr,Frm94
	mult	FrmPtr1,Frm94
.	unpack	Frm94,str9,str1,str4
.	if (str4 > "4999")
.		add	C1,Frm94
.	endif
	move	Frm94,FrmPtr2
	return

CalcProdCost Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3,FrmPtr4,FrmPtr5
.FrmPtr5 = Production Cost
.FrmPtr  = Net Names
.FrmPtr1 = Package Cost
.FrmPtr2 = Returns
.FrmPtr3 = Premium
.FrmPtr4 = Premium %
	move	C0,FrmPtr5
	calc	FrmPtr5 = (FrmPtr * FrmPtr1/1000) + (FrmPtr2 * FrmPtr3 * FrmPtr4)
	return

CalcTotalCost Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = Total Cost
.FrmPtr  = Production Cost
.FrmPtr1 = List Cost
	move	C0,FrmPtr2
	calc	FrmPtr2 = FrmPtr + FrmPtr1
	return

CalcReturns Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = Returns
.FrmPtr  = Net Names
.FrmPtr1 = Response Rate
	move	C0,FrmPtr2
	move	C0,Frm64
	move	C0,Frm94
	calc	Frm64 = FrmPtr1 * .01
	calc	Frm94 = FrmPtr * Frm64
	move	Frm94,FrmPtr2
	return

CalcRevenue Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = Revenue
.FrmPtr  = Returns
.FrmPtr1 = Gifts
	move	C0,FrmPtr2
	calc	FrmPtr2 = FrmPtr * FrmPtr1
	return

CalcNetPlusMinus Routine FrmPtr,FrmPtr1,FrmPtr2
.FrmPtr2 = Net +-
.FrmPtr  = Revenue
.FrmPtr1 = Total Cost
	move	C0,FrmPtr2
	calc	FrmPtr2 = FrmPtr - FrmPtr1
	return

.CalcCostMember Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3,FrmPtr4,FrmPtr5,FrmPtr6,FrmPtr7,FrmPtr8,FrmPtr9,FrmPtrA,FrmPtrB,FrmPtrC,FrmPtrD,FrmPtrE,FrmPtrF,FrmPtrG,FrmPtrH,FrmPtrI,FrmPtrJ,FrmPtrK,FrmPtrL,FrmPtrM,FrmPtrN,FrmPtrO
..Returned Values:
..FrmPtrE = Cost per Member
..FrmPtrF = Net +-
..FrmPtrG = Revenue
..FrmPtrH = Returns
..FrmPtrI = Net Names
..FrmPtrJ = Total Cost
..FrmPtrK = Production Cost
..FrmPtrL = List Cost
..FrmPtrM = List Cost per M
..FrmPtrN = Exchange Total
..FrmPtrO = Rent Total
..Required Values:
..FrmPtr  = Received Qty
..FrmPtr1 = Average Net
..FrmPtr2 = Response Rate
..FrmPtr3 = Average Gift
..FrmPtr4 = Package Price
..FrmPtr5 = Package Premium
..FrmPtr6 = Package Premium Price %
..FrmPtr7 = Average List Cost per M
..FrmPtr8 = Exchange Base
..FrmPtr9 = Rent Base
..FrmPtrA = Net Requested	UNKNOWN IF STILL NEEDED
..FrmPtrB = Running Charge Fee
..FrmPtrC = Select Fee
..FrmPtrD = Ship/Tape Fee
..Initialize Return/Calculated Values
.        move    C0,FrmPtrE
.        move    C0,FrmPtrF
.        move    C0,FrmPtrG
.        move    C0,FrmPtrH
.        move    C0,FrmPtrI
.        move    C0,FrmPtrJ
.        move    C0,FrmPtrK
.        move    C0,FrmPtrL
.        move    C0,FrmPtrM
.        move    C0,FrmPtrN
.        move    C0,FrmPtrO
.        move    FrmPtrE,CALCSTATCOSTMEM
.        move    FrmPtrF,CALCSTATNETP
.        move    FrmPtrG,statrev
.        move    FrmPtrH,statresp
.        move    FrmPtrI,CALCSTATNETNAME
.        move    FrmPtrJ,CALCSTATTOTCOST
.        move    FrmPtrK,CALCSTATPROCOST
.        move    FrmPtrL,CALCSTATLSTCOST
.        move    FrmPtrM,statlpm
.        move    FrmPtrN,CALCSTATEXTOT
.        move    FrmPtrO,CALCSTATRTOT
..Initialize Static Values
.	move	FrmPtr,CALCSTATQTY
.	move	FrmPtr1,CALCSTATAVGNET
.	move	FrmPtr2,statresp2
.	move	FrmPtr3,statgift
.	move	FrmPtr4,NPRCTotal
.	move	FrmPtr5,NPRCPremium
.	move	FrmPtr6,N10		.Currently there is no field allocated for Premium Price %
.	move	FrmPtr7,statlcpm
.	move	FrmPtr8,statexbase
.	move	FrmPtr9,statrbase
.	move	FrmPtrA,CALCSTATNETREQ
.	move	FrmPtrB,statrun
.	move	FrmPtrC,statselfee
.	move	FrmPtrD,statship
..
.	call	CalcNetNames using CALCSTATAVGNET,CALCSTATQTY,CALCSTATNETNAME
.	if (CALCSTATNETNAME > 0)
.		call	CalcNetPlusMinus using statrev,CALCSTATTOTCOST,CALCSTATNETP
.	else
..Cost per Member = 0
.	endif
.	calc	FrmPtr2 = FrmPtr/FrmPtr1
.	return

CalcCostMember Routine FrmPtr,FrmPtr1,FrmPtr2,FrmPtr3
.FrmPtr3 = Cost per Member
.FrmPtr  = Net Names
.FrmPtr1 = Net +-
.FrmPtr2 = Returns
        move    C0,FrmPtr3
        move    C0,Frm94
	if (FrmPtr > 0)
		calc	Frm94 = FrmPtr1/FrmPtr2
		move	Frm94,FrmPtr3
	endif
	return

CalcStatQty Routine DimPtr,DimPtr1,DimPtr2,FrmPtr,FrmPtr1
.FrmPtr  = Total Quantity found
.DimPtr  = STATLR
.DimPtr1 = STATLOL
.FrmPtr1 = Flag for Quantity field
	move	C0,FrmPtr
	move	C1,STATPATH
	pack	STAT2FLD2,"01X",DimPtr
	pack	STAT2FLD3,"02X",DimPtr1
	move	"CalcLRQty-STAT2AIM",Location
	pack	KeyLocation,"Key: ",STAT2FLD2,STAT2FLD3
	call	STAT2AIM
	loop
		until over
		if (DimPtr2 <> STATNUM)
			if (FrmPtr1 = 1)
				add	STATMQTY,FrmPtr
			else
				add	STATRECQTY,FrmPtr
			endif
		endif
		move	"CalcLRQty-STAT2KG",Location
		call	STAT2KG
	repeat
	return

	include	statsio2.inc
	include	comlogic.inc
