.INCOME Run with tnc
PC EQU 0
	INCLUDE	common.inc
	INCLUDE	cons.inc
	INCLUDE	NDATDD.INC
;Compute 
	INCLUDE	NSHPDD.INC
	INCLUDE	NMRGDD.INC
        INCLUDE CONSACCT.inc
        Include nacddd.inc
        INCLUDE NOWNDD.INC
        INCLUDE NDAT3DD.INC
;patch1.7
.         include   nmlrdd.inc
	include	compdd.inc
	include	cntdd.inc
;patch1.7
;Compute

	INCLUDE	NORDDD.INC
	INCLUDE	NINVDD.INC
	INCLUDE	NINVACDDD.INC
	INCLUDE	NJSTDD.INC
	INCLUDE	NADJDD.INC
.>Patch 2.1 
	include nescdd.inc
	include	NUSEDD.INC	
        include winapi.inc	
.>Patch 2.1	
MONTHINCREPORT	PFILE	
.>Patch 2.1
HoldLstName	Dim 55
.>Patch 2.1
PRTFILENAME	DIM	50
PRINTNAME	DIM	50
.Patch1.2
INCLISTS	FILE	
LNUM    DIM     6                 
DATEBY  DIM     1                
LTYPE   DIM	1                
LMONTH  FORM    2                
REP1    DIM     1                
REP2    DIM     1                
REP3    DIM     1                
LYEAR   FORM    4                
RECIPIENT       DIM     255   
COMMENTS                DIM     255

	         
LVARS	VARLIST LNUM:
	DATEBY:  
	LTYPE:
	LMONTH:  	
	REP1:    
	REP2:    
	REP3:    
	LYEAR:   
	RECIPIENT:
	COMMENTS
ManualFlag	DIM	1
.patch1.2

Arial8    font
        create  Arial8,"Arial",size=8
Arial9    font
        create  Arial9,"Arial",size=9
Arial10    font
        create  Arial10,"Arial",size=10
Arial11    font
        create  Arial11,"Arial",size=11
Arial12    font
        create  Arial12,"Arial",size=12
Arial14    font
        create  Arial14,"Arial",size=14
Arial16    font
        create  Arial16,"Arial",size=16
TimesNew6	font
	create  TimesNew6,"Times New Roman",size=7
TimesNew10	font
	create  TimesNew10,"Times New Roman",size=10
TimesNew10I	font
	create  TimesNew10I,"Times New Roman",size=10,Italic
Header1	form	9
Header2	form	9
Header3	form	9
Header4	form	9
Header5	form	9
Header6	form	9
Header7	form	9
Header9	form	9
Header10	form	9
Header11	form	9
Header12	form	9
Header13	form	9
Header14	form	9
Header15	form	9
Header16	form	9
Header17	form	9
Header18	form	9
Header19	form	9
Header20	form	9
Release		INIT	"2.3"	 DMB	05JAN2006       Added code to show date and name of person who runs a manual income report
.Release		INIT	"2.2"	 DMB	01JAN2006       Added code for new budget numbers
.Release		INIT	"2.1"	 DMB	04OCT2005       Added code to aid in solving pdf failures
.Release	INIT	"2.0"	 DMB	28JAN2005	Rewrote nloinc0003 for new format
.Release	INIT	"1.8.1"	 DMB	10OCT2004	Added code to loop through ajustments until the 9th adj is read even if it doesn't exist.
.Release	INIT	"1.8"	 ASH	06AUG2004	Logo Conversion
.Release	INIT	"1.7"	 DMB 26MAY2004	Mailer COnversion
.Release	INIT	"1.6"	 01MAR2004	DMB	Added code select correct year for fiscal printouts
;Release	INIT	"1.5"	 07JAN2003	DMB	Added code to put list number in the filename
.Release	INIT	"1.4"	 30NOV2003	DMB	Added code to copy file from c:\work to e:\data\income
;Release	INIT	"1.2"	 11NOV2003	DMB	Added code to not calc variance between projections and actuals if actual is 0
.Release	INIT	"1.1"	 now shows open payables
.Release	INIT	"1.0"	Initial Release	of BO Monthy income report for clients
SingleSpaced	FORM	"160"
OneandahalfSpaced	FORM	"240"
DoubleSpaced	FORM	"320"
LgBoxHeight	FORM	"6951"
;LgBoxHeight	FORM	"6451"
;LgBoxHeight	FORM	"7491"
SmBoxHeight	FORM	"410"
;SmBoxHeight	FORM	"490"
;HalfsmboxHeight FORM	"245"
HalfsmboxHeight FORM	"205"
;MonthTextSmall 	FORM	"480"
TotBoxHeight	Form	9
totBoxRow	FORM	9
BegRowLine	FORM	9
;BoxOneLeft	FORM	"0"
BoxOneLeft	FORM	"1150"
;BoxOneRight	FORM	"500"
BoxOneRight	FORM	"1550"
;BoxOneRight	FORM	"600"
BoxTwoLeft	FORM	"1600"
;BoxTwoRight	FORM	"2750"
BoxTwoRight	FORM	"3900"
;BoxTwoLeft1	FORM	"1700"
BoxTwoLeft1	FORM	"2750"
;BoxTwoRight1	FORM	"2750"
BoxTwoRight1	FORM	"3900"
;BoxThreeLeft	FORM	"2800"
BoxThreeLeft	FORM	"3950"
;BoxThreeRight	FORM	"6400"
BoxThreeLeft1	FORM	"5100"
BoxThreeRight1	FORM	"6200"
.Patch 2.0 
BoxThreeRight	FORM	"6250"
.BoxThreeRight	FORM	"4180"
.Patch 2.0 
BoxFourLeft	FORM	"6750"
;BoxFourLeft	FORM	"6600"
;BoxFourRight	FORM	"7200"
BoxFourRight	FORM	"7150"
;BoxFiveLeft	FORM	"7250"
;BoxFiveLeft	FORM	"7150"
BoxFiveLeft	FORM	"7200"
;BoxFiveRight	FORM	"9500"
;BoxFiveRight	FORM	"9400"
BoxFiveRight	FORM	"8350"
;BoxFiveLeft1	FORM	"8375"
;BoxFiveLeft1	FORM	"8275"
BoxFiveLeft1	FORM	"7250"
;BoxFiveRight1	FORM	"9500"
;BoxFiveRight1	FORM	"9400"
BoxFiveRight1	FORM	"8300"
;BoxSixLeft	FORM	"9550"
;BoxSixLeft	FORM	"9450"
BoxSixLeft	FORM	"8400"
;BoxSixRight	FORM	"10500"
.Patch 2.0 Modification of Variable for decreased size - removing cells
.BoxSixRight	FORM	"10500"
BoxSixRight	FORM	"9550"
.Patch 2.0
;List 
;BoxTwoVert	FORM	"1700"
BoxTwoVert	FORM	"2750"
BoxThreeVert	FORM	"5100"
;BoxThreeVert1	FORM	"4000"
BoxThreeVert1	FORM	"4260"
;BoxThreeVert2	FORM	"5200"
BoxThreeVert2	FORM	"5180"
;BoxFiveVert	FORM	"8275"
BoxFiveVert	FORM	"7770"
;BoxFiveVert	FORM	"8375"
BoxSixVert1	FORM	"6540"
BoxSixVert2	FORM	"10580"


;For Text Align close to but not on Vertical Line
;BoxTwoVertText		FORM	"1690"
BoxTwoVertText		FORM	"3850"
;BoxThreeVert1Text	FORM	"3990"
.Patch 2.0 Code Modification Moving Below
BoxThreeVert1Text	FORM	"4250"
;BoxThreeVert2Text	FORM	"5150"
BoxThreeVert2Text	FORM	"5170"
.Patch 2.0 Code Modification
;BoxFiveVertText		FORM	"8265"
BoxFiveVertText		FORM	"7760"
.Patch 2.0 Modification of Variable for decreased size - removing cells
BoxSixVert1Text		FORM	""
BoxSixVert2Text		FORM	"9540"
.Patch 2.0

;BoxTwoRightText	FORM	"2740"
BoxTwoRightText	FORM	"3280"
;BoxThreeRightText	FORM	"6390"
BoxThreeRightText	FORM	"6090"
;BoxFiveRightText	FORM	"9390"
BoxFiveRightText	FORM	"8680"
;BoxSixRightText	FORM	"10390"
BoxSixRightText	FORM	"11490"
LtGrey	color
	create ltgrey=220:220:220
;Report Description
	move "5250" to Header1
;Description of Fiscal Year 1
	move	"3400" to Header2
;Description of Fiscal Year 2
	move	"9000" to Header3
;Description of Fiscal Year 3
	move	"2600" to Header4
;Description of Fiscal Year 4
	move	"4500" to Header5
;Description of Fiscal Year 1
	move	"6500" to Header6
;Description of Fiscal Year 2
	move	"9000" to Header7
;Cell Header equations
;Header9
	sub BoxTwoleft,BoxTworight,n9
	div c4 in n9,n8
	add BoxTwoLeft,n8,Header9
;Header10
	sub BoxTwoleft,BoxTworight,n9
	div c4 in n9,n8
	mult c3,n8
	add BoxTwoLeft,n8,Header10
.Patch 2.0 Moving code below
;Header11
	sub BoxThreeleft,BoxThreeRight,n9
	div c3 in n9,n8
	div c2,n8
	add BoxThreeLeft,n8,Header11
;Header12
	sub BoxThreeleft,BoxThreeRight,n9
	div c3 in n9,n8
	div c2,n8
	mult	c3,n8
	add BoxThreeLeft,n8,Header12
;Header13
	sub BoxThreeleft,BoxThreeRight,n9
	div c3 in n9,n8
	div c2,n8
	mult	c5,n8
	add BoxThreeLeft,n8,Header13
;Header14
	sub BoxFiveleft,BoxFiveright,n9
	div c4 in n9,n8
	add BoxFiveLeft,n8,Header14
;Header15
	sub BoxFiveleft,BoxFiveright,n9
	div c4 in n9,n8
	mult c3,n8
	add BoxFiveLeft,n8,Header15
;Header16
;	sub BoxSixleft,BoxSixright,n9
;	div c2 in n9,n8
;	add BoxSixLeft,n8,Header16
;Header16
.Patch 2.0 Comment Out
	sub BoxSixleft,BoxSixRight,n9
	div c2 in n9,n8
	add BoxSixLeft,n8,Header16
.	sub BoxSixleft,BoxSixRight,n9
.	div c3 in n9,n8
.	div c2,n8
.	add BoxSixLeft,n8,Header16
.Patch 2.0 Comment OUt
;Header17
	sub BoxSixleft,BoxSixRight,n9
	div c3 in n9,n8
	div c2,n8
	mult	c3,n8
	add BoxSixLeft,n8,Header17
;Header18
	sub BoxSixleft,BoxSixRight,n9
	div c3 in n9,n8
	div c2,n8
	mult	c5,n8
	add BoxSixLeft,n8,Header18
.Patch 2.0 
	sub BoxThreeRight,BoxTwoLeft,n9
	div c2 in n9,n8
	add BoxThreeRight,n8,Header19
.Patch 2.0
;Data Vars
BegFiscCur	FORM	5
EndFiscCur	FORM	5
BegFiscPrev	FORM	5
EndFiscPrev	FORM	5
JAN1		FORM	15
JAN2		FORM	15
FEB1		FORM	15
FEB2		FORM	15
MAR1		FORM	15
MAR2		FORM	15
APR1		FORM	15
APR2		FORM	15
MAY1		FORM	15
MAY2		FORM	15
JUN1		FORM	15
JUN2		FORM	15
JUL1		FORM	15
JUL2		FORM	15
AUG1		FORM	15
AUG2		FORM	15
SEP1		FORM	15
SEP2		FORM	15
OCT1		FORM	15
OCT2		FORM	15
NOV1		FORM	15
NOV2		FORM	15
DEC1		FORM	15
DEC2		FORM	15
OrdTOT1		FORM	15
OrdTOT2		FORM	15
JANRENT1	FORM	15
JANEXCH1	FORM	15
JANRENT2	FORM	15
JANEXCH2	FORM	15
FEBRENT1	FORM	15
FEBEXCH1	FORM	15
FEBRENT2	FORM	15
FEBEXCH2	FORM	15
MARRENT1	FORM	15
MAREXCH1	FORM	15
MARRENT2	FORM	15
MAREXCH2	FORM	15
APRRENT1	FORM	15
APREXCH1	FORM	15
APRRENT2	FORM	15
APREXCH2	FORM	15
MAYRENT1	FORM	15
MAYEXCH1	FORM	15
MAYRENT2	FORM	15
MAYEXCH2	FORM	15
JUNRENT2	FORM	15
JUNEXCH2	FORM	15
JUNRENT1	FORM	15
JUNEXCH1	FORM	15
JULRENT2	FORM	15
JULEXCH2	FORM	15
JULRENT1	FORM	15
JULEXCH1	FORM	15
AUGRENT2	FORM	15
AUGEXCH2	FORM	15
AUGRENT1	FORM	15
AUGEXCH1	FORM	15
SEPRENT2	FORM	15
SEPEXCH2	FORM	15
SEPRENT1	FORM	15
SEPEXCH1	FORM	15
OCTRENT2	FORM	15
OCTEXCH2	FORM	15
OCTRENT1	FORM	15
OCTEXCH1	FORM	15
NOVRENT2	FORM	15
NOVEXCH2	FORM	15
NOVRENT1	FORM	15
NOVEXCH1	FORM	15
DECRENT1	FORM	15
DECEXCH1	FORM	15
DECRENT2	FORM	15
DECEXCH2	FORM	15
EXCHTOT1	FORM	15
RENTTOT1	FORM	15
EXCHTOT2	FORM	15
RENTTOT2	FORM	15
;AP1
AP1JAN1	FORM	10.2
AP1JAN2	FORM	10.2
AP1FEB1	FORM	10.2
AP1FEB2	FORM	10.2
AP1MAR1	FORM	10.2
AP1MAR2	FORM	10.2
AP1APR1	FORM	10.2
AP1APR2	FORM	10.2
AP1MAY1	FORM	10.2
AP1MAY2	FORM	10.2
AP1JUN1	FORM	10.2
AP1JUN2	FORM	10.2
AP1JUL1	FORM	10.2
AP1JUL2	FORM	10.2
AP1AUG1	FORM	10.2
AP1AUG2	FORM	10.2
AP1SEP1	FORM	10.2
AP1SEP2	FORM	10.2
AP1OCT1	FORM	10.2
AP1OCT2	FORM	10.2
AP1NOV1	FORM	10.2
AP1NOV2	FORM	10.2
AP1DEC1	FORM	10.2
AP1DEC2	FORM	10.2
AP1TOT1	FORM	10.2
AP1TOT2	FORM	10.2

;Proj
ProjJAN1	FORM	10.2
ProjJAN2	FORM	10.2
ProjFEB1	FORM	10.2
ProjFEB2	FORM	10.2
ProjMAR1	FORM	10.2
ProjMAR2	FORM	10.2
ProjAPR1	FORM	10.2
ProjAPR2	FORM	10.2
ProjMAY1	FORM	10.2
ProjMAY2	FORM	10.2
ProjJUN1	FORM	10.2
ProjJUN2	FORM	10.2
ProjJUL1	FORM	10.2
ProjJUL2	FORM	10.2
ProjAUG1	FORM	10.2
ProjAUG2	FORM	10.2
ProjSEP1	FORM	10.2
ProjSEP2	FORM	10.2
ProjOCT1	FORM	10.2
ProjOCT2	FORM	10.2
ProjNOV1	FORM	10.2
ProjNOV2	FORM	10.2
ProjDEC1	FORM	10.2
ProjDEC2	FORM	10.2
ProjTOT1	FORM	10.2
ProjTOT2	FORM	10.2

.Patch 2.0
ProjectVARS	VARLIST ProjJAN1:
 ProjJAN2:
 ProjFEB1:
 ProjFEB2:
 ProjMAR1:
 ProjMAR2:
 ProjAPR1:
 ProjAPR2:
 ProjMAY1:
 ProjMAY2:
 ProjJUN1:
 ProjJUN2:
 ProjJUL1:
 ProjJUL2:
 ProjAUG1:
 ProjAUG2:
 ProjSEP1:
 ProjSEP2:
 ProjOCT1:
 ProjOCT2:
 ProjNOV1:
 ProjNOV2:
 ProjDEC1:
 ProjDEC2:
 ProjTOT1:
 ProjTOT2
.Patch 2.0




;Var
VarJAN1	FORM	10.2
VarJAN2	FORM	10.2
VarFEB1	FORM	10.2
VarFEB2	FORM	10.2
VarMAR1	FORM	10.2
VarMAR2	FORM	10.2
VarAPR1	FORM	10.2
VarAPR2	FORM	10.2
VarMAY1	FORM	10.2
VarMAY2	FORM	10.2
VarJUN1	FORM	10.2
VarJUN2	FORM	10.2
VarJUL1	FORM	10.2
VarJUL2	FORM	10.2
VarAUG1	FORM	10.2
VarAUG2	FORM	10.2
VarSEP1	FORM	10.2
VarSEP2	FORM	10.2
VarOCT1	FORM	10.2
VarOCT2	FORM	10.2
VarNOV1	FORM	10.2
VarNOV2	FORM	10.2
VarDEC1	FORM	10.2
VarDEC2	FORM	10.2
VarTOT1	FORM	10.2
VarTOT2	FORM	10.2

VarianceVARS VARLIST ProjJAN1:
 VarJAN1:
 VarJAN2:
 VarFEB1:
 VarFEB2:
 VarMAR1:
 VarMAR2:
 VarAPR1:
 VarAPR2:
 VarMAY1:
 VarMAY2:
 VarJUN1:
 VarJUN2:
 VarJUL1:
 VarJUL2:
 VarAUG1:
 VarAUG2:
 VarSEP1:
 VarSEP2:
 VarOCT1:
 VarOCT2:
 VarNOV1:
 VarNOV2:
 VarDEC1:
 VarDEC2:
 VarTOT1:
 VarTOT2


;For compute
mrgsw    dim       1
shipsw   dim       1
;compute
.patch1.3
TODAYIS	FORM	5
.patch1.3
.Patch 2.0 Flag to see if variance column is include
VarFlag	DIM	1
.Patch 2.0
.	move	"22350",ProjJAN1
.	move	"27111",ProjJAN2
.	move	"19447",ProjFEB1
.	move	"7961",ProjFEB2
.	move	"14786",ProjMAR1
.	move	"47289",ProjMAR2
.	move	"21619",ProjAPR1
.	move	"14604",ProjAPR2
.	move	"84894",ProjMAY1
.	move	"67277",ProjMAY2
.	move	"89529",ProjJUN1
.	move	"75534",ProjJUN2
.	move	"89726",ProjJUL1
.	move	"110000",ProjJUL2
.	move	"80575",ProjAUG1
.	move	"80000",ProjAUG2
.	move	"52395",ProjSEP1
.	move	"20000",ProjSEP2
.	move	"62830",ProjOCT1
.	move	"61000",ProjOCT2
.	move	"63345",ProjNOV1
.	move	"61500",ProjNOV2
.	move	"58504",ProjDEC1
.	move	"56800",ProjDEC2
.Patch 2.0 Move
.	move	"18860",ProjJAN1
.	move	"13490",ProjFEB1
.	move	"29678",ProjMAR1
.	move	"67450",ProjAPR1
.	move	"32376",ProjMAY1
.	move	"102524",ProjJUN1
.	move	"107452",ProjJUL1
.	move	"101246",ProjAUG1
.	move	"66030",ProjSEP1
.	move	"52824",ProjOCT1
.	move	"52824",ProjNOV1
.	move	"48220",ProjDEC1
.	add ProjJAN1,ProjTOT1	
.	add ProjFEB1,ProjTOT1
.	add ProjMAR1,ProjTOT1
.	add ProjAPR1,ProjTOT1
.	add ProjMAY1,ProjTOT1
.	add ProjJUN1,ProjTOT1
.	add ProjJUL1,ProjTOT1
.	add ProjAUG1,ProjTOT1
.	add ProjSEP1,ProjTOT1
.	add ProjOCT1,ProjTOT1
.	add ProjNOV1,ProjTOT1
.	add ProjDEC1,ProjTOT1
.patch 2.0
.	add ProjJAN2,ProjTOT2	
.	add ProjFEB2,ProjTOT2
.	add ProjMAR2,ProjTOT2
.	add ProjAPR2,ProjTOT2
.	add ProjMAY2,ProjTOT2
.	add ProjJUN2,ProjTOT2
.	add ProjJUL2,ProjTOT2
.	add ProjAUG2,ProjTOT2
.	add ProjSEP2,ProjTOT2
.	add ProjOCT2,ProjTOT2
.	add ProjNOV2,ProjTOT2
.	add ProjDEC2,ProjTOT2


.Patch 2.0
FORCEDAY	DIM 4
.Patch 2.0

;
DUMQTY	FORM	10
TMPVAR	FORM	10
RQTY	FORM	10
EXQTY	FORM	10
NUM102	FORM	10.2
AP1TOT	FORM	10.2
NUM1021	FORM	10.2
NUM1022	FORM	10.2
FISCMONTH FORM	2
.>Patch 2.3 Var Added to keep track of Date Ran
DateRan dim	10
.>Patch 2.3 Var Added to keep track of Date Ran
;
;Maksing Vars
mask20      init    "ZZZ,ZZZ,ZZZ,ZZZ,ZZ9"         ;formatting vars
Dim20a      dim     20	    ;formatting vars
mask16      init    "Z,ZZZ,ZZZ,ZZZ.99"        ;formatting vars
Dim16a      dim     16	    ;formatting vars
mask18      init    "(Z,ZZZ,ZZZ,ZZZ.99)"        ;formatting vars
Dim18a      dim     18	    ;formatting vars
.patch1.1
mask19      init    "($ZZZ,ZZZ,ZZZ.99)"        ;formatting vars
Dim19a      dim     19	    ;formatting vars
OPENPAY	FORM	10.2
.patch1.1
HOLDAP1	FORM	10.2
.patch1.3
.START PATCH 1.8 ADDED LOGIC
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
.END PATCH 1.8 ADDED LOGIC
	clock timestamp,timestamp
	unpack timestamp,str2,yy,mm,dd
	call	cvtjul
	move juldays  to TODAYIS
.patch1.3
.>Patch 2.3 Code Added
	pack DATERAN with mm,slash,dd,slash,str2,yy
.>Patch 2.3 Code Ended 
.patch1.4
.patch1.5
.	pack	PRINTNAME,timestamp
.endpatch1.4

.patch1.1

.patch1.4commentout
.	move "standinc.lst",PRINTNAME
;	move	"c:\work\standinc.lst" to prtfilename
.	pack	prtfilename with "c:\work\",printname,".lst"
.patch1.4commentout
.	move "IncomeVariance.lst",PRINTNAME
.	move	"c:\work\incomevariance.lst" to prtfilename
.patch1.4
.patch1.2
.	PRTOPEN MONTHINCREPORT,"Acrobat Distiller",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
.	PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
.                          *ORIENT=*LANDSCAPE;
	OPEN	INCLISTS,"INCLISTS"
;Patch1.5

	call	paint
.patch1.3
DATEIS
	move	"Y" to str1
.        PACK      str10 FROM mm,SLASH,dd,SLASH,yy	 
.Patch 2.0
        PACK      str12 FROM mm,SLASH,dd,SLASH,str2,yy	 
		  PACK	   FORCEDAY,str2,YY
.Patch 2.0
        KEYIN     *P10:6,*DV,str12," OK? ",*t20,str1
        CMATCH    "N" TO str1
        GOTO      WhichWay IF NOT EQUAL
        KEYIN     *P10:6,*+,mm,"/",dd,"/",FORCEDAY
		  unpack	forceday,str2,yy
        PACK      str12 FROM mm,SLASH,dd,SLASH,str2,yy
	call	cvtjul
	move juldays  to TODAYIS
	goto DateIS
.patch1.3
WhichWay
        MOVE      "A" to str1
        KEYIN     *ES,*P10:8,*EF,"M",*white,"anual or ",*cyan,"(A)uto: ",*T15,*RV,STR1
	if (str1 = "M")
		move YES to ManualFlag
		move "V" to REP1

NewList
	   KEYIN     *P10:10,*EF,*white,"List:  ",LNUM
		GOTO	NEWLIST IF EOS
		if (LNUM = "")
			goto NewList
		else
.patch1.5
			call zfillit using LNUM
.endpatch1.5
			move lnum to NDATFLD
			REP	" 0" IN NDATFLD
			move c1 to ndatpath
			CALL NDATKEY
			if over
				clear LNUM
				Goto NEWLIST
			endif
		endif
.patch1.5
	call	OpenPrintFile
.patch1.5
	DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
         MOVE      "O" to str1
GetDate
         KEYIN     *P10:14,*EF,"Maildate Or ",*cyan,"(O)","rder date: ",*RV,DATEBY
		goto	getdate if ((DATEBY <> "M") and (DATEBY <> "O"))
Basis	
         KEYIN     *P10:14,*EF,*white,"Accrual (By ",*CYAN,"[I]",*white,"nvoice) Or ","Cash (",*cyan,"[C]",*White,"heck Date) Basis: ",*RV,LTYPE
		GOTO      BASIS IF EOS
		goto	BASIS if ((LTYPE <> "I") and (LTYPE <> "C"))
.Patch 2.0 Adding option of Variance
VarianceQues	
      KEYIN     *P10:14,*EF,*white,*CYAN,"[V]",*white,"ariance?",*RV,VARFLAG
		GOTO      VarianceQues IF EOS
		goto	VarianceQues if ((VARFLAG <> "Y") and (VARFLAG <> "N"))
.Patch 2.0
NewYear	
		KEYIN     *P1:1,*P10:16,*JR,"ENTER STARTING 4 - DIGIT YEAR FOR TWO YEAR ANALYSIS ",*RV,str4
		GOTO      NEWYear IF EOS
		TYPE      str4
		GOTO      NewYear IF NOT EQUAL
		COUNT	n2,str4
		move	str4 to lyear
		goto	NEWYEAR if (n2 <> c4)

NewMonth	
		move	"01" to LMONTH
		KEYIN     *P1:1,*P10:18,*JR,"ENTER STARTING FISCAL YEAR'S STARTING MONTH  ",*RV,str2
		GOTO      NEWMONTH IF EOS
		TYPE      STR2
		GOTO      NewMonth IF NOT EQUAL
		REP       " 0" IN STR2
		move	str2 to LMONTH
		GOTO	MANUALLIST
	elseif (str1 <> "A")
		Goto WhichWay
	endif



READITa
	READ	INCLISTS,SEQ;LVARS
	GOTO endprogram IF OVER
	GOTO READITa IF (REP1 <> "V")
.Patch 2.0
	move rep2 to varflag
.Patch 2.0
   DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
.>Patch 2.1
	move olstname to  HoldLstName
.>Patch 2.1
.patch1.5
.Patch 2.0 Move
.  	call	OpenPrintFile
.Patch 2.0 Move
.patch1.5
READIT
.Patch 2.0
  	call	OpenPrintFile
.Patch 2.0 
	reset timestamp
	unpack timestamp,str4,n2
	move str4 to LYEAR

.For Fiscal
MANUALLIST
		move LMONTH to fiscmonth
		MOVE LMONTH TO MM
		MOVE	"01" TO DD
.PATCH1.6
	Move ForceDay to N4
.		if (Lmonth <> C1)
.			move LYEAR to n4
.			sub c1 from n4
		move n4 to LYEAR
.		endif
		unpack LYEAR,str2,YY
.PATCH1.6
;Year 1
		call	cvtjul
.Patch 2.0 replacement for code about
;If this is a fiscal year report and todayis[report date] is less than latest fiscal date than do not add add another year to it
;i.e. if the fiscal month is jul 04 and the report date is mar 04 then we must sub a year for jul 03 - jun 04 FY
;i.e. if the fiscal month is jul 04 and the report date is aug 04 then we leave alone to create jul 04 - jun 05 FY
		if (Lmonth <> C1)
			if (todayis < juldays)
				sub c1 from n4
				move n4,str4
				unpack str4 to str2,YY
				rep zfill,YY
				call	cvtjul
			endif
		endif
.Patch 2.0
		move juldays to BegFiscCur
		sub	c1 from juldays
		call	cvtgreg
		CLEAR 	N2
		MOVE	yy TO N2
		pack str4 with CC,YY
		move str4 to n4
		add c1,n4
		unpack n4,str2,n2
		MOVE N2 TO YY
		CALL CVTJUL
		move juldays to EndFiscCur 
Year2
;Beg
		move begfiscCur to JULDAYS
		call cvtgreg
		pack str4 with CC,YY
		move str4 to n4
		sub c1,n4
		unpack n4,str2,n2
		move n2 to YY
		rep zfill,YY
		call cvtjul
		move juldays to BegFiscPrev
;End
		move endfiscCur to JULDAYS
		call cvtgreg
		pack str4 with CC,YY
		move str4 to n4
		sub c1,n4
		unpack n4,str2,n2
		move n2 to YY
		rep zfill,YY
		call cvtjul
		move juldays,EndFiscPrev
.PATCH1.2

	move lnum to NDATFLD
	move c1 to ndatpath
	CALL NDATKEY
.	STOP IF OVER
	Goto Readit1 if over
        DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
.Patch1.2
.Patch 2.0 Move
	If (lstnum = "012594")
.income numbers updated per sherene 11/1/05	
..income numbers updated per sherene 1/1/06
..		move	"86000",ProjJAN1
		move	"100000",ProjJAN1
..		move	"67000",ProjFEB1
		move	"75000",ProjFEB1
..		move	"82000",ProjMAR1
		move	"35000",ProjMAR1
..		move	"52000",ProjAPR1
		move	"55000",ProjAPR1
.		move	"73000",ProjMAY1
..		move	"70000",ProjMAY1
		move	"65000",ProjMAY1
.		move	"17000",ProjJUN1
..		move	"28000",ProjJUN1
		move	"40000",ProjJUN1
..		move	"29000",ProjJUL1
		move	"25000",ProjJUL1
.		move	"52000",ProjAUG1
..		move	"16000",ProjAUG1
		move	"55000",ProjAUG1
.		move	"46000",ProjSEP1	
..		move	"20000",ProjSEP1	
		move	"50000",ProjSEP1	
.		move	"53000",ProjOCT1
..		move	"45000",ProjOCT1
		move	"45000",ProjOCT1
.		move	"51000",ProjNOV1
..		move	"35000",ProjNOV1
		move	"70000",ProjNOV1
.		move	"87000",ProjDEC1
..		move	"110000",ProjDEC1
		move	"80000",ProjDEC1
		add ProjJAN1,ProjTOT1	
		add ProjFEB1,ProjTOT1
		add ProjMAR1,ProjTOT1
		add ProjAPR1,ProjTOT1
		add ProjMAY1,ProjTOT1
		add ProjJUN1,ProjTOT1
		add ProjJUL1,ProjTOT1
		add ProjAUG1,ProjTOT1
		add ProjSEP1,ProjTOT1
		add ProjOCT1,ProjTOT1
		add ProjNOV1,ProjTOT1
		add ProjDEC1,ProjTOT1
	elseif (lstnum = "002303")
.		move	"18886",ProjJAN1
		move	"19000",ProjJAN1
.		move	"13490",ProjFEB1
		move	"49000",ProjFEB1
.		move	"29678",ProjMAR1
		move	"29000",ProjMAR1
.		move	"67450",ProjAPR1
		move	"31000",ProjAPR1
.		move	"32376",ProjMAY1
		move	"80000",ProjMAY1
.		move	"102524",ProjJUN1
		move	"112000",ProjJUN1
.		move	"107452",ProjJUL1
		move	"111000",ProjJUL1
.		move	"101246",ProjAUG1
		move	"36000",ProjAUG1
.		move	"66030",ProjSEP1
		move	"42000",ProjSEP1
.		move	"52824",ProjOCT1
		move	"29000",ProjOCT1
.		move	"52824",ProjNOV1
		move	"46000",ProjNOV1
.		move	"48220",ProjDEC1
		move	"36000",ProjDEC1
		add ProjJAN1,ProjTOT1	
		add ProjFEB1,ProjTOT1
		add ProjMAR1,ProjTOT1
		add ProjAPR1,ProjTOT1
		add ProjMAY1,ProjTOT1
		add ProjJUN1,ProjTOT1
		add ProjJUL1,ProjTOT1
		add ProjAUG1,ProjTOT1
		add ProjSEP1,ProjTOT1
		add ProjOCT1,ProjTOT1
		add ProjNOV1,ProjTOT1
		add ProjDEC1,ProjTOT1
 endif
.patch 2.0





IncHeader
	clear row
	prtpage	MONTHINCREPORT;*units=*HIENGLISH,*Pictrect=*off,*PICT=0:800:0:5000:NINLogo
	add "500" to row
	call	trim using olstname
	prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial14,*ll,*boldon,OLSTNAME,*boldoff;
	add	OneandahalfSpaced to row
	prtpage MONTHINCREPORT;*pHeader1:row,*ALIGNMENT=*CENTER,*font=Arial12,*ll,"MONTHLY LIST INCOME/VOLUME REPORT - REPORTED ON A CASH BASIS (BY CHECK DATE)";
	add	Doublespaced to row
;Header2
	move BegFiscPrev to juldays 
	call	cvtgreg
	pack	str10 with mm,"/",DD,"/",YY	
	move EndFiscPrev to juldays 
	call	cvtgreg
	pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
		prtpage MONTHINCREPORT;*pBoxFiveLeft:row,*ALIGNMENT=*LEFT,*font=Arial11,*ll,*boldon,TASKNAME,*boldoff;
;Header3
	if (Varflag = "N")
		move "5180" to BoxThreeRight
;Header11
		sub BoxThreeleft,BoxThreeright,n9
		div c4 in n9,n8
		add BoxThreeLeft,n8,Header11
;Header12
		sub BoxThreeleft,BoxThreeright,n9
		div c4 in n9,n8
		mult c3,n8
		add BoxThreeLeft,n8,Header12
	else 
		move "6100" to BoxThreeRight
		move	"6090" to BoxThreeRightText
;Header11
		sub BoxThreeleft,BoxThreeRight,n9
		div c3 in n9,n8
		div c2,n8
		add BoxThreeLeft,n8,Header11

;Header12
		sub BoxThreeleft,BoxThreeRight,n9
		div c3 in n9,n8
		div c2,n8
		mult	c3,n8
		add BoxThreeLeft,n8,Header12
;Header13
		sub BoxThreeleft,BoxThreeRight,n9
		div c3 in n9,n8
		div c2,n8
		mult	c5,n8
		add BoxThreeLeft,n8,Header13
	endif
	move BegFiscCur to juldays 
	call	cvtgreg
	pack	str10 with mm,"/",DD,"/",YY	
	move EndFiscCur to juldays 
	call	cvtgreg
	pack taskname with "FISCAL YEAR ",CC,YY," (",str10," - ",MM,"/",DD,"/",YY,")"
	prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*ALIGNMENT=*left,*font=Arial11,*ll,*boldon,taskname,*boldoff;
	add	Doublespaced to row
;Header4
	prtpage MONTHINCREPORT;*pBoxTwoVert:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Volume",*boldoff;
;Header5
.Patch 2.0 
	if (Varflag = "N")
		prtpage MONTHINCREPORT;*pBoxThreeVert1:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Income",*boldoff;
	else
		prtpage MONTHINCREPORT;*pHeader11:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Income",*boldoff;
	endif
.Patch 2.0
;Header6
	prtpage MONTHINCREPORT;*pBoxFiveVert:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Income",*boldoff;
;Header7
	prtpage MONTHINCREPORT;*pBoxFiveVert:row,*ALIGNMENT=*CENTER,*font=Arial11,*ll,*boldon,"Volume",*boldoff;

ExcelGrid
	
	add	OneandaHalfSpaced to row
	move	row to BegRowLine
;Volume 1
	prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxTwoLeft:BoxTwoRight
;Income 1
	prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxThreeLeft:BoxThreeRight
.Patch 2.0
;Volume2
	prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxFiveLeft:BoxFiveRight
;Actual
	prtpage MONTHINCREPORT;*pensize=15,*RECT=ROW:LgBoxHeight:BoxSixLeft:BoxSixRight
;Rent exchange halfboxes
;Add Shading - Using Cyan for now
;;
	move row to n9
	move begrowline to row
	move begrowline to n8
	add  halfsmboxheight to n8
	clear n2
	loop	
		
		add	c1 to n2
		prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=Row:n8:BoxTwoLeft1:BoxTwoRight
		prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=Row:n8:BoxFiveLeft1:BoxFiveRight
	until 	(n2 = "13")
		add	smboxheight to row
		add	smboxheight to n8
	repeat
	move	n9 to row
;;EndShading

;
;Using Boxes above in order to use shading
;		add HalfsmboxHeight,row
;		prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=15,*overlayon,*line=BoxTwoRight1:row;
;		prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=15,*overlayon,*line=BoxFiveRight1:row;
;		sub HalfsmboxHeight,row
;End comment out
TotalsSection
;Volume 1
	add lgboxheight,totBoxRow
	add "100",totBoxRow
	add smboxheight,totboxrow,TotBoxHeight
;
;Rent exchange halfboxes

;
	prtpage MONTHINCREPORT;*pensize=10,*FILL=*ON,*BGCOLOR=*WHITE,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
;	prtpage MONTHINCREPORT;*pensize=10,*RECT=TotBoxROW:TotBoxHeight:BoxOneLeft:BoxOneRight
	prtpage MONTHINCREPORT;*pensize=10,*RECT=TotBoxROW:TotBoxHeight:BoxFourLeft:BoxFourRight

	prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxTwoLeft:BoxTwoRight
;Income 1
	prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxThreeLeft:BoxThreeRight
;Volume2
	prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxFiveLeft:BoxFiveRight
;Actual
	prtpage MONTHINCREPORT;*pensize=15,*RECT=TotBoxROW:TotBoxHeight:BoxSixLeft:BoxSixRight

;Rent exchange halfboxes
		clear n8
		add HalfsmboxHeight,totboxrow,n8
		prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=totboxrow:n8:BoxTwoLeft1:BoxTwoRight
		prtpage MONTHINCREPORT;*pensize=15,*fill=*ON,*bgcolor=*YELLOW,*RECT=totboxrow:n8:BoxFiveLeft1:BoxFiveRight
;		prtpage MONTHINCREPORT;*pBoxTwoLeft1:totboxrow,*pensize=15,*overlayon,*line=BoxTwoRight1:totboxrow;
;		prtpage MONTHINCREPORT;*pBoxFiveLeft1:totboxrow,*pensize=15,*overlayon,*line=BoxFiveRight1:totboxrow;
;		sub HalfsmboxHeight,totboxrow

;Vertical Lines For Totals
        	prtpage MONTHINCREPORT;*pBoxTwoVert:TotBoxROW,*bgcolor=*WHITE,*pensize=10,*line=BoxTwoVert:TotBoxHeight;
        	prtpage MONTHINCREPORT;*pBoxThreeVert1:TotBoxROW,*pensize=10,*line=BoxThreeVert1:TotBoxHeight;
.Patch 2.0 Code Modification
			if (varflag = "Y")
	        	prtpage MONTHINCREPORT;*pBoxThreeVert2:TotBoxROW,*pensize=10,*line=BoxThreeVert2:TotBoxHeight;
			endif
.Patch 2.0 Code Modification
        	prtpage MONTHINCREPORT;*pBoxFiveVert:TotBoxROW,*pensize=10,*line=BoxFiveVert:TotBoxHeight;
.Patch 2.0 
.        	prtpage MONTHINCREPORT;*pBoxSixVert1:TotBoxROW,*pensize=10,*line=BoxSixVert1:TotBoxHeight;
.        	prtpage MONTHINCREPORT;*pBoxSixVert2:TotBoxROW,*pensize=10,*line=BoxSixVert2:TotBoxHeight;
.Patch 2.0
;Title Cell
	move row to n9
;Topmost
	add "10" to Row
;	prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Revised",*boldoff;
	sub	"10",Row
;Top

	add	"40" to row
	prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Total",*boldoff;
	prtpage MONTHINCREPORT;*pHeader10:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader14:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Total",*boldoff;
	prtpage MONTHINCREPORT;*pHeader15:row,*bgcolor=*YELLOW,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Exch. Volume",*boldoff;
	sub	"40" to row
;TopMiddle
	add	"120" to row
;	prtpage MONTHINCREPORT;*pHeader12:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Projections",*boldoff;
	sub	"120" to row
;Middle Centered
	add singlespaced to row
	prtpage MONTHINCREPORT;*pHeader11:row,*bgcolor=*WHITE,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Projections",*boldoff;
	prtpage MONTHINCREPORT;*pHeader12:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
.Patch 2.0
	if (Varflag = YES) 
		prtpage MONTHINCREPORT;*pHeader13:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Variance",*boldoff;
	endif
.Patch 2.0
.Patch 2.0 Comment out
.	prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Projections",*boldoff;
.Patch 2.0 Comment out
.	prtpage MONTHINCREPORT;*pHeader17:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
	prtpage MONTHINCREPORT;*pHeader16:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Actual",*boldoff;
.Patch 2.0 Comment out
.	prtpage MONTHINCREPORT;*pHeader18:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Variance",*boldoff;
.Patch 2.0 Comment out
;Second Line
	add "80" to row
;	add singlespaced to row
	prtpage MONTHINCREPORT;*pHeader9:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader10:row,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*boldon,"Rental Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader14:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,"Volume",*boldoff;
	prtpage MONTHINCREPORT;*pHeader15:row,*ALIGNMENT=*CENTER,*font=Arial9,*ll,*boldon,"Rental Volume",*boldoff;
	add "20" to row
	move n9 to row



;Month Boxes
	add	SmBoxHeight to row
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxOneLeft:BoxOneRight
	prtpage MONTHINCREPORT;*pensize=10,*RECT=ROW:LgBoxHeight:BoxFourLeft:BoxFourRight
;Months Inserted
	Clear N2
	move row to n9
	add	halfsmboxheight to row
	add	"40" to row
	loop	
;
		add	c1 to n2
		if (FiscMonth = c1)
			load	str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
		else
				If (fiscmonth = 2)
					load	str3,n2,"FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN"
				elseif (fiscmonth = 3) 
					load	str3,n2,"MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB"
				elseif (fiscmonth = 4) 
					load	str3,n2,"APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR"
				elseif (fiscmonth = 5) 
					load	str3,n2,"MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR"
				elseif (fiscmonth = 6) 
					load	str3,n2,"JUN","JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY"
				elseif (fiscmonth = 7) 
					load	str3,n2,"JUL","AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN"
				elseif (fiscmonth = 8) 
					load	str3,n2,"AUG","SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL"
				elseif (fiscmonth = 9) 
					load	str3,n2,"SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT"
				elseif (fiscmonth = 10) 
					load	str3,n2,"OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT","SEP"
				elseif (fiscmonth = 11) 
					load	str3,n2,"NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT","SEP","OCT"
				elseif (fiscmonth = 12) 
					load	str3,n2,"DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","OCT","SEP","OCT","NOV"
				endif
		endif
;
;		add	c1 to n2
;		load	str3 with n2,"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"
		prtpage MONTHINCREPORT;*p1350:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
		prtpage MONTHINCREPORT;*p6600:row,*ALIGNMENT=*CENTER,*font=Arial10,*ll,*boldon,str3;
		add	SmBoxHeight to row
	until 	(n2 = "12")
	repeat
;Total Inserted
	add	halfsmboxheight to Totboxrow
	add	"40" to Totboxrow
		prtpage MONTHINCREPORT;*p1350:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";
		prtpage MONTHINCREPORT;*p6600:TotBoxRow,*ALIGNMENT=*CENTER,*font=Arial8,*ll,*ulon,*boldon,"Total";

	sub	halfsmboxheight to Totboxrow
	sub	"40" to Totboxrow
	move n9 to row
;Horizontal Lines
	Clear N2
	loop	
	        prtpage MONTHINCREPORT;*pBoxOneLeft:row,*pensize=10,*line=BoxOneRight:row;
	        prtpage MONTHINCREPORT;*pBoxTwoLeft:row,*pensize=10,*line=BoxTwoRight:row;
		     prtpage MONTHINCREPORT;*pBoxThreeLeft:row,*pensize=10,*line=BoxThreeRight:row;
	        prtpage MONTHINCREPORT;*pBoxFourLeft:row,*pensize=10,*line=BoxFourRight:row;
	        prtpage MONTHINCREPORT;*pBoxFiveLeft:row,*pensize=10,*line=BoxFiveRight:row;
        	prtpage MONTHINCREPORT;*pBoxSixLeft:row,*pensize=10,*line=BoxSixRight:row;
		add	c1 to n2
;		add	HalfsmboxHeight,row
;	        prtpage MONTHINCREPORT;*pBoxTwoLeft1:row,*pensize=10,*line=BoxTwoRight1:row;
;	        prtpage MONTHINCREPORT;*pBoxFiveLeft1:row,*pensize=10,*overlayon,*line=BoxFiveRight1:row;
;		sub	HalfsmboxHeight,row
		add	SmBoxHeight to row
	until 	(n2 = "12")
	repeat
;Vertical Lines
        	prtpage MONTHINCREPORT;*pBoxTwoVert:BegRowLine,*pensize=10,*line=BoxTwoVert:LgBoxHeight;
        	prtpage MONTHINCREPORT;*pBoxThreeVert1:BegRowLine,*pensize=10,*line=BoxThreeVert1:LgBoxHeight;
.Patch 2.0 Code Modification
			if (varflag = "Y")
   	     	prtpage MONTHINCREPORT;*pBoxThreeVert2:BegRowLine,*pensize=10,*line=BoxThreeVert2:LgBoxHeight;
.	        	prtpage MONTHINCREPORT;*pBoxSixVert1:BegRowLine,*pensize=10,*line=BoxSixVert1:LgBoxHeight;
.   	     	prtpage MONTHINCREPORT;*pBoxSixVert2:BegRowLine,*pensize=10,*line=BoxSixVert2:LgBoxHeight;
			endif
.Patch 2.0
        	prtpage MONTHINCREPORT;*pBoxFiveVert:BegRowLine,*pensize=10,*line=BoxFiveVert:LgBoxHeight;

;Fill Months in

;Order Search
	MOVE	C1 TO NORDPATH
	Pack	NORDFLD2,"02L",lSTNUM
	call	nordaim
	IF NOT OVER
		IF (DATEBY = "M")
			MOVE	OMDTEM,MM
			MOVE	OMDTEY,YY
			MOVE	OMDTED,DD
			call	cvtjul
		else
			MOVE	OODTEM,MM
			MOVE	OODTEY,YY
			MOVE	OODTED,DD
			call	cvtjul
		endif
		Goto Enter

	else
		if (manualflag <> "Y")
			goto READIT1
		else
			goto ENDPROGRAM
		endif
	Endif
;Last Years Orders
	Loop	
		call	nordkg
		clear	rqty
		clear	exqty
	until	OVER
		IF NOT OVER
		IF (DATEBY = "M")
			MOVE	OMDTEM,MM
			MOVE	OMDTEY,YY
			MOVE	OMDTED,DD
			call	cvtjul
		else
			MOVE	OODTEM,MM
			MOVE	OODTEY,YY
			MOVE	OODTED,DD
			call	cvtjul
		endif
.			MOVE	OODTEM,MM
.			MOVE	OODTEY,YY
.			MOVE	OODTED,DD
.			call	cvtjul
		endif
Enter
.Patch1.3
		goto SKIP if (Juldays > TODAYIS)
.Patch1.3
		if ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
			if (OSTAT = "B" or OSTAT = "0")
				clear TMPVAR
				if (oexqty > "0")
					move oexqty to n9
					move	oqty to n10
					sub n9 from n10,TMPVAR 
					move	TMPVAR to RQTY
					move	n9 to EXQTY
						add	EXQTY to EXCHTOT2 
						add	EXQTY to ORDTOT2
						add	RQTY to RENTTOT2 
						add	RQTY to ORDTOT2
				else
					reset excodes
					scan oelcode in excodes
					if equal
						move oqty to EXQTY
						add	EXQTY to EXCHTOT2 
						add	EXQTY to ORDTOT2
					else
						move oqty to RQTY
						add	RQTY to RENTTOT2 
						add	RQTY to ORDTOT2
					endif
				endif
					call	cvtgreg	
					if (mm = "01")
						reset	mm
					endif
						
					clear n2
.For Fiscal
					If (FiscMonth <> c1)
						If (fiscmonth = 2)
							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
						elseif (fiscmonth = 3) 
							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
						elseif (fiscmonth = 4) 
							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
						elseif (fiscmonth = 5) 
							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
						elseif (fiscmonth = 6) 
							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
						elseif (fiscmonth = 7) 
							move mm to n2
							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
						elseif (fiscmonth = 8) 
							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
						elseif (fiscmonth = 9) 
							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
						elseif (fiscmonth = 10) 
							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
						elseif (fiscmonth = 11) 
							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
						elseif (fiscmonth = 12) 
							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
						endif
.						If (fiscmonth = 2)
.							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
.						elseif (fiscmonth = 3) 
.							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
.						elseif (fiscmonth = 4) 
.							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
.						elseif (fiscmonth = 5) 
.							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
.						elseif (fiscmonth = 6) 
.							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
.						elseif (fiscmonth = 7) 
.							move mm to n2
.							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
.						elseif (fiscmonth = 8) 
.							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
.						elseif (fiscmonth = 9) 
.							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
.						elseif (fiscmonth = 10) 
.							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
.						elseif (fiscmonth = 11) 
.							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
.						elseif (fiscmonth = 12) 
.							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
.						endif
					EndIf
					move mm to n2
;					call	cvtgreg	
;					clear n2
;					move	mm to n2
;Qty 
					clear dumqty
					clear n9
					clear str15
					load	str15,n2,JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2,SEP2,OCT2,NOV2,DEC2	
					move 	str15 to dumqty
					move	oqty to n9
					add		  n9 to dumqty
					move dumqty to str15
					store	str15,n2,JAN2,FEB2,MAR2,APR2,MAY2,JUN2,JUL2,AUG2,SEP2,OCT2,NOV2,DEC2	
;Rental
					clear dumqty
					clear n9
					clear str15
					load	str15,n2,JANRENT2,FEBRENT2,MARRENT2,APRRENT2,MAYRENT2,JUNRENT2,JULRENT2,AUGRENT2,SEPRENT2,OCTRENT2,NOVRENT2,DECRENT2	
					move 	str15 to dumqty
					move	rqty to n9
					add		  n9 to dumqty
					move dumqty to str15
					store	str15,n2,JANRENT2,FEBRENT2,MARRENT2,APRRENT2,MAYRENT2,JUNRENT2,JULRENT2,AUGRENT2,SEPRENT2,OCTRENT2,NOVRENT2,DECRENT2	
;Exch
					clear dumqty
					clear n9
					clear str15
					load	str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2	
					move 	str15 to dumqty
					move	exqty to n9
					add		  n9 to dumqty
					move dumqty to str15
					store	str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2	
			endif
;					store	str15,n2,JANEXCH2,FEBEXCH2,MAREXCH2,APREXCH2,MAYEXCH2,JUNEXCH2,JULEXCH2,AUGEXCH2,SEPEXCH2,OCTEXCH2,NOVEXCH2,DECEXCH2	
;;
;Invoice Part
.				MOVE      OLRN TO NINVFLD
.				If 	(OSTAT = "B" | OSTAT="Q")
.					move	c1 to ninvpath
.					CALL      NINVKEY
.					if Not OVER
.						CMATCH    "P" TO STATB	        ;PAID
.						If Equal
.								move      "01" to n2
.								move      n2 to str2
.								rep       zfill in str2
.								CLEAR     NJSTFLD
.	        						PACK      NJSTFLD FROM INVNUM,str2
.								rep       zfill in njstfld
.								CALL      NJSTKEY
.								if not over
.									add jstap1 to ap1
.									loop
.										add	c1 to n2
.										move      n2 to str2
.										rep       zfill in str2
.										CLEAR     NJSTFLD
.										PACK      NJSTFLD FROM INVNUM,str2
.										rep       zfill in njstfld
.										CALL      NJSTKEY
.									until over
.										add jstap1 to ap1
.									repeat
.								endif
.								move	CHK1DTEM to N2
;IncomePart
.								clear dumqty
.								clear n9
.								clear str15
.								load	num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2	
.								add ap1 to num102
.								add	ap1 to AP1TOT2
.								store	num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2	
;;;;
.						Endif
.					Endif
.				Endif
;;
		IF (DATEBY = "M")
			MOVE	OMDTEM,MM
			MOVE	OMDTEY,YY
			MOVE	OMDTED,DD
			call	cvtjul
		else
			MOVE	OODTEM,MM
			MOVE	OODTEY,YY
			MOVE	OODTED,DD
			call	cvtjul
		endif
.		MOVE	OODTEM,MM
.		MOVE	OODTEY,YY
.		MOVE	OODTED,DD
.		call	cvtjul
		elseif ((Juldays >= BegFiscCur) & (Juldays  <=  EndFiscCur))
;		elseif (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
			if (OSTAT = "B" or OSTAT = "0")
				clear TMPVAR
				if (oexqty > "0")
					move oexqty to n9
					move	oqty to n10
					sub n9 from n10,TMPVAR 
					move	TMPVAR to RQTY
					move	n9 to EXQTY
						add	EXQTY to EXCHTOT1 
						add	EXQTY to ORDTOT1
						add	RQTY to RENTTOT1 
						add	RQTY to ORDTOT1
				else
					reset excodes
					scan oelcode in excodes
					if equal
						move oqty to EXQTY
						add	EXQTY to EXCHTOT1
						add	EXQTY to ORDTOT1
					else
						move oqty to RQTY
						add	RQTY to RENTTOT1
						add	RQTY to ORDTOT1
					endif
				endif
					call	cvtgreg	
					if (mm = "01")
Tester
						reset	mm
					endif
;
.For Fiscal
					If (FiscMonth <> c1)
						If (fiscmonth = 2)
							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
						elseif (fiscmonth = 3) 
							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
						elseif (fiscmonth = 4) 
							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
						elseif (fiscmonth = 5) 
							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
						elseif (fiscmonth = 6) 
							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
						elseif (fiscmonth = 7) 
							move mm to n2
							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
						elseif (fiscmonth = 8) 
							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
						elseif (fiscmonth = 9) 
							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
						elseif (fiscmonth = 10) 
							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
						elseif (fiscmonth = 11) 
							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
						elseif (fiscmonth = 12) 
							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
						endif
.						If (fiscmonth = 2)
.							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
.						elseif (fiscmonth = 3) 
.							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
.						elseif (fiscmonth = 4) 
.							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
.						elseif (fiscmonth = 5) 
.							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
.						elseif (fiscmonth = 6) 
.							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
.						elseif (fiscmonth = 7) 
.							move mm to n2
.							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
.						elseif (fiscmonth = 8) 
.							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
.						elseif (fiscmonth = 9) 
.							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
.						elseif (fiscmonth = 10) 
.							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
.						elseif (fiscmonth = 11) 
.							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
.						elseif (fiscmonth = 12) 
.							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
.						endif
;						If (fiscmonth = 2)
;							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
;						elseif (fiscmonth = 3) 
;							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
;						elseif (fiscmonth = 4) 
;							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
;						elseif (fiscmonth = 5) 
;							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
;						elseif (fiscmonth = 6) 
;							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
;						elseif (fiscmonth = 7) 
;							move mm to n2
;							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
;						elseif (fiscmonth = 8) 
;							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
;						elseif (fiscmonth = 9) 
;							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
;						elseif (fiscmonth = 10) 
;							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
;						elseif (fiscmonth = 11) 
;							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
;						elseif (fiscmonth = 12) 
;							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
;						endif
					EndIf
					move	mm to n2
;Qty 
					clear dumqty
					clear n9
					clear str15
					load	str15,n2,JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1	
					move 	str15 to dumqty
					move	oqty to n9
					add	N9 to dumqty
					move dumqty to str15
					store	str15,n2,JAN1,FEB1,MAR1,APR1,MAY1,JUN1,JUL1,AUG1,SEP1,OCT1,NOV1,DEC1	
;Rental
					clear dumqty
					clear n9
					clear str15
					load	str15,n2,JANRENT1,FEBRENT1,MARRENT1,APRRENT1,MAYRENT1,JUNRENT1,JULRENT1,AUGRENT1,SEPRENT1,OCTRENT1,NOVRENT1,DECRENT1	
					move 	str15 to dumqty
					move	rqty to n9
					add	n9 to dumqty
					move dumqty to str15
					store	str15,n2,JANRENT1,FEBRENT1,MARRENT1,APRRENT1,MAYRENT1,JUNRENT1,JULRENT1,AUGRENT1,SEPRENT1,OCTRENT1,NOVRENT1,DECRENT1	
;Exch
					clear dumqty
					clear n9
					clear str15
					load	str15,n2,JANEXCH1,FEBEXCH1,MAREXCH1,APREXCH1,MAYEXCH1,JUNEXCH1,JULEXCH1,AUGEXCH1,SEPEXCH1,OCTEXCH1,NOVEXCH1,DECEXCH1	
					move 	str15 to dumqty
					move	exqty to n9
					add	n9 to dumqty
					move dumqty to str15
					store	str15,n2,JANEXCH1,FEBEXCH1,MAREXCH1,APREXCH1,MAYEXCH1,JUNEXCH1,JULEXCH1,AUGEXCH1,SEPEXCH1,OCTEXCH1,NOVEXCH1,DECEXCH1	
			endif
;;
		endif
SKIP
	call	INVOICEPART

.
	repeat 
;Variance Part
.PATCH1.2
	IF 	(AP1JAN1 <> C0)
		sub 	ProjJAN1,AP1JAN1,VarJan1
	ELSE
		MOVE C0 TO VARJAN1
	ENDIF	
	IF 	(AP1FEB1 <> C0)
		sub 	ProjFEB1,AP1FEB1,VarFeb1
	ELSE
		MOVE c0 TO VARFEB1
	ENDIF	
	IF 	(AP1MAR1 <> C0)
		sub 	ProjMAR1,AP1MAR1,VarMAR1
	ELSE
		MOVE c0 TO VARMAR1
	ENDIF	
	IF 	(AP1APR1 <> C0)
		sub 	ProjAPR1,AP1APR1,VarAPR1
	ELSE
		MOVE c0 TO VARAPR1
	ENDIF	
	IF 	(AP1MAY1 <> C0)
		sub 	ProjMAY1,AP1MAY1,VarMAY1
	ELSE
		MOVE c0 TO VARMAY1
	ENDIF		
	IF 	(AP1JUN1 <> C0)
		sub 	ProjJUN1,AP1JUN1,VarJUN1
	ELSE
		MOVE c0 TO VARJUN1
	ENDIF			
	IF 	(AP1JUL1 <> C0)
		sub 	ProjJUL1,AP1JUL1,VarJUL1
	ELSE
		MOVE c0 TO VARJUL1
	ENDIF			
	IF 	(AP1AUG1 <> C0)
		sub 	ProjAUG1,AP1AUG1,VarAUG1
	ELSE
		MOVE c0 TO VARAUG1
	ENDIF			
	IF 	(AP1SEP1 <> C0)
		sub 	ProjSEP1,AP1SEP1,VarSEP1
	ELSE
		MOVE c0 TO VARSEP1
	ENDIF			
	IF 	(AP1OCT1 <> C0)
		sub 	ProjOCT1,AP1OCT1,VarOCT1
	ELSE
		MOVE c0 TO VAROCT1
	ENDIF			
	IF 	(AP1NOV1 <> C0)
		sub 	ProjNOV1,AP1NOV1,VarNOV1
	ELSE
		MOVE c0 TO VARNOV1
	ENDIF			
	IF 	(AP1DEC1 <> C0)
		sub 	ProjDEC1,AP1DEC1,VarDEC1
	ELSE
		MOVE c0 TO VARDEC1
	ENDIF			
	IF 	(AP1JAN2 <> C0)
		sub 	ProjJAN2,AP1JAN2,VarJAN2
	ELSE
		MOVE c0 TO VARJAN2
	ENDIF			
	IF 	(AP1FEB2 <> C0)
		sub 	ProjFEB2,AP1FEB2,VarFEB2
	ELSE
		MOVE c0 TO VARFEB2
	ENDIF			
	IF 	(AP1MAR2 <> C0)
		sub 	ProjMAR2,AP1MAR2,VarMAR2
	ELSE
		MOVE c0 TO VARMAR2
	ENDIF			
	IF 	(AP1APR2 <> C0)
		sub 	ProjAPR2,AP1APR2,VarAPR2
	ELSE
		MOVE c0 TO VARAPR2
	ENDIF			
	IF 	(AP1MAY2 <> C0)
		sub 	ProjMAY2,AP1MAY2,VarMAY2
	ELSE
		MOVE c0 TO VARMAY2
	ENDIF			
	IF 	(AP1JUN2 <> C0)
		sub 	ProjJUN2,AP1JUN2,VarJUN2
	ELSE
		MOVE c0 TO VARJUN2
	ENDIF			
	IF 	(AP1JUL2 <> C0)
		sub 	ProjJUL2,AP1JUL2,VarJUL2
	ELSE
		MOVE c0 TO VARJUL2
	ENDIF			
	IF 	(AP1AUG2 <> C0)
		sub 	ProjAUG2,AP1AUG2,VarAUG2
	ELSE
		MOVE c0 TO VARAUG2
	ENDIF			
	IF 	(AP1SEP2 <> C0)
		sub 	ProjSEP2,AP1SEP2,VarSEP2
	ELSE
		MOVE c0 TO VARSEP2
	ENDIF			
	IF 	(AP1OCT2 <> C0)
		sub 	ProjOCT2,AP1OCT2,VarOCT2
	ELSE
		MOVE c0 TO VAROCT2
	ENDIF			
	IF 	(AP1NOV2 <> C0)
		sub 	ProjNOV2,AP1NOV2,VarNOV2
	ELSE
		MOVE c0 TO VARNOV2
	ENDIF			
	IF 	(AP1DEC2 <> C0)
		sub 	ProjDEC2,AP1DEC2,VarDEC2
	ELSE
		MOVE c0 TO VARDEC2
	ENDIF			
.PATCH1.2
	add	VARJAN1,VARTOT1
	add	VARFEB1,VARTOT1
	add	VARMAR1,VARTOT1
	add	VARAPR1,VARTOT1
	add	VARMAY1,VARTOT1
	add	VARJUN1,VARTOT1
	add	VARJUL1,VARTOT1
	add	VARAUG1,VARTOT1
	add	VARSEP1,VARTOT1
	add	VAROCT1,VARTOT1
	add	VARNOV1,VARTOT1	
	add	VARDEC1,VARTOT1
	add	VARJAN2,VARTOT2
	add	VARFEB2,VARTOT2
	add	VARMAR2,VARTOT2
	add	VARAPR2,VARTOT2
	add	VARMAY2,VARTOT2
	add	VARJUN2,VARTOT2
	add	VARJUL2,VARTOT2
	add	VARAUG2,VARTOT2
	add	VARSEP2,VARTOT2
	add	VAROCT2,VARTOT2
	add	VARNOV2,VARTOT2	
	add	VARDEC2,VARTOT2


	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask20 to dim20a
        edit JAN1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit FEB1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAR1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit APR1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAY1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JUN1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row						       
        move mask20 to dim20a
        edit JUL1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit AUG1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit SEP1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit OCT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit NOV1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit DEC1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;RentVolumeYear1
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask20 to dim20a
        edit JANRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit FEBRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MARRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit APRRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAYRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JUNRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JULRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit AUGRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit SEPRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit OCTRENT1 to dim20a	
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit NOVRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit DECRENT1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;ExchVolumeYear1
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	"40" to row
        move 	mask20 to dim20a
        edit 	JANEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	FEBEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	MAREXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	APREXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	MAYEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	JUNEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	JULEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	AUGEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	SEPEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	OCTEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	NOVEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move 	mask20 to dim20a
        edit 	DECEXCH1 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxTwoRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
;
;Proj
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask16 to dim16a
        edit ProjJAN1 to dim16a
	call	trim using	dim16a
.Patch 2.0 Code Modification
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*BGCOLOR=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjFEB1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjMAR1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjAPR1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjMAY1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjJUN1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row						       
      move mask16 to dim16a
      edit ProjJUL1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjAUG1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjSEP1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjOCT1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjNOV1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
		add	SmBoxHeight to row
      move mask16 to dim16a
      edit ProjDEC1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
;
	if (varflag = "Y")
;Variance
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
   move mask18 to dim18a
   edit VarJAN1 to dim18a
	squeeze	dim18a,dim18a	
.	call	trim using	dim18a
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*BGCOLOR=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarFEB1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarMAR1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarAPR1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarMAY1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarJUN1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row						       
   move mask18 to dim18a
   edit VarJUL1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarAUG1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarSEP1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarOCT1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarNOV1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	add	SmBoxHeight to row
   move mask18 to dim18a
   edit VarDEC1 to dim18a
	squeeze	dim18a,dim18a	
	prtpage MONTHINCREPORT;*pBoxThreeRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	endif
;Actual
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask16 to dim16a
        edit AP1JAN1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1FEB1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1MAR1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1APR1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1MAY1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1JUN1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row						       
        move mask16 to dim16a
        edit AP1JUL1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1AUG1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1SEP1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1OCT1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1NOV1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1DEC1 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxThreeVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
;Previous Year
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask20 to dim20a
        edit JAN2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit FEB2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAR2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit APR2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAY2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JUN2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row						       
        move mask20 to dim20a
        edit JUL2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit AUG2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit SEP2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit OCT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit NOV2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit DEC2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveVertText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;RentVolumeYear2
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask20 to dim20a
        edit JANRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit FEBRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MARRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit APRRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAYRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JUNRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JULRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit AUGRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit SEPRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit OCTRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit NOVRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit DECRENT2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;

;ExchVolumeYear2
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	"40" to row
        move mask20 to dim20a
        edit JANEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit FEBEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit	MAREXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit APREXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit MAYEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JUNEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit JULEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit AUGEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit SEPEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit OCTEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit NOVEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
	add	SmBoxHeight to row
        move mask20 to dim20a
        edit DECEXCH2 to dim20a
	call	trim using	dim20a
	prtpage MONTHINCREPORT;*pBoxFiveRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim20a;
;
;Proj
.Patch 2.0  Comment Out
.	move	 BegRowLine to row
.	add	SmBoxHeight to row
.	add	OneandahalfSpaced to row
.        move mask16 to dim16a
.        edit ProjJAN2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*BGCOLOR=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjFEB2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjMAR2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjAPR2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjMAY2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjJUN2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row						       
.        move mask16 to dim16a
.        edit ProjJUL2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjAUG2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjSEP2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjOCT2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjNOV2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
.	add	SmBoxHeight to row
.        move mask16 to dim16a
.        edit ProjDEC2 to dim16a
.	call	trim using	dim16a
.	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
;
.Patch 2.0  Comment Out

;
;Previous Year Acutals
	move	 BegRowLine to row
	add	SmBoxHeight to row
	add	OneandahalfSpaced to row
        move mask16 to dim16a
        edit AP1JAN2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1FEB2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1MAR2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1APR2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1MAY2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1JUN2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row       
        move mask16 to dim16a
        edit AP1JUL2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1AUG2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1SEP2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1OCT2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1NOV2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
	add	SmBoxHeight to row
        move mask16 to dim16a
        edit AP1DEC2 to dim16a
	call	trim using	dim16a
	prtpage MONTHINCREPORT;*pBoxSixVert1Text:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim16a;
;Old Variance
.Patch 2.0 
.	move	 BegRowLine to row
.	add	SmBoxHeight to row
.	add	OneandahalfSpaced to row
.        move mask18 to dim18a
.        edit VarJAN2 to dim18a
.	squeeze	dim18a,dim18a	
.	call	trim using	dim18a
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*BGCOLOR=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarFEB2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarMAR2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarAPR2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarMAY2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarJUN2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row						       
.        move mask18 to dim18a
.        edit VarJUL2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarAUG2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarSEP2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarOCT2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarNOV2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.	add	SmBoxHeight to row
.        move mask18 to dim18a
.        edit VarDEC2 to dim18a
.	squeeze	dim18a,dim18a	
.	prtpage MONTHINCREPORT;*pBoxSixRightText:row,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.Patch 2.0 
;

Totals ;End Of Page
		add	"40" to Totboxrow
	        move mask20 to dim20a
	        edit EXCHTOT1 to dim20a
		call	trim using	dim20a
		prtpage MONTHINCREPORT;*pBoxTwoRightText:TotBoxRow,*bgcolor=*YELLOW,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
	        move mask20 to dim20a
	        edit EXCHTOT2 to dim20a
		call	trim using	dim20a
		prtpage MONTHINCREPORT;*pBoxFiveRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
		sub	"40" to Totboxrow

		add	halfsmboxheight to Totboxrow
		add	"40" to Totboxrow
	        move mask20 to dim20a
	        edit ORDTOT1 to dim20a
		call	trim using	dim20a
		prtpage MONTHINCREPORT;*pBoxTwoVertText:TotBoxRow,*bgcolor=*WHITE,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
	        move mask20 to dim20a
	        edit RENTTOT1 to dim20a
		call	trim using	dim20a
		prtpage MONTHINCREPORT;*pBoxTwoRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
;		clear	DIM20a
;	        move mask20 to dim20a
;	        edit ORDTOT1 to dim20a
;		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,"Total";
;		clear	DIM20a
;	        move mask20 to dim20a
;	        edit ORDTOT1 to dim20a
;		prtpage MONTHINCREPORT;*pBoxThreeVert2Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,"Total";
	        move mask16 to dim16a
	        edit PROJTOT1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert1Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim16a;

	        move mask16 to dim16a
	        edit AP1TOT1 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxThreeVert2Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim16a;
.Variance
.Patch 2.0
	if (VarFlag = YES)
		move mask18 to dim18a
		edit VARTOT1 to dim18a
		squeeze	dim18a,dim18a
		prtpage MONTHINCREPORT;*pBoxThreeRightText:TotBoxrow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
	endif
.Patch 2.0
      move mask20 to dim20a
      edit ORDTOT2 to dim20a
		call	trim using	dim20a
		prtpage MONTHINCREPORT;*pBoxFiveVertText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
	        move mask20 to dim20a
	        edit RENTTOT2 to dim20a
		call	trim using	dim20a
		prtpage MONTHINCREPORT;*pBoxFiveRightText:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim20a;
.OldProj
.Patch 2.0 Comment Out
.	        move mask16 to dim16a
.	        edit PROJTOT2 to dim16a
.		call	trim using	dim16a
.		prtpage MONTHINCREPORT;*pBoxSixVert1Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim16a;
.Patch 2.0
.Patch 2.0
		move mask16 to dim16a
		edit AP1TOT2 to dim16a
		call	trim using	dim16a
		prtpage MONTHINCREPORT;*pBoxSixVert1Text:TotBoxRow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,*ulon,dim16a;
.Patch 2.0
.Variance
.Patch 2.0
.		move mask18 to dim18a
.		edit VARTOT2 to dim18a
.		squeeze	dim18a,dim18a
.		prtpage MONTHINCREPORT;*pBoxSixRightText:TotBoxrow,*ALIGNMENT=*RIGHT,*font=Arial10,*ll,dim18a;
.Patch 2.0
	sub	halfsmboxheight to Totboxrow
	sub	"40" to Totboxrow

.patch1.1 Open Payables
	move	todayis to juldays
	call	cvtgreg

	pack	str10 with mm,"/",dd,"/",cc,yy
.	clock timestamp,TIMESTAMP
.	unpack	timestamp,str4,str2,dd
.	call	trim using str3
.	pack	str10 with str2,"/",dd,"/",str4

	if (LTYPE = "C")
.		Move row to N9
		move	TotBoxHeight to Row
		add	singlespaced to Row
	        move	mask19 to dim19a
        	edit	OPENPAY to dim19a
		call	trim using	dim19a
		pack Taskname with "Payables = ",dim19a, " as of ",str10
		prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.>Patch 2.3 Code Added	
		if (ManualFlag = YES)
			call getuser
			call trim using nuseuser
			clear taskname
			pack taskname with "Report run by ",nuseuser," ","on ",DateRan
			prtpage MONTHINCREPORT;*pBoxSixVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,taskname;
		Endif
.>Patch 2.3 Code Added
	else
		move	TotBoxHeight to Row
		add	singlespaced to Row
		pack Taskname with "*As of ",str10
		prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.>Patch 2.3 Code Added		
		if (ManualFlag = YES)
			call getuser
			call trim using nuseuser		
			pack taskname with "Report run by ",nuseuser," on ",DateRan		
			prtpage MONTHINCREPORT;*pBoxSixVert2Text:row,*ALIGNMENT=*RIGHT,*font=Arial9,*ll,taskname;
		endif
.>Patch 2.3 Code Added		
	endif
..	if (LTYPE = "C")
.		Move row to N9
..		move	TotBoxHeight to Row
..		add	singlespaced to Row
..	        move	mask19 to dim19a
.        	edit	OPENPAY to dim19a
.		call	trim using	dim19a
.
.		clock timestamp,TIMESTAMP
.		unpack	timestamp,str4,str2,dd
.		call	trim using str3
.		pack	str10 with str2,"/",dd,"/",str4
.		pack Taskname with "Payables = ",dim19a, " as of ",str10
.		prtpage MONTHINCREPORT;*pBoxOneLeft:row,*ALIGNMENT=*LEFT,*font=Arial9,*ll,taskname;
.	endif
.patch1.1

	if (ManualFlag <> YES)
READIT1
		READ	INCLISTS,SEQ;LVARS
		GOTO FinalRun IF OVER
		GOTO READIT1 IF (REP1 <> "V")
.Patch 2.0
		move rep2 to varflag
.Patch 2.0
        	DISPLAY     *P10:10,*EF,*white,"List:  ",LNUM,"   ",OLSTNAME
.Patch 2.0
.	        prtpage   MONTHINCREPORT;*NEWPAGE:
.			*ORIENT=*LANDSCAPE
.Patch 2.0
		CALL CLEARVARS
		call EndProgram
		GOTO READIT
	ENDIF
	
ENDPROGRAM
	prtclose	MONTHINCREPORT
	prtplay		PRTFILENAME,"PDF995"
.>Patch 2.1	
	pack	APIFileName,"c:\progra~1\pdf995\flag.dat",hexzero
	loop
		call	FindFirstFile
		until (APIResult = 0 | APIResult = hexeight)
		pause	"1"
	repeat
	pause	"2"
	erase	"c:\progra~1\pdf995\flag.dat"	
.>Patch 2.1
;	pause c10
.patch1.4
	if (ManualFlag = YES)
.>Patch 2.3 Comment Out - We find out who user is above 	
.		move      c1 to nusepath
.		clock	  port to str3
.		unpack    str3 into str2,str1
.		pack      str3 from str1,str2
.		MOVE      str3 TO NUSEFLD .removed FOR TESTING only
.		REP       ZFILL IN NUSEFLD
.		CALL      NUSEKEY
.		goto      userng if over
.		scan      "INVALID" in nuseuser
.		goto      userng if equal
.>Patch 2.3 Comment Out - User is found out above		
		squeeze	 nuseuser,nuseuser
	else
.Patch 3.9
	  	call trim using recipient
     		move Recipient to nuseuser
	endif
.patch3.2
.>Patch 2.1
EmailReport
       pack str55,PRINTNAME,".pdf"
	pack   SmtpSubject,"Here is your Income PDF File for ",HoldLstName
;.   Set the text message that is send with the attachments
	move    str55,SmtpTextMessage(1)   Array <Text message >
	move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
	move    "NTS4",SmtpEmailServer                   Address of email serverc
	clear   smtpemailaddress
	append  nuseuser,SmtpEmailAddress
	append  "@nincal.com",SmtpEmailAddress
	reset   smtpemailaddress
	move    nuseuser,SmtpUserName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
	move    smtpemailaddress,SmtpDestinations(1,1)
	move    nuseuser,SmtpDestinations(1,2)
	move    "1",SmtpDestIndexLast                          originators UserName
	move    str55,SmtpAttachments(1,1)                     Attached file name
	move    "c:\work\pdf",SmtpAttachments(1,2)           Path to attached file name
	move    "1",SmtpAttIndexLast                                Index to last entry - Only 1 entry
	clear   SmtpLogFile                                         'Clear' disables the LogFile
	move    "1",SmtpProgress                                    Enable progress bars
	call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	if not equal
		pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
		"Status Code ",SmtpStatus," - ",SmtpStatusText
		move    "PDF File not found",SmtpSubject Subject
		move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
		call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
	endif
.>Patch 2.1
	if (manualflag <> YES)
		pack str55,PRINTNAME,".pdf"
		clear	taskname
		call getwinver
		If	(osflag = c1 | osflag = c5)
			append  "!c:\winnt\system32\cmd.exe /c ",taskname
		ElseIf	(osflag = c3 | osflag = c4)
			append  "!c:\command.com /c ",taskname
		ElseIf	(osflag = c6)
			append  "!c:\windows\system32\cmd.exe /c ",taskname
		Endif
		append  " copy ",taskname
		append  "c:\work\pdf\",taskname
		append  str55,taskname
		append  " ",taskname
		append  NTWKPATH5,taskname
		append  "INCOME\",taskname
		append  LSTNUM,taskname
		append  "_",taskname
		append  str55,taskname
		reset	taskname
		execute	taskname
.Patch 2.0
		pause c5
		erase str55
		erase prtfilename
		return
.Patch 2.0
	endif
.endpatch1.4
.Patch 2.0
	if (MANUALFLAG = "Y")
		stop
	endif
.Patch 2.0
.	stop
.	prtclose	MONTHINCREPORT
.	prtplay		PRTFILENAME,"\\NTS0\Laser2"

.ENDPROGRAM

InvoicePart
;Invoice Part
		MOVE      OLRN TO NINVFLD
		If 	(OSTAT = "B" | OSTAT="Q")
			move	c1 to ninvpath
			CALL      NINVKEY

			if Not OVER
;JDpatch
				CMATCH    "0" TO STATB	        ;Open and still have to check for "14" Adjustment
;For invoice accrued basis
				If Equal
;For invoice Date
.patch1.1 Open Payables
.					if (LTYPE = "I")
.patch1.1 Open Payables
;;;;;;;;;;;;;
;Compute
					if (ap2 <= c0)
						MOVE      YES TO SUBPPSW
						MOVE      OLRN to nmrgfld
						REP       ZFILL IN NMRGFLD
						move      c0 to nmrgrqty
						move      c0 to nmrgiqty
						move      c0 to nmrgnet
						move      no to mrgsw
	        				move      no to shipsw
						CALL      NMRGKEY
						if        not over
							move      yes to mrgsw
						endif
						MOVE    NordFLD to nshpfld
						REP     ZFILL IN NshpFLD
						CALL    NshpKEY
						if      not over
							move yes to shipsw
						endif
						call    wipecvars
						move    c1 to ndatpath
						move    olnum to ndatfld
						call    ndatkey
						move    lrn to nshpfld
				        	call    nshpkey
					        call    NInvAcdRecClear
               					CLEAR   NInvAcdfld
               						packkey NInvAcdFld from Invnum
;               call           NInvAcdRecClear
               					call    NinvAcdTst
               					Call    NInvAcdRecLoad
						CALL    COMPUTE
						move    ap to ap1
;;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
						clear	holdap1
						move	ap to holdap1
;;;;;;;;;;C
					else
						move    ap2 to ap1
;;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
						clear	holdap1
						move	ap2 to holdap1
;;;;;;;;;;C
					endif
.patch1.1 Open Payables
.					endif
.patch1.1 Open Payables
;Compute
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
					move      "01" to n2
					move      n2 to str2
					rep       zfill in str2
					CLEAR     NJSTFLD
					PACK      NJSTFLD FROM INVNUM,str2
					rep       zfill in njstfld
					clear	  num1022
;For Invoice Accrual Basis
.patch1.1 Open Payables
.				if (LTYPE = "I")
.patch1.1 Open Payables
					CALL      NJSTKEY
					if Not Over
;COmpute
						if (ap2 <= c0)
;;;;;;;;;;C								add jstap1 to ap1
;master adj check
							add	jstap1 to num1022
;master adj check

						else
;;;;;;;;;;C								add jstap2 to ap1
;master adj check
							add	jstap2 to num1022
;master adj check
						endif
					else
.Patch1.1 Open Payables
						if (LTYPE = "C")
							add ap1 to OPENPAY
							CALL      NJSTKEY
							RETURN	if OVER
						else
.Patch1.1       				
							goto Buckets
.patch1.1
						endif
.patch1.1
					endif
;COmpute
.Patch1.1 Open Payables
.				else
.Patch1.1 Open Payables
;For Invoice Accrual Basis
.Patch1.1 Open Payables
.						CALL      NJSTKEY
.						RETURN	if OVER
.Patch1.1 Open Payables
;;;;;;;;;;Invoice Accrual
.Patch1.1 Open Payables
.					endif
.Patch1.1 Open Payables
;For Invoice Accrual Basis

					MATCH     "14" TO JSTREASN
					IF      EQUAL
						move jstap1 to ap1
						mult seq to ap1
						if (ap1 < c0)
							mult seq by ap1
						endif
.Patch1.1 Open Payables
						add ap1 to OPENPAY
.						if (LTYPE = "C")
.								return
.						endif
.Patch1.1
;Grab Adj Date - 
						unpack JSTDATE,str2,YY,MM,DD
						move 	MM to n2
						Goto	ManualBuckets
					ENDIF
.Patch 1.8.1 Logic Added
					clear n2
					for n2,"2","9"
.					loop
.						add	c1 to n2
.Patch 1.8.1 Logic Added
						move      n2 to str2
						rep       zfill in str2
						CLEAR     NJSTFLD
						PACK      NJSTFLD FROM INVNUM,str2
						rep       zfill in njstfld
						CALL      NJSTKEY
.Patch 1.8.1 Comment Out
.					until over
.Patch 1.8.1 Comment Out
;For Invoice Accrual 
.Patch1.1 Open Pay
.						if (LTYPE = "I")
.Patch1.1 Open Pay
						if (ap2 <= c0)
;;;;;;;;;;C								add jstap1 to ap1
;master adj check
							add	jstap1 to num1022
;master adj check
						else
;;;;;;;;;;C								add jstap2 to ap1
;master adj check
							add	jstap2 to num1022
;master adj check
						endif
.Patch1.1 Open Pay
.						endif
.Patch1.1 Open Pay
;For INvoice Accrual

					        MATCH     "14" TO JSTREASN
						IF      EQUAL
							move jstap1 to ap1
							mult seq to ap1
							if (ap1 < c0)
								mult seq by ap1
							endif
.Patch1.1 Open Payables
							add ap1 to OPENPAY
.							if (LTYPE = "C")
.									return
.							endif
.Patch1.1
;Grab Adj Date - 
							unpack JSTDATE,str2,YY,MM,DD
							move 	MM to n2
							Goto	ManualBuckets
						ENDIF
					repeat
;For invoice accrual
.patch1.1 Open Payables
.				if (LTYPE = "I")
.patch1.1 Open Payables
;;;;;;;;;;C
					if (ap2 <= c0)
;;;;;;;;;;C
						move holdap1 to ap1
						MOVE	OLRN TO NADJFLD
						REP	ZFILL IN NADJFLD
						CALL	NADJKEY
						add	aspayad1 to ap1
					else
						move   holdap1 to ap1
						MOVE	OLRN TO NADJFLD
						REP	ZFILL IN NADJFLD
						CALL	NADJKEY
						add	aspayad2 to ap1
;;;;;;;;;;C					        
					endif
;;;;;;;;;;C

					return if (ap1 < 0)
.Patch1.1 Open Payables
					add ap1 to OPENPAY
					if (LTYPE = "C")
						return
					endif
.Patch1.1
					goto	Buckets
.patch1.1 Open Payables
				endif
.patch1.1 Open Payables
;for invoice accrual
.patch1.1 Open Payables
.			endif
.patch1.1 Open Payables
				CMATCH    "P" TO STATB	        ;PAID
				If Equal
;;;;;;;;;;;;;
;Compute
					if (ap2 <= c0)
					 	MOVE      YES TO SUBPPSW
				         	MOVE      OLRN to nmrgfld
				         	REP       ZFILL IN NMRGFLD
			        	 	move      c0 to nmrgrqty
			        	 	move      c0 to nmrgiqty
				         	move      c0 to nmrgnet
				         	move      no to mrgsw
        				 	move      no to shipsw
			        	 	CALL      NMRGKEY
				         	if        not over
					        	move      yes to mrgsw
				         	endif
				         	MOVE      NordFLD to nshpfld
			        	 	REP       ZFILL IN NshpFLD
				         	CALL      NshpKEY
				         	if        not over
					        	move      yes to shipsw
				         	endif
			        	 	call      wipecvars
				         	move      c1 to ndatpath
				         	move      olnum to ndatfld
				         	 call      ndatkey
				         	move      lrn to nshpfld
			        	 	call      nshpkey
               call           NInvAcdRecClear
               CLEAR          NInvAcdfld
               packkey           NInvAcdFld from Invnum
;               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
				         	CALL      COMPUTE
					 	move      ap to ap1
;;;;;;;;;;C   - may have to use this hold for master adjustment reconcile
						clear	holdap1
						move	ap to holdap1
;;;;;;;;;;C
					 else
						move ap2 to ap1
					 endif
;Compute
;;;;;;;;;;;;;;
					move      "01" to n2
					move      n2 to str2
					rep       zfill in str2
					CLEAR		NUM1022
					CLEAR     NJSTFLD
					PACK      NJSTFLD FROM INVNUM,str2
					rep       zfill in njstfld
					CALL      NJSTKEY
					if not over
;Compute
						if (ap2 <= c0)
.....
							if (JSTREASN <> "14")    ..add for exception test
.....
								add jstap1 to ap1
.....
							endif		       .add for exception test
;master adj check
								add	jstap1 to num1022
;master adj check
.....
						else
							add jstap2 to ap1
;master adj check
								add	jstap2 to num1022
;master adj check
						endif
;Compute
						
;Comment out for exception Test
.....					        MATCH     "14" TO JSTREASN
.....					        IF      EQUAL
.....							move jstap1 to ap1
.....							mult seq to ap1
.....							if (ap1 < c0)
.....								mult seq by ap1
.....							endif
;Grab Adj Date - 
.....							unpack JSTDATE,str2,YY,MM,DD
.....							move 	MM to n2
.....							Goto	ManualBuckets
.....						ENDIF
;Comment out for exception Test
.Patch 1.8.1 Logic Added
					clear n2
					for n2,"2","9"
.						loop
.							add	c1 to n2
.Patch 1.8.1 Logic Added
							move      n2 to str2
							rep       zfill in str2
							CLEAR     NJSTFLD
							PACK      NJSTFLD FROM INVNUM,str2
							rep       zfill in njstfld
							CALL      NJSTKEY
.Patch 1.8.1 Comment Out
.						until over
.Patch 1.8.1 Comment Out
;compute
							if (ap2 <= c0)
.....
								if (JSTREASN <> "14")	      ..add for exception test
.....
									add jstap1 to ap1
.....
								endif			       .add for exception test
;master adj check
								add	jstap1 to num1022
;master adj check
.....
							else
								add jstap2 to ap1
;master adj check
								add	jstap2 to num1022
;master adj check
							endif
;compute
.....						        MATCH     "14" TO JSTREASN
.....						        IF      EQUAL
.....								move jstap1 to ap1
.....								mult seq to ap1
.....								if (ap1 < c0)
.....									mult seq by ap1
.....								endif
;Grab Adj Date - 
.....							unpack JSTDATE,str2,YY,MM,DD
.....							move 	MM to n2
.....							Goto	ManualBuckets
.....							ENDIF
						repeat

.....Code Added for exception test of the infamous "14"
;;;;;;;;;;;;;;
					move      "01" to n2
					move      n2 to str2
					rep       zfill in str2
					CLEAR     NJSTFLD
					PACK      NJSTFLD FROM INVNUM,str2
					rep       zfill in njstfld
					CALL      NJSTKEY
					if not over
;Compute
;Added code for exception Test
						if (ap2 <= c0)
							clear num102
						        MATCH     "14" TO JSTREASN
						        IF      EQUAL
								move jstap1 to num102
...........
							ENDIF
..........
.Patch 1.8.1 Logic Added
					clear n2
					for n2,"2","9"
.							loop
.								add	c1 to n2
.Patch 1.8.1 Logic Added
								move      n2 to str2
								rep       zfill in str2
								CLEAR     NJSTFLD
								PACK      NJSTFLD FROM INVNUM,str2
								rep       zfill in njstfld
								CALL      NJSTKEY
.Patch 1.8.1 Comment Out
.							until over
.Patch 1.8.1 Comment Out
									if (JSTREASN = "14")
										add jstap1 to num102
									endif
							repeat
							mult seq to num102
							if (num102 < c0)
								mult seq by num102
							endif
;;;;;;;;;;C
							move    holdap1 to ap1
							MOVE	OLRN TO NADJFLD
							REP	ZFILL IN NADJFLD
							CALL	NADJKEY
							add	aspayad1 to ap1
;;;;;;;;;;C
							sub	num102 from ap1,num1021
							if (num1021 <> c0)
								add num102,ap1
;;;;;;;;;;C
							else
								move holdap1 to ap1
								MOVE	OLRN TO NADJFLD
								REP	ZFILL IN NADJFLD
								CALL	NADJKEY
								add	aspayad1 to ap1
;;;;;;;;;;C
							endif
;;;;;;;;;;C
.							else
..
							if (num102 = c0)
..
								move holdap1 to ap1
								MOVE	OLRN TO NADJFLD
								REP	ZFILL IN NADJFLD
								CALL	NADJKEY
								add	aspayad1 to ap1
							endif
;;;;;;;;;;C
;;;;;;;;;;C
						else
							move holdap1 to ap1
							MOVE	OLRN TO NADJFLD
							REP	ZFILL IN NADJFLD
							CALL	NADJKEY
							add	aspayad2 to ap1
;;;;;;;;;;C
						endif
					endif
.....Code Added for the exception test of the infamous "14"

;						if (NINVFLD = "486497")
;BP
;							reset ninvfld
;						endif
.				CMATCH    "P" TO STATB	        ;PAID
.				If Equal
;;;;;;;;;;;;;
;Compute
.			if (ap2 <= c0)
.				 MOVE      YES TO SUBPPSW
.			         MOVE      OLRN to nmrgfld
.			         REP       ZFILL IN NMRGFLD
.			         move      c0 to nmrgrqty
.		        	 move      c0 to nmrgiqty
.			         move      c0 to nmrgnet
.			         move      no to mrgsw
.        			 move      no to shipsw
.			         CALL      NMRGKEY
.			         if        not over
.				         move      yes to mrgsw
.			         endif
.			         MOVE      NordFLD to nshpfld
.			         REP       ZFILL IN NshpFLD
.			         CALL      NshpKEY
.			         if        not over
.				         move      yes to shipsw
.			         endif
.			         call      wipecvars
.			         move      c1 to ndatpath
.			         move      olnum to ndatfld
.			         call      ndatkey
.			         move      lrn to nshpfld
.			         call      nshpkey
.			         CALL      COMPUTE
.				 move      ap to ap1
.			else
.				move ap2 to ap1
.			endif
;Compute
;;;;;;;;;;;;;;
.						move      "01" to n2
.						move      n2 to str2
.						rep       zfill in str2
.						CLEAR     NJSTFLD
.						PACK      NJSTFLD FROM INVNUM,str2
.						rep       zfill in njstfld
.						CALL      NJSTKEY
.						if not over
;COmpute
.							if (ap2 <= c0)
.								add jstap1 to ap1
.							else
.								add jstap2 to ap1
.							endif
;COmpute
.							
.						         MATCH     "14" TO JSTREASN
.						        IF      EQUAL
.								move jstap1 to ap1
.								mult seq to ap1
.								if (ap1 < c0)
.									mult seq by ap1
.								endif
.								Goto	Buckets
.							ENDIF
.							loop
.								add	c1 to n2
.								move      n2 to str2
.								rep       zfill in str2
.								CLEAR     NJSTFLD
.								PACK      NJSTFLD FROM INVNUM,str2
.								rep       zfill in njstfld
.								CALL      NJSTKEY
.							until over
;compute
.								if (ap2 <= c0)
.									add jstap1 to ap1
.								else
.									add jstap2 to ap1
.								endif
;compute
.							         MATCH     "14" TO JSTREASN
.							        IF      EQUAL
.									move jstap1 to ap1
.									mult seq to ap1
.									if (ap1 < c0)
.										mult seq by ap1
.									endif
.									Goto	Buckets
.								ENDIF
.							repeat
.						endif
........................................
				endif
......................................


Buckets
				move	CHK1DTEM to N2
				move	CHK1DTEM to MM
				move	CHK1DTED to DD
				move	CHK1DTEY to YY
ManualBuckets
;patchfor by invoice date
				if (LTYPE = "I")
					move	INVDTEM to N2
					move	INVDTEM to MM
					move	INVDTED to DD
					move	INVDTEY to YY
				endif
				call	CVTJUL

;;;Master Adjustment Check
						MOVE	OLRN TO NADJFLD
						REP	ZFILL IN NADJFLD
						CALL	NADJKEY
						if (AP2 <= C0)
							COMPARE	ASPAYAD1,NUM1022
							if NOT EQUAL
								pack taskname,"The Maser AP1 Adj: ",ASPAYAD1," Does not Equal the detail Adj: ",NUM1022," For LR: ",OLRN
								call emailtrouble
							endif
						else
							COMPARE	ASPAYAD2,NUM1022
							if NOT EQUAL
								pack taskname,"The Maser AP2 Adj: ",ASPAYAD2," Does not Equal the detail Adj: ",NUM1022," For LR: ",OLRN
								call emailtrouble
							endif
						endif
;;
;endpatch
;do not allow for neg
						return if (ap1 < 0)


.IncomePart
.For Fiscal
					If (FiscMonth <> c1)
						If (fiscmonth = 2)
							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
						elseif (fiscmonth = 3) 
							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
						elseif (fiscmonth = 4) 
							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
						elseif (fiscmonth = 5) 
							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
						elseif (fiscmonth = 6) 
							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
						elseif (fiscmonth = 7) 
							move mm to n2
							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
						elseif (fiscmonth = 8) 
							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
						elseif (fiscmonth = 9) 
							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
						elseif (fiscmonth = 10) 
							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
						elseif (fiscmonth = 11) 
							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
						elseif (fiscmonth = 12) 
							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
						endif
;						If (fiscmonth = 2)
;							load	MM,n2,"02","03","04","05","06","07","08","09","10","11","12","01"
;						elseif (fiscmonth = 3) 
;							load	MM,n2,"03","04","05","06","07","08","09","10","11","12","01","02"
;						elseif (fiscmonth = 4) 
;							load	MM,n2,"04","05","06","07","08","09","10","11","12","01","02","03"
;						elseif (fiscmonth = 5) 
;							load	MM,n2,"05","06","07","08","09","10","11","12","01","02","03","04"
;						elseif (fiscmonth = 6) 
;							load	MM,n2,"06","07","08","09","10","11","12","01","02","03","04","05"
;						elseif (fiscmonth = 7) 
;							move mm to n2
;							load	MM,n2,"07","08","09","10","11","12","01","02","03","04","05","06"
;						elseif (fiscmonth = 8) 
;							load	MM,n2,"08","09","10","11","12","01","02","03","04","05","06","07"
;						elseif (fiscmonth = 9) 
;							load	MM,n2,"09","10","11","12","01","02","03","04","05","06","07","08"
;						elseif (fiscmonth = 10) 
;							load	MM,n2,"10","11","12","01","02","03","04","05","06","07","08","09"
;						elseif (fiscmonth = 11) 
;							load	MM,n2,"11","12","01","02","03","04","05","06","07","08","09","10"
;						elseif (fiscmonth = 12) 
;							load	MM,n2,"12","01","02","03","04","05","06","07","08","09","10","11"
;						endif
					EndIf
						move mm to n2
;do not allow for neg
				return if (ap1 < 0)
.Patch1.3
				return if (Juldays > TODAYIS)
.Patch1.3
					if (Juldays >= BegFiscCur & Juldays  <=  EndFiscCur)
						clear dumqty
						clear n9
						clear str15
						load	num102,n2,AP1JAN1,AP1FEB1,AP1MAR1,AP1APR1,AP1MAY1,AP1JUN1,AP1JUL1,AP1AUG1,AP1SEP1,AP1OCT1,AP1NOV1,AP1DEC1	
						add ap1 to num102
						add	ap1 to AP1TOT1
						store	num102,n2,AP1JAN1,AP1FEB1,AP1MAR1,AP1APR1,AP1MAY1,AP1JUN1,AP1JUL1,AP1AUG1,AP1SEP1,AP1OCT1,AP1NOV1,AP1DEC1	
					elseif ((Juldays >= BegFiscPrev) & (Juldays  <=  EndFiscPrev))
						move mm to n2
							clear dumqty
							clear n9
							clear str15
							load	num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2	
							add ap1 to num102
							add	ap1 to AP1TOT2
							store	num102,n2,AP1JAN2,AP1FEB2,AP1MAR2,AP1APR2,AP1MAY2,AP1JUN2,AP1JUL2,AP1AUG2,AP1SEP2,AP1OCT2,AP1NOV2,AP1DEC2	
					endif

;;;
				Endif
			Endif
		Endif
		return
CLEARVARS
	Clear BegFiscCur	 	
	Clear EndFiscCur	 	
	Clear BegFiscPrev	 	
	Clear EndFiscPrev	 	

	Clear JAN1	 	 
	Clear JAN2	 	 
	Clear FEB1	 	 
	Clear FEB2	 	 
	Clear MAR1	 	 
	Clear MAR2	 	 
	Clear APR1	 	 
	Clear APR2	 	 
	Clear MAY1	 	 
	Clear MAY2	 	 
	Clear JUN1	 	 
	Clear JUN2	 	 
	Clear JUL1	 	 
	Clear JUL2	 	 
	Clear AUG1	 	 
	Clear AUG2	 	 
	Clear SEP1	 	 
	Clear SEP2	 	 
	Clear OCT1	 	 
	Clear OCT2	 	 
	Clear NOV1	 	 
	Clear NOV2	 	 
	Clear DEC1	 	 
	Clear DEC2	 	 
	Clear OrdTOT1	 	 
	Clear OrdTOT2	 	 
	Clear JANRENT1	 	 
	Clear JANEXCH1	 	 
	Clear JANRENT2	 	 
	Clear JANEXCH2	 	 
	Clear FEBRENT1	 	 
	Clear FEBEXCH1	 	 
	Clear FEBRENT2	 	 
	Clear FEBEXCH2	 	 
	Clear MARRENT1	 	 
	Clear MAREXCH1	 	 
	Clear MARRENT2	 	 
	Clear MAREXCH2	 	 
	Clear APRRENT1	 	 
	Clear APREXCH1	 	 
	Clear APRRENT2	 	 
	Clear APREXCH2	 	 
	Clear MAYRENT1	 	 
	Clear MAYEXCH1	 	 
	Clear MAYRENT2	 	 
	Clear MAYEXCH2	 	 
	Clear JUNRENT2	 	 
	Clear JUNEXCH2	 	 
	Clear JUNRENT1	 	 
	Clear JUNEXCH1	 	 
	Clear JULRENT2	 	 
	Clear JULEXCH2	 	 
	Clear JULRENT1	 	 
	Clear JULEXCH1	 	 
	Clear AUGRENT2	 	 
	Clear AUGEXCH2	 	 
	Clear AUGRENT1	 	 
	Clear AUGEXCH1	 	 
	Clear SEPRENT2	 	 
	Clear SEPEXCH2	 	 
	Clear SEPRENT1	 	 
	Clear SEPEXCH1	 	 
	Clear OCTRENT2	 	 
	Clear OCTEXCH2	 	 
	Clear OCTRENT1	 	 
	Clear OCTEXCH1	 	 
	Clear NOVRENT2	 	 
	Clear NOVEXCH2	 	 
	Clear NOVRENT1	 	 
	Clear NOVEXCH1	 	 
	Clear DECRENT1	 	 
	Clear DECEXCH1	 	 
	Clear DECRENT2	 	 
	Clear DECEXCH2	 	 
	Clear EXCHTOT1	 	 
	Clear RENTTOT1	 	 
	Clear EXCHTOT2	 	 
	Clear RENTTOT2	 	 
;AP1
	Clear AP1JAN1	 	
	Clear AP1JAN2	 	 
	Clear AP1FEB1	 	 
	Clear AP1FEB2	 	 
	Clear AP1MAR1	 	 
	Clear AP1MAR2	 	 
	Clear AP1APR1	 	 
	Clear AP1APR2	 	 
	Clear AP1MAY1	 	 
	Clear AP1MAY2	 	 
	Clear AP1JUN1	 	 
	Clear AP1JUN2	 	 
	Clear AP1JUL1	 	 
	Clear AP1JUL2	 	 
	Clear AP1AUG1	 	 
	Clear AP1AUG2	 	 
	Clear AP1SEP1	 	 
	Clear AP1SEP2	 	 
	Clear AP1OCT1	 	 
	Clear AP1OCT2	 	 
	Clear AP1NOV1	 	 
	Clear AP1NOV2	 	 
	Clear AP1DEC1	 	 
	Clear AP1DEC2	 	 
	Clear AP1TOT1	 	 
	Clear AP1TOT2	 	 

;Proj
	Clear ProjJAN1	 	 
	Clear ProjJAN2	 	 
	Clear ProjFEB1	 	 
	Clear ProjFEB2	 	 
	Clear ProjMAR1	 	 
	Clear ProjMAR2	 	 
	Clear ProjAPR1	 	 
	Clear ProjAPR2	 	 
	Clear ProjMAY1	 	 
	Clear ProjMAY2	 	 
	Clear ProjJUN1	 	 
	Clear ProjJUN2	 	 
	Clear ProjJUL1	 	 
	Clear ProjJUL2	 	 
	Clear ProjAUG1	 	 
	Clear ProjAUG2	 	 
	Clear ProjSEP1	 	 
	Clear ProjSEP2	 	 
	Clear ProjOCT1	 	 
	Clear ProjOCT2	 	 
	Clear ProjNOV1	 	 
	Clear ProjNOV2	 	 
	Clear ProjDEC1	 	 
	Clear ProjDEC2	 	 
	Clear ProjTOT1	 	 
	Clear ProjTOT2	 	 
;Var
	Clear VarJAN1	 	 
	Clear VarJAN2	 	 
	Clear VarFEB1	 	 
	Clear VarFEB2	 	 
	Clear VarMAR1	 	 
	Clear VarMAR2	 	 
	Clear VarAPR1	 	 
	Clear VarAPR2	 	 
	Clear VarMAY1	 	 
	Clear VarMAY2	 	 
	Clear VarJUN1	 	 
	Clear VarJUN2	 	 
	Clear VarJUL1	 	 
	Clear VarJUL2	 	 
	Clear VarAUG1	 	 
	Clear VarAUG2	 	 
	Clear VarSEP1	 	 
	Clear VarSEP2	 	 
	Clear VarOCT1	 	 
	Clear VarOCT2	 	 
	Clear VarNOV1	 	 
	Clear VarNOV2	 	 
	Clear VarDEC1	 	 
	Clear VarDEC2	 	 
	Clear VarTOT1	 	 
	Clear VarTOT2	 	 
	CLEAR	OPENPAY
	CLEAR	DUMQTY	
	CLEAR	TMPVAR	
	CLEAR	RQTY	
	CLEAR	EXQTY	
	CLEAR	NUM102	
	CLEAR	AP1TOT	
	CLEAR	NUM1021	
	CLEAR	NUM1022	
	CLEAR	FISCMONTH 
	clear	totboxrow
	clear	totboxheight
	RETURN
OpenPrintFile
.patch1.5
.>Patch 2.1 Logic Addition for PDF Quality Control
			call	"GU$INI;WRITE_TO_INI" USING "c:\progra~1\pdf995\res\pdf995.ini":
			"Parameters":
			"ProcessPDF":
			"\\nts0\c\apps\plb\code\pdftest.bat":
			result
			if (result = C0)
.Prepare Flag file
				prep	tempfile,"c:\progra~1\pdf995\flag.dat"
				write	tempfile,SEQ;"flag set"
				close	tempfile
			endif
.>Patch 2.1 Logic Addition for PDF Quality Control
	bump timestamp,8
	pack	PRINTNAME,LNUM,timestamp
	pack	prtfilename with "c:\work\pdf\",printname,".lst"
	PRTOPEN MONTHINCREPORT,"PDF995",PRINTNAME,NOPRINT,SPOOLFILE=PRTFILENAME
	PRTPAGE MONTHINCREPORT;*UNITS=*HIENGLISH:
                          *ORIENT=*LANDSCAPE;
;EndPatch1.5
	return
.>patch2.1	
.>Patch 2.3
GetUser
	move      c1 to nusepath
	clock	  port to str3
	unpack    str3 into str2,str1
	pack      str3 from str1,str2
	MOVE      str3 TO NUSEFLD .removed FOR TESTING only
	REP       ZFILL IN NUSEFLD
	CALL      NUSEKEY
	goto      userng if over
	scan      "INVALID" in nuseuser
	goto      userng if equal
	return
.>Patch 2.3
userng
	clear	taskname
	append	"I'm sorry I've lost track of who you are,",taskname
	append	NewLine,taskname
	append	"Please leave the program and try again!",taskname
	reset	taskname
	alert	caution,taskname,result
	shutdown
   stop
.>patch2.1


EmailTrouble
                    move    "Houston We May have a problem",SmtpSubject Subject
;.   Set the text message that is send with the attachments
                    move    Taskname,SmtpTextMessage(1)   Array <Text message >
                    move    "1",SmtpTextIndexLast                               Index to last entry in TextMessage array
                    move    "NTS4",SmtpEmailServer                   Address of email serverc
                    clear   smtpemailaddress
                    append  "dbaca",SmtpEmailAddress
                    append  "@nincal.com",SmtpEmailAddress
                    reset   smtpemailaddress
                    move    "dbaca",SmtpUserName                                User name
;   Set the destinations of the email. Max 100 (Mime spec)
                    move    smtpemailaddress,SmtpDestinations(1,1)
                    move    "dbaca",SmtpDestinations(1,2)
                    move    "1",SmtpDestIndexLast                          originators UserName
                    move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
                    clear   SmtpLogFile                                         'Clear' disables the LogFile
                    move    "1",SmtpProgress                                    Enable progress bars
;                    call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
;                    if not equal
;                            pack    Mess,"Result Code ",SmtpResult," - ",SmtpResultText,NewLine:
;                                "Status Code ",SmtpStatus," - ",SmtpStatusText
;                            move    "Error Sending Message",SmtpSubject Subject
;                            move    "0",SmtpAttIndexLast                                Index to last entry - Only 1 entry
;                            call    SmtpSend   ( 'Send' is in Smtp.Pri which is included in TestSmtp.Dbs )
;                    endif
		return
FINALRUN
	call	ENDPROGRAM
	stop

	INCLUDE	NORDIO.INC
	INCLUDE	NDATIO.INC
;compute
	INCLUDE	NSHPIO.INC
	INCLUDE	NMRGIO.INC
	INCLUDE	COMPUTE.INC
	INCLUDE	NDAT3IO.INC
   INCLUDE	nacdIO.inc

;compute
	INCLUDE	NINVACDIO.INC
	INCLUDE	NINVIO.INC
	INCLUDE	NJSTIO.INC
	INCLUDE	NADJIO.INC
.>Patch 2.1	
	INCLUDE	NUSEIO.INC	
.>Patch 2.1	
	INCLUDE	COMLOGIC.INC