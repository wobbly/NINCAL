............................................................................
.
. PROGRAM    : Nadj0666
. DATE       : 07/19/06
. AUTHOR     : D.L. Herrick LAKE
. DESCRIPTION: PRODUCES NIN ADJUSTMENT Error REGISTER.
.
............................................................................
.
PC       EQU       0
         INCLUDE   COMMON.inc
         include   hp.inc
.
RELEASE  INIT      "1.00"    DLH 2006Jul19
.                           INITIAL RELEASE
............................................................................
         INCLUDE   CONS.inc
	INCLUDE	COMPDD.inc
	INCLUDE	CNTDD.inc
         INCLUDE   NBILDD.inc    BILL-TO
         INCLUDE   NORDDD.inc
         INCLUDE   NJSTDD.inc    ADJUSTMENT DETAIL
	INclude	Nadjdd.inc
.
.
DATE     DIM       8
PRTFLAG  FORM      1
LOCAL    INIT      "LOCAL"
PAGE     FORM      5
PRTLINES FORM      2
xfoot     form      10.2
TAR      FORM      8.2
TAP1     FORM      8.2
TAP2     FORM      8.2
TAP      FORM      8.2        TOTAL AP1+AP2
TNININC  FORM      8.2
TLRINC   FORM      8.2
TLRINC1  FORM      8.2        BROKERAGE EXCHANGE
TLRINC2  FORM      8.2        LIST MANAGEMENT EXCHANGE
TLRINC3  FORM      8.2        BROKERAGE RENTAL
TLRINC4  FORM      8.2        LIST MANAGEMENT RENTAL & TOTALS PRINT
TSTAX    FORM      7.2
TCTAX    FORM      7.2
TPOST    FORM      7.2
R11      FORM      7.2
R12      FORM      7.2
R14      FORM      7.2
R16      FORM      7.2
R20      FORM      7.2
R21      FORM      7.2
R22      FORM      7.2
R23      FORM      7.2
R24      FORM      7.2
R25AR    FORM      7.2
R25AP    FORM      7.2
R25LR    FORM      7.2
R26      FORM      7.2
R27      form      7.2
R30      FORM      7.2
R31      FORM      7.2
hund     form      "100"
form82   form      8.2
TINVQTY  FORM      9
DATEMASK INIT      "XX/XX/XX"
DATEPRT1 DIM       8
DATEPRT2 DIM       8
M1       INIT      " JANUARY 19?? "
M2       INIT      "FEBRUARY 19?? "
M3       INIT      "  MARCH 19??  "
M4       INIT      "  APRIL 19??  "
M5       INIT      "   MAY 19??   "
M6       INIT      "  JUNE 19??   "
M7       INIT      "  JULY 19??   "
M8       INIT      " AUGUST 19??  "
M9       INIT      "SEPTEMBER 19??"
M10      INIT      " OCTOBER 19?? "
M11      INIT      "NOVEMBER 19?? "
M12      INIT      "DECEMBER 19?? "
MONTH    DIM       14
PREFLAG  DIM       1
NAME1    DIM       45
NAME2    DIM       45
.
HoldLR    Dim       6
HoldINV   Dim       6
HAR       FORM      9.2       A/R
HAP1      FORM      9.2       A/P1
HAP2      FORM      9.2       A/P2
HAP       FORM      9.2       A/P1 + A/P2
HLRINC    FORM      9.2       LR INCOME
HNINC     Form	     9.2
+...........................................................................
.
         MATCH     B8 TO TODAY
         IF        EQUAL
         CALL      GETDATE
                   ELSE
         IF        EOS
         CALL      GETDATE
         ENDIF
         ENDIF
         MOVE      "Adjustment Error check" TO STITLE
         MATCH     "NADJ0666" TO PROGRAM        *CHAINED FROM DSINIT?
         IF        NOT EQUAL                    *NO
         MOVE      "NADJ0666" TO PROGRAM
         MOVE      "adjVerf" TO PRTNAME
         MOVE      "Names in the News" TO COMPNME
         ENDIF
         CALL      PAINT
.
         MOVE      "Exit" TO PF5
         TRAP      END IF F5
         CALL      FUNCDISP
DATEOK   KEYIN     *P10:10,"DATE OK? ",*T60,STR1
         CMATCH    NO TO STR1
         IF        EQUAL
         KEYIN     *P10:12,*EL,"MM/DD/YY":
                   *P10:12,*+,*ZF,*JR,MM,*DV,SLASH,*ZF,*JR,DD:
                   *DV,SLASH,*ZF,*JR,YY,*-
         PACK      DATE FROM MM,DD,YY
         PACK      TODAY FROM MM,SLASH,DD,SLASH,YY
         DISPLAY   *P10:12,TODAY,*EL
         GOTO      DATEOK
         ENDIF
         DISPLAY   *P10:10,*EL,*P10:12,*EL
         GOTO      BEGIN
GETDATE  CLOCK     DATE TO DATE
         IFNZ      PC
         MOVE      "ZZ/ZZ/ZZ" TO TODAY
         MOVE      DATE TO N6
         EDIT      N6 TO TODAY
         MOVE      C0 TO N6
         XIF
         IFZ       PC
         MOVE      DATE TO TODAY
         XIF
         RETURN
.
BEGIN    UNPACK    TODAY TO MM,STR1,DD,STR1,YY
         MOVE      MM TO N2
         LOAD      MONTH USING N2 FROM M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12
         SCAN      "??" IN MONTH
         GOTO      PRTGET IF NOT EQUAL
         BUMP      MONTH BY -1
         LENSET    MONTH
         APPEND    YY TO MONTH
         RESET     MONTH
         SETLPTR   MONTH
.
         DISPLAY   *P01:06,"Input File  : Nadjust":
                   *P01:07,"Print File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Adjust Date : "
.
.
PRTGET   MATCH     B8 TO PRTNAME
         GOTO      PRTNG IF EQUAL
         MOVE      C1 TO PRTFLAG
         MATCH     LOCAL TO PRTNAME
         GOTO      START IF EQUAL
         MOVE      C2 TO PRTFLAG
         PACK      PRTFILE WITH PDRIVE,PRTNAME
         SPLOPEN   PRTFILE
         DISPLAY   *P15:07,PRTNAME
         GOTO      START
PRTNG    KEYIN     *P01:24,*EL,"Print File answer is invalid.":
                   *P15:07,PRTNAME
         GOTO      PRTGET
.
START    
         CALL      HEADING
.
RDADJ    	Loop
	CALL      NJSTKS
         	if	Over
         	Break
         	goto	Eoj
         	endif
         ADD       C1 TO N5
         DISPLAY   *P15:08,N5:
                   *P15:09,JSTDATE
.
	If	(n5 = c1)
	move	Jstlr to Holdlr
	MOve	Jstinvno to Holdinv
	ElseIF	(Jstlr <> HoldLR)
	call	Testit
	endif
	Add	JSTAR,Har
	add	JSTAP1,HAP1
	add	JSTAP2,Hap2
	add	JSTLRINC,Hlrinc
	add	JSTNININC,HNinc
	repeat
	
.Testit          .check master	
Testit                        
	packkey	Nadjfld,Holdlr
	call	Nadjkey
	IF	OVer
	call	nomaster
	else

			if	(Har <> ASRECADJ)
			call	errorAr
			endif
			if	(Hap1 <> ASpayad1)
			call	errorAP1
			endif
			IF	(Hap2 <> ASpayad2)
			call	errorAP2
			endif
			IF	(HLRinc <> ASLRinc)
			call	errorLR
			endif
			IF	(HNINC <> ASNININC)
			call	errorNIN
			endif
	
	endif
	Move	JStlr to Holdlr
	MOve	JstINvno to Holdinv
	move	c0 to har
	move	c0 to hap1
	move	c0 to hap2
	move	c0 to hlrinc
	move	c0 to hNINc
	return
	


NoMaster
  	COMPARE   C60 TO PRTLINES
  	CALL      HEADING IF NOT LESS
         	PRINT     *01,"No Master record LR = ",HoldLR," INv = ",Holdinv
         	ADD       C1 TO PRTLINES
         	return
ErrorAR
  	COMPARE   C60 TO PRTLINES
  	CALL      HEADING IF NOT LESS
         	PRINT     *01,"AR bad ",HoldLR," master = ",asrecadj," Details = ",hAr
         	ADD       C1 TO PRTLINES
         	return
.
ErrorAp1
  	COMPARE   C60 TO PRTLINES
  	CALL      HEADING IF NOT LESS
         	PRINT     *01,"AP1 bad ",HoldLR," master = ",ASpayad1," Details = ",hAp1
         	ADD       C1 TO PRTLINES
         	return
ErrorAp2
  	COMPARE   C60 TO PRTLINES
  	CALL      HEADING IF NOT LESS
         	PRINT     *01,"AP2 bad ",HoldLR," master = ",ASpayad2," Details = ",hAp2
         	ADD       C1 TO PRTLINES
         	return
ErrorLR
  	COMPARE   C60 TO PRTLINES
  	CALL      HEADING IF NOT LESS
         	PRINT     *01,"LR bad ",HoldLR," master = ",asLRINC," Details = ",hLRinc
         	ADD       C1 TO PRTLINES
         	return

ErrorNIN
  	COMPARE   C60 TO PRTLINES
  	CALL      HEADING IF NOT LESS
         	PRINT     *01,"NIN bad ",HoldLR," master = ",asNININC," Details = ",hNINC
         	ADD       C1 TO PRTLINES
         	return
.
EOJ     
.          COMPARE   C45 TO PRTLINES
.         CALL      HEADING IF NOT LESS
         CALL      HEADING 
         BRANCH    PRTFLAG TO END
         print     hpreset
         release
         SPLCLOSE
         CALL      REMVTOF
         GOTO      END
+............................................................................
.
HEADING  ADD       C1 TO PAGE
         compare    c1 to page
         if         equal
         print      hp17ptch,hpdupl,hptop,*f
         endif
         PRINT        *f,*n,"CONFIDENTIAL":
                      *54,"NAMES IN THE NEWS":
                      *119,"DATE: ",TODAY:
                   *N,*55,"ADJUSTMENT Error REGISTER":
                      *119,"PAGE:    ",PAGE:
                   *N,*58,MONTH
                   
.END PATCH 1.12 REPLACED LOGIC
         MOVE      C8 TO PRTLINES
         RETURN
+...........................................................................
.
.START PATCH 1.11 REPLACED LOGIC
.         INCLUDE   NMLRIO.inc    MAILER
	INCLUDE	COMPIO.inc
	INCLUDE	CNTIO.inc
.END PATCH 1.11 REPLACED LOGIC
+
         INCLUDE   NBILIO.inc    BILL-TO
+
         INCLUDE   NORDIO.inc
+
         INCLUDE   NJSTIO.inc    ADJUSTMENT DETAIL
+
	INclude 	Nadjio.inc
+
         INCLUDE   COMLOGIC.inc

