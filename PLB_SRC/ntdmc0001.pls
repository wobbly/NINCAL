;NAMES IN THE NEWS CALIF. TRIPLEX TO NINCAL Notification program

PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   hp.inc
Release  init      "1.2"       DMB  31JUL2006 Update to Correct Formatting Issues with Masterfile - was not writing data to correct locations
//release  init      "1.1"       JD 27DEC2005  New format from Triplex/Donnelley
;RELEASE  INIT      "1.0"      jd192003
IN         FORM        5
APPLIED    FORM        5
first    init      "Y"
eop      form      "58"
lines    form      2
page     form      2
NUM      FORM      1
TDMCLR   DIM       6
tdmcdat  dim       10
tdmctime dim       10
INPUT    FILE      var=80
output   IfiLE      var=80,keylen=6
skipped  file      var=441
skipcnt  form      5
;begin patch 1.8
DIFF     FORM      10
fivePER  FORM      10
last     form      3
;end patch 1.8
           MOVE      "Names In The News Ca." TO COMPNME
           MOVE      "Ntdmc0001" TO PROGRAM
           MOVE      "APPLY TDMC Notification INFO" TO STITLE
           CLOCK    DATE TO TODAY
           CALL      PAINT
;START PATCH 1.93 REPLACED LOGIC
           open      output,"tdmcnotify"
           OPEN      INPUT,"TDMCnot",EXCLUSIVE
.LOOPer     READ          INPUT,SEQ;TDMCLR,str2,tdmcdat,b1,tdmctime
;START PATCH 1.1
//Patch 1.2 Change in formatting
.LOOPer     READ          INPUT,SEQ;*cdfon,str1,str1,TDMCLR,tdmctime
LOOPer     READ          INPUT,SEQ;*cdfon,str1,str1,TDMCLR,str8,tdmctime
//Patch 1.2
;end PATCH 1.1 REPLACED LOGIC
           GOTO        EOJ IF OVER
           ADD         C1 TO IN
           DISPLAY   *P10:10,"RECORDS READ : ",IN
           TYPE      tdmcLR
           GOTO      LOOPer IF NOT EQUAL
           read      output,tdmclr;str1
           goto      looper if not over
;START PATCH 1.1
			  unpack    str8 into cc,yy,mm,dd
			  pack      tdmcdat from cc,yy,dash,mm,dash,dd
;End PATCH 1.1 
//Patch 1.2 Change in formatting
	   Squeeze tdmctime,tdmctime,"PM"
	   Squeeze tdmctime,tdmctime,"AM"	   
//Patch 1.2 
           write     output,tdmclr;TDMCLR,b1,tdmcdat,b1,tdmctime

           DISPLAY   *P10:10,"records added : ",applied
           goto      looper
eoj
           stop
         INCLUDE   COMLOGIC.inc
