.NownConcert - database change
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   \\nins1\e\library\include\NownDD.inc
RELEASE  INIT      "1.0"        DLH
Reldate	Init	"2014 March 27"
. .............................................................................
Email100	Dim	100
NewFile	File	Fixed=310
.
.
DATE     DIM       8                   USED WITH CLOCK VERB.
.
. ..MISCELLANEOUS
.
ANS      DIM       1                   ANSWER
MODE     DIM       1                   INDICATES IF PASSWORD WAS VALID
LIN      FORM      2                   NUMBER USED TO BRANCH ON UPDATES
CHANGE   DIM       1                   IF A FIELD WAS MODIFIED IT HAS A 'C'
BILKEY   DIM       8                   HOLDS APPENDED MLR/CNT/BILCD FOR READ
BROKER   DIM       21                  HOLDS NIN BROKER COMPANY NAME
DMM      DIM       2                   DISPLAY
DDD      DIM       2                          OF
DYY      DIM       2                            REVISION DATE.
REaD      FORM      4
UPD      FORM      4
.
.
.
         MOVE      C1 TO NownPATH
         Prepare	NewFIle,"\\nins1\e\data\text\ninown.new|NINS1:502"
START    MOVE      "NownCONV" TO PROGRAM
         MOVE      "NINL" TO COMPNME
         MOVE      "OWNER UPDATE" TO STITLE
         MOVE      "ABORT" TO PF1
         MOVE      "EXIT" TO PF5
         call       paint
         call       funcdisp
         TRAP      ABORT IF F1
         TRAP      EXIT IF F5
READ
	Clear	OwnEmail
         CALL      Nownks
         GOTO      STOP IF OVER
         ADD       "1" TO REaD
         DISPLAY   *ES,*P10:14,"NUMBER OF RECORDS READ = ",REaD
         CALL      WRite
         GOTO      READ
Write
	Squeeze	ownemail,Email100
	Rep	":,",Email100
	Write	NewFIle,seq;OWNLOC,OWNBLK,OWNLON,OWNLONM,OWNOCPY,OWNLOSA,OWNLOCTY,OWNLOS,OWNLOZC:
			OWNNEC,OWNCTN,OWNTELE,OWNPASS,OWNRDTE,owngally,OWNTAXID,OWNFAX,ownfax2:
			OWNstat,OwnTranFlag,Email100,OwnCOMp,OwnCont,OwnCopy,OwnFIll

         ADD       "1" TO UPD
         DISPLAY   *P10:15,"NUMBER OF OWNER's Written = ",UPD
         RETURN
ABORT
.
         TRAPCLR   F1
         NORETURN
         TRAP      ABORT IF F1
         DISPLAY   *P1:24,*EL,"NOT ALL RECORDS UPDATED",*B,*W3;

EXIT
	Weof	Newfile,seq
	Close	NewFIle
STOP
         STOP
.
         INCLUDE   NownIO.inc
         INCLUDE   COMLOGIC.inc

