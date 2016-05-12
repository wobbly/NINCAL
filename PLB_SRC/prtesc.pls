..............................................................................
.
. Programmname :  PRTESC
.
. Erstellt von :  BTM Unternehmensberatung GmbH, Triberg
.                 St. Keller
.
. Erstellt am  :  10.02
.
. Erstellt für :
.
...............................................................................
.
. Geändert am  :
. auf Version  :  1.0.0
. Geändert von :
.
...............................................................................
.
.                 ****  Allgemeine Programmbeschreibung ****
.
. Convert string to binary code.
.
...............................................................................
.
.                 ****  Beschreibung fuer den Bediener  ****
.
. Input:	String in ASCCI code for example.
.	HexCode is given with leading "H"
.	DezCode is given with leading "D"
.	OctalCode ist given with leadin "O"
.	For Example: ESC=O033=O33 (Octal 033)
.	         or: ESC=H1B
.	         or: ESC=D27
.	For your requestet ESC sequence
.	ASCII     ESC j n  
.           Dec       27 106 n 
.           Hex       1B 6A n  
.           Oct       033 152 n
.
.	pack	string,O033,j,D01
.	Calls	"prtesc;AsciiToBinary" using string
.	print	*ll,*abson,string,*absoff,*pl
.
. Output:	String converted in binary code, ready to print
.                                             
...............................................................................
.
+        D E F I N I T I O N E N
.
$PROGVS	INIT	"PRTESC   1.0.0  10.02"
$BEREICH	INIT	"System-Modules                          "
$PROGFKT	INIT	"Convert ASCCI to binary code            "
.
wblank	init	" "
WALTAB	INIT	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
.
* .............................................................................
.
ptrDim	Dim	^	;ESC String given in ASCCI output in binary, ready to print
.
* .............................................................................
. Work fields

wdim1	dim	1	
wdim3	dim	3
wdstring	dim	250	;Workfielf for pack and unpack
.
wform1	form	1	
wform3	form	3
.
wflptr	form	5	;Lengthpointer
wffptr	form	5	;Formpointer
.
prtDim1	dim	1
wrkstring	dim	250	;Work-Field
wdescstring	dim	250	;Binary-String after conversion
.
+ .............................................................................
. Procedure
.
AsciiToBinary	routine	ptrDim

	clear	wdescstring			;clear output string
	
	loop	
	  PARSE	ptrDIM TO wrkstring using " +-z" NOSKIP	;get next sequence, separated by komma (,)
.
	  MOVELPTR  wrkstring TO wflptr
.
	  if	(wflptr < "5")	;3 or 4 character's? (less than five char)
	    unpack	  wrkstring,wdim1,wdim3		  ;perhaps Dnn, Dnnn, Hnn, Onn or Onnn
 	    REPLACE   "dDhHoO",wdim1		  ;check first digit D=decimal, H=hex, O=octal

	    If	    (wdim1 = "O")
	      unpack    wdim3,wrkstring
	      call      OctToBin using wrkstring
	    ElseIf    (wdim1 = "D")
	      unpack    wdim3,wrkstring
	      call      DezToBin using wrkstring
	    ElseIf    (wdim1 = "H")
	      unpack    wdim3,wrkstring
	      call      HexToBin using wrkstring
	    endif
	  endif
.
	  call        JustPack using wrkstring	  ;Just pack together
	  
	  bump	  ptrDim,1
	repeat	if not Eos
.
	unpack	wdescstring,ptrDim
	return
	
...............................................................................
. Octal
.
OctString	dim	^

$PRTOCT	DIM	1	Verdichtetes Byte

$PRTOTB1 DIM	   1(3),(0100),(0200),(0300)
$PRTOTB2 DIM	   1(7),(0010),(0020),(0030),(0040),(0050),(0060),(0070)
$PRTOTB3 DIM	   1(7),(001),(002),(003),(004),(005),(006),(007)
.
OctToBin	routine	OctString

	type	OctString	
	return	if not equal	;Input must be numeric 
	
	move	OctString to wform3	;General 3 characters
	move	wform3 to OctString	
	replace	" 0",OctString

	MOVE	WBLANK TO $PRTOCT

	CMOVE	000 TO $PRTOCT	;Initialize Output
.
	MOVE      OctString TO prtDim1	;Get first digit...
	MOVE      prtDim1 TO WFORM1
.
	COMPARE   "0",WFORM1
	IF        NOT EQUAL
	  COMPARE   "4",WFORM1
	  IF        LESS
	    OR        $PRTOTB1(WFORM1),$PRTOCT	;Set first zwo bit's of output char
	  ENDIF
	ENDIF
.
	BUMP      OctString,1
	MOVE      OctString TO prtDim1
	MOVE      prtDim1 TO WFORM1
.
	COMPARE   "0",WFORM1
	IF        NOT EQUAL
	  OR        $PRTOTB2(WFORM1),$PRTOCT	;set next three bits of output char
	ENDIF
.
	BUMP      OctString,1
	MOVE      OctString TO prtDim1
	MOVE      prtDim1 TO WFORM1
.
	COMPARE   "0",WFORM1
	IF        NOT EQUAL
	  OR        $PRTOTB3(WFORM1),$PRTOCT	;set next three bits of output char
	ENDIF
.
	unpack	$PrtOct,OctString
	return
.
* .............................................................................
. Dezimal
.
DezString	dim	^	;Inputstring, 1D, 1F
$prtdez	DIM	1	;Outputchar (bin)

$PRTDTB1 DIM	   1(8),(128),(64),(32),(16),(8),(4),(2),(1)
$PRTDTB2 FORM	   3(8),("128"),("64"),("32"),("16"),("8"),("4"),("2"),("1")

DezToBin	routine	DezString
	
	MOVE	WBLANK TO $prtdez
	CMOVE	000 TO $prtdez
.
	MOVE	DezString TO wflptr
.
	type	DezString
	return	if not equal
	
	move	DezString to wform3	;General 3 characters
	move	wform3 to DezString	
	replace	" 0",DezString
	
	COMPARE	"256",wflptr	;Dezimal must between 0 and 255
	return	IF NOT LESS
.
	MOVE	"1" TO WFORM1
.
	LOOP
	  COMPARE   $PRTDTB2(WFORM1),wflptr
	  IF        NOT LESS
	    OR        $PRTDTB1(WFORM1),$prtdez
	    SUBTRACT  $PRTDTB2(WFORM1) FROM wflptr
	  ENDIF
.
	  ADD       "1" TO WFORM1
.
	  COMPARE   "0",wflptr
	REPEAT    UNTIL EQUAL
.
	unpack	$PrtDez,DezString
	return
.
* .............................................................................
. Hex
.
. Inputstring: p.e.	1D
.
HexString	dim	^
$prthex	DIM	1	Verdichtetes Byte

$PRTHTB1 DIM	   1(15):
		   (0x10),(0x20),(0x30),(0x40),(0x50),(0x60),(0x70),(0x80):
		   (0x90),(0xA0),(0xB0),(0xC0),(0xD0),(0xE0),(0xF0)
.
$PRTHTB2 DIM	   1(15):
		   (0x01),(0x02),(0x03),(0x04),(0x05),(0x06),(0x07),(0x08):
		   (0x09),(0x0A),(0x0B),(0x0C),(0x0D),(0x0E),(0x0F)
.
HexToBin	Routine	HexString

	replace	"aAbBcCdDeEfF" in HexString

	MOVE      WBLANK TO $prthex
	CMOVE     000 TO $prthex
.
	movelptr	HexString to wform1
	if	(wform1 = "1")	;Input ist only one byte long
	  pack	  wdstring,"0",HexString
	  unpack	  wdstring,HexString
	else	if (Wform1 > "2")	;more than 2 bytes not possible
	  return
	endif
	
	MOVE      HexString TO prtDim1
.
	CALL      HexNum
	IF        NOT ZERO
	  OR        $PRTHTB1(wflptr),$prthex
	ENDIF
.
	BUMP      HexString,1
	MOVE      HexString TO prtDim1
.
	CALL      HexNum
	IF        NOT ZERO
	  OR        $PRTHTB2(wflptr),$prthex
	ENDIF
.
	unpack	$PrtHex,HexString
	return
.
HexNum
	TYPE      prtDim1
	IF        EQUAL
	  MOVE      prtDim1 TO wflptr
	ELSE
	  SCAN      prtDim1,WALTAB
	  MOVEFPTR  WALTAB TO wflptr
	  ADD       "9" TO wflptr
	  RESET     WALTAB,1
	ENDIF
.
	RETURN
.
* .............................................................................
. Just pack the field to output string

PackString	dim	^

JustPack	Routine	PackString
	pack	wdescstring,wdescstring,PackString
	return
	
* .................................................