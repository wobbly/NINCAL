*****************************************************************************
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         inc       hp.inc
         INCLUDE   NSPIDD.inc
RELEASE  INIT      "2.1"        DLH  convert Sunbelt pdf
Reldate   Init      "2013 April 23"
.RELEASE  INIT      "2.0"        DLH  convert to prtpage and pdf
.Reldate   Init      "09 June 2012"
.RELEASE  INIT      "1.2"        ASH 09AUG2004    Logo Conversion
.RELEASE  INIT      "1.1"        JD 13JUL94
.RELEASE  INIT      "1.0"       DLH   20FEB92
PAGE     FORM      2
COUNT    FORM      5
date     dim       8
.....................................
Laser     pfile
Header1   form    9
font8    font
        create  font8,"Times New Roman",size=8
Row1      Form      9
row2      FOrm      9

.
         MOVE      "NSPI0002" TO PROGRAM
         MOVE      "NINCAL" TO COMPNME
         MOVE      "SPECIAL INST. PRINT " TO STITLE
         CLOCK     DATE TO date
         move      date to today
         MOVE      "EXIT" TO PF5
         TRAP      DONE IF F5
         MOVE      C1 TO PAGE
.         splopen   ""
          move    "0",column
          move    "1000",column1
          MOve    "3500",Header1
          move    "7000",column2
          move    "7500",column3
          PACK      STR35,NTWKPATH1,"NINSPI.PDF"
          call      GetWinVer
.begin patch 2.1
.          Call      PDF995Auto
.          call      SetPDFFlag
.          PRTOPEN   Laser,"PDF995","NINSPI.PDF"
          PRTOPEN   Laser,"PDF:","c:\work\pdf\NINSPI.PDF"
.end patch 2.1

         CALL      PAINT
         CALL      FUNCDISP
.         CALL      HD0

LOOP      loop
          CALL      NSPIKS
.          GOTO      DONE IF OVER
          until     over
          ADD       C1 TO COUNT
          DISPLAY   *P10:12,"RECORDS PROCESSED ",COUNT
.         COMPARE   "67" TO N5
.         GOTO      DT0 IF LESS
          call      DT0
         repeat
          goto      Done
DT0      
          if        (count = c1)
         CALL      HD0
         endif
          COMPARE  "9900" to ROW
          call      header if not less
          prtpage Laser;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,SPINO;                 
          prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,INST1;                 
.          PRINT     *N,*5,SPINO,*10,INST1
.         ADD       C3 TO N5
          if        (inst2 = b1 or inst2 = b12)
.         CMATCH    B1 TO INST2
.         Return    IF EOS
.         MATCH     B12 TO INST2
.         Return   IF EQUAL
          add     eightlpi,row          
          add     eightlpi,row          
          add     eightlpi,row          
          Move      Row,Row1
          add     eightlpi,row1          
          return
          endif
          prtpage Laser;*pcolumn1:row1,*ALIGNMENT=*LEFT,*font=font8,*ll,INST2;                 
          add     eightlpi,row          
          add     eightlpi,row          
          add     eightlpi,row          
          Move      Row,Row1
          add     eightlpi,row1          
.          add       "135",row1

.         PRINT     *10,INST2
.         ADD       C1 TO N5
          return
         GOTO      LOOP
.START PATCH 1.2 REPLACED LOGIC
.HD0      PRINT     hp17ptch,*F,*N,*N,*1,TODAY,*30,hpbon,hpunon,"NAMES IN THE NEWS CALIFORNIA":
.                   *72,"PAGE : ",PAGE:
.                   *N,*N,*30,"SPECIAL INSTRUCTION LISTING":
.                   *N,*N,*2,"NUMBER",*10,"INSTRUCTIONS",hpboff,hpunoff
HD0
.HD0      PRINT     hp17ptch,*F,*N,*N,*1,TODAY,*30,hpbon,hpunon,"NAMES IN THE NEWS":
.                   *72,"PAGE : ",PAGE:
.                   *N,*N,*30,"SPECIAL INSTRUCTION LISTING":
.                   *N,*N,*2,"NUMBER",*10,"INSTRUCTIONS",hpboff,hpunoff
..END PATCH 1.2 REPLACED LOGIC
.         MOVE      N7 TO N5
HEADER
          Move      "200",row
          compare   c1 to page
          if        equal
          prtpage   Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*Duplex=2
          Else
          prtpage   Laser;*NEWPAGE
         endif
          clear     row

          prtpage Laser;*pColumn:row,*ALIGNMENT=*Left,*font=font8,"Names in the News";
          clock   timestamp,str8
          unpack  str8,str2,yy,mm,dd
          clear   str10
          pack    str10,mm,slash,dd,slash,str2,yy
          prtpage Laser;*p7050:row,*ALIGNMENT=*LEFT,*font=font8,"Date:",str10;        
          prtpage Laser;*pHeader1:row,*ALIGNMENT=*CENTER,*font=font8,*ll,*boldon,"Special Instructions Listing";      
          add     eightlpi,row        
          add     eightlpi,row        
          prtpage Laser;*p7400:row,*ALIGNMENT=*Left,*font=font8,*ll,"Page:",Page;                 
          add     eightlpi,row        
          add     eightlpi,row  
          prtpage Laser;*pcolumn:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"Number";                       
          prtpage Laser;*pcolumn1:row,*ALIGNMENT=*LEFT,*font=font8,*ll,"Instructions ";                        
          add     eightlpi,row  
          add     eightlpi,row          
          Move      Row,Row1
          add     eightlpi,row1  
.          add       "135",row1
         ADD       C1 TO PAGE
         RETURN

DONE 
.          PRINT     *F
          prtclose  Laser
         RELEASE
         STOP
         INCLUDE   NSPIIO.inc
         INCLUDE   COMLOGIC.inc

