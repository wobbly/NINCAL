pc       equ       0
         inc       common.inc
         inc       cons.inc
.patch2.45
				include	compdd.inc
				include	cntdd.inc
.         INC       NMLRDD.INC
.patch2.45
         inc       NMOADD.INC
         INCLUDE   NMOBDD.INC
str28    dim       26 
test80   dim       82

test132  dim       132
text     dim       127
texta    dim       127
textb    dim       127
text1    dim       127
text1a    dim      127
text1b    dim      127
text2     dim      300
text3     dim      300
text4     dim      259
inFILE   file      uncomp
OUTPUT   FILE      uncomp
CHANGE   FORM      7.2         CHANGE TO BE APPLIED TO BALANCE.
input    form      8
count    form      8
ques     init      "???????????????"
release  init      "1.0"                  JD	26MAY2004	Mailer Conversion
;release  init      "pre"
.
         clock     date to today
         move      "exit" to pf5
         trap      eoj if f5
         keyin     *cl
         call      paint
         call      funcdisp
         display   *p1:24,*el,"preparing output";
         open       inFILE,"c:\work\edit08.096",exclusive
         PREPARE    OUTPUT,"C:\WORK\stripped"
         trap       eoj if range
         display   *p1:24,*el,"Lets get to it";
loop     
.         read      inFILE,seq;*abson,text,*abson,text1
.         read      infile,seq;*28,test80,*124,test132
         read      infile,seq;str28,test80,str15,test132
         goto      eoj if over
         add       c1 to input
         display   *p1:10,"input : ",input
         display   *p1:15,*el,str28
         display   *p1:16,*el,test80
         display   *p1:17,*el,str15
         display   *p1:18,*el,test132
         goto      write
         clear     texta
         clear     textb
         clear     text1a
         clear     text1b
loop1    clear     str1 
         append     text,str1
         cmatch    0014,str1
         if        equal
         display   *p1:14,*el,"its a Form feed",str1,b1,text
         bump      text
         goto      loop1x if eos
         goto      loop1
         else      
         append    str1,texta
.         display   *p1:20,*el,text
.         display   *p1:21,*el,text1
.         display   *p1:14,*el,"its ok ",texta
         bump      text
         goto      loop1x if eos
         goto      loop1
         endif
loop1x   reset     texta,0
         reset     texta
         clear     textb
loop2    clear     str1
         append    texta,str1
.         display   *p1:21,*el,str1,b1,texta
         cmatch    015,str1
         if        equal
         display   *p1:14,*el,"its a Carriage return"
         bump      texta
         goto      loop2x if eos
         goto      loop2
         else      
.         display   *p1:14,*el
         append     str1,textb
         bump      texta
         goto      loop2x if eos
         goto      loop2
         endif
loop2x   reset     textb,0
         reset     textb
         clear     text1a
loop3   
         clear     str1 
         append     text1,str1
.         display   *p1:20,*el,str1,b1,text1
         cmatch    0014,str1
         if        equal
.         display   *p1:14,*el,"its a Form feed",str1,b1,text1
         bump      text1
         goto      loop3x if eos
         goto      loop3
         else      
         append    str1,text1a
.         display   *p1:14,*el,"its ok ",textb
         bump      text1
         goto      loop3x if eos
         goto      loop3
         endif
loop3x   reset     text1a,0
         reset     text1a
         clear     text1b
loop4    clear     str1
         append    text1a,str1
         display   *p1:21,*el,str1,b1,text1a
         cmatch    015,str1
         if        equal
         display   *p1:14,*el,"its a Carriage return"
         bump      text1a
         goto      matchit if eos
         goto      loop4
         else      
         display   *p1:14,*el
         append     str1,text1b
         bump      text1a
         goto      matchit if eos
         goto      loop4
         endif
matchit  reset     textb,0
         reset     textb
         scan      "CONFIDENTIAL" in textb
         goto      loop if equal
         reset     textb
         scan      "Statement" IN textb
         GOTO      LOOP IF EQUAL
         reset     textb
         SCAN      "1995" IN textb
         GOTO      LOOP IF EQUAL
         reset     textb
         SCAN      "over 90" IN textb
         GOTO      LOOP IF EQUAL
         reset     textb
         SCAN      "RENTAL ##" IN textb
         GOTO      LOOP IF EQUAL
         reset     textb
         SCAN      "M O N E Y" IN textb
         GOTO      LOOP IF EQUAL
         reset     textb
         SCAN      "**********" IN textb
         GOTO      LOOP IF EQUAL
         reset     textb
         scan      "PAGE" in textb
         goto      loop if equal
         reset     textb
         reset     text1b,0
         reset     text1b
         scan      "RECEIVED" in text1b
         goto      loop if equal
         reset     text1b
         scan      "APPLIED" IN text1b
         GOTO      LOOP IF EQUAL
         reset     text1b
         SCAN      "TRANSACTION" IN text1b
         GOTO      LOOP IF EQUAL
         reset     text1b
         SCAN      "MLR/BRK" IN text1b
         GOTO      LOOP IF EQUAL
         reset     text1b
         SCAN      "RENTAL ##" IN text1b
         GOTO      LOOP IF EQUAL
         reset     text1b
         SCAN      "M O N E Y" IN text1b
         GOTO      LOOP IF EQUAL
         reset     text1b
         SCAN      "**********" IN text1b
         GOTO      LOOP IF EQUAL
         reset     text1b
         scan      "PAGE" in text1b
         goto      loop if equal
         reset     text1b
         scan      "REASON/C" in text1b
         goto      loop if equal
........................................................................................................

write    
         reset     text1b
         reset     textb
. .        display   *p1:21,*el,textb
. .        display   *p1:22,*el,text1b
.         match     "               " to textb
.         goto      loop if equal
.         cmatch    b1 to textb
.         goto      loop if eos
.         match     " ." to textb
.         if        not equal
.         clear     text1
.         append    " .                       " to text1         
.         append    textb to text1
.         reset     text1
.         move     text1 to textb
.         endif
.         WRITE     OUTPUT,SEQ;textb,text1b
         write     output,seq;test80,test132
         add       c1 to count
         display   *p1:12,"written: ",count
         goto     loop
eoj      WEOF     OUTPUT,SEQ
         CLOSE    OUTPUT
         stop
.         include   nMOAIO.INC
.         INCLUDE   NMOBIO.INC
.         include   nmlrio.inc

.patch2.45
				include	compio.inc
				include	cntio.inc
.         INCLUDE   NMLRIO.INC
.patch2.45
         inc       comlogic.inc                 
         
