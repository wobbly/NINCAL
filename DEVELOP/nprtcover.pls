........................................
. Program:      Nprttemplate.PLS
. Function:     Report Template Program
. Author:       Andrew Harkins
. Orig. Date:   MAY 10, 2006
. Release:      1.0
........................................

PC      EQU     1
.Include Files
        include common.inc
        include cons.inc

release init    "1.0"   ASH 10MAY2006  DEVELOPMENT RELEASE

.Files to open
prfile  pfile
First   init    "Y"
page	form	5

pict3   pict
mss1    plform  Error
.Define Fonts to be used
font1   font
font1i  font
font2   font
font3   font
font4   font

testvar1	dim	100
testvar2	dim	100
testvar3	dim	100
testvar4	dim	100
testvar5	dim	100
testvar6	dim	100
testvar7	dim	100
testvar8	dim	100
testvar9	dim	100

testvarptr1	dim	^
testvarptr2	dim	^
testvarptr3	dim	^
testvarptr4	dim	^
testvarptr5	dim	^
testvarptr6	dim	^
testvarptr7	dim	^
testvarptr8	dim	^
testvarptr9	dim	^

        formload mss1
        create  font1,"Arial",size=12,bold
        create  font1i,"Arial",size=12,italic
        create  font2,"Arial",size=8
        create  font3,"Arial",size=9
        create  font4,"Arial",size=10


NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"

	call	GetWinVer

PrintFile
	move	"TEST ROW ONE",testvar1
	move	"TEST ROW TWO",testvar2
	move	"TEST ROW THREE",testvar3
	move	"TEST ROW FOUR",testvar4
	move	"TEST ROW FIVE",testvar5
	move	"TEST ROW SIX",testvar6
	move	"CENTERED UNIQUE REPORT NAME - FOR EASE IN ASSISTING CLIENTS",testvar7
	move	"Italicized information - 'Via facsimile'",testvar8
	move	"Andrew Harkins",testvar9


	call    OpenFile
	call    PrintCover using testvar1,testvar2,testvar3,testvar4,testvar5,testvar6,testvar7,testvar8,testvar9

        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
        shutdown


OpenFile
.Print newly sorted file
	PRTOPEN prfile,"@\\NTS0\Laser2","FAXFILE.PRN"
        RETURN

.Print Heading
PrintCover Routine testvarptr1,testvarptr2,testvarptr3,testvarptr4,testvarptr5,testvarptr6,testvarptr7,testvarptr8,testvarptr9

	move    "500",column
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        prtpage prfile;*UNITS=*HIENGLISH;
        move    "300",row
	//prtpage prfile;*Pictrect=*off,*PICT=0:2000:300:8000:NINLogo
	prtpage prfile;*Pictrect=*off,*PICT=0:1400:500:8100:NINLogo
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p500:row,*font=font4,"TO:";
.        prtpage prfile;*p5200:row,"DATE:  ";
.        prtpage prfile;*p5700:row,str10;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p700:row,testvarptr1;
        add     eightlpi,row
        prtpage prfile;*p700:row,testvarptr2;
        add     eightlpi,row
        prtpage prfile;*p700:row,testvarptr3;
        add     eightlpi,row
        prtpage prfile;*p700:row,testvarptr4;
        add     eightlpi,row
        prtpage prfile;*p700:row,testvarptr5;
        add     eightlpi,row
        prtpage prfile;*p700:row,testvarptr6;
	//Create border for address window
	//We create this after the text has been printed in order to ensure that the right border prints correctly
	prtpage prfile;*pensize=10,*RNDRECT=2000:3000:500:4500:200:200;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p4000:row,*font=font1,*boldon,*alignment=*center,testvarptr7,*alignment=*left,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*p4000:row,*font=font1i,*alignment=*center,testvarptr8,*alignment=*left;
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        call	Trim using testvarptr9
        if (testvarptr9 <> "")
		prtpage prfile;*p500:row,*font=font3,"Prepared By:";
        	add     eightlpi,row
		prtpage prfile;*p500:row,testvarptr9;
	else
		add     eightlpi,row
	endif
        add     eightlpi,row
        add     eightlpi,row
	prtpage prfile;*p500:row,*font=font3,str10;
        return

        include comlogic.inc