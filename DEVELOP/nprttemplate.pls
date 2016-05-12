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
font2   font
font3   font
font4   font
font5   font


        formload mss1
        create  font1,"Arial",size=12,bold
        create  font2,"Arial",size=8
        create  font3,"Arial",size=9
        create  font4,"Arial",size=10
        create  font5,"Fixed",size=9


NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline.jpg"
NINLogo2 PICT
	CREATE	NINLogo2=3:13:30:50:
		"\\nts0\c\netutils\NIN logo black outline no footer.jpg"

	call	GetWinVer

PrintFile
        move    "500",column
        move    "1200",column1
        move    "2700",column2
        move    "3450",column3
        move    "4200",column4
        move    "4950",column5
        move    "5700",column6
        move    "6750",column7

	call    OrderOpenFile
	call    OrderPrintHeader
	call    OrderListHeader
	call    OrderPrintRecord
	prtpage prfile;*NEWPAGE;
	call    OrderPrintHeader
	call    OrderListHeader
	call    OrderPrintRecord

        PRTCLOSE prfile         .CLOSE AND PRINT LAST FILE
        shutdown


OrderOpenFile
.Print newly sorted file
	PRTOPEN prfile,"@\\NTS0\Laser2","FAXFILE.PRN"
        RETURN

.Print Heading
OrderPrintHeader
.Starting point for first row set here - if this changes border value needs to change!!!!!!!
        clock   timestamp,timestamp
        unpack  timestamp,CC,YY,MM,DD
        pack    str10,MM,SLASH,DD,SLASH,CC,YY
        add     C1,page
        prtpage prfile;*UNITS=*HIENGLISH;
        move    "300",row
        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page;
	//prtpage prfile;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
	if (page = 1)
		prtpage prfile;*Pictrect=*off,*PICT=0:1000:500:7800:NINLogo
	else
		prtpage prfile;*Pictrect=*off,*PICT=0:1000:500:7800:NINLogo2
	endif
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"TO:";
        prtpage prfile;*p900:row,"Andrew Harkins";
        prtpage prfile;*p5200:row,"FROM:";
        prtpage prfile;*p5700:row,"Agnes Alvarez";
        add     eightlpi,row
        prtpage prfile;*p900:row,"names in the news";
        prtpage prfile;*p5700:row,"(510) 302-4662";
        add     eightlpi,row
        prtpage prfile;*p900:row,"1300 Clay Street";
	prtpage prfile;*p5700:row,"agnesalvarez@nincal.com";
        add     eightlpi,row
	pack    taskname,"Oakland",COMMA,"CA 95612"
	prtpage prfile;*p900:row,taskname;
        prtpage prfile;*p5200:row,"FAX:";
        prtpage prfile;*p5700:row,"(510) 628-8313";
        add     eightlpi,row
	prtpage prfile;*p900:row,"(510) 302-4662";
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,"FAX:";
	prtpage prfile;*p900:row,"(415) 433-7796";
        prtpage prfile;*p5200:row,"DATE:  ";
        prtpage prfile;*p5700:row,"05/10/2006";
        add     eightlpi,row
        add     eightlpi,row
        //4000 is center of page - ALIGNMENT=CENTER will place text in center of that marker
        //This way we never have to measure length of language
        //Remember, alignment MUST be reset!!
        prtpage prfile;*p4000:row,*font=font1,*boldon,"CENTERED UNIQUE REPORT NAME - FOR EASE IN ASSISTING CLIENTS",*alignment=*left,*boldoff;
        add     eightlpi,row
        add     eightlpi,row
        return

OrderListHeader
        prtpage prfile;*pcolumn:row,*font=font3,"Optional Unique identifier if Client receives multiple copies - could be descriptive language";
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font2,"Optional additional information that may be necessary for Client";
        add     eightlpi,row
        add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font4,*boldon,"NIN##/";
        prtpage prfile;*pcolumn1:row,"Status";
        prtpage prfile;*pcolumn4:row,"Mail Date/";
        prtpage prfile;*pcolumn6:row,"Sample";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Mailer/";
        prtpage prfile;*pcolumn4:row,"Quantity";
        prtpage prfile;*pcolumn5:row,"Reco.";
        prtpage prfile;*pcolumn6:row,"Attached/";
        prtpage prfile;*pcolumn7:row,"Answer";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,"Offer/";
        prtpage prfile;*pcolumn6:row,"Description";
        add     sixlpi,row
        prtpage prfile;*pcolumn:row,*pensize=20,*line=7800:row;
        add     eightlpi,row
        return

OrderPrintRecord
.TEST FOR ENOUGH ROOM ON PAGE
        if (row >= 8637)        .Position of Largest Possible Last Record (would include 7 lines of Special Instructions)
                prtpage prfile;*NEWPAGE;
                call    OrderPrintHeader
                call    OrderListHeader
        endif
        prtpage prfile;*pcolumn:row,*font=font2,*boldoff,"Record information lined up on columns.  Bolding may be used for important information";
	add     eightlpi,row
	add     eightlpi,row
        prtpage prfile;*pcolumn:row,*font=font5,*boldoff,"Different fonts may be used if printing things like Special Instructions, which may require parsing.";
        add     eightlpi,row
        return

        include comlogic.inc