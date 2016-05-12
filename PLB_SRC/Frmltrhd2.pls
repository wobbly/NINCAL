        include     common.inc
        include     cons.inc
        include     hp.inc
Release init        "1.0"      08Oct98 DLH conversion to PLB
* creates Letterhead as macro 9, 
* saves cursor pos, selects 0,0, sends graphics, restores cursor pos
* initial Clipper release   4/14/92
.ot     init        183
printer  Pfile
print    file
font2   font
NINLogo	PICT
	CREATE	NINLogo=3:13:30:50:
		"\\nins1\e\netutils\NIN logo black outline.jpg"
.Bullet   init	   0362     .presumes pc8 symbol set in use
.			\     \		\
.			  \	 Bullet  \
.			   \	 dec 7	   reset symbol set
.			    legal symbol set
.	splopen   "c:\work\ltrhead.pcl"
.	prepare   print,"c:\work\ltrhead.pcl"
.        splopen
.        prtopen   printer,"",""
.        PRTOPEN printer,"c:\work\ltrhead.pcl",""
.        PRTOPEN printer,"@","FAXFILE.PRN"
        	PRTOPEN printer,"\\NTS0\Laser6",str25,noprint,spoolfile="c:\work\ltrhead2.pcl"
.//start macro, set up printer
.//                          id & start macro 9
.//                         /        start macro dll.
.//                        /       /       top margin in lines
.//                       /      /         /               position to 0.0
.//                      /     /          /                /
.	print  033,"&f9yX",033,"&f0S",033,"&l1E",033,"&a0c0R":
.               033,"*p632x75Y":
.	       033,"(8U",033,"(s1p24.00v0s+3b5T",hpt250,"Names":
.               b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
.	       *l,hpln4,*l:
.               033,"(8U",033,"(s1p12.00v0s-2b5T",hpt250," C  A  L  I  F  O  R  N  I  A        I  N  C .":
.	       "*p600x3038Y":
.	       033,"(8U",033,"(s1p10.00v0s-2b5T",hpt150,"One Bush Street, San Francisco, CA 94104 ":
.               bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"*p0x0Y":
. 	         "&l3E&f1S&f1X&f10X";
.	write  print,seq;033,"&f9yX",033,"&f0S",033,"&l1E",033,"&a0c0R":
.               033,"*p632x75Y":
.	       033,"(8U",033,"(s1p24.00v0s+3b5T",012,015,012,015,hpt250,"Names":
.               b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
.                 012,015,hpln4,012,015:
.               033,"(8U",033,"(s1p12.00v0s-2b5T",hpt250," C  A  L  I  F  O  R  N  I  A        I  N  C .":
.	       "*p600x3038Y":
.	       033,"(8U",033,"(s1p10.00v0s-2b5T",012,015,hpt150,"One Bush Street, San Francisco, CA 94104 ":
.	       033,"(8U",033,"(s1p10.00v0s-2b5T",012,015,hpt150,"1300 Clay Street, Oakland, CA 94612-1429 ":
.               bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"*p0x0Y":
. 	         "&l3E&f1S&f1X&f10X";
.                     \
.                      top margin 3 lines
        close    print


      	prtpage  printer;033,"&f9yX",033,"&f0S",033,"&l1E",033,"&a0c0R":
                  033,"*p632x75Y"
;        prtpage printer;*UNITS=*HIENGLISH;
.START PATCH 1.8 - REPLACED LOGIC
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=1:15000:1:8750:PICT1
..        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9700:14700:100:8750:PICT2
.        prtpage prfile;*PICTRECT=*OFF,*PICTVIS=9400:14700:100:8750:PICT2
.        move    "300",row
.        prtpage prfile;*p7000:50,*font=font2,*uloff,"page ",page
.        add     sixlpi,row
.        add     sixlpi,row
.        add     eightlpi,row
        move    "300",row
;        prtpage printer;*p7000:50,*font=font2,*uloff,"page ",page;
.START PATCH 2.04 REPLACED LOGIC
.        prtpage prfile;*p2700:row,*font=font10,"Names";
.        prtpage printer;*font=font11,"  in the News";
;	prtpage	printer;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
.	prtpage  printer;033,"&f9yX",033,"&f0S",033,"&l1E",033,"&a0c0R":
.                 033,"*p632x75Y":
.                 033,"(8U",033,"(s1p24.00v0s+3b5T",hpt250,"Names":
.                 b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
.                 *l,hpln4,*l:
.                 033,"(8U",033,"(s1p12.00v0s-2b5T",hpt250," C  A  L  I  F  O  R  N  I  A        I  N  C .":
.	         "*p600x3038Y":
.                 033,"(8U",033,"(s1p10.00v0s-2b5T",hpt150,"One Bush Street, San Francisco, CA 94104 ":
.                 bullet," 415-989-3350 ",bullet,"Fax 415-433-7796",033,"*p0x0Y":
. 	         "&l3E&f1S&f1X&f10X";
.//End macro
.	print   "&l3E&f1S&f1X&f10X";
        prtclose   printer
.        splclose
.        release
        stop

