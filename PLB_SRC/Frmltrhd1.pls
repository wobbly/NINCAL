        include     common.inc
        include     cons.inc
        include     hp.inc
Release init        "1.0"      12Oct98 DLH conversion to PLB
* creates Landscape Letterhead as macro 10, 
* saves cursor pos, selects 0,0, sends graphics, restores cursor pos
* initial Clipper release   4/14/92
printer  Pfile
print    file
.Bullet   init	   0362     .presumes pc8 symbol set in use
	prepare   print,"c:\work\ltrhead1.pcl"
.        splopen
.        prtopen   printer,"",""
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
	write  print,seq;033,"&f10yX",033,"&f0S",033,"&l1O",033,"&l1E",033,"&a0c0R":
               033,"*p2150x45Y":
	       033,"(8U",033,"(s1p24.00v0s+3b5T",012,015,012,015,hpt400,"Names":
               b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
                012,015,hpln5,012,015:
               033,"(8U",033,"(s1p12.00v0s-2b5T",hpt400," C  A  L  I  F  O  R  N  I  A        I  N  C .":
	       "*p2000x2425Y":
	       033,"(8U",033,"(s1p6.00v0s-2b5T",hpt400,"One Bush Street, San Francisco, CA 94104 ":
               bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"*p0x0Y":
 	         "&l3E&f1S&f1X&f10X";
        close    print


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
.        prtclose   printer
.        splclose
.        release
        stop

