..PRTPAGEIO.inc
.common print subroutines that require PRTPAGEdd.inc
.create 13Sep01 DLH
:local vars
#STR4           DIM     4
#STR25          DIM     25
.............................................................................................
.Portrait_Letter_head
.requires open print device called Laser
.prints letterhead form
.programmers responsibility to restore to their font.
PortraitLTRHEAD
              PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT
              prtpage         Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
;                  *p=2607:250,*font=prtpg24B,"Names  ":
;                   *font=prtpg24I," in the News":
;                   *PENSIZE=10,*p=2232:550,*Line=5732:550:
;                   *p=2607:650,*font=prtpg10,"C    A    L    I    F    O    R    N    I    A       I    N    C.":
;                   *p=1500:10527,*font=prtpg10,"180 Grand Ave, Suite 1545, ":
;                    "Oakland, CA 94612-3799 ","·":
;                    " 415-989-3350 ","·"," Fax 415-433-7796"
.        print  033,"&l1E",033,"&a0c0R",033,"*p632x75Y":
.                    033,"(8U",033,"(s1p24.00v0s+3b5T",012,015,012,015,hpt250,"Names":
.               b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
.                 012,015,hpln4,012,015:
.               033,"(8U",033,"(s1p12.00v0s-2b5T",hpt250," C  A  L  I  F  O  R  N  I  A        I  N  C .":.
.                    "*p600x3038Y":
.                    033,"(8U",033,"(s1p10.00v0s-2b5T",012,015,hpt150,"1300 Clay Street, Oakland, CA 94612-1429 ":
.              bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"&a0c0R";
        return
.............................................................................................
.Landscape_Letter_head
.requires open print device
.prints letterhead form
.programmers responsibility to restore to their font.
LandscapeLTRHEAD
              PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
              prtpage         Laser;*Pictrect=*off,*PICT=0:1300:500:8100:NINLogo
;                   *p=3925:250,*font=prtpg24B,"Names  ":
;                   *font=prtpg24I," in the News":
;                   *PENSIZE=10,*p=3550:550,*Line=7050:550:
;                   *p=3925:650,*font=prtpg10,"C    A    L    I    F    O    R    N    I    A       I    N    C.":
;                   *p=2800:7900,*font=prtpg10,"180 Grand Ave, Suite 1545, ":
;                    "Oakland, CA 94612-3799 ","·":
;                    " 415-989-3350 ","·"," Fax 415-433-7796"
.             Print  033,"&l1E",033,"&a0c0R":
.               033,"*p2150x45Y":
.                    033,"(8U",033,"(s1p24.00v0s+3b5T",012,015,012,015,hpt400,"Names":
.               b2,033,"(8U",033,"(s1p24.00v1s-2b5T","in the News":
.               012,015,hpln5,012,015:
.               033,"(8U",033,"(s1p12.00v0s-2b5T",hpt400," C  A  L  I  F  O  R  N  I  A        I  N  C .":
.                    "*p2000x2425Y":
.                    033,"(8U",033,"(s1p6.00v0s-2b5T",hpt400,"1300 Clay Street, Oakland, CA 94612-1429 ":
.               bullet," 415-989-3350 ",bullet," Fax 415-433-7796",033,"*p0x0Y";
        return
.............................................................................................
.............................................................................................
.PortraitLogoc
.requires open print device called Laser
.prints Logo box only COLOR
.programmers responsibility to restore to their font.
PortraitLOgoc
              PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT
              prtpage         Laser;*Pictrect=*off,*PICT=0:650:500:4050:NINLogoColorbox
        return
.............................................................................................
.Landscape_Logoc
.requires open print device
.prints Logo box only COLOR
.programmers responsibility to restore to their font.
LandscapeLogoc
              PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
              prtpage         Laser;*Pictrect=*off,*PICT=0:650:500:4050:NINLogoColorbox
        return
.............................................................................................
.PortraitLogo
.requires open print device called Laser
.prints Logo box only COLOR
.programmers responsibility to restore to their font.
PortraitLOgo
              PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*PORTRAIT
              prtpage         Laser;*Pictrect=*off,*PICT=0:650:250:3800:NINLogobox
        return
.............................................................................................
.Landscape_Logoc
.requires open print device
.prints Logo box only COLOR
.programmers responsibility to restore to their font.
LandscapeLogo
              PRTPAGE     Laser;*units=*HIENGLISH,*ROWSPACE=0,*COLSPACE=0,*Overlayon,*ORIENT=*LANDSCAPE
              prtpage         Laser;*Pictrect=*off,*PICT=0:650:500:4050:NINLogobox
        return
.............................................................................................
.CopyRightPort
.requires open print device
.prints Copywrite at bottom left hand of portrait page
.Optional parameter "date" variable ie  "2001-2006" if not present uses current year
.programmers responsibility to restore to their font.
CopyRightPort Lroutine #str25
              call            Trim using #STR25
              IF              (#str25 = "")
              clock          timestamp,timestamp                         ;requires cons.inc
              unpack         timestamp,#STR4
              Move            #str4,#str25
              endif
.              prtpage        Laser;*p=1:10500,*font=PRTpg7," Names in the News ",Copyright," ",#str25
              prtpage        Laser;*p=1:10500,*font=PRTpg7," Names in the News ",CopyrightS," ",#str25
        return
.............................................................................................
.CopyRightland
.requires open print device
.prints Copywrite at bottom left hand of landscape page
.Optional parameter "date" variable ie  "2001-2006" if not present uses current year
.programmers responsibility to restore to their font.
CopyRightland Lroutine #str25
              call            Trim using #STR25
              IF              (#str25 = "")
              clock          timestamp,timestamp                         ;requires cons.inc
              unpack         timestamp,#STR4
              Move            #str4,#str25
              endif
              prtpage        Laser;*p=1:8000,*font=PRTpg7," Names in the News ",CopyrightS," ",#str25
        return

