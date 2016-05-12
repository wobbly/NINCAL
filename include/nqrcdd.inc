******************************************************
*  NQRCDD INCLUSION
*  SUBSIDIARY FILES FOR DATACARD
*  Quick REco FILES.
* ****************************************************
* NAMES IN THE NEWS MASTER LIST recoFILES.
* ****************************************************
.
.      FILE:      NINQRC
.    LENGTH:      141
.      TYPE:      AIM,ISAM,MANAGED
. INDEX KEY:      1-10        NQRCLIST + NQRCNUM
.AIMDEX KEY:      1-6         NQRCLIST
......................................................
NQRCNAME  INIT      "NINQRC"                    .recoss     
NQRCDNAME INIT      "NINQRCD"                       .description file
.
NQRCFLIST Filelist
NQRCFILE  IFILE     KEYLEN=6,FIXED=10,Name="NINQRC.isi|NINS1:502"
          filelistend

NQRCDFLIST          Filelist
NQRCDFILE IFILE     KEYLEN=4,FIXED=49,Name="NINQRCD.isi|NINS1:502"
          filelistend
.
NQRCLOCK  FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NQRCFLD   DIM       6
NQRCFLAG  FORM      1
NQRCPATH  FORM      1

NQRCDLOCK FORM      1         0 OR 1=filepi,2=recordlock, 3=no lock
NQRCDFLD  DIM       4
NQRCDFLAG FORM      1
NQRCDPATH FORM      1
.
. 
NQRCVARS  LIST
NQRCLIST  DIM       6         1-6       LIST NUMBER
NQRCNUM   DIM       4         7-10      Reco Section NUMBER
          LISTEND

NQRCDVARS LIST
NQRCDNum  DIM       4         1-4       ID number
NQRCDdesc DIM       45        5-49      Reco Section Description
          LISTEND


.Recos
.Animal Welfare
.Catalog
.Charitable
.Children
.Civil Rights / Human Rights
.Cultural
.Environmental
.Food Banks And Missions
.Health
.Jewish
.Native American
.Outdoor Activities
.Political
.Publications and Publishing
.Public TV & Radio
.Women's Rights
