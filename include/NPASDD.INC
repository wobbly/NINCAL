...............................................................................
.NPASDD - DATA DESCRIPTOR
.
.FILE:  NINPASS
.
.SIZE:  68 VARIABLE,COMPRESSED
.
.INDEX:  1-6   ()
...............................................................................
NPASNAME INIT      "NINPASS.dat|NINS1:502 "
NPASFLIST FILELIST  
NPASFILE IFILE     KEYLEN=6,Fixed=68,Name="NINPASS.isi|NINS1:502"
NPASFLE1 IFILE     KEYLEN=1,Name="NINPASS1.isi|NINS1:502" 
NPASFLE2 AFILE     Fixed=68,Name="NINPASS.aam|NINS1:502"  
                    FILELISTEND   
.
NPASFLD  DIM       6            KEY
NPASFLD1 DIM           1                KEY  // for isi read  // added 2/28/06 DMS
NPASFLD2 DIM           8                KEY  // for aam read  (5+3)  // added 2/28/06 DMS
NPASFLAG FORM      1
NPASLOCK FORM      1    0 OR 1=File Locks, 2=Record Locks, 3=No Locks
.Vars used with Passwrd.plf     .Added 03/19/99 by ASH
PROGCODE DIM       1
PASSFLAG DIM       1
.End of added vars
.
NPASLIST  LIST
NPASKEY  DIM       6           1-6  PASSWORD - KEY:  "PROGCODE" + ACTUAL PASSWORD
NPASUSER DIM       10          7-16 USER ID
NPASFILL DIM       1          17-17 NOT USED.
NPASNAM1 DIM       22         18-39 FULL USER NAME.
NPASDESC DIM       21         40-60 DESCRIPTION OF PROGRAM ACCESS
NPASDATE DIM       8          61-68 DATE ENTERED/MODIFIED 'CCYYMMDD'
                    LISTEND
.
