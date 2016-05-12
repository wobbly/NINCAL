...............................................................................
.                                                                             .
.NUSEDD.INC - VARIBLES MODULE FOR DATABUS PORT/USER IDENTIFICATION.           .
...............................................................................
. FILENAME:     NINUSER
. LENGTH:       42 FIXED
. ACCESS:       ISI
. KEY   :       1-3    (PORT NUMBER)
.              29-31   (TYPIST INITIALS)
..............................................................................
. patch 1.2 11Jan2007  DLH  Added company code
. patch 1.1 06SEP2005  ASH  Added FileList logic
. rev 22Jan2002 DLH  add  call to trim plb 8.6 appears to have a problem with trailing blanks on nt4 machines
. and dim var for NUSENAME
. REVISED 04FEB92.
.CREATED 13 JUNE 90.
....................
.START PATCH 1.1 REPLACED LOGIC
.NUSENAME  Dim       12
..NUSENAME  INIT      "NINUSER "
.NUSENAM1  INIT      "NINUSER"
.NUSENAM2  INIT      "NINUSER2"
.NUSEFILE  IFILE     KEYLEN=3,FIXED=42
NUSENAME  INIT      "NINUSER "

NUSEFLIST FILELIST
NUSEFILE  IFILE     KEYLEN=3,FIXED=42,Name="NINUSER.isi|NINS1:502"
NUSEFLE2  IFILE     KEYLEN=3,FIXED=42,Name="NINUSER2.isi|NINS1:502"
          FILELISTEND
.END PATCH 1.1 REPLACED LOGIC
NUSEFLD   DIM       3
NUSEFLD2  DIM       3
NUSEPATH  FORM      1
NUSEFLAG  FORM      1
..............................................................................
.WSNUM    DIM       3    1-3        PORT NUMBER (KEY).
.WSNAME   DIM       25   4-28       USERS NAME.
.WSINIT   DIM       2   29-30        USERS INITIALS
.START PATCH 1.1 ADDED LOGIC
NUSEVARS  LIST
.END PATCH 1.1 ADDED LOGIC
NUSENUM             DIM       3          1-3         PORT NUMBER (KEY 1)
NUSEUSER            DIM                 25         4-28        USERS NAME.
NUSEINIT            DIM                 3         29-31        USERS UNITIALS  (KEY 2)
nuseRNID            dim                 3         32-34        Net ring address (1st 3 bytes of Ip address)
NuseADPt            dim                 2         35-36        Card type ie HP
NuseComp  Dim       1         37-37     .comp ID  ' ' or 'C' for NIN, 'P' = pacific lists
nuseActive          dim       1         38-38
nusefill  dim       4         39-42
          LISTEND
HOLDNUM  DIM       3
.START PATCH 1.1 ADDED LOGIC
.begin patch 1.2
.NuseMDL   dim       6   37-42        id j2577a(EISA),J2573a(ISA),J25875A(pci)
.                                    23Feb98
.end patch 1.2
.NUSEAGID  DIM       6   32-37        AGENDA ID NUMBER.
.NUSEFILL  DIM       5   38-42        NOT USED
.WSAGENDA DIM       6   31-36        AGENDA ID NUMBER. USED TO BUILD KEY TO MES
.WSFILL   DIM       6   37-45
.
.WSKEY    DIM       3
.END PATCH 1.1 ADDED LOGIC
.
.END OF INCLUSION.

