******************************************************************************
*VARIABLE INCLUSION FOR CLIENT ESCROW FILE
******************************************************************************
*              FILE: ESCROW
*       RECORD SIZE:  
*          COMPRESS: NONE
*              TYPE: ISAM
*               KEY: 1 MLR/broker NUMBER  1-12
*                 KEY: 2 OWNER/list NUMBER  13-18,19-24
******************************************************************************
. Patch 1.1         01/10/2005 ASH Converted Mailer/Broker fields
.patch 1.2 add type of escrow DLH
.               
.START PATCH 1.1 REPLACED LOGIC
.NESCFILE  IFILE     KEYLEN=8,dup
.NESCFLE2 IFILE        KEYLEN=10,FIXED=18
..
.NESCFLAG  FORM      1
.NESCFLG2  FORM      1
.NESCPATH  FORM      1
.NESCNAME  INIT      "ESCROW  "
.NESCNME2  INIT      "ESCROW2 "
..
.NESCFLD   DIM       8
.NESCFLD2  DIM       10
..
.NESCMLR   DIM       4     1-4      MAILER
.NESCBRK   DIM       4     5-8      BROKER NUMBER
.NESCOWN   DIM       4     9-12     OWNER
.NESClist  DIM      6   13-18     list #
............................................................................
NEscFlist Filelist
NESCFILE  IFILE     KEYLEN=12,dup,Name="Escrow.ISI|NINS1:502"
NESCFLE2 IFILE         KEYLEN=12,FIXED=60,Name="Escrow2.ISI|NINS1:502"
          FILELISTEND
.         
NESCFLAG  FORM      1
NESCFLG2  FORM      1
NESCPATH  FORM      1
NESCNAME  INIT      "ESCROW  "
NESCNME2  INIT      "ESCROW2 "
.
NESCFLD             DIM       12
NESCFLD2            DIM       12        .owner & list
NESCLOck   FORM       1          0 OR 1=filepi,2=recordlock, 3=no lock
.
Nescvars            List
NESCMLR             DIM       6     1-6      MAILER
NESCBRK             DIM       6     7-12     BROKER NUMBER
NESCOWN             DIM       6    13-18     OWNER
NESClist            DIM       6    19-24     list #
NEscBDate           Dim       8    25-32          Begin date --- not yet implemented
NEscSDate           Dim       8    33-40          Stop date --- not yet implemented
.begin patch 1.2
NescType            Dim       1    41-41     '2' = Paid Monthly '1'= quarterly '3' = Acquisition
NescFill            Dim       19   42-60     
                    Listend
.end patch 1.2
.END PATCH 1.1 REPLACED LOGIC
