......................................
.NCALDD INCLUSION
.NIN List Management Broker Call File Definition
.
.FILE NAME:  NINCAL.DAT
.REC LENGTH: 814 FIXED
.INDEX KEY:  (1) 001-012  (CALLIST + CALMLR)
.
.AAMDEX KEY: (1) 001-006  (CALLIST)
.            (2) 007-012  (CALMLR)
.            (3) 013-021  (CALBRK)
.
.         PATCH 1.2 DMB 18JUN2005 Change File Manager IP
.         PATCH 1.1 ASH 18NOV2004 Increased Mailer/Broker field
.
NCALNAME        INIT    "NINCAL"
.>Patch 1.2 Begin
CALFLIST Filelist
.NCALFILE        IFILE   KEYLEN=12,FIXED=814,Name="NINCAL.isi|20.20.30.103:502"
NCALFILE        IFILE   KEYLEN=12,FIXED=814,Name="NINCAL.isi|NINS1:502"
.NCALFIL1        AFILE   FIXED=814,Name="NINCAL.aam|20.20.30.103:502"
NCALFIL1        AFILE   FIXED=814,Name="NINCAL.aam|NINS1:502"
.NCALFILE        IFILE   KEYLEN=12,FIXED=814,Name="NINCAL.isi"
.NCALFIL1        AFILE   FIXED=814,Name="NINCAL.aam"
.NCALFILE        IFILE   KEYLEN=12,FIXED=814,Name="NINCAL.ISI"
.NCALFIL1        AFILE   FIXED=814,Name="NINCAL.AAM"
         filelistend
.>Patch 1.2 End         
.START PATCH 1.1 REPLACED LOGIC
.NCALFLD         DIM     10     CALLIST + CALMLR (Primary Index)
.NCALFLD1        DIM     9      CALLIST (Aamdex)
.NCALFLD2        DIM     7      CALMLR (Aamdex)
.NCALFLD3        DIM     10     CALBRK (Aamdex)
NCALFLD         DIM     12     CALLIST + CALMLR (Primary Index)
NCALFLD1        DIM     9      CALLIST (Aamdex)
NCALFLD2        DIM     9      CALMLR (Aamdex)
NCALFLD3        DIM     12     CALBRK (Aamdex)
.END PATCH 1.1 REPLACED LOGIC
NCALFLAG        FORM    1
NCALPATH        FORM    1
NCALLOCK        FORM    1
.
.
CALVARS         LIST
CALLIST         DIM     6       001-006 List Number - AAM/ISAM
.START PATCH 1.1 REPLACED LOGIC
.CALMLR          DIM     4       007-010 Mailer Number - AAM/ISAM
.CALBRK          DIM     7       011-017 Broker/Contact Number - AAM
CALMLR          DIM     6       007-012 Mailer Number - AAM/ISAM
CALBRK          DIM     9       013-021 Broker/Contact Number - AAM
.END PATCH 1.1 REPLACED LOGIC
CALCOMMENT      DIM     750     022-771 Comments for Account
CALDATE         DIM     8       772-779 Date of First record
CALREVDATE      DIM     8       780-787 Revision Date
CALREVINITS     DIM     3       788-790 Revision Inits
CALINITS        DIM     3       791-793 Initials of Record Creator
.begin patch dec 15 2009
CALSales        Dim     2       794-795  Sales number from Oslspern.inc    
..CALFILLER       DIM     25      790-814 Filler
.CALFILLER       DIM     21      794-814 Filler
CALFILLER       DIM     19      796-814 Filler
.end patch dec 15 2009
                LISTEND                                                                                                                        
