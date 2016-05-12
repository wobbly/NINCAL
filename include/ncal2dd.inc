......................................
.NCAL2DD INCLUSION
.NIN List Management Broker Call Detail File Definition
.
.FILE NAME:  NINCAL2.DAT
.REC LENGTH: 868 FIXED
.INDEX KEY:  (1) 001-017  (CAL2LIST + CAL2MLR + CAL2NUM)
.            (2) 018-025  (CAL2DATE) 
.            (3) 026-033  (CAL2DATE2) 
.AAMDEX KEY: (1) 001-006  (CAL2LIST)
.            (2) 007-012  (CAL2MLR)
.
.         patch 1.3 DLH 12June2012 add follow up date
.       PATCH 1.2 DMB 18JUN2005  Changed IP of File Manager
.         PATCH 1.1 ASH 18NOV2004 Increased Mailer/Broker field
.
NCAL2NAME       INIT    "NINCAL2                       "
.>Patch 1.2 Begin
CALFLIST2 Filelist
.NCAL2FILE       IFILE   KEYLEN=17,FIXED=868,Name="NINCAL2.ISI|20.20.30.103:502"
NCAL2FILE       IFILE   KEYLEN=17,FIXED=868,Name="NINCAL2.ISI|NINS1:502"
.NCAL2FIL1       AFILE   FIXED=868,Name="NINCAL2.AAM|20.20.30.103:502"
NCAL2FIL1       AFILE   FIXED=868,Name="NINCAL2.AAM|NINS1:502"
NCAL2FIL3       IFILE   KEYLEN=8,FIXED=868,Name="NINCAL3.ISI|NINS1:502"
NCAL2FIL4       IFILE   KEYLEN=8,FIXED=868,Name="NINCAL4.ISI|NINS1:502"
.NCAL2FILE       IFILE   KEYLEN=17,FIXED=868,Name="NINCAL2.ISI"
.NCAL2FIL1       AFILE   FIXED=868,Name="NINCAL2.AAM"
.NCAL2FILE       IFILE   KEYLEN=17,FIXED=868,Name="NINCAL2.ISI"
.NCAL2FIL1       AFILE   FIXED=868,Name="NINCAL2.AAM"
         filelistend
.>Patch 1.2 End      
.START PATCH 1.1 REPLACED LOGIC
.NCAL2FLD        DIM     15     CAL2LIST + CAL2MLR + CAL2NUM (Primary Index)
NCAL2FLD        DIM     17     CAL2LIST + CAL2MLR + CAL2NUM (Primary Index)
.END PATCH 1.1 REPLACED LOGIC
NCAL2FLD1       DIM     9      CAL2LIST (Aamdex)
.START PATCH 1.1 REPLACED LOGIC
.NCAL2FLD2       DIM     7      CAL2MLR (Aamdex)
NCAL2FLD2       DIM     9      CAL2MLR (Aamdex)
NCAL2FLD3       DIM     8      CAL2Date (Index)
NCAL2FLD4       DIM     8      CAL2Date2 (Index)
.END PATCH 1.1 REPLACED LOGIC
NCAL2FLAG       FORM    1
NCAL2PATH       FORM    1
NCAL2LOCK       FORM    1
.
.
CAL2VARS        LIST
CAL2LIST        DIM     6       001-006 List Number - AAM/ISAM
.START PATCH 1.1 REPLACED LOGIC
.CAL2MLR         DIM     4       007-010 Mailer Number - AAM/ISAM
CAL2MLR         DIM     6       007-012 Mailer Number - AAM/ISAM
.END PATCH 1.1 REPLACED LOGIC
CAL2NUM         DIM     5       013-017 Call Number - ISAM
CAL2DATE        DIM     8       018-025 Creation Date
CAL2DATE2       DIM     8       026-033 Call Date
CAL2INITS       DIM     3       034-036 Initials of record creator
CAL2REVDATE     DIM     8       037-043 Revision Date
CAL2REVINITS    DIM     3       044-047 Revision Inits
CAL2CNT         DIM     2       048-049 Call Sender (NIN Contact)
CAL2CNT2        DIM     45      050-094 Call Recipient
CAL2PRINT       DIM     1       095-095 Report Print Flag
CAL2COMMENT     DIM     750     096-845 Comments for Account
.START PATCH 1.1 REPLACED LOGIC
.CAL2FILLER      DIM     25      844-868 Filler
CAL2SAles       DIM     2       846-847 osales #
CAL2DATE3       DIM     8       848-855 Follow up Date
CAL2FILLER      DIM     13      856-868 Filler
.END PATCH 1.1 REPLACED LOGIC
                LISTEND                                                                                                                        
