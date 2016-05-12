;----------------------------------------------------------------
;
;   npkgdd.inc - 
;
;   Copyright(c) Names in the News Ca., Inc. 2000
;
;   Author: David Herrick
;   Created: 1/28/2002 7:43:20 AM
;   Last change: DLH 1/28/2002 7:43:20 AM
;----------------------------------------------------------------

...................................................
.
. NPKGDD INCLUSION
. NIN PACKAGE FILE DEFINITION
. FILE NAME : NINPKG.DAT
. REC LENGTH: 793 FIXED
. INDEX KEY :   (1)     001-012 (Mailer + Package Number)
.
. AAM KEYS  :   (1)     001-006 (Mailer Number)
.               (2)     007-012 (Package Number)
.               (3)     013-162 (Package Name)
.               (4)     163-187 (Package ID)
.
.PATCH 1.2	ASH	13OCT2004	INCREASED MAILER NUMBER SIZE
.PATCH 1.1	ASH	25FEB2002	ADDED FIELDS FOR MASTER PACKAGE LOGIC
.
NPKGNAME        INIT    "NINPKG"
NPKGLIST	FILELIST
NPKGFILE        IFILE   Fixed=793,NAME="NINPKG.ISI"
NPKGFLE2        AFILE   Fixed=793,NAME="NINPKG.AAM"
		FILELISTEND
NPKGNME3        INIT    "NINPKG.dat"
NPKGFLE3	    FILE	    Fixed=793   .Used for Sequential reading

.START PATCH 1.2 REPLACED LOGIC
.NPKGFLD         DIM     10	Mailer + Package Number(ISAM)
.NPKGFLD1        DIM     7       Mailer Number(AAM)
NPKGFLD         DIM     12	Mailer + Package Number(ISAM)
NPKGFLD1        DIM     9       Mailer Number(AAM)
.END PATCH 1.2 REPLACED LOGIC
NPKGFLD2        DIM     9       Package Number(AAM)
NPKGFLD3        DIM     153     Package Name(AAM)
NPKGFLD4        DIM     28      Package ID(AAM)
NPKGFLD5        DIM     9	Master Package Number(AAM)
NPKGFLAG        FORM    1
NPKGPATH        FORM    1
NPKGLOCK        FORM    1       0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.
NPKGVARS        LIST
.START PATCH 1.2 REPLACED LOGIC
.NPKGMlr	        DIM     4	001-004	Mailer Number(ISAM/AAMKey)
.NPKGNum         DIM     6	005-010	Package Number(ISAM/AAMKey)
.NPKGPName       DIM     150     011-160 Package Name(AAMKey)
.NPKGID          DIM     25      161-185 Package ID(AAMKey) ID given to Package by Source/Client
.NPKGDate	DIM	8	186-193	Package Date
.NPKGNotes	DIM	500	194-693	Package Notes
..START PATCH 1.1 REPLACED LOGIC
..NPKGFiller	DIM	100	694-793 Filler
.NPKGMaster	DIM	1	694-694	Master Package Indicator
.NPKGMastNum	DIM	6	695-700	Master Package Association Number
.NPKGFiller	DIM	93	701-793 Filler
..END PATCH 1.1 REPLACED LOGIC
NPKGMlr	        DIM     6	001-006	Mailer Number(ISAM/AAMKey)
NPKGNum         DIM     6	007-012	Package Number(ISAM/AAMKey)
NPKGPName       DIM     150     013-162 Package Name(AAMKey)
NPKGID          DIM     25      163-187 Package ID(AAMKey) ID given to Package by Source/Client
NPKGDate	DIM	8	188-195	Package Date
NPKGNotes	DIM	500	196-695	Package Notes
NPKGMaster	DIM	1	696-696	Master Package Indicator
NPKGMastNum	DIM	6	697-702	Master Package Association Number
NPKGFiller	DIM	91	703-793 Filler
.END PATCH 1.2 REPLACED LOGIC
                LISTEND
