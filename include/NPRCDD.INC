...................................................
.
. NPRCDD INCLUSION
. NIN PACKAGE PRICE FILE DEFINITION
. FILE NAME : NINPRC.DAT
. REC LENGTH: 355 FIXED
. INDEX KEY :   (1)     001-020 (Mailer + Package Number + Price Date)
.
. AAM KEYS  :   (1)     001-006 (Mailer Number)
.		(2)     007-012 (Package Number)
.
.PATCH 1.1	ASH	13OCT2004	INCREASED MAILER NUMBER SIZE
.
NPRCNAME        INIT    "NINPRC"
NPRCLIST	FILELIST
NPRCFILE        IFILE   NAME="NINPRC.ISI"
NPRCFLE2        AFILE   NAME="NINPRC.AAM"
		FILELISTEND
.START PATCH 1.1 REPLACED LOGIC
.NPRCFLD         DIM     18	Mailer + Package Number + Price Date(ISAM)
.NPRCFLD1        DIM     7       Mailer Number(AAM)
NPRCFLD         DIM     20	Mailer + Package Number + Price Date(ISAM)
NPRCFLD1        DIM     9       Mailer Number(AAM)
.END PATCH 1.1 REPLACED LOGIC
NPRCFLD2        DIM     9       Package Number(AAM)
NPRCFLAG        FORM    1
NPRCPATH        FORM    1
NPRCLOCK        FORM    1       0/1=FILEPI, 2=RECORD LOCK, 3=NO LOCK
.
NPRCVARS        LIST
.START PATCH 1.1 REPLACED LOGIC
.NPRCMlr		DIM     4	001-004	Mailer Number(ISAM)
.NPRCNum         DIM     6	005-010	Package Number(ISAM)
.NPRCDate	DIM	8	011-018 Price Date 'CCYYMMDD' (ISAM)
.NPRCPrint	FORM	6.2	019-027	Package Print Price
.NPRCPost	FORM	6.2	028-036	Package Postage Price
.NPRCPremium	FORM	6.2	037-045	Package Premium Price
.NPRCTotal	FORM	8.2	046-056	Package Total Price
.NPRCNotes	DIM	200	057-256	Price Notes
.NPRDFiller	DIM	100	257-355	Filler
NPRCMlr		DIM     6	001-006	Mailer Number(ISAM)
NPRCNum         DIM     6	007-012	Package Number(ISAM)
NPRCDate	DIM	8	013-020 Price Date 'CCYYMMDD' (ISAM)
NPRCPrint	FORM	6.2	021-029	Package Print Price
NPRCPost	FORM	6.2	030-038	Package Postage Price
NPRCPremium	FORM	6.2	039-047	Package Premium Price
NPRCTotal	FORM	8.2	048-058	Package Total Price
NPRCNotes	DIM	200	059-258	Price Notes
NPRDFiller	DIM	97	259-355	Filler
.END PATCH 1.1 REPLACED LOGIC
                LISTEND
