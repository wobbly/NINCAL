......................................
.NPNDDD INCLUSION
.NIN Pending, LCR & Pending Cancelled ORDER FILE DEFINITION
.
.FILE NAME:  NINPND
.REC LENGTH: 48 FIXED
.INDEX KEY:  (1) 1-3  (LR#)
.
.
NPNDNAME        INIT    "NINPND.isi|NINS1:502"
NPNDFILE        IFILE   KEYLEN=3,FIXED=48
NPNDFLD         DIM     6
NPNDFLAG        FORM    1
NPNDPATH        FORM    1
NPNDLOCK        FORM    1
.
SEQEOF2		FORM	"-4"
.
.
NPNDVARS        LIST
NPNDCODE        DIM     1       001-001 TYPE OF STATUS
."p" = pending
."x" = cancelled
."l" = LCR
NPNDSTAT        DIM     2       002-003 STATUS CODE
NPNDDESC        DIM     45      004-048 STATUS DESCRIPTION
                LISTEND
