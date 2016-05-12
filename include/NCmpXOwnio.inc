..............................................................................
.
. NCXOIO INCLUSION
. NIN Company- Owner XREF FILE I/O ROUTINES
.
. FILE NAME : CompXOwn
. REC LENGTH: 35 FIXED
. INDEX KEY : 1-6 (6 POSITION Company#)
. INDEX KEY : 7-10 (4 POSITION Owner#, DUPLICATES)
.
..............................................................................
.
. ENTRY POINT : NCXOKEY
. REQUIRED    : 'NCXOFLD', NCXOPATH
. RETURNED    : Company- Owner XREF RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCXOKEY BRANCH     NCXOPATH  TO NCXO1,NCXO1A
NCXO1    BRANCH    NCXOFLAG TO NCXO1B
         CALL      NCXOOPEN
NCXO1B    FILEPI    1;NCXOFILE
         READ      NCXOFILE,NCXOFLD;NCXOVARS
         RETURN
.
NCXO1A   BRANCH    NCXOFLG2 TO NCXO1C
         CALL      NCXOOPEN
NCXO1C    FILEPI    1;NCXOFIL2
         READ      NCXOFIL2,NCXOFLD2;NCXOVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCXOTST
. REQUIRED    : NCXOFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NCXOTST  BRANCH    NCXOPATH OF NCXO2,NCXO2A
NCXO2    BRANCH    NCXOFLAG TO NCXO2B
         CALL      NCXOOPEN
NCXO2B    FILEPI    1;NCXOFILE
         READ      NCXOFILE,NCXOFLD;;
         RETURN
.
NCXO2A   BRANCH    NCXOFLG2 TO NCXO2C
         CALL      NCXOOPEN
NCXO2C    FILEPI    1;NCXOFIL2
         READ      NCXOFIL2,NCXOFLD2;;
         RETURN
..............................................................................
.
. ENTRY POINT : NCXOKS
. REQUIRED    :
. RETURNED    : Company- Owner XREF RECORD
. DESCRIPTION : KEY SEQUENTIAL Company- Owner XREF FILE READ
.
NCXOKS   BRANCH    NCXOPATH OF NCXO3,NCXO3A
NCXO3    BRANCH    NCXOFLAG TO NCXO3B
         CALL      NCXOOPEN
NCXO3B    FILEPI    1;NCXOFILE
         READKS    NCXOFILE;NCXOVARS
         RETURN
.
NCXO3A   BRANCH    NCXOFLG2 TO NCXO3C
         CALL      NCXOOPEN
NCXO3C    FILEPI    1;NCXOFIL2
         READKS    NCXOFIL2;NCXOVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCXOSEQ
. REQUIRED    :
. RETURNED    : Company- Owner XREF RECORD
. DESCRIPTION : SEQUENTIAL Company- Owner XREF FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NCXOSEQ  BRANCH    NCXOPATH TO NCXO4,NCXO4A
NCXO4    BRANCH    NCXOFLAG TO NCXO4B
         CALL      NCXOOPEN
NCXO4B   FILEPI    1;NCXOFILE
         READ      NCXOFILE,SEQ;NCXOVARS
         RETURN
.
NCXO4A   BRANCH    NCXOFLG2 TO NCXO4C
         CALL      NCXOOPEN
NCXO4C   FILEPI    1;NCXOFIL2
         READ      NCXOFIL2,SEQ;NCXOVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCXOWRT
. REQUIRED    : 'NCXOFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NCXOWRT  BRANCH    NCXOFLAG TO NCXO5
         CALL      NCXOOPEN
NCXO5    FILEPI    1;NCXOFlst
         WRITE     NCXOFLst;NCXOVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCXOUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE Company- Owner XREF FILE
.
NCXOUPD  BRANCH    NCXOFLAG TO NCXO6
         CALL      NCXOOPEN
NCXO6    FILEPI    1;NCXOFlst
         UPDATE    NCXOFlst;NCXOVARS
         RETURN
..............................................................................
.
. ENTRY POINT : NCXODEL
. REQUIRED    : 'NCXOFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NCXODEL  BRANCH    NCXOFLAG TO NCXO7
         CALL      NCXOOPEN
NCXO7    FILEPI    1;NCXOFLst
         DELETE    NCXOFlst
         RETURN
...............................................................................
.
. ENTRY POINT : NCXOOPEN
. REQUIRED    : 'NCXOPATH'
. RETURNED    : 'NCXOFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN Company- Owner XREF FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NCXOOPEN
         MOVE      NCXONAM1 TO NCXONAME
         TRAP      NCXOGONE IF IO
         OPEN      NCXOFlst
         TRAPCLR   IO
         MOVE      C1 TO NCXOFLAG
         MOVE      C1 TO NCXOFLG2
         RETURN


NCXOOPN1 MOVE      NCXONAM1 TO NCXONAME
         TRAP      NCXOGONE IF IO
         OPEN      NCXOFILE,NCXONAME
         TRAPCLR   IO
         MOVE      C1 TO NCXOFLAG
         RETURN
.
NCXOOPN2 MOVE      NCXONAM2 TO NCXONAME
         TRAP      NCXOGONE IF IO
         OPEN      NCXOFIL2,NCXONAME
         TRAPCLR   IO
         MOVE      C1 TO NCXOFLG2
         RETURN
.
NCXOGONE MOVE      NCXONAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
