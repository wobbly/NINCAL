..............................................................................
.
. NADJIO INCLUSION
. NIN ADJUSTMENT FILE I/O ROUTINES
.
. FILE NAME : NINADJ
.
.patch 1.1 new structure use adjvars
..............................................................................
.
. ENTRY POINT : NADJKEY
. REQUIRED    : 'NADJFLD'
. RETURNED    : MASTER ADJUSTMENT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
NADJKEY  BRANCH    NADJFLAG TO NADJ1
         CALL      NADJOPEN
NADJ1    FILEPI    1;NADJFILE
         READ      NADJFILE,NADJFLD;adjvars
         RETURN
..............................................................................
.
. ENTRY POINT : NADJTST
. REQUIRED    : 'NADJFLD'
. RETURNED    :
. DESCRIPTION : ISAM KEY TEST READ
.
NADJTST  BRANCH    NADJFLAG TO NADJ2
         CALL      NADJOPEN
NADJ2    FILEPI    1;NADJFILE
         READ      NADJFILE,NADJFLD;STR1
         RETURN
..............................................................................
.
. ENTRY POINT : NADJKS
. REQUIRED    :
. RETURNED    : MASTER ADJUSTMENT RECORD
. DESCRIPTION : KEY SEQUENTIAL MASTER ADJUSTMENT FILE READ
.
NADJKS   BRANCH    NADJFLAG TO NADJ3
         CALL      NADJOPEN
NADJ3    FILEPI    1;NADJFILE
         READKS    NADJFILE;adjvars
         RETURN
..............................................................................
.
. ENTRY POINT : NADJSEQ
. REQUIRED    :
. RETURNED    : MASTER ADJUSTMENT RECORD
. DESCRIPTION : SEQUENTIAL MASTER ADJUSTMENT FILE READ
.
NADJSEQ  BRANCH    NADJFLAG TO NADJ4
         CALL      NADJOPEN
NADJ4    FILEPI    1;NADJFILE
         READ      NADJFILE,SEQ;adjvars
         RETURN
..............................................................................
.
. ENTRY POINT : NADJWRT
. REQUIRED    : 'NADJFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NADJWRT  BRANCH    NADJFLAG TO NADJ5
         CALL      NADJOPEN
NADJ5    FILEPI    1;NADJFILE
         WRITE     NADJFILE,NADJFLD;adjvars
         RETURN
..............................................................................
.
. ENTRY POINT : NADJUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE LIST FILE
.
NADJUPD  BRANCH    NADJFLAG TO NADJ6
         CALL      NADJOPEN
NADJ6    FILEPI    1;NADJFILE
         UPDATE    NADJFILE;adjvars
         RETURN
..............................................................................
.
. ENTRY POINT : NADJDEL
. REQUIRED    : 'NADJFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NADJDEL  BRANCH    NADJFLAG TO NADJ7
         CALL      NADJOPEN
NADJ7    FILEPI    1;NADJFILE
         DELETE    NADJFILE,NADJFLD
         RETURN
...............................................................................
.
. ENTRY POINT : NADJOPEN
. REQUIRED    : 'NADJFLAG'
. RETURNED    : 'NADJFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN MASTER ADJUSTMENT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NADJOPEN TRAP      NADJGONE IF IO
         OPEN      NADJFILE,NADJNAME
         TRAPCLR   IO
         MOVE      C1 TO NADJFLAG
         RETURN
NADJGONE MOVE      NADJNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................

