* *****************************************************************************
* MOANotesDD.INC CREATED 2003Dec DLH
* *****************************************************************************
.    FILE:      MOANotes      - FILE.
.  LENGTH:      507
.COMPRESS:      NONE
.    TYPE:      ISAM
.    KEYS:      1)TRANSNUM              1-7
...............................................................................
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
MOANotesFile    IFILE          keylen=7,Variable=507
MoaNotesVars    List
MoaNoteskey     dim            7    moa record number
MOANotes        Dim           500
                ListEnd
.
MoaNotesfld    dim            7
moaNotesFlag   Form            1
.START PATCH 01/20/2005 ASH REPLACED LOGIC
.MOANotesName   INIT           "\\nins1\e\data\index\MOANotes"
MOANotesName   INIT           "MOANotes"
.END PATCH 01/20/2005 ASH REPLACED LOGIC
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++