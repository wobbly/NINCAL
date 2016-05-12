* *****************************************************************************
* MOANotesDD.INC CREATED 2003Dec DLH
* *****************************************************************************
.    FILE:      MOANotes      - FILE.
.  LENGTH:      507
.COMPRESS:      NONE
.    TYPE:      ISAM
.    KEYS:      1)TRANSNUM              1-7
...............................................................................
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.
. ENTRY POINT : MOANotesKEY
. REQUIRED    : 'MOANotesFLD'
. RETURNED    : DETAIL RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
MOANotesKEY    BRANCH         MOANotesFlag OF MOANotes1
               CALL           MOANotesOPEN
               
.
MOANotes1
               FILEPI         1;MOANotesFILE
               READ           MOANotesFILE,MOANotesFLD;moaNotesvars
               RETURN
...............................................................................
.
. ENTRY POINT : MOANotesTST
. REQUIRED    : MOANotesFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
MOANotesTST    BRANCH         MOANotesFlag OF MOANotes2
               CALL           MOANotesOPEN
               
.
MOANotes2
               FILEPI         1;MOANotesFILE
               READ           MOANotesFILE,MOANotesFLD;str1
               RETURN
...............................................................................
.key seq read programmers responsiblity to check for correct record
MOANotesKS     BRANCH         MOANotesFlag OF MOANotes3
               CALL           MOANotesOPEN
               
.
MOANotes3
               FILEPI         1;MOANotesFILE
               READKS         MOANotesFILE;moaNotesvars
               RETURN
...............................................................................
MOANotesSeq     BRANCH         MOANotesFlag OF MOANotes4
               CALL           MOANotesOPEN
               
.
MOANotes4
               FILEPI         1;MOANotesFILE
               READ           MOANotesFILE,Seq;moaNotesvars
               RETURN
...............................................................................
.
. ENTRY POINT : MOANotesWRT
. REQUIRED    : 'MOANotesFLD, MoaNoteskey 
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
MOANotesWrt    BRANCH         MOANotesFlag OF MOANotes5
               CALL           MOANotesOPEN
               
.
MOANotes5
               FILEPI         1;MOANotesFILE
               Write          MOANotesFILE,MOANotesFLD;moaNotesvars
               RETURN
...............................................................................
.
. ENTRY POINT : MOANotesUPD
. REQUIRED    : Previous valid read
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD BY TRANSACTION NUMBER FILE
.
MOANotesUPD    BRANCH         MOANotesFlag OF MOANotes6
               CALL           MOANotesOPEN
               
.
MOANotes6
               FILEPI         1;MOANotesFILE
               Update         MOANotesFILE;moaNotesvars
               RETURN
...............................................................................
;.DEL - requires previous valid read
MOANotesDel    BRANCH         MOANotesFlag OF MOANotes7
               CALL           MOANotesOPEN
               
.
MOANotes7
               FILEPI         1;MOANotesFILE
               Delete         MOANotesFILE,MOANotesFLD
               RETURN
...............................................................................
.MOANotesOPEN 
MOANotesOPEN   TRAP           MOANotesGONE IF IO
               OPEN           MOANotesFILE,MOANotesNAME
               TRAPCLR        IO
               MOVE           C1 TO MOANotesFLAG
               RETURN
...............................................................................
.MOANotesPrep 
MOANotesPrep   
.;               Prepare        MOANotesFILE,"\\nins1\e\data\text\MOANotes.dat","\\nts1\e\data\index\MOANotes.isi","7","507"
.;               MOVE           C1 TO MOANotesFLAG
               RETURN
...............................................................................
          
MOANotesGONE   MOVE           MOANotesNAME TO FILENAME
               GOTO           FILEGONE

