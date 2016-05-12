..............................................................................
.
. statnio INCLUSION
. Stats FILE DEFINITION
.
. FILE NAME : nstatnote
. REC LENGTH:  212 FIXED
. INDEX KEY : 
..............................................................................
.
..............................................................................
.
. ENTRY POINT : statnKEY
. REQUIRED    : 'statnFLD'
. RETURNED    : STAT NOTES RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
statnKEY branch    statnflag to statn1
         CALL      statnOPEN
statn1
	 trap	   IOMssg giving Error if IO
         branch    statnlock to statn1L,statn1R,statn1N
statn1L  FILEPI    1;statnFILE
         READ      statnFILE,statnFLD;statnVARS
	 trapclr   IO
         RETURN
statn1R
         READLK   statnFILE,statnFLD;statnVARS
	 trapclr   IO
         RETURN
statn1N
         READ      statnFILE,statnFLD;statnVARS
	 trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : statnTST
. REQUIRED    : 
. RETURNED    :
. DESCRIPTION : TEST KEY
.
statnTST branch    statnflag to statn2
         CALL      statnOPEN
statn2   FILEPI    1;statnFILE
         READ      statnFILE,statnFLD;;
         RETURN
..............................................................................
.
. ENTRY POINT : statnKS
. REQUIRED    :
. RETURNED    : STATS NOTES RECORD
. DESCRIPTION : KEY SEQUENTIAL STATS NOTES FILE READ
.
statnKs  branch    statnflag to statn3
         CALL      statnOPEN
statn3   FILEPI    1;statnFILE
         READKS    statnFILE;statnVARS
         RETURN
..............................................................................
.
. ENTRY POINT : statnSEQ
. REQUIRED    :
. RETURNED    : STATS NOTES RECORD
. DESCRIPTION : SEQUENTIAL STATS NOTES FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
statnSEQ BRANCH    statnflag TO statn4
         CALL      statnOPEN
statn4   FILEPI    1;statnFILE
         READ      statnFILE,SEQ;statnVARS
         RETURN
..............................................................................
.
. ENTRY POINT : statnWRT
. REQUIRED    :
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
statnWRT BRANCH    statnflag OF statn5
         CALL      statnOPEN
statn5
	 FILEPI    1;statnFLst
         WRITE     statnFlst;statnVARS
         RETURN
..............................................................................
.
. ENTRY POINT : statnUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE STATS NOTES FILE
.
statnUPD
         BRANCH    statnflag TO statn6
         CALL      statnOPEN
statn6
	 FILEPI    1;statnFlst
         UPDATE     statnFlst;statnVARS
         RETURN
..............................................................................
.
. ENTRY POINT : statnDEL
. REQUIRED    : 'MKEY'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
statnDEL BRANCH    statnflag TO statn7
         CALL      statnOPEN
statn7
	 FILEPI    1;statnFlst
         DELETE     statnFLst
         RETURN
..............................................................................
.
. ENTRY POINT : statnOPEN
. REQUIRED    : 'statnflag'
. RETURNED    : 'statnflag' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN STATS NOTES FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
statnOPEN
	 TRAP      statnGONE giving error IF IO
         OPEN      statnFlst
         TRAPCLR   IO
         MOVE      C1 TO statnflag
         RETURN
.
statnGONE
	 MOVE      statnNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
.END OF INCLUDE
