..............................................................................
.
. NtxtIO INCLUSION
. NIN DATACARD TEXT FILE I/O ROUTINES
.
. FILE NAME : NINtxt
. REC LENGTH: 510 VAR
. INDEX KEY : 1-9 (6 BYTE LIST NUMBER + 3 BYTE RECORD #)
.
..............................................................................
.
. ENTRY POINT : NtxtKEY
. REQUIRED    : 'NtxtFLD'
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NtxtKEY
          BRANCH    NtxtFLAG,Ntxt1
          CALL      NtxtOPEN
Ntxt1
          trap      IOMssg giving Error if IO
.         FILEPI    1;NtxtFILE
          READ      NtxtFILE,NtxtFLD;NtxtVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtTST
. REQUIRED    : NtxtFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NtxtTST
          BRANCH    NtxtFLAG,Ntxt2
          CALL      NtxtOPEN
Ntxt2
          trap      IOMssg giving Error if IO
.         FILEPI    1;NtxtFILE
          READ      NtxtFILE,NtxtFLD;;
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtKS
. REQUIRED    :
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : KEY SEQUENTIAL DATACARD TEXT FILE READ
.
NtxtKS
          BRANCH    NtxtFLAG,Ntxt3
          CALL      NtxtOPEN
Ntxt3
          trap      IOMssg giving Error if IO
.         FILEPI    1;NtxtFILE
          READKS    NtxtFILE;NtxtVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtSEQ
. REQUIRED    :
. RETURNED    : DATACARD TEXT RECORD
. DESCRIPTION : SEQUENTIAL DATACARD TEXT FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NtxtSEQ
          BRANCH    NtxtFLAG,Ntxt4
          CALL      NtxtOPEN
Ntxt4
          trap      IOMssg giving Error if IO
.         FILEPI    1;NtxtFILE
          READ      NtxtFILE,SEQ;NtxtVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtWRT
. REQUIRED    : 'NtxtFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT
.
NtxtWRT
          BRANCH    NtxtFLAG,Ntxt5
          CALL      NtxtOPEN
Ntxt5
          trap      IOMssg giving Error if IO
          FILEPI    1;NtxtFLIST
          WRITE     NtxtFLIST;NtxtVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE RECORD NUMBER FILE
.
NtxtUPD
          BRANCH    NtxtFLAG,Ntxt6
          CALL      NtxtOPEN
Ntxt6
          trap      IOMssg giving Error if IO
          FILEPI    1;NtxtFLIST
          UPDATE    NtxtFLIST;NtxtVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtDEL
. REQUIRED    : 'NtxtFLD'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI DELETE
.
NtxtDEL
          BRANCH    NtxtFLAG,Ntxt7
          CALL      NtxtOPEN
Ntxt7
          trap      IOMssg giving Error if IO
          FILEPI    1;NtxtFLIST
          DELETE    NtxtFLIST
          trapclr   IO
          RETURN

..............................................................................
. ENTRY POINT : NtxtAIM
. REQUIRED    : NtxtFLD1,NtxtFLD2
. RETURNED    : RECORD
. DESCRIPTION : AIM FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NtxtAIM
          BRANCH    NtxtFLAG,Ntxt8
          CALL      NtxtOPEN
Ntxt8   
          trap      IOMssg giving Error if IO
          READ      NtxtFLE2,NtxtFLD1,NtxtFLD2;NtxtVARS
          trapclr   IO
          RETURN

..............................................................................
.
. ENTRY POINT : NtxtKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : RECORD
. DESCRIPTION : AIM KEY GENEREIC FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NtxtKG
          BRANCH    NtxtFLAG,Ntxt10
          CALL      NtxtOPEN
Ntxt10
          trap      IOMssg giving Error if IO
          READKG    NtxtFLE2;NtxtVARS
          trapclr   IO
          RETURN

...............................................................................
.
. ENTRY POINT : NtxtOPEN
. REQUIRED    : 'NtxtFLAG'
. RETURNED    : 'NtxtFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN DATACARD TEXT FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NtxtOPEN
          TRAP      NtxtGONE giving Error if IO
          OPEN      NtxtFLIST .,EXCLUSIVE
.         OPEN      NtxtFLIST
          trapclr   IO
          MOVE      C1,NtxtFLAG
          RETURN
.
NtxtGONE
          MOVE      NtxtNAME,FILENAME
          CALL      FILEGONE
.
..............................................................................
