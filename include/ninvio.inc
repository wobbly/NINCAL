..............................................................................
.
. NINVIO INCLUSION
. NIN INVOICE FILE I/O ROUTINES
.
. FILE NAME : NININV
. REC LENGTH: 300 FIXED
. See DD for RElease info
..............................................................................
.
. ENTRY POINT : NINVKEY
. REQUIRED    : 'NINVPATH', 'NINVFLD'
. RETURNED    : INVOICE RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
NINVKEY
               BRANCH         ninvpath TO ninv1a,ninv1c
Ninv1a         branch         NinvFlag to Ninv1b
               CALL           NINVOPEN
NINV1b         
               branch    nInvlock to nInv1bL,nInv1bR,nInv1bN
Ninv1bl        FILEPI         1;NINVFILE
               READ           NINVFILE,NINVFLD;INVVARS
               return
Ninv1bR
               READLK           NINVFILE,NINVFLD;INVVARS
               return
Ninv1bN
               READ           NINVFILE,NINVFLD;INVVARS
               return

Ninv1c         branch         NinvFlag to Ninv1d
               CALL           NINVOPen
NINV1d         
               branch    nInvlock to nInv1dL,nInv1dR,nInv1dN

NINV1dL        FILEPI         1;NINVfil2
               READ           NINVfil2,NINVFLD;INVVARS
               RETURN
NINV1dR 
               READLK          NINVfil2,NINVFLD;INVVARS
               RETURN
NINV1dN 
               READ          NINVfil2,NINVFLD;INVVARS
               RETURN
..............................................................................
.
. ENTRY POINT : NINVTST
. REQUIRED    : NINVFLD
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NINVTST        Branch         Ninvpath to Ninv2a,Ninv2c
Ninv2a        Branch          Ninvflag to Ninv2b
;             COMPARE        NINVPATH TO NINVFLAG
;               GOTO           NINV2 IF EQUAL
               CALL           NINVOPEN
Ninv2B                      
               FILEPI         1;NINVFILE
               READ           NINVFILE,NINVFLD;str1
               RETURN
Ninv2C        Branch          Ninvflag to Ninv2D
;             COMPARE        NINVPATH TO NINVFLAG
;               GOTO           NINV2 IF EQUAL
               CALL           NINVOPEN
               
NINV2D         FILEPI         1;NINVFIL2
               READ           NINVFIL2,NINVFLD;str1
               RETURN
..............................................................................
.
. ENTRY POINT : NINVKS
. REQUIRED    : 'NINVPATH'
. RETURNED    : INVOICE RECORD
. DESCRIPTION : KEY SEQUENTIAL INVOICE FILE READ
.
NINVKS         Branch         Ninvpath to ninv3a,ninv3c
;               COMPARE        NINVPATH TO NINVFLAG
;               GOTO           NINV3 IF EQUAL
;               CALL           NINVOPEN
;Ninv3a       Branch          Ninvflag to Ninv2b         updated 5/6/05 JD testing neom0001
Ninv3a        Branch          Ninvflag to Ninv3b
               CALL           NINVOPEN

NINV3B
               branch    nInvlock to nInv3bL,nInv3bR,nInv3bN
Ninv3bL        FILEPI         1;NINVFILE
               READKS         NINVFILE;INVVARS
               REturn
Ninv3bR        
               READKSLK       NINVFILE;INVVARS
               REturn
Ninv3bN        
               READKS         NINVFILE;INVVARS
.
               RETURN
Ninv3C         Branch         Ninvflag to Ninv3D
               CALL           NINVOPEN

NINV3D
               branch    nInvlock to nInv3dL,nInv3dR,nInv3dN
Ninv3dL        FILEPI         1;NINVfil2
               READKS         NINVFIL2;INVVARS
               REturn
Ninv3dR
               READKSLK       NINVFIL2;INVVARS
               REturn
Ninv3dN        
               READKS         NINVFIL2;INVVARS
.
               RETURN
..............................................................................
.
. ENTRY POINT : NINVSEQ
. REQUIRED    :
. RETURNED    : INVOICE RECORD
. DESCRIPTION : SEQUENTIAL INVOICE FILE READ
.
NINVSEQ        Branch         Ninvpath to Ninv4a,ninv4c
Ninv4A         BRANCH         NINVFLAG TO NINV4B
               CALL           NINVOPEN             
NINV4B          FILEPI         1;NINVFILE
               READ           NINVFILE,SEQ;INVVARS
               RETURN
Ninv4C         BRANCH         NINVFLAG TO NINV4C
               CALL           NINVOPEN             
NINV4D         FILEPI         1;NINVFIL2
               READ           NINVFIL2,SEQ;INVVARS
               RETURN
..............................................................................
.
. ENTRY POINT : NINVWRT
. REQUIRED    : 'NINVFLD', 'NINVPATH=1'
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI INSERT., ISI 2/3 INSERTS.
.
NINVWRT        MOVE           C1 TO NINVPATH
               Branch         Ninvflag to Ninv5
               CALL           NINVOPEN
NINV5          FILEPI         1;NinvFLst
               WRITE          NinvFlst;INVVARS
.
               RETURN
.
.
..............................................................................
.
. ENTRY POINT : NINVUPD
. REQUIRED    : A PREVIOUS KEY READ, 'NINVPATH'
. RETURNED    :
. DESCRIPTION : KEY UPDATE INVOICE FILE
.
NINVUPD        Branch         Ninvflag to Ninv6 
               CALL           NINVOPEN
NINV6          FILEPI         1;NINVFLst
               UPDATE         NINVFlst;INVVARS
               RETURN
.
..............................................................................
.
. ENTRY POINT : NINVDEL
. REQUIRED    : 'NINVPATH', 'NINVFLD'
. RETURNED    :
. DESCRIPTION : DELETES REcord & ALL KEYS.
.
NINVDEL        BRANCH         NINVflag TO NINV7
.
               CALL           NINVOPEN
NINV7
               Filepi         1;NinvFLst
               DELETE         NINVFLst
               RETURN
.
..............................................................................
.
. ENTRY POINT : NINVKP
. REQUIRED    : 'NINVPATH'
. RETURNED    : INVOICE RECORD
. DESCRIPTION : KEY SEQUENTIAL INVOICE FILE READ
.
NINVKP         COMPARE        NINVPATH TO NINVFLAG
               GOTO           NINV8 IF EQUAL
               CALL           NINVOPEN
NINV8          FILEPI         1;NINVFILE
               READKP         NINVFILE;INVVARS
.
               RETURN
..............................................................................
.
. ENTRY POINT : NINVAIM   
. REQUIRED    : NINVFLD1,NINVFLD2,NINVFLD3,NINVFLD4,Ninvfld5
. RETURNED    : INVOICE RECORD
. DESCRIPTION : AIM INVOICEFILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NINVAIM        Branch         Ninvflag to Ninv9
               CALL           NINVOPen
NINV9          FILEPI         1;NINvFLst
               READ           NINVFIL3,NINVFlD1,NINVFLD2,NINVFLD3,NINVFLD4,NINVFLD5;INVVARS
               RETURN
..............................................................................
.
. ENTRY POINT : NINVKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : INVOICE RECORD
. DESCRIPTION : AIM KEY GENEREIC INVOICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NINVKG         Branch         NinvFlag to Ninv10
               Call           NinvOpen
NINV10         FILEPI         1;NINVFLst
               READKG         NINVFIL3;INVVARS
               RETURN
..............................................................................
.
. ENTRY POINT : NINVKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : INVOICE RECORD
. DESCRIPTION : KEY GENERIC PRIOR INVOICE FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NINVKGP        Branch         NinvFlag to Ninv11
               CALL           NINVOPen
NINV11         FILEPI         1;NINVFLst
               READKGP        NINVFIL3;INVVARS
               RETURN
.
...............................................................................
.
. ENTRY POINT : NINVOPEN
. REQUIRED    : 'NINVPATH', 'NINVFLAG'
. RETURNED    : 'NINVFLAG' SET TO THE PATH NUMBER: 'NINVPATH', IF OPENNED.
. DESCRIPTION : OPEN NIN INVOICE FILE, DEPENDENT ON PATH NUMBER.
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
Ninvopen
               TRAP           NINVGONE IF IO
               Open           NInvFLst
               TRAPCLR        IO
               MOVE           c1 TO NINVFLAG
               RETURN
NINVGONE       MOVE           NINVNAME TO FILENAME
               CALL           FILEGONE
.
;...............................................................................
..............................................................................
.
. NINVIO INCLUSION
. NIN INVOICE Print FILE I/O ROUTINES
.
. FILE NAME : NINVOICE
. REC LENGTH: 412 FIXED
..............................................................................
.
. ENTRY POINT : PINVKEY
. REQUIRED    : 'NINVFLD' - containing LR ##
. RETURNED    : INVOICE Print RECORD
. DESCRIPTION : EXACT ISAM KEY READ
.
PINVKEY
               branch         PinvFlag to Pinv1
               CALL           PINVOPEN
PINV1          FILEPI         1;PINVFILE
               READ           PINVFile,NINVFLD;INVVARS,inits
               return
..............................................................................
.
. ENTRY POINT : PINVTST
. REQUIRED    : PINVFLD
. RETURNED    : Over flag if over  First to bytes of the record
. DESCRIPTION : TEST KEY
.
PINVTST        branch         PinvFlag to Pinv2
               CALL           PINVOPEN
PINV2          FILEPI         1;PINVFILE
               READ           PINVFILE,NINVFLD;str2
               RETURN
..............................................................................
.
. ENTRY POINT : PINVKS
. REQUIRED    : 
. RETURNED    : INVOICE Print RECORDs
. DESCRIPTION : KEY SEQUENTIAL INVOICE Print FILE READ
.
PINVKS         branch         PinvFlag to Pinv3
               CALL           PINVOPEN
PINV3          FILEPI         1;PINVFILE
               READKS         PINVFILE;INVVARS,inits
.
               RETURN
..............................................................................
.
. ENTRY POINT : PINVSEQ
. REQUIRED    :
. RETURNED    : INVOICE PRINT RECORD
. DESCRIPTION : SEQUENTIAL INVOICE PRINT FILE READ
.
PINVSEQ        BRANCH         PINVFLAG TO PINV4
               CALL           PINVOPEN
PINV4          FILEPI         1;PINVFILE
               READ           PINVFILE,SEQ;INVVARS,inits
               RETURN
..............................................................................
.
. ENTRY POINT : PINVWRT
. REQUIRED    : 'NINVFLD' containing valid LR #  all Invoice vars and typist inits
. RETURNED    :
. DESCRIPTION : EXACT ISAM KEY TXT/ISI Write
.
PINVWRT
               Branch         Pinvflag to Pinv5
               CALL           PINVOPEN
PINV5          FILEPI         1;PinvFILE
               WRITE          PinvFILE,NINVFLD;INVVARS,inits
.
               RETURN
.
.
..............................................................................
.
. ENTRY POINT : PINVUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE INVOICE Print FILE
.
PINVUPD        Branch         Pinvflag to Pinv6 
               CALL           PINVOPEN
PINV6          FILEPI         1;PINVFILE
               UPDATE         PINVFILE;INVVARS,inits
               RETURN
.
..............................................................................
.
. ENTRY POINT : PINVDEL
. REQUIRED    : Ninvfld
. RETURNED    :
. DESCRIPTION : DELETES REcord & ALL KEYS.
.
PINVDEL        Branch         Pinvflag to Pinv7
.
               CALL           PINVOPEN
PINV7
               Filepi         1;PinvFile
               DELETE         PINVFile
               RETURN
.
..............................................................................
.
. ENTRY POINT : PINVOPEN
. REQUIRED    : 
. RETURNED    : 'NINVPATH', SET IF OPENNED.
. DESCRIPTION : OPEN NIN INVOICE PRINT FILE,
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
Pinvopen
               TRAP           PINVGONE IF IO
               Open           PInvFILE,PINVNAME
               TRAPCLR        IO
               TRAP           IOmssg GIVING ERROR IF IO
               MOVE           c1 TO PINVFLAG
               RETURN
PINVGONE       MOVE           PINVNAME TO FILENAME
               CALL           FILEGONE
.
;...............................................................................

