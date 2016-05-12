..............................................................................
.
. NINVBRKGIO INCLUSION
. NIN Broker Guar letter log file
;. ............................................................................
;. 
;.     FILE:       NINVBRKGuar
;.   LENGTH:       33
;. COMPRESS:       NONE
;.     TYPE:       ISAM
;. isi  KEY:       01-06  INvoice #          DUplicates allowed
;*******************************************************************************
;*patch 1.0   09Feb2004 DLH  Broker Guar letter 1st release
;*******************************************************************************
..............................................................................
. ENTRY POINT : NINVBRKGKey
. REQUIRED    :
. RETURNED    :  RECORD
. DESCRIPTION : Isam READ
.
NINVBRKGKey    BRANCH         NINVBRKGFLAG TO NINVBRKG1
               CALL           NINVBRKGOPEN
NINVBRKG1      FILEPI         1;NINVBRKGFILE
               READ           NINVBRKGFILE,NinvBRKGfld;NinvBRKGINV:
                              NinvBRKGCC:  
                              NinvBRKGyy:  
                              NinvBRKGMM:  
                              NinvBRKGDD:  
                              NinvBRKGTime:
                              NinvBRKGLTR:
                              NinvBRKGFill
               RETURN
..............................................................................
. ENTRY POINT : NINVBRKGTST
. REQUIRED    :
. RETURNED    :  RECORD
. DESCRIPTION :  Test Isam READ
.
NINVBRKGTST    BRANCH         NINVBRKGFLAG TO NINvBrkG2
               CALL           NINVBRKGOPEN
NINVBRKG2      FILEPI         1;NINVBRKGFILE
               READ           NINVBRKGFILE,NinvBRKGFLD;;
               RETURN
..............................................................................
. ENTRY POINT : NINVBRKGKS
. REQUIRED    :
. RETURNED    :  RECORD
. DESCRIPTION : Key SEQUENTIAL READ
.
NINVBRKGKS     BRANCH         NINVBRKGFLAG TO NinvBrkg3
               CALL           NINVBRKGOPEN
NINVBRKG3      FILEPI         1;NINVBRKGFILE
               READKS         NINVBRKGFILE;NinvBRKGINV:
                              NinvBRKGCC:  
                              NinvBRKGyy:  
                              NinvBRKGMM:  
                              NinvBRKGDD:  
                              NinvBRKGTime:
                              NinvBRKGLTR:
                              NinvBRKGFill
               RETURN
..............................................................................
. ENTRY POINT : NINVBRKGSEQ
. REQUIRED    :
. RETURNED    :  RECORD
. DESCRIPTION : SEQUENTIAL READ
.
NINVBRKGSEQ    BRANCH         NINVBRKGFLAG TO NinvBrkg4
               CALL           NINVBRKGOPEN
NINVBRKG4      FILEPI         1;NINVBRKGFILE
               READ           NINVBRKGFILE,SEQ;NinvBRKGINV:
                              NinvBRKGCC:  
                              NinvBRKGyy:  
                              NinvBRKGMM:  
                              NinvBRKGDD:  
                              NinvBRKGTime:
                              NinvBRKGLTR:
                              NinvBRKGFill
               RETURN
..............................................................................
. ENTRY POINT : NINVBRKGWrt
. REQUIRED    :
. RETURNED    :  RECORD
. DESCRIPTION : Key WRite
.
NINVBRKGWRT    BRANCH         NINVBRKGFLAG TO NinvBrkg5
               CALL           NINVBRKGOPEN
NINVBRKG5      FILEPI         1;NINVBRKGFILE
               WRite          NINVBRKGFILE,NINVBRKGFLD;NinvBRKGINV:
                              NinvBRKGCC:  
                              NinvBRKGyy:  
                              NinvBRKGMM:  
                              NinvBRKGDD:  
                              NinvBRKGTime:
                              NinvBRKGLTR:
                              NinvBRKGFill
               RETURN
..............................................................................
. ENTRY POINT : NINVBRKGUpd
. REQUIRED    :  Previous valid read
. RETURNED    :  RECORD
. DESCRIPTION :  Isam Update
.
NINVBRKGUPD    BRANCH         NINVBRKGFLAG TO NinvBrkg6
               CALL           NINVBRKGOPEN
NINVBRKG6      FILEPI         1;NINVBRKGFILE
               Update         NINVBRKGFILE;NinvBRKGINV:
                              NinvBRKGCC:  
                              NinvBRKGyy:  
                              NinvBRKGMM:  
                              NinvBRKGDD:  
                              NinvBRKGTime:
                              NinvBRKGLTR:
                              NinvBRKGFill
               RETURN
..............................................................................
. ENTRY POINT : NINVBRKGDel
. REQUIRED    :  Previous valid read
. RETURNED    :  
. DESCRIPTION :  Isam Delete
.
NINVBRKGdel    BRANCH         NINVBRKGFLAG TO NinvBrkg7
               CALL           NINVBRKGOPEN
NINVBRKG7      FILEPI         1;NINVBRKGFILE
               Delete         NINVBRKGFILE,NinvBRKGfld
               RETURN
..............................................................................
.
. ENTRY POINT : NINVBRKGOPEN
. REQUIRED    : 'NINVBRKGFLAG'
. RETURNED    : 'NINVBRKGFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
..............................................................................
NINVBRKGOPEN   TRAP           NinvBrkGGONE IF IO
               OPEN           NINVBRKGFILE,NINVBRKGNAME
               TRAPCLR        IO
               MOVE           C1 TO NINVBRKGFLAG
               RETURN
..............................................................................
NINVBRKGGONE   MOVE           NINVBRKGNAME TO FILENAME
               CALL           FILEGONE
.
..............................................................................
