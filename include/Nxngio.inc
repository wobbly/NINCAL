..............................................................................
.
. NXNGIO INCLUSION
. NIN EXCHANGE MASTER FILE I/O ROUTINES
.
.
. FILE NAME : NINXNUM
. REC LENGTH: 22 FIXED
. AIM KEYS : 1-4 (4 POSITION MLR# 1)
.            5-8 (4 POSITION MLR# 2)
.
. PATCH - 05May99 ASH Added logic to trap for IO errors.
..............................................................................
.
.
. ENTRY POINT : NXNGTST
. REQUIRED    : NXNGFLD1 & 2
. RETURNED    :
. DESCRIPTION : TEST KEY
.
NXNGTST  BRANCH    NXNGFLAG TO NXNG2
         CALL      NXNGOPEN
NXNG2    trap      IOMssg giving Error if IO
         FILEPI    1;NXNGFILE
         READ      NXNGFILE,NXNGFLD1,NXNGFLD2;STR1
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NXNGSEQ
. REQUIRED    :
. RETURNED    : EXCHANGE MASTER RECORD
. DESCRIPTION : SEQUENTIAL EXCHANGE MASTER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXNGSEQ  BRANCH    NXNGFLAG TO NXNG4
         CALL      NXNGOPEN
NXNG4    trap      IOMssg giving Error if IO
          Branch    Nxnglock to Nxng4L,Nxng4r,Nxng4N
NXNG4L   FILEPI    1;NXNGFILE
         READ      NXNGFILE,SEQ;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16      Date inactivated.
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG4R 
         READLK      NXNGFILE,SEQ;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16      Date inactivated.
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG4N
         READ      NXNGFILE,SEQ;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16      Date inactivated.
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NXNGWRT
. REQUIRED    : VALID RECORD
. RETURNED    :
. DESCRIPTION : EXACT AIM KEY TXT/ISI INSERT
.
NXNGWRT  BRANCH    NXNGFLAG TO NXNG5
         CALL      NXNGOPEN
NXNG5    trap      IOMssg giving Error if IO
         FILEPI    1;NXNGFILE
         WRITE     NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NXNGUPD
. REQUIRED    : A PREVIOUS KEY READ
. RETURNED    :
. DESCRIPTION : KEY UPDATE EXCHANGE MASTER FILE
.
NXNGUPD  BRANCH    NXNGFLAG TO NXNG6
         CALL      NXNGOPEN
NXNG6    trap      IOMssg giving Error if IO
         FILEPI    1;NXNGFILE
         UPDATE    NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NXNGDEL
. REQUIRED    : PREVIOUS VALID READ
. RETURNED    :
. DESCRIPTION : EXACT AIM KEY TXT/ISI DELETE
.
NXNGDEL  BRANCH    NXNGFLAG TO NXNG7
         CALL      NXNGOPEN
NXNG7    trap      IOMssg giving Error if IO
         FILEPI    1;NXNGFILE
         DELETE    NXNGFILE
         trapclr   IO
         RETURN
...............................................................................
.
. ENTRY POINT : NXNGAIM
. REQUIRED    : XNGAKEY
. RETURNED    : EXCHANGE MASTER RECORD
. DESCRIPTION : AIM EXCHANGE MASTER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXNGAIM  BRANCH    NXNGFLAG TO NXNG9
         CALL      NXNGOPEN
NXNG9    trap      IOMssg giving Error if IO
          Branch   Nxnglock to NXNG9L,NXNG9R,NXNG9N
NXNG9L    FILEPI    1;NXNGFILE
         READ      NXNGFILE,NXNGFLD1,NXNGFLD2;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG9R
         READLK      NXNGFILE,NXNGFLD1,NXNGFLD2;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG9n
         READ      NXNGFILE,NXNGFLD1,NXNGFLD2;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NXNGKG
. REQUIRED    : VALID PREVIOUS AIM READ
. RETURNED    : EXCHANGE MASTER RECORD
. DESCRIPTION : AIM KEY GENEREIC EXCHANGE MASTER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXNGKG   BRANCH    NXNGFLAG TO NXNG10
         CALL      NXNGOPEN
NXNG10   trap      IOMssg giving Error if IO
          Branch   Nxnglock to NXNG10L,NXNG10R,NXNG10N
NXNG10L  FILEPI    1;NXNGFILE
         READKG    NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG10R
         READKGLK    NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG10N
         READKG    NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
..............................................................................
.
         IFZ      PC
. ENTRY POINT : NXNGKGP
. REQUIRED    : PREVIOUS VALID AIM READ
. RETURNED    : EXCHANGE MASTER RECORD
. DESCRIPTION : KEY GENERIC PRIOR EXCHANGE MASTER FILE READ
.               APPLICATION'S RESPONSIBILITY TO TEST FLAGS
.
NXNGKGP  BRANCH    NXNGFLAG TO NXNG11
         CALL      NXNGOPEN
NXNG11   trap      IOMssg giving Error if IO
          Branch   Nxnglock to NXNG11L,NXNG11R,NXNG11N
NXNG11L  FILEPI    1;NXNGFILE
         READKGP   NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG11R
         READKGPLK   NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
NXNG11N
         READKGP   NXNGFILE;ACCKEY:          1-8       MAILER #1/MAILER #2.
                                nxngdate:         9-16       Date inactivated..
                                ENTRY:           17-21      ENTRY NUMBER.
                                FLAG             22-22      Status "I" = Inactive.
         trapclr   IO
         RETURN
         XIF
...............................................................................
.
. ENTRY POINT : NXNGAIMT
. REQUIRED    : XNGAKEY
. RETURNED    : NADA
. DESCRIPTION : TESTS TO SEE IF RECORD EXISTS
.
NXNGAIMT BRANCH    NXNGFLAG TO NXNG12
         CALL      NXNGOPEN
NXNG12   trap      IOMssg giving Error if IO
         FILEPI    1;NXNGFILE
         READ      NXNGFILE,NXNGFLD1,NXNGFLD2;;
         trapclr   IO
         RETURN
..............................................................................
.
. ENTRY POINT : NXNGOPEN
. REQUIRED    : 'NXNGFLAG'
. RETURNED    : 'NXNGFLAG' SET TO '1' IF OPENNED
. DESCRIPTION : OPEN NIN EXCHANGE MASTER FILE
.               DISPLAY ERROR AND ABORT IF NOT ON-LINE.
.
NXNGOPEN TRAP      NXNGGONE IF IO
         OPEN      NXNGFILE,NXNGNAME
         TRAPCLR   IO
         MOVE      C1 TO NXNGFLAG
         RETURN
.
NXNGGONE MOVE      NXNGNAME TO FILENAME
         CALL      FILEGONE
.
..............................................................................
