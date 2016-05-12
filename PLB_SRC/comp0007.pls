PC       EQU       0
         INC       COMMON.INC
         INCLUDE   CONS.INC
;         INC       NBRKDD.INC
         include   oslspern.inc
          include compdd.inc
          include cntdd.inc
         inc       hp.inc
          include   NUSEDD.INC
          include winapi.inc
release   init      "1.00"        DLH   replace all occorances of a salesperson in company & contact file
Reldate   Init      "12 November 2009"
.
prfile    pfile
userlogn dim        7
font2   font
font3   font
SYSDATE  DIM       8
DATEMASK INIT      "99/99/99"
PAGE     FORM      5
DATE     DIM       8
LINES    FORM      2
ZERO     FORM      "0"
ONE      FORM      "1"
FOUR     FORM      "4"
TEN      FORM      "10"
COUNT    FORM      5
TABLE    FORM      1
NewSales  Dim       2
OldSales  Dim       2
.
slspern  dim       25
                    pack      taskname,"\\nins1\e\data\text\company.dat,c:\work\company.srt;1-6"
                    sort      taskname
                    OPEN      COMPFILE7,"c:\work\company.srt",EXCLUSIVE

          MOVE      C1,COMPPath
          CLOCK     DATE TO DATE
          IFNZ      PC
                    MOVE      DATEMASK TO SYSDATE
                    EDIT      DATE TO SYSDATE
          XIF
          IFZ       PC
                    MOVE      DATE TO SYSDATE
          XIF
          MOVE      SYSDATE TO TODAY
          call      Trim using PROGRAM
          if (PROGRAM = "")
                    MOVE      "COMP0007" TO PROGRAM
                    move      "LOCAL" TO PRTNAME
                    move      "1",FUNC
          endif
          CALL      PAINT
          MOVE      "Names In The News"  TO COMPNME
          MOVE      "Company Change Sales Person" TO STITLE
                    create  font2,"Arial",size=8
                    create  font3,"Arial",size=8,italic
.
.
.make selection here for change
.Hardwire for now

          Move      "15",NewSales            .Inga
          MOve      "21",Oldsales

LOOP
          read      Compfile7,seq;COMPVARS
          goto      EojCOmp if over
          if        (CompContact = OldSales)
          call      Debug
          packkey   Compfld from COmpnum
          call      Compkey
          move      Newsales,CompContact
          call      Compupd
          endif
          goto      Loop


EojCOMp
                    pack      taskname,"\\nins1\e\data\text\contacts.dat,c:\work\contacts.srt;1-9"
                    sort      taskname
                    OPEN      CNCTFILE4,"c:\work\contacts.srt",EXCLUSIVE

LoopCnt
          read      CNCTFILE4,seq;COMPVARS
          goto      Eoj if over
          if        (CNCTSALES = OldSales)
          call      Debug
          packkey   CNCTFLD from CNCTCODE,CNCTID
          call      CNCTKey
          move      Newsales,CNCTSALES
          call      CNCTupd
          endif
          goto      LoopCNT


EOJ
          shutdown  "cls"
          STOP

          include   cntio.inc
          include   compio.inc
          include   NUSEIO.INC
          INCLUDE   COMLOGIC.INC