pc       equ       0
         INCLUDE   COMMON.inc
         INCLUDE   ndatdd.inc
         INCLUDE   nowndd.inc
         INCLUDE   norddd.inc
         include   cons.inc
          include compdd.inc
          include cntdd.inc
NUM      FORM      1
release  init          "1.0"  DLH cleanup cleaned codes on exclusives
Reldate   Init      "06 Dec 2012"
OUTFILE  FILE      VAR=3002
input    file   
.


         DISPLAY   *P1:1,*ES
         TRAP      EOJ IF F5
         MOVE      "Abort" TO PF5
         move     c1 to nordpath
         CALL      PAINT
         CALL      FUNCDISP
PREP 
         PACK       TASKNAME,NTWKPATH1,"dataClean"
          Prepare   Outfile,taskname,exclusive         
INPUT
         call      ndatks
         GOTO      EOJ IF OVER
         add       c1 to n7
         display   *p10:15,"records read = ",n7
          if        (elstcde <> "C" and Elstcde <> "P")
          goto      Input
          endif
          if        (CLEANCDE = "C000" or CLEANCDE = "C001" or CLEANCDE = "C023" or CLEANCDE = "C010" or CLEANCDE = "    " or CLEANCDE = "")
          Goto    input
         endif
          if        (CLEANCDE = "C002" & CLNINFO <> "Daily")
          Goto    input
         endif
          if        (CLEANCDE = "0000" & CLNINFO <> "")
          Goto    input
         endif
         
WRITE   
      
         WRITE      OUTFILE,SEQ;datvars
         add       c1 to n6
         display   *p10:16,"records written = ",n6

         GOTO      INPUT
EOJ
         WEOF      OUTFILE,SEQ
         CLOSE     OUTFILE
          clear     taskname
           append    "\\Nins1\Winbatch\butil job=NRH123 INfile=",TASKNAME
           APPEND    "dataclean" TO TASKNAME
           APPEND    " C=1",TASKNAME
           APPEND    " B=",TASKNAME
           APPEND    user TO TASKNAME
         APPEND    " PA=DH",TASKNAME
          append    " CO=",taskname
          append    Company,taskname
         RESET     TASKNAME
         display   *p1:24,*el,"Submitting the RH, Please wait"
         pause     "5"
         EXECUTE   TASKNAME
         display   *p1:24,*el,"RH, Submitted"


         PACK       TASKNAME,NTWKPATH1,"dataClean"
          Open      outfile,taskname,read
loop
          read      outfile,seq;datvars
          goto      Eoj1 if over
          packkey   Ndatfld from LSTNUM
          call      Ndatkey
          if        not over
          clear     cleancde
          clear     CLNINFO
          call      Ndatupd
          endif
         add       c1 to n5
         display   *p10:17,"records Updated = ",n5
          goto      loop
eoj1




         shutdown
         include   nordio.inc
         include   nownio.inc
         INCLUDE   ndatio.inc
.START PATCH 1.4 REPLACED LOGIC
..START PATCH 1.2 ADDED LOGIC
.         include   nfulio.inc
          include compio.inc
          include cntio.inc
..END PATCH 1.2 ADDED LOGIC
.START PATCH 1.4 REPLACED LOGIC         
         include   comlogic.inc

