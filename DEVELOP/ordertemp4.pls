..........................................................
.
. Author:  Andrew Harkins
. Date:    July 21, 1998
.
.program used to find missing LR #
..........................................................

PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NORDDD.inc
         include   gnxtdd.inc

Newfile file
release  init      "TEMP"

         erase	"G:\DATA\TEXT\NEWFILE4.DAT"
         PREPARE   Newfile,"G:\DATA\TEXT\NEWFILE4.DAT"
         move      c1 to nORDpath
         call      paint
.         move      "346000",N6
         move      "508440",N6
.
         move      "NORDNXT",GNXTFLD
         call      GNXTKEY
         move      GNXTNUM,N8
input    add       c1 to N6
         move      N6,NORDFLD
         if (N6 > N8)
                GOTO EOJ
         ENDIF
         CALL      NORDTST
         if OVER
                   WRITE     Newfile,SEQ;nordfld
         endif
         add       C1,N7
         display   *p10:12,"records processed : ",n7
         goto input
eoj
         WRITE     Newfile,SEQ;"Records:     ",N7
         WRITE     Newfile,SEQ;nordfld
         display   *p10:23,n7,*b,*w5
         stop

         include   gnxtIO.inc
         include   comlogic.INC
         include   NORDIO.inc
