*.............................................................................
.
.     PROGRAM:     ANIMATE.PLS   (DAVID GABLER@APOLLO SOFTWARE)
.   Orig.Date:     04-29-98
.     Purpose:     TO ANIMATE A BITMAP
.   Revisions:
.
         INCLUDE   PROFILE.INC    * DECLARE WINAPI AND COMMON EXTERNAL ROUTINES
.
PROGRAM  INIT      "DEMONSTRATE ANIMATION"
PVERSION INIT      "1.00"
.
*..............................................................................
.
ONE      FORM      "1"
ZERO     FORM      "0"
ICONID   FORM      4
CURICON  FORM      4
FRAMES   FORM      4
ICON$ANIM    ICON
FORM32   FORM      32
FORM32A  FORM      32
FORM32B  FORM      32
winder  window  ^
.
RESOURCE PLFORM    Animate  * CONTAINS ALL THE ICONS
        moveaddr animate,winder
.
*..............................................................................
.
PROGRAM
         TRAP      EOJ NORESET IF INTERRUPT
         FORMLOAD  RESOURCE
         winhide
.
         LOOP
         CALL      ANIMATE
         CHECKEVENT         * CHECK FOR CANCEL EVENT WHILE WE PROCESS THIS
         REPEAT                            
.
EOJ
         SHUTDOWN
.
*.............................................................................
