;. START HERE
;.
         INC       COMMON.INC
VAR1     DIM       1
VAR      DIM       10
ANTHR    FORM      1
LETTER   FORM      1
COUNT    FORM      2
VERT     FORM      3
HORZ     FORM      2
PAUSE    FORM      3
;
         TRAP      END IF F3
         DISPLAY   *ES
START    DISPLAY   *P50:04,"              _______"
         DISPLAY   *P50:05,"              |     |"
         DISPLAY   *P50:06,"             ||     |"
         DISPLAY   *P50:07,"             ||     |"
         DISPLAY   *P50:08,"             ||     |"
         DISPLAY   *P50:09,"             ||     |"
         DISPLAY   *P50:10,"  -----------*+     |"
         DISPLAY   *P50:11," /                  |"
         DISPLAY   *P50:12," \                  |"
         DISPLAY   *P50:13,"  \______          / "
         DISPLAY   *P50:14,"          \       /  "
         DISPLAY   *RPTCHAR,"=":80
         DISPLAY   *HON:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*N:
                   *RPTCHAR " ":80,*HOFF
         DISPLAY   *P58:16,"/          /"
         DISPLAY   *P57:17,"/          /"
         DISPLAY   *P56:18,"/          /"
         DISPLAY   *P55:19,"/          /"
         DISPLAY   *RPTCHAR "-":54:
                   "+          +":
                   *RPTCHAR "-":30
         DISPLAY   ""
         DISPLAY   *RPTCHAR "-":80
ENTER
         CLEAR     VAR
         DISPLAY   *P1:1,"KEYIN ITEM TO BE FLUSHED (F3 TO EXIT)",*W
         KEYIN     *P1:1,*EL,"__________",*P1:1,VAR
         CMATCH    " " TO VAR
         GOTO      ENTER IF EOS
NXTCHAR
         MOVE      "0" TO ANTHR
         MOVELPTR  VAR TO COUNT
         RESET     VAR TO COUNT
         MOVE      VAR TO VAR1
         SUB       "1" FROM COUNT
         GOTO      DONE IF EQUAL
         SETLPTR   VAR TO COUNT
         MOVE      "1" TO ANTHR
         DISPLAY   *P1:1,VAR,*EL
         GOTO      SNDCHAR
DONE
         DISPLAY   *P1:1,*EL
SNDCHAR
         MOVE      "1" TO VERT
         MOVE      "10" TO HORZ
SUBSCRN  MOVE      "3" TO COUNT
         CALL      SCRLEFT
RIGHT    DISPLAY   *PHORZ:VERT,VAR1
         CALL      PAUS
         DISPLAY   *PHORZ:VERT," "
         ADD       "2" TO HORZ
         SUB       "1" FROM COUNT
         GOTO      RIGHT IF NOT EQUAL
         ADD       "1" TO VERT
         COMPARE   "9" TO VERT
         GOTO      SUBSCRN IF NOT EQUAL
         DISPLAY   *P53:09,"!!SPLASH!!"
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         DISPLAY   *P53:09,"          "
         COMPARE   "1" TO ANTHR
         GOTO      NXTCHAR IF EQUAL
         DISPLAY   *P53:09,*HON,"! FLUSH !",*HOFF
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         DISPLAY   *P53:09,"<<GURGLE>>"
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         DISPLAY   *P53:09,"          "
         CALL      PAUS
         RESET     VAR TO 1
         SETLPTR   VAR TO 10
         MOVE      "59" TO HORZ
         MOVE      "16" TO VERT
DOWN
         DISPLAY   *PHORZ:VERT,VAR
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         CALL      PAUS
         DISPLAY   *PHORZ:VERT,"          "
         SUB       "1" FROM HORZ
         ADD       "1" TO VERT
         COMPARE   "21" TO VERT
         GOTO      DOWN IF NOT EQUAL
         SUB       "3" FROM HORZ
         DISPLAY   *PHORZ:VERT,"   ",VAR
FLUSH
         MOVE      "100" TO VERT
PAU      CALL      PAUS
         SUB       "1" FROM VERT
         GOTO      PAU IF NOT EQUAL
         GOTO      START
SCRLEFT
         DISPLAY   *SETSWTB 21:21:
                   *SETSWLR 1:80;
         ADD       "1" TO LETTER
         COMPARE   "9" TO LETTER
         GOTO      SCROLL IF NOT EQUAL
         MOVE      "1" TO LETTER
SCROLL
         BRANCH    LETTER TO M0,E0,R0,D0,E0,B0,B0,B0,B0
M0       DISPLAY   *SCRLEFT,"M"
         GOTO      RESET
E0       DISPLAY   *SCRLEFT,"E"
         GOTO      RESET
R0       DISPLAY   *SCRLEFT,"R"
         GOTO      RESET
D0       DISPLAY   *SCRLEFT,"D"
         GOTO      RESET
B0       DISPLAY   *SCRLEFT," "
RESET    DISPLAY   *RESETSW
         RETURN
PAUS     MOVE      "20" TO PAUSE
WAIT     SUB       "1" FROM PAUSE
         GOTO      WAIT IF NOT EQUAL
         CALL      SCRLEFT
         RETURN
END      DISPLAY   *RESETSW
          STOP
