pc       equ       0
         INCLUDE   COMMON.inc
         INCLUDE   ndatdd.inc
         include   cons.inc
NUM      FORM      1

release  init      "1.0"         change list owner number from x to y
Reldate    init       "2014 November 6"
input    file   
output     File
.
           move       c1,ndatpath
         DISPLAY   *P1:1,*ES
         TRAP      EOJ IF F5
         MOVE      "Abort" TO PF5
         move     c1 to ndatpath
         CALL      PAINT
         CALL      FUNCDISP
           Prepare    output,"c:\work\dataupd.csv"
           Write      output,seq;*cdFon,"List Name","List Number","Old Owner","New Owner"
           open          Input,"\\nins1\e\data\text\nindat.dat|nins1:502"         
INPUT

         read      input,seq;datvars
         GOTO      EOJ IF OVER
         add       c1 to n6
          display   *p10:15,"records read = ",n6
CHECK
           if         (ownnum = "005787")
           packkey    Ndatfld using lstnum
           call       ndatkey
                      if         Not over
                      move       "001713",ownnum
                      call       ndatupd
           add       c1 to n5
           display   *p10:16,"records Updated = ",n5
           Write      output,seq;*cdFon,mlstname,LSTNUM,"005787","001713"
                      endif
        endif
         GOTO      INPUT


EOJ        weof       output,seq
           close      output
         shutdown
         include   ndatio.inc
         include   comlogic.inc

