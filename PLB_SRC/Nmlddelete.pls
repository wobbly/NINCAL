.nmld del delete mistakenly saved maildate changes
pc        Equ       0
          include   common.inc
          include   cons.inc
        
          include   nmlddd.inc
release  init      "1.0"
Reldate   Init      "2013 August 9"
           MOVE      "NAMES IN THE NEWS" TO COMPNME
           MOVE      "DELETE RECORDS" TO STITLE
           CALL      PAINT
          
          
K22      KEYIN     *P8:7,NMLDFLD
         scan       star,Nmldfld
         stop       if equal
         reset      Nmldfld
         pack      NMLDFLD1,"01X",Nmldfld
         call      NMLDAIM
         if         over
         DISPLAY   *P1:24,*EL,"NO RECORD WITH THIS KEY ON FILE !!!!!",*W;
         DISPLAY   *W,*W;
          goto      K22
         endif
foundone  DISPLAY   *P10:09,*EL,NMLDVARS
         Keyin     *P10:10,*EL,"This RECORD? ",str1
          if          (str1 = "Y") 
          call      NMLDdel
         DISPLAY   *P1:24,*EL,"RECORD Deleted !!!!!",*W;
         DISPLAY   *W,*W;
           else
           call       NMLDKG
                      if         over
                      DISPLAY   *P1:24,*EL,"NO More RECORDs WITH THIS KEY ON FILE !!!!!",*W;
                      DISPLAY   *W,*W;
                      else
                      goto       foundone
                      endif
                      
          goto      K22
           endif
          goto      K22
          stop          
          
          include nmldio.inc
          include comlogic.inc          