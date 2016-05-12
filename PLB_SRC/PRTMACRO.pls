         inc       common.inc
         include   cons.inc
         inc       F:\LIBRARY\INCLUDE\hp.inc
         
         splopen  "f:\data\prtmacro.prn"
         print     hpdcard,*L,*L,"test",*f
         splclose
         stop
