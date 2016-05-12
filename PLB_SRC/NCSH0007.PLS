.Program which sorts moa file and nincsh for certain control
.Used for cash receipts print option

PC      Equ     0
        include common.inc
        include cons.inc
. Patch 1.1	01/11/05 ASH  Mailer/Broker field conversion
. 12/12/01 DMB  Added Sort for Edit
.Sort Parameters=======================================================
INDAT    init  "DAT25N.DAT"   .File to be sorted
INMOA    init  "NINMOA.DAT"   .File to be sorted
.INDAT    init  "\\nins1\e\data\text\DAT25N.DAT"   .File to be sorted
OUTCSH   init  "NCSH"   .Partial Name of sorted Output file for ncsh
OUTMOA   init  "NMOA"     .Partial Name of sorted Output file for moa
.OUTCSH   init  "c:\work\nincsh.DMB"   .Sorted Output file
.START PATCH 1.1 REPLACED LOGIC
.CSHSRT   init  "3-6,41-46,7-12"   .Sort
CSHSRT   init  "3-8,43-54,9-14"   .Sort
.END PATCH 1.1 REPLACED LOGIC
NMOASRT  init  "122-125"          .Sort
.START PATCH 1.1 REPLACED LOGIC
.EDTSRT   init  "3-6,7-12"
EDTSRT   init  "3-8,9-14"
.END PATCH 1.1 REPLACED LOGIC
SORTFLE  dim    100                          .Var to pack file names of sort
.==========================================================================

        scan    "ED" in func
        if equal
        	goto EDITSORT
        endif
CASHSORT
         pack   str16,OUTCSH,comment,".dat"
         pack   SortFle,NTWKPATH6,indat,comma,NTWKPATH5,str16
.START PATCH 1.1 REPLACED LOGIC
.     	 pack   taskname,sortfle,";",CSHSRT,comma,"s=38=","'",comment,"'","&17=","'",inits,"'"
	pack	taskname,sortfle,";",CSHSRT,comma,"s=40=","'",comment,"'","&19=","'",inits,"'"
.END PATCH 1.1 REPLACED LOGIC
;      	 pack   taskname,sortfle,";",CSHSRT,comma,"s=38=","'",comment,"'"
..       	 pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","RB","'","&9=","'",YR1,"'","|","1=","'","RB","'","&9=","'",YR2,"'"
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
         endif


MOASORT
         pack   str16,OUTMOA,comment,".dat"
         pack   SortFle,NTWKPATH6,INMOA,comma,NTWKPATH5,str16
       	 pack   taskname,sortfle,";",NMOASRT,comma,"s=21=","'",comment,"'","&17=","'",func,"'","&","15=","'",inits,"'"
.       	 pack   taskname,sortfle,";",clintsrt,comma,"s=1=","'","RB","'","&9=","'",YR1,"'","|","1=","'","RB","'","&9=","'",YR2,"'"
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
         endif
         goto Endsrt

EDITSORT
         pack   str16,OUTCSH,comment,".dat"
         pack   SortFle,NTWKPATH6,indat,comma,NTWKPATH5,str16
;       	 pack   taskname,sortfle,";",EDTSRT,comma,"s=38=","'",comment,"'"
.START PATCH 1.1 REPLACED LOGIC
.       	 pack   taskname,sortfle,";",EDTSRT,comma,"s=38=","'",comment,"'","&17=","'",inits,"'"
       	 pack   taskname,sortfle,";",EDTSRT,comma,"s=40=","'",comment,"'","&19=","'",inits,"'"
.END PATCH 1.1 REPLACED LOGIC
         sort   taskname
         if over
               alert caution,S$ERROR$,result,"No Sort"
         endif
ENDSRT
         shutdown  "cls"
