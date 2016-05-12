	include common.inc
	include cons.inc
	
Release Init "1.0"

NotFile1 File
NotFile  File

TDMCLR   DIM       6
tdmcdat  dim       10
tdmctime dim       10
	Prep      NotFile1,"C:\work\not.dat"

        Open      NotFile,"\\nts1\e\data\text\tdmcnotify"
        loop
        Read    NotFile,seq;TDMCLR,b1,tdmcdat,b1,tdmctime
        until over
		scan      "0--",tdmcdat
		If Equal
			  Unpack tdmctime,str2,yy,mm,dd
			  Pack tdmcdat,str2,yy,dash,mm,dash,dd
			  Clear tdmctime
			  Write NotFile1,seq;TDMCLR,b1,tdmcdat,b1,tdmctime
		Else
			  Write NotFile1,seq;TDMCLR,b1,tdmcdat,b1,tdmctime		
		Endif
	repeat
        
	stop
	
	
	include comlogic.inc
	