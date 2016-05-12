****************************************************
. Sunfm Log Analyzer :o)
.
.    by Matthew Lake
.	Sunbelt Computer Systems, Inc.
.
.
.
.Start with the log file structure...
.

Log_entry	LIST
GMT	DIM	8	; 1-8  unix time stamp
dumb1	DIM	2	; 9-10 padding
conNum	DIM	8	;11-18 Hex connection number
dumb2	DIM	1	;19-19 padding
msg	DIM	17	;20-36 log message
dumb3	DIM	1	;37-37 padding
mdata	DIM	40	;48-87 message data
	LISTEND

logfname	DIM	250
logf	FILE
seq	FORM	"-1"
fpos	INTEGER	4

Path        Dim         250
PathFname   Dim         500
V           Form        2
            move       "*.log",logfname
            Clear       PathFName
            move              "\\nins1\c\program files\sunbelt\sunfm86\" to pathfname
            GETFNAME        OPEN,"Open Test", LogFNAME,PATH,"PLS"
        IF      NOT OVER
        DISPLAY *HD,*R,"File name returned: ",logFName,*R,*p1:1,"     Path: ",PATH,"  ",*w3;
        ELSE
.       KEYIN   *HD,*R,"Dialog cancelled...",str1;
        stop
        ENDIF

;	MOVE	"c:\sunbelt\sunfm.86\test\sunfm.log",logfname
; quickly get caught up
            Pack        PathFname From Path,Logfname
	OPEN	logf,PathFName,sharenf
	LOOP
		READ	logf,seq;log_entry
		BREAK	IF OVER
		CALL	process_entry
	REPEAT
; now just go into monitor mode :o)
	FPOSIT	logf,fpos
	CALL	displayConns
	LOOP
		READ	logf,seq;log_entry
		IF OVER
			PAUSE	"2"
			REPOSIT	logf,fpos
			CONTINUE
		ENDIF
		CALL	process_entry
		CALL	displayConns
		FPOSIT	logf,fpos
	REPEAT
	
	STOP

conns	RECORD	(512)
conNum	DIM	8	;Hex connection number
msg	DIM	17	;log message
mdata	DIM	40	;message data
	RECORDEND

counter	FORM	3
avail	FORM	3
. the process entry will manager the connection status
. array and displaying them on the screen
process_entry
;            scan        "PLBSERVE" in Logfname
;            If          not equal
;lets add info here instead of returning - display the goodies
            If               (ConNum="00000000" or ConNum = "00000001")
;RETURN IF (ConNum="00000000")
            call              DisInfo
            return
            endif
           	CLEAR	avail
	FOR counter,"1","512"
		IF ( ConNum=conns(counter).conNum )
			SCAN	"Term",msg
			IF EQUAL
				CLEAR conns(counter)
				BREAK
			ENDIF
		ENDIF
		IF ( conns(counter).conNum="" & avail=0 )
			MOVE counter,avail
		ENDIF
	REPEAT
	SCAN	"Start",msg
	IF EQUAL
		RESET	msg
		MOVE	conNum,conns(avail).conNum
		MOVE	msg,conns(avail).msg
		MOVE	mdata,conns(avail).mdata
	ENDIF
 RETURN

conip	DIM	16
horz	FORM	2
displayConns
	DISPLAY	*HU," connNum  Ip Address     | connNum  Ip Address     | connNum  Ip Address     |",*N,*EF;
	DISPLAY	*V=1
	SET	horz
	FOR counter,"1","512"
		IF ( conns(counter).conNum!="")
		PARSE	conns(counter).mdata,conip,"09.."
		DISPLAY *H=horz,conns(counter).conNum," ",conip;
		ADD "26",horz
		IF (horz >"75")
			MOVE "1",horz
			DISPLAY	*N;
		ENDIF
		RESET	conns(counter).mdata
		ENDIF
	REPEAT
 RETURN
DisInfo
            If          (v = 0)
            DISPLAY	*p1:1,*ef;
            endif
            Display     *p1:v,Log_Entry,*w2
            add         "1" to V
            return