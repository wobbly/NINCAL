	include	common.inc
	include	cons.inc
	include	ndatwdd.inc

release	init	"temp"

.START PATCH 1.2.1 ADDED LOGIC
//Clean up pre-existing Web Datacard records
	move	C1,NDATWPATH
	move	C1,N6
	loop
		move	N6,NDATWFLD
		rep	zfill,NDATWFLD
		move	"NDATWKEY",Location
		pack	KeyLocation,"Key: ",NDATWFLD
		call	NDATWKEY
		until not over
		if (N6 = "999999")
			move	C0,N6
			break
		endif
		add	C1,N6
	repeat
	if (N6 <> C0)
	//There are valid records in the file.
		loop
		//Delete them all
			move	"NDATWDEL",Location
			call	NDATWDEL
			move	"NDATWKS",Location
			call	NDATWKS
			until over
		repeat
	endif
.END PATCH 1.2.1 ADDED LOGIC
	SHUTDOWN

	include	ndatwio.inc
	include	comlogic.inc