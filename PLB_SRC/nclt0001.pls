.WEBSITE CONSULTANT/MAILER PROGRAM
........................................
. Program:	NCLT0001.PLS
. Function:	WEBSITE CONSULTANT/MAILER PROGRAM
. Author:	Andrew Harkins
. Orig.	Date:	February 24, 2006
. Release:	1.0
.
.	Barebones:  Requires using Debug to actually run.  Needs to be
.	rewritten at some point in the future
.
........................................
	include	common.inc
	include	cons.inc
	include	ncltdd.inc

release	init	"1.0"

	loop
		call	NCLTTST
		if not over
			call	NCLTDEL
		endif
	repeat
	shutdown

	include	ncltio.inc
	include	comlogic.inc