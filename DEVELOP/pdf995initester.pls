	include	common.inc
	include	cons.inc

release	init	"temp"
. To use:
.         CALL      "GU$INI;GET_FROM_INI" USING <ini file name>:
.                                               <section name>:
.                                               <key name>:
.                                               <value>
. <ini file name>= the name of the INI file (dim field or literal)
. <section name> = section in the INI file  (dim field or literal)
. <key name>     = key name in the INI file (dim field or literal)
. <value>        = returned value (dim field)
. IF not found, <value> is cleared and the OVER flag is set
	call	"GU$INI;GET_FROM_INI" USING "c:\work\pdf995.ini":
                                               "Parameters":
                                               "User File":
                                               str45
.****************************************************************************
. WRITE_TO_INI Routine
. To use:
.         CALL      "GU$INI;WRITE_TO_INI" USING <ini file name>:
.                                               <section name>:
.                                               <key name>:
.                                               <value>:
.                                               <result>
. <ini file name>= the name of the INI file (dim field or literal)
.                  If the name does not contain the path, it defaults
.                  to the windows direcory.
.                  If the file by does not exist, it is created.
. <section name> = section in the INI file  (dim field or literal)
.                  If the section does not exist, it is created.
. <key name>     = key name in the INI file (dim field or literal)
.                  If the key does not exist, it is created.
.                  If this is NULL (cleared) then the section is deleted
.                  from the INI file
. <value>        = value to write (dim field or literal)
.                  If value is NULL (cleared) then the key is deleted.
. <result>       = Result flag (form field)
.                  returned as 0 if write OK
.                  returned as 1 if write failed
.HexZero
	CALL	"GU$INI;WRITE_TO_INI" USING "c:\work\pdf995.ini":
                                               "Parameters":
                                               "peanut":
                                               HexZero:
                                               result


	shutdown	
	
	include	comlogic.inc