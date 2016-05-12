result	form	9
howmany	form	9
FrmPtr	form	^

	move	"10",result
	call	CalcIt using result
	shutdown

CalcIt Routine FrmPtr
temp	form	9

	move	FrmPtr,temp
	calc	howmany=(temp/2)
	if (howmany > 1)
		call	CalcIt using howmany
	endif
	alert	note,"return",result
	return
