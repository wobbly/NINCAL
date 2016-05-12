ArrLength const	"30"
Str30	init	"(F10YW*08*HT80I8Y*MA2C455KL0Y)"
str30a	dim	1(ArrLength)
str1	dim	1
str2	dim	2
N1	form	1
N2	form	2
N2a	form	2
.Pointers
DimPtr	dim	^
ArrPtr	dim	^(ArrLength)

	move	"*",str1
	scan	str1,str30
	if equal
		pack	str2,str1,"E"
	else
		pack	str2,str1,"T"
	endif
	reset	str30
	rep	str2,str30
	call	MoveDimToArray using str30,str30a
	movelptr str30,N2a
	for N2,1,N2a
		if (str30a(N2) = "Y")
			move	"S",str30a(N2)
		else
			type	str30a(N2)
			if equal
				if (str30a(N2) = "0")
					move	str30a(N2),N1
					add	"2",N1
					move	N1,str30a(N2)
				endif
			endif
		endif
	repeat
	clear	str30
	for N2,1,N2a
		append	 str30a(N2),str30
	repeat
	reset	str30
	move	"2N",str2
	rep	str2,str30
	call	MoveDimToArray using str30,str30a
	clear	str30
	for N2,N2a,"1","-1"
		append str30a(N2),str30
	repeat
	reset	str30,10
	setlptr	str30,26
	pack	str2,"8 "
	rep	str2,str30
	shutdown		.What is the value of str30?

MoveDimToArray Routine DimPtr,ArrPtr
.Dumps contents of a string into an Array
	move	"0",N2
	loop
		add	"1",N2
		until (N2 > ArrLength)
		unpack	DimPtr,ArrPtr(N2)
		bump	DimPtr
	repeat
	return