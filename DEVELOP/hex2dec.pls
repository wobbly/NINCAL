	include	common.inc
	include	cons.inc

release	init	"1.0"
N11	form	11
result2	form	20
result3	form	20
DimPtr	dim	^
DimPtr2	dim	^
ex	automation


LastDayArray	form	6(220)		.First 69 not used!!!
MonthArray	form	4(12),("0"),("31"),("59"),("90"),("120"),("151"),("181"),("212"),("243"),("273"),("304"),("334")	.Number of days in year at the beginning of each corresponding month
modulus		form	0.10
resultb		form	9.2
Days		form	9
Hour		form	2
Minute		form	2
Second		form	2


	loop
		call	HexToDec using str10,result2
andrew
		call	UnixDateToCalcDate using str10,timestamp
andrew2
.		call	UnixDateToCalcDate2 using str10,timestamp
andrew3
	repeat

UnixDateToCalcDate3 LRoutine DimPtr,DimPtr2
	clear	DimPtr2
	call	HexToDec using DimPtr,result2
.All unfilled values are initialized to "0"
	for result,"70","220"
		calc	LastDayArray(result)=(365+LastDayArray(result-1))
		move	result,resultb
		calc	modulus=(resultb/4)
		if (modulus = 0)
			calc	LastDayArray(result)=(LastDayArray(result)+1)
		endif
	repeat
one
	calc	Days=(result2/86400)
	for result,"70","220"
		if (Days <= LastDayArray(result))
			break
		endif
	repeat
two
	for N5,"1","11"
		calc	howmany=(LastDayArray(result-1)+MonthArray(N5+1))
		if (Days < howmany)
			move	N5,N2
			move	N2,MM
			rep	zfill,MM
			calc	N2=(100+Days-LastDayArray(result-1)-MonthArray(N5)+1)
			move	N2,DD
			rep	zfill,DD
			if (result < 100)
				calc	N4=(1900+result)
			elseif (result < 200)
				calc	N4=(2000+(result-100))
			elseif (result < 300)
				calc	N4=(3000+(result-200))
			endif
			move	N4,str4
			rep	zfill,str4
			break
		endif
	repeat
	calc	result3=(result2-(LastDayArray(result-1)+MonthArray(N5)+N2-1)*86400)
.
	calc	Hour=(result3/3600)
	calc	Minute=((result3-(Hour*3600))/60)
	calc	Second=(result3-((Hour*3600)+(Minute*60)))
	pack	DimPtr2,str4,MM,DD,Hour,Minute,Second
	rep	zfill,DimPtr
	return
 
	include	comlogic.inc