apiGetLocalTime PROFILE Kernel32,GetLocalTime,None,Dim

dfnLocalTimeLen DEFINE 16
LocalTime RECORD DEFINITION
wYear 	          INTEGER 2
wMonth 	          INTEGER 2
wDayOfWeek         INTEGER 2
wDay 	          INTEGER 2
wHour 	          INTEGER 2
wMinute            INTEGER 2
wSecond            INTEGER 2
wMilliseconds      INTEGER 2
         RECORDEND

strLocalTime DIM dfnLocalTimeLen
recLocalTime RECORD LIKE LocalTime

fYear 	          FORM 2
fMonth 	          FORM 2
fDayOfWeek         FORM 2
fDay 	          FORM 2
fHour 	          FORM 2
fMinute            FORM 2
fSecond            FORM 2
fMilliseconds      FORM 2
.
work150	dim 150

fmRslt form 1

    WINAPI    apiGetLocalTime USING strLocalTime
    unpack    strLocalTime into recLocalTime
    Fill      " " into work150
    MOVE recLocalTime.wYear,fYear
    MOVE recLocalTime.wMonth,fMonth
    MOVE recLocalTime.wDayOfWeek,fDayOfWeek
    MOVE recLocalTime.wDay,fDay
    MOVE recLocalTime.wHour,fHour
    MOVE recLocalTime.wMinute,fMinute
    MOVE recLocalTime.wSecond,fSecond
    MOVE recLocalTime.wMilliseconds,fMilliseconds
.
    Pack	work150 with "Yr:",fYear:
		   "Mn:",fMonth:
                      "Dw:",fDayOfWeek:
                      "Dy:",fDay:
                      "Hr:",fHour:
                      "Mn:",fMinute:
                      "Sc:",fSecond:
                      "Ms:",fMilliSeconds
    alert	note,work150,fmRslt
    alert	note,strLocalTime,fmRslt
