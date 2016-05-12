.------------------------------------------------------------------------------
.      Name: PopUpCal.PLS
.    Author: Brian C. SWING
.            Judicial Information Services
.            Clay County
.            Liberty, MO
. Orig.Date: Feburary 04, 2002
.How to use:
.    calls "PopUpCal;PopUpCalendar" using wnForm,etDate,EditDate,"19780101","20011231":
.                                         Canceled,DayOfWeek
.          wnForm - WINDOW OBJECT: window calendar will be attached to
.          etDate - EDITTEXT OBJECT: used as a guide to place the calendar
.        EditDate - DIM 8: date passed in and out. If empty then will use current date
.                   format: ccyymmdd
.         MinDate - DIM 8: minimum date calendar will go back to
.         MaxDate - DIM 8: maximum date calendar will go to
.        Canceled - FORM 1: if date wasn't selected, then will return a '1'
.       DayOfWeek - FORM 1: numeric day of week 1=Sun, 2=Mon,...,7=Sat
. Notes:
. * Holidays are in red, if holiday falls on weekend the previous Friday or
.   fallen Monday are marked as a holiday
. * Weekends have a gray background
. * Day of Week is passed back, 1=Sun, 2=Mon, ..., 7=Sat
. * Minimum and Maximum dates can be passed to calendar. Displayed at bottom of calendar
. * Added Cancel button, at bottom of calendar. Escape closes calendar and
.   sets variable Canceled to 1
. * Can Click on gray dates
.Revisions:
.------------------------------------------------------------------------------
     include plbequ.inc
. * common variables
ccyy    dim     4
mm      dim     2
dd      dim     2
nyyyy   form    4
nmm     form    2
ndd     form    2
days    form    2

work01  dim     1
work02  dim     2
work03  dim     3
work04  dim     4
work06  dim     6
work06a dim     6
work08  dim     8
work09  dim     9
work10  dim     10
work10a dim     10
work25  dim     25
work35  dim     35
work50  dim     50
work80  dim     80
work150 dim     150
work1024 dim    1024

nwork01 form    1
nwork02 form    2
nwork02a form   2
nwork03 form    3
nwork04 form    4
nwork04a form   4
nwork09 form    9
nwork10 form    10
ndx     form    4
count   form    3
result  form    4
curindex form   4
chars   form    4
left    form    4

jandays form    "31"
febdays form    "28"     // (non leap year)
febldays form   "29"     // (leap year)
mardays form    "31"
aprdays form    "30"
maydays form    "31"
jundays form    "30"
juldays form    "31"
augdays form    "31"
sepdays form    "30"
octdays form    "31"
novdays form    "30"
decdays form    "31"
long_month  dim  9(12),("January  "),("February "),("March    "),("April    "):
                       ("May      "),("June     "),("July     "),("August   "):
                       ("September"),("October  "),("November "),("December ")
DOWlist     dim 9(7),("Sunday"),("Monday"),("Tuesday"),("Wednesday"):
                     ("Thursday"),("Friday"),("Saturday")
leap_yrs  init "1904.1908.1912.1916.1920.1924.1928.1932.1936.1940.":
               "1944.1948.1952.1956.1960.1964.1968.1972.1976.1980.":
               "1984.1988.1992.1996.2000.2004.2008.2012.2016.2020.":
               "2024.2028.2032.2036.2040.2044.2048.2052.2056.2060"

ScrWdth form    4       //Screen width
ScrHght form    4       //Screen height
red     color
yellow  color
.------------------------------------------------------------------------------
. * passed in variables
#psMnWindow  window   ^
#psEditObj   edittext ^
#psCrntDate  dim      ^  // date passed in and passed back
#psMinDate   dim      8  // minimum date
#psMaxDate   dim      8  // maximum date
#psCanceled  form     ^  // 1=calendar was canceled, 0=
#psDayOfWeek form     ^  // day of week

. * variables
#fmBrkLoop   form    1   // loop repeat break switch

#stCalCell   stattext (42)
#spCalShape  shape   (42)
#daHolidays  dim     40(31)

#fmMinYear   form    4   // "1900"
#fmMinMonth  form    2
#fmMinDay    form    2

#fmMaxYear   form    4   // "2100"
#fmMaxMonth  form    2
#fmMaxDay    form    2

#dmTmpDate   dim     8
#dmTdysDate  dim     8

#fmStLeft    form    4
#fmStTop     form    4
#fmLeft      form    4
#fmTop       form    4
#fmBegDay    form    2
#fmEndDay    form    2
#fmCrntCell  form    2
#fmShapeCell form    2

#fmMnTop     form    4
#fmMnLeft    form    4
#fmEOTop     form    4
#fmEOLeft    form    4
#fmEOHeight  form    4
#fmEOWidth   form    4
#fmCalTop    form    4
#fmCalLeft   form    4
#fmCalHeight form    " 190"
#fmCalWidth  form    " 170"
.------------------------------------------------------------------------------
. * define forms
#wnPopUpCal          Window
#stTop               StatText
#stBottom            StatText
#btCancel            Button
#btPCalBack          Button
#btPCalForward       Button
#btMonth             Button
#btYear              Button
#lnBorder            Line
#stExample           StatText
#stRange             StatText
#stToday             StatText
#vsbMonth            VScrollBar
#vsbYear             VScrollBar
#stDay               stattext (7)
#fmnMonths  floatmenu
#pmnYears   popupmenu
#fntCalendar font
#fntWing     font
.------------------------------------------------------------------------------
. * initialize calendar and activate
PopUpCalendar routine #psMnWindow,#psEditObj,#psCrntDate,#psMinDate,#psMaxDate:
                      #psCanceled,#psDayOfWeek
    clear #psCanceled
    clear #fmBrkLoop
    create red=*red
    create yellow=*yellow
    create #fntCalendar,"'>Ms Sans Serif'",size=8
    create #fntWing,"'>Wingdings 3'",size=9
    call PopUpCal_Create
    call Calendar_Initialize
.   *
.   * activate popupcal, sense it is a modal dialog no 'loop eventwait repeat' is needed
.   *
    activate #wnPopUpCal
.   *
.   * set day of week
.   *
    if (not #psCanceled)
       calls "DateRoutines;CalDateToLinearDate" using #psCrntDate,nwork10
       calls "DateRoutines;DowFromLinearDate" using nwork10,#psDayOfWeek,work09
    endif

. *
    return
.------------------------------------------------------------------------------
. * date selected, deactivate calendar and return to main calling program
DateSelected
    deactivate #wnPopUpCal
    set #fmBrkLoop
. *
    return
.------------------------------------------------------------------------------
. * deactivate calendar without selecting date
PopUpCal_Canceled
    set #psCanceled
    deactivate #wnPopUpCal
    set #fmBrkLoop
. *
    return
.------------------------------------------------------------------------------
. * initialize calendar with dates and range dates
Calendar_Initialize
.   *
.   * set date variable fields
.   *
    reset #psCrntDate
    setlptr #psCrntDate
    rep " 0" in #psCrntDate
    if (#psCrntDate = "00000000")
       clock timestamp in #psCrntDate
    else
       calls "DateRoutines;CalDateToLinearDate" using #psCrntDate,nwork10
.      *
.      * valid date is between 01/01/1900 & 12/31/2099
.      * in linear terms is between 1 & 73049
.      *
       if (nwork10 < 1 | nwork10 > 73049)
          clock timestamp in #psCrntDate
       endif
    endif
    clock timestamp in #dmTdysDate
.   *
.   * min & max dates
.   *
    reset #psMinDate
    setlptr #psMinDate
    rep " 0" in #psMinDate
    if (#psMinDate = "00000000")
       move "19000101" to #psMinDate
    endif
    reset #psMaxDate
    setlptr #psMaxDate
    rep " 0" in #psMaxDate
    if (#psMaxDate = "00000000")
       move "20991231" to #psMaxDate
    endif
.   *
.   * make sure pass thru current date is within bounds
.   *
    if (#psCrntDate < #psMinDate)
       move #psMinDate to #psCrntDate
    endif

    if (#psCrntDate > #psMaxDate)
       move #psMaxDate to #psCrntDate
    endif
.   *
.   * display date range
.   *
    unpack #psMinDate into #fmMinYear,#fmMinMonth,#fmMinDay
    pack work10 with #fmMinMonth,".",#fmMinDay,".",#fmMinYear
    rep " 0" in work10
    unpack #psMaxDate into #fmMaxYear,#fmMaxMonth,#fmMaxDay
    pack work10a with #fmMaxMonth,".",#fmMaxDay,".",#fmMaxYear
    rep " 0" in work10a
    pack work50 with work10," thru ",work10a
    setitem #stRange,0,work50
.   *
.   * setup year's vscrollbar
.   *
    setitem #vsbYear,1,#fmMinYear
    setitem #vsbYear,2,#fmMaxYear

    call SetCalendar
. *
    return
.------------------------------------------------------------------------------
. * set calendar to month/year
SetCalendar
.   *
.   * reset current day and selected day cells
.   *
    setprop #spCalShape(#fmShapeCell),BdrColor=$Window
    setprop #spCalShape(#fmCrntCell),FillColor=$Window
.   *
.   * set month & year buttons
.   *
    unpack #psCrntDate into ccyy,mm,dd
    move mm to nmm
    move dd to ndd
    move ccyy to nyyyy
    chop long_month(nmm),work09
    pack work25 with work09," ",ccyy
    setitem #btMonth,0,work09
    setitem #btYear,0,ccyy
.   *
.   * set month vscrollbar
.   *
    setitem #vsbMonth,0,nmm
.   *
.   * Now Build The Calendar
.   * find out what day the 1st falls on
.   *
    pack work08 with ccyy,mm,"01"
    calls "DateRoutines;CalDateToLinearDate" using work08,nwork10
    calls "DateRoutines;DowFromLinearDate" using nwork10,#fmBegDay,work09
.   *
.   * setup calendar
.   *
.   * get number of days in the month
    move nyyyy to ccyy
    call GetNbrDays using nmm,ccyy,nwork02
    calc #fmEndDay = #fmBegDay + nwork02 - 1
.   *
.   * if current day (dd) is not in new month
.   * then set day (dd) to maximum days in month
.   *
    if (ndd > nwork02)
       unpack #psCrntDate into ccyy,mm,dd
       move nwork02 to ndd
       move ndd to dd
       pack #psCrntDate with ccyy,mm,dd
       rep " 0" in #psCrntDate
    endif
.   *
.   * get holidays for month
.   *
    unpack "" into #daHolidays
    calls "DateRoutines;CalculateHolidays" using #psCrntDate,#daHolidays,"Y"
.   *
.   * now build the calendar
.   *
    move "0" to days
    for ndx from #fmBegDay to #fmEndDay
       add "1" to days
       move days to work02
       if (#fmMinYear = nyyyy & #fmMinMonth = nmm & days < #fmMinDay)       // min date
          setitem #stCalCell(ndx),0,""  // blank out days not in range
          if (ndx = 1 | ndx = 8 | ndx = 15 | ndx = 22 | ndx = 29 | ndx = 36 | :
              ndx = 7 | ndx = 14 | ndx = 21 | ndx = 28 | ndx = 35 | ndx = 42)
             setprop #spCalShape(ndx),FillColor=$Window  // make sure weekend day bg is cleared
          endif
       else if (#fmMaxYear = nyyyy & #fmMaxMonth = nmm & days > #fmMaxDay)  // max date
          setitem #stCalCell(ndx),0,""  // blank out days not in range
          if (ndx = 1 | ndx = 8 | ndx = 15 | ndx = 22 | ndx = 29 | ndx = 36 | :
              ndx = 7 | ndx = 14 | ndx = 21 | ndx = 28 | ndx = 35 | ndx = 42)
             setprop #spCalShape(ndx),FillColor=$Window  // make sure weekend day bg is cleared
          endif
       else
.         *
.         * set day number
.         *
          setitem #stCalCell(ndx),0,work02
          setprop #stCalCell(ndx),FgColor=$BtnText
.         *
.         * search holiday listview to see if date is a holiday
.         *
          move #daHolidays(days) to work80
          count chars in work80
          if not zero      // it's a holiday
             setprop #stCalCell(ndx),fgColor=red
          endif
.         *
.         * weekends
.         *
          if (ndx = 1 | ndx = 8 | ndx = 15 | ndx = 22 | ndx = 29 | ndx = 36 | :
              ndx = 7 | ndx = 14 | ndx = 21 | ndx = 28 | ndx = 35 | ndx = 42)
             setprop #spCalShape(ndx),FillColor=$Menu        // weekend color
          endif
.         *
.         * today's date
.         *
          pack #dmTmpDate with ccyy,mm,work02
          rep " 0" in #dmTmpDate
          if (#dmTdysDate = #dmTmpDate)
             move ndx to #fmShapeCell
             setprop #spCalShape(ndx),BdrColor=Red
          endif
.         *
.         * current date
.         *
          if (days = ndd)
             move ndx to #fmCrntCell
             setprop #spCalShape(ndx),FillColor=yellow
          endif
       endif
    repeat
.   *
.   * gray previous months days
.   *
    if (#fmBegDay > 1)
.      * don't display calendar before min date
       move #psMinDate to work06
       move #psCrntDate to work06a
       if (work06 = work06a)    // blank out days before min date
          for ndx from "1" to (#fmBegDay - 1)
              setitem #stCalCell(ndx),0,""
              if (ndx = 1)
                 setprop #spCalShape(ndx),FillColor=$Window
              endif
          repeat
       else  // set previous months days to gray
          unpack #psCrntDate into ccyy,mm,dd
          move mm to nmm
          move ccyy to nyyyy
.         * get number of days in the previous month
          if (nyyyy >= #fmMinYear | nmm <> 1)
             if (nmm = 1)
                move "12" to nmm
                sub "1" from nyyyy
             else
                sub "1" from nmm
             endif
             call GetNbrDays using nmm,ccyy,nwork02

             move "0" to days
             calc days = nwork02 - (#fmBegDay - 1)
             for ndx from "1" to (#fmBegDay - 1)
                add "1" to days
                move days to work02
                setprop #stCalCell(ndx),fgcolor=$GrayText
                setitem #stCalCell(ndx),0,work02
                if (ndx = 1)
                   setprop #spCalShape(ndx),FillColor=$Window
                endif
             repeat
          endif
       endif
    endif
.   *
.   * gray next months days
.   * don't display calendar after max date
    move #psMaxDate to work06
    move #psCrntDate to work06a
    if (work06 = work06a)    // blank out days after max date
       for ndx from (#fmEndDay + 1) to "42"
          setitem #stCalCell(ndx),0,""
          if (ndx = 36 | ndx = 35 | ndx = 42)
             setprop #spCalShape(ndx),FillColor=$Window
          endif
       repeat
    else  // set next months days to gray
       move "0" to days
       for ndx from (#fmEndDay + 1) to "42"
          add "1" to days
          move days to work02
          setprop #stCalCell(ndx),fgcolor=$GrayText
          setitem #stCalCell(ndx),0,work02
          if (ndx = 36 | ndx = 35 | ndx = 42)
             setprop #spCalShape(ndx),FillColor=$Window
          endif
       repeat
    endif
.   *
.   * set focus to main calendar winow so left and right arrow keys work
.   *
    setfocus #wnPopUpCal
. *
    return
.------------------------------------------------------------------------------
. * date was clicked, find which and return to main calling program
CalendarCell_Click
.   *
.   * now see what cell was clicked
.   *
    move #fmCrntCell to nwork02
    eventinfo 0,objectid=ndx
    move ndx to #fmCrntCell

    unpack #psCrntDate into ccyy,mm,dd

    unpack "" into dd
    getitem #stCalCell(#fmCrntCell),0,dd
    reset dd
    setlptr dd
    rep " 0" in dd
.   *
.   * return if cell that was clicked had no date
.   *
    return if (dd = "00")

.   *
.   * see if they clicked on gray day
.   *
    getprop #stCalCell(#fmCrntCell),fgcolor=nwork10
    if (nwork10 = 2147483665)   // grayed out day
       move mm to nmm
       if (#fmCrntCell >= 1 & #fmCrntCell <= 7)    // top row sub 1 from month
          if (nmm = 1)
             move "12" to nmm
             move ccyy to nyyyy
             sub "1" from nyyyy
             move nyyyy to ccyy
          else
             sub "1" from nmm
          endif
       else      // bottom rows add 1 to month
          if (mm = "12")
             move "1" to nmm
             move ccyy to nyyyy
             add "1" to nyyyy
             move nyyyy to ccyy
          else
             add "1" to nmm
          endif
       endif
       move nmm to mm
    endif
    pack #psCrntDate with ccyy,mm,dd
    rep " 0" in #psCrntDate
.   *
.   * stop program and return
.   *
    call DateSelected
. *
    return
.------------------------------------------------------------------------------
. * today button was clicked
stTodaysDate_Clicked
    unpack #dmTdysDate into ccyy,mm,dd
    pack #psCrntDate with ccyy,mm,dd
.   *
.   * stop program and return
.   *
    call DateSelected
. *
    return
.------------------------------------------------------------------------------
. * Routine used by the previous and next buttons on the calendar
. * the ObjectID can tell us which button was pressed.
ChangeMonthYear
    eventinfo 0,objectid=nwork09
    unpack #psCrntDate into ccyy,mm,dd
    move mm to nmm
    move ccyy to nyyyy
    if (nwork09 = 100)      // previous button
       return if (nyyyy = #fmMinYear & nmm = #fmMinMonth)
       if (nmm = 1)
          move "12" to nmm
          sub "1" from nyyyy
       else
          sub "1" from nmm
       endif
    else if (nwork09 = 200)   // next button
       return if (nyyyy = #fmMaxYear & nmm = #fmMaxMonth)
       if (nmm = 12)
          move "1" to nmm
          add "1" to nyyyy
       else
          add "1" to nmm
       endif
    endif

    move nmm to mm
    move nyyyy to ccyy
    pack #psCrntDate with ccyy,mm,dd
    rep " 0" in #psCrntDate
    call SetCalendar
. *
    return
.------------------------------------------------------------------------------
. * scroll bars for year
VScrollBarYear_Change
    unpack #psCrntDate into ccyy,mm,dd
    move ccyy to nyyyy
    move mm to nmm

    if (nwork01 = 2 | nwork01 = 4)      // 2=up 4=at top add 1 to year
       return if (nyyyy = #fmMaxYear)
       add "1" to nyyyy
.      *
.      * make sure month is within range
.      *
       if (nyyyy = #fmMaxYear)
          if (nmm > #fmMaxMonth)
             move #fmMaxMonth to nmm
             move nmm to mm
          endif
       endif
    elseif (nwork01 = 1 | nwork01 = 5)  // 1=down, 5=at bottom sub 1 from year
       return if (nyyyy = #fmMinYear)
       sub "1" from nyyyy
.      *
.      * make sure month is within range
.      *
       if (nyyyy = #fmMinYear)
          if (nmm < #fmMinMonth)
             move #fmMinMonth to nmm
             move nmm to mm
          endif
       endif
    endif

    move nyyyy to ccyy
    pack #psCrntDate with ccyy,mm,dd

    rep " 0" in #psCrntDate
    call SetCalendar
. *
    return
.------------------------------------------------------------------------------
. * scroll bars for month
VScrollBarMonth_Change
    unpack #psCrntDate into ccyy,mm,dd
    move ccyy to nyyyy
    move mm to nmm

    if (nwork01 = 2 | nwork01 = 4)      // 2=up 4=at top add 1 to month
       if (nmm = 12)
          if (nyyyy = #fmMinYear)
             move #fmMinMonth to nmm
          else
             move "1" to nmm
          endif
       else
.         * don't go beyond maxMonth if maxYear, instead go to Jan.
          if (nyyyy = #fmMaxYear & nmm = #fmMaxMonth)
             move "1" to nmm
          else
             add "1" to nmm
          endif
       endif
    elseif (nwork01 = 1 | nwork01 = 5)  // 1=down, 5=at bottom sub 1 from month
       if (nmm = 1)
          if (nyyyy = #fmMaxYear)
             move #fmMaxMonth to nmm
          else
             move "12" to nmm
          endif
       else
          if (nyyyy = #fmMinYear & nmm = #fmMinMonth)
             move "12" to nmm
          else
             subtract "1" from nmm
          endif
       endif
    endif

    move nmm to mm
    pack #psCrntDate with ccyy,mm,dd
    rep " 0" in #psCrntDate
    call SetCalendar
. *
    return
.------------------------------------------------------------------------------
. * get number of days in a given month
#fmMM   form    2
#fmCCYY dim     4
#fmDim  form    ^
GetNbrDays lroutine #fmMM,#fmCCYY,#fmDim
.   *
.   * get number of days in the month
.   *
    load #fmDim from #fmMM of jandays,febdays,mardays,aprdays,maydays:
                             jundays,juldays,augdays,sepdays,octdays:
                             novdays,decdays
    if (#fmMM = 2)        // leap year?
       reset leap_yrs
       scan #fmCCYY in leap_yrs
       if equal
          move febldays to #fmDim
          reset leap_yrs
       endif
    endif
. *
    return
.------------------------------------------------------------------------------
#EventType   Form  4
#EventResult Form  11
#EventObjId  Form  8
#EventChar   Dim   1
#EventMod    Form  4
.------------------------------------------------------------------------------
. * create popupcal window
PopUpCal_Create
    create #wnPopUpCal=193:383:282:452:
           BgColor=16777215,Enabled=1:
           FgColor=2147483666,ObjectId=0,Title="Form001",MinBox=1:
           MaxBox=0,SysMenu=1,Caption=0,ClipCtrl=1,Appearance=2:
           WinType=1,AutoRedraw=1,ScrollBar=3,Font=#fntCalendar:
           GridAlign=0,GridSizeH=4,GridSizeV=4,Units=5,NegBorders=0:
           NegMenus=1,RightToLeft=0,NoMenuResize=0,tabid=10
.   * top border
    create #wnPopUpCal;#stTop=0:24:0:170,"",#fntCalendar:
           Alignment=2,BgColor=2147483663,Style=1:
           Enabled=1,FgColor=2147483666,ZOrder=100:
           BackStyle=1
.   * bottom border
    create #wnPopUpCal;#stBottom=168:192:0:170,"",#fntCalendar:
           Alignment=2,BgColor=2147483663,Style=1:
           DropId=0,Enabled=1,FgColor=2147483666,ZOrder=100
.   * cancel button
    create #wnPopUpCal;#btCancel=172:188:148:164,"X":
           DropId=0,Enabled=1,HelpId=0,ObjectId=0:
           TabId=0,ZOrder=360,Cancel=1,Default=0:
           Font=#fntCalendar:
           ToolTip="Click Here to Cancel",CauseValid=1
.   * previous month button
    create #wnPopUpCal;#btPCalBack=4:20:4:20,"t",DropId=0,Enabled=1,HelpId=0:
           ObjectId=100,TabId=0,ZOrder=360,Cancel=0,Default=0:
           Font=#fntWing,ToolTip="Previous Month",CauseValid=1
.   * next month button
    create #wnPopUpCal;#btPCalForward=4:20:148:164,"u",DropId=0,Enabled=1:
           HelpId=0,ObjectId=200,TabId=0,ZOrder=360,Cancel=0,Default=0:
           Font=#fntWing,ToolTip="Next Month",CauseValid=1
.   * line border
    create #wnPopUpCal;#lnBorder=44:44:0:168,BdrColor=0,ObjectId=0,ZOrder=140:
           BdrWidth=1,DrawMode=2,BdrPattern=6
.   *
.   * create day of week stattexts
.   *
    move "0" to left
    for ndx from "1" to "7"
       move DOWlist(ndx) to work03
       create #wnPopUpCal;#stDay(ndx)=28:48:left:(left + 24),work03,#fntCalendar:
              Alignment=1,BgColor=16777215,BdrColor=0,Style=2:
              Enabled=1,FgColor=2147483666,ZOrder=120
       add "24" to left
    repeat
.   * range stattext
    create #wnPopUpCal;#stRange=172:188:8:146,"",#fntCalendar:
           Alignment=2,BgColor=16777215,Style=2:
           Enabled=1,FgColor=2147483666,ZOrder=310,BackStyle=2
.   * today button
    create #wnPopUpCal;#stToday=144:164:116:168,"&Today",#fntCalendar:
           Alignment=1,BgColor=$ActiveCaption,Border=1,BdrColor=0,Style=4,DropId=0:
           Enabled=1,FgColor=$CaptionText,ObjectId=0,ZOrder=360,BackStyle=1:
           UseAltKey=1,ToolTip="Click Here to Select Today's Date",CauseValid=1
.   * vscrollbar month
    create #wnPopUpCal;#vsbMonth=4:20:24:37,1,12,1,DropId=0,Enabled=1,HelpId=0:
           ObjectId=0,TabId=0,ZOrder=360,ToolTip="Go to Another Month",CauseValid=1
.   * month button
    create #wnPopUpCal;#btMonth=4:20:36:96,"",DropId=0,Enabled=1,HelpId=0:
           ObjectId=0,TabId=0,ZOrder=940,Cancel=0,Default=0,Font=#fntCalendar:
           ToolTip="Click to Select Month",CauseValid=1
.   * vscrollbar year
    create #wnPopUpCal;#vsbYear=4:20:132:145,0,500,10,DropId=0,Enabled=1:
           HelpId=0,ObjectId=0,TabId=0,ZOrder=360:
           ToolTip="Go to Another Year",CauseValid=1
.   * year button
    create #wnPopUpCal;#btYear=4:20:96:132,"",DropId=0,Enabled=1,HelpId=0:
           ObjectId=0,TabId=0,ZOrder=950,Cancel=0,Default=0,Font=#fntCalendar:
           ToolTip="Click to Select Year",CauseValid=1
.   *
.   * register objects events
.   *
    eventreg #wnPopUpCal,$CLOSE,Close_wnPopUpCal
    eventreg #wnPopUpCal,$KeyPress,KeyPress_wnPopUpCal:
             Result=#EventResult,Char=#EventChar

    eventreg #btCancel,$CLICK,Click_btCancel
    eventreg #btPCalBack,$CLICK,Click_btPCalBack
    eventreg #btPCalForward,$CLICK,Click_btPCalForward
    eventreg #stToday,$CLICK,Click_stTodaysDate
    eventreg #vsbMonth,$CHANGE,Change_vsbMonth:
             Type=#EventType,Result=#EventResult,ObjectId=#EventObjId:
             Char=#EventChar,Modifier=#EventMod
    eventreg #vsbYear,$CHANGE,Change_vsbYear:
             Type=#EventType,Result=#EventResult,ObjectId=#EventObjId:
             Char=#EventChar,Modifier=#EventMod
    eventreg #btMonth,$CLICK,btMonth_Click
    eventreg #btYear,$CLICK,btYear_Click
.   *
.   * activate objects
.   *
    activate    #stTop
    activate    #stBottom
    activate    #btCancel
    activate    #btPCalBack
    activate    #btPCalForward
    activate    #btMonth
    activate    #btYear
    activate    #lnBorder
    activate    #stRange
    activate    #stToday
    activate    #stDay
    activate    #vsbMonth
    activate    #vsbYear
.   *
.   * create day objects
.   *
    getprop #stDay(1),left=#fmStLeft,top=nwork04,height=nwork04a
    calc #fmStTop = nwork04 + nwork04a
    calc #fmTop = #fmStTop - 16
    for ndx from "1" to "42"
       if (ndx = 1 | ndx = 8 | ndx = 15 | ndx = 22 | ndx = 29 | ndx = 36)
          calc #fmTop = #fmTop + 16
          calc #fmLeft = #fmStLeft - 24
       endif
       add "24" to #fmLeft
       move ndx to nwork02
       move nwork02 to work02
       create #wnPopUpCal;#stCalCell(ndx)=#fmTop:(#fmTop + 16):#fmLeft:(#fmLeft + 24):
                  "",#fntCalendar,Alignment=1,ObjectId=ndx,ZOrder=250:
                  FgColor=$BtnText,BackStyle=2
       activate #stCalCell(ndx)
       eventreg #stCalCell(ndx),4,CalendarCell_Click
       calc nwork04 = #fmTop - 1
       create #wnPopUpCal;#spCalShape(ndx)=nwork04:(nwork04 + 17):#fmLeft:(#fmLeft + 24):
                  BgColor=16777215,ZOrder=10:
                  BdrColor=$Window,FillStyle=5,Shape=1:
                  BdrWidth=2
       activate #spCalShape(ndx)
    repeat

    move "1" to #fmCrntCell,#fmShapeCell
.   *
.   * position calendar on screen based off edittext object and calling window
.   * Calculate the position for the calendar window
.   * get desktop size
.   *
    getinfo system,work150
    reset work150 to 13
    move work150 to work04
    move work04 to ScrWdth
    reset work150 to 17
    move work150 to work04
    move work04 to ScrHght

    getprop #psMnWindow,top=#fmMnTop,left=#fmMnLeft
    getprop #psEditObj,top=#fmEOTop,left=#fmEOLeft,height=#fmEOHeight,width=#fmEOWidth
.   *
.   * check to see if will fall off screen
.   *
.   * top
.   *
    calc #fmCalTop = #fmMnTop + #fmEOTop + 20 + #fmEOHeight
    calc nwork04 = #fmCalTop + #fmCalHeight
    if (nwork04 > ScrHght)
       calc #fmCalTop = #fmMnTop + #fmEOTop - #fmCalHeight + 20
    endif
.   *
.   * left
.   *
    calc nwork04 = #fmMnLeft + #fmEOLeft + 3 + #fmCalWidth
    if (nwork04 > ScrWdth)
       calc #fmCalLeft = #fmMnLeft + #fmEOLeft - (#fmCalWidth - #fmEOWidth)
    else
       calc #fmCalLeft = #fmMnLeft + #fmEOLeft + 3
    endif
    setprop #wnPopUpCal,top=#fmCalTop,left=#fmCalLeft
. *
    return
.------------------------------------------------------------------------------
Close_wnPopUpCal
    call PopUpCal_Canceled
    return

KeyPress_wnPopUpCal
    if (#EventResult = 37)
       setprop #wnPopUpCal,objectid=100
       call ChangeMonthYear
    else if (#EventResult = 39)
       setprop #wnPopUpCal,objectid=200
       call ChangeMonthYear
    endif
    return

Click_btCancel
    call PopUpCal_Canceled
    return

Click_btPCalBack
	call ChangeMonthYear
    return

Click_btPCalForward
	call ChangeMonthYear
    return

Click_spExample
	call stTodaysDate_Clicked
    return

Click_stTodaysDate
	call stTodaysDate_Clicked
    return

Change_vsbMonth
	move #eventmod to nwork01
	call VScrollBarMonth_Change
    return

Change_vsbYear
	move #eventmod to nwork01
	call VScrollBarYear_Change
    return

#faMonths  form  2(12)
btMonth_Click
    unpack "" into work150
    unpack #psCrntDate into ccyy
    move ccyy to nyyyy
    if (nyyyy = #fmMinYear)
       move #fmMinMonth to nwork02
       move "12" to nwork02a
    else if (nyyyy = #fmMaxYear)
       move "1" to nwork02
       move #fmMaxMonth to nwork02a
    else
       move "1" to nwork02
       move "12" to nwork02a
    endif
    move "0" to count
    clear #faMonths
    for ndx from nwork02 to nwork02a
       append ";" to work150
       append long_month(ndx) to work150

       add "1" to count
       move ndx to #faMonths(Count)
    repeat
    reset work150
    create #wnPopUpCal;#fmnMonths=2:2:2:25,work150

    getprop #btMonth,top=nwork04,left=nwork04a
    setprop #fmnMonths,top=(nwork04 + 16),left=1
    activate #fmnMonths,fmnMonths_Click,result
. *
    return

fmnMonths_Click
    unpack #psCrntDate into ccyy,mm,dd
    move #faMonths(result) to mm
    pack #psCrntDate with ccyy,mm,dd

    rep " 0" in #psCrntDate
    call SetCalendar
. *
    return

btYear_Click
    unpack #psCrntDate into ccyy,mm,dd
    unpack "" into work1024
    move #fmMinYear to nwork04
    move "0" to ndx,curindex
    loop
       add "1" to ndx
       append ";" to work1024
       move nwork04 to work04
       if (work04 = ccyy)
          move ndx to curindex
       endif
       append work04 to work1024
       add "1" to nwork04
    while (nwork04 <= #fmMaxYear)
    repeat
    reset work1024
    getprop #btyear,top=nwork04,left=nwork04a
    create #wnPopUpCal;#pmnYears=(nwork04 + 16):(nwork04 + 21):nwork04a:(nwork04a + 48):
           "",work1024
    eventreg #pmnYears,$LostFocus,pmnYears_LostFocus
    eventreg #pmnYears,$CLICK,pmnYears_Click
    setprop #pmnYears,tabid=100
    activate #pmnYears
    setitem #pmnYears,0,curindex
    setfocus #pmnYears
. *
    return

pmnYears_Click
    unpack #psCrntDate into ccyy,mm,dd
    move mm to nmm
    getitem #pmnYears,0,nwork03
    getitem #pmnYears,nwork03,ccyy
    destroy #pmnYears
    move ccyy to nyyyy
    if (#fmMaxYear = nyyyy & nmm > #fmMaxMonth)
       move #fmMaxMonth to nmm
    else if (#fmMinYear = nyyyy & nmm < #fmMinMonth)
       move #fmMinMonth to nmm
    endif
    move nmm to mm
    pack #psCrntDate with ccyy,mm,dd

    rep " 0" in #psCrntDate
    call SetCalendar
. *
    return
pmnYears_LostFocus
    destroy #pmnYears
. *
    return
.------------------------------------------------------------------------------
.------------------------------------------------------------------------------
