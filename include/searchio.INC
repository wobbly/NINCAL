...................................................................
.
. SEARCH INCLUSION
.
. HANDLES ALL COMMON ROUTINES USED WITH SEARCH.PLF
.
. 
. RELEASE	1.5	DLH 2014 July 28 search for Service B.
. RELEASE	1.4	DLH 1/18/07 search for Fulfillment (RTN)
. RELEASE	1.3	ASH 10/26/04 ADDED LOGIC TO DISPLAY COMPANY NUMBER
. RELEASE	1.2	ASH 09/17/02 ADDED OWNER
. RELEASE       1.1     ASH 01/31/02 REPLACED DATALIST WITH MSLISTVIEW OBJECT
. RELEASE       1.0     ASH 03/10/99 INITIAL RELEASE
....................................................................
....................................................................
. ENTRY POINT: SEARCHSETTITLE
. REQUIRED   : SEARCH.PLF
. RETURNED   :
. DESCRIPTION: DYNAMICALLY CHANGES TITLE ON SEARCH.PLF
. PROGRAMMER : ANDREW HARKINS   03/10/99
.              INITIAL RELEASE
....................................................................
SearchSetTitle
.Dynamically changes
.START PATCH 1.4 ADDED LOGIC
.START PATCH 1.5 ADDED LOGIC
        branch  SrchFlag to SearchSetTit1,SearchSetTit2,SearchSetTit3,SearchSetTit4,SearchSetTit5,SearchSetTit6,SearchSetTit7,SearchSetTit8
.        branch  SrchFlag to SearchSetTit1,SearchSetTit2,SearchSetTit3,SearchSetTit4,SearchSetTit5,SearchSetTit6,SearchSetTit7
.End PATCH 1.5 
.       branch  SrchFlag to SearchSetTit1,SearchSetTit2,SearchSetTit3,SearchSetTit4,SearchSetTit5,SearchSetTit6
.End PATCH 1.4 ADDED LOGIC
SearchSetTit1       
        setprop Search,title="Broker Search"
        goto    SearchSetTitDone
SearchSetTit2
        setprop Search,title="List Search"
        goto    SearchSetTitDone
SearchSetTit3
        setprop Search,title="Mailer Search"
        goto    SearchSetTitDone
SearchSetTit4
        setprop Search,title="Ship-To Search"
        goto    SearchSetTitDone
SearchSetTit5
        setprop Search,title="Campaign Search"
        goto    SearchSetTitDone
.START PATCH 1.2 ADDED LOGIC
SearchSetTit6
        setprop Search,title="Owner Search"
        goto    SearchSetTitDone
.END PATCH 1.2 ADDED LOGIC
.START PATCH 1.4 ADDED LOGIC
SearchSetTit7
        setprop Search,title="Fulfillment (RTN-To) Search"
        goto    SearchSetTitDone
.END PATCH 1.4 ADDED LOGIC
.START PATCH 1.5 ADDED LOGIC
SearchSetTit8
        setprop Search,title="Service Bureau Search"
        goto    SearchSetTitDone
.END PATCH 1.4 ADDED LOGIC
SearchSetTitDone
        return

....................................................................
. ENTRY POINT: SEARCHSETVISIBLE
. REQUIRED   : SEARCH.PLF
. RETURNED   :
. DESCRIPTION: REFRESHES SEARCH.PLF AND MAKES VISIBLE
. PROGRAMMER : ANDREW HARKINS   03/10/99
.              INITIAL RELEASE
....................................................................
SearchSetVisible
        setitem SearchComboBox,0,SrchFlag
        setitem SearchCheckFree,0,1
        setitem SearchEditText,0,""
        setitem SearchStatText2,0,""
        setitem SearchStatText3,0,""
        setitem SearchStatText4,0,""
.START PATCH 1.3 REPLACED LOGIC
        setitem SearchStatText6,0,""
.END PATCH 1.3 REPLACED LOGIC
.START PATCH 1.1 REPLACED LOGIC
.        deleteitem SearchDataList,0
	SearchListView.ListItems.Clear
.END PATCH 1.1 REPLACED LOGIC
        setfocus SearchEditText
        setprop Search,visible=1
        return
        
....................................................................
. ENTRY POINT: SEARCHENABLE
. REQUIRED   : SEARCH.PLF
. RETURNED   :
. DESCRIPTION: ENABLES BUTTONS, USUALLY FOLLOWED BY ANOTHER 'RETURN' TO
.              SEARCH.PLF AFTER AN ERROR MESSAGE REGARDING SEARCHING.
. PROGRAMMER : ANDREW HARKINS   03/10/99
.              INITIAL RELEASE
....................................................................
SearchEnable
        setfocus SearchEditText
        reset   SrchKey
        setprop SearchOK,enabled=1
        setprop SearchClose,enabled=1
        setprop SearchComboBox,enabled=1
        return

....................................................................
. ENTRY POINT: SEARCHCLEAR
. REQUIRED   : SEARCH.PLF
. RETURNED   :
.START PATCH 1.1 REPLACED LOGIC
.. DESCRIPTION: CLEARS DATALIST AND STATTEXT BOXES, USUALLY BEFORE A SEARCH
. DESCRIPTION: CLEARS LISTVIEW AND STATTEXT BOXES, USUALLY BEFORE A SEARCH
.END PATCH 1.1 REPLACED LOGIC
. PROGRAMMER : ANDREW HARKINS   03/10/99
.              INITIAL RELEASE
....................................................................
SearchClear
.START PATCH 1.1 REPLACED LOGIC
.        deleteitem SearchDataList,0
	SearchListView.ListItems.Clear
.END PATCH 1.1 REPLACED LOGIC
        setitem SearchStatText2,0,""
        setitem SearchStatText3,0,""
        setitem SearchStatText4,0,""
.START PATCH 1.1 ADDED LOGIC
        setitem SearchStatText5,0,""
.END PATCH 1.1 ADDED LOGIC
.START PATCH 1.3 REPLACED LOGIC
        setitem SearchStatText6,0,""
.END PATCH 1.3 REPLACED LOGIC
        return                
