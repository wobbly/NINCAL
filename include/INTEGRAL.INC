FILE SPECS

*******************************************
***************LRFile.dat******************
*******************************************
Order Status				1	Text		“0”=Live, “B”=Billed, “Q”=Cancelled Billed, “X”=Cancelled, “l”=LCR, “z”=Cancelled LCR, “p”=Pending, “x”=Cancelled Pending
Mailer Number				4	Numeric
LR Number (Key)				6	Numeric
List Number				6	Numeric
List Owner Number			4	Numeric
Mailer Purchase Order Number		12	Text
Order Quantity				9	Numeric
Mailer Key				12	Text
Return Date				8	Numeric
Mail Date				8	Numeric
Entire/Rent/Exchange Code		1	Numeric		“1”=Entire, “2”=Exchange, “3”=Entire & Exchange
Order Net Quantity			9	Numeric
Order Campaign Number			6	Numeric
*Exchange Price per Thousand		5	Numeric
Return To Company Number		4	Numeric
NINCA Contact Code			2	Numeric
NINCA Caller Code			2	Numeric
Order Date				8	Numeric
Order Exchange Quantity			9	Numeric
Broker Number				4	Numeric
Broker Contact Number			3	Numeric
Net Name Percentage			2	Numeric
Net Price (Run Fee/Charge)		6	Decimal
Mailer Name				45	Text
Owner Name				25	Text
Broker Name				45	Text
Merge Percentage			10	Numeric		Merge % Percentage for this LR
Last Merge % for Mlr/List		6	Numeric		Merge % for LR with this Mlr/List with latest Order Date
LCR/Pending Order SubStatus Desc.	45	Text		Will remain blank until LCRs/Pending Orders are sent to Integral
Gross Billed Quantity			8	Numeric
Net Merge Quantity			8	Numeric
Total Billed (AR)			10.2	Numeric		(total of 13 bytes)
Base Price				5.2	Numeric
Select Price				5.2	Numeric		If applicable
Price Calculator			20	Text		"/m", "Flat", etc.
Select Number				4	Numeric		Valid Number OR "XXXX" = keyed in Select OR "----" = Order placed before new Datacard logic
Select Name				75	Text
Test/Continuation Code			1	Numeric		"1"=Test, "2"=Retest, " "=Continuation

*********************************************
***************ListFile.dat******************
*********************************************
List Number(Key)			6	Numeric
List Name				75	Text
Status					1	Text		"W"=Withdrawn, "T"=Temporarily Withdrawn
Universe				9	Numeric
Gender Info				15	Text
Owner Number				4	Numeric

*********************************************
***************SelFile.dat******************
*********************************************
List Number(Key)			6	Numeric
Select Number (Key)			4	Unique Numeric Identifier
Select Name				75	Text
Select Quantity				10	Numeric
Select Price				5.2	Numeric		(Total of 8 bytes)
								If the record is a Base/Secondary Base or unassociated Select,
								this field represents the total Select Price.  If the record
								does have an associated Base/Secondary Base, this field would need
								to be added to the Select Price of the associated Base/Secondary Base
								in order to achieve the correct total.  It was done this way to allow
								different print options.
Commission Flag				1	Numeric
Price Modifier				3	Numeric
Inactive Flag				1	Numeric		'1' = Inactive Select
Select Status				1	Numeric		'1' = Special (does not print), '2' = Office Use Only
Notes Flag				1	Numeric		'1' = User should view Notes field in corresponding Datacard
Exchange Flag				1	Numeric		' ' of '0' = Exchange or Rent, '1' = Exchange Only, '2' = Rental Only
Base Association			4	Text		'BASE' = Base Select - 1 per Datacard
								'SEC.' = Secondary Base Select - 0+ per Datacard
								A 4 byte Numeric field in this location establishes this record as a Select
								off of a Base/Secondary Base.  That 4 byte number would be the corresponding
								Select Number of a Base/Secondary Base record.
								There is the possibility of no data in this location.  This would pertain to a Select
								that is independent of an associated Base/Secondary Base, but is not a Base/Secondary Base
								itself.
Index Flag				4	Numeric		Established Print/Display order of Select Records for a Datacard
Select Date				8	Numeric
Initials				3	Text
Filler					11	Text

*********************************************
***************TxtFile.dat******************
*********************************************
(Unique identifier starts at '1', allowing up to nine 500 byte records of Free TExt per Datacard.)
Datacard Number				6	Numeric
Text Number				1	Numeric
Text					500	Text

********************************************
***************PkgFile.dat******************
********************************************
(The following fields are mutually exclusive:  )
(Master Package Indicator)
(Master Package Associaton Number)
(Note on Master Packages:  A Master Package will have an Indicator value of "1",)
(and the Association Number will be cleared.  Regular Packages may have a valid)
(Master Package Number in the Association Number field.)
Mailer Number(Key)			6	Numeric
Package Number(Key)			6	Numeric
Package Name				150	Text
Client Package ID			25	Text
Package Date				8	Numeric
Package Notes				500	Text
Master Package Indicator		1	Numeric
Master Package Association Number	6	Numeric
Filler					91	Text

Cross-Reference for New 6 byte Mailer Number
____________________
New      	Old
____________________
000619    	0173 (TNC)
000811       	0055 (USF)
000933       	0193 (DCCC)
000861      	0108 (EarchJustice)
001016     	0308 (HRC)
000899       	0153 (League of Cons.)
003948     	4261 (21st Century)
002094     	1921 (Witness)
000830       	0075 (PFAW)
000843       	0088 (EngenderHealth)
001173     	0532 (OxFam)
001385     	0808 (Amer. Rivers)
007754     	8881 (WaterKeeper)
000590       	0031 (AFT)
008218     	9366 (Dean 4 Amer.)
004117     	4450 (Ducks Unlim.)
000828       	0073 (NOW)
008637     	0276 (NGLTF)
000834       	0079 (Int. Rescue)
000913       	0170 (NWF)
001238		0620 (NPCA)
002859		3028 (Farm Sanctuary)
000929		0188 (Defenders of Wildlife)
001226		0601 (CARE)
001882		1604 (Special Olympics International)
001082		0400 (PETA)
000814		0059 (Kennedy for Senate)
009318		1443 (Committee for a Democratic Majority)

*********************************************
***************PkgPFile.dat******************
*********************************************
Mailer Number(Key)			6	Numeric
Package Number(Key)			6	Numeric
Price Date(Key)				8	Numeric
Package Print Price			6.2	Numeric		(total of 9 bytes)
Package Postage Price			6.2	Numeric		(total of 9 bytes)
Package Premium Price			6.2	Numeric		(total of 9 bytes)
Package Total Price			8.2	Numeric		(total of 10 bytes)
Price Notes				200	Text
Filler					97	Text

*********************************************
***************XRefFile.dat******************
*********************************************
List Number(Key)			6	Numeric
Mailer Number(Key)			6	Numeric

*********************************************
***************ExchFile.dat******************
*********************************************
(Note on Inactivated Date:  This field will USUALLY contain valid data)
(if the Flag has a value of "I".  Otherwise do not be surprised to garbage)
(in this field!)
Mailer1 Number(Key)			6	Numeric
Mailer2 Number(Key)			6	Numeric
Inactivated Date			8	Text** See Notes
Entry Number				5	Numeric
Flag					1	Text		" "=normal, "I" = inactive
Mailer1 Usage				10	Numeric
Mailer2 Usage				10	Numeric

*********************************************
***************PrcFile.dat******************
*********************************************
(Additional Prices associated with an Order)
LR Number (Key)				6	Numeric
LR/LOL Code (Key)			1	Numeric
Additional Price Code (Key) 		1	Text		"A" = Fixed Fees
Additional Price Number	(Key)		3	Numeric
Additional Price Amount			5.2	Numeric
Additional Price Text			46	Text
Additional Price Calculator		20	Text

*********************************************
***************PkgFile2.dat******************
*********************************************
LR Number (Key)				6	Numeric
Package Record Code (Key)		1	Numeric		"1" = LR, "0" = LOL
Package Number				6	Numeric
Package Total Price			8.2	Numeric		(total of 10 bytes)

*********************************************
***************ConFile.dat******************
*********************************************
Contact Number (Key)			2	Numeric
Contact Name				35	Text
Contact Phone Number			25	Text
Contact Port Number			3	Numeric
Team Number				2	Numeric		(Not implemented)
Rights					1	Numeric		(Not implemented)
Rights 2				40	Numeric		(Not implemented)
Default Printer				1	Numeric
Caller/Planner				1	Numeric
