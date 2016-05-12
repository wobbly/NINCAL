;----------------------------------------------------------------
;
;   DMEXFDD.INC -
;
;   Copyright(c) Names in the News 2005
;
;   Author: David L Herrick
;----------------------------------------------------------------
;   Release         1.0                2005 May
;   Secondary Company file.
;   COntains data regarding electronic information transfer.
;..............................................
DMEXFILE      FILE
DMEXNAME      Dim             250
DMEXPATH      FORM            1
DMEXFLAG      FORM            1
DMEXFLD        DIM            6
;
DMEXLOCK      FORM            1

DMVARs        list
DMREc         dim             3              "SHP" - for Shipping records, "RCP" - for ORder receipt acknowledgement
DMTDate       Dim             19               transmission date
DMLname       dim             255
DMMName       Dim             255
DMPO          dim             50
;DMQshp       form            8
DMQshp        dim             8
DMVia         dim             50
DMSdate       dim             19
DMAirB        Dim             50
DMMedia       Dim             50
DMSer         Dim             50
;DMChrg       form            8
DMChrg        dim             8
DMMdate       dim             19
DMSEl         dim             255
;DMOqty       form            8
DMOqty        dim             8
DMLR          dim             50
DMODate       dim             19
;DMcnt        init            "List Management"
;DMPhone      INit            "(415)989-3350"
;DMEmail      Init            "ListRequest@nincal.com"
DMcnt         dim             255
DMPhone       dim             255
DMEmail       dim             255
              listend
;DMExcahnge Record Layout
;Record Type                   1-3    SHP or RCp  ------ cannot be in double quotes Per DMExchange 3/27/06
;Transmission Date            Fixed 19 mm/dd/ccyy hh:MM
;List Name                    Max 255    Must be in Double Quotes ie  "" list name ""
;Mailer Name                  Max 255    Must be in Double Quotes ie  "" list name ""
;Broker Order #               Max 50    Must be in Double Quotes  ie "" list name ""
;qyt shipped                  max 8
;Shipped Via                  MAx 50    Must be in Double Quotes  ie "" list name ""
;Shipped Date                 Fixed 19 mm/dd/ccyy hh:MM
;Airbill #                    Max 50     Must be in Double Quotes  ie  "" list name ""
;Material SHipped             Max 50     Must be in Double Quotes  ie  "" list name ""
;Volume Serial #              Max 50     Must be in Double Quotes  ie  "" list name ""
;Shipping Charge              Max 8
;Mail Date                    Fixed 19 mm/dd/ccyy hh:MM
;Select                       Max 255    Must be in Double Quotes ie  "" list name ""
;order qty                    Max 8
;Our LR#                      Max 50 Must be in Double Quotes ie  "" list name ""
;Order Date                   Fixed 19 mm/dd/ccyy hh:MM
;O/m contact name                                           Max 255    Must be in Double Quotes ie  "" list name ""
;O/m contact phone                                          Max 255    Must be in Double Quotes ie  "" list name ""
;O/m contact email                                          Max 255    Must be in Double Quotes ie  "" list name ""
