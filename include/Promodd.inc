.----------------------------------------------------------------
.
.   Promodd.inc -
.
.   Copyright(c) Names in the News Ca., Inc. 2013
.
.   release         1.0             .merge of mrkt data into master contact file   
.                                   .this dd is used for outputing promo list(s)    
.   Author: David L Herrick
.   Created: 2013 August 14
.----------------------------------------------------------------

PromoFile          file          fixed=325
PromoName        Dim            25
PromoName1       Init           "PromoList"

PromoVars        List
PromoCoName      Dim            55      1-55
PromoCntName     Dim            45     56-100       Contact Name
PromoAddr1       dim            35    101-145
PromoAddr2       dim            35    146-180
PromoCity        Dim            35    181-215
PromoState       Dim            2     216-217
PromoZip         Dim            10    218-227
PromoDtacrd      Dim            1     228-228       T=member of datacard list
PromoPromo       Dim            1     229-229       T=member of Promo list
PromoHlydy       Dim            1     230-230       T=Member of Holiday list
PromoNews        Dim            1     231-231       T=Member of Newsletter list
PromoParty       Dim            1     232-232       T=Member of Party List
PromoActive      Dim           1      233-233      T=Member of Active List    
PromoPrsct       Dim           1      234-234      T=Member of Prospect List    
PromoVndr        Dim           1      235-235      T=Member of Vendor List    
PromoWeek        Dim          1       236-236      T=Member of weekly promo list
PromoBrkr       Dim           1       237-237      T=Member of broker promo list
PromoDMA         Dim          1       238-238      T=Member of DMA promo list      
Promoclnt       Dim           1       237-237      T=Member of client promo list
PromoEmail     Dim           50       238-287        
PromoPhone     Dim          10        288-297        
PromoFax       Dim          10        298-307        
PromoComp      Dim           6        308-313            .company #
PromoCnt       Dim   3                314-316             .contact #
PromoFill2     Dim          9         317-325
               Listend
.

.
PromoFlag           Form            1                - = 1 If open

