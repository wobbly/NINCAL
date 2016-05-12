......................................
.NCNTDD INCLUSION
.NIN Contact/Caller File Definition
.
.2013 August beef up sales portion change ncnt0001  to only use active records for dupe port check
.19 June 2012 add ninuser file data. will ultimately get rid of ninuser
.20May 2009 DLH add two bytes to hold Salesperson # to ultimately get rid of oslspern
.FILE NAME:  NINCNT.DAT
.REC LENGTH: 135 FIXED
.INDEX KEY:  (1) 001-002  (CNTNUM)
.            (2) 063-065  (CNTPORT)
.
.AAMDEX KEY: (1) 003-037  (CNTNAME)
.
.NCNTNAME        INIT    "NINCNT"
.NCNTNME1        INIT    "NINCNT"
.NCNTNME2        INIT    "NINCNT2"

NCNTNAME        INIT    "NINCNT.aam|NINS1:502  "
NCNTNME1        INIT    "NINCNT.isi|NINS1:502  "
NCNTNME2        INIT    "NINCNT2.isi|NINS1:502"
NCNTNME3        INIT    "NINCNT3.isi|NINS1:502"
NCNTNME4        INIT    "NINCNT4.isi|NINS1:502"

NCNTFLIST FILELIST
NCNTFILE        IFILE   KEYLEN=2,FIXED=135,Name="NINCNT.isi|NINS1:502  "
NCNTFIL1        IFILE   KEYLEN=3,FIXED=135,Name="NINCNT2.isi|NINS1:502"
NCNTFIL2        AFILE   FIXED=135,Name="NINCNT.aam|NINS1:502  "
NCNTFIL3        IFILE   KEYLEN=3,FIXED=135,Name="NINCNT3.isi|NINS1:502"
.Sales only programmer needs to check active status
NCNTFIL4        IFILE   keylen=2,FIXED=135,Name="NINCNT4.isi|NINS1:502"
          FILELISTEND

NCNTFLD         DIM     2      CNTNUM (Primary Index)
NCNTFLD1        DIM     3      CNTPORT (Secondary Index)
NCNTFLD2        DIM     38     CNTNAME (Primary Aamdex)
NCNTFLD3        DIM     3      CNTINITS(Secondary Index)
NCNTFLD4        DIM     2      CNTSALES(Secondary Index)
NCNTFLAG        FORM    1
NCNTFLG1        FORM    1
NCNTFLG2        FORM    1
NCNTPATH        FORM    1        .1=cntnum,2=aim(left over),3=cntport,4=salesnumber
NCNTLOCK        FORM    1
.
.
CNTVARS         LIST
CNTNUM          DIM     2       001-002 Contact # - KEY    - duplicates allowed but only one should be active, zero filled
CNTNAME         DIM     35      003-037 Contact Name
CNTPHONE        DIM     25      038-062 Contact Phone # - Allows standard
.                               14 digit #: "(XXX) XXX-XXXX" with possible extension #
CNTPORT         DIM     3       063-065 Port # for Contact           
CNTTEAM         DIM     2       066-067 Team # for Contact
CNTRIGHTS       DIM     1       068-068 Contact Rights:  "1" - Access to ALL Contacts
.                                                              CNTRIGHTS2 not needed
.                                                              Modification of all Contacts, including Captains
.                                                        "2" - Captain - Access to all Team members
.                                                              CNTRIGHTS2 needed to access other Teams
.                                                              Modification only other Team Members
.                                                        "3" - Team - Access to all Team members
.                                                              CNTRIGHTS2 needed to access other Teams
.                                                              No modification allowed
.                                                        "4" - Individual - No outside access
.                                                              CNTRIGHTS2 needed to access other Contacts
.                                                              No modification allowed
CNTRIGHTS2      DIM     40      069-108 Accessible Outside Contacts
.                                       Maximum of 20 extra Contacts (20 * 2byte CNTNUM = 40)
CNTPRINT        FORM    1       109-109 Printer Default
CNTCNT          FORM     1       110-110 "1" = Contact a valid Caller/Contact/Planner
.if cntcnt is not "1" this record will not display in the contact comboboxes in the order program(s)
CNTComp         DIM     1      111-111  "1"=NIN, "2"=PLI Filler
.CNTFILLER       DIM     25      111-135 Filler
.CNTFILLER       DIM     24      112-135 Filler
CntSales        Dim     2       112-113   two byte salesperson # from Oslspern.inc, zero filled
CntInactive     Dim      1      114-114   ."Y" = Inactive Sales person
CntInactiv1     Dim      1      115-115   ."Y" = Inactive Contact
CNTINITS        Dim      3      116-118
CNTFILLER       DIM     17       119-135 Filler
                LISTEND
.. .............................................................................
..CONTACT1/INC
.. .CONTACT TABLE USED FOR DISPLAY AND PRINT PROGRAMS
.. 
.OCNT      DIM       40
.OCNT0    INIT      "Billing(415) 291-5621"
.OCNT1    INIT      "Suzie McGuire(415) 291-5637"
.OCNT2    INIT      "Catherine Osuna(415) 291-5665"
..OCNT2    INIT      "jennifer ost" left 9/92
..OCNT2    INIT      "glendi henion"
..OCNT3    INIT      "laura trupin"
.OCNT3    INIT      "Jereza Mendoza(415) 291-5656"
..OCNT4    INIT      "tina maultsby"
..OCNT4    INIT      "catherine veyna"
..OCNT4    INIT      "melinda rivera"     1/8/92
..OCNT4    INIT      "stacey white"      3/2/93
.OCNT4     INIT      "Jeanette Cassano(415) 291-5644"
..OCNT4    INIT       "leola wong"   5/93     
.OCNT5     INIT      "Elki Taba(415) 291-5669"
..OCNT5     INIT      "Mary Gregory(415) 291-5669"
..OCNT5    INIT      "jennifer post"
..OCNT5    INIT      "marguerite salmon"
..OCNT5    INIT      "jason cambern"
..OCNT5    INIT      "derek glass"
..OCNT6    INIT      "michael dekker"
..OCNT6    INIT      "karen gallion"
.OCNT6    INIT      "Gillian Cavana(415) 291-5668"
..OCNT6    INIT      "Elissa Buchanan(415) 291-5668" 11/10/97
..OCNT7    INIT      "linda mietus"
..OCNT7    INIT      "Cheryl Piry(415) 291-5667" 12/98
..OCNT7    INIT      "Laramie Howard(415) 291-5667" 01/10/2000
.OCNT7    INIT      "Shana Betner (415) 291-5621"
..OCNT8   INIT      "Jone rabaino"
..OCNT8    INIT      "michael garland"
..OCNT8    INIT      "liora natelson"
..OCNT8    INIT      "Crispin Herrick(415) 291-5655" 4/1/98
..OCNT8    INIT      "Marilyn Brookins(415) 291-5609"
.OCNT8    INIT      "Lisa Chapin(415) 291-5609"
..OCNT9    INIT      "elizabeth corsale"
.OCNT9    INIT      "Phoebe Fearing(415) 291-5667"
..OCNT9    INIT      "Nancy Purcell(415) 291-5638" 5/1/98
.OCNT10   INIT      "Steve Kehrli(415) 291-5640"
..OCNT11   INIT      "james eves"
..OCNT11   INIT      "sonya brown"
..OCNT11   INIT      "julie doherty"            .4/28/94
..OCNT11   INIT      "hillary harper"           .7/19/94
..OCNT11   INIT      "rebecca mumma(415) 291-5640"
..OCNT11   INIT      "Brendan Peterson(415) 291-5603"
.OCNT11   INIT      "Bonnie Olson(415) 291-5647"
..OCNT12   INIT      "marguerite salmon"  cold calls
.OCNT12   INIT      "Susan Anstrand(415) 291-5643"
..OCNT13   INIT      "yolanda montes"
..OCNT13   INIT      "robert santiago"
..OCNT13   INIT      "meg van wagenen" left sales 7/93.
..OCNT13   INIT      "kathleen pavao"
..OCNT13   INIT      "Brenda Villanueva(415) 291-5657" 5/31/98
.OCNT13   INIT      "Becky Chavez(415) 291-5657"     08/04/98
..OCNT14   INIT      "tiffany figoni"
.OCNT14   INIT      "Brian Schulz(415) 291-5642"
..OCNT15   INIT      "kim kouri"
.OCNT15   INIT      "Brian Karo(415) 291-5663"
..OCNT15   INIT      "laurie williams"
.OCNT16   INIT      "Jane Nagatoshi(415) 291-5662"
.OCNT17   INIT      "Greg Kettering(415) 291-5658"               
.Modified Contact: 08 Lisa Chapin to Katinka Partridge 07/31/2000 by Julie Enomoto                                                                                                                      
.Modified Contact: 23 Elizabeth Green to Jennifer Gemmellaro 07/31/2000 by Julie Enomoto                                                                                                                
.Modified Contact: 34 Meg Plaza to Nancy La Shell 08/25/2000 by Julie Enomoto                                                                                                                           
.Modified Contact: 21 Julie Hansen to Spare 10/09/2000 by Jose Duenas                                                                                                                                   
.Modified Contact: 17 Greg Kettering to Julie Hansen 10/09/2000 by Jose Duenas                                                                                                                          
.Modified Contact: 28 Julie Enomoto to David Baca 10/09/2000 by Jose Duenas                                                                                                                             
.Modified Contact: 05 Elki Taba to Spare 10/11/2000 by David Baca                                                                                                                                       
.Modified Contact: 07 Brad Van Unen to Ben Frutos 10/11/2000 by David Baca                                                                                                                              
.Modified Contact: 05 Spare to Tami Frost 10/26/2000 by David Baca                                                                                                                                      
.Modified Contact: 02 Catherine Osuna to Jane Nagatoshi 10/31/2000 by Jose Duenas                                                                                                                       
.Modified Contact: 16 Jane Nagatoshi to Catherine Osuna 10/31/2000 by Jose Duenas                                                                                                                       
.Modified Contact: 21 Spare to Audrey Fairley 11/13/2000 by Jose Duenas                                                                                                                                 
.Modified Contact: 15 Brian Karo to Mac McIntosh 11/15/2000 by David Baca                                                                                                                               
.Modified Contact: 22 Eliza Fox to Kathy Obee 03/09/2001 by David Baca                                                                                                                                  
.Modified Contact: 29 Jennifer Shanks to Diego Montoya 09/04/2001 by Jennifer Shanks                                                                                                                    
.Modified Contact: 06 Gillian Cross to Robin Skillings 10/01/2001 by Jose Duenas                                                                                                                        
.Modified Contact: 17 Julie Hansen to Manna Embaye 10/02/2001 by Jose Duenas                                                                                                                            
.Modified Contact: 03 Jereza Mendoza to Not Currently Used 04/08/2002 by Jose Duenas                                                                                                                    
.Modified Contact: 41 Ann Lovi to Not Currently Used #2 04/08/2002 by Jose Duenas                                                                                                                       
.Modified Contact: 34 Nancy La Shell to Nancy LaShell 04/26/2002 by Andrew Harkins                                                                                                                      
.Modified Contact: 39 Tami Frost to Tami Barocio 04/29/2002 by Jose Duenas                                                                                                                              
.Modified Contact: 31 Laramie Howard to Sarah Tan 08/09/2002 by Diego Montoya                                                                                                                           
.Modified Contact: 07 Ben Frutos to Ann Lovi 09/03/2002 by Andrew Harkins                                                                                                                               
.Modified Contact: 17 Manna Embaye to Janice Rounds 02/11/2003 by Diego Montoya                                                                                                                         
.Modified Contact: 17 Janice Rounds to Diana Zaslaw 05/23/2003 by Diego Montoya                                                                                                                         
.Modified Contact: 30 Mark Hadox to Sandra Inouye 06/11/2003 by Mr Batch                                                                                                                                
.Modified Contact: 30 Mark Hadox to Sandra Inouye 06/11/2003 by Mr Batch                                                                                                                                
.Modified Contact: 48 List Management to List Management Clearances 08/11/2003 by Jose Duenas                                                                                                           
.Modified Contact: 17 Diana Zaslaw to Joey Gamache 08/22/2003 by Diego Montoya                                                                                                                          
.Modified Contact: 13 Becky Chavez to Becky Chavez - Sales 10/22/2003 by Diego Montoya                                                                                                                  
.Modified Contact: 50 Becky Chavez to available 10/23/2003 by Jose Duenas                                                                                                                               
.Modified Contact: 13 Becky Chavez - Sales to Becky Chavez 10/23/2003 by Andrew Harkins                                                                                                                 
.Modified Contact: 50 available to Sue Adams 11/06/2003 by Diego Montoya                                                                                                                                
.Modified Contact: 36 Anne Douglas to Tiffany Godfrey 04/16/2004 by David Baca                                                                                                                          
.Modified Contact: 55 Dana Nusbaum to James Ericson 01/14/2005 by Diego Montoya                                                                                                                         
.Modified Contact: 55 James Ericson to Denisha Delane 01/26/2005 by Diego Montoya                                                                                                                       
.Modified Contact: 58 Sherene Kelley to Sherene Kelly 03/14/2005 by Diego Montoya                                                                                                                       
.Modified Contact: 62 Carol Schalla to Jenell-Marie King 10/31/2005 by David Baca                                                                                                                       
.Modified Contact: 62 Jenell-Marie King to Edward Arkin 11/21/2005 by Diego Montoya                                                                                                                     
.Modified Contact: 65 Ruka AL-Hassan to Ruka ALHassan 03/16/2006 by Diego Montoya                                                                                                                       
.Modified Contact: 82 Nichole Lima to Jamie Mitton 05/30/2007 by Jose Duenas                                                                                                                            
.Modified Contact: 82 Nichole Lima to Elizabeth Ayres 06/05/2007 by Jose Duenas                                                                                                                         
.Modified Contact: 48 List Management Clearances to List Management Clearances NIN 10/11/2007 by David Herrick                                                                                          
.Modified Contact: 72 Bahar Tabatabai to Bahar Foltz 01/07/2008 by David Herrick                                                                                                                        
.Modified Contact: 73 Jennifer Woodford to Jennifer Deerr 01/07/2008 by David Herrick                                                                                                                   
.Modified Contact: 03 Not Currently Used to Front Desk 03/11/2011 by David Herrick                                                                                                                                                                             
.Modified Contact: 23 Jennifer Gemmellaro to Jennifer Cox 04/17/2012 by David Herrick                                                                                                                                                                          
.Modified Contact: 41 Not Currently Used #2 to Krsni Watkins 04/17/2012 by David Herrick                                                                                                                                                                       
.Modified Contact: 43 Telecomm to Pia Payne 06/30/2012 by David Herrick                                                                                                                                                                                        
.Modified Contact: 01 Billing to Cold Calls LM 08/16/2013 by Mr Batch                                                                                                                                                                                          
.Modified Contact: 02 Billing to Amy Frey 05/13/2014 by David Herrick                                                                                                                                                                                          
.Modified Contact: 08 Katinka Partridge to Alban Dufal 10/23/2014 by David Herrick                                                                                                                                                                             
.Modified Contact: 09 Phoebe Fearing to Denise Hubbard 03/13/2015 by David Herrick                                                                                                                                                                             
.Modified Contact: 41 Krsni Watkins to Krsni Tongbra 09/30/2015 by Robb Whiting                                                                                                                                                                                
.Modified Contact: 41 Krsni Watkins to Krsni Tongbra 09/30/2015 by Robb Whiting                                                                                                                                                                                
.Modified Contact: 41 Krsni Watkins to Krsni Tongbra 09/30/2015 by Robb Whiting                                                                                                                                                                                
