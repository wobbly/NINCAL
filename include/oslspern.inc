.;    Last change:  JD   11 May 2007    Added Agnes Alvarez
.;    Last change:  JD   23 Apr 2007    Added Stephen King
.;    Last change:  JD   23 Apr 2007    Added Stephen King
.;    Last change:  JD   08 Mar 2007    Added Gayle,David Bryant,Thay
.;    Last change:  JD   26 Feb 2007    Added Karla,Pam,Demaris PL
.;    Last change:  JD   29 Dec 2006    Added Rick Fox  to #12
.;    Last change:  JD   05 Jul 2006    Added Inga Beck to #15.
.;    Last change:  JD   14 Mar 2006    Added TF to #17.
.;    Last change:  DM   07 Nov 2005    Changed BC's old #13 to AK
.;    Last change:  JD   06 Sep 2005    Added CB # 15..
.;    Last change:  JD   04 Apr 2005    BLO,SK,JC,BS turned off.
.;    Last change:  JD   31 Jan 2002    8:10 am
.;    14Apr04  added sk to sls19 listmgmt.
.;    15Mar05  added DC to sls12 brokerage.
*..............................................................................
* OSLSPERN/INC -   ORDER SALES PERSON TABLE.     21OCT87
* variables should be padded out to 25 to avoid pcbus problem
* Any changes here also need to be made in Ncnt0001
*..............................................................................
OSLS0    INIT      "no salesperson            "
OSLS1    INIT      "Amy Frey                   "
.OSLS1    INIT      "Sue Adams                 " started 11/17/03
.OSLS1    INIT      "Jereza M.                 " left 11/2001
.OSLS2    INIT      "Bonnie                    " Left 11/2003
OSLS2    INIT      "Cold Calls - LM            "
OSLS3    INIT      "Susan Anstrand             "
.OSLS4    INIT      "Ann Lovi                   "
OSLS4    INIT      "Alban Dufal                 "     .2015 May 20
.OSLS5    INIT      "Jennifer Woodford          "    .start 02/07
OSLS5    INIT      "Denise Hubbard             "       .start 2015
.OSLS5    INIT      "Phoebe Fearing             "       .left 12/06
OSLS6    INIT      "List Management            "       .7/90
.OSLS7    INIT      "Steve Kehrli              " Left 12/2004
OSLS7    INIT      "Krsni Tongbra             "   .added 10/07/15
.OSLS8    INIT      "Jeanette                  " Left 12/2004
OSLS8    INIT      "Reuben Holland             "   .added 25June2010
OSLS9    INIT      "Cold calls                "
OSLS10   INIT      "Mac McIntosh              "
.OSLS10   INIT      "Brendan Peterson          " left 4/99.
.OSLS11   INIT      "Brian Schulz              " Left 12/2004
OSLS11   INIT      "Agnes Alvarez              "  left 9/25/2015
.OSLS11   INIT      "                          "
OSLS12   INIT      "Melody McCoy               " started 2/07
.OSLS12   INIT      "Rick Fox             "
.OSLS12   INIT      "Delynne Cullen            " started 03/05
.OSLS12   INIT      "Brian Karo                "
OSLS13    INIT      "Jennifer Deerr            "    .Married 10/07
.OSLS13    INIT      "Jennifer Woodford          "    .start 02/07
.OSLS13   INIT      "Andrea Kalabokes           "
.OSLS13   INIT      "Becky Chavez               "
.OSLS13   INIT      "                           "
OSLS14   INIT      "Suzie McGuire              "
OSLS15   INIT      "Inga Beck                  "   started 05/06
.OSLS15   INIT      "Courtney Brown            "   started 09/05
.oSLS15   INIT      "Gillian Cavana            "         *fixed by dh 1/10
OSLS16   INIT      "Kattt Sammon              "
OSLS17   INIT      "Tiffany Godfrey           " started 03/14/06
OSLS18   INIT      "Chris Pottle              " started 03/06
.OSLS19   INIT      "Steve Kehrli              "  ***************OSLS19 reserved for list management only!!!!!!!!!!!!!!!!!!!!!!
OSLS19   INIT      "Katinka Partridge         "  ***************OSLS19 reserved for list management only!!!!!!!!!!!!!!!!!!!!!!
OSLS20   INIT      "Karla Echenique           "
OSLS21   INIT      "Pam Grossman              "
.OSLS22   INIT      "Mary Gregory              " left 8/99
OSLS22   INIT      "Demaris Hammond            "
.OSLS23   INIT      "Gayle Trenam               "
OSLS23   INIT      "Katherine Quamina          " .July 15 2015
OSLS24   INIT      "David Bryant               "
OSLS25   INIT      "Thay Sisouvanh             "
OSLS26   INIT      "Stephen King               "
.OSLS27   INIT      "Sarah Lewin                "    2/1/08
OSLS27   INIT      "List Management-NIN/PLI    "
OSLS28   INIT      "Carolyn Fremgen            "
OSLS29   INIT      "Shirley Schoevaars         "
.OSLS30   INIT      "Pia Payne                  "                        .reserved for LM
OSLS30   INIT      "Denise Hubbard(LM)         "       .2015 Mar 13        .reserved for LM
OSLS31   INIT      "Jack Forder                "      .added 12/17/08
OSLS32   INIT      "Jennifer Cox               "                     .added 04/25/2012
OSLS33   INIT      " "
OSLS34   INIT      " "
OSLS35   INIT      "Imported PL Brokerage      "
.OSLS1    INIT      "ardis "
.OSLS1    INIT      "                "
.OSLS1    INIT      "lisa" left in 1/91.
.OSLS4    INIT      "Elaine                   "
.OSLS5    INIT      "Nancy                     "    started april 90
.OSLS5    INIT      "kevin                   "    02/90 left niN
.OSLS6    INIT      "ines                    "    not in list mAN.
.OSLS7    INIT      "patrick                  "
.OSLS7    INIT      "mike f."   04/90 left nin
.OSLS7    INIT      "kim kouri" 11/91 left  nin
.OSLS7    INIT      "meg " 07/93 left sales.
.OSLS7    INIT      "julie                      "    started sales person.9/93.
.OSLS7    INIT      "Kathi Mirza              "    started sales person.1/94.
.OSLS9    INIT      "cheryl"
.OSLS9    INIT      "jennifer b." 02/90 left nin
.OSLS9    INIT      "tina maultsby"    left 6/90???
.OSLS9    INIT      "marguerite salmon"      as of 9/13/90, lefT 11/21/90
.OSLS9    INIT      "ines"                    left 3/15/91
.OSLS10   INIT      "glendi"    left 7/90
.OSLS11   INIT      "Josette Peterson         "
.OSLS11   INIT      "jennifer p.              " 1/93 left nin
.OSLS12   INIT      "mike l."        07/89 left nin
.OSLS12   INIT      "james"     09/90    left 3/92
.OSLS12   INIT      "Julie Doherty            "
.OSLS12   INIT      "Carol Diaz            " left 10/98
.OSLS13   INIT      "regina"
.OSLS13   INIT      "jone"      02/90 left nin
.OSLS13   INIT      "michael garland"     moved to ssg 7/90
.OSLS13   INIT      "Tiffany Figoni           "
.OSLS15   INIT      "julie m."    04/90 left nin
.OSLS15   INIT      "Cheryl Piry              " left 12/98         *fixed by dh 1/10
.                                                      *dont change it!
.OSLS16   INIT      "larry"     10/89 left nin
.OSLS16   INIT      "rebecca                  "
.OSLS17   INIT      "elizabeth"  04/90 left nin
.OSLS17   INIT      "Monte Fields             "
.OSLS18   INIT      "yolanda                  "   08/89 left niN
.OSLS18   INIT      "beth" 12/91 letf nin
.***************OSLS19 reserved for list management only!!!!!!!!!!!!!!!!!!!!!!
.OSLS19   INIT      "michael d."  04/91 left nin
.............................................................................
.OSLS20   INIT      "linda mietus"     moved to ssg 7/90
.OSLS21   INIT      "laurie" 7/91 letf nin
.OSLS22   INIT      "sonya brown"    left 3/92
.OSLS23   INIT      " "
.OSLS24   INIT      " "
.OSLS25   INIT      " "
.OSLS26   INIT      " "
.OSLS27   INIT      " "
.OSLS28   INIT      " "
.OSLS29   INIT      " "
...............................................................................
.ANY CHANGES IN THIS table need to will change table in salesprT. AFTER
.PARAGRAPH READ2.
...............................................................................

