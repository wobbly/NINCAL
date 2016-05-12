.******************************************************
.*  SRDS List FILE DESCRIPTORS.
.. ....................................................
.* ****************************************************
.* SRDS DATACARD FILES.
.* ****************************************************
.
.      FILE:      
.    LENGTH:      Variable
.  COMPRESS:      NONE
.      TYPE:      XML from SRDS if there is not a hit
.                 on our system they get put into an indexed/aimdexed file for review
......................................................
. LAST MODIFIED
. patch 1.0 13 April  2011 DLH New
......................................................
.
SRDSLOFlist  Filelist
SrdsLOfIle   IFile     fixed=500,NoDup,Name="SRDS_Own.isi|NINS1:502"
SrdsLOfile2   AFile     fixed=500,Name="SRDS_Own.aam|NINS1:502"
          FilelistEnd

SRDSLONAME Init     "SRDS_Own|NINS1:502"
SRDSLOFLAG   FORM      1
SRDSLOPATH   FORM      1
SRDSLOLock   FORM      1
SRDSLOFLd    Dim       6
SRDSLOfld2   Dim       58                  .aim key
.owner / manager file
SRDSLOVARS         LIST                   
SRDSONUM            DIM       6       1-6     .SRDS owner  NUMBER. **KEY**
SRDSCOMP            DIM       55      7-61    .*******Required*******
SRDSADDR            DIM       35     62-96
SRDSADDR2           DIM       35     97-131
SRDSCITY            DIM       30    132-161   .CITY.
SRDSSTATE           DIM       2     162-163   .STATE..MAY NEED TO INCREASE  FOR USE IN FOREIGN COUNTRY STATES
SRDSZIP             DIM       10    164-173   .ZIP..MAY NEED TO INCREASE FOR USE IN FOREIGN COUNTRY
SRDSCNTRY           DIM       15    174-188    .MAY NEED TO INCREASE FOR USE IN FOREIGN COUNTRY
.MANY OF THE NEWER POST SOVIET COUNTRIES ARE IN THE 99* AREA, SO GO TO 4 BYTES
SRDSCNTRYCDE        DIM       4     189-192   .country code for phone/fax  default = 1
.MAY NEED TO INCREASE FOLLOWING 3 FIELDS FOR USE IN FOREIGN COUNTRY
.Do other countries have longer phone number lengths?
SRDSLOPHONE          RECORD           193-202   .Phone Number
SRDSLOPHONE1         DIM        3      
SRDSLOPHONE2         DIM        3      
SRDSLOPHONE3         DIM        4      
                     REcordEnd
SRDSLOFAX             REcord          203-212   .Fax Number  **********Required*************
SRDSLOfax1           DIM        3      
SRDSLOfax2           DIM        3
SRDSLOfax3           DIM        4  
                    REcordend
SRDSLOACCTFAX         DIM       10    213-222   .Accounting fax Number
SRDSLOEMAIL           DIM       50    223-272   .Corporate Email Address (Generic)
SRDSLOFTP             DIM       50    273-322   .SRDSanies FTP Site
SRDSLOType            Dim       2     323-324       
SRDSOFIll             Dim       176   325-500    
          Listend
