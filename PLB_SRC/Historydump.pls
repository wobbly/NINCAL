...............................................................................
PC       EQU       0
               INC            COMMON.INC
               INC            CONS.INC
               INC            CONSACCT.inc
.
               INCLUDE        ninvdd.inc
               Include        NInvAcdDD.inc
                              include   compdd.inc
                              include   cntdd.inc
               Include        Nsel2dd.inc
               INCLUDE        NBILDD.INC
               INCLUDE        NOWNDD.INC
               INCLUDE        NRTXDD.INC
               INC            NMTXDD.INC
               INCLUDE        NACDDD.inc
               include        nshpdd.INC
               inc            npaydd.INC
         INCLUDE    NORD2DD.inc
.               INCLUDE        NORDDD.INC
               include        nmrgdd.INC
               include        nrtndd.INC
               include        ndatdd.INC
               include        ndat3dd.INC
               include        nofrdd.inc
               INCLUDE        ncntdd.inc
               include        nmlddd.inc
          include   Oslspern.inc
          include   Nadjdd.inc
Release   INit      "test"
reldate   Init      "06 April 09"
FileIN    FIle                
Fileout   FIle
slspern   dim      2
shipsw     Dim       1
mrgsw     dim       1
count     form      7

          OPen      Filein,"diskin50.dat|NINS1:502",exclusive
          Prepare   FilEout,"c:\work\OrderDump.csv",exclusive
          

Loop      Read      Filein,seq;ORD2VARS
          Goto      eoj if over
          add       c1,count
          display   *p10:10,"rec's in ",count," ",olrn
          clear               slspern
          pack                slspern from osales10,osales
          move      slspern,N2
          MOVE                OSLS0 TO str25
          LOAD                str25 FROM n2 OF OSLS1,OSLS2,OSLS3,OSLS4,OSLS5:
                              OSLS6,OSLS7,OSLS8,OSLS9,OSLS10,OSLS11,OSLS12,OSLS13:
                              OSLS14,OSLS15,OSLS16,OSLS17,OSLS18,OSLS19,OSLS20,OSLS21:
                              OSLS22,osls23,osls24,osls25:
                              osls26,osls27,osls28,osls29,osls30,osls31,osls32,osls33,osls34,osls35
          Packkey      Mkey from Omlrnum,OCOBN
          call      Nmlrkey
          packkey      Ndatfld from Olnum
          call      Ndatkey
          pack      Ninvfld from Olrn
          call      Ninvkey
          if        over
          MOve      c0,lrinc
          goto      Write
          endif
                  
         move      c1 to nownpath
         move      olon to nownfld
         call      nownkey
         move      c2 to tdmcflag
         MOVE      olrn to nmrgfld
         REP       ZFILL IN NMRGFLD
         move      c0 to nmrgrqty
         move      c0 to nmrgiqty
         move      c0 to nmrgnet
         move      no to mrgsw
         CALL        NMRGKEY
         if      not over
                       move    yes to mrgsw
         endif   
                  
         move      lrn to nshpfld
         move      no to shipsw

         call      nshpkey
         if        not over
         move      yes to shipsw
         endif
               call           Ninvacdrecclear
               CLEAR          NInvAcdfld
               pack           NInvAcdFld from Invnum
               call           NInvAcdRecClear
               call           NinvAcdTst
               Call           NInvAcdRecLoad
               

         CALL      COMPUTE
          MOVE      LRN TO NADJFLD
          rep       zfill in nadjfld
          CALL      NADJKEY
          if        not over
          add       ASLRINC,lrinc
          endif
          
Write     Write     Fileout,seq;*cdfon,ORD2VARS,str25,Mcomp,Olstname,Lrinc
          Goto      Loop



Eoj       Weof      Fileout,seq
          Close     FIleout
          stop








           INCLUDE   NMTXIO.INC
           INCLUDE   NRTXIO.INC
           INCLUDE   NBILIO.INC
           INCLUDE   NOWNIO.INC
         INCLUDE    NORD2io.inc
.           INCLUDE   NORDIO.INC
           INCLUDE   NACDIO.INC
         include     nofrio.inc
         include   npayio.INC
               include        compute.inc
               INCLUDE        ninvio.inc
               Include        NInvAcdIO.inc
                              include   compio.inc
                              include   cntio.inc
               Include        Nsel2io.inc
         include   nmrgio.INC
         include   nrtnio.INC
         include   ndatio.INC
         include   nshpio.INC
         include   ndat3io.inc
         include   ncntio.inc
         include   nmldio.inc
         include   nadjio.inc

           INC     COMLOGIC.INC
