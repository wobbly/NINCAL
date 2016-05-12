PC        EQU                 0
          INCLUDE             COMMON.inc
          INCLUDE             CONS.inc
          INCLUDE             NDATDD.inc
          include   NQRCdd.inc

Release   Init      "1.10"   DLH     double check list status
REldate   INit      "2013 June 6"
.Release  Init      "1.00"   DLH     New - Create Quick reco pages for the website
.REldate  INit      "20 June 2008"
.This Job reads the NinQRC & NinQRC files and creates Data1XXXX files  xxx being the reco category number
Output    IFILE     KEYLEN=6,FIXED=600
          Erase     "\\nins1\e\data\data10001.dat"
          Erase     "\\nins1\e\data\data10002.dat"
          Erase     "\\nins1\e\data\data10003.dat"
          Erase     "\\nins1\e\data\data10004.dat"
          Erase     "\\nins1\e\data\data10005.dat"
          Erase     "\\nins1\e\data\data10006.dat"
          Erase     "\\nins1\e\data\data10007.dat"
          Erase     "\\nins1\e\data\data10008.dat"
          Erase     "\\nins1\e\data\data10009.dat"
          Erase     "\\nins1\e\data\data10010.dat"
          Erase     "\\nins1\e\data\data10011.dat"
          Erase     "\\nins1\e\data\data10012.dat"
          Erase     "\\nins1\e\data\data10013.dat"
          Erase     "\\nins1\e\data\data10014.dat"
          Erase     "\\nins1\e\data\data10015.dat"
          Erase     "\\nins1\e\data\data10016.dat"
.main
Main
          call      NQRCDseq
          GOto      EOJ if over
          pack      taskname from "\\nins1\e\data\Data1",NQRCDNum,".dat"
          pack      str55 from "\\nins1\e\data\Data1",NQRCDNum,".isi"
          rep       zfill in taskname
          prepare   Output,Taskname,str55,"6","600",exclusive

                    Loop
                    call      NQRcSeq
                    until     OVer
                    if        (nQRCdnum = NQRCnum)          .hit
                    pack      Ndatfld from NqrcList
                    call      Ndatkey
                    Move                mlstname to str55
                    rep                 uplow in str55
                    scan                "office use" in str55
                    goto                Skip if equal

                    scan                "Office Use" in mlstname
                    goto                Skip if equal
                    scan                "OFFICE USE" in mlstname
                    goto                Skip if equal
                    scan                "Office use" in mlstname
                    goto                Skip if equal
                    goto      Skip if (NDATOFF = "1")
                    goto      Skip if (NDATWEB = "1")
                    Goto      Skip IF (Elstcde <> "C" & ElstCde <> "P")
                    goto      Skip if (STatus <> " ")
                    read      Output,Ndatfld;;               .do not write dupes
                    if        over
                    write     output,Ndatfld;datvars
                    endif
Skip
                    endif
                    repeat
.                   weof      output,seq
                    close     output
                    Close     NqrcFlist
                    move      c0,Nqrcflag
          Goto      Main      
EOJ       
          shutdown  
          

          include   ndatio.inc
          include   Nqrcio.inc
          INclude   Comlogic.inc
