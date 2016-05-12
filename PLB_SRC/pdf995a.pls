.pdf995settings
PC        equ       0
          include   common.inc
          include   cons.inc
Release   init      "1.00"          .DLH New
Reldate   Init      "27 February 2012"
.Use external calls

.testing
          call      pdf995auto
          call      SetPDFDefaults
          Stop
                    







          include   comlogic.inc