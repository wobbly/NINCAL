.pdf995settings
PC        equ       0
          include   common.inc
          include   cons.inc
Release   init      "1.00"          .DLH New
Reldate   Init      "27 February 2012"
DimPtr    Dim       ^
. external calls for changing pdf995.ini file

                    

.set autolaunch off and 
.using "flag.dat" to know when printing is done
PDFAutoSET Routine 
          call       GetPDFPath
          pack      str45 from PDFPATH,"\res\pdf995.ini"

         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Autolaunch":
                   "0":
                  result

PDFFlagSET Routine 
          call       GetPDFPath
          pack      str45 from PDFPATH,"\res\pdf995.ini"
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "ProcessPDF":
                   "\\nins1\e\apps\plb\code\pdftest.bat":
.                   "\\nins1\e\apps\winbatch\Del995flag.exe":
                  result

                              if (result = C0)
.Prepare Flag file
                                        pack      str45 from PDFPATH,"\flag.dat"
                                        prep      tempfile,str45
                                        write     tempfile,SEQ;"flag set"
                                        close     tempfile
                              endif
          return
PDFDefaultsSet Routine

          call       GetPDFPath
          pack      str45 from PDFPATH,"\res\pdf995.ini"
.settings to close pdf viewer after using printpdf
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "PrintPDFAcrobatShutdown":
                   "1":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "PrintPdfAcrobatShutdownAfterNSeconds":
                   "600":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Unicode":
                   "0":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Display Readme":
                   "0":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Output Folder":
                   "C:\work\pdf":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Output File":
                   "SAMEASDOCUMENT":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Autolaunch":
                   "0":
                  result
         call      "GU$INI;WRITE_TO_INI" USING str45:
                   "Parameters":
                   "Quiet":
                   "0":
                  result
          Return






          include   comlogic.inc