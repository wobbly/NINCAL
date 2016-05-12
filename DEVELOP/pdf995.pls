FlagReset995Launch Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
FlagReset995Email  Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
FlagReset995SMTP   Dim            1                             ;'Y' means we played with pdf99.ini and need to restore
SmtpDef         INIT "N"
EmailDef      INIT "N" 

SMTP995.ini
			Open	TestFile,"c:\progra~1\\pdf995\res\pdf995.ini"
			Prepare	TempFile,"c:\progra~1\\pdf995\res\pdf995.out"
			write	tempfile,Seq;"[Parameters]                       "
			write	tempfile,Seq;"Display Readme=0                   "
			write	tempfile,Seq;"Output Folder=C:\work\pdf          "
			write	tempfile,Seq;"Output File=SAMEASDOCUMENT "
			write	tempfile,Seq;"Quiet=0"
			write	tempfile,Seq;"Use GPL Ghostcript=1"
			write	tempfile,Seq;"Use GPL Ghostscript=1"
			write	tempfile,Seq;"Use AFPL Ghostscript=1"
			write	tempfile,Seq;"Document Name=Document"
			write	tempfile,Seq;"User File=C:\work\pdf\Document.pdf"
			write	tempfile,Seq;"Launch=C:\work\pdf\Document.pdf"
			write	tempfile,Seq;""
			write	tempfile,Seq;"Stationery File=Stationery"
			write	tempfile,Seq;"Autolaunch=0"
			write	tempfile,Seq;"Email=1"
			write	tempfile,Seq;"SMTP=1"
			write	tempfile,Seq;"Email Subject=Your PDF file"
			write	tempfile,Seq;"Email Recipient Address=dbaca@nincal.com"
			write	tempfile,Seq;"Email Recipient Name=David Baca"
			write	tempfile,Seq;"Email Server=NTS4"
			write	tempfile,Seq;"Email From Address=dmontoy@nincal.com"
			write	tempfile,Seq;"SMTP Return Code=0"
			write	tempfile,Seq;"[OmniFormat]"
			write	tempfile,Seq;"Accept EULA=1 "
			weof	tempfile,seq
			close	tempfile
			close	testfile
			Erase	"c:\progra~1\\pdf995\res\pdf995.Sav"
			Rename	"c:\progra~1\\pdf995\res\pdf995.ini","c:\progra~1\\pdf995\res\pdf995.Sav"
			Rename	"c:\progra~1\\pdf995\res\pdf995.out","c:\progra~1\\pdf995\res\pdf995.ini"
			return


RestorePDF995Ini
			move	"PDF995 Restore",Filename
			pack	KeyLocation,"Erase temp ini FIle "
			Erase	"c:\progra~1\\pdf995\res\pdf995.ini"
			move	"PDF995 Restore",Filename
			pack	KeyLocation,"Ren ini  to Save "
			Rename	"c:\progra~1\\pdf995\res\pdf995.Sav","c:\progra~1\\pdf995\res\pdf995.ini"

