.// include files
	Include Common.inc
	Include Cons.inc
	  

NFTPCOMP      DIM   6                     .Eventually Fulfillment Company Number     .1-6
NFTPCOMPID              DIM   3           .Counter for mulitple sites                .7-9
NFTPDESC      DIM   100                   .Company Description                       .10-109
NFTPPROTOCOL        DIM 50                .FTP/FTPS                                  .110-159
NFTPADDRESS         DIM 255               .Ftp Address                               .160-414
NFTPUSERNAME        DIM 255               .User Name                                 .415-669
NFTPPASSWORD        DIM 255               .Password                                  .670-924
ErrorReturn	    DIM 490

MySite     automation      class="CuteFTPPro.TEConnection"
          create  MySite

Main
	Pack NFTPPROTOCOL,"FTP"
	Pack NFTPADDRESS,"ftp.mmidirect.com"
	Pack NFTPUSERNAME,"nincal"
	Pack NFTPPASSWORD,"b7h4tt"
	
	SQUEEZE   NFTPCOMP,NFTPCOMP
	SQUEEZE   NFTPCOMPID,NFTPCOMPID
	SQUEEZE   NFTPPROTOCOL,NFTPPROTOCOL
	SQUEEZE   NFTPADDRESS,NFTPADDRESS
	SQUEEZE   NFTPUSERNAME,NFTPUSERNAME
	SQUEEZE   NFTPPASSWORD,NFTPPASSWORD
	
	Setprop
	
	Trap ConnectFailure giving error if object
	MySite.Connect giving N9
	Display *P10:6,N9
	MySite.Disconnect giving N9
	Display *P10:6,N9
	Pause "5"
	STOP
	
ConnectFailure
	GETINFO   EXCEPTION,ErrorReturn
	Display *P10:10, ErrorReturn 
	Display *P10:112,Error
	return