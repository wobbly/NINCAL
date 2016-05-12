.		whatpass.plc
.		RVW 09/09/2014
.		A simple program to find out what was passed via command line when sending to a web based CGI call.

start

	stream *stdout,"I was given a string of ", S$CMDLIN
	
	
end