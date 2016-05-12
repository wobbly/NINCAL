Ftest  IFILE     keylen=9,fix=304,uncomp,Name="CONTACTS.ISI|NINS1:502"

data dim 50

start
// insert this code under the start label
newline init 0x0d,0x0a
    stream *stdout,"HTTP/1.0 200 OK",newline:
           "Content-type: text/html",newline,newline

          exceptset openError if io
        open fTest,"CONTACTS.ISI|NINS1:502"
        readks fTest;data
    exceptclear io
    stream *stdout,"File open/read worked.  Data read: "
    stream *stdout,data
    stop

openError
    stream *stdout,"File open/read error: "
    stream *stdout,s$error$
    stop
