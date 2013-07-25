#!./wisp-multiline.sh
; !#

define : timestring
  string-join 
    list
        number->string : tm:hour : gmtime : current-time
        number->string : tm:min : gmtime : current-time
    . ":" ; delimiter

define : greeting
  if : string? : getlogin
    getlogin
    . "Sucker!"

define : hello-world-handler request request-body
  values 
    ; header
    ' : content-type . : text/plain
    ; content
    let : : text "Hello World!"
      string-join
        list 
          . text
          greeting
          timestring
        . " " ; delimiter

; run the webserver
use-modules : web server

display "Server starting. Test it at http://127.0.0.1:8081"
newline

run-server hello-world-handler 'http ' : #:port 8081


