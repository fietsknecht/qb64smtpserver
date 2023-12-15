' #######################################
'
' SMTP server 14-12-2023  Ubuntu version
' -- stnest.net --
'
'########################################

'no window output, console only
$Console:Only

DefInt A-Z
crlf$ = Chr$(13) + Chr$(10)
Open "smtp.log" For Append As #9
Print #9, "* "; Date$; " "; Time$; " Server started"
Close #9

host = _OpenHost("TCP/IP:587")

Print "* Host open"
Do
    Print "* Listening on port 587..."

    Do
        client = _OpenConnection(host)
        If InKey$ = "q" GoTo closeall
    Loop Until client

    If _Connected(client) Then
        Open "smtp.log" For Append As #9
        ip$ = _ConnectionAddress(client)
        Print "* Connected to " + ip$ + ". Waiting for requests..."
        Print #9, "* "; Date$; " "; Time$; " Connected to " + ip$
        starttime! = Timer
        timeout = 20
        user$ = ""
        pw$ = ""
        readytosend = 0
        authorized = 0
        auth = 0
        intruder = 0
        answerclient ("220 smtp.stnest.net SMTP ready")

        Do
            getdata
            If InStr(stream$, "HELO") Or InStr(stream$, "EHLO") Then
                answerclient ("250-stnest.net")
                answerclient ("250 AUTH LOGIN")
                stream$ = ""
            End If
            If InStr(stream$, "AUTH LOGIN") Then
                'stream$ = ""
                auth = 1
                answerclient ("334 VXNlcm5hbWU6") ' thats BASE64 for 'username'
                getdata
                user$ = stream$
            End If
            If auth = 1 And user$ <> "" Then
                answerclient ("334 UGFzc3dvcmQ6") ' that's BASE64 for 'password'
                getdata
                pw$ = stream$
                If user$ <> "" And pw$ <> "" Then
                    Call authorization(user$, pw$)
                    If authorized = 1 Then
                        answerclient ("235 Authorized")
                        getdata
                        mailfrom$ = stream$
                        answerclient ("250 OK")
                        getdata
                        rcptto$ = stream$
                        answerclient ("250 OK")
                        getdata
                        dat$ = stream$
                        If InStr(dat$, "DATA") Then
                            answerclient ("354 Go on then")
                            getdata
                            msg$ = stream$
                            If InStr(msg$, crlf$ + "." + crlf$) Or InStr(msg$, Chr$(10) + "." + Chr$(10)) Then
                                'check content - no HTML or attachments
                                If InStr(msg$, "<HTML>") Or InStr(msg$, "multiple part message") Or Len(msg$) > 32000 Then
                                    answerclient ("221 Content rejected")
                                    Exit Do
                                End If
                                answerclient ("250 OK")
                                readytosend = 1
                                Print "* Mail received"
                                getdata
                                q$ = stream$
                                If InStr(q$, "QUIT") Then
                                    answerclient ("221 Closing")
                                Else
                                    answerclient ("221 Forced quit (no QUIT reply received)")
                                End If
                            End If
                        End If
                    End If
                End If
            End If
            If intruder = 1 Or readytosend = 1 Then Exit Do
            If InKey$ = "q" Then GoTo closeall
        Loop Until Timer > starttime! + timeout
        Close #client
        Print "* Connection closed"
        Print #9, "* "; Date$; " "; Time$; " Connection closed"
        Close #9
    Else
        Print "* Connection lost"
        Print #9, "* "; Date$; " "; Time$; " Connection lost"
        Close #9
    End If


    If readytosend = 1 Then
        Open "mail.txt" For Output As #2
        Print #2, msg$
        Close #2
        Shell _Hide "dos2unix mail.txt"
        ' send the reveived mail to the stnest.net server that does the actual sending
        Shell _Hide "curl -T mail.txt ftp://<user>:<password>@stnest.net/outbox/"
        Open "index.txt" For Input As #1
        Line Input #1, num$
        Close #1
        num = Val(num$)
        num = num + 1
        num$ = LTrim$(Str$(num))
        Open "index.txt" For Output As #2
        Print #2, num$
        Close #2
        sentmail$ = "mail" + num$ + ".txt"
        Name "mail.txt" As sentmail$
        Kill "user.b64"
        Kill "pw.b64"
        Kill "user.txt"
        Kill "pw.txt"
    End If

    If InKey$ = "q" Then GoTo closeall
Loop

closeall:
Print "Closing..."
Close

Print "=END="
Print "Press any key..."
Sleep
System


Sub getdata
    Shared starttime!, timeout, client, stream$
    Do
        _Delay 0.5
        Get #client, , stream$
    Loop Until stream$ <> "" Or Timer > starttime! + timeout
    Print #9, "C: "; stream$
End Sub


Sub answerclient (ans$)
    Shared crlf$, client
    ans$ = ans$ + crlf$: Put #client, , ans$
    Print #9, "S: "; ans$
End Sub


Sub authorization (user$, pw$)
    Shared authorized, intruder
    If _FileExists("user.txt") Then Kill "user.txt"
    If _FileExists("pw.txt") Then Kill "pw.txt"
    Open "user.b64" For Output As #1
    Print #1, user$
    Close #1
    Open "pw.b64" For Output As #1
    Print #1, pw$
    Close #1
    Shell _Hide "dos2unix *.b64"
    Shell _Hide "base64 --decode user.b64 > user.txt"
    Shell _Hide "base64 --decode pw.b64 > pw.txt"
    Open "user.txt" For Input As #1
    Line Input #1, u$
    Close #1
    Open "pw.txt" For Input As #1
    Line Input #1, p$
    Close #1
    login$ = u$ + ":" + p$
    Open "auth_plain.txt" For Input As #1
    While Not EOF(1)
        Line Input #1, reguser$
        If reguser$ = login$ Then
            authorized = 1
            Print "* "; login$; " authorized"
            Print #9, "* "; login$; " authorized"
            Exit While
        Else
            Print #9, "* intruder detected"
            intruder = 1
        End If
    Wend
    Close #1
End Sub

