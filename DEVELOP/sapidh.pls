//works

result        form            9
Sapi          Automation      //Define the ActiveX control
Voices        automation      //Define a Voice
Voice1        automation      //Define a Voice
str500        dim 500
//Create the ActiveX control
              Create          Sapi,class="Sapi.SpVoice"

//Use the Speak Method of this control
              //getprop Sapi,*Voice=Voices
              //setprop       Voices,*Name="Microsoft mary", *Language="409"
              Sapi.GetVoices giving Voices   //using "Name=Microsoft mary", "Language=409"
              Move            "1" to result
              Sapi.GetVoices giving Voices Using Result  //using "Name=Microsoft mary", "Language=409"
              getprop Voices,*count=result
;              Voices.item giving Voice1 using result
              setprop Sapi.Voice,*Voices

              Sapi.Speak using "Good Bye David"
              shutdown

// so know I want to change the voice used





.' Declaring variable to hold SAPI object.
.Dim voic
.
.' Creating SAPI object using spvoice.
.Set voic = Server.CreateObject("SAPI.SpVoice")
.
.' If you want to change the voice then uncomment any of the following 3 lines.
.
.'Set voic.voice = voic.GetVoices("Name=Microsoft mary", "Language=409").Item(0)
.'Set voic.voice = voic.GetVoices("Name=Microsoft mike", "Language=409").Item(0)
.'Set voic.voice = voic.GetVoices("Name=Microsoft sam", "Language=409").Item(0)
.
.' you can set other parameters and properties like voice pitch, speed etc.
.
.' Using speak function of SpVoice to speak a string.
.voic.Speak("Welcome To My First Speech Enabled ASP Page, Have a nice day!")
.
.' Destroying SAPI.spvoice object.
.Set voic = nothing
.
