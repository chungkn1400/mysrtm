'
#Include Once "windows.bi"

ScreenRes 400,400,32,1

Dim Shared As Integer i,j,k,file,fileout,auxvar,auxvar2,auxvar3,auxvar4,auxvar5
Dim Shared As Short ficin(100000),ficin2(100000)
Dim Shared As Integer x,xx
Dim Shared As String fic1,fic2
Dim Shared As Integer dx,dy,size,sizex,sizey,iiii,jjjj,iiii1,jjjj1
fic1="D:/srtm/w020n90.dem" 
fic2="D:/srtm/°Europe.bi2"
dx=1080:dy=2400:size=3600
fic1="D:/srtm/e100n40.dem" 
fic2="D:/srtm/°Asia.bi2"
dx=0:dy=0:size=4000
fic1="D:/srtm/e020n40.dem" 
fic2="D:/srtm/°Mideast.bi2"
dx=0:dy=0:size=4000
fic1="D:/srtm/w100n40.dem" 
fic2="D:/srtm/°EastUSA.bi2"
dx=0:dy=0:size=4000
fic1="D:/srtm/w140n40.dem" 
fic2="D:/srtm/°WestUSA.bi2"
dx=1600:dy=0:size=3200
fic1="D:/srtm/e060n40.dem" 
fic2="D:/srtm/°India.bi2"
dx=0:dy=0:size=4800
fic1="D:/srtm/w020n40.dem" 
fic2="D:/srtm/°Africa.bi2"
dx=0:dy=0:size=4800
fic1="D:/srtm/w060s10.dem" 
fic2="D:/srtm/°Brazil.bi2"
dx=0:dy=0:size=3600
fic1="D:/srtm/e020s10.dem" 
fic2="D:/srtm/°SouthAfrica.bi2"
dx=0:dy=0:size=3760
fic1="D:/srtm/e140s10.dem" 
fic2="D:/srtm/°Australia.bi2"
dx=0:dy=0:size=4640
fic1="D:/srtm/w100s10.dem" 
fic2="D:/srtm/°SouthAmerica.bi2"
dx=800:dy=1520:size=4000
fic1="D:/srtm/e100s10.dem" 
fic2="D:/srtm/°WAustralia.bi2"
dx=1360:dy=0:size=3440
'/ 6000x4800 km * 2(ushort) = 56Mo  w180/e140/n90/n40/s10 lng40xlat50 (9x3)
fic1="D:/srtm/w020n90.dem" 
fic2="D:/srtm/°Europe.test"
fic1="D:/srtm/w020n40.dem" 
fic2="D:/srtm/°Africa.bi2"
fic1="D:/srtm/e100n40.dem" 
fic2="D:/srtm/°Asia.bi2"
fic1="D:/srtm/e020n40.dem" 
fic2="D:/srtm/°Mideast.bi2"
fic1="D:/srtm/w100n40.dem" 
fic2="D:/srtm/°EastUSA.bi2"
fic1="D:/srtm/w140n40.dem" 
fic2="D:/srtm/°WestUSA.bi2"
fic1="D:/srtm/e060n40.dem" 
fic2="D:/srtm/°India.bi2"
fic1="D:/srtm/w060s10.dem" 
fic2="D:/srtm/°Brazil.bi2"
fic1="D:/srtm/e020s10.dem" 
fic2="D:/srtm/°SouthAfrica.bi2"
fic1="D:/srtm/e140s10.dem" 
fic2="D:/srtm/°Australia.bi2"
fic1="D:/srtm/w100s10.dem" 
fic2="D:/srtm/°SouthAmerica.bi2"
fic1="D:/srtm/e100s10.dem" 
fic2="D:/srtm/°WAustralia.bi2"
fic1="D:/srtm/e100n90.dem" 
fic1="D:/srtm/w060n40.dem" 
fic1="D:/srtm/w140n90.dem" 
fic1="D:/srtm/e020n90.dem" 
fic1="D:/srtm/w100n90.dem" 
fic1="D:/srtm/e140n40.dem" 
fic1="D:/srtm/w020s10.dem" 
fic1="D:/srtm/e060n90.dem" 
fic1="D:/srtm/w180n90.dem" 
fic1="D:/srtm/w060n90.dem" 
fic1="D:/srtm/e140n90.dem" 
dx=0:dy=4800*3/5:size=4800/5
Sub test1()
file=FreeFile  
Open fic1 For Binary Access Read As #file
?Str(Lof(file))
Get #file,10,ficin(0),100
?Str(ficin(0))
fileout=FreeFile
Open fic2 For Binary Access write As #fileout

For i=1 To size 
 Get #file,dx*2+9600*(i-1+dy),ficin(0),size	
 For j=0 To size-1
 	x=xx
 	xx=ficin(j)
 	If ficin(j)<14 Then auxvar+=1':ficin(j)=0
 	If Abs(xx-x)>1280 Then auxvar2+=1
 	'If Abs(xx-x)>127 Then auxvar2+=1
 	If xx>auxvar3 Then auxvar3=xx
 Next
 'Put #fileout,,ficin(0),size
Next
End Sub 
Dim Shared As Integer xxx,dxxx,dxx,j0,x0,dxx0,xx1,dxxx0,ii,jj,dxmax,i1,j1,i2,j2
Dim Shared As Single ix,iy
Sub test2()
file=FreeFile  
Open fic1 For Binary Access Read As #file
?Str(Lof(file))
Get #file,10,ficin(0),100
?Str(ficin(0))
fileout=FreeFile
Open fic2 For Binary Access write As #fileout
size=4800\5
For ii=0 To 4
For jj=0 To 4 

dx=ii*size
dy=jj*size
auxvar3=0

For i=1 To size-1 Step 2 
 Get #file,dx*2+9600*(i-1+dy),ficin(0),size
 Get #file,dx*2+9600*(i-1+dy+1),ficin2(0),size
 j0=-1:x0=0:dxx0=0	
 For j=0 To size-2 Step 2
 	x=xx
 	xx=(ficin(j)+ficin(j+1)+ficin2(j)+ficin2(j+1))*0.25
 	If xx<14 Then auxvar+=1':xx=14':ficin(j)=0
 	xx1=x0+(j-j0)*dxx0*2
 	If Abs(xx1-xx)>2 Or (j-j0)>=15 Then
 		dxx=(xx-x)/2
 		dxxx0=(xx-x0)/((j-j0)*2)
 		If dxxx0>120 Then dxxx0=120:auxvar2+=1
 		If dxxx0<-120 Then dxxx0=-120:auxvar2+=1
 		x0=x0+dxxx0*(j-j0)*2
 		j0=j
 		dxx0=dxxx0+(dxx-dxxx0)*0.5
 		If dxx0>120 Then dxx0=120':auxvar2+=1
 		If dxx0<-120 Then dxx0=-120':auxvar2+=1
 		auxvar3+=1
 	EndIf
 	'If Abs(xx-x)>1280 Then auxvar2+=1
 	'If Abs(xx-x)>127 Then auxvar2+=1
 	'If xx>auxvar3 Then auxvar3=xx
 Next
 'Put #fileout,,ficin(0),size
Next

'If auxvar3>auxvar4 Then auxvar4=auxvar3
auxvar4+=auxvar3

Next jj
Next ii
End Sub 
Sub test3()
file=FreeFile  
Open fic1 For Binary Access Read As #file
?Str(Lof(file))
Get #file,10,ficin(0),100
?Str(ficin(0))
fileout=FreeFile
Open fic2 For Binary Access write As #fileout
size=4800\5
For ii=0 To 4
For jj=0 To 4 

dx=ii*size
dy=jj*size
auxvar3=0

For i=1 To size-1 Step 2 
 Get #file,dx*2+9600*(i-1+dy),ficin(0),size
 Get #file,dx*2+9600*(i-1+dy+1),ficin2(0),size
 j0=-1:x0=0:dxx0=0	
 For j=0 To size-2 Step 2
 	x=xx
 	xx=Int((ficin(j)+ficin(j+1)+ficin2(j)+ficin2(j+1))*0.25+0.1)
 	If xx<14 Then auxvar+=1':xx=14':ficin(j)=0
   dxmax=30*60
 	If (xx-x)>dxmax Then xx=x+dxmax:auxvar2+=1
 	If (xx-x)<-dxmax Then xx=x-dxmax:auxvar2+=1
  	If Abs(xx-x)>0 Then
 		auxvar3+=1
 		If j0>0 Then
 			auxvar3+=2:j0=0
 		EndIf
 	Else
 		j0=1
 	EndIf
 	'If xx>auxvar3 Then auxvar3=xx
 	'auxvar3+=1
 Next
 'Put #fileout,,ficin(0),size
Next

If auxvar3>auxvar5 Then auxvar5=auxvar3
auxvar4+=auxvar3

Next jj
Next ii
End Sub 
Dim Shared As String abc:abc="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
Dim Shared As String abcd(64),ficout
For i=0 To 61
	abcd(i)=Mid(abc,i+1,1)
Next
Dim Shared As String msg,crlf,msg1(130),msg2(130)
Sub test4()
msg="":crlf=Chr(13)+Chr(10)
file=FreeFile  
Open fic1 For Binary Access Read As #file
?Str(Lof(file))
Get #file,10,ficin(0),100
?Str(ficin(0))
size=4800\10'=480
sizex=4800\10'=480
sizey=4800\8'=600
Var h=14.0
For ii=0 To 7
For jj=0 To 9 

'fic1="D:/srtm/w020n90.dem" 
Var tlng=LCase(Mid(fic1,Len(fic1)-10,4))
Var tlat=LCase(Mid(fic1,Len(fic1)-6,3))
Var lng=Int(Val(Mid(tlng,2)))
If Left(tlng,1)="w" Then lng=-lng
Var lat=Int(Val(Mid(tlat,2)))
If tlat="n40" Then lat=40
If tlat="s10" Then lat=-10
lng=lng+ii*5
lat=lat-jj*5-5
If lng<=0 Then
	tlng="lngW"+Str(Abs(lng))
Else
	tlng="lngE"+Str(Abs(lng))
EndIf
If lat>=0 Then
	tlat="latN"+Str(lat)
Else
	tlat="latS"+Str(Abs(lat))
EndIf
Var lnglat=tlng+tlat
fic2="D:/srtm/last/"+lnglat+".js"
fileout=FreeFile
Open fic2 For Binary Access write As #fileout
'Put #fileout,,lnglat+"="""

dx=ii*sizex
dy=jj*sizey
auxvar3=0
auxvar5=0
Var ih=0,jh=0
x=0:xx=0

For i1=1 To size Step 1
 ih+=1
 jh=0
 If ih>size*0.25 Then
 	ih=0
 	h=14
 EndIf
 ficout=""
 ix=i1'(i1*sizex)/size
 i=Int(ix+0.01)
 'i2=i+1:If i2>sizex Then i2=sizex	
 Get #file,dx*2+9600*(sizex-(i-1)+dy),ficin(0),sizey
 'Get #file,dx*2+9600*(sizex-(i2-1)+dy),ficin2(0),size
 j0=0:x0=0:dxx0=0
 x=0:xx=0
 For j1=0 To size-1 Step 1
 	iy=j1*sizey/size
 	j=Int(iy+0.01)
 	j2=j+1:If j2>=sizey Then j2=sizey-1
 	x=xx
 	If Abs(iy-j)<0.01 Then
 		xx=Int(ficin(j)+0.01)
 	Else
   	xx=Int(ficin(j)*(j+1-iy)+ficin(j2)*(iy-j)+0.01)
 	EndIf
 	If xx<14 Then auxvar+=1':xx=14':ficin(j)=0
   h+=(xx-h)*0.1
   If ih=0 Then 
    jh+=1
    If jh>size*0.7 Then
   	jh=0
      If h<14 Then msg+=" " Else msg+="x"
    EndIf   
   EndIf
   dxmax=30*60
 	If (xx-x)>dxmax Then xx=x+dxmax:auxvar2+=1
 	If (xx-x)<-dxmax Then xx=x-dxmax:auxvar2+=1
  	If Abs(xx-x)>0.1 Or j1=(size-1) Then
 		auxvar3+=1
 		iiii=Int((xx-x)/60+0.0001)
 		jjjj=Int((xx-x-iiii*60)+0.0001)
 		If (xx-x)=0 Then j0+=1
 		If j0>0 Then
 		   ficout+=abcd(30+0)+abcd(0)
 		   iiii1=Int(j0/60+0.0001)
 		   jjjj1=Int((j0-iiii1*60)+0.0001)
 			ficout+=abcd(30+iiii1)+abcd(jjjj1)
 			auxvar5+=j0
 			auxvar3+=1:j0=0
 		EndIf
 		If abs(xx-x)>0.1 Then
 			ficout+=abcd(30+iiii)+abcd(jjjj)
 			auxvar5+=1
 		EndIf
  	Else
 		j0+=1
  	EndIf
 	'If xx>auxvar3 Then auxvar3=xx
 	'auxvar3+=1
 Next
 'Put #fileout,,ficout
Next

'If auxvar3>auxvar5 Then auxvar5=auxvar3
auxvar4+=auxvar3

Put #fileout,,"""; "	
Close #fileout

Next jj
msg1(ii)=msg
msg=""'+=crlf
h=0

?Str(ii)
Next ii
fileout=-9999
End Sub 
Dim Shared As Short map(6001,6001)
Sub test55()
msg="":crlf=Chr(13)+Chr(10)
file=FreeFile  
Open fic1 For Binary Access Read As #file
?Str(Lof(file))
Var lfile=Lof(file)
?Str(4800*4800*10\8)'4800x6000
'Get #file,10,ficin(0),100
'?Str(ficin(0))
For i=1 To 6000
	Get #file,(i-1)*4800*2,ficin(0),4800
	For j=1 To 4800
		map(i,j)=ficin(j)
	Next
Next
Close #file
file=-9999

size=4800\8'=600
sizey=4800\8'=600
sizex=6000\10'=600
Var h=14.0
For ii=0 To 9
For jj=0 To 7

'fic1="D:/srtm/w020n90.dem" 
Var tlng=LCase(Mid(fic1,Len(fic1)-10,4))
Var tlat=LCase(Mid(fic1,Len(fic1)-6,3))
Var lng=Int(Val(Mid(tlng,2)))
If Left(tlng,1)="w" Then lng=-lng
Var lat=Int(Val(Mid(tlat,2)))
If tlat="n40" Then lat=40
If tlat="s10" Then lat=-10
lng=lng+jj*5
lat=lat-ii*5-5
If lng<=0 Then
	tlng="lngW"+Str(Abs(lng))
Else
	tlng="lngE"+Str(Abs(lng))
EndIf
If lat>=0 Then
	tlat="latN"+Str(lat)
Else
	tlat="latS"+Str(Abs(lat))
EndIf
Var lnglat=tlng+tlat
fic2="D:/srtm/last/"+lnglat+".js"
fileout=FreeFile
Open fic2 For Binary Access write As #fileout
Put #fileout,,lnglat+"="""

dx=ii*sizex
dy=jj*sizey
auxvar3=0
auxvar5=0
Var ih=0,jh=0

For i1=1 To size Step 1
 ih+=1
 jh=0
 If ih>size*0.6 Then
 	ih=0
 	h=14
 EndIf
 ficout=""
 ix=(i1*sizex)/size
 i=Int(ix+0.01)
 i2=i+1:If i2>sizex Then i2=sizex	
 'Get #file,dx*2+9600*(sizex-(i-1)+dy),ficin(0),sizey
 'Get #file,dx*2+9600*(sizex-(i2-1)+dy),ficin2(0),size
 j0=0:x0=0:dxx0=0
 x=0:xx=0	
 For j1=0 To size-1 Step 1
 	iy=j1*sizey/size
 	j=Int(iy+0.01)
 	j2=j+1:If j2>=sizey Then j2=sizey-1
 	x=xx
 	'If Abs(iy-j)<10.01 Then
 		'xx=Int(ficin(j)+0.01)
 		xx=map(dx+i,dy+j+1)
 	'Else
   '	'xx=Int(ficin(j)*(j+1-iy)+ficin(j2)*(iy-j)+0.01)
   '	xx=Int(map(dx+i,dy+j+1)*(i+1-ix)+map(dx+i,dy+j2+1)*(ix-i)+0.01)
 	'EndIf
 	If xx<14 Then auxvar+=1':xx=14':ficin(j)=0
   h+=(xx-h)*0.05
   If ih=0 Then 
    jh+=1
    If jh>size*0.7 Then
   	jh=0
      If h<14 Then msg+=" " Else msg+="x"
    EndIf   
   EndIf
   dxmax=30*60
 	If (xx-x)>dxmax Then xx=x+dxmax:auxvar2+=1
 	If (xx-x)<-dxmax Then xx=x-dxmax:auxvar2+=1
  	If Abs(xx-x)>0.1 Or j1=(size-1) Then
 		auxvar3+=1
 		iiii=Int((xx-x)/60+0.0001)
 		jjjj=Int((xx-x-iiii*60)+0.0001)
 		If (xx-x)=0 Then j0+=1
 		If j0>0 Then
 		   ficout+=abcd(30+0)+abcd(0)
 		   iiii1=Int(j0/60+0.0001)
 		   jjjj1=Int((j0-iiii1*60)+0.0001)
 			ficout+=abcd(30+iiii1)+abcd(jjjj1)
 			auxvar5+=j0
 			auxvar3+=1:j0=0
 		EndIf
 		If abs(xx-x)>0.1 Then
 			ficout+=abcd(30+iiii)+abcd(jjjj)
 			auxvar5+=1
 		EndIf
  	Else
 		j0+=1
  	EndIf
 	'If xx>auxvar3 Then auxvar3=xx
 	auxvar3+=1
 Next
 Put #fileout,,ficout
Next

'If auxvar3>auxvar5 Then auxvar5=auxvar3
auxvar4+=auxvar3

Put #fileout,,"""; "	
Close #fileout

Next jj
msg1(ii)=Str(ii)+"/"+Str(dx)+msg
msg=""'+=crlf

?Str(ii)
Next ii
fileout=-9999
End Sub 
Sub test5()
msg="":crlf=Chr(13)+Chr(10)
file=FreeFile  
Open fic1 For Binary Access Read As #file
?Str(Lof(file))
Var lfile=Lof(file)
?Str(4800*4800*10\8)'4800x6000
'Get #file,10,ficin(0),100
'?Str(ficin(0))
For i=1 To 6000
	Get #file,(i-1)*4800*2,ficin(0),4800
	For j=1 To 4800
		map(i,j)=ficin(j)
	Next
Next
Close #file
file=-9999

size=4800\32'=480
sizey=4800\32'=600
sizex=6000\40'=480
Var h=14.0
For ii=0 To 39
For jj=0 To 31

'fic1="D:/srtm/w020n90.dem" 
Var tlng=LCase(Mid(fic1,Len(fic1)-10,4))
Var tlat=LCase(Mid(fic1,Len(fic1)-6,3))
Var lng=Int(Val(Mid(tlng,2)))
If Left(tlng,1)="w" Then lng=-lng
Var lat=Int(Val(Mid(tlat,2)))
If tlat="n40" Then lat=40
If tlat="s10" Then lat=-10
lng=lng+jj*5
lat=lat-ii*5-5
If lng<=0 Then
	tlng="lngW"+Str(Abs(lng))
Else
	tlng="lngE"+Str(Abs(lng))
EndIf
If lat>=0 Then
	tlat="latN"+Str(lat)
Else
	tlat="latS"+Str(Abs(lat))
EndIf
Var lnglat=tlng+tlat
fic2="D:/srtm/last/"+lnglat+".js"
fileout=FreeFile
'Open fic2 For Binary Access write As #fileout
'Put #fileout,,lnglat+"="""

dx=ii*sizex
dy=jj*sizey
auxvar3=0
auxvar5=0
Var ih=0,jh=0

For i1=1 To size Step 1
 ih+=1
 jh=0
 If ih>size*0.6 Then
 	ih=0
 	h=14
 EndIf
 ficout=""
 ix=(i1*sizex)/size
 i=Int(ix+0.01)
 i2=i+1:If i2>sizex Then i2=sizex	
 'Get #file,dx*2+9600*(sizex-(i-1)+dy),ficin(0),sizey
 'Get #file,dx*2+9600*(sizex-(i2-1)+dy),ficin2(0),size
 j0=0:x0=0:dxx0=0
 x=0:xx=0	
 For j1=0 To size-1 Step 1
 	iy=j1*sizey/size
 	j=Int(iy+0.01)
 	j2=j+1:If j2>=sizey Then j2=sizey-1
 	x=xx
 	'If Abs(iy-j)<10.01 Then
 		'xx=Int(ficin(j)+0.01)
 		xx=map(dx+i,dy+j+1)
 	'Else
   '	'xx=Int(ficin(j)*(j+1-iy)+ficin(j2)*(iy-j)+0.01)
   '	xx=Int(map(dx+i,dy+j+1)*(i+1-ix)+map(dx+i,dy+j2+1)*(ix-i)+0.01)
 	'EndIf
 	If xx<14 Then auxvar+=1':xx=14':ficin(j)=0
   h+=(xx-h)*0.05
   If ih=0 Then 
    jh+=1
    If jh>size*0.7 Then
   	jh=0
      If h<14 Then msg+=" " Else msg+="x"
    EndIf   
   EndIf
   dxmax=30*60
 	If (xx-x)>dxmax Then xx=x+dxmax:auxvar2+=1
 	If (xx-x)<-dxmax Then xx=x-dxmax:auxvar2+=1
 	'If xx>auxvar3 Then auxvar3=xx
 	'auxvar3+=1
 Next
 'Put #fileout,,ficout
Next

'If auxvar3>auxvar5 Then auxvar5=auxvar3
auxvar4+=auxvar3

'Put #fileout,,"""; "	
'Close #fileout

Next jj
msg1(ii)=Str(ii)+"/"+Str(dx)+msg
msg=""'+=crlf

?Str(ii)
Next ii
fileout=-9999
End Sub 
test55()
?"auxvar="+Str(auxvar)
?"auxvar2="+Str(auxvar2)
?"auxvar3="+Str(auxvar3)
?"auxvar4="+Str(auxvar4)
?"auxvar5="+Str(auxvar5)
For j=0 To 40
	msg2(j)=""
Next
For i=0 To 40
	?msg1(i)
	For j=0 To 40
		If j<Len(msg1(i)) Then msg2(j)+=Mid(msg1(i),Len(msg1(i))-j,1)
	Next
	If msg1(i)="" Then Exit for
Next
For j=40 To 0 Step -1
	'?msg2(j)
	'If msg2(j)<>"" Then ?msg2(j)
Next
If file<>-9999 Then Close #file
If fileout<>-9999 Then Close #fileout

?"ok"
Sleep


End
