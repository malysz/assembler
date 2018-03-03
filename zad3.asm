.386
data segment 
	args db 200 dup('$')	
	counter db 1			;licznik argumentow
	argslength db 0			;dlugosc argumentu
	inname db 13 dup (0)
	inhandler dw ?
	bufferIn db 1000d dup (?)
	bufferSize dw 200d
	
	Enoarguments db "Brak argumentow",10,13,"$"	
	Eread db "Blad odczytu z pliku",10,13,"$"			
	Eargscount db "Zla ilosc argumentow",10,13,"$"
	Eopen db "Blad otwarcia pliku do odczytu",10,13,"$"
	Ecommand db "Bledna komenda",10,13,"$"
	Emode db "Zly argumentu(jedyna mozliwosc -d)",10,13,"$"
	Enameleng db "Niepoprawna dlugosc nazwy pliku(max 8 znakow nazwy + . + 3 znaki rozszerzenia)",10,13,"$"
	
	tmp dw ? 		;zmienna tymczasowa do konwersji na calkowite
	angle dw 270d	;poczatkowy kat zolwia
	xbeg dw 160		;poczatkowa wspolrzedna x zolwia
	ybeg dw 160		;poczatkowa wspolrzedna y zolwia
	x1 dd 160		;wspolrzedne x poczatku odc
	y1 dd 100		;wspolrzedne y poczatku odc
	x2 dd ?			;wspolrzedne x konca odc
	y2 dd ?			;wspolrzedne x konca odc
	strRotate db "otate $"		;wzorce komend bez pierwszych znakow
	strMove db "ove $"
	strPen db "en$"
	strUp db "enup$"
	strDown db "endown$"
	penmode db 1d		;tryb piora, 1-opuszczone 0-podniesione
	dist dw 80		;dlugosc odcinka jaki trzeba przejsc
	one80 dw 180d
	two dw 2d
	deltaX dd ?
	deltaY dd ?
	deltaDi dd ?
data ends

code segment use16
start:
	mov ax,seg top			;inicializacja stosu
	mov ss,ax
	mov sp,offset top
	mov ax,seg data			;przypisanie do ds seg data
	mov ds,ax
	
	call parser
	call arglen
	call check
	call filenames
	call openfiles
	call draw
	
theend:						;zakonczenie programu
	mov ah,3eh
	int 21h
	mov ah,4ch
	int 21h
;****************************funkcje parsera****************************************
parser proc
	push ax
	push bx
	push cx
	push dx
	mov si,80h
	mov ch,byte ptr es:[si]	;ilosc znakow w psp
	cmp ch,1d				;jezeli <=1 to zanczy ze wywolano bez argumentow
	jna errnoargs
	mov si,82h				;ustawnie na pierwszy znak po spacji w psp
	mov di,offset args		;ustawnienie wskaznika do tablicy argumentow
	dec di					;di-1 bo w skipwhite po pierwszym przejsci bysmy zaczynali od 1 zamiast 0
	dec ch
	loadingloop:			;petla wczytywania znakow
		cmp ch,0d			;warunek konca
		je endloading
		call skipwhite
		call loadchar
		jmp loadingloop
endloading:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
parser endp
;//podfunkcje parsera
loadchar proc			;procedura wczytywania znaku
loading:	
	mov al,es:[si]
	cmp al,32d			;jezeli spacja to koniec argumentu
	je finish
	cmp al,9d			;jezeli tab to koniec argumentu
	je finish
	mov ds:[di],al		;przepisanie al do tablicy argumentow
	inc di				;przesuniecie wskaznikow
	inc si
	dec ch				;zmniejszenie liczby znakow wczytanych do psp
	cmp ch,0d
	je endload
	jmp loading
endload:
	inc di
	ret
finish:
	inc ds:[counter]	;zwiekszenie liczby arg
	jmp endload
loadchar endp

skipwhite proc			;pomijanie bielych znakow
skippingloop:
	mov al,es:[si]
	cmp al,32d			;spacja
	je next
	cmp al,9d			;tabulacja
	je next
	inc di
	ret
next:
	inc si
	dec ch
	jmp skippingloop
skipwhite endp

arglen proc
	push cx
	push ax
	push dx
	
	mov di,offset args			;ustalenie wskaznikow na odpowiednie tablice i segment data
	mov si,offset argslength
	xor cl,cl					;wyzerowanie licznika do argumentow
	xor ch,ch
countlength:
	mov al,ds:[di]
	cmp al,'$'					;jesli wczytano $ to koniec argumentu
	je endarglen
	inc ch						;w przeciwnym wypadku zwieksz ch-dlugosc argumentu
	inc di
	jmp countlength
endarglen:
	mov ds:[si],ch				;zapisanie dlugosci argumentu
	pop dx
	pop ax
	pop cx
	ret
arglen endp

check proc 
	push ax
	mov al,ds:[counter]
	cmp al,1d	;czy 1 argument
	je accept
	mov al,0d	;ustawiamy al na 0 dla bledu
	call errorfun
accept:
	mov al,ds:[argslength]
	cmp al,12d					;dlugosc nazwy pliku moze byc max 12
	jg lengtherr
	pop ax
	ret
lengtherr:
	mov al,1d
	call errorfun
check endp


;**************************funkcje rle******************************************

filenames proc			;zapisanie nazw plikow do odczytu i zapisu i ustawienie trybu programu
	push ax
	push si
	push di
	
	mov si,offset inname
	mov di,offset args
	cmp al,3d
inputname:
	mov al,ds:[di]		;przepisanie znaku z tablicy argumentow
	mov ds:[si],al		;do nazwy pliku
	inc di
	inc si
	cmp al,'$'					;sprawdzamy nastepny znak
	jne inputname				;jesli rozne od $ to petla
	dec si						;jesli nie to powrot wskaznika o 1
	mov byte ptr [si],0d		;dodanie znaku konca pliku
	
	pop di
	pop si
	pop ax
	ret
filenames endp
;-----------------------------
openfiles proc		;otwarcie plikow
	push ax
	push dx
	mov ah,3dh				;otworzenie pliku funkcja 3dh przerwania 21h
	mov dx,offset inname
	xor al,al
	int 21h
	jnc okopen				;jesli ok to przejdz dalej
	mov al,2d				;jak nie to blad
	call errorfun
okopen:
	mov ds:[inhandler],ax	;zapisanie uchwytu do pliku wejsciowego
	pop dx
	pop ax
	ret
openfiles endp
;-----------------------------
getchar proc
	push ax			;readbuffer zmieni
	cmp si,ds:[bufferSize]	;czy skonczono przetwarzac inbuffer
	jb inbuffnotcomp
	call readbuffer			;wczytaj nowe dane
	mov cx,ax
	add cx,2d
	xor si,si			;bufor zapisujemy od nowa
	cmp ax,ds:[bufferSize]		;jesli ax > bufferSize to eof
	je inbuffnotcomp
	mov cx,ax					;eof
inbuffnotcomp:
	pop ax
	mov al,ds:[bufferIn+si]
	inc si
	ret
getchar endp
;-----------------------------
readbuffer proc
	push bx
	push cx
	push dx
	mov dx,offset bufferIn
	mov cx,ds:[bufferSize]
	mov bx,ds:[inhandler]
	mov ah,3fh
	int 21h
	jnc goodread		;sprawdzenie poprawnosci odczytu
	mov al,3d			;ustalenie kodu bledu 
	call errorfun		;i jego wywolanie
goodread:
	pop dx
	pop cx
	pop bx
	ret
readbuffer endp

;********************************funkcje zolwia*****************************************

draw proc 
	push cx
	push si
	finit
	fild ds:[xbeg]
	fst ds:[x1]
	fild ds:[ybeg]
	fst ds:[y1]
	mov ax,13h			;przejscie do trybu graficznego
	int 10h				
	mov ax,0A000h
	mov es,ax			;ustawienie segmentu dla trybu graficznego
	mov si,ds:[bufferSize]
	inc si
	
drawingloop:
	call getchar
	cmp al,'r'
	jne jmpM
	call rotate
	jmp contdrawingloop
jmpM:
	cmp al,'m'
	jne jmpP
	call move
	jmp contdrawingloop
jmpP:
	cmp al,'p'
	jne jmpErr
	call pen
	jmp contdrawingloop
jmpErr:
	mov al,4d
	call errorfun
contdrawingloop:
	cmp cx,0d
	ja drawingloop
	
	xor ah,ah
	int 16h			;czekanie na przycisk
	mov ax,3		;powrot do trybu txt
	int 10h
	pop si
	pop cx
	ret
draw endp
;---------------------------
rotate proc
	push ax
	push bx
	push dx
	push di
	
	mov di,offset strRotate
cmpRotate:
	cmp byte ptr [di],'$'
	je endcmpRotate
	call getchar
	cmp ds:[di],al
	je strRotateEq
	mov al,4d
	call errorfun
strRotateEq:
	inc di
	jmp cmpRotate
endcmpRotate:
	call getInt
	mov ax,dx			;przepisujemy liczbe do ax
	xor dx,dx			;zerujemy dx
	add ax,ds:[angle]	;dodajemy do obecnego kata
	mov bx,360d			;aby otrzymac kat z zakresu <0,360>
	div bx
	mov ds:[angle],dx 	;ustawiamy biezacy kat
	call getchar
	
	pop di
	pop dx
	pop bx
	pop ax
	ret
rotate endp
;---------------------------
move proc
	push ax
	push bx
	push dx
	push di
	
	mov di,offset strMove
cmpMove:
	cmp byte ptr [di],'$'
	je endcmpMove
	call getchar
	cmp al,ds:[di]
	je strMoveEq
	mov al,4d
	call errorfun
strMoveEq:
	inc di
	jmp cmpMove
endcmpMove:
	call getInt
	mov ds:[dist],dx
	call drawline
	call getchar
	
	pop di
	pop dx
	pop bx
	pop ax
	ret
move endp
;---------------------------
pen proc
	push ax
	push bx
	push dx
	push di
	
	;mov di,offset strPen
;cmpPen:
;	cmp byte ptr [di],'$'
;	je endcmpPen
;	call getchar
;	cmp al,ds:[di]
;	je strPenEq
;	mov al,4d
;	call errorfun
;strPenEq:
;	inc di
;	jmp cmpPen
;endcmpPen:
;	call getchar
;	cmp al,'u'
;	jne pendown
;	call getchar
;	cmp al,'p'
;	jne penerror
;	call getchar
;	cmp al,13d
;	jne penerror
;	mov ds:[mode],0d
;	jmp endpen
;pendown:
;	cmp al,'d'
;	jne penerror
;	mov di,offset strDown
;pendownloop:
;	cmp byte ptr [di],'$'
;	je enddown
;	call getchar
;	cmp al,byte ptr [di]
;	je pendownEq
;	mov al,4d
;	call errorfun
;pendownEq:
;	inc di
;	jmp pendownloop
;enddown:
;	mov ds:[mode],1d
;	jmp endpen
	mov di,offset strDown
cmpDown:
	cmp byte ptr [di],'$'
	je endDownPen
	call getchar
	cmp al,byte ptr [di]
	je pendownEq
	jmp cmpUp
penDownEq:
	inc di
	jmp cmpDown
endDownPen:
	mov ds:[penmode],1d
	jmp endpen
	
cmpUp:
	mov di,offset strUp
cmpUp2:
	cmp byte ptr [di],'$'
	je endUpPen
	call getchar
	cmp al,byte ptr [di]
	je penupEq
	jmp penerror
penUpEq:
	inc di
	jmp cmpUp2
endUpPen:
	mov ds:[penmode],0d
	jmp endpen
penerror:
	mov al,4d
	call errorfun
endpen:
	
	pop di
	pop dx
	pop bx
	pop ax
	ret
pen endp
;---------------------------
getInt proc
	push ax
	push bx
	push di
	
	xor dx,dx
	xor ax,ax
	xor bx,bx
getintloop:
	call getchar
	cmp al,13d
	je endintloop
	cmp al,48d
	jae oknumber1
	mov al,4d
	call errorfun
oknumber1:
	cmp al,57d
	jbe oknumber2
	mov al,4d
	call errorfun
oknumber2:
	sub al,48d
	push ax
	inc bx
	jmp getintloop
endintloop:
	mov di,1d			;kolejne potegi 10
	xchg bx,cx
xchgloop:
	pop ax				;sciagamy ze stosu ostatnio wartosc
	imul ax,di			;razy potega 10
	add dx,ax			;dodajemy do obecnej wartosci
	imul di,10d			;zwiekszamy potege 10
loop xchgloop
	xchg bx,cx
	pop di
	pop bx
	pop ax
	ret
getInt endp
;---------------------------
drawline proc
	push eax
	push cx
	push si
	push di
	push bp
	push es
	
	call coordinates
	call checkArea
	
	finit
	fld ds:[deltaY]
	fimul ds:[two]
	fsub ds:[deltaX]
	fst ds:[deltaDi]
	
	finit
	fld ds:[y1]
	fist ds:[tmp]
	mov di,ds:[tmp]
	imul di,320d
	fld ds:[x1]
	fist ds:[tmp]
	add di,ds:[tmp]
	
	fld ds:[deltaX]
	fist ds:[tmp]
	mov cx, ds:[tmp]
	
lineloop:
	finit
	fld ds:[deltaDi]
	fist ds:[tmp]
	mov ax,ds:[tmp]
	cmp ax,0d
	jl lessZero
	add di,si
	add di,bp
	finit
	fld ds:[deltaY]
	fsub ds:[deltaX]
	fimul ds:[two]
	fadd ds:[deltaDi]
	fst ds:[deltaDi]
	jmp drawingpixel
lessZero:
	add di,si
	finit
	fld ds:[deltaY]
	fimul ds:[two]
	fadd ds:[deltaDi]
	fst ds:[deltaDi]
drawingpixel:
	cmp ds:[penmode],1d
	jne donotpaint
	mov byte ptr es:[di],15d
donotpaint:
loop lineloop
	mov eax,ds:[x2]
	mov ds:[x1],eax
	mov eax,ds:[y2]
	mov ds:[y1],eax
	pop es
	pop bp
	pop di
	pop si
	pop cx
	pop eax
	ret
drawline endp
;---------------------------
coordinates proc
	push eax
	push edx
	cmp ds:[angle],90d
	jne not90
	mov eax,ds:[x1]
	mov ds:[x2],eax
	finit
	fld ds:[y1]
	fiadd ds:[dist]
	fst ds:[y2]
	jmp endcoordinates
not90:
	cmp ds:[angle],270d
	jne not270
	mov eax,ds:[x1]
	mov ds:[x2],eax
	finit
	fld ds:[y1]
	fisub ds:[dist]
	fst ds:[y2]
	jmp endcoordinates
not270:
	cmp ds:[angle],0d
	jne not0
	mov eax,ds:[y1]
	mov ds:[y2],eax
	finit
	fld ds:[x1]
	fiadd ds:[dist]
	fst ds:[x2]
	jmp endcoordinates
not0:
	cmp ds:[angle],180d
	jne not180
	mov eax,ds:[y1]
	mov ds:[y2],eax
	finit
	fld ds:[x1]
	fisub ds:[dist]
	fst ds:[x2]
	jmp endcoordinates
not180:
	finit
	fldpi				;zaladuj pierwszy
	fidiv ds:[one80]	;st0 pi/180
	fimul ds:[angle]	;w st0 tworzymy kat w radianach - pi/180*kat
	fldz				;st0=0.0 st1=kat
	fadd st(0),st(1)	;"kopiujemy" kat aby miec w st0 i st1
	
	fcos				;obliczamy x2, w st0 mamy cos kata
	fimul ds:[dist]
	fadd ds:[x1]		;w st0 obliczam x2
	fst ds:[x2]			;zapisuje x2
	
	fsub ds:[x1]		;obliczamy y2, st0=delta x , w st1 jest nasz kat
	fxch st(1)			;xchg st0 i st1
	fptan				;w st1 tanges kata st0=1.0 i st2=dx
	fxch st(2)			;jedynka jest nam nie potrzebna wiec xchg st0 i st2
	fmul st(0),st(1)
	fadd ds:[y1]		;w st0 obliczam y2
	fst ds:[y2]			;zapisuje y2
endcoordinates:
	pop edx
	pop eax
	ret
coordinates endp
;-------------------------------
checkArea proc
	push ax
	
	mov ax,ds:[angle]
	cmp ax,45d
	jb first
	cmp ax,90d
	jb second
	cmp ax,135d
	jb third
	cmp ax,180d
	jb fourth
	cmp ax,225d
	jb fifth
	cmp ax,270d
	jb sixth
	cmp ax,315d
	jb seventh
	jmp eight
first:
	finit
	fld ds:[x2]
	fsub ds:[x1]
	fst ds:[deltaX]
	fld ds:[y2]
	fsub ds:[y1]
	fst ds:[deltaY]
	mov si,1d
	mov bp,320d
	jmp checked
second:
	finit
	fld ds:[y2]
	fsub ds:[y1]
	fst ds:[deltaX]
	fld ds:[x2]
	fsub ds:[x1]
	fst ds:[deltaY]
	mov si,320d
	mov bp,1d
	jmp checked
third:
	finit
	fld ds:[y2]
	fsub ds:[y1]
	fst ds:[deltaX]
	fld ds:[x1]
	fsub ds:[x2]
	fst ds:[deltaY]
	mov si,320d
	mov bp,(-1d)
	jmp checked
fourth:
	finit
	fld ds:[x1]
	fsub ds:[x2]
	fst ds:[deltaX]
	fld ds:[y2]
	fsub ds:[y1]
	fst ds:[deltaY]
	mov si,(-1d)
	mov bp,320d
	jmp checked
fifth:
	finit
	fld ds:[x1]
	fsub ds:[x2]
	fst ds:[deltaX]
	fld ds:[y1]
	fsub ds:[y2]
	fst ds:[deltaY]
	mov si,(-1d)
	mov bp,(-320d)
	jmp checked
sixth:
	finit
	fld ds:[y1]
	fsub ds:[y2]
	fst ds:[deltaX]
	fld ds:[x1]
	fsub ds:[x2]
	fst ds:[deltaY]
	mov si,(-320d)
	mov bp,(-1d)
	jmp checked
seventh:
	finit
	fld ds:[y1]
	fsub ds:[y2]
	fst ds:[deltaX]
	fld ds:[x2]
	fsub ds:[x1]
	fst ds:[deltaY]
	mov si,(-320d)
	mov bp,1d
	jmp checked
eight:
	finit
	fld ds:[x2]
	fsub ds:[x1]
	fst ds:[deltaX]
	fld ds:[y1]
	fsub ds:[y2]
	fst ds:[deltaY]
	mov si,1d
	mov bp,(-320d)
	jmp checked
checked:
	pop ax
	ret
checkArea endp

;**************************bledy******************************************

errnoargs:				;obsluga bledu braku argumentow
	mov ax,seg Enoarguments
	mov ds,ax
	mov dx,offset Enoarguments
	mov ah,9
	int 21h
	jmp theend

errorfun proc
	cmp al,0d
	jne e1
	mov dx, offset Eargscount
	jmp enderror
e1: cmp al,1d
	jne e2
	mov dx, offset Enameleng
	jmp enderror
e2:	cmp al,2d
	jne e2
	mov dx,offset Eopen
	jmp enderror
e3: cmp al,3d
	jne e4
	mov dx, offset Eread
	jmp enderror
e4: push ax
	mov ax,3
	int 10h
	pop ax
	cmp al,4d
	mov dx,offset Ecommand
	jmp enderror
enderror:
	mov ah,9
	int 21h
	mov ah,4ch
	int 21h
errorfun endp

code ends 

stack1 segment stack
	db 200 dup(?)
	top db ?
stack1 ends
end start