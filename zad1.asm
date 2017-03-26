data segment 
	args db 300 dup('$')
	counter db 1			;licznik argumentow
	argslength db 2 dup(0)		;tablica dlugosci argumentow
	binary db 16 dup(0)		;tablica na klucz w zapisie binarnym
	map db 154 dup(0)		;tablica 17x9 na ASCIIart
	asciitab db '.','o','+','=','*','B','O','X','@','%','&','#','/','^' ;tab znakow ASCII
	topframe db "+---[ASCII Art]---+$"
	botframe db "+-----------------+$"
	stoppos db 0
	
	Enoarguments db "Brak argumentow",10,13,"$"
	Ewrongfirstarg db "Niewlasciwy pierwszy argument",10,13,"$"
	Enotenougharg db "Zla liczba argumentow(podaj 2)",10,13,"$"
	Ekeylength db "Niepoprawna dlugosc klucza",10,13,"$"
	Ekeyvalue db "Nie poprawne wartosci klucza",10,13,"$"
	newline db 10,13,'$'
data ends

code segment
start:
	mov ax,seg top			;inicializacja stosu
	mov ss,ax
	mov dx,offset top
	
	mov si,80h
	mov ch,byte ptr es:[si]	;ilosc znakow w psp
	cmp ch,1d				;jezeli <=1 to zanczy ze wywolano bez argumentow
	jna errnoargs
	mov si,82h				;ustawnie na pierwszy znak po spacji w psp
	mov di,offset args			;ustawnienie wskaznika do tablicy argumentow
	dec di					;di-1 bo w skipwhite po pierwszym przejsci bysmy zaczynali od 1 zamiast 0
	mov ax,seg data
	mov ds,ax
	dec ch
	loadingloop:				;petla wczytywania znakow
		cmp ch,0d			;warunek konca
		je return
		call skipwhite
		call loadchar
		jmp loadingloop
return:
	call arglen
	call check
	call tobinary
	call starter
	call printer
	
theend:						;zakonczenie programu
	mov ah,4ch
	int 21h
;********************************************************************

loadchar proc			;procedura wczytywania znaku
loading:	
	mov al,es:[si]
	cmp al,32d			;jezeli spacja to koniec argumentu
	je finish
	cmp al,9d			;jezeli tab to koniec argumentu
	je finish
	mov ds:[di],al			;przepisanie al do tablicy argumentow
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
;-------------------------------------------------------------
endline proc			;przejscie do nowej linii
	push ax				;odlozenie rejestrow na stos
	push dx
	mov ax,seg newline
	mov ds,ax
	mov dx,offset newline
	mov ah,9
	int 21h
	pop dx				;przywrocenie rejestrow ze stosu
	pop ax
	ret
endline endp
;-------------------------------------------------------------
starter proc
	push ax
	push bx
	push cx
	push dx
	
	mov di, offset args
	xor bx,bx
	mov bl,ds:[di]			;pobieramy 1 argument
	cmp bl,'1'			;jezeli 1 to zmodyfikowany
	je modified
	jmp standard			;jezeli nie to 0 czyli standardowe
standard:
	call getbit
	call makeart
	jmp endstarter
modified:
	call getmodbit
	call makeart
	jmp endstarter
endstarter:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
starter endp
;-------------------------------------------------------------
arglen proc
	push cx
	push ax
	push dx
	
	mov di,offset args			;ustalenie wskaznikow na odpowiednie tablice i segment data
	mov si,offset argslength
	mov ax,seg data
	mov ds,ax
	xor cl,cl				;wyzerowanie licznika do argumentow
	xor ch,ch
countlength:
	mov al,ds:[di]
	cmp cl,2				;jesli cl>1 to znaczy ze juz przeanalizowano 2 argumenty
	je endarglen				;wiec koniec
	cmp al,'$'				;jesli wczytano $ to koniec argumentu
	je nextarg
	inc ch					;w przeciwnym wypadku zwieksz ch-dlugosc argumentu
	inc di
	jmp countlength
nextarg:
	mov ds:[si],ch				;zapisanie dlugosci argumentu
	add di,2d				;przesuniecie wskaznikow
	inc cl
	inc si
	mov ch,0d
	jmp countlength
endarglen:
	pop dx
	pop ax
	pop cx
	ret
arglen endp
;-------------------------------------------------------------
check proc				;procedura sprawdzajaca poprawnosc argumentow
	push ax
	push dx
	push cx
	push bx
	pushf
	;/////////////////liczba i dlugosc argumentow/////////
	mov ax,seg data
	mov ds,ax
	cmp ds:[counter],2			;sprawdzenie ilosci argumentow
	jne errcountarg
	
	mov si,offset argslength		;ustawienie wskaznika do tablicy dlugosci
	mov cl,1
	cmp [si],cl				;czy pierwsza dlugosc jest 1
	jne errfirstarg
	mov cl,32d
	inc si
	cmp [si],cl				;czy druga dlugosc jest 32
	jne errkeylength
	;///////////wartosci argumentow/////////////////////
	mov cl,ds:[si]			;dlugosc 2 arg
	mov di,offset args
	mov ah,49d			;1 w ASCII
	cmp ds:[di],ah			;jezeli pierwszy argument >1
	ja errfirstarg
	mov ah,48d			;0 w ASCII
	cmp ds:[di],ah			;jezeli pierwsz argument <0
	jb errfirstarg
	add di,2d			;przesuniecie na drugi argument
	inc cl
	secargloop:				;petla sprawdzajaca drugi argument
		dec cl
		cmp cl,0d			;jezeli dlugosc jest 0 to sprawdzono wszystkie znaki
		je endcheck
		inc di
		mov ah,103d			;f
		cmp ds:[di],ah			;jesli wieksze od f
		ja errkeyvalue
		mov ah,97d			;a
		cmp ds:[di],ah 			;jesli wieksze lub rowne a
		jae letters
		mov ah,47d			;0
		cmp ds:[di],ah			;jesli mniejsze od 0
		jbe errkeyvalue
		mov ah,57d			;9
		cmp ds:[di],ah			;jesli mniejsze lub rowne 9
		jbe digits
	letters:
		mov al,87d			;a-10=97-10=87
		sub ds:[di],al
		jmp secargloop
	digits:
		mov al,48d			;0
		sub ds:[di],al
		jmp secargloop
endcheck:
	popf
	pop bx
	pop cx
	pop dx
	pop ax
	ret
check endp
;-------------------------------------------------------------
tobinary proc				;zamiana klucza na binarny
	push ax					;wrzucenie rejestrow na stos
	push bx
	push cx
	push dx
	
	mov si,offset args			;wskaznik tablicy argumentow
	add si,2d				;ustawienie na 2 argument
	mov di,offset binary			;wskaznik tablicy klucza w sys bin
	mov cx,16d
	makebinar:
		mov al,ds:[si]			;zaladowanie argumentu do al
		inc si
		push cx
		mov cl,4d			;przesuwamy 1 argument z pary
		shl al,cl			;o 4 bity w lewo
		pop cx
		mov dl,al			;przechowujemy go w dl
		mov al,ds:[si]			;ladujemy drugi arg z pary do al
		inc si				;przesuwamy si
		add dl,al			;sumujemy oba arguemnty
		mov ds:[di],dl			;zapisujemy otrzymany bajt w tablicy binary
		inc di
	loop makebinar
	pop dx					;wyjscie z procedury
	pop cx
	pop bx
	pop ax
	ret
tobinary endp
;---------------------------------------------------------------
moveul proc					;gora lewo
	push ax
	push bx
	push cx
	push dx
	cmp si,0h				;jezeli goniec ustawiony w lewym gornym rogu
	je endul				;koniec ruch
	cmp si,17d				;jezeli przy suficie
	jb lefttopslide				;slizgaj w lewo
	sub si,17d				;jezeli nie to w gore o 1 pole
	mov ax,si				;reszta  dzielenia przez 17
	mov bl,17d				;zostanie umieszczona w ah
	div bl					;jezeli bedzie rowna 0 to prawda,
	cmp ah,0d				;ruch w gore juz wykonany
	je endul
	dec si					;jezeli nie to ruch w lewo
	jmp endul
lefttopslide:					;slizg po gornej krawedzi w lewo
	dec si
endul:
	mov bx,offset map			;poczatek tablicy ascii art
	add bx,si				;do bx offset pola gonca
	mov ax,1d				;zwiesz liczbe odwiedzin pola gonca
	add ds:[bx],ax
	pop dx
	pop cx
	pop bx
	pop ax
	ret
moveul endp

moveur proc
	push ax
	push bx
	push cx
	push dx
	cmp si,16d				;jezeli goniec w prawym gornym rogu
	je endur
	cmp si,17d				;jezeli goniec jest przy suficie
	jb righttopslide
	sub si,17d				;w gore o 1 pole
	mov ax,si
	mov bl,17d				
	div bl					
	cmp ah,16d				;jezeli reszta z dzielenia przez 17 rowna 16
	je endur				;ruch w gore juz wykonany
	inc si					;jezeli nie to przesun w prawo
	jmp endur
righttopslide:
	inc si
endur:
	mov bx,offset map
	add bx,si
	mov ax,1d
	add ds:[bx],ax
	pop dx
	pop cx
	pop bx
	pop ax
	ret
moveur endp

movedl proc
	push ax
	push bx
	push cx
	push dx
	cmp si,136d				;jezeli goniec w lewym dolnym rogu
	je enddl
	cmp si,135d				;jezeli goniec przy dolnej krawedzi
	ja leftbotslide
	add si,17d				;w dol o 1 pole
	mov ax,si
	mov bl,17d
	div bl
	cmp ah,0d				;jezeli reszta rowna 0 to ruch w gore juz wykonany
	je enddl
	dec si					;przesuniecie w lewo
	jmp enddl
leftbotslide:
	dec si
enddl:
	mov bx,offset map
	add bx,si
	mov ax,1d
	add ds:[bx],ax
	pop dx
	pop cx
	pop bx
	pop ax
	ret
movedl endp

movedr proc
	push ax
	push bx
	push cx
	push dx
	cmp si,152d				;jezeli goniec w prawym dolnym rogu
	je enddr
	cmp si,135d				;jezeli przy opdlodze
	ja rightbotslide
	add si,17d				;w dol o 1 pole
	mov ax,si
	mov bl,17d
	div bl
	cmp ah,16d				;jezeli reszta 16 to ruch w gore juz wykonany
	je enddr
	inc si					;przesuniecie w prawo
	jmp enddr
rightbotslide:
	inc si
enddr:
	mov bx,offset map
	add bx,si
	mov ax,1d
	add ds:[bx],ax
	pop dx
	pop cx
	pop bx
	pop ax
	ret
movedr endp
;---------------------------------------------------------------
getbit proc					;tworzenie licznika odwiedzin
	push ax
	push bx
	push cx
	push dx
	
	mov di,offset binary			;wskaznik na binary
	mov cx,16d				;licznik bajtow do analizy
	mov si,76d				;poczatkowa pozycja gonca
	
bitloop:					
	mov al,ds:[di]				;pobranie 8 bitow klucza do al
	inc di
	mov dx,5d				;licznik par bitow w bajcie (na poczatku petli bd dec dlatego 5)
	startbit:
		dec dx
		cmp dx,0d			;czy sprawdzono wszystkie
		je endbit			;jesli tak to wyjscie
		
		shr al,1d			;przesuwamy 1bit w prawo i sprawdzamy flage cf
		jc right			;jesli byla 1 to w prawo
		jmp left			;jesli 0 to w lewo
		
		right:
			shr al,1d		;sprawdzamy drugi bit pary
			jc rdown		;jak 1 to prawo dol
			jmp rup			;jak 0 to prawo gora
		left:
			shr al,1d		;sprawdamy drugi bit pary
			jc ldown		;jak 1 to lewo dol
			jmp lup			;jak 0 to lewo gora
		
		rup:
			call moveur		;ruch i skok do petli
			jmp startbit
		rdown:
			call movedr
			jmp startbit
		lup:
			call moveul
			jmp startbit
		ldown:
			call movedl
			jmp startbit
	endbit:
		loop bitloop
		mov di,offset stoppos
		mov ds:[di],si			;zapisanie pozycji koncowej gonca
		pop dx
		pop cx
		pop bx
		pop ax
		ret
getbit endp

makeart proc					;tworzenie ascii art na podstawie licznika odwiedzin
	push ax
	push bx
	push cx
	push dx
	mov si,offset asciitab			;tablica znakow ascii
	mov di,offset map
	dec di
	mov cx,154d				;licznik konwersji=153(dec na poczatku dla tego 154)
nextp:
	dec cx
	cmp cx,0d				;jezeli 0 to wczytano wszystkie
	je convert
	inc di					;przesuniecie na nastepne pole
	mov al,ds:[di]
	cmp al,0d				;jezeli zero odwiedzin zostawiamy puste
	je nextp
	cmp al,13d				;jezeli wiecej niz 13 odwiedzin
	ja hood					;to daszek
	xor bx,bx				;wyzerowanie dx
	mov bl,ds:[di]				;zaladowanie liczby odwiedzin do bl
	mov dh,ds:[si+bx-1d]			;znak do dh
	mov ds:[di],dh				;znak z dh do map
	jmp nextp
hood:
	mov al,"^"				;znak dla powyzej 13 odwiedzin
	mov ds:[di],al
	jmp nextp
convert:
	mov bx,offset map			;wskaznik na map
	mov di,offset [stoppos]
	add bx,ds:[di]				;wstawienie pozycji zakonczenia
	mov al,"E"
	mov ds:[bx],al				;wstawiamy na pozycje konca E
	mov di,offset map			;wskaznik na poczatek map
	mov al,"S"
	mov ds:[di+76d],al			;wstawienie na poz poczatkowa S
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
makeart endp

;+++++++++++++++++++++modyfikacja++++++++++++++++++++++++
getmodbit proc
push ax
	push bx
	push cx
	push dx
	
	mov di,offset binary			;wskaznik na binary
	mov cx,16d				;licznik bajtow do analizy
	mov si,76d				;poczatkowa pozycja gonca
	
modbitloop:					
	mov al,ds:[di]				;pobranie 8 bitow klucza do al
	inc di
	mov dx,5d				;licznik par bitow w bajcie (na poczatku petli bd dec dlatego 5)
	startmodbit:
		dec dx
		cmp dx,0d			;czy sprawdzono wszystkie
		je endmodbit			;jesli tak to wyjscie
		
		shr al,1d			;przesuwamy 1bit w prawo i sprawdzamy flage cf
		jc vert				;jesli byla 1 to pionowy
		jmp horiz			;jesli 0 to poziomy
		
		vert:
			shr al,1d		;sprawdzamy drugi bit pary
			jc down			;jak 1 to  dol
			jmp up			;jak 0 to  gora
		horiz:
			shr al,1d		;sprawdamy drugi bit pary
			jc rig			;jak 1 to prawo
			jmp lef			;jak 0 to lewo 
		
		up:
			call moveup		;ruch i skok do petli
			jmp startmodbit
		down:
			call movedown
			jmp startmodbit
		lef:
			call moveleft
			jmp startmodbit
		rig:
			call moveright
			jmp startmodbit
	endmodbit:
		loop modbitloop
		mov di,offset stoppos
		mov ds:[di],si			;zapisanie pozycji koncowej gonca
		pop dx
		pop cx
		pop bx
		pop ax
		ret
getmodbit endp

moveright proc
	push ax
	push bx
	push cx
	push dx
	mov ax,si			;jezeli goniec jest przy prawej krawedzi
	mov bl,17d			;to reszta z dzielenia przez 17 bedzie rowna 16
	div bl				;jezeli to prawda to sie nie ruszamy
	cmp ah,16d
	je endright
	inc si				;jezeli nie to przesuwamy sie w prawo
	jmp endright
endright:
	mov bx,offset map		;wskaznik na map
	add bx,si			;zaladownie pola gonca do bx
	mov ax,1d			;zwiekszenie licznika
	add ds:[bx],ax			;odwiedzin pola
	pop dx
	pop cx
	pop bx
	pop ax
	ret
moveright endp

moveleft proc
	push ax
	push bx
	push cx
	push dx
	mov ax,si			;jezeli goniec jest przy lewej krawedzi
	mov bl,17d			;to reszta z dzielenia przez 17
	div bl				;bezdie rowna 0
	cmp ah,0d		
	je endleft
	dec si				;w przeciwnym wypadku przesuwamy w lewo
	jmp endleft
endleft:
	mov bx,offset map		;poczatek map
	add bx,si			;pozycja gonca
	mov ax,1d			;zwiekszenie licznika
	add ds:[bx],ax
	pop dx
	pop cx
	pop bx
	pop ax
	ret
moveleft endp

moveup proc
	push ax
	push bx
	push cx
	push dx
	cmp si,17d			;jezeli goniec jest przy gornej krawedzi
	je endup			;to nie ruszamy
	sub si,17d			;w przeciwnym wypadku ruszamy sie w gore
	jmp endup
endup:
	mov bx,offset map		;poczatek map
	add bx,si			;pozycja gonca
	mov ax,1d
	add ds:[bx],ax			;zwiekszenie licznika
	pop dx
	pop cx
	pop bx
	pop ax
	ret
moveup endp

movedown proc
	push ax
	push bx
	push cx
	push dx
	cmp si,135d			;jezeli goniec jest przy dolnej krawedzi
	ja enddown			;to nie ruszamy
	add si,17d			;jak nie to w dol
	jmp enddown
enddown:
	mov bx,offset map		;poczatek map
	add bx,si			;poz gonca
	mov ax,1d			;zwiekszenie licznika
	add ds:[bx],ax
	pop dx
	pop cx
	pop bx
	pop ax
	ret
movedown endp
;+++++++++++++++++++++++++++++++++++++++++++++++++++
;---------------------------------------------------------------
printer proc
	push ax
	push bx
	push cx
	push dx
	mov cx,9d			;licznik wierszy
	mov di,offset map		;wskaznik na map
	mov dx,offset topframe		;wskaznik do wypisania gornej ramki ascii art
	mov ah,9
	int 21h
	mov dx,10d			;przejscie do nowej linii
	mov ah,2
	int 21h
printloop:
	mov bx,0d			;licznik wypisanych el wiersza
	mov dx,"|"
	mov ah,2
	int 21h
nchar:
	cmp bx,17d			;jezeli wypisano 17 elementow
	je endl				;nowa linia
	inc bx				;zwiekszamy licznik wypisywanych el wiersza
	mov dx,ds:[di]			;wczytujemy z tablicy do dx
	mov ah,2			;wypisujemy
	int 21h
	inc di				;przesuwamy wskaznik tablicy map
	jmp nchar
endl:
	mov dx,"|"			;wypisanie ramki bocznej
	mov ah,2
	int 21h
	mov dx,10d			;przejscie do nowej linii
	mov ah,2
	int 21h
	loop printloop
	
	mov dx,offset botframe			;wypisanie dolnej ramki
	mov ah,9
	int 21h
						;koniec wypisywania
	pop dx
	pop cx
	pop bx
	pop ax
	ret
printer endp
;********************************************************************

errnoargs:					;obsluga bledu braku argumentow
	mov ax,seg Enoarguments
	mov ds,ax
	mov dx,offset Enoarguments
	mov ah,9
	int 21h
	jmp theend
errfirstarg:					;obsluga bledu zlego pierwszego argumentu
	mov ax,seg Ewrongfirstarg
	mov ds,ax
	mov dx,offset Ewrongfirstarg
	mov ah,9
	int 21h
	jmp theend
errcountarg:					;obsluga bledu ilosci argumentow
	mov ax,seg Enotenougharg
	mov ds,ax
	mov dx,offset Enotenougharg
	mov ah,9
	int 21h
	jmp theend
errkeylength:					;obsluga bledu dlugosci klucza
	mov ax,seg Ekeylength
	mov ds,ax
	mov dx,offset Ekeylength
	mov ah,9
	int 21h
	jmp theend
errkeyvalue:					;obsluga bledu wartosci w kluczu
	mov ax,seg Ekeyvalue
	mov ds,ax
	mov dx,offset Ekeyvalue
	mov ah,9
	int 21h
	jmp theend
code ends

;*************************************************************************

stack1 segment stack
	db 200 dup(?)
	top db ?
stack1 ends
end start
