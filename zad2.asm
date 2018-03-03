data segment 
	args db 200 dup('$')	
	counter db 1			;licznik argumentow
	argslength db 3 dup(0)	;tablica dlugosci argumentow
	inname db 13 dup (0)
	outname db 13 dup (0)
	inhandler dw ?
	outhandler dw ?
	mode db 0				;1-dekompresja 0-kompresja
	bufferIn db 4000h dup (?)
	bufferOut db 4000h dup (?)
	bufferSize dw 4000h
	lastbyte db ?
	
	Enoarguments db "Brak argumentow",10,13,"$"
	Eread db "Blad odczytu z pliku",10,13,"$"
	Eargscount db "Zla ilosc argumentow",10,13,"$"
	Eopen db "Blad otwarcia pliku do odczytu",10,13,"$"
	Esave db "Blad otwarcia pliku do zapisu",10,13,"$"
	Emode db "Zly argumentu(jedyna mozliwosc -d)",10,13,"$"
	Ewrite	db "Blad przy zapisie do pliku",10,13,"$"
	Enameleng db "Niepoprawna dlugosc nazwy pliku(max 8 znakow nazwy + . + 3 znaki rozszerzenia)",10,13,"$"
data ends

code segment
start:
	mov ax,seg top			;inicializacja stosu
	mov ss,ax
	mov sp,offset top
	
	mov si,80h
	mov ch,byte ptr es:[si]	;ilosc znakow w psp
	cmp ch,1d				;jezeli <=1 to zanczy ze wywolano bez argumentow
	jna errnoargs
	mov si,82h				;ustawnie na pierwszy znak po spacji w psp
	mov di,offset args		;ustawnienie wskaznika do tablicy argumentow
	dec di					;di-1 bo w skipwhite po pierwszym przejsci bysmy zaczynali od 1 zamiast 0
	mov ax,seg data
	mov ds,ax
	dec ch
	loadingloop:			;petla wczytywania znakow
		cmp ch,0d			;warunek konca
		je endloading
		call skipwhite
		call loadchar
		jmp loadingloop
		
endloading:
	call arglen
	call check
	call filenames
	call openfiles
	cmp ds:[mode],0d
	je compression		;jesli mode 0 to kompresja jesli 1 to dekompresja
	call decompress
	jmp theend
compression:
	call compress
	
theend:						;zakonczenie programu
	mov ah,3eh
	int 21h
	mov ah,4ch
	int 21h
;****************************funkcje parsera****************************************

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
	cmp cl,3					;jesli cl>2 to znaczy ze juz przeanalizowano 3 argumenty
	je endarglen				;wiec koniec
	cmp al,'$'					;jesli wczytano $ to koniec argumentu
	je nextarg
	inc ch						;w przeciwnym wypadku zwieksz ch-dlugosc argumentu
	inc di
	jmp countlength
nextarg:
	mov ds:[si],ch				;zapisanie dlugosci argumentu
	add di,2d						;przesuniecie wskaznikow
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

check proc 
	push ax
	mov al,ds:[counter]
	cmp al,2d	;czy 2 argumenty
	je accept2
	cmp al,3d	;czy 3 argumenty
	je accept3
	mov al,0d	;ustawiamy al na 0 dla bledu
	call errorfun
accept3:
	mov al,ds:[argslength]	;sprawdzamy czy dlugosc -d jest 2
	cmp al,2d
	jne accept3err			;jezeli nie to blad
	mov al,ds:[args]		;sprawdzamy czy drugi arg to -d
	cmp al,'-'
	jne accept3err
	mov al,ds:[args+1]
	cmp al,'d'
	jne accept3err
	mov al,ds:[argslength+1]
	cmp al,12d					;dlugosc nazwy pliku moze byc max 12
	jg lengtherr
	mov al,ds:[argslength+2]
	cmp al,12d
	jg lengtherr
accept:
	pop ax
	ret
accept2:
	mov al,ds:[argslength]
	cmp al,12d
	jg lengtherr
	mov al,ds:[argslength+1]
	cmp al,12d
	jg lengtherr
	jmp accept
accept3err:
	mov al,1d
	call errorfun
lengtherr:
	mov al,6d
	call errorfun
check endp


;**************************funkcje rle******************************************

filenames proc								;zapisanie nazw plikow do odczytu i zapisu i ustawienie trybu programu
	push ax
	push si
	push di
	
	mov si,offset inname
	mov di,offset args
	mov al,ds:[counter]
	cmp al,3d
	je three
	jmp inputname
three:
	add di,4d								;dodanie do wskaznika 
	mov ds:[mode],1d					;ustawienie mode na 1 dla dekompresji
inputname:
	mov al,ds:[di]							;przepisanie znaku z tablicy argumentow
	mov ds:[si],al							;do nazwy pliku
	inc di
	inc si
	cmp al,'$'								;sprawdzamy nastepny znak
	jne inputname							;jesli rozne od $ to petla
	dec si									;jesli nie to powrot wskaznika o 1
	mov byte ptr [si],0d				;dodanie znaku konca pliku
	
	mov si,offset outname				;ustawienie wskaznika na miejsce na nazwe
	inc di
outputname:
	mov al,ds:[di]							;przepisanie znaku z tablicy arg
	mov ds:[si],al							;do nazwy pliku
	inc si
	inc di
	cmp al,'$'
	jne outputname
	dec si
	mov byte ptr [si],0d				;dodanie znaku konca
	
	pop di
	pop si
	pop ax
	ret
filenames endp
;-----------------------------
openfiles proc								;otwarcie plikow
	push ax
	push dx
	mov ah,3dh							;utworzenie pliku funkcja 3dh przerwania 21h
	mov dx,offset inname
	xor al,al
	int 21h
	jnc okopen								;jesli ok to przejdz dalej
	mov al,2d								;jak nie to blad
	call errorfun
okopen:
	mov ds:[inhandler],ax				;zapisanie uchwytu do pliku wejsciowego
	mov ah,3ch							;utworzenie pliku funkcja 3ch przerwania 21h
	mov dx,offset outname
	xor cx,cx
	int 21h
	jnc oksave								;jak ok to przejdz dalej
	mov al,3d								;jak nie to blad
	call errorfun
oksave:
	mov ds:[outhandler],ax			;zapisanie uchwytu do pliku wyjsciowego
	pop dx
	pop ax
	ret
openfiles endp
;-----------------------------
decompress proc
	push bx
	push dx
	push di
	push si
	
	mov si,ds:[bufferSize]				;zaladowanie rozmiaru bufora do si
	inc si										;zwiekszmy si zeby bufor odczytu zadaldowal dane
	xor di,di
readingloop:
	call getchar			
	cmp al,0d
	je zerobyte								;sprawdzamy dlaczego jest 0
	call putchar							;nie bylo 0 przepisujemy
returnreadingloop:
	loop readingloop
	
	cmp di,0d								;jezeli 0 to przed wyjsciem z petli oprozniono bufor
	je noflush
	call flush								;jak nie to trzeba oproznic
noflush:
	pop di
	pop si
	pop dx
	pop bx
	ret
	
zerobyte:
	mov dx,cx								;zachowujemy cx
	call getchar							;wczytujemy wart skompresowanego bajtu
	cmp dx,cx								;sprawdzamy czy cx sie zmienilo
	je cxnotmod			
	mov dx,cx								;zapisujemy nowe cx
	inc dx				
	
cxnotmod:
	dec dx									;dlugosc readingloop musi  sie zmniejszyc przez getchar
	mov cx,dx								;przywracamy cx
	cmp al,0d	
	jne compressed						;drugi bajt rozny od 0 wiec skompresowany fragment
	call putchar							;jezeli jest zerem to koduja pojedynczy zerowy bajt
	jmp returnreadingloop
	
compressed:
	mov dx,cx								;zapisujemy cx
	mov bl,al								;zapisujemy dlugosc skompresowanego bajtu, nie do cx bo getchar moze zmienic
	call getchar							;wczytujemy wartosc skompresowanego bajtu
	cmp dx,cx								;sprawdzamy czy wczytano nowa partie
	je cxnotmod2		
	mov dx,cx								;zapiujemy nowe cx
	inc dx				

cxnotmod2:
	dec dx									;zmniejszamy dx bo wywolanie getchar wykonalo dodatkowa iteracje readingloop
	xor cx,cx			
	mov cl,bl								;w bl dlugosc skompresowanego fragmentu

shortloop:
	call putchar
	loop shortloop
	
	mov cx,dx								;przywracamy cx
	jmp returnreadingloop
decompress endp
;-----------------------------
compress proc
	push bx
	push dx
	push si
	push di
	
	mov si, ds:[bufferSize]				;zaladowanie bufferSize do si
	inc si										;zeby bufor odczytu zaladowal dane
	xor di,di									;wyzerowanie iteratora po buforze zapisu
	mov bl,1d								;bl licznik powtorzen bajtow na poczatku 1
	call getchar
	mov ah,al								;poprzedni bajt zapisujemy w ah
readloop:
	call getchar							;teraz al nowy bajt w ah poprzedni
	cmp ah,0d
	jne notzerobyte
	call zerobytefun
	jmp endreadloop

notzerobyte:
	cmp ah,al
	jne notequalb
	call equalb
	jmp endreadloop

notequalb:
	cmp bl,3d								;sprawdzamy czy bylo wiecej niz 3 znaki
	jbe	lessthree
	call morethree
	jmp endreadloop
	
lessthree:
	call lessthreebytes

endreadloop:
	loop readloop
	
	cmp ah,0d
	jne notzerobyte2
	call zerobytefun
	jmp endoperations

notzerobyte2:
	cmp bl,3d								;sprawdzamy czy bylo wiecej niz 3 znaki
	jbe lessthree2
	call morethree
	jmp endoperations
lessthree2:
	call lessthreebytes
endoperations:
	cmp di,0d
	je noflush2
	call flush
noflush2:
	pop di
	pop si
	pop dx
	pop bx
	ret
	
compress endp
;-----------------------------
zerobytefun proc
	xchg ah,al
	call putchar
	call putchar
	ret
zerobytefun endp
;-----------------------------
equalb proc
	cmp bl,0ffh							;jesli 255 bajt to trzeba zapisac bo koniec miejsca w liczniku pow
	jne less255
	call morethree
	ret
less255:
	inc bl
	ret
equalb endp
;-----------------------------
morethree proc
	push ax									;bd zmieniac al
	mov al,0d
	call putchar							;wpisujemy znacznik modyfikacji
	mov al,bl
	call putchar							;wpisujemy ilosc powtorzen
	pop ax
	xchg ah,al								;poprzedni bajt do al zapisania do bufora obecny na miejsce poprzedniego
	call putchar
	mov bl,1d								;ilosc powtorzen od nowa
	ret
morethree endp
;-----------------------------
lessthreebytes proc
	push cx
	xor cx,cx
	mov cl,bl
	xchg ah,al								;poprzedni zapisujemy, obecny zapamietujemy jako poprzedni
less3loop:									;zapisanie znaku bl razy
	call putchar
	loop less3loop
	mov bl,1d								;reset licznika
	pop cx
	ret
lessthreebytes endp
;-----------------------------
getchar proc
	push ax									;readbuffer zmieni
	cmp si,ds:[bufferSize]				;czy skonczono przetwarzac inbuffer
	jb inbuffnotcomp
	call readbuffer						;wczytaj nowe dane
	mov cx,ax
	add cx,2d
	xor si,si									;bufor zapisujemy od nowa
	cmp ax,ds:[bufferSize]				;jesli ax > bufferSize to eof
	je inbuffnotcomp
	mov cx,ax								;eof
inbuffnotcomp:
	pop ax
	mov al,ds:[bufferIn+si]
	inc si
	ret
getchar endp
;-----------------------------
putchar proc
	push ax
	mov ds:[bufferOut+di],al
	inc di
	cmp di,ds:[bufferSize]				;czy bufor pelny
	jb outbuffnotfull
	mov ax,ds:[bufferSize]				;zapisujemy caly bufor
	call writebuffer						;zapisujemy bufor
	xor di,di									;zerujemy di
outbuffnotfull:
	pop ax
	ret
putchar endp
;-----------------------------
flush proc
	mov ax,di								;wyprozniamy bufor
	call writebuffer
	ret
flush endp
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
	jnc goodread
	mov al,4d
	call errorfun
goodread:
	pop dx
	pop cx
	pop bx
	ret
readbuffer endp
;-----------------------------
writebuffer proc
	push ax
	push bx
	push cx
	push dx
	
	mov dx,offset bufferOut
	mov cx,ax
	mov bx,ds:[outhandler]
	mov ah,40h
	int 21h
	jnc goodwrite
	mov al,5d
	call errorfun
goodwrite:
	cmp ax,cx
	je endwrite
	mov al,5d
	call errorfun
endwrite:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
writebuffer endp

;**************************bledy******************************************

errnoargs:									;obsluga bledu braku argumentow
	mov ax,seg Enoarguments
	mov ds,ax
	mov dx,offset Enoarguments
	mov ah,9
	int 21h
	jmp theend

errorfun proc
	cmp al,0d
	jne e1
	mov dx,offset Eargscount
	jmp enderror
e1:	cmp al,1d
	jne e2
	mov dx,offset Emode
	jmp enderror
e2:	cmp al,2d
	jne e3
	mov dx,offset Eopen
	jmp enderror
e3:	cmp al,3d
	jne e4
	mov dx,offset Esave
	jmp enderror
e4:	cmp al,4d
	jne e5
	mov dx,offset Eread
	jmp enderror
e5:	cmp al,5d
	jne e6
	mov dx,offset Ewrite
	jmp enderror
e6: cmp al,6d
	mov dx,offset Enameleng
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