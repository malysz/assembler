data segment 
	args db 300 dup('$')
	Enoarguments db "Brak argumentow",10,13,"$"
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
	mov di,offset args		;ustawnienie wskaznika do tablicy argumentow
	dec di					;di-1 bo w skipwhite po pierwszym przejsci bysmy zaczynali od 1 zamiast 0
	mov ax,seg data
	mov ds,ax
	dec ch
	loadingloop:			;petla wczytywania znakow
		cmp ch,0d			;warunek konca
		je theend
		call skipwhite
		call loadchar
		jmp loadingloop
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
	mov ds:[di],al		;przepisanie al do tablicy argumentow
	mov dl,al			;wypisanie znaku
	mov ah,2
	int 21h
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
	call endline		;przejscie do nowej linii po bialym znaku
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

endline proc			;przejscie do nowej linii
	push ax				;odlozenie rejestrow na stos
	push bx
	push cx
	push dx
	mov ax,seg newline
	mov ds,ax
	mov dx,offset newline
	mov ah,9
	int 21h
	pop dx				;przywrocenie rejestrow ze stosu
	pop cx
	pop bx
	pop ax
	ret
endline endp

;********************************************************************

errnoargs:				;obsluga bledu braku argumentow
	mov ax,seg Enoarguments
	mov ds,ax
	mov dx,offset Enoarguments
	mov ah,9
	int 21h
	jmp theend
code ends

stack1 segment stack
	db 200 dup(?)
	top db ?
stack1 ends
end start