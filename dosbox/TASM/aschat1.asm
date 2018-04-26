; Modificado por Larissa Pinto Lopes

; Problemas encontrados:
;    1- 

; Mudanca significativa:
;    1- Dentro da procedure erro: zerar dh.
;    2- Conversao de 3Xh para decimal
;    3- Recebimento de nota: Procedure nzero, ncem. Apos ultimo digito informado,
;                                                   SI estarah apontando para ultimo caractere.

; Duvida encontrada
;    1- O incremento de SI (proc. unidade e "main") naum farah com que eu aponte para um espaco de memoria que
;       naum pertence a SI, ou seja, ocorreria corrupcao de memoria?
;    2- Conferir onde SI está ao fim da insercao das notas.

; PROGRAMA COM SI APONTANDO PARA O ULTIMO DIGITO DO ARRAY NOTA!!!

pilha   segment stack
	db      128 dup(?)      ;inicia
pilha   ends

dados   segment
mens1   db      'Digite uma nota, devera conter tres caracteres: #'
mens3   db      '(O programa finaliza apos ser informado tres notas) #'
mens2   db      'Caractere invalido. Por favor, digite novamente! #'
mens4   db      'Aluno aprovado por media.#'
mens5   db      'Aluno deve fazer prova final.#'
mens6   db      'Aluno reprovado por media.#'
mens7   db      'Media: #'
mens8   db      'Aluno devera alcancar a seguinte nota na prova final: #'
mens9   db      'Fim do programa.#'
mens10  db      'Obrigado por utiliza-lo.#'

nota    db      9 dup(?)
restod  db      3 dup(?)
numero  dw      3 dup(?)
nmedia  db      1 dup(?)
nfinal  db      1 dup(?)
naux    dw      0
;vaux    db      1 dup(?)
;nmedia  dw      0
dados ends

codigo  segment
	assume  ss:pilha, cs:codigo, ds:dados, es:dados
teste   proc    far
	push    ds
	xor     ax,ax
	push    ax
	mov     ax,dados
	mov     ds,ax
	mov     es,ax   ;ds e es segmento de dados com valor dados
	call    limpa
	mov     dx,0
	call    pos
	lea     bx, mens1
	lea     si, nota
	;mov     cl,3d	
	;mov     ch,3d
	;mov     dl, 1d
	call tenter
	lea si, mens2
	mov dx, 3fbh
        mov al, 8fh
        out dx, al
        mov dx, 3f8h
        mov al, 10h
        out dx, al
        inc dx
        mov al, 00h
        out dx, al
;2a parte das anotacoes
        mov dx, 3fbh
        mov al, 07h
        out dx, al
        mov dx, 3fch
        mov al, 10h
        out dx, al
; 
bola1:  mov dx, 3fdh
        in al, dx ;HEIM!? 
        test al, 01h ;0000 0001b
		jz trans
		mov dx, 3f8h
		in al, dx
		call video
		;jmp bola1
trans:  mov dx, 3fdh
	in al, dx
	test al, 20h ;0010 0000b
        jz bola1
enviar: mov al, [si]
        mov dx, 3f8h
		cmp al, '#'
		jz fim
        out dx, al
        inc si  
        jmp bola1	 
fim:     ;call tenter
         ;mov al, 'F'
         ;call video
         call    tenter
         call    tenter
		 lea bx, mens9
         call exibe
		 call tenter
		 lea bx, mens10
         call exibe
		 call tenter
		 ret
teste    endp

video   proc    near
	push    si
	push    bx
	push    di
	push    cx
	mov     bx,0
	mov     ah,14
	int     10h
	pop     cx
	pop     di
	pop     bx
	pop     si
	ret
video   endp

;dl,dh posiciona o cursor
pos proc near
    push AX
    push BX
    push cx
    xor BX,BX
    mov AH,2
    int 10h
    pop cx
    pop bx
    pop ax
    ret
pos endp

tec     proc    near
	push    di
	push    bx
	push    si
	push    cx
	mov     ah,0
	int     16h
	pop     cx
	pop     si
	pop     bx
	pop     di
	ret
tec     endp

limpa proc near
      push AX
      push BX
      push CX
      push DX
      xor al,al ; numero de linhas a rolar(0 significa todas as linhas)
      xor cx,cx ; cl = coluna do canto superior esquerdo, ch = linha do canto superior esquerdo
      mov dh,24 ; dh = linha do canto inferior direito
      mov dl,79 ; dl = coluna do canto inferior direito
      mov bh,07h; bh = atributo a ser usado nas linhas que ficarem em branco
      mov ah,06 ;rola a pagina de vídeo ativa para cima
      int 10h
      pop DX
      pop CX
      pop BX
      pop AX
      ret
limpa endp

tenter proc near
      push ax
      mov     al,0dh
	  call    video
	  mov     al,0ah
	  call    video 
	  pop ax
	  ret
tenter endp	  

exibe proc near
	;push ax
	;push bx
mens: mov     al,[bx]
	cmp     al,'#'
	jz      final
	call    video; exibe a mensagem 1 por caracter
	inc     bx
	jmp     mens
	;pop bx
	;pop ax
final:	ret
exibe endp

nzero proc near
       ;call video	
resto: mov [si],al
       dec cl
       jz  tdok
	   ;jnz conti
	   ;jmp resto
conti: inc si
       call tec
	   ;cmp al,0dh
       ;jz fim ; finaliza o programa
       cmp al,30h
       jl prob
       cmp al,39h
       jg prob
       call video
       jmp resto	   
tdok:  ret
prob:   mov dh, 1d
        ret	
nzero endp

msgs proc near
		;call tenter
        call exibe
	    jmp      bola6
	    ;call    video; exibe a mensagem 1 por caracter
		call    tenter
	    ;inc     bx
	    ;jmp     bola2		
bola6:  lea     bx, mens3
        cmp     dl, 0d
        jz      fin
		jnz     sep		
sep:    mov     al,0dh
	    call    video
	    mov     al,0ah
	    call    video
		;jmp     smsg
smsg:   call exibe
	    jmp      fin
	    ;call    video; exibe a mensagem 1 por caracter
	    ;inc     bx
	    ;jmp     smsg	
fin:    ret		
msgs endp	

ncem proc near
       mov [si], al
       mov al, 30h
	   ;call video
	   ;jmp cnulo
cnulo: ;call video
       dec cl
	   cmp cl, 0d
	   ;call video
       jz  term
       jnz tzero
tzero: inc si
       mov [si],al	   
       call video
       jmp cnulo
term:   ret
ncem endp

erro proc near
      lea bx,mens2
      call    tenter
	  ;call    tenter
      call tenter
      call exibe
      ;call video
	  cmp dh, 1d
	  jz zflag
	  jnz vfim
zflag: mov dh, 0d
vfim:  ret
erro endp

nascii proc near
         xor ax, ax
		 xor bx, bx
         mov al, [di]
		 ;cmp al, 100d  ;para garantir que a nota tenha tres digitos - compara com 100d pois esta eh a maior nota.
		 mov dl, al
		 mov cl, 30h
		 mov bl, 10d
		 jmp maior10
nmax:    inc si
         mov [si], cl
		 jmp pmostra
maior10: div bl
		 add ah, cl
		 mov [si], ah
		 ;call video
		 mov ah, 0d
		 inc si
		 cmp al, 10d
		 jb fimdiv
		 jae maior10
		 ;jg maior10
fimdiv:  add al, cl
         mov [si], al
		 cmp dl, 100d
		 jb nmax
;exibindo na tela
pmostra: mov bl, 3d
mostra:  mov al, [si]
		 call video
		 dec si
		 dec bl
		 cmp bl, 0d
		 jz nafim
		 jnz mostra
nafim: ret
nascii endp

codigo  ends
	end     teste   