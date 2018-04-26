pilha   segment stack
	db      128 dup(?)      ;inicia
pilha   ends

dados   segment
mens1   db      'Digite uma nota, com tres digitos:#'
mens2   db      'Digite um numero!#'
mensA	db		'Parabens, voce foi aprovado com media: #'
mensR	db		'Voce foi reprovado com media: #'
mensF1  db 		'Voce esta na final com media #'
mensF2	db		', e precisa de #'
mensF3  db		' para ser aprovado.#'
nota1 	db      1 dup(?)
nota2 	db		1 dup(?)
nota3 	db 		1 dup(?)
nfinal  db		1 dup(?)
valor1 	db      3 dup(?)
valor2 	db      3 dup(?)
valor3 	db      3 dup(?)
ascii	db 		3 dup(?)
media 	db 		1 dup(?)

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
		mov 	ch, 0d
		lea     si, valor1

;pnota vai pegar a proxima nota
pnota:
		lea     bx, mens1;bx recebe o endereço de mens1
		mov     cl, 3d 	;armazena 3 em cl (contador para a posição do array)

;exibe a mens1
bola2:
		mov     al,[bx]	;armazena o valor da posição apontada por bx em al
		cmp     al,'#'	;verifica se al=#
		jz      bola1	;se ZF está ativado, vai para bola1
		call    video 	;exibe a mensagem 1 por caracter
		inc     bx		;incrementa bx
		jmp     bola2	;vai para bola2

;bola1 verifica se o valor inserido é valido
bola1:
		call tec 		;chama a rotina do teclado
		cmp al, 31h		;verifica se al=1
		jz nota10		;se al=1, vai para nota10
		jg bola3		;se al>1, vai para bola3
		cmp al, 30h		;verifica se al=0
		jz menor10		;se al=0, vai para menor10
		jl bola3		;se al<0, vai para bola3

;nota10 coloca 10 na nota quando for inserido o digito 1
nota10:
		call video		;exibe o caracter inserido
		mov [si], al	;armazena o valor de al na posição de si
		inc si			;incrementa si
		dec cl			;decrementa cl
		mov al, 30h		;armazena 0 em al
		cmp cl, 0d		;verifica se cl=0
		jz proxnota		;se ZF está ativado, vai para proxnota
		jnz nota10		;se ZF não está ativado, vai para nota10

;menor10 coloca 0 no primeiro espaço do array e vai para dif100
menor10:
		call video		;exibe o caracter inserido
		mov [si], al	;armazena o valor de al na posição de si
		inc si			;incrementa si
		dec cl			;decrementa cl
		cmp cl, 0d		;verifica se cl=0
		jz  proxnota	;se ZF está ativado, vai para proxnota

;dif100 recebe os outros 2 digitos da nota
dif100:
		call tec 		;chama a rotina do teclado
		cmp al, 30h		;verifica se al=0
		jl bola3		;se al<0, vai para bola3
		cmp al, 39h		;verifica se al=9
		jg bola3		;se al>9, vai para bola3
		jmp menor10		;vai para menor10

;bola3 -> bola5 exibe mensagem de erro
bola3:
		lea di, mens2	;di recebe o endereço de mens2
		mov al,0dh
		call video		;exibe o que esta em al
		mov al,0ah
		call video		;exibe o que esta em al
bola4:
		mov al, [di]	;armazena o valor da posição apontada por di em al
		cmp al,'#'		;verifica se al=#
		jz bola5		;se ZF esta ativado, vai para bola5
		call video		;exibe o que esta em al
		inc     di		;incrementa di
		jmp     bola4	;vai para bola4
bola5:
		mov     al, 0dh
		call    video	;exibe o que esta em al
		mov     al, 0ah
		call    video	;exibe o que esta em al
		jmp     bola1	;vai para bola1

;proxnota verifica qual nota esta sendo inserida
proxnota:
		inc ch			;incrementa ah
		cmp ch, 1d		;verifica se ah=1
		jz proxnota1	;se ZF está ativado, vai para proxnota1
		cmp ch, 2d		;verifica se ah=2
		jz proxnota2	;se ZF está ativado, vai para proxnota2
		cmp ch, 3d		;verifica se ah=3
		jz proxnota3	;se ZF está ativado, vai para proxnota3

;proxnota1 converte e armazena a primeira nota
proxnota1:
		lea si, valor1  ;Faz si voltar a apontar para a primeira posição de valor1
		lea di, nota1	;di recebe o endereço de nota1
		call converte	;converte a nota em binário
		mov [di], cl	;armazena a nota convertida
		lea si, valor2	;aloca valor2 em si
		call linha		;posiciona o cursor na proxima linha
		jmp pnota		;vai para pnota

;proxnota2 converte e armazena a segunda nota
proxnota2:
		lea si, valor2	;faz si voltar a apontar para a primeira posição de valor2
		lea di, nota2	;di recebe o endereço de nota2
		call converte	;converte a nota em binário
		mov [di], cl	;armazena a nota convertida
		mov [di], cl	;armazena a nota convertida
		lea si, valor3	;aloca valor3 em si
		call linha		;posiciona o cursor na proxima linha
		jmp pnota		;vai para pnota

;proxnota3 converte e armazena a terceira nota
proxnota3:
		lea si, valor3	;faz si voltar a apontar para a primeira posiçao de valor3
		lea di, nota3
		call converte	;converte a nota em binario
		mov [di], cl	;armazena a nota convertida
		call linha		;posiciona o cursor na proxima linha

;cmedia calcula a media e armazena o resultado na variável media
cmedia:
		xor ax, ax		;limpa ax
		xor bx, bx		;limpa bx
		lea di, media	;di recebe o endereço de media

		lea si, nota1	;si recebe o endereço de nota1
		mov bl, [si]	;armazena o valor da posiçao apontada por si em bl
		add ax, bx		;soma ax com bx
		xor bx, bx 		;limpa bx, por garantia

		lea si, nota2	;si recebe o endereço de nota2
		mov bl, [si]	;armazena o valor da posiçao apontada por si em bl
		add ax, bx		;soma ax com bx
		xor bx, bx 		;limpa bx, por garantia

		lea si, nota3	;si recebe o endereço de nota3
		mov bl, [si]	;armazena o valor da posiçao
		add ax, bx		;soma ax com bx
		xor bx, bx 		;limpa bx, por garantia

		mov bl, 3d		;armazena 3 em bl para a divisão
		div bl			;divide al por bl
		mov [di], al	;armazena o valor de al na posição apontada por di

;aval avalia a média
aval:
		cmp media, 70d	;verifica se media=70
		jae aprov		;se media>=70, vai para aprov
		cmp media, 40d	;verifica se media=40
		jl reprov		;se media<40, vai para reprov
		jmp final		;vai para final

;aprov exibe a aprovacao e exibe a media
aprov:
		lea bx, mensA	;bx recebe o endereço de mensA
		call exibe 		;exibe mensA
		lea si, ascii	;si recebe o endereço de ascii
		lea di, media	;di recebe o endereço de media
		call convDH		;convDH converte media e exibe
		jmp fim			;vai para fim

;reprov exibe a reprovação e exibe a media
reprov:
		lea bx, mensR	;bx recebe o endereço de mensR
		call exibe 		;exibe mensR
		lea si, ascii	;si recebe o endereço de ascii
		lea di, media	;di recebe o endereço de media
		call convDH		;convDH converte media e exibe
		jmp fim			;vai para fim

;final exibe a media e diz quanto precisa para ser aprovado
final:
		xor ax, ax		;limpa ax
		xor cx, cx		;limpa cx
		lea si, ascii	;si recebe o endereço de ascii
		lea di, media	;di recebe o endereço de media
		mov al, [di]	;armazena o valor da posição apontada por di em al
		mov bl, 6d		;armazena 6 em bl para a multiplicaçao
		mul bl			;multiplica al por bl
		mov cx, ax		;armazena o valor de ax em cx
		mov ax, 500d	;armazena 500 em ax
		sub ax, cx		;subtrai ax de cx
		mov bl, 4d		;armazena 4 em bl
		div bl			;divide al por bl
		lea di, nfinal	;di recebe o endereço de nfinal
		mov [di], al	;armazena o valor de al na posição apontada por di

		lea bx, mensF1	;bx recebe o endereço de mensF1
		call exibe 		;exibe mensF1

		lea di, media	;di recebe o endereço de media
		call convDH		;convDH converte media e exibe

		lea bx, mensF2	;bx recebe o endereço de mensF2
		call exibe		;exibe mensF2

		lea di, nfinal  ;di recebe o endereço de nfinal
		call convDH		;convDH converte nfinal e exibe

		lea bx, mensF3	;dx recebe o endereço de mensF3
		call exibe		;exibe mensF3

fim:    ret
teste   endp

;tst:	push ax
;		mov al, 'A'
;		call video
;		pop ax

;exibe a mensagem passada em bx
exibe proc near
mens:
	mov     al,[bx]
	cmp     al,'#'
	jz      fin
	call    video	;exibe a mensagem 1 por caracter
	inc     bx
	jmp     mens
fin:	ret
exibe endp

;converte converte a nota inserida em decimal
converte proc near
		push si
		push ax
		mov al, [si]	;armazena o valor da posição de si em al
		sub al, 30h		;subtrai 30h de al
		mov bl, 100d	;armazena 100 em bl
		mul bl			;multiplica al por bl
		mov cl, al		;armazena o valor de al em cl
		inc si			;incrementa si
		mov al, [si]	;armazena o valor da posição de si em al
		sub al, 30h		;subtrai 30h de al
		mov bl, 10d		;armazena 10 em bl
		mul bl			;multiplica al por bl
		add cl, al		;adiciona o valor de al em cl
		inc si			;incrementa si
		mov al, [si] 	;armazena o valor da posição de si em al
		sub al, 30h		;subtrai 30h de al
		add cl, al		;adiciona o valor de al em cl
		pop ax
		pop si
		ret
converte endp

;        lea si, ascii      Toda vez que chamar convDH deve-se chamar os 2 lea, e muda o nmedia para o valor da nota em decimal
;		 lea di, nmedia
;convDH converte e exibe o valor inserido em hexadecimal
convDH proc near
		push ax
		push bx
		push cx
		xor ax, ax
		mov al, [di]
		mov bl, 10d
		mov cl, 30h
        div bl
		add ah, cl

		mov [si], ah
		mov ah, 0d
		inc si
		div bl
		add ah, cl
		mov [si], ah
		mov ah, 0d
		inc si
		add al, cl
		mov [si], al

		mov al, [si]
		call video
		dec si
		mov al, [si]
		call video
		dec si
		mov al,[si]
		call video
		pop cx
		pop bx
		pop ax
		ret
convDH endp

;exibe al
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

;pula uma linha
linha 	proc	near
		push    si
		push    bx
		push    di
		push    cx
		mov 	al, 0dh
		mov     bx, 0
		mov     ah, 14
		int     10h
		mov 	al, 0ah
		mov     bx, 0
		mov     ah, 14
		int     10h
		pop     cx
		pop     di
		pop     bx
		pop     si
		ret
linha	endp

;rotina do teclado
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

;limpa a tela
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

codigo  ends
	end     teste
