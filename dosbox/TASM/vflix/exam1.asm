; use ; para fazer coment�rios em programas assembly
.MODEL SMALL ;modelo de mem�ria
.STACK ;espa�o de mem�ria para instru��es do programa na pilha
.CODE ;as linhas seguintes s�o instru��es do programa
mov ah,01h ;move o valor 01h para o registrador ah
mov cx,07h ;move o valor 07h para o registrador cx
int 10h ;interrup��o 10h
mov ah,4ch ;move o valor 4ch para o registrador ah
int 21h ;interrup��o 21h
END ;finaliza o c�digo do programa