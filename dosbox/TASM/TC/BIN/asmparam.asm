;DEMONSTRACAO DE UMA ROTINA COM 3 PARAMETROS

MODEL	small   ;modelo de memoria

;------------------------------------------------------
;extern int Soma(int x, int y, int z);
;------------------------------------------------------

CODESEG
	PUBLIC _Soma  ;implementacao da rotina disponvel para outros modulos

	_Soma PROC  ;implementacao da rotina
		PUSH BP
		MOV  BP,SP

		MOV  AX, [BP + 04] ; AX := primeiro parametro
		ADD  AX, [BP + 06] ; soma AX com segundo parametro
		ADD  AX, [BP + 08] ; soma AX com terceiro parametro
		
		;RETORNA RESULTADO EM AX
		
		POP BP
		
		RET
	ENDP _Soma
END
