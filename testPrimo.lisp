; se asume N impar
(DEFUN es_primo_impar(N PosibleDivisor Tope)
    (COND
        
        (
            (> PosibleDivisor Tope)
                T
        )
        
        (
            (= (mod N PosibleDivisor) 0)
                NIL
        )

            
        (
            T
            (esPrimoImpar N (+ PosibleDivisor 2) Tope)
        )      
    )
)

(DEFUN es_primo(N)
    (COND
        (
            (< N 2)
                NIL
        )
    
        (
            (= N 2)
                T
        )
        
        (
            (= (mod N 2) 0)
                NIL
        )
        
    
        (
            T
            (esPrimoImpar N 3 (floor (sqrt N)))
        )
    )
)


; Para un natural N, suma todos los primos en el intervalo (0, ..., N)
(DEFUN sumaPrimos (N)
    (COND
        ; Si N < 2 entonces no hay ningún primo, se retorna 0.
        (
            (< N 2)
                0
        )
        ; Si N >= 2, el resultado lo calculará el método auxiliar 'sumaPrimos_ex'.
        (
            T
                (sumaPrimos_ex N 2)
        )
    )
)



; Verifica todos los números en el intervalo (Pos, ..., N), sumando todos los primos que encuentre.
(DEFUN sumaPrimos_ex (N Pos)
    (COND
        ; Caso base:
        ; Si pos > N ya no debemos continuar verificando, luego el resultado es 0.
        (
            (< N Pos)
				0
        )
        ; Caso recursivo:
        ; Si pos <= N, pos es primo, entonces el resultado de la suma de primos es pos + sumaPrimos del resto (instancia reducida).
        (
            (es_primo Pos)
				(+ Pos (sumaPrimos_ex N (1+ Pos)))
        )
        ; Caso recursivo:
        ; Si pos <= N, pos es compuesto, entonces el resultado de la suma de primos es sumaPrimos del resto (instancia reducida).
        (
            T
				(sumaPrimos_ex N (1+ Pos))
        )
    )
)