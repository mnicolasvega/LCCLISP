;gnu clisp 2.49



;====================================================================================================================================================



; Calcula la longitud de la lista 'List'
(DEFUN lista_len (List)
    (COND
        (
            (NULL List)
            0
        )
        (
            T
            (+ 1 (lista_len (CDR List)))
        )
    )
)



; Obtiene el elemento numéro 'Pos' de la lista 'List'
(DEFUN lista_elem (List Pos)
    (COND
        (
            (= Pos 0)
            (CAR List)
        )
        (
            T
            (lista_elem (CDR List) (1- Pos))
        )
    )
)



(DEFUN lista_add_last (List Elem)
    (APPEND List (LIST Elem))
)



;====================================================================================================================================================



; Obtiene la cantidad de filas de una matriz 'Matrix'.
(DEFUN matriz_filas(Matrix)
    (lista_len Matrix)
)



; Obtiene la cantidad de columnas de una matriz 'Matrix'.
(DEFUN matriz_columnas(Matrix)
    (lista_len (CAR Matrix))
)



;====================================================================================================================================================



; Devuelve en una lista los elementos de la fila número 'FilaID' de la matriz 'Matriz'.
(DEFUN matriz_fila (Matriz FilaID)
    (COND
        (
            (= FilaID 0)
            (CAR Matriz)
        )
        (
            T
            (matriz_fila (CDR Matriz) (1- FilaID))
        )
    )
)



; Devuelve en una lista los elementos de la columna número 'ColID' de la matriz 'Matriz'.
(DEFUN matriz_columna (Matriz ColID)
    (COND
        (
            (NULL (CDR Matriz))
            (LIST (lista_elem (CAR Matriz) ColID))
        )
        (
            T
            (CONS (lista_elem (CAR Matriz) ColID) (matriz_columna (CDR Matriz) ColID))
        )
    )
)



;====================================================================================================================================================



; Obtiene el elemento (Fila, Col) de la matriz 'Matrix'.
(DEFUN matriz_elem (Matrix Fila Col)
    (lista_elem (lista_elem Matrix Fila) Col)
)



;====================================================================================================================================================



;
(DEFUN trans (Matriz)
    (trans_aux Matriz 0 (1- (matriz_columnas Matriz)))
)



;
(DEFUN trans_aux (Matriz ColID ColUlt)
    (COND
        (
            (= ColID ColUlt)
            (LIST (matriz_columna Matriz ColID))
        )
        (
            T
            (CONS (matriz_columna Matriz ColID) (trans_aux Matriz (1+ ColID) ColUlt))
        )
    )
)



;====================================================================================================================================================



(DEFUN es_primo (N)
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
            (verificar_divisores N (1- N))
            T
        )
    )
)



(DEFUN verificar_divisores (N Pos)
    (COND
        (
            (= Pos 1)
            T
        )
        (
            (> (mod N pos) 0) ; pos NO divide a N
            (verificar_divisores N (1- Pos))
        )
        (
            T
            NIL
        )
    )
)



(DEFUN sumaPrimos (N)
    (COND
        (
            (< N 2)
            0
        )
        (
            T
            (sumaPrimos_ex N 2)
        )
    )
)



(DEFUN sumaPrimos_ex (N Pos)
    (COND
        (
            (< N Pos)
            0
        )
        (
            (es_primo Pos)
            (+ Pos (sumaPrimos_ex N (1+ Pos)))
        )
        (
            T
            (sumaPrimos_ex N (1+ Pos))
        )
    )
)




;====================================================================================================================================================




; Remuve el elemento de la posición 'Pos' de la lista 'Lista'.
(DEFUN lista_remover (Lista Pos)
    (COND
        (
            (= 0 Pos)
            (CDR Lista) ; Descarta el primer caracter (el de la posicion 'Pos')
        )
        (
            T
            (CONS (CAR Lista) (lista_remover (CDR Lista) (1- Pos)))
        )
    )
)


; Quita el elto. en la posición 'Pos' y lo coloca al comienzo de la lista 'Lista', el primer elemento original queda en segundo lugar.
(DEFUN lista_acomodar (Lista Pos)
     (CONS (lista_elem Lista Pos) (lista_remover Lista Pos))
)


(DEFUN permLex (Lista)
    (COND
        (
            (NULL Lista)
            `()
        )
        (
            (permLex_ex Lista `() 0 (1- (lista_len Lista)))
        )
    )
)

; Si la lista es de longitud n, invoca a permutar n veces (desde 0 hasta n-1) y construye una lista con los resultados de cada permutar (lista de listas)
(DEFUN permLex_ex (Lista ListaPref Pos Ult)
    (COND
        (
            (= Pos Ult)
            (permutar Lista ListaPref Pos Ult)
        )
        (
            (< Pos Ult)
            (APPEND (permutar Lista ListaPref Pos Ult) (permLex_ex Lista ListaPref (1+ Pos) Ult))
        )
    )
)

(DEFUN permutar (Lista ListaPref Pos Ult)
    (COND
        (
            (NULL (CDR Lista)) ; len = 1
            (LIST (lista_add_last ListaPref (CAR Lista)))
        )
        (
            (permLex_ex
                (lista_remover Lista Pos) ; Quitamos el elto. número 'Pos' de la lista.
                (lista_add_last ListaPref (lista_elem Lista Pos)) ; Lo añadimos al final de la lista auxiliar.
                0
                (1- Ult)
            )
        )
    )
)



;====================================================================================================================================================




; Imprimir el resultado de la operacion suc(5)
(SETQ lista `(1 2 3 4 a b))
(SETQ listaP `(a b c d))
(SETQ matrix `((1 2 3) (4 5 6) (7 8 9) (a b c)))
;(write (list_len lista))
;(write (list_elem lista 4))
;(write (matriz_elem matrix 1 1))

;(write (matriz_filas matrix))
;(write " x ")
;(write (matriz_columnas matrix))

;(write (matriz_fila matrix 2))
;(write (matriz_columna matrix 1))

;(write (trans matrix))

;(write (trans `((1 2 3 4) (5 6 7 8))))
;(write (trans (trans `((1 2 3) (4 5 6)))))

;(write (sumaPrimos 17))

;(write (lista_remover lista 0))
;(write (lista_acomodar lista 2))

;(write (lista_add_last lista `z))
(write (permLex listaP))
       
       ;hola franco soy agustina