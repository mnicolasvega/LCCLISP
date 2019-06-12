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



; Obtiene la cantidad de columnas de una matriz 'Matrix'.
(DEFUN matriz_columnas(Matrix)
    (lista_len (CAR Matrix))
)



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



;====================================================================================================================================================




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



; Devuelve la matriz transpuesta
(DEFUN trans (Matriz)
    (trans_aux Matriz 0 (1- (matriz_columnas Matriz)))
)



; Construye la matriz transpuesta Mt a partir de la matriz M, colocando las columnas de M como filas en Mt.
(DEFUN trans_aux (Matriz ColID ColUlt)
    (COND
        ; Caso base:
        ; Si hay una única columna, entonces el resultado es una matriz con una sola fila (la columna transpuesta).
        (
            (= ColID ColUlt)
            (LIST (matriz_columna Matriz ColID))
        )
        ; Caso recursivo:
        ; Si hay más de una columna, la transpuesta será agregar una nueva fila al final de Mt la cual será la primer columna de M, y luego repetir
        ; el proceso con M sin su primer columna (instncia reducida).
        (
            T
            (CONS (matriz_columna Matriz ColID) (trans_aux Matriz (1+ ColID) ColUlt))
        )
    )
)



;====================================================================================================================================================




; Intenta dividir a N por todos los números en el intervalo (2 .. N-1), si alguno lo divide entonces es compuesto y retorna T, si ninguno lo
; divide entonces es primo y retorna NIL.
(DEFUN verificar_divisores (N Pos)
    (COND
        ; Caso base:
        ; Si se llegó hasta intentar dividir por uno (ningun número divide a N) entonces es primo, devuelve T.
        (
            (= Pos 1)
            T
        )
        ; Caso base:
        ; Si 'pos' divide al número, entonces es compuesto, devuelve NIL.
        (
            (= (mod N pos) 0) ; pos NO divide a N
            NIL
        )
        ; Caso recursivo:
        ; Si pos > 1 y no divide a N, tratar de dividirlo con pos-1 (instancia reducida).
        (
            T
            (verificar_divisores N (1- Pos))
        )
    )
)



; Verifica si el número N es primo
(DEFUN es_primo (N)
    (COND
        ; Si n < 2, entonces no es primo.
        (
            (< N 2)
            NIL
        )
        ; Si n = 2, es primo.
        (
            (= N 2)
            T
        )
        ; Si n > 2, será primo si ningún número en el intervalo (2 .. n-1) lo divide.
        (
            T
            (verificar_divisores N (1- N))
        )
    )
)




; Para un natural N, suma todos los primos en el intervalo (0 .. N)
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



; Verifica todos los números en el intervalo (Pos .. N), sumando todos los primos que encuentre.
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

; Para una cadena de longitud n, se encarga de invocar a 'permutar' n veces y va concatenando los resultados que permutar genera.
(DEFUN permLex_ex (Lista ListaPref Pos Ult)
    (COND
        ; Caso base:
        ; Si estamos en el último elemento, entonces el resultado es lo que produzca la función 'permutar'.
        (
            (= Pos Ult)
            (permutar Lista ListaPref Pos Ult)
        )
        ; Caso recursivo:
        ; Si no estamos en el último elemento, entonces el resultado es concatenar la permutación del primero y las permutaciones de los restantes ('permLex_ex') (instancia reducida).
        (
            (< Pos Ult)
            (APPEND (permutar Lista ListaPref Pos Ult) (permLex_ex Lista ListaPref (1+ Pos) Ult)) ; APPEND ya que permutar devuelve una lista con otra lista adentro, luego la con append se forma una lista de listas
        )
    )
)

; Permuta el primer símbolo de una lista y luego llama a permLex_ex con una instancia reducida para que permute esta instancia.
; El caso base es cuando la lista tiene un único elemento, entonces sus permutaciones son sólo el elemento, entonces devuelve una lista con otra lista dentro, la cual tiene al elemento.
(DEFUN permutar (Lista ListaPref Pos Ult)
    (COND
        ; Caso base:
        ; La lista a permutar tiene un elemento, luego las permutaciones posibles son 1: él mismo.
        ; Devolver una lista de listas que contenga todo el prefijo de la permutación concatenado al elemento; (( permutacionesAnteriores.elto ))
        (
            (NULL (CDR Lista)) ; len = 1
            (LIST (lista_add_last ListaPref (CAR Lista)))
        )
        ; Caso recursivo:
        ; Si la longitud de la lista es mayor a 1, removemos el i-ésimo ('Pos') elemento y al final de lista 'ListaPref' la cual va guardando el resultado de la permutación.
        ; Al quitar un elemento se produce una instancia reducida, y la recursividad cruzada llama a permLex_ex el cual permutará los n-1 elementos restantes.
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



; Variables de prueba
(SETQ lista `(1 2 3 4 a b))
(SETQ listaP `(a b c d))
(SETQ matrix `((1 2 3) (4 5 6) (7 8 9) (a b c)))

;(write (list_len lista))
;(write (list_elem lista 4))

;(write (matriz_columnas matrix))

;(write (matriz_columna matrix 1))

;(write (trans `((1 2 3 4) (5 6 7 8))))
;(write (trans (trans `((1 2 3) (4 5 6)))))

;(write (lista_remover lista 0))
;(write (lista_add_last lista `z))

(write (trans matrix)) ; (trans (trans matrix)))
(write (sumaPrimos 10))
(write (permLex listaP))



;====================================================================================================================================================