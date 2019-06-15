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



; Obtiene el elemento número 'Pos' de la lista 'List'
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



; Añade el elemento 'Elem' al final de la lista 'List'
(DEFUN lista_add_last (List Elem)
	(APPEND List (LIST Elem))
)



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



; Obtiene la cantidad de columnas de una matriz 'Matrix'.
(DEFUN get_cantidad_columnas(Matrix)
	(lista_len (CAR Matrix))
)



; Verifica si 'Matrix' es una matriz, es decir, una lista de listas
(DEFUN es_matriz (Matrix)
    (COND
        (
            (NULL Matrix)
            NIL
        )
        (
            T
            (es_matriz_ex Matrix 0 (1- (lista_len Matrix)))
        )
    )
)



(DEFUN es_matriz_ex (Matriz Pos Ult)
    (COND
        (
            (= Pos Ult)
            (LISTP (CAR Matriz))
        )
        (
            T
            (AND (LISTP (CAR Matriz)) (es_matriz_ex (CDR Matriz) (1+ Pos) Ult))
        )
    )
)




;====================================================================================================================================================




; Devuelve en una lista los elementos de la columna número 'ColID' de la matriz 'Matriz'.
(DEFUN matriz_get_columna (Matriz ColID)
	(COND
		(
			(NULL (CDR Matriz))
			(LIST (lista_elem (CAR Matriz) ColID))
		)
		(
			T
			(CONS (lista_elem (CAR Matriz) ColID) (matriz_get_columna (CDR Matriz) ColID))
		)
	)
)



; Devuelve la matriz transpuesta
(DEFUN trans (Matriz)
	(COND
		(
			(es_matriz Matriz)
			(trans_aux Matriz 0 (1- (get_cantidad_columnas Matriz)))
		)
	)
)



; Construye la matriz transpuesta Mt a partir de la matriz M, colocando las columnas de M como filas en Mt.
(DEFUN trans_aux (Matriz ColID ColUlt)
	(COND
		; Caso base:
		; Si hay una única columna, entonces el resultado es una matriz con una sola fila (la columna transpuesta).
		(
			(= ColID ColUlt)
			(LIST (matriz_get_columna Matriz ColID))
		)
		; Caso recursivo:
		; Si hay más de una columna, la transpuesta será agregar una nueva fila al final de Mt la cual será la primer columna de M, 
		; y luego repetir el proceso con M sin su primer columna (instancia reducida).
		(
			T
			(CONS (matriz_get_columna Matriz ColID) (trans_aux Matriz (1+ ColID) ColUlt))
		)
	)
)



;====================================================================================================================================================



; Dado un N impar tal que N > 2 y PosibleDivisor natural impar, determina si N es divisible por algún número impar k tal que PosibleDivisor <= k <= Tope
; Si existe algún k (i.e. N no es un número primo) entonces se retorna NIL, en caso contrario se retorna T.
(DEFUN es_primo_impar(N PosibleDivisor Tope)
	(COND
		; Caso base:
		;	El número 'PosibleDivisor' es mayor a 'Tope', por lo tanto no existe un posible k que divida a N.
		(
			(> PosibleDivisor Tope)
			T
		)
		
		; Caso base:
		;	Existe algún k que divida a N. K es igual a PosibleDivisor.
		(
			(= (mod N PosibleDivisor) 0)
			NIL
		)

		; Caso recursivo:
		;	como PosibleDivisor no divide a N, tenemos que existe algún k impar tal que PosibleDivisor <= k <= Tope y k divide a N
		;	si y solo si existe algún k' impar tal que PosibleDivisor+2 <= k' <= Tope y k' divide a N.
		(
			T
			(es_primo_impar N (+ PosibleDivisor 2) Tope)
		)	  
	)
)



; Dado un numero N determina si N es primo, en caso de serlo retorna T, en caso contrario retorna NIL.
(DEFUN es_primo(N)
	(COND
		(
			(< N 2) ; Si N es menor a 2 no es primo por definición.
			NIL
		)
	
		(
			(= N 2) ; Si N es igual a 2 es un número primo (el primer y único número primo par).
			T
		)
		
		(
			(= (mod N 2) 0) ; Si N no es igual 2 pero N es divisible por 2 entonces claramente no es primo.
			NIL
		)
		
	
		(
			T
			(es_primo_impar N 3 (FLOOR (sqrt N))) ; Si N es impar y mayor a 2 entonces N es primo si y solo si no existe algún número impar k tal que 3 <= k <= sqrt(N) y k | N.
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



;====================================================================================================================================================



; Dada una lista Lista y dos enteros Inicio, Final retorna una sublista S de Lista tal que
; S comienza en Inicio y finaliza en Final, ambos enteros incluídos,
; es decir, sea Lista = [X_1, ..., X_n] tenemos que S = [X_Inicio, ..., X_Final].
; Se asume que 0 <= Inicio < tam(subList) y 0 <= Final < tam(subList)
(DEFUN subList(Lista Inicio Final)
	(COND
		; Caso base:
		;	Si Inicio > Final entonces S = []
		(
			(> Inicio Final)
			`()
		)
			
		; Caso recursivo:
		;	Si Inicio <= Final entonces S es el elemento ubicado en la posición Inicio de Lista concatenado con 
		;	la sublista desde Inicio+1 hasta Final de Lista.
		;	Es decir, S = [X_Inicio] U [X_Inicio+1, ..., X_Final] 
		(
			T
			(APPEND
				(LIST
					(lista_elem Lista Inicio)
				)
				(subList
					Lista
					(1+ Inicio)
					Final
				)
			)
					
		)
	
	)
)



; Dada una lista de letras Lista se retorna una lista L con los mismos elementos de Lista pero ordenados lexicograficamente.
; Para realizar el orden se aplica el algoritmo Merge Sort.
(DEFUN mergeSort(Lista)
	(COND
		; Caso base:
		;	Lista es una lista vacía, por lo tanto L = [].
		(
			(NULL Lista)
			`()
		)
		
		; Caso base:
		;	Lista tiene un solo elemento por lo que ya está ordenada, L = Lista.
		(
			(= (lista_len Lista) 1)
			Lista
		)
		
		; Caso recursivo:
		;	Lista tiene más de un elemento.
		;	Luego L será igual a la primera mitad de la lista ordenada y la segunda mitad de la lista ordenada intercaladas entre si de manera ordenada.
		(
			T
			(intercalarOrdenado ; se intercala de manera ordenada las dos mitades ya ordenadas.
				(mergeSort ; se aplica merge de la primera mitad.
					(subList
						Lista
						0
						(1-
							(FLOOR
								(/ 
									(lista_len Lista)
									2
								)
							)
						)
					) 
				)
				
				(mergeSort ; se aplica merge de la segunda mitad.
					(subList
						Lista
						(FLOOR
							(/ 
								(lista_len Lista)
								2
							)
						)
						(1-
							(lista_len Lista)
						)
					) 
				)
			)
		)
	)
)



; Dada dos listas de letras ordenadas lexicograficamente, ListaA y listaB, retorna una lista L tal que
; L es el resultado de intercalar ListaA y ListaB de manera ordenada,
; es decir L contendrá todo elemento de ListaA y ListaB pero ordenados.
(DEFUN intercalarOrdenado(ListaA ListaB)
	(COND
		; Caso base:
		;	ListaA está vacía por lo tanto L = ListaB.
		(
			(NULL ListaA)
			ListaB
		)
		
		; Caso base:
		;	ListaB está vacía por lo tanto L = ListaA.
		(
			(NULL ListaB)
			ListaA
		)
		
		; Caso recursivo:
		;	Si el primer elemento de ListaA es menor al primer elemento de ListaB,
		;	entonces L será el primer elemento de ListaA concatenado
		;	a la lista resultante de intercalar de manera ordenadada las listas ListaA (sin su primer elemento) y listaB.
		(
			(CHAR< (COERCE (CAR ListaA) 'character) (COERCE (CAR ListaB) 'character) )
			(APPEND
				(LIST
					(CAR ListaA)
				)
				(intercalarOrdenado ; instancia reducida
					(CDR ListaA)
					ListaB
				)
			)
		)
		
		; Caso recursivo:
		;	En caso contrario, el primer elemento de ListaB es menor o igual a el primer elemento de listaA.
		;	Por lo tanto, L será el primer elemento de ListaB concatenado
		;	a la lista resultante de intercalar de manera ordenadada las listas ListaA y listaB (sin su primer elemento).
		(
			T
			(APPEND
				(LIST
					(CAR ListaB)
				)
				(intercalarOrdenado ; instancia reducida
					ListaA
					(CDR ListaB)
				)
			)
		)
	)
)



;====================================================================================================================================================



(DEFUN permLex (Lista)
	(COND
		(
			(NULL Lista)
			`()
		)
		(
			T
			(permLex_ex (mergeSort Lista) `() 0 (1- (lista_len Lista))) ; Le pasamos la lista ordenada.
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
			(APPEND (permutar Lista ListaPref Pos Ult) (permLex_ex Lista ListaPref (1+ Pos) Ult)) ; APPEND ya que permutar devuelve una lista con otra lista adentro, luego con append se forma una lista de listas
		)
	)
)



; Permuta el primer símbolo de una lista y luego llama a permLex_ex con una instancia reducida para que permute esta instancia.
; El caso base es cuando la lista tiene un único elemento, entonces sus permutaciones son sólo el elemento, entonces devuelve una lista con otra lista dentro, la cual tiene al elemento.
(DEFUN permutar (Lista ListaPref Pos Ult)
	(COND
		; Caso base:
		; La lista a permutar tiene un elemento, luego las permutaciones posibles son 1: él mismo.
		; Devolver una lista de listas que contenga todo el prefijo de la permutación concatenado al elemento; (( permutacionesAnteriores.elemento ))
		(
			(NULL (CDR Lista)) ; len = 1
			(LIST (lista_add_last ListaPref (CAR Lista)))
		)
		; Caso recursivo:
		; Si la longitud de la lista es mayor a 1, removemos el i-ésimo ('Pos') elemento y al final de lista 'ListaPref' la cual va guardando el resultado de la permutación.
		; Al quitar un elemento se produce una instancia reducida, y la recursividad cruzada llama a permLex_ex el cual permutará los n-1 elementos restantes.
		(
			(permLex_ex
				(lista_remover Lista Pos) ; Quitamos el elemento número 'Pos' de la lista.
				(lista_add_last ListaPref (lista_elem Lista Pos)) ; Lo añadimos al final de la lista auxiliar.
				0
				(1- Ult)
			)
		)
	)
)



;====================================================================================================================================================