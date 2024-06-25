(ns tp-clojure-lf.core-test
  (:require [clojure.test :refer :all]
            [tp-clojure-lf.core :refer :all]))

;;
;; Pruebas para cargar-linea
;;
(deftest test-cargar-linea-1
  (testing "Probando la funcion cargar-linea agregando primer instruccion"
    (let [linea '(10 (PRINT X))
          amb ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]
          expected-output ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]]
      (is (= (cargar-linea linea amb) expected-output)))))

(deftest test-cargar-linea-2
  (testing "Probando la funcion cargar-linea agregando segunda instruccion"
    (let [linea '(20 (X = 100))
          amb ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
          expected-output ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]]
      (is (= (cargar-linea linea amb) expected-output)))))

(deftest test-cargar-linea-3
  (testing "Probando la funcion cargar-linea agregando instruccion en el medio"
    (let [linea '(15 (X = X + 1))
          amb ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
          expected-output ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]]
      (is (= (cargar-linea linea amb) expected-output)))))

(deftest test-cargar-linea-4
  (testing "Probando la funcion cargar-linea reemplazando instruccion"
    (let [linea '(15 (X = X - 1))
          amb ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
          expected-output ['((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]]
      (is (= (cargar-linea linea amb) expected-output)))))

;;
;; Pruebas para variable-float?
;;
(deftest test-variable-float?-con-variable-float-mas-de-una-letra
  (testing "Probando la funcion variable-float? con una variable float"
    (is (not (variable-float? 'XX))) ; debería retornar falso
    ))
(deftest test-variable-float?-con-variable-float
  (testing "Probando la funcion variable-float? con una variable float"
    (is (variable-float? 'X)) ; debería retornar verdadero
    ))

(deftest test-variable-float?-con-variable-integer
  (testing "Probando la funcion variable-float? con una variable integer"
    (is (not (variable-float? 'X%))) ; debería retornar falso
    ))

(deftest test-variable-float?-con-variable-string
  (testing "Probando la funcion variable-float? con una variable de string"
    (is (not (variable-float? 'X$))) ; debería retornar falso
    ))

;;
;; Pruebas para variable-integer?
;;
(deftest test-variable-integer?-con-variable-integer
  (testing "Probando la funcion variable-integer? con una variable integer"
    (is (variable-integer? 'X%)) ; debería retornar verdadero
    ))

(deftest test-variable-integer?-con-variable-float
  (testing "Probando la funcion variable-integer? con una variable float"
    (is (not (variable-integer? 'X))) ; debería retornar falso
    ))

(deftest test-variable-integer?-con-variable-string
  (testing "Probando la funcion variable-integer? con una variable de string"
    (is (not (variable-integer? 'X$))) ; debería retornar falso
    ))

;;
;; Pruebas para variable-string?
;;
(deftest test-variable-string?-con-variable-string
  (testing "Probando la funcion variable-string? con una variable de string"
    (is (variable-string? 'X$)) ; debería retornar verdadero
    ))

(deftest test-variable-string?-con-variable-float
  (testing "Probando la funcion variable-string? con una variable float"
    (is (not (variable-string? 'X))) ; debería retornar falso
    ))

(deftest test-variable-string?-con-variable-integer
  (testing "Probando la funcion variable-string? con una variable integer"
    (is (not (variable-string? 'X%))) ; debería retornar falso
    ))

;;
;; Pruebas para contar-sentencias
;;
(deftest test-contar-sentencias-10
  (testing "Probando la funcion contar-sentencias con la línea 10"
    (let [amb [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]]
      (is (= (contar-sentencias 10 amb) 2)))))

(deftest test-contar-sentencias-15
  (testing "Probando la funcion contar-sentencias con la línea 15"
    (let [amb [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]]
      (is (= (contar-sentencias 15 amb) 1)))))

(deftest test-contar-sentencias-20
  (testing "Probando la funcion contar-sentencias con la línea 20"
    (let [amb [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]]
      (is (= (contar-sentencias 20 amb) 2)))))

;;
;; Pruebas para expandir-nexts
;;
(deftest test-expandir-nexts-1-parametros-2-next
  (testing "Probando la funcion expandir-nexts 1 parametro 2 next"
    (let [n (list (list 'NEXT 'A (symbol ",") 'B))]
      (is (= (expandir-nexts n) '((NEXT A) (NEXT B)))))))

(deftest test-expandir-nexts-2-parametros-2-next
  (testing "Probando la funcion expandir-nexts 2 parametros 2 next"
    (let [n (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))]
      (is (= (expandir-nexts n) '((PRINT 1) (NEXT A) (NEXT B)))))))

(deftest test-expandir-nexts-2-parametros-3-next
  (testing "Probando la funcion expandir-nexts 2 parametros 3 next"
    (let [n (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C))]
      (is (= (expandir-nexts n) '((PRINT 1) (NEXT A) (NEXT B) (NEXT C)))))))

;;
;; Pruebas para eliminar-cero-decimal
;;
(deftest test-eliminar-cero-decimal-con-decimal
  (testing "Prueba eliminar-cero-decimal con un número decimal"
    (is (= (eliminar-cero-decimal 1.5) 1.5))))

(deftest test-eliminar-cero-decimal-con-entero-terminado-en-cero
  (testing "Prueba eliminar-cero-decimal con un número entero terminado en cero"
    (is (= (eliminar-cero-decimal 150) 150))))

(deftest test-eliminar-cero-decimal-con-decimal-cero
  (testing "Prueba eliminar-cero-decimal con un número decimal terminado en cero"
    (is (= (eliminar-cero-decimal 1.50) 1.5))))

(deftest test-eliminar-cero-decimal-con-decimal-doble-cero
  (testing "Prueba eliminar-cero-decimal con un número decimal terminado en doble cero"
    (is (= (eliminar-cero-decimal 1.500) 1.5))))

(deftest test-eliminar-cero-decimal-con-entero
  (testing "Prueba eliminar-cero-decimal con un número entero"
    (is (= (eliminar-cero-decimal 1.0) 1))))

(deftest test-eliminar-cero-decimal-con-no-numero
  (testing "Prueba eliminar-cero-decimal con un valor no numérico"
    (is (= (eliminar-cero-decimal 'A) 'A))))

;;
;; Pruebas para eliminar-cero-entero
;;
(deftest test-eliminar-cero-entero-con-nil
  (testing "Prueba eliminar-cero-entero con nil"
    (is (= (eliminar-cero-entero nil) nil))))

(deftest test-eliminar-cero-entero-con-simbolo
  (testing "Prueba eliminar-cero-entero con un símbolo"
    (is (= (eliminar-cero-entero 'A) 'A))))

(deftest test-eliminar-cero-entero-con-cero
  (testing "Prueba eliminar-cero-entero con cero"
    (is (= (eliminar-cero-entero 0) 0))))

(deftest test-eliminar-cero-entero-con-multiples-ceros
  (testing "Prueba eliminar-cero-entero con multiples ceros"
    (is (= (eliminar-cero-entero 000) 0))))

(deftest test-eliminar-cero-entero-con-decimal
  (testing "Prueba eliminar-cero-entero con un número decimal"
    (is (= (eliminar-cero-entero 1.5) 1.5))))

(deftest test-eliminar-cero-entero-con-entero
  (testing "Prueba eliminar-cero-entero con un número entero"
    (is (= (eliminar-cero-entero 1) 1))))

(deftest test-eliminar-cero-entero-con-entero-con-cero-adelante
  (testing "Prueba eliminar-cero-entero con un número entero con cero adelante"
    (is (= (eliminar-cero-entero 01) 1))))

(deftest test-eliminar-cero-entero-con-negativo
  (testing "Prueba eliminar-cero-entero con un número negativo"
    (is (= (eliminar-cero-entero -1) -1))))

(deftest test-eliminar-cero-entero-con-negativo-decimal
  (testing "Prueba eliminar-cero-entero con un número decimal negativo"
    (is (= (eliminar-cero-entero -1.5) -1.5))))

(deftest test-eliminar-cero-entero-con-decimal-menor-uno
  (testing "Prueba eliminar-cero-entero con un número decimal menor a uno"
    (is (= (eliminar-cero-entero 0.5) ".5"))))

(deftest test-eliminar-cero-entero-con-negativo-decimal-menor-uno
  (testing "Prueba eliminar-cero-entero con un número decimal negativo menor a uno"
    (is (= (eliminar-cero-entero -0.5) "-.5"))))

(deftest test-eliminar-cero-entero-con-negativo-decimal-menor-uno-y-multiples-ceros
  (testing "Prueba eliminar-cero-entero con un número decimal negativo menor a uno y multiples ceros"
    (is (= (eliminar-cero-entero -00.5) "-.5"))))

;;
;; Pruebas para palabra-reservada?
;;
(deftest test-palabra-reservada
  (testing "palabra-reservada?"
    (is (palabra-reservada? 'REM))
    (is (palabra-reservada? 'NEW))
    (is (palabra-reservada? 'CLEAR))
    (is (palabra-reservada? 'LIST))
    (is (palabra-reservada? 'RUN))
    (is (palabra-reservada? 'LOAD))
    (is (palabra-reservada? 'SAVE))
    (is (palabra-reservada? 'LET))
    (is (palabra-reservada? 'AND))
    (is (palabra-reservada? 'OR))
    (is (palabra-reservada? 'INT))
    (is (palabra-reservada? 'SIN))
    (is (palabra-reservada? 'ATN))
    (is (palabra-reservada? 'LEN))
    (is (palabra-reservada? 'MID$))
    (is (palabra-reservada? 'STR$))
    (is (palabra-reservada? 'CHR$))
    (is (palabra-reservada? 'ASC))
    (is (palabra-reservada? 'GOTO))
    (is (palabra-reservada? 'ON))
    (is (palabra-reservada? 'IF))
    (is (palabra-reservada? 'THEN))
    (is (palabra-reservada? 'FOR))
    (is (palabra-reservada? 'TO))
    (is (palabra-reservada? 'STEP))
    (is (palabra-reservada? 'NEXT))
    (is (palabra-reservada? 'GOSUB))
    (is (palabra-reservada? 'RETURN))
    (is (palabra-reservada? 'END))
    (is (palabra-reservada? 'INPUT))
    (is (palabra-reservada? 'READ))
    (is (palabra-reservada? 'RESTORE))
    (is (palabra-reservada? 'PRINT))))

(deftest test-palabra-reservada?-con-no-reservada
  (testing "Prueba palabra-reservada? con una palabra no reservada"
    (is (not (palabra-reservada? 'SPACE)))))

;;
;; Pruebas para operador?
;;
(deftest test-operador?-con-operador
  (testing "Prueba operador? con un operador"
    (is (operador? '+))
    (is (operador? '-))
    (is (operador? '*))
    (is (operador? '/))
    (is (operador? \^))
    (is (operador? '=))
    (is (operador? '<>))
    (is (operador? '<))
    (is (operador? '<=))
    (is (operador? '>))
    (is (operador? '>=))
    ;; Prueba con simbolos como string
    (is (operador? (symbol "+")))
    (is (operador? (symbol "-")))
    (is (operador? (symbol "*")))
    (is (operador? (symbol "/")))
    (is (operador? (symbol "^")))
    (is (operador? (symbol "=")))
    (is (operador? (symbol "<>")))
    (is (operador? (symbol "<")))
    (is (operador? (symbol "<=")))
    (is (operador? (symbol ">")))
    (is (operador? (symbol ">=")))
    ;; Prueba con strings
    (is (operador? "+"))
    (is (operador? "-"))
    (is (operador? "*"))
    (is (operador? "/"))
    (is (operador? "^"))
    (is (operador? "="))
    (is (operador? "<>"))
    (is (operador? "<"))
    (is (operador? "<="))
    (is (operador? ">"))
    (is (operador? ">="))))

  (deftest test-operador?-con-no-operador
    (testing "Prueba operador? con un símbolo no operador"
      (is (not (operador? (symbol "%"))))
      (is (not (operador? (symbol "&"))))
      (is (not (operador? (symbol "$"))))))


;;
;; Pruebas para anular-invalidos?
;;
(deftest test-anular-invalidos
  (testing "funcion anular-invalidos"
    (is (= (anular-invalidos '(LOAD NAME . BAS))
           '(LOAD NAME . BAS)))
    (is (= (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))
           '(IF X nil * Y < 12 THEN LET nil X = 0)))
    (is (= (anular-invalidos '(PRINT X + Y))
           '(PRINT X + Y)))
    (is (= (anular-invalidos '(PRINT X + Y &))
           '(PRINT X + Y nil)))
    (is (= (anular-invalidos '(IF X < Y THEN LET X = 0))
           '(IF X < Y THEN LET X = 0)))
    (is (= (anular-invalidos '(IF X < Y THEN LET X = 0 !))
           '(IF X < Y THEN LET X = 0 nil)))))


;;
;; Pruebas para dar-error
;;
(deftest test-dar-error-1
  (testing "funcion dar-error 16"
    (is (= (clojure.string/trim-newline (str (with-out-str (dar-error 16 [:ejecucion-inmediata 4])))) "?SYNTAX ERROR"))))

(deftest test-dar-error-2
  (testing "funcion dar-error ?ERROR DISK FULL"
    (is (= (clojure.string/trim-newline (str (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))) "?ERROR DISK FULL"))))

(deftest test-dar-error-3
  (testing "funcion dar-error 16 en 100 3"
    (is (= (clojure.string/trim-newline (str (with-out-str (dar-error 16 [100 3])))) "?SYNTAX ERROR IN 100"))))

(deftest test-dar-error-4
  (testing "funcion dar-error ?ERROR DISK FULL en 100 3"
    (is (= (clojure.string/trim-newline (str (with-out-str (dar-error "?ERROR DISK FULL" [100 3])))) "?ERROR DISK FULL IN 100"))))

;;
;; Pruebas para extraer-data
;;
(deftest test-extraer-data-vacio
  (testing "extraer-data con datos vacíos"
    (is (= (extraer-data '(()))))))

(deftest test-extraer-data-mixto
  (testing "extraer-data con datos mixtos"
    (is (= (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
           '("HOLA" "MUNDO" 10 20)))))

(deftest test-extraer-data-mixto-2
  (testing "extraer-data con datos mixtos"
    (is (= (extraer-data (list '(10 (PRINT X) (DATA 10) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
           '(10 "HOLA" "MUNDO" 10 20)))))

(deftest test-extraer-data-mixto-3
  (testing "extraer-data con datos mixtos"
    (is (= (extraer-data (list '(10 (PRINT X) (DATA 10) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA) (REM) (DATA ESTE) (DATA TAMPOCO)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
           '(10 "HOLA" "MUNDO" 10 20)))))

(deftest test-extraer-data-sin-datos
  (testing "extraer-data sin datos"
    (is (= (extraer-data (list '(10 (PRINT X)) '(20 (PRINT Y))))
           '()))))

;;
;; Pruebas para precedencia
;;
(deftest test-precedencia
  (testing "Testing precedencia"
    (is (= 1 (precedencia 'OR)))
    (is (= 2 (precedencia 'AND)))
    (is (= 4 (precedencia '=)))
    (is (= 4 (precedencia '<>)))
    (is (= 4 (precedencia '<)))
    (is (= 4 (precedencia '>)))
    (is (= 4 (precedencia '<=)))
    (is (= 4 (precedencia '>)))
    (is (= 5 (precedencia '+)))
    (is (= 5 (precedencia '-)))
    (is (= 6 (precedencia '*)))
    (is (= 6 (precedencia '/)))
    (is (= 7 (precedencia '-u)))
    (is (= 8 (precedencia '\^)))
    (is (= 9 (precedencia 'MID3$')))
    (is (= 9 (precedencia 'MID$)))))

;;
;; Pruebas para aridad
;;
(deftest test-aridad
  (testing "Testing aridad function"
    (is (= 3 (aridad 'MID3$)))
    (is (= 2 (aridad '*)))
    (is (= 2 (aridad '/)))
    (is (= 2 (aridad '+)))
    (is (= 2 (aridad '-)))
    (is (= 2 (aridad '<)))
    (is (= 2 (aridad '>)))
    (is (= 2 (aridad '=)))
    (is (= 2 (aridad '<=)))
    (is (= 2 (aridad '>=)))
    (is (= 2 (aridad '<>)))
    (is (= 2 (aridad '\^)))
    (is (= 2 (aridad 'AND)))
    (is (= 2 (aridad 'OR)))
    (is (= 2 (aridad 'LET)))
    (is (= 1 (aridad 'SIN)))
    (is (= 1 (aridad 'ATN)))
    (is (= 1 (aridad 'ASC)))
    (is (= 1 (aridad 'CHR$)))
    (is (= 1 (aridad 'STR$)))
    (is (= 1 (aridad 'INT)))
    (is (= 1 (aridad 'LEN)))
    (is (= 1 (aridad 'LOAD)))
    (is (= 1 (aridad 'SAVE)))
    (is (= 1 (aridad 'PRINT)))
    (is (= 1 (aridad 'DATA)))
    (is (= 1 (aridad 'READ)))
    (is (= 1 (aridad 'REM)))
    (is (= 1 (aridad 'RESTORE)))
    (is (= 1 (aridad 'NEXT)))
    (is (= 1 (aridad 'GOTO)))
    (is (= 0 (aridad 'OTHER)))))

;;
;; Pruebas para preprocesar-expresion
;;
(deftest test-preprocesar-expresion
  (testing "Testing preprocesar-expresion"
    (is (= (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}]) '("HOLA" + " MUNDO" + "")))
    (is (= (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}]) '(5 + 0 / 2 * 0)))))


;;
;; Pruebas para ejecutar-asignacion
;;
(deftest test-ejecutar-asignacion-1
  (testing "Testing ejecutar-asignacion function with X = 5 and empty environment"
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 {'X 5}]
           (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))))

(deftest test-ejecutar-asignacion-2
  (testing "Testing ejecutar-asignacion function with X = 5 and environment with X = 2"
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 {'X 5}]
           (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))))

(deftest test-ejecutar-asignacion-3
  (testing "Testing ejecutar-asignacion function with X = X + 1 and environment with X = 2"
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 {'X 3}]
           (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))))

(deftest test-ejecutar-asignacion-4
  (testing "Testing ejecutar-asignacion function with X$ = X$ + \" MUNDO\" and environment with X$ = \"HOLA\""
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 {'X$ "HOLA MUNDO"}]
           (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))))

(deftest test-ejecutar-asignacion-unused-variable
  (testing "Testing ejecutar-asignacion function with X = 5 and environment with X = 2 and unused variable Y"
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 {'X 5 'Y 10}]
           (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2 Y 10}])))))

(deftest test-ejecutar-asignacion-string-concatenation
  (testing "Testing ejecutar-asignacion function with X$ = X$ + \" WORLD\" and environment with X$ = \"HELLO\""
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 {'X$ "HELLO WORLD"}]
           (ejecutar-asignacion '(X$ = X$ + " WORLD") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HELLO"}])))))

;;
;; Pruebas para desambiguar
;;
(deftest test-desambiguar-1
  (testing "desambiguar function with unary minus"
    (is (= (str (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))
                (str '(-u 2 * ( -u 3 + 5 - ( 2 / 7 ) ))))))))

(deftest test-desambiguar-2
  (testing "desambiguar function with MID$"
    (is (= (str (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))
                (str '(MID$ ( 1 , 2 ))))))))

(deftest test-desambiguar-3
  (testing "desambiguar function with MID3$"
    (is (= (str (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))
                (str '(MID3$ ( 1 , 2 , 3 ))))))))

(deftest test-desambiguar-4
  (testing "desambiguar function with MID3$ and unary minus"
    (is (= (str (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))
                (str '(MID3$ ( 1 , -u 2 + K , 3 ))))))))

;;
;; Pruebas para buscar-lineas-restantes
;;
(deftest test-buscar-lineas-restantes-1
  (testing "buscar-lineas-restantes function with empty program"
    (is (nil? (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))))

(deftest test-buscar-lineas-restantes-2
  (testing "buscar-lineas-restantes function with one line program"
    (is (nil? (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))))

(deftest test-buscar-lineas-restantes-3
  (testing "buscar-lineas-restantes function with multiple lines program"
    (is (= (str (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
                (str '((10 (PRINT X) (PRINT Y)) (15 (X = X + 1)) (20 (NEXT I , J)))))))))

(deftest test-buscar-lineas-restantes-4
  (testing "buscar-lineas-restantes function with multiple lines program and one statement executed"
    (is (= (str (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
                (str '((10 (PRINT Y)) (15 (X = X + 1)) (20 (NEXT I , J)))))))))

(deftest test-buscar-lineas-restantes-5
  (testing "buscar-lineas-restantes function with multiple lines program and all statements executed"
    (is (= (str (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])
                (str '((10) (15 (X = X + 1)) (20 (NEXT I , J)))))))))

(deftest test-buscar-lineas-restantes-6
  (testing "buscar-lineas-restantes function with multiple lines program and one statement executed in the second line"
    (is (= (str (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])
                (str '((15 (X = X + 1)) (20 (NEXT I , J)))))))))

(deftest test-buscar-lineas-restantes-7
  (testing "buscar-lineas-restantes function with multiple lines program and all statements executed in the second line"
    (is (= (str (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])
                (str '((15) (20 (NEXT I , J)))))))))

(deftest test-buscar-lineas-restantes-8
  (testing "buscar-lineas-restantes function with multiple lines program and one statement executed in the third line"
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
           '((20 (NEXT I) (NEXT J)))))))

(deftest test-buscar-lineas-restantes-9
  (testing "buscar-lineas-restantes function with multiple lines program and two statements executed in the third line"
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])
           '((20 (NEXT I) (NEXT J)))))))

(deftest test-buscar-lineas-restantes-10
  (testing "buscar-lineas-restantes function with multiple lines program and all but one statements executed in the third line"
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])
           '((20 (NEXT J)))))))

(deftest test-buscar-lineas-restantes-11
  (testing "buscar-lineas-restantes function with multiple lines program and all statements executed in the third line"
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])
           '((20))))))

(deftest test-buscar-lineas-restantes-12
  (testing "buscar-lineas-restantes function with multiple lines program and more than all statements executed in the third line"
    (is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])
           '((20))))))

(deftest test-buscar-lineas-restantes-13
  (testing "buscar-lineas-restantes function with multiple lines program and non-existent line"
    (is (nil? (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))))

;;
;; Pruebas para continuar-linea
;;
(deftest test-continuar-linea-case-1
  (testing "Testing continuar-linea function - Case 1"
    (let [amb1 [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]
      (is (= (continuar-linea amb1) [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]])))))

(deftest test-continuar-linea-case-2
  (testing "Testing continuar-linea function - Case 2"
    (let [amb2 [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}]]
      (is (= (continuar-linea amb2) [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]])))))


(deftest test-calcular-rpn
  (testing "Testing calcular-rpn function"
    (is (= 0 (calcular-rpn '(3 1 <) [:ejecucion-inmediata 0])))))

(deftest test-calcular-rpn-5
  (testing "Testing calcular-rpn function"
    (is (= 3 (calcular-rpn '("asd" LEN) 10)))))

(deftest test-calcular-exp
  (testing "Testing calcular-expresion LEN"
    (is (= 3 (calcular-expresion '(LEN "ASD") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))))

(deftest test-calcular-dexp
  (testing "Testing calcular-expresion LEN"
    (is (= 3 (calcular-rpn '(1 2 < 1 <> OR 1 INT) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 1}])))))

(deftest test-valor-a-tipo-variable
  (testing "Convert integer to float"
    (is (= 42 (valor-a-tipo-variable "X" 42))))

  (testing "Convert string to float"
    (is (= (float 3.14) (valor-a-tipo-variable "X" "3.14"))))

  (testing "Convert value to string"
    (is (= "42" (valor-a-tipo-variable "X$" 42)))
    (is (= "3.14" (valor-a-tipo-variable "X$" 3.14))))

  (testing "Convert float to integer"
    (is (= 42 (valor-a-tipo-variable "X%" 42.0))))

  (testing "Convert string to integer"
    (is (= 42 (valor-a-tipo-variable "X%" "42"))))

  (testing "Default case"
    (is (= 42 (valor-a-tipo-variable "unknown" 42)))
    (is (= "hello" (valor-a-tipo-variable "unknown" "hello")))))


;(deftest test-parsear-sentencia
;  (testing "Testing parsear-sentencia function"
;    (is (= 0 (parsear-sentencia '(LEN "(" "A" + "(" "a" + "asd" ")" ")"))))))
;
;(deftest test-parsear-sentencia2
;  (testing "Testing parsear-sentencia function"
;    (is (= 0 (apply concat (parsear-sentencia '(PRINT LEN "(" "asd" ")")))))))
;
;(deftest test-parsear-sentencia2
;  (testing "Testing parsear-sentencia function"
;    (is (= 0 (resolver-subfunciones '(PRINT LEN "(" "abc" ")") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))))
;
;(deftest test-parsear-sdtencia2
;  (testing "Testing parsear-sentencia function"
;    (is (= 0 (calcular-expresion '("asd") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))))