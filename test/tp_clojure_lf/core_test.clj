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
    (is (= (eliminar-cero-entero 'A) "A"))))

(deftest test-eliminar-cero-entero-con-cero
  (testing "Prueba eliminar-cero-entero con cero"
    (is (= (eliminar-cero-entero 0) "0"))))

(deftest test-eliminar-cero-entero-con-multiples-ceros
  (testing "Prueba eliminar-cero-entero con multiples ceros"
    (is (= (eliminar-cero-entero 000) "0"))))

(deftest test-eliminar-cero-entero-con-decimal
  (testing "Prueba eliminar-cero-entero con un número decimal"
    (is (= (eliminar-cero-entero 1.5) "1.5"))))

(deftest test-eliminar-cero-entero-con-entero
  (testing "Prueba eliminar-cero-entero con un número entero"
    (is (= (eliminar-cero-entero 1) "1"))))

(deftest test-eliminar-cero-entero-con-entero-con-cero-adelante
  (testing "Prueba eliminar-cero-entero con un número entero con cero adelante"
    (is (= (eliminar-cero-entero 01) "1"))))

(deftest test-eliminar-cero-entero-con-negativo
  (testing "Prueba eliminar-cero-entero con un número negativo"
    (is (= (eliminar-cero-entero -1) "-1"))))

(deftest test-eliminar-cero-entero-con-negativo-decimal
  (testing "Prueba eliminar-cero-entero con un número decimal negativo"
    (is (= (eliminar-cero-entero -1.5) "-1.5"))))

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
    (is (= (dar-error 16 [:ejecucion-inmediata 4]) "?SYNTAX ERRORnil"))))

(deftest test-dar-error-2
  (testing "funcion dar-error ?ERROR DISK FULL"
    (is (= (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]) "?ERROR DISK FULLnil"))))

(deftest test-dar-error-3
  (testing "funcion dar-error 16 en 100 3"
    (is (= (dar-error 16 [100 3]) "?SYNTAX ERROR IN 100nil"))))

(deftest test-dar-error-4
  (testing "funcion dar-error ?ERROR DISK FULL en 100 3"
    (is (= (dar-error "?ERROR DISK FULL" [100 3]) "?ERROR DISK FULL IN 100nil"))))

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