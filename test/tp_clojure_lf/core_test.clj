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
(deftest test-variable-float?-con-variable-float
  (testing "Probando la funcion variable-float? con una variable float"
    (is (variable-float? 'X)) ; debería retornar verdadero
    ))

(deftest test-variable-float?-con-variable-integer
  (testing "Probando la función variable-float? con una variable integer"
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