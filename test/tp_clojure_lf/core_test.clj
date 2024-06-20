(ns tp-clojure-lf.core-test
  (:require [clojure.test :refer :all]
            [tp-clojure-lf.core :refer :all]))

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