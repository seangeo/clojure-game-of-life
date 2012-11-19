(ns seangeo.life-test
  (:use clojure.test
        seangeo.life))

(deftest world-builder
  (testing "The builder of the worlds"
    (is (=
          (set [(->Automaton 1 1)])
          (world [[1 1]])))))

(deftest find-neighbours
  (testing "Finding neighbour cells"
    (is (= 
          (set
            [[0 0] [1 0] [2 0]
             [0 1]       [2 1]
             [0 2] [1 2] [2 2]])
          (set (neighbours (->Automaton 1 1)))))))

(deftest find-neighbours-edge-1
  (testing "Finding neighbour cells on the edge of the screen"
    (is (= 
          (set
            [[0 0] [1 0] [2 0]
             [0 1]       [2 1]
             [0 2] [1 2] [2 2]])
          (set (neighbours (->Automaton 0 0)))))))

(deftest test-survival-1
   (testing "Automatons should survive if they have 3 neighbours")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{automaton} (living-automaton automaton true 3)))))

(deftest test-survival-2
   (testing "Automatons should survive if they have 2 neighbours")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{automaton } (living-automaton automaton true 2)))))

(deftest test-birth-1-when-dead
   (testing "Dead automatons should comeback if they have 3 neighbours")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{automaton} (living-automaton automaton false 3)))))

(deftest test-survival-2-when-dead
   (testing "Dead automatons shouldn't come back to life if they have 2 neighbours, this is handled by birthing")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{} (living-automaton automaton false 2)))))

(deftest test-survival-3
   (testing "Automatons should die if they have 4 neighbours")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{} (living-automaton automaton true 4)))))

(deftest test-survival-4
   (testing "Automatons should die if they have 1 neighbours")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{} (living-automaton automaton true 1)))))

(deftest test-survival-5
   (testing "Automatons should die if they have 0 neighbours")
      (let
        [automaton (->Automaton 1 1)]
        (is (= #{} (living-automaton automaton true 0)))))

(deftest test-evolve-world
   (testing "Test that the correct next frame is produced"
      (let
        [my-world (world [[1 0] [1 1] [1 2]])
         expected-world (world [[0 1] [1 1] [2 1]])
         width    4
         height   4]
        (is (= expected-world (evolve-world my-world width height))))))

