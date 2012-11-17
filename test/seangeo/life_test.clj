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

(deftest test-survival-1
   (testing "Automatons should survive if they have 3 neighbours")
      (let
        [automaton '(1 1)
         my-world (world [[0 1] [1 0] [2 0] [1 1]])]
        (is (= #{(->Automaton 1 1)} (survivor automaton my-world)))))

(deftest test-survival-2
   (testing "Automatons should survive if they have 2 neighbours")
      (let
        [automaton '(1 1)
         my-world (world [[0 1] [2 0] [1 1]])]
        (is (= #{(->Automaton 1 1)} (survivor automaton my-world)))))

(deftest test-survival-1-when-dead
   (testing "Dead automatons shouldn't comeback if they have 3 neighbours")
      (let
        [automaton '(1 1)
         my-world (world [[0 1] [1 0] [2 0]])]
        (is (= #{} (survivor automaton my-world)))))

(deftest test-survival-2-when-dead
   (testing "Dead automatons shouldn't come back to life if they have 2 neighbours, this is handled by birthing")
      (let
        [automaton '(1 1)
         my-world (world [[0 1] [2 0]])]
        (is (= #{} (survivor automaton my-world)))))
(deftest test-survival-3
   (testing "Automatons should die if they have 4 neighbours")
      (let
        [automaton '(1 1)
         my-world (world [[0 0] [0 1] [0 2] [1 2] [1 1]])]
        (is (= #{} (survivor automaton my-world)))))

(deftest test-survival-4
   (testing "Automatons should die if they have 1 neighbours")
      (let
        [automaton '(1 1)
         my-world (world [[0 0] [1 1]])]
        (is (= #{} (survivor automaton my-world)))))

(deftest test-survival-5
   (testing "Automatons should die if they have 0 neighbours")
      (let
        [automaton '(1 1)
         my-world (world [[1 1]])]
        (is (= #{} (survivor automaton my-world)))))

(deftest test-birth-1
   (testing "Automatons should be born if there are 3 neighbours")
      (let
        [cell '(1 1)
         my-world (world [[0 0] [1 0] [1 2]])]
        (is (= #{(->Automaton 1 1)} (birth cell my-world)))))

(deftest test-birth-2
   (testing "Automatons should not be born if there is 2 neighbours")
      (let
        [cell '(1 1)
         my-world (world [[1 0] [1 2]])]
        (is (= #{} (birth cell my-world)))))

(deftest test-birth-3
   (testing "Automatons should not be born if there is 4 neighbours")
      (let
        [cell '(1 1)
         my-world (world [[1 0] [0 1] [0 2] [2 2]])]
        (is (= #{} (birth cell my-world)))))

(deftest test-evolve-world
   (testing "Test that the correct next frame is produced"
      (let
        [my-world (world [[1 0] [1 1] [1 2]])
         expected-world (world [[0 1] [1 1] [2 1]])
         width    4
         height   4]
        (is (= expected-world (evolve-world my-world width height))))))

