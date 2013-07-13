(ns oberon.core-test
  (:require [clojure.test :refer :all]
            [oberon.core :refer :all]))

(deftest binomial-nth-p0-test
  (testing "If P = 0, take the last."
    (is (= 0 (binomial-nth 0 [1 1 1 1 1 0])))))

(deftest binomial-nth-p1-test
  (testing "If P = 1, take the first"
    (is (= 0 (binomial-nth 0 [0 1 1 1 1 1])))))
