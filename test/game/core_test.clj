(ns game.core-test
  (:require [clojure.test :refer :all]
            [game.core :refer :all]))

    
(deftest move-test
    (testing "move"
        (is (= (move [0 0] [0 0]) [0 0]))
        (is (= (move [1 1] [0 0]) [1 1]))
        (is (= (move [1 2] [1 2]) [2 4]))
        ))

(deftest update-game-object-test
    (testing "update-game-object"
        (is
            (=
                (update-game-object
                    {
                        :position [100 100]
                        :direction [1 2]
                        :radius 1
                        :get-next-direction get-next-direction-gravity
                    })
                    {
                        :position [101 102.1]
                        :direction [1 2.1]
                        :radius 1
                        :get-next-direction get-next-direction-gravity
                    }))))

(deftest collision?-test
    (testing "collision?"
        (is
            (=
                (collision?
                    { :position [1 1] :radius 1 }
                    { :position [1 0] :radius 1 })
                true))
        (is
            (=
                (collision?
                    { :position [1 1] :radius 1 }
                    { :position [1 4] :radius 1 })
                false))
                
                
                ))
