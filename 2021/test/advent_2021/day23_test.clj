(ns advent-2021.day23-test
  (:require [advent-2021.day23 :as sut]
            [clojure.test :as t]))

(def hallway [:empty :empty
              [:A :B :A]
              :empty
              [:B :C :D]
              :empty
              [:C :B :C]
              :empty
              [:D :D :A]
              :empty :empty])

(t/deftest room-nodes
  (t/testing "already filled"
    (t/is (= (sut/room-nodes [:A :A :A]) nil)))
  (t/testing "filled and empty"
    (t/is (= (sut/room-nodes [:A :empty :A]) nil)))
  (t/testing "found at second pos"
    (t/is (= (sut/room-nodes [:A :empty :B]) [2 :B])))
  (t/testing "empty"
    (t/is (= (sut/room-nodes [:A :empty :empty]) nil)))
  (t/testing "blocked wrong one"
    (t/is (= (sut/room-nodes [:A :A :B]) [1 :A])))
  (t/testing "two wrong ones"
    (t/is (= (sut/room-nodes [:A :C :B]) [1 :C])))
  (t/testing "Large room"
    (t/is (= (sut/room-nodes [:A :B :D :D :A]) [1 :B])))
  (t/testing "Large room"
    (t/is (= (sut/room-nodes [:A :empty :empty :D :A]) [3 :D]))))


(sut/nodes hallway)
(sut/nodes [:empty :B [:A :empty :A] :empty [:B :C :D] :empty [:C :B :C] :empty [:D :D :A] :empty :empty])
(sut/nodes [:empty :B [ :A :empty :A ] :C [ :B :empty :empty ] :D [ :C :B :C ] :empty [ :D :D :A ] :empty :empty ])

(sut/edges
 {:pos [0] :amph :D}
 [:D :empty [:A :B :D :D :A] :empty [:B :C :C :B :D] :empty [:C :B :B :A :C] :empty [:D :empty :empty :C :A] :empty :A])
(sut/edges {:pos [4 1] :amph :C}
       [:empty :empty [:A :B :A] :B [:B :C :D] :empty [:C :empty :C] :empty [:D :D :A] :empty :empty])
;; => ([[6 1] 400])
(sut/edges {:pos [1] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :empty [:C :B :C] :empty [:D :D :A] :empty :empty])
;; => () room is full
(sut/edges {:pos [1] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :empty [:C :empty :C] :B [:D :D :A] :empty :empty])
;; => ([[6 1] 600])
(sut/edges {:pos [1] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :empty [:C :empty :empty] :C [:D :D :A] :B :empty])
;; => ([[6 2] 700])
(sut/edges {:pos [1] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :B [:C :empty :empty] :C [:D :D :A] :empty :empty])
;; => () blocked by :B
(sut/edges {:pos [9] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :B [:C :empty :empty] :D [:D :empty :A] :C :empty])
;; => () blocked by :D
(sut/edges {:pos [9] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :B [:C :empty :empty] :empty [:D :D :A] :C :empty])
;; => ([[6 2] 500])
(sut/edges {:pos [8 1] :amph :C}
       [:empty :C [:A :B :A] :empty [:B :empty :D] :B [:C :empty :empty] :empty [:D :C :A] :D :empty])
;; => ([[6 2] 500])
(sut/edges {:pos [8 1] :amph :C}
       [:empty :C [:A :B :A] :B [:B :empty :D] :empty [:C :empty :empty] :empty [:D :C :A] :empty :D])
;; => ([[6 2] 500])
(sut/edges {:pos [8 1] :amph :C}
       [:empty :C [:A :empty :empty] :B [:B :empty :D] :empty [:C :B :A] :empty [:D :C :A] :empty :D])
;; => ([[7] 200] [[5] 400] [[9] 200])

(sut/edges {:pos [8 1] :amph :C}
       [:empty :C [:A :empty :empty] :B [:B :empty :D] :empty [:C :empty :empty :empty :empty] :empty [:D :C :A] :empty :D])


