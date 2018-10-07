(ns game.core
(:require [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(defn get-next-direction-gravity [[x y]]
    [x (max -10 (- y 0.1))])

(defn get-circle-game-object
    ([] (get-circle-game-object [100 100] [-1 -1] 20 [100 255 255]))
    ([position direction radius] (get-circle-game-object [100 255 255]))
    ([position direction radius color]
    {
        :position position
        :direction direction
        :radius radius
        :color color
        :get-next-direction get-next-direction-gravity
        :id (gensym)
    }))

(defn get-initial-state []
    [
    (get-circle-game-object [100 100] [-1 -1] 20 [100 255 255])
    (get-circle-game-object [300 100] [1 -1] 20 [100 255 255])
    ])
    
(defn square [x] (* x x))

(defn dot-product [& vectors]
    (apply + (apply (partial map *) vectors)))

(defn scalar-vector-multiplication [s v]
    (map (partial * s) v))

(defn vector-plus [& vectors]
    (apply (partial map +) vectors))

(defn vector-minus [& vectors]
    (apply (partial map -) vectors))

(defn invert-y [[x y]]
    [x (* -1 y)])    

(def magnitude
    (comp (fn [x] (Math/sqrt x)) (partial apply +) (partial map square)))

(defn get-color-by-vector [[x y]]
    [(mod x 256) (mod y 256) 255])

(defn normal [p1 p2]
    (let
        [v (vector-minus p1 p2)
        s (/ 1 (Math/sqrt (apply + (map square v))))]
        (scalar-vector-multiplication s v)))

(defn collision? [& circles]
    (<=
        (apply + (apply (partial map (comp square -)) (map :position circles)))
        (apply (comp square +) (map :radius circles))))

(defn wrap-edges [position]
    (map (fn [p] (mod p 500)) position))

(defn bounce-edges [[px py] [dx dy] radius]
    (let [max-x (- 500 radius) max-y (- 500 radius)
    min-x radius min-y radius]
    [
        (if (or (<= px min-x) (>= px max-x))
            (* -1 dx)
            dx)
        (if (or (<= py min-y) (>= py max-y))
            (* -1 dy)
            dy)
            ]))
    

(defn move [position direction radius]
    (map
        (comp (partial min (- 500 radius)) (partial max radius) +)
        position direction))



(defn collide [p1 p2 v1 v2]
    (let
        [n (normal p1 p2)
        negative-n (scalar-vector-multiplication -1 n)
        vn1 (scalar-vector-multiplication
            (dot-product v1 negative-n) negative-n)
        vn2 (scalar-vector-multiplication
            (dot-product v2 n) n)
        vt1 (vector-minus vn1 v1)
        vt2 (vector-minus vn2 v2)]
        { :v1-next (invert-y (vector-plus vt1 vn2))
            :v2-next (vector-plus vt2 vn1) }))

(defn get-collision-object [game-objects game-object]
    (some
        (fn [other-game-object]
            (and
                (not= (:id other-game-object) (:id game-object))
                (collision? game-object other-game-object)
                other-game-object))
        game-objects))

(defn collision-check [game-objects game-object]
    (if
        (get-collision-object game-objects game-object)
        (println "collision!!!")
        (println "safe!!!")))

(defn get-direction-after-collision [game-objects game-object]
    (let
        [collision-object (get-collision-object game-objects game-object)]
        (if collision-object
            (:v1-next (collide 
                (:position game-object)
                (:position collision-object)
                (:direction game-object)
                (:direction collision-object)))
            (:direction game-object))))

(defn update-game-object [game-objects game-object]
    (let
        [direction-after-collision (get-direction-after-collision game-objects game-object)
        new-direction
            (get-next-direction-gravity
                (bounce-edges (:position game-object) direction-after-collision (:radius game-object)))
        new-position (move (:position game-object) new-direction (:radius game-object))
        ]
    (merge
        game-object
        {
            :position new-position
            :direction new-direction
            :color (get-color-by-vector new-position)
        })))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
  (q/color-mode :hsb)
  (get-initial-state))

(defn update-state [state]
;(dorun (map (partial collision-check state) state))
    (let [without-small-magnitue (filter (comp (partial < 0.5) magnitude :direction) state)]
    (map (partial update-game-object without-small-magnitue) without-small-magnitue)))

(defn draw-circle [[x y] radius color]
    (let [draw-radius (* 2 radius)]
        (q/with-translation [0 0])
        (apply q/fill color)
        (q/ellipse x (- 500 y) draw-radius draw-radius)))
    
(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (dorun
    (map
        (fn [circle]
            (apply draw-circle (map circle [:position :radius :color])))
        state)))

(defn -main [& args]
  (q/defsketch q2
    :title "Single bouncing ball"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
