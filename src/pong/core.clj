(ns pong.core
  (:use quil.core))

; Constants ----

(def HEIGHT 300)
(def WIDTH 800) 
(def BALL-W 4)
(def BALL-H 4)
(def PADDLE-W 10)
(def PADDLE-H 80)

; Helpers ----

(defn map->rect [m]
  (apply rect
    ((juxt :x :y :w :h) m)))

(defn rand+- []
  (get [+ -]  (rand-int 2)))

; Keycodes ----

(def Q 65)
(def A 81)
(def P 76)
(def L 80)

; Paddles ----

(defn make-paddle [x y w h up dn]
  {:x x :y y :w w :h h :up up :dn dn})

(def initial-paddle1
  (make-paddle 0 (/ (- HEIGHT PADDLE-H) 2)
               PADDLE-W PADDLE-H
               Q A))

(def initial-paddle2
  (make-paddle (- WIDTH PADDLE-W) (/ (- HEIGHT PADDLE-H) 2)
               PADDLE-W PADDLE-H
               P L))

(defn draw-paddle [paddle]
  (fill 200)
  (map->rect paddle))

(defn update-paddle [paddle]
  (make-paddle
    (:x paddle)
    (+ (:y paddle)
      (if ; up key pressed
        (and 
          (@pressed (:up paddle))
          (> HEIGHT (+ (:y paddle) (:h paddle)))) 
      3 0)
      (if ; down key pressed
        (and 
          (@pressed (:dn paddle))
          (< 0 (:y paddle))) 
      -3 0))
    (:w paddle) (:h paddle)
    (:up paddle) (:dn paddle)))

; Ball ----

(defn make-ball [x y w h sx sy]
  {:x x :y y 
   :w w :h h
   :sx sx :sy sy})

(defn initial-ball []
  (make-ball 
    (- (/ WIDTH 2) (/ BALL-W 2))
    (- (/ HEIGHT 2) (/ BALL-H 2))
    BALL-W BALL-H
    ((rand+-) 3) ((rand+-) 3)))

(defn collision? [ball paddle]
  (and
    (> (:x ball) (:x paddle))
    (< (+ (:x ball) (:w ball)) (+ (:x paddle) (:w paddle)))
    (> (:y ball) (:y paddle))
    (< (:y ball) (+ (:y paddle) (:h paddle)))))

(defn hit-edge? [ball]
  (or
    (> 0 (:y ball))
    (< HEIGHT (+ (:y ball) (:h ball)))))

(defn out-of-bounds? [ball]
  (or 
    (> 0 (+ (:x ball) (:w ball)))
    (< WIDTH (:x ball))))

(defn update-ball [world]
  (let [ball (:ball world)
        paddle1 (:paddle1 world)
        paddle2 (:paddle2 world)
        newsx (if (or 
                    (collision? ball paddle1)
                    (collision? ball paddle2))
                (* -1.02 (:sx ball)) (:sx ball))
        newsy (if (hit-edge? ball)     
                (- (:sy ball)) (:sy ball))
        newx (+ (:x ball) newsx)
        newy (+ (:y ball) newsy)]
    (make-ball 
      newx newy
      (:w ball) (:h ball)
      newsx newsy)))

(defn draw-ball [ball]
  (fill 200)
  (map->rect ball))

; World ----

(defn make-world [ball paddle1 paddle2]
  {:ball ball :paddle1 paddle1 :paddle2 paddle2})

(defn initial-world []
  (make-world 
    (initial-ball)
    initial-paddle1
    initial-paddle2))

(defn end-game? [world]
  (out-of-bounds? (:ball world)))

(defn update-world [world]              
  (make-world
    (if (end-game? world)
      (initial-ball)
      (update-ball world))
    (update-paddle (:paddle1 world))
    (update-paddle (:paddle2 world))))

(defn draw-world [world]
  (draw-ball (:ball world))
  (draw-paddle (:paddle1 world))
  (draw-paddle (:paddle2 world)))

; Draw ---- 

(defn draw-background []
  (no-stroke)
  (fill 0)
  (rect 0 0 WIDTH HEIGHT))

(def game-states (iterate update-world (initial-world)))

(defn draw []
  (draw-background)
  (draw-world (nth game-states (frame-count))))

; Key Handling ----

(def pressed (atom #{}))

(defn key-pressed []
  (swap! pressed conj (key-code)))

(defn key-released []
  (swap! pressed disj (key-code)))

; Setup ----

(defn setup []
  (background 255)
  (smooth))

(defn go []
  (defsketch pong
    :title "Pong"
    :key-pressed key-pressed
    :key-released key-released
    :setup setup
    :draw draw
    :size [WIDTH HEIGHT]))

(defn -main [& args]
  (go))
