(ns seangeo.life
  (:gen-class)
  (use clojure.set))

(defrecord Automaton [x y])

(defn world
  [live-cells]
  (set (map #(apply ->Automaton %1) live-cells)))

(defn neighbours
  [automaton]
  (let
    [cx (:x automaton)
     cy (:y automaton)
     xs [(- cx 1) cx (+ cx 1)]
     ys [(- cy 1) cy (+ cy 1)]]
    (for [x xs y ys 
          :when (not (and (= x cx) (= y cy)))] 
      [x y])))

(defn alive?
  [automaton my-world]
  (contains? my-world automaton))

(defn dead?
  [automaton my-world]
  (not (contains? my-world automaton)))

(defn enough-to-survive?
  [live-neighbours]
  (or (= 2 live-neighbours) (= 3 live-neighbours)))

(defn survivor
  [cell my-world]
  (let
    [automaton     (apply ->Automaton cell)
     my-neighbours (world (neighbours automaton))
     live-neighbours (count (intersection my-world my-neighbours))]
    (if (and (alive? automaton my-world) (enough-to-survive? live-neighbours)) #{automaton} #{})))

(defn birth
  [cell my-world]
  (let
    [potential-life (apply ->Automaton cell)
     my-neighbours (world (neighbours potential-life))
     potential-parents (count (intersection my-world my-neighbours))]
     (if (and (= potential-parents 3) (dead? potential-life my-world)) 
       #{potential-life} #{})))

(defn world-cells [width height]
  (for [x (range width) y (range height)] [x y]))

(defn evolve-cell [old-world new-world cell]
  (union new-world (birth cell old-world) (survivor cell old-world)))

(defn evolve-world
  [current-world width height]
  (reduce (partial evolve-cell current-world) #{} (world-cells width height)))

;;;;;;;;;;;; UI!
(import 
  '(javax.swing JFrame JPanel)
  '(java.awt Dimension Color))

(def title "Game of Life")
(def height 300)
(def width  400)
(def grid-height (/ height 5))
(def grid-width  (/ width  5))
(def animation-sleep-ms 500)

(defn render
  [graphics world]
  (doto graphics
    (.setColor Color/BLACK)
    (.fillRect 10 10 10 10))
  graphics)

(defn update-world [my-world renderer]
  (send-off *agent* update-world renderer)
  (let [new-world (evolve-world my-world grid-width grid-height)]
    (send-off renderer render new-world)
    (println new-world)
    (. Thread (sleep animation-sleep-ms))
    new-world))

(defn- error [a e]
  (println e)
  (.printStackTrace e))

(defn run-ui
  [world]
  (let [world-updater (agent world :error-handler error)
        frame (JFrame. title)
        panel (JPanel. true)]
    (doto frame
      (.setSize (Dimension. width height))
      (.setContentPane panel)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (let [renderer (agent (.getGraphics panel))]
      (send-off renderer render world)
      (send-off world-updater update-world renderer))))

(defn -main
  [& args]
  (run-ui (world [[1 1] [1 0] [1 2]])))

