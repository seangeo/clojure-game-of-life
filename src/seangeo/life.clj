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
    (if (and 
          (alive? automaton my-world) 
          (enough-to-survive? live-neighbours)) 
      #{automaton} 
      #{})))

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
  '(javax.swing JFrame JPanel SwingUtilities)
  '(java.awt Dimension Color))

(def title "Game of Life")
(def height 300)
(def width  400)
(def automaton-height 10)
(def automaton-width  10)
(def grid-height (/ height automaton-height))
(def grid-width  (/ width  automaton-width))
(def animation-sleep-ms 150)

(defmacro in-swing [fn]
  `(let [runnable# (proxy [Runnable] []
                     (run [] ~fn))]
     (SwingUtilities/invokeLater runnable#)))

(defn render-automaton
  [graphics automaton]
  (let 
    [x (* (:x automaton) automaton-width)
     y (* (:y automaton) automaton-height)]
    (.fillRect graphics x y automaton-width automaton-height)))

(defn clear-screen [graphics]
  (doto graphics
    (.setColor Color/WHITE)
    (.fillRect 0 0 width height)))

(defn render-world
  [graphics the-world]
  (clear-screen graphics)
  (.setColor graphics Color/BLACK)
  (dorun
    (for [automaton the-world]
      (render-automaton graphics automaton))))

(defn render [panel the-world]
  (in-swing (render-world (.getGraphics panel) the-world))
  panel)

(defn update-world [my-world renderer]
  (send-off *agent* update-world renderer)
  (let [new-world (evolve-world my-world grid-width grid-height)]
    (send-off renderer render new-world)
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
    (let [renderer (agent panel)]
      (send-off renderer render world)
      (send-off world-updater update-world renderer))))

(defn -main
  [& args]
  (run-ui (world [[2 1] [2 3] [1 3] [3 2] [3 3]])))

