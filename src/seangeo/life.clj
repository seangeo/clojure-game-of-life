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
  '(javax.swing JFrame JPanel SwingUtilities JButton)
  '(java.awt Dimension Color BorderLayout)
  '(java.awt.event ActionListener))

(def title "Game of Life")
(def height 600)
(def width  800)
(def automaton-height 10)
(def automaton-width  10)
(def grid-height (/ height automaton-height))
(def grid-width  (/ width  automaton-width))
(def animation-sleep-ms 0)
(defrecord WorldUpdater [world renderer running])

(defmacro in-swing [fn]
  `(let [runnable# (proxy [Runnable] []
                     (run [] ~fn))]
     (SwingUtilities/invokeLater runnable#)))

(defmacro actionlistener [fn]
  `(proxy [ActionListener] []
     (actionPerformed [evt#] ~fn)))

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
  (in-swing (render-world (.getGraphics panel) the-world)) panel)

(defn update-world [world-updater]
  (let [old-world (:world world-updater)
        renderer  (:renderer world-updater)
        running   (:running world-updater)
        new-world (time (evolve-world old-world grid-width grid-height))]
    (when running (send-off *agent* update-world))
    (send-off renderer render new-world)
    (. Thread (sleep animation-sleep-ms))
    (->WorldUpdater new-world renderer running)))

(defn- pause [world-updater]
  (let [running (not (:running world-updater))]
    (when running (send-off *agent* update-world))
    (assoc world-updater :running running)))

(defn- error [a e]
  (println e)
  (.printStackTrace e))

(defn pause-button [world-updater]
  (let [pause-button (JButton. "Pause/Play")]
    (.addActionListener 
      pause-button
      (actionlistener (send-off world-updater pause)))
    pause-button))

(defn run-ui [world]
  (let [frame        (JFrame. title)
        surface      (JPanel. true)
        renderer     (agent surface)
        button-panel (JPanel. true)
        world-updater (->WorldUpdater world renderer true)
        world-updater-agent (agent world-updater :error-handler error)]
    (doto button-panel
      (.add (pause-button world-updater-agent)))
    (doto frame
      (.setSize (Dimension. width height))
      (.add button-panel BorderLayout/NORTH)
      (.add surface BorderLayout/CENTER)
      (.setVisible true)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
    (send-off renderer render world)
    (send-off world-updater-agent update-world)))

(defn -main
  [& args]
  (run-ui (world [[2 1] [2 3] [1 3] [3 2] [3 3]])))

