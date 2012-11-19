(ns seangeo.life
  (:gen-class)
  (use clojure.set))

(defrecord Automaton [x y])

(defn world
  [live-cells]
  (set (map #(apply ->Automaton %1) live-cells)))

(defn neighbours
  [automaton width height]
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

(defn enough-to-survive?
  [live-neighbours]
  (or (= 2 live-neighbours) (= 3 live-neighbours)))

(defn living-automaton 
  [automaton alive? number-of-neighbours]
  (if (or 
        (and alive? (enough-to-survive? number-of-neighbours))
        (and (not alive?) (= number-of-neighbours 3)))
    #{automaton}
    #{}))

(defn world-cells [width height]
  (for [x (range width) y (range height)] [x y]))

(defn evolve-cell [old-world width height new-world cell]
  (let 
    [automaton (apply ->Automaton cell)
     currently-alive (alive? automaton old-world)
     cell-neighbours (world (neighbours automaton width height))
     live-neighbours (count (intersection old-world cell-neighbours))]
  (union new-world (living-automaton automaton currently-alive live-neighbours))))

(defn evolve-world
  [current-world width height]
  (reduce (partial evolve-cell current-world width height) #{} (world-cells width height)))

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

(def die-hard [[6 7] [7 7] [7 8] [12 6] [11 8] [12 8] [13 8]])
(def acorn [[12 11] [11 13] [12 13] [14 12] [15 13] [16 13] [17 13]])
(def r-pentomino [[21 22] [22 21] [22 22] [22 23] [23 21]])

(defn -main
  [& args]
  (run-ui (world r-pentomino)))

