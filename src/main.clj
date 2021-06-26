(ns main
  (:require
   [clojure.string :as str])
  (:import
   [com.googlecode.lanterna
    TextCharacter
    TerminalPosition
    TextColor$ANSI
    screen.Screen$RefreshType
    input.KeyType
    terminal.DefaultTerminalFactory]))

(defn current-line [state]
  (get-in state [:text (-> state :cursor :y)]))

(defn set-cursor-col-from-x [state]
  (assoc-in state [:cursor :col] (-> state :cursor :x)))

(defn move-left [state]
  (if (pos? (-> state :cursor :x))
    (-> state
        (update-in [:cursor :x] dec)
        set-cursor-col-from-x)
    state))

(defn move-right [state]
  (if (< (-> state :cursor :x inc) (-> state current-line count))
    (-> state
        (update-in [:cursor :x] inc)
        set-cursor-col-from-x)
    state))

(defn clamp [x low high]
  (max low (min x high)))

(defn set-cursor-x-from-col [state]
  (assoc-in state [:cursor :x] (clamp (-> state :cursor :col)
                                      0 (-> state current-line count dec))))

(defn move-up [state]
  (if (pos? (-> state :cursor :y))
    (-> state
        (update-in [:cursor :y] dec)
        set-cursor-x-from-col)
    state))

(defn move-down [state]
  (if (< (-> state :cursor :y inc) (-> state :text count))
    (-> state
        (update-in [:cursor :y] inc)
        set-cursor-x-from-col)
    state))

(defn anchor [state]
  (assoc state :anchor (:cursor state)))

(defn handle-input [state input]
  (if input
    (condp = (.getKeyType input)
      KeyType/Character (case (.getCharacter input)
                          \h (-> state move-left anchor)
                          \j (-> state move-down anchor)
                          \k (-> state move-up anchor)
                          \l (-> state move-right anchor)
                          \H (-> state move-left)
                          \J (-> state move-down)
                          \K (-> state move-up)
                          \L (-> state move-right))
      state)
    state))

(defn selection [state]
  (sort-by (juxt :y :x) [(:cursor state) (:anchor state)]))

(defn in-selection? [state x y]
  (let [[from to] (selection state)]
    (and (or (< (:y from) y) (and (= (:y from) y) (<= (:x from) x)))
         (or (< y (:y to)) (and (= y (:y to)) (<= x (:x to)))))))

(defn draw-editor [state screen w h]
  (let [y-offset (clamp (-> state :cursor :y (- (int (/ h 2))))
                        0 (-> state :text count (- h)))
        text (->> state :text (drop y-offset) (take h) (map #(take w %)))]
    (doseq [[y line] (zipmap (range) text)]
      (doseq [[x char] (zipmap (range) (if (seq line) line " "))]
        (.setCharacter screen
                       x y
                       (cond-> (TextCharacter. char)
                         (in-selection? state x (+ y y-offset))
                         (.withBackgroundColor TextColor$ANSI/WHITE)))))
    (.setCursorPosition screen (TerminalPosition.
                                (-> state :cursor :x)
                                (-> state :cursor :y (- y-offset))))))

(defn pad-between [left right w]
  (str left (apply str (repeat (- w (count left) (count right)) \space)) right))

(defn draw-status [state screen w y]
  (doseq [[x char] (zipmap (range)
                           (pad-between (:file-path state)
                                        (str (-> state :cursor :x inc)
                                             ","
                                             (-> state :cursor :y inc))
                                        w))]
    (.setCharacter screen
                   x y
                   (-> (TextCharacter. char)
                     (.withBackgroundColor TextColor$ANSI/WHITE)))))

(defn draw [state screen]
  (let [resize? (.doResizeIfNecessary screen)
        size (or resize? (.getTerminalSize screen))
        w (.getColumns size)
        h (.getRows size)]
    (.clear screen)
    (draw-editor state screen w (- h 2))
    (draw-status state screen w (- h 2))
    (.refresh screen (if resize?
                       Screen$RefreshType/COMPLETE
                       Screen$RefreshType/DELTA))))

(defn initial-state [file-path]
  {:file-path file-path
   :text (str/split-lines (slurp file-path))
   :cursor {:x 0 :y 0 :col 0}
   :anchor {:x 0 :y 0 :col 0}
   :mode :normal})

(defn main [_]
  (with-open [screen (.createScreen (DefaultTerminalFactory.))]
    (.startScreen screen)
    (loop [state (initial-state (last *command-line-args*))]
      (draw state screen)
      (recur (handle-input state (.readInput screen))))))
