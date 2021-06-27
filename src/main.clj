(ns main
  (:require
   [clojure.string :as str])
  (:import
   [java.awt Toolkit datatransfer.StringSelection]
   [com.googlecode.lanterna
    TextCharacter
    TerminalPosition
    TextColor$ANSI
    screen.Screen$RefreshType
    input.KeyType
    terminal.DefaultTerminalFactory]))

(defn set-cursor-col-from-x [{{x :x} :cursor :as state}]
  (assoc-in state [:cursor :col] x))

(defn move-left [{{x :x} :cursor :as state}]
  (if (pos? x)
    (-> state
        (update-in [:cursor :x] dec)
        set-cursor-col-from-x)
    state))

(defn move-right [{text :text {x :x y :y} :cursor :as state}]
  (if (<= (inc x) (count (text y)))
    (-> state
        (update-in [:cursor :x] inc)
        set-cursor-col-from-x)
    state))

(defn clamp [x low high]
  (max low (min x high)))

(defn set-cursor-x-from-col [{text :text {col :col y :y} :cursor :as state}]
  (assoc-in state [:cursor :x] (clamp col 0 (count (text y)))))

(defn move-up [{{y :y} :cursor :as state}]
  (if (pos? y)
    (-> state
        (update-in [:cursor :y] dec)
        set-cursor-x-from-col)
    state))

(defn move-down [{text :text {y :y} :cursor :as state}]
  (if (< (inc y) (count text))
    (-> state
        (update-in [:cursor :y] inc)
        set-cursor-x-from-col)
    state))

(defn selection [{:keys [cursor anchor]}]
  (sort-by (juxt :y :x) [cursor anchor]))

(defn delete [state]
  (let [[from to] (selection state)]
    (-> state
        (update
         :text
        ;; Lot's of unnecessary copying here. If this turns out to be too slow,
        ;; or use too much memory, we can try catvec from
        ;; https://github.com/clojure/core.rrb-vector
         #(vec (concat (subvec % 0 (:y from))
                       [(str (subs (% (:y from)) 0 (:x from))
                             (subs (% (:y to)) (:x to)))]
                       (subvec % (inc (:y to))))))
        (assoc :cursor from :anchor from))))

(defn set-anchor-from-cursor [state]
  (assoc state :anchor (:cursor state)))

(defn delete-lines [{text :text :as state}]
  (let [[from to] (map :y (selection state))]
    (-> state
        (update :text #(vec (concat (subvec % 0 from) (subvec % (inc to)))))
        (assoc-in [:cursor :y] (if (= to (dec (count text)))
                                 (dec from)
                                 from))
        set-cursor-x-from-col
        set-anchor-from-cursor)))

(defn set-cursor-style [n]
    ;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
    (println (str "\033[" n " q")))

(defn start-insert [state]
  (set-cursor-style 3)
  (-> state
      delete
      (assoc :mode :insert)))

(defn stop-insert [state]
  (set-cursor-style 1)
  (assoc state :mode :normal))

(defn insert [{{x :x y :y} :cursor :as state} char]
  (-> state
      (update-in [:text y] #(str (subs % 0 x) char (subs % x)))
      (update-in [:cursor :x] inc)
      set-cursor-col-from-x
      set-anchor-from-cursor))

(defn insert-newline [{{x :x y :y} :cursor :as state}]
  (-> state
      (update
       :text
       #(vec (concat (subvec % 0 y)
                     [(subs (% y) 0 x)
                      (subs (% y) x)]
                     (subvec % (inc y)))))
      (assoc :cursor {:x 0 :y (inc y) :col 0})
      set-anchor-from-cursor))

(defn insert-backspace [{text :text {x :x y :y} :cursor :as state}]
  (cond
    (and (zero? x) (zero? y)) state
    (zero? x) (-> state
                  (update
                   :text
                   #(vec (concat (subvec % 0 (dec y))
                                 [(str (% (dec y)) (% y))]
                                 (subvec % (inc y)))))
                  (assoc :cursor {:x (count (text (dec y)))
                                  :y (dec y)})
                  set-cursor-col-from-x
                  set-anchor-from-cursor)
    :else (-> state
              (update
               :text
               #(vec (concat (subvec % 0 y)
                             [(str (subs (% y) 0 (dec x)) (subs (% y) x))]
                             (subvec % (inc y)))))
              (update-in [:cursor :x] dec)
              set-cursor-col-from-x
              set-anchor-from-cursor)))

(defn snapshot [state]
  (select-keys state [:text :cursor :anchor]))

(defn start-change [state]
  (update state :past conj {:before (snapshot state)}))

(defn stop-change [{past :past :as state}]
  (-> state
      (assoc-in [:past (-> past count dec) :after] (snapshot state))
      (assoc :future [])))

(defn undo [state]
  (if-let [change (-> state :past peek)]
    (-> state
        (update :past pop)
        (merge (:before change))
        (update :future conj change))
    state))

(defn redo [state]
  (if-let [change (-> state :future peek)]
    (-> state
        (update :future pop)
        (merge (:after change))
        (update :past conj change))
    state))

; (defn read-clipboard []
;   (with-open [r (-> (Toolkit/getDefaultToolkit)
;                     .getSystemClipboard
;                     DataFlavor/getReaderForText.)]
;     (slurp r)))

(defn write-clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) (StringSelection. ""))))

(defn copy [{text :text {x :x y :y} :cursor :as state}]
  (let [[from to] (selection state)]
    ;; TODO multi line
    (write-clipboard (subs (text y) (:x from) (:x to))))
  state)

(defn handle-input [state input]
  (case (:mode state)
    :normal
    (condp = (.getKeyType input)
      KeyType/Escape (set-anchor-from-cursor state)
      KeyType/Character (case (.getCharacter input)
                          \h (-> state move-left set-anchor-from-cursor)
                          \j (-> state move-down set-anchor-from-cursor)
                          \k (-> state move-up set-anchor-from-cursor)
                          \l (-> state move-right set-anchor-from-cursor)
                          \H (move-left state)
                          \J (move-down state)
                          \K (move-up state)
                          \L (move-right state)
                          \f (-> state start-change start-insert)
                          \d (-> state start-change delete stop-change)
                          \D (-> state start-change delete-lines stop-change)
                          \z (undo state)
                          \Z (redo state)
                          \c (copy state)
                          state)
      state)

    :insert
    (condp = (.getKeyType input)
      KeyType/ArrowUp (-> state move-up set-anchor-from-cursor)
      KeyType/ArrowDown (-> state move-down set-anchor-from-cursor)
      KeyType/ArrowLeft (-> state move-left set-anchor-from-cursor)
      KeyType/ArrowRight (-> state move-right set-anchor-from-cursor)
      KeyType/Escape (-> state stop-insert stop-change)
      KeyType/Enter (insert-newline state)
      KeyType/Backspace (insert-backspace state)
      KeyType/Character (insert state (.getCharacter input))
      state)))

(defn in-selection? [state x y]
  (let [[from to] (selection state)]
    (and (or (< (:y from) y) (and (= (:y from) y) (<= (:x from) x)))
         (or (< y (:y to)) (and (= y (:y to)) (< x (:x to)))))))

(defn draw-editor [state screen w h]
  (let [y-offset (clamp (-> state :cursor :y (- (int (/ h 2))))
                        0 (-> state :text count (- h)))
        text (->> state :text (drop y-offset) (take h) (map #(take w %)))]
    (doseq [[y line] (zipmap (range) text)]
      (doseq [[x char] (zipmap (range) (concat line " "))]
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
                     (.withBackgroundColor TextColor$ANSI/WHITE))))
  (doseq [[x char] (zipmap (range) (-> state :mode name))]
    (.setCharacter screen x (inc y) (TextCharacter. char))))

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
   :mode :normal
   :past []
   :future []})

(defn main [_]
  (with-open [screen (.createScreen (DefaultTerminalFactory.))]
    ;; TODO probably want to addResizeListener. Might not need the COMPLETE /
    ;; DELTA dance if I redraw on resize.
    (.startScreen screen)
    (loop [state (initial-state (last *command-line-args*))]
      (draw state screen)
      (recur (handle-input state (.readInput screen))))))
