(ns main
  (:require
   [clojure.string :as str])
  (:import
   [java.awt Toolkit datatransfer.StringSelection datatransfer.DataFlavor]
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

(defn move-line-start [{text :text {y :y} :cursor :as state}]
  (-> state
      (assoc-in [:cursor :x]
                (count (re-find #"\s*" (text y))))
      set-cursor-col-from-x))

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

(defn clamp-cursors-xs [{:keys [cursor anchor text] :as state}]
  (let [cursor-x (clamp (:x cursor) 0 (count (text (:y cursor))))
        anchor-x (clamp (:x anchor) 0 (count (text (:y anchor))))]
    (assoc state
           :cursor {:x cursor-x :col cursor-x :y (:y cursor)}
           :anchor {:x anchor-x :col anchor-x :y (:y anchor)})))

(defn trimr [state]
  (let [[from to] (map :y (selection state))]
    (-> state
        (update :text
                #(vec (concat (subvec % 0 from)
                              (map str/trimr (subvec % from (inc to)))
                              (subvec % (inc to)))))
        clamp-cursors-xs)))

(defn set-cursor-style [n]
  ;; https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
  (println (str "\033[" n " q")))

(defn start-insert [state]
  (set-cursor-style 3)
  (assoc state :mode :insert))

(defn stop-insert [state]
  (set-cursor-style 1)
  (-> state
      trimr
      (assoc :mode :normal
             :pending-close-delims [])))

(defn start-space [state]
  (assoc state :mode :space))

(defn stop-space [state]
  (assoc state :mode :normal))

(def delims
  {\( \) \[ \] \{ \} \" \"})

(def open-delim?
  (set (keys delims)))

(defn insert [{{x :x y :y} :cursor :as state} char]
  (cond
    (= char (-> state :pending-close-delims peek))
    (-> state
        (update-in [:cursor :x] inc)
        set-cursor-col-from-x
        set-anchor-from-cursor
        (update :pending-close-delims pop))

    (open-delim? char)
    (let [close (delims char)]
      (-> state
          (update-in [:text y] #(str (subs % 0 x) char close (subs % x)))
          (update-in [:cursor :x] inc)
          set-cursor-col-from-x
          set-anchor-from-cursor
          (update :pending-close-delims conj close)))

    :else
    (-> state
        (update-in [:text y] #(str (subs % 0 x) char (subs % x)))
        (update-in [:cursor :x] inc)
        set-cursor-col-from-x
        set-anchor-from-cursor)))

;; TODO other languages
;; TODO strings
(defn indent-level
  ([text y]
   (if (zero? y)
     0
     (indent-level text (dec y) (dec (count (text (dec y)))) [])))
  ([text y x pending-open-delims]
   (cond
     (neg? y)
     0

     (or (= "" (text y)) (and (neg? x) (seq pending-open-delims)))
     (recur text (dec y) (dec (count (text (dec y)))) pending-open-delims)

     (neg? x)
     (->> (text y) (re-find #"\s*") count)

     (and (pos? x) (= \\ (get-in text [y (dec x)])))
     (recur text y (dec x) pending-open-delims)

     (= (peek pending-open-delims) (get-in text [y x]))
     (recur text y (dec x) (pop pending-open-delims))

     (#{\[ \{} (get-in text [y x]))
     (inc x)

     (= \( (get-in text [y x]))
     (let [first-el (re-find #"^[a-zA-Z0-9*+!\-_'?<>=/.:]*"
                             (subs (text y) (inc x)))]
       (if (#{"case"
              "catch"
              "cond"
              "condp"
              "cond->"
              "cond->>"
              "def"
              "defmacro"
              "defmethod"
              "defmulti"
              "defn"
              "defn-"
              "do"
              "doseq"
              "dotimes"
              "doto"
              "finally"
              "for"
              "fn"
              "if"
              "if-let"
              "if-not"
              "let"
              "loop"
              "ns"
              "try"
              "when"
              "when-let"
              "when-not"
              "while"
              "with-open"}
            first-el)
         (+ 2 x)
         (if (and (seq first-el) (< (+ 2 (count first-el) x) (count (text y))))
           (+ 2 (count first-el) x)
           (inc x))))

     (#{\) \] \}} (get-in text [y x]))
     (recur text y (dec x) (conj pending-open-delims
                                 ({\) \( \] \[ \} \{} (get-in text [y x]))))

     :else
     (recur text y (dec x) pending-open-delims))))

(defn indent [state]
  (let [[from to] (map :y (selection state))]
    (reduce
     (fn [s y]
       (update s
               :text
               #(vec (concat (subvec % 0 y)
                             [(str (apply str (repeat (indent-level % y) \space))
                                   (str/trim (% y)))]
                             (subvec % (inc y))))))
     state
     (range from (inc to)))))

(defn insert-newline [{{x :x y :y} :cursor :as state}]
  (-> state
      (update
       :text
       #(vec (concat (subvec % 0 y)
                     [(str/trimr (subs (% y) 0 x))
                      (subs (% y) x)]
                     (subvec % (inc y)))))
      (assoc-in [:cursor :y] (inc y))
      set-anchor-from-cursor
      indent
      move-line-start
      set-anchor-from-cursor))

(defn start-insert-above [state]
  (-> state
      start-insert
      (assoc-in [:cursor :x] 0)
      set-cursor-col-from-x
      insert-newline
      move-up
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

(defn insert-delete [{text :text {x :x y :y} :cursor :as state}]
  (cond
    (and (= (count (text y)) x) (= (dec (count text)) y)) state
    (= (count (text y)) x) (update state
                                   :text
                                   #(vec (concat (subvec % 0 y)
                                                 [(str (% y) (% (inc y)))]
                                                 (subvec % (+ y 2)))))
    :else (update state
                  :text
                  #(vec (concat (subvec % 0 y)
                                [(str (subs (% y) 0 x) (subs (% y) (inc x)))]
                                (subvec % (inc y)))))))

(defn snapshot [state]
  (select-keys state [:text :cursor :anchor]))

(defn start-change [state]
  (-> state
      (update :past conj {:before (snapshot state)})
      (assoc :dirty? true)))

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

;; TODO try shelling out to pbcopy/pbpaste instead of using this java interface
;; to avoid the weird focus stealing.
(defn write-clipboard [s]
  (-> (Toolkit/getDefaultToolkit)
      .getSystemClipboard
      (.setContents (StringSelection. s) (StringSelection. ""))))

(defn read-clipboard []
  (let [clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))
        flavor (DataFlavor/selectBestTextFlavor
                (.getAvailableDataFlavors clipboard))]
    (with-open [r (.getReaderForText flavor (.getContents clipboard nil))]
      (slurp r))))

(defn copy [{text :text :as state}]
  (let [[from to] (selection state)]
    (write-clipboard
     (if (= (:y from) (:y to))
       (subs (text (:y from)) (:x from) (:x to))
       (str/join "\n" (concat [(subs (text (:y from)) (:x from))]
                              (subvec text (inc (:y from)) (:y to))
                              [(subs (text (:y to)) 0 (:x to))])))))
  state)

(defn copy-lines [{text :text :as state}]
  (let [[from to] (map :y (selection state))]
    (write-clipboard (str (str/join "\n" (subvec text from (inc to))) "\n")))
  state)

(defn paste [{{x :x y :y} :cursor :as state}]
  (let [clip (str/split (read-clipboard) #"\n" -1)]
    (update state
            :text
            #(vec (concat (subvec % 0 y)
                          (if (= 1 (count clip))
                            [(str (subs (% y) 0 x) (first clip) (subs (% y) x))]
                            (concat [(str (subs (% y) 0 x) (first clip))]
                                    (drop-last (rest clip))
                                    [(str (last clip) (subs (% y) x))]))
                          (subvec % (inc y)))))))

(defn paste-lines [{{y :y} :cursor :as state}]
  (let [clip (str/split-lines (read-clipboard))]
    (update state :text #(vec (concat (subvec % 0 y) clip (subvec % y))))))

(defn save [{:keys [file-path text] :as state}]
  (spit file-path (str (str/join "\n" text) "\n"))
  (assoc state :dirty? false))

(defn handle-input [state input]
  (case (:mode state)
    :normal
    (condp = (.getKeyType input)
      KeyType/Escape (set-anchor-from-cursor state)
      KeyType/Tab (-> state start-change indent trimr stop-change)
      KeyType/Character
      (case (.getCharacter input)
        \h (-> state move-left set-anchor-from-cursor)
        \H (move-left state)
        \j (-> state move-down set-anchor-from-cursor)
        \J (move-down state)
        \k (-> state move-up set-anchor-from-cursor)
        \K (move-up state)
        \l (-> state move-right set-anchor-from-cursor)
        \L (move-right state)
        \f (-> state start-change delete start-insert)
        \F (-> state start-change delete-lines start-insert-above)
        \d (-> state start-change delete stop-change)
        \D (-> state start-change delete-lines stop-change)
        \z (undo state)
        \Z (redo state)
        \x (-> state start-change copy delete stop-change)
        \X (-> state start-change copy-lines delete-lines stop-change)
        \c (copy state)
        \C (copy-lines state)
        \v (-> state start-change delete paste stop-change)
        \V (-> state start-change delete-lines paste-lines stop-change)
        \space (start-space state)
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
      KeyType/Delete (insert-delete state)
      KeyType/Character (insert state (.getCharacter input))
      state)

    :space
    (condp = (.getKeyType input)
      KeyType/Character
      (case (.getCharacter input)
        \q nil
        \s (-> state save stop-space)
        (stop-space state))
      (stop-space state))))

(defn in-selection? [state x y]
  (let [[from to] (selection state)]
    (and (or (< (:y from) y) (and (= (:y from) y) (<= (:x from) x)))
         (or (< y (:y to)) (and (= y (:y to)) (< x (:x to)))))))

(defn draw-editor [{:keys [text cursor] :as state} screen w h]
  (let [x-offset (clamp (- (:x cursor) (int (/ w 2)))
                        0 (- (inc (count (text (:y cursor)))) w))
        y-offset (clamp (- (:y cursor) (int (/ h 2)))
                        0 (- (count text) h))
        text (->> text
                  (drop y-offset)
                  (take h)
                  (map #(->> % (drop x-offset) (take w))))]
    (doseq [[y line] (zipmap (range) text)]
      (doseq [[x char] (zipmap (range) (concat line " "))]
        (.setCharacter screen
                       x y
                       (cond-> (TextCharacter. char)
                         (in-selection? state (+ x x-offset) (+ y y-offset))
                         (.withBackgroundColor TextColor$ANSI/WHITE)))))
    (.setCursorPosition screen (TerminalPosition. (- (:x cursor) x-offset)
                                                  (- (:y cursor) y-offset)))))

(defn pad-between [left right w]
  (str left (apply str (repeat (- w (count left) (count right)) \space)) right))

(defn draw-status [{:keys [file-path cursor mode dirty? msg]} screen w y]
  (let [top (pad-between (str file-path (when dirty? " *"))
                         (str (-> cursor :x inc) "," (-> cursor :y inc))
                         w)
        bottom (pad-between (name mode) (str msg) w)]
    (doseq [[x char] (zipmap (range) top)]
      (.setCharacter screen
                     x y
                     (-> (TextCharacter. char)
                         (.withBackgroundColor TextColor$ANSI/WHITE))))
    (doseq [[x char] (zipmap (range) bottom)]
      (.setCharacter screen x (inc y) (TextCharacter. char)))))

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
   :future []
   :dirty? false
   :pending-close-delims []})

(defn main [_]
  (with-open [screen (.createScreen (DefaultTerminalFactory.))]
    ;; TODO probably want to addResizeListener to redraw on resize
    (.startScreen screen)
    (loop [state (initial-state (last *command-line-args*))]
      (draw state screen)
      (when-let [state* (handle-input state (.readInput screen))]
        (recur state*)))))
