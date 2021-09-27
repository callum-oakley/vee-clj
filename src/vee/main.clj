(ns vee.main
  (:require
   [clojure.string :as str])
  (:import
   [java.awt Toolkit datatransfer.StringSelection datatransfer.DataFlavor]
   [com.googlecode.lanterna
    SGR
    TextCharacter
    TerminalPosition
    TextColor$ANSI
    TextColor$Indexed
    screen.Screen$RefreshType
    input.KeyType
    terminal.DefaultTerminalFactory]))

(defn set-cursor-col-from-x [{{x :x} :cursor :as state}]
  (assoc-in state [:cursor :col] x))

(defn select [{:keys [cursor anchor] :as state}]
  (if-not anchor
    (assoc state :anchor cursor)
    state))

(defn deselect [state]
  (assoc state :anchor nil))

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

;; TODO the word regex will want to be configurable by language
(def prev-word-re #"(?:[a-zA-Z0-9*+!\-_'?<>=/.:]+|\p{Punct}+)\s*$")
(def next-word-re #"^(?:[a-zA-Z0-9*+!\-_'?<>=/.:]+|\p{Punct}+|\s+)\s*")

(defn move-prev-word [{text :text {x :x y :y} :cursor :as state}]
  (if-let [match (re-find prev-word-re (subs (text y) 0 x))]
    (-> state
        (update-in [:cursor :x] #(- % (count match)))
        set-cursor-col-from-x)
    state))

(defn move-next-word [{text :text {x :x y :y} :cursor :as state}]
  (if-let [match (re-find next-word-re (subs (text y) x))]
    (-> state
        (update-in [:cursor :x] #(+ % (count match)))
        set-cursor-col-from-x)
    state))

(defn move-line-start [{text :text {y :y} :cursor :as state}]
  (-> state
      (assoc-in [:cursor :x]
                (count (re-find #"\s*" (text y))))
      set-cursor-col-from-x))

(defn move-line-end [{text :text {y :y} :cursor :as state}]
  (-> state
      (assoc-in [:cursor :x] (count (text y)))
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

(defn move-prev-paragraph [{:keys [text cursor] :as state}]
  (loop [y (dec (:y cursor))
         seen-nonempty? false]
    (cond
      (neg? y)
      (-> state
          (assoc-in [:cursor :y] 0)
          set-cursor-x-from-col)

      (and seen-nonempty? (= "" (str/trim (text y))))
      (-> state
          (assoc-in [:cursor :y] (inc y))
          set-cursor-x-from-col)

      (= "" (str/trim (text y)))
      (recur (dec y) seen-nonempty?)

      :else
      (recur (dec y) true))))

(defn move-next-paragraph [{:keys [text cursor] :as state}]
  (loop [y (inc (:y cursor))
         seen-empty? false]
    (cond
      (= y (count text))
      state

      (and seen-empty? (not= "" (str/trim (text y))))
      (-> state
          (assoc-in [:cursor :y] y)
          set-cursor-x-from-col)

      (= "" (str/trim (text y)))
      (recur (inc y) true)

      :else
      (recur (inc y) seen-empty?))))

(defn selection [{:keys [cursor anchor]}]
  (when anchor
    (sort-by (juxt :y :x) [cursor anchor])))

(defn annotate [{text :text :as state}]
  (let [unq? (fn [c x y]
               (and (= c (get-in text [y x]))
                    (or (zero? x) (not= \\ (get-in text [y (dec x)])))))
        [comment string dqs]
        (->> (range (count text))
             (mapcat
              (fn [y] (map (fn [x] [x y]) (range (count (text y))))))
             (reduce
              (fn [[comment string dqs state] [x y]]
                (let [state (if (and (= :comment state) (zero? x))
                              :normal
                              state)]
                  (case state
                    :normal
                    (cond
                      (unq? \; x y) [(conj comment [x y]) string dqs :comment]
                      (unq? \" x y) [comment string (conj dqs [x y]) :string]
                      :else [comment string dqs :normal])

                    :comment
                    [(conj comment [x y]) string dqs :comment]

                    :string
                    (if (unq? \" x y)
                      [comment (conj string [x y]) (conj dqs [x y]) :normal]
                      [comment (conj string [x y]) dqs :string]))))
              [#{} #{} #{} :normal]))]
    (assoc state :annotations {:in-comment? (fn [x y] (comment [x y]))
                               :in-string? (fn [x y] (string [x y]))
                               :dq? (fn [x y] (dqs [x y]))})))

(defn delete [state]
  (if-let [[from to] (selection state)]
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
        annotate
        (assoc :cursor from :anchor nil))
    state))

(defn delete-lines [{text :text {y :y} :cursor :as state}]
  (if-let [[{from :y} {to :y}] (selection state)]
    (-> state
        (update :text #(vec (concat (subvec % 0 from) (subvec % (inc to)))))
        annotate
        (assoc-in [:cursor :y] (if (= to (dec (count text)))
                                 (dec from)
                                 from))
        set-cursor-x-from-col
        (assoc :anchor nil))
    (-> state
        (update :text #(vec (concat (subvec % 0 y) (subvec % (inc y)))))
        annotate
        (assoc-in [:cursor :y] (if (= y (dec (count text)))
                                 (dec y)
                                 y))
        set-cursor-x-from-col)))

(defn clamp-cursors-xs [{:keys [cursor anchor text] :as state}]
  (let [cursor-x (clamp (:x cursor) 0 (count (text (:y cursor))))
        anchor-x (when anchor (clamp (:x anchor) 0 (count (text (:y anchor)))))]
    (assoc state
           :cursor {:x cursor-x :col cursor-x :y (:y cursor)}
           :anchor (when anchor {:x anchor-x :col anchor-x :y (:y anchor)}))))

(defn trimr [{{y :y} :cursor :as state}]
  (if-let [[{from :y} {to :y}] (selection state)]
    (-> state
        (update :text
                #(vec (concat (subvec % 0 from)
                              (map str/trimr (subvec % from (inc to)))
                              (subvec % (inc to)))))
        annotate
        clamp-cursors-xs)
    (-> state
        (update-in [:text y] str/trimr)
        annotate
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

;; TODO clean up duplication between this and mirror etc below
(def delims
  {\( \) \[ \] \{ \} \" \"})

(def open-delim?
  (set (keys delims)))

;; TODO have backspace and delete play nicely with auto pairs
(defn insert [{{x :x y :y} :cursor :as state} char]
  (cond
    ;; TODO only if the next char in the text matches
    (= char (-> state :pending-close-delims peek))
    (-> state
        (update-in [:cursor :x] inc)
        set-cursor-col-from-x
        (update :pending-close-delims pop))

    (open-delim? char)
    (let [close (delims char)]
      (-> state
          (update-in [:text y] #(str (subs % 0 x) char close (subs % x)))
          annotate
          (update-in [:cursor :x] inc)
          set-cursor-col-from-x
          (update :pending-close-delims conj close)))

    :else
    (-> state
        (update-in [:text y] #(str (subs % 0 x) char (subs % x)))
        annotate
        (update-in [:cursor :x] inc)
        set-cursor-col-from-x)))

(def mirror
  {\( \) \[ \] \{ \} \) \( \] \[ \} \{})

(defn open-delim
  [{text :text {:keys [in-comment? in-string? dq?]} :annotations} cursor limit]
  (let [char (fn [x y] (get-in text [y x]))]
    (if (in-string? (:x cursor) (:y cursor))
      (loop [x (dec (:x cursor))
             y (:y cursor)]
        (cond
          (or (and (zero? y) (neg? x)) (> (- (:y cursor) y) limit))
          nil

          (neg? x)
          (recur (-> y dec text count dec) (dec y))

          (dq? x y)
          {:x x :y y}

          :else
          (recur (dec x) y)))
      (loop [x (dec (:x cursor))
             y (:y cursor)
             pending []]
        (cond
          (or (and (zero? y) (neg? x)) (> (- (:y cursor) y) limit))
          nil

          (neg? x)
          (recur (-> y dec text count dec) (dec y) pending)

          (or (in-comment? x y)
              (in-string? x y)
              (and (pos? x) (= \\ (char (dec x) y))))
          (recur (dec x) y pending)

          (and (char x y) (= (peek pending) (char x y)))
          (recur (dec x) y (pop pending))

          (#{\) \] \}} (char x y))
          (recur (dec x) y (conj pending (mirror (char x y))))

          (#{\( \[ \{} (char x y))
          {:x x :y y}

          :else
          (recur (dec x) y pending))))))

(defn close-delim
  [{text :text {:keys [in-comment? in-string? dq?]} :annotations} cursor limit]
  (let [char (fn [x y] (get-in text [y x]))]
    (if (in-string? (:x cursor) (:y cursor))
      (loop [x (:x cursor)
             y (:y cursor)]
        (cond
          (or (and (>= y (-> text count dec)) (>= x (-> y text count)))
              (> (- y (:y cursor)) limit))
          nil

          (>= x (-> y text count))
          (recur 0 (inc y))

          (dq? x y)
          {:x x :y y}

          :else
          (recur (inc x) y)))
      (loop [x (:x cursor)
             y (:y cursor)
             pending []]
        (cond
          (or (and (>= y (-> text count dec)) (>= x (-> y text count)))
              (> (- y (:y cursor)) limit))
          nil

          (>= x (-> y text count))
          (recur 0 (inc y) pending)

          (or (in-comment? x y)
              (in-string? x y)
              (and (pos? x) (= \\ (char (dec x) y))))
          (recur (inc x) y pending)

          (and (char x y) (= (peek pending) (char x y)))
          (recur (inc x) y (pop pending))

          (#{\( \[ \{} (char x y))
          (recur (inc x) y (conj pending (mirror (char x y))))

          (#{\) \] \}} (char x y))
          {:x x :y y}

          :else
          (recur (inc x) y pending))))))

(defn move-open-delim [{cursor :cursor :as state}]
  (if-let [{x :x y :y} (open-delim state cursor 100)]
    (-> state
        (assoc-in [:cursor :y] y)
        (assoc-in [:cursor :x] (inc x))
        set-cursor-col-from-x)
    state))

(defn move-close-delim [{cursor :cursor :as state}]
  (if-let [{x :x y :y} (close-delim state cursor 100)]
    (-> state
        (assoc-in [:cursor :y] y)
        (assoc-in [:cursor :x] x)
        set-cursor-col-from-x)
    state))

(defn indent-level [{text :text :as state} y]
  (if-let [{x :x y :y} (open-delim state {:x 0 :y y} 100)]
    (if (= \( (get-in text [y x]))
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
          (if (and (seq first-el)
                   (< (+ 2 (count first-el) x) (count (text y))))
            (+ 2 (count first-el) x)
            (inc x))))
      (inc x))
    0))

(defn indent [{{y :y} :cursor :as state}]
  (let [indent-line
        (fn [s y]
          (-> s
              (update-in [:text y]
                         #(str (apply str (repeat (indent-level s y) \space))
                               (str/trim %)))
              annotate))]
    (if-let [[{from :y} {to :y}] (selection state)]
      (reduce indent-line state (range from (inc to)))
      (indent-line state y))))

(defn insert-newline [{{x :x y :y} :cursor :as state}]
  (-> state
      (update
       :text
       #(vec (concat (subvec % 0 y)
                     [(str/trimr (subs (% y) 0 x))
                      (subs (% y) x)]
                     (subvec % (inc y)))))
      annotate
      (assoc-in [:cursor :y] (inc y))
      indent
      move-line-start))

(defn start-insert-above [state]
  (-> state
      start-insert
      (assoc-in [:cursor :x] 0)
      set-cursor-col-from-x
      insert-newline
      move-up))

(defn insert-backspace [{text :text {x :x y :y} :cursor :as state}]
  (cond
    (and (zero? x) (zero? y)) state
    (zero? x) (-> state
                  (update :text
                          #(vec (concat (subvec % 0 (dec y))
                                        [(str (% (dec y)) (% y))]
                                        (subvec % (inc y)))))
                  annotate
                  (assoc :cursor {:x (count (text (dec y)))
                                  :y (dec y)})
                  set-cursor-col-from-x)
    :else (-> state
              (update-in [:text y] #(str (subs % 0 (dec x)) (subs % x)))
              annotate
              (update-in [:cursor :x] dec)
              set-cursor-col-from-x)))

(defn insert-delete [{text :text {x :x y :y} :cursor :as state}]
  (cond
    (and (= (count (text y)) x) (= (dec (count text)) y)) state
    (= (count (text y)) x) (-> state
                               (update :text
                                       #(vec (concat (subvec % 0 y)
                                                     [(str (% y) (% (inc y)))]
                                                     (subvec % (+ y 2)))))
                               annotate)
    :else (-> state
              (update-in [:text y] #(str (subs % 0 x) (subs % (inc x))))
              annotate)))

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
        annotate
        (update :future conj change))
    state))

(defn redo [state]
  (if-let [change (-> state :future peek)]
    (-> state
        (update :future pop)
        (merge (:after change))
        annotate
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
  (when-let [[from to] (selection state)]
    (write-clipboard
     (if (= (:y from) (:y to))
       (subs (text (:y from)) (:x from) (:x to))
       (str/join "\n" (concat [(subs (text (:y from)) (:x from))]
                              (subvec text (inc (:y from)) (:y to))
                              [(subs (text (:y to)) 0 (:x to))])))))
  state)

(defn copy-lines [{text :text {y :y} :cursor :as state}]
  (if-let [[{from :y} {to :y}] (selection state)]
    (write-clipboard (str (str/join "\n" (subvec text from (inc to))) "\n"))
    (write-clipboard (str (text y) "\n")))
  state)

(defn paste [{{x :x y :y} :cursor :as state}]
  (let [clip (str/split (read-clipboard) #"\n" -1)]
    (-> state
        (update
         :text
         #(vec (concat (subvec % 0 y)
                       (if (= 1 (count clip))
                         [(str (subs (% y) 0 x) (first clip) (subs (% y) x))]
                         (concat [(str (subs (% y) 0 x) (first clip))]
                                 (drop-last (rest clip))
                                 [(str (last clip) (subs (% y) x))]))
                       (subvec % (inc y)))))
        annotate)))

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
      KeyType/Escape (assoc state :anchor nil)
      KeyType/Tab (-> state start-change indent trimr stop-change)
      KeyType/Character
      (case (.getCharacter input)
        \y (-> state deselect move-open-delim)
        \Y (-> state select move-open-delim)
        \u (-> state deselect move-prev-word)
        \U (-> state select move-prev-word)
        \i (-> state deselect move-next-word)
        \I (-> state select move-next-word)
        \o (-> state deselect move-close-delim)
        \O (-> state select move-close-delim)
        \h (-> state deselect move-left)
        \H (-> state select move-left)
        \j (-> state deselect move-down)
        \J (-> state select move-down)
        \k (-> state deselect move-up)
        \K (-> state select move-up)
        \l (-> state deselect move-right)
        \L (-> state select move-right)
        \n (-> state deselect move-line-start)
        \N (-> state select move-line-start)
        \m (-> state deselect move-next-paragraph)
        \M (-> state select move-next-paragraph)
        \, (-> state deselect move-prev-paragraph)
        \< (-> state select move-prev-paragraph)
        \. (-> state deselect move-line-end)
        \> (-> state select move-line-end)
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
      KeyType/ArrowUp (move-up state)
      KeyType/ArrowDown (move-down state)
      KeyType/ArrowLeft (move-left state)
      KeyType/ArrowRight (move-right state)
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
  (when-let [[from to] (selection state)]
    (and (or (< (:y from) y) (and (= (:y from) y) (<= (:x from) x)))
         (or (< y (:y to)) (and (= y (:y to)) (< x (:x to)))))))

(defn draw-editor [{:keys [text cursor annotations] :as state} screen w h]
  (let [delim? (set [(open-delim state cursor (int (/ h 2)))
                     (close-delim state cursor (int (/ h 2)))])
        {:keys [in-comment? in-string? dq?]} annotations
        x-offset (clamp (- (:x cursor) (int (/ w 2)))
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
                         (delim? {:x (+ x x-offset) :y (+ y y-offset)})
                         (.withBackgroundColor (TextColor$Indexed. 255))

                         (in-selection? state (+ x x-offset) (+ y y-offset))
                         (.withBackgroundColor TextColor$ANSI/WHITE)

                         (in-comment? (+ x x-offset) (+ y y-offset))
                         (.withForegroundColor TextColor$ANSI/RED)

                         (or (in-string? (+ x x-offset) (+ y y-offset))
                             (dq? (+ x x-offset) (+ y y-offset)))
                         (.withModifier SGR/ITALIC)))))
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
  (annotate {:file-path file-path
             :text (str/split-lines (slurp file-path))
             :cursor {:x 0 :y 0 :col 0}
             :anchor nil
             :mode :normal
             :past []
             :future []
             :dirty? false
             :pending-close-delims []}))

(defn main [_]
  (with-open [screen (.createScreen (DefaultTerminalFactory.))]
    ;; TODO probably want to addResizeListener to redraw on resize
    (.startScreen screen)
    (loop [state (initial-state (last *command-line-args*))]
      (draw state screen)
      (when-let [state* (handle-input state (.readInput screen))]
        (recur state*)))))
