#(use freja/flow)
(import freja-jaylib :as fj)
(use freja/defonce)
(use freja-jaylib)


(import ../freja/freja/vector-math :prefix "")
(import ./coll :prefix "")
(import ./exp :as explo)

(import spork/path)

(defonce saved-levels
  @{})

(def levels @{})

(def fullscreen?
  # false
  true
  #
)


(comment
  (import freja/state)
  (put state/editor-state :left nil)

  (keys state/editor-state)
  #
)

(defonce sounds @{})


(defn sound
  [k]
  (let [s (in sounds k)]
    (play-sound s)
    s))

(def steps
  [:steps/step1
   :steps/step2
   # :steps/step3
   # :steps/step4 ## too sharp?
   :steps/step5
   :steps/step6
   :steps/step7])

(defn random-elem
  [ind]
  (in ind (math/floor (* (length ind) (math/random)))))

(var delay 20)

(defn walking-sound
  []
  (var last (sounds (random-elem steps)))
  (var second-to-last last)
  (var s last)

  (forever
    (if (sound-playing? last)
      (yield nil)
      (do (loop [_ :range [0 delay]]
            (yield nil))

        (while (or (= s last)
                   (= s second-to-last))
          (def v (random-elem steps))
          (print v)
          (set s (sounds v)))

        (set second-to-last last)
        (set last s)

        # (set-sound-pitch s 0.8)
        (set-sound-volume s 0.5)

        (yield (play-sound s))))))

(comment
  #
)

(defonce images @{:a 10})


(def render-scale 1)
(def debug-scale 10)

(def debug-logs @[])


(def anims @[])


(varfn death-screen
  [mw mh]
  (draw-rectangle
    0
    0
    2000
    1000
    [1 0 0 1])

  (let [size (math/floor (* render-scale 154))
        w (fj/measure-text "You died" size)]
    (fj/draw-text "You died"
                  (math/floor (- (/ (* render-scale mw) 2) (/ w 2)))
                  (math/floor (- (/ (* render-scale mh) 2) (/ size 2)))
                  size
                  :black)))

(var render-width 0)
(var render-height 0)

(defn log
  [& args]
  (array/push debug-logs (string/format
                           (string/join
                             (seq [i :range [0 (length args)]]
                               "%p")
                             " ")
                           ;args))

  (last args))

(defn run-animations
  [anims]

  (var i 0)
  (while (< i (length anims))
    (let [a (in anims i)]
      (if-not (fiber/can-resume? a)
        (array/remove anims i)
        (do
          (try (resume a)
            ([err fib]
              (debug/stacktrace fib err)))
          (++ i))))))


(defmacro anim
  [& body]
  ~(array/push anims
               (fiber/new (fn []
                            ,;body))))

(defn ease-in-out
  [p]
  (math/sin (* math/pi p 0.5)))

(defn ease-out-expo
  [p]
  (- 1 (math/pow 2 (* -10 p))))

(defn ease-in-expo
  [p]
  (math/pow p 3))

(defn flash-text
  [t x y size kind]
  (anim
    (let [w (fj/measure-text t size)
          nof-newlines (length (string/find-all "\n" t))
          newline-delay 5
          space-delay 0
          duration (max 180 (* (length t) 5))]

      (var index 0)
      (var delay 0)
      (var delay-pos nil)

      (loop [i :range [0 duration]]
        (if (pos? delay)
          (-- delay)
          (do
            (set delay 0.7)
            (++ index)))

        (def t-to-show
          (if (and (not (= kind :note))
                   (< index (length t)))
            (string/slice t 0 index)
            t))

        (when (and (not= delay-pos index)
                   (= (chr "\n") (last t-to-show)))
          (set delay-pos index)
          (set delay newline-delay))

        (when (and (not= delay-pos index)
                   (= (chr " ") (last t-to-show)))
          (set delay-pos index)
          (set delay space-delay))

        (def color
          (if (= kind :note)

            [0.407843 0.407843 0.619608
             (cond

               (< i (* duration 0.05))
               (ease-in-out (/ i (* duration 0.051)))

               (> i (* duration 0.95))
               (- 1 (ease-in-out (/ (- i (* duration 0.95)) (* duration 0.051))))
               1)]

            (cond
              (> i (* duration 0.95))
              [1 1 1 (- 1 (ease-in-out (/ (- i (* duration 0.95)) (* duration 0.051))))]

              :white)))

        (def color2
          (if (= kind :note)
            [1 0.978431 0.660784
             (cond
               (< i (* duration 0.05))
               (ease-in-out (/ i (* duration 0.051)))

               (> i (* duration 0.95))
               (- 1 (ease-in-out (/ (- i (* duration 0.95)) (* duration 0.051))))
               1)]

            (cond
              (> i (* duration 0.95))
              [0 0 0 (- 0.9 (* 0.9 (ease-in-out (/ (- i (* duration 0.95)) (* duration 0.051)))))]

              [0 0 0 0.9])))

        (draw-rectangle
          (math/floor (- x (/ w 2) 12))
          (math/floor (- y (/ size 2) 12))
          (+ 24 w)
          (+ 24 (* (inc nof-newlines) (+ 8 size)))
          color2)

        (when (= kind :player-says)
          (let [start-x (+ x (/ w 4))
                start-y
                (+ (math/floor (- y (/ size 2) 12))
                   (+ 24 (* (inc nof-newlines) (+ 8 size))))
                w 24
                h 24]
            (draw-triangle
              [(math/floor start-x) start-y]
              [(math/floor (+ start-x w)) (+ start-y h)]
              [(math/floor (+ start-x w)) start-y]
              color2)))

        (yield (fj/draw-text
                 t-to-show
                 (math/floor (- x (/ w 2)))
                 (math/floor (- y (/ size 2)))
                 size
                 color))))))

(var c nil)


(var map-pos nil)
(var map-w nil)
(var map-h nil)
(var map-pixels nil)
(var map-model nil)

# (disable-cursor)


# (unload-image im-map)

(def last-size @[nil nil])
(var rt nil)

(var bullets @[])
(var enemies @[])


(defonce player @{:inventory @{}
                  :holding-i 0
                  :hope 5
                  :recovery-duration 23
                  :radius 0.5
                  :attack-state @{:duration 0}
                  :hp 72
                  :max-hp 72})

(put player :dead false)
(put player :hp 72)
(put player :invul nil)


(var notes @[])

(var current-level nil)


(defn init-level
  [level-name spawn-point]
  #(array/clear anims)

  (def level (levels level-name))
  (assert level (string/format "only %p levels exist, not %p" (keys levels) level-name))

  # set up camera
  (let [spawn-point (get-in level [:spawn-points spawn-point])

        start-pos
        #(if c
        #(get-camera-position c)
        spawn-point # original start-pos
        #'(-9.02823 1.00828 -2.31514)
        #)
]

    (set-camera-position c start-pos)

    (put player
         :last-pos (get-camera-position c))
    (put player :pos (get-camera-position c)))

  # init map


  (let [im-map (load-image-1 (level :map))
        mesh (gen-mesh-cubicmap im-map [1 2 1])
        model (load-model-from-mesh mesh)
        mat (get-model-material model 0)
        pixels (get-image-data im-map)
        [w h] (image-dimensions im-map)]

    (set map-model model)
    (set map-pixels pixels)
    (set map-w w)
    (set map-h h)

    (set map-pos @[(- (math/floor (/ w 2))) 0 (- (math/floor (/ w 2)))])

    (set-material-texture mat (images (level :map-texture)) :diffuse))

  (if-let [{:enemies es
            :notes ns
            :bullets bs} # nil 
           (saved-levels level-name)
           #
]
    (do
      (print "you have been on this level")
      (set enemies es)
      (set notes ns)
      (set bullets bs))
    (do
      (print "new level!")
      (set enemies @[])
      (set notes @[])
      (set bullets @[])
      (:init level)))

  (set current-level level-name))

(var level-to-change-to nil)

(defn save-level
  [level]
  (put saved-levels
       level
       {:enemies enemies
        :notes notes
        :bullets bullets}))

(defn change-level
  [level-name spawn-point]
  (save-level current-level)
  (set level-to-change-to [level-name spawn-point]))

#### START OF FUNCS


(defn cam-rot
  [c]
  (normalize
    (v-
      (get-camera-target c)
      (get-camera-position c))))


(defn ->map-pos
  [map-w map-h px py]
  [(+ px (* 0.5 map-w))
   (+ py (* 0.5 map-h))])


(defn points-between-line
  [[emx emy] [pmx pmy]]

  (var points @[])

  (let [emx (+ 0.5 emx)
        emy (+ 0.5 emy)
        pmx (+ 0.5 pmx)
        pmy (+ 0.5 pmy)

        start-x emx
        stop-x pmx
        start-y emy
        stop-y pmy

        dir (v-
              [stop-x stop-y]
              [start-x start-y])

        divver (if (> (math/abs (dir 0)) (math/abs (dir 1)))
                 (dir 0)
                 (dir 1))

        dir2 (v* dir (/ 1 (math/abs divver)))]

    (var x start-x)
    (var y start-y)

    #                # determine direction of loop
    (while (and ((if (< start-x stop-x) < >)
                  x
                  stop-x)
                ((if (< start-y stop-y) < >)
                  y
                  stop-y))

      # draw line points
      (comment
        (draw-circle (* debug-scale (+ 0.5 (math/floor x)))
                     (* debug-scale (+ 0.5 (math/floor y)))
                     5
                     :yellow)
        #
)

      (array/push points [x y])

      (+= x (dir2 0))
      (+= y (dir2 1))))

  points)

(defn walls-between
  [o1 o2]
  (var colls @[])
  (loop [[x y] :in (points-between-line
                     (->map-pos map-w map-h
                                (get-in o1 [:pos 0])
                                (get-in o1 [:pos 2]))
                     (->map-pos map-w map-h
                                (get-in o2 [:pos 0])
                                (get-in o2 [:pos 2])))]
    (when
      (and (< y map-h)
           (< x map-w)
           (= [1 1 1 1]
              (map-pixels (+ (* (math/floor y) map-w)
                             (math/floor x)))))
      (math/floor x) (math/floor y)
      (array/push colls [(math/floor x) (math/floor y)])))
  colls)

(defn aquire-target
  [e]
  (if (e :aquiring)
    (do
      (if-not (empty? (walls-between e (e :aquiring)))
        (do
          # lost target
          (put e :aquiring nil)
          (put e :aquire-delay nil))

        (do
          (update e :aquire-delay dec)

          (when (= 112 (e :aquire-delay))
            (sound :notice1))

          (when (neg? (e :aquire-delay))

            (put e :target (e :aquiring))
            (put e :aquiring nil)
            (put e :aquire-delay nil)

            (sound :running)))))

    (when (empty? (walls-between e player))
      (put e :aquiring player)
      (put e :aquire-delay 120))))

(defn dying-npc
  [e])

(defn render-debug-log
  []

  (unless (empty? debug-logs)
    (let [y-offset 180
          size 16
          line-h 1.5
          h (math/floor (* line-h size))

          w (reduce (fn [big dl]
                      (max big (fj/measure-text dl size)))
                    0
                    debug-logs)]

      (draw-rectangle 0
                      (- y-offset 10)
                      (+ 20 w)
                      (+ 20 (* (length debug-logs) h))
                      0x000000dd)

      (var y 0)
      (loop [dl :in debug-logs]
        (fj/draw-text dl 10 (+ y y-offset) size :green)
        (+= y h))))

  (array/clear debug-logs))


(defn aggressive-melee-npc
  [e]
  (put e :moved false)
  (put-in e [:attack-state :input] nil)
  (if (e :dead)
    (dying-npc e)
    (do

      (when (and (e :target)
                 ((e :target) :dead))
        (print "target dead")
        (put e :target nil))

      (unless (e :target)
        (aquire-target e))

      (when (e :target)
        (def wb (walls-between e (e :target)))
        (if-not (empty? wb)

          (put e :target nil)

          (when-let [t (e :target)]
            (put e :target-pos (t :pos)))))

      (unless (e :aquiring)
        (let [{:target t
               :pos pos
               :speed speed
               :attack-range ar
               :attack-range-non-player arnp} e
              attack-range (if (= t player)
                             ar
                             arnp)
              distance (when t
                         (dist (t :pos) pos))]
          (when-let [tp (e :target-pos)]
            (update e :dir (fn [dir]
                             (->
                               (v+ dir
                                   (v*
                                     (v-
                                       (v- tp pos)
                                       dir)
                                     0.1))
                               normalize)))

            (put-in e [:dir 1] 0)

            (if (and distance
                     (e :target)
                     (< distance attack-range))
              (unless (get-in e [:target :invul])
                (:attack e (e :target)))
              (-> e
                  (put :moved true)
                  (update :pos v+ (v* (e :dir) speed))))))))))

(def rat
  @{:hp 28
    :attack-range 1.5
    :attack-range-non-player 0.4
    :attack-startup 13
    :attack-recovery 32

    :dir @[0 0 0]

    :attack (fn [self & args]
              (def {:target target} self)
              (when target
                (if (target :eatable?)
                  (put-in self [:attack-state :input] :try-eat)
                  (put-in self [:attack-state :input] :try-attack))))
    :speed 0.04
    :radius 0.15
    :blink 0
    :update aggressive-melee-npc})

(comment
  (array/push enemies (-> (table/clone rat)
                          (put :pos @[1 0.2 1])
                          (put :attack-state @{:duration 0})))
  (array/push enemies (-> (table/clone rat)
                          (put :pos @[0 0.2 1.2])
                          (put :attack-state @{:duration 0})))
  (array/push enemies (-> (table/clone rat)
                          (put :pos @[0.5 0.2 0.3])
                          (put :attack-state @{:duration 0})))
  #
)

(defn gain-hope
  [o amount]
  (update o :hope + amount)

  (comment anim
           (let [dur (* amount 10)]
             (loop [f :range [0 dur]
                    :let [p (/ f (dec dur))
                          p (- 1 (ease-in-expo p))]]
               (yield (draw-rectangle
                        0
                        0
                        2000
                        1000
                        [0 0 1 p])))))
  #
)

(defn handle-note
  [note mw mh]

  (let [{:pos pos
         :text text
         :dir dir
         :timer timer
         :last-tex last-tex} note
        close (> 0.1 (dist-sqr
                       pos
                       (v+
                         (in player :pos)
                         (v* (cam-rot c)
                             0.5))))

        _ (when (neg? timer)
            (put note :timer 5))

        tex (if (or (nil? last-tex)
                    (neg? timer))
              (if
                (< 0.3 (math/random))
                (images :note-2)
                (images :note))

              last-tex)]

    (put note :last-tex tex)
    (update note :timer dec)

    (comment
      (draw-billboard c
                      tex
                      pos
                      1
                      (if close
                        :white
                        :gray))
      #
)

    (do comment
      (draw-cube-texture
        tex
        pos
        0.2
        0.2
        0.2
        (if close
          :white
          :gray))
      #
)

    (when (and close
               (= :touch (get-in player [:attack-state :kind])))
      (flash-text
        text
        (/ mw 2)
        (/ mh 2)
        24
        :note))))

(def cabinet
  @{:radius 0.5

    :tick
    (fn [self mw mh]
      (let [{:pos pos
             :open open
             :content content} self
            close (> 1 (dist-sqr
                         pos
                         (v+
                           (in player :pos)
                           (v* (cam-rot c)
                               0.5))))]

        (draw-billboard
          c
          (if open
            (images :cabinet-open)
            (images :cabinet-closed))
          pos
          1
          (if close
            :white
            :gray))

        (when-let [content (and open
                                content)]
          (draw-billboard
            c
            (images content)
            pos
            1
            (if close
              :white
              :gray)))

        (when (and close
                   (= :touch (get-in player [:attack-state :kind])))
          (cond (and open
                     content)
            (do (put-in player [:inventory content] true)
              (:pickup self player content)
              (put self :content nil))

            (update self :open not))))

      #
)})


(varonce last-coll nil)


(defn handle-wall-coll
  [o o-rec wall]
  (def radius (in o :radius))

  (when (check-collision-circle-rec
          o-rec
          radius
          wall)
    (update o :collision/nof-hits |(inc (or $ 0)))

    (let [[ox oy oz] (in o :pos)]
      (def angle (v- o-rec wall))

      (def x-diff (- (+ 0.5 radius) (math/abs (- (o-rec 0) (wall 0)))))

      (def x-diff
        (if (neg? (angle 0))
          (- x-diff)
          x-diff))

      (def z-diff (- (+ 0.5 radius) (math/abs (- (o-rec 1) (wall 1)))))

      (def z-diff
        (if (pos? (angle 1))
          (- z-diff)
          z-diff))

      (if (> (math/abs (angle 0))
             (math/abs (angle 1)))
        # "left"
        [(+ ox x-diff) oy oz]
        # "up"
        [ox oy (- oz z-diff)]))))

(defn handle-non-wall-coll
  [o o2]
  (when (circle-circle?
          (in o :pos) (in o :radius)
          (in o2 :pos) (in o2 :radius))
    (let [[ox oy oz] (in o :pos)
          distance (dist (in o :pos) (in o2 :pos))
          to-move (- (+ (in o :radius)
                        (in o2 :radius))
                     distance)]
      #(tracev distance)
      (def angle (normalize (v- (in o :pos) (in o2 :pos))))

      (if (= o player)
        (set-camera-position c
                             (v+ (get-camera-position c) (v* angle to-move)))

        (update o :pos v+ (v* angle to-move))))))

(defn default-bullet-tick
  [b]

  (def {:pos pos
        :dir dir
        :speed speed
        :gravity gravity
        :sprite sprite
        :stopped stopped
        :scale scale} b)

  (unless stopped
    (update b :pos v+ (v* dir speed))
    (update-in b [:pos 1] - gravity)
    (update b :gravity + 0.002))

  (draw-billboard c sprite pos scale :white))


(defn tick
  [el]
  (def [x y z] (player :pos))
  (def attack-kind (get-in player [:attack-state :kind]))

  (when (el :focused?)
    (when (or (= attack-kind :shoot-full)
              (= attack-kind :shoot-half)) # (mouse-button-released? 0)
      (def b @{:pos (array ;(player :pos))
               :sprite (images :bullet)
               :damage 13
               :gravity 0

               :radius 0.05
               :scale 0.3

               :collision/nof-hits 0

               :dir (cam-rot c)
               :speed (if (= attack-kind :shoot-full)
                        0.25
                        0.125)

               :tick
               (fn [b]
                 (def {:pos pos} b)
                 (cond
                   (b :dead)
                   (draw-billboard c (get-render-texture explo/rt)
                                   pos 0.5 :white)

                   (neg? (pos 1))
                   (put b :dead true)

                   (default-bullet-tick b)))

               :hit (fn [b kind &opt obj]
                      (case kind
                        :enemy
                        (do
                          (update obj :hp - (b :damage))
                          (when (>= 0 (obj :hp))
                            (put obj :dead true))
                          (put obj :hit 20)
                          (put obj :blink-timer 5)
                          (put b :dead true))

                        :wall
                        (put b :dead true)))})

      (update b :pos v+ (v* (cam-rot c) 1))

      (update-in b [:pos 1] - 0.07)
      (array/push bullets b))

    (when (or (= attack-kind :throw))
      (def bread @{:pos (array ;(player :pos))
                   :sprite (images :bread)
                   :gravity 0
                   :scale 0.5
                   :dir (cam-rot c)
                   :speed 0.04

                   :collision/nof-hits 0

                   :radius 0.05

                   :smell-radius 5

                   :eatable? true

                   :tick
                   (fn [b]
                     (def {:pos pos
                           :smell-radius smell-radius} b)

                     (unless (b :dead)
                       (let [[bx by bz] pos
                             b-pos [bx bz]]

                         (loop [e :in enemies
                                :let [{:pos pos
                                       :radius e-radius
                                       :target target
                                       :aquiring aquiring} e
                                      [ex ey ez] pos
                                      e-pos [ex ez]]
                                :when (not (e :dead))]
                           (unless (or (= aquiring b)
                                       (= target b)
                                       (get target :eatable?))
                             (when (circle-circle? e-pos
                                                   e-radius
                                                   b-pos
                                                   smell-radius)
                               (put e :aquiring b)
                               (put e :aquire-delay 10)
                               (put e :target nil))))))

                     (cond
                       (b :dead)
                       (do #dead
)

                       (neg? (pos 1))
                       (do
                         (put b :stopped true)
                         (put-in b [:pos 1] 0))

                       (default-bullet-tick b)))

                   :hit (fn [b kind &opt obj]
                          (case kind
                            :player
                            (when (b :stopped)
                              (put b :dead true)

                              (flash-text
                                ``
The bread is dirty...
                       ``
                                (/ render-width 2)
                                (/ render-height 1.5)
                                24
                                :player-says)

                              (put-in player [:inventory :bread] true))

                            :enemy
                            (do # eating bread
)

                            :wall
                            (let [[bx by bz] (b :pos)]
                              (handle-wall-coll b
                                                (->map-pos map-w map-h bx bz)
                                                obj)

                              (put b :dir [0 1 0]))))})

      (update bread :pos v+ (v* (cam-rot c) 0.3))

      (update-in bread [:pos 1] - 0.07)

      (put-in player [:inventory :bread] nil)

      (array/push bullets bread)))

  (loop [e :in enemies]
    (:update e)

    (loop [e2 :in enemies
           :when (not= e e2)]
      (handle-non-wall-coll e e2))
    #
)

  (var i 0)

  (while (< i (length bullets))
    (if ((in bullets i) :dead)
      (++ i) ##1 # (array/remove bullets i)
      (++ i))))


(defn handle-map-collisions
  []
  (let [scale 10
        player-pos (get-camera-position c)
        [px py pz] player-pos
        [pmx pmy] (->map-pos map-w map-h px pz)]

    ## draw map
    #
    (comment
      (loop [x :range [0 map-w]
             y :range [0 map-h]
             :when (and (= [1 1 1 1]
                           (map-pixels (+ (* y map-w)
                                          x))))]

        (draw-rectangle (* scale x)
                        (* scale y)
                        scale
                        scale
                        :red)))

    ## draw player
    #
    (comment
      (draw-circle
        (math/round (* scale (+ pmx 0.5)))
        (math/round (* scale (+ pmy 0.5)))
        (* scale 0.5)
        :blue))

    ## draw enemies
    #
    (comment
      (loop [{:pos pos} :in enemies
             :let [[ex ey ez] pos
                   [emx emy] (->map-pos map-w map-h ex ez)]]
        (draw-circle
          (math/round (* scale (+ emx 0.5)))
          (math/round (* scale (+ emy 0.5)))
          (* scale 0.5)
          :purple)

        (draw-line-ex
          [(math/round (* scale (+ ex (* 0.5 map-w) 0.5)))
           (math/round (* scale (+ ez (* 0.5 map-h) 0.5)))]
          [(math/round (* scale (+ px (* 0.5 map-w) 0.5)))
           (math/round (* scale (+ pz (* 0.5 map-h) 0.5)))]
          2
          :white)))

    #
    ## end of drawing map


    (loop [x :range [0 map-w]
           y :range [0 map-h]
           :when (and (= [1 1 1 1]
                         (map-pixels (+ (* y map-w)
                                        x))))
           :let [tile-rec [x y 1 1]]]

      (let [bullet-size 0.1]
        (loop [b :in bullets
               :let [{:pos pos
                      :radius radius} b
                     [bx by bz] pos]
               :when
               (check-collision-circle-rec
                 (->map-pos map-w map-h bx bz)
                 radius
                 tile-rec)]

          (:hit b :wall tile-rec)))

      (when-let [new-pos (handle-wall-coll
                           player
                           (->map-pos map-w map-h px pz)
                           tile-rec)]

        (set-camera-position c new-pos)

        ## draw collision
        (comment
          (draw-rectangle (* scale x)
                          (* scale y)
                          scale
                          scale
                          :green)))

      (loop [e :in enemies
             :let [[ex ey ez] (in e :pos)]]
        (when-let
          [new-pos (handle-wall-coll
                     e
                     (->map-pos map-w map-h ex ez)
                     tile-rec)]

          (put e :pos new-pos)

          ## draw enemy wall collision
          (comment
            (draw-rectangle (* scale x)
                            (* scale y)
                            scale
                            scale
                            :green)))))))

(def walk-sound-fib (fiber/new walking-sound))

(defn render-hand
  [{:focused? focused?
    :width mw
    :height mh}]

  (set render-width mw)
  (set render-height mh)

  (let [as (in player :attack-state)
        last-state (get-in player [:attack-state :kind])
        input-state (cond
                      (and focused?
                           (mouse-button-pressed? 0))
                      :touch

                      nil)

        state (cond
                (or (and (> (in as :duration) 7)
                         (= last-state :touch-end))
                    (= last-state :touch))
                :touch-end

                (and (> (in as :duration) 7)
                     (= last-state :touch-start))
                :touch

                (or (and (= input-state :touch)
                         (nil? last-state))
                    (and (= input-state :touch)
                         (= :moving last-state))
                    (= last-state :touch-start))
                :touch-start

                (player :moved?) :moving

                nil)

        _ (if (= state last-state)
            (update as :duration inc)
            (-> as
                (put :duration 0)
                (put :kind state)))

        duration (in as :duration)

        tex-kw (cond
                 (= state :touch)
                 :hand-grab

                 (and (> duration 4)
                      (= state :touch-start))
                 :hand-grab

                 :hand-passive)

        t (in images tex-kw)
        scale (case state
                :touch-start 7

                6)

        [w h] (texture-dimensions t)

        render-x
        (case state
          :touch1236
          (- (- (* 0.5 mw) (* 0.5 (* scale w)))
             (* (- (- (* 0.5 mw) (* 0.5 (* scale w)))
                   (- mw (* scale w)))
                (- 1
                   (ease-in-expo (/ (min duration 1) 1)))))

          (- mw (* scale w)))

        render-y (case state
                   :moving (- mh (* scale h)
                              -30
                              (* 30
                                 (math/sin
                                   (* 2 math/pi (/ (mod duration 40) 40)))))

                   nil (- mh (* scale h)
                          -5
                          (* 5
                             (math/sin
                               (* math/pi (/ (mod duration 60) 60)))))

                   (- mh (* scale h)))]

    (draw-texture-ex
      t
      [(* render-scale render-x)
       (* render-y render-scale)]
      0
      (* render-scale scale)
      :white)))

(defn render-holding
  [{:focused? focused?
    :width mw
    :height mh}]

  (set render-width mw)
  (set render-height mh)

  (let [as (in player :attack-state)
        last-state (get-in player [:attack-state :kind])
        input-state (cond
                      (and focused?
                           (mouse-button-pressed? 0))
                      :throw

                      nil)

        state (cond
                (or (and (< (in as :duration) 7)
                         (= last-state :throw-end))
                    (= last-state :throw))
                :throw-end

                (and (> (in as :duration) 7)
                     (= last-state :throw-start))
                :throw

                (or (and
                      (= input-state :throw)
                      (nil? last-state))
                    (and
                      (= input-state :throw)
                      (= last-state :moving))
                    (= last-state :throw-start))
                :throw-start

                (player :moved?) :moving

                nil)

        _ (if (= state last-state)
            (update as :duration inc)
            (-> as
                (put :duration 0)
                (put :kind state)))

        duration (in as :duration)

        tex-kw (cond
                 (= state :throw-end)
                 :hand-throw

                 (= state :throw)
                 :hand-throw

                 #                 (and (> duration 4)
                 #                      (= state :throw-start))
                 #                 :hand-throw

                 :hand-bread)

        t (in images tex-kw)
        scale (case state
                :throw-start 7
                :throw 7
                :throw-end 8

                6)

        [w h] (texture-dimensions t)

        render-x
        (case state
          :throw1236
          (- (- (* 0.5 mw) (* 0.5 (* scale w)))
             (* (- (- (* 0.5 mw) (* 0.5 (* scale w)))
                   (- mw (* scale w)))
                (- 1
                   (ease-in-expo (/ (min duration 1) 1)))))

          (- mw (* scale w)))

        render-y (case state
                   :moving (- mh (* scale h)
                              -30
                              (* 30
                                 (math/sin
                                   (* 2 math/pi (/ (mod duration 40) 40)))))

                   nil (- mh (* scale h)
                          -5
                          (* 5
                             (math/sin
                               (* math/pi (/ (mod duration 60) 60)))))

                   (- mh (* scale h)))]

    (draw-texture-ex
      t
      [(* render-scale render-x)
       (* render-y render-scale)]
      0
      (* render-scale scale)
      :white)))

(defn render-slangbella
  [{:focused? focused?
    :width mw
    :height mh}]
  (let [as (in player :attack-state)
        recovery-duration (in player :recovery-duration)
        moved? (player :moved?)
        duration (in as :duration)
        last-state (get-in player [:attack-state :kind])
        input-state (cond
                      (and focused?
                           (mouse-button-down? 0))
                      :aim

                      (and focused?
                           (mouse-button-released? 0))
                      :shoot

                      :dangle)

        state (cond
                (or
                  (= last-state :shoot-full)
                  (= last-state :shoot-half))
                :recovery

                (and (< duration recovery-duration)
                     (= last-state :recovery))
                :recovery

                (and (= last-state :pull-full)
                     (= input-state :shoot))
                :shoot-full

                (and (= last-state :pull-half)
                     (= input-state :shoot))
                :shoot-half

                (and (= input-state :aim)
                     (= last-state :pull-full))
                :pull-full

                (and
                  (= last-state :pull-half)
                  (= input-state :aim)
                  (>= duration 20))
                :pull-full

                (= input-state :aim)
                :pull-half

                moved?
                :moving

                nil)

        _ (if (= state last-state)
            (update as :duration inc)
            (-> as
                (put :duration 0)
                (put :kind state)))

        tex-kw (cond
                 (= state :pull-full) :pull-full

                 (= state :pull-half) :pull-half

                 (= state :recovery) :slangbella-shoot

                 (= state :moving)
                 (if
                   (< 20 (mod (+ duration 10) 40))
                   :dangle-left
                   :dangle-right)

                 (in as :last-tex)
                 (in as :last-tex)

                 :dangle-right)

        _ (put as :last-tex
               (if (or (= tex-kw :dangle-left)
                       (= tex-kw :dangle-right))
                 tex-kw
                 nil))

        t (in images tex-kw)
        scale (case state
                :recovery (+ 4
                             (ease-in-expo (/ duration recovery-duration)))
                5)
        [w h] (texture-dimensions t)

        render-x
        (case state
          :recovery
          (+ (- mw (* scale w))
             (*
               (- 1 (ease-in-expo
                      (/ (max 0 (- duration 10)) (- recovery-duration 10))))
               (-
                 (- (* mw 0.5) (* scale w 0.5))
                 (- mw (* scale w)))))

          :shoot-full (- (* mw 0.5) (* scale w 0.5))
          :shoot-half (- (* mw 0.5) (* scale w 0.5))

          :pull-full (let [d (min 600
                                  (max 0
                                       (- duration 60)))]
                       (- (* mw 0.5)
                          (* scale w 0.5)
                          (* d -0.05)
                          (* 0.05
                             d
                             (/ (mod duration 7) 7))))

          :pull-half (- (* mw 0.5) (* scale w 0.5))

          (- mw (* scale w)))

        render-y (case state
                   :moving (- mh (* scale h)
                              -30
                              (* 30
                                 (math/sin
                                   (* 2 math/pi (/ (mod duration 40) 40)))))

                   :pull-half (- mh (* scale h)
                                 -5
                                 (* 5
                                    (/ (mod duration 5) 5)))

                   :pull-full (let [d (min 600
                                           (max 0
                                                (- duration 60)))]
                                (- mh (* scale h)
                                   (* d -0.05)
                                   (* 0.05
                                      d
                                      (/ (mod duration 8) 8))))

                   nil (- mh (* scale h)
                          -5
                          (* 5
                             (math/sin
                               (* math/pi (/ (mod duration 60) 60)))))

                   (- mh (* scale h)))]

    (draw-texture-ex
      t
      [(* render-scale render-x)
       (* render-y render-scale)]
      0
      (* render-scale scale)
      :white)))


(defn render
  [el]
  (def {:width mw
        :height mh} el)

  (when (el :focused?)
    (update-camera c)
    (put player :pos (get-camera-position c)))

  (put player :collision/nof-hits 0)

  (loop [e :in enemies]
    (put e :collision/nof-hits 0))

  (put player :moved?
       (not= (in player :pos)
             (in player :last-pos)))

  (when (player :moved?)
    (resume walk-sound-fib))

  (clear-background :white)

  (handle-map-collisions)

  (tick el)

  (def size 0.1)
  (def scale debug-scale)

  (defer (end-mode-3d)
    (begin-mode-3d c)

    (draw-model map-model map-pos 1 :white)
    (do comment
      (var coll false)

      # (log (player :pos))

      (let [{:pos pos :radius p-radius} player
            [px py pz] pos
            p-pos [px pz]]
        (loop [n :in notes]
          (when (n :radius)
            (handle-non-wall-coll player n))

          (when-let [walls (n :walls)]
            (let [{:pos pos} n]
              (loop [w :in walls]
                (loop [e :in enemies
                       :when (and (not (e :dead))
                                  (e :moved))
                       :let [[ex _ ez] (e :pos)]]

                  (when-let [new-pos (handle-wall-coll e
                                                       (->map-pos map-w map-h ex ez)
                                                       w)]

                    (put e :pos new-pos))

                  #
)

                (when-let [new-pos (handle-wall-coll player
                                                     (->map-pos map-w map-h px pz)
                                                     w)]

                  (set-camera-position c new-pos)))))

          (:tick n mw mh))

        (loop [b :in bullets
               :when (not (b :dead))
               :let [{:pos pos
                      :radius b-radius} b
                     [bx by bz] pos
                     b-pos [bx bz]]]

          (when (circle-circle? p-pos
                                p-radius
                                b-pos
                                b-radius)
            (:hit b :player player))

          (loop [e :in enemies
                 :let [{:pos pos
                        :radius e-radius} e
                       [ex ey ez] pos
                       e-pos [ex ez]]
                 :when (not (e :dead))]
            (when (and (< by 0.4)
                       (circle-circle? e-pos
                                       e-radius
                                       b-pos
                                       b-radius))
              (:hit b :enemy e)))))

      (when (< 2 (player :collision/nof-hits))
        (set-camera-position c (player :last-pos)))

      (loop [e :in enemies]
        (when (< 2 (in e :collision/nof-hits))
          (put e :pos (in e :last-pos)))

        (put e :last-pos (e :pos)))

      (when (player :invul)
        (update player :invul dec)

        (when (neg? (player :invul))
          (put player :invul nil)))

      (let [player-pos (get-camera-position c)
            [px py pz] player-pos
            player-pos-2d [px pz]]

        (loop [e :in (sort-by
                       |(- (dist-sqr player-pos (in $ :pos)))
                       enemies)
               :let [{:pos pos
                      :hit hit
                      :blink blink
                      :radius e-radius
                      :attack-range attack-range} e
                     [ex ey ez] pos
                     e-pos [ex ez]]]

          (when (and
                  (= :attack-hit (get-in e [:attack-state :state]))
                  (not (player :invul))
                  (circle-circle? e-pos
                                  attack-range
                                  player-pos-2d
                                  (player :radius)))

            (put player :invul 60)

            (update player :hp - 12)
            #(sound :scream)

            (if (>= 0 (player :hp))
              (do
                (put player :dead true)
                (put player :invul 9999999999)

                (anim
                  (let [dur 10]
                    (loop [f :range [0 dur]
                           :let [p (/ f (dec dur))
                                 p (ease-out-expo p)]]
                      (yield (draw-rectangle
                               0
                               0
                               2000
                               1000
                               [1 0 0 p])))

                    (forever
                      (death-screen mw mh)
                      (yield :ok)))))

              (anim
                (let [dur 30]
                  (loop [_ :range [0 10]]
                    (yield nil))

                  (loop [f :range [0 dur]
                         :let [p (/ f (dec dur))
                               p (- 1 (ease-out-expo p))]]
                    (yield (draw-rectangle
                             0
                             0
                             2000
                             1000
                             [1 0 0 p])))))))

          (if-not (-?> hit pos?)
            (do
              (put e :blink nil)
              (put e :hit nil))
            (do
              (update e :blink-timer dec)

              (when (neg? (e :blink-timer))
                (update e :blink not)
                (put e :blink-timer 5))

              (update e :hit dec)))

          (let [as (in e :attack-state)
                last-state (in as :last-state)

                state (cond
                        (and (> (e :attack-recovery) (as :duration))
                             (= last-state :recovery))
                        :recovery

                        (= last-state :attack-hit)
                        :recovery

                        (and
                          (<= (e :attack-startup) (as :duration))
                          (= last-state :attack-startup))
                        :attack-hit

                        (= last-state :attack-startup)
                        :attack-startup

                        (= :try-attack (in as :input))
                        :attack-startup

                        (= :try-eat (in as :input))
                        :eating

                        (e :moved)
                        :walking

                        :neutral)

                _ (if (= last-state state)
                    (update as :duration inc)
                    (-> as
                        (put :duration 0)
                        (put :state state)))

                rat-tex
                (cond
                  (= state :eating)
                  (images :rat-rest)

                  (e :dead)
                  (images :rat-dead)

                  (and (< 5 (as :duration))
                       (= state :recovery))
                  (images :rat-recovery)

                  (or
                    (= state :recovery)
                    (= state :attack-hit))
                  (images :rat-attack-hit)

                  (= state :attack-startup)
                  (images :rat-attack-startup)

                  (e :moved)
                  (if (< 10 (mod (as :duration) 20))
                    (images :rat-walk-right)
                    (images :rat-walk-left))

                  (images :rat-neutral))

                scale (case state
                        :attack-hit 1.6

                        :recovery
                        (- 1.5
                           (* 1
                              (ease-out-expo
                                (max 0 (/ (- (as :duration) 8) (- (e :attack-recovery) 8))))))

                        :attack-startup
                        (+ 0.5
                           (* 1
                              (ease-out-expo
                                (/ (as :duration) (e :attack-startup)))))

                        0.5)

                y-offset (case state
                           :eating
                           (* 0.01
                              (math/sin (* math/pi (/ (mod (as :duration) 30) 30))))

                           :attack-hit 0.3
                           :recovery
                           (- 0.3
                              (* 0.3
                                 (ease-out-expo
                                   (/ (as :duration) (e :attack-recovery)))))

                           :attack-startup
                           (+ 0
                              (* 0.3
                                 (ease-out-expo
                                   (/ (as :duration) (e :attack-startup))))))]

            (put as :last-state state)

            (draw-billboard c
                            rat-tex
                            (if y-offset
                              [(pos 0)
                               (+ y-offset (pos 1))
                               (pos 2)]

                              pos)
                            scale
                            (if blink
                              :red
                              :white)))))

      (loop [b :in bullets
             :let [{:pos pos
                    :sprite sprite
                    :dir dir
                    :gravity gravity
                    :speed speed} b]]

        (:tick b)))

    # aim-dot
    (comment
      (draw-rectangle (dec (* 0.5 mw))
                      (dec (* 0.5 mh))
                      3
                      3
                      [0 0 0 0.8])

      (draw-rectangle (* 0.5 mw)
                      (* 0.5 mh)
                      1
                      1
                      [1 1 1 0.8])
      #
))
  # (draw-fps 600 0)

  (run-animations anims)

  (when (and (player :holding)
             (not (get-in player [:inventory (player :holding)])))
    (put player :holding-i 0)
    (put player :holding nil))

  (when (or (not (zero? (get-mouse-wheel-move)))
            (key-pressed? :i))
    (update player :holding-i inc)

    (cond (> (player :holding-i)
             (length (player :inventory)))
      (put player :holding-i 0)

      (neg? (player :holding-i))
      (put player :holding-i (length (player :inventory))))

    (if (zero? (player :holding-i))
      (put player :holding nil)
      (put player :holding (get (sort (keys (player :inventory))) (dec (player :holding-i))))))

  (when (not (player :dead))
    (case (player :holding)
      :bread
      (render-holding el)

      :slingshot
      (render-slangbella el)

      (render-hand el)))

  (put player :last-pos (get-camera-position c))

  (draw-rectangle 5 5 100 30 :black)
  (draw-rectangle 6 6 98 28 :white)
  (draw-rectangle 7 7 96 26 :black)
  (draw-rectangle 9 9 (max 0 (math/round (* 92 (/ (player :hp) (player :max-hp)))))
                  22 :red)

  #
  (render-debug-log)
  #


  (when level-to-change-to
    (init-level ;level-to-change-to)
    (set level-to-change-to nil))

  #
)

(import freja/state)
(import freja/events :as e)

(defn frame
  [el]
  (def {:width mw
        :height mh} el)

  (unless (and (= (last-size 0) mw)
               (= (last-size 1) mh))
    (put last-size 0 mw)
    (put last-size 1 mh)

    (when rt
      (unload-render-texture rt))
    (when fullscreen?
      (e/put! state/editor-state :right nil))

    (print "load rt with size: " mw " " mh)

    (set rt (load-render-texture ;(map |(math/floor (* render-scale $)) last-size))))

  (rl-pop-matrix)
  (rl-pop-matrix)

  (defer (do (end-texture-mode)

           (rl-push-matrix)

           # hotfix for translation problems
           (rl-translatef (- (el :render-x) 2) (- (el :render-y) 2) 0)

           (rl-push-matrix))

    (explo/frame explo/explo123)

    (begin-texture-mode rt)

    (render el))

  (draw-texture-pro
    (get-render-texture rt)
    [0 0 (math/floor (* render-scale mw)) (math/floor (* render-scale (- mh)))]
    [0 0
     (+ mw 2) (+ mh 2)]
    [0 0]
    0
    :white)
  #
)

(def floor-1
  {:spawn-points {:cell [-14.5 1 -14.5]
                  :stairs [-10.5
                           1
                           13.599]
                  :stairs-to-floor-2 '(-4.43839 1.00569 13.1434)}

   :map "resources/map.png"
   :map-texture :cubicmap_atlas2

   :init (fn [self]
           (def enemy-map (load-image-1 "resources/enemy-map.png"))
           (def enemy-map-pixels (get-image-data enemy-map))

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not= [1 1 1 1]
                                   (enemy-map-pixels (+ (* y map-w)
                                                        x))))]
             (array/push enemies (-> (table/clone rat)
                                     (put :pos @[(- x -0.5 (* 0.5 map-w))
                                                 0.2
                                                 (- y -0.5 (* 0.5 map-w))])
                                     (put :last-pos @[(- x -0.5 (* 0.5 map-w))
                                                      0.2
                                                      (- y -0.5 (* 0.5 map-w))])
                                     (put :attack-state @{:duration 0}))))

           (do
             (array/clear notes)

             (array/push notes
                         @{:tick handle-note
                           :dir [0 0 1]
                           :timer 0
                           :text `
These rats are too scary. :(
We've gotta do something.
 - Todd
`
                           :pos [-10.41
                                 1
                                 -12.5]})

             (array/push notes
                         @{:tick handle-note
                           :dir [1 0 0]
                           :timer 0
                           :text `
I hid the walnut bread in the cabinet.
Just so the rats wouldn't get it.
It smells so good.
 - Caitlyn
`
                           :pos [-5.41
                                 1
                                 -9]})

             #### trapp till floor-2

             (array/push
               notes
               @{:dir [1 0 0]
                 :timer 0
                 :text `
I'm free!
`

                 :pos '(-4.5 1 11.5)

                 :tick (fn [note mw mh]
                         (let [{:pos pos
                                :text text
                                :dir dir
                                :timer timer
                                :last-tex last-tex} note
                               close (> 0.1 (dist-sqr
                                              pos
                                              (v+
                                                (in player :pos)
                                                (v* (cam-rot c)
                                                    0.5))))

                               _ (when (neg? timer)
                                   (put note :timer 5))

                               tex (if (or (nil? last-tex)
                                           (neg? timer))
                                     (if
                                       (< 0.3 (math/random))
                                       (images :trapp)
                                       (images :trapp))

                                     last-tex)]

                           (put note :last-tex tex)
                           (update note :timer dec)

                           (draw-cube-texture
                             tex
                             pos
                             1
                             2
                             0.2
                             :white)

                           (when close
                             (anim
                               (loop [i :range [0 60]]
                                 (when (= i 30) (change-level :floor-2 :stairs))
                                 (yield
                                   (draw-rectangle
                                     0
                                     0
                                     2000
                                     1000
                                     [0 0 0.2
                                      (math/sin (* math/pi
                                                   (min 1 (/ i 60))))])))

                               #
))))})

             #### trapp till floor0

             (array/push notes
                         @{:dir [1 0 0]
                           :timer 0
                           :text `
I'm free!
`
                           :pos [-10.5
                                 1
                                 14.599]

                           :tick (fn [note mw mh]
                                   (let [{:pos pos
                                          :text text
                                          :dir dir
                                          :timer timer
                                          :last-tex last-tex} note
                                         close (> 0.1 (dist-sqr
                                                        pos
                                                        (v+
                                                          (in player :pos)
                                                          (v* (cam-rot c)
                                                              0.5))))

                                         _ (when (neg? timer)
                                             (put note :timer 5))

                                         tex (if (or (nil? last-tex)
                                                     (neg? timer))
                                               (if
                                                 (< 0.3 (math/random))
                                                 (images :trapp)
                                                 (images :trapp))

                                               last-tex)]

                                     (put note :last-tex tex)
                                     (update note :timer dec)

                                     (draw-cube-texture
                                       tex
                                       pos
                                       1
                                       2
                                       0.2
                                       :white)

                                     (when close
                                       (anim
                                         (loop [i :range [0 60]]
                                           (when (= i 30) (change-level :floor0 :stairs))
                                           (yield
                                             (draw-rectangle
                                               0
                                               0
                                               2000
                                               1000
                                               [0 0 0.2
                                                (math/sin (* math/pi
                                                             (min 1 (/ i 60))))])))

                                         #
))

                                     #                             
))})

             # cabinet

             (array/push notes
                         (table/setproto
                           @{:pos [-12.5 0.5 -7.5]
                             :content :bread
                             :pickup (fn [_ player content]
                                       (gain-hope player 2)
                                       (flash-text
                                         ``
Ooh, fresh bread!
                       ``
                                         (/ render-width 2)
                                         (/ render-height 1.5)
                                         24
                                         :player-says))}
                           cabinet))

             (array/push notes
                         (table/setproto
                           @{:pos [-0.44 0.5 1.24]
                             :content :lockpick
                             :pickup (fn [_ player content]
                                       (gain-hope player 1)
                                       (flash-text
                                         ``
Didn't think I'd see a lockpick again...
``
                                         (/ render-width 2)
                                         (/ render-height 1.5)
                                         24
                                         :player-says))}
                           cabinet))

             # first bars
             (array/push notes
                         @{:pos [-13.4 1 -14.1]

                           :walls [[;(map math/ceil (->map-pos map-w map-h -13.4 -14.1))
                                    1
                                    1]]

                           :rotation 0

                           :tick
                           (fn [self mw mh]
                             (let [{:pos pos
                                    :rotation rot} self
                                   close (> 1 (dist-sqr
                                                pos
                                                (v+
                                                  (in player :pos)
                                                  (v* (cam-rot c)
                                                      0.5))))]

                               (defer (rl-pop-matrix)
                                 (rl-push-matrix)

                                 (rl-translatef ;pos)

                                 (rl-translatef 0 0 (* 2.5 0.2))

                                 (rl-rotatef rot 0 1 0)

                                 (loop [i :range [0 5]]

                                   (draw-cube-texture
                                     (in images :bar)

                                     [0 0 (* i -0.2)]

                                     0.1
                                     2
                                     0.1

                                     (if (and (self :walls) close)
                                       :white
                                       :gray))))

                               (when (and close
                                          (self :walls)
                                          (= :touch (get-in player [:attack-state :kind])))

                                 (do
                                   (put-in player [:inventory :lockpick] nil)

                                   (flash-text
                                     ``
It's not even locked...
``
                                     (/ mw 2)
                                     (/ mh 1.5)
                                     24
                                     :player-says)

                                   (anim
                                     (loop [_ :range [0 10]]
                                       (yield nil))

                                     (put self :walls false)

                                     (loop [i :range [0 60]]
                                       (print (self :rot))
                                       (put self :rotation (* 95 (ease-in-out (/ i 59))))
                                       (yield nil))

                                     (gain-hope player 1)))))

                             #
)})

             # bars
             (array/push notes
                         @{:pos [-10.7 1 0]

                           :walls [[;(map math/ceil (->map-pos map-w map-h -10 0))
                                    1
                                    1]
                                   [;(map math/ceil (->map-pos map-w map-h -11 0))
                                    1
                                    1]]

                           :rotation 0

                           :tick
                           (fn [self mw mh]
                             (let [{:pos pos
                                    :rotation rot} self
                                   close (> 1 (dist-sqr
                                                pos
                                                (v+
                                                  (in player :pos)
                                                  (v* (cam-rot c)
                                                      0.5))))]

                               (player :pos)

                               (defer (rl-pop-matrix)
                                 (rl-push-matrix)

                                 (rl-translatef ;pos)

                                 (rl-translatef (* 2.5 0.4) 0 0)

                                 (rl-rotatef rot 0 1 0)

                                 (loop [i :range [0 5]]

                                   (draw-cube-texture
                                     (in images :bar)

                                     [(* i -0.4) 0 0]

                                     0.1
                                     2
                                     0.1

                                     (if (and close
                                              (self :walls))
                                       :white
                                       :gray))))

                               (when (and close
                                          (self :walls)
                                          (= :touch (get-in player [:attack-state :kind])))
                                 (if (get-in player [:inventory :lockpick])
                                   (do
                                     (put-in player [:inventory :lockpick] nil)

                                     (flash-text
                                       ``
Let's see...
``
                                       (/ mw 2)
                                       (/ mh 1.5)
                                       24
                                       :player-says)

                                     (anim
                                       (loop [_ :range [0 180]]
                                         (yield nil))

                                       (flash-text
                                         ``
Yes!
``
                                         (/ mw 2)
                                         (/ mh 1.5)
                                         24
                                         :player-says)

                                       (put self :walls nil)

                                       (loop [i :range [0 60]]
                                         (print (self :rot))
                                         (put self :rotation (* 95 (ease-in-out (/ i 59))))
                                         (yield nil))

                                       (gain-hope player 3)))
                                   (flash-text
                                     ``
It's locked.
Jeez!
``
                                     (/ mw 2)
                                     (/ mh 1.5)
                                     24
                                     :player-says))))

                             #
)})

             # bars to floor-2
             (array/push notes
                         @{:pos '(-5.99988 1 13.9)

                           :walls [[;(map math/ceil (->map-pos map-w map-h -6 14))
                                    1
                                    1]]

                           :rotation 0

                           :tick
                           (fn [self mw mh]
                             (let [{:pos pos
                                    :rotation rot} self
                                   close (> 1 (dist-sqr
                                                pos
                                                (v+
                                                  (in player :pos)
                                                  (v* (cam-rot c)
                                                      0.5))))]

                               (player :pos)

                               (defer (rl-pop-matrix)
                                 (rl-push-matrix)

                                 (rl-translatef ;pos)

                                 (rl-translatef 0 0 (* 2.5 0.2))

                                 (rl-rotatef rot 0 1 0)

                                 (loop [i :range [0 5]]

                                   (draw-cube-texture
                                     (in images :bar)

                                     [0 0 (* i -0.2)]

                                     0.1
                                     2
                                     0.1

                                     (if (and (self :walls) close)
                                       :white
                                       :gray))))

                               (when (and close
                                          (self :walls)
                                          (= :touch (get-in player [:attack-state :kind])))
                                 (if (get-in player [:inventory :lockpick])
                                   (do
                                     (put-in player [:inventory :lockpick] nil)

                                     (flash-text
                                       ``
Let's see...
``
                                       (/ mw 2)
                                       (/ mh 1.5)
                                       24
                                       :player-says)

                                     (anim
                                       (loop [_ :range [0 180]]
                                         (yield nil))

                                       (flash-text
                                         ``
Yes!
``
                                         (/ mw 2)
                                         (/ mh 1.5)
                                         24
                                         :player-says)

                                       (put self :walls nil)

                                       (loop [i :range [0 60]]
                                         (print (self :rot))
                                         (put self :rotation (* 95 (ease-in-out (/ i 59))))
                                         (yield nil))

                                       (gain-hope player 3)))
                                   (flash-text
                                     ``
It's locked.
Jeez!
``
                                     (/ mw 2)
                                     (/ mh 1.5)
                                     24
                                     :player-says))))

                             #
)})

             :ok

             #
))})

(put levels :floor-1 floor-1)


(def floor-2
  {:spawn-points {:stairs [8.41213 1 -4.39485]
                  #                  [0 1 2]
}
   :map "resources/floor-2-map.png"
   :map-texture :cubicmap_atlas2

   :init (fn [self]

           #### trapp

           (array/push
             notes
             @{:dir [1 0 0]
               :timer 0
               :text `
I'm free!
`
               :pos [8.41213 1 -5.59485]

               :tick (fn [note mw mh]
                       (let [{:pos pos
                              :text text
                              :dir dir
                              :timer timer
                              :last-tex last-tex} note
                             close (> 0.3 (dist-sqr
                                            pos
                                            (v+
                                              (in player :pos)
                                              (v* (cam-rot c)
                                                  0.5))))

                             _ (when (neg? timer)
                                 (put note :timer 5))

                             tex (if (or (nil? last-tex)
                                         (neg? timer))
                                   (if
                                     (< 0.3 (math/random))
                                     (images :trapp)
                                     (images :trapp))

                                   last-tex)]

                         (put note :last-tex tex)
                         (update note :timer dec)

                         (draw-cube-texture
                           tex
                           pos
                           1
                           2
                           0.2
                           :white)

                         (when close
                           (anim
                             (loop [i :range [0 60]]
                               (when (= i 30) (change-level :floor-1 :stairs-to-floor-2))
                               (yield
                                 (draw-rectangle
                                   0
                                   0
                                   2000
                                   1000
                                   [0 0 0.2
                                    (math/sin (* math/pi
                                                 (min 1 (/ i 60))))])))

                             #
))))})

           (def enemy-map (load-image-1 "resources/floor-2-enemies.png"))
           (def enemy-map-pixels (get-image-data enemy-map))

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not= [1 1 1 1]
                                   (enemy-map-pixels (+ (* y map-w)
                                                        x))))]
             (array/push enemies (-> (table/clone rat)
                                     (put :pos @[(- x -0.5 (* 0.5 map-w))
                                                 0.2
                                                 (- y -0.5 (* 0.5 map-w))])
                                     (put :attack-state @{:duration 0}))))

           (def lockpick-map (load-image-1 "resources/floor-2-lockpicks.png"))
           (def lockpick-map-pixels (get-image-data lockpick-map))

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not= [1 1 1 1]
                                   (lockpick-map-pixels (+ (* y map-w)
                                                           x))))]

             (array/push notes
                         (table/setproto
                           @{:pos [(- x -0.5 (* 0.5 map-w))
                                   0.5
                                   (- y -0.5 (* 0.5 map-w))]
                             :content :lockpick
                             :pickup (fn [_ player content]
                                       (flash-text
                                         ``
A lockpick!
``
                                         (/ render-width 2)
                                         (/ render-height 1.5)
                                         24
                                         :player-says))}
                           cabinet)))

           (def breads-map (load-image-1 "resources/floor-2-breads.png"))
           (def breads-map-pixels (get-image-data breads-map))

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not= [1 1 1 1]
                                   (breads-map-pixels (+ (* y map-w)
                                                         x))))]

             (array/push notes
                         (table/setproto
                           @{:pos [(- x -0.5 (* 0.5 map-w))
                                   0.5
                                   (- y -0.5 (* 0.5 map-w))]
                             :content :bread
                             :pickup (fn [_ player content]
                                       (flash-text
                                         ``
Bread!
``
                                         (/ render-width 2)
                                         (/ render-height 1.5)
                                         24
                                         :player-says))}
                           cabinet)))

           (def slingshots-map (load-image-1 "resources/floor-2-slingshots.png"))
           (def slingshots-map-pixels (get-image-data slingshots-map))

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not= [1 1 1 1]
                                   (slingshots-map-pixels (+ (* y map-w)
                                                             x))))]

             (array/push notes
                         (table/setproto
                           @{:pos [(- x -0.5 (* 0.5 map-w))
                                   0.5
                                   (- y -0.5 (* 0.5 map-w))]
                             :content :slingshot
                             :pickup (fn [_ player content]
                                       (flash-text
                                         ``
A slingshot!
``
                                         (/ render-width 2)
                                         (/ render-height 1.5)
                                         24
                                         :player-says))}
                           cabinet)))

           (def gates-map (load-image-1 "resources/floor-2-gates.png"))
           (def gates-map-pixels (get-image-data gates-map))

           (var taken-walls @{})

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not (get taken-walls [x y]))
                             (not= [1 1 1 1]
                                   (gates-map-pixels (+ (* y map-w)
                                                        x))))]

             (var walls @[[x y 1 1]])

             (loop [x2 :range-to [(max 0 (dec x)) (min map-w (inc x))]
                    y2 :range-to [(max 0 (dec y)) (min map-h (inc y))]
                    :when (and (not (and (= x x2) (= y y2)))
                               (not= [1 1 1 1]
                                     (gates-map-pixels (+ (* y2 map-w)
                                                          x2))))]

               (put taken-walls [x2 y2] true)
               (array/push walls [x2 y2 1 1]))

             (array/push notes
                         @{:pos [(- x -0.5 (* 0.5 map-w))
                                 0.5
                                 (- y -0.5 (* 0.5 map-w))]

                           :walls walls

                           :rotation 0

                           :tick
                           (fn [self mw mh]
                             (let [{:pos pos
                                    :rotation rot} self
                                   close (> 1.5 (dist-sqr
                                                  pos
                                                  (v+
                                                    (in player :pos)
                                                    (v* (cam-rot c)
                                                        0.5))))]

                               (defer (rl-pop-matrix)
                                 (rl-push-matrix)

                                 (rl-translatef ;pos)

                                 (rl-translatef (* 2.5 0.4) 0 0)

                                 (rl-rotatef rot 0 1 0)

                                 (loop [i :range [0 5]]

                                   (draw-cube-texture
                                     (in images :bar)

                                     [(* i -0.4) 0 0]

                                     0.1
                                     2
                                     0.1

                                     (if (and close
                                              (self :walls))
                                       :white
                                       :gray))))

                               (when (and close
                                          (self :walls)
                                          (= :touch (get-in player [:attack-state :kind])))
                                 (if (get-in player [:inventory :lockpick])
                                   (do
                                     (put-in player [:inventory :lockpick] nil)

                                     (flash-text
                                       ``
Let's see...
``
                                       (/ mw 2)
                                       (/ mh 1.5)
                                       24
                                       :player-says)

                                     (anim
                                       (loop [_ :range [0 180]]
                                         (yield nil))

                                       (flash-text
                                         ``
Yes!
``
                                         (/ mw 2)
                                         (/ mh 1.5)
                                         24
                                         :player-says)

                                       (put self :walls nil)

                                       (loop [i :range [0 60]]
                                         (print (self :rot))
                                         (put self :rotation (* 95 (ease-in-out (/ i 59))))
                                         (yield nil))

                                       (gain-hope player 3)))
                                   (flash-text
                                     ``
It's locked.
Jeez!
``
                                     (/ mw 2)
                                     (/ mh 1.5)
                                     24
                                     :player-says))))

                             #
)}))

           (def vials-map (load-image-1 "resources/floor-2-vials.png"))
           (def vials-map-pixels (get-image-data vials-map))

           (loop [x :range [0 map-w]
                  y :range [0 map-h]
                  :when (and (not= [1 1 1 1]
                                   (vials-map-pixels (+ (* y map-w)
                                                        x))))]

             (array/push notes
                         (table/setproto
                           @{:pos [(- x -0.5 (* 0.5 map-w))
                                   0.5
                                   (- y -0.5 (* 0.5 map-w))]
                             :content :vial
                             :pickup (fn [_ player content]
                                       (flash-text
                                         ``
Finally, the vial!
``
                                         (/ render-width 2)
                                         (/ render-height 1.5)
                                         24
                                         :player-says))}
                           cabinet)))

           #
)})

(put levels :floor-2 floor-2)


(defn converse
  [talker convo]

  (anim
    (var next false)

    (let [t ``
Hello?
``
          size 24
          w (fj/measure-text t size)
          nof-newlines (length (string/find-all "\n" t))]

      (while (not next)

        (when (mouse-button-pressed? 0)
          (set next true))

        (yield
          (fj/draw-text
            t
            (math/floor (- (/ render-width 2) (/ w 2)))
            (math/floor (- (/ render-height 2) (/ size 2)))
            size
            :white))))

    (var next false)

    (let [t ``
Hi! Here's a lockpick. Please fetch my vial.
``
          size 24
          w (fj/measure-text t size)
          nof-newlines (length (string/find-all "\n" t))]

      (put-in player [:inventory :lockpick] true)

      (while (not next)

        (when (mouse-button-pressed? 0)
          (set next true))

        (yield
          (fj/draw-text
            t
            (math/floor (- (/ render-width 2) (/ w 2)))
            (math/floor (- (/ render-height 2) (/ size 2)))
            size
            :white)))))

  #
)


(defn init-floor0
  [self]
  (print "inited floor0")

  # doctor


  (array/push
    notes
    @{:dir [1 0 0]
      :timer 0
      :text `
I'm free!
`
      :pos [-12.9403 0.70483 13.4632]

      :radius 0.5

      :tick (fn [note mw mh]
              (let [{:pos pos
                     :text text
                     :dir dir
                     :timer timer
                     :last-tex last-tex} note
                    close (> 1 (dist-sqr
                                 pos
                                 (v+
                                   (in player :pos)
                                   (v* (cam-rot c)
                                       0.5))))

                    _ (when (neg? timer)
                        (put note :timer 5))

                    tex

                    (if close
                      (images :doctor-open-mouth)
                      (images :doctor))]

                (put note :last-tex tex)
                (update note :timer dec)

                (draw-billboard
                  c
                  tex
                  pos
                  0.7
                  :white)

                (when (and close
                           (= :touch (get-in player [:attack-state :kind])))
                  (converse self :doctor)
                  #
)))})

  #### trapp till floor-1

  (array/push
    notes
    @{:dir [1 0 0]
      :timer 0
      :text `
I'm free!
`
      :pos [-5 1 21.59]

      :tick (fn [note mw mh]
              (let [{:pos pos
                     :text text
                     :dir dir
                     :timer timer
                     :last-tex last-tex} note
                    close (> 0.1 (dist-sqr
                                   pos
                                   (v+
                                     (in player :pos)
                                     (v* (cam-rot c)
                                         0.5))))

                    _ (when (neg? timer)
                        (put note :timer 5))

                    tex (if (or (nil? last-tex)
                                (neg? timer))
                          (if
                            (< 0.3 (math/random))
                            (images :trapp)
                            (images :trapp))

                          last-tex)]

                (put note :last-tex tex)
                (update note :timer dec)

                (draw-cube-texture
                  tex
                  pos
                  1
                  2
                  0.2
                  :white)

                (when close
                  (anim
                    (loop [i :range [0 60]]
                      (when (= i 30) (change-level :floor-1 :stairs))
                      (yield
                        (draw-rectangle
                          0
                          0
                          2000
                          1000
                          [0 0 0.2
                           (math/sin (* math/pi
                                        (min 1 (/ i 60))))])))

                    #
))))})

  ###
)


(def floor0
  {:spawn-points {:stairs [-5 1 20.5]}
   :map "resources/floor0.png"
   :map-texture :cubicmap_atlas2

   :init init-floor0})

(put levels :floor0 floor0)


#(put-in player [:inventory :lockpick] true)

(var wait-for-reload false)

(comment
  (start-game {:left fullscreen?
               :render (fn [el]
                         (unless wait-for-reload
                           (try
                             (frame el)
                             ([err fib]
                               (set wait-for-reload true)
                               (debug/stacktrace fib err)))))})
  #
)

(defn init
  []

  (set-target-fps 60)

  (init-audio-device)

  (set rt (load-render-texture 0 0))

  (merge-into images
              (->> (os/dir "resources")
                   (filter |(= ".png" (path/ext $)))
                   (map (fn [path]
                          [(keyword
                             (string/slice path 0 -5))
                           (load-texture (path/join "resources" path))]))
                   from-pairs))

  (merge-into sounds
              (-> @{:notice1 (load-sound "resources/notice1.wav")
                    :notice (load-sound "resources/notice2.wav")
                    :running (load-sound "resources/running.wav")
                    :scream (load-sound "resources/scream.wav")}

                  (merge-into
                    (->> (os/dir "resources")
                         (filter |(= ".ogg" (path/ext $)))
                         (map (fn [path]
                                [(keyword
                                   (string/slice path 0 -5))
                                 (load-sound (path/join "resources" path))]))
                         from-pairs))

                  (merge-into
                    (->> (os/dir "resources/music")
                         (filter |(= ".ogg" (path/ext $)))
                         (map (fn [path]
                                [(keyword
                                   "music/"
                                   (string/slice path 0 -5))
                                 (load-music-stream (path/join "resources/music" path))]))
                         from-pairs))

                  (merge-into
                    (->> (os/dir "resources/steps")
                         (filter |(= ".ogg" (path/ext $)))
                         (map (fn [path]
                                [(keyword
                                   "steps/"
                                   (string/slice path 0 -5))
                                 (load-sound (path/join "resources" "steps" path))]))
                         from-pairs))))

  (set c
       (let [c
             (camera-3d :target [0 1 0]
                        :up [0 1 0]
                        :fovy 65
                        :type :perspective
                        :position [0 1 -2])]

         (set-camera-mode c :first-person)

         c))

  (play-music-stream (sounds :music/ljusare-vind))
  (play-music-stream (sounds :music/low-vind-reverb))

  (do
    (array/clear anims)
    (anim
      (var i 500)
      (def limit 700)
      (def m (sounds :music/ljusare-vind))
      (forever
        (++ i)

        (when (> i limit)
          (set i 0))

        (set-music-volume m (+ 0.2
                               (* 0.2
                                  (math/sin (* math/pi (/ i limit))))))

        (yield (update-music-stream m))))

    (anim
      (var i 0)
      (def limit 900)
      (def m (sounds :music/low-vind-reverb))
      (forever
        (++ i)

        (when (> i limit)
          (set i 0))

        (set-music-volume m (+ 0.2
                               (* 0.2
                                  (math/sin (* math/pi (/ i limit))))))

        (yield (update-music-stream m)))))

  #(init-level :floor-2 :stairs)
  (init-level :floor-1 :cell)
  #(init-level :floor0 :stairs)
)


(defn main
  [& _]
  (init-window 800 600 "Rats")

  (explo/init)
  (init)

  (while (not (window-should-close))
    (begin-drawing)
    (clear-background :white)
    (frame {:width (get-screen-width)
            :height (get-screen-height)
            :render-x 0
            :render-y 0
            :focused? true})
    (end-drawing))

  (close-window))
#


# (main)

