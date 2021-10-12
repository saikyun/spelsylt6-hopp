(use freja/flow)
(import freja-jaylib :as fj)

(import ../freja/freja/vector-math :prefix "")
(import ./coll :prefix "")
(import ./exp :as explo)

(import spork/path)

(defonce inited-audio
  (do (init-audio-device)
    :ok))

(defonce sounds
  @{:notice1 (load-sound "resources/notice1.wav")
    :notice (load-sound "resources/notice2.wav")
    :running (load-sound "resources/running.wav")
    :scream (load-sound "resources/scream.wav")})

(defonce images
  (->> (os/dir "resources")
       (filter |(= ".png" (path/ext $)))
       (map (fn [path]
              [(keyword
                 (string/slice path 0 -5))
               (load-texture (path/join "resources" path))]))
       from-pairs))


(defn sound
  [k]
  (play-sound (in sounds k)))

(def render-scale 1)
(def debug-scale 10)

(def debug-logs @[])

(set-target-fps 60)

(def anims @[])

(varfn death-screen
  [mw mh]
  (draw-rectangle
    0
    0
    1000
    1000
    [1 0 0 1])

  (let [size (math/floor (* render-scale 154))
        w (fj/measure-text "You died" size)]
    (fj/draw-text "You died"
                  (math/floor (- (/ (* render-scale mw) 2) (/ w 2)))
                  (math/floor (- (/ (* render-scale mh) 2) (/ size 2)))
                  size
                  :black)))

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

(defonce c
  (let [c (camera-3d :target [0 0 0]
                     :up [0 1 0]
                     :fovy 45
                     :type :perspective
                     :position [7 1 7])]

    (set-camera-mode c :first-person)
    c))

(disable-cursor)

(defonce im-map (load-image-1 "resources/cubicmap.png"))
(defonce cubicmap (load-texture-from-image im-map))
(defonce mesh (gen-mesh-cubicmap im-map [1 2 1]))
(defonce model (load-model-from-mesh mesh))

(defonce tex (load-texture "resources/cubicmap_atlas.png"))
(defonce mat (get-model-material model 0))
(set-material-texture mat tex :diffuse)

(defonce map-pixels (get-image-data im-map))
# (unload-image im-map)

(def map-pos @[-16 0 -8])

(def [map-w map-h] (image-dimensions im-map))

(defonce bill (load-texture "resources/halm.png"))

(defonce bullet (load-texture "resources/bullet.png"))

(def last-size @[nil nil])
(var rt (load-render-texture 0 0))

(def bullets @[])
(def enemies @[])

(def player @{:pos (get-camera-position c)
              :recovery-duration 23
              :last-pos (get-camera-position c)
              :radius 0.5
              :attack-state @{:duration 0}
              :hp 38})


#### START OF FUNCS


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
      (= [1 1 1 1]
         (map-pixels (+ (* (math/floor y) map-w)
                        (math/floor x))))
      (math/floor x) (math/floor y)
      (array/push colls [(math/floor x) (math/floor y)])))
  colls)

(defn aquire-target
  [e]

  (if (e :aquiring)
    (if-not (empty? (walls-between e (e :aquiring)))
      (do
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

          (sound :running))))

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
  (if (e :dead)
    (dying-npc e)
    (do
      (unless (e :target)
        (aquire-target e))

      (when (e :target)
        (def wb (walls-between e (e :target)))
        (if-not (empty? wb)

          (put e :target nil)

          (when-let [t (e :target)]
            (put e :target-pos (t :pos)))))

      (let [{:target t
             :pos pos
             :speed speed
             :attack-range attack-range} e

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

          (if (and distance
                   (e :target)
                   (< distance attack-range))
            (:attack e (e :target))
            (update e :pos v+ (v* (e :dir) speed))))))))

(array/push enemies @{:pos @[1 0.2 1]
                      :dir @[0 0 0]
                      :hp 28
                      :attack-range 1
                      :attack (fn [& args]
                                #TODO: attack code
)
                      :speed 0.04
                      :radius 0.5
                      :blink 0
                      :update aggressive-melee-npc})

(varonce last-coll nil)

(defn cam-rot
  [c]
  (normalize
    (v-
      (get-camera-target c)
      (get-camera-position c))))

(defn tick
  [el]
  (def [x y z] (player :pos))
  (def attack-kind (get-in player [:attack-state :kind]))

  (when (el :focused?)
    (when (or (= attack-kind :shoot-full)
              (= attack-kind :shoot-half)) # (mouse-button-released? 0)
      (def b @{:pos (array ;(player :pos))
               :sprite bullet
               :damage 13
               :gravity 0
               :dir (cam-rot c)
               :speed (if (= attack-kind :shoot-full)
                        0.25
                        0.125)
               :color :green})

      (update b :pos v+ (v* (cam-rot c) 1))

      (update-in b [:pos 1] - 0.07)
      (array/push bullets b)))

  (loop [e :in enemies]
    (:update e))

  (var i 0)

  (while (< i (length bullets))

    (if ((in bullets i) :dead)
      (++ i) ##1 # (array/remove bullets i)
      (++ i))))

(defn handle-wall-coll
  [o o-rec wall]
  (when (check-collision-circle-rec
          o-rec
          0.25
          wall)

    (update o :collision/nof-hits inc)

    (let [[ox oy oz] (in o :pos)]
      (def angle (v- o-rec wall))

      (def x-diff (- (+ 0.5 0.25) (math/abs (- (o-rec 0) (wall 0)))))

      (def x-diff
        (if (neg? (angle 0))
          (- x-diff)
          x-diff))

      (def z-diff (- (+ 0.5 0.25) (math/abs (- (o-rec 1) (wall 1)))))

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

(defn handle-map-collisions
  []
  (let [scale 10
        player-pos (get-camera-position c)
        [px py pz] player-pos
        [pmx pmy] (->map-pos map-w map-h px pz)]

    ## drawing map
    #
    (do comment
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

    (do comment
      #draw player
      (draw-circle
        (math/round (* scale (+ pmx 0.5)))
        (math/round (* scale (+ pmy 0.5)))
        (* scale 0.5)
        :blue))

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
        :white))

    #
    ## end of drawing map

    (put player :collision/nof-hits 0)

    (loop [e :in enemies]
      (put e :collision/nof-hits 0))

    (loop [x :range [0 map-w]
           y :range [0 map-h]
           :when (and (= [1 1 1 1]
                         (map-pixels (+ (* y map-w)
                                        x))))
           :let [tile-rec [x y 1 1]]]

      (let [bullet-size 0.1]
        (loop [b :in bullets
               :let [{:pos pos} b
                     [bx by bz] pos]
               :when
               (check-collision-circle-rec
                 (->map-pos map-w map-h bx bz)
                 bullet-size

                 tile-rec)]

          (put b :dead true)))

      (when-let [new-pos (handle-wall-coll
                           player
                           (->map-pos map-w map-h px pz)
                           tile-rec)]

        (set-camera-position c new-pos)

        (draw-rectangle (* scale x)
                        (* scale y)
                        scale
                        scale
                        :green))

      (loop [e :in enemies
             :let [[ex ey ez] (in e :pos)]]
        (when-let
          [new-pos (handle-wall-coll
                     e
                     (->map-pos map-w map-h ex ez)
                     tile-rec)]

          (put e :pos new-pos)

          (draw-rectangle (* scale x)
                          (* scale y)
                          scale
                          scale
                          :green))))

    (when (< 2 (player :collision/nof-hits))
      (set-camera-position c (player :last-pos)))

    (loop [e :in enemies]
      (when (< 2 (in e :collision/nof-hits))
        (put e :pos (in e :last-pos))))))

(defn render
  [el]
  (def {:width mw
        :height mh} el)

  (when (el :focused?)
    (update-camera c)
    (put player :pos (get-camera-position c)))

  (clear-background :white)

  (tick el)

  (draw-rectangle-rec [0 0 33 10] :gray)

  (def size 0.1)
  (def scale debug-scale)

  (defer (end-mode-3d)
    (begin-mode-3d c)

    (draw-model model map-pos 1 :white)

    (var coll false)

    (loop [b :in bullets
           :when (not (b :dead))
           :let [{:damage damage
                  :pos pos} b
                 [bx by bz] pos
                 b-pos [bx
                        bz]]]
      (loop [e :in enemies
             :let [{:pos pos
                    :radius e-radius} e
                   [ex ey ez] pos
                   e-pos [ex ez]]
             :when (not (e :dead))]
        (when (circle-circle? e-pos
                              e-radius
                              b-pos
                              size)
          (update e :hp - damage)
          (when (>= 0 (e :hp))
            (put e :dead true))
          (put e :hit 20)
          (put e :blink-timer 5)
          (put b :dead true))))

    (when (player :invul)
      (update player :invul dec)

      (when (neg? (player :invul))
        (put player :invul nil)))

    (let [player-pos (get-camera-position c)
          [px py pz] player-pos
          player-pos-2d [px pz]]

      (loop [e :in enemies
             :let [{:pos pos
                    :hit hit
                    :blink blink
                    :radius e-radius} e
                   [ex ey ez] pos
                   e-pos [ex ez]]]

        (when (and
                (not (player :invul))
                (circle-circle? e-pos
                                e-radius
                                player-pos-2d
                                (player :radius)))

          (put player :invul 60)

          (update player :hp - 12)
          (sound :scream)

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
                             1000
                             1000
                             [1 0 0 p])))

                  (forever
                    (death-screen mw mh)
                    (yield :ok)))))

            (anim
              (let [dur 30]
                (loop [f :range [0 dur]
                       :let [p (/ f (dec dur))
                             p (- 1 (ease-out-expo p))]]
                  (yield (draw-rectangle
                           0
                           0
                           1000
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

        (draw-billboard c bill
                        pos 0.5 (if blink
                                  :red
                                  :white))))

    (loop [b :in bullets
           :let [{:pos pos
                  :sprite sprite
                  :dir dir
                  :gravity gravity
                  :speed speed} b]]

      (cond
        (b :dead)
        (draw-billboard c (get-render-texture explo/rt)
                        pos 0.5 :white)

        (neg? (pos 1))
        (put b :dead true)

        (do
          (update b :pos v+ (v* dir speed))
          (update-in b [:pos 1] - gravity)
          (update b :gravity + 0.002)
          (draw-billboard c sprite pos 0.1 :white)))))

  (handle-map-collisions)

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

  (draw-fps 600 0)

  (run-animations anims)

  (let [as (in player :attack-state)
        recovery-duration (in player :recovery-duration)
        moved? (not= (in player :pos)
                     (in player :last-pos))
        duration (in as :duration)
        last-state (get-in player [:attack-state :kind])
        input-state (cond
                      (and (el :focused?)
                           (mouse-button-down? 0))
                      :aim

                      (and (el :focused?)
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
      [render-x
       render-y]
      0
      scale
      :white))

  (put player :last-pos (get-camera-position c))

  #
  (render-debug-log)
  #


  #
)

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
     mw mh]
    [0 0]
    0
    :white)
  #
)

(start-game {:render (fn [el]
                       (try
                         (frame el)
                         ([err fib]
                           (debug/stacktrace fib err))))})
