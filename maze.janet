(use freja/flow)
(import freja-jaylib :as fj)

(import ../freja/freja/vector-math :prefix "")
(import ./coll :prefix "")
(import ./exp :as explo)


(def render-scale 1)

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

  (first args))

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

(defonce c
  (let [c (camera-3d :target [0 0 0]
                     :up [0 1 0]
                     :fovy 45
                     :type :perspective
                     :position [0.2 0.4 0.2])]

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

(def [map-w map-h] (get-image-dimensions im-map))

(defonce bill (load-texture "resources/halm.png"))

(defonce bullet (load-texture "resources/bullet.png"))

(def last-size @[nil nil])
(var rt (load-render-texture 0 0))

(def bullets @[])
(def enemies @[])

(def player @{:pos (get-camera-position c)
              :radius 0.5
              :hp 38})

(defn aquire-target
  [e]
  (let [{:target t
         :pos pos} e]
    (unless t
      # just a ray, but some sort of delay after aquisition
      # TODO: need to add (check-collision-ray-sphere)

      (put e :target player))))

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
      (aquire-target e)

      (let [{:target t
             :pos pos
             :speed speed
             :attack-range attack-range} e
            distance (dist (t :pos)
                           pos)]

        (when t
          (update e :dir (fn [dir]
                           (->
                             (v+ dir
                                 (v*
                                   (v-
                                     (v- (t :pos) pos)
                                     dir)
                                   0.1))
                             normalize)))

          (if (< distance attack-range)
            (:attack e t)
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


(var last-player-pos (get-camera-position c))
(varonce last-coll nil)

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

(defn tick
  [el]
  (def [x y z] (player :pos))

  (when (el :focused?)
    (when (mouse-button-pressed? 0)
      (def b @{:pos (array ;(player :pos))
               :sprite bullet
               :damage 13
               :dir (cam-rot c)
               :speed 0.25
               :color :green})
      (update-in b [:pos 1] - 0.01)
      (array/push bullets b)))

  (loop [e :in enemies]
    (:update e))

  (var i 0)

  (while (< i (length bullets))

    (if ((in bullets i) :dead)
      (++ i) ##1 # (array/remove bullets i)
      (++ i))))

(defn handle-map-collisions
  []
  (let [scale 10
        player-pos (get-camera-position c)
        [px py pz] player-pos]

    (comment
      #draw player
      (draw-circle
        (math/round (* scale (+ px (* 0.5 map-w) 0.5)))
        (math/round (* scale (+ pz (* 0.5 map-h) 0.5)))
        (* scale 0.5)
        :blue))

    (loop [x :range [0 map-w]
           y :range [0 map-h]
           :when (and (= [1 1 1 1]
                         (map-pixels (+ (* y map-w)
                                        x))))]
      (comment
        # drawing map
        (draw-rectangle (* scale x)
                        (* scale y)
                        scale
                        scale
                        :red)))

    (var nof-hits 0)

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
               :when (check-collision-circle-rec
                       (->map-pos map-w map-h bx bz)
                       bullet-size

                       tile-rec)]
          (put b :dead true)))

      (when (check-collision-circle-rec
              (->map-pos map-w map-h px pz)

              0.25

              tile-rec)

        #(set-camera-position c last-player-pos)
        (set last-coll [[(+ px (* 0.5 map-w) -0.0)
                         (+ pz (* 0.5 map-h) -0.0)]
                        [x
                         y
                         1
                         1]])

        (++ nof-hits)

        (let [[p wall] last-coll]
          (def angle (v- p wall))

          (def x-diff (- (+ 0.5 0.25) (math/abs (- (p 0) (wall 0)))))

          (def x-diff
            (if (neg? (angle 0))
              (- x-diff)
              x-diff))

          (def z-diff (- (+ 0.5 0.25) (math/abs (- (p 1) (wall 1)))))

          (def z-diff
            (if (pos? (angle 1))
              (- z-diff)
              z-diff))

          (if (> (math/abs (angle 0))
                 (math/abs (angle 1)))
            # "left"
            (do (print "left")
              (set-camera-position c [(+ px x-diff) py pz]))
            # "up"
            (set-camera-position c [px py (- pz z-diff)])))

        (draw-rectangle (* scale x)
                        (* scale y)
                        scale
                        scale
                        :green)))

    (when (< 2 nof-hits)
      (set-camera-position c last-player-pos))

    (set last-player-pos (get-camera-position c))))

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
  (def scale 10)

  (defer (end-mode-3d)
    (begin-mode-3d c)

    (draw-model model map-pos 1 :white)

    (var coll false)

    (loop [b :in bullets
           :when (not (b :dead))
           :let [{:damage damage} b
                 b-pos [(get-in b [:pos 0])
                        (get-in b [:pos 2])]]]
      (loop [e :in enemies
             :let [{:pos pos
                    :radius e-radius} e
                   [ex ey ez] pos
                   e-pos [ex ez]]
             :when (not (e :dead))]
        (print "try hit enemy")
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
                  :speed speed} b]]

      (if (b :dead)
        (draw-billboard c (get-render-texture explo/rt)
                        pos 0.5 :white)

        (do
          (update b :pos v+ (v* dir speed))
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
