(use freja/flow)
(import freja-jaylib :as fj)

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

(array/push enemies @{:pos @[0 0.2 0]
                      :radius 0.5
                      :blink 0})


(import ../freja/freja/vector-math :prefix "")
(import ./coll :prefix "")


(var last-player-pos (get-camera-position c))
(varonce last-coll nil)

(def debug-logs @[])

(defn log
  [& args]
  (array/push debug-logs (string/format
                           (string/join
                             (seq [i :range [0 (length args)]]
                               "%p")
                             " ")
                           ;args)))

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
  (def player-pos (get-camera-position c))
  (def [x y z] player-pos)

  (when (el :focused?)
    (when (mouse-button-pressed? 0)
      (def b @{:pos (array ;player-pos)
               :sprite bullet
               :dir (cam-rot c)
               :speed 0.25
               :color :green})
      (update-in b [:pos 1] - 0.01)
      (array/push bullets b)))

  (var i 0)

  (while (< i (length bullets))

    (if ((in bullets i) :dead)
      (array/remove bullets i)
      (++ i))))

(defn handle-map-collisions
  []
  (let [scale 10
        player-pos (get-camera-position c)
        [px py pz] player-pos]

    (draw-circle
      (math/round (* scale (+ px (* 0.5 map-w) 0.5)))
      (math/round (* scale (+ pz (* 0.5 map-h) 0.5)))
      (* scale 0.5)
      :blue)

    (loop [x :range [0 map-w]
           y :range [0 map-h]
           :when (and (= [1 1 1 1]
                         (map-pixels (+ (* y map-w)
                                        x))))]

      (draw-rectangle (* scale x)
                      (* scale y)
                      scale
                      scale
                      :red))

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
    (update-camera c))

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
           :let [b-pos [(- (get-in b [:pos 0]))
                        (- (get-in b [:pos 2]))]]]
      (loop [e :in enemies
             :let [{:pos pos
                    :radius e-radius} e
                   [ex ey ez] pos
                   e-pos [ex ez]]]

        (when (circle-circle? e-pos
                              e-radius
                              b-pos
                              size)
          (put e :hit 20)
          (put e :blink-timer 5)
          (put b :dead true))))

    (loop [e :in enemies
           :let [{:pos pos
                  :hit hit
                  :blink blink} e]]
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
      (draw-billboard c bill pos 0.5 (if blink

                                       :red
                                       :white)))

    (loop [b :in bullets
           :let [{:pos pos
                  :sprite sprite
                  :dir dir
                  :speed speed} b]]

      (update b :pos v+ (v* dir speed))

      (draw-billboard c sprite pos 0.1 :white)))

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

  (array/clear debug-logs)

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

    (set rt (load-render-texture ;last-size)))

  (rl-pop-matrix)
  (rl-pop-matrix)

  (defer (do (end-texture-mode)

           (rl-push-matrix)

           # hotfix for translation problems
           (rl-translatef (- (el :render-x) 2) (- (el :render-y) 2) 0)

           (rl-push-matrix))

    (begin-texture-mode rt)

    (render el))

  (draw-texture-pro
    (get-render-texture rt)
    [0 0 mw (- mh)]
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
