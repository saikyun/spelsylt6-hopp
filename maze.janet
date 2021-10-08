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
(def bill-pos @[0 0.2 0])

(defonce bullet (load-texture "resources/bullet.png"))

(def last-size @[nil nil])
(var rt (load-render-texture 0 0))

(def bullets @[])

(import ../freja/freja/vector-math :prefix "")
(import ./coll :prefix "")


(var last-player-pos (get-camera-position c))
(varonce last-coll nil)

(defn cam-rot
  [c]
  (normalize
    (v-
      (get-camera-target c)
      (get-camera-position c))))

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

  (def cell-x (math/round (- x (map-pos 0))))
  (def cell-y (math/round (- y (map-pos 2)))))

(defn render
  [el]
  (def {:width mw
        :height mh} el)

  (when (el :focused?)
    (update-camera c))

  (clear-background :white)

  (tick el)

  (draw-rectangle-rec [0 0 33 10] :gray)

  (def bill-pos-2d [(- (get-in bill-pos [0]))
                    (- (get-in bill-pos [2]))])

  (def size 0.1)
  (def scale 10)

  (defer (end-mode-3d)
    (begin-mode-3d c)

    (draw-model model map-pos 1 :white)

    (var coll false)

    (loop [b :in bullets
           :let [b-pos [(- (get-in b [:pos 0]))
                        (- (get-in b [:pos 2]))]]]

      (when (circle-circle? b-pos size bill-pos-2d size)
        (set coll true)))

    (draw-billboard c bill bill-pos 0.5 (if coll :red :white))

    (loop [b :in bullets
           :let [{:pos pos
                  :sprite sprite
                  :dir dir
                  :speed speed} b]]

      (update b :pos v+ (v* dir speed))

      (draw-billboard c sprite pos 0.1 :white)))

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

    (loop [x :range [0 map-w]
           y :range [0 map-h]
           :when (and (= [1 1 1 1]
                         (map-pixels (+ (* y map-w)
                                        x)))

                      (check-collision-circle-rec
                        (do tracev [(+ px (* 0.5 map-w) -0.0)
                                    (+ pz (* 0.5 map-h) -0.0)])

                        0.25

                        (do tracev [x
                                    y
                                    1
                                    1])))]

      (set-camera-position c last-player-pos)
      (set last-coll [[(+ px (* 0.5 map-w) -0.0)
                       (+ pz (* 0.5 map-h) -0.0)]
                      [x
                       y
                       1
                       1]])

      (draw-rectangle (* scale x)
                      (* scale y)
                      scale
                      scale
                      :green))

    (set last-player-pos (get-camera-position c)))

  (draw-fps 600 0)

  (when-let [[p wall] last-coll]

    (def angle (v- p wall))

    (draw-rectangle 0 90 300 140 0x000000dd)

    (fj/draw-text (string/format "player %p" p) 0 100 22 :green)
    (fj/draw-text (string/format "wall %p" wall) 0 130 22 :green)
    (fj/draw-text (string/format "angle %p" angle) 0 160 22 :blue)
    (fj/draw-text (string/format "angle %p" (if (> (angle 0)
                                                   (angle 1))
                                              "left"
                                              "up")) 0 190 22 :blue))

  (comment
    (fj/draw-text (string/format "pos %p" (get-camera-position c)) 0 100 22 :green)
    (fj/draw-text (string/format "%p" (get-camera-target c)) 0 130 22 :green)
    (fj/draw-text (string/format "%p" (v- (get-camera-position c) (get-camera-target c)))
                  0 160 22 :blue)
    #
)
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
