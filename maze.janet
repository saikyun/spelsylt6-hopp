(use freja/flow)
(import freja-jaylib :as fj)

(def c (camera-3d :target [0 0 0]
                  :up [0 1 0]
                  :fovy 45
                  :type :perspective
                  :position [0.2 0.4 0.2]))

(set-camera-mode c :first-person)
#(show-cursor)

(def im-map (load-image-1 "resources/cubicmap.png"))
(defonce cubicmap (load-texture-from-image im-map))
(defonce mesh (gen-mesh-cubicmap im-map [1 1 1]))
(defonce model (load-model-from-mesh mesh))

(defonce tex (load-texture "resources/cubicmap_atlas.png"))
(def mat (get-model-material model 0))
(set-material-texture mat tex :diffuse)

(def map-pixels (get-image-data im-map))
(unload-image im-map)

(def map-pos @[-16 0 -8])

(def [map-w map-h] (get-image-dimensions im-map))

(defonce bill (load-texture "resources/billboard.png"))
(def bill-pos @[0 0 0])

(defonce bullet (load-texture "resources/bullet.png"))

(def last-size @[nil nil])
(var rt (load-render-texture 1800 1000))

(def bullets @[])

(defn tick
  []

  (def player-pos (get-camera-position c))

  (def [x y z] player-pos)

  (when (mouse-button-down? 0)
    (tracev @{:pos player-pos
              :sprite bullet})
    (array/push bullets @{:pos (array ;player-pos)
                          :sprite bullet}))

  (def cell-x (math/round (- x (map-pos 0))))
  (def cell-y (math/round (- y (map-pos 2))))

  (loop [x :range [0 map-w]
         y :range [0 map-h]
         :when (and (= [1 1 1 1]
                       (map-pixels (+ (* y map-w)
                                      x)))
                    (do tracev
                      (check-collision-circle-rec
                        [x y]
                        0.1
                        [(+ (map-pos 0) -0.5 x)
                         (+ (map-pos 2) -0.5 y)
                         1
                         1])))]
    (print "colide!")))

(defn render
  [el]
  (def {:width mw
        :height mh} el)

  (clear-background :white)

  (tick)

  (draw-rectangle-rec [0 0 33 10] :gray)ooooo

  (do
    (begin-mode-3d c)

    (update-camera c)

    (draw-model model map-pos 1 :white)

    (draw-billboard c bill bill-pos 2 :white)

    (loop [{:pos pos
            :sprite sprite} :in bullets]
      (update pos 0 + 0.05)
      (draw-billboard c sprite pos 0.1 :white))

    (end-mode-3d))

  (draw-rectangle-rec [0 100 33 10] :red)

  (draw-fps 0 0))

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

  (begin-texture-mode rt)

  (render el)

  (end-texture-mode)

  (rl-push-matrix)

  # hotfix for translation problems
  (rl-translatef (- (el :render-x) 2) (- (el :render-y) 2) 0)

  (rl-push-matrix)

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

(start-game {:render frame})
