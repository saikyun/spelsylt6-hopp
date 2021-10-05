(use freja/flow)

(def c (camera-3d :target [0 2 0]
                  :up [0 1 0]
                  :fovy 45
                  :type :perspective
                  :position [15 14 15]))

(set-camera-mode c :first-person)
(show-cursor)

(defonce bill (load-texture "resources/billboard.png"))
(def bill-pos @[10 2 0])

(def last-size @[nil nil])
(var rt (load-render-texture 1800 1000))

(defn render
  [el]

  (clear-background :white)

  (do
    (begin-mode-3d c)

    (update-camera c)

    (draw-grid 10 1)

    (draw-billboard c bill bill-pos 2 :white)

    (end-mode-3d))

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
