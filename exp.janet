#(use freja/flow)
(use freja-jaylib)

(defn ->c
  [[r g b a]]
  (default a 255)
  [(/ r 255)
   (/ g 255)
   (/ b 255)
   (/ a 255)])

(var time 0)

(var extra 0)

(var total-time 16)

(var max-extra 0)

(def rt-size 100)

(var rt nil)

(def explo123
  (seq [i :range [0 20]
        :let [v (/ i 20)
              v (* v)]]
    [(* 600 (* (cond (> v 0.5) 0.01
                 (< v 0.3)
                 1.6
                 1)
               (- 1 (* v v))
               (- (math/random) 0.5)))
     (* 600 (* (cond (> v 10) 0.01
                 (< v 0.3)
                 1.6
                 1)
               (- 1 (* v v))
               (- (math/random) 0.5)))
     (+ 20 (* 200 v (math/random)))
     (- 1 (* 0.5 v))]))

(defn frame
  [explo]

  (++ time)

  (def prog (/ time total-time))

  (def expand-mult 1)

  (def inner-prog (/ time (* expand-mult total-time)))
  #(def progress (* inner-prog inner-prog (math/sin (* math/pi (* inner-prog inner-prog inner-prog)))))
  (def progress (- 1 (math/pow (- 1 (* 1 inner-prog)) 8)))

  (def progress2 #(/ time (* total-time))
    (- 1 (math/pow (- 1 (* 1 (max 0 (- inner-prog 0.3)))) 2)))

  (set max-extra (max extra max-extra))

  (def prog-wo-extra (+ max-extra
                        progress))

  (def progress (+ progress
                   extra))

  (when (> time total-time)
    (set time -10)
    (set extra 0)
    (set max-extra 0))

  (def visibility
    (if (< inner-prog 0.7)
      255
      (let [hide-prog (min 1 (/ (- inner-prog 0.7) 0.4))
            hide-prog (- 1 (math/pow (- 1 hide-prog) 4))]
        (* (- 1 hide-prog) 255))))

  (def visibility2
    (if (< prog 0.9)
      255
      (let [hide-prog (min 1 (/ (- prog 0.9) 0.3))
            hide-prog (math/sin hide-prog)]
        (* (- 1 hide-prog) 255))))

  (def shine
    (if (< prog 0.4)
      1
      (let [hide-prog (min 1 (/ (- prog 0.4) 0.6))
            hide-prog (math/sin hide-prog
                                # (- 1 (math/pow (- 1 hide-prog) 4))
)]
        (- 1 hide-prog))))

  (def ox (* rt-size 0.5))
  (def oy (* rt-size 0.5))

  (comment
    [[20 20 35]
     [-60 30 70]
     [30 -20 60]])

  (rl-pop-matrix)
  (rl-pop-matrix)

  (rl-load-identity)

  (begin-texture-mode rt)

  (rl-load-identity)

  (clear-background :blank)

  (when (pos? time)
    (when (or true (> visibility 80))
      (+= extra (- (* 0.1 (math/random)) 0.05)))

    (def mult 0.1)

    (loop [[color inner-scale extra]
           :in [[(->c [39 23 2 visibility2]) 1.1 :slow]
                [(->c [186 28 0 visibility]) 0.9]
                [(->c [222 159 22 visibility]) 0.65]
                [(->c [244 245 237 visibility]) 0.5 :shine]]]
      (loop [[x y scale lt] :in explo]

        (draw-circle
          (math/floor (+ ox (* mult progress2 x)))
          (math/floor (+ oy (* mult progress2 y)))
          (* lt
             mult
             (case extra
               :slow
               prog-wo-extra

               :shine
               (* shine progress progress)

               progress)
             (* inner-scale scale))
          color)))

    #
)

  (end-texture-mode)

  (rl-push-matrix)
  (rl-push-matrix)

  (def ren-mult 6)

  (comment
    (draw-texture-pro
      (get-render-texture rt)
      [0 0 rt-size (- rt-size)]
      [550 40 (* ren-mult rt-size)
       (* ren-mult rt-size)]
      [0 0]
      0
      :white))
  #
)


(comment

  (draw-circle
    ox
    oy
    (* prog-wo-extra 35)
    (->c [39 23 2 visibility2]))

  (draw-circle
    (math/floor (+ ox (* progress2 60)))
    (math/floor (+ oy (* progress2 30)))
    (* prog-wo-extra 70)
    (->c [39 23 2 visibility2]))

  (draw-circle
    (math/floor (+ ox (* progress2 30)))
    (math/floor (+ oy (* progress2 -20)))
    (* prog-wo-extra 60)
    (->c [39 23 2 visibility2]))

  (draw-circle
    200
    200
    (* progress 30)
    (->c [186 28 0 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 60)))
    (math/floor (+ oy (* progress2 30)))
    (* progress 60)
    (->c [186 28 0 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 30)))
    (math/floor (+ oy (* progress2 -20)))
    (* progress 50)
    (->c [186 28 0 visibility]))

  (draw-circle
    200
    200
    (* progress 25)
    (->c [222 159 22 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 60)))
    (math/floor (+ oy (* progress2 30)))
    (* progress 50)
    (->c [222 159 22 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 30)))
    (math/floor (+ oy (* progress2 -20)))
    (* progress 40)
    (->c [222 159 22 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 60)))
    (math/floor (+ oy (* progress2 30)))
    (* shine progress progress 40)
    (->c [244 245 237 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 0)))
    (math/floor (+ oy (* progress2 0)))
    (* shine progress progress 20)
    (->c [244 245 237 visibility]))

  (draw-circle
    (math/floor (+ ox (* progress2 30)))
    (math/floor (+ oy (* progress2 -20)))
    (* shine progress progress 30)
    (->c [244 245 237 visibility])))

(defn init
  []
  (set rt (load-render-texture rt-size rt-size)))

#(start-game {:render (fn [el] (frame explo123))})

