(import ../freja/freja/vector-math :prefix "")

(defn circle-circle?
  [p1 r1
   p2 r2]
  (<= (dist p1 p2)
      (+ r1 r2)))

(comment
  (circle-circle?
    [1 1] 0.5
    [2 1] 0.5)
  #=> true


  (circle-circle?
    [1 1] 0.5
    [2.0001 1] 0.5)
  #=> false

  #
)
