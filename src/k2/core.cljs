(ns k2.core)

(def default-start-points-normalized
  [[0 0]
   [0.5 0.1]
   [0.4 0.3]
   [0.7 0.7]
   [0.3 0.9]
   [0 1]])

(def default-end-points-normalized
  [[0 0]
   [0.4 0.2]
   [0.6 0.4]
   [0.4 0.6]
   [0.5 0.8]
   [0 1]])

(defn scale-point [scaling [x y]]
  [(* x scaling) (* y scaling)])

(defn calculate-control-points [t [x0 y0] [x1 y1] [x2 y2]]
  (let [d01 (Math/sqrt (+ (Math/pow (- x0 x1) 2) (Math/pow (- y0 y1) 2)))
        d12 (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2)))
        fa  (/ (* t d01) (+ d01 d12))
        fb  (/ (* t d12) (+ d01 d12))
        c1x (- x1 (* fa (- x2 x0)))
        c1y (- y1 (* fa (- y2 y0)))
        c2x (+ x1 (* fb (- x2 x0)))
        c2y (+ y1 (* fb (- y2 y0)))]
    [[c1x c1y] [c2x c2y]]))

(defn draw-bezier [ctx [cp0-x cp0-y] [cp1-x cp1-y] [x y]]
  (.bezierCurveTo ctx cp0-x cp0-y cp1-x cp1-y x y))

(defn get-control-points [points tension]
  (mapcat (partial calculate-control-points tension) points (rest points) (rest (rest points))))

(defn draw-shape [ctx points tension color]
  (let [control-points    (get-control-points points tension)
        [start-x start-y] (first points)]
    (set! (.-fillStyle ctx) color)
    (.beginPath ctx)
    (.moveTo ctx start-x start-y)
    (let [[cx cy] (first control-points)
          [x y]   (second points)]
      (.quadraticCurveTo ctx cx cy x y))
    (doall (map (partial draw-bezier ctx)
                (take-nth 2 (rest control-points))
                (take-nth 2 (rest (rest control-points)))
                (rest (rest points))))
    (let [[cx cy] (last control-points)
          [x y]   (last points)]
      (.quadraticCurveTo ctx cx cy x y))
    (.closePath ctx)
    (.fill ctx)))

(defn substract-points [[x-a y-a] [x-b y-b]]
  [(- x-a x-b) (- y-a y-b)])

(defn add-points [[x-a y-a] [x-b y-b]]
  [(+ x-a x-b) (+ y-a y-b)])

(defn calculate-next-point [now-msec start-point end-point]
  (let [delta (substract-points end-point start-point)]
    (add-points start-point (scale-point (/ (+ 1 (Math/sin (/ now-msec 5000))) 2) delta))))

(defn start-loop [canvas {:keys [start-points-normalized end-points-normalized fps color tension]
                          :or   {start-points-normalized default-start-points-normalized
                                 end-points-normalized   default-end-points-normalized
                                 fps                     30
                                 tension                 0.5
                                 color                   "#FF0000"}}]
  (let [ctx          (.getContext canvas "2d")
        canvas-width (.-width canvas)
        start-points (map (partial scale-point canvas-width) start-points-normalized)
        end-points   (map (partial scale-point canvas-width) end-points-normalized)]
    (defn animate [now-msec]
      (.clearRect ctx 0 0 canvas-width (.-height canvas))
      (.setTimeout js/window #(.requestAnimationFrame js/window animate) (/ 1000 fps))
      (let [points (map (partial calculate-next-point now-msec) start-points end-points)]
        (draw-shape ctx points tension color)))
    (.requestAnimationFrame js/window animate)))

(defn ^:export bezier-js [element opts]
  (start-loop element (js->clj opts :keywordize-keys true)))
