1(ns k2.core)

(def fps 30)

(enable-console-print!)

(def start-points-normalized
  [{:x 0 :y 0}
   {:x 0.5 :y 0.1}
   {:x 0.4 :y 0.3}
   {:x 0.7 :y 0.7}
   {:x 0.3 :y 0.9}
   {:x 0 :y 1}])

(def end-points-normalized
  [{:x 0 :y 0}
   {:x 0.4 :y 0.2}
   {:x 0.6 :y 0.4}
   {:x 0.4 :y 0.6}
   {:x 0.5 :y 0.8}
   {:x 0 :y 1}])

(defn scale-point [scaling {:keys [x y]}]
  {:x (* x scaling) :y (* y scaling)})

(defn calculate-control-points [{x0 :x y0 :y} {x1 :x y1 :y} {x2 :x y2 :y} t]
  (let [d01 (Math/sqrt (+ (Math/pow (- x0 x1) 2) (Math/pow (- y0 y1) 2)))
        d12 (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2)))
        fa (/ (* t d01) (+ d01 d12))
        fb (/ (* t d12) (+ d01 d12))
        c1x (- x1 (* fa (- x2 x0)))
        c1y (- y1 (* fa (- y2 y0)))
        c2x (+ x1 (* fb (- x2 x0)))
        c2y (+ y1 (* fb (- y2 y0)))]
    [{:x c1x :y c1y} {:x c2x :y c2y}]))

(defn draw-bezier [ctx cp0 cp1 p]
  (let [{:keys [x y]} p]
    (.bezierCurveTo ctx (:x cp0) (:y cp0) (:x cp1) (:y cp1) x y)))

(defn get-control-points [points tension]
  (mapcat #(calculate-control-points %1 %2 %3 tension) points (rest points) (rest (rest points))))

(defn draw-shape [ctx points tension]
  (let [control-points (get-control-points points tension)
        {start-x :x start-y :y} (first points)]
    (set! (.-fillStyle ctx) "#FF0000")
    (.beginPath ctx)
    (.moveTo ctx start-x start-y)
    (let [{cx :x cy :y} (first control-points)
          {:keys [x y]} (second points)]
      (.quadraticCurveTo ctx cx cy x y))
    (doall (map #(draw-bezier ctx %1 %2 %3)
                (take-nth 2 (rest control-points))
                (take-nth 2 (rest (rest control-points)))
                (rest (rest points))))
    (let [{cx :x cy :y} (last control-points)
          {:keys [x y]} (last points)]
      (.quadraticCurveTo ctx cx cy x y))
    (.closePath ctx)
    (.fill ctx)))

(defn substract-points [{x-a :x y-a :y} {x-b :x y-b :y}]
  {:x (- x-a x-b) :y (- y-a y-b) })

(defn add-points [{x-a :x y-a :y} {x-b :x y-b :y}]
  {:x (+ x-a x-b) :y (+ y-a y-b)})

(defn calculate-next-point [start-point end-point now-msec]
  (let [delta (substract-points end-point start-point)]
    (add-points start-point (scale-point (/ (+ 1 (Math/sin (/ now-msec 5000))) 2) delta))))

(defn start-loop []
  (let [canvas (.getElementById js/document "shape-canvas")
        ctx (.getContext canvas "2d")
        start-points (map #(scale-point (.-width canvas) %1) start-points-normalized)
        end-points (map #(scale-point (.-width canvas) %1) end-points-normalized)
        tension 0.5]
    (defn animate [now-msec]
      (.clearRect ctx 0 0 (.-width canvas) (.-height canvas))
      (.setTimeout js/window #(.requestAnimationFrame js/window animate) (/ 1000 fps))
      (let [points (map #(calculate-next-point %1 %2 now-msec) start-points end-points)]
        (draw-shape ctx points tension)))
    (.requestAnimationFrame js/window animate)))

(defonce animation-started (atom false))
(when-not @animation-started
  (start-loop)
  (reset! animation-started true))

(defn on-js-reload [])
