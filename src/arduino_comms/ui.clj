(ns arduino-comms.ui
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [arduino-comms.communication :as c]
            [clojure.string :as str]
            [medley.core :as util]
            [clojure.set :as set]))


(defonce restart-sketch? true)

(def yellow -256)
(def blue -7876885)
(def red -65536)


(defonce settings (atom {:filter-out-var-set? true
                         :use-auto-generated-ui? false}))


(defn filter-visible-messages [msg-list]
  (if (:filter-out-var-set? @settings)
    (filter #(not= :var-set (c/classify %)) msg-list)
    msg-list))


(def visual-layout (read-string (slurp "layout.edn")))

(def generated-layout (atom []))

(def default-components
  (->> [{:type  :toggle
         :value false
         :text  "DEFAULT TEXT"
         :x     10
         :y     10
         :color blue}
        {:type  :slider
         :value 0
         :text  "DEFAULT TEXT"
         :min   0
         :max   1024
         :x     10
         :y     10
         :color blue}
        {:type  :text
         :text  "Default text"
         :value "Default value"
         :x     10
         :y     10
         :color blue}]
       (group-by :type)
       (util/map-vals first)))

;;;;;;;;;;;;;;;
;; ui rendering
;;;;;;;;;;;;;;;

(defmulti draw-ui-element :type)
(defmulti valid-type-for-comp? (fn [c _] (:type c)))

(defmethod draw-ui-element :toggle [{:keys [value text x y color]}]
  (q/stroke color)
  (if value (q/fill color)
            (q/fill 0 0 0))
  (q/rect x y 20 20)
  (q/fill color)
  (q/text (str text ": " value) (+ 30 x) y))

(defmethod valid-type-for-comp? :toggle [_ value] (contains? #{1 0 true false} value))

(defmethod draw-ui-element :slider [{:keys [value text x y color min max]}]
  (q/stroke color)
  (q/fill 0 0 0)
  (q/rect x y 150 20)
  (when value (q/fill color))

  (q/rect x y (q/map-range value min max 0 150) 20)
  (q/fill 255 255 255)
  (q/text (str text ": " value) (+ 170 x) y))

(defmethod valid-type-for-comp? :slider [_ value] (or (integer? value)
                                                      (float? value)))

(defmethod draw-ui-element :text [{:keys [text value x y color]}]
  (q/stroke color)
  (q/fill color)
  (when value (q/fill color))
  (q/text (str text value) x y))

(defmethod valid-type-for-comp? :text [_ _] true)

;;;;;;;;;;;;;;;;;;;;
;; auto generated ui
;;;;;;;;;;;;;;;;;;;;

(defn unaccounted-custom-keys [ui-configuration]
  (let [lst (reduce #(concat %1 (second %2)) [] ui-configuration)
        accounted-values (mapcat (fn [{:keys [value]}]
                                   (if (= :custom-values (first value))
                                     [(second value)]
                                     [])) lst)
        leftover-vals (set/difference
                        (set (keys (:custom-values @c/current-accumulated-message)))
                        (set accounted-values))]
    leftover-vals))


(defn type-guess [value]
  (cond (integer? value) :slider
        (util/boolean? value) :toggle
        :default :text))

(defn generate-default-component [value-loc]
  (let [v (get-in @c/current-accumulated-message value-loc)
        type (type-guess v)]
    (merge (type default-components)
           {:text (str (second value-loc))
            :value value-loc})))

(defn handle-additional-custom-values []
  (let [uack (unaccounted-custom-keys visual-layout)
        comps (map #(generate-default-component [:custom-values %]) uack)]
    (reset! generated-layout comps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert ui configuration to renderable components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn render-edn-fn-to-fn [edn-fn]
  (when edn-fn
    (let [fns {:lt < :gt > :eq = :ne not= :lte <= :gte >=}
          [f-kw & args] edn-fn
          f (fns f-kw)
          replace-with (fn [old new curr] (if (= curr old) new curr))]
      (fn [value] (when value (apply f (map (partial replace-with :value value) args)))))))


(defn render-edn-component-to-ui-config [edn-component]
  ;; transforms: error-fn -> fn; warning-fn -> fn; fallback? -> component;
  (when edn-component
    (let [error-fn (render-edn-fn-to-fn (:error-fn edn-component))
          warning-fn (render-edn-fn-to-fn (:warning-fn edn-component))
          comp edn-component
          comp (if error-fn (assoc comp :error-fn error-fn) comp)
          comp (if warning-fn (assoc comp :warning-fn warning-fn) comp)]
      comp)))


(defn realize-ui-element
  "Configure a ui element with the proper data for rendering"
  [component-config data]
  (let [{:keys [error-fn warning-fn value] :or {error-fn   (constantly false)
                                                warning-fn (constantly false)}} component-config
        fallback (merge (:text default-components)
                        {:text  "Missing"
                         :value value                       ; so we don't fallback on our fallback
                         :color yellow})
        v (get-in data value :ui/k-missing)
        correct-type? (valid-type-for-comp? component-config v)
        missing-value? (and (= :ui/k-missing v) (not (nil? value)))
        use-fallback (or missing-value? (not correct-type?))
        color (when (not use-fallback)
                (cond (error-fn v) red
                      (warning-fn v) yellow
                      :default blue))]
    (if use-fallback
      fallback
      (assoc component-config
        :value v
        :color color))))


(defn render-edn-to-config [edn]
  (let [upf #(map render-edn-component-to-ui-config %)
        configured-layout (-> edn
                              (update :left upf)
                              (update :right upf))]
    (if-not (:use-auto-generated-ui? @settings)
      configured-layout
      (let [additional-comps @generated-layout
            num-for-l (- 5 (count (:left configured-layout)))
            [r l] (split-at num-for-l additional-comps)]
        (-> configured-layout
            (update :left #(concat % l))
            (update :right #(concat % r)))))))


(defn ui-configuration [] (render-edn-to-config visual-layout))


;; LAYERS :: raw-data -> pretty-data -> ui-settings -> ui-layout -> render

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive ui elements
;;;;;;;;;;;;;;;;;;;;;;;;;;


(def click-hotspots
  [{:x     45 :y 30
    :width 300 :height 50
    :run   #(update % :filter-out-var-set? not)}
   {:x     480 :y 30
    :width 300 :height 50
    :run   #(update % :use-auto-generated-ui? not)}])

(defn additional-ui-elements []
  [(merge (:toggle default-components)
          {:text  "Filter out :var-set?"
           :value (:filter-out-var-set? @settings)
           :x     45 :y 50})
   (merge (:toggle default-components)
          {:text  "Use auto-generated ui?"
           :value (:use-auto-generated-ui? @settings)
           :x     480 :y 50})])

(defn mouse-pressed [_ details]
  (let [mx (:x details) my (:y details)]
    (if-let [item (->> click-hotspots
                       (filter (fn [{:keys [x y width height]}]
                                 (and (< x mx (+ x width))
                                      (< y my (+ y height)))))
                       first)]
      (swap! settings (:run item)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ui layout configuration 
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-spacing [start space]
  (iterate (partial + space) start))


(defn data->ui-settings [data]
  (let [conf (ui-configuration)]
    {:left   (map #(realize-ui-element % data) (:left conf))

     :right  (map #(realize-ui-element % data) (:right conf))

     :bottom (map (fn [value]
                    (let [ret (merge (:text default-components) {:value value :text ""})
                          classification (c/classify value)]
                      (->> classification
                           (get {:error   red
                                 :var-set yellow
                                 :default blue})
                           (assoc ret :color))))
                  (->> (:msgs data) reverse filter-visible-messages (take 21)))}))



(defn ui-settings->ui-layout [settings]
  (let [{:keys [left right bottom]} settings]
    (concat (map (fn [elem y]
                   (assoc elem :x 45
                               :y y))
                 left (generate-spacing 130 35))
            (map (fn [elem y]
                   (assoc elem :x 480
                               :y y))
                 right (generate-spacing 130 35))
            (map (fn [elem y]
                   (assoc elem :x 40
                               :y (+ 840 y)))
                 bottom (generate-spacing 20 -26)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (mostly) static ui elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn draw-boxes []
  (q/stroke blue)
  (q/fill 0 0)
  (q/rect 25 330 850 560)
  (q/rect 25 25 850 70)
  (q/rect 450 110 425 200)
  (q/rect 25 110 400 200)

  (q/no-stroke)

  (let [c (- (System/currentTimeMillis)
             (:time-posted @c/current-accumulated-message)
             300)]
    (q/fill (q/lerp-color blue red (q/map-range c 0 1000 -0.5 1)))
    (q/rect 0 0 (q/map-range c 0 1000 0 900) 5))
  )

;;;;;;;;;;;;;;
;; quill stuff
;;;;;;;;;;;;;;

(defn setup []

  ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
  ; Set color mode to HSB (HSV) instead of default RGB.
  ;(q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (let [font (q/create-font "Ubuntu Mono" 20)]
    (q/text-font font))
  (q/text-align :left :top))


(defn draw-state [state]


  ; Clear the sketch by filling it with light-grey color.
  (q/background 0)
  ; Set circle color.
  (handle-additional-custom-values)
  (q/text-align :left :top)
  (q/text-size 20)
  (if-let [data @c/current-accumulated-message]
    (do (draw-boxes)
        (doall (map draw-ui-element (->> data
                                         data->ui-settings
                                         ui-settings->ui-layout
                                         (concat (additional-ui-elements))))))
    (do (q/text-size 40)
        (q/text-align :center :center)
        (q/fill 255 0 0)
        (q/text "Data not available" 450 450)))
  )


(when restart-sketch?
  (q/defsketch arduino-comms
               :title "Debugger - Arduino"
               :size [900 900]
               ; setup function called only once, during sketch initialization.
               :setup setup
               :draw draw-state
               :features [:keep-on-top]
               :mouse-pressed mouse-pressed
               ; This sketch uses functional-mode middleware.
               ; Check quil wiki for more info about middlewares and particularly
               ; fun-mode.
               :middleware [m/fun-mode]))

(def restart-sketch? false)