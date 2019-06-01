(ns arduino-comms.ui
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [arduino-comms.communication :as c]
            [clojure.string :as str])
  (:import (processing.core PConstants PApplet)))

;; TODO: unify handling of errors in :custom-values. Should be easy.

(defonce restart-sketch? true)

(def sample-data (first @c/cached-messages))

(def yellow -256)
(def blue -7876885)
(def red -65536)


(defonce settings (atom {:filter-out-var-set? true}))


(defn filter-visible-messages [msg-list]
  (if (:filter-out-var-set? @settings)
    (filter #(not= :var-set (c/classify %)) msg-list)
    msg-list))


(def visual-layout
  {:left  [{:type       :toggle
            :text       "P D7"
            :value      [:digital 7]
            :error-fn   [:eq false :value]
            :warning-fn [:eq true :value]
            :fallback   {:type  :text
                         :value "Missing value PD7"}}

           {:type     :toggle
            :text     "Toggle"
            :value    [:custom-values :toggle]
            :error-fn [:eq true :value]
            :fallback {:type  :text
                       :value "Missing :toggle"}}]
   :right [{:type       :slider
            :min        0
            :max        100
            :value      [:custom-values :dist-to-wall]
            :warning-fn [:lt :value 8]
            :fallback   {:type  :text
                         :value "Missing :dist-to-wall"}}
           
           {:type     :slider
            :min      0
            :max      1000000
            :value    [:custom-values :millis]
            :error-fn [:gt :value 300000]}]})


(defn switch-input [overrides]
  (merge {:type  :toggle
          :value false
          :text  "DEFAULT TEXT"
          :x     10
          :y     10
          :color blue}
         overrides))

(defn analog-input [overrides]
  (merge {:type  :slider
          :value -8
          :text  "DEFAULT TEXT"
          :min   0
          :max   255
          :x     10
          :y     10
          :color blue}
         overrides))

(defn text [overrides]
  (merge {:type  :text
          :x     10
          :y     10
          :color blue}
         overrides))

(defmulti draw-ui-element :type)

(defmethod draw-ui-element :toggle [{:keys [value text x y color]}]
  (q/stroke color)
  (if value (q/fill color)
            (q/fill 0 0 0))
  (q/rect x y 20 20)
  (q/fill color)
  (q/text (str text ": " value) (+ 30 x) y))

(defmethod draw-ui-element :slider [{:keys [value text x y color min max]}]
  (q/stroke color)
  (q/fill 0 0 0)
  (q/rect x y 150 20)
  (when value (q/fill color))

  (q/rect x y (q/map-range value min max 0 150) 20)
  (q/fill 255 255 255)
  (q/text (str text ": " value) (+ 170 x) y))

(defmethod draw-ui-element :text [{:keys [value x y color]}]
  (q/stroke color)
  (q/fill color)
  (when value (q/fill color))
  (q/text value x y))




;; LAYERS :: raw-data -> pretty-data -> ui-settings -> ui-layout -> render

(def click-hotspots
  [{:x     45 :y 30
    :width 300 :height 50
    :run   #(update % :filter-out-var-set? not)}])

(defn additional-ui-elements []
  [(switch-input {:text  "Filter out :var-set?"
                  :value (:filter-out-var-set? @settings)
                  :x     45 :y 50})])

(defn mouse-pressed [_ details]
  (let [mx (:x details) my (:y details)]
    (if-let [item (->> click-hotspots
                       (filter (fn [{:keys [x y width height]}]
                                 (and (< x mx (+ x width))
                                      (< y my (+ y height)))))
                       first)]
      (swap! settings (:run item)))))

(defn generate-spacing [start space]
  (iterate (partial + space) start))


(defn data->ui-settings [data]
  (let [aval #(get-in data [:analog %])
        dval #(= 1 (get-in data [:digital %]))

        ;sa (fn [label [min max]]
        ;     (if-not value
        ;       (text {:value (str label " - Missing")
        ;              :color red})
        ;       (merge-with #(if %2 %2 %1)
        ;                   (analog-input {:text label :value value})
        ;                   {:min   min
        ;                    :max   max
        ;                    :color (when (and error-fn (error-fn value)) red)})))

        data-or-fallback (fn [data-path transform fails-fn works-fn]
                           (let [v (get-in data data-path)
                                 vt (try
                                      (let [td (transform v)]
                                        (if (not= nil td)
                                          td
                                          :ac/failure))
                                      (catch Exception e
                                        :ac/failure))]
                             (if (not= vt :ac/failure)
                               (works-fn vt)
                               (fails-fn v))))

        attach-error (fn [error-fn component]
                       (if (try (error-fn (:value component))
                                (catch Exception e false))
                         (assoc component :color red)
                         component))

        default-error-handler (fn [data-path transform component]
                                (data-or-fallback data-path transform
                                                  #(text {:value (str "Error: " (:text component)
                                                                      " = " %)
                                                          :color yellow})
                                                  #(assoc component
                                                     :value %)))

        ]
    {:top-left  [(text {:value (str "Curr Mode " (get-in data [:custom-values :curr-mode]))})
                 (->> (switch-input {:text "P D7"})
                      (default-error-handler [:digital 12] #(if (integer? %) % nil))
                      (attach-error (partial = 0)))

                 (->> (switch-input {:text "Toggle"})
                      (default-error-handler [:custom-values :toggle] identity)
                      (attach-error (partial = true)))
                 ]

     :top-right [(->> (analog-input {:text "Dist to wall"})
                      (default-error-handler [:custom-values :dist-to-wall] #(if (integer? %) % nil))
                      (attach-error (partial < 500)))
                 (->> (analog-input {:text "Time (s)" :min 0 :max 1000})
                      (default-error-handler [:custom-values :millis] #(float (/ % 1000)))
                      (attach-error (partial < 50)))
                 ]

     :bottom    (map (fn [value]
                       (let [ret (text {:value value})
                             classification (c/classify value)]
                         (->> classification
                              (get {:error   red
                                    :var-set yellow
                                    :default blue})
                              (assoc ret :color))))
                     (->> (:msgs data) reverse filter-visible-messages (take 21)))}))



(defn ui-settings->ui-layout [settings]
  (let [{:keys [top-left top-right bottom]} settings]
    (concat (map (fn [elem y]
                   (assoc elem :x 45
                               :y y))
                 top-left (generate-spacing 130 35))
            (map (fn [elem y]
                   (assoc elem :x 480
                               :y y))
                 top-right (generate-spacing 130 35))
            (map (fn [elem y]
                   (assoc elem :x 40
                               :y (+ 840 y)))
                 bottom (generate-spacing 20 -26)))))


(defn draw-boxes []
  (q/stroke blue)
  (q/fill 0 0)
  (q/rect 25 330 850 560)
  (q/rect 25 25 850 70)
  (q/rect 450 110 425 200)
  (q/rect 25 110 400 200)
  )


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
               :title "You spin my circle right round"
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