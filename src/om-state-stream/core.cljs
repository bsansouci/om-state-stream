(ns om-state-stream.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

;; This is to be added in IWillReceiveProps such that a new strean is
;; generated when new props come in (the stream should depend on the props
;; as much as possible)
;; It will also update the stream to be (rest stream). This is a way to move
;; along the stream
;;
;; Something weird that I just noticed noticed is that I get a performance drop
;; (ie the animation gets choppier) when I remove the om/set-state. This is
;; weird because it's redundant for app2, the requestAnimationFrame loop should
;; take care of stepping through the stream.
(defn update-on-app-change [next-app app owner]
  (do
    (cond (not= next-app app) (om/set-state! owner :__stream ((om/get-state owner :__state-function) (first (om/get-state owner :__stream)) next-app)))
    (om/set-state! owner :__stream (rest (om/get-state owner :__stream)))))


;; This is a helper function to initialize the stream and create the state
;; associated with the component and the given state-function
(defn init-stream [state-function start-state app]
  {:__stream (state-function start-state app)
   :__state-function state-function})

;; This is just a helper function to abstract out the stream
(defn get-state-from-stream [state]
  (first (:__stream state)))

;; This is to be called in IDidMount to start a loop that will iterate through
;; the stream and replace it with (rest stream)
;; This is the step function that could only be used inside the top component
;; and all the others can simply have update-on-app-change which will be called
;; at every frame because the parent component will have changed
(defn start-state-stream [app owner]
  (.requestAnimationFrame js/window
    (fn render-loop []
      (let [stream (om/get-state owner :__stream)]
        (do
          (.requestAnimationFrame js/window render-loop)
          (om/set-state! owner :__stream (rest stream)))))))

;; Helper functions

(defn extend-to [n s]
  (let [li (take n s)]
    (take n (concat li (repeat (last li))))))

(defn to-frame-count [ms]
  (/ (* ms 60) 1000))

(defn to-ms [frame]
  (/ (* frame 1000) 60))

(def frame-length (/ 1000 60))

(defn time-stream []
  (iterate (fn [t] (+ t frame-length)) 0))

;; Physics stuff (ported from react-state-stream)

(defn ease-in-out-quad [t b _c d]
  (let [c (- _c b)
        t2 (/ t (/ d 2))]
    (cond (< t2 1) (+ (* (/ c 2) t2 t2) b)
          :else (+ b
                   (* (/ c (- 0 2))
                      (- (* (dec t2)
                              (- t2 3))
                         1))))))

(defn linear [t b _c d]
  (let [c (- _c b)]
    (+ b (/ (* t c) d))))

(defn ease-out-bounce [t b _c d]
  (let [c (- _c b)
        t2 (/ t d)
        n1 (/ 1 2.75)
        n2 (/ 2 2.75)
        n3 (/ 2.5 2.75)
        div1 (/ 1.5 2.75)
        div2 (/ 2.25 2.75)
        div3 (/ 2.625 2.75)
        constant 7.5625]
    (cond (< t2 n1) (+ b (* c t2 t2 constant))
          (< t2 n2) (+ b (* c (+ 0.75 (* constant (- t2 div1) (- t2 div1)))))
          (< t2 n3) (+ b (* c (+ 0.9375 (* constant (- t2 div2) (- t2 div2)))))
          :else     (+ b (* c (+ 0.984375 (* constant (- t2 div3) (- t2 div3))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; Example 1
;; ----------------------------------------------------------------------------
;; This first example shows how a state-stream description "maps" onto the app
;; (formally called props in React).
(defn child-style [deg]
  #js {:border "1px solid gray"
       :borderRadius 20
       :display "inline-block"
       :padding 18
       :WebkitTransform (str "rotate(" deg "deg)")
       :transform (str "rotate(" deg "deg)")})

;; This could be optimized to have the cond outside of the stream because
;; the cond can't change within the stream (and if :turn-left changes, we
;; call this function again to reset the state-stream.
;;
;; After-thought: The point of having the cond inside of the stream is that
;; it's easier to reason about. The idea behind state-stream is that we want
;; a nice way to describe the state of a component over time, so building
;; a stream is a very natural way to do so.
(defn child-stream-description [cur app]
  (iterate (fn [state] {:deg (+ (:deg state) (cond (:turn-left app) (- 0 2)
                                                    :else 6))}) cur))

(defn child-component [app owner]
  (reify
    om/IInitState
    (init-state [this]
      (init-stream child-stream-description {:deg 0} app))
    ;om/IDidMount ;; this is not needed but could be there
    ;(did-mount [this]
    ;  (start-state-stream app owner))
    om/IWillReceiveProps
    (will-receive-props [this next-props]
      (update-on-app-change next-props app owner))
    om/IRenderState
    (render-state [this state]
      (let [deg (:deg (get-state-from-stream state))]
        (dom/div nil
            (dom/div #js {:style (child-style deg)} "asd"))))))

(defn parent-style [deg]
  #js {:border "1px solid gray"
       :borderRadius 30
       :display "inline-block"
       :padding 30
       :WebkitTransform (str "rotate(" deg "deg)")
       :transform (str "rotate(" deg "deg)")
       :marginLeft 100})

;; Description of the state over time. This is what state-stream enables us to
;; do in a very nice way.
(defn parent-stream-description [cur app]
  (iterate (fn [state] {:deg (- (:deg state) 2)}) cur))

;; This enables us to do this sort of thing very easily
;; We can build on top of existing animations to add to its behavior
(defn parent-stream-description2 [cur app]
    (map (fn [x] {:deg (* 4 (:deg x))}) (parent-stream-description cur app)))


;; Notie how easy it is to update the state-stream simply by changing the app
;; cursor.
(defn parent-component [app owner]
  (reify
    om/IInitState
    (init-state [this]
      (init-stream parent-stream-description {:deg 0} app)) ;; Change this between parent-stream-description and parent-stream-description2
    om/IDidMount
    (did-mount [this]
      (start-state-stream app owner))
    om/IWillReceiveProps ;; this is also not needed because this component doesnt receive props
    (will-receive-props [this next-props]
      (update-on-app-change next-props app owner))
    om/IRenderState
    (render-state [this state]
      (let [deg (:deg (get-state-from-stream state))]
        (dom/div #js {:height 200}
           (dom/button #js {:onClick #(om/update! app :turn-left (not (:turn-left app)))} "Click")
           (dom/div #js {:style (parent-style deg)}
              (om/build child-component app)))))))

(def parent-state
  (atom {:turn-left false}))

(om/root parent-component parent-state
  {:target (. js/document (getElementById "app1"))})



;; Example 2
;; ----------------------------------------------------------------------------
;;
(defn app2-style [x top]
  #js {:border "1px solid gray"
       :borderRadius 10
       :display "inline-block"
       :padding 20
       :position "relative"
       :top top
       :WebkitTransform (str "translate3d(" x "px,0,0)")
       :transform (str "translate3d(" x "px,0,0)")})

(defn app2-stream-description [cur app]
  (let [[final-x1 final-x2 :as final-state] (cond (:going-left app) [50 400] :else [300 100])
        duration 1500
        frame-count (to-frame-count duration)
        [cur-x1 cur-x2] (:block-x cur)]
    (concat (map (fn [t]
                   {:block-x [(ease-in-out-quad t cur-x1 final-x1 duration)
                              (ease-out-bounce t cur-x2 final-x2 duration)]})
                 (take frame-count (time-stream)))
            (map (fn [_]
                   {:block-x final-state})
                 (drop frame-count (time-stream))))))

(defn app2-component [app owner]
  (reify
    om/IInitState
    (init-state [this]
      (init-stream app2-stream-description {:block-x [50 400]} app))
    om/IDidMount
    (did-mount [this]
      (start-state-stream app owner))
    om/IWillReceiveProps
    (will-receive-props [this next-props]
      (update-on-app-change next-props app owner))
    om/IRenderState
    (render-state [this state]
      (let [[block-x1 block-x2] (:block-x (get-state-from-stream state))]
        (dom/div #js {:style #js {:height 120}}
          (dom/button #js {:onClick #(om/update! app :going-left (not (:going-left app)))} "Click")
          (dom/div #js {:style (app2-style block-x1 10)})
          (dom/div #js {:style (app2-style block-x2 60)}))))))

(def parent-state2
  (atom {:going-left false}))

(om/root app2-component parent-state2
  {:target (. js/document (getElementById "app2"))})