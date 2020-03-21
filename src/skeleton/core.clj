(ns skeleton.core
  (:require [skeleton.gdx :as lg])
  (:import [com.badlogic.gdx            
            Gdx
            Input$Keys])
  (:import [com.badlogic.gdx.graphics
            GL20])
  (:import [com.badlogic.gdx.graphics.glutils
            ;; FIXME: shapes should probably be in the lg ns as utils?
            ;; even more, with some macros to handle "begin" / "end"??
            ShapeRenderer])
  (:gen-class))

;; A place to hold all Gdx relevant stuff
(def app-store (atom {}))

;; Exception store -- we'll get logs of these during dev
(def exception-store (atom '()))

;; Some global params and stuff

;; Note: we could recalc these on resize
(def ^:const width 800)
(def ^:const height 600)
(def ^:const width-meters 200)
(def ^:const height-meters 150) ;; could be calculated, but never mind

;; A game "screen" (in the libGdx sense) is a function map and a game state
;; The game state is just an atom containing a map, holding all the state
;; and resources for a screen
;; The function map is a receiver for the application listener events
;; The above will hold for all the game screens

;; FIXME resources can be disposed collectively -- do so

;; FIXME: calculate ratios from app-state on resize
(def width-ratio (/ width (float width-meters)))
(def height-ratio (/ height (float height-meters)))

(defn world->screen [{:keys [x y]}] 
  {:x (- (* x width-ratio) (/ width 2))
   :y (- (* y height-ratio) (/ height 2))})


(defn make-static-stars
  "make `n` static stars in the bg. each star is a map {:x, :y, :color}
  we'll add flicker at some point"
  [n w h]
  (for [_ (range n)]
    {:x (rand w) :y (rand h)
     ;; blue-ish stars
     :radius (rand 2.0)
     :color (list (float (rand 0.4))
                  (float (rand 0.4))
                  (float (+ 0.4 (rand 0.4))))}))

(defn render-stars [sr stars & col]
  (doseq [s stars]
    (let [{:keys [x y]} (world->screen s)
          [r g b] (or col (:color s))]
      (.setColor sr r g b 1.0)
      (.circle sr x y (or (:radius s) 2)))))

(defn make-star
  [w h]
  {:x w :y (rand h)
   :radius (rand 1.5)
   :ux (rand-nth [-5 -10 -15 -20 -25 -30 -35]) ;; 1 out of 4 speeds in m/s
   :color (list
           (float (+ 0.4 (rand 0.2)))
           (float (+ 0.4 (rand 0.2)))
           (float (+ 0.6 (rand 0.4))))})

(defn new-star? [state]
  (if (and
       (< (count (:stars state)) 200) ;; FIXME magic num
       (< (rand) 0.1)) ;; FIXME: another magic num (actually, prob w.r.t to frame rate and so on)
    (update state :stars conj (make-star width-meters height-meters))
    state))

(defn move-star [s dt]
  (update s :x #(+ % (* (:ux s) dt))))

(defn update-stars [stars dt]
  (filter #(> (:x %) 0)
          (map (fn [q] (move-star q dt)) stars)))

(defn update-boxes [boxes dt]
  boxes)

;; (map (fn [b]
;;        (-> b
;;            (update :t #(+ dt %))
;;            (
         

(defn update-intro-state
  "The main update state for the demo.
  Updates the star position, filters out the stars
  that are out of the screen and possibly adds a new
  star"
  [state dt]
  (-> state
      (update :time #(+ % dt))
      new-star?
      (update :stars update-stars dt)
      (update :boxes update-boxes dt)))

;; intro screen will be a horizontal old-school starfield
;; width / height refer to "field dimensions", however, we'll
;; define star speeds w.r.t. these

(def intro-screen-state
  (atom {:stars [] :bgstars []
         :time 0.0
         :resources {}}))

(defn render-message-box
  [shape-renderer x y w h]
  ;; 1st step (BG)
  (.begin shape-renderer
          com.badlogic.gdx.graphics.glutils.ShapeRenderer$ShapeType/Filled)
  (.setColor shape-renderer 0.1 0.9 0.1 0.4)
  (.rect shape-renderer x y w h)
  (.end shape-renderer)
  ;; 2nd step (rect)
  (.begin shape-renderer
          com.badlogic.gdx.graphics.glutils.ShapeRenderer$ShapeType/Line)
  (.setColor shape-renderer 0.1 0.9 0.1 1.0)
  (.rect shape-renderer x y w h)
  (.end shape-renderer))

(defn render-box [shape-renderer {:keys [x y fr fg fb sz]
                                  :or {fr (constantly 1) fg (constantly 1) fb (constantly 1)}} t]
  (let [r (fr t)
        g (fg t)
        b (fb t)]
    (.setColor shape-renderer r g b 1.0)
    (.rect shape-renderer x y sz sz)))


(defn render-boxes [state]
  (lg/with-shape-renderer-and-camera
    [[sr (-> state :resources :shape-renderer)]
     (-> state :resources :camera)
     com.badlogic.gdx.graphics.glutils.ShapeRenderer$ShapeType/Filled]
    (doseq [r (:boxes state)]
      (render-box sr r (:time state)))
    state))

(defn set-bg [state]
  (.glClearColor Gdx/gl 0.0 0.0 0.0 1.0)
  (.glClear Gdx/gl GL20/GL_COLOR_BUFFER_BIT)
  state)

(defn do-render-stars [state]
  (lg/with-shape-renderer-and-camera
    [[sr (-> state :resources :shape-renderer)]
     (-> state :resources :camera)
     com.badlogic.gdx.graphics.glutils.ShapeRenderer$ShapeType/Filled]
    ;; fixme: ^^^^^ too long 
    (render-stars sr (:bgstars state));  0.2 0.0 0.7)
    (render-stars sr (:stars state)))
  state)

;;;
;;; Sample implementation of the (Game -> Screens) approach
;;; 
(def intro-screen-fnmap
  {:dispose (fn [state]
              (do
                (println "dispose intro-screen -- clean up resources")
                (.dispose (-> @state :resources :shape-renderer))))
   :hide (fn [state]
           (println "intro screen is hiding"))
   :pause (fn [state]
            (println "intro screen paused"))
   :resize (fn [state w h]
             (println "intro screen resized" w h))
   :resume (fn [state]
             (println "intro screen resuming"))
   :render (fn [state dt] ;; this should be broken into parts in a realsituation
             #_(let [nstate (update-intro-state @state dt)]
                 (.glClearColor Gdx/gl 0.0 0.0 0.0 1.0)
               (.glClear Gdx/gl GL20/GL_COLOR_BUFFER_BIT)
               (lg/with-shape-renderer-and-camera
                 [[sr (-> @state :resources :shape-renderer)]
                  (-> @state :resources :camera)
                  com.badlogic.gdx.graphics.glutils.ShapeRenderer$ShapeType/Filled]
                 ;; fixme: ^^^^^ too long 
                 (render-stars sr (:bgstars @state));  0.2 0.0 0.7)
                 (render-stars sr (:stars @state)))
               (reset! state nstate))

             (swap!
              state
              (fn [x]
                ;; update
                (-> x
                    (update-intro-state dt)
                    set-bg
                    render-boxes
                    do-render-stars
                    ))))
   :show (fn [state]
           (println "init resources")
           (lg/gl-run
            (let [res {:shape-renderer (ShapeRenderer.)
                       :camera (lg/setup-camera width height)}]
              (swap! state assoc :resources res))))})




(defn make-boxes [w0 w1 w2 k0 k1 k2 f0 f1 f2 n]
  (map (fn [ni]
         {:y 100
          :x (+ 5 (* ni 10))
          :sz 5
          :fr (fn [t] (+ 0.5 (* 0.4 (Math/sin (+ f0 (* w0 t) (* k0 (+ 5 (* ni 10))))))))
          :fg (fn [t] (+ 0.5 (* 0.4 (Math/sin (+ f1 (* w1 t) (* k1 (+ 5 (* ni 10))))))))
          :fb (fn [t] (+ 0.5 (* 0.4 (Math/sin (+ f2 (* w2 t) (* k2 (+ 5 (* ni 10))))))))})
       (range n)))
       
         
;; Instructions for running in a REPL
;; Using Game -> Screen Approach
(comment

  (make-boxes 0.1 0.5 0.7 0.3 1.1 2.7 0 0 0 60)
  
  (def the-game
    (lg/make-game-listener
     (fn []
       (println "empty create fn for now"))))

  (def the-app
    (lg/make-application the-game (lg/make-config "starfield" width height)))

  (def intro-screen0 (lg/make-screen #'intro-screen-fnmap intro-screen-state exception-store))

  (.setScreen the-game intro-screen0)

  ;; add the static stars
  (swap! intro-screen-state assoc :bgstars (make-static-stars 100 200 150))

  (swap! intro-screen-state assoc :boxes (make-boxes 21 0 42
                                                     10 0 -13 0 0 0 60))
  
  ;; quit gdx (and the JVM)
  (.exit Gdx/app)
  
  )


;; Using AppListener
(comment
  (def app (atom nil))

  (.exit Gdx/app)
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [empty-game    (lg/make-game-listener (fn []
                                               (println "empty create fn for now")))
        app-config    (lg/make-config "Starfield" width height)
        app           (lg/make-application empty-game (lg/make-config "Starfield"
                                                                      width
                                                                      height))
        intro-screen0 (lg/make-screen #'intro-screen-fnmap
                                      intro-screen-state
                                      exception-store)]
    (.setScreen empty-game intro-screen0)
    ;; temp
    (swap! intro-screen-state assoc :bgstars (make-static-stars 100 200 150))
    (swap! intro-screen-state assoc :boxes (make-boxes 21 0 42
                                                       10 0 -13 0 0 0 60))
  
    (Thread/sleep 10000)
    (println "Hello, World!")))
