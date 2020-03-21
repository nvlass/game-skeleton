(ns skeleton.gdx
  (:import [com.badlogic.gdx
            Gdx
            ;; Should consider either Game / Screens or ApplicationAdapter (plain)
            ApplicationAdapter Game 
            InputProcessor
            ApplicationListener
            ScreenAdapter
            ;; Input processor related
            InputProcessor
            Input])
  (:import [com.badlogic.gdx.backends.lwjgl
            LwjglApplication
            LwjglApplicationConfiguration])
  (:import [com.badlogic.gdx.graphics
            OrthographicCamera
            Texture])
  (:import [com.badlogic.gdx.graphics.g2d
            SpriteBatch
            Sprite])
  (:gen-class))


(defn last-n
  "append something to a (atom list)
  keeping only n first items.
  We'll use it for storing exceptions and
  stuff"
  [itm lst n]
  (swap! lst (fn [x] (take n (conj x itm)))))

;; moved out of here
;; (def exception-store
;;   (atom '()))

;; How many exceptions to store?
;; put it as a parameter
(def ^:const exc-store-count 10)


;; Non-state version
;; (defmacro fnmap-or-super
;;   "A small macro that either calls a function from a provided
;;   map or the proxy-super equivalent.
;;   Any exceptions are stored in the exc-queue
;;   "
;;   [fnname fnmap excstore & params]
;;   `(if (nil? (~(keyword fnname) ~fnmap))
;;      (proxy-super ~fnname ~@params)
;;      (try
;;        ;;(apply (~(keyword fnname) ~fnmap) ~params)
;;        ((~(keyword fnname) ~fnmap) ~@params)
;;        (catch Exception ex#
;;          (last-n ex# ~excstore exc-store-count)))))

;; State version
(defmacro fnmap-or-super
  "A small macro that either calls a function from a provided
  map or the proxy-super equivalent.
  Any exceptions are stored in the excstore
  "
  [fnname fnmap app-state excstore & params]
  `(if (nil? (~(keyword fnname) ~fnmap))
     (proxy-super ~fnname ~@params)
     (try
       ((~(keyword fnname) ~fnmap) ~app-state ~@params)
       (catch Exception ex#
         (last-n ex# ~excstore exc-store-count)))))


(defmacro gl-run
  "Some things must be run in the GL thread"
  [& what]
  `(.postRunnable Gdx/app (fn [] ~@what)))

;; 

(defn make-app-listener
  "Makes a proxy for the ApplicationAdapter (ApplicationListener).
  Calls the respective function from fnmap, if it is provided,
  or the superclass one if it is not. The current app-state is
  passed to each function and exceptions are stored in excstore"
  [fnmap app-state excstore]
  (proxy [ApplicationAdapter] []
    (create [] (fnmap-or-super create @fnmap app-state excstore))
    (dispose [] (fnmap-or-super dispose @fnmap app-state excstore))
    (pause [] (fnmap-or-super pause @fnmap app-state excstore))
    (render [] (fnmap-or-super render @fnmap app-state excstore))
    (resize [width height] (fnmap-or-super resize @fnmap app-state excstore width height))
    (resume [] (fnmap-or-super resume @fnmap app-state excstore))))


(defn make-game-listener
  "Makes a proxy for the Game GDX class. Using game instead of
  ApplicationAdapter allows for screen switching. Alternatively
  switching can be implemented at an fnmap level in the app-listener"
  [create-fn]
  (proxy [Game] []
    (create [] (create-fn))
    (dispose [] (proxy-super dispose))
    (pause [] (proxy-super pause))
    (render [] (proxy-super render))
    (resize [w h] (proxy-super resize w h))
    (resume [] (proxy-super resume))))

(defn make-screen
  "Makes a proxy for the ScreenAdapter.
  Calls the respective function from fnmap, if it is provided,
  or the superclass one if it is not. The current app-state is
  passed to each function and exceptions are stored in excstore"
  [fnmap app-state excstore]
  (proxy [ScreenAdapter] []
    (dispose [] (fnmap-or-super dispose @fnmap app-state excstore))
    (hide [] (fnmap-or-super hide @fnmap app-state excstore))
    (pause [] (fnmap-or-super pause @fnmap app-state excstore))
    (render [dt] (fnmap-or-super render @fnmap app-state excstore dt))
    (resize [width height] (fnmap-or-super resize @fnmap app-state excstore width height))
    (resume [] (fnmap-or-super resume @fnmap app-state excstore))
    (show [] (fnmap-or-super show @fnmap app-state excstore))))


(defmacro input-processor-fn [fnmap fnkey excstore & params]
  (if-let [f (fnkey fnmap)]
    (try
      (apply f params)
      (catch Exception ex
        (last-n ex excstore exc-store-count)
        true))
    true))
      
                              
(defn make-input-processor
  "InputProcessor is an interface, so we'll use reify.
  Each of the functions should return true if the input
  was processed or false if not"
  [fnmap app-state excstore]
  (reify InputProcessor
    (mouseMoved [this x y] (input-processor-fn fnmap :mouse-moved excstore app-state x y))
    (touchDown [this x y ptr btn] (input-processor-fn :touch-down excstore app-state x y ptr btn))
    (touchDragged [this x y ptr] (input-processor-fn :touch-dragged excstore app-state x y ptr))
    (touchUp [this x y ptr btn] (input-processor-fn :touch-up excstore app-state x y ptr))
    (keyDown [this k] (input-processor-fn :key-down excstore app-state k))
    (keyUp [this k] (input-processor-fn :key-up excstore app-state k))
    (keyTyped [this k] (input-processor-fn :key-typed excstore app-state k))
    (scrolled [this k] (input-processor-fn :scrolled excstore app-state k))))


(defn setup-camera
  "Setup an orthographic camera. There are lots of other
  options in libgdx, but this is OK for small games"
  [width height]
  (OrthographicCamera. width height))

(defn make-config
  "
  see https://libgdx.badlogicgames.com/ci/nightlies/docs/api/com/badlogic/gdx/backends/lwjgl/LwjglApplicationConfiguration.html for possible options.
  In this case we'll use only [width, height, title]
"
  [title width height]
  (let [cfg (LwjglApplicationConfiguration.)]
    (set! (.title cfg) title)
    (set! (.width cfg) width)
    (set! (.height cfg) height)
    cfg))


(defn make-application
  "make the application with a given adapter"
  [app-listener config]
  (LwjglApplication.
   app-listener
   config))


(defmacro with-shape-renderer [[c sr type] & stuff]
  `(let [~c ~sr]
     (.begin ~sr ~type)
     (try ;; we want this for the "finally" part, so rethrow
       ~@stuff
       (catch Exception ex#
         ;;(last-n ex# ~excstore exc-store-count))
         (throw ex#))
       (finally (.end ~sr)))))


(defmacro with-shape-renderer-and-camera [[[sr-name sr] cam type] & stuff]
  `(let [~sr-name ~sr]
     (.update ~cam)
     (.setProjectionMatrix ~sr-name (.combined ~cam))
     (.begin ~sr ~type)
     (try ;; exceptions thrown here should be ignored so that 
       ~@stuff
       (catch Exception ex#
         ;;(last-n ex# ~excstore exc-store-count))
         (throw ex#))
       (finally (.end ~sr)))))


