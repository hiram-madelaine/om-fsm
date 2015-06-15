(ns ^:figwheel-always om-automat.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! >! <! alts!]]
            [automat.core :as a]
            [om-bootstrap.button :as b]))

(enable-console-print!)


;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom { }))

(defn display?
  [s k]
  (get-in s [:value k :display]))

(defn fsm-component
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [inputs (interpose (a/$ :next) [:start :one :two (a/* :three) :end])
            fsm (a/compile inputs {:reducers {:next (fn [view-state action]
                                                      (assoc-in view-state [action :display] true))}})]
        {:inputs inputs
         :adv        (partial a/advance fsm)
         :chan/fsm   (chan)
         :init-state {}}))
    om/IWillMount
    (will-mount [_]
      (let [{:keys [adv chan/fsm init-state]} (om/get-state owner)]
        (go-loop []
                 (let [r (adv init-state :start)]
                   (om/set-state! owner :fsm-state r))
                 (loop []
                   (let [a (<! fsm)
                         fsm-state (om/get-state owner :fsm-state)
                         next-state (adv fsm-state a :ko)]
                     (om/set-state! owner :last-input a)
                     (if (= :ko next-state)
                       (om/set-state! owner :rejected a)
                       (do
                         (om/set-state! owner :rejected nil)
                         (om/set-state! owner :fsm-state next-state)))
                     (when-not (:accepted? next-state)
                       (recur))))
                 (recur))
        ))
    om/IRenderState
    (render-state [_ {:keys [chan/fsm fsm-state rejected last-input] :as state}]
      (dom/div #js {:className "container"}
               (dom/div #js {:className "well"}
                        (dom/p #js {}
                               "This is a small Finite-State Machine with 5 transitions
                               The view is simple : there is a button for each transition"))
               (dom/div #js {:className "well"}
                (b/toolbar {}
                           (when (get-in fsm-state [:value :start :display])
                             (b/button {:onClick  #(put! fsm :one)
                                        :bs-style "danger"} "One"))
                           (when (get-in fsm-state [:value :one])
                             (b/button {:onClick  #(put! fsm :two)
                                        :bs-style "primary"} "Two"))
                           (when (get-in fsm-state [:value :two])
                             (b/button {:onClick  #(put! fsm :three)
                                        :bs-style "success"} "Three *"))
                           (when (get-in fsm-state [:value :three])
                             (b/button {:onClick  #(put! fsm :end)
                                        :bs-style "warning"} "End")))
                        (when last-input (dom/div #js {:className "well"}
                                  "Last input : " (name last-input))))
               (let [{:keys [state-index value stream-index]} fsm-state]
                 (dom/div #js {:className "well"}
                          (dom/label #js {} "FSM")
                          (dom/p #js {} (str "value : " value))
                          (dom/p #js {} (str "state-index : " state-index))
                          (dom/p #js {} (str "stream-index : " stream-index))
                          (when rejected (dom/p #js {} "Transition rejected : " (name rejected)))))))))


(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (om/build fsm-component app ))))
  app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
) 

