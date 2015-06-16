(ns ^:figwheel-always om-automat.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! >! <! alts!]]
            [automat.core :as a]
            [om-bootstrap.button :as b]))

(enable-console-print!)


(defonce app-state (atom { }))

(defn display?
  [s k]
  (get-in s [:value :view  k :display]))

(defn fsm-component
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [inputs (interpose (a/$ :next) [:start :one :two (a/* :three) :end])
            fsm (a/compile inputs {:reducers {:next  (fn [view-state input]
                                                       (-> view-state
                                                           (assoc-in [:view input :display] true)
                                                           (assoc :last-input input)
                                                           (update-in  [:counters input] inc)))
                                              }})]
        {:inputs inputs
         :adv        (partial a/advance fsm)
         :chan/fsm   (chan)
         :init-state {}}))
    om/IWillMount
    (will-mount [_]
      (let [{:keys [adv chan/fsm init-state]} (om/get-state owner)]
        (go-loop []
                 (let [r (adv init-state :start)]
                   (om/set-state! owner :fsm-state r))      ;First transition is automatic.
                 (loop []
                   (let [input (<! fsm)
                         fsm-state (om/get-state owner :fsm-state) ;Get the current fsm state from local state.
                         next-state (adv fsm-state input :ko)] ;Try to advance the fsm with an input.
                     (when-not (= :ko next-state)
                       (om/set-state! owner :fsm-state next-state)) ;The next state is saved in local state
                     (when-not (:accepted? next-state)      ;We continue until the fsm is in an accepted state.
                       (recur))))
                 (recur))))
    om/IRenderState
    (render-state [_ {:keys [chan/fsm fsm-state last-input] :as state}]
      (dom/div #js {:className "container"}
               (dom/div #js {:className "well"}
                        (dom/p #js {}
                               "This is a small Finite-State Machine with 5 transitions
                               The view is simple : there is a button for each transition"))
               (dom/div #js {:className "well"}
                (b/toolbar {}
                           (when (display? fsm-state :start)
                             (b/button {:onClick  #(put! fsm :one)
                                        :bs-style "danger"} "One"))
                           (when (display? fsm-state :one)
                             (b/button {:onClick  #(put! fsm :two)
                                        :bs-style "primary"} "Two"))
                           (when (display? fsm-state :two)
                             (b/button {:onClick  #(put! fsm :three)
                                        :bs-style "success"} "Three *"))
                           (when (display? fsm-state :three)
                             (b/button {:onClick  #(put! fsm :end)
                                        :bs-style "warning"} "End")))
                        (when last-input (dom/div #js {:className "well"}
                                  "Last input : " (name last-input))))
               (let [{:keys [state-index value stream-index]} fsm-state]
                 (dom/div #js {:className "well"}
                          (dom/label #js {} "FSM")
                          (dom/p #js {} (str "View state : " (:view value)))
                          (dom/p #js {} (str "Counters : " (:counters value)))
                          (dom/p #js {} (str "state-index : " state-index))
                          (dom/p #js {} (str "stream-index : " stream-index))))))))


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

