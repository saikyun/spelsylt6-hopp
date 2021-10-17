(import freja/state)
(import freja/events :as e)

(def hiccup [:block {} "Hello, World!"])

(e/put! state/editor-state :right (fn [_] hiccup))