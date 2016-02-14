(defn fetch-records
  ([]
   (fetch-records 100))
  ([n]
   (lazy-seq
    (when (pos? n)
      (Thread/sleep 100)
      (cons {:score n
             :age (* 3 n)}
            (fetch-records (dec n)))))))

(def record-sum   (atom 0))
(def record-count (atom 0))

(defn average-records []
  (doseq [record (fetch-records)]
    (swap! record-sum   + (:score record))
    (swap! record-count + 1))
  (/ @record-sum @record-count))

(defn accumulate-average [record-average id]
  (doseq [record (fetch-records id)]
    (swap! record-average
           (fn [{:keys [sum count]}]
             {:sum   (+ sum (:score record))
              :count (+ count 1)
              :finished false})))
  (swap! record-average assoc :finished true))

(defn calculate-average [{:keys [sum count finished]}]
  [(/ sum count) finished])

(defn average-records [id]
  (let [record-average
        (atom {:sum   0
               :count 0
               :finished false})]
    (future (accumulate-average record-average id))
    (fn [] (calculate-average record-average))))

(defn accumulate [accum finish vals]
  (doseq [val vals]
    (accum val))
  (finish))

(defn average-accumulator []
  (let [average (atom {:sum 0
                       :count 0
                       :finished false})]
    [(fn [val]
       (swap! average
         (fn [{:keys [sum count]}]
           {:sum   (+ sum val)
            :count (+ count 1)
            :finished false})))
     (fn [] (swap! average assoc :finished true))
     (fn []
       (let [{:keys [sum count finished]} @average]
         [(/ sum count) finished]))]))

(defn average [f vals]
  (let [[accum finish current] (average-accumulator)]
    (future (accumulate (comp accum f) finish vals))
    current))

(defn sum-accumulator []
  (let [average (atom {:sum 0
                       :finished false})]
    [(fn [val]
       (swap! average
         (fn [{:keys [sum]}]
           {:sum   (+ sum val)
            :finished false})))
     (fn [] (swap! average assoc :finished true))
     (fn []
       (let [{:keys [sum finished]} @average]
         [sum finished]))]))

(defn sum [f vals]
  (let [[accum finish current] (sum-accumulator)]
    (future (accumulate (comp accum f) finish vals))
    current))

;; Example of calculating the sum

(comment
  (def sum-progress (sum :age (fetch-records 1000000)))
  (println (sum-progress)))
