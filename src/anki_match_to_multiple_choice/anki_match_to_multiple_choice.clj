(ns anki-match-to-multiple-choice.anki-match-to-multiple-choice
  "FIXME: my new org.corfield.new/scratch project."
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn csv-lines->map [lines]
  (if (> (count lines) 9)
    (throw (ex-info "line count too many" {:line-count (count lines)}))
    (let [question-prefix (-> lines first first)
          matches-seq (rest lines)
          q->answer (into {} matches-seq)
          all-rights (->> matches-seq
                          (map second)
                          set)
          rows
          (map
           (fn [[itm-by-left _]]
             (let [answer (q->answer itm-by-left)
                   options
                   (map-indexed
                    (fn [idx-by-right itm-by-right]
                      {:itm-by-right itm-by-right
                       :is-answer? (= answer itm-by-right)
                       :name (str "Q_" (inc idx-by-right))})
                    all-rights)
                   answers
                   (->> options
                        (map
                         (fn [{:keys [is-answer?] :as _opt}]
                           (if is-answer?
                             1
                             0)))
                        (string/join " "))
                   options-as-map
                   (->> options
                        (map (fn [{:keys [itm-by-right name]}]
                               [(keyword name) itm-by-right]))
                        (into {}))]
               {:full-q (if (string/blank? question-prefix)
                          itm-by-left
                          (str question-prefix " - " itm-by-left))
                :answers answers
                :options-as-map options-as-map
                }))
           matches-seq)]
      {:question-prefix question-prefix
       :all-rights all-rights
       :rows rows})))

(defn csv-filepath->lines [filepath]
  (with-open [reader (io/reader filepath)]
    (doall
     (csv/read-csv reader))))

(defn canon-row-maps->csv-str [canon-row-maps]
  (let [rows (map
              (fn [canon-row-map]
                (let [opts (:options-as-map canon-row-map)]
                 [(:full-q canon-row-map)
                  ""
                  2
                  (or (:Q_1 opts) "")
                  (or (:Q_2 opts) "")
                  (or (:Q_3 opts) "")
                  (or (:Q_4 opts) "")
                  (or (:Q_5 opts) "")
                  (or (:Q_6 opts) "")
                  (or (:Q_7 opts) "")
                  (or (:Q_8 opts) "")
                  (:answers canon-row-map)]))
              canon-row-maps)
        comma-del-rows (map #(string/join "|" %) rows)
        full-csv-str (string/join "\n" comma-del-rows)]
    full-csv-str))


(comment

  (let [lines (csv-filepath->lines "in.csv")
        canon-row-maps (:rows (csv-lines->map lines))
        csv-str (canon-row-maps->csv-str canon-row-maps)
        _ (spit "out.csv" csv-str)])

  )
