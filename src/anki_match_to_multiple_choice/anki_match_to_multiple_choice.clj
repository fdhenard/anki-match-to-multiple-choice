(ns anki-match-to-multiple-choice.anki-match-to-multiple-choice
  "FIXME: my new org.corfield.new/scratch project."
  (:require [clojure.string :as string]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn exec
  "Invoke me with clojure -X anki-match-to-multiple-choice.anki-match-to-multiple-choice/exec"
  [opts]
  (println "exec with" opts))

(defn -main
  "Invoke me with clojure -M -m anki-match-to-multiple-choice.anki-match-to-multiple-choice"
  [& args]
  (println "-main with" args))


(defn csv-lines->map [lines]
  (if (> (count lines) 7)
    (throw (ex-info "line count too many" {:line-count (count lines)}))
    (let [question-prefix (-> lines first first)
          matches-seq (rest lines)
          #_#_$ (map-indexed
             (fn [idx itm]
               [idx itm])
             $)
          q->answer (into {} matches-seq)
          #_#_answer->q (->> matches-seq
                         (map
                          (fn [[left right]]
                            [right left]))
                         (into {}))
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
                      {#_#_:full-q (str question-prefix " - " itm-by-left)
                       #_#_:itm-by-left itm-by-left
                       :itm-by-right itm-by-right
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
                #_#_:options options
                :answers answers
                :options-as-map options-as-map
                }))
           matches-seq)

          ]
      {:question-prefix question-prefix
       #_#_:q->answer q->answer
       #_#_:answer->q answer->q
       :all-rights all-rights
       :rows rows}))
  )

(defn csv-filepath->lines [filepath]
  (with-open [reader (io/reader filepath)]
    (doall
     (csv/read-csv reader))))

;; (defn row-map->anki-row-map [canon-row-map]
;;   (let [opts-as-map (:options-as-map canon-row-map)
;;         opts-as-map-string-keys
;;         (->> opts-as-map
;;              (map
;;               (fn [[k v]]
;;                 [(name k) v]))
;;              (into {}))]
;;     (merge
;;      {"Question" (:full-q canon-row-map)
;;       "QType" 2
;;       "Answers" (:answers canon-row-map)}
;;      opts-as-map-string-keys)))

(defn canon-row-maps->csv-str [canon-row-maps]
  (let [#_#_header [["Question" "Q_Type" "Answers" "Q_1" "Q_2" "Q_3" "Q_4" "Q_5" "Q_6"]]
        rows (map
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
                  (:answers canon-row-map)]))
              canon-row-maps)
        #_#_rows-as-vectors (concat header rows)
        comma-del-rows (map #(string/join "|" %) rows)
        full-csv-str (string/join "\n" comma-del-rows)]
    full-csv-str))


(comment

  (let [lines (csv-filepath->lines "in.csv")
        canon-row-maps (:rows (csv-lines->map lines))
        #_#_anki-row-maps (map row-map->anki-row-map canon-row-maps)
        csv-str (canon-row-maps->csv-str canon-row-maps)
        _ (spit "out.csv" csv-str)]
    csv-str
    #_canon-row-maps)
  

  )
