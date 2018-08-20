(ns awslogs.core
  (:gen-class)
  (:import (java.io File))
   (:require [clojure.string :as str])
)

(use 'clojure.java.io)


(defn list-files [path] (-> path file .listFiles))


(defn process-lines [file pred transf]
  (with-open [rdr (clojure.java.io/reader (.getAbsolutePath file))]
    (doall
      (map transf
                (filter pred
                        (line-seq rdr))))))


(defn extract-field [str fieldno]
  (nth (str/split str #" ") fieldno))


(def prefix "https://www.sparrho.com:443/api/")

(defn response-times [file page]
  (process-lines file
                 (fn [l] (str/starts-with? (extract-field l 13) (str prefix page) ))
                 (fn [l] (Double/parseDouble (extract-field l 6)))))

(defn stddev [avg nums]

  (let [diffs (map (fn [x] (- x avg)) nums )
        sqdiffs (map (fn [x] (* x x)) diffs)
        sqtot (reduce + sqdiffs)
        variance (/ sqtot (count nums))
        ]
    ;(println nums)
    ;(println diffs)
    ;(println sqdiffs)
    ;(println sqtot)
    (Math/sqrt variance)))

(defn get-timings [path page]
  (->> (list-files path)
    (map (fn [x] ( response-times x page)))
    (flatten)))


(defn calc-stat [timings page]

  (if (== (count timings) 0)
    (println "no responses for " page)

    (let [sorted (sort timings)
          size (count sorted)
          avg (/ (reduce + sorted) size)
          median (nth sorted (* size 0.5))
          p20 (nth sorted (* size 0.2))
          p80 (nth sorted (* size 0.8))
          stddev (stddev avg sorted)]

      (println "number of response for " page ": " size)
      (println "resp. time mean " avg )
      (println "resp. time median " median  )
      (println "resp. time 20 percentile " p20  )
      (println "resp. time 80 percentile " p80  )
      (println "resp. time standard deviation " stddev ))))

(defn process-dir [path page]

  (println "Parsing files from " path)

  (calc-stat (get-timings path page) page))


(defn -main [& args]

  (process-dir (first args) (second args)))
