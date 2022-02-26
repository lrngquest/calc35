(ns calc-35.cli
  (:require [calc-35.mach1  :as mc])
  (:require [calc-35.rom    :as r])
  (:gen-class)  )

(def lastdisp (atom ""))

(defn -main "simple cli from *in" [& args]
  
 (mc/run-instrs r/romV4)  ;;default rom:0 ==> power on seq.
 (println (mc/disp) )

 (with-open [r (clojure.java.io/reader *in*) ]
   (doseq [line (line-seq r)]
     (let [ vch  (vec (seq line))  ]
       (println line)

       ;; Run cmds only when 1st char is a valid cmd-abbreviation
       ;;  implicitly treating others as comments
       
       (when  (r/afmap (vch 0))
         (doseq [i (range (count vch))]
           (reset! lastdisp  (mc/run-instr-seq (r/afmap (vch i)))) )
         
         (println @lastdisp) )

       ) )  )   )
