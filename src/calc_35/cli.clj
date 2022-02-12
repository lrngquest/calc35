(ns calc-35.cli
  (:require [calc-35.mach1  :as mc])
  (:require [calc-35.rom    :as r])
  (:gen-class)  )

(def lastdisp (atom ""))

(defn -main "simple cli from *in" [& args]  
 (mc/run-instrs r/romV4) ;;default rom:0 ==> power on seq.
 (println (mc/disp) " power on." )

 (with-open [r (clojure.java.io/reader *in*) ]
   (doseq [line (line-seq r)]
     (let [ vch  (vec (seq line))  ]
       (println line)
       (doseq [i (range (count vch))]
         (reset! lastdisp  (mc/run-instr-seq (r/afmap (vch i)))) )
       
       (println @lastdisp) ;; could add  (println @lastdisp)  in doseq i
       ) )  )
 )
