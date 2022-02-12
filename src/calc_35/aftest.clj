( ns calc-35.aftest
  (:require [calc-35.mach1  :as mc])
  (:require [calc-35.rom    :as r])
  (:gen-class) )

(def inp-expA
  [ {:inp "~1.2345678909x~35"  :exp "-1.234567890-35"}
    {:inp "p"                  :exp " 3.141592654   "}
    {:inp "2 3+"               :exp " 5.            "}
    {:inp "3 2-"               :exp " 1.            "}
    {:inp "3 2*"               :exp " 6.            "}
    {:inp "3 2/"               :exp " 1.5           "}
    {:inp "4f"                 :exp " .25           "}
    {:inp "25q"                :exp " 5.            "}
    {:inp "pl"                 :exp " 1.144729886   "}
    {:inp "pg"                 :exp " .4971498728   "}
    {:inp "pe"                 :exp " 23.14069264   "}
    {:inp "ps"                 :exp " .054803665    "}
    {:inp "pc"                 :exp " .9984971498   "}
    {:inp "pt"                 :exp " .0548861507   "}
    {:inp "pgas"               :exp " 29.81161556   "}
    {:inp "pgac"               :exp " 60.18838444   "}
    {:inp "pat"                :exp " 72.34321286   "}
    {:inp "1 2 3 4 5wrrr"      :exp " 2.            "}
    {:inp "123>456 ,<"         :exp " 123.          "}
    {:inp "9sctatacas"         :exp " 9.004076901   "}
    {:inp "9 2^"               :exp " 511.9999999   "}  ] )


;; afmap moved to  rom.clj

(def lastdisp (atom ""))

(defn -main "" []

  (mc/run-instrs r/romV4)  ;; MUST do "power on' instr-seq !!
  
  (doseq [t (range (count inp-expA))]
    ;;(printf "%17s    |%s|\n" (:inp (inp-expA t)) (:exp (inp-expA t)) )

    (let [n  (:inp (inp-expA t))   x  (:exp (inp-expA t))    vch  (vec (seq n))]

      (doseq [i (range (count vch))]
        (reset! lastdisp  (mc/run-instr-seq (r/afmap (vch i)))) )

      (if (= @lastdisp x)
        (printf "t:%2d OK\n" t)
        (printf "t:%2d    x:|%s|  act:|%s| %s\n" t  x @lastdisp vch) )
      )
    )
  )
