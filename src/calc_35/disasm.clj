(ns calc-35.disasm
  (:require [calc-35.mach1  :as mc])
  (:require [calc-35.rom    :as r])  )


(def fs32  [
   ["ifregzero( b, %d, %d)"  []]   ["zeroreg( b, %d,%d)"    [:b]]
   ["regsgte( a, c, %d,%d)"  []]   ["regsgte1( c, %d,%d)"    []]  ;;4
   ["setreg( c, b, %d,%d)"  [:c]]  ["negc( %d,%d)"          [:c]]
   ["zeroreg( c, %d,%d)"    []]    ["negsubc( %d,%d)"       [:c]] ;;8  ;;zer
   ["shiftl( a, %d,%d)"     [:a]]  ["setreg( b, a, %d,%d)"  [:b]]
   ["sub( c, a, c, %d,%d)"  [:c]]  ["decreg( c, %d,%d)"     [:c]] ;;12
   ["setreg( a, c, %d,%d)"  [:a]]  ["ifregzero( c, %d,%d )"  []]
   ["add( c, a, c, %d,%d )" [:c]]  ["increg( c, %d,%d)"     [:c]] ;; 16
   ["regsgte( a, b, %d,%d)"  []]   ["exchreg( b, c, %d,%d)" [:b :c]]
   ["shiftr( c, %d,%d )"    [:c]]  ["regsgte1( a, %d,%d)"   []]   ;;20
   ["shiftr( b, %d,%d)"     [:b]]  ["add( c, c, c, %d,%d)"  [:c]] 
   ["shiftr( a, %d,%d)"     [:a]]  ["zeroreg( a, %d,%d)"    []]   ;;24 ;;zer
   ["sub( a, a, b, %d,%d)"  [:a]]  ["exchreg( a, b, %d,%d)" [:a :b]]
   ["sub( a, a, c, %d,%d)"  [:a]]  ["decreg( a, %d,%d)"     [:a]]
   ["add( a, a, b, %d,%d)"  [:a]]  ["exchreg( a, c, %d,%d)" [:a :c]]
   ["add( a, a, c, %d,%d )" [:a]]  ["increg( a, %d,%d)"     [:a]]   ]  )

(def trc (atom { :offset 0  :pc 0  :dis ""  :regkys []  :valkys []}) )


(defn pre-ex "capture info before instr is exec" [offset pc]
  (let [opcode  (r/romV4 (+ offset pc))
        n       (bit-shift-right (bit-and opcode 0x3fc) 2)
        op      (bit-shift-right opcode 5)
        fld     (bit-and 7 (bit-shift-right opcode 2) )
        [first last] ([[-1 -1] [3 12] [0 2] [0 13]
                       [0 -1]  [3 13] [2 2] [13 13]] fld)
        arg     (bit-shift-right n 4)
        opc2b   (bit-and opcode 3)
        n4b     (bit-and n  0xF)   ]
 
    (swap! trc assoc  :offset offset :pc pc   :regkys []  :valkys [])
    
    (cond
     (= opc2b  3)  (swap! trc assoc :dis (format "goto(%d)"  n) )
     (= opc2b  1)  (swap! trc assoc :dis (format "jsb(%d)"   n) )    
     (= opc2b  2)
       (swap! trc assoc :dis (format ((fs32 op) 0)   first last)
            :regkys ((fs32 op) 1))

     (and (= opc2b 0) (= n4b 0))    (swap! trc assoc :dis "nop")
     (and (= opc2b 0) (= n4b 1))
       (swap! trc assoc :dis (format "sets( %d,1)" arg)  :regkys [:s])
     (and (= opc2b 0) (= n4b 3))
       (swap! trc assoc :dis (format "setp(%d)" arg)  :valkys [:p])

     (and (= opc2b 0) (= n4b 4)  (= arg 3))
       (swap! trc assoc :dis "keyrom" :valkys [:pc])
     (and (= opc2b 0) (= n4b 4)  (even? arg))
       (swap! trc assoc :dis (format "setrom( %d)" (bit-shift-right opcode 7 ))
            :valkys [:offset])

     (and (= opc2b 0) (= n4b 5))
       (swap! trc assoc :dis (format "tests( %d)" arg) )
     (and (= opc2b 0) (= n4b 6))
       (swap! trc assoc :dis (format "loadconst( %d)" arg)
            :regkys [:c]  :valkys [:p])
     (and (= opc2b 0) (= n4b 7))  (swap! trc assoc :dis "decp"  :valkys [:p] ) 
     (and (= opc2b 0) (= n4b 9))
       (swap! trc assoc :dis (format "sets( %d,0)" arg)  :regkys [:s])

     (and (= opc2b 0) (= n4b 10) (= arg  0)) (swap! trc assoc :dis "disptoggle")
     (and (= opc2b 0) (= n4b 10) (= arg  2))  
       (swap! trc assoc :dis "exchreg( c, m, 0,13)"  :regkys [:c :m])  
     (and (= opc2b 0) (= n4b 10) (= arg  4))
       (swap! trc assoc :dis "cstack"  :regkys [:d :e :f])
     (and (= opc2b 0) (= n4b 10) (= arg  6))
        (swap! trc assoc :dis "stacka"  :regkys [:a :d :e])
     (and (= opc2b 0) (= n4b 10) (= arg  8))  (swap! trc assoc :dis "dispoff")
     (and (= opc2b 0) (= n4b 10) (= arg 10))
       (swap! trc assoc :dis  "setreg( c, m,  0,13)"   :regkys [:c])
     (and (= opc2b 0) (= n4b 10) (= arg 12))
       (swap! trc assoc :dis "downrot"  :regkys [:c :d :e :f])
     (and (= opc2b 0) (= n4b 10) (= arg 14))  ;; s/b omit trace -v ?  TODO
       (swap! trc assoc :dis "clearregs"  :regkys [:a :b :c :d :e :f :m])

     (and (= opc2b 0) (= n4b 11))
       (swap! trc assoc  :dis (format "testp( %d)" arg) ) ;; :carry ?
     (and (= opc2b 0) (= n4b 12))  (swap! trc assoc :dis "retn" )  ;; :pc
     (and (= opc2b 0) (= n4b 13))  (swap! trc assoc :dis "clears"  :regkys [:s])
     (and (= opc2b 0) (= n4b 15))  (swap! trc assoc :dis "incp" :valkys [:p])  )
    )) ;; let,defn


(defn post-ex "print trace line(s) after instr exec" []
  (let [{:keys  [:offset :pc :dis :valkys :regkys]}  @trc
        longaddr  (+ offset pc)
        pg        (bit-shift-right longaddr 8)
        cycls     (:cy @mc/ms)
        sv        (if (> (count valkys) 0)
                    (format "%s %s" (valkys 0) ((valkys 0) @mc/ms)) "")     ]   
    (when (= (mod cycls 10) 0)  (printf "   cycles %d\n" cycls) )
    (printf " %3d %d.%d %s %s\n" longaddr pg pc dis sv )
    (doseq [x (range(count regkys))]
      (printf "%s %s\n" (regkys x) ((regkys x) @mc/ms)))    )  )


(defn t-run-instrs "run from  offset+pc until wait-loop" [rom ]
  (loop []
    (let[{:keys [:pc :upTr :offset :carry] } @mc/ms      ]

      (pre-ex  offset pc)  ;; tracing hook 1of2
      (swap! mc/ms assoc :prev-carry carry  :carry 0 :pc (bit-and(inc pc) 255) )
      
      (mc/decodEx (rom  (+ offset  pc)) )
      (post-ex )   ;; tracint hook 2of2
                      ;; wait-loop detected ---v  ==> suspend at checkpoint
      (if (and (= upTr 1) (= 0 (:upTr @mc/ms)))   0   (recur )) )  )  )


(defn t-run-instr-seq "from ckpt run key-seq" [ksra]
  (swap! mc/ms assoc  :lastkey  ksra   :s  (assoc (:s @mc/ms)  0 1) )
  (t-run-instrs r/romV4)
  (mc/disp )  )


(def lastdisp (atom ""))

(defn -main "arg0 as input str ala  cli.clj" [& args] 
     (t-run-instrs r/romV4)  ;;default rom:0 ==> power on seq.
     (printf "%s  (cycles %d)\n" (mc/disp) (:cy @mc/ms) )
     
     (let [ vch  (vec (seq (first args))) ]
       (when  (r/afmap (vch 0))
         (doseq [i (range (count vch))]
           (reset! lastdisp  (t-run-instr-seq (r/afmap (vch i)))) )
         
         (printf "%s  (cycles %d)\n" @lastdisp (:cy @mc/ms))  )   )   )

;; bpmach EdV$ bb -cp src -m calc-35.disasm "p"
;;  SUCCESS!
