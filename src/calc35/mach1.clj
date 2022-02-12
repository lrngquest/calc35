(ns calc35.mach1  )

(def ms (atom {:a [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :b [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :c [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :d [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :e [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :f [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :m [0 0 0  0 0 0  0 0 0  0 0 0  0 0 ]
               :s [0 0 0  0 0 0  0 0 0  0 0 0] ;; 12
               :z [0 0 0  0 0 0  0 0 0  0 0 0  0 0] ;;constant all-zeroes
               :p 0
               :pc 0         :ret 0    :offset 0
               :carry 0      :prev-carry 0
               :disp 0       :ksft 0   :upTr 1  :cy 0   :lastkey 0})  )


(defn keyrom ""     [] (swap! ms assoc :pc (:lastkey @ms) :upTr 1)  )
(defn disptoggle "" []     (swap! ms assoc :disp  (- 1 (:disp @ms))  )  )
(defn dispoff ""    []     (swap! ms assoc :disp 0) )
(defn goto ""       [addr] (swap! ms assoc :pc
                                  (if (= 0 (:prev-carry @ms))   addr
                                                               (:pc @ms)) ) )
(defn jsb  "" [addr]  (swap! ms assoc  :ret (:pc @ms)  :pc addr) )
(defn retn "" []      (swap! ms assoc  :pc  (:ret @ms) ) )

(defn sets  "" [num val]
  (let [v  (:s @ms)  nv (assoc v num val) ]   (swap! ms assoc :s nv) ) )

(defn tests  "" [num]
  (let [[k u] (if (= 0 num) [(inc (:ksft @ms)) 0] [(:ksft @ms) (:upTr @ms)]) ]
    (swap! ms assoc :carry ((:s @ms) num)  :ksft k  :upTr u)) )

(defn setp   "" [val]  (swap! ms assoc :p val))
(defn testp  "" [num]  (swap! ms assoc :carry (if (= (:p @ms) num) 1 0) ) )
(defn incp   "" []     (swap! ms assoc :p (bit-and 15 (inc (:p @ms)))  ) )
(defn decp   "" []     (swap! ms assoc :p (bit-and 15 (dec (:p @ms)))  ) )
(defn setrom "" [num]  (swap! ms assoc :offset ([0 256 512] num)) )
    (declare setreg)    (declare negcdec)
(defn zeroreg "" [rky first last] (setreg rky :z first last))
(defn negc ""    [first last] (negcdec first last 0))  ;; 0 - c    -> c[f]
(defn negsubc "" [first last] (negcdec first last 1))  ;; 0 - c -1 -> c[f]

(defn stacka "" [] (swap! ms assoc  :a (:d @ms)  :d (:e @ms)  :e (:f @ms))  )


(defn field "" [val]  (if (= val -1)  (:p @ms)  val)  )

(defn setreg "" [rkaa rkbb first last]
  (let [l    (field last)    rtbb  (rkbb @ms)
        upd  (loop [i  (field first)   rtaa  (rkaa @ms)]
               (if (<= i l)
                 (recur  (inc i)  (assoc rtaa i  (rtbb i))  )
                 rtaa ) )   ] ;; export modified state value from loop
    (swap! ms assoc rkaa upd  :carry 0)    ))


(defn ir-dr-outer " common logic" [carryFn opFn  rky first last]
  (let [l    (field last)
        upd  (loop [i  (field first)   carry 1  rtmp  (rky @ms)]
               (if (<= i l)
                 (recur  (inc i)
                         (carryFn (rtmp i) 0 carry)  ;; new carry val
                         (assoc rtmp i (opFn (rtmp i) 0 carry) ) ) ;; new digit
                 [carry  rtmp] ) )  ] ;; export modified state values from loop
    (swap! ms assoc rky (upd 1)  :carry (upd 0))    )
  )

(defn doCarrySub "" [rtaa rtbb carryin]
  (let [res0  (- rtaa rtbb carryin) ]  (if (< res0 0)  1  0) ) )
(defn doSub "" [rtaa rtbb carryin]
  (let [res0  (- rtaa rtbb carryin) ]  (if (< res0 0)  (+ res0 10)  res0) ) )


(defn decreg "" [rky first last] (ir-dr-outer doCarrySub doSub  rky first last))


(defn doCarryAdd "" [rtaa rtbb carryin]
  (let [res0  (+ rtaa rtbb carryin)]  (if (> res0 9)  1  0) ) )
(defn doAdd "" [rtaa rtbb carryin]
  (let [res0  (+ rtaa rtbb carryin)]  (if (> res0 9)  (- res0 10)  res0) ) )


(defn increg "" [rky first last] (ir-dr-outer doCarryAdd doAdd  rky first last))


(defn exchreg "" [rkaa rkbb first last]
  (let [l    (field last)
        upd  (loop [i  (field first)   rtaa  (rkaa @ms)   rtbb  (rkbb @ms)]
               (if (<= i l)
                 (recur  (inc i)    (assoc rtaa i  (rtbb i))
                         (assoc rtbb i  (rtaa i))  )
                 [rtaa  rtbb] ) )   ]
    (swap! ms assoc rkaa (upd 0)  rkbb (upd 1)  :carry 0)    )  )


(defn negcdec "" [first last carryin]  ;; ala 0 - c[f]
  (let [lv  (field last)   rtaa  (:z @ms)  rtbb (:c @ms)
        upd (loop [i (field first)  carry carryin  rtdd (:c @ms)]
              (if (<= i lv)
                (recur  (inc i)
                        (doCarrySub (rtaa i) (rtbb i) carry)
                        (assoc rtdd i (doSub (rtaa i) (rtbb i) carry)) )
                [carry rtdd] )  )  ]
    (swap! ms assoc  :c (upd 1)  :carry (upd 0))  ))


(defn downrot "" []
  (swap! ms assoc  :c (:d @ms)  :d (:e @ms)  :e (:f @ms)  :f (:c @ms))   )


(defn clearregs "" []
  (let [z  (:z @ms)]
    (swap! ms assoc  :a z  :b z  :c z  :d z  :e z  :f z  :m z) )  )


(defn clears "" [] (swap! ms assoc  :s [0 0 0 0  0 0 0 0  0 0 0 0]) )


(defn shiftl "" [rkaa first last] ;;c/p/edit from 'shiftr'
  (let [f    (field first)
        upd  (loop [i  (field last)   rtaa  (rkaa @ms)]
               (if (>= i f)
                 (recur  (dec i) (assoc rtaa i (if (= i f) 0  (rtaa (dec i)))) )
                 rtaa ) )   ]
    (swap! ms assoc rkaa upd  :carry 0)   )  )

(defn shiftr "shift right r[f]" [rkaa first last]
  (let [ls   (field last)
        upd  (loop [i  (field first)   rtaa  (rkaa @ms)]
               (if (<= i ls)
                 (recur (inc i)  (assoc rtaa i (if (= i ls) 0 (rtaa (inc i)))) )
                 rtaa ) )   ]
    (swap! ms assoc rkaa upd  :carry 0)   )  )


(defn cstack "" [] (swap! ms assoc  :f (:e @ms)  :e (:d @ms)  :d (:c @ms))  )


(defn nonz "" [digit] (if (not= 0 digit) 1  0)  )

(defn ifregzero "if b[f] = 0" [rkaa first last]
  (let [lv   (field last)    fv  (field first)    rtaa  (rkaa @ms)
        rgsv (if (< lv 13)  (subvec rtaa fv (inc lv))  (subvec rtaa fv) )
        upd  (reduce bit-or 0 (map nonz rgsv) )  ]
      (swap! ms assoc  :carry upd))  )


(defn arith2 "outer logic" [carryFn opFn  rkdd rkaa rkbb first last]
  (let [lv  (field last)  rtaa (rkaa @ms)  rtbb (rkbb @ms)
       upd (loop [i (field first)  carry 0  rtdd (rkdd @ms)]
             (if (<= i lv)
               (recur  (inc i)
                       (carryFn (rtaa i) (rtbb i) carry)
                       (assoc rtdd i (opFn (rtaa i) (rtbb i) carry)) )
               [carry rtdd] )  )  ]
    (swap! ms assoc  rkdd (upd 1)  :carry (upd 0))  ))


(defn sub "" [rkdd rkaa rkbb first last]
  (arith2 doCarrySub doSub rkdd rkaa rkbb first last) )


(defn add "" [rkdd rkaa rkbb first last]
  (arith2 doCarryAdd doAdd  rkdd rkaa rkbb first last) )


(defn regsgte "if a >= c[f]" [rkaa rkbb first last]
  (let [lv  (field last)  rtaa (rkaa @ms)  rtbb (rkbb @ms)
        upd (loop [i (field first)  carry 0 ]
              (if (<= i lv)
                (recur  (inc i)   (doCarrySub (rtaa i) (rtbb i) carry) )
                carry )  )  ]  ;;set carry _ONLY_ !
    (swap! ms assoc  :carry upd)  )  )


(defn isz "" [digit] (if (= 0 digit) 1  0) )

(defn regsgte1 "if a[f] >= 1" [rkaa first last]
  (let [lv   (field last)    fv (field first)    rtaa  (rkaa @ms)
        rgsv (if (< lv 13)  (subvec rtaa fv (inc lv))  (subvec rtaa fv) )
        upd  (reduce bit-and 1 (map isz rgsv))   ]
    (swap! ms assoc  :carry upd)    )  )


(defn loadconst "no-op reqd if p >= 14 !" [num]
  ;;(println "loadconst p" (:p @ms)" " num)
  (let [op  (:p @ms)    oc  (:c @ms)  ]
    (swap! ms assoc :c (if (< op 14) (assoc  oc op num) oc)
           :p (bit-and 15 (dec op)) ) )
  )


(defn decodEx "decode and execute instruction" [opcode]
  (let [n    (bit-shift-right (bit-and opcode 0x3fc) 2) ;;must >>2
        op   (bit-shift-right opcode 5);; >>5
        fld  (bit-and 7 (bit-shift-right opcode 2) );; opcode>>2
        [first last] ([[-1 -1] [3 12] [0 2] [0 13]
                       [0 -1]  [3 13] [2 2] [13 13]] fld)
        arg  (bit-shift-right n 4) ]
    
    (case (bit-and opcode 3)
      3  (goto n)
      1  (jsb  n)
      2  (case op
           0  (ifregzero :b first last)  ;; if b[f] = 0
           1  (zeroreg :b first last)    ;; 0 -> b[f]
           2  (regsgte :a :c first last) ;; if a >= c[f]
           3  (regsgte1 :c first last)   ;; if c[f] >= 1
           4  (setreg :c :b first last ) ;; b -> c[f]
           5  (negc first last)          ;; 0 - c -> c[f]
           6  (zeroreg :c first last)    ;; 0 -> c[f]
           7  (negsubc first last)       ;; 0 - c -1 -> c[f]
           8  (shiftl :a first last)     ;; shift left a[f]
           9  (setreg :b :a first last)  ;; a -> b[f]
           10 (sub :c :a :c first last)  ;; a - c -> c[f]
           11 (decreg :c first last)     ;; c - 1 -> c[f]
           12 (setreg :a :c first last)  ;; c -> a[f]
           13 (ifregzero :c first last)  ;; if c[f] = 0
           14 (add :c :a :c first last)  ;; a + c -> c[f]
           15 (increg :c first last)     ;; c + 1 -> c[f]
           16 (regsgte :a :b first last) ;; if a >= b[f]
           17 (exchreg :b :c first last) ;; b exch c[f]
           18 (shiftr :c first last)     ;; shift right c[f]
           19 (regsgte1 :a first last)   ;; if a[f] >= 1
           20 (shiftr :b first last)     ;; shift right b[f]
           21 (add :c :c :c first last)  ;; c + c -> c[f]
           22 (shiftr :a first last)     ;; shift right a[f]
           23 (zeroreg :a first last)    ;; 0 -> a[f]
           24 (sub :a :a :b first last)  ;; a - b -> a[f]
           25 (exchreg :a :b first last) ;; a exch b[f]
           26 (sub :a :a :c first last)  ;; a - c -> a[f]
           27 (decreg :a first last)     ;; a - 1 -> a[f]
           28 (add :a :a :b first last)  ;; a + b -> a[f]
           29 (exchreg :a :c first last) ;; a exch c[f]
           30 (add :a :a :c first last)  ;; a + c -> a[f]
           31 (increg :a first last)   ) ;; a + 1 -> a[f]
      0  (case (bit-and n 0xf)
           0   0 ;; no-op
           1   (sets arg 1)
           3   (setp arg)
           4   (cond
                (= arg 3)    (keyrom )
                (even? arg)   (setrom (bit-shift-right opcode 7))  )
           5   (tests arg) ;;arg==0 ==> key_input
           6   (loadconst arg)
           7   (decp )
           9   (sets arg 0)
           10  (case arg
                 0   (disptoggle )
                 2   (exchreg :c :m 0 13)
                 4   (cstack )
                 6   (stacka )
                 8   (dispoff )
                 10  (setreg :c :m 0 13)
                 12  (downrot )
                 14  (clearregs )    )
           11   (testp arg)
           12   (retn )
           13   (clears )
           15   (incp )     )
      ))
  (swap! ms assoc :cy (inc (:cy @ms)) )
  )

(def romV4 [  ;;extracted from "hp35.zip"
221 767 548 23 324 580 132 272 721 1019 95 195 424 871 750 994
46 144 1002 1002 1002 107 617 168 680 255 1002 1002 1002 48 204 170
424 67 467 204 48 0 131 324 68 187 580 159 644 779 46 144
808 879 1002 1002 1002 75 615 936 369 887 971 718 196 475 296 52
718 885 302 762 278 874 899 442 923 822 844 923 28 490 2 307
708 726 934 276 543 381 887 210 370 218 906 375 206 52 398 780
298 394 442 419 170 378 351 332 938 276 267 810 42 989 266 718
812 551 946 491 721 144 276 987 946 250 398 442 511 218 170 844
278 362 638 315 630 515 202 989 726 414 812 591 142 494 76 274
60 418 575 942 236 999 202 388 491 254 424 46 1018 1018 506 506
74 655 942 934 422 671 942 550 74 763 654 1002 14 763 675 758
212 723 894 254 468 735 296 452 206 366 190 510 558 48 144 369
324 887 718 414 548 831 506 516 340 823 490 795 40 20 799 36
28 812 835 552 532 819 270 356 208 296 942 373 452 989 701 555
726 28 172 279 780 750 758 994 994 140 60 866 959 2 939 994
814 48 260 724 115 447 254 676 783 404 1011 28 658 489 680 879

975 814 161 424 161 424 596 39 942 340 75 222 665 296 661 609
149 424 665 660 875 750 994 294 934 362 658 442 103 722 490 119
718 654 296 558 263 558 268 891 296 942 418 183 174 398 138 815
398 84 151 660 439 340 87 254 958 55 658 894 235 510 818 466
814 302 850 239 424 718 946 814 274 296 1022 1022 143 206 42 726
713 354 424 942 268 657 396 621 524 621 140 536 652 621 569 621
817 270 621 142 813 817 686 665 596 435 254 609 100 206 354 490
84 663 665 817 686 661 817 686 686 597 686 941 817 652 625 569
524 629 140 536 396 625 268 625 625 814 590 844 344 1007 396 536
408 344 152 280 600 84 875 48 750 994 16 272 270 662 558 647
510 782 643 910 272 272 330 272 482 846 675 974 270 28 594 44
679 183 482 790 715 918 278 28 44 719 183 28 918 879 16 378
378 746 862 638 795 272 518 811 254 814 782 272 206 716 472 536
344 216 600 536 88 408 216 344 780 48 16 906 891 354 510 44
751 938 746 98 923 718 590 554 202 780 699 272 658 658 382 947
466 786 562 142 894 955 946 424 30 7 270 946 296 658 382 574

16 830 1022 598 274 75 424 665 398 532 267 750 838 3 718 382
3 510 302 601 866 71 818 926 7 460 437 524 629 588 625 1017
652 625 501 716 625 893 625 741 625 985 942 334 26 191 334 814
28 270 108 195 942 446 227 230 490 716 789 596 27 340 595 985
669 595 985 945 741 716 621 893 652 621 501 588 621 1017 524 621
621 621 396 754 844 558 942 408 571 148 379 1002 634 779 790 359
918 270 362 371 718 210 938 446 435 814 782 238 718 558 206 358
148 475 280 486 487 408 108 471 590 590 148 595 48 460 216 216
24 536 344 24 600 939 601 994 302 382 539 722 942 278 942 894
547 814 994 817 144 722 894 599 766 910 48 144 718 558 643 910
382 639 942 278 942 439 204 458 350 687 190 806 750 812 791 102
731 84 3 146 870 506 562 934 144 548 408 600 216 88 280 472
88 923 998 403 910 354 787 718 60 876 791 490 766 780 46 610
859 270 362 622 831 206 298 910 638 799 934 398 46 780 491 588
216 88 24 88 472 600 536 24 344 344 216 887 942 302 390 698
379 506 718 490 971 415 206 780 152 216 24 152 344 519 332 507
] ) ;; 768 10-bit entries as base10 (unsigned) int


(defn disp "" []
  (loop [i 13   dstr ""]
    (if       (>= i 0)
      (recur
       (dec i)
       (let [ai ((:a @ms)i)   bi ((:b @ms)i)
             d1  (cond
                  (>= bi 8)              (str dstr " ")
                  (or (= i 2) (= i 13))  (str dstr (if (>= ai 8) "-" " "))
                  :else                  (str dstr ai))
             d2  (if (= bi 2)  (str d1 ".")  d1)  ]
         d2)  )
      
      dstr) )   )

(def  kw-2-addr
  {:k0 36     :k1 28     :k2 27      :k3 26     :k4 20      :k5 19    :k6 18
   :k7 52     :k8 51     :k9 50      :kDot 35   :kEnter 62  :kSub 54  :kAdd 22
   :kMult 30  :kDiv 38   :kPi 34     :kChs 59   :kClr 0     :kClx 56  :kEex 58
   :kArc 44   :kSin 43   :kCos 42    :kTan 40   :kLn 3      :kLog 4   :kExp 2
   :kPow 6    :kSqrt 46  :kRecip 14  :kSwap 12  :kRoll 11   :kSto 10  :kRcl 8} )


(defn run-instrs "run from  offset+pc until wait-loop" [rom traceq]
  (loop []
    (let [pc (:pc @ms)  upTr (:upTr @ms)  ra (+ (:offset @ms) pc)]
      
      (swap! ms assoc :prev-carry (:carry @ms)  :carry 0
             :pc (bit-and (inc pc) 255) )
      
      (decodEx (rom  ra) )
      
      (if (and (= upTr 1) (= 0 (:upTr @ms)))
        0                  ;; wait-loop detected ==> suspend
        (recur )  )   )
    )
  )


(defn run-instr-seq "" [ksra]
  (swap! ms assoc
         :lastkey  ksra
         :s        (assoc (:s @ms) 0 1) )
  (run-instrs romV4 0)  ;; simplify formal-params ?  Todo
  (disp )
  )
