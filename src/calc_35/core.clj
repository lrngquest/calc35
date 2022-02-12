(ns calc-35.core
  (:import (java.awt Color Dimension Font FontMetrics) 
           (javax.swing JPanel JFrame  JOptionPane)
           (java.awt.event  KeyListener KeyEvent  MouseListener MouseEvent))
  (:require [calc-35.mach1  :as mc])  (:require [calc-35.rom  :as r])
  (:gen-class) )  ;; 2019May14


(declare keyinfo  ffn)   (def aadr (atom 0) ) ;;instr. seq. start addr

(def DISPLAY_HT 52)  (def WINDOW_WD 255)  (def WINDOW_HT 484)
(def colormap {:black    (Color. 0x000000)  :white  (Color. 0xFFFFFF)
               :med-grey (Color. 0x585858)  :dk-grey (Color. 0x383838)
               :blue   (Color. 0x4040ff)
               :dk-red   (Color. 0x500000)  :br-red (Color. 0xff4000) } )

(def buttonwidth 30)  (def buttonheight 24)

(defn mkfnt [i] (if (< i 6) 6  (Font.  "Helvetica" Font/BOLD i) ) );;PLAIN
(def ftv (vec (map mkfnt (range 25) )) )

(def f-fm (atom {:key-font (ftv 0)  :kfm 0  :disp-font (ftv 5)  :dfm 0}) )

(defn init "atom with {key,disp}-font and their font metrics" [ ^JFrame jfrm]
  (loop [i  24   key-font (ftv 24)   kfm  (.getFontMetrics jfrm (ftv 24)) ]
    (if (and (> i 6)  ;;find largest font satisfying constraints
             (>= buttonwidth  (+ 4 (.stringWidth kfm "x<>y")))
             (>= buttonheight (+ 2 (.getHeight kfm )))  ;;DisplayRegWidth 116
             (>= 116          (+ 2 (.stringWidth kfm "012345677890123"))) )
      (do
        (swap! f-fm  assoc  :key-font key-font
              :kfm kfm
              :disp-font  (ftv (min 24 (+ i 9) ));;was + 5
              :dfm        (.getFontMetrics jfrm  (ftv (min 24 (+ i 5) )) ) )   )
      (recur  (dec i)
              (ftv  (dec i))
              (.getFontMetrics jfrm (ftv (dec i))) )    )  ) )


(defn draw-string "" [ ^sun.java2d.SunGraphics2D g  ^String s
                      [rx ry wd ht] fg bg  ^FontMetrics fm]
  (let [descent      (.getMaxDescent fm)
        sheight      (.getHeight     fm)
        swidth       (.stringWidth   fm  s) ]
    (.setColor g (bg colormap) ) ;; was color[ fg]
    (.fillRect g rx ry wd ht)
    (.setColor g (fg colormap) )  ;; 'int' required below, else we get  ratio.
    (.drawString g s  (int(+ rx (/ (- wd swidth) 2)))
                 (int (+ ry (/ (- (* 2 ht) sheight) 2) descent)) )    )   )


(defn draw-calc "" [ ^sun.java2d.SunGraphics2D g]
  (.setColor g (:dk-grey colormap) )
  (.fillRect g 0 DISPLAY_HT  WINDOW_WD (- WINDOW_HT DISPLAY_HT) )
  (.setFont g (:key-font @f-fm))
  (doseq [i (range (count keyinfo))]
    (let [[rect lbl uu krsa fg bg]  (keyinfo i)]
      (draw-string g lbl rect fg bg (:kfm @f-fm)) )  )  )


(defn draw-disp "" [ ^sun.java2d.SunGraphics2D g s]
  (.setFont g (:disp-font @f-fm))
  (draw-string g s  [0 0 WINDOW_WD DISPLAY_HT] :br-red :dk-red  (:dfm @f-fm) ))
  

(defn calc-panel [frame ]
  (proxy [JPanel  KeyListener MouseListener] []
    (paintComponent [ ^sun.java2d.SunGraphics2D g] 
      (draw-calc g)
      (draw-disp g  (mc/run-instr-seq @aadr) )    )

    (keyTyped [ ^KeyEvent e]
      (let [ ksa   (r/afmap (char (.getKeyChar e)))     ]       
        (when ksa  (swap! aadr  (constantly ksa))  (.repaint^JPanel this) ) )  )
   
    (keyPressed [e]) (keyReleased [e])

    
    (mouseClicked [ ^MouseEvent e]
      (let [x  (.getX e)        y  (.getY e) 
            kirow  (first (filter (partial ffn x y) keyinfo)) ]
        (when kirow
          (swap!  aadr  (constantly (kirow 3))) (.repaint ^JPanel this) )  ) )

    (mousePressed [e] )  (mouseReleased[e] ) (mouseEntered [e] )
    (mouseExited  [e] )
    
    (getPreferredSize []  (Dimension.  WINDOW_WD  WINDOW_HT ) )    ))


(defn calcfn [] 
  (let [frame (JFrame. "calc-35")
        _             (init frame)
        ^JPanel panel (calc-panel frame )  ]
    (doto panel 
      (.setFocusable true)
      (.addKeyListener panel)  (.addMouseListener panel ) )
    (doto frame 
      (.add panel)
      (.pack)
      (.setDefaultCloseOperation (JFrame/EXIT_ON_CLOSE)) ;;added
      (.setVisible true))   ) ) 

(defn -main [] (calcfn) )

;;Swing gui app inspired by:
;; github.com/stuarthalloway/programming-clojure/
;;     blob/master/src/examples/atom_snake.clj
;;
;;and the Java applet code from
;; Jacques Laporte  and  David G. Hicks

;;Microcode "machine" based on:
;;  https://github.com/AshleyF/HP35


(def keyinfo [  ;; 2021May15 change to AshleyF 1-char abbreviations
[[ 14 100 30 24] "x^y"    ""  (r/afmap \^)   :white :black ]
[[ 62 100 30 24] "log"    ""  (r/afmap \g)   :white :black ]
[[110 100 30 24] "ln"     ""  (r/afmap \l)   :white :black ]
[[158 100 30 24] "e^x"    ""  (r/afmap \e)   :white :black ]
[[206 100 30 24] "CLR"    ""  (r/afmap \!)   :white :blue  ]

[[ 14 148 30 24] "sqrt x" ""  (r/afmap \q)   :white :black    ]
[[ 62 148 30 24] "arc"    ""  (r/afmap \a)   :white :med-grey ]
[[110 148 30 24] "sin"    ""  (r/afmap \s)   :white :med-grey ]
[[158 148 30 24] "cos"    ""  (r/afmap \c)   :white :med-grey ]
[[206 148 30 24] "tan"    ""  (r/afmap \t)   :white :med-grey ]

[[ 14 196 30 24] "1/x"    ""  (r/afmap \f)   :white :black ]
[[ 62 196 30 24] "x<>y"   ""  (r/afmap \w)   :white :black ]
[[110 196 30 24] "Rdwn"   ""  (r/afmap \r)   :white :black ]
[[158 196 30 24] "STO"    ""  (r/afmap \>)   :white :black ]
[[206 196 30 24] "RCL"    ""  (r/afmap \<)   :white :black ]

[[ 14 244 78 24] "ENTER^" ""  (r/afmap \space) :white :blue ]
[[110 244 30 24] "CHS"    ""  (r/afmap \~)   :white :black ]
[[158 244 30 24] "EEX"    ""  (r/afmap \x)   :white :black ]
[[206 244 30 24] "CLX"    ""  (r/afmap \,)   :white :black ]

[[ 14 292 24 24] "-"      ""  (r/afmap \-)   :white :blue  ]
[[ 63 292 37 24] "7"      ""  (r/afmap \7)   :white :black ]
[[131 292 37 24] "8"      ""  (r/afmap \8)   :white :black ]
[[199 292 37 24] "9"      ""  (r/afmap \9)   :white :black ]

[[ 14 340 24 24] "+"      ""  (r/afmap \+)   :white :blue  ]
[[ 63 340 37 24] "4"      ""  (r/afmap \4)   :white :black ]
[[131 340 37 24] "5"      ""  (r/afmap \5)   :white :black ]
[[199 340 37 24] "6"      ""  (r/afmap \6)   :white :black ]

[[ 14 388 24 24] "x"      ""  (r/afmap \*)   :white :blue  ]
[[ 63 388 37 24] "1"      ""  (r/afmap \1)   :white :black ]
[[131 388 37 24] "2"      ""  (r/afmap \2)   :white :black ] 
[[199 388 37 24] "3"      ""  (r/afmap \3)   :white :black ]

[[ 14 436 24 24] "/"      ""  (r/afmap \/)   :white :blue  ]
[[ 63 436 37 24] "0"      ""  (r/afmap \0)   :black :white ]
[[131 436 37 24] "."      ""  (r/afmap \.)   :black :white ]
[[199 436 37 24] "Pi"     ""  (r/afmap \p)   :black :white ]      ])


(defn ffn "point-in-rect filter fn" [x y kv]
  (let [[rx ry wd ht]  (kv 0) ] ;;get the rect, de-structure
    (and (>= x rx) (< x (+ rx wd)) (>= y ry) (< y (+ ry ht))) )  )

