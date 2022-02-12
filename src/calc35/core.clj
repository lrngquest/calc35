(ns calc35.core
  (:import (java.awt Color Dimension Font FontMetrics) 
           (javax.swing JPanel JFrame  JOptionPane)
           (java.awt.event  KeyListener KeyEvent  MouseListener MouseEvent))
  (:require [calc35.mach1  :as mc])
  (:gen-class) )  ;; 2019May14


(def keyinfo [
[[ 14 100 30 24] "x^y"    ""  (:kPow mc/kw-2-addr)   :white :black  \U]
[[ 62 100 30 24] "log"    ""  (:kLog mc/kw-2-addr)   :white :black  \G]
[[110 100 30 24] "ln"     ""  (:kLn  mc/kw-2-addr)   :white :black  \L]
[[158 100 30 24] "e^x"    ""  (:kExp mc/kw-2-addr)   :white :black  \E]
[[206 100 30 24] "CLR"    ""  (:kClr mc/kw-2-addr)   :white :blue   \\]

[[ 14 148 30 24] "sqrt x" ""  (:kSqrt mc/kw-2-addr)  :white :black    \Q]
[[ 62 148 30 24] "arc"    ""  (:kArc mc/kw-2-addr)   :white :med-grey \V]
[[110 148 30 24] "sin"    ""  (:kSin mc/kw-2-addr)   :white :med-grey \S]
[[158 148 30 24] "cos"    ""  (:kCos mc/kw-2-addr)   :white :med-grey \C]
[[206 148 30 24] "tan"    ""  (:kTan mc/kw-2-addr)   :white :med-grey \T]

[[ 14 196 30 24] "1/x"    ""  (:kRecip mc/kw-2-addr) :white :black \F]
[[ 62 196 30 24] "x<>y"   ""  (:kSwap mc/kw-2-addr)  :white :black \W]
[[110 196 30 24] "Rdwn"   ""  (:kRoll mc/kw-2-addr)  :white :black \R]
[[158 196 30 24] "STO"    ""  (:kSto  mc/kw-2-addr)  :white :black \B]
[[206 196 30 24] "RCL"    ""  (:kRcl  mc/kw-2-addr)  :white :black \D]

[[ 14 244 78 24] "ENTER^" ""  (:kEnter mc/kw-2-addr) :white :blue  \space]
[[110 244 30 24] "CHS"    ""  (:kChs mc/kw-2-addr)   :white :black \H]
[[158 244 30 24] "EEX"    ""  (:kEex mc/kw-2-addr)   :white :black \X]
[[206 244 30 24] "CLX"    ""  (:kClx mc/kw-2-addr)   :white :black \,]

[[ 14 292 24 24] "-"      ""  (:kSub mc/kw-2-addr)   :white :blue  \-]
[[ 63 292 37 24] "7"      ""  (:k7 mc/kw-2-addr)     :white :black \7]
[[131 292 37 24] "8"      ""  (:k8 mc/kw-2-addr)     :white :black \8]
[[199 292 37 24] "9"      ""  (:k9 mc/kw-2-addr)     :white :black \9]

[[ 14 340 24 24] "+"      ""  (:kAdd mc/kw-2-addr)   :white :blue  \A]
[[ 63 340 37 24] "4"      ""  (:k4   mc/kw-2-addr)   :white :black \4]
[[131 340 37 24] "5"      ""  (:k5   mc/kw-2-addr)   :white :black \5]
[[199 340 37 24] "6"      ""  (:k6   mc/kw-2-addr)   :white :black \6]

[[ 14 388 24 24] "x"      ""  (:kMult mc/kw-2-addr)  :white :blue  \M]
[[ 63 388 37 24] "1"      ""  (:k1    mc/kw-2-addr)  :white :black \1]
[[131 388 37 24] "2"      ""  (:k2    mc/kw-2-addr)  :white :black \2] 
[[199 388 37 24] "3"      ""  (:k3    mc/kw-2-addr)  :white :black \3]

[[ 14 436 24 24] "/"      ""  (:kDiv mc/kw-2-addr)   :white :blue  \/]
[[ 63 436 37 24] "0"      ""  (:k0   mc/kw-2-addr)   :black :white \0]
[[131 436 37 24] "."      ""  (:kDot mc/kw-2-addr)   :black :white \.]
[[199 436 37 24] "Pi"     ""  (:kPi  mc/kw-2-addr)   :black :white \P]      ])

(defn ffn "point-in-rect filter fn" [x y kv]
  (let [[rx ry wd ht]  (kv 0) ] ;;get the rect, de-structure
    (and (>= x rx) (< x (+ rx wd)) (>= y ry) (< y (+ ry ht))) )  )

(defn kcffn "key shortcut filter fn" [ch kv]  (= ch (kv 6))  )


(def DISPLAY_HEIGHT 52)  (def WINDOW_WIDTH 255)  (def WINDOW_HEIGHT 484)
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
  (.fillRect g 0 DISPLAY_HEIGHT  WINDOW_WIDTH (- WINDOW_HEIGHT DISPLAY_HEIGHT) )
  (.setFont g (:key-font @f-fm))
  (doseq [i (range (count keyinfo))]
    (let [[rect lbl uu krsa fg bg]  (keyinfo i)]
      (draw-string g lbl rect fg bg (:kfm @f-fm)) )  )  )


(defn draw-disp "" [ ^sun.java2d.SunGraphics2D g s]
  (.setFont g (:disp-font @f-fm))
  (draw-string g s  [0 0 WINDOW_WIDTH DISPLAY_HEIGHT]
               :br-red :dk-red  (:dfm @f-fm)  ))
  

(def aadr (atom 0) ) ;;instruction sequence start addr

(defn calc-panel [frame ]
  (proxy [JPanel  KeyListener MouseListener] []
    (paintComponent [ ^sun.java2d.SunGraphics2D g] 
      ;;(proxy-super paintComponent g)  ;;why?
      (draw-calc g)
      (draw-disp g  (mc/run-instr-seq @aadr) )    )
   
    (keyPressed [ ^KeyEvent e] 
      (let [kc    (.getKeyCode e)
            kirow (first (filter (partial kcffn (char kc)) keyinfo)) ]
        (when kirow
          (swap! aadr  (constantly (kirow 3)))
          (.repaint ^JPanel this))  )    )
    (keyReleased [e])  (keyTyped [e])

    (mouseClicked [ ^MouseEvent e]
      (let [x  (.getX e)
            y  (.getY e) 
            kirow  (first (filter (partial ffn x y) keyinfo)) ]
        (when kirow
          (swap!  aadr  (constantly (kirow 3)))
          (.repaint ^JPanel this) )   ) )
    (mousePressed [e] )  (mouseReleased[e] ) (mouseEntered [e] )
    (mouseExited  [e] )
    
    (getPreferredSize []  (Dimension.  WINDOW_WIDTH  WINDOW_HEIGHT ) )    ))


(defn calcfn [] 
  (let [frame (JFrame. "calc35")
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


