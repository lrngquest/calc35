# calc35


An emulation of HP "classic" architecture hosting  HP-35 microcode v4, as a Clojure learning exercise.



## Background

Articles in the HP-Journal

| Title                               | Author       | Date    | Pages|
|:------------------------------------|:-----------  |:------- |:-----|
|The 'Powerful Pocketful'...          |Whitney, et al|Jun 1972 | 2-9  |
|Algorithms and Accuracy in the HP-35 |D.S. Cochran  |Jun 1972 |10-11 |
|The New Accuracy: Making 2^3 = 8     |D.W. Harms    |Nov 1976 |16-17 |
|I.   Square Root                     |W.E. Egbert   |May 1977 |22-24 |
|II.  Trigonometric Functions         |W.E. Egbert   |Jun 1977 |17-20 |
|III. Inverse Trigonometric Functions |W.E. Egbert   |Nov 1977 |22-23 |
|IV.  Logarithmic Functions           |W.E. Egbert   |Apr 1978 |29-32 |


## Credits

The Clojure code herein is a translation or free paraphrase of code from:

Ashley N. Feniello
  https://github.com/AshleyF/HP35
  Source for javascript emulator, including tests.


Jacques Laporte
  http://www.jacques-laporte.org/sim35.java
  Java code for applet
  Rom from  casmsim-0.14/hp35.lst  http://www.brouhaha.com/~eric/software/casmsim/


 
The Swing GUI code is inspired by:

   https://github.com/stuarthalloway/programming-clojure/blob/master/src/examples/atom_snake.clj


Copyright 2011 Stuart Halloway and Aaron Bedra. All rights reserved.

The  pragprog.com  book site contained the following notice:

    ```
  "  Source Code for Programming Clojure (2nd edition)
  Copyrights apply to this source code. You may use the source code in
  your own projects, however the source code may not be used to create
  training material, courses, books, articles, and the like. We make
  no guarantees that this source code is fit for any purpose."

    ```


## Usage

Normal GUI operation  (from dir containing  src/... )

    ```
    $ clojure -M -m  calc-35.core  # start GUI; click on "buttons"
    ```
Text input/output from CLI (can use clojure or babashka)

    ```
    $ clojure -M -m  calc-35.cli                 # interactive use case
       # Enter cmd-char-abbrevs, as in  core.clj  keyinfo ("column" 4).
       # (control-d) when done.

    $ clojure -M -m  calc-35.script  file1.txt   # "batch" use case
       # a series of cmd-abbrevs. and literal values
    ```

## Examples

Interactive text input/output; after the ` 0.`,  line pairs: input, result; then ending with a control-d for EOF.

   ```
    $ bb -cp src -m  calc-35.cli
 0.            
355 113/
 3.14159292    
p
 3.141592654   
-
 2.66       -07
( enter a control-d)
   ```

Running a script from a text file(s).

   ```
    $ clojure -M -m  calc-35.script p1c.txt p2c.txt p3c.txt
 0.            
; Collection solid angle from pt source  HP-J 1972-06 p7  Expect:  .1772825509
2.5 10.3/ *1+fq~ 1+2*p*
 .1772825509   

; Great Circle dist San Francisco..Miami  HP-J 1972-06 p7  Expect: 2254.093016
52.4c64.3c*52.4s64.3s*42.3c*+ac60*
 2254.093016   

; pH of a buffer solution.   HP-J 1972-06 p7  Expect: -7.47877778
7.21 10^.03*1+2.16 10^.0087*+>.03 11.7 10^/.0087 7.21 10^/+</qg
-7.47877778    

   ```

### Bugs

None known.

## License

Other than credited above:
Copyright © 2019-2022   L. E. Vandergriff

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

   ```
"Freely you have received, freely give."  Mt. 10:8
   ```
