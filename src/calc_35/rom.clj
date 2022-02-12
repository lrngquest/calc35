(ns calc-35.rom )

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
            ]) ;; 768 10-bit entries as base10 (unsigned) int


;; Use  AF abbrev char to lookup rom addr for start-of-key-instr-seq
;;   HP35-specific; seems to apply to both V2 and V4 roms
(def afmap
  {\space 62, \! 0, \a 44, \c 42, \e 2, \f 14, \g 4, \* 30, \+ 22, \, 56,
   \l 3, \- 54, \. 35, \/ 38, \0 36, \p 34, \1 28, \q 46, \2 27, \r 11,
   \3 26, \s 43, \4 20, \t 40, \5 19, \6 18, \7 52, \w 12, \8 51, \x 58,
   \9 50, \< 8, \> 10, \^ 6, \~ 59}  )
