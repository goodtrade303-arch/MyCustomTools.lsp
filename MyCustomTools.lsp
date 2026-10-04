;;; =========================================================================
;;; AH CUSTOM TOOLS - MASTER FILE
;;; =========================================================================

;; 1. Global Version (قم بزيادة هذا الرقم كلما أضفت أوامر جديدة)
(setq *AH-VER* "1.3")

;; -------------------------------------------------------------------------
;; 2. Your Custom Commands & LISP Tools
;; -------------------------------------------------------------------------

(defun c:ar ()

(setq AnItem (getvar "OSMODE"))
(AList "OLDSNAP" AnItem)
(setq AnItem (getvar "HIGHLIGHT"))
(AList "OLDHIGH" AnItem)
(setq AnItem (getvar "CMDECHO"))
(AList "OLDECHO" AnItem)

(setq eset (ssget))
(setq cntr 0)

(setq tap (getpoint "\nInsertion point: "))

(setq pt1 (mapcar '+ tap (list 0.5 -0.45))) ; No
(setq pt2 (mapcar '+ pt1 (list 1 0)))       ; Length
(setq pt3 (mapcar '+ pt2 (list 1 0)))       ; Width
(setq pt4 (mapcar '+ pt3 (list 1 0)))       ; Area
(setq pt5 (mapcar '+ pt4 (list 1 0)))       ; Height
(setq pt6 (mapcar '+ pt5 (list 1 0)))       ; Volume

(setq h (getreal "\nHeight: "))

(while (< cntr (sslength eset))

  (setq en (ssname eset cntr))
  (setq enlist (entget en))
  (setq myVertexList (list))

  (foreach a enlist
    (if (= 10 (car a))
      (setq myVertexList
        (append myVertexList
          (list (cdr a))
        )
      )
    )
  )

  (setq listana
    (vl-sort myVertexList
      '(lambda (j k) (< (car j) (car k)))
    )
  )

  (setq p1 (car listana))

  ;; ???? ????? ??????
  (setq xmin (apply 'min (mapcar 'car myVertexList)))
  (setq xmax (apply 'max (mapcar 'car myVertexList)))

  (setq ymin (apply 'min (mapcar 'cadr myVertexList)))
  (setq ymax (apply 'max (mapcar 'cadr myVertexList)))

  (setq len (- xmax xmin))
  (setq wid (- ymax ymin))

  (if (< len wid)
    (progn
      (setq tmp len)
      (setq len wid)
      (setq wid tmp)
    )
  )

  (setq AnItem (getvar "OSMODE"))
  (AList "OLDSNAP" AnItem)
  (setq AnItem (getvar "HIGHLIGHT"))
  (AList "OLDHIGH" AnItem)
  (setq AnItem (getvar "CMDECHO"))
  (AList "OLDECHO" AnItem)

  (setvar "Osmode" 0)
  (setvar "Highlight" 0)
  (setvar "Cmdecho" 0)

  (setq ptx1 (mapcar '+ p1 (list 0.20 0.20)))
  (setq ptx2 (mapcar '+ p1 (list 0.20 0.50)))

  (command "area" "Object" en)

  (setq are (getvar "area"))
  (setq ar (strcat (rtos are 2 2) " m2"))

  (setq vol
    (strcat (rtos (* are h) 2 2) " m3")
  )

  (command "layer" "m" "ali_text" "")

  ;; ???? ?????

  (command "text" "j" "mc" pt1 "0.2" "0"
           (strcat "O" (rtos (+ 1 cntr) 2 0)) "")

  (command "text" "j" "mc" pt2 "0.2" "0"
           (rtos len 2 2) "")

  (command "text" "j" "mc" pt3 "0.2" "0"
           (rtos wid 2 2) "")

  (command "text" "j" "mc" pt4 "0.2" "0"
           ar "")

  (command "text" "j" "mc" pt5 "0.2" "0"
           (rtos h 2 2) "")

  (command "text" "j" "mc" pt6 "0.2" "0"
           vol "")

  ;; ??????? ????? ??????

  (command "text" ptx2 "0.25" "0"
           (strcat "O" (rtos (+ 1 cntr) 2 0)) "")

  (command "text" ptx1 "0.25" "0"
           (strcat
             "L=" (rtos len 2 2)
             " W=" (rtos wid 2 2)
             " Area=" ar)
           "")

  (setq cntr (+ 1 cntr))

  (setq pt1 (mapcar '+ pt1 (list 0 -0.30)))
  (setq pt2 (mapcar '+ pt2 (list 0 -0.30)))
  (setq pt3 (mapcar '+ pt3 (list 0 -0.30)))
  (setq pt4 (mapcar '+ pt4 (list 0 -0.30)))
  (setq pt5 (mapcar '+ pt5 (list 0 -0.30)))
  (setq pt6 (mapcar '+ pt6 (list 0 -0.30)))

)

(setvar "OSMODE" (RList "OLDSNAP"))
(setvar "HIGHLIGHT" (RList "OLDHIGH"))
(setvar "CMDECHO" (RList "OLDECHO"))

(princ)
)

(defun AList (Name Val)

  (setq item (list (cons Name Val)))
  (setq MainList (append item Mainlist))

)

(defun RList (TheName)

  (cdr (assoc TheName MainList))

)

(defun dtr (x)

  (* pi (/ x 180.0))

)

(princ)










(defun c:zdMALL ( / ss tol dimOff tblPt i j e1 e2 d1 d2
                      p11 p12 p21 p22 ang1 ang2
                      mid mid2 d used found
                      dx dy len ux uy nx ny pt
                      lengths idx startPt rowPt)

  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))

  (setq tol (getreal "\nEnter merge tolerance: "))
  (if (not tol) (setq tol 0.5))

  (setq dimOff (getreal "\nEnter dimension offset: "))
  (if (not dimOff) (setq dimOff 1.0))

  ;; ===== table insertion point =====
  (setq tblPt (getpoint "\nPick table insertion point: "))

  (setq used '())
  (setq lengths '())
  (setq i 0)

  (while (< i (sslength ss))

    (setq e1 (ssname ss i))

    (if (not (member e1 used))

      (progn

        (setq d1 (entget e1))

        ;; ===== extract =====
        (cond
          ((= (cdr (assoc 0 d1)) "LINE")
            (setq p11 (cdr (assoc 10 d1)))
            (setq p12 (cdr (assoc 11 d1)))
          )
          ((= (cdr (assoc 0 d1)) "LWPOLYLINE")
            (setq pts1
              (mapcar 'cdr
                (vl-remove-if-not '(lambda (x) (= (car x) 10)) d1)))
            (setq p11 (car pts1))
            (setq p12 (last pts1))
          )
        )

        (setq ang1 (angle p11 p12))
        (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p11 p12))

        (setq found nil)
        (setq j (+ i 1))

        ;; ===== compare =====
        (while (< j (sslength ss))

          (setq e2 (ssname ss j))

          (if (not (member e2 used))

            (progn

              (setq d2 (entget e2))

              (cond
                ((= (cdr (assoc 0 d2)) "LINE")
                  (setq p21 (cdr (assoc 10 d2)))
                  (setq p22 (cdr (assoc 11 d2)))
                )
                ((= (cdr (assoc 0 d2)) "LWPOLYLINE")
                  (setq pts2
                    (mapcar 'cdr
                      (vl-remove-if-not '(lambda (x) (= (car x) 10)) d2)))
                  (setq p21 (car pts2))
                  (setq p22 (last pts2))
                )
              )

              (setq ang2 (angle p21 p22))

              (if (< (abs (- ang1 ang2)) 0.05)

                (progn

                  (setq d (distance p11 p21))

                  (if (<= d tol)

                    (progn

                      ;; ===== midpoint =====
                      (setq mid2
                        (mapcar '(lambda (a b) (/ (+ a b) 2.0))
                                mid
                                (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p21 p22)))

                      ;; ===== direction vector =====
                      (setq dx (- (car p12) (car p11)))
                      (setq dy (- (cadr p12) (cadr p11)))
                      (setq len (distance p11 p12))

                      (setq ux (/ dx len))
                      (setq uy (/ dy len))

                      (setq nx (- uy))
                      (setq ny ux)

                      ;; ===== dimension point =====
                      (setq pt
                        (list
                          (+ (car mid2) (* nx dimOff))
                          (+ (cadr mid2) (* ny dimOff))
                          (caddr mid2)
                        )
                      )

                      (command "_.DIMALIGNED" p11 p12 pt)

                      ;; ===== STORE LENGTH FOR TABLE =====
                      (setq lengths (cons len lengths))

                      (setq used (cons e1 used))
                      (setq used (cons e2 used))

                      (setq found T)
                    )
                  )
                )
              )
            )
          )

          (setq j (+ j 1))
        )

        ;; ===== single =====
        (if (not found)
          (progn

            (setq dx (- (car p12) (car p11)))
            (setq dy (- (cadr p12) (cadr p11)))
            (setq len (distance p11 p12))

            (setq ux (/ dx len))
            (setq uy (/ dy len))

            (setq nx (- uy))
            (setq ny ux)

            (setq pt
              (list
                (+ (car mid) (* nx dimOff))
                (+ (cadr mid) (* ny dimOff))
                (caddr mid)
              )
            )

            (command "_.DIMALIGNED" p11 p12 pt)

            (setq lengths (cons len lengths))
          )
        )
      )
    )

    (setq i (+ i 1))
  )

  ;; ================= TABLE OUTPUT =================

  (setq lengths (reverse lengths))
  (setq idx 0)

  (foreach l lengths
    (setq rowPt
      (list
        (car tblPt)
        (- (cadr tblPt) (* idx 0.5))
        (caddr tblPt)
      )
    )

    (command "_.TEXT" rowPt 0.25 0
             (strcat "L" (itoa (+ idx 1)) " = " (rtos l 2 2)))

    (setq idx (+ idx 1))
  )

  (princ)
)











(defun c:v2XLS ( / file cols ss i e d txt colData done)

  (vl-load-com)

  (setq cols '())
  (setq done nil)

  ;; ===============================
  ;; SELECT COLUMNS
  ;; ===============================
  (while (not done)

    (prompt "\nSelect column TEXTs (Press Enter to finish): ")

    (setq ss (ssget '((0 . "TEXT,MTEXT"))))

    (if ss
      (progn

        (setq colData '())
        (setq i 0)

        ;; collect column
        (while (< i (sslength ss))

          (setq e (ssname ss i))
          (setq d (entget e))

          (setq txt
            (cond
              ((= (cdr (assoc 0 d)) "TEXT") (cdr (assoc 1 d)))
              ((= (cdr (assoc 0 d)) "MTEXT")
                (vl-string-subst " " "\\P" (cdr (assoc 1 d))))
            )
          )

          (setq colData (append colData (list txt)))

          (setq i (+ i 1))
        )

        (setq cols (append cols (list colData)))
      )
      (setq done T)
    )
  )

  ;; ===============================
  ;; EXPORT FILE
  ;; ===============================
  (setq file (getfiled "Save CSV File" "export.csv" "csv" 1))

  (if file
    (progn

      (setq f (open file "w"))

      (setq i 0)

      ;; write rows
      (while (< i (apply 'max (mapcar 'length cols)))

        (setq line "")
        (setq j 0)

        (foreach c cols

          (setq line
            (strcat line
              "\""
              (if (nth i c) (nth i c) "")
              "\""
              ","
            )
          )
        )

        (write-line line f)

        (setq i (+ i 1))
      )

      (close f)

      (alert "Export Done ? Open file in Excel")
    )
  )

  (princ)
)














(vl-load-com)

(defun c:A1 (/ oldSnap oldHigh oldEcho eset tap h items i en ed
               objType closed pts xmin xmax ymin ymax centerPt
               labelText area len wid vol
               pt1 pt2 pt3 pt4 pt5 pt6
               topLeft baseTopLeft namePt dimPt dimText
               txtH gap temp rowGap)

  ;; =========================
  ;; ??? ??????? AutoCAD
  ;; =========================
  (setq oldSnap  (getvar "OSMODE"))
  (setq oldHigh  (getvar "HIGHLIGHT"))
  (setq oldEcho  (getvar "CMDECHO"))

  ;; =========================
  ;; ?????? ???????
  ;; =========================
  (setq eset (ssget))

  (if eset
    (progn

      ;; =========================
      ;; ???? ????? ??????
      ;; =========================
      (setq tap (getpoint "\nInsertion point: "))

      (if tap
        (progn

          ;; =========================
          ;; ????????
          ;; =========================
          (setq h (getreal "\nHeight: "))

          (if (null h)
            (setq h 0.0)
          )

          ;; =========================
          ;; ??????? ??? ???? ??????
          ;; =========================
          (setq rowGap 0.45)

          ;; =========================
          ;; ???? ????? ??????
          ;; =========================
          (setq pt1 (mapcar '+ tap (list 0.5 0.0)))
          (setq pt2 (mapcar '+ tap (list 2.5 0.0)))
          (setq pt3 (mapcar '+ tap (list 4.0 0.0)))
          (setq pt4 (mapcar '+ tap (list 5.5 0.0)))
          (setq pt5 (mapcar '+ tap (list 7.5 0.0)))
          (setq pt6 (mapcar '+ tap (list 9.5 0.0)))

          ;; =========================
          ;; ????? ???????
          ;; =========================
          (setq items '())
          (setq i 0)

          ;; =========================
          ;; ??? ??????? ????????
          ;; =========================
          (while (< i (sslength eset))

            (setq en (ssname eset i))
            (setq ed (entget en))
            (setq objType (cdr (assoc 0 ed)))

            ;; ??????? ??? ?? LWPOLYLINE
            (if (= objType "LWPOLYLINE")
              (progn

                ;; =========================
                ;; ?? ??????
                ;; =========================
                (setq closed
                  (= 1
                    (logand
                      1
                      (cdr (assoc 70 ed))
                    )
                  )
                )

                (if closed
                  (progn

                    ;; =========================
                    ;; ??????? ??? vertices
                    ;; =========================
                    (setq pts '())

                    (foreach x ed
                      (if (= 10 (car x))
                        (setq pts
                          (cons (cdr x) pts)
                        )
                      )
                    )

                    (setq pts (reverse pts))

                    (if (> (length pts) 2)
                      (progn

                        ;; =========================
                        ;; ???? ???????
                        ;; =========================
                        (setq xmin
                          (apply 'min
                            (mapcar 'car pts)
                          )
                        )

                        (setq xmax
                          (apply 'max
                            (mapcar 'car pts)
                          )
                        )

                        (setq ymin
                          (apply 'min
                            (mapcar 'cadr pts)
                          )
                        )

                        (setq ymax
                          (apply 'max
                            (mapcar 'cadr pts)
                          )
                        )

                        ;; =========================
                        ;; ???? ???????
                        ;; =========================
                        (setq centerPt
                          (list
                            (/ (+ xmin xmax) 2.0)
                            (/ (+ ymin ymax) 2.0)
                          )
                        )

                        ;; =========================
                        ;; ???????
                        ;; =========================
                        (setq len (- xmax xmin))
                        (setq wid (- ymax ymin))

                        ;; L ?? ??????
                        (if (< len wid)
                          (progn
                            (setq temp len)
                            (setq len wid)
                            (setq wid temp)
                          )
                        )

                        ;; =========================
                        ;; ???????
                        ;; =========================
                        (setq area
                          (abs
                            (vlax-curve-getArea en)
                          )
                        )

                        ;; =========================
                        ;; ??? ???????
                        ;; =========================
                        (setq labelText
                          (GetTextInsideRectangle
                            xmin xmax ymin ymax
                          )
                        )

                        (if
                          (or
                            (null labelText)
                            (= labelText "")
                          )
                          (setq labelText "NoID")
                        )

                        ;; =================================================
                        ;; ????? ?????? ?????? ???????
                        ;; =================================================
                        (setq topLeft
                          (list
                            xmin
                            ymax
                          )
                        )

                        ;; =========================
                        ;; ????? ????????
                        ;; =========================
                        (setq items
                          (cons
                            (list
                              en
                              centerPt
                              labelText
                              len
                              wid
                              area
                              topLeft
                            )
                            items
                          )
                        )
                      )
                    )
                  )
                )
              )
            )

            (setq i (1+ i))
          )

          ;; =========================
          ;; ????? ??? ??? ???????
          ;; =========================
          (setq items
            (vl-sort
              items
              '(lambda (a b)
                (<
                  (FixNum (nth 2 a))
                  (FixNum (nth 2 b))
                )
              )
            )
          )

          ;; =========================
          ;; ??????? ?????
          ;; =========================
          (setvar "OSMODE" 0)
          (setvar "HIGHLIGHT" 0)
          (setvar "CMDECHO" 0)

          ;; =========================
          ;; ????? Layer
          ;; =========================
          (if
            (not (tblsearch "LAYER" "ali_text"))
            (entmakex
              (list
                '(0 . "LAYER")
                '(100 . "AcDbSymbolTableRecord")
                '(100 . "AcDbLayerTableRecord")
                '(70 . 0)
                (cons 2 "ali_text")
                (cons 62 7)
                (cons 6 "Continuous")
              )
            )
          )

          ;; =========================
          ;; Header ??????
          ;; =========================
          (MakeText pt1 0.20 "NAME")
          (MakeText pt2 0.20 "L")
          (MakeText pt3 0.20 "W")
          (MakeText pt4 0.20 "AREA")
          (MakeText pt5 0.20 "H")
          (MakeText pt6 0.20 "VOLUME")

          ;; =========================
          ;; ?????? ???? ??
          ;; =========================
          (setq pt1
            (mapcar '+ pt1 (list 0 (- rowGap)))
          )

          (setq pt2
            (mapcar '+ pt2 (list 0 (- rowGap)))
          )

          (setq pt3
            (mapcar '+ pt3 (list 0 (- rowGap)))
          )

          (setq pt4
            (mapcar '+ pt4 (list 0 (- rowGap)))
          )

          (setq pt5
            (mapcar '+ pt5 (list 0 (- rowGap)))
          )

          (setq pt6
            (mapcar '+ pt6 (list 0 (- rowGap)))
          )

          ;; =========================
          ;; ????? ????????
          ;; =========================
          (foreach itm items

            (setq en        (nth 0 itm))
            (setq centerPt  (nth 1 itm))
            (setq labelText (nth 2 itm))
            (setq len       (nth 3 itm))
            (setq wid       (nth 4 itm))
            (setq area      (nth 5 itm))

            ;; ????? ?????? ?????? ????? ???? ???????
            (setq baseTopLeft (nth 6 itm))

            ;; =========================
            ;; ?????
            ;; =========================
            (setq vol (* area h))

            ;; =========================
            ;; ?????? ??????
            ;; =========================
            (MakeText
              pt1
              0.20
              labelText
            )

            (MakeText
              pt2
              0.20
              (rtos len 2 2)
            )

            (MakeText
              pt3
              0.20
              (rtos wid 2 2)
            )

            (MakeText
              pt4
              0.20
              (strcat
                (rtos area 2 2)
                " m2"
              )
            )

            (MakeText
              pt5
              0.20
              (rtos h 2 2)
            )

            (MakeText
              pt6
              0.20
              (strcat
                (rtos vol 2 2)
                " m3"
              )
            )

            ;; =========================
            ;; ???????? ????? ??????
            ;; =========================
            (setq pt1
              (mapcar '+ pt1 (list 0 (- rowGap)))
            )

            (setq pt2
              (mapcar '+ pt2 (list 0 (- rowGap)))
            )

            (setq pt3
              (mapcar '+ pt3 (list 0 (- rowGap)))
            )

            (setq pt4
              (mapcar '+ pt4 (list 0 (- rowGap)))
            )

            (setq pt5
              (mapcar '+ pt5 (list 0 (- rowGap)))
            )

            (setq pt6
              (mapcar '+ pt6 (list 0 (- rowGap)))
            )

            ;; =================================================
            ;; ???????? ??? ???????
            ;; =================================================

            ;; =========================
            ;; ??? ???? ??? ??? ???????
            ;; =========================
            (setq txtH
              (max
                0.12
                (min
                  0.22
                  (/ len 12.0)
                )
              )
            )

            ;; =========================
            ;; ????? ????? ??? ??????? ?????
            ;; =========================
            (setq gap
              (max
                0.25
                (* txtH 1.5)
              )
            )

            ;; =================================================
            ;; ??? ???????
            ;; ??? ??? ???????
            ;; =================================================
            (setq namePt
              (list
                (car baseTopLeft)
                (+ (cadr baseTopLeft)
                   gap
                   (* txtH 1.5)
                )
              )
            )

            (MakeTextLeft
              namePt
              txtH
              labelText
            )

            ;; =================================================
            ;; ??? ???????
            ;; =================================================
            (setq dimText
              (strcat
                "L="
                (rtos len 2 2)
                "  W="
                (rtos wid 2 2)
                "  Area="
                (rtos area 2 2)
                " m2"
              )
            )

            ;; =========================
            ;; ???? ????? ??? ???????
            ;; =========================
            (setq dimPt
              (list
                (car baseTopLeft)
                (+ (cadr baseTopLeft)
                   gap
                )
              )
            )

            (MakeTextLeft
              dimPt
              txtH
              dimText
            )
          )

          ;; =========================
          ;; ????? ???????
          ;; =========================
          (princ
            (strcat
              "\n?? ????????. ??? ???????: "
              (itoa (length items))
            )
          )
        )
      )
    )
    (princ "\n?? ??? ?????? ?? ?????.")
  )

  ;; =========================
  ;; ??????? ?????????
  ;; =========================
  (setvar "OSMODE" oldSnap)
  (setvar "HIGHLIGHT" oldHigh)
  (setvar "CMDECHO" oldEcho)

  (princ)
)


;; =========================================================
;; ????? ?? TEXT / MTEXT ???? ???????
;; =========================================================

(defun GetTextInsideRectangle
  (xmin xmax ymin ymax / ss i en ed pt txt best)

  (setq best nil)

  (setq ss
    (ssget
      "_X"
      '(
        (-4 . "<OR")
        (0 . "TEXT")
        (0 . "MTEXT")
        (-4 . "OR>")
      )
    )
  )

  (if ss
    (progn

      (setq i 0)

      (while (< i (sslength ss))

        (setq en (ssname ss i))
        (setq ed (entget en))
        (setq pt (cdr (assoc 10 ed)))

        (if
          (and
            pt
            (>= (car pt) xmin)
            (<= (car pt) xmax)
            (>= (cadr pt) ymin)
            (<= (cadr pt) ymax)
          )
          (progn

            (setq txt (cdr (assoc 1 ed)))

            (if
              (and
                txt
                (/= txt "")
              )
              (setq best txt)
            )
          )
        )

        (setq i (1+ i))
      )
    )
  )

  best
)


;; =========================================================
;; ??????? ????? ?? ??? ???????
;; =========================================================

(defun FixNum
  (txt / res i c)

  (setq res "")
  (setq i 1)

  (if txt
    (while
      (<= i (strlen txt))

      (setq c
        (substr txt i 1)
      )

      (if
        (and
          (>= c "0")
          (<= c "9")
        )
        (setq res
          (strcat res c)
        )
      )

      (setq i (1+ i))
    )
  )

  (if
    (= res "")
    999999
    (atoi res)
  )
)


;; =========================================================
;; ????? TEXT ??????
;; Center / Middle
;; =========================================================

(defun MakeText
  (pt height txt /)

  (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      (cons 8 "ali_text")

      '(100 . "AcDbText")
      (cons 10 pt)
      (cons 40 height)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
      (cons 50 0.0)

      ;; Center / Middle
      '(72 . 1)
      '(73 . 2)

      ;; ???? ????????
      (cons 11 pt)
    )
  )
)


;; =========================================================
;; ????? TEXT ???? ?? ??????
;; Left / Baseline
;; =========================================================

(defun MakeTextLeft
  (pt height txt /)

  (entmakex
    (list
      '(0 . "TEXT")
      '(100 . "AcDbEntity")
      (cons 8 "ali_text")

      '(100 . "AcDbText")
      (cons 10 pt)
      (cons 40 height)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
      (cons 50 0.0)

      ;; Left / Baseline
      '(72 . 0)
      '(73 . 0)
    )
  )
)


(princ "\nA1 Loaded. Type A1 to run.")
(princ)
















(load "cbv.lsp")











;;; =========================================================================
;;; LISP Command: CLL
;;; Description: Advanced Column Placement with English DCL GUI, Dynamic Dragging,
;;;              Clean Refresh Preview (Zero Screen Artifacts / Ghosting),
;;;              Protected Polyline (Prevents YQArch Trimming),
;;;              Full Native OSNAP with Orientation Options, Empty Insertion Buttons,
;;;              & Fixed Base Point Alignment on Spacebar Toggle.
;;; Units: Meters
;;; =========================================================================

(vl-load-com)

;; Global variables for persistence
(if (null *cll-shape*)  (setq *cll-shape* "rect"))
(if (null *cll-w*)      (setq *cll-w* 0.25))
(if (null *cll-l*)      (setq *cll-l* 0.60))
(if (null *cll-dia*)    (setq *cll-dia* 0.50))
(if (null *cll-align*)  (setq *cll-align* "3")) ; Default Bottom-Left (3)
(if (null *cll-orient*) (setq *cll-orient* "horiz")) ; Default Horizontal

;; -------------------------------------------------------------------------
;; 1. Automatically Create DCL File
;; -------------------------------------------------------------------------
(defun CLL-make-dcl ( / filepath file)
  (setq filepath (strcat (getvar "ROAMABLEROOTPREFIX") "cll_dialog.dcl"))
  (setq file (open filepath "w"))
  (write-line "cll_dialog : dialog {" file)
  (write-line "  label = \"Column Settings\";" file)
  (write-line "  : boxed_column {" file)
  (write-line "    label = \"Column Shape\";" file)
  (write-line "    : radio_row {" file)
  (write-line "      key = \"shape\";" file)
  (write-line "      : radio_button { label = \"Rectangle\"; key = \"rect\"; }" file)
  (write-line "      : radio_button { label = \"Circle\"; key = \"circ\"; }" file)
  (write-line "    }" file)
  (write-line "  }" file)
  (write-line "  : boxed_column {" file)
  (write-line "    label = \"Orientation\";" file)
  (write-line "    : radio_row {" file)
  (write-line "      key = \"orient\";" file)
  (write-line "      : radio_button { label = \"Horizontal\"; key = \"horiz\"; }" file)
  (write-line "      : radio_button { label = \"Vertical\"; key = \"vert\"; }" file)
  (write-line "    }" file)
  (write-line "  }" file)
  (write-line "  : boxed_column {" file)
  (write-line "    label = \"Dimensions (Meters)\";" file)
  (write-line "    : edit_box { label = \"Width (W):\"; key = \"txt_w\"; edit_width = 8; }" file)
  (write-line "    : edit_box { label = \"Length (L):\"; key = \"txt_l\"; edit_width = 8; }" file)
  (write-line "    : edit_box { label = \"Diameter (D):\"; key = \"txt_dia\"; edit_width = 8; }" file)
  (write-line "  }" file)
  (write-line "  : boxed_column {" file)
  (write-line "    label = \"Insertion Point\";" file)
  ;; Row 1
  (write-line "    : row {" file)
  (write-line "      alignment = centered;" file)
  (write-line "      : button { key = \"btn_2\";  width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "      : button { key = \"btn_tc\"; width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "      : button { key = \"btn_1\";  width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "    }" file)
  ;; Row 2
  (write-line "    : row {" file)
  (write-line "      alignment = centered;" file)
  (write-line "      : button { key = \"btn_cl\"; width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "      : button { key = \"btn_5\";  width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "      : button { key = \"btn_cr\"; width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "    }" file)
  ;; Row 3
  (write-line "    : row {" file)
  (write-line "      alignment = centered;" file)
  (write-line "      : button { key = \"btn_3\";  width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "      : button { key = \"btn_bc\"; width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "      : button { key = \"btn_4\";  width = 6; fixed_width = true; height = 2; fixed_height = true; }" file)
  (write-line "    }" file)
  (write-line "  }" file)
  (write-line "  spacer;" file)
  (write-line "  : row {" file)
  (write-line "    : button { label = \"Start Drawing\"; key = \"accept\"; is_default = true; width = 14; }" file)
  (write-line "    : cancel_button { label = \"Cancel\"; width = 10; }" file)
  (write-line "  }" file)
  (write-line "}" file)
  (close file)
  filepath
)

;; Refresh button labels (Icon style)
(defun CLL-update-button-labels ()
  (set_tile "btn_2"  (if (= *cll-align* "2")  "[" ""))
  (set_tile "btn_tc" (if (= *cll-align* "tc") "[" ""))
  (set_tile "btn_1"  (if (= *cll-align* "1")  "[" ""))
  (set_tile "btn_cl" (if (= *cll-align* "cl") "[" ""))
  (set_tile "btn_5"  (if (= *cll-align* "5")  "[" ""))
  (set_tile "btn_cr" (if (= *cll-align* "cr") "[" ""))
  (set_tile "btn_3"  (if (= *cll-align* "3")  "[" ""))
  (set_tile "btn_bc" (if (= *cll-align* "bc") "[" ""))
  (set_tile "btn_4"  (if (= *cll-align* "4")  "[" ""))
)

;; -------------------------------------------------------------------------
;; 2. DCL Dialog Control
;; -------------------------------------------------------------------------
(defun CLL-show-dialog ( / dcl-file dcl-id status)
  (setq dcl-file (CLL-make-dcl))
  (setq dcl-id (load_dialog dcl-file))
  (if (not (new_dialog "cll_dialog" dcl-id)) (exit))

  (set_tile "shape" *cll-shape*)
  (set_tile "orient" *cll-orient*)
  (set_tile "txt_w" (rtos *cll-w* 2 2))
  (set_tile "txt_l" (rtos *cll-l* 2 2))
  (set_tile "txt_dia" (rtos *cll-dia* 2 2))

  (CLL-update-button-labels)

  (defun update-mode ()
    (if (= (get_tile "shape") "rect")
      (progn
        (mode_tile "txt_w" 0)
        (mode_tile "txt_l" 0)
        (mode_tile "txt_dia" 1)
        (mode_tile "orient" 0))
      (progn
        (mode_tile "txt_w" 1)
        (mode_tile "txt_l" 1)
        (mode_tile "txt_dia" 0)
        (mode_tile "orient" 1))
    )
  )
  (update-mode)

  (action_tile "shape" "(update-mode)")

  (action_tile "btn_1"  "(setq *cll-align* \"1\") (CLL-update-button-labels)")
  (action_tile "btn_2"  "(setq *cll-align* \"2\") (CLL-update-button-labels)")
  (action_tile "btn_tc" "(setq *cll-align* \"tc\")(CLL-update-button-labels)")
  (action_tile "btn_cl" "(setq *cll-align* \"cl\")(CLL-update-button-labels)")
  (action_tile "btn_5"  "(setq *cll-align* \"5\") (CLL-update-button-labels)")
  (action_tile "btn_cr" "(setq *cll-align* \"cr\")(CLL-update-button-labels)")
  (action_tile "btn_3"  "(setq *cll-align* \"3\") (CLL-update-button-labels)")
  (action_tile "btn_bc" "(setq *cll-align* \"bc\")(CLL-update-button-labels)")
  (action_tile "btn_4"  "(setq *cll-align* \"4\") (CLL-update-button-labels)")

  (action_tile "accept"
    "(setq *cll-shape*  (get_tile \"shape\")
           *cll-orient* (get_tile \"orient\")
           *cll-w*      (atof (get_tile \"txt_w\"))
           *cll-l*      (atof (get_tile \"txt_l\"))
           *cll-dia*    (atof (get_tile \"txt_dia\")))
     (done_dialog 1)")

  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl-id)
  (= status 1)
)

;; -------------------------------------------------------------------------
;; 3. Calculate Offsets
;; -------------------------------------------------------------------------
(defun CLL-get-offsets (w l align / ox oy)
  (cond
    ((= align "1")  (setq ox (- w)        oy (- l)))
    ((= align "2")  (setq ox 0.0          oy (- l)))
    ((= align "tc") (setq ox (- (/ w 2.0)) oy (- l)))
    ((= align "cl") (setq ox 0.0          oy (- (/ l 2.0))))
    ((= align "5")  (setq ox (- (/ w 2.0)) oy (- (/ l 2.0))))
    ((= align "cr") (setq ox (- w)        oy (- (/ l 2.0))))
    ((= align "3")  (setq ox 0.0          oy 0.0))
    ((= align "bc") (setq ox (- (/ w 2.0)) oy 0.0))
    ((= align "4")  (setq ox (- w)        oy 0.0))
  )
  (list ox oy)
)

;; Helper to get active width and length based on runtime toggle status
(defun CLL-get-active-dims (is-toggled / w l)
  (if (or (and (= *cll-orient* "horiz") is-toggled)
          (and (= *cll-orient* "vert")  (not is-toggled)))
    (list (min *cll-w* *cll-l*) (max *cll-w* *cll-l*)) ; Vertical dimensions
    (list (max *cll-w* *cll-l*) (min *cll-w* *cll-l*)) ; Horizontal dimensions
  )
)

;; -------------------------------------------------------------------------
;; 4. OSNAP Markers Drawer
;; -------------------------------------------------------------------------
(defun CLL-draw-osnap-marker (pt type / sz x y p1 p2 p3 p4 i a)
  (if (and pt type)
    (progn
      (setq sz (/ (getvar "VIEWSIZE") 65.0)
            x (car pt)
            y (cadr pt))
      
      (cond
        ((= type "_mid")
         (setq p1 (list x (+ y sz) 0.0)
               p2 (list (- x sz) (- y sz) 0.0)
               p3 (list (+ x sz) (- y sz) 0.0))
         (grdraw p1 p2 3 0)
         (grdraw p2 p3 3 0)
         (grdraw p3 p1 3 0))

        ((or (= type "_cen") (= type "_node"))
         (setq i 0)
         (while (< i 8)
           (setq a (* i (/ pi 4.0))
                 p1 (list (+ x (* sz (cos a))) (+ y (* sz (sin a))) 0.0)
                 a (* (1+ i) (/ pi 4.0))
                 p2 (list (+ x (* sz (cos a))) (+ y (* sz (sin a))) 0.0))
           (grdraw p1 p2 3 0)
           (setq i (1+ i))))

        ((= type "_int")
         (grdraw (list (- x sz) (- y sz) 0.0) (list (+ x sz) (+ y sz) 0.0) 3 0)
         (grdraw (list (- x sz) (+ y sz) 0.0) (list (+ x sz) (- y sz) 0.0) 3 0))

        ((= type "_perp")
         (grdraw (list (- x sz) (- y sz) 0.0) (list (+ x sz) (- y sz) 0.0) 3 0)
         (grdraw (list (- x sz) (- y sz) 0.0) (list (- x sz) (+ y sz) 0.0) 3 0)
         (grdraw (list (- x sz) y 0.0) (list x y 0.0) 3 0)
         (grdraw (list x (- y sz) 0.0) (list x y 0.0) 3 0))

        ((= type "_nea")
         (grdraw (list (- x sz) (+ y sz) 0.0) (list (+ x sz) (+ y sz) 0.0) 3 0)
         (grdraw (list (+ x sz) (+ y sz) 0.0) (list (- x sz) (- y sz) 0.0) 3 0)
         (grdraw (list (- x sz) (- y sz) 0.0) (list (+ x sz) (- y sz) 0.0) 3 0)
         (grdraw (list (+ x sz) (- y sz) 0.0) (list (- x sz) (+ y sz) 0.0) 3 0))

        (T
         (setq p1 (list (- x sz) (- y sz) 0.0)
               p2 (list (+ x sz) (- y sz) 0.0)
               p3 (list (+ x sz) (+ y sz) 0.0)
               p4 (list (- x sz) (+ y sz) 0.0))
         (grdraw p1 p2 3 0)
         (grdraw p2 p3 3 0)
         (grdraw p3 p4 3 0)
         (grdraw p4 p1 3 0))
      )
    )
  )
)

;; Accurate OSNAP Finder
(defun CLL-get-snap (pt / osnap-pt best-mode best-dist test-pt dist modes)
  (setq osnap-pt (osnap pt "_end,_mid,_int,_cen,_perp,_node,_nea"))
  (if osnap-pt
    (progn
      (setq modes '("_end" "_mid" "_int" "_cen" "_perp" "_node" "_nea")
            best-dist 1e9
            best-mode "_end")
      (foreach m modes
        (setq test-pt (osnap pt m))
        (if (and test-pt (< (distance osnap-pt test-pt) 0.0001))
          (progn
            (setq dist (distance pt test-pt))
            (if (< dist best-dist)
              (setq best-dist dist
                    best-mode m)
            )
          )
        )
      )
      (list osnap-pt best-mode)
    )
    nil
  )
)

;; Helper to draw preview box cleanly with consistent base point alignment
(defun CLL-draw-preview (draw-pt is-toggled / dims cur-w cur-l offsets ox oy p1 p2 p3 p4)
  (if (= *cll-shape* "rect")
    (progn
      (setq dims  (CLL-get-active-dims is-toggled)
            cur-w (car dims)
            cur-l (cadr dims)
            offsets (CLL-get-offsets cur-w cur-l *cll-align*)
            ox (+ (car draw-pt) (car offsets))
            oy (+ (cadr draw-pt) (cadr offsets))
            p1 (list ox oy 0.0)
            p2 (list (+ ox cur-w) oy 0.0)
            p3 (list (+ ox cur-w) (+ oy cur-l) 0.0)
            p4 (list ox (+ oy cur-l) 0.0))
      (grdraw p1 p2 1 0)
      (grdraw p2 p3 1 0)
      (grdraw p3 p4 1 0)
      (grdraw p4 p1 1 0)
    )
    (grdraw draw-pt (list (+ (car draw-pt) (/ *cll-dia* 2.0)) (cadr draw-pt) 0.0) 1 0)
  )
)

;; -------------------------------------------------------------------------
;; 5. Entity Drawing Functions (Direct Database Insert - YQArch Protected)
;; -------------------------------------------------------------------------
(defun CLL-draw-rect (pt is-toggled align / dims cur-w cur-l offsets ox oy p1 p2 p3 p4)
  (setq dims  (CLL-get-active-dims is-toggled)
        cur-w (car dims)
        cur-l (cadr dims)
        offsets (CLL-get-offsets cur-w cur-l align)
        ox (+ (car pt) (car offsets))
        oy (+ (cadr pt) (cadr offsets))
        p1 (list ox oy)
        p2 (list (+ ox cur-w) oy)
        p3 (list (+ ox cur-w) (+ oy cur-l))
        p4 (list ox (+ oy cur-l)))

  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "S-COLUMN")
      '(100 . "AcDbPolyline")
      '(90 . 4)
      '(70 . 1)
      (cons 10 p1)
      (cons 10 p2)
      (cons 10 p3)
      (cons 10 p4)
    )
  )
)

(defun CLL-draw-circ (pt dia)
  (entmake
    (list
      '(0 . "CIRCLE")
      '(8 . "S-COLUMN")
      (cons 10 pt)
      (cons 40 (/ dia 2.0))
    )
  )
)

;; -------------------------------------------------------------------------
;; 6. Main Execution Loop
;; -------------------------------------------------------------------------
(defun c:CLL ( / col-layer run loop gr code pt is-toggled snap-info snap-pt snap-type draw-pt)
  (setq col-layer "S-COLUMN")
  
  (if (null (tblsearch "LAYER" col-layer))
    (command "._layer" "_make" col-layer "_color" "1" "" "")
  )

  (setq run T)
  (while run
    (if (CLL-show-dialog)
      (progn
        (princ "\n[Click] Place Column | [SPACEBAR] Toggle Horizontal/Vertical | [D] Settings | [ENTER/ESC] Exit")
        (setq loop T is-toggled nil)

        (while loop
          (setvar "CLAYER" col-layer)
          (setq gr (grread T 15 0)
                code (car gr)
                pt (cadr gr))

          (cond
            ;; Mouse Motion (Clean Refresh Preview Engine)
            ((= code 5)
             (redraw)

             (setq snap-info (CLL-get-snap pt))
             (if snap-info
               (progn
                 (setq snap-pt (car snap-info)
                       snap-type (cadr snap-info))
                 (CLL-draw-osnap-marker snap-pt snap-type)
                 (setq draw-pt snap-pt))
               (setq draw-pt pt)
             )

             (CLL-draw-preview draw-pt is-toggled)
            )

            ;; Left Mouse Click -> Insert Object
            ((= code 3)
             (redraw)
             (setq snap-info (CLL-get-snap pt))
             (if snap-info
               (setq draw-pt (car snap-info))
               (setq draw-pt pt)
             )

             (if (= *cll-shape* "rect")
               (CLL-draw-rect draw-pt is-toggled *cll-align*)
               (CLL-draw-circ draw-pt *cll-dia*)
             )
             (princ "\nColumn Inserted! Click to place more, Spacebar to toggle, or D for Settings.")
            )

            ;; Keypress Handler
            ((= code 2)
             (cond
               ;; SPACEBAR (32) -> Toggle Horizontal / Vertical with same Base Point Alignment
               ((= pt 32)
                (redraw)
                (setq is-toggled (not is-toggled))
                (princ "\nColumn Orientation Toggled."))

               ;; 'D' or 'd' Key (68 / 100) -> Re-open DCL Dialog
               ((or (= pt 68) (= pt 100))
                (redraw)
                (setq loop nil))

               ;; ENTER key -> Exit
               ((or (= pt 13) (= pt 10))
                (redraw)
                (setq loop nil run nil))
             )
            )

            ;; Right Click / Cancel -> Exit Command
            ((or (= code 11) (= code 25))
             (redraw)
             (setq loop nil run nil))
          )
        )
      )
      (setq run nil)
    )
  )
  (redraw)
  (princ "\nCLL Finished.")
  (princ)
)

(princ "\nCLL loaded successfully. Type CLL to start.")
(princ)



;;; ============================================================================
;;;  HCOL.LSP  -  SINGLE COMMAND: HCOL
;;; ----------------------------------------------------------------------------
;;;  Supports ANY closed column shape: rectangular, polygonal, or an
;;;  arbitrary mix of straight/curved edges drawn as ONE closed
;;;  LWPOLYLINE or old-style 2D POLYLINE (RECTANG, BOUNDARY, or a PLINE
;;;  properly closed) - AND also a native CIRCLE or full ELLIPSE entity
;;;  drawn directly with the CIRCLE/ELLIPSE command. A polyline that was
;;;  only snapped back to its start point by eye (without using PLINE's
;;;  Close option) is still recognised, as long as its start and end
;;;  points coincide. All geometry tests (intersection, distance,
;;;  inside/outside) work on the curve generically.
;;;
;;;  Workflow:
;;;    1) Prompt: select the OLD column entities (closed LWPOLYLINE /
;;;       POLYLINE of any shape, or a CIRCLE / ELLIPSE).
;;;    2) Prompt: select WALL entities (LINE or LWPOLYLINE).
;;;    3) For every valid (closed) old column:
;;;         - create an exact duplicate (same shape/size/location) via
;;;           vla-Copy - this duplicate is the new column boundary.
;;;         - fill the duplicate with a SOLID hatch.
;;;         - for every wall, find exact intersection points with the
;;;           duplicate boundary (IntersectWith, acExtendNone = 0), split
;;;           the wall into ordered sub-segments.
;;;         - each sub-segment's midpoint is classified against every
;;;           column as:
;;;             ON  - within 0.001 of the boundary curve (sitting
;;;                   directly on top of / coincident with an edge)
;;;             IN  - strictly inside the closed column shape (a
;;;                   genuine interior/through-crossing portion, even
;;;                   far from any edge - detected with a point-in-
;;;                   polygon test, NOT just a raw distance check,
;;;                   because a chord through a real-size column can
;;;                   have its midpoint tens of centimetres from the
;;;                   nearest edge)
;;;             OUT - genuinely external -> kept
;;;         - ON and IN sub-segments are discarded; only OUT
;;;           sub-segments are drawn as new LINE entities, on the
;;;           wall's original layer.
;;;         - a wall with ZERO crossing points that is fully
;;;           coincident with a column edge (IntersectWith cannot
;;;           report an overlap as point intersections) is separately
;;;           detected and discarded whole.
;;;         - the original (now replaced) wall entities are deleted.
;;;         - the original old column is deleted, leaving only the new
;;;           duplicate + its hatch.
;;;    4) Whole process wrapped in one Undo Group, and a message box
;;;       (alert) at the end reports exactly what was done, so nothing
;;;       is missed even if the command-line history isn't checked.
;;; ============================================================================

(vl-load-com)

(setq *HCOL-TOL* 0.001)          ; precision tolerance for the actual cutting
(setq *HCOL-CLOSE-TOL* 0.01)     ; looser tolerance (1 cm) just for deciding
                                  ; whether a manually-closed polyline counts
                                  ; as closed (hand-snapping is rarely sub-mm)

;; ----------------------------------------------------------------------------
;; Error handler - guarantees the undo group is always closed.
;; ----------------------------------------------------------------------------
(defun HCOL:ErrorHandler (msg)
  (if (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort"))
      (princ (strcat "\nHCOL error: " msg))
  )
  (if *HCOL-OLD-ERROR* (setq *error* *HCOL-OLD-ERROR*))
  ;; Never call (command) from the error handler.
  ;; End the undo mark through ActiveX instead.
  (if *HCOL-DOC*
      (vl-catch-all-apply 'vla-EndUndoMark (list *HCOL-DOC*))
  )
  (setq *HCOL-DOC* nil)
  (if *HCOL-OLD-CMDECHO* (setvar "cmdecho" *HCOL-OLD-CMDECHO*))
  (princ)
)

;; ----------------------------------------------------------------------------
;; Flatten a closed curve (straight edges, arcs/bulges, or a full circle -
;; ANY shape, as long as it is one single closed polyline) into an ordered
;; list of 2D points by sampling it at even distances along its length.
;; Used only for the ray-casting inside/outside test - the exact vla-object
;; (not this approximation) is still what gets hatched and used for the
;; precise IntersectWith / closest-point distance tests.
;;
;; The sample count is NOT fixed: it adapts to each column's own perimeter
;; (roughly one sample point every 5 cm), so a small rectangular column and
;; a large circular one are both approximated accurately, within sensible
;; lower/upper bounds so performance stays bounded either way.
;; ----------------------------------------------------------------------------
(defun HCOL:FlattenCurve (obj / totalLen samples step d pts pt)
  (setq pts '())
  (setq totalLen (vl-catch-all-apply 'vlax-curve-getDistAtParam
                                      (list obj (vlax-curve-getEndParam obj))))
  (if (and totalLen (not (vl-catch-all-error-p totalLen)) (> totalLen 0.0))
      (progn
        (setq samples (fix (/ totalLen 0.05)))   ; ~1 sample every 5 cm
        (if (< samples 64)  (setq samples 64))   ; lower bound: small columns
        (if (> samples 720) (setq samples 720))  ; upper bound: performance
        (setq step (/ totalLen (float samples)))
        (setq d 0.0)
        (repeat (1+ samples)
          (if (> d totalLen) (setq d totalLen))
          (setq pt (vl-catch-all-apply 'vlax-curve-getPointAtDist (list obj d)))
          (if (and pt (not (vl-catch-all-error-p pt)))
              (setq pts (cons (list (car pt) (cadr pt)) pts))
          )
          (setq d (+ d step))
        )
      )
  )
  (reverse pts)
)

;; ----------------------------------------------------------------------------
;; Standard ray-casting point-in-polygon test (2D, XY plane only).
;; poly = list of (x y) points, treated as closed (last connects to first).
;; ----------------------------------------------------------------------------
(defun HCOL:PtInPolyP (pt poly / n i j vi vj xi yi xj yj x y inside)
  (setq x (car pt) y (cadr pt))
  (setq n (length poly))
  (setq inside nil)
  (if (>= n 3)
      (progn
        (setq j (1- n))
        (setq i 0)
        (while (< i n)
          (setq vi (nth i poly))
          (setq vj (nth j poly))
          (setq xi (car vi) yi (cadr vi) xj (car vj) yj (cadr vj))
          (if (and (/= (> yi y) (> yj y))
                   (< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
              (setq inside (not inside))
          )
          (setq j i)
          (setq i (1+ i))
        )
      )
  )
  inside
)

;; ----------------------------------------------------------------------------
;; Extract every intersection point between two curve objects using
;; IntersectWith with acExtendNone (0). Returns a list of 3D points.
;;   (vlax-invoke polyObj 'IntersectWith wallObj 0)
;;
;; IMPORTANT: depending on the AutoCAD/Visual LISP build, vlax-invoke may
;; hand back the result either as a PLAIN AutoLISP list of reals (most
;; common) or as a raw SAFEARRAY object that still needs converting. This
;; version tries the plain-list form first and only falls back to
;; vlax-safearray->list (and then vlax-variant-value) if that fails.
;; ----------------------------------------------------------------------------
(defun HCOL:GetIntersectionPts (polyObj wallObj / res lst n i pts tmp)
  (setq pts '())
  (setq res (vl-catch-all-apply 'vlax-invoke (list polyObj 'IntersectWith wallObj 0)))
  (if (and res (not (vl-catch-all-error-p res)))
      (progn
        (setq lst nil)
        (cond
          ((listp res) (setq lst res))
          (T
           (setq tmp (vl-catch-all-apply 'vlax-safearray->list (list res)))
           (if (not (vl-catch-all-error-p tmp))
               (setq lst tmp)
               (progn
                 (setq tmp (vl-catch-all-apply 'vlax-variant-value (list res)))
                 (if (not (vl-catch-all-error-p tmp))
                     (progn
                       (setq tmp (vl-catch-all-apply 'vlax-safearray->list (list tmp)))
                       (if (not (vl-catch-all-error-p tmp)) (setq lst tmp))
                     )
                 )
               )
           )
          )
        )
        (if (and lst (> (length lst) 0))
            (progn
              (setq n (/ (length lst) 3))
              (setq i 0)
              (repeat n
                (setq pts (cons (list (nth (* i 3) lst)
                                       (nth (+ (* i 3) 1) lst)
                                       (nth (+ (* i 3) 2) lst))
                                 pts))
                (setq i (1+ i))
              )
            )
        )
      )
  )
  pts
)

(defun HCOL:AddUniquePt (pt lst tol / found)
  (setq found nil)
  (foreach p lst
    (if (<= (distance (list (car pt) (cadr pt) 0.0)
                       (list (car p) (cadr p) 0.0)) tol)
        (setq found T)
    )
  )
  (if found lst (cons pt lst))
)

(defun HCOL:DedupeSorted (vals tol / result last v)
  (setq result '())
  (setq last nil)
  (foreach v vals
    (if (or (null last) (> (abs (- v last)) tol))
        (progn (setq result (cons v result)) (setq last v))
    )
  )
  (reverse result)
)

(defun HCOL:BuildParamList (wallObj ptList / params p cp)
  (setq params '())
  (setq params (cons (vlax-curve-getStartParam wallObj) params))
  (setq params (cons (vlax-curve-getEndParam wallObj) params))
  (foreach pt ptList
    (setq p (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list wallObj pt)))
    (if (and p (not (vl-catch-all-error-p p)))
        (setq params (cons p params))
        (progn
          (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list wallObj pt nil)))
          (if (and cp (not (vl-catch-all-error-p cp)))
              (progn
                (setq p (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list wallObj cp)))
                (if (and p (not (vl-catch-all-error-p p)))
                    (setq params (cons p params))
                )
              )
          )
        )
    )
  )
  (setq params (vl-sort params '<))
  (HCOL:DedupeSorted params 1e-6)
)

;; ----------------------------------------------------------------------------
;; Shortest distance from a point to a column boundary curve.
;; ----------------------------------------------------------------------------
(defun HCOL:DistToBoundary (pt polyObj / cp)
  (setq cp (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list polyObj pt nil)))
  (if (and cp (not (vl-catch-all-error-p cp)))
      (distance (list (car pt) (cadr pt) 0.0) (list (car cp) (cadr cp) 0.0))
      1e99
  )
)

;; ----------------------------------------------------------------------------
;; Classify a point relative to ONE column: 'ON (within tol of the
;; boundary curve - coincident/edge case), 'IN (strictly inside the
;; closed shape, via point-in-polygon - genuine interior/crossing case),
;; or 'OUT (neither - genuinely external).
;; cd = (vlaObj . flattenedPointList)
;; ----------------------------------------------------------------------------
(defun HCOL:PtStatus (pt cd / colObj colPoly)
  (setq colObj (car cd))
  (setq colPoly (cdr cd))
  (cond
    ((<= (HCOL:DistToBoundary pt colObj) *HCOL-TOL*) 'ON)
    ((HCOL:PtInPolyP pt colPoly) 'IN)
    (T 'OUT)
  )
)

;; ----------------------------------------------------------------------------
;; Is a point ON or IN relative to ANY column in colData? (i.e. must be
;; discarded). Returns T if so, nil if the point is OUT for every column.
;; ----------------------------------------------------------------------------
(defun HCOL:PtBlockedP (pt colData / blocked cd st)
  (setq blocked nil)
  (foreach cd colData
    (if (not blocked)
        (progn
          (setq st (HCOL:PtStatus pt cd))
          (if (or (eq st 'ON) (eq st 'IN)) (setq blocked T))
        )
    )
  )
  blocked
)

;; ----------------------------------------------------------------------------
;; Put an object below another object using AutoCAD's SortentsTable.
;; This is the ActiveX equivalent of DRAWORDER -> Move Below and avoids
;; COMMAND/PEDIT completely. Here it is used so the SOLID hatch stays below
;; the column boundary, making the boundary clearly visible on top.
;; ----------------------------------------------------------------------------
(defun HCOL:PutObjectBelow (obj target owner / extDict sortObj arr res)
  (setq extDict
        (vl-catch-all-apply 'vla-GetExtensionDictionary (list owner)))
  (if (vl-catch-all-error-p extDict)
      nil
      (progn
        (setq sortObj
              (vl-catch-all-apply
                'vla-GetObject
                (list extDict "ACAD_SORTENTS")))

        ;; Create the SortentsTable if this space does not have one yet.
        (if (vl-catch-all-error-p sortObj)
            (setq sortObj
                  (vl-catch-all-apply
                    'vla-AddObject
                    (list extDict "ACAD_SORTENTS" "AcDbSortentsTable")))
        )

        (if (vl-catch-all-error-p sortObj)
            nil
            (progn
              (setq arr (vlax-make-safearray vlax-vbObject '(0 . 0)))
              (vlax-safearray-put-element arr 0 obj)
              (setq res
                    (vl-catch-all-apply
                      'vla-MoveBelow
                      (list sortObj arr target)))
              (not (vl-catch-all-error-p res))
            )
        )
      )
  )
)

(defun HCOL:CreateSolidHatch (colObj modelSpace / hatchObj sa vArr ok doc layers hatchLayer)
  ;; Dedicated layer for HCOL solid hatches.
  ;; The column remains on its original layer.
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  ;; Get SH-COLUMN; create it automatically if it does not exist.
  (setq hatchLayer
        (vl-catch-all-apply 'vla-Item (list layers "SH-COLUMN")))
  (if (vl-catch-all-error-p hatchLayer)
      (setq hatchLayer
            (vl-catch-all-apply 'vla-Add (list layers "SH-COLUMN")))
  )

  (setq hatchObj
        (vl-catch-all-apply
          'vla-AddHatch
          (list modelSpace 1 "SOLID" :vlax-false)))   ; 1 = acPreDefined

  (if (and hatchObj (not (vl-catch-all-error-p hatchObj)))
      (progn
        (setq sa (vlax-make-safearray vlax-vbObject (cons 0 0)))
        (vlax-safearray-fill sa (list colObj))
        (setq vArr
              (vlax-make-variant
                sa
                (logior vlax-vbarray vlax-vbobject)))

        (setq ok
              (vl-catch-all-apply
                'vla-AppendOuterLoop
                (list hatchObj vArr)))

        (if (not (vl-catch-all-error-p ok))
            (progn
              ;; Put every new HCOL hatch on SH-COLUMN.
              (vl-catch-all-apply
                'vla-put-Layer
                (list hatchObj "SH-COLUMN"))
              (vl-catch-all-apply 'vla-Evaluate (list hatchObj))

              ;; IMPORTANT: keep the hatch BELOW its column boundary.
              ;; This makes the duplicated column outline visible above
              ;; the solid fill without changing the draw order of the
              ;; rest of the drawing.
              (HCOL:PutObjectBelow hatchObj colObj modelSpace)
            )
            (vl-catch-all-apply 'vla-Delete (list hatchObj))
        )
      )
  )
  hatchObj
)

;; ----------------------------------------------------------------------------
;; Determine whether an entity counts as a valid closed column boundary.
;; Uses the generic vlax-curve-isClosed (works for CIRCLE, ELLIPSE, and
;; properly-closed LWPOLYLINE/POLYLINE alike), with a fallback: if that
;; reports "not closed" (e.g. a polyline drawn point-by-point and snapped
;; back to its start WITHOUT using the explicit Close option - AutoCAD
;; does not set the internal Closed flag in that case even though it
;; looks closed), treat it as closed anyway when its start and end points
;; coincide within *HCOL-TOL*.
;; ----------------------------------------------------------------------------
(defun HCOL:IsUsableColumn (ent / obj closedFlag sp ep)
  (setq obj (vlax-ename->vla-object ent))
  (setq closedFlag (vl-catch-all-apply 'vlax-curve-isClosed (list obj)))
  (cond
    ((and closedFlag (not (vl-catch-all-error-p closedFlag)) closedFlag) T)
    (T
     (setq sp (vl-catch-all-apply 'vlax-curve-getStartPoint (list obj)))
     (setq ep (vl-catch-all-apply 'vlax-curve-getEndPoint (list obj)))
     (if (and sp ep (not (vl-catch-all-error-p sp)) (not (vl-catch-all-error-p ep)))
         (<= (distance (list (car sp) (cadr sp) 0.0)
                        (list (car ep) (cadr ep) 0.0)) *HCOL-CLOSE-TOL*)
         nil
     )
    )
  )
)

(defun HCOL:CopyProps (edata baselist / code)
  (foreach code '(6 62 48 370)
    (if (assoc code edata)
        (setq baselist (append baselist (list (assoc code edata))))
    )
  )
  baselist
)

;; ----------------------------------------------------------------------------
;; Close a polyline WITHOUT using the AutoCAD COMMAND/PEDIT command.
;; This is important because COMMAND can produce:
;;   "bad order function: COMMAND"
;; when invoked while HCOL is already inside another command/error path.
;; The ActiveX Closed property works for LWPOLYLINE and 2D POLYLINE.
;; ----------------------------------------------------------------------------
(defun HCOL:ClosePolylineIfNeeded (ent / obj closedFlag res)
  (setq obj (vlax-ename->vla-object ent))
  (setq closedFlag
        (vl-catch-all-apply 'vlax-curve-isClosed (list obj)))
  (if (and closedFlag
           (not (vl-catch-all-error-p closedFlag))
           closedFlag)
      T
      (progn
        (setq res
              (vl-catch-all-apply
                'vla-put-Closed
                (list obj :vlax-true)))
        (if (vl-catch-all-error-p res)
            nil
            T
        )
      )
  )
)

;; ----------------------------------------------------------------------------
;; MAIN (AND ONLY) COMMAND
;; ----------------------------------------------------------------------------
(defun c:HCOL ( / doc modelSpace colSet colData colEnameSet oldColEnames
                  wallSet wallEnames wallResults wallEnt wallObj edata layer
                  allPts params i p1 p2 midP midPt segList
                  newSegCount delCount oldDelCount hatchCount skippedCount
                  n res ent obj newObj pt1 pt2 newdata totalWallsTouched
                  reportMsg closedNow closeRes autoClosedCount )

  (setq *HCOL-OLD-ERROR* *error*)
  (setq *error* 'HCOL:ErrorHandler)
  (setq *HCOL-OLD-CMDECHO* (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  ;; Keep document available to the error handler so it can close the
  ;; ActiveX undo mark without invoking (command).
  (setq *HCOL-DOC* doc)
  (setq modelSpace (vla-get-ModelSpace doc))

  ;; -------------------------------------------------------------------
  ;; 1) Select OLD columns  (closed LWPOLYLINE or old-style 2D POLYLINE)
  ;; -------------------------------------------------------------------
  (princ "\n>> Select the OLD COLUMN entitie(s) - polyline/circle/ellipse, then press ENTER: ")
  (setq colSet (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE"))))
  (if (null colSet)
      (progn
        (alert "HCOL: No column entities were selected.\nNothing was done - command cancelled.")
        (HCOL:ErrorHandler nil)
        (exit)
      )
  )
  (princ (strcat "\n>> " (itoa (sslength colSet)) " column entitie(s) picked."))

  ;; -------------------------------------------------------------------
  ;; 2) Select walls
  ;; -------------------------------------------------------------------
  (princ "\n>> Select WALL entities (LINE / LWPOLYLINE), then press ENTER: ")
  (setq wallSet (ssget '((0 . "LINE,LWPOLYLINE"))))
  (if (null wallSet)
      (progn
        (alert "HCOL: No wall entities were selected.\nNothing was done - command cancelled.")
        (HCOL:ErrorHandler nil)
        (exit)
      )
  )
  (princ (strcat "\n>> " (itoa (sslength wallSet)) " wall entitie(s) picked."))

  ;; ActiveX undo mark: safer than (command "_.undo" "_group").
  (vl-catch-all-apply 'vla-StartUndoMark (list doc))

  ;; -------------------------------------------------------------------
  ;; Duplicate every CLOSED old column -> colData / colEnameSet
  ;; colData item = (newDuplicateVlaObj . flattenedBoundaryPoints)
  ;; oldColEnames = the ORIGINAL entities, deleted at the very end
  ;; -------------------------------------------------------------------
  (setq colData '())
  (setq colEnameSet '())
  (setq oldColEnames '())
  (setq skippedCount 0)
  (setq autoClosedCount 0)
  (setq n (sslength colSet))
  (setq i 0)
  (while (< i n)
    (setq ent (ssname colSet i))
    (setq edata (entget ent))
    (if (HCOL:IsUsableColumn ent)
        (progn
          ;; If this column only passed the tolerant endpoint test,
          ;; close the polyline through ActiveX. NEVER use PEDIT/COMMAND.
          ;; This avoids the "bad order function: COMMAND" failure.
          (setq closedNow
                (vl-catch-all-apply
                  'vlax-curve-isClosed
                  (list (vlax-ename->vla-object ent))))
          (if (not (and closedNow
                        (not (vl-catch-all-error-p closedNow))
                        closedNow))
              (if (HCOL:ClosePolylineIfNeeded ent)
                  (setq autoClosedCount (1+ autoClosedCount))
                  (setq skippedCount (1+ skippedCount))
              )
          )
          (setq obj (vlax-ename->vla-object ent))  ; same entity, re-fetched safely
          (setq newObj (vl-catch-all-apply 'vla-copy (list obj)))
          (if (and newObj (not (vl-catch-all-error-p newObj)))
              (progn
                (setq colData (cons (cons newObj (HCOL:FlattenCurve newObj)) colData))
                (setq colEnameSet (cons (vlax-vla-object->ename newObj) colEnameSet))
                (setq oldColEnames (cons ent oldColEnames))
              )
              (setq skippedCount (1+ skippedCount))
          )
        )
        (setq skippedCount (1+ skippedCount))
    )
    (setq i (1+ i))
  )

  (if (= (length colData) 0)
      (progn
        (alert (strcat "HCOL: none of the " (itoa n)
                        " selected entitie(s) is a closed shape.\n"
                        "Supported: CIRCLE, ELLIPSE, or a closed\n"
                        "LWPOLYLINE/POLYLINE (drawn with RECTANG/BOUNDARY,\n"
                        "or a PLINE explicitly closed with its Close\n"
                        "option / snapped exactly back to its start point).\n"
                        "Command cancelled, nothing was changed."))
        (HCOL:ErrorHandler nil)
        (exit)
      )
  )

  ;; -------------------------------------------------------------------
  ;; Hatch every NEW duplicate boundary with SOLID
  ;; -------------------------------------------------------------------
  (setq hatchCount 0)
  (foreach cd colData
    (setq res (HCOL:CreateSolidHatch (car cd) modelSpace))
    (if (and res (not (vl-catch-all-error-p res)))
        (setq hatchCount (1+ hatchCount))
    )
  )

  ;; -------------------------------------------------------------------
  ;; Wall enames, excluding new duplicates / old columns re-picked by
  ;; mistake into the wall selection set.
  ;; -------------------------------------------------------------------
  (setq wallEnames '())
  (setq n (sslength wallSet))
  (setq i 0)
  (while (< i n)
    (setq ent (ssname wallSet i))
    (if (and (not (member ent colEnameSet)) (not (member ent oldColEnames)))
        (setq wallEnames (cons ent wallEnames))
    )
    (setq i (1+ i))
  )

  ;; -------------------------------------------------------------------
  ;; PHASE 1 - ANALYSE ONLY: split each wall at its intersections with
  ;; the new duplicate boundaries, keep only sub-segments whose midpoint
  ;; is OUT (neither ON any boundary nor IN any column) for every column.
  ;; -------------------------------------------------------------------
  (setq wallResults '())
  (setq totalWallsTouched 0)

  (foreach wallEnt wallEnames
    (setq wallObj (vlax-ename->vla-object wallEnt))
    (setq edata (entget wallEnt))

    (setq allPts '())
    (foreach cd colData
      (foreach np (HCOL:GetIntersectionPts (car cd) wallObj)
        (setq allPts (HCOL:AddUniquePt np allPts *HCOL-TOL*))
      )
    )

    (cond
      ;; ---- Case A: at least one real crossing point was found --------
      ((> (length allPts) 0)
       (setq totalWallsTouched (1+ totalWallsTouched))
       (setq params (HCOL:BuildParamList wallObj allPts))
       (setq segList '())
       (setq i 0)
       (while (< (1+ i) (length params))
         (setq p1 (nth i params))
         (setq p2 (nth (1+ i) params))
         (if (> (- p2 p1) 1e-9)
             (progn
               (setq midP  (/ (+ p1 p2) 2.0))
               (setq midPt (vl-catch-all-apply 'vlax-curve-getPointAtParam (list wallObj midP)))
               (if (and midPt (not (vl-catch-all-error-p midPt))
                        (not (HCOL:PtBlockedP midPt colData)))
                   (progn
                     (setq pt1 (vlax-curve-getPointAtParam wallObj p1))
                     (setq pt2 (vlax-curve-getPointAtParam wallObj p2))
                     (setq segList (cons (cons pt1 pt2) segList))
                   )
               )
             )
         )
         (setq i (1+ i))
       )
       (setq wallResults (cons (list wallEnt edata segList) wallResults))
      )
      ;; ---- Case B: zero crossing points -------------------------------
      ;; IntersectWith only reports discrete crossing points; a wall that
      ;; is fully COLLINEAR / COINCIDENT with a column edge (running
      ;; exactly on top of it) produces NO crossing points at all even
      ;; though it must still be discarded. Test the whole wall's
      ;; midpoint to catch that case; if it is not blocked by any
      ;; column, the wall is simply unrelated and is left untouched.
      (T
       (setq midPt (vl-catch-all-apply
                     'vlax-curve-getPointAtParam
                     (list wallObj
                           (/ (+ (vlax-curve-getStartParam wallObj)
                                 (vlax-curve-getEndParam wallObj)) 2.0))))
       (if (and midPt (not (vl-catch-all-error-p midPt))
                (HCOL:PtBlockedP midPt colData))
           (progn
             ;; entire wall lies on/inside a column -> discard fully,
             ;; keep nothing in its place.
             (setq totalWallsTouched (1+ totalWallsTouched))
             (setq wallResults (cons (list wallEnt edata '()) wallResults))
           )
       )
      )
    )
  )

  ;; -------------------------------------------------------------------
  ;; PHASE 2 - DRAW ONLY
  ;; -------------------------------------------------------------------
  (setq newSegCount 0)
  (foreach wr wallResults
    (setq edata (cadr wr))
    (setq layer (cdr (assoc 8 edata)))
    (foreach seg (caddr wr)
      (setq newdata (list (cons 0 "LINE")
                           (cons 8 layer)
                           (cons 10 (car seg))
                           (cons 11 (cdr seg))))
      (setq newdata (HCOL:CopyProps edata newdata))
      (if (entmake newdata) (setq newSegCount (1+ newSegCount)))
    )
  )

  ;; -------------------------------------------------------------------
  ;; PHASE 3 - DELETE ONLY: cut originals first, then the old columns.
  ;; -------------------------------------------------------------------
  (setq delCount 0)
  (foreach wr wallResults
    (setq wallEnt (car wr))
    (if (and wallEnt (entget wallEnt))
        (progn
          (entdel wallEnt)
          (setq delCount (1+ delCount))
        )
    )
  )

  (setq oldDelCount 0)
  (foreach oldEnt oldColEnames
    (if (and oldEnt (entget oldEnt))
        (progn
          (entdel oldEnt)
          (setq oldDelCount (1+ oldDelCount))
        )
    )
  )

  ;; Close the ActiveX undo mark; no COMMAND call.
  (vl-catch-all-apply 'vla-EndUndoMark (list doc))
  (setvar "cmdecho" *HCOL-OLD-CMDECHO*)
  (setq *HCOL-DOC* nil)
  (setq *error* *HCOL-OLD-ERROR*)

  (setq reportMsg
        (strcat
          "HCOL finished:\n"
          "- Columns duplicated & hatched: " (itoa hatchCount) "\n"
          "- Columns auto-closed (small gap fixed): " (itoa autoClosedCount) "\n"
          "- Columns skipped (not closed / copy failed): " (itoa skippedCount) "\n"
          "- Walls that touched a column: " (itoa totalWallsTouched) "\n"
          "- New external wall segments drawn: " (itoa newSegCount) "\n"
          "- Original wall entities removed: " (itoa delCount) "\n"
          "- Old column entities removed: " (itoa oldDelCount)
        ))
  (if (= totalWallsTouched 0)
      (setq reportMsg
            (strcat reportMsg
                    "\n\nNo wall touched any column, so nothing was cut.\n"
                    "Check that the wall lines actually cross/touch the\n"
                    "column polyline geometry (same XY plane, no offset\n"
                    "elevation), and that you selected the wall LINES\n"
                    "themselves, not a block containing them."))
  )
  (alert reportMsg)
  (princ)
)

(princ "\nHCOL loaded. Type HCOL to run.")
(princ)
