;;; =========================================================================
;;; AH CUSTOM TOOLS - MASTER FILE
;;; =========================================================================

;; 1. Global Version (قم بزيادة هذا الرقم كلما أضفت أوامر جديدة)
(setq *AH-VER* "1.0")

;; -------------------------------------------------------------------------
;; 2. Your Custom Commands & LISP Tools
;; -------------------------------------------------------------------------


;;; =========================================================================
;;; AH Tools - Updater Shortcut Command (AHU)
;;; =========================================================================
(defun c:AHU (/ updaterPath)
  (setq updaterPath "C:\\YQArch\\sys\\AH_Updater.exe")
  (if (findfile updaterPath)
    (progn
      (startapp updaterPath)
      (princ "\n[AH Tools] Launching Updater...")
    )
    (alert "Error: AH_Updater.exe not found at C:\\YQArch\\sys\\AH_Updater.exe")
  )
  (princ)
)







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
(defun c:SCL ( / col-layer run loop gr code pt is-toggled snap-info snap-pt snap-type draw-pt)
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

(princ "\nSCL loaded successfully. Type SCL to start.")
(princ)








;;; ============================================================================
;;;   HCOL.LSP  -  SINGLE COMMAND: HCOL
;;; ----------------------------------------------------------------------------
;;;   Supports ANY closed column shape: rectangular, polygonal, or an
;;;   arbitrary mix of straight/curved edges drawn as ONE closed
;;;   LWPOLYLINE or old-style 2D POLYLINE (RECTANG, BOUNDARY, or a PLINE
;;;   properly closed) - AND also a native CIRCLE or full ELLIPSE entity
;;;   drawn directly with the CIRCLE/ELLIPSE command. A polyline that was
;;;   only snapped back to its start point by eye (without using PLINE's
;;;   Close option) is still recognised, as long as its start and end
;;;   points coincide. All geometry tests (intersection, distance,
;;;   inside/outside) work on the curve generically.
;;;
;;;   Workflow:
;;;     1) Prompt: select the OLD column entities (closed LWPOLYLINE /
;;;        POLYLINE of any shape, or a CIRCLE / ELLIPSE).
;;;     2) Prompt: select WALL entities (LINE or LWPOLYLINE).
;;;     3) For every valid (closed) old column:
;;;        - create an exact duplicate (same shape/size/location) via
;;;          vla-Copy - this duplicate is the new column boundary.
;;;        - fill the duplicate with an ANSI37 hatch.
;;;        - for every wall, find exact intersection points with the
;;;          duplicate boundary (IntersectWith, acExtendNone = 0), split
;;;          the wall into ordered sub-segments.
;;;        - each sub-segment's midpoint is classified against every
;;;          column as:
;;;            ON  - within 0.001 of the boundary curve (sitting
;;;                  directly on top of / coincident with an edge)
;;;            IN  - strictly inside the closed column shape (a
;;;                  genuine interior/through-crossing portion, even
;;;                  far from any edge - detected with a point-in-
;;;                  polygon test, NOT just a raw distance check,
;;;                  because a chord through a real-size column can
;;;                  have its midpoint tens of centimetres from the
;;;                  nearest edge)
;;;            OUT - genuinely external -> kept
;;;        - ON and IN sub-segments are discarded; only OUT
;;;          sub-segments are drawn as new LINE entities, on the
;;;          wall's original layer.
;;;        - a wall with ZERO crossing points that is fully
;;;          coincident with a column edge (IntersectWith cannot
;;;          report an overlap as point intersections) is separately
;;;          detected and discarded whole.
;;;        - the original (now replaced) wall entities are deleted.
;;;        - the original old column is deleted, leaving only the new
;;;          duplicate + its hatch.
;;;     4) Whole process wrapped in one Undo Group, and a message box
;;;        (alert) at the end reports exactly what was done, so nothing
;;;        is missed even if the command-line history isn't checked.
;;; ============================================================================

(vl-load-com)

(setq *HCOL-TOL* 0.001)          ; precision tolerance for the actual cutting
(setq *HCOL-CLOSE-TOL* 0.01)     ; looser tolerance (1 cm) just for deciding
                                 ; whether a manually-closed polyline counts
                                 ; as closed (hand-snapping is rarely sub-mm)
(setq *HCOL-HATCH-TOL* 0.01)     ; tolerance (1 cm) used when looking for an OLD
                                 ; hatch that sits on a column

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
;; Extract every intersection point between two curve objects using IntersectWith.
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
;; Classify a point relative to ONE column ('ON, 'IN, or 'OUT)
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
;; Is a point ON or IN relative to ANY column in colData?
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
;; Delete any OLD hatch that sits on a column (called BEFORE the new hatch is
;; drawn). A hatch counts as "old hatch of this column" when its bounding box
;; lies completely inside the column's bounding box (+/- *HCOL-HATCH-TOL*), so
;; big hatches that merely pass under/over the column are never touched.
;; Returns (deletedCount failedCount).
;; ----------------------------------------------------------------------------
(defun HCOL:DeleteOldHatches (colObj / mn mx hmn hmx bbRes ss k ent hobj tol hdel hfail)
  (setq hdel 0)
  (setq hfail 0)
  (setq tol *HCOL-HATCH-TOL*)
  (setq bbRes (vl-catch-all-apply 'vla-GetBoundingBox (list colObj 'mn 'mx)))
  (if (not (vl-catch-all-error-p bbRes))
      (progn
        (setq mn (vlax-safearray->list mn))
        (setq mx (vlax-safearray->list mx))
        (setq ss (ssget "_C"
                        (list (- (car mn) tol) (- (cadr mn) tol))
                        (list (+ (car mx) tol) (+ (cadr mx) tol))
                        '((0 . "HATCH"))))
        (if ss
            (progn
              (setq k 0)
              (while (< k (sslength ss))
                (setq ent  (ssname ss k))
                (setq hobj (vlax-ename->vla-object ent))
                (setq bbRes (vl-catch-all-apply 'vla-GetBoundingBox (list hobj 'hmn 'hmx)))
                (if (not (vl-catch-all-error-p bbRes))
                    (progn
                      (setq hmn (vlax-safearray->list hmn))
                      (setq hmx (vlax-safearray->list hmx))
                      (if (and (>= (car hmn)  (- (car mn)  tol))
                               (>= (cadr hmn) (- (cadr mn) tol))
                               (<= (car hmx)  (+ (car mx)  tol))
                               (<= (cadr hmx) (+ (cadr mx) tol)))
                          (if (vl-catch-all-error-p (vl-catch-all-apply 'entdel (list ent)))
                              (setq hfail (1+ hfail))
                              (setq hdel (1+ hdel))
                          )
                      )
                    )
                )
                (setq k (1+ k))
              )
            )
        )
      )
  )
  (list hdel hfail)
)

;; ----------------------------------------------------------------------------
;; Create ANSI37 hatch and explicit Send-To-Back (Draw Order)
;; ----------------------------------------------------------------------------
(defun HCOL:CreateSolidHatch (colObj modelSpace / hatchObj sa vArr ok
                                     doc layers shLayer extDict sortTable saHatch vArrHatch)

  ;; Create / get dedicated hatch layer: SH-COLUMN
  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  (setq shLayer
        (vl-catch-all-apply
          'vla-Item
          (list layers "SH-COLUMN")))

  (if (vl-catch-all-error-p shLayer)
      (setq shLayer
            (vla-Add layers "SH-COLUMN"))
  )

  ;; Create ANSI37 hatch (PatternType = 1 -> acPreDefined)
  (setq hatchObj
        (vl-catch-all-apply
          'vla-AddHatch
          (list modelSpace 1 "ANSI37" :vlax-false)))

  (if (and hatchObj
           (not (vl-catch-all-error-p hatchObj)))

      (progn
        ;; Optional: Ensure standard pattern scale (adjust 1.0 if needed according to units)
        (vl-catch-all-apply 'vla-put-PatternScale (list hatchObj 1.0))

        ;; Build outer loop from the NEW column boundary
        (setq sa (vlax-make-safearray vlax-vbObject (cons 0 0)))
        (vlax-safearray-fill sa (list colObj))
        (setq vArr (vlax-make-variant sa (logior vlax-vbarray vlax-vbobject)))

        ;; Append column boundary as hatch loop
        (setq ok (vl-catch-all-apply 'vla-AppendOuterLoop (list hatchObj vArr)))

        (if (not (vl-catch-all-error-p ok))
            (progn
              ;; Put hatch on SH-COLUMN layer
              (vl-catch-all-apply 'vla-put-Layer (list hatchObj "SH-COLUMN"))

              ;; Evaluate hatch
              (vl-catch-all-apply 'vla-Evaluate (list hatchObj))

              ;; ---------------------------------------------------
              ;; DRAW ORDER ADJUSTMENT: Send Hatch to Back
              ;; Robust method using ModelSpace ExtensionDictionary/SortentsTable
              ;; ---------------------------------------------------
              (setq extDict (vl-catch-all-apply 'vla-GetExtensionDictionary (list modelSpace)))
              (if (and extDict (not (vl-catch-all-error-p extDict)))
                  (progn
                    (setq sortTable (vl-catch-all-apply 'vla-GetObject (list extDict "ACAD_SORTENTS")))
                    (if (vl-catch-all-error-p sortTable)
                        (setq sortTable (vl-catch-all-apply 'vla-AddObject (list extDict "ACAD_SORTENTS" "AcDbSortentsTable")))
                    )
                    (if (and sortTable (not (vl-catch-all-error-p sortTable)))
                        (progn
                          (setq saHatch (vlax-make-safearray vlax-vbObject (cons 0 0)))
                          (vlax-safearray-fill saHatch (list hatchObj))
                          (setq vArrHatch (vlax-make-variant saHatch (logior vlax-vbarray vlax-vbobject)))
                          (vl-catch-all-apply 'vla-MoveToBottom (list sortTable vArrHatch))
                        )
                    )
                  )
              )
            )

            ;; If hatch loop failed -> delete hatch
            (vl-catch-all-apply 'vla-Delete (list hatchObj))
        )
      )
  )

  hatchObj
)

;; ----------------------------------------------------------------------------
;; Determine whether an entity counts as a valid closed column boundary.
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
;; ----------------------------------------------------------------------------
(defun HCOL:ClosePolylineIfNeeded (ent / obj closedFlag res)
  (setq obj (vlax-ename->vla-object ent))
  (setq closedFlag (vl-catch-all-apply 'vlax-curve-isClosed (list obj)))
  (if (and closedFlag
           (not (vl-catch-all-error-p closedFlag))
           closedFlag)
      T
      (progn
        (setq res (vl-catch-all-apply 'vla-put-Closed (list obj :vlax-true)))
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
                  reportMsg closedNow closeRes autoClosedCount
                  oldMeasurement newMeasurement oldHatchDel oldHatchFail hRes )

  (setq *HCOL-OLD-ERROR* *error*)
  (setq *error* 'HCOL:ErrorHandler)
  (setq *HCOL-OLD-CMDECHO* (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *HCOL-DOC* doc)
  (setq modelSpace (vla-get-ModelSpace doc))

  ;; -------------------------------------------------------------------
  ;; 1) Select OLD columns
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

  ;; Start Undo Mark
  (vl-catch-all-apply 'vla-StartUndoMark (list doc))

  ;; Duplicate CLOSED old columns
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
          (setq closedNow (vl-catch-all-apply 'vlax-curve-isClosed (list (vlax-ename->vla-object ent))))
          (if (not (and closedNow
                        (not (vl-catch-all-error-p closedNow))
                        closedNow))
              (if (HCOL:ClosePolylineIfNeeded ent)
                  (setq autoClosedCount (1+ autoClosedCount))
                  (setq skippedCount (1+ skippedCount))
              )
          )
          (setq obj (vlax-ename->vla-object ent))
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
                       "LWPOLYLINE/POLYLINE.\n"
                       "Command cancelled, nothing was changed."))
        (HCOL:ErrorHandler nil)
        (exit)
      )
  )

  ;; -------------------------------------------------------------------
  ;; Hatch every NEW duplicate boundary with ANSI37 (includes Draw Order)
  ;; -------------------------------------------------------------------
  ;; MEASUREMENT -> 0 (set BEFORE any hatch is drawn)
  (setq oldMeasurement (getvar "MEASUREMENT"))
  (vl-catch-all-apply 'setvar (list "MEASUREMENT" 0))
  (setq newMeasurement (getvar "MEASUREMENT"))

  ;; Delete any OLD hatch on the column(s) BEFORE drawing the new hatch
  (setq oldHatchDel 0)
  (setq oldHatchFail 0)
  (foreach cd colData
    (setq hRes (HCOL:DeleteOldHatches (car cd)))
    (setq oldHatchDel  (+ oldHatchDel  (car hRes)))
    (setq oldHatchFail (+ oldHatchFail (cadr hRes)))
  )

  (setq hatchCount 0)
  (foreach cd colData
    (setq res (HCOL:CreateSolidHatch (car cd) modelSpace))
    (if (and res (not (vl-catch-all-error-p res)))
        (setq hatchCount (1+ hatchCount))
    )
  )

  ;; Filter wall selection set
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

  ;; PHASE 1 - ANALYSE
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
      (T
       (setq midPt (vl-catch-all-apply
                     'vlax-curve-getPointAtParam
                     (list wallObj
                           (/ (+ (vlax-curve-getStartParam wallObj)
                                 (vlax-curve-getEndParam wallObj)) 2.0))))
       (if (and midPt (not (vl-catch-all-error-p midPt))
                (HCOL:PtBlockedP midPt colData))
           (progn
             (setq totalWallsTouched (1+ totalWallsTouched))
             (setq wallResults (cons (list wallEnt edata '()) wallResults))
           )
       )
      )
    )
  )

  ;; PHASE 2 - DRAW ONLY
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

  ;; PHASE 3 - DELETE ORIGINALS
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

  ;; End Undo Mark
  (vl-catch-all-apply 'vla-EndUndoMark (list doc))
  (setvar "cmdecho" *HCOL-OLD-CMDECHO*)
  (setq *HCOL-DOC* nil)
  (setq *error* *HCOL-OLD-ERROR*)

  (setq reportMsg
        (strcat
          "HCOL finished:\n"
          "- MEASUREMENT: was " (vl-princ-to-string oldMeasurement)
          " -> now " (vl-princ-to-string newMeasurement) " (set before hatching)\n"
          "- Old hatches deleted before drawing the new ones: " (itoa oldHatchDel) "\n"
          (if (> oldHatchFail 0)
              (strcat "- Old hatches that could NOT be deleted (locked layer?): "
                      (itoa oldHatchFail) "\n")
              "")
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






;; =========================================================
;; Load Smart Wall Custom Tool Automatically with YQArch
;; =========================================================
(vl-load-com)
(load "YQ_SmartWall.lsp" "\nError: Could not load YQ_SmartWall.lsp")







;; ===========================================================================
;; Load APLAN Auto Apartment Planner Custom Tool Automatically with YQArch
;; ===========================================================================
(vl-load-com)
(load "APLAN.lsp" "\nError: Could not load APLAN.lsp")







;; ===========================================================================
;; Load AXCL.lsp Custom Tool Automatically with YQArch
;; ===========================================================================
(vl-load-com)
(load "AXCL.lsp" "\nError: Could not load AXCL.lsp")







;; ===========================================================================
;; Load SUPERWALL.lsp Custom Tool Automatically with YQArch
;; ===========================================================================
(vl-load-com)
(load "SUPERWALL.lsp" "\nError: Could not load SUPERWALL.lsp")


















;;; ------------------------------------------------------------------
;;; HWALL.lsp
;;; Solid hatch for walls drawn as pairs of parallel lines (open shapes)
;;; Command : HWALL
;;; Units   : meters
;;;
;;; How it works
;;;  1. Reads LINE, LWPOLYLINE, ARC and MLINE objects on layers whose
;;;     name contains WALL (any case). Layers containing HATCH are ignored.
;;;  2. Finds pairs of parallel lines (or concentric arcs) whose distance
;;;     is between 0.05 m and the max thickness typed by the user.
;;;  3. For every pair builds a closed boundary (the shorter common part)
;;;     and creates a SOLID hatch on layer WALL-HATCH (color 8).
;;;  4. Fills the small squares at L / T / X junctions.
;;;  5. Sends the new hatches to back. Old hatches are never deleted.
;;; ------------------------------------------------------------------
(vl-load-com)

(setq *hw-min* 0.05)
(if (not *hw-max*) (setq *hw-max* 0.30))

;;; ---------------- basic helpers ----------------

(defun hw:norm-ang (a)
  (while (< a 0.0) (setq a (+ a (* 2.0 pi))))
  (while (>= a (* 2.0 pi)) (setq a (- a (* 2.0 pi))))
  a
)

(defun hw:arc-ext (a1 a2 / d)
  (setq d (- a2 a1))
  (while (<= d 1.0e-9) (setq d (+ d (* 2.0 pi))))
  d
)

(defun hw:pt-polar (c r a)
  (list (+ (car c) (* r (cos a))) (+ (cadr c) (* r (sin a))))
)

(defun hw:uv (ux uy tt o)
  (list (- (* tt ux) (* o uy)) (+ (* tt uy) (* o ux)))
)

(defun hw:wall-layer-p (lay / u)
  (setq u (strcase lay))
  (and (vl-string-search "WALL" u)
       (not (vl-string-search "HATCH" u)))
)

;;; ---------------- segment extraction ----------------

;; bulge segment -> (L p1 p2) or (A center radius a1 a2)
(defun hw:bulge-seg (p1 p2 b / tmp th dx dy len r ux uy h cen)
  (if (< (abs b) 1.0e-9)
    (if (> (distance p1 p2) 1.0e-4) (list 'L p1 p2) nil)
    (progn
      (if (< b 0.0)
        (setq tmp p1 p1 p2 p2 tmp b (- b))
      )
      (setq th  (* 4.0 (atan b))
            dx  (- (car p2) (car p1))
            dy  (- (cadr p2) (cadr p1))
            len (sqrt (+ (* dx dx) (* dy dy)))
      )
      (if (< len 1.0e-6)
        nil
        (progn
          (setq r   (/ len (* 2.0 (sin (/ th 2.0))))
                ux  (/ dx len)
                uy  (/ dy len)
                h   (* r (cos (/ th 2.0)))
                cen (list (+ (/ (+ (car p1) (car p2)) 2.0) (* (- uy) h))
                          (+ (/ (+ (cadr p1) (cadr p2)) 2.0) (* ux h)))
          )
          (list 'A cen r
                (hw:norm-ang (angle cen p1))
                (hw:norm-ang (angle cen p2)))
        )
      )
    )
  )
)

(defun hw:lw-segs (ed / d vs closed n i res s a b)
  (foreach d ed
    (cond
      ((= (car d) 10)
       (setq vs (cons (list (cadr d) (caddr d) 0.0) vs)))
      ((and (= (car d) 42) vs)
       (setq vs (cons (list (car (car vs)) (cadr (car vs)) (cdr d)) (cdr vs))))
    )
  )
  (setq vs     (reverse vs)
        n      (length vs)
        closed (= 1 (logand 1 (cdr (assoc 70 ed))))
        i      0
  )
  (while (< i (1- n))
    (setq a (nth i vs)
          b (nth (1+ i) vs)
          s (hw:bulge-seg (list (car a) (cadr a)) (list (car b) (cadr b)) (caddr a))
    )
    (if s (setq res (cons s res)))
    (setq i (1+ i))
  )
  (if (and closed (> n 2))
    (progn
      (setq a (nth (1- n) vs)
            b (car vs)
            s (hw:bulge-seg (list (car a) (cadr a)) (list (car b) (cadr b)) (caddr a))
      )
      (if s (setq res (cons s res)))
    )
  )
  (reverse res)
)

(defun hw:mline-segs (en / obj res lst o e ed typ segs)
  (setq obj (vlax-ename->vla-object en)
        res (vl-catch-all-apply 'vla-explode (list obj))
  )
  (if (not (vl-catch-all-error-p res))
    (progn
      (setq lst (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value res))))
      (if (not (vl-catch-all-error-p lst))
        (foreach o lst
          (setq e   (vlax-vla-object->ename o)
                ed  (entget e)
                typ (cdr (assoc 0 ed))
          )
          (if (member typ '("LINE" "ARC"))
            (setq segs (append segs (hw:ent-segs e)))
          )
          (entdel e)
        )
      )
    )
  )
  segs
)

(defun hw:ent-segs (en / ed typ p1 p2)
  (setq ed  (entget en)
        typ (cdr (assoc 0 ed))
  )
  (cond
    ((= typ "LINE")
     (setq p1 (cdr (assoc 10 ed))
           p2 (cdr (assoc 11 ed))
           p1 (list (car p1) (cadr p1))
           p2 (list (car p2) (cadr p2))
     )
     (if (> (distance p1 p2) 1.0e-4)
       (list (list 'L p1 p2))
       nil
     )
    )
    ((= typ "ARC")
     (setq p1 (cdr (assoc 10 ed)))
     (list (list 'A
                 (list (car p1) (cadr p1))
                 (cdr (assoc 40 ed))
                 (hw:norm-ang (cdr (assoc 50 ed)))
                 (hw:norm-ang (cdr (assoc 51 ed)))))
    )
    ((= typ "LWPOLYLINE") (hw:lw-segs ed))
    ((= typ "MLINE") (hw:mline-segs en))
    (t nil)
  )
)

;;; ---------------- straight walls ----------------

;; COLUMN boundary detection. HATCH layers are deliberately ignored.
(defun hw:column-layer-p (lay / u)
  (setq u (strcase lay))
  (and (vl-string-search "COLUMN" u)
       (not (vl-string-search "HATCH" u)))
)

(defun hw:pt-on-edge-p (p e / a b dx dy px py ll cr dot tol)
  (setq a (car e)
        b (cadr e)
        dx (- (car b) (car a))
        dy (- (cadr b) (cadr a))
        px (- (car p) (car a))
        py (- (cadr p) (cadr a))
        ll (+ (* dx dx) (* dy dy))
        cr (- (* dx py) (* dy px))
        dot (+ (* px dx) (* py dy))
        tol 1.0e-5)
  (and (> ll 1.0e-12)
       (<= (abs cr) (* tol (sqrt ll)))
       (>= dot (- (* tol (sqrt ll))))
       (<= dot (+ ll (* tol (sqrt ll)))))
)

(defun hw:point-on-column-edge-p (p edges / e hit)
  (foreach e edges
    (if (and (not hit) (hw:pt-on-edge-p p e))
      (setq hit T)))
  hit
)

;; Even/odd ray test works for closed column polylines and closed loops
;; made from separate LINE objects.
(defun hw:point-in-columns-p (p edges / e a b x y ay by ix hits onedge)
  (setq x (car p) y (cadr p) hits 0 onedge nil)
  (foreach e edges
    (setq a (car e) b (cadr e)
          ay (cadr a) by (cadr b))
    (if (hw:pt-on-edge-p p e)
      (setq onedge T))
    (if (or (and (<= ay y) (> by y))
            (and (<= by y) (> ay y)))
      (progn
        (setq ix (+ (car a) (* (/ (- y ay) (- by ay)) (- (car b) (car a)))) )
        (if (> ix x) (setq hits (1+ hits)))))
  )
  (or onedge (= (rem hits 2) 1))
)

(defun hw:edge-cut-param (p1 p2 e / q1 q2 rx ry sx sy qpx qpy den tt uu)
  (setq q1 (car e) q2 (cadr e)
        rx (- (car p2) (car p1))
        ry (- (cadr p2) (cadr p1))
        sx (- (car q2) (car q1))
        sy (- (cadr q2) (cadr q1))
        qpx (- (car q1) (car p1))
        qpy (- (cadr q1) (cadr p1))
        den (- (* rx sy) (* ry sx)))
  (if (> (abs den) 1.0e-10)
    (progn
      (setq tt (/ (- (* qpx sy) (* qpy sx)) den)
            uu (/ (- (* qpx ry) (* qpy rx)) den))
      (if (and (>= tt -1.0e-8) (<= tt 1.00000001)
               (>= uu -1.0e-8) (<= uu 1.00000001))
        (max 0.0 (min 1.0 tt)))))
)

(defun hw:unique-params (nums / sorted result prev n)
  (setq sorted (vl-sort nums '<) result nil prev nil)
  (foreach n sorted
    (if (or (null prev) (> (abs (- n prev)) 1.0e-7))
      (setq result (cons n result) prev n)))
  (reverse result)
)

;; Split a straight wall segment at column boundaries and discard portions
;; inside the column footprint. Each result is (p1 p2 blocked-start blocked-end).
(defun hw:trim-wall-seg (p1 p2 edges / cuts e tt ordered t0 t1 tm pm q1 q2 res)
  (setq cuts (list 0.0 1.0))
  (foreach e edges
    (if (setq tt (hw:edge-cut-param p1 p2 e))
      (setq cuts (cons tt cuts))))
  (setq ordered (hw:unique-params cuts)
        res nil)
  (while (cdr ordered)
    (setq t0 (car ordered)
          t1 (cadr ordered))
    (if (> (- t1 t0) 1.0e-6)
      (progn
        (setq tm (/ (+ t0 t1) 2.0)
              pm (list (+ (car p1) (* (- (car p2) (car p1)) tm))
                       (+ (cadr p1) (* (- (cadr p2) (cadr p1)) tm)))
        )
        (if (not (hw:point-in-columns-p pm edges))
          (progn
            (setq q1 (list (+ (car p1) (* (- (car p2) (car p1)) t0))
                           (+ (cadr p1) (* (- (cadr p2) (cadr p1)) t0)))
                  q2 (list (+ (car p1) (* (- (car p2) (car p1)) t1))
                           (+ (cadr p1) (* (- (cadr p2) (cadr p1)) t1)))
                  res (cons (list q1 q2
                                  (or (> t0 1.0e-6) (hw:point-on-column-edge-p q1 edges))
                                  (or (< t1 0.999999) (hw:point-on-column-edge-p q2 edges))) res))
          )
        )
      )
    )
    (setq ordered (cdr ordered))
  )
  (reverse res)
)

;; Line record adds blocked-start / blocked-end flags at indexes 8 and 9.
(defun hw:make-line-core (p1 p2 cut1 cut2 / dx dy len ux uy tmp tmpc)
  (setq dx  (- (car p2) (car p1))
        dy  (- (cadr p2) (cadr p1))
        len (sqrt (+ (* dx dx) (* dy dy)))
        ux  (/ dx len)
        uy  (/ dy len))
  ;; Use the dominant axis to keep near-vertical lines oriented consistently.
  ;; A tiny negative X component must not reverse an otherwise upward line.
  (if (or (and (>= (abs ux) (abs uy)) (< ux 0.0))
          (and (< (abs ux) (abs uy)) (< uy 0.0)))
    (setq tmp p1 p1 p2 p2 tmp
          tmpc cut1 cut1 cut2 cut2 tmpc
          ux (- ux) uy (- uy)))
  (list p1 ux uy len
        (min (car p1) (car p2)) (min (cadr p1) (cadr p2))
        (max (car p1) (car p2)) (max (cadr p1) (cadr p2))
        cut1 cut2)
)

(defun hw:make-line (p1 p2)
  (hw:make-line-core p1 p2 nil nil)
)

;; Detect a perpendicular thickness step at a candidate hatch endpoint.
;; Stop at that step instead of extending the thinner hatch by *hw-max*.
(defun hw:step-edge-p (p ref partner all / ux uy nx ny refp1 refp2 partp1 e ep1 ep2 c cp1 cp2 cr gpair galt tc rvec cvec rproj cproj hit)
  (setq ux (nth 1 ref)
        uy (nth 2 ref)
        nx (- uy)
        ny ux
        refp1 (car ref)
        refp2 (list (+ (car refp1) (* ux (nth 3 ref)))
                    (+ (cadr refp1) (* uy (nth 3 ref))))
        partp1 (car partner)
        gpair (abs (+ (* nx (- (car partp1) (car refp1)))
                      (* ny (- (cadr partp1) (cadr refp1))))))
  (foreach e all
    (if (not hit)
      (progn
        (setq ep1 (car e)
              ep2 (list (+ (car ep1) (* (nth 1 e) (nth 3 e)))
                        (+ (cadr ep1) (* (nth 2 e) (nth 3 e))))
              cr (abs (- (* ux (nth 2 e)) (* uy (nth 1 e)))))
        (if (and (> cr 0.50)
                 (or (<= (distance p refp1) 1.0e-3)
                     (<= (distance p refp2) 1.0e-3))
                 (<= (hw:pt-seg-dist p ep1 ep2) 1.0e-3))
          (foreach c all
            (if (and (not hit)
                     (not (equal c ref))
                     (not (equal c partner)))
              (progn
                (setq cp1 (car c)
                      cp2 (list (+ (car cp1) (* (nth 1 c) (nth 3 c)))
                                (+ (cadr cp1) (* (nth 2 c) (nth 3 c))))
                      cr (abs (- (* ux (nth 2 c)) (* uy (nth 1 c)))))
                (setq tc (hw:edge-cut-param cp1 cp2 (list ep1 ep2)))
                (if (and (< cr 0.01)
                         tc
                         (or (<= (abs tc) 1.0e-3)
                             (<= (abs (- tc 1.0)) 1.0e-3)))
                  (progn
                    (setq rvec (if (<= (distance p refp1) 1.0e-3)
                                 (list (- (car refp2) (car refp1))
                                       (- (cadr refp2) (cadr refp1)))
                                 (list (- (car refp1) (car refp2))
                                       (- (cadr refp1) (cadr refp2))))
                          cvec (if (< tc 0.5)
                                 (list (- (car cp2) (car cp1))
                                       (- (cadr cp2) (cadr cp1)))
                                 (list (- (car cp1) (car cp2))
                                       (- (cadr cp1) (cadr cp2))))
                          rproj (+ (* ux (car rvec)) (* uy (cadr rvec)))
                          cproj (+ (* ux (car cvec)) (* uy (cadr cvec)))
                          galt
                      (abs (+ (* nx (- (car cp1) (car refp1)))
                              (* ny (- (cadr cp1) (cadr refp1))))))
                    (if (and (< (* rproj cproj) 0.0)
                             (> (abs (- galt gpair)) 0.005))
                      (setq hit T))))))))))
  )
  hit
)

(defun hw:pair-step-end-p (a b tt t1 dp / ap ux uy bp bux buy k pa pb)
  (setq ap (car a)
        ux (nth 1 a)
        uy (nth 2 a)
        bp (car b)
        bux (nth 1 b)
        buy (nth 2 b)
        k (/ (- tt t1) dp)
        pa (list (+ (car ap) (* ux tt))
                 (+ (cadr ap) (* uy tt)))
        pb (list (+ (car bp) (* bux k))
                 (+ (cadr bp) (* buy k))))
  (or (hw:step-edge-p pa a b *hw-lines*)
      (hw:step-edge-p pb b a *hw-lines*))
)

;; rect record: (P1 P2 P3 P4 ux uy omin omax tlo thi skew blocked-lo blocked-hi)
;; A small angular mismatch is accepted only while the actual gap stays within
;; the requested wall-thickness limits at both ends. Skewed pairs use a true
;; quadrilateral rather than forcing the second face to be perfectly parallel.
(defun hw:line-pair (a b / ap ux uy alen bp bux buy blen cr nx ny dx dy s dp
                       t1 t2 bl bh lo hi lo0 hi0 ulo uhi p1 p2 p3 p4 om1
                       slo shi k1 k2 blocklo blockhi steplo stephi skew)
  (setq ap   (car a)
        ux   (nth 1 a)
        uy   (nth 2 a)
        alen (nth 3 a)
        bp   (car b)
        bux  (nth 1 b)
        buy  (nth 2 b)
        blen (nth 3 b)
        cr   (- (* ux buy) (* uy bux))
        dp   (+ (* ux bux) (* uy buy)))
  (if (and (< (abs cr) 0.01) (> dp 0.0))
    (progn
      (setq nx (- uy)
            ny ux
            dx (- (car bp) (car ap))
            dy (- (cadr bp) (cadr ap))
            s  (+ (* nx dx) (* ny dy))
            t1 (+ (* ux dx) (* uy dy))
            t2 (+ t1 (* blen dp))
            bl (min t1 t2)
            bh (max t1 t2)
            lo0 (max 0.0 bl)
            hi0 (min alen bh))
      (if (> (- hi0 lo0) 1.0e-4)
        (progn
          (setq slo (+ s (* cr (/ (- lo0 t1) dp)))
                shi (+ s (* cr (/ (- hi0 t1) dp))))
          (if (and (> (* slo shi) 0.0)
                   (>= (abs slo) (- *hw-min* 1.0e-4))
                   (<= (abs slo) (+ *hw-max* 1.0e-4))
                   (>= (abs shi) (- *hw-min* 1.0e-4))
                   (<= (abs shi) (+ *hw-max* 1.0e-4)))
            (progn
              (setq ulo (min 0.0 bl)
                    uhi (max alen bh)
                    ;; Step endpoints stop longitudinal extension, but only
                    ;; column-cut flags are stored in the rect for gap filling.
                    blocklo (or (nth 8 a) (nth 8 b))
                    blockhi (or (nth 9 a) (nth 9 b))
                    steplo (hw:pair-step-end-p a b lo0 t1 dp)
                    stephi (hw:pair-step-end-p a b hi0 t1 dp)
                    lo (if (or blocklo steplo) lo0 (max ulo (- lo0 *hw-max*)))
                    hi (if (or blockhi stephi) hi0 (min uhi (+ hi0 *hw-max*)))
                    k1 (/ (- lo t1) dp)
                    k2 (/ (- hi t1) dp)
                    p1 (list (+ (car ap) (* ux lo)) (+ (cadr ap) (* uy lo)))
                    p2 (list (+ (car ap) (* ux hi)) (+ (cadr ap) (* uy hi)))
                    p3 (list (+ (car bp) (* bux k2)) (+ (cadr bp) (* buy k2)))
                    p4 (list (+ (car bp) (* bux k1)) (+ (cadr bp) (* buy k1)))
                    om1 (+ (* nx (car ap)) (* ny (cadr ap)))
                    skew (> (abs cr) 2.0e-4))
              (list p1 p2 p3 p4 ux uy
                    (min om1 (+ om1 s)) (max om1 (+ om1 s))
                    (+ (* ux (car p1)) (* uy (cadr p1)))
                    (+ (* ux (car p2)) (* uy (cadr p2)))
                    skew blocklo blockhi)))))))
)

;; Supplemental recovery for a skewed pair whose gap changes along its
;; overlap. The normal hw:line-pair routine runs first and is not changed.
;; This returns only the longitudinal interval where the measured gap remains
;; within the existing [*hw-min*, *hw-max*] limits.
(defun hw:line-pair-clipped (a b / ap ux uy alen bp bux buy blen cr dp nx ny dx dy
                               s t1 t2 bl bh lo hi slo shi dlo dhi dmin dmax ta tb
                               clo chi k1 k2 p1 p2 p3 p4 om1 blocklo blockhi)
  (setq ap   (car a)
        ux   (nth 1 a)
        uy   (nth 2 a)
        alen (nth 3 a)
        bp   (car b)
        bux  (nth 1 b)
        buy  (nth 2 b)
        blen (nth 3 b)
        cr   (- (* ux buy) (* uy bux))
        dp   (+ (* ux bux) (* uy buy)))
  (if (and (< (abs cr) 0.01) (> dp 0.0))
    (progn
      (setq nx (- uy)
            ny ux
            dx (- (car bp) (car ap))
            dy (- (cadr bp) (cadr ap))
            s  (+ (* nx dx) (* ny dy))
            t1 (+ (* ux dx) (* uy dy))
            t2 (+ t1 (* blen dp))
            bl (min t1 t2)
            bh (max t1 t2)
            lo (max 0.0 bl)
            hi (min alen bh)
            dmin (- *hw-min* 1.0e-4)
            dmax (+ *hw-max* 1.0e-4))
      (if (> (- hi lo) 1.0e-4)
        (progn
          (setq slo (+ s (* cr (/ (- lo t1) dp)))
                shi (+ s (* cr (/ (- hi t1) dp)))
                dlo (abs slo)
                dhi (abs shi))
          ;; Do not hatch crossed lines or pairs whose gap is effectively zero.
          ;; This pass is needed only when the normal full-span test rejects
          ;; at least one end because the gap changes outside the allowed range.
          (if (and (> (* slo shi) 0.0)
                   (or (< dlo dmin) (> dlo dmax)
                       (< dhi dmin) (> dhi dmax))
                   (> (abs (- dhi dlo)) 1.0e-9))
            (progn
              ;; Solve the two points where the linear gap crosses min/max.
              (setq ta (+ lo (* (/ (- dmin dlo) (- dhi dlo)) (- hi lo)))
                    tb (+ lo (* (/ (- dmax dlo) (- dhi dlo)) (- hi lo)))
                    clo (max lo (min ta tb))
                    chi (min hi (max ta tb)))
              (if (> (- chi clo) 1.0e-4)
                (progn
                  (setq k1 (/ (- clo t1) dp)
                        k2 (/ (- chi t1) dp)
                        p1 (list (+ (car ap) (* ux clo))
                                 (+ (cadr ap) (* uy clo)))
                        p2 (list (+ (car ap) (* ux chi))
                                 (+ (cadr ap) (* uy chi)))
                        p3 (list (+ (car bp) (* bux k2))
                                 (+ (cadr bp) (* buy k2)))
                        p4 (list (+ (car bp) (* bux k1))
                                 (+ (cadr bp) (* buy k1)))
                        om1 (+ (* nx (car ap)) (* ny (cadr ap)))
                        blocklo (or (nth 8 a) (nth 8 b))
                        blockhi (or (nth 9 a) (nth 9 b)))
                  (list p1 p2 p3 p4 ux uy
                        (min om1 (+ om1 s))
                        (max om1 (+ om1 s))
                        clo chi (> (abs cr) 2.0e-4)
                        blocklo blockhi)))))))))
)

(defun hw:rect->poly (r / res k)
  (foreach k '(0 1 2 3)
    (setq res (cons (list (car (nth k r)) (cadr (nth k r)) 0.0) res))
  )
  (reverse res)
)

;;; ---------------- curved walls ----------------

(defun hw:arc-pieces (c r1 r2 lo hi / n step i a1 a2 bg res)
  (setq n    (max 1 (fix (+ 0.999999 (/ (- hi lo) pi))))
        step (/ (- hi lo) n)
        bg   (/ (sin (/ step 4.0)) (cos (/ step 4.0)))
        i    0
  )
  (while (< i n)
    (setq a1 (+ lo (* i step))
          a2 (+ a1 step)
    )
    (setq res
      (cons (list (append (hw:pt-polar c r1 a1) (list bg))
                  (append (hw:pt-polar c r1 a2) (list 0.0))
                  (append (hw:pt-polar c r2 a2) (list (- bg)))
                  (append (hw:pt-polar c r2 a1) (list 0.0)))
            res)
    )
    (setq i (1+ i))
  )
  res
)

;; arc record: (center radius a1 a2)
(defun hw:arc-pair (a b / ca ra sa ea cb rb sb eb d r1 r2 k sbk lo hi res)
  (setq ca (car a)
        ra (cadr a)
        sa (caddr a)
        ea (hw:arc-ext sa (cadddr a))
        cb (car b)
        rb (cadr b)
        sb (caddr b)
        eb (hw:arc-ext sb (cadddr b))
  )
  (if (< (distance ca cb) 1.0e-3)
    (progn
      (setq d (abs (- ra rb)))
      (if (and (>= d (- *hw-min* 1.0e-4)) (<= d (+ *hw-max* 1.0e-4)))
        (progn
          (setq r1 (max ra rb)
                r2 (min ra rb)
          )
          (foreach k '(-1 0 1)
            (setq sbk (+ sb (* k 2.0 pi))
                  lo  (max sa sbk)
                  hi  (min (+ sa ea) (+ sbk eb))
            )
            (if (> (* r2 (- hi lo)) 1.0e-4)
              (setq res (append res (hw:arc-pieces ca r1 r2 lo hi)))
            )
          )
        )
      )
    )
  )
  res
)

;;; ---------------- junction filling (X crossings) ----------------

(defun hw:cr (a b p)
  (- (* (- (car b) (car a)) (- (cadr p) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car p) (car a))))
)

(defun hw:in-quad (p q / c1 c2 c3 c4)
  (setq c1 (hw:cr (nth 0 q) (nth 1 q) p)
        c2 (hw:cr (nth 1 q) (nth 2 q) p)
        c3 (hw:cr (nth 2 q) (nth 3 q) p)
        c4 (hw:cr (nth 3 q) (nth 0 q) p)
  )
  (or (and (>= c1 -1.0e-6) (>= c2 -1.0e-6) (>= c3 -1.0e-6) (>= c4 -1.0e-6))
      (and (<= c1 1.0e-6) (<= c2 1.0e-6) (<= c3 1.0e-6) (<= c4 1.0e-6)))
)

(defun hw:in-rects (p rects / q ok)
  (foreach q rects
    (if (and (not ok) (hw:in-quad p q))
      (setq ok T)
    )
  )
  ok
)

(defun hw:on-line (p lines / l ok dx dy tt pp)
  (foreach l lines
    (if (not ok)
      (progn
        (setq dx (- (car p) (car (car l)))
              dy (- (cadr p) (cadr (car l)))
              tt (+ (* (nth 1 l) dx) (* (nth 2 l) dy))
              pp (abs (- (* (nth 1 l) dy) (* (nth 2 l) dx)))
        )
        (if (and (>= tt -1.0e-3) (<= tt (+ (nth 3 l) 1.0e-3)) (< pp 1.0e-3))
          (setq ok T)
        )
      )
    )
  )
  ok
)

;; gap between two collinear wall pieces: fill only if both faces are
;; supported (a continuous wall line, or another wall touching the face)
(defun hw:gap-test (r1 r2 rects lines / ux uy om ox t1 t2 tm)
  (setq ux (nth 4 r1)
        uy (nth 5 r1)
        om (nth 6 r1)
        ox (nth 7 r1)
        t1 (nth 9 r1)
        t2 (nth 8 r2)
        tm (/ (+ t1 t2) 2.0)
  )
  (if (and (or (hw:on-line (hw:uv ux uy tm om) lines)
               (hw:in-rects (hw:uv ux uy tm (- om 0.02)) rects))
           (or (hw:on-line (hw:uv ux uy tm ox) lines)
               (hw:in-rects (hw:uv ux uy tm (+ ox 0.02)) rects)))
    (list (hw:uv ux uy tm (/ (+ om ox) 2.0))
          (list (append (hw:uv ux uy t1 om) (list 0.0))
                (append (hw:uv ux uy t2 om) (list 0.0))
                (append (hw:uv ux uy t2 ox) (list 0.0))
                (append (hw:uv ux uy t1 ox) (list 0.0))))
    nil
  )
)

(defun hw:gap-fill (rects lines / res done rest a b r1 r2 g f c)
  (setq rest rects)
  (while rest
    (setq a    (car rest)
          rest (cdr rest)
    )
    (foreach b rest
      (if (and (not (nth 10 a)) (not (nth 10 b))
               (< (abs (- (nth 4 a) (nth 4 b))) 1.0e-4)
               (< (abs (- (nth 5 a) (nth 5 b))) 1.0e-4)
               (< (abs (- (nth 6 a) (nth 6 b))) 5.0e-4)
               (< (abs (- (nth 7 a) (nth 7 b))) 5.0e-4))
        (progn
          (setq r1 nil r2 nil)
          (cond
            ((>= (nth 8 b) (nth 9 a)) (setq r1 a r2 b))
            ((>= (nth 8 a) (nth 9 b)) (setq r1 b r2 a))
          )
          ;; Never refill the specific end gap cut by a column boundary.
          (if (and r1 (or (nth 12 r1) (nth 11 r2)))
            (setq r1 nil r2 nil))
          (if r1
            (progn
              (setq g (- (nth 8 r2) (nth 9 r1)))
              (if (and (> g 1.0e-4) (<= g (+ *hw-max* 1.0e-4)))
                (progn
                  (setq f (hw:gap-test r1 r2 rects lines))
                  (if f
                    (if (not (vl-some '(lambda (c) (< (distance c (car f)) 1.0e-3)) done))
                      (setq done (cons (car f) done)
                            res  (cons (cadr f) res))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  res
)


;;; ---------------- conservative residual L/T patching ----------------

(defun hw:pt-seg-dist (p a b / dx dy ll tt q)
  (setq dx (- (car b) (car a))
        dy (- (cadr b) (cadr a))
        ll (+ (* dx dx) (* dy dy)))
  (if (< ll 1.0e-12)
    (distance p a)
    (progn
      (setq tt (/ (+ (* (- (car p) (car a)) dx)
                   (* (- (cadr p) (cadr a)) dy)) ll)
            tt (max 0.0 (min 1.0 tt))
            q (list (+ (car a) (* tt dx))
                    (+ (cadr a) (* tt dy))))
      (distance p q)))
)

(defun hw:poly-center (poly / p sx sy n)
  (setq sx 0.0 sy 0.0 n 0)
  (foreach p poly
    (setq sx (+ sx (car p))
          sy (+ sy (cadr p))
          n (1+ n)))
  (if (> n 0) (list (/ sx n) (/ sy n)) nil)
)

;; True when the candidate touches/intersects a rectangle or comes within tol.
(defun hw:poly-near-rect-p (poly rr tol / q p v i j a b near)
  (setq q (hw:rect->poly rr))
  (foreach p poly
    (if (hw:in-quad p q) (setq near T)))
  (foreach p q
    (if (hw:in-quad p poly) (setq near T)))
  (setq i 0)
  (while (and (< i (length poly)) (not near))
    (setq a (nth i poly)
          b (nth (rem (1+ i) (length poly)) poly))
    (setq j 0)
    (while (and (< j (length q)) (not near))
      (if (<= (hw:pt-seg-dist a (nth j q) (nth (rem (1+ j) (length q)) q)) tol)
        (setq near T))
      (if (<= (hw:pt-seg-dist (nth j q) a b) tol)
        (setq near T))
      (setq j (1+ j)))
    (setq i (1+ i)))
  near
)

;; Sample interior points rather than boundary vertices, so merely touching a
;; column face is allowed but any meaningful incursion into a column is rejected.
(defun hw:poly-enters-columns-p (poly edges / ctr p ip bad)
  (if edges
    (progn
      (setq ctr (hw:poly-center poly))
      (if (and ctr (hw:point-in-columns-p ctr edges))
        (setq bad T))
      (foreach p poly
        (if (not bad)
          (progn
            (setq ip (list (+ (* 0.80 (car p)) (* 0.20 (car ctr)))
                           (+ (* 0.80 (cadr p)) (* 0.20 (cadr ctr)))))
            (if (hw:point-in-columns-p ip edges)
              (setq bad T)))))
    )
  )
  bad
)

;; Test whether the sampled boundary and center of a candidate are already
;; covered by accepted rectangles. If any sample is uncovered, retain the patch
;; so a covered center cannot hide a missing narrow end of the candidate.
(defun hw:poly-covered-by-rects-p (poly rects / samples p i a b ctr all-covered)
  (setq samples nil
        ctr (hw:poly-center poly)
        i 0
        all-covered T)
  (if ctr (setq samples (cons ctr samples)))
  (while (< i (length poly))
    (setq a (nth i poly)
          b (nth (rem (1+ i) (length poly)) poly)
          samples (cons a samples)
          samples (cons (list (/ (+ (car a) (car b)) 2.0)
                              (/ (+ (cadr a) (cadr b)) 2.0)) samples)
          i (1+ i)))
  (if (null rects) (setq all-covered nil))
  (foreach p samples
    (if (not (hw:in-rects p rects))
      (setq all-covered nil)))
  all-covered
)

;; Find only those partial, valid-thickness spans rejected by the complete-pair
;; test. Sample checks avoid unnecessary duplicate patches while preserving an
;; uncovered end; column checks keep recovery away from column footprints.
(defun hw:transition-patches (lines rects col-edges max-thick
                              / rest a rr b lim cand poly ctr centers result)
  (setq rest lines centers nil result nil)
  (while rest
    (setq a (car rest)
          rest (cdr rest)
          rr rest
          lim (+ (nth 6 a) max-thick 1.0e-3))
    (while (and rr (<= (nth 4 (car rr)) lim))
      (setq b (car rr)
            rr (cdr rr))
      (if (and (<= (nth 5 a) (+ (nth 7 b) max-thick 1.0e-3))
               (>= (nth 7 a) (- (nth 5 b) max-thick 1.0e-3))
               (not (hw:line-pair a b)))
        (if (setq cand (hw:line-pair-clipped a b))
          (progn
            (setq poly (hw:rect->poly cand)
                  ctr (hw:poly-center poly))
            (if (and ctr
                     (not (hw:point-in-columns-p ctr col-edges))
                     (not (hw:poly-enters-columns-p poly col-edges))
                     (not (vl-some '(lambda (p) (< (distance p ctr) 0.003)) centers))
                     (not (vl-some '(lambda (r)
                                      (hw:in-quad ctr (hw:rect->poly r))) result)))
              (setq centers (cons ctr centers)
                    result (cons cand result))))))))
  result
)

;; Recover only short, slightly over-thick wall pieces at a supported L/T
;; junction. The normal pair test always runs first with the original limit.
(defun hw:residual-junction-patches (lines rects col-edges base-max
                                     / *hw-max* rest a rr b lim cand poly ctr
                                       support ok dx dy crossang span centers c patch-list)
  (setq *hw-max* base-max
        rest lines
        centers nil)
  (while rest
    (setq a (car rest)
          rest (cdr rest)
          rr rest
          lim (+ (nth 6 a) base-max 0.021))
    (while (and rr (<= (nth 4 (car rr)) lim))
      (setq b (car rr)
            rr (cdr rr))
      (if (and (<= (nth 5 a) (+ (nth 7 b) base-max 0.021))
               (>= (nth 7 a) (- (nth 5 b) base-max 0.021))
               (not (hw:line-pair a b)))
        (progn
          ;; The 2 cm allowance exists only in this conservative fallback.
          (setq *hw-max* (+ base-max 0.02)
                cand (hw:line-pair a b)
                *hw-max* base-max)
          (if cand
            (progn
              (setq span (abs (- (nth 9 cand) (nth 8 cand)))
                    poly (hw:rect->poly cand)
                    ctr (hw:poly-center poly)
                    ok nil)
              ;; Keep the fallback local: no more than twice the original
              ;; maximum wall thickness along the wall direction.
              (if (and (<= span (+ (* 2.0 base-max) 1.0e-4))
                       ctr
                       (not (hw:in-rects ctr rects))
                       (not (hw:poly-enters-columns-p poly col-edges)))
                (foreach support rects
                  (if (not ok)
                    (progn
                      (setq crossang
                        (abs (- (* (nth 1 a) (nth 5 support))
                                (* (nth 2 a) (nth 4 support)))))
                      ;; Require a genuine L/T-style direction change and
                      ;; direct support from an already accepted wall hatch.
                      (if (and (> crossang 0.50)
                               (hw:poly-near-rect-p poly support 0.025))
                        (setq ok T))))))
              (if (and ok
                       (not (vl-some
                         '(lambda (c) (< (distance c ctr) 0.01)) centers))
                       (not (vl-some
                         '(lambda (r) (hw:in-quad ctr (hw:rect->poly r))) rects)))
                (setq centers (cons ctr centers)
                      patch-list (cons cand patch-list)))))))))
  patch-list
)

;; Fill an uncovered cell where two accepted wall strips meet at a right angle.
;; The calculation uses projected strip limits, avoiding polygon clipping.
(defun hw:perpendicular-junction-patches
       (rects col-edges max-thick coverage
        / rest a rr b ux uy dot crossang tmin tmax omin omax p tx ox
          tpad opad lo hi ilo ihi patch ctr area2 centers result)
  (setq rest rects centers nil result nil)
  (while rest
    (setq a (car rest)
          rest (cdr rest)
          rr rest
          ux (nth 4 a)
          uy (nth 5 a))
    (while rr
      (setq b (car rr)
            rr (cdr rr)
            dot (+ (* ux (nth 4 b)) (* uy (nth 5 b)))
            crossang (abs (- (* ux (nth 5 b)) (* uy (nth 4 b)))))
      (if (> crossang 0.995)
        (progn
          (setq tmin nil tmax nil omin nil omax nil)
          (foreach p (hw:rect->poly b)
            (setq tx (+ (* ux (car p)) (* uy (cadr p)))
                  ox (+ (* (- uy) (car p)) (* ux (cadr p)))
                  tmin (if tmin (min tmin tx) tx)
                  tmax (if tmax (max tmax tx) tx)
                  omin (if omin (min omin ox) ox)
                  omax (if omax (max omax ox) ox)))
          ;; Extend only along each wall's own direction.
          (setq tpad (* max-thick (abs dot))
                opad (* max-thick crossang)
                lo (max (- (nth 8 a) max-thick) (- tmin tpad))
                hi (min (+ (nth 9 a) max-thick) (+ tmax tpad))
                ilo (max (nth 6 a) (- omin opad))
                ihi (min (nth 7 a) (+ omax opad)))
          (if (and (> (- hi lo) 1.0e-5)
                   (> (- ihi ilo) 1.0e-5))
            (progn
              (setq patch
                (list (append (hw:uv ux uy lo ilo) (list 0.0))
                      (append (hw:uv ux uy hi ilo) (list 0.0))
                      (append (hw:uv ux uy hi ihi) (list 0.0))
                      (append (hw:uv ux uy lo ihi) (list 0.0)))
                    ctr (hw:poly-center patch)
                    area2 (* 2.0 (- hi lo) (- ihi ilo)))
              (if (and (> area2 1.0e-8)
                       (<= area2 (+ (* 2.0 max-thick max-thick) 1.0e-5))
                       (not (hw:poly-enters-columns-p patch col-edges))
                       (not (hw:poly-covered-by-rects-p patch coverage))
                       (hw:poly-near-rect-p patch a 0.025)
                       (hw:poly-near-rect-p patch b 0.025)
                       (not (vl-some '(lambda (c) (< (distance c ctr) 0.01)) centers)))
                (setq centers (cons ctr centers)
                      result (cons patch result)))))))))
  result
)

;;; ---------------- hatch creation ----------------

(defun hw:ensure-layer (name col / rec color-entry new-color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   '(70 . 0)
                   (cons 62 col)
                   '(6 . "Continuous")))
  )
  ;; Keep WALL-HATCH at the requested color even if the layer already exists.
  ;; Preserve a negative color value, which means the layer is turned off.
  (if (setq rec (tblsearch "LAYER" name))
    (if (setq color-entry (assoc 62 rec))
      (progn
        (setq new-color (if (< (cdr color-entry) 0) (- col) col))
        (if (/= (cdr color-entry) new-color)
          (entmod (subst (cons 62 new-color) color-entry rec))
        )
      )
    )
  )
)

(defun hw:try-hatch (pts layer flag / hb el p)
  (setq hb (if (vl-some '(lambda (q) (> (abs (caddr q)) 1.0e-9)) pts) 1 0))
  (setq el (list '(0 . "HATCH")
                 '(100 . "AcDbEntity")
                 (cons 8 layer)
                 '(62 . 8)
                 '(100 . "AcDbHatch")
                 '(10 0.0 0.0 0.0)
                 '(210 0.0 0.0 1.0)
                 '(2 . "SOLID")
                 '(70 . 1)
                 '(71 . 0)
                 '(91 . 1)
                 (cons 92 flag)
                 (cons 72 hb)
                 '(73 . 1)
                 (cons 93 (length pts))))
  (foreach p pts
    (setq el (append el (list (list 10 (car p) (cadr p)))))
    (if (= hb 1)
      (setq el (append el (list (cons 42 (caddr p)))))
    )
  )
  (setq el (append el (list '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 0))))
  (entmake el)
)

(defun hw:make-hatch (pts layer / flag r)
  (foreach flag '(7 3 2)
    (if (not r)
      (setq r (hw:try-hatch pts layer flag))
    )
  )
  r
)

;;; ---------------- main command ----------------

(defun c:HWALL (/ *error* oldecho uopen v ss i en lay sg sg2 typ lines wall-lines col-edges wl
                  arcs rects rescue transition polys fills rest rr lim a b r pl sn cnt fail
                  *hw-lines*)
  (vl-load-com)

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if uopen (command "_.UNDO" "_END"))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nHWALL error: " msg))
    )
    (princ)
  )

  (setq oldecho (getvar "CMDECHO"))

  (initget 6)
  (setq v (getreal (strcat "\nMax wall thickness in meters <" (rtos *hw-max* 2 3) ">: ")))
  (if v (setq *hw-max* v))

  (cond
    ((<= *hw-max* *hw-min*)
     (princ "\nMax thickness must be greater than 0.05 m.")
    )
    (t
     (prompt "\nSelect the drawing (only layers containing WALL are used): ")
     (setq ss (ssget '((0 . "LINE,LWPOLYLINE,ARC,MLINE"))))
     (if (not ss)
       (princ "\nNothing selected.")
       (progn
         (setvar "CMDECHO" 0)
         (command "_.UNDO" "_BEGIN")
         (setq uopen T)

         ;; Collect wall geometry and COLUMN boundary edges independently.
         ;; COLUMN-HATCH layers are ignored by hw:column-layer-p.
         (setq i 0)
         (repeat (sslength ss)
           (setq en  (ssname ss i)
                 i   (1+ i)
                 lay (cdr (assoc 8 (entget en)))
                 typ (cdr (assoc 0 (entget en)))
           )
           (if (and (hw:column-layer-p lay)
                    (member typ '("LINE" "LWPOLYLINE")))
             (foreach sg (hw:ent-segs en)
               (if (eq (car sg) 'L)
                 (setq col-edges (cons (list (cadr sg) (caddr sg)) col-edges)))))
           (if (hw:wall-layer-p lay)
             (foreach sg (hw:ent-segs en)
               (if (eq (car sg) 'L)
                 (setq wall-lines (cons (list (cadr sg) (caddr sg)) wall-lines))
                 (setq arcs (cons (cdr sg) arcs)))))
         )

         ;; Split straight wall lines at column boundaries; keep only outside pieces.
         (foreach wl wall-lines
           (foreach sg2 (hw:trim-wall-seg (car wl) (cadr wl) col-edges)
             (setq lines (cons (hw:make-line-core (car sg2) (cadr sg2)
                                                   (caddr sg2) (cadddr sg2)) lines))))

         ;; straight walls: sweep on min X to limit the pair tests
         (setq lines (vl-sort lines '(lambda (p q) (< (nth 4 p) (nth 4 q))))
               *hw-lines* lines
         )
         (setq rest lines)
         (while rest
           (setq a    (car rest)
                 rest (cdr rest)
                 rr   rest
                 lim  (+ (nth 6 a) *hw-max* 1.0e-3)
           )
           (while (and rr (<= (nth 4 (car rr)) lim))
             (setq b  (car rr)
                   rr (cdr rr)
             )
             (if (and (<= (nth 5 a) (+ (nth 7 b) *hw-max* 1.0e-3))
                      (>= (nth 7 a) (- (nth 5 b) *hw-max* 1.0e-3)))
               (if (setq r (hw:line-pair a b))
                 (setq rects (cons r rects))
               )
             )
           )
         )

         ;; Conservative recovery of tiny residual L/T junction pieces.
         ;; This pass does not alter the normal wall-pair logic.
         (setq rescue (hw:residual-junction-patches lines rects col-edges *hw-max*))

         ;; Recover valid portions of wall pairs whose thickness changes along
         ;; their overlap and therefore failed the original whole-pair test.
         (setq transition (hw:transition-patches lines rects col-edges *hw-max*))

         ;; curved walls
         (setq rest arcs)
         (while rest
           (setq a    (car rest)
                 rest (cdr rest)
           )
           (foreach b rest
             (foreach pl (hw:arc-pair a b)
               (setq polys (cons pl polys))
             )
           )
         )

         ;; boundaries for straight walls + junction fills
         (foreach r rects
           (setq polys (cons (hw:rect->poly r) polys))
         )
         ;; Only the small, supported residual L/T patches from the fallback.
         (foreach r rescue
           (setq polys (cons (hw:rect->poly r) polys))
         )
         ;; Add the valid-thickness subspans missed by the original full-pair test.
         ;; Skip a subspan already covered by a normal or rescue rectangle.
         (foreach r transition
           (setq pl (hw:rect->poly r)
                 sg (hw:poly-center pl))
           (if (and sg
                    (not (hw:poly-covered-by-rects-p pl (append rects rescue)))
                    (not (hw:poly-enters-columns-p pl col-edges)))
             (setq polys (cons pl polys))
           )
         )
         (setq fills (hw:gap-fill rects lines))
         (setq fills
           (append
             (hw:perpendicular-junction-patches
               rects col-edges *hw-max* (append rects rescue transition))
             fills))
         (foreach pl fills
           (setq polys (cons pl polys))
         )

         ;; create hatches
         (setq sn (ssadd) cnt 0 fail 0)
         (if polys
           (progn
             (hw:ensure-layer "WALL-HATCH" 8)
             (foreach pl polys
               (if (hw:make-hatch pl "WALL-HATCH")
                 (progn
                   (ssadd (entlast) sn)
                   (setq cnt (1+ cnt))
                 )
                 (setq fail (1+ fail))
               )
             )
             (if (> cnt 0)
               (command "_.DRAWORDER" sn "" "_B")
             )
           )
         )

         (command "_.UNDO" "_END")
         (setq uopen nil)

         (cond
           ((null polys)
            (princ "\nHWALL: no wall pairs found. Check the layer name (must contain WALL) and the max thickness.")
           )
           (t
            (princ (strcat "\nHWALL: " (itoa cnt) " hatch pieces created on layer WALL-HATCH"
                           " (" (itoa (length fills)) " junction fills)."
                           (if (> fail 0)
                             (strcat " Failed: " (itoa fail) ".")
                             "")))
           )
         )
       )
     )
    )
  )

  (if oldecho (setvar "CMDECHO" oldecho))
  (princ)
)

(princ "\nHWALL loaded. Type HWALL to start.")
(princ)
