(defun c:CHECKDIMLFACAUTOCAD ( / ss i ent obj lf badDims cnt msg ssSel)

  ;; Load Visual LISP / COM (required for vlax / vla functions)
  ;; Works in both AutoCAD and DraftSight
  (vl-load-com)

  ;; Initialize counters and storage
  (setq cnt 0)
  (setq badDims '())

  ;; Select all DIMENSION entities in Model Space
  ;; (410 . "Model") works in DraftSight and AutoCAD
  (setq ss
    (ssget "X"
      '(
        (0 . "DIMENSION")
        (410 . "Model")
       )
    )
  )

  (if ss
    (progn
      (setq i 0)

      ;; Loop through all dimensions
      (repeat (sslength ss)

        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; Read linear scale factor safely
        ;; DraftSight and AutoCAD may expose different property names
        (setq lf
          (cond
            ;; DraftSight / some versions
            ((vlax-property-available-p obj 'LinearScaleFactor)
             (vlax-get obj 'LinearScaleFactor))

            ;; Alternative DraftSight naming
            ((vlax-property-available-p obj 'DimScaleLinear)
             (vlax-get obj 'DimScaleLinear))

            ;; AutoCAD standard property
            ((vlax-property-available-p obj 'Dimlfac)
             (vlax-get obj 'Dimlfac))

            ;; Fallback if property is unavailable
            (T 1.0)
          )
        )

        ;; Check for non-1:1 scale
        (if (/= lf 1.0)
          (progn
            (setq badDims (cons ent badDims))
            (setq cnt (1+ cnt))
          )
        )

        (setq i (1+ i))
      )

      ;; Build result message
      (if (> cnt 0)
        (setq msg (strcat (itoa cnt) " dimension(s) are not scaled 1:1."))
        (setq msg "All dimensions in Model Space are scaled 1:1.")
      )

      ;; Display result
      (alert msg)

      ;; Highlight non-compliant dimensions
      (if badDims
        (progn
          (setq ssSel (ssadd))
          (foreach e badDims (ssadd e ssSel))
          (sssetfirst nil ssSel)
        )
      )
    )
    (alert "No dimensions found in Model Space.")
  )

  (princ)
)
