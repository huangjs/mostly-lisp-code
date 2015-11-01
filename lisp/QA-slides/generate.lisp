(defpackage :slide
    (:use :cl :iterate))

(in-package :slide)

(defparameter *begin*
  "\\documentclass[11pt]{beamer}
\\beamersetaveragebackground{green!80!gray}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{latexsym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\tolerance=1000
\\usepackage{amsmath}
\\usepackage{wasysym}
\\usepackage{color}
\\usepackage{xcolor}
\\usepackage{upquote}
\\usepackage{listings}
\\usepackage{tikz}
\\usepackage{fancyvrb}
\\usepackage{fontspec}
\\usepackage{xunicode}
\\usepackage{xltxtra}
\\usepackage{xeCJK}

\\title{First Aid 2010 (Pharm)}
\\author{huangjs}

\\begin{document}

")

(defparameter *end*
  "
\\end{document}
")

(defun string-trim-whitespace (string)
  (string-trim " 　	" string))

(defun cleanup-lists (lists)
  (let (one-line
        one-list
        result)
    (iter (for list in lists)
          (setf one-line '())
          (setf one-list '())
          (push (first list) one-list)
          (let (left right)
            (iter (for (l . r) in (rest list))
                  (if (not (zerop (length (string-trim-whitespace l))))
                      (progn
                        (when (or left right)
                          (setf one-line (cons left right))
                          (push one-line one-list))
                        (setf left (clean-utf8-string (or l "")))
                        (setf right (mapcar (lambda (s) (clean-utf8-string s))
                                            (or r '("")))))
                      (progn
                        (setf right
                              (iter (for r1 in right)
                                    (for i from 0)
                                    (for r2 = (nth i r))
                                    (collect
                                        (concatenate 'string (or r1 "") " \\\\ " (clean-utf8-string (or r2 "")))))))))) 
          (push (nreverse one-list) result))
    (when one-list
      (push one-list result))
    (nreverse result)))


(defun generate-from-lists (lists &optional stream) 
  (let ((pagecount 0))
    (labels ((do-all (lists)
               (with-output-to-string (s)
                 (unless stream
                   (setf stream s))
                 (format stream "~a~%" *begin*)
                 (loop for list in lists
                       for header = (first list)
                       for lh = (first header)
                       for rh = (format nil "~{~A~^/~}" (rest header))
                       for body = (rest list)
                       do
                    (do-1 lh rh body))
                 (format stream "~a~%" *end*)))
             (do-1 (left-header right-header body)
               (iter (for (left . right) in body)
                     (format stream "%% page: ~a~%" (incf pagecount))
                     (format stream "\\begin{frame}~% \\frametitle{~a}~%" (or left-header ""))
                     (format stream "~a~%\\end{frame}~%~%" left)
                     (format stream "%% page: ~a~%" (incf pagecount))
                     (format stream "\\begin{frame}~% \\frametitle{~a}~%" (or right-header ""))
                     (when right
                       (format stream "\\begin{itemize}~%")
                       (loop for r in right
                             do
                          (format stream "\\item{~a}~%" r))
                       (format stream "\\end{itemize}~%"))
                     (format stream "\\end{frame}~%~%"))))
      (do-all (cleanup-lists lists)))))



(defun clean-utf8-string (string)
  (with-output-to-string (s)
    (iter (for c in-string string)
          (format s "~a"
                  (case c
                    (#\% "\\%")
                    (#\# "\\#")
                    (#\$ "\\$")
                    (#\& "\\&")
                    (#\− #\-)
                    (#\‐ #\-)
                    (#\× "${\\times}$")
                    (#\⁄ #\/)
                    (#\± "${\\pm}$")
                    (#\< "$<$")
                    (#\> "$>$")
                    (#\≥ "${\\geq}$")
                    (#\≅ "${\\cong}$")
                    (#\≈ "${\\simeq}$")
                    (#\α "${\\alpha}$")
                    (#\β "${\\beta}$")
                    (#\γ "${\\gamma}$")
                    (#\χ "${\\chi}$")
                    (#\δ "${\\delta}$")
                    (#\π "${\\pi}$")
                    (#\η "${\\eta}$")
                    (#\σ "${\\sigma}$")
                    (#\↓ "${\\downarrow}$")
                    (#\↑ "${\\uparrow}$")
                    (#\→ "${\\rightarrow}$")
                    (#\↔ "${\\leftrightarrow}$")
                    (#\° "$^{\\circ}$")
                    (#\⊥ "${\\perp}$")
                    (#\Χ #\X)
                    (#\¼ "$1/4$")
                    (#\∞ "${\\infty}$")
                    ((#\ #\) "${\\Box}$")
                    (#\∆ "${\\triangle}$")
                    (#\√ "${\\surd}$")
                    (otherwise c))))))


#|

(let ((csvs '("combined.csv"))
      (tex "combined.tex"))
  (setf lists (loop for f in csvs collect (fare-csv:read-csv-file f)))
  (with-open-file (f tex :direction :output :if-exists :supersede) (generate-from-lists lists f)))

|#

