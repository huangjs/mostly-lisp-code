(in-package :pf-function)

(defun foo ()
  (labels ((write_mbstring (obj height str)
	     (let ((str_len (getstringwidth obj str)))
	       (setx obj (- 100 str_len))
	       (php-write obj height str))))
    (let ((foo (mbfpdf)))
      (setf bar foo)
      (addmbfont foo "mincho")
      (php-open foo)
      (addpage foo)
      (rect foo 130 10 70 30)
      (rect foo 10 10 115 30)

      ;; test line
      (line foo 10 40 210 297)

      (setx foo 140)
      (setfont foo "gothic" "" 12)
      (php-write foo 15 "株式会社IMJネットワークB")
      (setx foo 140)
      (setfont foo "gothic" "" 10)
      (php-write foo 25 "〒141-0031")
      (setx foo 140)
      (php-write foo 35 "東京都品川区西五反田7-1-1B")
      (setx foo 140)
      (php-write foo 45 "住友五反田ビル 3FB")

      (setdrawcolor foo 255 0 0)
      (setlinewidth foo 0.3)
      (line foo 10 50 200 50)
      
      (setdrawcolor foo 0 0 255)
      (setlinewidth foo 0.05)
      (line foo 100 50 100 150)
      
      (setxy foo 100 60)
      (setx foo (- 100 10))

      (write_mbstring foo 0 "abc")
      (write_mbstring foo 10 "123")
      (write_mbstring foo 20 "_@%$#")

      (setfont foo "gothic" "U" 20)
      (write_mbstring foo 40 "あいう123abc(GOTHIC)")
      (setfont foo "mincho" "U" 20)
      (write_mbstring foo 60 "あいう123abc(MINCHO)")

      (image foo "/home/hjs/shain.jpg" 160 140 23 22)

      (setxy foo 140 170)
      (setfont foo "gothic" "" 10)
      (php-write foo 10 "By 数理システム" "http://www.msi.co.jp/")
      
      (image foo "/home/hjs/logo_msi.png" 150 180 54 20 "png" "http://www.msi.co.jp/")

      ;; new page
      (addpage foo "landscape")
      (rect foo 130 10 70 30)
      (rect foo 10 10 115 30)

      (line foo 10 40 210 297)

      (setx foo 140)
      (setfont foo "gothic" "" 12)
      (php-write foo 15 "株式会社IMJネットワークB")
      (setx foo 140)
      (setfont foo "gothic" "" 10)
      (php-write foo 25 "〒141-0031")
      (setx foo 140)
      (php-write foo 35 "東京都品川区西五反田7-1-1B")
      (setx foo 140)
      (php-write foo 45 "住友五反田ビル 3FB")

      (setdrawcolor foo 255 0 0)
      (setlinewidth foo 0.3)
      (line foo 10 50 200 50)
      
      (setdrawcolor foo 0 0 255)
      (setlinewidth foo 0.05)
      (line foo 100 50 100 150)
      
      (setxy foo 100 60)
      (setx foo (- 100 10))

      (write_mbstring foo 0 "abc")
      (write_mbstring foo 10 "123")
      (write_mbstring foo 20 "_@%$#")

      (setfont foo "gothic" "U" 20)
      (write_mbstring foo 40 "あいう123abc(GOTHIC)")
      (setfont foo "mincho" "U" 20)
      (write_mbstring foo 60 "あいう123abc(MINCHO)")

      (image foo "/home/hjs/shain.jpg" 160 140 23 22)

      (setxy foo 220 170)
      (setfont foo "gothic" "" 10)
      (php-write foo 10 "By 数理システム")
      
      (image foo "/home/hjs/logo_msi.png" 220 180 54 20)

      (output foo "mbfpdf-sample.pdf" "F")))
  )

(progn
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:basic-rect 130 10 70 30)
      (pdf:stroke)
      (pdf:basic-rect 10 10 115 30)
      (pdf:stroke)
      (let ((image (pdf::make-image "/Users/huangjianshi/tmp/logo_msi.png")))
	(pdf:add-images-to-page image)
	(pdf:draw-image image 200 500 270 100 0 t)
	(pdf:add-uri-link 200 500 270 100 "http://www.msi.co.jp/")))
    (pdf:write-document "/Users/huangjianshi/tmp/mbfpdf-sample2.pdf")))

(defun bar ()
  (labels ((write_mbstring (obj height str)
	     (let ((str_len (getstringwidth obj str)))
	       (setx obj (- 100 str_len))
	       (php-write obj height str))))
    (let ((foo (mbfpdf)))
      (setf bar foo)
      (addmbfont foo "mincho")
      (php-open foo)
      (addpage foo)
      (rect foo 130 10 70 30)

      ;; test line
      (line foo 10 40 210 297)

      (setdrawcolor foo 255 0 0)
      (setlinewidth foo 0.3)
      (line foo 10 50 200 50)
      
      (setdrawcolor foo 0 0 255)
      (setlinewidth foo 0.05)
      (line foo 100 50 100 150)
      
      (setxy foo 100 60)
      (setx foo (- 100 10))

      (setfont foo "gothic" "U" 20)
      (write_mbstring foo 40 "あいう123abc(GOTHIC)")

      (image foo "/home/hjs/shain.jpg" 160 140 23 22)
    
      (output foo "mbfpdf-sample.pdf" "F")))
  )

