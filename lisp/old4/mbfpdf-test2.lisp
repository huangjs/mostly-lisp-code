;;; from pol compiler
(in-package :pf-function)

(setq *env* (create-env))

(DEFUN foo ()
  (HANDLER-CASE
      (BLOCK GSCOPE
	(LET ((CURR-FILE
	       "mbfpdf-sample.php"))
	  (ADD-NEWREQUIRED CURR-FILE)
	  (SETF (ENV-CURRENT-PHP *ENV*) CURR-FILE)
	  (ADD-NEW-FUNC
	   (LIST
	    (LIST 'WRITE_MBSTRING
		  #'(LAMBDA (|$hight| |$str|)
		      (BLOCK LSCOPE
			(LET* ((NEWENV (CREATE-NEWENV))
			       (*ENV* NEWENV)
			       (NEWTABLE (LOCAL-TABLE *ENV*)))
			  (ADD-NEWVAR '("hight" "str") (LIST |$hight| |$str|)
				      NEWTABLE)
			  (PROGN (GLOBAL-DECLARETION ($ "mbfpdf"))
				 (SETF ($ "str_len")
				       (GETSTRINGWIDTH ($ "mbfpdf") ($ "str")))
				 (SETX ($ "mbfpdf") (PHP- 100 ($ "str_len")))
				 (PHP-WRITE ($ "mbfpdf") ($ "hight")
					    ($ "str")))))))))
	  (PROGN ;; POL では、mbfpdf.php は読み込まない。
	    ;; (REQUIRE-PF-FUNCTION "./mbfpdf.php")

	    ;;  MBFPDFインスタンスを作成する
	    (SETF ($ "mbfpdf") (MBFPDF))

	    ;; フォントを追加する
	    (ADDMBFONT ($ "mbfpdf") "GOTHIC")
	    (ADDMBFONT ($ "mbfpdf") "MINCHO")

	    ;; ドキュメントを生成
	    (PHP-OPEN ($ "mbfpdf"))

	    ;; ページを作成する
	    (ADDPAGE ($ "mbfpdf"))

	    ;; 矩形を描画する(x, y, width, height)
	    (RECT ($ "mbfpdf") 130 10 70 30)
	    (RECT ($ "mbfpdf") 10 10 115 30)

	    ;; 現在のX座標を変更する
	    (SETX ($ "mbfpdf") 140)
	    ;; フォントを設定する
	    (SETFONT ($ "mbfpdf") "GOTHIC" "" 12)
	    ;; 文字列を描く
	    (PHP-WRITE ($ "mbfpdf") 15 (PHP. "株式会社IMJネットワークByy"))
	    (SETX ($ "mbfpdf") 140)
	    (SETFONT ($ "mbfpdf") "GOTHIC" "" 10)
	    (PHP-WRITE ($ "mbfpdf") 25 (PHP. "〒141-0031"))
	    (SETX ($ "mbfpdf") 140)
	    (PHP-WRITE ($ "mbfpdf") 35 (PHP. "東京都品川区西五反田7-1-1B"))
	    (SETX ($ "mbfpdf") 140)
	    (PHP-WRITE ($ "mbfpdf") 45 (PHP. "住友五反田ビル 3FB"))

	    ;; 赤い横線を描く
	    (SETDRAWCOLOR ($ "mbfpdf") 255 0 0)
	    (SETLINEWIDTH ($ "mbfpdf") 0.3)
	    (LINE ($ "mbfpdf") 10 50 200 50)

	    ;; 青い縦線を描く
	    (SETDRAWCOLOR ($ "mbfpdf") 0 0 255)
	    (SETLINEWIDTH ($ "mbfpdf") 0.05)
	    (LINE ($ "mbfpdf") 100 50 100 150)

	    ;; 末尾をそろえて文字列を描く
	    (SETXY ($ "mbfpdf") 100 60)
	    (SETX ($ "mbfpdf") (PHP- 100 10))

	    (PHPFUNCALL WRITE_MBSTRING 0 (PHP. "abc"))
	    (PHPFUNCALL WRITE_MBSTRING 10 (PHP. "123"))
	    (PHPFUNCALL WRITE_MBSTRING 20 (PHP. "_@%$#"))

	    (SETFONT ($ "mbfpdf")
		     "GOTHIC" "U" 20)
	    (PHPFUNCALL WRITE_MBSTRING 40 (PHP. "あいう123abc(GOTHIC)"))
	    (SETFONT ($ "mbfpdf")
		     "MINCHO" "U" 20)
	    (PHPFUNCALL WRITE_MBSTRING 60 (PHP. "あいう123abc(MINCHO)"))

	    ;; JPEGファイルを挿入する(x, y, width, height)
	    (IMAGE ($ "mbfpdf") "./image/shain.jpg" 160 140 23 22)

	    ;; PDFをファイルに出力する
	    ;; 売上くんでは、"I"が指定される。
	    (OUTPUT ($ "mbfpdf") (PHP. "mbfpdf-sample.pdf") (PHP. "F")))))
    (ERROR (C) (PROGN (FORMAT (ENV-OUTPUT-STREAM *ENV*) "~A~%" C) (THROW 'EXIT NIL)))))

(|PF-_pol_uriagekun_uri_mbfpdf-sample.php|)
