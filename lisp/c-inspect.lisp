(defun c-inspect (&key include type value)
  (flet ((gen-source ()
           (with-output-to-string (out)
             (format out "~&#include <iostream>~%")
             (dolist (inc include)
               (format out "~&#include <~a>~%" inc))

             (format out "~%int main() {~%")
             (dolist (ty type)
               (format out "  std::cout << \"sizeof(~a) = \" << sizeof(~a) << std::endl;~%" ty ty))

             (dolist (val value)
               (format out "  std::cout << \"~a = \" << ~a << std::endl;~%" val val))

             (format out "}~%"))))
    ;; 定数や型の情報を出力するためのC++ソースを生成
    (with-input-from-string (in (gen-source))
      ;; 生成したソースをコンパイル
      (let ((ret (sb-ext:run-program "g++" `("-x" "c++" "-" "-o" "/tmp/c.inspect.tmp")
                                     :search t :input in :output *standard-output*)))
        ;; コンパイルに成功しているなら、コマンドを実行
        (when (zerop (sb-ext:process-exit-code ret))
          (sb-ext:run-program "/tmp/c.inspect.tmp" '() :output *standard-output*)))))
  (values))

