#|
exec sbcl --noinform --no-userinit --load $0 --end-toplevel-options "$@"
|#
(format t "hello, world!~%")
(quit)
