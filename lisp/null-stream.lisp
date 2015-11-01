(defclass null-character-output-stream (sb-gray:fundamental-character-output-stream)
  ())

(defmethod sb-gray:stream-line-column ((stream null-character-output-stream))
  nil)

(defmethod sb-gray:stream-write-char ((stream null-character-output-stream) character)
  )

(defmethod sb-gray:stream-write-string ((stream null-character-output-stream) string &optional start end)
  (declare (ignorable start end))
  )


#| TEST

CL-USER> (defparameter *null-character-output-stream* (make-instance 'null-character-output-stream))
*NULL-CHARACTER-OUTPUT-STREAM*
CL-USER> (gc :full t)
NIL
CL-USER> (room)
Dynamic space usage is:   61,178,656 bytes.
Read-only space usage is:      6,352 bytes.
Static space usage is:         4,064 bytes.
Control stack usage is:        9,360 bytes.
Binding stack usage is:        1,040 bytes.
Control and binding stack usage is for the current thread only.
Garbage collection is currently enabled.

Breakdown for dynamic space:
16,299,856 bytes for    15,754 code objects.
12,409,472 bytes for   142,826 instance objects.
10,770,960 bytes for   673,185 cons objects.
9,744,928 bytes for    85,783 simple-vector objects.
11,985,968 bytes for   200,021 other objects.
61,211,184 bytes for 1,117,569 dynamic objects (space total.)
; No value
CL-USER> (time (loop repeat 100000000
                     with s = (make-string 1000 :initial-element #\a)
                     do (write-line s *null-character-output-stream*)))
Evaluation took:
  13.227 seconds of real time
  13.200000 seconds of total run time (13.160000 user, 0.040000 system)
  99.80% CPU
  39,682,330,245 processor cycles
  72,912 bytes consed
  
NIL
CL-USER> (room)
Dynamic space usage is:   63,073,792 bytes.
Read-only space usage is:      6,352 bytes.
Static space usage is:         4,064 bytes.
Control stack usage is:        9,360 bytes.
Binding stack usage is:        1,040 bytes.
Control and binding stack usage is for the current thread only.
Garbage collection is currently enabled.

Breakdown for dynamic space:
  16,300,464 bytes for    15,756 code objects.
  12,924,752 bytes for   148,834 instance objects.
  11,182,512 bytes for   698,907 cons objects.
  10,065,472 bytes for    91,537 simple-vector objects.
  12,600,592 bytes for   219,478 other objects.
  63,073,792 bytes for 1,174,512 dynamic objects (space total.)
; No value


|#

