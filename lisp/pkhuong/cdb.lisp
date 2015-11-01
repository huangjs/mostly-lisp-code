;;; TODO: Linear probing must wrap around in each table!
;;;
;;; N.B. Zach Beane's ZCDB, <https://github.com/xach/zcdb>, is better
;;; tested and maintained.
;;;
;;;
;;; cdb reader
;;;
;;; cdb is a `constant database' format designed by D. J. Bernstein.
;;; It is fully described at <http://cr.yp.to/cdb.html>. There, you will
;;; also find tools to build, read and dump files in that format.
;;;
;;; This package *only* provides reading functionality, and is only
;;; expected to work in SBCL running on Unixoids in little-endian mode.
;;; Basic support for searching and iterating through the matches is
;;; provided. No writing functionality is implemented. Note that the
;;; package is meant to used with explicit package qualifiers, without
;;; USEing it in another package.
;;;
;;; Care was taken to avoid consing too many temporary vectors.
;;;
;;; The code is inherently non-thread-safe. Sharing a file descriptor
;;; makes that nearly impossible already. To cope with that problem, one
;;; could either manually manage locks around the cdbs and iterators, or
;;; work with different copies in each thread (each cdb object takes
;;; slightly more than 4KB of memory and each iterator slightly more than
;;; 2KB, in both cases nearly all unboxed data).

;;; INTERFACE
;;;
;;; Search iterators are represented as values of type ITERATOR.
;;; cdb files are represented as values of type CDB, which may also
;;; be used as iterators (for the last search executed if successful).
;;; CDB and ITERATOR are however disjoint types.
;;;
;;; OPEN path &optional old-cdb => new-cdb
;;;  Opens a cdb at path (a string). If old-cdb is not nil then old-cdb
;;;  is closed, and reused for the new cdb.
;;;  To avoid FD leaks, a finalizer is associated with the CDB object.
;;;  However, it is, as always, preferable to explicitly close the CDB.
;;;
;;; CLOSE cdb-or-iterator
;;;  Closes the cdb or iterator (and any associated cdb or iterator).
;;;  It is a noop if the cdb or iterator has already been closed. A CLOSEd
;;;  cdb may be reused in CDB-OPEN.
;;;
;;; COPY source-cdb &optional old-cdb => new-cdb
;;;  Creates a new CDB that reads from the same path as source-cdb.
;;;  old-cdb is closed and reused if non-nil. If the CDB file has changed,
;;;  the copy will obviously not contain the same data. This provides a
;;;  simple way to synchronize to a modified cdb. An attempt is made to
;;;  preserve any iteration data. However, if the cdb has changed since
;;;  the creation of source-cdb, no guarantee is made to the results.
;;;
;;; GET key cdb &key external-format => value, foundp
;;;  Searches an open cdb for the string key, encoded as per external-format.
;;;  Returns the value (decoded as per external-format) and t if an entry
;;;  was found, nil and nil otherwise. As a side-effect, keeps a reference
;;;  to the key on successful lookups. This makes it possible to iterate
;;;  through the hits with NEXT. It is an error to use GET on a cdb that
;;;  has been CLOSEd.
;;;
;;; %GET key cdb => value, foundp
;;;  Searches an open cdb for the key, as a (simple-array (unsigned-byte 8) 1).
;;;  Returns the value (as a (simple-array (unsigned-byte 8) 1)) and t if an
;;;  entry was found, nil and nil otherwise. Also keeps a reference to the
;;;  key on successful lookups. It is an error to use %GET on a cdb that has
;;;  been CLOSEd.
;;;
;;; CLEAR cdb => cdb
;;;  Removes any reference to a key due to a successful lookup.
;;;
;;; MAKE-ITERATOR cdb-or-iterator &optional old-iterator => iterator
;;;  Returns a new iterator for the current key and position of the argument
;;;  cdb or iterator. old-iterator may be reused if non-nil.
;;;
;;; COPY-ITERATOR cdb-or-iterator &optional old-iterator => iterator
;;;  Returns a new iterator for the current key and position of the argument
;;;  cdb or iterator, but reopening the cdb file. Told-iterator may be
;;;  reused if non-nil. his new copy is safe to use across threads. However,
;;;  no guarantee is offered if the underlying file has changed.
;;;
;;; NEXT cdb-or-iterator &key external-format => value, foundp
;;;  Searches for the next entry with the key from the last GET or %GET
;;;  executed in cdb or iterator. If there is such an entry, returns the
;;;  value, decoded as per external-format and t, nil and nil otherwise.
;;;  It is an error to use NEXT on a cdb or iterator that has been CLOSEd,
;;;
;;; %NEXT cdb-or-iterator => value, foundp
;;;  Searches for the next entry with the key from the last GET or %GET
;;;  executed in cdb or iterator. If there is such an entry, returns the
;;;  value, as a (simple-array (unsigned-byte 8) 1), and t, nil and nil
;;;  otherwise. It is similarly an error to use %NEXT on CLOSEd cdb or
;;;  iterators.

;;; NEWS
;;;
;;; 2008-05-06
;;;  Initial release. Not tested, except for trivial reads from a correct
;;;  cdb.
;;;
;;; 2008-05-07
;;;  Added basic support for iteration. Changed to a package-ful interface.

;;; License: Modified BSD
;;; License Copyright (c) 2008, Paul-Virak Khuong
;;;  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; Neither the name of the Million Monkey Enterprises nor the names of
;;; its contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(cl:defpackage "CDB"
  (:use "CL")
  (:export "CDB" "CDB-P" "ITERATOR" "ITERATOR-P"
           "OPEN" "CLOSE" "COPY"
           "GET" "%GET"
           "CLEAR"
           "MAKE-ITERATOR" "COPY-ITERATOR"
           "NEXT" "%NEXT")
  (:shadow "OPEN" "CLOSE" "GET"))

(cl:in-package "CDB")

(declaim (type (integer 1 (#.(cl:min cl:array-dimension-limit
                                     (1+ (ash 1 32)))))
               *initial-block-size* *maximal-block-size*))
(defvar *initial-block-size* 8
  "Initial size of views. Each iterator or cdb takes 2 (unsigned-byte 32)
   per size unit.")
(defvar *maximal-block-size* 256
  "Maximal size of views. Block sizes are doubled up to *maximal-block-size*
   when a query overflows the current block size.")

;; Contains all the data to search sequentially through the entries
;; in a cdb.
;; %iterator instead of iterator so that iterator and cdb are disjoint
;; types.
(defstruct (%iterator (:copier nil)
                      (:constructor nil)
                      (:conc-name iterator-)
                      (:predicate nil))
  ;; nil, (CONS nil nil) or (CONS FD nil). If the latter, FD is the
  ;; file descriptor for the cdb. Otherwise, the iterator is closed.
  (fd-box  (error "Must supply fd-box")
               :type (or null cons))
  ;; path to the cdb.
  (path    (error "Must supply path")
               :type string)
  ;; Last key if an entry was found, nil otherwise (no entry found,
  ;; or all entries iterated through). If nil, all the following
  ;; slots are meaningless.
  (key     nil :type (or null (simple-array (unsigned-byte 8) 1)))
  ;; Hash value of the key.
  (hash      0 :type (unsigned-byte 32))
  ;; Block, temporary view of 256 entries in a hash table
  ;; (may be used differently transiently). If key is not nil,
  ;; this is the part of the table in which the entry was found.
  (block   (make-array *initial-block-size* :element-type '(unsigned-byte 32))
               :type (simple-array (unsigned-byte 32) 1))
  ;; Byte offset in the file at which block begins.
  (offset    0 :type (unsigned-byte 32))
  ;; Remaining number of entries in the table, including those
  ;; in block.
  (remaining 0 :type (unsigned-byte 32))
  ;; Index (in the block) of the last entry.
  (last-hit  0 :type (unsigned-byte 32)))

(defstruct (iterator (:include %iterator)
                     (:copier nil)
                     (:constructor %make-iterator)
                     (:print-object print-iterator)))
(defun print-iterator (iterator stream)
  (declare (type iterator iterator))
  (print-unreadable-object (iterator stream :type t :identity t)
    (format stream "~S (key: ~S)"
            (iterator-path iterator)
            (iterator-key iterator))))

;; This is inherently *not* thread-safe.
;; A cdb is simply an iterator with a TOC of the hash-tables
;; attached.
(defstruct (cdb (:include %iterator)
                (:copier nil)
                (:print-object print-cdb)
                (:conc-name %cdb-)
                (:predicate cdb-p))
  (toc      (make-array 512 :element-type '(unsigned-byte 32))
            :type (simple-array (unsigned-byte 32) (512))
            :read-only t))
(defun print-cdb (cdb stream)
  (declare (type cdb cdb))
  (print-unreadable-object (cdb stream :type t :identity t)
    (format stream "~S (fd: ~A)" (%cdb-path cdb) (car (%cdb-fd-box cdb)))))

(declaim (inline cdb-table-offset cdb-table-length cdb-table-info))
(defun cdb-table-offset (cdb table-id)
  (declare (type cdb cdb)
           (type (mod 256) table-id))
  (aref (%cdb-toc cdb) (* 2 table-id)))
(defun cdb-table-length (cdb table-id)
  (declare (type cdb cdb)
           (type (mod 256) table-id))
  (aref (%cdb-toc cdb) (1+ (* 2 table-id))))
(defun cdb-table-info (cdb table-id)
  (declare (type cdb cdb)
           (type (mod 256) table-id))
  (let ((toc   (%cdb-toc cdb))
        (index (* 2 table-id)))
    (values (aref toc index) (aref toc (1+ index)))))

(declaim (sb-ext:maybe-inline read-in-ub32 read-in-ub8))
(defun read-in-ub32 (fd offset nwords
                     &optional
                     (buffer (make-array nwords
                                         :element-type '(unsigned-byte 32))))
  (declare (type (or null (unsigned-byte 32)) offset)
           (type (unsigned-byte 32) nwords)
           (type (simple-array (unsigned-byte 32) 1) buffer)
           (inline sb-unix:unix-read) ; avoid boxing SAPs
           ;; offset is almost always of constant nillity.
           (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (when offset
    (sb-unix:unix-lseek fd offset sb-unix:l_set))
  (sb-sys:with-pinned-objects (buffer)
    (let* ((to-read (* 4 nwords))
           (read    (sb-unix:unix-read fd (sb-sys:vector-sap buffer)
                                       (* 4 nwords))))
      (unless read
        (error "unix-read of FD ~A failed." fd))
      (unless (= read to-read)
        (error "not enough byte unix-read from FD ~A. Expected ~A, read ~A."
               fd to-read read))))
  buffer)

(defun read-in-ub8 (fd offset nbytes
                    &optional
                    (buffer (make-array nbytes
                                        :element-type '(unsigned-byte 8))))
  (declare (type (or null (unsigned-byte 32)) offset)
           (type (unsigned-byte 32) nbytes)
           (type (simple-array (unsigned-byte 8) 1) buffer)
           (inline sb-unix:unix-read)
           (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (when offset
    (sb-unix:unix-lseek fd offset sb-unix:l_set))
  (sb-sys:with-pinned-objects (buffer)
    (let ((read (sb-unix:unix-read fd (sb-sys:vector-sap buffer)
                                   nbytes)))
      (unless read
        (error "unix-read of FD ~A failed." fd))
      (unless (= read nbytes)
        (error "not enough byte unix-read from FD ~A. Expected ~A, read ~A."
               fd nbytes read))))
  buffer)

(defun close (iterator)
  (declare (type %iterator iterator))
  (when (car (iterator-fd-box iterator))
    (sb-ext:cancel-finalization (iterator-fd-box iterator))
    (sb-unix:unix-close (car (iterator-fd-box iterator)))
    (setf (car (iterator-fd-box iterator)) nil
          (iterator-key     iterator)      nil))
  ;; null the fd-box out unconditionally to avoid holding on
  ;; to garbage.
  (setf (iterator-fd-box iterator) nil)
  (values))

(defun open-path (path)
  "Opens an fd to path, and a list containing that fd. The list
   is finalized to close the fd. Thus, every access to the fd
   should be done througn the list, or with the list kept alive."
  (let* ((fd     (or (sb-unix:unix-open path sb-unix:o_rdonly 0)
                     (error "Unable to open ~S" path)))
         (fd-box (list fd)))
    (sb-ext:finalize fd-box (lambda ()
                              (sb-unix:unix-close fd)))
    (values fd fd-box)))

(defun open (path &optional old-cdb)
  "Return a cdb to path, reusing old-cdb if non-nil."
  (declare (type string path)
           (type (or null cdb) old-cdb))
  (when old-cdb (close old-cdb))
  (multiple-value-bind (fd fd-box)
      (open-path path)
    (let* ((cdb (if old-cdb
                    (prog1 old-cdb
                      (setf (%cdb-fd-box old-cdb) fd-box
                            (%cdb-path   old-cdb) path))
                    (make-cdb :fd-box fd-box :path path)))) ; this is a bad magic number hack...
      (read-in-ub32 fd 0 512 (%cdb-toc cdb))
      cdb)))

(defun copy (source &optional old-cdb)
  "Open a cdb to the same path as source"
  (declare (type %iterator source)
           (type (or null cdb) old-cdb))
  (let ((key       (iterator-key source))
        (hash      (iterator-hash source))
        (offset    (iterator-offset source))
        (remaining (iterator-remaining source))
        (last-hit  (iterator-last-hit source))
        (new-cdb   (open (iterator-path source) old-cdb)))
    (when (iterator-key source)
      (setf (%cdb-key       new-cdb) key
            (%cdb-hash      new-cdb) hash
            (%cdb-offset    new-cdb) offset
            (%cdb-remaining new-cdb) remaining
            (%cdb-last-hit  new-cdb) last-hit)
      (unless (ignore-errors
                (read-in-ub32 (car (%cdb-fd-box new-cdb)) (%cdb-offset new-cdb)
                              (* 4 2 (min 256 (%cdb-remaining new-cdb)))
                              (%cdb-block new-cdb)))
        (setf (%cdb-key new-cdb) nil)))
    new-cdb))

(defun clear (cdb)
  (declare (type cdb cdb))
  (setf (%cdb-key cdb) nil)
  cdb)

;; fixme
(defun init-iterator-from-source (source old-iterator)
  (declare (type %iterator source)
           (type (or null iterator) old-iterator))
  (unless (car (iterator-fd-box source))
    (error "Source has been closed. Source: ~A" source))
  (unless (iterator-key source)
    (error "Source is not a valid (live) iterator key. Source: ~A" source))
  (if old-iterator
      (prog1 old-iterator
        (macrolet ((copy-slots (&rest slots)
                     `(setf ,@(mapcan (lambda (slot)
                                        (list `(,slot old-iterator)
                                              `(,slot source)))
                                      slots))))
          (copy-slots iterator-fd-box
                      iterator-path
                      iterator-key
                      iterator-hash
                      iterator-offset
                      iterator-remaining
                      iterator-last-hit)))
      (%make-iterator :fd-box    (iterator-fd-box    source)
                      :path      (iterator-path      source)
                      :key       (iterator-key       source)
                      :hash      (iterator-hash      source)
                      :offset    (iterator-offset    source)
                      :remaining (iterator-remaining source)
                      :last-hit  (iterator-last-hit  source))))

(defun make-iterator (source &optional old-iterator)
  (declare (type %iterator source)
           (type (or null iterator) old-iterator))
  (let ((new-iterator (init-iterator-from-source source old-iterator)))
    (replace (iterator-block new-iterator) (iterator-block source))
    new-iterator))

(defun copy-iterator (source &optional old-iterator)
  (declare (type %iterator source)
           (type (or null iterator) old-iterator))
  (multiple-value-bind (fd fd-box)
      (open-path (iterator-path source))
    (let ((new-iterator (init-iterator-from-source source old-iterator)))
      (setf (iterator-fd-box new-iterator) fd-box)
      (read-in-ub32 fd (iterator-offset new-iterator)
                    (* 4 2 (min 256 (iterator-remaining new-iterator)))
                    (iterator-block new-iterator))
      new-iterator)))

(defconstant +cdb-hash-seed+ 5381)
(defconstant +natural-size+ sb-vm:n-word-bits)

(declaim (ftype (function ((simple-array (unsigned-byte 8) 1))
                          (values (unsigned-byte 32) &optional))
                cdb-hash)
         (sb-ext:maybe-inline cdb-hash))
(defun cdb-hash (key)
  (declare (type (simple-array (unsigned-byte 8) 1) key)
           (optimize speed))
  (let ((hash +cdb-hash-seed+))
    (declare (type (unsigned-byte #.+natural-size+) hash))
    (map nil (lambda (x)
               (setf hash (logxor (mod (+ hash (ash hash 5))
                                       (ash 1 +natural-size+))
                                  x)))
         key)
    (mod hash (ash 1 32))))

(defun find-value (iterator key position byte-buffer)
  "Searches for key in the entry at byte-position position.
   byte-buffer is a temporary simple vector of octets which
   may be used arbitrarily. If a match is found, returns the
   value and t; a byte-buffer and nil otherwise."
  (declare (type %iterator iterator)
           (type (simple-array (unsigned-byte 8) 1) key byte-buffer)
           (type (unsigned-byte 32) position)
           (inline read-in-ub32 read-in-ub8)
           (optimize speed)
           (values (or null (simple-array (unsigned-byte 8) 1))
                   boolean &optional))
  (let* ((fd      (car (iterator-fd-box iterator)))
         (word-buffer (make-array 2 :element-type '(unsigned-byte 32)))
         (lengths (read-in-ub32 fd position 2 word-buffer))
         (key-len (aref lengths 0))
         (val-len (aref lengths 1)))
    (declare (dynamic-extent word-buffer))
    (when (< (length byte-buffer) key-len)
      (setf byte-buffer (make-array key-len :element-type '(unsigned-byte 8))))
    (let ((keyp (read-in-ub8 fd nil key-len byte-buffer)))
      (dotimes (i (aref lengths 0)
                (values (if (< (length byte-buffer) val-len)
                            (read-in-ub8 fd nil val-len)
                            (sb-kernel:%shrink-vector
                             (read-in-ub8 fd nil val-len byte-buffer)
                             val-len))
                        t))
        (unless (= (aref key i) (aref keyp i))
          (return-from find-value (values byte-buffer nil)))))))

(declaim (sb-ext:maybe-inline search-in-buffer))
(defun search-in-buffer (iterator key hash-value buffer beginning len byte-buffer)
  "Searches for key in the buffer (section of a hash table), from index
   beginning (inclusive) and below index len. byte-buffer is a temporary
   simple vector of octets that may be used arbitrarily. If an entry is
   found, returns the value and t and updates iterator-last-hit. Otherwise,
   returns a new byte-buffer and nil. If no more entries may be found,
   clears iterator-key."
  (declare (type %iterator iterator)
           (type (simple-array (unsigned-byte 8) 1) key byte-buffer)
           (type (unsigned-byte 32) hash-value beginning len)
           (type (simple-array (unsigned-byte 32) (512)) buffer)
           (optimize speed))
  (loop for i from beginning below len
        do (when (= hash-value (aref buffer (* 2 i)))
             (let ((position (aref buffer (1+ (* 2 i)))))
               (when (zerop position)
                 (setf (iterator-key iterator) nil)
                 (return (values byte-buffer nil)))
               (multiple-value-bind (value foundp)
                   (find-value iterator key position byte-buffer)
                 (if foundp
                     (progn
                       (setf (iterator-last-hit iterator) i)
                       (return (values value t)))
                     (setf byte-buffer value)))))
        finally (return (values byte-buffer nil))))

(defun search-in-hash (iterator key hash-value slot
                       offset length
                       byte-buffer)
  "Searches for key, beginning at slot in the table beginning at
   byte-offset offset and of length length. byte-buffer is a temporary
   simple vector of octets that may be used arbitrarily.
   If an entry is found, updates iterator-offset, iterator-remaining and
   iterator-block, and returns the value and t. Otherwise, return a new
   byte-buffer and nil, nulling iterator-key if no more entry may be found."
  (declare (type %iterator iterator)
           (type (simple-array (unsigned-byte 8) 1) key byte-buffer)
           (type (unsigned-byte 32) hash-value slot offset length)
           (optimize speed)
           (inline read-in-ub32 search-in-buffer)
           (values (or null (simple-array (unsigned-byte 8) 1))
                   boolean &optional))
  (let ((fd     (car (iterator-fd-box iterator)))
        (buffer (iterator-block iterator))
        (max-size *maximal-block-size*))
    (do* ((buffer-size (truncate (length buffer) 2)
                       (if (< buffer-size max-size)
                           (let* ((new-size (min (* 2 buffer-size)
                                                 max-size))
                                  (new-buffer (make-array (* 2 new-size)
                                               :element-type '(unsigned-byte 32))))
                             (setf buffer                    new-buffer
                                   (iterator-block iterator) new-buffer)
                             new-size)
                           buffer-size))
          (offset (+ offset (* 4 2 slot)) ; byte offset, entries are 2 words
                  (+ offset (* 4 2 buffer-size))) ; buffer-size entries at a time
          (remaining (- length slot)      ; how many entries left?
                     (- remaining buffer-size))   ; read by buffer-size
          (block-size (min buffer-size remaining)))
         ((<= remaining 0)
          (setf (iterator-key iterator) nil)
          (values nil nil))
      (declare (type (unsigned-byte 32) offset buffer-size))
      (multiple-value-bind (value foundp)
          (search-in-buffer iterator key hash-value
                            (read-in-ub32 fd offset (the (unsigned-byte 32)
                                                      (* 2 block-size))
                                          buffer)
                            0 block-size byte-buffer)
        (cond (foundp
               (setf (iterator-offset    iterator) offset
                     (iterator-remaining iterator) remaining)
               (return (values value t)))
              ((null (iterator-key iterator))
               (return (values nil nil)))
              (t
               (setf byte-buffer value)))))))

(declaim (ftype (function ((simple-array (unsigned-byte 8) 1) cdb)
                          (values (or (simple-array (unsigned-byte 8) 1)
                                      null)
                                  boolean &optional))
                %get)
         (ftype (function (iterator)
                          (values (or (simple-array (unsigned-byte 8) 1)
                                      null)
                                  boolean &optional))
                %next))
(defun %get (key cdb)
  "Find the first entry corresponding to key in cdb. Updates
   iterator slots if a match is found, nulls the iterator out
   otherwise. Returns the value and t if an entry is found, nil
   and nil otherwise."
  (declare (type (simple-array (unsigned-byte 8) 1) key)
           (type cdb cdb)
           (optimize speed)
           (inline cdb-hash))
  (unless (car (%cdb-fd-box cdb))
    (error "CDB has been CLOSEd. CDB: ~A" cdb))
  (setf (%cdb-key cdb) key)
  (let ((hash-value (cdb-hash key)))
    (setf (%cdb-hash cdb) hash-value)
    (multiple-value-bind (slot hash)
        (truncate hash-value 256)
      (multiple-value-bind (offset length)
          (cdb-table-info cdb hash)
        (when (zerop length)
          (setf (%cdb-key cdb) nil)
          (return-from %get (values nil nil)))
        (search-in-hash cdb key hash-value (mod slot length)
                        offset length
                        (make-array 32 :element-type '(unsigned-byte 8)))))))

(defun %next (iterator)
  "Finds the next value corresponding to the search. If none is
   found, nulls the iterator out and returns nil and nil. Otherwise,
   updates the iterator slots and returns the value and t.  "
  (declare (type %iterator iterator)
           (optimize speed)
           (inline search-in-buffer))
  (unless (car (iterator-fd-box iterator))
    (error "Iterator has been CLOSEd. Iterator: ~A" iterator))
  (unless (iterator-key iterator)
    (return-from %next (values nil nil)))
  (let* ((byte-buffer (make-array 32 :element-type '(unsigned-byte 8)))
         (key         (iterator-key       iterator))
         (hash-value  (iterator-hash      iterator))
         (buffer      (iterator-block     iterator))
         (buffer-size (truncate (length buffer) 2))
         (remaining   (iterator-remaining iterator))
         (beginning   (1+ (iterator-last-hit iterator))))
    (multiple-value-bind (value foundp)
        (search-in-buffer iterator key hash-value buffer beginning
                          (min buffer-size remaining)
                          byte-buffer)
      (cond (foundp
             (values value t))
            ((and (> remaining buffer-size)
                  (iterator-key iterator))
             (search-in-hash iterator key hash-value 0
                             (+ (* buffer-size 4 2) (iterator-offset iterator))
                             (- remaining buffer-size)
                             value))
            (t (setf (iterator-key iterator) nil)
               (values nil nil))))))

(declaim (ftype (function (string cdb &key (:external-format t))
                          (values (or null string) boolean &optional))
                get)
         (ftype (function (%iterator &key (:external-format t))
                          (values (or null string) boolean &optional))
                next))
(defun get (key cdb &key (external-format :default))
  "Searches for key in cdb and returns the value and t if one is found,
   nil and nil if not. It is assumed that strings are encoded according
   to external-format (:default by default) in the cdb. If a match is
   found, the iteration slots are updated; otherwise, they are nulled out."
  (declare (type string key)
           (type cdb cdb))
  (multiple-value-bind (value foundp)
      (%get (sb-ext:string-to-octets key :external-format external-format)
            cdb)
    (values (and value
                 (sb-ext:octets-to-string value
                                          :external-format external-format))
            foundp)))

(defun next (iterator &key (external-format :default))
  "Searches for the next value associated with the key in iterator. Returns
   the value and t if one is found, nil and nil if not. It is assumed that
   the return value must be decoded according to :external-format (:default
   by default) from the cdb. If a match is found, the iteration slots are
   updated; otherwise, they are nulled out."
  (declare (type %iterator iterator))
  (multiple-value-bind (value foundp)
      (%next iterator)
    (values (and value
                 (sb-ext:octets-to-string value
                                          :external-format external-format))
            foundp)))
