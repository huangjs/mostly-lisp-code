(defconstant +roots-parent+ -1)

#||
Type: (array integer (*)) -> integer

Return: height of the tree, where height(empty-tree) is 0, and height(root-only-tree) is 1.

Precondition:
 - the input array is a valid representation of a tree, 

Postcondition:
 - the content of the input array cannot be changed.

Note:
 - 'a valid tree' can be described as follows:
   the array index represents the value of a node and the array element represents the value of the node's parent.
 - the algorithm use dynamic programming techniques based on this simple algorithm:
   For each node in the array
	traverse through all its ancesters and count the steps
   the largest number of steps is the height of the tree
||#
(defun height-of-tree (tree)
  (check-type tree (array integer (*)))
  ;; NOTE: input validation is omitted.
  (let* ((size (length tree))
	 (max-depth 0) 
	 ;; cache of depth for each node, 0 means uncalculated
	 (cache (make-array (length tree) :element-type 'integer :initial-element 0)))
    (loop for i below size
       do (let ((counter 0)
		(reference-depth 0))
	    ;; count depth that is necessary
	    (loop for current-node = i then (aref tree current-node)
	       while (not (= current-node +roots-parent+))
	       do (let ((depth-if-calculated (aref cache current-node)))
		    (when (not (zerop depth-if-calculated))
		      (setf reference-depth depth-if-calculated)
		      (return))
		    (incf counter)))
	    ;; fill the cache
	    (loop for current-node = i then (aref tree current-node)
	       repeat counter ; we already know how many nodes we need to update
	       for d downfrom (+ counter reference-depth)
	       do (setf (aref cache current-node) d))
	    ;; update max-depth
	    (let ((depth (+ counter reference-depth)))
	      (when (> depth max-depth)
		(setf max-depth depth))))) 
    max-depth))
`

#| Examples:
CL-USER> (height-of-tree #())
0
CL-USER> (height-of-tree #(-1))
1
CL-USER> (height-of-tree #(5 0 5 0 5 -1))
3
CL-USER> (height-of-tree #(3 -1 0 1))
4
|#
