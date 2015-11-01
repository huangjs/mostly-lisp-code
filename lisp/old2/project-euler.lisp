(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf :lazy-list)
  (asdf :anaphora)
  (use-package :anaphora))

;;; p1
(defun merge2 (list1 list2)
  (cond ((null list1) list2)
	((null list2) list1)
	(t
	 (let ((x (ll:car list1))
	       (y (ll:car list2)))
	   (cond ((< x y)
		  (ll:cons x (merge2 (ll:cdr list1) list2)))
		 ((= x y)
		  (ll:cons x (merge2 (ll:cdr list1) (ll:cdr list2))))
		 (t
		  (ll:cons y (merge2 list1 (ll:cdr list2)))))))))

(defun integers (start &optional end)
  (if (or (null end) (<= start end))
      (ll:cons start (integers (1+ start) end))
      nil))

(defun take (n list)
  (ll:subseq list 0 n))

(defun take-when (pred list)
  (let ((head (ll:first list)))
    (if (funcall pred head)
	(ll:cons head (take-when pred (ll:cdr list)))
	nil)))

(defun sum (list)
  (apply #'+ (ll:enumerate-all list)))

(defun p1 ()
  (let ((3s (ll:mapcar #L (* 3 !1) (integers 1)))
	(5s (ll:mapcar #L (* 5 !1) (integers 1))))
    (sum (take-when #L(< !1 1000) (merge2 3s 5s)))))

;;; p2
(defun fibgen (a b)
  (ll:cons a (fibgen b (+ a b))))

(defun fibs ()
  (fibgen 1 1))

(defun p2 ()
  (sum
   (take-when #L (<= !1 1000000)
	      (ll:filter #l (evenp !1)
			 (fibs)))))

;;; p3
(defun sieve (max)
  "compute primes by annoted as 1 in the bit vector from 0 to max-1 (the same to its position in the bit vector).
Because the output is possibly too large, bind the value of symbol sym with the matrix and output the total number below max."
  (declare (type fixnum max)
	   (optimize (speed 3) (debug 1) (safety 0)))
  (let ((result (make-array max :element-type 'bit :initial-element 1))
	(sqrt-of-max (1+ (isqrt max)))
	(count (if (> max 2)
		   (- max 2)
		   0)))
    (declare (type fixnum sqrt-of-max count))
    (setf (aref result 0) 0)
    (setf (aref result 1) 0)
    (dotimes (i sqrt-of-max)
      (when (= (aref result i) 1)
	(loop for j of-type fixnum = (* 2 i) then (+ j i)
	   until (> j (- max 1))
	   do (when (= (aref result j) 1)
		(setf (aref result j) 0)
		(decf count)))))
    (values result count)))

(defun take-n-from-head (n vector)
  (loop with length = (length vector)
     with result = '()
     with count = 0
     for i from 0 to (1- length)
     until (>= count n)
     do (when (= (aref vector i) 1)
	  (push i result)
	  (incf count))
     finally (return (nreverse result))))

(defun take-n-from-tail (n vector)
  (loop with length = (length vector)
     with result = '()
     with count = 0
     for i from (1- length) downto 0
     until (>= count n)
     do (when (= (aref vector i) 1)
	  (push i result)
	  (incf count))
     finally (return result)))


(defun p3 ()
  (let ((primes-under (nreverse
		       (take-n-from-head most-positive-fixnum
					 (sieve (1+ (floor (sqrt 317584931803))))))))
    (find-if #l (integerp (/ 317584931803 !1))
	     primes-under)))

(defun p4 ()
  (iter outer (for i from 100 to 999)
	(iter (for j from 100 to 999)
	      (for p = (* i j))
	      (when (>= p 100000)
		(let ((ps (write-to-string p)))
		  (when (and (eql (aref ps 0) (aref ps 5))
			     (eql (aref ps 1) (aref ps 4))
			     (eql (aref ps 2) (aref ps 3)))
		    (in outer (maximize p))))))))

(defun p5 ()
  (let* ((primes (take-n-from-head most-positive-fixnum (sieve 20))))
    (labels ((max-factor-n (n)
	       (iter (for i from 1 to 20)
		     (for fl = (factor i))
		     (awhen (assoc n fl)
		       (maximize (second it))))))
      (let ((factors (iter (for i in primes)
			   (collect (cons i (max-factor-n i))))))
	(iter (for (b . e) in factors)
	      (multiply (expt b e)))))))

(defun p6 ()
  (let ((sum-of-square
	 (iter (for i from 1 to 100)
	       (sum (* i i))))
	(square-of-sum
	 (* #1=(iter (for i from 1 to 100)
		     (sum i)) #1#)))
    (- square-of-sum sum-of-square)))

(defun p7 ()
  (let ((primes (take-n-from-head 10001 (sieve 1000000))))
    (if (= (length primes) 10001)
	(car (last primes))
	(error "sieve too small"))))

(defun p8 ()
  (let ((string "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"))
    (let ((bag
	   (iter (for i from 0 below (- (length string) 5))
		 (collect
		     (list (parse-number (make-string 1 :initial-element (aref string i)))
			   (parse-number (make-string 1 :initial-element (aref string (+ 1 i))))
			   (parse-number (make-string 1 :initial-element (aref string (+ 2 i))))
			   (parse-number (make-string 1 :initial-element (aref string (+ 3 i))))
			   (parse-number (make-string 1 :initial-element (aref string (+ 4 i)))))))))
      (iter (for l in bag)
	    (maximize (apply #'* l))))))

(defun p9 ()
  (apply #'*
	 (first (collect-list (list x y z)
		  (for x from 1 to 500)
		  (for y from 1 to (- 1000 x))
		  (for z from 1 to (- 1000 x y))
		  (= (+ x y z) 1000)
		  (= (+ (* x x) (* y y))
		     (* z z))))))

(defun p10 ()
  (let ((primes (take-n-from-head most-positive-fixnum (sieve 1000000))))
    (apply #'+ primes)))

(defun v+ (x y)
  (mapcar #'+ x y))

(defun v- (x y)
  (mapcar #'- x y))

(defun v* (x y)
  (apply #'+ (mapcar #'* x y)))

(defun mod-v+ (x y mod)
  (list (mod (+ (first x) (first y)) mod)
	(mod (+ (second x) (second y)) mod)))

(defun mod-v- (x y mod)
  (list (mod (- (first x) (first y)) mod)
	(mod (- (second x) (second y)) mod)))

(defun p11 ()
  (let ((matrix
	 #2a((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
	     (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
	     (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
	     (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
	     (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
	     (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
	     (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
	     (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
	     (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
	     (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
	     (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
	     (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
	     (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
	     (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
	     (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
	     (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
	     (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
	     (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
	     (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
	     (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))
	(size 20))
    (let ((down '(1 0))
	  (right '(0 1))
	  (diag '(1 1))
	  (diag2 '(1 -1)))
      (labels ((gen-idx (p dir)
		 (iter (repeat 4)
		       (collect p)
		       (setf p (mod-v+ p dir size))))
	       (mul (p dir)
		 (iter (for idx in (gen-idx p dir))
		       (for val = (apply #'aref matrix idx))
		       (multiply val))))
	(iter outer (for i from 0 below size)
	      (iter (for j from 0 below size)
		    (for mul-down = (mul (list i j) down))
		    (for mul-right = (mul (list i j) right))
		    (for mul-diag = (mul (list i j) diag))
		    (for mul-diag2 = (mul (list i j) diag2))
		    (in outer
			(maximizing
			 (max mul-down mul-right mul-diag mul-diag2)))))))))

(defun p12 ()
  (iter (for i from 2)
	(for tri initially 1 then (+ tri i))
	(for factors = (factor tri))
	(when (> (apply #'* (mapcar #l (1+ (second !1)) factors)) 500)
	  (return tri))))

;;; p13
(declaim (inline char->num num->char char->string string->char
		 num->string string->num))

(defun char->num (char)
  (let ((code (char-code char)))
    (when (and (<= code 57) (>= code 48))
      (- code 48))))

(defun num->char (num)
  (when (and (>= num 0) (<= num 9))
    (code-char (+ num 48))))

(defun char->string (char)
  (make-string 1 :initial-element char))

(defun string->char (string)
  (when (= (length string) 1)
    (char string 0)))

(defun num->string (num)
  (write-to-string num))

(defun string->num (string)
  (parse-number string))

(defun p13 ()
  (let ((nums '(37107287533902102798797998220837590246510135740250
		46376937677490009712648124896970078050417018260538
		74324986199524741059474233309513058123726617309629
		91942213363574161572522430563301811072406154908250
		23067588207539346171171980310421047513778063246676
		89261670696623633820136378418383684178734361726757
		28112879812849979408065481931592621691275889832738
		44274228917432520321923589422876796487670272189318
		47451445736001306439091167216856844588711603153276
		70386486105843025439939619828917593665686757934951
		62176457141856560629502157223196586755079324193331
		64906352462741904929101432445813822663347944758178
		92575867718337217661963751590579239728245598838407
		58203565325359399008402633568948830189458628227828
		80181199384826282014278194139940567587151170094390
		35398664372827112653829987240784473053190104293586
		86515506006295864861532075273371959191420517255829
		71693888707715466499115593487603532921714970056938
		54370070576826684624621495650076471787294438377604
		53282654108756828443191190634694037855217779295145
		36123272525000296071075082563815656710885258350721
		45876576172410976447339110607218265236877223636045
		17423706905851860660448207621209813287860733969412
		81142660418086830619328460811191061556940512689692
		51934325451728388641918047049293215058642563049483
		62467221648435076201727918039944693004732956340691
		15732444386908125794514089057706229429197107928209
		55037687525678773091862540744969844508330393682126
		18336384825330154686196124348767681297534375946515
		80386287592878490201521685554828717201219257766954
		78182833757993103614740356856449095527097864797581
		16726320100436897842553539920931837441497806860984
		48403098129077791799088218795327364475675590848030
		87086987551392711854517078544161852424320693150332
		59959406895756536782107074926966537676326235447210
		69793950679652694742597709739166693763042633987085
		41052684708299085211399427365734116182760315001271
		65378607361501080857009149939512557028198746004375
		35829035317434717326932123578154982629742552737307
		94953759765105305946966067683156574377167401875275
		88902802571733229619176668713819931811048770190271
		25267680276078003013678680992525463401061632866526
		36270218540497705585629946580636237993140746255962
		24074486908231174977792365466257246923322810917141
		91430288197103288597806669760892938638285025333403
		34413065578016127815921815005561868836468420090470
		23053081172816430487623791969842487255036638784583
		11487696932154902810424020138335124462181441773470
		63783299490636259666498587618221225225512486764533
		67720186971698544312419572409913959008952310058822
		95548255300263520781532296796249481641953868218774
		76085327132285723110424803456124867697064507995236
		37774242535411291684276865538926205024910326572967
		23701913275725675285653248258265463092207058596522
		29798860272258331913126375147341994889534765745501
		18495701454879288984856827726077713721403798879715
		38298203783031473527721580348144513491373226651381
		34829543829199918180278916522431027392251122869539
		40957953066405232632538044100059654939159879593635
		29746152185502371307642255121183693803580388584903
		41698116222072977186158236678424689157993532961922
		62467957194401269043877107275048102390895523597457
		23189706772547915061505504953922979530901129967519
		86188088225875314529584099251203829009407770775672
		11306739708304724483816533873502340845647058077308
		82959174767140363198008187129011875491310547126581
		97623331044818386269515456334926366572897563400500
		42846280183517070527831839425882145521227251250327
		55121603546981200581762165212827652751691296897789
		32238195734329339946437501907836945765883352399886
		75506164965184775180738168837861091527357929701337
		62177842752192623401942399639168044983993173312731
		32924185707147349566916674687634660915035914677504
		99518671430235219628894890102423325116913619626622
		73267460800591547471830798392868535206946944540724
		76841822524674417161514036427982273348055556214818
		97142617910342598647204516893989422179826088076852
		87783646182799346313767754307809363333018982642090
		10848802521674670883215120185883543223812876952786
		71329612474782464538636993009049310363619763878039
		62184073572399794223406235393808339651327408011116
		66627891981488087797941876876144230030984490851411
		60661826293682836764744779239180335110989069790714
		85786944089552990653640447425576083659976645795096
		66024396409905389607120198219976047599490197230297
		64913982680032973156037120041377903785566085089252
		16730939319872750275468906903707539413042652315011
		94809377245048795150954100921645863754710598436791
		78639167021187492431995700641917969777599028300699
		15368713711936614952811305876380278410754449733078
		40789923115535562561142322423255033685442488917353
		44889911501440648020369068063960672322193204149535
		41503128880339536053299340368006977710650566631954
		81234880673210146739058568557934581403627822703280
		82616570773948327592232845941706525094512325230608
		22918802058777319719839450180888072429661980811197
		77158542502016545090413245809786882778948721859617
		72107838435069186155435662884062257473692284509516
		20849603980134001723930671666823555245252804609722
		53503534226472524250874054075591789781264330331690)))
    (let ((sum (apply #'+ nums)))
      (values sum
	      (string->num (subseq (num->string sum) 0 10))))))

(defun p14 ()
  (labels ((chain-len-iter (n acc)
	     (if (<= n 1)
		 acc
		 (chain-len-iter (if (evenp n) (/ n 2) (1+ (* 3 n)))
				 (incf acc))))
	   (chain-len (n)
	     (chain-len-iter n 1)))	; item start from 1
    (iter (for i from 1 to 1000000)
	  (finding i maximizing (chain-len i)))))

;;; p15
(declaim (inline factorial C))

(defun factorial (n)
  (iter (for i from 1 to n)
	(multiply i)))

(defun C (n m)
  "Combinatrial, n >= m, choosing m items from n items."
  (/ (factorial n) (factorial (- n m)) (factorial m)))

(defun p15 ()
  (C (+ 20 20) 20))

(defun p16 ()
  (let ((2^1000 (expt 2 1000)))
    (apply #'+ (map 'list #'char->num (num->string 2^1000)))))

(defun p17 ()
  (iter (for i from 1 to 1000)
	(for str = (remove-if #l (or (char= !1 #\Space) (char= !1 #\-))
			      (format nil "~r" i)))
	(for length = (if (and (> i 100) (< i 1000)
			       (> (mod i 100) 0))
			  (+ 3 (length str))
			  (length str)))
	(sum length)))

(defun triangle-sum (start-point triangle)
  (labels ((get-val (line pos)
	     (aref (aref triangle line) pos)))
    (destructuring-bind (line pos) start-point
      (if (= (1+ line) (length triangle))
	  (get-val line pos)
	  (+ (get-val line pos)
	     (max (triangle-sum (list (1+ line) pos) triangle)
		  (triangle-sum (list (1+ line) (1+ pos)) triangle)))))))

(defun p18 ()
  (let ((triangle
	 #(#(75)
	   #(95 64)
	   #(17 47 82)
	   #(18 35 87 10)
	   #(20 04 82 47 65)
	   #(19 01 23 75 03 34)
	   #(88 02 77 73 07 63 67)
	   #(99 65 04 28 06 16 70 92)
	   #(41 41 26 56 83 40 80 70 33)
	   #(41 48 72 33 47 32 37 16 94 29)
	   #(53 71 44 65 25 43 91 52 97 51 14)
	   #(70 11 33 28 77 73 17 78 39 68 17 57)
	   #(91 71 52 38 17 14 91 43 58 50 27 29 48)
	   #(63 66 04 68 89 53 67 30 73 16 69 87 40 31)
	   #(04 62 98 27 23 09 70 98 73 93 38 53 60 04 23))))
    (memoization::with-memoization (triangle-sum)
      (triangle-sum '(0 0) triangle))))

(defun p19 ()
  (iter outer (for y from 1901 to 2000)
	(iter (for m from 1 to 12)
	      (when (= 6 (weekday :time (encode-universal-time 1 1 1 1 m y)))
		(in outer (counting y))))))

(defun p20 ()
  (reduce #l (+ !1 (char->num !2))
	  (num->string (factorial 100))
	  :initial-value 0))

;;; p21
(defun power-select (bags)
  "bags is like: ((5 2) (2 1)) which means in the bag we have 2 of 5s and 1 of 2s. the combination is (() (2) (5) (2 5) (5 5) (2 5 5))"
  (if (null bags)
      '(())
      (let* ((first-pair (first bags))
	     (rest-pairs (rest bags))
	     (p (first first-pair))
	     (m (second first-pair)))
	(iter outer (for i from 0 to m)
	      (iter (for l in (power-select rest-pairs))
		    (in outer
			(if (zerop i)
			    (collect l)
			    (collect (nconc (make-list i :initial-element p)
					    l)))))))))

(defun all-factors (n)
  (let* ((factors (factor n))
	 (non-null-ones (rest (power-select factors))))
    (cons 1
	  (mapcar #l (apply #'* !1) non-null-ones))))

(defun all-divisors (n)
  "all-factors without self"
  (if (= n 1)
      '(1)
      (butlast (all-factors n))))

(defun p21 ()
  (labels ((peer (n)
	     (apply #'+ (all-divisors n)))
	   (amicable (n)
	     (let ((peer (peer n)))
	       (and (not (= peer n))
		    (= n (peer peer))))))
    (iter (for i from 1 to 10000)
	  (when (amicable i)
	    (sum i)))))

;;; p22
(declaim (inline alpha-rank))

(defun alpha-rank (c)
  (- (char-code (char-upcase c)) 64))

(defun p22 ()
  (let ((names (with-open-file (f "names.txt")
		 (let ((result (make-array (list (file-length f))
					   :element-type 'character)))
		   (read-sequence result f)
		   (sort
		    (mapcar #'read-from-string
			    (split-sequence:split-sequence #\, result))
		    #'string<)))))
    (iter (for i from 1)
	  (for name in names)
	  (for rank = (iter (for c in-string name) (sum (alpha-rank c))))
	  (sum (* i rank)))))

;;; p23
(defun what-number (n)
;;; note: 0 => proper 1=> deficient ???
  (let ((sum (apply #'+ (all-divisors n))))
    (cond ((< sum n) 'deficient)
	  ((> sum n) 'abundant)
	  (t 'proper))))

(memoization::memoize 'what-number :test #'eql :key #'first)

(defun p23 ()
  (iter (for x from 1 to 28123)
	(when (null (iter (for y from 1 to (floor x 2))
			  (when (and (eql (what-number y) 'abundant)
				     (eql (what-number (- x y)) 'abundant))
			    (return x))))
	  (sum x))))

;;; p24
(defun permutation (list)
  (if (null list)
      '(())
      (collect-list (cons x ys)
	(for x in list)
	(for ys in (permutation (remove x list))))))

(defun permutation-as (type list)
  (if (null list)
      '(())
      (collect-list (coerce (cons x ys) type)
	(for x in list)
	(for ys in (permutation (remove x list))))))

(defun p24 ()
  (prog1 
      (with-output-to-string (s)
	(iter (for c in-vector
		   (nth 999999 (permutation-as '(vector (unsigned-byte 8))
					       '(0 1 2 3 4 5 6 7 8 9))))
	      (princ c s)))
    (gc :full t)))

;;; p25
(defun lazy-find-if (pred list)
  (iter (for i from 0)
	(for e in list by #'ll:cdr)
	(when (funcall pred e)
	  (return (values e i)))))

(defun p25 ()
  (let ((fibs (fibs)))
    (1+ (nth-value 1 (lazy-find-if #l (>= (length (num->string !1)) 1000) fibs)))))

(defun p26 ()
  (labels ((move (base div)
	     (iter (for i initially base then (* 10 i))
		   (when (> (truncate i div) 0)
		     (return i)))))
    (iter (for d from 1 below 1000)
	  (finding d maximizing
		   (iter (for (values quo rem) initially (truncate (move 1 d) d)
			      then (truncate (move rem d) d))
			 (with bag = '())
			 (cond ((= 0 rem) (return 0))
			       ((find (cons quo rem) bag :test #'equal)
				(return (length bag)))
			       (t
				(push (cons quo rem) bag))))))))

;;; p27
(defun factor (n)
  (factor n))

(defun primep (n)
  (declare (integer n))
  (if (= n 2)
      t
      (and (> n 1)
	   (oddp n)
	   (iter (for i from 3 to (floor (sqrt n)) by 2)
		 (never (integerp (/ n i)))))))

(defun p27 ()
  (labels ((eqt (n a b)
	     #i "n^^2 + a*n +b"))
    (iter outer (for a from -999 to 999)
	  (iter (for b from -999 to 999)
		(in outer
		    (finding (* a b) maximizing
			     (iter (for n from 0)
				   (when (not (primep (eqt n a b)))
				     (return n)))))))))


(defun p28 ()
  (iter (for step initially 2 then (incf step 2))
	(for size from 3 to 1001 by 2)
	(with num = 1)
	(repeating 4
	  (sum (incf num step) into total))
	(finally (return (1+ total)))))

(defun p29 ()
  (length
   (remove-duplicates
    (collect-list (expt a b)
      (for a from 2 to 100)
      (for b from 2 to 100)))))

(defun p30 ()
  (iter (for i from 2 to 1000000)
	(when (= i
		 (apply #'+
			(map 'list #l (expt (char->num !1) 5)
			     (num->string i))))
	  (sum i))))

(defun p31 ()
  (let ((coins #(1 2 5 10 20 50 100 200)))
    (labels ((count-change (amount)
               (cc amount (length coins)))
             (cc (amount kinds-of-coins)
               (cond ((= amount 0) 1)
                     ((or (< amount 0) (= kinds-of-coins 0)) 0)
                     (t (+ (cc amount
                               (- kinds-of-coins 1))
                           (cc (- amount
                                  (first-denomination kinds-of-coins))
                               kinds-of-coins)))))
             (first-denomination (kinds-of-coins)
               (aref coins (1- kinds-of-coins))))
      (count-change 200))))

(defun p32 ()
  (iter outer (for i from 1 to 10000)
	(for fs = (remove-if-not #l (< !1 (floor (sqrt i))) (all-factors i)))
	(for gs = (mapcar #l (/ i !1) fs))
	(iter (for f in fs)
	      (for g in gs)
	      (for nums = (s+ (num->string i)
			      (num->string f)
			      (num->string g)))
	      (when (and (= 9 (length nums) (length (remove-duplicates nums)))
			 (not (find #\0 nums)))
		(in outer
		    (sum i))
		(return 'continue)))))

(defun P33 ()
  (denominator
   (iter outer (for denom from 50 to 99)
	 (iter (for num from 10 to denom)
	       (when (and (multiple-value-call #'/= (truncate denom 10))
			  (multiple-value-call #'/= (truncate num 10))
			  (= (nth-value 1(truncate num 10))
			     (truncate denom 10)))
		 (let ((new-denom (nth-value 1 (truncate denom 10)))
		       (new-num (truncate num 10)))
		   (if (and (> new-denom 0)
			    (= (/ num denom) (/ new-num new-denom)))
		       (in outer
			   (multiply (/ num denom))))))))))

(defun p34 ()
  (declare (inline factorial))
  (iter (for i from 3 to 2540160)
	(for sum-of-fac = (apply #'+ (map 'list #l (factorial (char->num !1))
					  (num->string i))))
	(when (= i sum-of-fac)
	  (sum i))))

(defun p35 ()
  (labels ((circular (n)
	     (mapcar #'string->num
		     (iter (with str = (num->string n))
			   (for i from 0 below (length str))
			   (collect (s+ (subseq str i)
					(subseq str 0 i)))))))
    (iter (for i below 1000000)
	  (when (primep i)
	    (when (every #'primep (circular i))
	      (count i))))))

;;; p36
(defun palindromic (n &optional (base 10))
  (iter (with str = (write-to-string n :base base))
	(with length = (length str))
	(return
	  (iter (for i from 0 to (floor length 2))
		(always (eql (aref str i) (aref str (- length i 1))))))))

(defun p36 ()
  (iter (for i below 1000000)
	(when (and (palindromic i)
		   (palindromic i 2))
	  (sum i))))

(defun p37 ()
  (iter (for i from 10)
	(when (primep i)
	  (when (iter (with str = (num->string i))
		      (for j from 0 below (length str))
		      (always
		       (and (primep (string->num (subseq str j)))
			    (primep (string->num (subseq str 0 (- (length str)
								  j)))))))
	    (progn
	      (sum i into total)
	      (count i into times)
	      (if (= times 11)
		  (return-from p37 total)))))))

;;; p38
(defun pandigit-p (n)
  (iter (with str = (num->string n))
	(for i from 1 to (length str))
	(always (find (num->char i) str))))

(defun p38 ()			 ; only num with 4 digits are feasible
  (iter (for i from 9000 to 9999)	; the first digit must be 9
	(for 2i = (* 2 i))
	(let ((candidate (string->num
			  (s+ (num->string i) (num->string 2i)))))
	  (if (pandigit-p candidate)
	      (maximizing candidate)))))


;;; p39
(defun gougu-divide (n)
  (iter outer (for x from 1 below (floor n 3))
	(iter (for y from x below (- n x))
	      (for z = (- n x y))
	      (when (= (+ (* x x) (* y y))
		       (* z z))
		(in outer
		    (collect (list x y z)))))))

(defun p39 ()
  (iter (for i below 1000)
	(finding i maximizing
		 (length (gougu-divide i)))))

(defun p40 ()
  (let ((result (make-array 1000010 :element-type 'character :fill-pointer 0)))
    (iter (with len = 0)
	  (while (<= len 1000000))
	  (for i from 0)
	  (iter (for c in-string (num->string i))
		(progn
		  (vector-push c result)
		  (incf len)))
	  (finally
	   (return (* (char->num (aref result 1))
		      (char->num (aref result 10))
		      (char->num (aref result 100))
		      (char->num (aref result 1000))
		      (char->num (aref result 10000))
		      (char->num (aref result 100000))
		      (char->num (aref result 1000000))))))))

(defun p41 ()
  (iter (with primes = (take-n-from-head most-positive-fixnum
					 (sieve 100000000)))
	(for i in primes)
	(when (pandigit-p i)
	  (maximizing i))))

(defun p42 ()
  (let ((txt (with-output-to-string (s)
	       (with-open-file (f "words.txt")
		 (write-line (read-line f nil nil) s)))))
    (let ((words (mapcar #'read-from-string
			 (split-sequence:split-sequence #\, txt)))
	  (ts (iter (for n from 1)
		    (for tn = (* n (1+ n) 1/2))
		    (while (< tn 500))
		    (collect tn))))
      (iter (for w in words)
	    (for rank = (apply #'+ (map 'list #'alpha-rank w)))
	    (when (find rank ts)
	      (count w))))))

;;; p43
(declaim (inline divisible-p))
(defun divisible-p (n m)
  "whether n is divisable by m"
  (integerp (/ n m)))

(defun p43 ()
  (labels ((to-num (vec)
	     (iter (with len = (length vec))
		   (for i from 1 to len)
		   (for base initially 1 then (* base 10))
		   (sum (* base (aref vec (- len i)))))))
    (iter (with pans = (permutation-as '(vector (unsigned-byte 8))
				       '(0 1 2 3 4 5 6 7 8 9)))
	  (for v in pans)
	  (when (and (divisible-p (to-num (subseq v 1 4)) 2)
		     (divisible-p (to-num (subseq v 2 5)) 3)
		     (divisible-p (to-num (subseq v 3 6)) 5)
		     (divisible-p (to-num (subseq v 4 7)) 7)
		     (divisible-p (to-num (subseq v 5 8)) 11)
		     (divisible-p (to-num (subseq v 6 9)) 13)
		     (divisible-p (to-num (subseq v 7 10)) 17))
	    (sum (to-num v))))))

(defun p44 ()
  (labels ((pantagonal (n)
	     #i "n*(3*n-1)/2"))
    (iter (with bag = '())
	  (for i from 1)
	  (for cur = (pantagonal i))
	  (progn
	    (push cur bag)
	    (iter (for p in bag)
		  (aif (find (- cur p) bag)
		       (if (and (/= p it)
				(find (abs (- p it)) bag))
			   (return-from p44 (abs (- p it))))))))))


(defun p45 ()
  (labels ((tri (n)
	     #i "n*(n+1)/2")
	   (pen (n)
	     #i "n*(3*n-1)/2")
	   (hex (n)
	     #i "n*(2*n-1)")
	   (maximum (px py pz)
	     (symbol-macrolet ((x (car px))
			       (y (car py))
			       (z (car pz)))
	       (if (< x y)
		   (if (< y z) pz py)
		   (if (< x z) pz px))))
	   (p< (px py)
	     (< (car px) (car py)))
;;; copy-cons
	   (copy-cons (cons)
	     (cons (car cons) (cdr cons))))
    (iter (with i = 286)
	  (with j = 166)
	  (with k = 144)
	  (with tnum = (cons (tri i) i))
	  (with pnum = (cons (pen j) j))
	  (with hnum = (cons (hex k) k))
	  (for max = (copy-cons (maximum tnum pnum hnum)))
	  (progn
	    (iter (while (p< tnum max))
		  (incf (cdr tnum))
		  (setf (car tnum) (tri (cdr tnum))))
	    (iter (while (p< pnum max))
		  (incf (cdr pnum))
		  (setf (car pnum) (pen (cdr pnum))))
	    (iter (while (p< hnum max))
		  (incf (cdr hnum))
		  (setf (car hnum) (hex (cdr hnum)))))
	  (when (= (car tnum) (car pnum) (car hnum))
	    (return (car max))))))

;;; p46
(defun squarep (n &optional (epsilon 1f-20))
  (assert (<= n 16000000))
  (let ((sqrt (sqrt n)))
    (< (abs (nth-value 1 (round sqrt)))
       epsilon)))

(defun p46 ()
  (let* ((odds (iter (for i from 9 to 10000 by 2) (collect i)))
	 (primes (take-n-from-head 1000 (sieve 100000)))
	 (odd-comp (sort (set-difference odds primes) #'<)))
    (iter (for i in odd-comp)
	  (when (iter (for j in primes)
		      (while (< j i))
		      (never (squarep #i "(i-j)/2")))
	    (return-from p46 i)))))

(defun p47 ()
  (iter (for i from 1)
	(when (and
	       (= 4 (length (factor i)))
	       (= 4 (length (factor (+ i 1))))
	       (= 4 (length (factor (+ i 2))))
	       (= 4 (length (factor (+ i 3)))))
	  (return i))))

(defun p48 ()
  (iter (for i from 1 to 1000)
	(sum (expt i i) into total)
	(finally
	 (let* ((v (num->string total))
		(len (length v)))
	   (return (string->num (subseq v (- len 10))))))))

(defun p49 ()
  (let ((primes (remove-if-not #l (and (>= !1 1001) (<= !1 9999))
			       (take-n-from-head 10000 (sieve 1000000)))))
    (iter outer (for i in primes)
	  (for p = (remove-duplicates
		    (mapcar #'string->num
			    (permutation-as 'string
					    (coerce (num->string i) 'list)))))
	  (iter (for j in p) 
		(for k = #i "j+(j-i)")
		(when (and (> j i)
			   (primep j)
			   (primep k)
			   (find k p))
		  (in outer
		      (collect
			  (string->num
			   (s+ (num->string i)
			       (num->string j)
			       (num->string k))))))))))

(defun p52 ()
  (iter (for i from 1)
	(for str = (sort (num->string i) #'char<)) 
	(when (and 
	       (let ((s (num->string (* 6 i))))
		 (and (= (length s) (length str))
		      (string= str (sort s #'char<))))
	       (let ((s (num->string (* 5 i))))
		 (and (= (length s) (length str))
		      (string= str (sort s #'char<))))
	       (let ((s (num->string (* 4 i))))
		 (and (= (length s) (length str))
		      (string= str (sort s #'char<))))
	       (let ((s (num->string (* 3 i))))
		 (and (= (length s) (length str))
		      (string= str (sort s #'char<))))
	       (let ((s (num->string (* 2 i))))
		 (and (= (length s) (length str))
		      (string= str (sort s #'char<)))))
	  (return i))))

(defun p53 ()
  (iter outer (for n from 1 to 100)
	(iter (for r from 1 to n)
	      (when (> (c n r) 1000000)
		(in outer
		    (count (list n r)))))))

(defun p54 ()
  (iter outer (for a from 1 to 100)
	(iter (for b from 1 to 100)
	      (in outer
		  (maximize (apply #'+ (map 'list 
					    #'char->num
					    (num->string (expt a b)))))))))


(defun p67 ()
  (let ((triangle
	 (with-open-file (f "triangle.txt")
	   (iter (for line = (read-line f nil nil))
		 (while line)
		 (collect
		     (with-input-from-string (s line)
		       (iter (for n = (read s nil nil))
			     (while n)
			     (collect n result-type 'vector)))
		   result-type 'vector)))))
    (memoization::with-memoization (triangle-sum)
      (triangle-sum '(0 0) triangle))))

