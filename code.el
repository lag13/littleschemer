;; -*- lexical-binding: t; -*-

;; Code from The Little Schemer adapted for elisp.
;; https://github.com/pkrumins/the-little-schemer

(defun atom? (x)
  (not (listp x)))

(defun null? (x)
  (null x))

(setq else t)

(defun lat? (l)
  (cond
   ((null? l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (else nil)))

(defun eq? (x y)
  (eq x y))

;; Chapter 2

(defun member? (a lat)
  (cond
   ((null? lat) nil)
   ((eq? (car lat) a) t)
   (else (member? a (cdr lat)))))

;; Chapter 3

(defun rember (a lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) a) (cdr lat))
   (else (cons (car lat)
	       (rember a (cdr lat))))))

(defun firsts (l)
  (cond
   ((null? l) ())
   (else (cons (car (car l))
	       (firsts (cdr l))))))

(defun insertR (new old lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) old) (cons old (cons new (cdr lat))))
   (else (cons (car lat) (insertR new old (cdr lat))))))

(defun insertL (new old lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) old) (cons new lat))
   (else (cons (car lat) (insertL new old (cdr lat))))))

(defun subst (new old lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) old) (cons new (cdr lat)))
   (else (cons (car lat)
	       (subst new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  (cond
   ((null? lat) ())
   ((or (eq? (car lat) o1)
	(eq? (car lat) o2))
    (cons new (cdr lat)))
   (else (cons (car lat)
	       (subst2 new o1 o2 (cdr lat))))))

(defun multirember (a lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) a) (multirember a (cdr lat)))
   (else (cons (car lat)
	       (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) old) (cons old
			      (cons new
				    (multiinsertR new old (cdr lat)))))
   (else (cons (car lat)
	       (multiinsertR new old (cdr lat))))))

(defun multiinsertL (new old lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) old) (cons new
			      (cons old
				    (multiinsertL new old (cdr lat)))))
   (else (cons (car lat)
	       (multiinsertL new old (cdr lat))))))

(defun multisubst (new old lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) old) (cons new
			      (multisubst new old (cdr lat))))
   (else (cons (car lat)
	       (multisubst new old (cdr lat))))))

;; Chapter 4

(defun add1 (x)
  (1+ x))

(defun sub1 (x)
  (1- x))

(defun zero? (x)
  (zerop x))

;; These two functions are so similar that it feels like you could
;; abstract them into a helper function. But I suppose doing so is not
;; really necessary since they're so small. But is doing something
;; like that worth it? Will it reveal some more general computational
;; pattern?
(defun o+ (n m)
  (cond
   ((zero? m) n)
   (else (add1 (o+ n (sub1 m))))))

(defun o- (n m)
  (cond
   ((zero? m) n)
   (else (sub1 (o- n (sub1 m))))))

(defun addtup (tup)
  (cond
   ((null? tup) 0)
   (else (o+ (car tup) (addtup (cdr tup))))))

(defun ox (n m)
  (cond
   ((zero? m) 0)
   (else (o+ n (ox n (sub1 m))))))

(defun tup+ (tup1 tup2)
  (cond
   ((null? tup1) tup2)
   ((null? tup2) tup1)
   (else (cons (o+ (car tup1) (car tup2))
	       (tup+ (cdr tup1) (cdr tup2))))))

(defun o> (n m)
  (cond
   ((zero? n) nil)
   ((zero? m) t)
   (else (o> (sub1 n) (sub1 m)))))

(defun o< (n m)
  (cond
   ((zero? m) nil)
   ((zero? n) t)
   (else (o< (sub1 n) (sub1 m)))))
   
(defun o= (n m)
  (cond
   ((zero? m) (zero? n))
   ((zero? n) nil)
   (else (o= (sub1 n) (sub1 m)))))

(defun o^ (n m)
  (cond
   ((zero? m) 1)
   (else (ox n (o^ n (sub1 m))))))

(defun o/ (n m)
  (cond
   ((o< n m) 0)
   (else (add1 (o/ (o- n m) m)))))

(defun olength (l)
  (cond
   ((null? l) 0)
   (else (add1 (olength (cdr l))))))

(defun pick (n lat)
  (cond
   ((zero? (sub1 n)) (car lat))
   (else (pick (sub1 n) (cdr lat)))))

(defun rempick (n lat)
  (cond
   ((zero? (sub1 n)) (cdr lat))
   (else (cons (car lat)
	       (rempick (sub1 n) (cdr lat))))))

(defun number? (n)
  (numberp n))

(defun no-nums (lat)
  (cond
   ((null? lat) ())
   ((number? (car lat)) (no-nums (cdr lat)))
   (else (cons (car lat)
	       (no-nums (cdr lat))))))

(defun all-nums (lat)
  (cond
   ((null? lat) ())
   ((number? (car lat)) (cons (car lat)
			      (all-nums (cdr lat))))
   (else (all-nums (cdr lat)))))

(defun eqan? (a1 a2)
  (cond
   ((and (number? a1) (number? a2)) (o= a1 a2))
   ((or (number? a1) (number? a2)) nil)
   (else (eq? a1 a2))))

;; This was called "occur" in the book but that is already an emacs
;; function. I suppose it is a bit dangerous to be defining functions
;; in emacs in the event of overwriting something.
(defun ooccur (a lat)
  (cond
   ((null? lat) 0)
   ((eqan? (car lat) a) (add1 (ooccur a (cdr lat))))
   (else (ooccur a (cdr lat)))))

(defun one? (n)
  (o= n 1))

;; Chapter 5

(defun rember* (a l)
  (cond
   ((null? l) ())
   ((atom? (car l)) (cond
		     ((eq? (car l) a) (rember* a (cdr l)))
		     (else (cons (car l)
				 (rember* a (cdr l))))))
   (else (cons (rember* a (car l))
	       (rember* a (cdr l))))))

(defun insertR* (new old l)
  (cond
   ((null? l) ())
   ((atom? (car l))
    (cond
     ((eq? (car l) old) (cons old
			      (cons new
				    (insertR* new old (cdr l)))))
     (else (cons (car l)
		 (insertR* new old (cdr l))))))
   (else (cons (insertR* new old (car l))
	       (insertR* new old (cdr l))))))

(defun occur* (a l)
  (cond
   ((null? l) 0)
   ((atom? (car l))
    (cond
     ((eq? (car l) a) (add1 (occur* a (cdr l))))
     (else (occur* a (cdr l)))))
   (else (o+ (occur* a (car l))
	     (occur* a (cdr l))))))

(defun subst* (new old l)
  (cond
   ((null? l) ())
   ((atom? (car l))
    (cond
     ((eq? (car l) old) (cons new
			      (subst* new old (cdr l))))
     (else (cons (car l)
		 (subst* new old (cdr l))))))
   (else (cons (subst* new old (car l))
	       (subst* new old (cdr l))))))

(defun insertL* (new old l)
  (cond
   ((null? l) ())
   ((atom? (car l))
    (cond
     ((eq? (car l) old) (cons new
			      (cons old
				    (insertL* new old (cdr l)))))
     (else (cons (car l)
		 (insertL* new old (cdr l))))))
   (else (cons (insertL* new old (car l))
	       (insertL* new old (cdr l))))))

;; Turns out member* is also an elisp function hence the rename.
(defun mmember* (a l)
  (cond
   ((null? l) nil)
   ((atom? (car l)) (or (eq? (car l) a)
			(mmember* a (cdr l))))
   (else (or (mmember* a (car l))
	     (mmember* a (cdr l))))))

(defun leftmost (l)
  (cond
   ((atom? (car l)) (car l))
   (else (leftmost (car l)))))

;; That simplification we made using equal? is really cool. I didn't
;; see it at first.
(defun eqlist? (l1 l2)
  (cond
   ((and (null? l1) (null? l2)) t)
   ((or (null? l1) (null? l2)) nil)
   (else (and (equal? (car l1) (car l2))
	      (eqlist? (cdr l1) (cdr l2))))))

(defun equal? (s1 s2)
  (cond
   ((and (atom? s1) (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1) (atom? s2)) nil)
   (else (eqlist? s1 s2))))

(defun rember (s l)
  (cond
   ((null? l) ())
   ((equal? (car l) s) (cdr l))
   (else (cons (car l)
	       (rember s (cdr l))))))

;; Chapter 6
(defun numbered? (aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (else (and (numbered? (car aexp))
	      (numbered? (car (cdr (cdr aexp))))))))

(defun 1st-sub-exp (aexp)
  (car (cdr aexp)))

(defun 2nd-sub-exp (aexp)
  (car (cdr (cdr aexp))))

(defun operator (aexp)
  (car aexp))

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq? (operator nexp) '+)
    (o+ (value (1st-sub-exp nexp))
	(value (2nd-sub-exp nexp))))
   ((eq? (operator nexp) 'X)
    (ox (value (1st-sub-exp nexp))
	(value (2nd-sub-exp nexp))))
   (else
    (o^ (value (1st-sub-exp nexp))
	(value (2nd-sub-exp nexp))))))

(defun sero? (n)
  (null? n))

(defun edd1 (n)
  (cons () n))

(defun zub1 (n)
  (cdr n))

(defun oo+ (n m)
  (cond
   ((sero? m) n)
   (else
    (edd1 (oo+ n (zub1 m))))))

;; Chapter 7
(defun member? (a lat)
  (cond
   ((null? lat) nil)
   ((equal? (car lat) a) t)
   (else (member? a (cdr lat)))))
 
(defun set? (lat)
  (cond
   ((null? lat) t)
   ((member? (car lat) (cdr lat)) nil)
   (else (set? (cdr lat)))))

(defun makeset (lat)
  (cond
   ((null? lat) ())
   ((member? (car lat) (cdr lat))
    (makeset (cdr lat)))
   (else (cons (car lat)
	       (makeset (cdr lat))))))

(defun omakeset (lat)
  (cond
   ((null? lat) ())
   (else
    (cons (car lat)
	  (omakeset (multirember (car lat)
				 (cdr lat)))))))

(defun subset? (set1 set2)
  (cond
   ((null? set1) t)
   (else (and (member? (car set1) set2)
	      (subset? (cdr set1) set2)))))

(defun eqset? (set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(defun intersect? (set1 set2)
  (cond
   ((null? set1) nil)
   (else (or (member? (car set1) set2)
	     (intersect? (cdr set1) set2)))))

(defun intersect (set1 set2)
  (cond
   ((null? set1) ())
   ((member? (car set1) set2)
    (cons (car set1)
	  (intersect (cdr set1) set2)))
   (else (intersect (cdr set1) set2))))

(defun union (set1 set2)
  (cond
   ((null? set1) set2)
   ((member? (car set1) set2)
    (union (cdr set1) set2))
   (else (cons (car set1)
	       (union (cdr set1) set2)))))

(defun xxx (set1 set2)
  (cond
   ((null? set1) ())
   ((member? (car set1) set2)
    (xxx (cdr set1) set2))
   (else (cons (car set1)
	       (xxx (cdr set1) set2)))))

;; My first version of this function was more complicated. The book's
;; version is very clever!
(defun intersectall (l-set)
  (cond
   ((null? (cdr l-set)) (car l-set))
   (else (intersect (car l-set)
		    (intersectall (cdr l-set))))))

(defun a-pair? (x)
  (cond
   ((atom? x) nil)
   (else (o= 2 (olength x)))))

(defun first (p)
  (car p))

(defun second (p)
  (car (cdr p)))

(defun build (s1 s2)
  (cons s1 (cons s2 ())))

(defun fun? (rel)
  (set? (firsts rel)))

(defun revpair (pair)
  (build (second pair) (first pair)))

(defun revrel (rel)
  (cond
   ((null? rel) ())
   (else (cons (revpair (car rel))
	       (revrel (cdr rel))))))

(defun seconds (l)
  (cond
   ((null? l) ())
   (else (cons (car (cdr (car l)))
	       (seconds (cdr l))))))

(defun fullfun? (fun)
  (set? (seconds fun)))

(defun one-to-one? (fun)
  (fun? (revrel fun)))

;; Chapter 8
(defun rember-f (test? a l)
  (cond
   ((null? l) ())
   ((funcall test? (car l) a) (cdr l))
   (else (cons (car l)
	       (rember-f test? a l)))))

;; Its too bad that, at this point elisp, starts to fall apart in
;; terms of conciseness because functions are not treated as just
;; regular values.
(defun eq?-c (a)
  (lambda (x)
    (eq? x a)))

;; a more curried version of rember-f
(defun rember-f (test?)
  (lambda (a l)
    (cond
     ((null? l) ())
     ((funcall test? (car l) a) (cdr l))
     (else (cons (car l)
		 (funcall (rember-f test?) a (cdr l)))))))

(defun insertL-f (test?)
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((funcall test? (car l) old) (cons new l))
     (else (cons (car l)
		 (funcall (insertL-f test?) new old (cdr l)))))))

(defun insertR-f (test?)
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((funcall test? (car l) old)
      (cons old
	    (cons new (cdr l))))
     (else (cons (car l)
		 (funcall (insertR-f test?) new old (cdr l)))))))

(defun insert-g (seq)
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((eq? (car l) old)
      (funcall seq new old (cdr l)))
     (else (cons (car l)
		 (funcall (insert-g leftOrRight) new old (cdr l)))))))

(setq seqL (lambda (new old l) (cons new (cons old l))))
(setq seqR (lambda (new old l) (cons old (cons new l))))
;; Allows you to define subst using insert-g
(setq seqS (lambda (new old l) (cons new l)))
;; Allows you to define rember using insert-g
(setq seqrem (lambda (new old l) l))

(defun atom-to-function (x)
  (cond
   ((eq? x '+) 'o+)
   ((eq? x 'X) 'ox)
   (else 'o^)))

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   (else (funcall (atom-to-function (operator nexp))
		  (value (1st-sub-exp nexp))
		  (value (2nd-sub-exp nexp))))))

(defun multirember-f (test?)
  (lambda (a lat)
    (cond
     ((null? lat) ())
     ((funcall test? (car lat) a)
      (funcall (multirember-f test?) a (cdr lat)))
     (else (cons (car lat)
		 (funcall (multirember-f test?) a (cdr lat)))))))

(defun multirember&co (a lat col)
  (cond
   ((null? lat) (funcall col () ()))
   ((eq? (car lat) a)
    (multirember&co a
		    (cdr lat)
		    (lambda (newlat seen)
		      (funcall col newlat
			       (cons (car lat) seen)))))
   (else
    (multirember&co a
		    (cdr lat)
		    (lambda (newlat seen)
		      (funcall col (cons (car lat) newlat)
			       seen))))))

;; My attempt to write this function based on my first instinct for
;; how it "should" be written.
(defun multirember&co2 (a lat f)
  ((lambda (newlatAndSeen)
     (funcall f (first newlatAndSeen) (second newlatAndSeen)))
   (multirember&co2helper a lat () ())))

(defun multirember&co2helper (a lat newlat seen)
  (cond
   ((null? lat) (build (reverse newlat) seen))
   ((eq? (car lat) a)
    (multirember&co2helper a
			   (cdr lat)
			   newlat
			   (cons (car lat) seen)))
   (else
    (multirember&co2helper a
			   (cdr lat)
			   (cons (car lat) newlat)
			   seen))))

(defun multiinsertLR (new oldL oldR lat)
  (cond
   ((null? lat) ())
   ((eq? (car lat) oldL)
    (cons new
	  (cons oldL
		(multiinsertLR new oldL oldR (cdr lat)))))
   ((eq? (car lat) oldR)
    (cons oldR
	  (cons new
		(multiinsertLR new oldL oldR (cdr lat)))))
   (else (cons (car lat)
	       (multiinsertLR new oldL oldR (cdr lat))))))

(defun multiinsertLR&co (new oldL oldR lat col)
  (cond
   ((null? lat) (funcall col (quote ()) 0 0))
   ((eq? (car lat) oldL)
    (multiinsertLR&co new
		      oldL
		      oldR
		      (cdr lat)
		      (lambda (newlat L R)
			(funcall col (cons new (cons oldL newlat)) (add1 L) R))))
   ((eq? (car lat) oldR)
    (multiinsertLR&co new
		      oldL
		      oldR
		      (cdr lat)
		      (lambda (newlat L R)
			(funcall col (cons oldR (cons new newlat)) L (add1 R)))))
   (else
    (multiinsertLR&co new
		      oldL
		      oldR
		      (cdr lat)
		      (lambda (newlat L R)
			(funcall col (cons (car lat) newlat) L R))))))

(defun even? (n)
  (o= (ox (o/ n 2) 2) n))

(defun evens-only* (l)
  (cond
   ((null? l) ())
   ((atom? (car l))
    (cond
     ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
     (else (evens-only* (cdr l)))))
   (else (cons (evens-only* (car l))
	       (evens-only* (cdr l))))))

;; My first attempt at writing evens-only*&co on my own. Holy shit
;; this is crazy. Can this sort of thing be done at all in a typed
;; language? Because with our previous lambda's we used "col" in the
;; definition so I think the types would work out. But here we've
;; passed in lambda's which do not use "col" at all, they just build a
;; list. EDIT: Turns out the book (in its vast wisdom) has a better
;; implementation which fixes this problem.
(defun evens-only*&co-first (l col)
  (cond
   ((null? l) (funcall col () 1 0))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (evens-only*&co-first (cdr l)
			    (lambda (newl p s)
			      (funcall col (cons (car l) newl)
				       (ox (car l) p)
				       s))))
     (else
      (evens-only*&co-first (cdr l)
			    (lambda (newl p s)
			      (funcall col newl p (o+ (car l) s)))))))
   (else
    ((lambda (carres cdrres)
       (funcall col (cons (car carres) (car cdrres))
		(ox (car (cdr carres)) (car (cdr cdrres)))
		(o+ (car (cdr (cdr carres))) (car (cdr (cdr cdrres))))))
     (evens-only*&co-first (car l) (lambda (newl p s)
				     (cons newl (cons p (cons s ())))))
     (evens-only*&co-first (cdr l) (lambda (newl p s)
				     (cons newl (cons p (cons s ())))))))))

;; HOLY SHIT. Now this is ridonkulous. And this could be done in a
;; typed language as well.
(defun evens-only*&co (l col)
  (cond
   ((null? l) (funcall col () 1 0))
   ((atom? (car l))
    (cond
     ((even? (car l))
      (evens-only*&co
       (cdr l)
       (lambda (newl p s)
	 (funcall col (cons (car l) newl)
		  (ox (car l) p)
		  s))))
     (else
      (evens-only*&co
       (cdr l)
       (lambda (newl p s)
	 (funcall col newl p (o+ (car l) s)))))))
   (else
    (evens-only*&co
     (car l)
     (lambda (al ap as)
       (evens-only*&co
	(cdr l)
	(lambda (dl dp ds)
	  (funcall col
		   (cons al dl)
		   (ox ap dp)
		   (o+ as ds)))))))))

;; Chapter 9
(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(defun keep-looking (a sorn lat)
  (cond
   ((number? sorn) (keep-looking a (pick sorn lat) lat))
   (else (eq? a sorn))))

(defun shift (pair)
  (build (first (first pair))
	 (build (second (first pair))
		(second pair))))

(defun align (pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (align (shift pora)))
   (else (build (first pora)
		(align (second pora))))))

(defun shuffle (pora)
  (cond
   ((atom? pora) pora)
   ((a-pair? (first pora))
    (shuffle (revpair pora)))
   (else (build (first pora)
		(shuffle (second pora))))))

(defun C (n)
  (cond
   ((one? n) 1)
   (else
    (cond
     ((even? n) (C (o/ n 2)))
     (else (C (add1 (ox 3 n))))))))

(defun A (n m)
  (cond
   ((zero? n) (add1 m))
   ((zero? m) (A (sub1 n) 1))
   (else (A (sub1 n)
	    (A n (sub1 m))))))

;; Elisp's treatment of functions as something different than a plain
;; old value makes these implementations not super aesthetically
;; pleasing. These things are complicated enough as it is!
(setq Y (lambda (f)
	  ((lambda (g) (funcall g g))
	   (lambda (g)
	     (funcall f (lambda (x) (funcall (funcall g g) x)))))))

(setq almost-factorial (lambda (factorial)
			 (lambda (n)
			   (cond
			    ((zero? n) 1)
			    (else (* n (funcall factorial (- n 1))))))))

(setq factorial (funcall Y almost-factorial))

(funcall factorial 6)

(setq crazy-length ((lambda (f)
		      ((lambda (g) (funcall g g))
		       (lambda (g)
			 (funcall f (lambda (x) (funcall (funcall g g) x))))))
		    (lambda (length)
		      (lambda (l)
			(cond
			 ((null? l) 0)
			 (else (add1 (funcall length (cdr l)))))))))

(funcall crazy-length '(a b c d e))

;; Chapter 10

(defun new-entry (s1 s2)
  (build s1 s2))

;; TODO: Rewrite all/most of the code samples from this book in the
;; interpreter that they define at the end of the book!!

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name
			(first entry)
			(second entry)
			entry-f))

(defun lookup-in-entry-help (name names values entry-f)
  (cond
   ((null? names) (funcall entry-f name))
   ((eq? (car names) name) (car values))
   (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))

(defun extend-table (x y)
  (cons x y))

(defun lookup-in-table (name table table-f)
  (cond
   ((null? table) (funcall table-f name))
   (else
    (lookup-in-entry name
		     (car table)
		     (lambda (name)
		       (lookup-in-table name
					(cdr table)
					table-f))))))
					
(defun expression-to-action (e)
  (cond
   ((atom? e) (atom-to-action e))
   (else (list-to-action e))))

;; NOTE: Originally I thought I would be able to define pretty much
;; the exact same language as they do in the book including using #t
;; and #f for boolean values but this is not true. It makes sense (and
;; is pretty damn cool!) after I explain.

;; My current understanding is that going from some language
;; (represented as a string) to running code involves 2 steps:

;; 1. String -> Abstract Syntax Tree (AST). We do this transformation
;; because its much easier to work with structures than strings. For a
;; language like C, this code: 1 + 2 * 3 could possibly (and I'm
;; straight up guessing) be transformed into a tree like:

;;   +
;; 1    *
;;    2   3

;; A structure like that is straightforward to parse and determine the
;; answer.

;; 2. AST -> Executable. This involves iterating over the AST and
;; either:

;; 2a. Calling functions/consructs in another language as you go.
;; This, I believe, would be referred to as an interpreter.

;; 2b. Converting the AST into something runnable. This could be
;; machine code or another language.

;; The neat thing is that since they are defining another language,
;; let's call it LittleSchemer, with the same syntax as Scheme they
;; leverage their Scheme interpreter to perform the String -> AST
;; conversion which means they just need to do the AST -> Executable
;; conversion. It is also, very *very*, important to note that the AST
;; here is not some esoteric data structure, its just a good'ol
;; S-expression, which we are very used to manipulating, making this
;; AST -> Executable conversion a lot more straightforward.

;; So the reason I cannot define the same exact language using #t and
;; #f boolean values is that I'm leveraging the emacs lisp interpreter
;; to do the String-(i.e. raw unstructured elisp) -> AST conversion
;; and elisp does not seem to allow '#' (when doing something like
;; (quote #t) I get the error: read--expression: Invalid read syntax:
;; "#").

;; Some side thoughts:

;; 1. The "value" function we are creating is the interpreter meaning
;; that whatever we pass as an argument to this function is the
;; LittleSchemer language.

;; 2. When people say that the syntax of Lisp IS an AST I *think* they
;; mean that doing String-(i.e. the raw lisp code) -> AST is a pretty
;; straightforward conversion to the point where the two
;; representations are practically the same. I have also heard people
;; say things like, probably a bit jokingly, "lisp has no syntax" for
;; the same reason (to elaborate here: the concept of "syntax" only
;; exists as a String not as an AST and since the raw String of Lisp
;; is so close to an AST they just say it has no syntax). I personally
;; think this is a bit misleading since when you say "syntax" people
;; think of the characters you type (at least I do). Put another way,
;; people probably say these sorts of things because *semantically*
;; the String for your Lisp code is the same thing as an S-expression.
;; For example this math in C "1 + 2 * 3" does not really reflect in
;; any way what order those numbers get combined in but the AST that
;; it gets converted to will.

;; 3. You can write Lisp code which modifies lisp code because lisp
;; code is a list. This is a really cool idea and is probably
;; something you never imagined doing in other programming languages.
;; This ability makes things like "defmacro" possible.

;; 4. Practically it might not make sense to write a Lisp interpreter
;; in Lisp since you already have the interpreter in C so you are
;; creating one more layer of indirection that does not actually
;; accomplish anything i.e. to write an interpreter you'd probably
;; write a function "eval" in Lisp which takes an expression like
;; (cons x y), so you'd call it like (eval '(cons x y)), and it
;; converts that to the appropriate function calls in Lisp which will
;; just be (cons x y). So you've transformed the data into the same
;; thing... But its pretty cool that you can do that without needing
;; very much code. Although I think its cool I also think defining a
;; lisp interpreter in Lisp feels a bit like cheating because the
;; interpreter does not need to do IO or handle the String -> AST
;; conversion.

;; 5. Since lisp is so comfortable manipulating trees (its lists are
;; basically trees), maybe it would be a good language for language
;; experimentation. I think this is what Racket is all about and would
;; like to learn more about it:
;; https://en.wikipedia.org/wiki/Racket_(programming_language),
;; https://practicaltypography.com/why-racket-why-lisp.html

(defun atom-to-action (e)
  (cond
   ((number? e) *const)
   ((eq? e 'true) *const)
   ((eq? e 'false) *const)
   ((eq? e 'cons) *const)
   ((eq? e 'car) *const)
   ((eq? e 'cdr) *const)
   ((eq? e 'null?) *const)
   ((eq? e 'eq?) *const)
   ((eq? e 'atom?) *const)
   ((eq? e 'zero?) *const)
   ((eq? e 'add1) *const)
   ((eq? e 'sub1) *const)
   ((eq? e 'number?) *const)
   (else *identifier)))

(defun list-to-action (e)
  (cond
   ((atom? (car e))
    (cond
     ((eq? (car e) 'quote) *quote)
     ((eq? (car e) 'lambda) *lambda)
     ((eq? (car e) 'cond) *cond)
     (else *application)))
   (else *application)))

(defun value (e)
  (meaning e ()))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(setq *const
      (lambda (e table)
	(cond
	 ((number? e) e)
	 ((eq? e 'true) t)
	 ((eq? e 'false) nil)
	 (else (build 'primitive e)))))

(setq *quote
      (lambda (e table)
	(second e)))

(setq *identifier
      (lambda (e table)
	(lookup-in-table e table (lambda (name) (car ())))))

(setq *lambda
      (lambda (e table)
	(build 'non-primitive
	       (cons table (cdr e)))))

(defun table-of (l) (first l))
(defun formals-of (l) (second l))
(defun body-of (l) (car (cdr (cdr l))))

(defun evcon (lines table)
  (cond
   ((eq? (question-of (car lines)) 'else)
    (meaning (answer-of (car lines)) table))
   ((meaning (question-of (car lines)) table)
    (meaning (answer-of (car lines)) table))
   (else (evcon (cdr lines) table))))

(defun question-of (l) (first l))
(defun answer-of (l) (second l))

(setq *cond
      (lambda (e table)
	(evcon (cond-lines-of e) table)))

(defun cond-lines-of (l) (cdr l))

(defun evlis (args table)
  (cond
   ((null? args) ())
   (else (cons (meaning (car args) table)
	       (evlis (cdr args) table)))))

(setq *application
      (lambda (e table)
	(lsapply
	 (meaning (function-of e) table)
	 (evlis (arguments-of e) table))))

(defun function-of (l) (car l))
(defun arguments-of (l) (cdr l))

(defun primitive? (l)
  (eq? (first l) 'primitive))

(defun non-primitive? (l)
  (eq? (first l) 'non-primitive))

(defun lsapply (fun vals)
  (cond
   ((primitive? fun)
    (apply-primitive (second fun) vals))
   ((non-primitive? fun)
    (apply-closure (second fun) vals))))

(defun apply-primitive (name vals)
  (cond
   ((eq? name 'cons)
    (cons (first vals) (second vals)))
   ((eq? name 'car)
    (car (first vals)))
   ((eq? name 'cdr)
    (cdr (first vals)))
   ((eq? name 'null?)
    (null? (first vals)))
   ((eq? name 'eq?)
    (eq? (first vals) (second vals)))
   ((eq? name 'atom?)
    (another-atom? (first vals)))
   ((eq? name 'zero?)
    (zero? (first vals)))
   ((eq? name 'add1)
    (add1 (first vals)))
   ((eq? name 'sub1)
    (sub1 (first vals)))
   ((eq? name 'number?)
    (number? (first vals)))))

(defun another-atom? (x)
  (cond
   ((atom? x) t)
   ((null? x) nil)
   ((eq? (car x) 'primitive) t)
   ((eq? (car x) 'non-primitive) t)
   (else nil)))

(defun apply-closure (closure vals)
  (meaning (body-of closure)
	   (extend-table
	    (new-entry (formals-of closure) vals)
	    (table-of closure))))

;; Some examples from the book rewritten in our "LittleSchemer"
;; language! It makes me think of these funny sorts of articles:
;; https://medium.com/@webseanhickey/the-evolution-of-a-software-engineer-db854689243
(value
 (quote
  ((lambda (Y1 Y2 Y3)
     ((lambda (lat? member? rember add rember* multirember&co)
	(cons
	 (lat? (quote (1 2 3 4)))
	 (cons
	  (member? (quote orange) (quote (apple orange)))
	  (cons
	   (rember (quote orange) (quote (apple orange mango)))
	   (cons
	    (add 33 9)
	    (cons
	     (rember* 1 (quote (1 2 (1 3 (1) 4) 9)))
	     (cons
	      (multirember&co 1 (quote (1 2 3 4 5)) cons)
	      (quote ()))))))))
      (Y1 (lambda (lat?)
      	    (lambda (l)
      	      (cond
      	       ((null? l) true)
      	       ((atom? (car l)) (lat? (cdr l)))
      	       (else false)))))
      (Y2 (lambda (member?)
	    (lambda (a lat)
	      (cond
	       ((null? lat) false)
	       ((eq? (car lat) a) true)
	       (else (member? a (cdr lat)))))))
      (Y2 (lambda (rember)
      	    (lambda (a lat)
      	      (cond
      	       ((null? lat) (quote ()))
      	       ((eq? (car lat) a) (cdr lat))
      	       (else (cons (car lat) (rember a (cdr lat))))))))
      (Y2 (lambda (add)
      	    (lambda (n m)
      	      (cond
      	       ((zero? m) n)
	       (else (add1 (add n (sub1 m))))))))
      (Y2 (lambda (rember*)
	    (lambda (a l)
	      (cond
	       ((null? l) (quote ()))
	       ((atom? (car l))
		(cond
		 ((eq? (car l) a) (rember* a (cdr l)))
		 (else (cons (car l) (rember* a (cdr l))))))
	       (else (cons (rember* a (car l))
			   (rember* a (cdr l))))))))
      (Y3 (lambda (multirember&co)
	    (lambda (a lat col)
	      (cond
	       ((null? lat) (col (quote ()) (quote ())))
	       ((eq? (car lat) a)
		(multirember&co a
				(cdr lat)
				(lambda (newlat seen)
				  (col newlat
				       (cons (car lat) seen)))))
	       (else
		(multirember&co a
				(cdr lat)
				(lambda (newlat seen)
				  (col (cons (car lat) newlat)
				       seen))))))))))
		    
   (lambda (f)
     ((lambda (g) (g g))
      (lambda (g) (f (lambda (x) ((g g) x))))))
   (lambda (f)
     ((lambda (g) (g g))
      (lambda (g) (f (lambda (x y) ((g g) x y))))))
   (lambda (f)
     ((lambda (g) (g g))
      (lambda (g) (f (lambda (x y z) ((g g) x y z)))))))))
