 (define atom?
   (lambda (x)
     (and (not (pair? x))
          (not (null? x)))))
 
 (define lat?
   (lambda (l)
     (cond
       ((null? l) #t)
       ((atom? (car l)) (lat? (cdr l)))
       (else #f))))
 
 (define member?
   (lambda (x lat)
     (cond
       ((null? lat) #f)
       (else (or (eq? (car lat) x)
                 (member? x (cdr lat)))))))
 
 (define rember
   (lambda (x lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) x) (cdr lat))
       (else (cons (car lat)
                   (rember x (cdr lat)))))))
 
 (define firsts
   (lambda (l)
     (cond
       ((null? l) '())
       (else (cons (car (car l)) 
                   (firsts (cdr l)))))))
 
 (define insertR
   (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat) 
                   (insertR new old (cdr lat)))))))
 
 (define insertL
   (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new lat))
       (else (cons (car lat) (insertL new old (cdr lat)))))))
 
 (define subst
   (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new (cdr lat)))
       (else (cons (car lat) (subst new old (cdr lat)))))))
 
 (define subst2
   (lambda (new o1 o2 lat)
     (cond 
       ((null? lat) '())
       ((or (eq? (car lat) o1)
            (eq? (car lat) o2))
        (cons new (cdr lat)))
       (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))
 
 (define multirember
   (lambda (a lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) a) (multirember a (cdr lat)))
       (else (cons (car lat) (multirember a (cdr lat)))))))
 
 (define multiinsertR
   (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons old 
                                  (cons new 
                                        (multiinsertR new old (cdr lat)))))
       (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
 
 (define multiinsertL
   (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new
                                 (cons old
                                       (multiinsertL new old (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertL new old (cdr lat)))))))
 
 (define multisubst
   (lambda (new old lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat) (multisubst new old (cdr lat)))))))
 
 (define add1
   (lambda (n)
     (+ n 1)))
 
 (define sub1
   (lambda (n)
     (- n 1)))
 
 (define o+ 
   (lambda (a b)
     (cond
       ((zero? a) b)
       (else (add1 (o+ (sub1 a) b))))))
 
 (define o-
   (lambda (a b)
     (cond
      ((zero? b) a)
      (else (sub1 (o- a (sub1 b)))))))
 
 (define addtup
   (lambda (tup)
     (cond
       ((null? tup) 0)
       (else (o+ (car tup) (addtup (cdr tup)))))))
 
 (define ox
   (lambda (a b)
     (cond
       ((eq? b 1) a)
       (else (o+ a (ox a (sub1 b)))))))
 
 (define ox-book
   (lambda (a b)
     (cond
       ((zero? b) 0)
       (else (o+ a (ox a (sub1 b)))))))
 
 (define tup+
   (lambda (tup1 tup2)
     (cond
       ((null? tup1) tup2)
       ((null? tup2) tup1)
       (else (cons (o+ (car tup1) (car tup2))
                   (tup+ (cdr tup1) (cdr tup2)))))))
 
 (define o>
   (lambda (n m)
     (cond
       ((zero? n) #f)
       ((zero? m) #t)
       (else
        (o> (sub1 n) (sub1 m))))))
 
 (define o<
   (lambda (n m)
     (cond
       ((zero? m) #f)
       ((zero? n) #t)
       (else
        (o< (sub1 n) (sub1 m))))))
 
 (define o=
   (lambda (n m)
     (cond
       ((zero? m) (zero? n))
       ((zero? n) #f)
       (else
        (o= (sub1 n) (sub1 m))))))
 
 (define o=2
   (lambda (n m)
     (cond
       ((> n m) #f)
       ((< n m) #f)
       (else #t))))
 
 (define pow
   (lambda (n m)
     (cond
       ((zero? m) 1)
       (else 
        (ox n (pow n (sub1 m)))))))
 
 (define o/
   (lambda (n m)
     (cond 
       ((< n m) 0)
       (else
        (add1 (o/ (- n m) m))))))
 
 (define olength
   (lambda (lat)
     (cond
       ((null? lat) 0)
       (else
        (add1 (olength (cdr lat)))))))
 
 (define pick
   (lambda (n lat)
     (cond
       ((zero? (sub1 n)) (car lat))
       (else
        (pick (sub1 n) (cdr lat))))))
 
 (define rempick
   (lambda (n lat)
     (cond
       ((zero? (sub1 n)) (cdr lat))
       (else
        (cons (car lat) 
              (rempick (sub1 n) (cdr lat)))))))

 
 (define no-nums
   (lambda (lat)
     (cond
       ((null? lat) '())
       ((number? (car lat)) (no-nums (cdr lat)))
       (else
        (cons (car lat) (no-nums (cdr lat)))))))
 
 (define all-nums
   (lambda (lat)
     (cond
       ((null? lat) '())
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else
        (all-nums (cdr lat))))))
 
 (define eqan?
   (lambda (a b)
     (cond
       ((and (number? a) (number? b))
        (= a b))
       ((or (number? a) (number? b))
        #f)
       (else
        (eq? a b)))))
 
 (define occur
   (lambda (a lat)
     (cond
       ((null? lat) 0)
       ((eq? (car lat) a) (add1 (occur a (cdr lat))))
       (else
        (occur a (cdr lat))))))
 
 (define one?
   (lambda (n)
     (= n 1)))
 
 (define rember*
   (lambda (a l)
     (cond
       ((null? lat) '())
       ((atom? (car l))
        (cond
          ((eq? (car l) a) (rember* a (cdr l)))
          (else
           (cons (car l) (rember* a (cdr l))))))
       (else
        (cons (rember* a (car l))
              (rember* a (cdr l)))))))
 
 (define insertR*
   (lambda (new old l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? (car l) old) (cons old (cons new 
                                             (insertR* new old (cdr l)))))
          (else
           (cons (car l) (insertR* new old (cdr l))))))
       (else
        (cons (insertR* new old (car l))
              (INSERTR* new old (cdr l)))))))
 
 (define occur*
   (lambda (a l)
     (cond
       ((null? l) 0)
       ((atom? (car l))
        (cond
          ((eq? (car l) a) (add1 (occur* a (cdr l))))
          (else
           (occur* a (cdr l)))))
       (else
        (+ (occur* a (car l)) (occur* a (cdr l)))))))
 
 (define subst*
   (lambda (new old l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? (car l) old) (cons new (subst* new old (cdr l))))
          (else
           (cons (car l) (subst* new old (cdr l))))))
       (else
        (cons (subst* new old (car l))
              (subst* new old (cdr l)))))))
 
 (define insertL*
   (lambda (new old l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? (car l) old) (cons new (cons old (insertL new old (cdr l)))))
          (else
           (cons (car l) (insertL new old (cdr l))))))
       (else
        (cons (insertL new old (car l))
              (insertL new old (cdr l)))))))
 
 (define member*
   (lambda (a l)
     (cond
       ((null? l) #f)
       ((atom? (car l))
        (or (eq? (car l) a)
            (member* a (cdr l))))
       (else
        (or (member* a (car l))
            (member* a (cdr l)))))))
      
 (define leftmost
   (lambda (l)
     (cond
       ((atom? (car l)) (car l))
       (else
        (leftmost (car l))))))
 
 (define eqlist?
   (lambda (l1 l2)
     (cond
       ((and (null? l1) (null? l2)) #t)
       ((or (null? l1) (null? l2)) #f)
       ((and (atom? (car l1)) (atom? (car l2)))
        (cond
          ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
          (else #f)))
       ((or (atom? (car l1)) (atom? (car l2)))
        #f)
       (else
        (and (eqlist? (car l1) (car l2))
             (eqlist? (cdr l1) (cdr l2)))))))
 
 
 (define numbered?
   (lambda (aexp)
     (cond
       ((atom? (car aexp)) (number? (car aexp)))
       (else
        (and (numbered? (car aexp))
             (numbered? (car (cdr (cdr aexp)))))))))

 
 (define operator
   (lambda (aexp)
     (car aexp)))
 
 (define first-sub-expr
   (lambda (aexp)
     (car (cdr aexp))))
 
 (define second-sub-expr
   (lambda (aexp)
     (car (cdr (cdr aexp)))))
 
 (define myzero?
   (lambda (n)
     (null? n)))
 
 (define multirember-f
   (lambda (test?)
     (lambda (a l)
       (cond
         ((null? l) '())
         ((test? (car l) a) ((multirember-f test?) a (cdr l)))
         (else
          (cons (car l) ((multirember-f test?) a (cdr l))))))))
 
 (define multirember-eq (multirember-f eq?))
 
 (define myadd1
   (lambda (n)
     (cons '() n)))
 
 (define subset?
   (lambda (s1 s2)
     (cond
       ((null? s1) #t)
       ((and
         (member? (car s1) s2)
         (subset? (cdr s1) s2))))))
 
 (define eqset?
   (lambda (s1 s2)
     (and (subset? s1 s2)
          (subset? s2 s20))))
 
 (define intersect?
   (lambda (s1 s2)
     (cond
       ((null? s1) #f)
       ((or 
         (member? (car s1) s2) #t)
         (intersect? (cdr s1) s2)))))
 
 (define intersect
   (lambda (s1 s2)
     (cond
       ((null? s1) '())
       ((member? (car s1) s2)
        (cons (car s1) (intersect (cdr s1) s2)))
       (else
        (intersect (cdr s1) s2)))))
 
 (define union
   (lambda (s1 s2)
     (cond
       ((null? s1) s2)
       ((member? (car s1) s2) (union (cdr s1) s2))
       (else
        (cons (car s1) (union (cdr s1) s2))))))
 
 (define intersect-all
   (lambda (l)
     (cond
       ((null? (cdr l)) (car l))
       (else
        (intersect (car l) (intersect-all (cdr l)))))))
 
 (define a-pair?
   (lambda (x)
     (cond
       ((null? x) #f)
       ((atom? x) #f)
       ((null? (cdr x)) #f)
       ((null? (cdr (cdr x))) #t))))
 
 (define first
   (lambda (p)
     (car p)))
 
 (define second
   (lambda (p)
     (car (cdr p))))
 
 (define third
   (lambda (l)
     (car (cdr (cdr l)))))
 
 (define build
   (lambda (x y)
     (cons x (cons y '()))))
 
 (define set?
   (lambda (lat)
     (cond
       ((null? lat) #t)
       ((member? (car lat) (cdr lat)) #f)
       (else
        (set? (cdr lat))))))
       
 (define fun?
   (lambda (rel)
     (set? (firsts rel))))
 
 (define revrel
   (lambda (rel)
     (cond
       ((null? rel) '())
       ((cons (build (second (car rel)) (first (car rel)))
              (revrel (cdr rel)))))))
 
 (define seconds
   (lambda (rel)
     (cond
       ((null? rel) '())
       ((cons (second (car rel))
              (seconds (cdr rel)))))))
 
 (define fullfun 
   (lambda (rel)
     (set? (seconds rel))))
 
 (define one-to-one?
   (lambda (rel)
     (fun? (revrel rel))))
 
 (define rember-f
   (lambda (test?)
     (lambda (a l)
       (cond
         ((null? l) '())
         ((test? (car l) a) (cdr l))
         (else
          (cons (car l) ((rember-f test?) a (cdr l))))))))
 
 (define insertL-f
   (lambda (test?)
     (lambda (new old l)
       (cond
         ((null? l) '())
         ((test? (car l) old) (cons new (cons old (cdr l))))
         (else
          (cons (car l) ((insertL-f test?) new old (cdr l))))))))
 
 (define seqL
   (lambda (new old l)
     (cons new (cons old l))))
 
 (define seqR
   (lambda (new old l)
     (cons old (cons new l))))
 
 (define insert-g
   (lambda (seq-f)
     (lambda (new old l)
       (cond
         ((null? l) '())
         ((eq? (car l) old) (seq-f new old (cdr l)))
         (else
          (cons (car l) ((insert-g seq-f) new old (cdr l))))))))
 
 (define subst
   (insert-g
    (lambda (new old l)
      (cons new l))))
 
 (define atom-to-function
   (lambda (a)
     (cond
       ((eq? a '+) o+)
       ((eq? a '-) o-)
       (else pow))))
 
 (define value
   (lambda (nexp)
     (cond
       ((atom? nexp) nexp)
       (else
        ((atom-to-function (operator nexp))
           (value (first-sub-expr nexp))
           (value (second-sub-expr nexp)))))))
 
 (define eq?-c
   (lambda (c)
     (lambda (k)
       (eq? c k))))
 
 (define eq?-tuna
   (eq?-c 'tuna))
 
 (define multiremberT
   (lambda (test? l)
     (cond
       ((null? l) '())
       ((test? (car l)) (multiremberT test? (cdr l)))
       (else
        (cons (car l) (multiremberT test? (cdr l)))))))
 
 (define multirember&co
   (lambda (a lat col)
     (cond
       ((null? lat) (col '() '()))
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
                               seen)))))))
 
 (define multiinsertLR
   (lambda (new oldL oldR lat)
     (cond
       ((null? lat) '())
       ((eq? (car lat) oldL)
        (cons new 
              (cons oldL
                    (multiinsertLR new oldL oldR (cdr lat)))))
       ((eq? (car lat) oldR)
        (cons oldR
              (cons new
                    (multiinsertLR new oldL oldR (cdr lat)))))
       (else
        (cons (car lat)
              (multiinsertLR new oldL oldR (cdr lat)))))))
 
 (define multiinsertLR&co
   (lambda (new oldL oldR lat col)
     (cond
       ((null? lat)
        (col '() 0 0))
       ((eq? (car lat) oldL)
        (multiinsertLR&co new oldL oldR 
                          (cdr lat)
                          (lambda (newlat L R)
                            (col (cons new
                                       (cons oldL newlat))
                                 (+ L 1)
                                 R))))
       ((eq? (car lat) oldR)
        (multiinsertLR&co new oldL oldR
                          (cdr lat)
                          (lambda (newlat L R)
                            (col (cons oldR
                                       (cons new new))
                                 L
                                 (+ R 1)))))
       (else
        (multiinsertLR&co oldL oldR
                          (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat)
                                 L
                                 R)))))))
 
 (define evens-only*
   (lambda (l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((even? (car l)) (cons (car l)
                                 (evens-only* (cdr l))))
          (else (evens-only* (cdr l)))))
       (else
        (cons (evens-only* (car l))
              (evens-only* (cdr l)))))))
 
 (define evens-only*&co
   (lambda (l col)
     (cond
       ((null? l) (col '() 1 0))
       ((atom? (car l))
        (cond
          ((even? (car l)) (evens-only*&co
                            (cdr l)
                            (lambda (evens prod sum)
                                (col
                                 (cons (car l) evens)
                                 (* (car l) prod)
                                 sum))))
          (else
           (evens-only*&co
            (cdr l)
            (lambda (evens prod sum)
              (col
               evens
               prod
               (+ (car l) sum)))))))
       (else
        (evens-only*&co
         (car l)
         (lambda (evens prod sum)
           (evens-only*&co (cdr l)
                           (lambda (e p s)
                             (col
                              (cons evens e)
                              (* prod p)
                              (+ s sum))))))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      ((eq? sorn a) #t)
      (else #f))))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else
       (build (first pora)
              (align (second pora)))))))

(define eternity
  (lambda (x)
    (eternity x)))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-helper
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-helper
  (lambda (name names values entry-f)
    (cond 
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else
       (lookup-in-entry-helper name 
                               (cdr names) 
                               (cdr values)
                               entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry
        name
        (car table)
        (lambda (name)
          (lookup-in-table name (cdr table) table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e)    *const)
      ((eq? e #t)     *const)
      ((eq? e #f)     *const)
      ((eq? e 'null?) *const)
      ((eq? e 'cons)  *const)
      ((eq? e 'car)   *const)
      ((eq? e 'cdr)   *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1)  *const)
      ((eq? e 'sub1)  *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'number?) *const)
      ((eq? e 'eq?)     *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote)
          *quote)
         ((eq? (car e) 'lambda)
          *lambda)
         ((eq? (car e) 'cond)
          *cond)
         (else *application)))
      (else *application))))

(define value-e
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else
       (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define table-of first)
(define formals-of second)
(define body-of third)

(define else?
  (lambda (e)
    (cond
      ((atom? e) (eq? e 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)))
       (meaning (answer-of (car lines))))
      (else
       (evcon (cdr lines) table)))))

(define cond-lines-of cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evlist
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       (cons (meaning (car args) table)
             (evlist (cdr args) table))))))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define *application
  (lambda (e table)
    (apply2
     (meaning (function-of e) table)
     (evlist (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (car l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (car l) 'non-primitive)))

(define apply2
  (lambda (f vals)
    (cond
      ((primitive? f) 
       (apply-primitive (second f)
                        vals))
      ((non-primitive? f)
       (apply-closure
        (second f) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive #t))
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))