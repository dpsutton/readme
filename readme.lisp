;;;; readme.lisp

(in-package #:readme)

;; we want to generate random buzzword-y readme files
;; # Title
;; ## Introduction
;; RandomParagraph
;; ## Directions
;; RandomParagraph
;; ## InstallationInstructions
;; RandomParagraph

;; README => Title  Introduction RandomParagraph
;;                  Directions RandomParagraph Installation  RandomParagraph
;; And we will use the following grammar :
;; Title           => Nounr Prep Platform
;; Introduction    => TeaserSentence
;; Directions      => SolveSentence
;; Installation    => QuestionVerb Prep InstallVerb
;; TeaserSentence  => QuestionVerb Pronoun DesireVerb
;; SolveSentence   => InstallVerb Noun-Phrase
;; RandomParagraph => Sentence Sentence*
;; Sentence*       => nil | Sentence Sentence*
;; Sentence        => Noun-Phrase Verb-Phrase
;; Noun-Phrase     => Article Adj* Noun PP*
;; Verb-Phrase     => Verb Noun-Phrase PP*
;; PP*             => nil | PP PP*
;; Adj*            => nil | Adj Adj*
;; PP              => Prep Noun-Phrase
;; Nounr           => Lispr | Compilr | ...
;; Platform        => linux | C | osx | ...
;; Prep            => to in by with on
;; Adj             => shiny | latest | revolutionary | ...
;; Article         => the | a
;; InstallVerb     => install | compile | load | ...
;; QuestionVerb    => why | how | ...
;; DesireVerb      => should | desire | ...
;; Noun            => software | package | system | ...
;; Pronoun         => you | they | ...
;; Verb            => revolutionize | compile | optimize | ...

(defparameter *readme-simple-grammar*
  '((readme           -> (title introduction
                          random-paragraph directions random-paragraph
                          installation-instructions random-paragraph))
    (title            -> (nounr prep platform))
    (introduction     -> (teaser))
    (directions       -> (solve-sentence))
    (installation     -> (question-verb prep install-verb))
    (teaser           -> (question-verb pronoun desire-verb))
    (solve-sentence   -> (install-verb noun-phrase))
    (random-paragraph -> (sentence sentence*))
    (sentence*        -> () (sentence sentence*))
    (sentence         -> (noun-phrase verb-phrase punctuation))
    (noun-phrase      -> (article adj* noun PP*))
    (verb-phrase      -> (verb noun-phrase PP*))
    (PP*              -> () (PP PP*))
    (adj*             -> () (adj adj*))
    (PP               -> (prep noun-phrase))
    (nounr            -> Lispr Compilr Bottlr Quotr)
    (platform         -> C linux javascript lisp react)
    (prep             -> to in by with on)
    (adj              -> shiny latest revolutionary optimized)
    (article          -> the a)
    (punctuation      -> period question exclamation)
    (install-verb     -> install compile load clone)
    (question-verb    -> how why)
    (desire-verb      -> should desire)
    (noun             -> software package system)
    (pronoun          -> you they)
    (verb             -> compile parse optimize uglify)))


(defvar *grammar* *readme-simple-grammar*)

(defun generate (phrase)
  "Generate a random readme or phrase."
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate the AST of the readme."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun emit-markdown (ast)
  (list-to-joined-string (mapcar #'string (mapcar #'second (rest ast)))))

;; note that '(octo) and '(newline) make sense after a second of
;; reflection since these are abstract syntax trees, and these tokens
;; have an empty body. But this lets us match on (type (val ...) ...)
;; but these forms just have empty bodies.
(defun emit (ast)
  "emit the markdown syntax"
  (cond ((type-equal ast 'readme)
         (mappend #'emit (rest ast)))
        ((type-equal ast 'title)
         ;; we can't leave the title token on there so rename
         ;; title-with-newline otherwise it would keep matching on the
         ;; same rule
         (emit (append (cons 'title-with-newline
                             ;; prepend an octothorpe # for markdown title
                             (append (list '(octo)) (rest ast)))
                       ;; append a newline
                                        ;TODO: make newlining a function
                       (list '(newline)))))
        ((type-equal ast 'newline)
         (list (string #\linefeed)))
        ((type-equal ast 'octo)
         (list (string "#")))
        ;; got a problem here: i've assumed lists of length two are
        ;; (tag token) but i have (introduciton (.....)) which has
        ;; length two. so i'm calling string on it, and
        ;; kablooey. might have to alter grammar, we'll see. it's 2am
        ((and (listp ast) (> (length ast) 2))
         (mappend #'emit (rest ast)))
        (t (list (string (second ast))))))

(defun type-equal (ast type)
  (equalp (car ast) type))

;; grammar interacting rules
(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))
(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))
(defun symbol-lookup (symb)
  (or  (rule-rhs (assoc symb *symbol-lookup*))
       symb))
(defun rewrites (category)
  "Return a list of the possible rewrites for this categor."
  (rule-rhs (assoc category *grammar*)))

;; utility functions
(defun mappend (fn the-list)
  "Apply fn ot each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Choose an element at random from list choices."
  (elt choices (random (length choices))))

(defun list-to-joined-string (list)
  (format nil "~{~a~^ ~}" list))


;;; "readme" goes here. Hacks and glory await!
