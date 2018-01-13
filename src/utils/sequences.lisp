;;;;
;;;; Useful function for working with sequences.
;;;;

(in-package :saexplorer)

(defun gather (seq &key (key #'identity) (test #'eql))
  "Gather elements of SEQ into groups with equal keys."
  (loop :with distinct-sets = ()
     :for item :in seq
     :for k = (funcall key item)
     :for matching = (find k distinct-sets :key #'car :test test)
     :do (if matching
             (push item (cdr matching))
             (push (list k item) distinct-sets))
     :finally (return (mapcar #'(lambda (x) (nreverse (cdr x))) (nreverse distinct-sets)))))


(defun longest-common-substring (a b)
  "Longest common subsequence."
  (let ((L (make-array (list (length a) (length b)) :initial-element 0))
        (z 0)
        (result '()))
    (dotimes (i (length a))
      (dotimes (j (length b))
        (when (char= (char a i) (char b j))
          (setf (aref L i j)
                (if (or (zerop i) (zerop j))
                    1
                    (1+ (aref L (1- i) (1- j)))))
          (when (> (aref L i j) z)
            (setf z (aref L i j)
                  result '()))
          (when (= (aref L i j) z)
            (pushnew (subseq a (1+ (- i z)) (1+ i))
                     result :test #'equal)))))
    result))


(defun sparse-matchings (seq pattern &key (test #'eql))
  "The list of all ordered sparse matchings of SEQ by PATTERN. An
ordered matching is a list of positions where PATTERN's items appear
in SEQ. If item a precedes to b in PATTERN, then a's position in SEQ
is strictly less then position for b."
  (let (matchings)
    (labels ((rec (pat offset matching-rev) ; match PAT over SEQ at offset, collect path
               (when (null pat)
                 (push (reverse matching-rev) matchings))
               (loop with item = (car pat)
                  for start = offset then (1+ item-pos)
                  for item-pos = (position item seq :test test :start start)
                  while item-pos do
                    (rec (cdr pat) (1+ item-pos) (cons item-pos matching-rev)))))
      (rec pattern 0 '())
      matchings)))
