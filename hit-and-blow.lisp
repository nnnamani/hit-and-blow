;;;; hit-and-blowを解く
;;; 4桁を作る数字のリスト
(defparameter *seeds* '(0 1 2 3 4 5 6 7 8 9))

;;; 4桁の数字の候補
(defparameter *candidates* '())

;;; 許可するコマンド
(defparameter *allowed-commands* '(ans))

;;; 順列を生成
(defun perm (func ls n &optional a)
  (if (or (null ls) (eq n 0))
      (funcall func a)
      (dolist (x ls)
	(perm func (remove x ls) (1- n ) (cons x a)))))
  



;;; 2つの4桁の数字を比較し hitとblow を求める
(defun check-hit-blow (true-num req)
  (let ((hit 0)
	(blow 0))
    (dolist (a req)
      (cond ((eq (position a true-num) (position a req))
	     (setf hit (1+ hit)))
	    ((position a true-num)
	     (setf blow (1+ blow)))))
    (list :hit hit :blow blow)))




;;; ある4桁の数字numとseedsで与えられる候補とのhit,blowから
;;; 次の候補となる数字を生成する
;;; ex) num -> '(1 2 3 4)
(defun make-candidate (num seeds hit blow)
  (let* ((target (car seeds))
	 (hit-blow (check-hit-blow target num)))
    (cond ((null seeds)
	   nil)
	  ;;; 見つかった時
	  ((and (eq (getf hit-blow :hit) hit)
		(eq (getf hit-blow :blow) blow))
	   (cons target (make-candidate num (cdr seeds) hit blow)))
	  ;;; 見つからなかった時
	  (t
	   (make-candidate num (cdr seeds) hit blow)))))
    

		       

;;; ゲームを初期化
(defun initialize ()
  ;; 4桁の数字の全ての候補を生成
  (perm #'(lambda (x) (push x *candidates*))
        *seeds*
        4))

;;; 候補を選択する
(defun select-candidate ()
  ;; 単純に候補リストの先頭を選択
  (car *candidates*))



;;;
;;; ゲームのコマンド


;;; ユーザの入力を処理
(defun ans (hit blow)
  (let ((candidate (select-candidate)))
    (setf *candidates* (make-candidate candidate
                                        *candidates*
                                        hit
                                        blow))
    (select-candidate)))
                          



;;;
;;; CLI
(defun game-start ()
  (initialize)
  (format t "数字が重複しない4桁の数字を考えてください。~%")
  (game-print (select-candidate))
  (game-repl))

(defun game-reset ()
  (initialize)
  (format t "Reset game ...~%"))
  
(defun game-repl ()
  (format t "'ans hit数 blow数'の形式で結果を入力してください。(quitで終了)~%")
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun game-print (msg)
  (format t "~A~%" msg))
