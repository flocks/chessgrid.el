(defgroup chessgrid nil
  "Chessgrid customization group."
  :group 'convenience)

(defcustom chessgrid--buffer-name "*chessgrid*"
  "Name of chessgrid buffer"
  :group 'chessgrid)

(defgroup chessgrid-faces nil
  "Chesssgrid faces customization group."
  :group 'chessgrid
  :group 'faces)

(defface chessgrid-title
  '((t :height 200 :inherit bold))
  "Face for title of the buffer."
  :group 'chessgrid-faces)

(defface chessgrid-score
  '((t  :inherit success :height 200))
  "Face for success counter."
  :group 'chessgrid-faces)

(defface chessgrid-board
  '((t :height 300))
  "Face for the chess board."
  :group 'chessgrid-faces)

(defvar chessgrid--cols (vconcat "abcdefgh"))
(defvar chessgrid--lines (vconcat "12345678"))
(defvar chessgrid--pov 'white)
(defvar chessgrid--cell '(1 . 2))
(defvar chessgrid--score 0)

(define-derived-mode chessgrid-mode special-mode "Chessgrid"
  "Mode for chessgrid")

(defun chessgrid--display (cell)
  "Display the grid with CELL highlighted if non nil"
  (dotimes (j 8)
	(dotimes (i 8)
	  (let* ((is-highlighted (and cell
								  (eq (car cell) j)
								  (eq (cdr cell) i)))
			 (mod (mod j 2))
			 (is-white (eq mod (mod i 2))))
		(let ((square (cond (is-highlighted "🟥")
							(is-white "⬜") 
							(t "⬛"))))
		  (insert (propertize square 'face 'chessgrid-board)))))
	(newline)))


(defun chessgrid--search-vector (val vector)
  (let ((seq (append vector nil)))
	(when-let ((m (member val seq)))
	  (- (length seq) (length m)))))

(defun chessgrid--get-coordinate (pos)
  "from POS letter representation get the (x . y) coordinate"
  (let* ((pos-grid (append pos nil))
		 (pos-x (car pos-grid))
		 (pos-y (cadr pos-grid)))
	(unless (and pos-x pos-y)
	  (user-error "invalid position"))
	(cons
	 (chessgrid--search-vector pos-x chessgrid--cols)
	 (chessgrid--search-vector pos-y chessgrid--lines))))

(defun chessgrid--get-random-position ()
  (cons (random 8) (random 8)))

(defun chessgrid--insert-heading ()
  (goto-char (point-min))
  (insert (propertize "Chessgrid.el" 'face 'chessgrid-title))
  (newline 2)
  (insert (propertize "Train your brain to recognize chess positions." 'face 'font-lock-comment-face))
  (newline)
  (insert (propertize "Try to identify the most number of chess positions in a row" 'face 'font-lock-comment-face))
  (newline 2)
  (insert (propertize "C-c C-c - start the game" 'face 'font-lock-comment-face))
  (newline)
  (insert (propertize "C-c C-v - change color point of view" 'face 'font-lock-comment-face))
  (newline 3))

(defun chessgrid--display-score ()
  (insert (propertize (format "Score: %s" (number-to-string chessgrid--score)) 'face 'chessgrid-score)))

(defun chessgrid--display-pov ()
  (insert (format "POV: %s" (if (eq chessgrid--pov 'white) "⬜" "⬛"))))

(defun chessgrid--draw-buffer ()
  ;; TODO check we are in the good buffer
  (let ((inhibit-read-only t))
	(erase-buffer)
	(chessgrid--insert-heading)
	(chessgrid--display chessgrid--cell)
	(newline)
	(chessgrid--display-pov)
	(newline 2)
	(chessgrid--display-score)
	(goto-char (point-min))))

(defun chessgrid-toggle-pov ()
  (interactive)
  (setq chessgrid--pov
		(if (eq chessgrid--pov 'white)
			'black
		  'white))
  (chessgrid--draw-buffer))

(defun chessgrid-challenge ()
  ;; TODO check we are in the good buffer
  (interactive)
  (setq chessgrid--score 0)
  (let ((count 0)
		(continue t))
	(while continue 
	  (setq chessgrid--cell (chessgrid--get-random-position))
	  
	  (chessgrid--draw-buffer)
	  (let ((guess (downcase (read-string "Guess the position: "))))
		(if (equal chessgrid--cell (chessgrid--get-coordinate guess))
			(setq chessgrid--score (+ 1 chessgrid--score))
		  (setq continue nil)
		  (setq chessgrid--cell nil)
		  (message "%s" chessgrid--cell)))
	  (chessgrid--draw-buffer))))

(defun chessgrid ()
  (interactive)
  (pop-to-buffer chessgrid--buffer-name)
  (chessgrid-mode)
  (chessgrid--draw-buffer))



(let ((map chessgrid-mode-map))
  (define-key map (kbd "C-c C-v") 'chessgrid-toggle-pov)
  (define-key map (kbd "C-c C-c") 'chessgrid-challenge))
