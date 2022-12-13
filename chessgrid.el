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

(defcustom chessgrid--countdown-value 10 "Number of seconds"
  :group 'chessgrid
  :type 'number)


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

(defconst chessgrid--cols (vconcat "abcdefgh") "All the columns of the chessboard")
(defconst chessgrid--lines (vconcat "12345678") "All the lines of the chessboard")

(defvar chessgrid--pov 'white "Current point of view. Wheither black or white")
(defvar chessgrid--cell nil "The current cell to guess")
(defvar chessgrid--success 0 "Track the number of success")
(defvar chessgrid--error 0 "Track the number of error")

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
		(let ((square (cond (is-highlighted (format "🟥" j i))
							(is-white (format "⬜" j i)) 
							(t (format "⬛" j i)))))
		  (insert (propertize square 'face 'chessgrid-board)))))
	(newline)))



(defun chessgrid--search-vector (val vec)
  "Return the index of VAL inside the vector VEC"
  (let ((seq (append vec nil)))
	(when-let ((m (member val seq)))
	  (- (length seq) (length m)))))


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
  (insert
   (format "❌ %s ✅ %s"
		   (number-to-string chessgrid--error)
		   (number-to-string chessgrid--success))))

(defun chessgrid--display-pov ()
  (insert (format "POV: %s" (if (eq chessgrid--pov 'white) "⬜" "⬛"))))

(defun chessgrid--draw-buffer ()
  (chessgrid--ensure-buffer)
  (let ((inhibit-read-only t))
	(erase-buffer)
	(chessgrid--insert-heading)
	(chessgrid--display-pov)
	(newline 2)
	(chessgrid--display-score)
	(newline 2)
	(chessgrid--display chessgrid--cell)
	(newline)
	(goto-char (point-min))))

(defun chessgrid-toggle-pov ()
  (interactive)
  (setq chessgrid--pov
		(if (eq chessgrid--pov 'white)
			'black
		  'white))
  (chessgrid--draw-buffer))

(defun chessgrid--get-coordinate-from-pos (pos)
  "From POS like a2, h3 gives the coordinates (lines . col) depending
on chessgrid--pov"
  (let ((pos-list (append pos nil)) 
		(lines (if (eq chessgrid--pov 'white)
				   (reverse chessgrid--lines)
				 chessgrid--lines))
		(cols (if (eq chessgrid--pov 'white)
				  chessgrid--cols
				(reverse chessgrid--cols))))
	(cons
	 (chessgrid--search-vector (cadr pos-list) lines)
	 (chessgrid--search-vector (car pos-list) cols))))

(defun chessgrid--ensure-buffer ()
  "Ensure current-buffer is chessgrid buffer"
  (unless (string= chessgrid--buffer-name (buffer-name))
	(user-error (format "Not in %s buffer" chessgrid--buffer-name))))

(defun chessgrid-challenge ()
  (interactive)
  (chessgrid--ensure-buffer)
  (setq chessgrid--success 0)
  (setq chessgrid--error 0)
  (unwind-protect
	  (let ((start-time (truncate (float-time (current-time)))))
		(while t 
		  (when (> (- (truncate (float-time (current-time))) start-time) chessgrid--countdown-value)
			(user-error  "Time is over"))
		  (setq chessgrid--cell (chessgrid--get-random-position))
		  (chessgrid--draw-buffer)
		  (let ((guess (downcase (read-string "Guess the position: "))))
			(if (equal chessgrid--cell (chessgrid--get-coordinate-from-pos guess))
				(setq chessgrid--success (+ 1 chessgrid--success))))
		  (chessgrid--draw-buffer)))

	(setq chessgrid--cell nil)
	(chessgrid--draw-buffer)))

(defun chessgrid ()
  (interactive)
  (pop-to-buffer chessgrid--buffer-name)
  (chessgrid-mode)
  (chessgrid--draw-buffer))


(let ((map chessgrid-mode-map))
  (define-key map (kbd "C-c C-v") 'chessgrid-toggle-pov)
  (define-key map (kbd "C-c C-c") 'chessgrid-challenge))

