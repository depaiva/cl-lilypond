;imported function (lisp cookbook)

(defun split-by-newline (string)
  (loop
     for i = 0 then (1+ j)
     as j = (position #\Newline string :start i)
     collect (subseq string i j)
     while j))


;;; general functions

(defun cat-lines (list)
  (format nil "~{~a~}"
	  (remove nil list)))

(defun lily-header (title &key composer subtitle date?)
  (cat-lines
   (list
    (format nil "\\header \{~%")
    (format nil "~T title = ~a~%" title)
    (format nil "~T composer = ~a~%" composer)
    (when subtitle (format nil "~T subtitle = ~a~%" subtitle))
    (when date? (format nil "~T subsubtitle = \\date"))
    (format nil "\}"))))

(defun lily-block (expression &optional (command "score") (parameter nil))
  (if (find #\Newline expression) (setq expression (split-by-newline expression)))
  (format nil "\\~a ~@[~a ~]\{~{~%~T ~a~}~%\}" command parameter expression))


(defun lily-variable (name expression)
  (format nil "~a = \{~%~T ~a~%\}" name expression))

(defun lily-repeat (m-expression &optional (times 2) (repeat-type "volta"))
  (if (find #\Newline m-expression) (setq m-expression (split-by-newline m-expression)))
    (append (list (format nil "\\repeat ~a ~a \{" repeat-type times))
	    (mapcar #'(lambda (x) (format nil "~2t~a" x)) m-expression)
	    '("}")))
	

(defun lily-date-message (&optional (message "automatically generated in"))
  (format nil "date = #(format \"~a ~~a\" (strftime \"\%c\" (localtime (current-time))))" message))

(defvar  *within-system-spacing-grobs* '("VerticalAxisGroup" "StaffGrouper"))

(defun select-vertical-space-grob (grob)
  (declare ((integer 0 1) grob)) (nth grob *within-system-spacing-grobs*))

(defvar *vertical-axis-group-properties* '("staff-staff-spacing"
  "default-staff-staff-spacing"
  "staff-affinity"
  "nonstaff-relatedstaff-spacing"
  "nonstaff-nonstaff-spacing"
  "nonstaff-unrelatedstaff-spacing"))

(defun select-vertical-axis-group-property (property)
  (declare ((integer 0 5) property)) (nth property *vertical-axis-group-properties*))

(defun override-staff-spacing* (&key (grob 0) (property 0) basic-dist min-dist padding stretch &aux key value)
  (setf grob (select-vertical-space-grob grob))
  (setf property (select-vertical-axis-group-property property))
  (when stretch (push "stretchability" key) (push stretch value))
  (when padding (push "padding" key) (push padding value)) 
  (when min-dist (push "minimum-distance" key) (push min-dist value)) 
  (when basic-dist (push "basic-distance" key) (push basic-dist value))
  (when (= (length key) 1) (setf key (car key)) (setf value (car value)))
  (override-staff-spacing grob property key value))
	
(defmethod override-staff-spacing ((grob string) (property string) (key string) (value integer) &aux end-exp)
  (format nil "\\override ~a.~a.~a = #~d" grob property key value))

(defmethod override-staff-spacing ((grob string) (property string) (key list) (value list) &aux end-exp)
  (dotimes (i (length (butlast (cdr key))))
    (push (format nil "~4t(~a . ~a)" (pop (cdr key)) (pop (cdr value))) end-exp)) 
  (push (format nil "~2t#'(~a . ~d)" (pop key) (pop value)) end-exp)
  (nconc end-exp (list (format nil "~4t(~a . ~a))" (pop key) (pop value))))
  (push (format nil "\\override ~a.~a = " grob property) end-exp))

(defun lily-omit (score-object)
  (declare (string score-object) (values string))
  (format nil "\\omit ~a" score-object))

(defun lily-tempo (figure bpm)
  (declare (number figure bpm) (values string))
  (format nil "\\tempo ~a = ~a" figure bpm))

(defun staff-name (instrument-name &key short-name midi-name)
  (declare (string instrument-name short-name midi-name) (values cons))
  (remove nil
	  (list
	   (format nil "instrumentName = #~s" instrument-name)
	   (when short-name (format nil "shortInstrumentName = #~s" short-name))
	   (when midi-name (format nil "midiInstrument = #~s" midi-name)))))

(defun staff-with-block (override-spacing staff-name)
  (declare (cons override-spacing staff-name))
  (append '("\\with {")
	  (when override-spacing (indent override-spacing 2))
	  (when staff-name (indent staff-name 2))
	  '("}")))

(defun lily-staff (m-expression &key type spacing name tempo omit xtra &aux staff with)
  (when (or spacing name) (setf with (staff-with-block spacing name)))
  (push (format nil "\\new ~@[~a~]Staff ~@[~a~]" type with) staff)
  (if (or spacing name) (nconc staff '("{")) (setf staff (list (format nil "~{~a\{~}" staff))))
  (when tempo (nconc staff (indent (lily-tempo (car tempo) (cadr tempo)))))
  (when omit (nconc staff (indent (lily-omit omit))))
  (when xtra (nconc staff (indent xtra)))
  (nconc staff (indent m-expression))
  (nconc staff '("}")))

(defun lily-staff-block (staves &key type xtra &aux header)
  (cond ((equal type "group") (setf header "\\new StaffGroup "))
	((equal type "choir") (setf header "\\new ChoirStaff "))
	((equal type "piano") (setf header "\\new PianoStaff "))
	((equal type "grand") (setf header "\\new GrandStaff "))
	(t nil))
  (append (list (format nil "~@[~a~]<<" header))
	  (when xtra (indent xtra 2))
	  (indent staves 2)
	  '(">>")))

(defun indent (strings &optional (n 2))
  (mapcar #'(lambda (x) (format nil (format nil "~~~at~~a" n) x)) strings))

