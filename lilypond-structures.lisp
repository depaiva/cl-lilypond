
(defun print-event (figure &key (pitch "r") dynamic articulation)
  ;(declare (ratio figure) (string pitch dynamic articulation) (values string))
  (format nil "~a~a~@[~a~]~@[~a~]" pitch (abs (/ 1 figure)) dynamic articulation))
  

(defun note2lily (note)
  (print-event (note-figure note)
	       :pitch (note-pitch note)
	       :dynamic (note-dynamic note)
	       :articulation (note-articulation note)))

(defun pause2lily (pause)
  (print-event (pause-figure pause)))

(defun events2lily (events)
  (format nil "~{~a ~}" 
	  (mapcar #'(lambda (x)
		      (cond ((note-p x) (note2lily x))
			    ((pause-p x) (pause2lily x))))
		  events)))

(defun measure2lily (measure  &key tempo? sig?)
  (let ((tempo (measure-tempo measure))
	(time-signature (measure-time-signature measure))
	(events (measure-events measure)))
    (remove nil
	    (list (when tempo? (format nil "\\tempo ~a = ~a" (first tempo) (second tempo)))
		  (when sig? (format nil "\\time ~a" time-signature))
		  (events2lily events)))))

(defun measures2lily* (measures &optional (ant-tempo -1) (ant-sig -1))
    (loop for item in measures
       append (measure2lily item
			    :tempo? (not (equal (measure-tempo item) ant-tempo))
			    :sig?   (not (equal (measure-time-signature item) ant-sig))) 
       do (setq ant-tempo (measure-tempo item)
		ant-sig (measure-time-signature item))))

(defmethod last-tempo ((measures smeasures))
  (measure-tempo (car (last (smeasures-measures measures)))))

(defmethod last-tempo ((measures repeats))
  (measure-tempo (car (last (repeats-measures measures)))))

(defmethod last-sig ((measures smeasures))
  (measure-time-signature (car (last (smeasures-measures measures)))))

(defmethod last-sig ((measures repeats))
  (measure-time-signature (car (last (repeats-measures measures)))))

(defmethod measures2lily ((measures smeasures) &optional (ant-tempo -1) (ant-sig -1))
  (measures2lily* (smeasures-measures measures) ant-tempo ant-sig))

(defmethod measures2lily ((measures repeats) &optional (ant-tempo -1) (ant-sig -1))
  (lily-repeat (measures2lily* (repeats-measures measures) ant-tempo ant-sig)
	       (repeats-n measures)
	       (repeats-type measures)))

(defun staff2lily (staff)
  (lily-staff
   (let ((ant-tempo -1)
	 (ant-sig -1))
     (loop for x in (staff-measures staff)
	append (measures2lily x ant-tempo ant-sig)
	do (setq ant-tempo (last-tempo x)
		 ant-sig (last-sig x))))
   :spacing (staff-spacing staff)
   :name (staff-name staff)
   :omit (staff-omit staff)
   :xtra (staff-xtra staff)
   :tempo (staff-tempo staff)
   :type (staff-type staff)))
  
       

(defun staff-system2lily (staff-system)
  (lily-staff-block (loop for x in (staff-system-staves staff-system)
			 append (staff2lily x))
		    :type (staff-system-type staff-system)
		    :xtra (staff-system-xtra staff-system)))
  

(defun score2lily (score)
  (cat-lines
	  (list
	   (loop for x in (score-variables score)
	      append (lily-variable (car x) (cdr x)))
   
	   (header2lily (score-header score))

	   (lily-block
	    (loop for x in (score-staff-systems score)
	       append (staff-system2lily x))))))

;; (defun figures2measure (figures)
;;   (make-measure :events (mapcar #'figure2note figures)))

;; (defun measures2staff (measures)
;;   (make-staff :measures (mapcar #'figures2measure measures)))

;; (defun staff2staff-system (staves)
;;   (make-staff-system :staffs (mapcar #'measures2staff staves)))

(defun figure2note (figure &optional (pitch "c"))
  (declare (ratio figure) (string pitch))
  (if (< figure 0)
      (make-pause :figure figure)
      (make-note :figure figure
		 :pitch pitch)))

;;; specific functions

(defun save-clapping-ly ()
)

;(mapcar #'(lambda (x) (format nil "c~a" x)) figuras)

(defstruct note
  pitch 
  figure
  dynamic
  articulation)

(defstruct pause
  figure)

(defstruct chord
  pitches
  figure
  dynamic
  articulation)

(defstruct tuplet)

(defstruct measure
  tempo
  time-signature
  events)

;measures sem repeti
(defstruct smeasures
  measures)

(defstruct repeats
  measures
  type
  n)

(defstruct staff
  measures
  type
  spacing
  name
  xtra
  tempo
  omit) 

(defstruct staff-system
  staves
  type
  xtra)

(defstruct score
  staff-systems
  header
  variables)

(defstruct header
  composer
  title
  subtitle
  date)

(defun header2lily (header)
  (lily-header (header-title header)
	       :composer (header-composer header)
	       :subtitle (header-subtitle header)
	       :date? (header-date header)))



(defun savelily (source filename)
  (with-open-file
      (arq (make-pathname :name (pathname-name filename)
			  :type "ly"
			  :defaults *default-pathname-defaults*)    
	   :direction :output
	   :if-exists :supersede
	   :if-does-not-exist :create)
    (format arq "~a" source)
    (namestring arq)))
    
  

(defun run-lily (lyfile)
  (run-program "/usr/local/bin/lilypond" (list lyfile)
	       :output *standard-output*))

(defun notelist2lily (notelist)
  