(load "~/lisp/lilypond/lilypond-structures.lisp")
(load "~/lisp/clappingmusic/clappingmusic.lisp")

(defparameter *events1* (list (make-note :pitch "c" :figure 1/8)
			(make-note :pitch "d" :figure 1/4)
			(make-pause :figure 1/4)
			(make-note :pitch "e" :figure 1/8)))


(defparameter *measure1* (make-measure :tempo '(4 120)
				       :time-signature 3/4
				       :events *events1*))

(defparameter *measure2* (make-measure :tempo '(4 120)
				       :time-signature 3/4
				       :events *events1*))

(defparameter *measure3* (make-measure :tempo '(4 120)
				       :time-signature 2/4
				       :events *events1*))

(defparameter *repeats1* (make-repeats :measures (list *measure1* *measure3*)
				       :type "volta"
				       :n 4)) 

(defparameter *staff1* (make-staff
			:measures (list
				   (make-smeasures :measures
						   (list *measure1* *measure2*))
				   *repeats1*
				   (make-smeasures :measures (list *measure3*)))))
						   

(defun test-events2lily ()
    (events2lily *events1*))

(defun test-measure2lily ()
  (list
   (measure2lily *measure1* :tempo? t :sig? t)
   (measure2lily *measure1* :tempo? nil :sig? t)
   (measure2lily *measure1* :tempo? t :sig? nil)
   (measure2lily *measure1* :tempo? nil :sig? nil)))

(defun test-measures2lily* ()
  (measures2lily* (list *measure1* *measure2* *measure3*)))

(defun test-repeats2lily ()
  (repeats2lily *repeats1*))

(defun test-staff2lily ()
  (staff2lily *staff1*))

(defun test-clap ()
  (let ((line (car (clapping-lines 1/8 3 1 1 1))))
    (staff2lily
     (make-staff :type "Rhythmic"
		 :measures (list
			    (make-smeasures :measures
					    (loop for x in line
					       collect (make-measure :events
								     (mapcar #'figure2note x)
								     :tempo '(4 120)
								     :time-signature 12/8))))))))

(defun test-clap2 ()
  (let ((line1 (car (clapping-ritornello (clapping-lines 1/8 3 1 1 1))))
	(line2 (second (clapping-ritornello (clapping-lines 1/8 3 1 1 1)))))
    (score2lily
     (make-score :header
		 (make-header :composer "Charles"
			      :title "bababa")
		 :staff-systems
		 (list
		 (make-staff-system :staves (list
					     (make-staff :type "Rhythmic"
							 :measures
							 (list (make-smeasures :measures
									       (loop for x in line1
										  collect (make-measure :events
													(mapcar #'figure2note x)
													:tempo '(4 120)
													:time-signature 12/8)))))
    
					     (make-staff :type "Rhythmic"
							 :measures
							 (list (make-smeasures :measures
									       (loop for x in line2
										  collect (make-measure :events
													(mapcar #'figure2note x)
													:tempo '(4 120)
													:time-signature 12/8))))))))))))
					

      
