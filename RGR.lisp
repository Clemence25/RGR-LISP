;Рекурсивна функція обрахунку F(i)
(defun calc-f (i)
  (cond
    ((= i 1) 1.0)
    ((= i 10) 1.0)
        
    ; i = 2..9: F(і) = cos(F(і-1)) * 2*cos(i)
    ((and (>= i 2) (<= i 9))
     (* (cos (calc-f (- i 1)))
        (* 2 (cos i))))

    ; i = 11..20: F(і) = cos(F(і-1)) * 100 / i
    ((and (>= i 11) (<= i 20))
     (* (cos (calc-f (- i 1)))
        (/ 100 i)))
       
    (t (format t "Undefinded"))))

;Функція перевірки коректності обрахунку F(i)
(defun check-calc-f (name input expected)
  (let ((result (calc-f input))
        (tolerance 0.1))
    (format t "~:[FAILED~;PASSED~]... ~a: Expected ~a, Got ~5$~%"
            (< (abs (- result expected)) tolerance)
            name
            expected
            result)))

;Тестові набори
(defun test-calc-f ()
  (format t "Running tests:~%")
  
  ;Позитивні тести
  (format t "True tests~%")
  (check-calc-f "Test 1 i=1" 1 1)
  
  (check-calc-f "Test 2 i=2" 2 -0.44969)

  (check-calc-f "Test 3 i=9" 9 -1.7465)
  
  (check-calc-f "Test 4 i=10" 10 1.0)
  
  (check-calc-f "Test 5 i=11" 11 4.91184)

  (check-calc-f "Test 7 i=20" 20 -2.81803)              

  ;Негативні тести
  (format t "~%False tests~%")
  (format t "Test 8 i=0 : Expected Undefinded, Got ")
  (calc-f 0)
  (format t "~%Test 9 i=21: Expected Undefinded, Got ")
  (calc-f 21))

(test-calc-f)
