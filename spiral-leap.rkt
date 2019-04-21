#lang racket
 
(require pict3d
         pict3d/universe)
 
(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 0.6
                            #:roughness 0.2))
 
(define lights+camera
  (combine (light (pos 0 1 2) (emitted "white" 10))
           (light (pos 0 -1 -2) (emitted "orange" 7))
           (basis 'camera (point-at (pos 1 1 1) origin))))

(define (easing-fn x)
  ;; https://gist.github.com/gre/1650294
  (* x (- 2 x)))

(define (ball-position-z t)
  (define jump-duration 1000)
  ;; transform t+my jump-duration into a 0-1 number
  (define jump-done-ratio
    (/
     (remainder (round t) jump-duration)
     jump-duration))
  ;; split the 0-1 range in two ranges (going up then down)
  (if  (> jump-done-ratio 1/2)
       ;; going up.. transform 0.5->1 into 0->1 then apply the easing fn
       (easing-fn (* 2 (- jump-done-ratio 1/2)))
       ;; going down.. transform 0->0.5 into 0->1 then 1->0 then apply the easing fn
       (easing-fn (- 1 (* jump-done-ratio 2)))))

(define (on-draw s n t)
  (combine

   ;; žoga
   (combine
    (with-color
        (rgba "green" 1)
      (move-z (sphere (pos 1/4 1/4 0) 1/15) (ball-position-z t))))

   ;; cevka 1
   (rotate-z (pipe
              (pos -1/2 -1/2 -1/15)
              (pos 1/2 1/2 1/15)
              #:arc (arc 90 360))
             (+ s 70))
   (cylinder (pos -1/3 -1/3 -4) (pos 1/8 1/8 2))
   
   ;; cevka 2
   (rotate-z (pipe
              (pos -1/2 -1/2 -16/15)
              (pos 1/2 1/2 -18/15)
              #:arc (arc 90 360))
             (+ s 17))
   
   ;; cevka 3
   (rotate-z (pipe
              (pos -1/2 -1/2 10/15)
              (pos 1/2 1/2 8/15)
              #:arc (arc 90 360))
             (+ s 173))

   ;; cilinder na sredini
   (cylinder (pos -1/3 -1/3 -4) (pos 1/8 1/8 2))
   
   lights+camera))

(define (on-key s n t k)
  (define move-unit 10)
  (case k
    [("left") (+ s move-unit)]
    [("right") (- s move-unit)]
    [else s]))
 
(big-bang3d
 0
 #:on-key on-key
 #:on-draw on-draw)