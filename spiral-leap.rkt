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
 
(define (on-draw s n t)
  (combine

   ;; žoga
   (combine
    (with-color
        (rgba "green" 0.5)
      (move-z (sphere (pos 1/2 1/2 0) 1/15) (- (round t) t))))

   ;; cevka 1
   (rotate-z (pipe
              (pos -1/2 -1/2 -1/15)
              (pos 1/2 1/2 1/15)
              #:arc (arc 90 360))
             (/ t 7))
   (cylinder (pos -1/3 -1/3 -4) (pos 1/8 1/8 2))
   
   ;; cevka 2
   (rotate-z (pipe
              (pos -1/2 -1/2 -16/15)
              (pos 1/2 1/2 -18/15)
              #:arc (arc 90 360))
             (/ t 17))
   
   ;; cevka 3
   (rotate-z (pipe
              (pos -1/2 -1/2 10/15)
              (pos 1/2 1/2 8/15)
              #:arc (arc 90 360))
             (/ t 3))

   ;; cilinder na sredini
   (cylinder (pos -1/3 -1/3 -4) (pos 1/8 1/8 2))
   
   lights+camera))
 
(big-bang3d 0 #:on-draw on-draw)