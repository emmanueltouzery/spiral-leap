#lang racket
 
(require pict3d
         pict3d/universe)
(require match-plus)
(require racket/gui)

(define collision-sound "spin_jump-Brandino480-2020916281.wav")
 
(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 0.6
                            #:roughness 0.2))
 
(define lights+camera
  (combine (light (pos 0 1 2) (emitted "white" 10))
           (light (pos 0 -1 -2) (emitted "orange" 7))
           (basis 'camera (point-at (pos 1 1 .7) origin))))

(define (easing-fn x)
  ;; https://gist.github.com/gre/1650294
  (* x (- 2 x)))

;; transform t+my jump-duration into a 0-1 number
(define (jump-ratio t)
  (define jump-duration 1000)
  (/
   (remainder (round t) jump-duration)
   jump-duration))

(define (ball-position-z t)
  (define jump-done-ratio
    (jump-ratio t))
  ;; split the 0-1 range in two ranges (going up then down)
  ;; then scale it down by 0.7 to fit between the pipes
  (* 0.7 (if  (> jump-done-ratio 1/2)
              ;; going up.. transform 0.5->1 into 0->1 then apply the easing fn
              (easing-fn (* 2 (- jump-done-ratio 1/2)))
              ;; going down.. transform 0->0.5 into 0->1 then 1->0 then apply the easing fn
              (easing-fn (- 1 (* jump-done-ratio 2))))))

(struct game-st (rotation ball-direction))

(define/match* (on-frame (game-st rot dir) n t)
  (define new-dir
    (if (> (jump-ratio t) .5)
        'up
        'down))
  (if (eq? new-dir dir)
      (game-st rot dir)
      (if (eq? new-dir 'up)
          (check-collision (game-st rot new-dir))
          (game-st rot new-dir))))

(define (check-collision st)
  (play-sound collision-sound #t)
  st)

(struct pipe-info (rotation-offset) #:transparent)

(define pipes
  (list
   (pipe-info 30)
   (pipe-info 70)
   (pipe-info 140)))

(define/match* (render-pipe (pipe-info offset) idx)
  ;; times 0.9 it looks nicer like that
  (define v-offset* (* (- idx 1) .9))
  (rotate-z (pipe
             (pos -1/2 -1/2 (- -1/15 v-offset*))
             (pos 1/2 1/2 (- 1/15 v-offset*))
             #:arc (arc 90 360))
            offset))

(define/match* (on-draw (game-st rot _) n t)
  (combine

   ;; žoga
   (combine
    (with-color
        (rgba "green" 1)
      (move-z (sphere (pos 1/4 1/4 0) 1/15) (ball-position-z t))))

   (for/list ([pipe pipes]
              [idx (in-naturals)])
     (rotate-z (render-pipe pipe idx) rot))

   ;; cilinder na sredini
   (cylinder (pos -1/3 -1/3 -4) (pos 1/8 1/8 2))
   
   lights+camera))

(define/match* (on-key (game-st rot dir) n t k)
  (define move-unit 10)
  (case k
    [("left") (game-st (+ rot move-unit) dir)]
    [("right") (game-st (- rot move-unit) dir)]
    [else (game-st rot dir)]))
 
(big-bang3d
 (game-st 0 'up)
 #:on-frame on-frame
 #:on-key on-key
 #:on-draw on-draw)