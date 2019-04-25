#lang racket
 
(require pict3d
         pict3d/universe)
(require match-plus)
(require racket/gui)

(define collision-sound "spin_jump-Brandino480-2020916281.wav")
(define ball-x-y 1/4)
(define pipe-interval 0.9)
(define pipe-height 1/7)
(define ball-radius 1/15)
(define bounce-speed 0.8)
(define gravity-acceleration-constant -0.5)

(struct pos-checkpoint-info (speed z time) #:transparent)
(struct game-st (rotation pos-cp-info) #:transparent)

(current-material
 (material
  #:ambient 0.01
  #:diffuse 0.39
  #:specular 0.6
  #:roughness 0.2))

(struct pipe-section (start end) #:transparent)
(struct pipe-info (normal-sections killer-sections) #:transparent)

(define pipes
  (list
   (pipe-info
    (list (pipe-section 70 120))
    (list (pipe-section 40 60) (pipe-section 140 160)))
   (pipe-info
    (list (pipe-section 90 160))
    (list))
   (pipe-info
    (list (pipe-section 40 90))
    (list))
   (pipe-info
    (list (pipe-section 20 60))
    (list))
   (pipe-info
    (list (pipe-section 70 100))
    (list))
   (pipe-info
    (list (pipe-section 0 40))
    (list))
   (pipe-info
    (list (pipe-section 80 130))
    (list))))
 
(define (lights+camera ball-base-height)
  (combine (light (pos 0 1 2) (emitted "white" 10))
           (light (pos 0 -1 -2) (emitted "orange" 7))
           (basis 'camera
                  (point-at (pos 1 1 (+ ball-base-height .7))
                            (pos 0 0 ball-base-height)))))

(define/match* (get-ball-z (pos-checkpoint-info cp-speed cp-z cp-time) t)
  (define time-diff (/ (- t cp-time) 1000))
  (+ cp-z (* gravity-acceleration-constant time-diff time-diff) (* cp-speed time-diff)))

(define/match* (on-frame (game-st rot pos-cp-info) n t)
  (define ball-z (get-ball-z pos-cp-info t))
  (define input-st (game-st rot pos-cp-info))
  (define pipe-index
    (exact-round
     (-
      pipe-height
      (/ ball-z (+ pipe-interval pipe-height)))))
  (if (<= 0 pipe-index (sub1 (length pipes)))
      (check-collision pipe-index input-st rot ball-z t)
      input-st))

(define (check-collision pipe-index input-st rot ball-z t)
  (define pipe (list-ref pipes pipe-index) )
  (define cur-p (render-pipe pipe pipe-index))
  (if (trace (rotate-z cur-p rot)
             (pos ball-x-y ball-x-y ball-z)
             (pos ball-x-y ball-x-y (- ball-z 0.05)))
      (begin
        (play-sound collision-sound #t)
        (game-st rot (pos-checkpoint-info bounce-speed ball-z t))) 
      input-st))

(define (render-pipe-section color v-offset* arc-a arc-b)
  (with-color color
    (pipe
     (pos -1/2 -1/2 (- (- (/ pipe-height 2)) v-offset*))
     (pos 1/2 1/2 (- (/ pipe-height 2) v-offset*))
     #:arc (arc arc-a arc-b))))

(define/match* (render-pipe (pipe-info normal-sections killer-sections) idx)
  (define v-offset* (* idx (+ pipe-interval (/ pipe-height 2))))
   (combine
    (for/list ([normal-section normal-sections])
      (render-pipe-section (rgba "white" 1) v-offset*
                           (pipe-section-start normal-section)
                           (pipe-section-end normal-section)))

    ;; TODO copy-paste
    (for/list ([killer-section killer-sections])
      (render-pipe-section (rgba "red" 1) v-offset*
                           (pipe-section-start killer-section)
                           (pipe-section-end killer-section)))))

(define/match* (on-draw (game-st rot pos-cp-info) n t)
  (define ball-z (get-ball-z pos-cp-info t))
  
  (combine

   ;; žoga
   (combine
    (with-color
        (rgba "green" 1)
      (sphere
       (pos ball-x-y ball-x-y ball-z) ball-radius)))

   (for/list ([pipe pipes]
              [idx (in-naturals)])
     (rotate-z (render-pipe pipe idx) rot))

   ;; cilinder na sredini
   (cylinder (pos -1/3 -1/3 -10) (pos 1/8 1/8 2))
   
   (lights+camera ball-z)))

(define/match* (on-key (game-st rot pos-cp-info) n t k)
  (define move-unit 10)
  (case k
    [("left") (game-st (+ rot move-unit) pos-cp-info)]
    [("right") (game-st (- rot move-unit) pos-cp-info)]
    [else (game-st rot pos-cp-info)]))
 
(big-bang3d
 (game-st 0 (pos-checkpoint-info bounce-speed 0 0))
 #:on-frame on-frame
 #:on-key on-key
 #:on-draw on-draw)