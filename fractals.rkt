;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fractals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ============================
; constants


(define CUTOFF 2)
(define CANVAS (empty-scene 350 350))


; ============================
; functions


(define (draw-triangle l)
  ; N -> Img
  ; draw a simple triangle
  (triangle l "outline" "red"))


(define (draw-square l)
  ; N -> Img
  ; draw a simple square
  (rectangle l l "outline" "red"))


(define (draw-circle l)
  ; N -> Img
  (circle l "solid" "brown"))


(define (sierpinski l)
  ; N -> Img
  ; draw a sierpinski fractal recursively
  (cond
    [(<= l CUTOFF) (draw-triangle l)]
    [else (local (
                  (define quad-tri (sierpinski (quotient l 2))))
            ; - IN -
            (above quad-tri (beside quad-tri quad-tri)))]))


(define (squarepinski l)
  ; N -> Img
  ; draw a sierpinskesque square fractal recursively
  (cond
    [(<= l CUTOFF) (draw-square l)]
    [else (local (
                  (define plain-square (draw-square (quotient l 3)))
                  (define nona-square (squarepinski (quotient l 3))))
            ; - IN -
            (above (beside nona-square nona-square nona-square)
                   (beside nona-square plain-square nona-square)
                   (beside nona-square nona-square nona-square)))]))
    

(define (circle-fractal l)
  ; N -> Img
  ; draw a mandlebrot-like fractus
  (local (
          (define (fract l)
            (cond
              [(<= l CUTOFF) (draw-circle l)]
              [else
               (local (
                       (define tri-circle (fract (quotient (* 2 l) 5))))
                 ; - IN -
                 (beside
                  (above
                   (rotate 90 tri-circle)
                   (draw-circle l)
                   (rotate 270 tri-circle))
                  tri-circle))]))
          (define quad-circle (fract (quotient (* 2 l) 5))))
    ; - IN -
    (beside
     (rotate 180 quad-circle)
     (above
      (rotate 90 quad-circle)
      (draw-circle l)
      (rotate 270 quad-circle))
     quad-circle)))



(define (von-kochflake l)
  ; N -> Img
  (local (
          (define (recursor l)
            (cond
              [(<= l CUTOFF) (rectangle l 1 "solid" "blue")]
              [else
               (local (
                       (define RECURSIVE (recursor (/ (- l (sqrt 3)) 3))))
                 ; - IN -
                 (beside/align "bottom"
                               RECURSIVE
                               (rotate  60 RECURSIVE)
                               (rotate -60 RECURSIVE)
                               RECURSIVE))]))
          (define FLAKENESS (recursor l)))
    ; - IN -
    (overlay
     (above
      FLAKENESS
      (beside
       (rotate  120 FLAKENESS)
       (rotate -120 FLAKENESS)))
     CANVAS)))
    

;(sierpinski 256)

;(squarepinski 243)

;(circle-fractal 200)

(von-kochflake 300) 

;(foldr + -1 (map (lambda (n) (* 2 (expt (/ 2 5) n))) (build-list 200 identity)))