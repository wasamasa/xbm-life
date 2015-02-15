;;; xbm-life.el --- A XBM version of Conway's Game of Life

;; Copyright (C) 2015 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/xbm-life
;; Version: 0.0.1
;; Package-Requires:
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A XBM version of Conway's Game of Life

;;; Code:

(defvar xbm-life-grid-size 16)

(defvar xbm-life-tile-size 8)

(defvar xbm-life-grid [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0]
                       [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0]
                       [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0]
                       [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0]
                       [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0]
                       [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0]
                       [0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

(defun xbm-life-render-image (grid)
  "Turn GRID into a XBM image."
  (let* ((size (* xbm-life-grid-size xbm-life-tile-size))
         (xbm (make-vector size nil)))
    (dotimes (row size)
      (let ((line (make-bool-vector size nil)))
        (dotimes (col size)
          ;; iterate over the upscaled xbm, do integer division to
          ;; obtain the coordinates to look up the original values in
          (aset line col (= 1 (xbm-life-peek grid (/ row xbm-life-tile-size)
                                             (/ col xbm-life-tile-size)))))
        (aset xbm row line)))
    xbm))

(defun xbm-life-peek (grid row col)
  "Return value for GRID at ROW and COL."
  (aref (aref grid row) col))

(defun xbm-life-poke (grid row col value)
  "Set value for GRID at ROW and COL to VALUE."
  (aset (aref grid row) col value))

(defun xbm-life-out-of-bounds (row col)
  "Check whether ROW and COL are out of bounds."
  (or (< row 0) (>= row xbm-life-grid-size)
      (< col 0) (>= col xbm-life-grid-size)))

(defun xbm-life-neighbors (grid row col)
  "Return number of neighbors on GRID at ROW and COL."
  (let ((neighbors 0)
        (offsets '((-1 . -1) (-1 . 0) (-1 . 1)
                   ( 0 . -1)          ( 0 . 1)
                   ( 1 . -1) ( 1 . 0) ( 1 . 1))))
    (dolist (xy offsets)
      (let* ((x (car xy))
             (y (cdr xy))
             (row+x (+ row x))
             (col+y (+ col y)))
        (when (and (not (xbm-life-out-of-bounds row+x col+y))
                   (= (xbm-life-peek grid row+x col+y) 1))
          (setq neighbors (1+ neighbors)))))
    neighbors))

(defun xbm-life-create-empty-grid ()
  "Return empty grid."
  (let (grid)
    (dotimes (_ xbm-life-grid-size)
      (setq grid (cons (make-vector xbm-life-grid-size 0) grid)))
    (vconcat grid)))

(defun xbm-life-next-generation (grid)
  "Create the next generation based on GRID."
  (let ((new (xbm-life-create-empty-grid)))
    (dotimes (row xbm-life-grid-size)
      (dotimes (col xbm-life-grid-size)
        (let ((state (xbm-life-peek grid row col))
              (neighbors (xbm-life-neighbors grid row col)))
          (if (= state 1)
              (if (or (= neighbors 2) (= neighbors 3))
                  (xbm-life-poke new row col 1)
                (xbm-life-poke new row col 0))
            (if (= neighbors 3)
                (xbm-life-poke new row col 1)
              (xbm-life-poke new row col 0))))))
    new))

(defun xbm-life-redraw-grid ()
  "Redraw grid on game buffer."
  (interactive)
  (with-current-buffer "*xbm life*"
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-image
     (create-image (xbm-life-render-image xbm-life-grid) 'xbm t
                   :width (* xbm-life-grid-size xbm-life-tile-size)
                   :height (* xbm-life-grid-size xbm-life-tile-size)))
    (insert "\n")
    (set-window-point (car (get-buffer-window-list "*xbm life*")) (point-max))
    (setq buffer-read-only t)))

(defun xbm-life-advance-generation ()
  "Advance the current generation and redraw the grid."
  (interactive)
  (setq xbm-life-grid (xbm-life-next-generation xbm-life-grid))
  (xbm-life-redraw-grid))

(defvar xbm-life-bitmap
  "#define glider_width 8
#define glider_height 16
static unsigned char glider_bits[] = {
0x18, 0x18, 0x00, 0xc0, 0xc0, 0x00, 0xdb, 0xdb,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };")

(defvar xbm-life-icon
  (propertize " " 'display `(image :type xbm :data ,xbm-life-bitmap
                                    :background ,(face-background 'mode-line))))

(define-derived-mode xbm-life-mode special-mode xbm-life-icon
  "A XBM demonstration."
  (buffer-disable-undo))

(defvar xbm-life-timer)

(defun xbm-life-quit ()
  "Cancel animation and quit window."
  (interactive)
  (cancel-timer xbm-life-timer)
  (quit-window))

(define-key xbm-life-mode-map (kbd "q") 'xbm-life-quit)

;;;###autoload
(defun xbm-life ()
  "Launch a XBM demo of Conway's Game of Life."
  (interactive)
  (with-current-buffer (get-buffer-create "*xbm life*")
    (xbm-life-mode))
  (display-buffer "*xbm life*")
  (xbm-life-redraw-grid)
  (setq xbm-life-timer (run-with-timer 0 1 'xbm-life-advance-generation)))

;; TODO make variables mode-/buffer-local to allow for multiple demos
;; going on at the same time

;; TODO stop animation on focus loss (or maybe just neither redraw nor
;; update when you're not viewing a xbm-life buffer)

;; TODO add more controls, like tile size, grid size, speed, pause
;; with single step, randomization, restart, etc.

;; TODO add customization options for defaults, like tile size, grid
;; size, initial layout, etc.

(provide 'xbm-life)

;;; xbm-life.el ends here
