;;; markmacro.el --- Keyboard macro for marked objects   -*- lexical-binding: t; -*-

;; Filename: markmacro.el
;; Description: Keyboard macro for marked objects
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-11-24 13:57:16
;; Version: 0.1
;; Last-Updated: 2022-11-24 13:57:16
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/markmacro
;; Keywords:
;; Compatibility: GNU Emacs 28.2
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Keyboard macro for marked objects
;;

;;; Installation:
;;
;; Put markmacro.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'markmacro)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET markmacro RET
;;

;;; Change log:
;;
;; 2022/11/24
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(require 'cl-macs)

(defgroup markmacro nil
  "Keyboard macro for marked objects."
  :group 'markmacro)

(defvar-local markmacro-overlays '())
(defvar-local markmacro-start-overlay nil)
(defvar-local markmacro-rect-used nil)
(defvar-local markmacro-mark-target-orig-info nil)
(defvar-local markmacro-mark-target-last nil)

(defface markmacro-mark-face
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for marked regions."
  :group 'markmacro)

(defcustom markmacro-secondary-region-mark-cursors-type 'symbol
  "The default type used by `markmacro-secondary-region-mark-cursors-type'.

See `thing-at-point' for more information."
  :type '(choice
          (const :tag "Sym" symbol)
          (const :tag "List" list)
          (const :tag "Sexp" sexp)
          (const :tag "Defun" defun)
          (const :tag "Word" word)
          (const :tag "Line" line)
          (const :tag "Num" number))
  :group 'markmacro)

;;;###autoload
(defun markmacro-mark-words ()
  (interactive)
  (when markmacro-overlays
    (end-kbd-macro)
    (markmacro-exit))
  (let ((bound (if (region-active-p)
                   (cons (region-beginning) (region-end))
                 (bounds-of-thing-at-point 'word)))
        (mark-bounds '()))
    (when bound
      (when (region-active-p)
        (deactivate-mark))

      (let ((mark-bound-start (car bound))
            (mark-bound-end (cdr bound))
            (last-point 0)
            current-bound)
        (save-excursion
          (goto-char mark-bound-start)
          (while (and (<= (point) mark-bound-end)
                      (> (point) last-point))
            (setq current-bound (bounds-of-thing-at-point 'word))
            (when current-bound
              (add-to-list 'mark-bounds current-bound t))
            (setq last-point (point))
            (forward-word))))

      (dolist (bound mark-bounds)
        (let* ((overlay (make-overlay (car bound) (cdr bound))))
          (overlay-put overlay 'face 'markmacro-mark-face)
          (add-to-list 'markmacro-overlays overlay t)))

      (markmacro-select-last-overlay))))

;;;###autoload
(defun markmacro-mark-lines ()
  "Mark all lines and start kmacro recording.

This function has the following usages:
1. mark all lines in a region when it is active.
2. mark all lines in a secondary region when it is active.
3. mark all lines in the buffer by default."
  (interactive)
  (when markmacro-overlays
    (end-kbd-macro)
    (markmacro-exit))
  (when (bound-and-true-p rectangle-mark-mode)
    (markmacro-secondary-region-set))
  (let ((bound (cond
                ((region-active-p)
                 (cons (region-beginning) (region-end)))
                ((overlay-start mouse-secondary-overlay)
                 (cons (overlay-start mouse-secondary-overlay)
                       (overlay-end mouse-secondary-overlay)))
                (t (cons (point-min) (point-max)))))
        (mark-bounds '()))
    (when bound
      (when (region-active-p)
        (deactivate-mark))

      (let ((mark-bound-start (car bound))
            (mark-bound-end (cdr bound))
            (col-counts (markmacro-cursor-in-secondary-region-p))
            current-bound)
        (save-excursion
          (goto-char mark-bound-start)
          (dotimes (i (count-lines mark-bound-start mark-bound-end))
            (unless (string-match-p "^[ ]*$" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              (setq current-bound (if col-counts
                                      (cons (point) (+ (point) col-counts))
                                    (bounds-of-thing-at-point 'line)))
              (when current-bound
                (add-to-list 'mark-bounds current-bound t)))

            (unless (= i (1- (count-lines mark-bound-start mark-bound-end)))
              (line-move 1)))))

      (dolist (bound mark-bounds)
        (let* ((overlay (make-overlay (car bound) (cdr bound))))
          (overlay-put overlay 'face 'markmacro-mark-face)
          (add-to-list 'markmacro-overlays overlay t)))

      (markmacro-select-last-overlay))))

(defun markmacro-cursor-in-secondary-region-p ()
  "Return a number if current cursor is in a secondary region.

Otherwise, it return nil.

The number is the column count between the secondary region start column and
end column."
  (when-let* ((sr-ov-start (overlay-start mouse-secondary-overlay))
              (sr-ov-end (overlay-end mouse-secondary-overlay))
              (sr-start-point-info (save-excursion
                                     (goto-char sr-ov-start)
                                     (cons (line-number-at-pos) (current-column))))
              (sr-end-point-info (save-excursion
                                   (goto-char sr-ov-end)
                                   (cons (line-number-at-pos) (current-column))))
              (sr-start-line (car sr-start-point-info))
              (sr-start-column (cdr sr-start-point-info))
              (sr-end-line (car sr-end-point-info))
              (sr-end-column (cdr sr-end-point-info))
              (current-line (line-number-at-pos))
              (current-column (current-column)))
    (if (and (>= current-column sr-start-column )
             (<= current-column sr-end-column))
        (- sr-end-column sr-start-column))))

;;;###autoload
(defun markmacro-secondary-region-set ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (when (bound-and-true-p rectangle-mark-mode)
    (setq-local markmacro-rect-used t))
  (if (region-active-p)
      (progn
        (secondary-selection-from-region)
        (advice-add 'keyboard-quit :before #'markmacro-exit))
    (delete-overlay mouse-secondary-overlay)
    (setq mouse-secondary-start (make-marker))
    (move-marker mouse-secondary-start (point)))
  (deactivate-mark t))

;;;###autoload
(defun markmacro-secondary-region-mark-cursors ()
  "Mark all in the region that is the same as the word under the cursor.

Usage:
1. Select a region.
2. Call `markmacro-secondary-region-set'.
3. Jump to an entity in the region, then Call
`markmacro-secondary-region-mark-cursors'.
4. Type something.
5. Call `markmacro-apply-all' apply kmacro to all mark entities."
  (interactive)
  (cond
   (markmacro-mark-target-orig-info
    (markmacro-exit))
   (markmacro-overlays
    (end-kbd-macro)
    (markmacro-exit)))
  (when-let
      ((sec-region-start (or (overlay-start mouse-secondary-overlay)
                             (point-min)))
       (sec-region-end (or (overlay-end mouse-secondary-overlay)
                           (point-max)))
       (target (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (thing-at-point markmacro-secondary-region-mark-cursors-type t)))
       (mark-bounds '(t))
       (current-point (point))
       (temp-bound 'bound))
    (save-excursion
      (goto-char sec-region-start)
      (pop mark-bounds)
      (while (search-forward target sec-region-end t)
        (let ((mstart (match-beginning 0))
              (mend (match-end 0)))
          (cond
           (markmacro-rect-used
            (when (markmacro-cursor-in-secondary-region-p)
              (if (and (<= mstart current-point)
                       (>= mend current-point))
                  (setq temp-bound (cons mstart mend))
                (push (cons mstart mend) mark-bounds))))
           (t (if (and (<= mstart current-point)
                       (>= mend current-point))
                  (setq temp-bound (cons mstart mend))
                (push (cons mstart mend) mark-bounds)))))))
    (add-to-list 'mark-bounds temp-bound t)

    (dolist (bound mark-bounds)
      (let* ((overlay (make-overlay (car bound) (cdr bound))))
        (overlay-put overlay 'face 'markmacro-mark-face)
        (add-to-list 'markmacro-overlays overlay t)))

    (delete-overlay mouse-secondary-overlay)
    (markmacro-select-last-overlay)))

(defun markmacro-select-last-overlay ()
  (if (> (length markmacro-overlays) 0)
      (progn
        (goto-char (overlay-start (nth (- (length markmacro-overlays) 1) markmacro-overlays)))
        (markmacro-kmacro-start))
    (markmacro-exit)
    (message "Nothing to selected, exit markmacro.")))

(defun markmacro-kmacro-start ()
  (setq-local markmacro-start-overlay
              (cl-dolist (overlay markmacro-overlays)
                (when (and (>= (point) (overlay-start overlay))
                           (< (point) (overlay-end overlay)))
                  (cl-return overlay))))
  (advice-add 'keyboard-quit :before #'markmacro-exit)
  (kmacro-start-macro 0))

;;;###autoload
(defun markmacro-apply-all ()
  (interactive)
  (markmacro-apply (if markmacro-mark-target-orig-info nil t)))

;;;###autoload
(defun markmacro-apply-all-except-first ()
  (interactive)
  (markmacro-apply nil))

(defun markmacro-apply (include-first)
  (end-kbd-macro)

  (save-excursion
    (dolist (overlay (if include-first markmacro-overlays (cdr markmacro-overlays)))
      (unless (equal overlay markmacro-start-overlay)
        (goto-char (overlay-start overlay))
        (call-last-kbd-macro)
        )))

  (markmacro-exit))

;;;###autoload
(defun markmacro-exit ()
  (interactive)
  (advice-remove 'keyboard-quit #'markmacro-exit)
  (setq-local markmacro-rect-used nil)
  (markmacro-delete-overlays)
  
  (delete-overlay mouse-secondary-overlay)
  (setq mouse-secondary-start (make-marker))
  (move-marker mouse-secondary-start (point))

  (when markmacro-mark-target-orig-info
    (goto-char (cdr markmacro-mark-target-orig-info))
    (setq markmacro-mark-target-orig-info nil
          markmacro-mark-target-last nil))

  (deactivate-mark t))

(defun markmacro-delete-overlays ()
  (when markmacro-overlays
    (dolist (overlay markmacro-overlays)
      (delete-overlay overlay))
    (setq-local markmacro-overlays nil)))


(defun markmacro-mark-target (direction)
  (when (and markmacro-overlays
             (not markmacro-mark-target-orig-info))
    (end-kbd-macro)
    (markmacro-exit))
  (when-let* ((lc last-command)
              (tc this-command)
              (last-ov (car (last markmacro-overlays)))
              (last-ov-beg (overlay-start last-ov))
              (last-ov-end (overlay-end last-ov))
              markmacro-mark-target-orig-info)
    (when (and (not (eq tc lc))
               (not (eq lc 'markmacro-unmark-current-target)))
      (goto-char (if (eq tc 'markmacro-mark-current-or-next-target)
                     (cdar markmacro-mark-target-last)
                   (caar markmacro-mark-target-last)))
      (push (cons last-ov-beg last-ov-end) markmacro-mark-target-last)))
  (cond
   ((and markmacro-mark-target-orig-info
         (funcall direction (car markmacro-mark-target-orig-info) nil t))
    (let* ((beg-pos (match-beginning 0))
           (end-pos (match-end 0))
           ov)
      (unless (cl-find beg-pos markmacro-overlays :key 'overlay-start)
        (setq ov (make-overlay beg-pos end-pos))
        (overlay-put ov 'face 'markmacro-mark-face)
        (add-to-list 'markmacro-overlays ov t))))
   ((not markmacro-mark-target-orig-info)
    (when-let* ((target (if (use-region-p)
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))
                          (thing-at-point markmacro-secondary-region-mark-cursors-type)))
                (target-bound (if (use-region-p)
                                  (cons (region-beginning) (region-end))
                                (bounds-of-thing-at-point markmacro-secondary-region-mark-cursors-type)))
                (ov (make-overlay (car target-bound) (cdr target-bound))))
      (deactivate-mark t)
      (advice-add 'keyboard-quit :before #'markmacro-exit)
      (advice-add 'kmacro-start-macro :before #'markmacro-mark-target-goto-orig-pos)
      (setq markmacro-mark-target-orig-info
            (cons target (car target-bound)))
      (goto-char (if (eq this-command 'markmacro-mark-current-or-next-target)
                     (cdr target-bound)
                   (car target-bound)))
      (overlay-put ov 'face 'markmacro-mark-face)
      (add-to-list 'markmacro-overlays ov t)
      (push target-bound markmacro-mark-target-last)))
   (t)))

;;;###autoload
(defun markmacro-mark-current-or-next-target ()
  (interactive)
  (markmacro-mark-target 'search-forward))

;;;###autoload
(defun markmacro-mark-current-or-previous-target ()
  (interactive)
  (markmacro-mark-target 'search-backward))

;;;###autoload
(defun markmacro-unmark-current-target ()
  (interactive)
  (when markmacro-mark-target-orig-info
    (let ((last-sec-ov (car (last markmacro-overlays 2))))
      (when (and (= (overlay-start last-sec-ov)
                    (caar markmacro-mark-target-last))
                 (length> markmacro-mark-target-last 1))
        (pop markmacro-mark-target-last))
      (delete-overlay (car (last markmacro-overlays)))
      (setq markmacro-overlays (butlast markmacro-overlays))
      (goto-char (or (overlay-end last-sec-ov)
                     (cdr markmacro-mark-target-orig-info))))))

(defun markmacro-mark-target-goto-orig-pos (_arg)
  (when markmacro-mark-target-orig-info
    (goto-char (cdr markmacro-mark-target-orig-info)))
  (advice-remove 'kmacro-start-macro #'markmacro-mark-target-goto-orig-pos))

;;;###autoload
(defun markmacro-swap-region ()
  "Swap region and secondary selection."
  (interactive)
  (when-let* ((rbeg (region-beginning))
              (rend (region-end))
              (region-str (when (region-active-p) (buffer-substring-no-properties rbeg rend)))
              (sel-str  (with-current-buffer (overlay-buffer mouse-secondary-overlay)
                          (buffer-substring-no-properties
                           (overlay-start mouse-secondary-overlay)
                           (overlay-end mouse-secondary-overlay))))
              (next-marker (make-marker)))
    (when region-str (delete-region rbeg rend))
    (when sel-str (insert sel-str))
    (move-marker next-marker (point))
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (insert (or region-str "")))
    (when (overlayp mouse-secondary-overlay)
      (delete-overlay mouse-secondary-overlay))
    (setq mouse-secondary-start next-marker)
    (deactivate-mark t)))

(provide 'markmacro)

;;; markmacro.el ends here
