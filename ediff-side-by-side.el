;;; ediff-side-by-side.el --- Make Ediff navigate side by side.

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: https://github.com/tarao/ediff-side-by-side-el
;; Version: 0.1
;; Keywords: emacs diff ediff

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ediff-init)
(require 'ediff-util)
(require 'ediff-help)
(require 'whitespace)
(eval-when-compile (require 'cl))

;; Configurations

(defgroup ediff-sbs nil
  "Ediff side by side."
  :prefix "ediff-sbs:"
  :group 'ediff)

(defcustom ediff-sbs:before-quit-hook nil
  "Hooks to run before quitting an Ediff session."
  :type 'hook)

(defcustom ediff-sbs:wide-display-grows-to 'center
  "Direction to which `ediff-sbs:make-wide-display' grows the
size of the frame."
  :type '(choice (const :tag "Center" center)
                 (const :tag "Left" left)
                 (const :tag "Right" right)))

(defun ediff-sbs-before-quit (&rest _args)
  (run-hooks 'ediff-sbs:before-quit-hook))
(advice-add 'ediff-really-quit :before 'ediff-sbs-before-quit)

(defun ediff-sbs-setup-keymap ()
  (let ((map ediff-mode-map))
    (define-key map "j" #'ediff-sbs:scroll-line-down)
    (define-key map "k" #'ediff-sbs:scroll-line-up)
    (define-key map "J" #'ediff-sbs:scroll-down)
    (define-key map "K" #'ediff-sbs:scroll-up)
    (define-key map "h" #'ediff-sbs:scroll-left)
    (define-key map "l" #'ediff-sbs:scroll-right)))
(add-hook 'ediff-keymap-setup-hook 'ediff-sbs-setup-keymap)

(loop for sym in '(ediff-long-help-message-compare3
                   ediff-long-help-message-compare2
                   ediff-long-help-message-narrow2
                   ediff-long-help-message-word-mode
                   ediff-long-help-message-merge)
      do (progn
           (set sym (replace-regexp-in-string
                     "  j -jump to diff "
                     "j/k -go line dn/up"
                     (symbol-value sym)))
           (set sym (replace-regexp-in-string
                     "v/V -scroll up/dn"
                     "J/K -scroll dn/up"
                     (symbol-value sym)))
           (set sym (replace-regexp-in-string
                     "</>"
                     "h/l"
                     (symbol-value sym)))))

(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-merge-split-window-function 'split-window-horizontally)

;; Filling hunks

(defun ediff-sbs-types ()
  (mapcar #'car ediff-difference-vector-alist))

(defun ediff-sbs-diff-overlay (n type)
  (ignore-errors (ediff-get-diff-overlay n type)))

(defun ediff-sbs-overlays (n &optional types)
  (let ((types (or types (ediff-sbs-types))))
    (loop for type in types
          for overlay = (ediff-sbs-diff-overlay n type)
          when overlay
          collect (list type overlay))))

(defun ediff-sbs-overlay-height (overlay)
  (let ((buffer (overlay-buffer overlay))
        (beg (overlay-start overlay))
        (end (overlay-end overlay)))
    (with-current-buffer buffer
      (count-lines beg end))))

(defun ediff-sbs-overlay-heights (n &optional types)
  (sort
   (loop for (type o) in (ediff-sbs-overlays n types)
         collect (list type o (ediff-sbs-overlay-height o)))
   #'(lambda (a b) (> (nth 2 a) (nth 2 b)))))

(defun ediff-sbs-expand-overlay (overlay lines)
  (when (> lines 0)
    (let ((lines (propertize (make-string lines ?\n)
                             'face 'whitespace-hspace)))
      (overlay-put overlay 'after-string lines))))

(defun ediff-sbs-fill-hunks ()
  (let ((types (ediff-sbs-types)))
    (loop for n below ediff-number-of-differences
          for (first . others) = (ediff-sbs-overlay-heights n types)
          for max = (nth 2 first)
          do (loop for (_ o h _ _) in others
                   do (ediff-sbs-expand-overlay o (- max h))))))
(add-hook 'ediff-startup-hook 'ediff-sbs-fill-hunks)
(advice-add 'ediff-update-diffs :after 'ediff-sbs-fill-hunks)

;; Adjusting scroll position

(defun ediff-sbs-get-window (type)
  (let* ((sym (intern (format "ediff-window-%s" type)))
         (window (and (boundp sym) (symbol-value sym))))
    (and (window-live-p window) window)))

(defmacro ediff-sbs-with-window-buffer (type &rest body)
  (declare (indent 1) (debug t))
  `(let ((buffer (ediff-get-buffer ,type))
         (window (ediff-sbs-get-window ,type)))
     (when (and window buffer)
       (with-selected-window window
         (with-current-buffer buffer
           ,@body)))))

(defun ediff-sbs-diff-at-window-start (type)
  (let* ((pos (ediff-sbs-with-window-buffer type
                (1+ (window-start))))
         (diff (ediff-diff-at-point type pos 'before))
         (overlay (ediff-sbs-diff-overlay diff type))
         (start (and overlay (overlay-start overlay))))
    (if (or (and start (< pos start)) (= diff ediff-number-of-differences))
        (1- diff)
      diff)))

(defun ediff-sbs-overlay-offset (type overlay)
  (ediff-sbs-with-window-buffer type
    (let ((pos (window-start))
          (beg (overlay-start overlay))
          (end (overlay-end overlay)))
      (if (> pos end)
          (list (count-lines end pos) #'overlay-end)
        (if (> pos beg)
            (list (count-lines beg pos) #'overlay-start)
          (list (- (count-lines pos beg)) #'overlay-start))))))

(defun ediff-sbs-overlay-offsets (n &optional types)
  (loop for (type o h) in (ediff-sbs-overlay-heights n types)
        for offset = (ediff-sbs-overlay-offset type o)
        when offset
        collect (list* type o h offset)))

(defun ediff-sbs-scroll-to (type pos)
  (ediff-sbs-with-window-buffer type
    (goto-char pos)
    (set-window-start (selected-window) pos t)))

(defun ediff-sbs-rescroll-1 (type overlay offset fun limit)
  (let* ((from (funcall fun overlay))
         (offset (if (and (eq fun #'overlay-start) (>= offset limit))
                     0
                   offset))
         (buffer (overlay-buffer overlay))
         (pos (with-current-buffer buffer
                (save-excursion
                  (goto-char from)
                  (forward-line offset)
                  (point)))))
    (ediff-sbs-scroll-to type pos)))

(defun ediff-sbs-rescroll (&optional type)
  (let* ((types (ediff-sbs-types))
         (primary (or type (car types)))
         (n (ediff-sbs-diff-at-window-start primary)))
    (if (and (<= 0 n) (< n ediff-number-of-differences))
        (let* ((diffs (ediff-sbs-overlay-offsets n types))
               (first (car diffs))
               (others (cdr diffs))
               (offset (nth 3 first))
               (fun (nth 4 first)))
          (loop for (type o h _ _) in others
                do (ediff-sbs-rescroll-1 type o offset fun h)))
      (ediff-sbs-with-window-buffer primary
        (let ((pos (window-start)))
          (loop for type in types
                do (ediff-sbs-scroll-to type pos)))))))

(defun ediff-sbs:rescroll (&rest _args)
  "Adjust scroll positions of Ediff buffers to show differences
side by side."
  (ediff-sbs-rescroll))
(add-hook 'ediff-select-hook 'ediff-sbs:rescroll)
(advice-add 'ediff-scroll-vertically :after 'ediff-sbs:rescroll)

(defun ediff-sbs-recenter (&rest _args)
  (ediff-sbs-rescroll (ediff-char-to-buftype (ediff-last-command-char))))
(advice-add 'ediff-recenter :after 'ediff-sbs-recenter)

;; Making a wide display

(defun ediff-sbs:make-wide-display ()
  "Widen the frame size to fit windows into the frame.

The size will be doubled or tripled (for a 3-way job) or becomes
the screen size if it gets too large.

The position of the frame is also adjusted according to the value
of `ediff-sbs:wide-display-grows-to'.

This function is meant to be used as
`ediff-make-wide-display-function' and called from
`ediff-toggle-wide-display'."
  (let* ((frame (window-frame ediff-window-A))
         (params (frame-parameters frame))
         (left (cdr (assq 'left params)))
         (width (frame-width frame))
         (cw (frame-char-width frame))
         (border-pixel-width (- (frame-pixel-width frame) (* width cw)))
         (border-width (/ border-pixel-width cw))
         (windows (if (and (boundp 'ediff-3way-job) ediff-3way-job) 3 2))
         (new-width (+ (* width windows) (* border-width (1- windows))))
         (new-pixel-width (+ (* new-width cw) border-pixel-width))
         (display-pixel-width (with-selected-frame frame
                                (display-pixel-width))))
    (setq ediff-wide-display-orig-parameters
          (list (cons 'left (max 0 (eval left)))
                (cons 'width width))
          ediff-wide-display-frame frame)
    (when (> new-pixel-width display-pixel-width)
      (setq new-width (/ (- display-pixel-width border-pixel-width) cw)
            new-pixel-width (+ (* new-width cw) border-pixel-width)))
    (let* ((growth (* (- new-width width) cw))
           (offset (cond ((eq ediff-sbs:wide-display-grows-to 'left) growth)
                         ((eq ediff-sbs:wide-display-grows-to 'right) 0)
                         (t (/ growth 2))))
           (min 0)
           (max (- display-pixel-width new-pixel-width min)))
      (setq left (- left offset)
            left (max min left)
            left (min max left)))
    (modify-frame-parameters
     frame `((left . ,left) (width . ,new-width) (user-position . t)))))
(setq-default ediff-make-wide-display-function 'ediff-sbs:make-wide-display)

(defun ediff-sbs:use-wide-display ()
  "Widen the frame size if it has not been widened yet."
  (when (and (display-graphic-p)
             (not ediff-wide-display-p))
    (ediff-toggle-wide-display)))
(add-hook 'ediff-startup-hook 'ediff-sbs:use-wide-display)

(defun ediff-sbs:unuse-wide-display ()
  "Unwiden the frame size if it has been widened."
  (when (and (display-graphic-p)
             ediff-wide-display-p)
    (ediff-toggle-wide-display)))
(add-hook 'ediff-sbs:before-quit-hook 'ediff-sbs:unuse-wide-display)

;; Commands

(defun ediff-sbs-operate-on-windows (operation &rest args)
  (ediff-barf-if-not-control-buffer)
  (loop for type in (ediff-sbs-types)
        do (ediff-sbs-with-window-buffer type
             (apply operation args)))
  (ediff-sbs:rescroll))

(defun ediff-sbs:scroll-line-down (count)
  "Scroll down buffers A, B (and C if appropriate).

With optional argument COUNT, scroll COUNT lines; otherwise
scroll one line down."
  (interactive "p")
  (ediff-sbs-operate-on-windows #'scroll-up count))

(defun ediff-sbs:scroll-line-up (count)
  "Scroll up buffers A, B (and C if appropriate).

With optional argument COUNT, scroll COUNT lines; otherwise
scroll one line up."
  (interactive "p")
  (ediff-sbs-operate-on-windows #'scroll-down count))

(defun ediff-sbs-scroll-down-1 (count)
  (let* ((max (1+ (- (line-number-at-pos (point-max))
                       (line-number-at-pos (window-end)))))
         (count (or count (min max (/ (- (window-height) 1) 2)))))
    (scroll-up count)))

(defun ediff-sbs-scroll-up-1 (count)
  (let* ((max (1- (line-number-at-pos (window-start))))
         (count (or count (min max (/ (- (window-height) 1) 2)))))
    (scroll-down count)))

(defun ediff-sbs:scroll-down (count)
  "Scroll down buffers A, B (and C if appropriate).

With optional argument COUNT, scroll COUNT lines; otherwise
scroll by nearly the on half of the window height."
  (interactive "P")
  (ediff-sbs-operate-on-windows #'ediff-sbs-scroll-down-1 count))

(defun ediff-sbs:scroll-up (count)
  "Scroll up buffers A, B (and C if appropriate).

With optional argument COUNT, scroll COUNT lines; otherwise
scroll by nearly the on half of the window height."
  (interactive "P")
  (ediff-sbs-operate-on-windows #'ediff-sbs-scroll-up-1 count))

(defun ediff-sbs:scroll-left (count)
  "Scroll left buffers A, B (and C if appropriate).

With optional argument COUNT, scroll COUNT columns; otherwise
scroll by nearly the width of the window."
  (interactive "P")
  (let ((last-command-event ?>))
    (ediff-scroll-horizontally count)))

(defun ediff-sbs:scroll-right (count)
  "Scroll right buffers A, B (and C if appropriate).

With optional argument COUNT, scroll COUNT columns; otherwise
scroll by nearly the width of the window."
  (interactive "P")
  (let ((last-command-event ?<))
    (ediff-scroll-horizontally count)))

(provide 'ediff-side-by-side)
;;; ediff-side-by-side.el ends here
