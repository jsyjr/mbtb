;;; mbtb.el --- Mini-Buffer overlaid on the Tab Bar -*- lexical-binding: t -*-

;; Author: John S. Yates, Jr. <john@yates-sheets.org>
;; URL: https://github.com/jsyjr/mbtb
;; Version: 0.1
;; Keywords: convenience

;;; Copyright © 2022 John S. Yates, Jr. <john@yates-sheets.org>
;;;
;;; This file is NOT part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Mbtb is proof-of-concept for a minibuffer positioned over the menu-bar.
;;; It is implemented using a separate mini-buffer frame per primary frame.
;;;

;;; Installation:
;;;
;;; To try this package your default-frame-alist needs these entries:
;;;
;;;    (minibuffer . nil)        ; supress minibuffer at bottom of frame
;;;    (tab-bar-lines . 1)       ; space will be overlaid by the MBTB frame
;;;
;;; You also need to enable a vaccuous version of tab-bar-mode enable:
;;;
;;;    (setq tab-bar-format '((lambda () " ")))
;;;
;;;    (tab-bar-mode  +1)
```


;;; Code:

(defgroup mbtb nil
  "Mbtb is proof-of-concept for a minibuffer positioned over the menu-bar."
  :prefix "mbtb"
  :group 'minibuffer)

(defcustom mbtb-debounce-delay 0.02
  "Seconds to wait before declaring a frame focus change debounced."
  :type 'float)

(defun mbtb-resize-mbf (mbf)
  "MBTB callback for the resize-mini-frames option."
  (let ((width (frame-text-width mbf)))
    (fit-frame-to-buffer-1 mbf nil 1 width width 'vertically)))

(with-eval-after-load 'mbtb     ; configure emacs options
  (custom-set-variables
   '(minibuffer-frame-alist     ; unspecified items from default-frame-alist
     `((minibuffer . only)
       (minibuffer-exit . nil)
       (visibility . nil)
       (min-height . 1)
       (menu-bar-lines . 0)
       (tool-bar-lines . 0)
       (tab-bar-lines . 0)
       (internal-border-width . 1)
       (skip-taskbar . t)
       (undecorated . t)
       (desktop-dont-save . t)))
   '(resize-mini-frames #'mbtb-resize-mbf)))

(defun mbtb-mbf (owner)
  "Qualify OWNER by returning its dedicated separate MBF or nil."
  (let ((owner-minibuffer (frame-parameter owner 'minibuffer)))
    (cond
     ((not owner-minibuffer)
      nil)
     ((eq owner-minibuffer 'only)
      nil)                      ; OWNER is actually a separate MBF
     (t
      (let ((mbf (window-frame owner-minibuffer)))
	(if (eq (frame-parameter mbf 'minibuffer) 'only)
	    mbf
	  nil))))))

(defun mbtb-apply-owner-width (owner mbf)
  "Apply qualified OWNER's width to separate MBF."
  (when (frame-size-changed-p owner)
    (let ((width (frame-parameter owner 'width)))
      (unless (eq (frame-parameter mbf 'width) width)
	(set-frame-parameter mbf 'width  width)))))

(defun mbtb-track-owner-width (owner)
  "Qualify OWNER and adjust its separate MBF's width."
  (let ((mbf (mbtb-mbf owner)))
    (when mbf
      (mbtb-apply-owner-width owner mbf))))
(add-hook 'window-size-change-functions #'mbtb-track-owner-width)

(defun mbtb-setup-colors-and-width (owner mbf)
  "Given OWNER and MBF, ensure proper MBF color, position and width."
  (set-face-background 'fringe          "black" mbf)
  (set-face-background 'internal-border "white" mbf)
  (mbtb-apply-owner-width owner mbf))

(defun mbtb-setup-minibuffer ()
  "Invoke mbtb-setup-colors-and-width with owner and mbf."
  (let ((owner (window-frame (minibuffer-selected-window))))
    (mbtb-setup-colors-and-width owner (mbtb-mbf owner))))
(add-hook 'minibuffer-setup-hook #'mbtb-setup-minibuffer)

(defun mbtb-after-make-frame (owner)
  "Final steps in creating an MBTB minibuffer frame."
  (let ((mbf (mbtb-mbf owner)))
    (when mbf
      (set-frame-parameter mbf 'parent-frame owner)
      (set-frame-position mbf 0 0)
      (mbtb-resize-mbf mbf)
      (mbtb-setup-colors-and-width owner mbf)
      (set-frame-parameter mbf 'visibility t))))
(add-hook 'after-make-frame-functions #'mbtb-after-make-frame)

(defun mbtb-refresh-owner-mbf-stacking-order (owner)
  "Refresh an MBF's stacking based oon whether it or its OWNER has focus."
  (let ((mbf (mbtb-mbf owner)))
    (when mbf
      (frame-restack mbf owner t))
      (when (frame-focus-state owner)
        (with-current-buffer (window-buffer (frame-root-window mbf))
          (delete-region (minibuffer-prompt-end) (point-max)))
        (mbtb-resize-mbf mbf))))

(defvar mbtb-focus-events-timer nil
  "When non-nil a debounce timer is running.")

(defvar mbtb-last-focus-state 'force-update
  "Most recent ‘frame-focus-state’ from a focus change event.")

(defun mbtb-focus-change ()
  "On actual focus change refresh all owner / mbf stacking orders."
  (setq mbtb-focus-events-timer nil)
  (let ((state (with-no-warnings (frame-focus-state))))
    (unless (eq mbtb-last-focus-state state)
      (setq mbtb-last-focus-state state)
      (dolist (frame (frame-list))
        (mbtb-refresh-owner-mbf-stacking-order frame)))))

;; This debouncing logic was inspired by
;; https://www.reddit.com/r/emacs/comments/kxsgtn/ignore_spurious_focus_events_for

(defun mbtb-debounce-focus-change ()
  "Drain spurious events by canceling a running timer and setup a new one."
  (if (timerp mbtb-focus-events-timer)
      (cancel-timer mbtb-focus-events-timer))
  (setq mbtb-focus-events-timer
        (run-with-timer mbtb-debounce-delay nil #'mbtb-focus-change)))
(add-function :after after-focus-change-function #'mbtb-debounce-focus-change)


(provide 'mbtb)
;;; mbtb.el ends here
