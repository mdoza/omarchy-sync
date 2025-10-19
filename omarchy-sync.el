;;; omarchy-sync.el --- Sync the Omarchy theme.      -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025  Matt Doza
;;
;; Author: Matt Doza <mdoza@me.com>
;; Keywords: faces
;;
;; Redistribution and use in source code form, with or without modification,
;; are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;;

(defvar omarchy-sync/waybar-config-file
  (expand-file-name "~/.config/omarchy/current/theme/waybar.css")
  "The current waybar theme configuration.")

(defvar omarchy-sync/alacritty-config-file
  (expand-file-name "~/.config/omarchy/current/theme/alacritty.toml")
  "The current alacritty theme configuration.")

(defvar omarchy-sync/last-theme-update-time 0
  "The last time the Omarchy theme update callback was called.")

(defvar omarchy-sync/theme-debounce-interval 1
  "Minimum number of seconds between theme update callbacks.")

(defun omarchy-sync/get-waybar-colors ()
  "Read the waybar theme config and return the foreground and background colors."
  (with-temp-buffer
    (insert-file-contents omarchy-sync/waybar-config-file)
    (let ((foreground nil)
	  (background nil))
      (goto-char (point-min))
      (while (re-search-forward
	      "@define-color[ \t]+\\([^ \t]+\\)[ \t]+\\(#[0-9A-Fa-f]+\\);" nil t)
	(let ((name (match-string 1))
	      (hex-color (match-string 2)))
	  (cond
	   ((string= name "foreground") (setq foreground hex-color))
	   ((string= name "background") (setq background hex-color)))))
      (list :foreground foreground
	    :background background))))

(defun omarchy-sync/get-alacritty-colors ()
  "Read the alacritty theme config and return the foreground and background
colors."
  (with-temp-buffer
    (insert-file-contents omarchy-sync/alacritty-config-file)
    (let ((foreground nil)
	  (background nil))
      (goto-char (point-min))
      (when (re-search-forward
	      "^\\[colors\\.primary\\]\\s-*\n\\(\\(?:[^[]\n?\\)+?\\)\\(?:^\\[\\|\\'\\)"
	      nil t)
	(let* ((color-block (match-string 1))
	       (colors nil))
	  (dolist (line (split-string color-block "\n" t))
	    (when (string-match "^\\([^ =]+\\) *= *[\"|\']\\(.*\\)[\"|\']" line)
	      (setq colors
		    (plist-put colors
			       (intern (concat ":" (match-string 1 line)))
			       (match-string 2 line)))))
	  (list
	   :foreground (plist-get colors :foreground)
	   :dim-foreground (plist-get colors :dim-foreground)
	   :background (plist-get colors :background)))))))

(defun omarchy-sync/color-darken (hex percent)
  "Darken a hex color by a percentage amount."
  (apply #'format "#%02x%02x%02x"
	 (mapcar (lambda (n)
		   (round (* 255
			     (max 0.0
				  (* n (- 1.0 (/ percent 100.0)))))))
		 (color-name-to-rgb hex))))

(defun omarchy-sync/normalize-color (color)
  "Normalize a hex color code."
  (when (and color (stringp color))
    (let ((trimmed (string-trim color)))
      (cond
       ((string-match-p "^#[0-9A-Fa-f]\\{6\\}$" trimmed)
	trimmed)
       ((string-match "^0x\\([0-9A-Fa-f]\\{6\\}\\)$" trimmed)
	(concat "#" (match-string 1 trimmed)))
       ((string-match "^[0-9A-Fa-f]\\{6\\}$" trimmed)
	(concat "#" trimmed))
       (t nil)))))

(defun omarchy-sync/get-color (colors-plist color-cat)
  "Gets a normalized color (foreground/background/etc) from a plist."
  (omarchy-sync/normalize-color
   (plist-get colors-plist color-cat)))

(defun omarchy-sync ()
  "Sync up with the Omarchy current theme."
  (interactive)
  (let* ((waybar-colors (omarchy-sync/get-waybar-colors))
	 (alacritty-colors (omarchy-sync/get-alacritty-colors))
	 (default-bg (face-background 'default))
	 (default-fg (face-foreground 'default))
	 (default-ml-bg (face-background 'mode-line))
	 (default-ml-fg (face-foreground 'mode-line))
	 (default-ml-inactive-bg (face-background 'mode-line-inactive))
	 (default-ml-inactive-fg (face-foreground 'mode-line-inactive))
	 (ml-bg-color (or (omarchy-sync/get-color waybar-colors :background)
			  default-ml-bg))
	 (ml-fg-color (or (omarchy-sync/get-color waybar-colors :foreground)
			  default-ml-fg))
	 (bg-color (or (omarchy-sync/get-color alacritty-colors :background)
		       default-bg))
	 (fg-color (or (omarchy-sync/get-color alacritty-colors :foreground)
		       default-fg))
	 (fg-dim-color (or
			(omarchy-sync/get-color alacritty-colors :dim-foreground)
			(omarchy-sync/color-darken ml-fg-color 32))))

    (when window-system
      (set-background-color bg-color)
      (set-foreground-color fg-color))

    (custom-set-faces
     `(default ((t (:background ,bg-color :foreground ,fg-color))))
     `(mode-line ((t (:background ,ml-bg-color :foreground ,ml-fg-color))))
     `(mode-line-inactive ((t (:foreground ,fg-dim-color))))))

    (message "Omarchy theme updated."))

(defun omarchy-sync/theme-update-callback (e)
  "The Omarchy theme has been updated."
  (let ((time-now (float-time)))
    (when (> (- time-now omarchy-sync/last-theme-update-time)
	     omarchy-sync/theme-debounce-interval)
      (setq omarchy-sync/last-theme-update-time time-now)
      (run-with-timer 1 nil #'omarchy-sync/start-file-watcher)
      (omarchy-sync))))

(defun omarchy-sync/start-file-watcher ()
  "Start watching for changes in the current theme directory."
  (file-notify-add-watch
   omarchy-sync/waybar-config-file '(change attribute-change)
   #'omarchy-sync/theme-update-callback))

(defun omarchy-sync/init ()
  (interactive)
  (omarchy-sync)
  (omarchy-sync/start-file-watcher))

(provide 'omarchy-sync)
;;; omarchy-sync.el ends here
