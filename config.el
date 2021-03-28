;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; DO NOT EDIT THIS FILE DIRECTLY
;; This is a file generated from a literate programing source file located at
;; https://gitlab.com/zzamboni/dot-doom/-/blob/master/doom.org
;; You should make any changes there and regenerate it from Emacs org-mode
;; using org-babel-tangle (C-c C-v t)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq user-full-name "Md Arif Shaikh"
      user-mail-address "arifshaikh.astro@gmail.com")

(defun arif/load-file (file-name)
  (if (file-exists-p file-name)
      (load-file file-name)
    (message (format "%s file not found" file-name))))

(arif/load-file "~/.config/emacs/remote-machines.el")
(defun arif/connect-remote-dir (remote-machine-name)
  "Connect to REMOTE-MACHINE-NAME."
  (interactive "sRemote Machine Name: ")
  (setq remote-user-name (cdr (assoc remote-machine-name remote-user-names)))
  (set-buffer (dired (format "/sshx:%s:/home/%s/" remote-machine-name remote-user-name)))
  )

(defun arif/connect-remote-term (remote-machine-name)
  "Connect to terminal in on REMOTE-MACHINE-NAME."
  (interactive "sRemote Machine Name: ")
  (let* ((remote-shell-types '(("comet" . "/bin/bash")
			       ("dodo" . "/bin/zsh")))
	 (default-directory ( format "/sshx:%s:" remote-machine-name))
	 (remote-shell-name  (cdr (assoc remote-machine-name remote-shell-types))))
    (shell remote-shell-name)))

(global-set-key (kbd "C-c r d") #'arif/connect-remote-dir)
(global-set-key (kbd "C-c r s") #'arif/connect-remote-term)

(defun arif/convert-time (from-zone to-zone time-to-convert)
  "Convert TIME from FROM-ZONE to TO-ZONE."
  (interactive "sFrom which timezone (use abbreviation, e.g., EST for Eeastern Standard Time):
sTo which timezone (use abbreviation, e.g., IST for Indian Standard Time):
sTime to be converted (HH:MM PM/pm(optional) DAY(optional)): ")
  (let* ((time-zones '(("EST" . "-0500")
		       ("CST" . "-0600")
		       ("CDT" . "-0500")
		       ("EET" . "+0200")
		       ("CET" . "+0100")
		       ("IST" . "+0530")
		       ("KOLKATA" . "+0530")
		       ("JST" . "+0900")
		       ("TOKYO" . "+0900")))
	 (days '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
	 (time (parse-time-string time-to-convert))
	 (from-zone-u (upcase from-zone))
	 (to-zone-u (upcase to-zone))
	 ;; (from-sec (nth 0 time))
	 (from-min (nth 1 time))
	 (from-hour (nth 2 time))
	 (from-day (nth 6 time)))
    (when (string-match-p (regexp-quote "PM") (upcase time-to-convert))
      (setq  from-hour (+ 12 from-hour)))
    (let* ((time-shift (- (nth 8 (parse-time-string (cdr (assoc to-zone-u time-zones))))
			  (nth 8 (parse-time-string (cdr (assoc from-zone-u time-zones))))))
	   (hour-shift (/ time-shift (* 60 60)))
	   (min-shift (/ (% time-shift (* 60 60)) 60))
	   (to-min (+ from-min min-shift))
	   (to-hour (+ from-hour hour-shift))
	   (to-day-name ""))
      (when (>= to-min 60)
	(setq to-min (- to-min 60))
	(setq to-hour (1+ to-hour)))
      (cond ((>= to-hour 24) (progn
			       (setq to-hour (- to-hour 24))
			       (if (not (equal from-day nil))
				   (setq to-day-name (nth (1+ from-day) days))
				 (setq to-day-name "+ 1 day"))))
	    ((< to-hour 0) (progn
			     (setq to-hour (+ 24 to-hour))
			     (if (not (equal from-day nil))
				 (setq to-day-name (nth (1- from-day) days))
			       (setq to-day-name "- 1 day"))))
	    ((and (> to-hour 0) (< to-hour 24)) (if (not (equal from-day nil))
						    (setq to-day-name (nth from-day days))
						  (setq to-day-name ""))))
      (cond ((= to-hour 0) (setq A-or-P "Midnight"))
	    ((< to-hour 12) (setq A-or-P "AM"))
	    ((= to-hour 12) (setq A-or-P "Noon"))
	    ((> to-hour 12) (progn
			      (setq to-hour (- to-hour 12))
			      (setq A-or-P "PM"))))
      (message (format "%s %s = %.2d:%.2d %s %s %s" (upcase time-to-convert) from-zone-u to-hour to-min (upcase A-or-P) (upcase to-day-name) to-zone-u)))))

(use-package! dired-x
  :after (dired)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(arif/load-file "~/.config/emacs/custom-commands.el")

(use-package! pyvenv
  :defer
  :config
  (if (eq system-type 'darwin)
      (setenv "WORKON_HOME" "/Users/arif/miniconda3/envs/")
    (setenv "WORKON_HOME" "/home/arif/anaconda3/envs/"))
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package! jupyter
  :defer)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(use-package! yasnippets
  :defer
  :init
  (add-hook 'after-init-hook 'yas-global-mode)
  :config
  (setq yas-snippet-dirs '("~/.doom.d/snippets")))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun arif/latex-greek-symbols (english-symbol)
  (interactive)
  (defvar arif/greek-symbols)
  (setq arif/greek-symbols '(("a" . "\\alpha")
			     ("b" . "\\beta")
			     ("c" . "\\chi")
			     ("d" . "\\delta")
			     ("D" . "\\Delta")
			     ("e" . "\\epsilon")
			     ("f" . "\\phi")
			     ("F" . "\\Phi")
			     ("g" . "\\gamma")
			     ("G" . "\\Gamma")
			     ("i" . "\\iota")
			     ("k" . "\\kappa")
			     ("l" . "\\lambda")
			     ("L" . "\\Lambda")
			     ("m" . "\\mu")
			     ("n" . "\\nu")
			     ("o" . "\\omega")
			     ("O" . "\\Omega")
			     ("p" . "\\pi")
			     ("P" . "\\Pi")
			     ("r" . "\\rho")
			     ("s" . "\\sigma")
			     ("t" . "\\tau")
			     ("x" . "\\xi")
			     ("ve" . "\\varepsilon")
			     ("vp" . "\\varphi"))
	)
  (cdr (assoc english-symbol arif/greek-symbols))
  )
