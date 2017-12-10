;;; Init --- Summary
;;; Commentary:

;;; RLL macOS/FreeBSD/Linux/Windows portable Emacs initialisation file
;;; This file should work across the OSs, both in text and GUI Emacs.
;;; There is code that enables Lisp/Slime, which may need to be tidied up
;;; or commented out.


;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; PACKAGES ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages in use RLL
(setq package-list '(color-theme-solarized smooth-scrolling sublimity evil async helm
                                           evil-surround evil-leader whitespace
                                           smart-tabs-mode undo-tree evil-quickscope
                                           evil-easymotion evil-numbers evil-matchit
                                           markdown-mode flycheck web-mode php-mode
					   projectile helm-projectile flx-ido
                                           ))
;; Considered but not using: evil-tabs

;; Package system init
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; MANAGED SETTINGS ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(global-linum-mode t)
 '(mac-mouse-wheel-mode t)
 '(mac-mouse-wheel-smooth-scroll t)
 '(mouse-wheel-mode t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(ns-right-alternate-modifier (quote none))
 '(ns-right-command-modifier (quote none))
 '(package-selected-packages
   (quote
    (flx-ido helm-projectile projectile markdown-mode flycheck web-mode php-mode evil-matchit evil-easymotion evil-quickscope smart-tabs-mode evil-leader evil-surround helm async evil sublimity smooth-scrolling color-theme-solarized evil-numbers transpose-frame 0blayout ## dash solarized-theme)))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote sendmail-send-it))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; VISUALS ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other visual customisations RLL
;;(require 'solarized)
(load-theme 'solarized t)

(if (display-graphic-p) (custom-set-variables '(tool-bar-mode nil)))

(set-face-attribute 'default nil :family "Menlo" :height 120 :weight 'normal)

;; Auto-size the window when in GUI to top, most of the height (bar 100 px) and
;; centred on the current display monitor.
;;(defun set-frame-size-according-to-resolution ()
;;  (interactive)
;;  (if (display-graphic-p)
;;      (progn
;;      (add-to-list 'default-frame-alist (cons 'top 0))
;;      (add-to-list 'default-frame-alist (cons 'width 140))
;;      (add-to-list 'default-frame-alist
;;                   (cons 'height (/ (- (display-pixel-height) 100)
;;                                    (frame-char-height))))
;;      (add-to-list 'default-frame-alist
;;                   (cons 'left
;;                         (/ (-
;;                             (nth 3 (car (frame-monitor-attributes)))
;;                             (* 140 (frame-char-width)))
;;                            2)))
;;      ))
;;  )

(defun set-frame-size-according-to-resolution (frame)
  (select-frame frame)
  (interactive)
  (if (display-graphic-p)
      (progn
        (set-frame-width frame 165)
        (set-frame-height frame (/ (- (display-pixel-height) 100) (frame-char-height)))
        (set-frame-position frame
                            (/ (-
                                (nth 3 (car (frame-monitor-attributes)))
                                (* 175 (frame-char-width)))
                               2)
                            0)
        ))
  )

(add-hook 'after-make-frame-functions 'set-frame-size-according-to-resolution)
;; (set-frame-size-according-to-resolution (selected-frame))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; FULL SCREEN ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; TABS & INDENT ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)
;; (setq-default tab-width 4)
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript 'python 'nxml)

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; NICETIES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save sessions on exit
;; (desktop-save-mode 1)
(savehist-mode 1)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; BELL ALERT ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subtle-visible-bell ()
  "A friendlier visual bell effect."
  (lambda ()
    (unless (memq this-command
                  '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
      (invert-face 'mode-line)
      (run-with-timer 0.1 nil 'invert-face 'mode-line))))

(setq visible-bell nil
      ring-bell-function #'subtle-visible-bell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SCROLLING ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if window-system
    (require 'sublimity)
  (require 'sublimity-scroll)
  ;;(require 'sublimity-map) ;; experimental
  ;;(require 'sublimity-attractive)
  (sublimity-mode 1)
  (setq sublimity-scroll-weight 5
	sublimity-scroll-drift-length 10)
  ;;(sublimity-map-set-delay 0)

  ;; Mouse smoothish scrolling
  (require 'smooth-scrolling)
  (setq smooth-scroll-margin 2)
  (setq smooth-scrolling-mode 1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
  )

(setq mouse-wheel-progressive-speed t)

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 3)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 3)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)
;; See further below (evil section) workaround for an xterm mouse issue


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; THEME ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize colour theme depending on the colour of the terminal, defaulting to light
(setq frame-background-mode (if (equal "12;8" (getenv "COLORFGBG")) 'dark 'light))
(load-theme 'solarized)

(defun toggle-theme-background ()
  "Toggle light/dark background color scheme."
  (interactive)
  (if (eq frame-background-mode 'dark)
      (setq frame-background-mode 'light)
    (setq frame-background-mode 'dark))
  (load-theme 'solarized)
  (mapc 'frame-set-background-mode (frame-list)))

(global-set-key (kbd "<f5>") 'toggle-theme-background)

;; Needs reloading
(load-theme 'solarized t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; EVIL ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq evil-want-C-u-scroll t)
(setq evil-want-fine-undo t)

;; Based on https://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
(setq evil-emacs-state-cursor '("#dc322f" box))        ;; Red solarized
(setq evil-normal-state-cursor '("#859900" box))       ;; Green
(setq evil-visual-state-cursor '("#cb4b16" box))       ;; Orange
(setq evil-insert-state-cursor '("#dc322f" bar))
(setq evil-replace-state-cursor '("#dc322f" bar))
(setq evil-operator-state-cursor '("#dc322f" hollow))

(require 'evil)
(evil-mode 1)
;; (require 'evil-tabs)
;; (global-evil-tabs-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "1" 'delete-other-windows
  "m" 'helm-bookmarks
  "0" 'toggle-fullscreen
  "w" 'whitespace-mode
  ";" 'comment-line)

(require 'evil-quickscope)
(global-evil-quickscope-always-mode 1)

(require 'evil-easymotion)
(evilem-default-keybindings "SPC")

(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Workaround for xterm-mouse-mode issue in text terminal with evil-mode
(unless window-system
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map [down-mouse-1] nil))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; OTHER SHORTCUTS ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)  ;; disable, allow f9 for toggling
(global-set-key (kbd "<f9>")
				'toggle-menu-bar-mode-from-frame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; UNDO TREE ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; BACKUPS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; ESC QUITS ALL ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; ASYNC ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROJECTILE and FLX-IDO ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(projectile-global-mode)
(setq projectile-enable-caching t)


(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 80)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; HELM ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)

;; save bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1) ;; save after every change


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; CLIPBOARD ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq select-enable-clipboard nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; SLIME ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialise SLIME
;(add-to-list 'load-path "~/Projects/slime")
;(require 'slime-autoloads)
;(setq inferior-list-program "/opt/local/bin/lisp")
;(add-to-list 'slime-contribs 'slime-fancy)
;(eval-after-load 'slime
;  '(define-key slime-prefix-map (kbd ",h") 'slime-documentation-lookup))
;
;(mapc 'frame-set-background-mode (frame-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OS X GUI/SERVER  ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Following https://korewanetadesu.com/emacs-on-os-x.html

(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))
