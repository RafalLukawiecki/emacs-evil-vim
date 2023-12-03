;; Init --- Summary
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
(defvar package-list '(solarized-theme smooth-scrolling sublimity
                                       evil async helm
                                       evil-surround evil-leader
                                       evil-collection
                                       whitespace smart-tabs-mode
                                       undo-tree evil-quickscope
                                       evil-easymotion
                                       evil-numbers evil-matchit
                                       markdown-mode flycheck
                                       web-mode php-mode
                                       projectile helm-gtags
                                       helm-projectile flx-ido
                                       dtrt-indent
                                       fill-column-indicator org
                                       evil-org apache-mode
                                       logview robots-txt-mode
                                       auto-complete ggtags
                                       js2-mode s f dash xcscope
                                       popup ac-php-core
                                       company-php ac-php
                                       web-mode ac-js2
                                       drupal-mode cider
                                       company-anaconda jedi
                                       yasnippet elpy
                                       exec-path-from-shell
                                       yaml-mode
				       editorconfig
				       ediprolog
				       quelpa quelpa-use-package
				       ))
;; Considered but not using: evil-tabs

;; Package system init
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;;("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/"))
                         ))

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
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(datetime-timezone "Europe/Dublin" t)
 '(ediprolog-system 'swi)
 '(mac-mouse-wheel-mode t)
 '(mac-mouse-wheel-smooth-scroll t)
 '(mouse-wheel-mode t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(ns-alternate-modifier 'meta)
 '(ns-command-modifier 'super)
 '(ns-right-alternate-modifier 'none)
 '(ns-right-command-modifier 'none)
 '(package-selected-packages
   '(evil-collection auctex cider js2-mode ggtags auto-complete robots-txt-mode php-extras drupal-mode logview org evil-org apache-mode fill-column-indicator dtrt-indent powerline flx-ido helm-projectile projectile markdown-mode flycheck web-mode php-mode evil-matchit evil-easymotion evil-quickscope smart-tabs-mode evil-leader evil-surround helm async evil sublimity smooth-scrolling evil-numbers transpose-frame 0blayout ## dash solarized-theme))
 '(scroll-bar-mode nil)
 '(send-mail-function 'sendmail-send-it)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; TZ ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((tz (shell-command-to-string ". ~/.timezone; echo -n $TZ")))
  (setenv "TZ" tz)
  (customize-set-variable 'datetime-timezone tz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PATH ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'exec-path "/opt/local/bin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; VISUALS ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Other visual customisations RLL
;;(require 'solarized)
;;(load-theme 'solarized t)


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
  "Resizes the FRAME according to the size of the monitor."
  (interactive)
  (select-frame frame)
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
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; TABS & INDENT ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs spaces trailing
                              ;; lines-tail
                              space-before-tab newline
                              indentation empty space-after-tab
                              space-mark tab-mark newline-mark))

;; (setq-default tab-width 4)
;; To disable tabs, or enable BSD, try https://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript 'nxml)

(smart-tabs-add-language-support php php-mode-hook
  ((php-indent-line . 2)
   (php-indent-region . 2)))

(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Auto fill column modes, use fci-mode
(require 'fill-column-indicator)
(add-hook 'text-mode-hook (lambda ()
                            (turn-on-auto-fill)
                            (set-fill-column 82)))
(add-hook 'markdown-mode-hook (lambda ()
                                (turn-on-auto-fill)
                                (fci-mode)
                                (set-fill-column 82)))

(add-hook 'python-mode-hook (lambda ()
                              (fci-mode)
                              (setq column-number-mode t)
                              (elpy-enable)
                              (setq python-shell-interpreter "jupyter-console"
                                    python-shell-interpreter-args "--simple-prompt"
                                    python-shell-prompt-detect-failure-warning nil
                                    elpy-shell-echo-output nil)
                              (add-to-list 'python-shell-completion-native-disabled-interpreters
                                           "jupyter")
                              (setq-default indent-tabs-mode nil)
                              (setq-default tab-width 4)
                              (setq-default python-indent 4)
                              (set-fill-column 79)))

(add-hook 'c-mode-hook (lambda ()
                         (fci-mode)
                         (setq column-number-mode t)
                         (set-fill-column 94)))

(add-hook 'sh-mode-hook (lambda ()
                          (set-fill-column 94)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; MODES ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("vhost\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.php\\|\\.php\\.dev|\\.php\\.prod'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; NICETIES ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save sessions on exit
;; (desktop-save-mode 1)
(savehist-mode 1)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-display-line-numbers-mode)


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

;; (if window-system
    ;;   (require 'sublimity)
    ;; (require 'sublimity-scroll)
    ;; ;;(require 'sublimity-map) ;; experimental
    ;; ;;(require 'sublimity-attractive)
    ;; (sublimity-mode 1)
    ;; (setq sublimity-scroll-weight 5
    ;;       sublimity-scroll-drift-length 10)
    ;; ;;(sublimity-map-set-delay 0)

    ;; Mouse smoothish scrolling
  ;;   (require 'smooth-scrolling)
  ;; (setq smooth-scroll-margin 2)
  ;; (setq smooth-scrolling-mode 1)
  ;; (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
  ;; )

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

;; Can above be eliminated now that pixel-scroll is available?
(pixel-scroll-precision-mode 1)

;; See further below (evil section) workaround for an xterm mouse issue



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; THEME ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize colour theme depending on the colour of the terminal, defaulting to light
(if (equal "11;15" (getenv "COLORFGBG"))
    (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t))

(defun toggle-theme-background ()
  "Toggle light/dark background color scheme."
  (interactive)
  (if (eq (car custom-enabled-themes) 'solarized-dark)
      (load-theme 'solarized-light t)
    (load-theme 'solarized-dark t)))

(global-set-key (kbd "<f5>") 'toggle-theme-background)


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
  "f" 'projectile-find-file
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

;; Enable M-. to work in lisp modes to show definition at point
(define-key evil-normal-state-map (kbd "M-.")
  `(menu-item "" evil-repeat-pop :filter
              ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))

;; Workaround for xterm-mouse-mode issue in text terminal with evil-mode
(unless window-system
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map [down-mouse-1] nil))
  )

;; (evil-collection-init 'reftex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; OTHER SHORTCUTS ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode -1)  ;; disable, allow f9 for toggling
(global-set-key (kbd "<f9>")
                'toggle-menu-bar-mode-from-frame)


(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
From BEG to END.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\S-+" "\\&" beg end))


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

(projectile-mode)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


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

;; save bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1) ;; save after every change


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; CLIPBOARD ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq select-enable-clipboard t)

(defun my:copy-to-tmux (text &optional push)
  "Copy TEXT (vim yank) to tmux clipboard using yank.sh.  PUSH ignored."
  (if window-system
      (gui-select-text text)
    (if (file-exists-p "~/.tmux/yank.sh")
        (let ((process-connection-type nil))
          (let ((proc (start-process "yank.sh" "*Messages*" "~/.tmux/yank.sh")))
            (process-send-string proc text)
            (process-send-eof proc))))
    ))

(setq interprogram-cut-function 'my:copy-to-tmux)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  COPILOT  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'quelpa)
(require 'quelpa-use-package)
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; ORG ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; C/JAVA ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; PHP ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda ()
             ;; Enable company-mode
             (company-mode t)
             (require 'company-php)

             ;; Enable ElDoc support (optional)
             (ac-php-core-eldoc-setup)

             (set (make-local-variable 'company-backends)
                  '((company-ac-php-backend company-dabbrev-code)
                    company-capf company-files))

             ;; Jump to definition (optional)
             (define-key php-mode-map (kbd "M-]")
               'ac-php-find-symbol-at-point)

             ;; Return back (optional)
             (define-key php-mode-map (kbd "M-[")
               'ac-php-location-stack-back)

	     ;; ggtags
	     (ggtags-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PYTHON ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; SLIME ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialise SLIME
(add-to-list 'load-path "/Users/rafal/Projects/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/opt/local/bin/sbcl")
(add-to-list 'slime-contribs 'slime-fancy 'inferior-slime)
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))))
(eval-after-load 'slime
  '(progn
     (define-key slime-prefix-map (kbd ",h") 'slime-documentation-lookup)
     (define-key slime-repl-mode-map (kbd "M-<up>") 'slime-repl-backward-input)
     (define-key slime-repl-mode-map (kbd "M-<down>") 'slime-repl-forward-input)
     (sublimity-mode 0)))
(setq common-lisp-hyperspec-root "file://Users/rafal/Documents/Knowledge/Lisp/clspec30/HyperSpec/")
(mapc 'frame-set-background-mode (frame-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PROLOG ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq prolog-system 'swi
      prolog-program-switches '((swi ("-G4g" "-T2g" "-L2g" "-O"))
                                (t nil))
      prolog-electric-if-then-else-flag t)

(defun prolog-insert-comment-block ()
  "Insert a PceEmacs-style comment block like /* - - ... - - */ "
  (interactive)
  (let ((dashes "-"))
    (dotimes (_ 36) (setq dashes (concat "- " dashes)))
    (insert (format "/* %s\n\n%s */" dashes dashes))
    (forward-line -1)
    (indent-for-tab-command)))

(global-set-key "\C-cq" 'prolog-insert-comment-block)

(global-set-key "\C-cl" (lambda ()
                          (interactive)
                          (insert ":- use_module(library()).")
                          (forward-char -3)))

(add-hook 'prolog-mode-hook
          (lambda ()
            (require 'flymake)
            (make-local-variable 'flymake-allowed-file-name-masks)
            (make-local-variable 'flymake-err-line-patterns)
            (setq flymake-err-line-patterns
                  '(("ERROR: (?\\(.*?\\):\\([0-9]+\\)" 1 2)
                    ("Warning: (\\(.*\\):\\([0-9]+\\)" 1 2)))
            (setq flymake-allowed-file-name-masks
                  '(("\\.pl\\'" flymake-prolog-init)))
            (flymake-mode 1)))

(defun flymake-prolog-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "swipl" (list "-q" "-t" "halt" "-s " local-file))))

(require 'ediprolog)

(global-set-key "\C-c\C-e" 'ediprolog-dwim)


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
        (exec-path-from-shell-initialize)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)

  (when (display-graphic-p)
    (ns-raise-emacs)))

;; Initialise shell PATH when run in a GUI on a Mac
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

(provide '.emacs)
;;; .emacs ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; TeX ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
