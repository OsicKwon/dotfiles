;; since 2020
;;    ___  ____ ___  ____ ___________
;;   / _ \/ __ `__ \/ __ `/ ___/ ___/
;; _/  __/ / / / / / /_/ / /__(__  ) 
;;(_)___/_/ /_/ /_/\__,_/\___/____/  
                                   
;;==================
;; Initial Setting
;;==================


(setq ns-pop-up-frames nil)                                  ;; only one frame use when openning a file 2021-01-28

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
    )
 )

;; Spell Check 2021-02-14
(when (display-graphic-p)
      (progn
        (flyspell-mode +1)
        (add-hook 'text-mode-hook 'turn-on-flyspell)
        (add-hook 'org-mode-hook 'turn-on-flyspell)
        (add-hook 'markdown-mode-hook 'turn-on-flyspell)
        (add-hook 'latex-mode-hook 'turn-on-flyspell)
        (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)  ;; for AUCTeX
      )
)

;; Recent opened file history 2020-12-31
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files)
(setq recentf-auto-cleanup 'never)

(setq backup-directory-alist `(("." . "~/.saves/")))         ;; Backup files relocated 2020-10-09
(setq inhibit-startup-screen t)                              ;; No welcome startup screen
(setq initial-scratch-message "")                            ;; No scratch message 2020-10-10
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximize GUI window

(global-undo-tree-mode)                                      ;; Undo-Tree Package 2021-02-13
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-auto-save-history t)
;; (setq auto-save-visited-mode t)                              ;; Auto Save   
;; (setq auto-save-visited-interval 1)                          ;; Auto Save - Interval
;; (global-auto-revert-mode t)                                  ;; Auto Refresh
(global-visual-line-mode 1)                                  ;; Visual Line Mode On
;; (global-display-line-numbers-mode)                           ;; Display Line Numbers On

;; Bettery mode 2021-01-03
(display-battery-mode 1)

(setq ispell-program-name "/opt/local/bin/ispell")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes nil)
 '(doc-view-continuous t)
 '(fringe-mode 0 nil (fringe))
 '(org-agenda-files
   '("~/Documents/nvALT/INBOX_TODO_2021.txt" "~/Documents/nvALT/testx-emacs-org-agenda.txt" "~/Documents/nvALT/infox-notex-Jiwoo.txt" "~/Documents/nvALT/projx-elt_221.txt" "~/Documents/nvALT/projx-TorontoLife.txt" "~/Documents/nvALT/projx-eix.txt"))
 '(org-export-backends '(ascii beamer html icalendar latex odt))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file-other-window)
     (wl . wl-other-frame)))
 '(package-selected-packages
   '(centered-window undo-tree olivetti ivy markdown-preview-mode rainbow-delimiters pdf-tools helm-ack helm-ag ack ag helm-projectile projectile evil-surround which-key auctex flymake jedi auto-complete pygen python-mode ein company-jedi ob-ipython company evil ace-jump-mode elpy use-package csv-mode pandoc smex ido-vertical-mode buffer-move markdown-mode multiple-cursors git-gutter helm magit exec-path-from-shell)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(org-block ((t (:background "#f1f6f9" :extend t))))
 '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#f0f0f0" :extend t))))
 '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#f0f0f0" :extend t))))
 '(org-document-title ((t (:foreground "midnight blue" :weight bold :height 1.4))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))



;;==========
;; window
;;==========

;; Buffer move package 2020-12-22
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; display date in modeline 2020-12-07
;; (setq display-time-day-and-date t)
;; (display-time)
(setq display-time-format "[%Y-%m-%d %H:%M]")
(display-time-mode 1)


;; display file size in modeline 2021-01-05
(size-indication-mode t)

;; pdflatex -> path "/Library/Tex/texbin" 2020-12-07
(setq latex-run-command "pdflatex")


;;===============================================================
;; PACKAGES
;;===============================================================

;; MELPA 2020-12-08
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; -- Unnecessary call -- see Line #6 in this file
(package-initialize)
;; (package-refresh-contents)
;; (package-install 'use-package)

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; ignore byte-compile warnings
;; 2021-01-16
;; http://tsengf.blogspot.com/2011/06/disable-byte-compile-warning-in-emacs.html
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))


;; company mode 2021-01-24
;; I think it's a kind of auto-completion
(add-hook 'after-init-hook 'global-company-mode)


;; auto complete 2021-01-16
;; https://www.youtube.com/watch?v=HTUE03LnaXA
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


;; yasnippet 2021-01-16
;; https://www.youtube.com/watch?v=HTUE03LnaXA
(require 'yasnippet)
(yas-global-mode 1)


;; PLUG-IN: exec-path-from-shell 2020-12-08
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; which-keys
(which-key-mode)                        ;; which-key package 2021-01-28


;; PLUG-IN: deft -> do not use: Korean issue, change filename forcely 2020-12-08
;(require 'deft)
;(setq deft-extensions '("txt" "tex" "org" "md"))
;(setq deft-directory "~/Documents/nvALT/")
;;(setq deft-directory "~/Documents/test/")
;(setq deft-recursive t)

;;------------------------
;; Enable Evil 2020-12-30
;;------------------------
;; (require 'evil)
(evil-mode 1)
(setq-default evil-default-state 'emacs)
;; Change color of which-func when entering and leaving Evil visual state
;;  - Resolves the "white on white" issue :: 2021-01-17
;; https://www.reddit.com/r/emacs/comments/6tdqt0/how_can_i_change_the_background_color_when/
;; https://evil.readthedocs.io/en/latest/hooks.html
(setq original-background (face-attribute 'default :background))
(setq original-foreground (face-attribute 'default :foreground))

;; (add-hook 'evil-normal-state-entry-hook (lambda () (set-background-color "gray")))
;; (add-hook 'evil-normal-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightgray")))
;; (add-hook 'evil-normal-state-entry-hook (lambda () (set-foreground-color "black")))
(add-hook 'evil-normal-state-entry-hook (lambda () (set-foreground-color original-foreground)))


(if (display-graphic-p)
    (progn
    ;; if graphic (GUI)
      (add-hook 'evil-normal-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightgray")))
      ;; (add-hook 'evil-normal-state-entry-hook (lambda () (set-background-color "lightgray")))
      (add-hook 'evil-operator-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightgray")))
      (add-hook 'evil-normal-state-exit-hook (lambda () (face-remap-add-relative 'default :background original-background)))
      ;; (add-hook 'evil-normal-state-entry-hook (lambda () (hl-line-mode 1) (set-face-attribute hl-line-face nil :background "lightgray")))
      ;; (add-hook 'evil-normal-state-exit-hook (lambda () (set-background-color original-background)))
      ;; (add-hook 'evil-normal-state-exit-hook (lambda () (set-foreground-color original-foreground)))

      ;; (add-hook 'evil-operator-state-entry-hook (lambda () (set-background-color "gray")))

      (add-hook 'evil-insert-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightyellow")))
      ;; (add-hook 'evil-insert-state-entry-hook (lambda () (set-background-color "lightyellow")))
      ;; (add-hook 'evil-insert-state-entry-hook (lambda () (set-foreground-color "black")))
      ;; (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode 1) (set-face-attribute hl-line-face nil :background "lightyellow")))
      ;; (add-hook 'evil-insert-state-exit-hook (lambda () (set-background-color original-background)))
      ;; (add-hook 'evil-insert-state-exit-hook (lambda () (set-face-attribute hl-line-face nil :weight 'normal)))
      ;; (add-hook 'evil-insert-state-exit-hook (lambda () (set-foreground-color original-foreground)))

      (add-hook 'evil-visual-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightgray")))
      ;; (add-hook 'evil-visual-state-entry-hook (lambda () (set-background-color "darkgray")))
      ;; (add-hook 'evil-visual-state-exit-hook (lambda () (set-background-color original-background)))

      ;; (add-hook 'evil-replace-state-entry-hook (lambda () (set-background-color "lightyellow")))
      ;; (add-hook 'evil-replace-state-entry-hook (lambda () (set-foreground-color "black")))
      ;; (add-hook 'evil-replace-state-exit-hook (lambda () (set-background-color original-background)))
      ;; (add-hook 'evil-replace-state-exit-hook (lambda () (set-foreground-color original-foreground)))
      (add-hook 'evil-emacs-state-entry-hook (lambda () (face-remap-add-relative 'default :background original-background)))
      (add-hook 'evil-emacs-state-entry-hook (lambda () (face-remap-add-relative 'default :foreground original-foreground)))
    )
    ;; else (optional)
    (add-hook 'evil-normal-state-entry-hook (lambda () (hl-line-mode 1) (set-face-attribute hl-line-face nil :underline t :background original-background)))
    (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode 1) (set-face-attribute hl-line-face nil :underline t :overline 't :background "black")))
    (add-hook 'evil-emacs-state-entry-hook (lambda () (set-background-color original-background)))
    (add-hook 'evil-emacs-state-entry-hook (lambda () (set-foreground-color original-foreground)))
    (add-hook 'evil-emacs-state-entry-hook (lambda () (hl-line-mode 0)))
)


;; [For Reference] GUI detection ~ more readable :)
;;
;; (when (display-graphic-p)
;;     (your)
;;     (code))
;;
;; https://stackoverflow.com/questions/5795451/how-to-detect-that-emacs-is-in-terminal-mode
;; 2021-02-10


;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window))
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "white"))))))
;;   (buffer-face-set 'default))
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)


;; https://emacs.stackexchange.com/questions/34251/change-modeline-background-when-in-normal-evil-mode
;;
;; (setq original-modeline-background (face-attribute 'mode-line :background))
;; (setq normal-state-background "red")
;; (add-hook 'evil-normal-state-entry-hook (lambda () (set-face-attribute 'mode-line nil :background normal-state-background)))
;; (add-hook 'evil-normal-state-exit-hook  (lambda () (set-face-attribute 'mode-line nil :background original-modeline-background)))


;; evil cursor in modes 2021-02-06
;; https://github.com/hlissner/doom-emacs/issues/1848
;; http://fnwiya.hatenablog.com/entry/2016/01/12/213149 
;; (setq evil-normal-state-cursor '(box "black")
;;       evil-insert-state-cursor '(bar "black")
;;       evil-visual-state-cursor '(hollow "black")
;;       evil-emacs-state-cursor '(box "black"))

;; https://emacs.stackexchange.com/questions/30582/how-do-i-change-the-mode-indicators-for-evil-mode-in-the-spaceline-mode-line-pac
(setq evil-normal-state-tag "NORMAL")
(setq evil-insert-state-tag "INSERT")
(setq evil-visual-state-tag "VISUAL")
(setq evil-replace-state-tag "REPLACE")

;; Color the evil tag - colors taken from spaceline
;; https://github.com/Malabarba/smart-mode-line/issues/195
(setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:background "black"          :foreground "white")))
      evil-emacs-state-tag    (propertize " <E> " 'face '((:background "gray"              :foreground "black")))
      evil-insert-state-tag   (propertize " INSERT " 'face '((:background "lightyellow"    :foreground "black")))
      evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "chocolate"     :foreground "black")))
      evil-motion-state-tag   (propertize " <Motion> " 'face '((:background "plum3"             :foreground "black")))
      evil-visual-state-tag   (propertize " VISUAL " 'face '((:background "darkgray"       :foreground "black")))
      evil-operator-state-tag (propertize " <Operator> " 'face '((:background "sandy brown"       :foreground "black"))))

;; evil key binding
(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

;; forward-sentence is based on the sentence-end variable
;; https://stackoverflow.com/questions/20257022/evil-emacs-mode-sentence-motions-and-other-questions
(setq sentence-end "[\\.\\?\\!] +") ;; . or ? or ! followed by spaces.
(define-key evil-normal-state-map ")" 'forward-sentence)


;; evil undo 2021-02-06
;; https://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims
(setq evil-want-fine-undo 't)
(setq evil-want-fine-undo 'fine)
;; (setq evil-undo-system 'undo-tree) ;; not working tested 2021-02-07

;; evil-surround package 2021-01-29
(global-evil-surround-mode 1)

;; evil-surround add pairs 2021-02-08
;; https://github.com/emacs-evil/evil-surround
;; this macro was copied from here: https://stackoverflow.com/a/22418983/4921402
(defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

(define-and-bind-quoted-text-object "pipe" "|" "|" "|")
(define-and-bind-quoted-text-object "slash" "/" "/" "/")
(define-and-bind-quoted-text-object "underscore" "_" "_" "_")
(define-and-bind-quoted-text-object "code" "~" "~" "~")
(define-and-bind-quoted-text-object "verbatim" "=" "=" "=")
(define-and-bind-quoted-text-object "strike_through" "+" "+" "+")
(define-and-bind-quoted-text-object "asterisk" "*" "*" "*")
(define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ;; sometimes your have to escape the regex



;;----------------------------------------------------------------
;; ACE JUMP MODE
;;----------------------------------------------------------------
;; ace jump mode major function
;; 
(add-to-list 'load-path "/Users/osickwon/.emacs.d/ace-jump-mode/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
;;
;; conflict org-mode: C-c SPC => org-table-blank-field

;; `C-u C-c SPC` was not worked properly, so Used `C-c C-c` 2021-01-17
;; (define-key global-map (kbd "C-c C-c SPC") 'ace-jump-char-mode)


;; When org-mode starts it (org-mode-map) overrides the ace-jump-mode.
;; (https://github.com/winterTTr/ace-jump-mode/issues/47)
(add-hook 'org-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "\C-c SPC") 'ace-jump-mode)))
            (local-set-key (kbd "\C-c SPC") 'ace-jump-char-mode)))
 
;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;If you use viper mode :
;(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;;If you use evil
;(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;;_EOL_ACE_SETTING------------------------------------------------


;; projectile package 2021-01-29
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; use 'helm-projectile-ag' inseted of C-c p s s, which stands for ag serching in projectile
;; once install package of 'helm-projectile', you can also use 'helm-projectile-ag'
;; or install 'helm-ag' package to use same function
;; however, required intall '(sudo) port install the_silver_searcher' first (the_silver_searcher = ag)


;; Olivetti 2021-02-11
;; to remove boundry -> '(fringe-mode 0 nil (fringe)) in `(custom-set-variables` lines in front of this file. 
(setq olivetti-body-width 95)
(setq olivetti-minimum-body-width 80)


;;===============================================================
;; ORG-MODE
;;===============================================================


;; built-in org-mouse turn on
(require `org-mouse)

(setq org-log-done 'time)                               ;; Show Closed(DONE) date in ORG-mode
(global-set-key "\C-ca" 'org-agenda)                    ;; Org Agenda View shortcut


(add-to-list 'org-emphasis-alist
             '("~" (:foreground "red")
               ))

;; === ORG-BABEL ===

;; Org Babel - inline code 2020-10-19
;;------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (R . t)
   (plantuml . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (latex . t)
   (java . t)
   (js . t)
   ))


;; set plantuml path 2021-01-08
(setq org-plantuml-jar-path
      (expand-file-name "~/Applications/plantuml.jar"))

;; ditta path 2021-01-14
(setq org-ditaa-jar-path "/Applications/ditaa0_9.jar")

;; babel inline images 2021-01-15
;; https://emacs.stackexchange.com/questions/30520/org-mode-c-c-c-c-to-display-inline-image
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)


;; ORG - Done checkbox with strike through 2020-12-08
;; https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
;;------------------------------------------------------------------------------------

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)


;; -------------------------------------------------------------------
;; [Test] ORG Direct Insert Online(Web) Image 2021-02-07
;; https://emacs.stackexchange.com/questions/42281/org-mode-is-it-possible-to-display-online-images
;; -------------------------------------------------------------------

;; And again we have a use-case for image data not stored on harddisk.
;; (The other two use-cases are base64 encoded images in org buffers and displaying previews of youtube videos.)

;; Get org-yt and paste the following elisp code into your init-file.
;; After evaluating your init-file you can use links analog to the example:

;; [[imghttp://tn-home.de/Pic/tn-home.png]]

;; (require 'org-yt)

;; (defun org-image-link (protocol link _description)
;;   "Interpret LINK as base64-encoded image data."
;;   (cl-assert (string-match "\\`img" protocol) nil
;;              "Expected protocol type starting with img")
;;   (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
;;     (cl-assert buf nil
;;                "Download of image \"%s\" failed." link)
;;     (with-current-buffer buf
;;       (goto-char (point-min))
;;       (re-search-forward "\r?\n\r?\n")
;;       (buffer-substring-no-properties (point) (point-max)))))

;; (org-link-set-parameters
;;  "imghttp"
;;  :image-data-fun #'org-image-link)

;; (org-link-set-parameters
;;  "imghttps"
;;  :image-data-fun #'org-image-link)


;;=========================
;; Key Bindings 2020-12-10
;;==========================

;; scroll window up/down by one line
;; http://pragmaticemacs.com/emacs/scrolling-and-moving-by-line/
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))


;; org refile-multi_level 2020-12-12
;; https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling


;; disable babel confirmation message 2020-12-13
(setq org-confirm-babel-evaluate nil)


;; org inline image resize 2020-12-13
(setq org-image-actual-width nil)


;; helm package key bindings 2020-12-17

(require 'helm-config)
(helm-mode 1)

;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
;; (global-set-key (kbd "M-x") 'helm-M-x)


;; Interactive Do Mode like showing suggestion keyword 2020-12-18
;; Added ido-vertical-mode 2021-01-04
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; smex with ido for M-x :: consider helm alternatively
;; 2021-01-07
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)
;; confilicted with magit commit command 'C-c C-c' 2021-02-12
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Git-Gutter 2020-12-18
(global-git-gutter-mode +1)

;; Multiple-Cursors Package 2020-12-19
(require 'multiple-cursors)
; for multiple-line
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
; for keyword
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; org-level whole line background in org-mode 2020-12-27
; ':extend t' option required to apply
; it also adjust in 'customize' menu in emacs configuaration 
; -------- Applied Automatically at Line 20 ------------------
;(setq org-fontify-whole-heading-line t
;      org-fontify-done-headline t
;      org-fontify-quote-and-verse-blocks t)


;;==============================================================
;; Python Development - Since 2021-01-06
;;==============================================================
;; src1: https://realpython.com/emacs-the-best-python-editor/#integration-with-jupyter-and-ipython

;; === ELPI ===
;; ;Enable elpy
(elpy-enable)

;; === JEDI ===
;; jedi 2021-01-16
;; https://tkf.github.io/emacs-jedi/latest/
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional


;; ;; ;; Use IPython for REPL
;; ;; (setq python-shell-interpreter "jupyter"
;; ;;       python-shell-interpreter-args "console --simple-prompt"
;; ;;       python-shell-prompt-detect-failure-warning nil)
;; ;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

(setq org-babel-python-command "python3")
;; (setq python-shell-interpreter "python3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")

;; (setq python-shell-completion-native-enable t)
;; (setq python-shell-completion-toggle t)

(setq python-shell-completion-native-enable nil)



