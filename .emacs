
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;(tool-bar-mode 0)
;(menu-bar-mode 0)
;(scroll-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/nvALT/projx-TorontoLife.txt" "~/Documents/nvALT/INBOX_TODO_2020.txt" "~/Documents/nvALT/projx-eix.txt"))
 '(package-selected-packages
   '(buffer-move markdown-mode elpy multiple-cursors git-gutter helm magit exec-path-from-shell))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(org-block ((t (:background "#EFF0F1" :extend t))))
 '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF" :extend t))))
 '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF" :extend t))))
 '(org-drawer ((t (:background "#e8e8e8" :foreground "#a685e2" :extend t))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold :width normal))))
 '(org-level-2 ((t (:inherit outline-2 :overline t :underline t))))
 '(org-level-3 ((t (:inherit outline-3 :underline t :weight bold))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; Backup files relocated 2020-10-09
(setq backup-directory-alist `(("." . "~/.saves")))

;; No welcome startup screen
(setq inhibit-startup-screen t)

;; No scratch message 2020-10-10
(setq initial-scratch-message "")

;; Maximize GUI window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show Closed(DONE) date in ORG-mode
(setq org-log-done 'time)

;; Org Agenda View shortcut
(global-set-key "\C-ca" 'org-agenda)

;; Org Babel - inline code 2020-10-19
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (latex . t)
   (java . t)
   (js . t)
   ))

;; Auto Refresh
(global-auto-revert-mode t)

;; Visual Line Mode On
(global-visual-line-mode 1)

;; Display Line Numbers On
;(global-display-line-numbers-mode)


;;----------------------------------------------------------------
;; ACE JUMP MODE
;;----------------------------------------------------------------

;;
;; ace jump mode major function
;; 
(add-to-list 'load-path "/Users/osickwon/.emacs.d/ace-jump-mode/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;
;; conflict org-mode: C-c SPC => org-table-blank-field

;; When org-mode starts it (org-mode-map) overrides the ace-jump-mode.
;; (https://github.com/winterTTr/ace-jump-mode/issues/47)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "\C-c SPC") 'ace-jump-mode)))
 
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

;; built-in org-mouse turn on
(require `org-mouse)


;; display date in modeline 2020-12-07
(setq display-time-day-and-date t)
(display-time)


;; pdflatex -> path "/Library/Tex/texbin" 2020-12-07
(setq latex-run-command "pdflatex")


;; MELPA 2020-12-08
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; -- Unnecessary call -- see Line #6 in this file
(package-initialize)

;; PLUG-IN: exec-path-from-shell 2020-12-08
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; PLUG-IN: deft -> do not use: Korean issue, change filename forcely 2020-12-08
;(require 'deft)
;(setq deft-extensions '("txt" "tex" "org" "md"))
;(setq deft-directory "~/Documents/nvALT/")
;;(setq deft-directory "~/Documents/test/")
;(setq deft-recursive t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG - Done checkbox with strike through 2020-12-08
;; https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings 2020-12-10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)


;; Interactive Do Mode like showing suggestion keyword 2020-12-18
(ido-mode 1)


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


(setq org-ditaa-jar-path "/Users/osickwon/Applications/")


;; buffer move package 2020-12-22
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


