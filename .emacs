;; -*- find-file-hook: view-mode; -*-
;; since 2020
;;    ___  ____ ___  ____ ___________
;;   / _ \/ __ `__ \/ __ `/ ___/ ___/
;; _/  __/ / / / / / /_/ / /__(__  ) 
;;(_)___/_/ /_/ /_/\__,_/\___/____/  

;; *******************************************************************************************************
;; NOTICE / REMINDER / TIPS
;; -------------------------
;; 1. always double check variables set by both automatically in M-x customize and manually    2021-03-20
;; 2. can cusomize whatever from its source file :: https://youtu.be/AaUlOH4GTCs?t=735         2021-03-21
;; *******************************************************************************************************

;; INITIAL SETTING

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
 
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; http://ohyecloudy.com/emacsian/2019/12/15/package-use-package-ensure-imenu/
(setq use-package-enable-imenu-support t)


;; == straight.el 2021-05-01 ==
;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Users of Emacs versions >= 27 will want to add:
(setq package-enable-at-startup nil)


;; == Use-Package Ref. ==
;; https://jwiegley.github.io/use-package/keywords/


;; 2021-04-20
;; https://blog.sumtypeofway.com/posts/emacs-config.html
;; not working 2021-04-22
;; -> maybe, some internal built-in package is not required ensure option
;; (setq use-package-always-ensure t)


;; --------------------
;; == RECENT SETTING ==
;; --------------------


;; 2021-05-07
;; https://www.reddit.com/r/emacs/comments/n71hj2/python_how_would_you_configure_emacs_for_data/
(use-package ein
  :ensure t
  :init 
  (setq ein:polymode t
        ein:output-area-inlined-images t)
  )


;; == markdown-preview-mode 2021 ==
;; Error in both 'require' and 'use-package' methods 2021-05-06
;; (require 'markdown-preview-mode)
;; (use-package markdown-preview-mode
;;   :ensure t
;;   :init (require 'markdown-preview-mode)
;;   )

;; additional css from github
;; https://github.com/ancane/markdown-preview-mode/issues/29
;; (setq markdown-preview-stylesheets
;;       (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
;;             "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
;;   <style>
;;    .markdown-body {
;;      box-sizing: border-box;
;;      min-width: 200px;
;;      max-width: 980px;
;;      margin: 0 auto;
;;      padding: 45px;
;;    }

;;    @media (max-width: 767px) {
;;      .markdown-body {
;;        padding: 15px;
;;      }
;;    }
;;   </style>
;; "))

(setq markdown-preview-stylesheets
      (list "
  <style>
   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 15px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))


(setq markdown-preview-javascript
      (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
"))



;; https://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
(setq-default indicate-empty-lines t)

;; == find-file-in-project 2021-05-03 ==
;; search including subdirectories
;; git only ???????????
;; better to use 'counsel-find-jump' 2021-05-03
;; https://github.com/redguardtoo/find-file-in-project
(use-package find-file-in-project
  :ensure t
  :disabled
  :init
  (require 'find-file-in-project)
  (global-set-key (kbd "C-x C-f") 'find-file-in-project)
  )


;; just use winner mode insted
;; == zoom-window 2021-05-03 ==
;; (use-package zoom-window
;;   :ensure t
;;   :init
;;   (require 'zoom-window)
;;   :config
;;   (global-set-key (kbd "C-c z") 'zoom-window-zoom)
;;   )


;; == writeroom 2021-05-02 ==
;; example :: https://github.com/Jassob/.emacs.d
;; just use winner-mode instead
(use-package writeroom-mode
  :ensure t
  :disabled
  :init (setq writeroom-width 120)
  (add-hook 'writeroom-mode-hook (lambda () (display-line-numbers-mode -1)))
  :bind (:map writeroom-mode-map
              ("C-c C-w <" . #'writeroom-decrease-width)
              ("C-c C-w >" . #'writeroom-increase-width)
              ("C-c C-w =" . #'writeroom-adjust-width)
              ("s-?" . nil)
              ("C-c C-w SPC" . #'writeroom-toggle-mode-line))
        (:map global-map
              ("C-c r" . #'writeroom-mode)))
              ;; ("C-r" . #'writeroom-mode)))


;; vi-like line insertion
;; https://stackoverflow.com/questions/5898448/how-to-add-a-new-line-without-breaking-the-current-line
;; (global-set-key (kbd "C-o") (lambda () (interactive)(beginning-of-line)(open-line 1)))
;; (global-set-key (kbd "M-o") (lambda () (interactive)(end-of-line)(newline)))  ; binded to ace-window
(global-set-key (kbd "C-o") (lambda () (interactive)(end-of-line)(newline)))


;; Korean Hangul 2021-04-29
;; (setq default-korean-keyboard 'korean-hangul2)
;; (global-set-key (kbd "S-SPC") 'toggle-input-method)  ; may conflict with osx korean key


;; org-clock 2021-04-29
;; http://ohyecloudy.com/emacsian/2017/10/14/org-clock-persistence/
;; (org-clock-persistence-insinuate)
;; (setq org-clock-persist t)
;; (setq org-clock-in-resume t)
;; (setq org-clock-persist-query-resume nil)


;; for org-clock preventing from forgeting stopping clock in washroom!
;; http://ohyecloudy.com/emacsian/2017/11/04/org-dealing-with-idle-time/
;;
;; walk away from your computer
;; https://orgmode.org/manual/Resolving-idle-time.html
;;
;; dealing with idle time
;; https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html#dealing-with-idle
(setq org-clock-idle-time 15)


;; == eww config  2021-04-27 ==
;; https://www.gnu.org/software/emacs/manual/html_mono/eww.html
(add-hook 'eww-after-render-hook 'view-mode)
(add-hook 'eww-after-render-hook 'olivetti-narrow-width)
(add-hook 'eww-after-render-hook (lambda () (text-scale-increase 0.5)))
;; (add-hook 'eww-after-render-hook 'evil-normal-state)
;; https://gitea.polonkai.eu/gergely/my-emacs-d/commit/0c381769c1987fd21fe4af3e111bbe6ec3e9f8c8
(setq eww-search-prefix "https://www.google.com/?q=")


;; == Custom Key-binding Starting with C-c x ==
;; custom >>> general.el
;; *******************************
;; just type the command functions
;; *******************************
;; ;; at-point things key bindings 2021-04-27
;; (global-set-key (kbd "C-c x p") 'powerthesaurus-lookup-word-at-point)
;; (global-set-key (kbd "C-c x d") 'define-word-at-point)
;; (global-set-key (kbd "C-c x t") 'google-translate-at-point)
;; (global-set-key (kbd "C-c x a") 'counsel-ag-thing-at-point)

;; ;; useful view-mode functionalities 2021-05-04
;; (global-set-key (kbd "C-c x i") 'my-clone-indirect-buffer)
;; (global-set-key (kbd "C-c x o") 'my-org-indirect-buffer)
;; (global-set-key (kbd "C-c x i") 'org-narrow-to-subtree)
;; (global-set-key (kbd "C-c x o") 'widen)

;; an alternatvie with SPC prefix
;; (global-set-key (kbd "C-c SPC p") 'powerthesaurus-lookup-word-at-point)
;; (global-set-key (kbd "C-c SPC d") 'define-word-at-point)
;; (global-set-key (kbd "C-c SPC t") 'google-translate-at-point)
;; (global-set-key (kbd "C-c SPC a") 'counsel-ag-thing-at-point)

;; ;; an alternatvie with <escape> prefix
;; (global-set-key (kbd "<escape> c p") 'powerthesaurus-lookup-word-at-point)
;; (global-set-key (kbd "<escape> c d") 'define-word-at-point)
;; (global-set-key (kbd "<escape> c t") 'google-translate-at-point)
;; (global-set-key (kbd "<escape> c a") 'counsel-ag-thing-at-point)


;; == Workgroup2 2021-04-26 ==
;; Save and Restore Sessions
;; https://www.reddit.com/r/emacs/comments/7au3hj/how_do_you_manage_your_emacs_windows_and_stay_sane/
(use-package workgroups2
  :ensure t
  :init
  (require 'workgroups2)
  ;; Change prefix key (before activating WG)
  (setq wg-prefix-key (kbd "C-c z"))  ; not work for me
  ;; Change workgroups session file
  (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
  ;; (workgroups-mode 1)   ; put this one at the bottom of .emacs
  (global-set-key (kbd "C-c w c") 'wg-create-workgroup)
  (global-set-key (kbd "C-c w o") 'wg-open-workgroup)
  ;; alternating key-binding with C-x
  (global-set-key (kbd "C-x w c") 'wg-create-workgroup)
  (global-set-key (kbd "C-x w o") 'wg-open-workgroup)
  )


;; [Replace to] Workgroup2 << == Purpose 2021-04-26 ==
;; https://github.com/bmag/emacs-purpose
;; (require 'window-purpose)
;; (use-package window-purpose :ensure t)
;; (purpose-mode)
;; (define-key purpose-mode-map (kbd "C-x b") nil)  ;; conflicted with ivy-switch-view
;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;; (setq purpose-use-default-configuration t) ; not really necessary, default is t
;; (purpose-compile-user-configuration) ; activates your changes

;; [Replace to] Workgroup2 << == Burly :: save window configuration in bookmark 2021-04-26 ==
;; https://www.reddit.com/r/emacs/comments/jf9kqn/wip_burlyel_save_and_restore_buffers_and_window/
;; https://github.com/alphapapa/burly.el
;; (use-package burly :ensure t)

;; [Replace to] Workgroup2 << altrnatives :: desktop-save-mode, register, ivy-view, bookmark+
;; instead of desktop-save-mode belows:
;; https://www.reddit.com/r/emacs/comments/171me2/auto_save_and_restore_sessions/
;; https://bmag.github.io/2015/12/26/desktop.html
;; (setq desktop-path '("~/.emacs.d/.cache/"))
;; https://emacs.stackexchange.com/questions/19190/desktop-save-mode-fails-to-save-window-layout 
;; https://jloubert.com/blog/simple-views/
;; (add-to-list 'desktop-globals-to-save 'ivy-views)


;; == split window and then select other window 2021-04-25 ==
;; https://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
;; (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
;; (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))


;; == all-the-icons-ivy-rich 2021-04-23 ==
;; https://github.com/seagle0128/all-the-icons-ivy-rich
(use-package all-the-icons-ivy-rich
  :ensure t
  :disabled
  :init (all-the-icons-ivy-rich-mode 1))


;; == indirect edit in selection 2021-04-23 ==
;; so powerful !!
;; https://github.com/Fanael/edit-indirect
(use-package edit-indirect :ensure t)
(global-set-key (kbd "C-t") 'edit-indirect-region)

;; == ranger 2021-04-22 ==
(use-package ranger
  :ensure t
  :init   (message "### ranger loaded in :init option")
  :config (progn (message "### ranger loaded in :config option"))
  :preface (progn (message "### ranger loaded in :preface option"))

  )

;; (global-set-key (kbd "C-c r") 'ranger)

;; buffer management (delete opened buffer after closing)
(setq ranger-cleanup-on-disable t)
(setq ranger-cleanup-eagerly t)

;; (setq ranger-parent-depth 3)  ; z- / z+ -> increase / decrease
;; (setq ranger-width-parents 0.12)
;; (setq ranger-width-preview 0.55)

(setq ranger-show-literal nil)  ; by pressing: zi
(setq ranger-excluded-extensions '("mkv" "iso" "mp4"))
(setq ranger-max-preview-size 10)
(setq ranger-dont-show-binary t)
;; (setq ranger-show-hidden t)


;; == neotree with all-the-icons 2021-04-22 ==
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons :ensure t)
;; Required :: M-x all-the-icons-install-fonts
(use-package neotree 
  :ensure t
  :disabled
  :init (message ">>> neotree loaded")
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-reset-size-on-open t)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 70)
  )


;; == Insert org-heading without breaking line 2021-04-22 ==
;; https://emacs.stackexchange.com/questions/43845/org-mode-evil-insert-heading-set-cursor-to-line-ending-and-change-to-insert
(setq org-M-RET-may-split-line nil)


;; == StartUp Function 2021-04-20 ==
;; https://blog.sumtypeofway.com/posts/emacs-config.html
(defun my-default-screen ()
  (if (display-graphic-p)
    (find-file "~/Documents/nvALT/org_capture_note.txt")
    ;; (find-file "~/Documents/nvALT/Quick Note.txt")
    )
  )

;; (add-hook 'emacs-startup-hook #'my-default-screen)


;; == Timers 2021-04-20 ==
;; https://emacs.stackexchange.com/questions/7534/run-with-timer-error-invalid-or-unitialized-timer
;; Open my calendar every 1 hour
;; (run-with-timer 0 3600 #'my-default-screen) 
;; https://emacs.stackexchange.com/questions/22692/effect-of-multiple-idle-timers
;; (run-with-idle-timer 2 nil #'message "Function 1")
;; (run-with-idle-timer 300 nil #'my-default-screen)
;; https://emacs.stackexchange.com/questions/6029/is-it-possible-to-execute-a-function-or-command-at-a-specific-time
(when (display-graphic-p)
  (progn
    (run-at-time "09:00" nil 'cfw:open-org-calendar)
    (run-at-time "12:00" nil 'cfw:open-org-calendar)
    (run-at-time "15:00" nil 'cfw:open-org-calendar)
    (run-at-time "18:00" nil 'cfw:open-org-calendar)
))
;; (run-with-timer 15 3 (lambda () (insert "success ")))
;; The third arg must be a function, -> lambda
;; https://emacs.stackexchange.com/questions/7534/run-with-timer-error-invalid-or-unitialized-timer
(run-with-timer 1000 1000 (lambda () (message "run-with-timer: Just Do It, Keep It Simple, Get It Done ")))
;; (run-at-time 500 500 (lambda () (message "run-at-time: Think Big, Start Small, Move Fast ")))
(run-with-timer 500 500 (lambda () (message "run-with-timer: Think Big, Start Small, Move Fast ")))
;; (run-with-idle-timer 30 30 (lambda () (message "run-with-idle-timer: Start at the end")))
(run-with-idle-timer 30 30 (lambda () (message "run-with-idle-timer: Just Do It from Starting at The End")))


;; == Link Abbreviation 2021-04-19 ==
;; format: [[engine:keyword][description]]
;; example: [[google:emacs][emacs search]]
;; https://yiufung.net/post/org-mode-hidden-gems-pt3/
;; https://orgmode.org/manual/Link-Abbreviations.html
(setq org-link-abbrev-alist
      '(
	("google" . "https://www.google.com/search?q=")
	)
      )


;; Demote sequence for list bullets 2021-04-19
;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
;; (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-")))


;; == org: Avoid inadvertent text edit in invisible area 2021-04-19 ==
;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
(setq org-catch-invisible-edits 'show-and-error)


;; == org heading blank line exactly work 2021-04-19 ==
;; https://stackoverflow.com/questions/40332479/org-mode-folding-considers-whitespace-as-content
;; --------------------------------------------------------------------------
;; Documentation:
;; Number of empty lines needed to keep an empty line between collapsed trees.
;; If you leave an empty line between the end of a subtree and the following
;; headline, this empty line is hidden when the subtree is folded.
;; Org mode will leave (exactly) one empty line visible if the number of
;; empty lines is equal or larger to the number given in this variable.
;; So the default 2 means at least 2 empty lines after the end of a subtree
;; are needed to produce free space between a collapsed subtree and the
;; following headline.
;; If the number is negative, and the number of empty lines is at least -N,
;; all empty lines are shown.
;; --------------------------------------------------------------------------
;; but, not wokring properly on evil-mode 
;; in evil-mode, it seems to be (setq org-cycle-separator-lines 0), which is no blank line
;; https://yiufung.net/post/org-mode-hidden-gems-pt1/
;; --------------------------------------------------------------------------
;; (setq org-cycle-separator-lines 2)   ;; default: 2 lines -> 1 blank between heading
;; (setq org-cycle-separator-lines 0)   ;; not allow blank line like 'evil-toggle-fold'
(setq org-cycle-separator-lines -1)  ;; 1 line = 1 blank


;; == top margin 2021-04-19 ==
;; https://stackoverflow.com/questions/12632399/how-to-get-top-and-bottom-margins-in-emacs
;; (set-frame-parameter nil 'internal-border-width 10)


;; == encrypt 2021-04-19 ==
;; https://media.ccc.de/v/glt21-251-emacs-org-mode-features-you-may-not-know#t=1289
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(require 'org-crypt)
;; (use-package org-crypt)  ;; built-in ???
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)


;; == Tufte : org html export theme 2021-04-19 ==
;; #+HTML_HEAD: <link rel="stylesheet" href="/css/tufte.css" type="text/css" />
;; example :: https://two-wrongs.com/essay-recommendation-on-having-enough-socks.html
;; (require 'ox-tufte)
(use-package ox-tufte
  :ensure t
  :disabled
  )
;; FYI > other recommendation for export HTML css
;; https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup
;; https://github.com/thi-ng/org-spec 


;; == centered cursor mode 2021-04-19 ==
;; https://github.com/andre-r/centered-cursor-mode.el
(use-package centered-cursor-mode :ensure t)


;; == Paredit 2021-04-19 ==
;; https://wikemacs.org/wiki/Paredit-mode
;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
; (autoload 'enable-paredit-mode "paredit"
;   "Turn on pseudo-structural editing of Lisp code."
;   t)
; (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
; (add-hook 'lisp-mode-hook             'enable-paredit-mode)
; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
; (add-hook 'scheme-mode-hook           'enable-paredit-mode)


;; == esup :: Emacs Start Up Profiler 2021-04-19 ==
;; https://github.com/jschaf/esup
(use-package esup
  :ensure t
  ;; https://blog.d46.us/advanced-emacs-startup/
  :init   ; Code to run immediately
  :config ; Code to run after the package is loaded
  ;; https://github.com/jschaf/esup/issues/60#issuecomment-461217431
  (setq esup-user-init-file (file-truename "~/dotfiles/.emacs"))
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  ;; https://github.com/jschaf/esup/issues/54#issuecomment-651247749
  (setq esup-depth 0)
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa
  )


;; crux  2021-04-18
;; Collection of Ridiculously Useful eXtensions for Emacs
;; https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/ 
;; https://github.com/bbatsov/crux
;; (use-package crux :ensure t)


;; == highlight under cursor 2021-04-18 ==
;; https://xenodium.com/ <- Powerful Emacs Contents
;; https://xenodium.com/emacs-highlight-symbol-mode/
(use-package highlight-symbol :ensure t
  :config
  (if (display-graphic-p)
      (set-face-attribute 'highlight-symbol-face nil :background "light yellow" :foreground "#FA009A" :underline t)
    (set-face-attribute 'highlight-symbol-face nil :background "black" :foreground "#FA009A" :underline t))
  (setq highlight-symbol-idle-delay 0)
  (setq highlight-symbol-on-navigation-p t)
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))
  (add-hook 'evil-normal-state-entry-hook #'highlight-symbol-mode)
  (add-hook 'evil-normal-state-entry-hook #'highlight-symbol-nav-mode))
  (add-hook 'evil-normal-state-exit-hook (lambda () (highlight-symbol-mode -1)))
  (add-hook 'evil-normal-state-exit-hook (lambda () (highlight-symbol-nav-mode -1)))


;; == org-appear 2021-04-18 ==
;; pros :: Nice-looking
;; cons :: Interupting column align such as ':tag'
(setq org-hide-emphasis-markers t)
(use-package org-appear :ensure t)
(add-hook 'org-mode-hook 'org-appear-mode)


;; == org-bullets ==
(use-package org-bullets
  :ensure t
  :after org
  :init
  (require 'org-bullets)
  (setq org-bullets-bullet-list '("■" "⚬" "▪" "•" "▭" "◦" "·" "□" "○" "■" "●" "◆" "◔" "▣" "❑" "⚀" "𝇇" "✗" "✓" "☺"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )


;; == rainbow-delimiters ==
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )


;; == focus package (too slow) ==
;; (use-package focus :ensure t)
;; (add-hook 'evil-normal-state-entry-hook 'focus-mode)
;; (add-hook 'evil-normal-state-exit-hook (lambda () (focus-mode -1)))


;; == dimmer (not needed) ==
;; (require 'dimmer)
;; (use-package dimmer :ensure t)
;;  (dimmer-configure-which-key)
;;  (dimmer-configure-helm)
;;  (dimmer-mode t)


;; open nearest link 2021-04-16
(defun my-org-next-link-open ()
  (interactive)
  (org-next-link)
  (org-open-at-point-global)
  )

;; == pdfgrep 2021-04-16 ==
;; not useful / some confused
;; (require 'pdfgrep)
;; (use-package pdfgrep :ensure t)
;; (pdfgrep-mode)


;; minimap mode 2021-04-13
(use-package minimap :ensure t)
;; for org-mode
;; https://github.com/dengste/minimap/issues/22
(setq minimap-major-modes '(prog-mode text-mode))
;; (add-hook 'minimap-mode-hook 'sublimity-mode)

(defun my-minimap-mode ()
  (interactive)
  (if minimap-mode
      (minimap-mode 0)
    (minimap-mode 1))
  (if sublimity-mode
      (sublimity-mode 0)
    (sublimity-mode 1))
  )


;; == imenu 2021-05-02 ==
(use-package imenu-list
  :ensure t
  ;; :bind ("C-." . imenu-list-minor-mode)
  :config
  ;; https://github.com/bmag/imenu-list
  (setq imenu-list-focus-after-activation t)
  ;; imenu-list resize 2021-04-13
  ;; https://github.com/bmag/imenu-list
  ;; (setq imenu-list-auto-resize t)
  ;; https://github.com/bmag/imenu-list/blob/1447cdc8c0268e332fb4adc0c643702245d31bde/imenu-list.el#L431
  (setq imenu-list-size 0.20)  ; default 0.2, in case of long head, use setq-local variable in the file that you want
  )
(setq org-imenu-depth 3) ; put outside of imenu-list package (bulit-in variable)

;; Local Variables Auto-Load without Confirmation 2021-04-12
;; https://emacs.stackexchange.com/questions/28/safe-way-to-enable-local-variables
;; DO NOT set this variable to :all, and do look at the values of variables first
;; -----
;; (setq enable-local-variables :safe)  ;; not applied font size
(setq enable-local-variables :all)   ;; unsafe way

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Local-Variables.html
;; -----------------------------------------------------
;; t (the default)
;;     Set the safe variables, and query (once) about any unsafe variables. 
;; :safe
;;     Set only the safe variables and do not query. 
;; :all
;;     Set all the variables and do not query. 
;; :none <- "I guess it works, but I am not sure." 2021-04-12
;; nil
;;     Don’t set any variables. 
;; -----------------------------------------------------


;; dired-narrow 2021-04-11
;; http://pragmaticemacs.com/emacs/dynamically-filter-directory-listing-with-dired-narrow/
(use-package dired-narrow :ensure t)


;; Google Translate 2021-04-11
;; https://github.com/atykhonov/google-translate/issues/137
(use-package google-translate
  :demand t
  ;; :disabled  ;; too powerful 2021-04-12
  :ensure t
  :init
  (require 'google-translate)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ko")
  (setq google-translate-output-destination 'echo-area)
  (setq google-translate-show-phonetic t)

  :functions (my-google-translate-at-point google-translate--search-tkk)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (defun my-google-translate-at-point()
    "reverse translate if prefix"
    (interactive)
    (if current-prefix-arg
	(google-translate-at-point)
      (google-translate-at-point-reverse)))
  :bind
  ;; ("C-t". my-google-translate-at-point)

  )


;; Starting a Function 2021-04-11
;; https://emacs.stackexchange.com/questions/15097/how-do-i-run-a-function-on-start-up

(defun starting-options ()
  ;; http://ergoemacs.org/emacs/elisp_idioms_prompting_input.html
  (if (y-or-n-p "Calendar view?")
      (progn
	(interactive)
	;; https://github.com/kiwanami/emacs-calfw/issues/45
	(select-frame (make-frame '((name . "calendar"))))         ; makes a new frame and selects it
	(set-face-attribute 'default (selected-frame) :height 135) ; reduces the font size of the new frame
	(cfw:open-org-calendar) ; opens the calendar there
	)
    (progn
      ;; code if user answered no.
      ;; (counsel-recentf)
      )
    )
)

(defun starting-calendar ()
  (interactive)
  (select-frame (make-frame '((name . "calendar"))))         ; makes a new frame and selects it
  (set-face-attribute 'default (selected-frame) :height 134) ; reduces the font size of the new frame
  (cfw:open-org-calendar) ; opens the calendar there
  )

;; (add-hook 'after-init-hook #'cfw:open-org-calendar)
;; (add-hook 'after-init-hook #'starting-options)  ; hold for a while 2021-04-13
;; (add-hook 'after-init-hook 'starting-functions)


(defun my-cfw-refresh-calendar ()
  (interactive)
  (cfw:open-org-calendar)
  (cfw:refresh-calendar-buffer)
  )


;; == elfeed 2021-04-09 ==
(use-package elfeed
  :ensure t
  :bind (
	 ;; ("j" . next-line)      ; interfered minibuffer
	 ;; ("k" . previous-line)  ; interfered minibuffer
	 ;; ("R" . elfeed-mark-all-as-read)
	 )
  )
(setq elfeed-feeds
      '(
	;; "https://lifehacker.com/rss"
	))


;; https://emacs.stackexchange.com/questions/2440/elfeed-mark-all-messages-as-read
(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))

(define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

;; get idea from https://noonker.github.io/posts/2020-04-22-elfeed/
(define-key elfeed-search-mode-map (kbd "j") 'next-line)
(define-key elfeed-search-mode-map (kbd "k") 'previous-line)
(define-key elfeed-search-mode-map (kbd "T") 'elfeed-search-first-entry)
(define-key elfeed-search-mode-map (kbd "B") 'elfeed-search-last-entry)

;; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun elfeed-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; (defun elfeed-firefox-open (&optional use-generic-p)
;;   "open with firefox"
;;   (interactive "P")
;;   (let ((entries (elfeed-search-selected)))
;;     (cl-loop for entry in entries
;;              do (elfeed-untag entry 'unread)
;;              when (elfeed-entry-link entry)
;;              do (browse-url-firefox it))
;;     (mapc #'elfeed-search-update-entry entries)
;;     (unless (use-region-p) (forward-line))))

;; (defun elfeed-w3m-open (&optional use-generic-p)
;;   "open with w3m"
;;   (interactive "P")
;;   (let ((entries (elfeed-search-selected)))
;;     (cl-loop for entry in entries
;;              do (elfeed-untag entry 'unread)
;;              when (elfeed-entry-link entry)
;;              do (ffap-w3m-other-window it))
;;     (mapc #'elfeed-search-update-entry entries)
;;     (unless (use-region-p) (forward-line))))

;; (define-key elfeed-search-mode-map (kbd "t") 'elfeed-w3m-open)
(define-key elfeed-search-mode-map (kbd "w") 'elfeed-eww-open)
(define-key elfeed-search-mode-map (kbd "e") 'elfeed-eww-open)
;; (define-key elfeed-search-mode-map (kbd "f") 'elfeed-firefox-open)

;; Update when it's starting
;; https://www.reddit.com/r/emacs/comments/bvbp92/is_there_a_simple_way_to_get_elfeed_to_update/
;; (add-hook 'emacs-startup-hook (lambda () (run-at-time 0 500 'elfeed-update)))

;; use an org file to organise feeds
;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Documents/nvALT/setx-elfeed.txt")))


;; (setq blink-cursor-blinks 0)  ; default 10, to continue 0 or -1 2021-04-05

;; view mode (bult-in) 2021-04-03
;; https://gist.github.com/ivan-krukov/63a586f2121519ca51b201c634402a84
;; https://www.youtube.com/watch?v=kZARKLxTeYQ

;; TEST but does not work
;; (defun my-view-general-prefix ()
;;   (lambda ()
;;     (interactive)
;;     (funcall (key-binding (kbd "M-SPC")))
;;     ;; (call-interactively (key-binding (kbd "M-SPC")))
;;     )
;;   )

;; TESTING
;; (define-key view-mode-map (kbd "\\") (kbd "M-spc"))
;; (global-set-key (kbd "C-c I") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
;; (add-hook 'view-mode-hook (kbd "\\") (kbd "M-x"))


;; == view-mode 2021 ==
;; add view mode keybindings 2021-04-04
;; https://www.youtube.com/watch?v=kZARKLxTeYQ
;; https://gist.github.com/ivan-krukov/63a586f2121519ca51b201c634402a84

(use-package view
  :config
  (setq view-read-only t) ;; enter view-mode for read-only file
  ;; https://github.com/jwiegley/use-package/issues/455#issuecomment-347750540
  ;; (define-key key-translation-map (kbd "\\") (kbd "M-SPC"))  ; for general package key-binding like a leader key
  :bind (("M-z" . view-mode) 
	 :map view-mode-map

	 ;; Default (built-in)
	 ;; ------------------
	 ;; <spc> page down
	 ;; <del> page up
	 ;; d . half-page down
	 ;; u . half-page up
	 ;; y . scroll up (backward)
	 ;; < . top
	 ;; > . bottom
	 ;; s . I-search
	 ;; / . Regex Search ( \ . backward)
	 ;; n / p . next / previous Regex search result
	 ;; = . line number
	 ;; e . edit
	 ;; h . help (use '?')
	 ;; q . quit ,but not close buffer

	 ;; Emacs style
	 ;; -----------
	 ;; ("n" . forward-line)
	 ;; ("p" . previous-line)
	 ;; ("f" . right-word)
	 ;; ("b" . left-word)
	 ("f" . evil-forward-WORD-begin)
	 ("b" . evil-backward-WORD-begin)
	 ;; window
	 ("0" . delete-window)
	 ("1" . delete-other-windows)      ; show only current selected widnow
	 ("2" . split-window-vertically)
	 ("3" . split-window-horizontally)
	 ("4" . window-swap-states)        ; alternatively ace-swap-window
	 ("5" . transpose-frame)
	 ("6" . ivy-push-view)
	 ("7" . ivy-switch-view)
	 ("8" . winner-undo)
	 ("9" . winner-redo)
	 ("RET" . other-window)
	 ("DEL" . beacon-blink)
	 ;; ("o" . ace-window)
	 ;; ("o" . other-window)
	 ("=" . balance-windows)
	 ("-" . maximize-window)
	 ("p" . toggle-window-dedicated)

	 ("s" . swiper)
	 ;; ("a" . avy-goto-char)
	 ;; ("`" . beacon-blink)
	 ;; ("`" . pop-global-mark)
	 ;; ("`" . avy-goto-char-2)
	 ;; ("`" . cfw:open-org-calendar)
	 ;; ("`" . org-open-at-point-global)
	 ("`" . evil-goto-mark)
	 ;; ("`" . my-org-next-link-open)
	 ;; ("~" . ace-link)
	 ;; ("`" . ace-link)
	 ;; ("F" . ace-link)
	 ;; ("f" . ace-link)
	 ;; ("w" . my-minimap-mode)
	 ;; ("w" . sublimity-mode)
	 ("w" . avy-goto-char-2)
	 ;; ("w" . ace-window)
	 ;; ("w" . other-window)
	 ;; ("w" . ace-jump-char-mode)

         ;; Vim style
	 ;; ---------
	 ;; ("j" . forward-line)
	 ;; ("k" . previous-line)
	 ;; ("j" . View-scroll-line-forward)
	 ;; ("k" . View-scroll-line-backward)
	 ("j" . forward-paragraph)
	 ("k" . backward-paragraph)
	 ;; ("h" . backward-sentence)
         ;; ("l" . forward-sentence)
	 ;; ("h" . beginning-of-visual-line)
         ;; ("l" . end-of-visual-line)
	 ;; ("h" . left-word)
         ;; ("l" . right-word)
	 ;; ("h" . evil-backward-WORD-begin)
         ;; ("l" . evil-forward-WORD-begin)
	 ("h" . org-tree-slide-move-previous-tree)
	 ("l" . org-tree-slide-move-next-tree)
	 ;; ("w" . right-word)
         ;; ("N" . View-search-last-regexp-backward)  ; Regex previous result
	 ("/" . evil-search-forward)
	 ("?" . evil-search-backward)
	 ("n" . evil-search-next)
	 ("N" . evil-search-previous)
	 ;; ("n" . evil-normal-state)
	 ;; ("i" . View-exit)  ;; like 'e'
         ("e" . View-scroll-line-forward)             ; scroll down (forward) - opposite to 'y'
	 ;; ("f" . evil-scroll-page-down)
	 ;; ("b" . evil-scroll-page-up)
         ;; ("0" . beginning-of-visual-line)
	 ("]" . switch-to-next-buffer)
	 ("[" . switch-to-prev-buffer)
	 ;; ("]" . org-tree-slide-move-next-tree)
	 ;; ("[" . org-tree-slide-move-previous-tree)
	 ;; ("\\" . counsel-buffer-or-recentf)
	 ;; ("\\" . imenu-list)
	 ;; ("\\" . my-view-general-prefix)
	 ;; ("m" . imenu-list)
	 ("m" . imenu-list-minor-mode)
	 ;; ("m" . counsel-M-x)
	 ;; ("m" . evil-exit-emacs-state)

	 ;; Vim :: power g
         ;; ---------------
	 ;; ("g" . nil)  ;; interupting 'gcc' comment key binding 2021-04-21
	 ;; ("gg" . beginning-of-buffer)
	 ;; ("g" . beginning-of-buffer)
	 ("g" . evil-goto-first-line)
	 ;; ("gg" . evil-goto-first-line)
	 ;; ("go" . evil-jump-backward)
	 ;; ("gi" . evil-jump-forward)
	 ;; ("G" . end-of-buffer)
	 ("G" . evil-goto-line)  ; end of buffer in evil mode
	 
	 ;; <SPACE> binidng 
         ;; ---------------
	 ("SPC" . nil)
	 ;; ("SPC" . ace-window)
	 ;; ("SPC o" . ace-window)
	 ;; ;; power search
	 ("SPC p" . powerthesaurus-lookup-word-at-point)
	 ("SPC d" . define-word-at-point)
	 ("SPC t" . google-translate-at-point)
	 ("SPC a" . counsel-ag-thing-at-point)
	 ("SPC g" . engine/search-google)
	 ("SPC y" . engine/search-youglish)
	 ;; ;; engines - googles
	 ;; ("SPC gt" . engine/search-google-trans)
	 ;; ("SPC gn" . engine/search-google-news)
	 ;; ;; engines - others
	 ;; just type the word with M-x
	 ;; ("SPC ej" . engine/search-just_the_word)         ; C-x / j
	 ;; ("SPC eo" . engine/search-onelook)               ; C-x / o
	 ;; ("SPC ee" . engine/search-etymology-dictionary)  ; C-x / e
	 ;; ("SPC ey" . engine/search-youglish)              ; C-x / y
	 ;; ("SPC eg" . engine/search-google)                ; C-x / g
	 ;; others
	 ;; ("SPC" . evil-exit-emacs-state)
	 ;; ("SPC" . ace-window)
	 ;; ("SPC" . avy-goto-char)
	 ;; ("SPC" . ace-jump-char-mode)

	 ;; Org binding
         ;; ------------
	 ;; ("o" . nil)
	 ;; ("ots" . org-tree-slide-mode)
	 ;; ("ot" . org-tree-slide-mode)
	 ;; ("os" . org-tree-slide-mode)
	 ;; ("or" . org-narrow-to-subtree)
	 ;; ("ow" . widen)
	 ;; ("oa" . org-agenda)
	 ;; ("oc" . org-capture)

	 ;; Trans Functions
         ;; ------------
	 ;; ("t" . nil)
	 ;; ("tp" . powerthesaurus-lookup-word-at-point)
	 ;; ("td" . define-word-at-point)

	 ;; <ESCAPE> binidng 
         ;; ---------------
	 ("<escape>" . nil)
	 ;; ("<escape>" . keyboard-quit)
	 ("<escape> <escape>" . keyboard-quit)
	 ("<escape> x" . counsel-M-x)
	 ;; ("<escape> f" . counsel-find-file)
	 ;; ("<escape> r" . ranger)
	 ;; ("<escape> RET" . ace-window)
	 ;; ("<escape> a" . ace-window)
	 ;; ("<escape> p" . powerthesaurus-lookup-word-at-point)
	 ;; ("<escape> d" . define-word-at-point)
	 ;; ("<escape> t" . google-translate-at-point)
	 ;; ("<escape> a" . counsel-ag-thing-at-point)
	 ;; ("<escape> y" . engine/search-youglish)

	 ;; Unbind-keys
	 ;; -----------
	 ;; ("h" . nil)

	 ;; Additional-keys
	 ;; -----------
	 ("r" . revert-buffer)
	 ;; ("r" . writeroom-mode)
	 ;; ("a" . end-of-buffer)
	 ;; ("a" . evil-goto-line)  ; end of line
	 ;; ("a" . counsel-ag)
	 ("a" . ace-link)
	 ;; ("c" . cfw:open-org-calendar)
	 ;; ("z" . end-of-buffer)
	 ("z" . View-exit)  ;; like 'e'
	 ;; ("x" . View-exit)  ;; like 'e'
	 ;; ("z" . evil-exit-emacs-state)
	 ;; ("z" . kill-current-buffer)  ; same as (s-k)
	 ("t" . org-tree-slide-mode)
	 ;; ("v" . ace-window)
	 ("v" . evil-exit-emacs-state)
	 ;; ("v" . evil-visual-state)
	 ;; ("RET" . evil-exit-emacs-state)
	 ;; ("SPC" . evil-exit-emacs-state)
	 ;;
	 ("q" . kill-current-buffer)    ; same as (s-k)
	 ;; ("q" . View-exit)
	 ("x" . my-kill-current-buffer-and-window)
	 ("c" . recenter-top-bottom)
	 ;; ("i" . my-indirect-buffer)
	 ;; ("i" . my-clone-indirect-buffer)
	 ;; ("i" . evil-insert-state)
	 ;; ("o" . my-org-indirect-buffer)
	 ("i" . org-narrow-to-subtree)
	 ("o" . widen)
	 ;; ("ic" . my-clone-indirect-buffer)
	 ;; ("io" . my-org-indirect-buffer)

	 ;; ("," . org-narrow-to-subtree)
	 ;; ("." . widen)
	 ("," . my-clone-indirect-buffer)
	 ("." . my-org-indirect-buffer)

	 ;; olivetti
	 (";" . olivetti-narrow-width)
	 ("'" . olivetti-default-width)
         )
  )


;; (with-eval-after-load "view-mode"
;;   (define-key view-mode-map (kbd "\\") (kbd "M-SPC")))

;; make sure the cursor is changed visually
;; complicted to Evil cursors
;; (setq-default cursor-type 'box)
;; (add-hook 'view-mode-hook
;; 	  (defun view-mode-change-cursor-type-hook ()
;;             (setq cursor-type (if view-mode '(hbar . 10)))))

;; https://karthinks.com/software/batteries-included-with-emacs/
;; enter view-mode for read-only files
;; (setq view-read-only t)
;; https://www.emacswiki.org/emacs/ViewMode
;; (define-key ctl-x-map "\C-q" 'view-mode)  ;; means C-x C-q
;; key hint : edit 'e' / quit 'q'

;; https://stackoverflow.com/questions/3674637/enabling-certain-emacs-modes-or-features-almost-always
;; (add-hook 'text-mode-hook 'view-mode)                ; conflicted with org-mode of C-c * (converting), and 'org export'
;; (add-hook 'prog-mode-hook 'view-mode)
;; (add-hook 'markdown-mode-hook 'view-mode)
;; (add-hook 'org-mode-hook 'view-mode)
;; https://stackoverflow.com/questions/7899949/is-there-an-emacs-hook-that-runs-after-every-buffer-is-created
;; (add-hook 'after-change-major-mode-hook 'view-mode)  ; conflicted with almost org-mode work
;; (with-eval-after-load 'text-mode (view-mode 1))
;; (eval-after-load 'text-mode 'view-mode)
;; (eval-after-load 'org-mode 'view-mode)
;; **********************************
;; (add-hook 'find-file-hook 'view-mode)  ;; solved all add-hook problem 2021-04-06
;; some issue when install packages
;; autoload-generate-file-autoloads: names-dev.el:0:0: error: buffer-read-only: (names-autoloads.el)
;; **********************************

;; Solved View-mode for all and enable package installation 2021-04-08
;; https://www.reddit.com/r/emacs/comments/741tx6/how_to_change_default_findfile_action_for/
;; (defun my-view-mode ()
;;   (view-mode 1)
;;   (when (and (stringp buffer-file-name)
;;              (string-match "\\.el\\'" buffer-file-name))
;;     (view-mode 0)))

;; (add-hook 'find-file-hook 'my-view-mode) 

;; (add-hook 'kill-buffer-hook (lambda () (read-only-mode -1)))
;; (add-hook 'kill-buffer-hook (lambda () (view-mode -1)))

;; (add-hook 'kill-buffer-enter-hook (lambda () (read-only-mode -1)))
;; (add-hook 'kill-buffer-enter-hook (lambda () (view-mode -1)))

;; (defun my-view-mode-after-load-hook ()
;;   "Stuff to run in `view-mode'."
;;   ;; (remove-hook 'text-mode-hook 'view-mode))
;;   (setq view-read-only nil))
;; (eval-after-load 'view '(my-view-mode-after-load-hook))
;;
;; https://emacs.stackexchange.com/questions/3323/is-there-any-way-to-run-a-hook-function-only-once
;; (defun my-view-mode ()
;;   (interactive)
;;   (view-mode 1)
;;   (remove-hook 'text-mode-hook 'my-view-mode)
;;   )
;; (add-hook 'text-mode-hook 'my-view-mode)
;; (add-to-list 'text-mode-hook 'view-mode)                ; conflicted with org-mode of C-c *

;; https://stackoverflow.com/questions/15906332/change-emacs-mode-line-color-based-on-major-mode
;; (add-hook 'view-mode-hook
;;           (lambda ()
;;             (face-remap-add-relative
;;              'mode-line '((:foreground "ivory" :background "DarkOrange2") mode-line))))
;;
;; https://emacs.stackexchange.com/questions/32595/is-there-a-hook-that-runs-when-exiting-read-only-mode
;; (add-hook 'view-mode-off-hook
;;          (lambda ()
;;             (face-remap-add-relative
;;              'mode-line '((:foreground original-foreground :background original-background) mode-line))))
;; tip > every mode has 'off' hook like below:
;; https://emacs.stackexchange.com/questions/32595/is-there-a-hook-that-runs-when-exiting-read-only-mode
;; -------------
;; MODE-hook
;; MODE-on-hook
;; MODE-off-hook

;; https://www.reddit.com/r/emacs/comments/knoyz2/need_help_toggling_modes_and_settings_when/
(defun my-view-mode ()
  "Custom behaviours for `view-mode'."
  (unless (display-graphic-p)
    (if view-mode
      (face-remap-add-relative 'mode-line '((:foreground "red" :background "white") mode-line))
    (face-remap-add-relative 'mode-line '((:foreground "white" :background "black") mode-line))
    ))
  ;; (if view-mode
  ;;     (face-remap-add-relative 'mode-line '((:foreground "white" :background "black") mode-line))
  ;;   (face-remap-add-relative 'mode-line '((:foreground "textColor" :background "textBackgroundColor") mode-line))
  ;;   )
  (if view-mode
      ;; https://emacs.stackexchange.com/questions/32123/evil-binding-q-to-view-quit-in-view-mode-instead-of-evil-record-macro
      (evil-emacs-state 1)  ;; always related between evil and view-mode 2021-04-04
    )
  ;; (if view-mode
  ;;     (centered-cursor-mode 1)
  ;;   (centered-cursor-mode 0)
  ;;   )
  (when (display-graphic-p) 
    (if view-mode
	;; (face-remap-add-relative 'default '((:background "controlHighlightColor")))
	;; (face-remap-add-relative 'default '((:background "gray95")))
	(face-remap-add-relative 'default '((:background "gray90")))
	;; (face-remap-add-relative 'default '((:background "gray")))
	;; (face-remap-add-relative 'default '((:background "light gray")))
	;; (face-remap-add-relative 'default '((:background "#fdf6e3")))
      (face-remap-add-relative 'default '((:background "textBackgroundcolor")))
      )
    ;; (if (evil-emacs-state-p)
    ;;   (face-remap-add-relative 'default '((:background "textBackgroundcolor")))
    ;; )
  )

  ;; (if view-mode 
  ;;     (progn
  ;; 	(face-remap-add-relative 'mode-line '((:foreground "red" :background "black") mode-line))
  ;; 	(face-remap-add-relative 'default '((:background "controlHighlightColor")))
  ;; 	(evil-emacs-state 1)  ;; always related between evil and view-mode 2021-04-04
  ;; 	)
  ;;   ;; else
  ;;   (progn
  ;;     (face-remap-add-relative 'mode-line '((:foreground "textColor" :background "textBackgroundColor") mode-line))
  ;;     (face-remap-add-relative 'default '((:background "textBackgroundcolor")))
  ;;     )
  ;;   )

  )

(add-hook 'view-mode-hook #'my-view-mode)


;; (put 'clone-indirect-buffer-other-window 'disabled "\n Use 'make-indirect-buffer' instead due to view-mode with face issues")
;; (put 'org-tree-to-indirect-buffer 'disabled "\n Use 'my-indirect-buffer' instead due to view-mode with face issues")
;; (setq disabled-command-function 'ignore)

;; My-Indirect-Buffer for view-mode, which made face coloring confusing 2021-04-07
;; https://www.emacswiki.org/emacs/IndirectBuffers
;; problem: not getting attributes from original file (not inherence)
;; (defun my-indirect-buffer ()
;;   "Edit stuff in this buffer in an indirect buffer.
;;     The indirect buffer can have another major mode."
;;   (interactive)
;;   (let ((buffer-name (generate-new-buffer-name "*indirect*"))))
;;     (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
;;     (org-mode)
;;     (view-mode)
;;     )
;;   )

;; https://stackoverflow.com/questions/47327066/how-to-open-an-indirect-buffer-in-a-new-frame-in-a-single-call-in-emacs
(defun my-clone-indirect-buffer (newname display-flag &optional norecord)
  "Like `clone-indirect-buffer' but display in another window."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
     (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
           (read-buffer "Name of indirect buffer: " (current-buffer)))
       t)))
  (let ((pop-up-windows t))
  ;; (let ((pop-up-frames t))
    (setq-default buffer-save-without-query t)
    (setq enable-local-variables :all)
    (save-buffer t)
    (clone-indirect-buffer newname display-flag norecord))
    (revert-buffer :ignore-auto :noconfirm)
    (view-mode)
    ;; (other-window 1)
    (previous-window-any-frame)
    (read-only-mode 0)
    (view-mode)
    ;; (other-window 1)
    (previous-window-any-frame)
    (setq enable-local-variables :none)
    (setq-default buffer-save-without-query nil)
    (other-window 1)
    (org-cycle)
    (other-window 1)
  )

(global-set-key (kbd "C-c C-x i") 'my-indirect-buffer)
(global-set-key (kbd "C-x 4 i") 'my-indirect-buffer)

(defun my-org-indirect-buffer ()
  (interactive)
  ;; https://emacs.stackexchange.com/questions/51828/disaable-the-prompts-to-save-files
  (setq-default buffer-save-without-query t)
  ;; https://emacs.stackexchange.com/questions/28/safe-way-to-enable-local-variables
  (setq enable-local-variables :all)
  (save-buffer t)
  (org-tree-to-indirect-buffer)
  (revert-buffer :ignore-auto :noconfirm)
  (put 'eval 'safe-local-variable #'stringp)
  (view-mode)
  ;; (other-window 1)
  (previous-window-any-frame)
  (read-only-mode 0)
  (view-mode)
  (setq enable-local-variables :none)
  (setq-default buffer-save-without-query nil)
  (other-window 1)
  (org-cycle)
  (other-window 1)
  )


(defun my-kill-current-buffer-and-window ()
  (interactive)
  (kill-current-buffer)
  (delete-window)
  )



;; [ winner mode 2021-04-02
(winner-mode 1)
;; Not Working with Capital: Left / Right
;; (global-set-key (kbd "C-c C-<Left>") 'winner-undo)
;; (global-set-key (kbd "C-c C-<Right>") 'winner-redo)
;; (global-set-key (kbd "C-c <C-Left>") 'winner-undo)
;; (global-set-key (kbd "C-c <C-Right>") 'winner-redo)
;; Both OK with: inside <C-left> and outside C-<left>
;; (global-set-key (kbd "C-c <C-left>") 'winner-undo)
;; (global-set-key (kbd "C-c <C-right>") 'winner-redo)
(global-set-key (kbd "C-c C-<left>") 'winner-undo)
(global-set-key (kbd "C-c C-<right>") 'winner-redo)
;; ]


;; == general 2021-04-09 ==
;; ~~~ Use M-x Typing ~~~
;; general << evil-leader
;; general << custom key binding
;; https://dev.to/huytd/emacs-from-scratch-1cg6
(use-package general
  :ensure t
  :disabled
  :config (general-define-key
  :states '(normal visual insert emacs)
  ;; :prefix "SPC"
  :prefix "\\"  ;; like the leader key in vim
  :non-normal-prefix "M-SPC"
  ;; :non-normal-prefix "C-SPC"  ;; conflicted with 'mark set' like 'visual' mode in evil
  :keymaps 'override

  "p" '(powerthesaurus-lookup-word-at-point :which-key "powerthesaurus")
  "d" '(define-word-at-point :which-key "define word")
  "t" '(google-translate-at-point :which-key "google translate")
  "a" '(counsel-ag-thing-at-point :which-key "counsel-ag-at-point")

  "i" '(org-narrow-to-subtree :which-key "narrow subtree")
  "o" '(widen :which-key "widen trees")

  "m" '(imenu-list :which-key "imenu-list")
  "x" '(my-kill-current-buffer-and-window :which-key "kill buffer and close window")

  "," '(my-clone-indirect-buffer :which-key "my-clone-indirect-buffer")
  "." '(my-org-indirect-buffer :which-key "my-org-indirect-buffer")

))


;; [ evil goggles - display visual hint 2021-04-02
;; https://github.com/edkolev/evil-goggles
(use-package evil-goggles
  :ensure t
  :disabled
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))
;; ]

;; pluse (like beacon) bulit-in 2021-04-02
;; https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
		   scroll-down-command
                   recenter-top-bottom
		   other-window   ; beacon is better in same buffer with different window
		   ace-window     ; beacon is better in same buffer with differnet window
		   org-forward-element
                   org-backward-element
		   forward-paragraph
                   backward-paragraph
		   swiper
		   ;; org-cycle      ; delay ??
		   ))
  (advice-add command :after #'pulse-line)
)


;; engine-mode 2021-04-02
;; (require 'engine-mode)
(use-package engine-mode
  :ensure t
  :defer 1 ; do not load right at startup
  :init
  (engine-mode t)

  :config

  (defengine ctan
    "http://www.ctan.org/search/?x=1&PORTAL=on&phrase=%s"
    :docstring "Search the Comprehensive TeX Archive Network (ctan.org)")

  (defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
  (defengine google-images "https://www.google.com/search?tbm=isch&q=%s")
  (defengine google-news "https://news.google.com/search?q=%s")
  (defengine google-trans "https://translate.google.ca/?sl=auto&tl=ko&text=%s&op=translate")
  (defengine google-map "https://www.google.com/maps/search/%s")

  (defengine naver "https://m.search.naver.com/search.naver?sm=mtp_hty.top&where=m&query=%s")
  (defengine koDic "http://dic.impact.pe.kr/ecmaster-cgi/search.cgi?bool=and&word=yes&kwd=%s")
  (defengine wordreference "https://www.wordreference.com/koen/%s")
  (defengine bluedic "http://www.bluedic.com/%s")
  (defengine youglish "https://youglish.com/pronounce/%s" :keybinding "y")

  (defengine onelook "https://www.onelook.com/?w=%s" :keybinding "o")
  (defengine just_the_word "http://www.just-the-word.com/main.pl?word=%s" :keybinding "j")
  (defengine American_Heritage_Dic "https://www.ahdictionary.com/word/search.html?q=%s")
  (defengine FreeDicitionary "https://www.thefreedictionary.com/%s")
  (defengine Etymology-Dictionary "https://www.etymonline.com/search?q=%s" :keybinding "e")
  (defengine ngram "https://books.google.com/ngrams/graph?content=%s")
  (defengine longman "https://www.ldoceonline.com/dictionary/%s" :keybinding "l")

  (defengine powerthesaurus "https://www.powerthesaurus.org/%s" :keybinding "p")

  (defengine forvo "https://forvo.com/search/%s/")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s")
  (defengine reddit "https://www.reddit.com/search/?q=%s")
  (defengine github "https://github.com/search?ref=simplesearch&q=%s")
  (defengine twitter "https://twitter.com/search?q=%s")
  (defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d")

  ;; (setq engine/browser-function 'eww-browse-url)
  )


;; pdf-tools 2021-03-31
;; https://github.com/politza/pdf-tools/issues/206
;; (require 'pdf-tools)
;; (use-package pdf-tools :ensure t)
;; (pdf-tools-install)
;; 
;; https://xenodium.com/trying-out-emacs-pdf-tools/
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))


;; remove warning of cl is depreciated 2021-03-29
;; https://github.com/kiwanami/emacs-epc/issues/35#issuecomment-660639327
(setq byte-compile-warnings '(cl-functions))


;; overwrite in selection 2021-03-29
;; http://pragmaticemacs.com/page/8/
(delete-selection-mode t)


;; turn on visible bell 2021-03-29
;; (setq visible-bell t)


;; dedicated window 2021-03-27
;; https://emacs.stackexchange.com/questions/2189/how-can-i-prevent-a-command-from-using-specific-windows
(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
     "%s is up for grabs.")
   (current-buffer)))

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)


;; redo in evil 2021-03-25
;; https://github.com/emacs-evil/evil/issues/1382


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-enabled-themes nil)
 '(doc-view-continuous t)
 '(evil-undo-system 'undo-tree)
 '(fringe-mode 0 nil (fringe))
 '(global-undo-tree-mode t)
 '(google-translate-backend-method 'curl t nil "Customized with use-package google-translate")
 '(latex-run-command "pdflatex")
 '(minimap-automatically-delete-window 'visible)
 '(minimap-mode nil)
 '(org-agenda-files
   '("~/Documents/nvALT/interx-Pantos-Dispatcher-20210507.txt" "~/Documents/nvALT/mainx-Jiwoo.txt" "~/Documents/nvALT/INBOX_TODO_2021.txt" "~/Documents/nvALT/projx-TorontoLife.txt" "~/Documents/nvALT/projx-eix.txt"))
 '(org-agenda-start-on-weekday 0)
 '(org-agenda-time-grid
   '((daily today require-timed)
     (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
     "......" "----------------"))
 '(org-agenda-use-time-grid t)
 '(org-babel-python-command "python3")
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(ascii beamer html latex md odt org))
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
   '(evil-leader workgroups2 workgroups cm-mode all-the-icons neotree ranger org-crypt key-chord dimmer pdfgrep writeroom-mode sr-speedbar dired-narrow google-translate pomidor elfeed highlight-symbol korean-holidays minimap simplenote2 podcaster org-notifications org-wild-notifier ivy-posframe deft ivy-rich shell-pop writegood-mode sublimity php-mode keycast org-alert dashboard flycheck counsel ox-pandoc calfw linguistic ace-link swiper evil-commentary imenu-list org-download org-superstar org-tree-slide org-noter org-bullets define-word powerthesaurus indent-guide ace-window helpful org-roam htmlize ox-reveal transpose-frame centered-window undo-tree olivetti ivy markdown-preview-mode rainbow-delimiters pdf-tools helm-ack helm-ag ack ag helm-projectile projectile evil-surround auctex flymake jedi auto-complete pygen python-mode ein company-jedi ob-ipython company evil ace-jump-mode elpy use-package csv-mode pandoc smex ido-vertical-mode buffer-move markdown-mode multiple-cursors git-gutter helm magit exec-path-from-shell))
 '(podcaster-feeds-urls
   '("https://ipn.li/kernelpanic/feed" "http://sachachua.com/blog/tag/emacs-chat/podcast" "http://feeds.harvardbusiness.org/harvardbusiness/ideacast"))
 '(show-paren-mode t)
 '(writeroom-restore-window-config t))


;; replace kill buffer to counsel-kill-buffer 2021-03-24
;; https://github.com/abo-abo/swiper/pull/2776/commits/b873e82282f5e6c3b03610fda5306c2a50087c2c
(defun counsel-kill-buffer ()
  "Kill a buffer interactively using `ivy'."
  (interactive)
  (ivy-read "Kill buffer: " #'internal-complete-buffer
            :action #'ivy--kill-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'counsel-kill-buffer))
(global-set-key (kbd "C-x k") 'counsel-kill-buffer)


(setq default-directory "~/Documents/nvALT/")


;; 2021-03-24
;; To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)


;; show path in title bar 2021-03-22
;; https://stackoverflow.com/questions/29816326/how-to-show-path-to-file-in-the-emacs-mode-line

(setq frame-title-format
      '(buffer-file-name "%b - %f" ; File buffer
        (dired-directory dired-directory ; Dired buffer
         (revert-buffer-function "%b" ; Buffer Menu
          ("%b - Dir: " default-directory))))) ; Plain buffer


;; minibuffer margin 2021-03-22
;; (setq-default left-margin-width 1 right-margin-width 1)
;; (set-window-buffer nil (current-buffer))
;; replace to (set-frame-parameter nil 'internal-border-width 10)

;; writegood-mode 2021-03-21
;; (require 'writegood-mode)
(use-package writegood-mode
  :ensure t
  :config
    (add-hook 'text-mode-hook 'writegood-mode)
    (add-hook 'org-mode-hook 'writegood-mode)
    (add-hook 'markdown-mode-hook 'writegood-mode)
    (add-hook 'latex-mode-hook 'writegood-mode)
    (add-hook 'Latex-mode-hook 'writegood-mode)  ; AUCTeX
)

;; Preventing to create Lock Files liks `#filename.ext#` 2021-03-21
;; these cannot be moved to a different directory.
;; https://www.emacswiki.org/emacs/LockFiles
(setq create-lockfiles nil)


;; abbrev-mode 2021-03-20
;; https://www.oreilly.com/library/view/learning-gnu-emacs/1565921526/ch04s04.html
;; http://ergoemacs.org/emacs/emacs_abbrev_mode_tutorial.html
(setq-default abbrev-mode t)
(setq abbrev-file-name "~/Documents/nvALT/abbrev_defs.txt")
;; (read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)
;; (setq save-abbrevs 'silent)
;; (setq save-abbrevs 'silently)
;; (setq save-abbrevs nil)


;; sublimity - sublime style minimap
(when (display-graphic-p)
  (require 'sublimity)
  ;; (require 'sublimity-scroll)
  (require 'sublimity-map) ;; experimental
  ;; (require 'sublimity-attractive)
  ;; (setq sublimity-scroll-weight 10
  ;;       sublimity-scroll-drift-length 5)
  ;; no `setq`
  ;; (sublimity-map-set-delay 5)
  ;; (sublimity-map-set-delay nil)   ; always show, different from 0 value
  (setq sublimity-map-size 35)
  (setq sublimity-map-fraction 0.3)  ; maximum fraction of width
  (setq sublimity-map-text-scale -6)
  (setq sublimity-map-active-region 'nil)
  ;; (sublimity-mode 1)
)


;; Search At Point
;; Next/Previous Matching words for Emacs mode 2021-03-20
;; https://www.emacswiki.org/emacs/SearchAtPoint 
(global-set-key (kbd "C-*") 'evil-search-word-forward)
;; (global-set-key (kbd "C-#") 'evil-search-word-backward) ;; conflicted with org-table keybinding 2021-03-21
(global-set-key (kbd "C-&") 'evil-search-word-backward)


;; web-mode package 2021-03-19
;; https://web-mode.org/
;; (require 'web-mode)
(use-package web-mode
  :ensure t
  :config
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    ;; Using web-mode for editing plain HTML files can be done this way
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    ;; keybinding (not working)
    ;; (define-key web-mode-map (kbd "Tab") 'web-mode-fold-or-unfold)
)

;; (setq org-ellipsis "  ")  ; nerd font `v` nf-oct-chevron_down, hex: f47
;; (setq org-ellipsis " ∞ ")
(setq org-ellipsis " ↩ ")

;; -----------------------
;; MANUAL INSTALL PACKAGES
;; -----------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "mwe-log-commands")
;; (load "sticky-windows.el")  ; more test required 2021-03-27


;; == annotate.el 2021-04-19 ==
;; https://github.com/bastibe/annotate.el
;; (load "annotate")
;; (require 'annotate)


;; mew-log-commands 2021-03-18
;; ---------------------------
;; source :: https://github.com/overtone/live-coding-emacs/blob/master/lib/mwe-log-commands.el
;; how to set :: http://ergoemacs.org/emacs/emacs_installing_packages.html
;; (load "mwe-log-commands")
;; (add-hook 'text-mode-hook 'mwe:log-keyboard-commands)
;; (add-hook 'text-mode-hook 'mwe:open-command-log-buffer)
;; (add-hook 'prog-mode-hook 'mwe:log-keyboard-commands)
;; (add-hook 'prog-mode-hook 'mwe:open-command-log-buffer)
;; (add-hook 'text-mode-hook (function mwe:log-keyboard-commands))
;; (add-hook 'text-mode-hook (function mwe:open-command-log-buffer))
;; (add-hook 'prog-mode-hook (function mwe:log-keyboard-commands))
;; (add-hook 'prog-mode-hook (function mwe:open-command-log-buffer))

;; make a function with kbd binding by myself
(defun mwe-commands-exec ()
  "activate command-log view"
  ;; (load "mwe-log-commands")
  (interactive)
  (mwe:log-keyboard-commands)
  (mwe:open-command-log-buffer)
  (evil-window-set-width 40)
  (toggle-window-dedicated)  ; do not allow buffer to change by others
  (other-window 1)
  (message "mwe-log-commands is activated now")
)
;; (global-set-key (kbd "C-M-g") 'mwe-commands-exec)
(global-set-key (kbd "C-M-,") 'mwe-commands-exec)


;; TODO unload pressing by "C-M-y"
;; (defun mwe-commands-unload ()
;;   "activate command-log view"
;;   (interactive)
;;   (mwe:log-keyboard-commands nil)
;;   (mwe:open-command-log-buffer nil)

;;   (message "mwe-log-commands is deactivated now")
;; )
;; (global-set-key (kbd "C-M-y") 'mwe-commands-unload)


;; Makrdown mode key-rebindings 2021-03-13
;; https://github.com/jrblevin/markdown-mode/blob/master/markdown-mode.el#L5299
;; https://gist.githubusercontent.com/edavis10/6084120/raw/33c2297926d94db6ed4a3f1c0412bae7a3b06a14/init.el
(add-hook 'markdown-mode-hook
  (lambda ()
    (local-set-key (kbd "M-<up>")    'markdown-move-up)
    (local-set-key (kbd "M-<down>")  'markdown-move-down)
    (local-set-key (kbd "M-<left>")  'markdown-promote)
    (local-set-key (kbd "M-<right>") 'markdown-demote)
    (local-set-key (kbd "<tab>")     'evil-toggle-fold)  ; interfere org cycling spaces, only use in Markdown mode 2021-04-23
  )
)


(unless (display-graphic-p) 
  ;; terminal mode
  )


;; ---------------------------------------------------
;; Recent opened file history 2020-12-31
;; (Interchangable/trade-off with `Dashboard` package)
;; ---------------------------------------------------
;; (require 'recentf)
;; (recentf-mode 1)
;; (global-set-key "\C-xf" 'recentf-open-files)
;; (setq recentf-auto-cleanup 'never)


;; ----------------------------
;; Dashboard package 2021-03-12
;; ----------------------------
;; (require 'dashboard)
;; (dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :disabled
  :config
  (dashboard-setup-startup-hook)

  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)

  ;; agenda conflicted with `recents` list with org-agenda files 2021-03-12
  (setq dashboard-items '(
			  (bookmarks . 5)
			  (recents  . 15)
			  (projects . 5)
			  (agenda . 5)
			  ;; (registers . 5)
			  )
  )
  
)

;;===============
;; FIXED SETTING
;;===============

;; ---------------
;; Initial Setting
;; ---------------

;; parenthesis highlight
;; to change color :: face color -> (show-paren-match)
;; '(show-paren-match ((t (:background "Green"))))
(show-paren-mode)               ;; little interupted but useful
(unless (display-graphic-p)
  ;; https://www.emacswiki.org/emacs/ShowParenMode
  ;; (require 'paren) (set-face-background 'show-paren-match (face-background 'default))
  ;; (set-face-foreground 'show-paren-match "#def")
  ;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-background 'show-paren-match "black")
)


(setq ns-pop-up-frames nil)        ; only one frame use when openning a file 2021-01-28

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      ;; (menu-bar-mode -1)
      (scroll-bar-mode -1)
      ;; (global-hl-line-mode t)
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
        (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)  ; for AUCTeX
      )
)


;; show relative number line in programming modes
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))


;; Focus on `Buffer List` window when it is opened - 2021-02-21
;; https://www.reddit.com/r/emacs/comments/qjkwf/is_there_a_way_to_make_emacs_switch_to_the_buffer/
(defun buffer-list-switch ()
  "Switch to buffer list and activate the window"
  (interactive)
  (list-buffers)
  (select-window (get-buffer-window "*Buffer List*" 0))
)
(global-set-key (kbd "C-x C-b") 'buffer-list-switch)

(global-set-key (kbd "C-M-]") 'switch-to-next-buffer)
(global-set-key (kbd "C-M-[") 'switch-to-prev-buffer)

(global-set-key (kbd "<C-M-up>")     'counsel-find-file)
(global-set-key (kbd "<C-M-down>")   'ivy-switch-buffer)
(global-set-key (kbd "<C-M-left>")   'switch-to-prev-buffer)
(global-set-key (kbd "<C-M-right>")  'switch-to-next-buffer)

(global-set-key (kbd "s-<return>") 'other-window)


;; == indent-guide package 2021-02-24 ==
(indent-guide-global-mode)


;; Others
(setq backup-directory-alist `(("." . "~/.saves/")))         ; Backup files relocated 2020-10-09
(setq inhibit-startup-screen t)                              ; No welcome startup screen
(setq initial-scratch-message "")                            ; No scratch message 2020-10-10
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Maximize GUI window
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Maximize GUI window

;; Interupted Working Process 2021-02-15
;; (setq auto-save-visited-mode t)                           ; Auto Save   
;; (setq auto-save-visited-interval 1)                       ; Auto Save - Interval
(setq auto-save-default nil)                                 ; Preventing auto saved files like `#filename.ext#`
;; (setq auto-save-visited-file-name t)
;; (global-auto-revert-mode t)                               ; Auto Refresh

(global-visual-line-mode 1)                                  ; Visual Line Mode On
;; (global-display-line-numbers-mode)                        ; Display Line Numbers On

;; Bettery mode 2021-01-03
(display-battery-mode 1)

(setq ispell-program-name "/opt/local/bin/ispell")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(aw-leading-char-face ((t (:background "black" :foreground "White" :box (:line-width 3 :color "Black") :slant italic :weight bold :height 1.4))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(ivy-minibuffer-match-face-2 ((t (:background "#e99ce8" :box (:line-width -1 :color "#e99ce8") :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#bbbbff" :box (:line-width -1 :color "#bbbbff") :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#ffbbff" :box (:line-width -1 :color "#ffbbff") :weight bold))))
 '(mode-line ((((type x w32 ns)) (:overline t)) (((type tty)) (:inverse-video t))))
 '(mode-line-inactive ((t (:inherit (shadow mode-line)))))
 '(org-document-title ((t (:foreground "midnight blue" :weight bold :height 1.4))))
 '(org-ellipsis ((t nil)))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(pulse-highlight-start-face ((t (:background "dark gray")))))


;; white mode-line came from Binder/Olivetti reddit like below:
;; https://www.reddit.com/r/emacs/comments/fc8hc2/binder_modes_for_structuring_a_multifile_writing/
;; Thanks, but there's actually not much theme at all, just these two lines.
;; https://github.com/rnkn/dotfiles/blob/74dff2b1eadf3134c01e376cf8f42b24a0d1cc05/emacs/settings.el#L416-L417


;;=====================
;; == WINDOW Control ==
;;=====================

;; == Window Navigating ==
;; (global-set-key (kbd "<C-M-up>")     'windmove-up)
;; (global-set-key (kbd "<C-M-down>")   'windmove-down)
;; (global-set-key (kbd "<C-M-left>")   'windmove-left)
;; (global-set-key (kbd "<C-M-right>")  'windmove-right)


;; == Window Resizing ==
;; Remove SHIFT to avoid confilct with Org-Mode
(global-set-key (kbd "s-M-<up>")    'enlarge-window)
(global-set-key (kbd "s-M-<down>")  'shrink-window)
(global-set-key (kbd "s-M-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-M-<right>") 'enlarge-window-horizontally)


;; == buffer-move == package 2020-12-22
(setq buffer-move-behavior 'move)  ;; no swaping, move liternally 2021-04-23
;; s(Super) + S(Shift)
(global-set-key (kbd "<s-S-up>")     'buf-move-up)
(global-set-key (kbd "<s-S-down>")   'buf-move-down)
(global-set-key (kbd "<s-S-left>")   'buf-move-left)
(global-set-key (kbd "<s-S-right>")  'buf-move-right)

;; Conflicted to ORG-MODE (kbd "S-C- ... ")(SHIFT+CONTRL) / (kbd "M-S- ... ")(Meta+SHIFT)  2021-02-28


;; == ace-window 2021-02-23 ==
(global-set-key (kbd "M-o") 'ace-window)
;; (global-set-key (kbd "M-w") 'ace-swap-window)


;; display date in modeline 2020-12-07
;; (setq display-time-day-and-date t)
;; (display-time)
;; (setq display-time-format "[%Y-%m-%d %H:%M]")
(setq display-time-format "[%H:%M %m/%d(%a),%Y]")
(display-time-mode 1)
;; http://emacs.1067599.n8.nabble.com/Day-of-week-time-in-mode-line-td382473.html
;; (setq display-time-string-forms
;;       '((propertize (format-time-string "%A %F %R" now) 'face 'bold)))


;; display file size in modeline 2021-01-05
(size-indication-mode t)


;; pdflatex -> path "/Library/Tex/texbin" 2020-12-07
(setq latex-run-command "pdflatex")


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
;; (require 'yasnippet)
;; (yas-global-mode 1)


;; PLUG-IN: exec-path-from-shell 2020-12-08
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; which-key package 2021-01-28
;; ----------------------------
(which-key-mode)

;; https://gitter.im/syl20bnr/spacemacs?at=5912366c33e9ee771c8e538d
;; ---
;; duianto @duianto May 09 2017 17:50
;; @vonHabsi It doesn't seem possible to page through the ESC- which-key popups 2nd and 3rd pages of key bindings. Because when ESC has been pressed, then it's interpreted as if the M (Alt) key is held down, and when C-h is pressed, then it calls the command mark-defun which is bound to C-M-h. A similar issue prevents paging through the C-h which-key popup, because C-h C-h calls the command help-for-help.
;; ---
;; duianto @duianto May 09 2017 18:04
;; @vonHabsi One way I found to see more of the listed keys, is to maximize Spacemacs SPC T M.
;; There's an issue here about C-h: Paging through 'C-h' justbur/emacs-which-key#93 , where it is suggested to evaluate: (define-key help-map "\C-h" 'which-key-C-h-dispatch)
;;
(global-set-key (kbd "C-M-h") 'which-key-C-h-dispatch)


;; PLUG-IN: deft -> do not use: Korean issue, change filename forcely 2020-12-08
;; (require 'deft)
(use-package deft
  :ensure t
  :init
  :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-directory "~/Documents/nvALT/")
  ;;(setq deft-directory "~/Documents/test/")
  ;;(setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  )


;; *****************************************************************
;; 
;; ███████ ██    ██ ██ ██       ███    ███  ██████  ██████  ███████ 
;; ██      ██    ██ ██ ██       ████  ████ ██    ██ ██   ██ ██      
;; █████   ██    ██ ██ ██ █████ ██ ████ ██ ██    ██ ██   ██ █████   
;; ██       ██  ██  ██ ██       ██  ██  ██ ██    ██ ██   ██ ██      
;; ███████   ████   ██ ███████  ██      ██  ██████  ██████  ███████ 
;;                                                             
;; Enable Evil 2020-12-30
;; *****************************************************************

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
;; (require 'evil)
;; (evil-mode 1)
(use-package evil
  :ensure t
  :config
    (evil-mode 1)
)

(setq-default evil-default-state 'emacs)
;; (setq-default evil-default-state 'normal)

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

;; View-Mode (Terminal)
(add-hook 'evil-normal-state-entry-hook (lambda () (view-mode 0) (read-only-mode 0)))
(add-hook 'evil-insert-state-entry-hook (lambda () (view-mode 0)))
(add-hook 'evil-visual-state-entry-hook (lambda () (view-mode 0)))
(add-hook 'evil-replace-state-entry-hook (lambda () (view-mode 0)))
;; (add-hook 'evil-normal-state-exit-hook (lambda () (view-mode 1)))  ;; conflicted to scratch buffer
;; (add-hook 'evil-emacs-state-entry-hook (lambda () (view-mode 1)))

(if (display-graphic-p)
    (progn
      ;; if graphic (GUI)
      ;; <NORMAL>
      (add-hook 'evil-normal-state-entry-hook (lambda () (face-remap-add-relative 'default :underline "gray85")))
      (add-hook 'evil-normal-state-exit-hook (lambda () (face-remap-add-relative 'default :underline nil)))
      ;; (add-hook 'evil-normal-state-entry-hook (lambda () (face-remap-add-relative 'default :background "light gray")))
      (add-hook 'evil-normal-state-entry-hook (lambda () (hl-line-mode 1) (face-remap-add-relative 'hl-line nil :background "light gray")))
      ;; (add-hook 'evil-normal-state-entry-hook (lambda () (set-background-color "lightgray")))
      (add-hook 'evil-normal-state-exit-hook (lambda () (hl-line-mode 0)))
      (add-hook 'evil-normal-state-exit-hook (lambda () (face-remap-add-relative 'default :background original-background)))
      ;; (add-hook 'evil-normal-state-entry-hook (lambda () (hl-line-mode 1) (set-face-attribute hl-line-face nil :background "lightgray")))
      ;; (add-hook 'evil-normal-state-exit-hook (lambda () (set-background-color original-background)))
      ;; (add-hook 'evil-normal-state-exit-hook (lambda () (set-foreground-color original-foreground)))
      ;;
      ;; =OPERATOR=
      ;; (add-hook 'evil-operator-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightgray")))
      ;; (add-hook 'evil-operator-state-entry-hook (lambda () (set-background-color "gray")))
      ;;
      ;; <INSERT>
      ;; (add-hook 'evil-insert-state-entry-hook (lambda () (face-remap-add-relative 'default :background "lightyellow")))
      ;; (add-hook 'evil-insert-state-entry-hook (lambda () (set-background-color "lightyellow")))
      ;; (add-hook 'evil-insert-state-entry-hook (lambda () (set-foreground-color "black")))
      (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode 1) (face-remap-add-relative 'hl-line nil :background "light yellow")))
      (add-hook 'evil-insert-state-entry-hook (lambda () (face-remap-add-relative 'default :underline "light yellow")))
      (add-hook 'evil-insert-state-exit-hook (lambda () (face-remap-add-relative 'default :underline nil)))
      ;; (add-hook 'evil-insert-state-exit-hook (lambda () (set-face-attribute hl-line-face nil :weight 'normal)))
      ;; (add-hook 'evil-insert-state-exit-hook (lambda () (set-background-color original-background)))
      ;; (add-hook 'evil-insert-state-exit-hook (lambda () (set-foreground-color original-foreground)))
      (add-hook 'evil-insert-state-exit-hook (lambda () (hl-line-mode 0)))
      ;;
      ;; <VISUAL>
      ;; (add-hook 'evil-visual-state-entry-hook (lambda () (face-remap-add-relative 'default :background "light cyan")))
      ;; (add-hook 'evil-visual-state-entry-hook (lambda () (set-background-color "darkgray")))
      (add-hook 'evil-visual-state-entry-hook (lambda () (hl-line-mode 1) (face-remap-add-relative 'hl-line nil :background "light cyan")))
      (add-hook 'evil-visual-state-entry-hook (lambda () (face-remap-add-relative 'default :underline "light cyan")))
      (add-hook 'evil-visual-state-exit-hook (lambda () (face-remap-add-relative 'default :underline nil)))
      ;; (add-hook 'evil-visual-state-exit-hook (lambda () (set-background-color original-background)))
      (add-hook 'evil-visual-state-exit-hook (lambda () (hl-line-mode 0)))
      ;;
      ;; <REPLACE>
      ;; (add-hook 'evil-replace-state-entry-hook (lambda () (set-background-color "lightyellow")))
      ;; (add-hook 'evil-replace-state-entry-hook (lambda () (set-foreground-color "black")))
      ;; (add-hook 'evil-replace-state-exit-hook (lambda () (set-background-color original-background)))
      ;; (add-hook 'evil-replace-state-exit-hook (lambda () (set-foreground-color original-foreground)))
      ;;
      ;; <EMACS>
      (add-hook 'evil-emacs-state-entry-hook (lambda () (face-remap-add-relative 'default :background original-background)))
      (add-hook 'evil-emacs-state-entry-hook (lambda () (face-remap-add-relative 'default :foreground original-foreground)))
    )
    ;; else (optional - terminal mode)
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
(setq evil-normal-state-cursor '(box "black")
      evil-insert-state-cursor '((bar . 2) "red")
      evil-visual-state-cursor '((hollow . 2) "blue")
      evil-emacs-state-cursor '(box "black")
      )

;; https://emacs.stackexchange.com/questions/30582/how-do-i-change-the-mode-indicators-for-evil-mode-in-the-spaceline-mode-line-pac
(setq evil-normal-state-tag "NORMAL")
(setq evil-insert-state-tag "INSERT")
(setq evil-visual-state-tag "VISUAL")
(setq evil-replace-state-tag "REPLACE")

;; Color the evil tag - colors taken from spaceline
;; https://github.com/Malabarba/smart-mode-line/issues/195
(setq evil-normal-state-tag   (propertize " NORMAL "     'face '((:background "black"             :foreground "white")))
      ;; evil-emacs-state-tag    (propertize " <E> "        'face '((:background original-background :foreground original-foreground)))
      evil-insert-state-tag   (propertize " INSERT "     'face '((:background "red"               :foreground "white")))
      evil-replace-state-tag  (propertize " REPLACE "    'face '((:background "chocolate"         :foreground "black")))
      evil-motion-state-tag   (propertize " <Motion> "   'face '((:background "plum3"             :foreground "black")))
      evil-visual-state-tag   (propertize " VISUAL "     'face '((:background "blue"              :foreground "white")))
      evil-operator-state-tag (propertize " <Operator> " 'face '((:background "sandy brown"       :foreground "black"))))

;; evil key binding
(define-key evil-normal-state-map (kbd "SPC")   'evil-ex)
(define-key evil-visual-state-map (kbd "SPC")   'evil-ex)
(define-key evil-normal-state-map (kbd "j")     'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k")     'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "C-u")   'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u")   'evil-scroll-up)

;; useless -> interfere org-cycle-separator spacing 2021-04-22
;; but, works in markdown folding -> use in markdown keybinding
;; (define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)

;; (define-key evil-normal-state-map (kbd "z")     'evil-emacs-state)  ; use H/M/L instead
(define-key evil-normal-state-map (kbd "ZQ")     'evil-emacs-state)  ; Same as Original Vim
;; (define-key evil-normal-state-map (kbd "q")     'evil-emacs-state)
;; (define-key evil-normal-state-map (kbd "z")     'view-mode)
(define-key evil-normal-state-map (kbd "m")     'view-mode)

;; Noraml mode
(define-key evil-normal-state-map (kbd "<escape>") nil)
(define-key evil-normal-state-map (kbd "<escape><escape>") 'evil-force-normal-state)
(define-key evil-normal-state-map (kbd "<escape> x") 'counsel-M-x)
;; Visual mode
(define-key evil-visual-state-map (kbd "<escape>") nil)
(define-key evil-visual-state-map (kbd "<escape><escape>") 'evil-force-normal-state)
(define-key evil-visual-state-map (kbd "<escape> x") 'counsel-M-x)

(define-key evil-normal-state-map (kbd "RET")   'other-window)

(define-key evil-visual-state-map (kbd "C-t")     'edit-indirect-region)  ; in Emacs mode, "C-t" binded as well
;; https://vim.fandom.com/wiki/Capitalize_words_and_regions_easily
(define-key evil-normal-state-map (kbd "gCiw") (kbd "guiw~l"))   ; gciw :: confilicted with evil-commentary
(define-key evil-normal-state-map (kbd "gCC") (kbd "guu~l"))

(define-key evil-normal-state-map (kbd "DEL") 'beacon-blink)


;; to enable cgn dgn like vim 2021-04-24
;; https://stackoverflow.com/questions/42348794/search-text-in-emacs-evil-mode
(evil-select-search-module 'evil-search-module 'evil-search)

;; forward-sentence is based on the sentence-end variable
;; https://stackoverflow.com/questions/20257022/evil-emacs-mode-sentence-motions-and-other-questions
(setq sentence-end "[\\.\\?\\!] +") ;; . or ? or ! followed by spaces.
(define-key evil-normal-state-map ")" 'forward-sentence)


;; evil undo 2021-02-06
;; https://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims
(setq evil-want-fine-undo 't)
(setq evil-want-fine-undo 'fine)
;; (setq evil-undo-system 'undo-tree) ; not working tested 2021-02-07

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
(define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ; sometimes your have to escape the regex

;; evil-commentary mode package 2021-03-03
(evil-commentary-mode)


;; == evil-leader 2021-04-28 ==
(use-package evil-leader
  :ensure t
  :disabled
  :init
  (require 'evil-leader)
  (evil-leader/set-key
    "p" 'powerthesaurus-lookup-word-at-point
    "d" 'define-word-at-point
    "t" 'google-translate-at-point
    "a" 'counsel-ag-thing-at-point
    ;; "r" 'writeroom-mode
    "m" 'imenu-list
    "i" 'org-narrow-to-subtree
    "o" 'widen
   )
  :config
  (global-evil-leader-mode)
)


;; Exit insert mode by pressing j and then j quickly 2021-04-17
;; https://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
(use-package key-chord
  :ensure t
  :defer 1
  :init
  ;; https://www.emacswiki.org/emacs/KeyChord
  ;; Max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.2) ; default 0.1
  ;; Max time delay between two presses of the same key to be considered a key chord.
  ;; Should normally be a little longer than `key-chord-two-keys-delay'.
  (setq key-chord-one-key-delay 0.3) ; default 0.2
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)  ; preventing from navigating accedently, immediately or unconsciousely
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)  ; preventing from navigating accedently, immediately or unconsciousely
  :config
  (key-chord-mode 1)
  )


;; avy (like ace-jump and easy-motion in vim) 2021-03-04
;; (global-set-key (kbd "C-M-s") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char)
;; (global-set-key (kbd "M-s") 'avy-goto-char)
;; (global-set-key (kbd "C-'") 'avy-goto-char-2)


;;----------------------------------------------------------------
;; ACE JUMP MODE
;;----------------------------------------------------------------
;; ace jump mode major function
;; 
;; (add-to-list 'load-path "/Users/osickwon/.emacs.d/ace-jump-mode/")
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
(use-package ace-jump-mode :ensure t)
;; you can select the key you prefer to
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
;; WARNING :: conflict org-mode: C-c SPC => org-table-blank-field

;; `C-u C-c SPC` was not worked properly, so Used `C-c C-c` 2021-01-17
;; (define-key global-map (kbd "C-c C-c SPC") 'ace-jump-char-mode)

;; When org-mode starts it (org-mode-map) overrides the ace-jump-mode.
;; (https://github.com/winterTTr/ace-jump-mode/issues/47)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             ;; (local-set-key (kbd "\C-c SPC") 'ace-jump-mode)))
;;             (local-set-key (kbd "\C-c SPC") 'ace-jump-char-mode)))
 
;; 
;; enable a more powerful jump back function from ace jump mode
;;
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;If you use viper mode :
;(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;;If you use evil
;(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)


;; projectile package 2021-01-29
;; -----------------------------
;; (require 'projectile)
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)
;; use 'helm-projectile-ag' inseted of C-c p s s, which stands for ag serching in projectile
;; once install package of 'helm-projectile', you can also use 'helm-projectile-ag'
;; or install 'helm-ag' package to use same function
;; however, required intall '(sudo) port install the_silver_searcher' first (the_silver_searcher = ag)


;; helm-projectile package 2021
;; ----------------------------
;; (require 'helm-projectile)
(use-package helm-projectile
  :ensure t
  :disabled
  :init
  ;; (setq helm-projectile-fuzzy-match nil)
  ;; (helm-projectile-on)
  :config
  (global-set-key (kbd "C-M-s") 'helm-projectile-ag)
)


;; Olivetti 2021-02-11
;; -------------------
;; to remove boundry -> '(fringe-mode 0 nil (fringe)) in `(custom-set-variables` lines in front of this file. 
;; (require 'olivetti)
(use-package olivetti
  :ensure t
  :init
  ;; (add-hook 'text-mode-hook 'olivetti-mode)
  ;; (add-hook 'prog-mode-hook 'olivetti-mode)
  (add-hook 'after-change-major-mode-hook 'olivetti-mode)  ; cover text-mode and prog-mode
  (setq olivetti-body-width 0.99)
  ;; (setq olivetti-body-width 100)  ;; maximum
  (setq olivetti-minimum-body-width 30)
  :config
  ;; functions by width
  (defun olivetti-narrow-width ()
    (interactive)
    (olivetti-set-width 95)
    ) 
  (global-set-key (kbd "C-M-;") 'olivetti-narrow-width)

  (defun olivetti-default-width ()
    (interactive)
    (olivetti-set-width 0.99)
    ) 
  (global-set-key (kbd "C-M-'") 'olivetti-default-width)
)


;; Undo-Tree Package 2021-02-13
;; ------------------------------
;; prevent accidents :: Redo(C-?)
;; ------------------------------
(global-undo-tree-mode)                                      
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-auto-save-history t)
;; https://emacs.stackexchange.com/questions/26993/saving-persistent-undo-to-a-single-directory-alist-format
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))


;; ---------------------------------------------------
;; hydra 2021-03-04
;; ---------------------------------------------------
;; https://oremacs.com/2015/04/14/hydra-org-mode/
(defhydra hydra-global-org (:color blue
                            :hint nil)
  "
Timer^^        ^Clock^         ^Capture^
--------------------------------------------------
s_t_art        _w_ clock in    _c_apture
 _s_top        _o_ clock out   _l_ast capture
_r_eset        _j_ clock goto
_p_rint
"
  ("t" org-timer-start)
  ("s" org-timer-stop)
  ;; Need to be at timer
  ("r" org-timer-set-timer)
  ;; Print timer value to buffer
  ("p" org-timer)
  ("w" (org-clock-in '(4)))
  ("o" org-clock-out)
  ;; Visit the clocked task from any buffer
  ("j" org-clock-goto)
  ("c" org-capture)
  ("l" org-capture-goto-last-stored))

(global-set-key [f11] 'hydra-global-org/body)


;;** Example 8: the whole menu for `Buffer-menu-mode'
;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))
;; Recommended binding:
(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


;; Hydra-Dired 2021-03-13
;; https://github.com/abo-abo/hydra/wiki/Dired

(defhydra hydra-dired (:hint nil :color pink)
  "

** Dired navigation
 ------------------
 2021-01-06

 Up Directory (^)

 _>_ Next Dirline dired-next-dirline
 Prev Dirline (<)


_+_ mkdir          _v_iew           _m_ark             _(_ details             _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode           _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay           _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf(refresh) _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort                  _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra        \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                     _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  (">" dired-next-dirline)   ; added 2021-03-19
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

;; (define-key dired-mode-map "." 'hydra-dired/body)
;; (define-key dired-mode-map (kbd "C-o") 'hydra-dired/body)
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd ".") 'hydra-dired/body) ))

;; // end of hydra


;; beacon mode package 2021-03-03
;; Emacs 26.3 ok, but Emacs 27.1 issue
;; Pros compared to pulse.el :
;; 1. show beacon without text when `other-window`
;; 2. show exact position(column) in a line
(beacon-mode 1)                             ; Interupted `org-tree-slide-mode`
(setq beacon-size 5)
(setq beacon-color "black")
(setq beacon-blink-when-window-scrolls nil) ; Solved 'org-tree-slide-mode` issue 2021-03-29
(setq beacon-blink-when-focused t)

;; command-log-mode 2021-03-09
;; https://github.com/lewang/command-log-mode 
;; (require 'command-log-mode)
;; (add-hook 'LaTeX-mode-hook 'command-log-mode)


;; org-downloda 2021-03-01
;; (require 'org-download)
;; Drag-and-drop to `dired`
;; (add-hook 'dired-mode-hook 'org-download-enable)
;; (setq-default org-download-image-dir "~/Pictures")
;; https://github.com/abo-abo/org-download/issues/95#issue-413481682
;; (require 'org-download)
(use-package org-download
  :ensure t
  :init
  :config
  ;; Drag and drop to Dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir "./img")
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq org-download-screenshot-file "./img/tmp.png")
  :bind  
)


;; calendar view 2021-03-05
;; to show calendar :: M-x cfw:open-calendar-buffer
;; (require 'calfw)
(use-package calfw :ensure t)
;; For Org User (https://github.com/kiwanami/emacs-calfw#for-org-users)
;; (require 'calfw-org)
(use-package calfw-org :ensure t)
;; Then, M-x cfw:open-org-calendar

;; For iCal(Google Calendar) Users: (https://github.com/kiwanami/emacs-calfw#for-ical-google-calendar-users)
(require 'calfw-ical)
;; not working for now 2021-04-12
;; (cfw:open-ical-calendar "https://calendar.google.com/calendar/ical/osic.kwon%40gmail.com/public/basic.ics")




;; =====================================================================
;; 
;;  ██████╗ ██████╗  ██████╗       ███╗   ███╗ ██████╗ ██████╗ ███████╗
;; ██╔═══██╗██╔══██╗██╔════╝       ████╗ ████║██╔═══██╗██╔══██╗██╔════╝
;; ██║   ██║██████╔╝██║  ███╗█████╗██╔████╔██║██║   ██║██║  ██║█████╗  
;; ██║   ██║██╔══██╗██║   ██║╚════╝██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  
;; ╚██████╔╝██║  ██║╚██████╔╝      ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗
;;  ╚═════╝ ╚═╝  ╚═╝ ╚═════╝       ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝
;;                                                           
;; =====                        ORG-MODE                           =====
;; =====================================================================


;; built-in org-mouse turn on
(require `org-mouse)
;; (use-package org-mouse :ensure t)

(setq org-log-done 'time)                                     ; Show Closed(DONE) date in ORG-mode
(global-set-key "\C-ca" 'org-agenda)                          ; Org Agenda View shortcut
(global-set-key (kbd "C-M-<return>") 'org-insert-subheading)  ; Org Insert Sub-Heading 2021-03-31

(add-to-list 'org-emphasis-alist '("~" (:foreground "red3")))
(add-to-list 'org-emphasis-alist '("/" (:foreground "dark blue" :slant italic)))

;; (setq org-emphasis-alist
;;       '(
;; 	("~" (:foreground "red3"))
;;         ("/" (italic (:foreground "dark blue")))
;; 	)
;;       )


;; === ORG-BABEL ===

;; Org Babel - inline code 2020-10-19
;;------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   ;; (ipython . t)
   ;; (jupyter . t)
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



;; === org caputre === 2021-04-02
(global-set-key (kbd "C-c c") 'org-capture)
;; https://www.reddit.com/r/emacs/comments/7zqc7b/share_your_org_capture_templates/
(setq org-capture-templates
    '(
      ("i" "Inbox" checkitem (file+headline "~/Documents/nvALT/org_capture_note.txt" "Inbox on Working")
       "- [ ] %U - %^{Initial Text} :: %?")

      ("s" "Scrap with selected area" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Scrap")
       "** %? \nselected area:\n---------------\n %i \n---------------\nsource: %a - %U" :empty-lines 1)

      ("r" "Remind" checkitem (file+headline "~/Documents/nvALT/org_capture_note.txt" "Remind")
       "- [ ] %U - %^{Title} :: %?")

      ;; ("x" "Emacs")
      ;; ("xi" "Inbox Emacs" checkitem (file+headline "~/Documents/nvALT/org_capture_note.txt" "Emacs Config")
      ;;  "- [ ] %U - %^{Initial Text} :: %?")
      ;; ("xs" "Scrap Emacs" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Emacs Scrap")
      ;;  "** %^{Header Title}\n%U\n\n%?" :empty-lines 2)

      ;; ----------------------------------------------------------
      ;; insert at CURRENT POSITION in a buffer 2021-04-02
      ;; ----------------------------------------------------------
      ;; tip: When called with a ‘C-0’ (zero) prefix, insert a template at point.
      ;; >> == C-0 M-x `org-capture` == <<
      ;; https://emacs.stackexchange.com/questions/30595/how-to-org-capture-at-current-location
      ;; 
      ;; ("x" "Big3" plain ()
      ;; "%U %^{Thing1} / %^{Thing2} / %^{Thing3} " :empty-lines 1)

      ;; ("x" "Big3" plain ()
      ;;  "- [ ] %U %^{Thing1} / %^{Thing2} / %^{Thing3} ")
      ;;  ;; "- [ ] %U %^{Thing1} / %^{Thing2} / %^{Thing3} " :empty-lines 1)

      ;; https://www.youtube.com/watch?v=qCdScs4YO8k
      ("d" "Demo")

      ("da" "A option" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Demo")
       "* %^{Initial Text} src: %a  %?")

      ("db" "B option" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Demo")
       "* %^{Initial Text|Opt1|Opt2|Opt3} %?")

      ("dc" "C option" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Demo")
       "* %^{Initial Text|Opt1|Opt2|Opt3} \n SCEHDULED: %^t \n Some test heare %?")

      ("dd" "D option" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Demo")
       "** %? " :empty-lines 2)

      ("de" "E option" entry (file+headline "~/Documents/nvALT/org_capture_note.txt" "Demo")
       "** demo heading \n area: %i \n\n clipboard: %c \n\n source: %a" :empty-lines 2)
     )
  )
;; (global-set-key (kbd "C-M-]") (kbd "C-0 M-x org-capture"))  ; just tried, but not worked 2021-04-02

;; ----------------------------------------------------------
;; Templage Expansion 2021-04-02
;; https://orgmode.org/manual/Template-expansion.html
;; ------------------------------------------------------------
;; '%?'        > position point here. 
;;
;; '%t'        > Timestamp, date only.
;; '%^t'       > Select Timestamp, date only.
;; '%T'        > Timestamp, with date and time.
;; '%u', '%U'  > Like ‘%t’, ‘%T’ above, but inactive timestamps.
;;
;; '%i'        > Selected Area Contents
;; '%c'        > Clipboard Contents (to select %^C)
;; '%a'        > with Source location : Annotation (org-store-link)
;; ------------------------------------------------------------

;; type 2021-04-02
;; https://orgmode.org/manual/Template-elements.html 
;; -------
;; entry
;; item
;; checkitem
;; table-line
;; plain
;; -------


;; org-capture at point 2021-04-07
;; https://emacs.stackexchange.com/questions/30595/how-to-org-capture-at-current-location
;; == Insert Org-Capture at Current Postion :: _C-0_ M-x 'org-capture' ==
(defun org-capture-at-point ()
  "Insert an org capture template at point."
  (interactive)
  (org-capture 0))

;; (global-set-key (kbd "C-c C-c") #'org-capture-at-point)


;; == Org-Roam 2021-02-22 ==
(setq org-roam-directory  "~/Documents/nvALT")  ;; multi-directory ???
;; (setq org-roam-directory "~/Desktop/MyCloudSync/1. Next Actions/ELT/4. Sector in Finance/Presentation")
(setq org-roam-file-extensions '("org" "txt"))  ;; https://github.com/org-roam/org-roam/issues/461


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
                           ;; (org-agenda-files :maxlevel . 9)
			   ))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling


;; disable babel confirmation message 2020-12-13
(setq org-confirm-babel-evaluate nil)


;; org inline image resize 2020-12-13
(setq org-image-actual-width nil)

;; ------------------------------------
;; Helm package key bindings 2020-12-17
;; ------------------------------------
;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x b") 'helm-mini)  ;; cache problem occured 2021-03-15
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)  ;; trade off against `counsel-find-file`
;; (global-set-key (kbd "C-s") 'helm-occur)
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; Helm Tab autocompletion filenames :: https://www.youtube.com/watch?v=k78f8NYYIto
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;; Use arrow keys in helm-find-files, found below-code myself
;; ref: https://github.com/emacs-helm/helm/blob/master/helm-files.el#L741
;; (define-key helm-find-files-map (kbd "<right>")  'right-char)
;; (define-key helm-find-files-map (kbd "<left>")   'left-char)
;; (define-key helm-read-file-map (kbd "<right>")  'right-char)
;; (define-key helm-read-file-map (kbd "<left>")   'left-char)

;; https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
;; (helm-autoresize-mode 1)
;; (setq helm-autoresize-max-height 50)
;; (setq helm-autoresize-min-height 50)


;; ---------------------------------------
;; Ivy, Swiper, Counsel Bundle 2021-03-03
;; ---------------------------------------
;; https://github.com/abo-abo/swiper
;; (ivy-mode 1)  " replaced Helm search - think about this more 2021-03-05
(setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-s") 'swiper-thing-at-point)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
;; https://www.reddit.com/r/emacs/comments/a6yrqx/find_file_recursively_with_ivy_and_counsel/
(global-set-key (kbd "C-c f") 'counsel-file-jump) ; include sub folders, but can't change path
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)  ;; connected gitignore, but solved (--skip-vcs-ignores)
;; (global-set-key (kbd "C-c k") 'counsel-ack)  ;; ignore git status 2021-03-28
(global-set-key (kbd "C-c t") 'counsel-outline)
;; (global-set-key (kbd "C-x b") 'counsel-buffer-or-recentf)
;; (global-set-key (kbd "C-x b") 'counsel-ibuffer)  ;; can't create new buffer typing a new name
;; https://www.reddit.com/r/emacs/comments/i22z8f/question_regarding_counselswitchbuffer/
;; (global-set-key (kbd "C-x b") 'counsel-switch-buffer)  ;; nice, but preview function is too slow for me
;; https://oremacs.com/swiper/#global-key-bindings
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x f") 'counsel-recentf)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package ivy
  :diminish
  :custom
  ;; (ivy-height 25)
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  ;; :bind (("C-c C-r" . #'ivy-resume)
  ;;        ("C-c s"   . #'swiper-thing-at-point)
  ;;        ("C-s"     . #'swiper))
  )

;; ivy-height relative setting
;; https://github.com/abo-abo/swiper/issues/1722
(setq ivy-height-alist
      '((t
         lambda (_caller)
         (/ (frame-height) 2))))

;; https://emacs.stackexchange.com/questions/32862/ivy-disable-completion-for-a-command-in-minibuffer
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-insert-current)
;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)  ;; default

;; remove Counsel M-x always shows “^” 2021-03-16
;; https://emacs.stackexchange.com/questions/38841/counsel-m-x-always-shows
(setq ivy-initial-inputs-alist nil)

;; orderless matching like Helm 2021-03-21
;; https://oremacs.com/swiper/#completion-styles
;; ---
;;6.2 ivy–regex-ignore-order
;;ivy--regex-ignore-order ignores the order of regexp tokens when searching for matching candidates.
;;For instance, the input "for example" will match "example test for".
;; ---
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))

;; ignore gitignore file in counsel-ag 2021-03-28
;; https://stackoverflow.com/questions/48048529/silver-searcher-how-to-unignore-files-in-the-gitignore
;; https://github.com/abo-abo/swiper/issues/1641
;; https://oremacs.com/swiper/Changelog
(setq counsel-ag-base-command "ag --nocolor --nogroup --skip-vcs-ignores %s -- .")


;; counsel-ag at point 2021-04-25
;; https://www.reddit.com/r/emacs/comments/b78e6u/tips_ivy_search_thing_at_point_with_swiper/
;; https://github.com/abo-abo/swiper/issues/1068
(defun ivy-with-thing-at-point (cmd)
 (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

;; Example 1
(defun counsel-ag-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-ag))

;; Example 2
;; (defun swiper-thing-at-point ()
;;   (interactive)
;;   (ivy-with-thing-at-point 'swiper))


;; ivy-view mode 2021-03-29
;; save layouts
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c s") 'ivy-switch-view)


;; ivy-rich package 2021-03-31
;; https://github.com/Yevgnen/ivy-rich
;; (require 'ivy-rich)
;; (ivy-rich-mode 1)

;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-modify-column 'ivy-switch-buffer
;;                           'ivy-rich-switch-buffer-major-mode
;;                           '(:width 20 :face error)))

(use-package ivy-rich
  :ensure t
  :hook (ivy-mode . ivy-rich-mode)
  :custom (ivy-rich-path-style 'abbrev)
  :init (ivy-rich-mode 1)
  :config
  (ivy-rich-modify-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-size (:align right))
     (ivy-rich-switch-buffer-major-mode (:width 20 :face error)))))

;; Additional settings for ivy-switch-buffer
;; To abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)
(setq ivy-rich-path-style 'abbrev)


;; Counsel Tip > `M-o` to show more options after searching 2021-03-21
;; Those are
;; `open in other window`,
;; `copy` and more
;; for example


;; Interactive Do Mode like showing suggestion keyword 2020-12-18
;; Added ido-vertical-mode 2021-01-04
(require 'ido-vertical-mode)
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;; smex with ido for M-x :: consider helm or counsel alternatively
;; 2021-01-07
;; (require 'smex)
(use-package smex :ensure t)
(smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update)
;; confilicted with magit commit command 'C-c C-c' 2021-02-12 << double check required
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Git-Gutter 2020-12-18
(global-git-gutter-mode +1)


;; Multiple-Cursors(mc/) Package 2020-12-19
;; (require 'multiple-cursors)
(use-package multiple-cursors :ensure t)
; for multiple-line
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
; for keyword
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)


;; org-level whole line background in org-mode 2020-12-27
; ':extend t' option required to apply
; it also adjust in 'customize' menu in emacs configuaration 
; -------- Applied Automatically at Line 20 ------------------
;(setq org-fontify-whole-heading-line t
;      org-fontify-done-headline t
;      org-fontify-quote-and-verse-blocks t)


;;==============================================================
;;  ██▓███ ▓██   ██▓▄▄▄█████▓ ██░ ██  ▒█████   ███▄    █ 
;; ▓██░  ██▒▒██  ██▒▓  ██▒ ▓▒▓██░ ██▒▒██▒  ██▒ ██ ▀█   █ 
;; ▓██░ ██▓▒ ▒██ ██░▒ ▓██░ ▒░▒██▀▀██░▒██░  ██▒▓██  ▀█ ██▒
;; ▒██▄█▓▒ ▒ ░ ▐██▓░░ ▓██▓ ░ ░▓█ ░██ ▒██   ██░▓██▒  ▐▌██▒
;; ▒██▒ ░  ░ ░ ██▒▓░  ▒██▒ ░ ░▓█▒░██▓░ ████▓▒░▒██░   ▓██░
;; ▒▓▒░ ░  ░  ██▒▒▒   ▒ ░░    ▒ ░░▒░▒░ ▒░▒░▒░ ░ ▒░   ▒ ▒ 
;; ░▒ ░     ▓██ ░▒░     ░     ▒ ░▒░ ░  ░ ▒ ▒░ ░ ░░   ░ ▒░
;; ░░       ▒ ▒ ░░    ░       ░  ░░ ░░ ░ ░ ▒     ░   ░ ░ 
;;          ░ ░               ░  ░  ░    ░ ░           ░ 
;;          ░ ░                                          
;;
;; Python Development - Since 2021-01-06
;;==============================================================
;; src1: https://realpython.com/emacs-the-best-python-editor/#integration-with-jupyter-and-ipython

;; === ELPI ===
(use-package elpy
  :ensure t
  :after python
  :defer t
  :init
  ;; (elpy-enable)
  (add-hook 'python-mode-hook #'elpy-enable)
  )

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


;; https://sqrtminusone.xyz/posts/2021-05-01-org-python/
(use-package jupyter
  :ensure t
  :straight t
  )


;; https://sqrtminusone.xyz/posts/2021-05-01-org-python/
(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb")
  :disabled
  :after ox)



;; Optional :: Put below line at the bottom of .emacs
;; (workgroups-mode 1)  ;; confiliced with its key-binding C-c, which I don't want to use


;; == Star-Up ==
;; https://stackoverflow.com/questions/19085514/how-can-i-start-emacs-with-predefined-window
;; https://stackoverflow.com/questions/6578373/sleep-in-emacs-lisp
;; https://emacs.stackexchange.com/questions/17188/open-org-agenda-weekly-view-window-when-emacs-is-loaded
;; (if (display-graphic-p)
;;   (progn

;;     ;; option 1 > full information
;;     ;; (org-agenda nil "a")
;;     ;; (split-window-horizontally)
;;     ;; (find-file "~/Documents/nvALT/INBOX_TODO_2021.txt")
;;     ;; (other-window 1)
;;     (split-window-horizontally)
;;     ;; (split-window-vertically)
;;     ;; (find-file "~/Documents/nvALT/org_capture_note.txt")
;;     ;; (other-window 1)
;;     ;; (other-window 1)
;;     ;; (delete-window)
;;     ;; (other-window 1)
    
;;     ;; option 2 > calendar view
;;     ;; https://stackoverflow.com/questions/27758800/why-does-emacs-leave-a-gap-when-trying-to-maximize-the-frame
;;     (setq frame-resize-pixelwise t)  ; real miximize window
;;     (dotimes (n 1)
;;       (toggle-frame-maximized))
;;     (cfw:open-org-calendar)
;;     (lambda () (execute-kbd-macro (kbd "q")))  ; quit first and run again
;;     (message "waiting 0.5 sec")
;;     (sleep-for 0.5)
;;     (cfw:open-org-calendar)

;;     ;; option 3 > add more information
;;     ;; (split-window-horizontally)
;;     (other-window 1) (find-file "~/Documents/nvALT/INBOX_TODO_2021.txt")
;;     (split-window-vertically) (other-window 1) (find-file "~/Documents/nvALT/org_capture_note.txt")
;;     ;; https://emacs.stackexchange.com/questions/63697/how-to-run-shrink-window-horizontally-multiple-times-or-how-to-repeat-a-functio
;;     (shrink-window 7)  ; seven times
;;     (other-window 1)
;;     ))
;; ===
;; EOF
;; ===
