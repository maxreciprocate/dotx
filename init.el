;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name) "core/core-versions.el")
nil (not init-file-debug))
(load (concat (file-name-directory load-file-name) "core/core-load-paths.el")
  nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper.el")
  nil (not init-file-debug))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  (let ((file-name-handler-alist nil))
    (require 'core-spacemacs)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    ;; (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    ;; (spacemacs/dump-eval-delayed-functions)
    ))

(setq-default mode-line-format nil)
(setq mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
                        (vc-mode vc-mode)))

(require 'server)
(when dotspacemacs-server-socket-dir
  (setq server-socket-dir dotspacemacs-server-socket-dir))
(unless (server-running-p)
  (message "Starting a server...")
  (server-start))

(setq history-length 100)
(put 'minibuffer-history 'history-length 500000)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)
(savehist-mode -1)

(setq bookmark-default-file "~/.emacs.d/bookmarks")
(setq bookmark-save-flag 1)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d./.cache")))
;; ■ ORG
(require 'alert)
(setq alert-default-style 'osx-notifier)

(alert "+" :title "it's u, the potion seller!")

(setq org-pomodoro-length 1
      org-pomodoro-short-break-length 1
      org-pomodoro-long-break-length 5
      org-pomodoro-clock-break nil
      org-pomodoro-finished-sound ""
      org-pomodoro-short-break-sound ""
      org-pomodoro-long-break-sound "")

(add-hook 'org-pomodoro-break-finished-hook 'org-pomodoro)

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; this might slowdown startup quite a lot
(setq org-agenda-files
      (append (directory-files-recursively "~/iros/" "^[^\\.]\\w*\\.org$") ;; don't match swap files
              (directory-files-recursively "~/leaf/" "^[^\\.]\\w*-?\\w*\\.org$")))

(setq org-agenda-span 'day)
(setq org-agenda-block-separator "")

(setq org-modules (quote (org-protocol org-drill)))

(require 'org-protocol)
(require 'org-drill)

(add-to-list 'load-path "/Users/loushxiv/pacs/org-roam")
(require 'org-roam)
(setq org-roam-directory "/Users/loushxiv/leaf/leafs")

(setq org-drill-save-buffers-after-drill-sessions-p nil
      org-drill-maximum-duration 30
      org-drill-save-buffers-after-drill-sessions-p nil
      org-drill-scope org-agenda-files
      org-drill-question-tag "@")

(setq warning-minimum-level :emergency
      browse-url-browser-function 'browse-url-default-browser; 'browse-url-firefox
      browse-url-firefox-program "open"
      ;; browse-url-firefox-arguments '("--private-window")
      shell-command-default-error-buffer "*Messages*"
      large-file-warning-threshold nil
      org-clock-persist 'history)

(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'yas-reload-all)
(add-hook 'org-mode-hook 'visual-line-mode)

;; nevertheless this mode is wack
(add-hook 'auto-mode-alist '("\\.log" . (lambda () (auto-revert-tail-mode))))
(setq confirm-kill-processes nil)

(setq ispell-program-name "aspell")
;; (pdf-tools-install)

(add-hook 'org-mode-hook (lambda ()
			               (push '("[ ]" . "☐") prettify-symbols-alist)
			               (push '("[X]" . "☑") prettify-symbols-alist)
			               (push '("[-]" . "❍") prettify-symbols-alist)
			               (prettify-symbols-mode)))

(setq org-roam-completion-everywhere t)
(setq org-capture-templates
      '(("t" "Hands task" plain (clock) "- [ ] %?" :clock-keep t)
        ;; ("g" "Hands global task" plain (file+headline "~/orgs/s.org" "■ χορηγια") "- [ ] %?")
        ("g" "Hands global task" plain (file "~/leaf/meta.org") "* TODO %?")
        ("i" "Input task" plain (clock) "- [ ] %? %i")
        ("d" "Hold up" plain (file "~/leaf/diota.org") "" :append t)
        ("v" "Input link" plain (clock) "%i" :immediate-finish t :append t)
        ("f" "File link"  plain (clock) "file:%F" :immediate-finish t :append t)
        ("p" "Paper" entry (file "~/leaf/papers.org") "* TODO %i")
        ; Some extraction here?
        ("e" "Anything paste" plain (file "~/leaf/every.org") "%i" :immediate-finish t :prepend t :empty-lines 1)))

(require 'ox-publish)
(setq org-publish-project-alist
      '("salience"
        :base-directory "~/iros/literate/"
        :base-extension "org"
        :publishing-directory "~/iros/literate/salience/"
        :recursive nil
        :publishing-function org-html-publish-to-html
        :headline-levels 4             ; Just the default for this project.
        :auto-preamble t
        )
      )

(setq org-html-postamble nil
      org-html-style-default ""
      org-html-metadata-timestamp-format ""
      org-html-scripts "")

;; ssh-agent for magit
(keychain-refresh-environment)
;; (org-clock-persistence-insinuate)

(setf org-html-mathjax-template "")
;; (org-clock-persistence-insinuate)
(setq org-clock-history-length 23)
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook #'visual-line-mode)

(setq yas-snippet-dirs '("~/dotx/snippets"))
(yas-global-mode)

(define-key evil-insert-state-map (kbd "C-q") 'yas-expand)

(defun core/select-line () (interactive)
       (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun core/insert-odious-line () (interactive)
       (insert "sin²(θ) is odious to me"))

(defun core/insert-lambda () (interactive) (insert "λ"))

(defun core/pwd ()
  (s-join "/" (-butlast (s-split "/" buffer-file-name))))

;;; Plainly opens zathura on a filename
;; (if (equal major-mode 'dired-mode)
;; (dired-do-shell-command "zathura")))
;; (defun core/zathura-read () (interactive)
;;        (let ((filename (progn (string-match "~\.*\\.pdf" (core/select-line))
;;                               (match-string-no-properties 0 (core/select-line)))))
;;          (start-process-shell-command "reading-time" "*Messages*" (format "open ~/shel/%s.pdf" filename))))

(defun core/zathura-read () (interactive)
       (let ((filename (car (reverse (split-string (core/select-line))))))
         (message filename)
         (start-process-shell-command "reading-time" "*Messages*" (format "open ~/shel/%s.pdf" filename))))


;;; Executed on org-entry will clock-in and either open zathura-reader or open file folder
;;; TODO Links
(defun core/entry-action () (interactive)
       (let* ((path (->> (core/select-line) (s-split " ") third)) ; third?
              (zathura-extensions '(".pdf" ".epub" ".djvu")))
         (if (not (s-starts-with? "~" path))
             nil
           (progn
             (org-clock-in)
             (if (-any? (lambda (extension) (s-ends-with? extension path)) zathura-extensions)
                 (core/zathura-read path)
               (progn
                 (org-show-entry)
                 (find-file path)))))))

(spacemacs/set-leader-keys
  (kbd "<ESC>") 'save-buffer
  (kbd "<DEL>") 'ispell
  (kbd "b k") 'kill-this-buffer
  (kbd "y f") 'core/zathura-read
  (kbd "y r") 'core/entry-action
  (kbd "i o") 'core/insert-odious-line
  (kbd "i l") 'core/insert-lambda
  (kbd "o c") 'org-clock-goto
  (kbd "t c") 'org-roam-node-insert
  (kbd "t f") 'org-roam-node-find
  (kbd "y d") 'org-drill
  (kbd "y c") 'org-drill-resume
  (kbd "s a") 'bookmark-set
  (kbd "s l") 'bookmark-bmenu-list
  ;; (kbd "o s") (lambda () (interactive) (find-file "~/leaf/spells.org"))
  (kbd "o s") (lambda () (interactive) (find-file "~/iros/trlx/autocrit/safe.org"))
  (kbd "o d") (lambda () (interactive) (find-file "~/leaf/diota.org"))
  (kbd "o l") (lambda () (interactive) (find-file "~/leaf/every.org"))
  (kbd "o e") (lambda () (interactive) (find-file "~/leaf/every.txt"))
  (kbd "o z") (lambda () (interactive) (find-file "~/leaf/meta.org"))
  ;; (kbd "o s") (lambda () (interactive) (find-file "~/leaf/star.org"))
  (kbd "o s") (lambda () (interactive) (find-file "~/leaf/spells.org"))
  (kbd "o m") (lambda () (interactive) (find-file "~/iros/muses/muses.org"))
  (kbd "o r") (lambda () (interactive) (find-file "~/iros/revel/revel.org"))
  (kbd "o g") (lambda () (interactive) (find-file "~/iros/engagement/engage.org"))
  (kbd "o p") (lambda () (interactive) (find-file "~/leaf/papers.org"))
  (kbd "o b") (lambda () (interactive) (find-file "~/leaf/bbb.org"))
  (kbd "o a") (lambda () (interactive) (find-file "~/iros/autocrit/autocrit.org"))
  (kbd "o c") (lambda () (interactive) (find-file "~/leaf/cakes.org"))
  (kbd "o x") (lambda () (interactive) (find-file "~/leaf/xpov.org"))
  (kbd "o q") (lambda () (interactive) (find-file "~/leaf/qrit.org"))
  (kbd "o f") (lambda () (interactive) (find-file "~/leaf/fun.org"))
  (kbd "o w") (lambda () (interactive) (find-file "~/leaf/wurned.org"))
  (kbd "o t") (lambda () (interactive) (find-file "~/leaf/leafs/word.org"))
  (kbd "o y") (lambda () (interactive) (find-file "~/leaf/leafs/kama.org"))
  (kbd "o g") (lambda () (interactive) (find-file "~/leaf/leafs/main.org"))
  (kbd "o n") (lambda () (interactive) (find-file "~/leaf/stash.txt"))
  (kbd "o v") (lambda () (interactive) (find-file "~/leaf/rev/rev.org"))
  (kbd "o k") (lambda () (interactive) (find-file "~/iros/expan/kr/kr.org"))
  (kbd "f e i") (lambda () (interactive) (find-file "~/dotx/init.el"))
  (kbd "f e d") (lambda () (interactive) (find-file "~/dotx/.spacemacs")))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))


;; ■ TEX
(require 'ox-latex)
(setq org-latex-pdf-process
      '("xelatex -shell-escape -8bit -interaction nonstopmode %f"
        "xelatex -shell-escape -8bit -interaction nonstopmode %f"))

(setq mode-require-final-newline t)
(setq require-final-newline t)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("fontsize" "\\footnotesize") ("obeytabs" "true")))
(setq org-src-preserve-indentation t)

(require 'olivetti)
(setq olivetti-body-width 0.8)
(add-hook 'tex-mode-hook 'flyspell-mode)
;; (ws-butler-global-mode 1)

(setq ispell-dictionary "british"
      ispell-extra-args '() ;; TeX mode "-t"
      ispell-silently-savep t)

(add-to-list 'org-latex-classes
             '("book-noparts"
               "\\documentclass{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(make-variable-buffer-local (defvar texorg-mode nil))

(defun texorg-mode (&optional ARG)
  (interactive (list 'toggle))
  (setq texorg-mode
        (if (eq ARG 'toggle)
            (not texorg-mode)
          (> ARG 0)))
  (if texorg-mode
      (message "texorg!")
    (message "🫡"))
  (run-hooks 'texorg-mode-hook))

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(defun build-tex () (interactive)
       (org-latex-export-to-latex)
       (call-process-shell-command (format "xelatex -shell-escape -8bit -interaction nonstopmode rev.tex") nil 0))

(defvar texorg-mode-hook nil)
(add-hook 'texorg-mode-hook (lambda () (add-hook 'after-save-hook #'build-tex nil 'local)))

;; ■ ELISP
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(setq wakatime-api-key "waka_f5f0cab6-ef03-43f5-becb-64fb9e9c4053")
(global-wakatime-mode)

;; ■ NETSCAPE
(setq js-indent-level 2)
(setq js2-strict-inconsistent-return-warning nil)

;; ■ C
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(setq c-basic-offset 2
      tab-width 2
      indent-tabs-mode nil)

(setq-default tab-width 4)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

;; (require 'ccls)
;; (setq ccls-executable "~/pacs/ccls/Release/ccls")
;; (setq c-c++-backend 'lsp-ccls)
;; (add-hook 'c++-mode #'lsp)
;; (add-hook 'c-mode #'lsp)

;; ■ SNAKE
(setq python-guess-indent t
      python-indent-offset 4)

;; (setq flycheck-checker 'python-pylint
;;       flycheck-checker-error-threshold 900
;;       flycheck-pylintrc "~/.pylintrc"
;;       lsp-pyls-plugins-pydocstyle-ignore t
;;       lsp-pyls-plugins-pydocstyle-enabled nil
;;       lsp-idle-delay 1
;;       lsp-eldoc-enable-hover t
;;       lsp-signature-auto-activate t
;;       ;; lsp-enable-on-type-formatting nil
;;       lsp-enable-symbol-highlighting t
;;       auto-completion-minimum-prefix-length 1
;;       lsp-ui-doc-enable t)

(add-hook 'python-mode-hook (lambda ()
                              (progn
                                (lsp)
                                (setq before-save-hook nil)
                                (lsp-toggle-signature-auto-activate)
                                )))

(add-hook 'python-init-hook (lambda () (remove-hook 'before-save-hook 'delete-trailing-whitespace t)))
(add-hook 'before-save-hook (lambda () (setq before-save-hook nil)))

(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-enable nil)
;; (setq lsp-toggle-)

(setq delete-trailing-lines nil)
(setq python-mode-hook nil)
(setq python-init-hook nil)

;; ■ JULY

;; (setq lsp-julia-package-dir nil)
;; (require 'lsp-julia)
;; (require 'eglot-jl)
;; (setq eglot-connect-timeout 9999999)
;; (add-hook 'julia-mode-hook (lambda () (auto-complete-mode -1)))

;; ■ JOVIAN

(require 'bind-key)
(setq jupyter-repl-echo-eval-p nil)
(setq ein:jupyter-default-server-command "/opt/homebrew/bin/jupyter")

(defconst sign "■ ~")

(defun jovian/insert-stop-sign () (interactive)
       (evil-insert-newline-above)
       (insert sign)
       (comment-line 1))

;;; TODO gg G
(defun jovian/jump-to-next-square () (interactive)
       (search-forward sign nil t)
       (forward-line)
       (evil-digit-argument-or-evil-beginning-of-line))

(defun jovian/jump-to-prev-square () (interactive)
       (forward-line -2)
       (search-backward sign nil t)
       (forward-line)
       (evil-digit-argument-or-evil-beginning-of-line))

(font-lock-add-keywords 'emacs-lisp-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'org-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'python-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'js2-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'julia-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'c-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'c++-mode '(("■.*" 0 font-lock-warning-face prepend)))

(defun jovian/send (string)
  (interactive "M")
  (with-current-buffer (jovian/current-notebook-buffer)
    (ein:shared-output-eval-string nil string nil)))

(defun jovian/send-line () (interactive)
       (let* ((line (core/select-line))
              (start (string-match "\\([a-z0-9_]+\\) = " line)))
         (if start
             ;; that's in the julia case of auto returns
             (jovian/send (format "%s;%s" line (match-string-no-properties 1 line)))
             ;; (jovian/send (format "%s" line))
           (jovian/send line))))

; ANY
(defun jovian/send-block () (interactive)
       (let ((start (save-excursion
                      (if (search-backward sign nil t)
                          (line-end-position)
                        (point-min))))
             (end  (save-excursion
                     (if (search-forward sign nil t)
                         (line-beginning-position)
                       (point-max)))))
         (jovian/send (buffer-substring-no-properties start end))))


; ANY
(defun jovian/send-edit () (interactive)
       (let* ((line (core/select-line))
              (start-pos (string-match "=\\(\.*\\)" line))
              (expression (if start-pos
                              (substring-no-properties line (+ start-pos 1))
                            line)))
         (jovian/send (format "@edit %s" expression))))

(defun jovian/send-help () (interactive)
       (let* ((line (core/select-line))
              ;; relies on leaving assignments with an open equal sign
              ;; but keywords with tight assignments
              (start-pos (string-match " = " line))
              (start-pos (if (null start-pos) nil (+ start-pos 3)))
              (end-pos (string-match "(" line start-pos))
              (expression (substring-no-properties line start-pos end-pos)))
         ;; (message (format "[%s]-[%s] help(%s)" start-pos end-pos expression))))
         (jovian/send (format "help(%s)" expression))))

(defun jovian/send-dir () (interactive)
       (jovian/send (format "dir(%s)" (core/select-line))))

(defun jovian/send-shape () (interactive)
       (jovian/send (format "print(len(%1$s)) if isinstance(%1$s, (list,set)) else print(%1$s.shape)" (core/select-line))))

(defun jovian/current-notebook-buffer () (interactive)
       (--last (string-suffix-p ".ipynb*" it) (helm-buffer-list)))

(defun jovian/nuke () (interactive)
       (->> (helm-buffer-list)
            (--filter (string-prefix-p "*ein:" it))
            (--map (kill-buffer it))))

(defun jovian/interrupt-kernel () (interactive)
       (with-current-buffer (jovian/current-notebook-buffer)
         (ein:notebook-kernel-interrupt-command)))

(defun sensual/wrap () (interactive)
       (progn
         (evil-append-line 1)
         (insert ")")
         (evil-insert-line 1)
         (insert "(")
         (evil-insert-line 1)))

(defun sensual/print-wrap () (interactive)
       (progn
         (evil-append-line 1)
         (insert "=}')")
         (evil-insert-line 1)
         (insert "print(f'{")
         (evil-insert-line 1)))

(defun jovian/restart-kernel () (interactive)
       (with-current-buffer (jovian/current-notebook-buffer)
         (ein:notebook-restart-session-command)))

(defun jovian/pop-output () (interactive)
       (let ((original (current-buffer)))
         ;; display-window is just too hard, isn't it?
         (ein:shared-output-pop-to-buffer)
         ;; disabling undo-tree, for it swears on >50MB of output
         ;; (undo-tree-mode)
         (pop-to-buffer original)))

(defun jovian/mode->kernel () (interactive)
       (pcase (format "%s" major-mode)
         ("js2-mode" "javascript")
         ("python-mode" "python3")
         ("julia-mode" "julia-1.8")
         ("c-mode" "c")))

;; (activate-input-method "TeX")
;; (let ((quail-current-package (assoc "TeX" quail-package-alist)))
;;   (quail-define-rules ((append . t))
;;                       ("^" ^)))

(defun jovian/new-notebook-steps (notebook-buffer created)
  (save-excursion
    (ein:connect-to-notebook-buffer (jovian/current-notebook-buffer))
    (jovian/pop-output)

    ;; (sit-for 10)
    ;; (evil-goto-first-line)
    ;; (forward-line)
    ;; (jovian/send-block)
    ))

;;; Starts jupyter notebook in the same directory as the current buffer
;;; With the kernel corresponding to the major mode of the current buffer
;;; TODO general URI
;;; TODO autoload init segment
;;; TODO delete Untitles
(defun jovian/fly () (interactive)
       (let ((path (core/pwd)))
         (jovian/nuke)
         (shell-command "rm Untitled*.ipynb")
         (ein:jupyter-server-start ein:jupyter-default-server-command path)
         (sit-for 1)
         (ein:notebooklist-new-notebook
          "http://127.0.0.1:8888" (jovian/mode->kernel) 'jovian/new-notebook-steps t nil)))

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "sc" 'ein:connect-to-notebook-buffer
  "so" 'jovian/start-notebook
  "sn" 'jovian/nuke
  "sk" 'jovian/interrupt-kernel
  "sr" 'jovian/restart-kernel
  "sy" 'jovian/pop-output
  "sj" 'jovian/fly)

(spacemacs/set-leader-keys-for-major-mode 'julia-mode
  "sc" 'ein:connect-to-notebook-buffer
  "so" 'jovian/start-notebook
  "sn" 'jovian/nuke
  "sk" 'jovian/interrupt-kernel
  "sr" 'jovian/restart-kernel
  "sy" 'jovian/pop-output
  "sj" 'jovian/fly)

(spacemacs/set-leader-keys-for-major-mode 'js2-mode
  "sc" 'ein:connect-to-notebook-buffer
  "so" 'jovian/start-notebook
  "sn" 'jovian/nuke
  "sk" 'jovian/interrupt-kernel
  "sr" 'jovian/restart-kernel
  "sy" 'jovian/pop-output
  "sj" 'jovian/fly)

(define-prefix-command 'jovian-map)
(define-key 'jovian-map (kbd "C-s") 'jovian/insert-stop-sign)
(define-key 'jovian-map (kbd "C-e") 'jovian/send-line)
(define-key 'jovian-map (kbd "C-q") 'jovian/send)
(define-key 'jovian-map (kbd "C-j") 'jovian/send-block)
(define-key 'jovian-map (kbd "C-r") 'ein:connect-eval-region)
(define-key 'jovian-map (kbd "C-b") 'ein:connect-eval-buffer)
(define-key 'jovian-map (kbd "C-h") 'jovian/send-help)
(define-key 'jovian-map (kbd "C-u") 'jovian/send-edit)
(define-key 'jovian-map (kbd "C-d") 'jovian/send-dir)
(define-key 'jovian-map (kbd "C-g") 'jovian/send-shape)
(define-key 'jovian-map (kbd "C-n") 'jovian/jump-to-next-square)
(define-key 'jovian-map (kbd "C-p") 'jovian/jump-to-prev-square)
(define-key 'jovian-map (kbd "C-w") 'sensual/wrap)
(define-key 'jovian-map (kbd "C-p") 'sensual/print-wrap)
(define-key 'jovian-map (kbd "C-l") '(lambda () (interactive) (insert "λ")))
(define-key 'jovian-map (kbd "C-o") '(lambda () (interactive) (insert "ö")))

(global-set-key (kbd "C-f") 'jovian-map)
(define-key evil-normal-state-map (kbd "C-f") 'jovian-map)
(define-key evil-insert-state-map (kbd "C-f") 'jovian-map)
(define-key evil-visual-state-map (kbd "C-f") 'jovian-map)

(defun fib (n)
  (fib-s 1 0 0 1 n))

(defun fib-s (a b p q count)
  (if (= count 0)
      b
    (if (= (mod count 2) 0)
        (fib-s a
               b
               (+ (* p p) (* q q))
               (+ (* 2 p q) (* q q))
               (/ count 2))
      (fib-s (+ (* b q) (* a q) (* a p))
             (+ (* b p) (* a q))
             p
             q
             (- count 1)))))

(message "%d" (fib 100))
(setq racer-rust-src-path "~/.cargo/bin")

(org-agenda-list)
(find-file "~/leaf/papers.org")

(bind-key* "C-1" (lambda () (interactive) (bookmark-jump "1")))
(bind-key* "C-2" (lambda () (interactive) (bookmark-jump "2")))
(bind-key* "C-3" (lambda () (interactive) (bookmark-jump "3")))
(bind-key* "C-4" (lambda () (interactive) (bookmark-jump "4")))
(bind-key* "C-5" (lambda () (interactive) (bookmark-jump "5")))
(bind-key* "C-6" (lambda () (interactive) (bookmark-jump "6")))
(bind-key* "C-7" (lambda () (interactive) (bookmark-jump "7")))
(bind-key* "C-8" (lambda () (interactive) (bookmark-jump "8")))
(bind-key* "C-9" (lambda () (interactive) (bookmark-jump "9")))
(bind-key* "C-0" (lambda () (interactive) (bookmark-jump "0")))
(bind-key* "<f1>" (lambda () (interactive) (bookmark-jump "11")))
(bind-key* "<f2>" (lambda () (interactive) (bookmark-jump "12")))
(bind-key* "<f3>" (lambda () (interactive) (bookmark-jump "13")))
(bind-key* "<f4>" (lambda () (interactive) (bookmark-jump "14")))
(bind-key* "<f5>" (lambda () (interactive) (bookmark-jump "15")))
(bind-key* "<f6>" (lambda () (interactive) (bookmark-jump "16")))
(bind-key* "<f7>" (lambda () (interactive) (bookmark-jump "17")))
(bind-key* "<f8>" (lambda () (interactive) (bookmark-jump "18")))
(bind-key* "<f9>" (lambda () (interactive) (bookmark-jump "19")))
(bind-key* "<f10>" (lambda () (interactive) (bookmark-jump "20")))
(bind-key* "<f11>" (lambda () (interactive) (bookmark-jump "21")))
(bind-key* "<f12>" (lambda () (interactive) (bookmark-jump "22")))
;; ■ ~


(browse-url "localhost")

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))

(add-hook 'prog-mode-hook 'copilot-mode)
;; (defvar global-keys-minor-mode-map (make-sparse-keymap)
;;   "global-keys-minor-mode keymap.")

;; (define-key global-keys-minor-mode-map "C-1" (lambda () (interactive) (bookmark-jump "1")))

;; (define-minor-mode global-keys-minor-mode
;;   "A minor mode so that global key settings override annoying major modes."
;;   t "global-keys" 'global-keys-minor-mode-map)

;; (global-keys-minor-mode 1)


;; (defvar global-minor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-6") (lambda () (interactive) (bookmark-jump "6")))
;;     map)
;;   "my-keys-minor-mode keymap.")

;; (define-minor-mode global-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   :init-value t
;;   :lighter " my-keys")

;; (global-minor-mode 1)

;; (defconst global-minor-mode-alist (list (cons 'global-keys-minor-mode
;;                                               global-keys-minor-mode-map)))

;; (setf emulation-mode-map-alists '(global-minor-alist))
