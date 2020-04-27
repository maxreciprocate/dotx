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

(require 'server)
(when dotspacemacs-server-socket-dir
  (setq server-socket-dir dotspacemacs-server-socket-dir))
(unless (server-running-p)
  (message "Starting a server...")
  (server-start))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-files
      (append (directory-files-recursively "~/iros/" "\.org$")
              (directory-files-recursively "~/leaf/" "\.org$")))

(setq org-agenda-span 'day)
(setq org-agenda-block-separator "")

;; ■ ORG
(setq org-modules (quote (org-protocol org-drill)))
(require 'org-protocol)
(require 'org-drill)
(setq org-drill-maximum-duration 30)
(setq org-drill-save-buffers-after-drill-sessions-p nil)
(setq org-drill-scope org-agenda-files)
(setq org-drill-question-tag "@")

(setq warning-minimum-level :emergency
      browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-program "firefox-bin"
      browse-url-firefox-arguments '("--private-window")
      shell-command-default-error-buffer "*Messages*"
      large-file-warning-threshold nil
      org-clock-persist 'history)

;; ssh-agent for magit
(keychain-refresh-environment)
(org-clock-persistence-insinuate)

(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'yas-reload-all)
(add-hook 'org-mode-hook 'visual-line-mode)

;; nevertheless this mode is wack
(add-hook 'auto-mode-alist '("\\.log" . (lambda () (auto-revert-tail-mode))))
(setq confirm-kill-processes nil)

(setq ispell-program-name "aspell")

(add-hook 'org-mode-hook (lambda ()
			               (push '("[ ]" .  "☐") prettify-symbols-alist)
			               (push '("[X]" . "☑" ) prettify-symbols-alist)
			               (push '("[-]" . "❍" ) prettify-symbols-alist)
			               (prettify-symbols-mode)))

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

(org-clock-persistence-insinuate)
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
(global-set-key (kbd "C-q") 'yas-expand)

(defun core/select-line () (interactive)
       (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun core/odious-line () (interactive)
       (insert "sin²(θ) is odious to me"))

(defun core/pwd ()
  (s-join "/" (-butlast (s-split "/" buffer-file-name))))

(defun arrayify (start end quote)
 (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;;; Plainly opens zathura on a filename
;; (if (equal major-mode 'dired-mode)
;; (dired-do-shell-command "zathura")))
(defun core/zathura-read (&optional filename) (interactive)
       (unless filename
         (setq filename (progn (string-match "\\(~\\)\\(\.*\\)+\\.\\(pdf\\|epub\\|djvu\\)" (core/select-line))
                               (match-string-no-properties 0 (core/select-line)))))
       (start-process-shell-command "reading-time" "*Messages*" (format "zathura %s" filename)))

;;; Executed on org-entry will clock-in and either open zathura-reader or open file folder
;;; TODO Links
(defun core/entry-action () (interactive)
       (let* ((path (->> (core/select-line) (s-split " ") third))
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
  (kbd "i o") 'core/odious-line
  (kbd "o c") 'org-clock-goto
  (kbd "y d") 'org-drill
  (kbd "y c") 'org-drill-resume
  (kbd "o d") (lambda () (interactive) (find-file "~/leaf/diota.org"))
  (kbd "o e") (lambda () (interactive) (find-file "~/leaf/every.org"))
  (kbd "o z") (lambda () (interactive) (find-file "~/leaf/metaz.org"))

  (kbd "o u") (lambda () (interactive) (find-file "~/iros/muses/muses.org"))
  (kbd "o m") (lambda () (interactive) (find-file "~/iros/motif/motif.org"))
  (kbd "o l") (lambda () (interactive) (find-file "~/iros/liederkreis/liederkreis.org"))
  (kbd "o s") (lambda () (interactive) (find-file "~/iros/space/space.org"))
  (kbd "o a") (lambda () (interactive) (find-file "~/iros/archs/archs.org"))
  (kbd "o t") (lambda () (interactive) (find-file "~/iros/expan/expan.org"))
  (kbd "o r") (lambda () (interactive) (find-file "~/iros/brows/brows.org"))
  (kbd "o b") (lambda () (interactive) (find-file "~/iros/bases/bases.org"))
  (kbd "o p") (lambda () (interactive) (find-file "~/leaf/papers.org"))
  (kbd "o x") (lambda () (interactive) (find-file "~/leaf/xpov.org"))

  (kbd "f e i") (lambda () (interactive) (find-file "~/dotx/init.el"))
  (kbd "f e d") (lambda () (interactive) (find-file "~/dotx/.spacemacs")))

;; ■ TEX
(require 'ox-latex)
(setq org-latex-pdf-process
      '("xelatex -shell-escape -8bit -interaction nonstopmode %f"
        "xelatex -shell-escape -8bit -interaction nonstopmode %f"))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("fontsize" "\\footnotesize") ("obeytabs" "true")))
(setq org-src-preserve-indentation t)

(require 'olivetti)
(setq olivetti-body-width 0.8)
(add-hook 'tex-mode-hook 'flyspell-mode)

(setq ispell-dictionary "british"
      ispell-extra-args '() ;; TeX mode "-t"
      ispell-silently-savep t)

;; ■ ELISP
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; ■ Netscape
(setq js-indent-level 2)
(setq js2-strict-inconsistent-return-warning nil)

;; ■ C?
(setq c-basic-offset 2
      tab-width 2
      indent-tabs-mode nil)

(setq-default tab-width 2)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(require 'ccls)
(setq ccls-executable "~/pacs/ccls/Release/ccls")
(setq c-c++-backend 'lsp-ccls)
(add-hook 'c++-mode #'lsp)
(add-hook 'c-mode #'lsp)

;; ■ SNAKE
(setq python-guess-indent t
      python-indent-offset 2)

(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))

;; ■ JULIE
;; (setq lsp-julia-package-dir nil)
;; (require 'lsp-julia)
(add-hook 'julia-mode-hook (lambda () (auto-complete-mode -1)))
(add-hook 'julia-mode-hook (lambda () (yas-global-mode)))
;; (add-hook 'julia-mode-hook (lambda () (set-input-method "TeX")))

;; ■ JOVIAN
(setq jupyter-repl-echo-eval-p nil)
(setq ein:jupyter-default-server-command "~/.local/bin/jupyter")

(defconst square-sign "■")

(defun jovian/insert-stop-sign () (interactive)
       (evil-insert-newline-above)
       (insert square-sign)
       (comment-line 1))

;;; TODO gg G
(defun jovian/jump-to-next-square () (interactive)
       (search-forward square-sign nil t)
       (forward-line)
       (evil-digit-argument-or-evil-beginning-of-line))

(defun jovian/jump-to-prev-square () (interactive)
       (forward-line -2)
       (search-backward square-sign nil t)
       (forward-line)
       (evil-digit-argument-or-evil-beginning-of-line))

(font-lock-add-keywords 'org-mode '(("■.*" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'emacs-lisp-mode '(("■.*" 0 font-lock-warning-face prepend)))
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
             ;; (jovian/send (format "%s;%s" line (match-string-no-properties 1 line)))
             (jovian/send (format "%s" line))
           (jovian/send line))))

; ANY
(defun jovian/send-block () (interactive)
       (let ((start (save-excursion
                      (if (search-backward square-sign nil t)
                          (line-end-position)
                        (point-min))))
             (end  (save-excursion
                     (if (search-forward square-sign nil t)
                         (line-beginning-position)
                       (point-max)))))
         (jovian/send (buffer-substring-no-properties start end))))

; ANY
(defun jovian/send-edit () (interactive)
       ;;; TODO check major mode
       (jovian/send (format "@edit %s" (core/select-line))))

;; (defun jovian/send-edit () (interactive)
;;        (jovian/send (format "@edit %s" (core/select-line))))

(defun jovian/send-help () (interactive)
       ;; (let ((command (if (eq major-mode 'julia-mode) ))))
       (if (eq major-mode 'julia-mode)
           (jovian/send (format "@doc %s" (core/select-line)))
         (jovian/send (format "help(%s)" (core/select-line)))))

(defun jovian/send-dir () (interactive)
       (jovian/send (format "dir(%s)" (core/select-line))))

;; (defun jovian/send-help () (interactive)
;;        (jovian/send (format "?%s" (core/select-line))))

(defun jovian/current-notebook-buffer () (interactive)
       (--last (string-suffix-p ".ipynb*" it) (helm-buffer-list)))

(defun jovian/nuke () (interactive)
       (->> (helm-buffer-list)
            (--filter (string-prefix-p "*ein:" it))
            (--map (kill-buffer it))))

(defun jovian/interrupt-kernel () (interactive)
       (with-current-buffer (jovian/current-notebook-buffer)
         (ein:notebook-kernel-interrupt-command)))

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
         ("julia-mode" "julia-1.3")
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
(defun jovian/ride () (interactive)
       (let ((path (core/pwd)))
         (ein:jupyter-server-start ein:jupyter-default-server-command path)
         (shell-command "rm *.ipynb")
         (sit-for 1)
         (ein:notebooklist-new-notebook
          "http://127.0.0.1:9144" (jovian/mode->kernel) 'jovian/new-notebook-steps t nil)))

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "sc" 'ein:connect-to-notebook-buffer
  "se" 'jovian/pop-output
  "so" 'jovian/start-notebook
  "sn" 'jovian/nuke
  "sk" 'jovian/interrupt-kernel
  "sr" 'jovian/restart-kernel
  "sj" 'jovian/ride)

(spacemacs/set-leader-keys-for-major-mode 'julia-mode
  "sc" 'ein:connect-to-notebook-buffer
  "sy" 'jovina/pop-output
  "so" 'jovian/start-notebook
  "sn" 'jovian/nuke
  "sk" 'jovian/interrupt-kernel
  "sr" 'jovian/restart-kernel
  "sy" 'jovian/pop-output
  "sj" 'jovian/ride)


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
(define-key 'jovian-map (kbd "C-n") 'jovian/jump-to-next-square)
(define-key 'jovian-map (kbd "C-p") 'jovian/jump-to-prev-square)

(global-set-key (kbd "C-e") 'jovian-map)
(define-key evil-normal-state-map (kbd "C-e") 'jovian-map)
(define-key evil-insert-state-map (kbd "C-e") 'jovian-map)
(define-key evil-visual-state-map (kbd "C-e") 'jovian-map)

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

(find-file "~/iros/motif/jam/jam.cu")

(org-agenda-list)
