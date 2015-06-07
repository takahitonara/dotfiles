;; ====================================
;; Starup Page
;; ====================================
;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; ====================================
;; Key bindings
;; ====================================
;; Set C-h delete key
(global-set-key "\C-h" 'delete-backward-char)





;; ====================================
;; Setting GUI
;; ====================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(global-linum-mode t))



;; ====================================
;; Backup file setting
;; ====================================
;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
;;(setq auto-save-default nil)


;; ====================================
;; Change Command key to Meta key
;; ====================================
(setq ns-command-modifier (quote meta))


;; ====================================
;; Translucent
;; ====================================
(if window-system 
    (progn
      (set-frame-parameter nil 'alpha 95)))



;; ====================================
;; Use El-Get
;; ====================================
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))



;; ====================================
;; powerline
;; ====================================
(el-get-bundle powerline)
(require 'powerline)
(powerline-default-theme)

;; ====================================
;; color theme
;; ====================================
(el-get-bundle color-theme-sanityinc-solarized)
(require 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-dark t)



;; ====================================
;; howm
;; ====================================
;; リンクを TAB で辿る
(el-get-bundle howm)

(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(mapc
  (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-grep howm-create
             howm-keyword-to-kill-ring))

;; リンクを TAB で辿る
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
     (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))
;; 「最近のメモ」一覧時にタイトル表示
(setq howm-list-recent-title t)
;; 全メモ一覧時にタイトル表示
(setq howm-list-all-title t)
;; メニューを 2 時間キャッシュ
(setq howm-menu-expiry-hours 2)

;; howm の時は auto-fill で
(add-hook 'howm-mode-on-hook 'auto-fill-mode)

;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; メニューの予定表の表示範囲
;; 10 日前から
(setq howm-menu-schedule-days-before 10)
;; 3 日後まで
(setq howm-menu-schedule-days 3)

;; howm のファイル名
;; 以下のスタイルのうちどれかを選んでください
;; で，不要な行は削除してください
;; 1 メモ 1 ファイル (デフォルト)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
;; 1 日 1 ファイルであれば
(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

(setq howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
;; 検索しないファイルの正規表現
(setq
 howm-excluded-file-regexp
 "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; いちいち消すのも面倒なので
;; 内容が 0 ならファイルごと削除する
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-cC-c で保存してバッファをキルする
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server start for emacs-client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable tool bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sr-speedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle sr-speedbar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle helm)

(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; migemo
;; http://qiita.com/catatsuy/items/c5fa34ead92d496b8a51
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle migemo)

(require 'migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle markdown-mode)
(require 'markdown-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle open-junk-file)
(require 'open-junk-file)
(setq open-junk-file-format "~/Documents/junk/%Y-%m%d-%H%M%S.")
(global-set-key "\C-xj" 'open-junk-file)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-install
;;
;; http://blog.wameiz.com/blog/2014/11/27/emacs-start-org/
;; http://orgmode.org/ja/index.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
;;(setq org-startup-truncated nil)
;;(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory "~/Documents/org/")
(setq org-default-notes-file (concat org-directory "agenda.org"))

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/Documents/org/tasks.org" "Tasks")
         "** TODO %?\n    %i\n    %a\n\n    %U")
        ("m" "Memo" entry
         (file+headline "~/Documents/org/memos.org" "Memos")
         "** %?\n   %i\n   %a\n\n   %U")
        ("d" "Dialy" entry
         (file+headline "~/Documents/org/dialies.org" "Dialies")
         "** %?\n   %i\n\n   %U")))

(setq org-agenda-files (list org-directory)) ;agendaを使うため

;; ショートカットキー
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-co" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(el-get-bundle elixir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 空白文字を強制表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default show-trailing-whitespace t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カーソルのある行をハイライトする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 全角・タブを可視化
;; http://weboo-returns.com/blog/emacs-shows-double-space-and-tab/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 折り返し切り替え
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; スクロールを一行ずつにする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-step 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
