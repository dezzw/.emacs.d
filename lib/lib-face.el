;;; lib-face.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun set-face-like-default (face)
  "Set FACE attributes to match the default face."
  (set-face-attribute face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height)
                      :weight (face-attribute 'default :weight)
                      :slant (face-attribute 'default :slant)))

(defun +setup-fonts ()
  "Setup fonts."
  ;; Setting the default
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font *default-font* :weight 'normal)

    (set-face-attribute 'italic nil
			:font "MonaspiceRn Nerd Font Mono"
			:slant 'italic)

    (set-face-attribute 'shadow nil
			:font "MonaspiceKr Nerd Font Mono")

    (set-face-attribute 'font-lock-comment-face nil :inherit 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :inherit 'italic)
    (set-face-attribute 'font-lock-variable-name-face nil :weight 'extra-bold)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'extra-bold)


    (set-face-like-default 'fixed-pitch-serif)
    (set-face-like-default 'variable-pitch)

    ;; 特殊字符需要安装 Symbola 字体
    ;; https://www.wfonts.com/font/symbola
    ;; "Emacs 28 now has 'emoji . before, emoji is part of 'symbol"
    ;; 根据上面这句话应该写成 'emoji 就可以了，但是由于 Emoji 本身
    ;; 分布比较散，所以还是先设置 'unicode 后再设置 CJK 比较靠谱。
    ;; 特例：'emoji 就会导致 ⛈️ fallback 到 ⛈
    ;; https://emacs-china.org/t/emacs/15676/34
    (cl-loop for font in *emoji-fonts*
             when (find-font (font-spec :name font))
             return (set-fontset-font
                     t
                     'unicode
                     (font-spec :family font
				:size
				(cond ((eq system-type 'darwin) 12)
                                      ((eq system-type 'gnu/linux) 12)
                                      ((eq system-type 'windows-nt) 12)))
                     nil 'prepend))
    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
			(font-spec :family *zh-default-font*)))
    ;; Setting fall-back fonts
    ;; https://idiocy.org/emacs-fonts-and-fontsets.html
    (dolist (font *fallback-fonts*)
      (when (member font (font-family-list))
	(set-fontset-font "fontset-default" 'han font nil 'append)))
    ;; Force Emacs to search by using font-spec
    (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
    ;; 不知道为什么会有一些字符不被覆盖，目前只能这么写了。 2025-04-07
    ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
    (let ((ranges '((#xE5FA . #xE6B7)   ;; Seti-UI + Custom
                    (#xE700 . #xE8EF)   ;; Devicons
                    (#xED00 . #xF2FF)   ;; Font Awesome
                    (#xE200 . #xE2A9)   ;; Font Awesome Extension
                    (#xF0001 . #xF1AF0) ;; Material Design Icons
                    (#xE300 . #xE3E3)   ;; Weather
                    (#xF400 . #xF533)   ;; Octicons
                    (#x2665 . #x2665)   ;; Octicons
                    (#x26A1 . #x26A1)   ;; Octicons
                    (#xE000 . #xE00A)   ;; Pomicons
                    (#xEA60 . #xEC1E))));; Codicons
      (dolist (range ranges)
	(set-fontset-font t range *symbol-default-font*))))
  )

;; https://github.com/mickeynp/ligature.el/issues/8
(defconst ligatures-alist
  '("<--" "<---" "<<-" "<-" "<->" "->" "->>" "-->" "--->"
    "<!--" "-<<" "-<" "-<-" "->-" ">-" ">>-" "<-->" "<--->"
    "<---->" "<==" "<===" "<<=" "<=" "<=>" "=>" "=>>" "==>"
    "===>" "<!---" "=<<" "=<" "=<=" "=>=" ">=" ">>=" "<==>"
    "<===>" "<====>" "<-------" "------->" "<======>" "<~~"
    "<~" "~>" "~~>" "\\/" "/\\" "==" "!=" "/=" "~=" "<>"
    "===" "!==" "=/=" "=!=" ":=" ":-" ":+" "<*" "<*>" "*>"
    "<|" "<|>" "|>" "+:" "-:" "=:" "::" ":::" "<." "<.>"
    ".>" "(*" "*)" ":>" "++" "+++" "|-" "-|"))

(sort ligatures-alist (lambda (x y) (> (length x) (length y))))

(dolist (pat ligatures-alist)
  (set-char-table-range composition-function-table
                        (aref pat 0)
                        (nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
                                             'compose-gstring-for-graphic)))))

(defun +suggest-other-faces (func &rest args)
  "Temporarily disable `global-hl-line-mode' while executing FUNC with ARGS."
  (if global-hl-line-mode
      (progn
        (global-hl-line-mode -1)
        (prog1 (apply func args)
          (global-hl-line-mode 1)))
    (apply func args)))

;; 用于检查 unicode 是否被字体覆盖
(defun check-symbols-nerd-font-mono-coverage (unicode)
  "Check if 'Symbols Nerd Font Mono' covers the specified UNICODE character."
  (interactive "sEnter Unicode (e.g., 0F11E7): ")
  (let ((font-family "Symbols Nerd Font Mono")
        (char (string-to-number unicode 16))
        (buffer (get-buffer-create "*Font Check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Checking coverage for Unicode %s in font: %s\n\n" unicode font-family))
      (insert (propertize (string char)
                          'face `(:family ,font-family :height 200)))
      (display-buffer buffer))))

(provide 'lib-face)
;;; lib-face.el ends here
