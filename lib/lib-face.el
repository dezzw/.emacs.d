;;; lib-face.el --- font and face setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defun set-face-like-default (face)
  "Set FACE attributes to match the default face."
  (set-face-attribute face nil
                      :family (face-attribute 'default :family)
                      :height (face-attribute 'default :height)
                      :weight (face-attribute 'default :weight)
                      :slant (face-attribute 'default :slant)))

(defun +setup-fonts ()
  "Apply default, CJK, emoji, and Nerd Font fallbacks on graphic frames."
  (interactive)
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font *default-font* :weight 'normal :height 110)
    (set-face-attribute 'font-lock-comment-face nil :inherit 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :inherit 'italic)
    (set-face-attribute 'font-lock-variable-name-face nil :weight 'extra-bold)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'extra-bold)
    (set-face-like-default 'fixed-pitch-serif)
    (set-face-like-default 'variable-pitch)
    (cl-loop for font in *emoji-fonts*
             when (find-font (font-spec :name font))
             return (set-fontset-font t 'unicode
                                      (font-spec :family font :size 12)
                                      nil 'prepend))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family *zh-default-font*)))
    (dolist (font *fallback-fonts*)
      (when (member font (font-family-list))
        (set-fontset-font "fontset-default" 'han font nil 'append)))
    (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
    (let ((ranges '((#xE5FA . #xE6B7)
                    (#xE700 . #xE8EF)
                    (#xED00 . #xF2FF)
                    (#xE200 . #xE2A9)
                    (#xF0001 . #xF1AF0)
                    (#xE300 . #xE3E3)
                    (#xF400 . #xF533)
                    (#x2665 . #x2665)
                    (#x26A1 . #x26A1)
                    (#xE000 . #xE00A)
                    (#xEA60 . #xEC1E))))
      (dolist (range ranges)
        (set-fontset-font t range *symbol-default-font*)))))

;; Ligature composition; longest patterns first.
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

(defun check-symbols-nerd-font-mono-coverage (unicode)
  "Check whether *symbol-default-font* covers UNICODE (hex string, e.g. 0F11E7)."
  (interactive "sEnter Unicode (e.g., 0F11E7): ")
  (let ((font-family *symbol-default-font*)
        (char (string-to-number unicode 16))
        (buffer (get-buffer-create "*Font Check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Checking coverage for Unicode %s in font: %s\n\n"
                      unicode font-family))
      (insert (propertize (string char)
                          'face (list :family font-family :height 200))))
    (display-buffer buffer)))

(provide 'lib-face)
;;; lib-face.el ends here
