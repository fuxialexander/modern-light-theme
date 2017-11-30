;;; modern-common.el --- Color theme with a dark and light versions.

;; Copyright (C) 2015-2016 Nasser Alshammari

;; Author: Nasser Alshammari
;; URL: <https://github.com/nashamri/modern-theme>
;;
;; Version: 0.1

;;; Code:

(defmacro dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup modern-theme nil
  "Modern-theme options."
  :group 'faces)

(defcustom modern-theme-comment-bg nil
  "Use a background for comment lines."
  :type 'boolean
  :group 'modern-theme)

(defcustom modern-theme-comment-italic nil
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'modern-theme)

(defcustom modern-theme-org-agenda-height nil
  "Use varying text heights for org agenda."
  :type 'boolean
  :group 'modern-theme)

(defcustom modern-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'modern-theme)

(defcustom modern-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'modern-theme)

(defcustom modern-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'modern-theme)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun custom-colors-override ()
  (mapcar (lambda (x) (list (car x) (cdr x)))
          modern-theme-custom-colors))

(defun create-modern-theme (variant theme-name)
  (dyn-let ((class '((class color) (min-colors 89))) ;;                   ~~ Dark ~~                              ~~ Light ~~
            ;;                                                          GUI       TER                           GUI       TER
            ;; generic
            (act1          (pcase variant ('dawn (if (true-color-p) "#18303F" "#121212")) ('dark (if (true-color-p) "#333332" "#121212")) ('light (if (true-color-p) "#FBFBFB" "#d7dfff"))))
            (act2          (pcase variant ('dawn (if (true-color-p) "#18303F" "#444444")) ('dark (if (true-color-p) "#333332" "#444444")) ('light (if (true-color-p) "#EAEAEB" "#afafd7"))))
            (base          (pcase variant ('dawn (if (true-color-p) "#C9BAAC" "#b2b2b2")) ('dark (if (true-color-p) "#ECEBE7" "#b2b2b2")) ('light (if (true-color-p) "#333332" "#5f5f87"))))
            (base-b        (pcase variant ('dawn (if (true-color-p) "#DDD1C6" "#b2b2b2")) ('dark (if (true-color-p) "#ECEBE7" "#b2b2b2")) ('light (if (true-color-p) "#333332" "#5f5f87"))))
            (base-dim      (pcase variant ('dawn (if (true-color-p) "#AAA198" "#585858")) ('dark (if (true-color-p) "#9BA3A6" "#585858")) ('light (if (true-color-p) "#60605F" "#afafd7"))))
            (bg1           (pcase variant ('dawn (if (true-color-p) "#101F29" "#262626")) ('dark (if (true-color-p) "#000000" "#262626")) ('light (if (true-color-p) "#ffffff" "#ffffff"))))
            (bg2           (pcase variant ('dawn (if (true-color-p) "#132632" "#1c1c1c")) ('dark (if (true-color-p) "#222222" "#1c1c1c")) ('light (if (true-color-p) "#F2F1EF" "#e4e4e4"))))
            (bg3           (pcase variant ('dawn (if (true-color-p) "#18303F" "#121212")) ('dark (if (true-color-p) "#333333" "#121212")) ('light (if (true-color-p) "#E3E3E3" "#d0d0d0"))))
            (bg4           (pcase variant ('dawn (if (true-color-p) "#214054" "#080808")) ('dark (if (true-color-p) "#444444" "#080808")) ('light (if (true-color-p) "#D4D4D4" "#bcbcbc"))))
            (border        (pcase variant ('dawn (if (true-color-p) "#18303F" "#111111")) ('dark (if (true-color-p) "#202933" "#111111")) ('light (if (true-color-p) "#73B3FF" "#b3b9be"))))

            (cblk          (pcase variant ('dawn (if (true-color-p) "#AAA198" "#b2b2b2")) ('dark (if (true-color-p) "#ECEBE7" "#b2b2b2")) ('light (if (true-color-p) "#332A2A" "#5f5f87"))))
            (cblk-bg       (pcase variant ('dawn (if (true-color-p) "#18303F" "#262626")) ('dark (if (true-color-p) "#161C25" "#262626")) ('light (if (true-color-p) "#FFEEEE" "#ffffff"))))
            (cblk-ln       (pcase variant ('dawn (if (true-color-p) "#519CCD" "#af5faf")) ('dark (if (true-color-p) "#9BA3A6" "#af5faf")) ('light (if (true-color-p) "#60605F" "#af5fdf"))))
            (cblk-ln-bg    (pcase variant ('dawn (if (true-color-p) "#214054" "#333333")) ('dark (if (true-color-p) "#283542" "#333333")) ('light (if (true-color-p) "#F2D3CF" "#dfdfff"))))
            (cursor        (pcase variant ('dawn (if (true-color-p) "#648DFF" "#d0d0d0")) ('dark (if (true-color-p) "#648DFF" "#d0d0d0")) ('light (if (true-color-p) "#648DFF" "#121212"))))
            (const         (pcase variant ('dawn (if (true-color-p) "#8CB6E1" "#d75fd7")) ('dark (if (true-color-p) "#8CB6E1" "#d75fd7")) ('light (if (true-color-p) "#9E6FB8" "#8700af"))))
            (comment       (pcase variant ('dawn (if (true-color-p) "#519CCD" "#008787")) ('dark (if (true-color-p) "#9BA3A6" "#008787")) ('light (if (true-color-p) "#a49da5" "#008787"))))
            (comment-light (pcase variant ('dawn (if (true-color-p) "#519CCD" "#008787")) ('dark (if (true-color-p) "#9BA3A6" "#008787")) ('light (if (true-color-p) "#a49da5" "#008787"))))
            (comment-bg    (pcase variant ('dawn (if (true-color-p) "#18303F" "#262626")) ('dark (if (true-color-p) "#2F2723" "#262626")) ('light (if (true-color-p) "#F2EDED" "#ffffff"))))
            (comp          (pcase variant ('dawn (if (true-color-p) "#D87184" "#d75fd7")) ('dark (if (true-color-p) "#8CB6E1" "#d75fd7")) ('light (if (true-color-p) "#49A3CB" "#8700af"))))
            (err           (pcase variant ('dawn (if (true-color-p) "#E46261" "#e0211d")) ('dark (if (true-color-p) "#FF7974" "#e0211d")) ('light (if (true-color-p) "#FF493F" "#e0211d"))))
            (func          (pcase variant ('dawn (if (true-color-p) "#648DFF" "#d75fd7")) ('dark (if (true-color-p) "#CFD838" "#d75fd7")) ('light (if (true-color-p) "#53B9E5" "#8700af"))))

            (highlight     (pcase variant ('dawn (if (true-color-p) "#2E2949" "#444444")) ('dark (if (true-color-p) "#42576B" "#444444")) ('light (if (true-color-p) "#D7E9FF" "#d7d7ff"))))
            (highlight-dim (pcase variant ('dawn (if (true-color-p) "#201D33" "#444444")) ('dark (if (true-color-p) "#3F5266" "#444444")) ('light (if (true-color-p) "#AFD4FF" "#d7d7ff"))))
            (keyword       (pcase variant ('dawn (if (true-color-p) "#648DFF" "#268bd2")) ('dark (if (true-color-p) "#53B9E5" "#268bd2")) ('light (if (true-color-p) "#5585BD" "#268bd2"))))
            (lnum          (pcase variant ('dawn (if (true-color-p) "#44505c" "#444444")) ('dark (if (true-color-p) "#44505c" "#444444")) ('light (if (true-color-p) "#a8a8bf" "#af87af"))))
            (str           (pcase variant ('dawn (if (true-color-p) "#64C2FF" "#2aa198")) ('dark (if (true-color-p) "#428EE9" "#2aa198")) ('light (if (true-color-p) "#0019D5" "#2aa198"))))
            (suc           (pcase variant ('dawn (if (true-color-p) "#82D88F" "#86dc2f")) ('dark (if (true-color-p) "#D9F267" "#86dc2f")) ('light (if (true-color-p) "#768F00" "#00af00"))))
            (ttip          (pcase variant ('dawn (if (true-color-p) "#DDD1C6" "#888888")) ('dark (if (true-color-p) "#77A2C6" "#888888")) ('light (if (true-color-p) "#357797" "#5f5f87"))))
            (ttip-sl       (pcase variant ('dawn (if (true-color-p) "#2C5672" "#333333")) ('dark (if (true-color-p) "#5E7998" "#333333")) ('light (if (true-color-p) "#73B3FF" "#afafff"))))
            (ttip-bg       (pcase variant ('dawn (if (true-color-p) "#214054" "#444444")) ('dark (if (true-color-p) "#202933" "#444444")) ('light (if (true-color-p) "#D7E9FF" "#dfdfff"))))
            (type          (pcase variant ('dawn (if (true-color-p) "#9C75ED" "#df005f")) ('dark (if (true-color-p) "#FF7974" "#df005f")) ('light (if (true-color-p) "#C73930" "#af005f"))))
            (var           (pcase variant ('dawn (if (true-color-p) "#FFED6A" "#8787d7")) ('dark (if (true-color-p) "#7EA5CC" "#8787d7")) ('light (if (true-color-p) "#9367AB" "#af5fd7"))))
            (war           (pcase variant ('dawn (if (true-color-p) "#DC9E66" "#dc752f")) ('dark (if (true-color-p) "#ED9E56" "#dc752f")) ('light (if (true-color-p) "#B66900" "#dc752f"))))

            ;; colors
            (aqua          (pcase variant ('dawn (if (true-color-p) "#DC9E66" "#2aa198")) ('dark (if (true-color-p) "#80CABF" "#2aa198")) ('light (if (true-color-p) "#3F8E95" "#2aa198"))))
            (aqua-l        (pcase variant ('dawn (if (true-color-p) "#C79C77" "#2aa198")) ('dark (if (true-color-p) "#80CABF" "#2aa198")) ('light (if (true-color-p) "#548E95" "#2aa198"))))
            (aqua-d        (pcase variant ('dawn (if (true-color-p) "#F1AD4D" "#2aa198")) ('dark (if (true-color-p) "#80CABF" "#2aa198")) ('light (if (true-color-p) "#1F8A95" "#2aa198"))))
            (aqua-bg       (pcase variant ('dawn (if (true-color-p) "#4E3824" "#262626")) ('dark (if (true-color-p) "#2F3C4C" "#262626")) ('light (if (true-color-p) "#D5FCFF" "#ffffff"))))
            (green         (pcase variant ('dawn (if (true-color-p) "#82D88F" "#67b11d")) ('dark (if (true-color-p) "#C8D85C" "#67b11d")) ('light (if (true-color-p) "#768F00" "#5faf00"))))
            (green-l       (pcase variant ('dawn (if (true-color-p) "#7FB087" "#67b11d")) ('dark (if (true-color-p) "#CDD885" "#67b11d")) ('light (if (true-color-p) "#7C8F2A" "#5faf00"))))
            (green-d       (pcase variant ('dawn (if (true-color-p) "#99DA63" "#67b11d")) ('dark (if (true-color-p) "#C1D822" "#67b11d")) ('light (if (true-color-p) "#768F00" "#5faf00"))))
            (green-bg      (pcase variant ('dawn (if (true-color-p) "#326046" "#262626")) ('dark (if (true-color-p) "#326046" "#262626")) ('light (if (true-color-p) "#F2FFB6" "#ffffff"))))
            (green-bg-s    (pcase variant ('dawn (if (true-color-p) "#224630" "#262626")) ('dark (if (true-color-p) "#224630" "#262626")) ('light (if (true-color-p) "#ECFF92" "#ffffff"))))
            (red           (pcase variant ('dawn (if (true-color-p) "#D87184" "#d70000")) ('dark (if (true-color-p) "#DC6661" "#d70000")) ('light (if (true-color-p) "#D44C44" "#d70008"))))
            (red-l         (pcase variant ('dawn (if (true-color-p) "#BB7B86" "#d70000")) ('dark (if (true-color-p) "#DC7976" "#d70000")) ('light (if (true-color-p) "#D46057" "#d70008"))))
            (red-d         (pcase variant ('dawn (if (true-color-p) "#E46261" "#d70000")) ('dark (if (true-color-p) "#FF8982" "#d70000")) ('light (if (true-color-p) "#D43D34" "#d70008"))))
            (red-bg        (pcase variant ('dawn (if (true-color-p) "#422328" "#262626")) ('dark (if (true-color-p) "#3F1D1C" "#262626")) ('light (if (true-color-p) "#FFE5E4" "#ffffff"))))
            (red-bg-s      (pcase variant ('dawn (if (true-color-p) "#572926" "#262626")) ('dark (if (true-color-p) "#572926" "#262626")) ('light (if (true-color-p) "#FFD2D0" "#ffffff"))))
            (blue          (pcase variant ('dawn (if (true-color-p) "#1E95CC" "#268bd2")) ('dark (if (true-color-p) "#1E95CC" "#268bd2")) ('light (if (true-color-p) "#648DFF" "#268bd2"))))
            (blue-l        (pcase variant ('dawn (if (true-color-p) "#519CCD" "#00ffff")) ('dark (if (true-color-p) "#8CB6E1" "#00ffff")) ('light (if (true-color-p) "#88A8FF" "#008080"))))
            (blue-d        (pcase variant ('dawn (if (true-color-p) "#648DFF" "#268bd2")) ('dark (if (true-color-p) "#0089CC" "#268bd2")) ('light (if (true-color-p) "#3065FF" "#268bd2"))))
            (blue-bg       (pcase variant ('dawn (if (true-color-p) "#214054" "#262626")) ('dark (if (true-color-p) "#16313F" "#262626")) ('light (if (true-color-p) "#BDD9FF" "#d7d7ff"))))
            (magenta       (pcase variant ('dawn (if (true-color-p) "#8778D8" "#af00df")) ('dark (if (true-color-p) "#C3A1D2" "#af00df")) ('light (if (true-color-p) "#9E6FB8" "#800080"))))
            (magenta-l     (pcase variant ('dawn (if (true-color-p) "#837EB9" "#af00df")) ('dark (if (true-color-p) "#C3A1D2" "#af00df")) ('light (if (true-color-p) "#AA93B8" "#800080"))))
            (magenta-d     (pcase variant ('dawn (if (true-color-p) "#9C75ED" "#af00df")) ('dark (if (true-color-p) "#C3A1D2" "#af00df")) ('light (if (true-color-p) "#8E47B8" "#800080"))))
            (magenta-bg    (pcase variant ('dawn (if (true-color-p) "#2E2949" "#af00df")) ('dark (if (true-color-p) "#3A2E41" "#af00df")) ('light (if (true-color-p) "#EED3FF" "#800080"))))
            (yellow        (pcase variant ('dawn (if (true-color-p) "#FFED6A" "#875f00")) ('dark (if (true-color-p) "#FFC65D" "#875f00")) ('light (if (true-color-p) "#D3B31B" "#875f00"))))
            (yellow-l      (pcase variant ('dawn (if (true-color-p) "#EADC77" "#875f00")) ('dark (if (true-color-p) "#FFD281" "#875f00")) ('light (if (true-color-p) "#D19D50" "#875f00"))))
            (yellow-d      (pcase variant ('dawn (if (true-color-p) "#FFDA53" "#875f00")) ('dark (if (true-color-p) "#FFB736" "#875f00")) ('light (if (true-color-p) "#DBA415" "#875f00"))))
            (yellow-bg     (pcase variant ('dawn (if (true-color-p) "#575124" "#262626")) ('dark (if (true-color-p) "#4B3610" "#262626")) ('light (if (true-color-p) "#FFEBB7" "#ffffff"))))
            )

           custom-colors-override

           (custom-theme-set-faces
            theme-name

            `(bold                                   ((,class (:foreground ,base-b :bold t))))
            `(variable-pitch                         ((,class (:height 1.2 :spacing 1.5 :family "Operator Ssm"))))

;;;;; Notmuch

            `(notmuch-crypto-decryption              ((,class (:background ,bg1     :foreground ,blue-l         ))))
            `(notmuch-crypto-part-header             ((,class (:background ,bg1     :foreground ,yellow-l       ))))
            `(notmuch-crypto-signature-bad           ((,class (:background ,bg1     :foreground ,red-l          ))))
            `(notmuch-crypto-signature-good          ((,class (:background ,bg1     :foreground ,base           ))))
            `(notmuch-crypto-signature-good-key      ((,class (:background ,bg1     :foreground ,aqua-l         ))))
            `(notmuch-crypto-signature-unknown       ((,class (:background ,bg1     :foreground ,yellow         ))))

            `(notmuch-hello-logo-background          ((,class (:background ,bg1     :foreground ,bg1            ))))
            `(notmuch-message-summary-face           ((,class (:background ,bg4     :foreground ,base           ))))

            `(notmuch-search-count                   ((,class (:background ,bg1     :foreground ,yellow-bg      ))))
            `(notmuch-search-date                    ((,class (:background ,bg1     :foreground ,aqua           ))))
            `(notmuch-search-flagged-face            ((,class (:background ,red-bg                              ))))
            `(notmuch-search-matching-authors        ((,class (:background ,bg1     :foreground ,blue-d         ))))
            `(notmuch-search-non-matching-authors    ((,class (:background ,bg1     :foreground ,blue           ))))
            `(notmuch-search-subject                 ((,class (:background ,bg1     :foreground ,base           ))))
            `(notmuch-search-unread-face             ((,class (:background ,bg1     :foreground ,base-b :bold t ))))

            `(notmuch-tag-added                      ((,class (:background ,bg1     :foreground ,green-d        ))))
            `(notmuch-tag-deleted                    ((,class (:background ,bg1     :foreground ,red-d          ))))
            `(notmuch-tag-face                       ((,class (:background ,bg1     :foreground ,blue-d         ))))
            `(notmuch-tag-flagged                    ((,class (:background ,red-bg  :foreground ,blue-d         ))))
            `(notmuch-tag-unread                     ((,class (:background ,bg1     :foreground ,blue-d         ))))

            `(notmuch-tree-match-author-face         ((,class (:background ,bg1     :foreground ,blue-d         ))))
            `(notmuch-tree-match-date-face           ((,class (:background ,bg1     :foreground ,aqua-d         ))))
            `(notmuch-tree-match-face                ((,class (:background ,bg1     :foreground ,base           ))))
            `(notmuch-tree-match-subject-face        ((,class (:background ,bg1     :foreground ,base           ))))
            `(notmuch-tree-match-tag-face            ((,class (:background ,bg1     :foreground ,blue-d         ))))
            `(notmuch-tree-match-tree-face           ((,class (:background ,bg1     :foreground ,yellow-d       ))))
            `(notmuch-tree-no-match-author-face      ((,class (:background ,bg1     :foreground ,blue-l         ))))
            `(notmuch-tree-no-match-date-face        ((,class (:background ,bg1     :foreground ,aqua-l         ))))
            `(notmuch-tree-no-match-face             ((,class (:background ,bg1     :foreground ,base-dim       ))))
            `(notmuch-tree-no-match-subject-face     ((,class (:background ,bg1     :foreground ,base-dim       ))))
            `(notmuch-tree-no-match-tag-face         ((,class (:background ,bg1     :foreground ,blue-l         ))))
            `(notmuch-tree-no-match-tree-face        ((,class (:background ,bg1     :foreground ,yellow-l       ))))

            `(notmuch-wash-cited-text                ((,class (:background ,bg1     :foreground ,blue           ))))
            `(notmuch-wash-toggle-button             ((,class (:background ,blue-bg :foreground ,blue           ))))




;;;;; Elfeed
            `(elfeed-log-date-face                   ((,class (:foreground ,aqua-l    ))))
            `(elfeed-log-debug-level-face            ((,class (:foreground ,yellow-l  ))))
            `(elfeed-log-error-level-face            ((,class (:foreground ,red-l     ))))
            `(elfeed-log-info-level-face             ((,class (:foreground ,base      ))))
            `(elfeed-log-warn-level-face             ((,class (:foreground ,aqua-l    ))))
            `(elfeed-search-date-face                ((,class (:foreground ,aqua-l  :family "Operator Mono" :height 1.0    ))))
            `(elfeed-search-feed-face                ((,class (:foreground ,yellow  :family "Operator Mono" :height 1.0    ))))
            `(elfeed-search-filter-face              ((,class (:foreground ,red-l   :family "Operator Mono" :height 1.0    ))))
            `(elfeed-search-last-update-face         ((,class (:foreground ,magenta-l  :family "Operator Mono" :height 1.0 ))))
            `(elfeed-search-tag-face                 ((,class (:foreground ,blue-d :background ,blue-bg :bold t :family "iosevka" :height 1.0   ))))
            `(elfeed-search-title-face               ((,class (:foreground ,base   :family "Operator Mono" :height 1.0     ))))
            `(elfeed-search-unread-count-face        ((,class (:foreground ,blue-l   :family "Operator Mono" :height 1.0   ))))
            `(elfeed-search-unread-title-face        ((,class (:foreground ,base-b :family "Operator Mono" :height 1.0 :bold t))))

;;;;; Message
            `(message-cited-text                     ((,class (:foreground ,blue-l    ))))
            `(message-header-cc                      ((,class (:foreground ,yellow-l  ))))
            `(message-header-name                    ((,class (:foreground ,red-l     ))))
            `(message-header-newsgroups              ((,class (:foreground ,base      ))))
            `(message-header-other                   ((,class (:foreground ,aqua-l    ))))
            `(message-header-subject                 ((,class (:foreground ,yellow  ))))
            `(message-header-to                      ((,class (:foreground ,blue    ))))
            `(message-header-xheader                 ((,class (:foreground ,bg4     ))))
            `(message-mml                            ((,class (:foreground ,yellow-bg ))))
            `(message-separator                      ((,class (:foreground ,blue-bg    ))))

;;;;; doom-line
            `(doom-modeline-buffer-path              ((,class (:foreground ,aqua :bold t  ))))
            `(doom-modeline-project-root-dir         ((,class (:foreground ,aqua :bold t  ))))
            `(doom-modeline-buffer-file              ((,class (:foreground ,blue :bold t  ))))
            `(doom-modeline-buffer-modified          ((,class (:foreground ,red :bold t  ))))
            `(doom-modeline-buffer-major-mode        ((,class (:foreground ,blue :bold t  ))))
            `(doom-modeline-highlight                ((,class (:background ,blue :bold t  ))))
            `(doom-modeline-panel                    ((,class (:background ,blue :foreground ,bg1 :bold t  ))))
            ;; `(doom-modeline-info                  ((,class (  ))))
            `(doom-modeline-perspname                ((,class (:foreground ,magenta :bold t  ))))
            `(doom-modeline-workspace-number         ((,class (:foreground ,magenta :weight normal  ))))
            `(doom-modeline-info                     ((,class (:bold t :foreground ,green-d  ))))
            `(doom-modeline-warning                  ((,class (:bold t :foreground ,yellow-d  ))))
            `(doom-modeline-urgent                   ((,class (:bold t :foreground ,red-d  ))))

            `(mode-line                              ((,class (:foreground ,base :weight bold :background ,act1  ))))
            `(mode-line-buffer-id                    ((,class (:weight bold :foreground ,func  ))))
            `(mode-line-inactive                     ((,class (:foreground ,base-dim :weight bold :background ,act1  ))))
            `(mode-line-highlight                    ((,class (:background ,act1 :foreground ,base :weight bold  ))))
            `(mode-line-emphasis                    ((,class (:background ,act1 :foreground ,yellow-l :weight bold  ))))

            `(doom-modeline-bar                      ((,class (:background ,blue))))
            ;; `(doom-modeline-eldoc-bar             ((,class ())))
            `(doom-modeline-inactive-bar             ((,class (:background ,blue-l))))
            `(window-divider                         ((,class (:foreground ,act1))))


;;;;; git-gutter+
            `(git-gutter+-added                      ((,class (:foreground ,green-d :weight bold))))
            `(git-gutter+-deleted                    ((,class (:foreground ,red-d :weight bold))))
            `(git-gutter+-modified                   ((,class (:foreground ,yellow-d :weight bold))))
            `(git-gutter+-separator                  ((,class (:foreground ,blue-d :weight bold))))


;;;;; basics
            `(cursor                                 ((,class (:background ,cursor))))
            `(custom-button                          ((,class :background ,bg2 :foreground ,base :box (:line-width 1 :style ))))
            `(default                                ((,class (:background ,bg1 :foreground ,base))))
            `(default-italic                         ((,class (:italic t))))
            `(error                                  ((,class (:foreground ,err))))
            `(eval-sexp-fu-flash                     ((,class (:background ,suc :foreground ,bg1))))
            `(eval-sexp-fu-flash-error               ((,class (:background ,err :foreground ,bg1))))
            `(font-lock-builtin-face                 ((,class (:foreground ,keyword :italic t))))
            `(font-lock-comment-face                 ((,class (:foreground ,(if modern-theme-comment-italic comment-light comment) :background ,(when modern-theme-comment-bg comment-bg) :slant ,(if modern-theme-comment-italic 'italic 'normal)))))
            `(font-lock-constant-face                ((,class (:foreground ,const))))
            `(font-lock-doc-face                     ((,class (:foreground ,comment))))
            `(font-lock-function-name-face           ((,class (:foreground ,func :inherit bold :italic t))))
            `(font-lock-keyword-face                 ((,class (:inherit bold :foreground ,keyword))))
            `(font-lock-negation-char-face           ((,class (:foreground ,const))))
            `(font-lock-preprocessor-face            ((,class (:foreground ,func))))
            `(font-lock-reference-face               ((,class (:foreground ,const))))
            `(font-lock-string-face                  ((,class (:foreground ,str))))
            `(font-lock-type-face                    ((,class (:foreground ,type :inherit bold))))
            `(font-lock-variable-name-face           ((,class (:foreground ,var))))
            `(font-lock-warning-face                 ((,class (:foreground ,war :background ,bg1))))
            `(fringe                                 ((,class (:background ,bg1 :foreground ,base))))
            `(header-line                            ((,class :background ,act1)))
            `(highlight                              ((,class (:foreground ,base :background ,highlight))))
            `(hl-line                                ((,class (:background ,bg2))))
            `(isearch                                ((,class (:foreground ,bg1 :background ,magenta))))
            `(lazy-highlight                         ((,class (:background ,blue-bg :weight normal))))
            `(link                                   ((,class (:foreground ,comment :underline t))))
            `(link-visited                           ((,class (:foreground ,comp :underline t))))
            `(match                                  ((,class (:background ,highlight :foreground ,magenta))))
            `(minibuffer-prompt                      ((,class (:inherit bold :foreground ,keyword))))
            `(page-break-lines                       ((,class (:foreground ,act2))))
            `(region                                 ((,class (:background ,highlight))))
            `(secondary-selection                    ((,class (:background ,bg3))))
            `(shadow                                 ((,class (:foreground ,base-dim))))
            `(success                                ((,class (:foreground ,suc))))
            `(tooltip                                ((,class (:background ,ttip-bg :foreground ,base :bold nil :italic nil :underline nil))))
            `(vertical-border                        ((,class (:foreground ,border))))
            `(warning                                ((,class (:foreground ,war))))

;;;;; auto-complete
            `(ac-completion-face                     ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
            `(avy-lead-face                          ((,class (:background ,blue-bg :foreground ,magenta))))
            `(avy-lead-face-0                        ((,class (:background ,blue-bg :foreground ,blue))))
            `(avy-lead-face-1                        ((,class (:background ,blue-bg :foreground ,magenta))))
            `(avy-lead-face-2                        ((,class (:background ,blue-bg :foreground ,blue))))

;;;;; company
            `(company-echo-common                    ((,class (:background ,base :foreground ,bg1))))
            `(company-preview                        ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(company-preview-common                 ((,class (:background ,ttip-bg :foreground ,base))))
            `(company-preview-search                 ((,class (:inherit match))))
            `(company-scrollbar-bg                   ((,class (:background ,bg2))))
            `(company-scrollbar-fg                   ((,class (:background ,act2))))
            `(company-template-field                 ((,class (:inherit region))))
            `(company-tooltip                        ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(company-tooltip-annotation             ((,class (:foreground ,keyword))))
            `(company-tooltip-common                 ((,class (:background ,ttip-bg :foreground ,base))))
            `(company-tooltip-common-selection       ((,class (:foreground ,base))))
            `(company-tooltip-mouse                  ((,class (:inherit highlight))))
            `(company-tooltip-search                 ((,class (:inherit match))))
            `(company-tooltip-selection              ((,class (:background ,ttip-sl :foreground ,base))))

;;;;; diff
            `(diff-added                             ((,class :background nil :foreground ,green)))
            `(diff-changed                           ((,class :background nil :foreground ,keyword)))
            `(diff-header                            ((,class :background ,cblk-ln-bg :foreground ,func)))
            `(diff-file-header                       ((,class :background ,cblk-ln-bg :foreground ,cblk)))
            `(diff-indicator-added                   ((,class :background nil :foreground ,green)))
            `(diff-indicator-changed                 ((,class :background nil :foreground ,keyword)))
            `(diff-indicator-removed                 ((,class :background nil :foreground ,red)))
            `(diff-refine-added                      ((,class :background ,green :foreground ,bg4)))
            `(diff-refine-changed                    ((,class :background ,keyword :foreground ,bg4)))
            `(diff-refine-removed                    ((,class :background ,red :foreground ,bg4)))
            `(diff-removed                           ((,class :background nil :foreground ,red)))

;;;;; diff-hl
            `(diff-hl-change                         ((,class :background ,blue-bg :foreground ,blue)))
            `(diff-hl-delete                         ((,class :background ,red-bg :foreground ,red)))
            `(diff-hl-insert                         ((,class :background ,green-bg :foreground ,green)))

;;;;; dired
            `(dired-directory                        ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
            `(dired-flagged                          ((,class (:foreground ,red))))
            `(dired-header                           ((,class (:foreground ,comp :inherit bold))))
            `(dired-ignored                          ((,class (:inherit shadow))))
            `(dired-mark                             ((,class (:foreground ,comp :inherit bold))))
            `(dired-marked                           ((,class (:foreground ,magenta :inherit bold))))
            `(dired-perm-write                       ((,class (:foreground ,base :underline t))))
            `(dired-symlink                          ((,class (:foreground ,blue-l :background ,bg1 :inherit bold))))
            `(dired-warning                          ((,class (:foreground ,war))))

;;;;; ediff
            `(ediff-current-diff-A                   ((,class(:background ,red-bg-s :foreground ,red))))
            `(ediff-current-diff-Ancestor            ((,class(:background ,aqua-bg :foreground ,aqua))))
            `(ediff-current-diff-B                   ((,class(:background ,green-bg-s :foreground ,green))))
            `(ediff-current-diff-C                   ((,class(:background ,blue-bg :foreground ,blue))))
            `(ediff-even-diff-A                      ((,class(:background ,bg3))))
            `(ediff-even-diff-Ancestor               ((,class(:background ,bg3))))
            `(ediff-even-diff-B                      ((,class(:background ,bg3))))
            `(ediff-even-diff-C                      ((,class(:background ,bg3))))
            `(ediff-fine-diff-A                      ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-Ancestor               ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-B                      ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-C                      ((,class(:background nil :inherit bold))))
            `(ediff-odd-diff-A                       ((,class(:background ,bg4))))
            `(ediff-odd-diff-Ancestor                ((,class(:background ,bg4))))
            `(ediff-odd-diff-B                       ((,class(:background ,bg4))))
            `(ediff-odd-diff-C                       ((,class(:background ,bg4))))

;;;;; eldoc
            `(eldoc-highlight-function-argument      ((,class (:foreground ,magenta :inherit bold))))

;;;;; eshell
            `(eshell-ls-archive                      ((,class (:foreground ,red :inherit bold))))
            `(eshell-ls-backup                       ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-clutter                      ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-directory                    ((,class (:foreground ,keyword :inherit bold))))
            `(eshell-ls-executable                   ((,class (:foreground ,suc :inherit bold))))
            `(eshell-ls-missing                      ((,class (:inherit font-lock-warning-face))))
            `(eshell-ls-product                      ((,class (:inherit font-lock-doc-face))))
            `(eshell-ls-special                      ((,class (:foreground ,yellow :inherit bold))))
            `(eshell-ls-symlink                      ((,class (:foreground ,blue-l :inherit bold))))
            `(eshell-ls-unreadable                   ((,class (:foreground ,base))))
            `(eshell-prompt                          ((,class (:foreground ,keyword :inherit bold))))

;;;;; evil
            `(evil-ex-substitute-matches             ((,class (:background ,red-bg :foreground ,red))))
            `(evil-ex-substitute-replacement         ((,class (:background ,green-bg :foreground ,green))))

;;;;; flycheck
            `(flycheck-error                         ((,(append '((supports :underline (:style line))) class) (:underline (:style line :color ,err))) (,class (:foreground ,base :background ,err :inherit bold :underline t))))
            `(flycheck-error-list-checker-name       ((,class (:foreground ,keyword))))
            `(flycheck-fringe-error                  ((,class (:foreground ,err :inherit bold))))
            `(flycheck-fringe-info                   ((,class (:foreground ,keyword :inherit bold))))
            `(flycheck-fringe-warning                ((,class (:foreground ,war :inherit bold))))
            `(flycheck-info                          ((,(append '((supports :underline (:style line))) class)
                              (:underline (:style line :color ,keyword)))
                             (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
            `(flycheck-warning                       ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,war)))
               (,class (:foreground ,base :background ,war :inherit bold :underline t))))
            `(flyspell-incorrect                     ((,class (:foreground ,err :inherit flycheck-error))))
            `(flyspell-duplicate                     ((,class (:foreground ,war :inherit flycheck-warning))))

;;;;; git-timemachine
            `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; guide-key
            `(guide-key/highlight-command-face       ((,class (:foreground ,base))))
            `(guide-key/key-face                     ((,class (:foreground ,keyword))))
            `(guide-key/prefix-command-face          ((,class (:foreground ,keyword :inherit bold))))

;;;;; highlights
            `(hi-green                               ((,class (:foreground ,green :background ,green-bg))))
            `(hi-yellow                              ((,class (:foreground ,yellow :background ,yellow-bg))))

;;;;; highlight-indentation
            `(highlight-indentation-face             ((,class (:background ,comment-bg))))

;;;;; highlight-symbol
            `(highlight-symbol-face                  ((,class (:background ,bg2))))

;;;;; hydra
            `(hydra-face-blue                        ((,class (:foreground ,blue))))
            `(hydra-face-red                         ((,class (:foreground ,red))))

;;;;; info
            `(info-header-xref                       ((,class (:foreground ,func :underline t))))
            `(info-menu                              ((,class (:foreground ,suc))))
            `(info-node                              ((,class (:foreground ,func :inherit bold))))
            `(info-quoted-name                       ((,class (:foreground ,keyword))))
            `(info-reference-item                    ((,class (:background nil :underline t :inherit bold))))
            `(info-string                            ((,class (:foreground ,str))))
            `(info-title-1                           ((,class (:height 1.4 :inherit bold))))
            `(info-title-2                           ((,class (:height 1.3 :inherit bold))))
            `(info-title-3                           ((,class (:height 1.3))))
            `(info-title-4                           ((,class (:height 1.2))))

;;;;; ivy
            `(ivy-current-match                      ((,class (:background ,highlight :weight bold))))
            `(ivy-minibuffer-match-face-1            ((,class (:weight bold :height 1.0))))
            `(ivy-minibuffer-match-face-2            ((,class (:foreground ,blue-d :height 1.0 :background ,blue-bg :weight bold))))
            `(ivy-minibuffer-match-face-3            ((,class (:foreground ,red  :height 1.0 :background ,red-bg :weight bold))))
            `(ivy-minibuffer-match-face-4            ((,class (:foreground ,magenta  :height 1.0 :background ,magenta-bg :weight bold))))
            `(ivy-remote                             ((,class (:foreground ,blue-l))))

;;;;; swiper
            `(swiper-line-face                       ((,class (:background ,highlight :inherit bold))))
            `(swiper-match-face-1                    ((,class (:weight bold :height 1.0))))
            `(swiper-match-face-2                    ((,class :foreground ,blue-d :background ,blue-bg  :height 1.0 :weight bold)))
            `(swiper-match-face-3                    ((,class (:foreground ,red :background ,red-bg  :height 1.0 :weight bold))))
            `(swiper-match-face-4                    ((,class (:foreground ,magenta :background ,magenta-bg  :height 1.0 :weight bold))))

;;;;; latex
            `(font-latex-bold-face                   ((,class (:foreground ,comp))))
            `(font-latex-italic-face                 ((,class (:foreground ,keyword :italic t))))
            `(font-latex-match-reference-keywords    ((,class (:foreground ,const))))
            `(font-latex-match-variable-keywords     ((,class (:foreground ,var))))
            `(font-latex-sectioning-0-face           ((,class (:inherit org-level-1))))
            `(font-latex-sectioning-1-face           ((,class (:inherit org-level-2))))
            `(font-latex-sectioning-2-face           ((,class (:inherit org-level-3))))
            `(font-latex-sectioning-3-face           ((,class (:inherit org-level-4))))
            `(font-latex-sectioning-4-face           ((,class (:inherit org-level-5))))
            `(font-latex-sectioning-5-face           ((,class (:inherit org-level-6))))
            `(font-latex-string-face                 ((,class (:foreground ,str))))
            `(font-latex-warning-face                ((,class (:foreground ,war))))

;;;;; magit
            `(magit-blame-culprit                    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-date                       ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-hash                       ((,class :background ,yellow-bg :foreground ,func)))
            `(magit-blame-header                     ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-heading                    ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-name                       ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-sha1                       ((,class :background ,yellow-bg :foreground ,func)))
            `(magit-blame-subject                    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-summary                    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-time                       ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-branch                           ((,class (:foreground ,const :bold t))))
            `(magit-branch-current                   ((,class (:background ,blue-bg :foreground ,blue :bold t :box t))))
            `(magit-branch-local                     ((,class (:background ,blue-bg :foreground ,blue :bold t))))
            `(magit-branch-remote                    ((,class (:background ,aqua-bg :foreground ,aqua :bold t))))
            `(magit-diff-context-highlight           ((,class (:background ,bg2 :foreground ,base))))
            `(magit-diff-file-header                 ((,class (:background ,comment-bg :foreground ,comment))))
            `(magit-diff-file-heading                ((,class (:background ,comment-bg :foreground ,comment))))
            `(magit-diff-file-heading-highlight      ((,class (:background ,bg2 :foreground ,comment))))
            `(magit-diff-hunk-header                 ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-diff-hunk-heading                ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-diff-hunk-heading-highlight      ((,class (:background ,ttip-sl :foreground ,ttip))))
            `(magit-hash                             ((,class (:foreground ,var))))
            `(magit-hunk-heading                     ((,class (:background ,bg2))))
            `(magit-hunk-heading-highlight           ((,class (:background ,bg3))))
            `(magit-item-highlight                   ((,class :background ,bg2)))
            `(magit-log-author                       ((,class (:foreground ,func))))
            `(magit-log-head-label-head              ((,class (:background ,yellow :foreground ,bg1 :bold t))))
            `(magit-log-head-label-local             ((,class (:background ,keyword :foreground ,bg1 :bold t))))
            `(magit-log-head-label-remote            ((,class (:background ,suc :foreground ,bg1 :bold t))))
            `(magit-log-head-label-tags              ((,class (:background ,magenta :foreground ,bg1 :bold t))))
            `(magit-log-head-label-wip               ((,class (:background ,blue-l :foreground ,bg1 :bold t))))
            `(magit-log-sha1                         ((,class (:foreground ,str))))
            `(magit-process-ng                       ((,class (:foreground ,war :bold t))))
            `(magit-process-ok                       ((,class (:foreground ,func :bold t))))
            `(magit-reflog-amend                     ((,class (:foreground ,magenta))))
            `(magit-reflog-checkout                  ((,class (:foreground ,blue))))
            `(magit-reflog-cherry-pick               ((,class (:foreground ,green))))
            `(magit-reflog-commit                    ((,class (:foreground ,green))))
            `(magit-reflog-merge                     ((,class (:foreground ,green))))
            `(magit-reflog-other                     ((,class (:foreground ,blue-l))))
            `(magit-reflog-rebase                    ((,class (:foreground ,magenta))))
            `(magit-reflog-remote                    ((,class (:foreground ,blue-l))))
            `(magit-reflog-reset                     ((,class (:foreground ,red))))
            `(magit-section-heading                  ((,class (:foreground ,keyword :bold t))))
            `(magit-section-highlight                ((,class (:background ,bg2))))
            `(magit-section-title                    ((,class (:background ,bg1 :foreground ,keyword :bold t))))

;;;;; Outline
            `(outline-1  ((,class (:inherit org-level-1))))
            `(outline-2  ((,class (:inherit org-level-2))))
            `(outline-3  ((,class (:inherit org-level-3))))
            `(outline-4  ((,class (:inherit org-level-4))))
            `(outline-5  ((,class (:inherit org-level-5))))
            `(outline-6  ((,class (:inherit org-level-6))))
            `(outline-7  ((,class (:inherit org-level-7))))
            `(outline-8  ((,class (:inherit org-level-8))))
            `(outshine-1 ((,class (:inherit org-level-1))))
            `(outshine-2 ((,class (:inherit org-level-2))))
            `(outshine-3 ((,class (:inherit org-level-3))))
            `(outshine-4 ((,class (:inherit org-level-4))))
            `(outshine-5 ((,class (:inherit org-level-5))))
            `(outshine-6 ((,class (:inherit org-level-6))))
            `(outshine-7 ((,class (:inherit org-level-7))))
            `(outshine-8 ((,class (:inherit org-level-8))))


;;;;; man
            `(Man-overstrike                         ((,class (:foreground ,red-d :inherit bold))))
            `(Man-reverse                            ((,class (:foreground ,highlight))))
            `(Man-underline                          ((,class (:foreground ,comp :underline t))))

;;;;; markdown
            `(markdown-header-face-1                 ((,class (:inherit org-level-1))))
            `(markdown-header-face-2                 ((,class (:inherit org-level-2))))
            `(markdown-header-face-3                 ((,class (:inherit org-level-3))))
            `(markdown-header-face-4                 ((,class (:inherit org-level-4))))
            `(markdown-header-face-5                 ((,class (:inherit org-level-5))))
            `(markdown-header-face-6                 ((,class (:inherit org-level-6))))

;;;;; mode-line
            ;; `(powerline-active1                   ((,class (:background ,act1 :foreground ,base ))))
            ;; `(powerline-active2                   ((,class (:background ,act1 :foreground ,base ))))
            ;; `(powerline-inactive1                 ((,class (:background ,act1 :foreground ,base ))))
            ;; `(powerline-inactive2                 ((,class (:background ,act1 :foreground ,base ))))
            ;; `(spaceline-highlight-face            ((,class (:background ,act1 :foreground ,base ))))


;;;;; neotree
            `(neo-dir-link-face                      ((,class (:foreground ,keyword :inherit bold))))
            `(neo-expand-btn-face                    ((,class (:foreground ,base))))
            `(neo-file-link-face                     ((,class (:foreground ,base))))
            `(neo-root-dir-face                      ((,class (:foreground ,func :inherit bold))))

;;;;; org
            `(org-agenda-clocking                    ((,class (:family "operator mono" :background ,highlight :foreground ,comp))))
            `(org-agenda-date                        ((,class (:family "operator mono" :foreground ,var ))))
            `(org-agenda-date-today                  ((,class (:family "operator mono" :foreground ,keyword :inherit bold ))))
            `(org-agenda-date-weekend                ((,class (:family "operator mono" :foreground ,var))))
            `(org-agenda-done                        ((,class (:family "operator mono" :foreground ,base-dim ))))
            `(org-agenda-structure                   ((,class (:family "operator mono" :weight bold :overline t :underline t :slant italic :foreground ,comp))))
            `(org-scheduled                          ((,class (:family "operator mono" :slant italic :weight bold :foreground ,comp))))
            `(org-scheduled-today                    ((,class (:family "operator mono" :slant italic :weight bold :foreground ,func ))))
            `(org-scheduled-previously               ((,class (:family "operator mono" :slant italic :weight bold :foreground ,func ))))
            `(org-warning                            ((,class (:family "operator mono" :slant italic :weight bold :foreground ,red-d ))))
            `(org-time-grid                          ((,class (:family "operator mono" :foreground ,str))))

            `(org-sexp-date                          ((,class (:family "operator mono" :foreground ,base))))
            `(org-special-keyword                    ((,class (:family "operator mono" :foreground ,func))))
            `(org-tag                                ((,class (:family "operator mono" :slant italic :weight bold :foreground ,aqua))))
            `(org-table                              ((,class (:family "operator mono" :foreground ,red ))))
            `(org-todo                               ((,class (:family "operator mono" :foreground ,war :inherit bold :background ,yellow-bg))))
            `(org-verbatim                           ((,class (:family "operator mono" :foreground ,keyword))))
            `(org-verse                              ((,class (:family "operator mono" :background ,cblk-bg :foreground ,cblk :slant italic))))

            `(org-habit-clear-face                   ((,class (:bold t :family "operator mono" :background ,bg1 :foreground ,bg1 ))))
            `(org-habit-clear-future-face            ((,class (:bold t :family "operator mono" :background ,bg1 :foreground ,bg1 ))))
            `(org-habit-ready-face                   ((,class (:bold t :family "operator mono" :background ,blue-bg :foreground ,blue-bg ))))
            `(org-habit-ready-future-face            ((,class (:bold t :family "operator mono" :background ,blue-bg :foreground ,blue-bg ))))
            `(org-habit-alert-face                   ((,class (:bold t :family "operator mono" :background ,yellow-bg :foreground ,yellow-bg ))))
            `(org-habit-alert-future-face            ((,class (:bold t :family "operator mono" :background ,yellow-bg :foreground ,yellow-bg ))))
            `(org-habit-overdue-face                 ((,class (:bold t :family "operator mono" :background ,red-bg :foreground ,red-bg ))))
            `(org-habit-overdue-future-face          ((,class (:bold t :family "operator mono" :background ,red-bg :foreground ,red-bg ))))

            `(org-block                              ((,class (:family "operator mono" :background ,cblk-bg :foreground ,cblk))))
            `(org-block-begin-line                   ((,class (:family "operator mono" :background ,cblk-ln-bg :foreground ,cblk-ln))))
            `(org-block-end-line                     ((,class (:family "operator mono" :background ,cblk-ln-bg :foreground ,cblk-ln))))
            `(org-clock-overlay                      ((,class (:family "operator mono" :foreground ,comp))))
            `(org-code                               ((,class (:family "operator mono" :foreground ,blue-l :height 1.0))))
            `(org-column                             ((,class (:family "operator mono" :foreground ,red))))
            `(org-column-title                       ((,class (:family "operator mono" :background ,highlight))))
            `(org-date                               ((,class (:family "operator mono" :underline nil :weight bold :foreground ,yellow-d :background ,yellow-bg))))
            `(org-date-selected                      ((,class (:family "operator mono" :weight bold :background ,yellow-bg :foreground ,yellow-d))))
            `(org-document-info-keyword              ((,class (:family "operator mono" :foreground ,base))))
            `(org-document-title                     ((,class (:family "futura" :foreground ,func :inherit bold :height ,(if modern-theme-org-height 1.4 1.0) :underline t))))
            `(org-headline-done                      ((,class (:foreground ,base-dim :weight bold :slant italic :background ,bg1))))
            `(org-ellipsis                           ((,class (:foreground ,bg4))))
            `(org-footnote                           ((,class (:underline t :foreground ,base))))
            `(org-hide                               ((,class (:foreground ,base))))
            `(org-kbd                                ((,class (:family "operator mono" :inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
            `(org-level-1                            ((,class (:family "operator ssm" :foreground ,blue-d  :weight ultra-bold :height ,(if modern-theme-org-height 1.3 1.0) :slant italic :background ,(when modern-theme-org-highlight blue-bg)))))
            `(org-level-2                            ((,class (:family "operator ssm" :foreground ,magenta-d :weight extra-bold :height ,(if modern-theme-org-height 1.2 1.0) :slant italic :background ,(when modern-theme-org-highlight magenta-bg)))))
            `(org-level-3                            ((,class (:family "operator ssm" :foreground ,red-d     :weight bold :height ,(if modern-theme-org-height 1.1 1.0) :slant italic :background ,(when modern-theme-org-highlight red-bg)))))
            `(org-level-4                            ((,class (:family "operator ssm" :foreground ,green-d   :weight bold :slant italic :background ,(when modern-theme-org-highlight green-bg)))))
            `(org-level-5                            ((,class (:family "operator ssm" :foreground ,blue-l  :weight bold :slant italic))))
            `(org-level-6                            ((,class (:family "operator ssm" :foreground ,magenta-l :weight bold :slant italic))))
            `(org-level-7                            ((,class (:family "operator ssm" :foreground ,red-l     :weight bold :slant italic))))
            `(org-level-8                            ((,class (:family "operator ssm" :foreground ,green-l   :weight bold :slant italic))))
            `(org-link                               ((,class (:inherit default :underline t))))
            `(org-meta-line                          ((,class (:family "operator mono" :foreground ,bg4))))
            `(org-mode-line-clock-overrun            ((,class (:foreground ,err))))
            `(org-priority                           ((,class (:family "operator mono" :foreground ,war :height 0.85 :inherit bold))))
            `(org-quote                              ((,class (:family "operator mono" :inherit org-block :slant italic))))

            `(org-todo-keyword-todo                ((,class (:foreground ,blue-d :background ,blue-bg :weight bold  ))))
            `(org-todo-keyword-habt                ((,class (:foreground ,yellow-d :background ,yellow-bg :weight bold  ))))
            `(org-todo-keyword-done                ((,class (:foreground ,green-d :background ,green-bg :weight bold  ))))
            `(org-todo-keyword-wait                ((,class (:foreground ,red-d :background ,red-bg :weight bold  ))))
            `(org-todo-keyword-kill                ((,class (:foreground ,magenta-d :background ,magenta-bg :weight bold  ))))
            `(org-todo-keyword-outd                ((,class (:foreground ,cblk :background ,cblk-bg :weight bold  ))))

;;;;; perspective
            `(persp-selected-face                    ((,class (:inherit bold :foreground ,func))))

;;;;; popup
            `(popup-enu-selection-face               ((,class (:background ,ttip-sl :foreground ,base))))
            `(popup-face                             ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(popup-isearch-match                    ((,class (:inherit match))))
            `(popup-menu-face                        ((,class (:background ,ttip-bg :foreground ,base))))
            `(popup-menu-mouse-face                  ((,class (:inherit highlight))))
            `(popup-scroll-bar-background-face       ((,class (:background ,bg2))))
            `(popup-scroll-bar-foreground-face       ((,class (:background ,act2))))
            `(popup-tip-face                         ((,class (:background ,blue-bg :foreground ,base :bold nil :italic nil :underline nil))))

;;;;; EIN

            `(ein:cell-input-prompt                  ((,class :foreground ,blue :background ,blue-bg :weight bold)))
            `(ein:notification-tab-selected          ((,class :foreground ,blue-d :background ,blue-bg :weight bold)))
            `(ein:cell-input-area                    ((,class :background ,cblk-bg)))
            `(ein:notification-tab-normal            ((,class :foreground ,bg1 :background ,blue-bg :weight bold)))
            `(ein:cell-heading-1                     ((,class :inherit org-level-1 )))
            `(ein:cell-heading-2                     ((,class :inherit org-level-2 )))
            `(ein:cell-heading-3                     ((,class :inherit org-level-3 )))
            `(ein:cell-heading-4                     ((,class :inherit org-level-4 )))
            `(ein:cell-heading-5                     ((,class :inherit org-level-5 )))
            `(ein:cell-heading-6                     ((,class :inherit org-level-6 )))
            `(ein:cell-output-prompt                 ((,class :foreground ,red :background ,red-bg :weight bold)))
            ;; `(ein:pos-tip-face                       ((,class :inherit pos-tip-face)))
            `(ein:cell-output-stderr                 ((,class :background ,red-bg )))


;;;;; rainbow-delimiters
            `(rainbow-delimiters-depth-1-face        ((,class :foreground ,blue-d)))
            `(rainbow-delimiters-depth-2-face        ((,class :foreground ,red-d)))
            `(rainbow-delimiters-depth-3-face        ((,class :foreground ,magenta-d)))
            `(rainbow-delimiters-depth-4-face        ((,class :foreground ,green-d)))
            `(rainbow-delimiters-depth-5-face        ((,class :foreground ,yellow-d)))
            `(rainbow-delimiters-depth-6-face        ((,class :foreground ,blue-l)))
            `(rainbow-delimiters-depth-7-face        ((,class :foreground ,red-l)))
            `(rainbow-delimiters-depth-8-face        ((,class :foreground ,magenta-l)))
            `(rainbow-delimiters-mismatched-face     ((,class :foreground ,err :overline t)))
            `(rainbow-delimiters-unmatched-face      ((,class :foreground ,err :overline t)))

;;;;; shm
            `(shm-current-face                       ((,class (:background ,green-bg-s))))
            `(shm-quarantine-face                    ((,class (:background ,red-bg-s))))

;;;;; show-paren
            `(show-paren-match                       ((,class (:background ,green-bg-s))))
            `(show-paren-mismatch                    ((,class (:background ,red-bg-s))))

;;;;; smartparens
            `(sp-pair-overlay-face                   ((,class (:background ,highlight :foreground nil))))
            `(sp-show-pair-match-face                ((,class (:foreground ,magenta :inherit bold :underline t))))

;;;;; smerge
            `(smerge-base                            ((,class (:background ,yellow-bg))))
            `(smerge-markers                         ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(smerge-mine                            ((,class (:background ,red-bg))))
            `(smerge-other                           ((,class (:background ,green-bg))))
            `(smerge-refined-added                   ((,class (:background ,green-bg-s :foreground ,green))))
            `(smerge-refined-changed                 ((,class (:background ,blue-bg :foreground ,blue))))
            `(smerge-refined-removed                 ((,class (:background ,red-bg-s :foreground ,red))))

;;;;; spaceline
            `(spaceline-flycheck-error               ((,class (:foreground ,err))))
            `(spaceline-flycheck-info                ((,class (:foreground ,keyword))))
            `(spaceline-flycheck-warning             ((,class (:foreground ,war))))
            `(spaceline-python-venv                  ((,class (:foreground ,comp))))

;;;;; modern-specific
            `(modern-transient-state-title-face      ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; term
            `(term                                   ((,class (:foreground ,base :background ,bg1))))
            `(term-color-black                       ((,class (:foreground ,bg4))))
            `(term-color-blue                        ((,class (:foreground ,keyword))))
            `(term-color-cyan                        ((,class (:foreground ,blue-l))))
            `(term-color-green                       ((,class (:foreground ,green))))
            `(term-color-magenta                     ((,class (:foreground ,magenta))))
            `(term-color-red                         ((,class (:foreground ,red))))
            `(term-color-white                       ((,class (:foreground ,base))))
            `(term-color-yellow                      ((,class (:foreground ,yellow))))

;;;;; web-mode
            `(web-mode-builtin-face                  ((,class (:inherit ,font-lock-builtin-face))))
            `(web-mode-comment-face                  ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-constant-face                 ((,class (:inherit ,font-lock-constant-face))))
            `(web-mode-doctype-face                  ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-function-name-face            ((,class (:inherit ,font-lock-function-name-face))))
            `(web-mode-html-attr-name-face           ((,class (:foreground ,func))))
            `(web-mode-html-attr-value-face          ((,class (:foreground ,keyword))))
            `(web-mode-html-tag-face                 ((,class (:foreground ,keyword))))
            `(web-mode-keyword-face                  ((,class (:foreground ,keyword))))
            `(web-mode-string-face                   ((,class (:foreground ,str))))
            `(web-mode-symbol-face                   ((,class (:foreground ,type))))
            `(web-mode-type-face                     ((,class (:inherit ,font-lock-type-face))))
            `(web-mode-warning-face                  ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
            `(which-key-command-description-face     ((,class (:foreground ,base))))
            `(which-key-group-description-face       ((,class (:foreground ,keyword))))
            `(which-key-key-face                     ((,class (:foreground ,func :inherit bold))))
            `(which-key-separator-face               ((,class (:background nil :foreground ,str))))
            `(which-key-special-key-face             ((,class (:background ,func :foreground ,bg1))))

;;;;; which-function-mode
            `(which-func                             ((,class (:foreground ,func))))

;;;;; whitespace-mode
            `(whitespace-empty                       ((,class ( :background nil   :foreground ,yellow))))
            `(whitespace-indentation                 ((,class ( :background nil   :foreground ,war))))
            `(whitespace-line                        ((,class ( :background nil   :foreground ,comp))))
            `(whitespace-newline                     ((,class ( :background nil   :foreground ,comp))))
            `(whitespace-space                       ((,class ( :background nil   :foreground ,act2))))
            `(whitespace-space-after-tab             ((,class ( :background nil   :foreground ,yellow))))
            `(whitespace-space-before-tab            ((,class ( :background nil   :foreground ,yellow))))
            `(whitespace-tab                         ((,class ( :background nil))))
            `(whitespace-trailing                    ((,class ( :background ,err  :foreground ,war))))

;;;;; other, need more work
            `(ac-completion-face                     ((,class ( :underline t      :foreground ,keyword))))
            `(ffap                                   ((,class ( :foreground ,base))))
            `(flx-highlight-face                     ((,class ( :foreground ,comp :underline nil))))
            `(icompletep-determined                  ((,class ( :foreground ,keyword))))
            `(js2-external-variable                  ((,class ( :foreground ,comp))))
            `(js2-function-param                     ((,class ( :foreground ,const))))
            `(js2-jsdoc-html-tag-delimiter           ((,class ( :foreground ,str))))
            `(js2-jsdoc-html-tag-name                ((,class ( :foreground ,keyword))))
            `(js2-jsdoc-value                        ((,class ( :foreground ,str))))
            `(js2-private-function-call              ((,class ( :foreground ,const))))
            `(js2-private-member                     ((,class ( :foreground ,base))))
            `(js3-error-face                         ((,class ( :underline ,war))))
            `(js3-external-variable-face             ((,class ( :foreground ,var))))
            `(js3-function-param-face                ((,class ( :foreground ,keyword))))
            `(js3-instance-member-face               ((,class ( :foreground ,const))))
            `(js3-jsdoc-tag-face                     ((,class ( :foreground ,keyword))))
            `(js3-warning-face                       ((,class ( :underline ,keyword))))
            `(slime-repl-inputed-output-face         ((,class ( :foreground ,comp))))
            `(trailing-whitespace                    ((,class (  :foreground nil   :background ,err))))
            `(undo-tree-visualizer-current-face      ((,class (  :foreground ,keyword))))
            `(undo-tree-visualizer-default-face      ((,class (  :foreground ,base))))
            `(undo-tree-visualizer-register-face     ((,class (  :foreground ,comp))))
            `(undo-tree-visualizer-unmodified-face   ((,class (  :foreground ,var)))))

           (custom-theme-set-variables
            theme-name
            `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,blue-l ,base]))

           ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'modern-common)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; modern-common.el ends here
