(require 'modern-common)

(deftheme modern-light "Modern theme, the light version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#333333")
            (base2   "#555555")
            (base3   "#777777")
            (base4   "#999999")

            (bg0     "#FFFFFF")
            (bg1     "#FFFFFF")
            (bg2     "#F5F5F5")
            (bg3     "#F0F0F0")
            (bg4     "#E4E4E4")

            (aqua    "#21BBC6")
            (green   "#52CC37")
            (red     "#FF594B")
            (blue    "#1159DB")
            (magenta "#D516BB")
            (violet  "#5831D6")
            (yellow  "#FFC618")
            (orange  "#FF7E0D")

            (blue-l     (modern-blend bg1 blue 0.4))
            (blue-d     (modern-blend bg1 blue 0.2))
            (blue-bg    (modern-blend bg1 blue 0.8))
            (red-l      (modern-blend bg1 red 0.4))
            (red-d      (modern-blend bg1 red 0.2))
            (red-bg     (modern-blend bg1 red 0.8))
            (yellow-l   (modern-blend bg1 yellow 0.4))
            (yellow-d   (modern-blend bg1 yellow 0.2))
            (yellow-bg  (modern-blend bg1 yellow 0.8))
            (magenta-l  (modern-blend bg1 magenta 0.4))
            (magenta-d  (modern-blend bg1 magenta 0.2))
            (magenta-bg (modern-blend bg1 magenta 0.8))
            (green-l    (modern-blend bg1 green 0.4))
            (green-d    (modern-blend bg1 green 0.2))
            (green-bg   (modern-blend bg1 green 0.8))
            (aqua-l     (modern-blend bg1 aqua 0.4))
            (aqua-d     (modern-blend bg1 aqua 0.2))
            (aqua-bg    (modern-blend bg1 aqua 0.8))
            (orange-l   (modern-blend bg1 orange 0.4))
            (orange-d   (modern-blend bg1 orange 0.2))
            (orange-bg  (modern-blend bg1 orange 0.8))
            (violet-l   (modern-blend bg1 violet 0.4))
            (violet-d   (modern-blend bg1 violet 0.2))
            (violet-bg  (modern-blend bg1 violet 0.8))



;;; General
            (act1          bg2)
            (act2          bg3)
            (cblk          base2)
            (cblk-bg       bg2)
            (cblk-ln       base3)
            (cblk-ln-bg    bg3)
            (cursor        blue)
            (const         green)
            (comment       base4)
            (comment-bg    bg2)

            (err           red)
            (war           orange)
            (suc           green)
            (func          blue)
            (comp          violet)
            (type          red)
            (var           yellow)
            (str           base3)
            (keyword       aqua)

            (highlight     (modern-blend bg1 blue 0.7))

            (ttip          base2)
            (ttip-sl       bg2)
            (ttip-bg       bg2)
            )
  (create-modern-theme 'modern-light)
  (provide-theme 'modern-light)
)

