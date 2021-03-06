(require 'modern-common)

(deftheme modern-spacegray "Modern theme, the spacegray version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#BDC2CB")
            (base2   "#BEC0C6")
            (base3   "#ABAFB7")
            (base4   "#93969D")

            (bg0     "#1C1F27")
            (bg1     "#2B303B")
            (bg2     "#444C5D")
            (bg3     "#515A6F")
            (bg4     "#5C677F")

            (aqua    "#88B2B4")
            (green   "#A2BF8A")
            (red     "#C15F65")
            (blue    "#8EA1B4")
            (magenta "#B38AA9")
            (violet  "#9386B7")
            (yellow  "#ECCC87")
            (orange  "#AB7965")

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
            (act2          bg2)
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
            (str           green)
            (keyword       magenta)

            (highlight     (modern-blend bg1 blue 0.7))

            (ttip          base2)
            (ttip-sl       bg3)
            (ttip-bg       bg2)

            )
  (create-modern-theme 'modern-spacegray)
  (provide-theme 'modern-spacegray)
)

