(require 'modern-common)

(deftheme modern-dark "Modern theme, the dark version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#CACACA")
            (base2   "#999999")
            (base3   "#777777")
            (base4   "#555555")

            (bg0     "#000000")
            (bg1     "#000000")
            (bg2     "#1A1B24")
            (bg3     "#1E1F29")
            (bg4     "#252633")

            (aqua    "#3F988E")
            (green   "#9AC657")
            (red     "#DC5F6D")
            (blue    "#5870BA")
            (magenta "#BF65B6")
            (violet  "#7E6BBC")
            (yellow  "#CAB25E")
            (orange  "#CA752A")

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
            (act1          bg3)
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
            (ttip-sl       bg4)
            (ttip-bg       bg2)

            )
  (create-modern-theme 'modern-dark)
  (provide-theme 'modern-dark)
)

