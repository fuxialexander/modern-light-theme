(require 'modern-common)

(deftheme modern-solarizeddark "Modern theme, the solarizeddark version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#9DACAC")
            (base2   "#93a1a1")
            (base3   "#839496")
            (base4   "#768688")

            (bg1     "#002b36")
            (bg2     "#073642")
            (bg3     "#18303F")
            (bg4     "#214054")

            (aqua    "#2aa198")
            (green   "#859900")
            (red     "#dc322f")
            (blue    "#268bd2")
            (magenta "#d33682")
            (violet "#6c71c4")
            (yellow  "#b58900")
            (orange  "#cb4b16")

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
            (act1          bg4)
            (act2          bg3)
            (cblk          base2)
            (cblk-bg       bg2)
            (cblk-ln       base3)
            (cblk-ln-bg    bg3)
            (cursor        blue)
            (const         green)
            (comment       base3)
            (comment-light base4)
            (comment-bg    bg2)

            (err           red-d)
            (war           orange)
            (suc           green)
            (func          blue)
            (comp          "#D87184")
            (type          red)
            (var           yellow)
            (str           base3)
            (keyword       aqua)

            (highlight     (modern-blend bg1 blue 0.7))
            (highlight-dim     (modern-blend bg1 blue 0.9))

            (ttip          base2)
            (ttip-sl       base4)
            (ttip-bg       bg4)

            )
  (create-modern-theme 'modern-solarizeddark)
  (provide-theme 'modern-solarizeddark)
)

