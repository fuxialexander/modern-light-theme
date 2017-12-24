(require 'modern-common)

(deftheme modern-dawn "Modern theme, the dawn version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#C9BAAC")
            (base2   "#DDD1C6")
            (base3   "#AAA198")
            (base4   "#AAA198")

            (bg0     "#111B21")
            (bg1     "#101F29")
            (bg2     "#132632")
            (bg3     "#18303F")
            (bg4     "#214054")

            (aqua    "#DC9E66")
            (green   "#82D88F")
            (red     "#D87184")
            (blue    "#1E95CC")
            (magenta "#8778D8")
            (violet  "#6c71c4")
            (yellow  "#FFED6A")
            (orange  "#FFA267")

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

            (highlight     (modern-blend bg1 blue 0.9))

            (ttip          base2)
            (ttip-sl       bg4)
            (ttip-bg       bg2)


            )
  (create-modern-theme 'modern-dawn)
  (provide-theme 'modern-dawn)
)

