(require 'modern-common)

(deftheme modern-oceanicnext "Modern theme, the oceanicnext version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#D8DEE9")
            (base2   "#CDD3DE")
            (base3   "#C0C5CE")
            (base4   "#A7ADBA")

            (bg0     "#111B21")
            (bg1     "#1B2B34")
            (bg2     "#343D46")
            (bg3     "#4F5B66")
            (bg4     "#65737E")

            (aqua    "#5FB3B3")
            (green   "#99C794")
            (red     "#EC5f67")
            (blue    "#6699CC")
            (magenta "#AB7967")
            (violet  "#C594C5")
            (yellow  "#FAC863")
            (orange  "#F99157")

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

            (err           red)
            (war           orange)
            (suc           green)
            (comp          violet)

            (comment       base4)
            (comment-bg    bg2)
            (func          blue)
            (type          red)
            (var           yellow)
            (str           green)
            (keyword       violet)
            (const         orange)

            (highlight     (modern-blend bg1 blue 0.7))

            (ttip          base2)
            (ttip-sl       bg4)
            (ttip-bg       bg2)

            )
  (create-modern-theme 'modern-oceanicnext)
  (provide-theme 'modern-oceanicnext)
)

