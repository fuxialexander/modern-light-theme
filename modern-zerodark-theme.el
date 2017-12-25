(require 'modern-common)

(deftheme modern-zerodark "Modern theme, the zerodark version")

(let* (
            (class '((class color) (min-colors 89)))
;;; colors
            (base1   "#abb2bf")
            (base2   "#9FA6B2")
            (base3   "#939AA5")
            (base4   "#878E98")

            (bg0     "#282c34")
            (bg1     "#282c34")
            (bg2     "#24282f")
            (bg3     "#22252c")
            (bg4     "#181A1F")

            (aqua    "#5FB3B3")
            (green   "#98be65")
            (red     "#ff6c6b")
            (blue    "#61afef")
            (magenta "PeachPuff3")
            (violet  "#c678dd")
            (yellow  "#FAC863")
            (orange  "#da8548")

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
            (ttip-sl       bg2)
            (ttip-bg       bg2)

            )
  (create-modern-theme 'modern-zerodark)
  (provide-theme 'modern-zerodark)
)

