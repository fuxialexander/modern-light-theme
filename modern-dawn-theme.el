(require 'modern-common)

(deftheme modern-dawn "Modern theme, the dawn version")

(let (
            (class '((class color) (min-colors 89)))
;;; colors
            (aqua          "#DC9E66")
            (green         "#82D88F")
            (red           "#D87184")
            (blue          "#1E95CC")
            (magenta       "#8778D8")
            (yellow        "#FFED6A")

            (blue-l     (colir-blend ,bg1 ,blue 0.4))
            (blue-d     (colir-blend ,bg1 ,blue 0.2))
            (blue-bg    (colir-blend ,bg1 ,blue 0.8))

            (red-l      (colir-blend ,bg1 ,red 0.4))
            (red-d      (colir-blend ,bg1 ,red 0.2))
            (red-bg     (colir-blend ,bg1 ,red 0.8))

            (yellow-l   (colir-blend ,bg1 ,yellow 0.4))
            (yellow-d   (colir-blend ,bg1 ,yellow 0.2))
            (yellow-bg  (colir-blend ,bg1 ,yellow 0.8))

            (magenta-l  (colir-blend ,bg1 ,magenta 0.4))
            (magenta-d  (colir-blend ,bg1 ,magenta 0.2))
            (magenta-bg (colir-blend ,bg1 ,magenta 0.8))

            (green-l    (colir-blend ,bg1 ,green 0.4))
            (green-d    (colir-blend ,bg1 ,green 0.2))
            (green-bg   (colir-blend ,bg1 ,green 0.8))

            (aqua-l     (colir-blend ,bg1 ,aqua 0.4))
            (aqua-d     (colir-blend ,bg1 ,aqua 0.2))
            (aqua-bg    (colir-blend ,bg1 ,aqua 0.8))


;;; General
            (act1          "#18303F")
            (act2          "#18303F")
            (base          "#C9BAAC")
            (base2       "#DDD1C6")
            (base3      "#AAA198")
            (base4      "#AAA198")
            (bg1           "#101F29")
            (bg2           "#132632")
            (bg3           "#18303F")
            (bg4           "#214054")

            (cblk          "#AAA198")
            (cblk-bg       "#18303F")
            (cblk-ln       "#519CCD")
            (cblk-ln-bg    "#214054")
            (cursor        "#648DFF")
            (const         "#8CB6E1")
            (comment       "#519CCD")
            (comment-light "#519CCD")
            (comment-bg    "#18303F")
            (comp          "#D87184")
            (err           "#E46261")
            (func          "#648DFF")

            (highlight     "#648DFF")
            (highlight-dim "#8CB6E1")
            (keyword       "#648DFF")
            (lnum          "#44505c")
            (str           "#64C2FF")
            (suc           "#82D88F")
            (ttip          "#DDD1C6")
            (ttip-sl       "#2C5672")
            (ttip-bg       "#214054")
            (type          "#9C75ED")
            (var           "#FFED6A")
            (war           "#DC9E66")

            )
  (create-modern-theme 'modern-dawn)
  (provide-theme 'modern-dawn)
)

