(declare-project
  :name "rats"
  :author "Jona Ekenberg <saikyun@gmail.com>"
  :dependencies ["https://github.com/janet-lang/spork"

                 ## using my own fork due to additions to jaylib
                 "https://github.com/saikyun/freja-jaylib"

                 # for vector math
                 "https://github.com/saikyun/freja"])

(declare-executable
  :name "rats"
  :entry "maze.janet")
