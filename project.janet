(declare-project
  :name "rats"
  :author "Jona Ekenberg <saikyun@gmail.com>"
  :dependencies ["https://github.com/janet-lang/spork"

                 ## using my own fork due to additions to jaylib
                 "https://github.com/saikyun/freja-jaylib"
])

(declare-executable
  :name "rats"
  :entry "maze.janet")