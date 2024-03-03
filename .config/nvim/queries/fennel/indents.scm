[
 (list)
 (table)
 (sequence)
] @indent.begin

((table) @indent.align
 (#set! indent.open_delimiter "{")
 (#set! indent.close_delimiter "}"))

((sequence) @indent.align
 (#set! indent.open_delimiter "[")
 (#set! indent.close_delimiter "]"))

[
  ")"
  "}"
  "]"
] @indent.end

(comment) @indent.ignore
