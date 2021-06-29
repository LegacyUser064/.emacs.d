;;; util.el -*- lexical-binding: t -*-

;;;###autoload
(defmacro command-wrap (func-call)
  "A macro that wraps a function call in an interactive lambda"
  `(lambda ()
     (interactive)
     ,func-call))

(provide 'util)
