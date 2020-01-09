(in-package #:cl-user)

(:import+ #:cl-dot)

(:include "uiop+")

(defun :visualize-graph (graph objects &optional attributes)
  "Generate a graph via `cl-dot:generate-graph' to a temporary file and open it in the default image viewer application."
  (let ((dgraph (cl-dot:generate-graph-from-roots graph objects attributes)))
    (uiop:with-temporary-file (:pathname pathname :keep t :type "png")
      (cl-dot:dot-graph dgraph pathname :format :png)
      (:open-in-default-application pathname)
      pathname)))
