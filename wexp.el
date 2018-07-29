;;; wexp.el --- Interrogating Wordpress.com exports
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: wordpress

;; Wexp is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Wexp is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Wexp; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'dom)

(defvar wexp-articles nil)

(defun wexp-parse-directory (dir)
  (setq wexp-articles
	(loop for file in (directory-files dir t "\.xml$")
	      for dom = (wexp-parse-file file)
	      append (loop for elem in (dom-by-tag dom 'item)
			   unless (string-match
				   "\\.files\\."
				   (dom-text (dom-by-tag elem 'guid)))
			   collect elem))))

(defun wexp-parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (xml-remove-comments (point-min) (point-max))
    (libxml-parse-xml-region (point-min) (point-max))))

(defun wexp-find (match)
  (with-temp-buffer
    (insert "<table>\n")
    (dolist (elem wexp-articles)
      (when (string-match match (dom-text (dom-by-tag elem 'encoded)))
	(wexp-format elem)))
    (insert "</table>\n\n")
    (buffer-string)))

(defun wexp-format (elem)
  (let ((text (dom-text (dom-by-tag elem 'encoded)))
	(comics nil)
	(covers nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (save-restriction
	(narrow-to-region (point) (search-forward "</strong>"))
	(goto-char (point-min))
	(while (re-search-forward "\\(.*?\\) (\\([0-9][0-9][0-9][0-9]\\))\\( *[-#0-9]+,?\\)?" nil t)
	  (push (list :title (match-string 1)
		      :year (match-string 2)
		      :issues (match-string 3))
		comics)))
      (goto-char (point-min))
      (while (re-search-forward "src=.\\(https[^\"]+[0-9]-e.jpg\\)" nil t)
	(push (match-string 1) covers))
      (unless covers
	(when (re-search-forward "src=.\\(https[^\"]+[0-9].jpg\\)" nil t)
	  (push (match-string 1) covers)))
      (setq covers (nreverse covers)))
    (dolist (comic (nreverse comics))
      (insert
       (format
	"<tr><td>%s<td><img src=\"%s?w=150\"><td><a href=\"https://totaleclipse.blog/%s/%s/\">%s %s</a> <a href=%S>Alt</a></tr>\n"
	(getf comic :year)
	(if (> (length covers) 1)
	    (pop covers)
	  (car covers))
	(replace-regexp-in-string
	 "-" "/" (car (split-string
		       (dom-text (dom-by-tag elem 'post_date)))))
	(dom-text (dom-by-tag elem 'post_name))
	(replace-regexp-in-string "<[^>]+>" "" (getf comic :title))
	(replace-regexp-in-string "," "" (or (getf comic :issues) ""))
	
	(dom-text (dom-by-tag elem 'link)))))))

(defun wexp-all-comics ()
  (with-temp-buffer
    (insert "<table>\n")
    (let ((start (point)))
      (dolist (elem wexp-articles)
	(when (string-match "<strong>" (dom-text (dom-by-tag elem 'encoded)))
	  (wexp-format elem)))
      (sort-lines nil start (point)))
    (insert "</table>\n\n")
    (buffer-string)))

(provide 'wexp)

;;; wexp.el ends here
