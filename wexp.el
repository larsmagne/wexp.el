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
    (let ((comics
	   (loop for elem in wexp-articles
		 when (and
		       (string-match "<strong>"
				     (dom-text (dom-by-tag elem 'encoded)))
		       (string-match match
				     (dom-text (dom-by-tag elem 'encoded))))
		 append (wexp-format elem))))
      (setq comics (cl-sort
		    (cl-sort comics 'string<
			     :key (lambda (e)
				    (getf e :date)))
		    'string<
		    :key (lambda (e)
			   (getf e :year))))
      (dolist (comic comics)
	(insert (getf comic :html) "\n")))
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
	(while (search-forward "\n" nil t)
	  (replace-match " " t t))
	(goto-char (point-min))
	(while (re-search-forward "\\(.*?\\) (\\([0-9][0-9][0-9][0-9]\\))\\( *[-#0-9]+,?\\)?[, ]*" nil t)
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
    (loop for comic in (nreverse comics)
	  collect
	  (list
	   :year (getf comic :year)
	   :date (dom-text (dom-by-tag elem 'post_date))
	   :html
	   (format
	    "<td><a href=\"https://totaleclipse.blog/%s/%s/\"><img src=\"%s?w=150\"><br>\n%s (%s)\n%s</a>%s\n"
	    (replace-regexp-in-string
	     "-" "/" (car (split-string
			   (dom-text (dom-by-tag elem 'post_date)))))
	    (dom-text (dom-by-tag elem 'post_name))
	    (if (> (length covers) 1)
		(pop covers)
	      (car covers))
	    (replace-regexp-in-string "<[^>]+>" "" (getf comic :title))
	    (getf comic :year)
	    (replace-regexp-in-string "," "" (or (getf comic :issues) ""))
	    (if (string-match "p=" (dom-text (dom-by-tag elem 'link)))
		(format " <a href=%S>Alt</a>"
			(dom-text (dom-by-tag elem 'link)))
	      ""))))))

(defun wexp-all-comics ()
  (wexp-find "<strong>"))

(defun wexp-divide-lines ()
  (interactive)
  (save-excursion
    (re-search-backward "<table[> ]")
    (let ((i 0)
	  (end (save-excursion (search-forward "</table>"))))
      (while (re-search-forward "<td>" end t)
	(when (zerop (mod i 5))
	  (beginning-of-line)
	  (unless (zerop i)
	    (insert "</tr>\n"))
	  (insert "<tr>\n"))
	(incf i))
      (search-forward "</table>")
      (beginning-of-line)
      (loop for j from (mod i 5) upto 4
	    do (insert "<td>\n"))
      (insert "</tr>\n"))))

(defun wexp-remove-alt ()
  (interactive)
  (save-excursion
    (re-search-backward "<table[> ]")
    (let ((i 0)
	  (end (save-excursion (search-forward "</table>"))))
      (while (re-search-forward " <a href.*>Alt</a>" end t)
	(replace-match "")))))

(defun wexp-prepare-table ()
  (wexp-remove-alt)
  (wexp-divide-lines)
  (wexp-unfold))

(defun wexp-unfold ()
  (interactive)
  (save-excursion
    (re-search-backward "<table[> ]")
    (let ((i 0)
	  (end (save-excursion (search-forward "</table>"))))
      (while (re-search-forward "<br>\n\\(.*\\)\n" end t)
	(replace-match "\\1")))))
  

(provide 'wexp)

;;; wexp.el ends here
