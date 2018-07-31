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
    (insert "<div class=\"index\">\n")
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
    (insert "</div>\n\n")
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
	    "<div><a href=\"https://totaleclipse.blog/%s/%s/\"><img src=\"%s?w=150\"><br>\n%s (%s)\n%s</a>%s</div>\n"
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

(defun wexp-remove-alt ()
  (interactive)
  (save-excursion
    (re-search-backward "<div ")
    (let ((i 0)
	  (end (save-excursion (re-search-forward "^</div>"))))
      (while (re-search-forward " <a href.*>Alt</a>" end t)
	(replace-match "")))))

(defun wexp-prepare-table ()
  (interactive)
  (wexp-remove-alt)
  (wexp-unfold))

(defun wexp-unfold ()
  (interactive)
  (save-excursion
    (re-search-backward "<div ")
    (let ((i 0)
	  (end (save-excursion (re-search-forward "^</div>"))))
      (while (re-search-forward "<br>\n\\(.*\\)\n" end t)
	(replace-match "\\1")))))

(defun wexp-transform-table ()
  (interactive)
  (save-excursion
    (re-search-backward "<table\\([> ]\\)")
    (replace-match "<div\\1")
    (let ((i 0)
	  (start (point))
	  (end (save-excursion (search-forward "</table>"))))
      (while (re-search-forward "<tr>\\|</tr>" nil t)
	(replace-match "" t t))
      (goto-char start)
      (while (re-search-forward "td>" nil t)
	(replace-match "div>" t t))
      (search-forward "</table>")
      (replace-match "</div>"))))

(provide 'wexp)

;;; wexp.el ends here
