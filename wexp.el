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
  (length
   (setq wexp-articles
	 (loop for file in (directory-files dir t "\.xml$")
	       for dom = (wexp-parse-file file)
	       append (loop for elem in (dom-by-tag dom 'item)
			    unless (string-match
				    "\\.files\\."
				    (dom-text (dom-by-tag elem 'guid)))
			    collect elem)))))

(defun wexp-parse-file (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (xml-remove-comments (point-min) (point-max))
    (goto-char (point-min))
    (re-search-forward "<rss")
    (beginning-of-line)
    (libxml-parse-xml-region (point) (point-max))))

(defun wexp-find (match &optional sort-key)
  (with-temp-buffer
    (insert "<div class=\"index\">\n")
    (let ((comics
	   (loop for elem in wexp-articles
		 when (and
		       (string-match "<strong>"
				     (dom-text (dom-by-tag elem 'encoded)))
		       (string-match match
				     (dom-text (dom-by-tag elem 'encoded)))
		       (not (string-match
			     "__trashed"
			     (dom-text (dom-by-tag elem 'post_name)))))
		 append (wexp-format elem))))
      (setq comics (cl-sort
		    (cl-sort comics 'string<
			     :key (lambda (e)
				    (getf e :date)))
		    'string<
		    :key (lambda (e)
			   (getf e :year))))
      (when sort-key
	(setq comics (cl-sort comics 'string< :key sort-key)))
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
	   :title (replace-regexp-in-string "<[^>]+>" "" (getf comic :title))
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

(defun wexp-alphabetical-comics ()
  (wexp-find "<strong>"
	     (lambda (e)
	       (let ((string 
		      (replace-regexp-in-string
		       "^ +" "" (getf e :title))))
		 (replace-regexp-in-string "^The \\|^A \\|^An " "" string)))))

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

(defun wexp-generate-sidebar ()
  (switch-to-buffer "*sidebar*")
  (erase-buffer)
  (insert "<ul>\n")
  (dolist (article (cl-sort
		    (loop for elem in wexp-articles
			  when (string-match
				"<strong>" (dom-text
					    (dom-by-tag elem 'encoded)))
			  collect elem)
		    '<
		    :key (lambda (e)
			   (let ((title (dom-text (dom-by-tag e 'title))))
			     (and (string-match "^\\([0-9]+\\)" title)
				  (string-to-number (match-string 1 title)))))))
    (insert (format "<li><a href=%S>%s</a>\n"
		    (dom-text (dom-by-tag article 'link))
		    (dom-text (dom-by-tag article 'title)))))
  (insert "</ul>\n"))

(defun wexp-generate-sidebar-2 ()
  (switch-to-buffer "*sidebar*")
  (erase-buffer)
  (insert "<ul>\n")
  (dolist (article (cl-sort
		    (loop for elem in wexp-articles
			  unless (string-match
				"<strong>" (dom-text
					    (dom-by-tag elem 'encoded)))
			  collect elem)
		    '<
		    :key (lambda (e)
			   (let ((title (dom-text (dom-by-tag e 'title))))
			     (or (and (string-match "^\\([0-9]+\\)" title)
				      (string-to-number (match-string 1 title)))
				 0)))))
    (insert (format "<li><a href=%S>%s</a>\n"
		    (dom-text (dom-by-tag article 'link))
		    (dom-text (dom-by-tag article 'title)))))
  (insert "</ul>\n"))  

(defun wexp-find-category (category title &optional sort-key)
  (with-temp-buffer
    (insert "<div class=\"index\">\n")
    (let ((posts
	   (loop for elem in wexp-articles
		 when (and (loop for cat in (dom-by-tag elem 'category)
				 when (equalp category (dom-text cat))
				 return t)
			   (string-match
			    title (dom-text (dom-by-tag elem 'title))))
		 collect (wexp-format-category elem))))
      (setq posts (cl-sort
		    (cl-sort posts 'string<
			     :key (lambda (e)
				    (getf e :date)))
		    'string<
		    :key (lambda (e)
			   (getf e :year))))
      (when sort-key
	(setq posts (cl-sort posts 'string< :key sort-key)))
      (dolist (post posts)
	(insert (getf post :html) "\n")))
    (insert "</div>\n\n")
    (buffer-string)))

(defun wexp-format-category-tsp (elem)
  (let* ((html
	  (with-temp-buffer
	    (insert (dom-text (dom-by-tag elem 'encoded)))
	    (libxml-parse-html-region (point-min) (point-max))))
	 (title (dom-text (dom-by-tag elem 'title)))
	 (year (and (string-match "TSP\\([0-9]+\\)" title)
		    (match-string 1 title)))
	 (title-text (replace-regexp-in-string "^[^:]+: " "" title)))
    (list
     :year year
     :title title-text
     :html
     (format
      "<div><a href=\"/%s/%s/\"><img src=\"%s?w=150\"><br>%s (%s)</a></div>\n"
      (replace-regexp-in-string
       "-" "/" (car (split-string
		     (dom-text (dom-by-tag elem 'post_date)))))
      (dom-text (dom-by-tag elem 'post_name))
      (or
       (car
	(last
	 (loop for img in (dom-by-tag html 'img)
	       when (string-match "shot" (dom-attr img 'src))
	       collect (dom-attr img 'src))))
       "https://larsmagne23.files.wordpress.com/2015/09/scene-missing2.png")
      title-text
      year))))

(defun wexp-format-category-bergman (elem)
  (let* ((html
	  (with-temp-buffer
	    (insert (dom-text (dom-by-tag elem 'encoded)))
	    (libxml-parse-html-region (point-min) (point-max))))
	 (title (dom-text (dom-by-tag elem 'title)))
	 (year (and (string-match "BT[A-Z]+ \\([0-9]+\\)" title)
		    (match-string 1 title)))
	 (title-text (replace-regexp-in-string "^[^:]+: " "" title)))
    (list
     :year year
     :title title-text
     :html
     (format
      "<div><a href=\"/%s/%s/\"><img src=\"%s?w=150\"><br>%s (%s)</a></div>\n"
      (replace-regexp-in-string
       "-" "/" (car (split-string
		     (dom-text (dom-by-tag elem 'post_date)))))
      (dom-text (dom-by-tag elem 'post_name))
      (loop for img in (dom-by-tag html 'img)
	    when (string-match "shot" (dom-attr img 'src))
	    return (dom-attr img 'src))
      title-text
      year))))

(defun wexp-format-category (elem)
  (let* ((html
	  (with-temp-buffer
	    (insert (dom-text (dom-by-tag elem 'encoded)))
	    (libxml-parse-html-region (point-min) (point-max))))
	 (title (dom-text (dom-by-tag elem 'title)))
	 (year (and (string-match "FF\\([0-9]+\\)" title)
		    (match-string 1 title)))
	 (title-text (replace-regexp-in-string "^[^:]+: " "" title)))
    (list
     :year year
     :title title-text
     :html
     (format
      "<div><a href=\"/%s/%s/\"><img src=\"%s?w=150\"><br>%s (%s)</a></div>\n"
      (replace-regexp-in-string
       "-" "/" (car (split-string
		     (dom-text (dom-by-tag elem 'post_date)))))
      (dom-text (dom-by-tag elem 'post_name))
      (dom-attr (dom-by-tag html 'img) 'src)
      title-text
      year))))

(defun wexp-format-category-netflix (elem)
  (let* ((html
	  (with-temp-buffer
	    (insert (dom-text (dom-by-tag elem 'encoded)))
	    (libxml-parse-html-region (point-min) (point-max))))
	 (title (dom-text (dom-by-tag elem 'title)))
	 (date (and (string-match "NFLX2019 \\([^:]+\\)" title)
		    (match-string 1 title)))
	 (title-text (replace-regexp-in-string "^[^:]+: " "" title)))
    (setq a html)
    (list
     :date date
     :title title-text
     :html
     (format
      "<div><a href=\"/%s/%s/\"><img src=\"%s?w=150\"></a><br><a href=%S>%s</a><br><a href=%S>%s</a></div>\n"
      (replace-regexp-in-string
       "-" "/" (car (split-string
		     (dom-text (dom-by-tag elem 'post_date)))))
      (dom-text (dom-by-tag elem 'post_name))
      (loop for img in (dom-by-tag html 'img)
	    when (string-match "img_" (dom-attr img 'src))
	    return (replace-regexp-in-string "[?]w=[0-9]+" ""
					     (dom-attr img 'src)))
      (loop for link in (dom-by-tag html 'a)
	    for href = (dom-attr link 'href)
	    when (string-match "wikipedia" href)
	    return href)
      title-text
      (loop for link in (dom-by-tag html 'a)
	    for href = (dom-attr link 'href)
	    when (string-match "imdb.com" href)
	    return href)
      (replace-regexp-in-string "[^☆★]" "" (dom-texts html))
      ))))

;; (length (setq urls (wexp-attachments "~/Downloads/randomthoughts.WordPress.2020-03-07.xml")))

(defun wexp-attachments (file)
  (let ((files nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward "<wp:attachment_url.*?\\(http[^]]+\\)" nil t)
	(push (match-string 1) files)))
    (nreverse files)))

(defun wexp-download-images (urls)
  (mapc 'wexp-download-image urls))

(defun wexp-download-image (url)
  (message "%s" url)
  (let* ((parsed (url-generic-parse-url url))
	 (path (url-filename parsed))
	 (dest (concat "~/Download" path)))
    (unless (file-exists-p (file-name-directory dest))
      (make-directory (file-name-directory dest) t))
    (unless (file-exists-p dest)
      (with-current-buffer (url-retrieve-synchronously url t)
	(goto-char (point-min))
	(when (re-search-forward "\n\n" nil t)
	  (write-region (point) (point-max) dest)
	  (wexp-scale-image dest))
	(kill-buffer (current-buffer))))))

(defun wexp-scale-image (file)
  (let* ((image (create-image file nil nil :scale 1))
	 (size (image-size image t))
	 (sizes '((1024 768)
		  ;;(150 150)
		  (1200 800)
		  (1536 1152)
		  (300 225)
		  (768 576)
		  ;;(825 510)
		  )))
    (loop for (x y) in sizes
	  when (> (car size) x)
	  do (call-process "convert" nil nil nil
			   (expand-file-name file) "-resize" (format "%dx" x)
			   (wexp-suffixsize
			    (expand-file-name file)
			    (format "%dx%d" x
				    (round (* (/ x (float (car size)))
						(cdr size)))))))
    (image-flush image)))
    
(defun wexp-suffixsize (file suf)
  (replace-regexp-in-string "[.][^.]+$" (format "-%s\\&" suf) file))

(provide 'wexp)

;;; wexp.el ends here
