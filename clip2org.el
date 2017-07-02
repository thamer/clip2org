;;; clip2org.el --- Convert Kindle's My Clippings.txt into Org

;; Author: Thamer Mahmoud <thamer.mahmoud@gmail.com>
;; Version: 1.2
;; Time-stamp: <2017-07-03 01:01:35 thamer>
;; URL: https://github.com/thamer/clip2org
;; Keywords: outlines
;; Compatibility: Tested on GNU Emacs 23.4 and 24.1
;; Copyright (C) 2012-7 Thamer Mahmoud, all rights reserved.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package converts Amazon Kindle's "My Clippings.txt" to a format
;; useable in Org mode. The result will be sorted by book title and
;; displayed in a temporary buffer named "*clippings*".
;;
;;; Install:
;;
;; Put this file in your Emacs-Lisp load path, and add the following
;; into your $HOME/.emacs startup file.
;;
;;     (require 'clip2org)
;;     (setq clip2org-clippings-file "/path/to/My Clippings.txt")
;;
;; You can also use the customize interface to see all the options:
;;
;;     M-x customize-group <ENTER> clip2org
;;
;;; Usage:
;;
;; After setting clip2org-clippings-file, do:
;;
;;     M-x clip2org
;;
;;; Code
(defgroup clip2org nil "Convert Kindle's My Clippings.txt into Org"
  :group 'org)

(defcustom clip2org-clippings-file
  (convert-standard-filename "~/My Clippings.txt")
  "Path to My Clippings.txt, including file name."
  :type 'file
  :group 'clip2org)

(defcustom clip2org-include-pdf-links nil
  "If t, add PDF page links under each clipping. See also
clip2org-include-pdf-folder."
  :type 'boolean
  :group 'clip2org)

(defcustom clip2org-include-pdf-folder ""
  "Folder used to generate page links to pdf files."
  :type 'file
  :group 'clip2org)

(defcustom clip2org-include-date nil
  "If t, include added date information as an Org property."
  :type 'boolean
  :group 'clip2org)

(defun clip2org-get-next-book-as-list ()
  (let (title is-highlight header loc date page start end content)
    (setq start (point))
    (if (not (re-search-forward "==========" nil t 1))
        ;; Return nil
        nil
      (setq end (point))
      (goto-char start)
      (setq title (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
      (when (re-search-forward "Highlight" end t 1)
        (setq is-highlight t))
      (beginning-of-line)
      (when (re-search-forward "- \\(.*\\)|" end t 1)
        (setq header (match-string 1)))
      (beginning-of-line)
      (when (re-search-forward "Page \\([0-9-]+\\)" end t 1)
        (setq page (match-string 1)))
      (when (re-search-forward "Loc. \\([0-9-]+\\)" end t 1)
        (setq loc (match-string 1)))
      (when (re-search-forward "Added on \\(.*\\)\n" end t 1)
        (setq date (match-string 1)))
      ;; From the end of date to ==========
      (if (re-search-forward
           "\n\\(.*?\\)\n==========" end t 1)
          (setq content (match-string 1)))
      (when (equal title "==========")
        (error "Clip2org: failed in getting content or quoted text."))
      (message (format "Clip2org: now processing \"%s\"" title))
      (forward-line)
      ;; Return list
      (list title is-highlight page loc date content header))))

(defun clip2org-convert-to-org (clist)
  "Process clip2org-alist and generate the output buffer."
  (with-current-buffer (get-buffer-create "*clippings*")
    (delete-region (point-min) (point-max))
    ;; Process headers of each book
    (while (caar clist)
      (insert "\n* " (caar clist))
      (let ((note-list (cdar clist)))
        ;; Process each clipping
        (while (car note-list)
          (let* ((item (car note-list))
                 (is-highlight (nth 0 item))
                 (page (nth 1 item))
                 (loc (nth 2 item))
                 (date (nth 3 item))
                 (content (nth 4 item)))
            (if (not is-highlight)
                (insert "\n** " content "\n")
              (insert "\n** ")
              (when page
                (insert "Page " page " "))
              (when loc
                (insert "Loc. " loc " "))
              (insert "\n"))
            (when clip2org-include-date
              (insert ":PROPERTIES:\n")
              (insert ":DATE: " date "\n")
              (insert ":END:\n\n"))
            (when is-highlight
                (insert content "\n"))
              ;; Insert pdf link
              (if (and clip2org-include-pdf-links page)
                  (insert (concat "[[docview:" clip2org-include-pdf-folder
                                  (caar clist) ".pdf"
                                  "::" page "][View Page]]\n"))))
          (setq note-list (cdr note-list))))
      ;; Increment to the next book
      (setq clist (cdr clist))))
  (switch-to-buffer "*clippings*")
  (org-mode))

(defun clip2org-append-to-alist-key (key value alist)
  "Append a value to the key part of an alist. This function is
used to create associated lists. If Key is not found, add new key
to the list"
  (let ((templ) (results) (found))
    (while alist
      ;; check if key is already in list
      (if (equal (caar alist) key)
          (progn
            (setq found t)
            (setq templ (list (nconc (car alist) (list value) templ)))
            ;; increment while loop
            (setq alist (cdr alist))
            ;; add/create to a new list
            (setq results (append results templ)))
        (progn
          (setq results (append (list (car alist)) results))
          (setq alist (cdr alist)))))
    ;; add the new key/value to old list
    (if (not (eq found t))
        (setq results (append (list (list key value)) results)))
    results))

(defun clip2org ()
  (interactive)
  (save-excursion
    (find-file clip2org-clippings-file)
    (goto-char (point-min))
    (let (clist (booklist (clip2org-get-next-book-as-list)))
      (while booklist
	(setq clist (clip2org-append-to-alist-key
		     (car booklist) (cdr booklist) clist))
	(setq booklist (clip2org-get-next-book-as-list)))
      (clip2org-convert-to-org clist))))

(provide 'clip2org)
;;; clip2org.el ends here.
