;;;; redmine.el --- See Redmine on Emacs

;; Copyright (C) 2010  Eitarow Fukamachi <e.arrows@gmail.com>

;; Author: Eitarow Fukamachi <e.arrows@gmail.com>
;; Twitter: http://twitter.com/nitro_idiot
;; Blog: http://e-arrows.sakura.ne.jp/
;;
;; Created: Mar 5, 2010
;; Version: 0.2.1
;; Keywords: redmine web

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; redmine.el is an utility for redmine.el
;; You can see lists of issues, activity and revisions with anything.el,
;;   and view the detail with your browser.

;;; Commands:

;; `redmine-show-issues-all'
;;   Display the recent issues with anything.el
;; `redmine-show-activity'
;;   Display the recent activity with anything.el
;; `redmine-show-revisions'
;;   Display the recent revisions with anything.el
;; `redmine-select-project'
;;   Change a project

;;; Settings:

;; Put informations of a Redmine to your .emacs.el.
;; See below for example.

;; (require 'redmine)
;; (setq redmine-project-alist
;;       '(("OpenPNE3" "http://redmine.openpne.jp/projects/op3/" "Your API Key (optional)")
;;         ("Redmine" "http://www.redmine.org/projects/redmine/")))

;;; Code:

(require 'anything)
(require 'xml)
(require 'url)
(eval-when-compile (require 'cl))

;;====================
;; Configurations
;;====================
(defvar redmine-project nil "Use this project as default")
(defvar redmine-project-alist '(nil) "Your Redmines assoc list")
(defvar anything-redmine-buffer-name "*anything-redmine*")

;;====================
;; For XML
;;====================
(defun redmine-get-xml (uri)
  (car (with-temp-buffer
         (url-insert-file-contents uri)
         (xml-parse-region (point-min) (point-max)))))

(defun redmine-xml->entries (xml)
  (xml-get-children xml 'entry))

(defun redmine-entry-get-attr (entry attr)
  (caddar (xml-get-children entry attr)))

;;====================
;; Project Methods
;;====================
(defun redmine-current-project ()
  (or (assoc redmine-project redmine-project-alist) (car redmine-project-alist)))

(defun redmine-project-name (&optional project)
  (car (or project (redmine-current-project))))

(defun redmine-project-uri (&optional project)
  (cadr (or project (redmine-current-project))))

(defun redmine-project-key (&optional project)
  (caddr (or project (redmine-current-project))))

(defun redmine-action-uri (action &optional project type)
  (format "%s%s?format=%s&key=%s"
          (redmine-project-uri project)
          action
          (or type "atom")
          (or (redmine-project-key project) "")))

(defun redmine-project-entries (action &optional project)
  (let* ((uri (redmine-action-uri action project))
         (entries (redmine-xml->entries (redmine-get-xml uri))))
    (mapcar (lambda (e) (concat (redmine-entry-get-attr e 'title) "\t" (redmine-entry-get-attr e 'id))) entries)))

;;====================
;; Functions
;;====================
(defun redmine-project-put-first! (name)
  (setq redmine-project-alist
        (cons
         (assoc name redmine-project-alist)
         (remove-if (lambda (e) (equal name (car e))) redmine-project-alist))))

(defun redmine-show-anything (lbl action &optional project)
  (anything
   `(((name . ,lbl)
      (candidates . ,(redmine-project-entries action project))
      (candidate-transformer . (lambda (candidates)
                                 (mapcar
                                  (lambda (c) (apply 'cons (split-string c "\t")))
                                  candidates)))
      (volatile)
      (migemo)
      (action . browse-url)))
   nil nil nil nil anything-redmine-buffer-name))

;;====================
;; Main
;;====================
(defun redmine-show-issues-all ()
  (interactive)
  (redmine-show-anything "Tickets" "issues"))

(defun redmine-show-activity ()
  (interactive)
  (redmine-show-anything "Activity" "activity"))

(defun redmine-show-revisions ()
  (interactive)
  (redmine-show-anything "Revisions" "repository/revisions"))

(defun redmine-select-project ()
  (interactive)
  (anything
   `(((name . "Select Project")
      (candidates . ,(mapcar 'car redmine-project-alist))
      (volatile)
      (action . (lambda (project-name)
                  (setq redmine-project project-name)
                  (redmine-project-put-first! project-name)
                  (message (format "Redmine Project was changed to \"%s\"." project-name))))))))

(provide 'redmine)
;;; redmine.el ends here
