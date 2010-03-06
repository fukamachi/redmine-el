;;;; redmine.el --- See Redmine on Emacs

;; Copyright (C) 2010  Eitarow Fukamachi <e.arrows@gmail.com>

;; Author: Eitarow Fukamachi <e.arrows@gmail.com>
;; Twitter: http://twitter.com/nitro_idiot
;; Blog: http://e-arrows.sakura.ne.jp/
;;
;; Created: Mar 5, 2010
;; Version: 0.02
;; Keywords: redmine web bts

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
;;       '(("OpenPNE3" . "http://redmine.openpne.jp/projects/op3/")
;;         ("Redmine" . "http://www.redmine.org/projects/redmine/")))
;; (setq redmine-key "Your Private Access Key")

;;; Code:

(require 'anything)
(require 'xml)
(eval-when-compile (require 'cl))

;;====================
;; Configurations
;;====================
(defvar redmine-key nil "An access key for your private")
(defvar redmine-project nil "Use this project as default")
(defvar redmine-project-alist '(nil) "Your Redmines assoc list")

;;====================
;; Uritilites
;;====================
(defun redmine-get-xml (uri)
  (car (with-temp-buffer
         (call-process "curl" nil (current-buffer) nil uri "-s")
         (xml-parse-region (point-min) (point-max)))))

(defun redmine-xml->entries (xml)
  (xml-get-children xml 'entry))

(defun redmine-uri (action type)
  (format "%s%s?format=%s&key=%s"
          (if redmine-project
              (cdr (assoc redmine-project redmine-project-alist))
            (cdar redmine-project-alist))
          action type (or redmine-key "")))

(defun redmine-project-put-first! (key)
  (setq redmine-project-alist
        (cons
         (assoc key redmine-project-alist)
         (remove-if (lambda (e) (equal key (car e))) redmine-project-alist))))

;;====================
;; Entry Methods
;;====================
(defun redmine-entry-get-attr (entry attr)
  (caddar (xml-get-children entry attr)))

;;====================
;; Main
;;====================
(defun redmine-get-tickets (uri)
  (let ((entries (redmine-xml->entries (redmine-get-xml uri))))
    (mapcar (lambda (e) (concat (redmine-entry-get-attr e 'title) "\t" (redmine-entry-get-attr e 'id))) entries)))

(defun redmine-show-anything (lbl uri)
  (anything
   `(((name . ,lbl)
      (candidates . ,(redmine-get-tickets uri))
      (candidate-transformer . (lambda (candidates)
                                 (mapcar
                                  (lambda (c) (apply 'cons (split-string c "\t")))
                                  candidates)))
      (volatile)
      (action . browse-url)))))

(defun redmine-show-issues-all ()
  (interactive)
  (redmine-show-anything "Tickets" (redmine-uri "issues" "atom")))

(defun redmine-show-activity ()
  (interactive)
  (redmine-show-anything "Activity" (redmine-uri "activity" "atom")))

(defun redmine-show-revisions ()
  (interactive)
  (redmine-show-anything "Revisions" (redmine-uri "repository/revisions" "atom")))

(defun redmine-select-project ()
  (interactive)
  (anything
   `(((name . "Select Project")
      (candidates . ,(mapcar 'car redmine-project-alist))
      (volatile)
      (action . (lambda (p)
                  (setq redmine-project p)
                  (redmine-project-put-first! p)
                  (message (format "Redmine Project was changed to \"%s\"." p))))))))

(provide 'redmine)
;;; redmine.el ends here
