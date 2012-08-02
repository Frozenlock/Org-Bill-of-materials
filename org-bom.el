;;; org-bom.el --- Collect components across the entire org buffer

;    Copyright 2011 Free Software Foundation, Inc.
;
;    Filename: org-bom.el
;    Version: 0.4
;    Author: Christian Fortin <frozenlock@gmail.com>
;    Keywords: org, bill-of-materials, collection, tables
;    Description: Create a bill-of-materials (bom) of the entire org buffer
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;=====================================================
; The program begins here
;=====================================================

;;; Code:

(require 'cl)
(require 'org)
(require 'org-table)
(require 'gnus-util)

;========== Global variable section ==========

(defvar org-bom-database nil
  "Global variable used to build a database of the components used, as
well as their section, tags and quantity.")

(defvar org-bom-details nil
  "Need to be given by the user. A suggested use is to bind it to
a local user's database. Should be a plist with at least \":name\" and
\":description\". It should also contain \":datasheetPdf\" in order
to use the bom-datasheet dynamic block.")

(defvar org-bom-update-enable t
  "Scan the buffer and update the BOM when a dynamic block is
refreshed. Should be set to nil for a buffer-wide dynamic block,
such as with `org-update-all-dblocks'. However, be sure to update
manually with `org-bom-total' in this case.")

(defcustom org-bom-latex-mode nil
  "If activated, every component's name will be replaced by a
reference to the datasheet and comments might be activated if
necessary (large number of tags). See `org-bom-latex-max-tags'.
Requires LaTeX package PDFCOMMENT, PDFPAGES and HYPERREF."
  :type 'boolean
  :group 'org-bom)

(defcustom org-bom-latex-max-tags 10
  "Define the maximum number before the tags start being hidden in a
PDF comment. Set to nil to disable."
  :type 'integer
  :group 'org-bom)

(defcustom org-bom-latex-datasheetPath ""
  "Default path to your datasheet."
  :type 'string
  :group 'org-bom)

;========== Database section ==========

(defun org-bom-add-component (comp)
  (push comp org-bom-database))

(defstruct component name section quantity tag)

(defun org-bom-select-in-db (database selector-fn value &optional remove part-match)
 "Return every entry in the database which has the corresponding
value for a given selector. Can be the DATABASE's argument of
itself in case of multiple SELECTOR-FN. The SELECTOR-FN must be
the quoted function, such as 'component-name. If REMOVE is
non-nil, every entry with a match will be discarded rather than
keeped. If PART-MATCH is non-nil, `string-match' function is used
instead of `gnus-string-equal'."
 (when (atom value)
   (setf value (list value)))
 (let ((temp-results database)
       (results nil))
   (dolist (current-value value)
     (setf temp-results
	   (funcall (if remove 'remove-if 'remove-if-not)
		    #'(lambda (component)
			(let ((current-component (funcall selector-fn component)))
			  (if (numberp current-component);if it's a component quantity
			      (equal current-component current-value)
			    (if part-match (string-match current-value current-component)
			      (gnus-string-equal current-component current-value)))))
		    (if remove temp-results database)))
     (unless remove (setf results (append results temp-results)))); cumulate the results
   (when remove (setf results temp-results))
   (org-bom-sort results)))

(defun org-bom-check-and-push-to-db (name section quantity tag)
  "Check if the combo name-section is already in the database. If it
is, add the quantity and the tag, otherwise create a new entry."
  (let ((exists-flag nil))
    (dolist (temp-car-db org-bom-database) ;For every item in the database...
      (when (and (gnus-string-equal (component-name temp-car-db)
				    name)
		 (gnus-string-equal (component-section temp-car-db)
				    section))
	(setf (component-quantity temp-car-db)
	      (+ (component-quantity temp-car-db) quantity)) ; if the combo name-section exists, simply add the quantity
	(setf (component-tag temp-car-db)
	      (append tag (component-tag temp-car-db)))
	(setf exists-flag t))) ; set the exist flag t
    (if (not exists-flag) (org-bom-add-component (make-component :name name ; if it's a new component (in the section), then add it in the database
						      :section section
						      :quantity quantity
						      :tag tag)))))

;========== End of database section ==========

(defun org-bom-total (&optional section-override)
  "Go to every tables in the buffer and get info from them."
  (interactive)
  (save-excursion
    (save-restriction
      (setq org-bom-database nil) ; Reset the database before each new buffer-wide scan
      (widen)
      (org-bom-prepare-linedata-for-database section-override) ;scan for line items
      (org-table-map-tables (lambda () (org-bom-prepare-tabledata-for-database
					section-override)) t)
      (setq org-bom-database
	    (org-bom-sort org-bom-database))))
  (message "org-bom-total"))

(defun org-bom-sort (database)
  "Return the DATABASE sorted alphabetically by component name"
  (sort database ;sort in alphabetical order
	(lambda (arg1 arg2)
	  (gnus-string< (component-name arg1)
			(component-name arg2)))))

(defun org-bom-get-keyword-column-numbers ()
  "Return a list of plists composed of \"components\", \"qty\",
\"tag\" and \"section\" column numbers."
  (org-table-get-specials)
  (let ((column-names org-table-column-names)
	results
	component-col
	qty-col
	tag-col
	section-col
	new-section-col
	(push-the-list '(push (list :name component-col
				    :qty qty-col
				    :tag tag-col
				    :section section-col) results)))
    (while column-names
      (let* ((temp-name (pop column-names))
	     (name (car temp-name))
	     (ncolumn (string-to-number (cdr (last temp-name)))))
	(when (string-match "section" name)
	  (setq new-section-col ncolumn))
	(when (string-match "component" name)
	  (when component-col ;test if it's the first component column
	    (eval push-the-list));when new component, we know there's no further tag or qty
	  (setq qty-col nil tag-col nil);set them all to nil
	  (setq component-col ncolumn)
	  (setq section-col (or new-section-col section-col))
	  (setq new-section-col nil))
	(when (string-match "qty" name)
	  (push ncolumn qty-col))
	(when (string-match "tag" name)
	  (push ncolumn tag-col))))
    (eval push-the-list)
    results))

(defun org-bom-after-header-line ()
 "Go to and return the position of the first non-header line."
 (let ((beg (org-table-begin))
       (end (org-table-end)))
   (goto-char beg)
   (if (and (re-search-forward org-table-dataline-regexp end t)
	    (re-search-forward org-table-hline-regexp end t)
	    (re-search-forward org-table-dataline-regexp end t))
       (match-beginning 0))))

(defun org-bom-transfer ()
  "Scan the buffer for an org-mode comment \"#+BOM-XFER:\".
Everything before the
keys (:FROM-SECTION, :TO-SECTION, :PART-MATCH, :EVERYTHING) is
considered to be the component's name, except the last
whitespaces. The only required key is the :TO-SECTION. It
specifies in which section the component must be sent.
Unless :FROM-SECTION is provided, the section from which to
transfer the components will be following the same rule as
`org-bom-prepare-tabledata-for-database'. A :PART-MATCH argument
can be provided and follows the same rule as
`org-bom-select-in-db' for the section selection. To disregard
the :FROM-SECTION altogether and simply take every instance of a
component in the entire database, provide :EVERYTHING non-nil."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+BOM-XFER:[ \t]+\\([^:\n]+\\)\\(.*\\)?" nil t)
      (let* ((name (org-no-properties (match-string 1)))
	     (params (read (concat "(" (match-string 2) ")")))
	     (from-section (or (plist-get params :from-section) (org-bom-check-possible-section)))
	     (to-section (plist-get params :to-section))
	     (everything (plist-get params :everything))
	     (part-match (plist-get params :part-match)))
	(setq name (org-trim name))
	(let ((temp-database (org-bom-select-in-db org-bom-database 'component-name name)))
	  (unless everything
	    (setq temp-database (org-bom-select-in-db temp-database
						      'component-section
						      from-section
						      nil
						      part-match)))
	  (dolist (current-component temp-database)
	    (setf (component-section current-component) to-section)))))))

(defun org-bom-prepare-linedata-for-database (&optional section-override)
  "Scan the buffer and add line-components to database. Search
for an org-mode comment \"#+BOM:\". Everything before the
keys (:section, :qty, :tag) is considered to be the component's
name, except the last whitespaces. The \"section\" priority is in
this order: Given with the :section key, in a :SECTION: property,
or the org heading."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*#\\+BOM:[ \t]+\\([^:\n]+\\)\\(.*\\)?" nil t)
    (let* ((name (org-no-properties (match-string 1)))
	   (params (read (concat "(" (match-string 2) ")")))
	   (quantity (or (plist-get params :qty) 1))
	   (section-given (plist-get params :section))
	   (tag (plist-get params :tag)))
      (setq name (org-trim name))
      (when section-given
	(unless (stringp section-given)
	  (setq section-given (symbol-name section-given))))
      (when tag
	(unless (stringp tag)
	  (if (numberp tag)
	      (setq tag (number-to-string tag))
	    (setq tag (symbol-name tag)))))
      (org-bom-check-and-push-to-db
       name
       (or section-override
	   section-given
	   (org-bom-check-possible-section))
      quantity
      (list (list tag))))));double `list' because there's a list per tag and a list per item


(defun org-bom-check-possible-section ()
  "Return a possible section from properties or heading"
  (let ((section-property (org-entry-get nil "SECTION" 'selective)))
    (when section-property
      (if (string= "" section-property)
	  (setq section-property nil))) ;; set to nil if empty string
    (or section-property
	(if (org-before-first-heading-p)
	    "" ; If we are before the first heading, default to "".
	  (substring-no-properties (org-get-heading t t))))))

(defun org-bom-prepare-tabledata-for-database (&optional section-override)
  "Scan in the current table for any column named as \"Component\". If
a name in the \"Component\" column starts with the '-' character, it
will be escaped. Optional info \"section\" must be somewhere before
the components' column. If no section is given, then will check for
a :SECTION: property. If none is found, the heading will be taken
as a section. A section-override will asign every single component
to this section. Optional info \"Qty\" and \"Tag\" should be a
column somewhere after the components column, as many times as
needed. To add another components column, simply add another
\"Component\". Note that if a \"Qty\" column is present, it will
default to 0 if the field is empty. This gives the possibility to
have many quantity columns without the need to enter 0 multiple
times."

  (unless (org-at-table-p) (error "Not at a table"))
  (org-bom-after-header-line)
  (forward-line -1) ;Don't go on the first dataline yet
  (let ((end (org-table-end))
	(beg (org-table-begin))
	(dline org-table-dataline-regexp)
	(possible-section (org-bom-check-possible-section)))
    (while (re-search-forward dline end t)
      (dolist (current-comp (org-bom-get-keyword-column-numbers))
	(when (plist-get current-comp :name) ;test if there's a component column
	(org-bom-check-and-push-to-db
	 (org-bom-comp-get-name (plist-get current-comp :name))
	 (or section-override
	     (org-bom-comp-get-section (plist-get current-comp :section)
				       possible-section))
	 (org-bom-comp-get-qty (plist-get current-comp :qty))
	 (org-bom-comp-get-tag (plist-get current-comp :tag))))))))



(defun org-bom-check-for-details-table ()
  "Scans the buffer to find \"#+TBLNAME: bom-details and add the data
in `org-bom-details'. Please use the form
(let ((org-bom-details (copy-tree org-bom-details))) before calling this
command, otherwise `org-bom-details' will be overwritten."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "#\\+TBLNAME: bom-details" nil t)
	(forward-line)
	(when (org-at-table-p)
	  (org-bom-add-details-from-table))))))


(defun org-bom-add-details-from-table ()
  "Scans the table for a special row (\"!\"), looking for \"name\"
\"description\", \"price\", and \"datasheetPdf\". Adds the data to
`org-bom-details' Warning, case-sensitive!."
  (org-table-get-specials) ;needed to refresh org-table-column-names
  (let ((column-names (nreverse org-table-column-names))
	(end (org-table-end))
	(dline org-table-dataline-regexp)
	(propertize '(lambda (arg) (read (concat ":" (car arg))))))
    (when (assoc "name" column-names);if there isn't a component name, do nothing
      (org-bom-after-header-line) (forward-line -1)
      (while (re-search-forward dline end t)
	(let ((temp-plist '()))
	  (dolist (current-column column-names)
	    (let ((property-value (org-bom-get-table-field
				   (string-to-number (cdr current-column)))))
	      (when (> (length property-value) 0)
		(push property-value temp-plist)
		(push (funcall propertize current-column)
		      temp-plist))))
	  (org-bom-add-or-replace-in-details temp-plist))))))


(defun org-bom-add-or-replace-in-details (plist)
  "Add or replace the plist in `org-bom-details', depending on
whether it already exists."
  (let ((component-details
	 (org-bom-get-current-component (plist-get plist :name))))
    (if component-details ;if the component already exists
	(let ((name-position (position ':name plist)))
	  (delete ;remove the name property
	   (nth name-position plist)
	   (delete ;remove the :name keyword
	    (nth name-position plist) plist))
	  (while plist
	    (plist-put component-details (pop plist) (pop plist))))
      (push plist org-bom-details))))


(defun org-bom-comp-get-tag (&optional column-number)
  "Retrieve the component-tag in the same row and apply some filter
functions."
  (let (temp-tag
	tag)
    (dolist (col column-number)
      (setq temp-tag (org-bom-get-table-field (org-table-goto-column col)))
      (when (> (length temp-tag) 0) (pushnew temp-tag tag)))
    (setq tag (org-bom-split-mix-tag tag)))) ; tags written as "foo-1, foo, bar," will be separated



(defun org-bom-comp-get-qty (&optional column-number)
  "Retrieve the component-qty in the same row and apply some filter
functions. If column-number is nil, default to 1."
  (let ((qty 0))
    (dolist (col column-number)
      (setq qty (+ qty (max 0 (string-to-number
			       (org-bom-get-table-field
				(org-table-goto-column col)))))))
    (if column-number qty 1)))


(defun org-bom-comp-get-name (column-number)
  "Retrieve the component-name in the same row and apply some filter
functions. (Remove footnotes, make \"-\" an escape character)"
  (let ((comp-name
	 (replace-regexp-in-string "\\[fn.*\\]" ""  ;Remove any footnotes [fn*]
				   (org-bom-get-table-field
				    (org-table-goto-column column-number)))))
    (if (string= "-" (if (> (length comp-name) 0)
			 (substring comp-name 0 1) ""))
	(setf comp-name "")) ;if the special character '-' is present, replace by an empty string
    comp-name))


(defun org-bom-comp-get-section (&optional column-number possible-section)
  "Retrieve the component-section at COLUMN-NUMBER in the same row,
or the POSSIBLE-SECTION."
  (or (when column-number
	(let ((field (org-bom-get-table-field
		      (org-table-goto-column column-number))))
	  (if (string= "" field) nil field)))
      possible-section))


(defun org-bom-get-table-field (&optional N REPLACE)
  "Same as `org-table-get-field', but with some string cleaning."
  (org-trim (substring-no-properties (org-table-get-field N))))

(defun org-bom-split-mix-tag (tag &optional separator)
  "Separate the tags and mix them. For example: '(\"foo, bar, foo\" \"do, ré, mi\")
would give '(\"foo\" \"do\") '(\"bar\" \"ré\") '(\"foo\" \"mi\") with the '\", \" separator."
  (let ((temp-tags tag) (new-tags nil))
    (dolist (single-string-tags temp-tags) ;separate the tags into single string
      (push (org-split-string single-string-tags (or separator ", ")) new-tags))
    (org-bom-mix-alternate new-tags)))


(defun org-bom-mix-alternate (list)
  "Create new lists composed alternatively of an element of each list"
  (let ((temp-list nil))
    (push (remove nil (mapcar 'car list)) temp-list) ;the first item is composed of the first element of each list
    (when (remove nil (mapcar 'cdr list)) ;while everything is not nil
      (setf temp-list (append temp-list (org-bom-mix-alternate (mapcar 'cdr list)))))
    temp-list))


(defun org-bom-list-to-tsv-file (list &optional filename column)
  "Export a list in a tsv file"
  (with-temp-file (or filename "list-export.txt")
    (let ((n-col (or column 1)))
      (dolist (single-item list)
	(if (> n-col 0)
	    (progn (setf n-col (1- n-col))
		   (insert single-item)
		   (if (> n-col 1)
		       (insert-tab)))
	  (setf n-col (1- (or column 1)))
	  (newline)
	  (insert single-item)
	  (if (> n-col 0)
	      (insert-tab)))))))

(defun org-bom-get-all (database selector-fn)
  "Return every different instance of a certain type in a single list. For
example, (org-bom-get-all org-bom-database 'component-tag) will return every
tag in the database. Empty strings are removed."
  (let ((collector nil))
    (dolist (current-component database)
      (let ((current-item (funcall selector-fn current-component)))
	(push current-item collector)))
    (delete-dups (remove "" collector))))


(defun org-bom-listify (list-with-lists)
  "Return everything contained in the argument (lists within lists) as
a plain list"
  (let ((new-list nil))
    (if (listp list-with-lists)
	(progn (dolist (temp-item list-with-lists) ;for each item
		 (if (atom temp-item)
		     (pushnew temp-item new-list) ;if it's an atom, add it to the list
		   (setf new-list (append new-list (org-bom-listify temp-item))))) ;otherwise listify it
	       (remove nil new-list)) ; remove any remaining 'nil' from the list
      (list list-with-lists))))

(defun org-bom-tag-to-list (&optional section-name remove part-match)
  "Return a list of all the tags in the section, those from the same
component in the same string. See `org-bom-select-in-db' for more details."
  (let ((results nil)
	(items (org-bom-get-all (if section-name
				   (org-bom-select-in-db org-bom-database
							 'component-section
							 section-name
							 remove
							 part-match)
				 org-bom-database) 'component-tag)))
    (dolist (current-item items)
      (dolist (current-tags current-item)
	(push (funcall '(lambda (x) (org-bom-concat-list (nreverse x) " ")) current-tags) results)))
    (sort (remove "" results) 'string<)))

(defun org-bom-tag-remove-to-list (section-name)
  "Return a list of all the tags NOT in the section. In case of
  multiple sections, add a \"+\" between."
  (setf section-name (org-split-string section-name "+")) ; convert the section-name in a list of string, so the user don't have to enter it as one
  (let ((list nil)
	(temp-tag nil)
	(component-db org-bom-database))
    (dolist (current-section-name section-name); For every section-name
      (setf component-db (org-bom-select-in-db
			  component-db
			  'component-section
			  current-section-name
			  'remove
			  'part-match))) ;remove any partly matching section-name
    (dolist (current-component component-db) ;; Put every tag in a list
      (dolist (single-component-tags (component-tag current-component))
	(push (org-bom-concat-list (org-bom-listify single-component-tags) " ") list)))
    (setf list (remove "" (sort list 'string<)))))

(defun org-bom-concat-list (list &optional separator)
  "Concatenate in a single string every string in the list with an
  optional separator, such as \" \"."
  (concat (car list) (unless (atom (cdr list))
		       (concat separator (org-bom-concat-list (cdr list) separator)))))

(defun org-dblock-write:bom (params)
  "Insert a table with every component gathered in the buffer.
See `org-bom-insert-table' for more details."
  (let ((org-bom-details (copy-tree org-bom-details)))
    (when org-bom-update-enable
      (org-bom-check-for-details-table)
      (org-bom-total); Scan the buffer and refresh the bill of materials
      (org-bom-transfer))
    (org-bom-insert-table params)
  (message "Bill of materials created")))

(defun org-bom-stringify (&optional argument)
  "If ARGUMENT is string, return unchanged. If it's a symbol,
return the symbol-name. If nil, return nil"
  (if (null argument)
      nil
    (cond ((not (stringp argument)) (symbol-name argument))
	  ((not (null argument)) argument))))


(defun org-bom-insert-table (params)
  "Insert a table with every component gathered in the buffer.

Set \":local-only\" to get components marked with the current heading
as their \"section\". Components with given section (either in a table
or a property) will NOT appear.

Set \":section\" to get a specified section only. Note that if a
section is given to a component, it won't appear in a local-only
table. In addition, set \"part-match\" to get partly matching
sections. In addition, a + sign will add an additionnal section. For
example: \":section A+B\" will retrieve section A and section B.

Set \":remove\" to remove the specified section and keep everything
else.

Set \":total\" to merge every section together and obtain a grand
total.

Set \":no-tag\" to remove the tags column.

Set \":description\" to insert a description column. You must have a
PLIST with \":name\" and \":description\" in it. The function will
search for a matching component's name and get its description. Copy
your property list to the variable `org-bom-details'.

Set \":org-bom-latex-max-tags\" to hide every remaining tags in a
pdf comment (need org-bom-latex-mode activated)

Set \":price\" to insert a price column. You must have a
PLIST with \":name\" and \":price\" in it. The function will
search for a matching component's name and get its price. Copy
your property list to the variable `org-bom-details'.

The columns' name can be set with :col-name-tag, :col-name-component,
:col-name-section, :col-name-quantity, col-name-price and
col-name-description.

See `org-bom-prepare-tabledata-for-database' for more information."
  (unless (if (and (plist-get params :local-only) (plist-get params :section))
	      (error "Specify a section OR local-only, not both"))

;; Check options given by the user
    (let ((heading-list '())
	  (table-list '())
	  (local-only (plist-get params :local-only))
	  (section-name
	   (org-bom-stringify (plist-get params :section)))
	  (grand-total (plist-get params :total))
	  (col-name-section
	   (or (org-bom-stringify (plist-get params :col-name-section))
	       "Section"))
	  (col-name-price
	   (or (org-bom-stringify (plist-get params :col-name-price))
	       "Price"))
	  (col-name-quantity
	   (or (org-bom-stringify (plist-get params :col-name-quantity))
	       "Quantity"))
	  (col-name-tag
	   (or (org-bom-stringify (plist-get params :col-name-tag))
	       "Tag"))
	  (col-name-component
	   (or (org-bom-stringify (plist-get params :col-name-component))
	       "Component"))
	  (col-name-description
	   (or (org-bom-stringify (plist-get params :col-name-description))
	       "Description"))
	  (insert-col-section
	   (not (or (plist-get params :total)
		    (plist-get params :local-only)
		    (plist-get params :section)))) ; No use to put a section column if it's given local or given by the user
	  (insert-col-description
	   (if (plist-get params :description) t nil)) ; Activate if the user want to use it
	  (insert-col-price
	   (if (plist-get params :price) t nil)) ; Activate if the user want to use it
	  (insert-col-tag
	   (if (plist-get params :no-tag) nil t)) ; Default ON, must be turned off by the user
	  (insert-col-component t) ; Always true, for now
	  (insert-col-quantity
	   (if (plist-get params :no-quantity) nil t))
	  (remove-mark
	   (if (plist-get params :remove) t nil)) ; indicate if we should remove rather than keep
	  (part-match
	   (if (plist-get params :part-match) t nil)); If 't', a string-match will be used to select the section
	  (current-heading (if (org-before-first-heading-p)
			       (format "") ; If we are before the first heading, then simply default to "".
			     (org-get-heading t t))))
;; End of user options

;; select what is needed in the database
        (when section-name
	(setf section-name (org-split-string section-name "+"))) ; convert the section-name in a list of string, so the user don't have to enter it as one
      (let ((temp-section-name)
	    (temp-db (org-bom-select-in-db
		      org-bom-database
		      'component-name
		      ""
		      'remove!))) ;Remove any blank names
	(when (setf section-name
		    (or section-name
			(if local-only (list current-heading))))
	  (setf temp-db (org-bom-totalize ;if a section is defined, then keep only the database's relevant part
			 (org-bom-select-in-db temp-db
					       'component-section
					       section-name
					       remove-mark
					       part-match))))
	(if grand-total (setf temp-db (org-bom-totalize temp-db))) ; fuse all sections and get the total

;; Now construct the orgtbl-lisp
      ;;heading
	(when insert-col-description
	  (push col-name-description heading-list))
	(when insert-col-price
	  (push col-name-price heading-list))
	(when insert-col-quantity
	  (push col-name-quantity heading-list))
	(when insert-col-component
	  (push col-name-component heading-list))
	(when insert-col-tag
	  (push col-name-tag heading-list))
	(when insert-col-section
	  (push col-name-section heading-list))

	;;add a separator line to the table
	(push 'hline table-list)

	;;now add the heading to the table
	(push heading-list table-list)

	;;The body of the table
	(setq table-list
	      (append table-list
		      (nreverse (org-bom-to-lisp-table
				 temp-db
				 insert-col-section
				 insert-col-tag
				 insert-col-price
				 insert-col-description
				 insert-col-quantity))))

	;;if there's a price, add a total line
	(when insert-col-price
	  (setq table-list (nreverse table-list))
	  (push 'hline table-list)
	  (push (append '("TOTAL:")
			(make-list (1- (length heading-list)) "")) table-list)
	  (setq table-list (nreverse table-list)))


	(insert (orgtbl-to-orgtbl table-list
				  (list
				   :remove-newlines t
				   :tstart nil :tend nil
				   :hline "|---"
				   :sep " | "
				   :lstart "| "
				   :lend " |")))
	(org-table-align)
	(when insert-col-price
	  (org-table-store-formulas
	   (list (cons (concat "@>$"
			       (number-to-string (1+ (position col-name-price heading-list))))
		       "vsum(@I..@>>)")))
	  (org-table-iterate))))))




(defun org-bom-to-lisp-table (database &optional section tag price description quantity)
  "Return an orgtbl compliant table from an org-bom DATABASE.
See `org-bom-to-lisp-table-row' for more details."
  (let ((table '())) ;an empty list
  (dolist (current-component database table)
    (push (org-bom-to-lisp-table-row current-component
				     section
				     tag
				     price
				     description
				     quantity)
	  table))))



(defun org-bom-to-lisp-table-row (component &optional section tag price description quantity)
  "Return an orgtbl compliant row for a given COMPONENT
from the org-bom-database."
  (let ((list '())
	(tags (component-tag component))
	(name (component-name component))
	(quantity-num (component-quantity component)))
    (when section
      (push (component-section component) list))
    (when tag
      (push (org-bom-to-lisp-table-tags (component-tag component)) list))
    (when component
      (push (org-bom-to-lisp-table-name (component-name component)) list))
    (when quantity
      (push (number-to-string (component-quantity component)) list))
    (when price
      (let ((current-price
	      (plist-get (org-bom-get-current-component name) :price)))
	(push (if current-price
		  (number-to-string (* quantity-num
				       (string-to-number current-price)))
		"" )
	      list)))
    (when description
	  (push (or (plist-get
		     (org-bom-get-current-component name) :description)
		    "N/A" )
		list))
    (setq list (nreverse list))
    list))



(defun org-bom-to-lisp-table-name (name)
  "Check if `org-bom-latex-mode' is non-nil, if the datasheet exists
and add the necessary LaTeX command."
  (let ((temp-datasheet
	 (plist-get (org-bom-get-current-component name) :datasheetPdf)))
    (if (and (> (length temp-datasheet) 1) org-bom-latex-mode)
	(concat "\\hyperref["temp-datasheet"]{"name"}")
      name)))



(defun org-bom-to-lisp-table-tags (tags)
  "Takes the initial tags list form org-bom-database and
convert it in a single string. If `org-bom-latex-mode' is
non-nil, and if the number of tags is greater than
`org-bom-latex-max-tags', a latex command to add a pdf comment
is inserted."
  (let ((temp-tag (sort (delete-dups (org-bom-listify tags)) 'string<))
			  (single-tag nil)
			  (max-tags-activated? nil))
    (with-temp-buffer ;easier than trying to concat everything
      (when (and org-bom-latex-mode
		 (numberp org-bom-latex-max-tags)
		 (> (length temp-tag) org-bom-latex-max-tags))
	(insert "\\pdfcomment[color=Ivory,subject={Tags},icon=Note,open=true,hoffset=-1cm]{")
	(setq max-tags-activated? t))
      (while (> (length temp-tag) 0)
	(when (stringp (setf single-tag (pop temp-tag)))
	  (insert single-tag)
	  (if (> (length temp-tag) 0)
	      (insert ", "))); Insert a white space between the tags
	(when (and org-bom-latex-mode (numberp org-bom-latex-max-tags)
		   (= (- (length temp-tag) org-bom-latex-max-tags) 0))
	  (delete-char -2) ;delete the last comma in the PDF comment
	  (insert "}{")))
      (if max-tags-activated? (insert "}..."))
      (replace-regexp-in-string "[\\]" "\\\\" (buffer-string)))))



(defun org-bom-totalize (database)
  "Will ignore the sections and return a new database with a true
total for each component."
  (let ((new-database nil))
    (dolist (current-item-old-db database) ;scan the given database
      (let ((exists-flag nil)) ; exist flag as nil
	(dolist (current-item-new-db new-database) ;scan the 'new' database
	  (when (gnus-string-equal
		 (component-name current-item-new-db)
		 (component-name current-item-old-db)) ;when the same name is found
	    (setf (component-quantity current-item-new-db)
		  (+ (component-quantity current-item-new-db)
		     (component-quantity current-item-old-db))) ;simply add the quantity...
	    (push (component-tag current-item-old-db)
		  (component-tag current-item-new-db)) ; add the tags...
	    (setf exists-flag t))) ; and set the flag as t
	(if (not exists-flag) (push (make-component
				     :name (component-name current-item-old-db) ;otherwise create a new entry with the same component name as the old database
				     :section "total" ;give a dummy name - should never really be used
				     :quantity (component-quantity current-item-old-db) ; take the old quantity
				     :tag (component-tag current-item-old-db)) new-database)))); finally take the old tags
    (nreverse new-database)));reverse so it will be in the same order as before



(defun org-bom-get-current-component (name)
  "Return the current component from the `org-bom-details' plist."
   (car (remove-if-not
    #'(lambda (component)
	(equal (plist-get component :name) name))
    org-bom-details)))

(defun org-bom-get-from-database (database selector value)
  "Return every entry in the database which has the corresponding
value for a given selector. Can be the database's argument of itself
in case of multiple selectors"
  (setf value (gnus-replace-in-string value "+" "[+]")) ;In a regexp, has a meaning and isn't considered a "string"
  (remove-if-not
   '(lambda (comp)
      (string-match value (plist-get comp selector))) database))

(defun org-dblock-write:bom-datasheet (params)
  "This is used to add used components datasheet (for LaTeX only).
For more details, see `org-bom-insert-datasheet-table'."
  (let ((org-bom-details (copy-tree org-bom-details)))
    (when org-bom-update-enable
      (org-bom-check-for-details-table)
      (org-bom-total)); Scan the buffer and refresh the bill of materials
    (org-bom-insert-datasheet-table params)))

(defun org-bom-get-datasheet-info (component-name-list)
  "Return a plist of the form \"(:name name :datasheetPdf
  datasheetPdf :datasheetPath datasheetPath)\", given a
  COMPONENT-NAME-LIST. Info is retrieved from `org-bom-details'."
  (let ((datasheet-info nil))
    (dolist (current-component component-name-list datasheet-info)
      (let ((item-info (org-bom-get-current-component current-component)))
	(push (list :name current-component
		    :datasheetPdf (plist-get item-info :datasheetPdf)
		    :datasheetPath (plist-get item-info :datasheetPath))
	      datasheet-info)))))


(defun org-bom-insert-datasheet-table (params)
  "This is used to add used components datasheet (for LaTeX
only). The filename will be taken in the org-bom-details plist,
with the property :datasheetPdf, and the path
with :datasheetPath. Set \":description\" to enable a summary of
components before the datasheets. As for the BOM dynamic block,
the columns names can be changed with \":col-name-component\" and
\":col-name-description\"."
  (save-excursion
    (save-restriction
      (widen)
      (let ((all-component-names nil)
	    (temp-database (org-bom-select-in-db org-bom-database 'component-name "" 'remove))
	    (col-name-component (plist-get params :col-name-component) )
	    (col-name-description (plist-get params :col-name-description)))
	(when (plist-get params :description)
	  (org-bom-insert-table (list :total t :no-tag t :no-quantity t :description t
				      :col-name-component col-name-component
				      :col-name-description col-name-description)))
	(dolist (current-item temp-database)
	  (add-to-list 'all-component-names (component-name current-item))); Gather every component used
	(setf all-component-names (sort all-component-names 'gnus-string<))
	(let ((component-info (org-bom-get-datasheet-info all-component-names))
	      (used-datasheet)); do not include the same datasheet more than once.
	  (newline)
	  (dolist (current-component-info (nreverse component-info));reverse for alphabetic order
	    (let ((datasheet (plist-get current-component-info :datasheetPdf)))
	      (when (and (> (length datasheet) 0) (not (find datasheet used-datasheet :test 'gnus-string-equal)))
		(add-to-list 'used-datasheet datasheet)
		(insert (concat "#+LaTeX: " "\\includepdf[pages=-,landscape=true,addtotoc={1, subsection, 1, "
				(plist-get current-component-info :name) ","
				(plist-get current-component-info :datasheetPdf) "}]{"
				(or (plist-get current-component-info :datasheetPath)
				    org-bom-latex-datasheetPath) "/"
				(plist-get current-component-info :datasheetPdf)) "}")
		(newline)))))))))


(provide 'org-bom)

;========================================
; The program ends here
;========================================
