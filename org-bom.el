;    Copyright 2011 Free Software Foundation, Inc.
;
;    Filename: org-bom.el
;    Version: 0.4
;    Author: Christian Fortin <frozenlock AT gmail DOT com>
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
;
;----------------------- TUTORIAL --------------------
;-------- This tutorial should read as an org-file----
;-----------------------------------------------------

;; * BOM introduction

;;   This add-on collects components across the entire org buffer (even
;;   in drawers), making it easy to retrieve and sort data. It uses the
;;   column special name as a landmark. We will refer to them as
;;   'Keywords'. The keywords are searched using a string-match function,
;;   which gives the ability to have multiple column with the same
;;   functionality, but also to use the column name as we would usually
;;   with org-mode. For example, we can have 'tag' and 'tag2', both are
;;   recognized by the BOM add-on and can be used in a spreadsheet-like
;;   formula without any confusion. The keywords are also
;;   case-insensitive. 'Component' and 'component' will work in the same
;;   way.

;;   The BOM is usually used with a dynamic block. (You can use the
;;   different functions in emacs-lisp code, but this is beyond the
;;   purpose of this tutorial.) Here is the basic dynamic block:

;; :  #+BEGIN: bom
;; :  #+END:

;;   And here is what we obtain at this point:
;; :  #+BEGIN: bom
;; : | Section | Tag | Component | Quantity |
;; : |---------+-----+-----------+----------|
;; :  #+END:

;;   The table is empty, because we have to either:
;;   1. Add keywords in a table;
;;   2. Add a line-component.

;; * BOM keywords
;; ** Component

;;      This is the most important keyword and act as the trigger. For
;;   this example, let's say we write down things we want to buy. In 
;;   this case, a new keyboard for our computer.  This is how the
;;   table should be:
  
;; :  |   | Material  |
;; :  | ! | Component |
;; :  |---+-----------|
;; :  |   | Keyboard  |
  
;;   The '!' character is used in org table to specify column name, such
;;   as our keyword, 'component'.
;;   And here is what the bill-of-materials for this table is:
     
;; :  #+BEGIN:  bom 
;; : | Section   | Tag | Component | Quantity |
;; : |-----------+-----+-----------+----------|
;; : | Component |     | Keyboard  |        1 |
;; :  #+END:

;;   As you can see, the heading (Component) was automatically
;;   used as the 'section', which doesn't require attention for
;;   now. The quantity is, unsurprisingly, 1. There is nothing in the tag
;;   column for now, so let's dismiss it by adding the parameter *:no-tag
;;   t*. 
;;   This will results in the following:
;; :  #+BEGIN: bom  :no-tag t
;; : | Section   | Component | Quantity |
;; : |-----------+-----------+----------|
;; : | Component | Keyboard  |        1 |
;; :  #+END: 
  
;;   Now suppose that our friend too wants a new keyboard.

;; :  |   | For    | Material  |
;; :  | ! |        | Component |
;; :  |---+--------+-----------|
;; :  |   | Me     | Keyboard  |
;; :  |   | Friend | Keyboard  |
     
;; :   #+BEGIN: bom :no-tag t
;; :  | Section   | Component | Quantity |
;; :  |-----------+-----------+----------|
;; :  | Component | Keyboard  |        2 |
;; :   #+END:

;;   As expected, we get 2 keyboards.
     
;; ** Section
   
;;      The section is used to separate what would otherwise be an
;;   identical component. Suppose we don't want our friend's wishes to be
;;   in the same BOM as our, but still have them in the same table.

;; :  |   | For     | Material  |
;; :  | ! | Section | Component |
;; :  |---+---------+-----------|
;; :  |   | Me      | Keyboard  |
;; :  |   | Friend  | Keyboard  |

;;   This will results in the following BOM:
 
;; :  #+BEGIN: bom :no-tag t
;; : | Section | Component | Quantity |
;; : |---------+-----------+----------|
;; : | Friend  | Keyboard  |        1 |
;; : | Me      | Keyboard  |        1 |
;; :  #+END:

;;   Please note that when a component is given a section, it isn't
;;   associated with the heading anymore. As an alternative, you can set
;;   a ':SECTION:' property in the heading, which will be inherited by
;;   all the components _without_ a specified section.
;;   Section's priorities are as follow: 

;;   1. Given section with the 'section' keyword;
;;   2. The SECTION property;
;;   3. The heading.

;; ** Qty

;;      With this keyword, it is possible to specify a quantity for the
;;   associated component. In our always improving scenario, we now want to
;;   give a keyboard to another of our friend (as a gift). This is going to
;;   be bought at the same time as our keyboard, so they belong together.

;; :  |   | For     | Material  |     |
;; :  | ! | Section | Component | Qty |
;; :  |---+---------+-----------+-----|
;; :  |   | Me      | Keyboard  |   2 |
;; :  |   | Friend  | Keyboard  |   1 |

;; :   #+BEGIN: bom :no-tag t
;; :  | Section | Component | Quantity |
;; :  |---------+-----------+----------|
;; :  | Friend  | Keyboard  |        1 |
;; :  | Me      | Keyboard  |        2 |
;; :   #+END:
     
;;   *Important*: If Qty keyword is present, then any empty field will
;;   be considered as _zero_. This way, multiple column quantity are
;;   made quite easily:
     
;; :  |   | For     | Material  | Personal | Gift |
;; :  | ! | Section | Component |      Qty | Qty2 |
;; :  |---+---------+-----------+----------+------|
;; :  |   | Me      | Keyboard  |        1 | 1    |
;; :  |   | Friend  | Keyboard  |        1 |      |

;; :   #+BEGIN: bom :no-tag t
;; :  | Section | Component | Quantity |
;; :  |---------+-----------+----------|
;; :  | Friend  | Keyboard  |        1 |
;; :  | Me      | Keyboard  |        2 |
;; :   #+END:  

;; ** Tag

;;      When a BOM starts to get big, we often need a quick reminder of
;;   why we need certain component. Another use is also to identify the
;;   component. As the Qty keyword, multiple Tag columns can be associated
;;   with a single component. Here we will simply use the tag as a reminder
;;   of what we want to look for in the store.

;; :  |   | For     | Material  | Personal | Gift | Need               |
;; :  | ! | Section | Component |      Qty | Qty2 | Tag                |
;; :  |---+---------+-----------+----------+------+--------------------|
;; :  |   | Me      | Keyboard  |        1 | 1    | Matching colors    |
;; :  |   | Friend  | Keyboard  |        1 |      | Dinosaurs pictures |

;;   To show the tag column in the BOM, we simply remove the no-tag
;;   parameter.
;; :  #+BEGIN: bom
;; : | Section | Tag                | Component | Quantity |
;; : |---------+--------------------+-----------+----------|
;; : | Friend  | Dinosaurs pictures | Keyboard  |        1 |
;; : | Me      | Matching colors    | Keyboard  |        2 |
;; :  #+END:  
  

;;   If two Tag columns are present for a single Component column, the
;;   tags will be associated with this component, separated by a comma.

;; * Renaming BOM columns
     
;;      It is possible to rename the BOM columns with the following
;;   parameters:
;;   - col-name-component
;;   - col-name-section
;;   - col-name-quantity
;;   - col-name-tag
;;   - col-name-description
;;   - col-name-price

;;   This is how our renamed BOM would look like:
     
;; :  #+BEGIN: bom :col-name-section For :col-name-tag Need :col-name-component Stuff :col-name-quantity Qty
;; : | For    | Need               | Stuff    | Qty |
;; : |--------+--------------------+----------+-----|
;; : | Friend | Dinosaurs pictures | Keyboard |   1 |
;; : | Me     | Matching colors    | Keyboard |   2 |
;; :  #+END:  

;; * Multiple component's column

;;      There is two way to add components in a section. Either by adding
;;   other rows with the same section's name, or by adding other
;;   columns. Both have their uses and they should come to you quite
;;   naturally. In our example, we want more stuff.

;; :  |   | For     | Material  | Personal | Gift | Need               | Stuff     | More stuff | Much more stuff | How many |
;; :  | ! | Section | Component |      Qty | Qty2 | Tag                | Component | Component  | Component       | Qty      |
;; :  |---+---------+-----------+----------+------+--------------------+-----------+------------+-----------------+----------|
;; :  |   | Me      | Keyboard  |        1 | 1    | Matching colors    | Mouse     | Headset    | USB flash drive | 23       |
;; :  |   | Friend  | Keyboard  |        1 |      | Dinosaurs pictures |           |            |                 |          |
;; :  |   | Friend  |           |          |      |                    |           |            | CDs             | 50       |
;; :  |   | Friend  | Mouse     |        1 |      |                    |           |            |                 |          |
     
;;   This is beginning to get interesting. Note that even if we can
;;   name the additional columns 'Component2' or 'ComponentAAA',
;;   there's no use to do it if no table-formula uses the column
;;   names. 

;; * Precise section selection
;;   Now suppose we want to get OUR to-buy list. Simply specify
;;   the section's parameter *:section Me*:

;; :   #+BEGIN: bom :section Me
;; :  | Tag             | Component       | Quantity |
;; :  |-----------------+-----------------+----------|
;; :  |                 | Headset         |        1 |
;; :  | Matching colors | Keyboard        |        2 |
;; :  |                 | Mouse           |        1 |
;; :  |                 | USB flash drive |       23 |
;; :   #+END:  
     
;;   Wait, where's the section column?  Well we don't need it anymore,
;;   as we specified one.

;;   A '+' sign will specify we want more than a single section. *:section
;;   Me+Friend* will select both section, and add the quantity and tags
;;   for each component. 

;; :  #+BEGIN: bom :section Me+Friend
;; : | Tag                                 | Component       | Quantity |
;; : |-------------------------------------+-----------------+----------|
;; : |                                     | CDs             |       50 |
;; : |                                     | Headset         |        1 |
;; : | Dinosaurs pictures, Matching colors | Keyboard        |        3 |
;; : |                                     | Mouse           |        2 |
;; : |                                     | USB flash drive |       23 |
;; :  #+END:

;;   *Do not* put a whitespace between the section name and the '+' sign.
;;   Speaking of whitespace, if you need one in a section name, simply
;;   put it in a string: 
;; : #+BEGIN: bom :section "Section with whitespace"

;;   We can also return every section that matches at least what we
;;   provide. To activate this, use *:part-match t*. With this, if we
;;   write "fr", the Friend section is returned. If we had another
;;   section named "Frosting", than Friend and Frosting would have been
;;   merged and we would have a total for both section.

;; :  #+BEGIN: bom :section fr :part-match t
;; : | Tag                | Component | Quantity |
;; : |--------------------+-----------+----------|
;; : |                    | CDs       |       50 |
;; : | Dinosaurs pictures | Keyboard  |        1 |
;; : |                    | Mouse     |        1 |
;; :  #+END:

;;   It is also possible to specify that we don't want any section
;;   containing "fr". For this, use the parameter *:remove t*.

;; :  #+BEGIN: bom :section fr :part-match t :remove t
;; : | Tag             | Component       | Quantity |
;; : |-----------------+-----------------+----------|
;; : |                 | Headset         |        1 |
;; : | Matching colors | Keyboard        |        2 |
;; : |                 | Mouse           |        1 |
;; : |                 | USB flash drive |       23 |
;; :  #+END:

;;   In this case, getting all sections not containing "fr" is the
;;   equivalent of choosing the section "Me".

;;   If you simply want the components from the current heading, use the
;;   parameter *:local-only t*. This will return components with the
;;   current heading as their section, which is the default of every
;;   component if no section is provided. If a section has been provided to
;;   a component (and is not exactly equal to the heading), the component
;;   will not be returned.

;;   Here, we don't have any component under this heading:
;; :  #+BEGIN: bom :local-only t
;; : | Tag | Component | Quantity |
;; : |-----+-----------+----------|
;; :  #+END:

;; * BOM total
;;   This is all really interesting, but when we're in a shop, we want
;;   to know how many of each item we have to buy, we need a *total*.
;;   For this, simply add the *:total t* parameter. We will also remove
;;   the tags once again by using *:no-tag t*.

;; :  #+BEGIN: bom :total t :no-tag t
;; : | Component       | Quantity |
;; : |-----------------+----------|
;; : | CDs             |       50 |
;; : | Headset         |        1 |
;; : | Keyboard        |        3 |
;; : | Mouse           |        2 |
;; : | USB flash drive |       23 |
;; :  #+END:

;;   This is the equivalent of merging every sections together.
;; * Adding a component without a table

;;   There is another option you might need. If you ever want to
;;   add a component without a table, use the #+BOM commentary. As any
;;   other org-mode commentary, this one won't appear when exported to
;;   another document (pdf, html, docbook..). It will, however, enable
;;   you to add a single component in the bill-of-materials. Here is an
;;   example:
;; :  #+BOM: Keyboard :section Need :tag "Matching colors"

;;   As with the table components, you can simply give a component name if
;;   you desire. If no section is given, it will be inherited as an
;;   ordinary component in a table: a section property or the current
;;   heading.

;; * Adding details
;;   There is two way to add details to a BOM. The first one is to setq
;;   `org-bom-details' with a plist containing, depending on your
;;   needs, :description, :datasheet-pdf and :price. You must, however, at
;;   least have the component name in the :name property. Here is an
;;   example on how to set this variable:

;; #+BEGIN_SRC emacs-lisp
;; (setq org-bom-details '((:name "Keyboard" :description
;;                           "Something" :price "40") 
;;                           (:name "CDs" :description "Not
;;                           DVDs" :datasheet-pdf "CD.pdf")))
;; #+END_SRC
;;   Please note that the price is a *string*.

;;   The other method, valid for the current buffer only, is to give one
;;   or more bom-details table. It is recognized when a table is named as
;;   such:
;; :  #+TBLNAME: bom-details

;;   Once again, the column names are used. Contrary to the normal BOM
;;   keywords however, these are case-sensitive and must be written
;;   exactly as their property name. For example, the column of the
;;   property ':name' must be 'name'.
;; :  #+TBLNAME: bom-details
;; : | ! | name     | description  | price |
;; : |---+----------+--------------+-------|
;; : |   | Keyboard | Used to type |    40 |
;; : |   | CDs      |              |       |
  
;;   Any bom-details table will temporarily overshadow the
;;   `org-bom-details' variable, but won't erase or modify it. This means
;;   you can safely use a bom-details table if you need to change some
;;   local buffer description, while using `org-bom-details' in multiple
;;   buffer.

;;   Look at the CDs description. When a field is empty, it is *not* used
;;   and BOM falls back to the property in the `org-bom-details'
;;   variable.
  
;; ** Description
   
;;    You can add a description column in a BOM by adding the
;;    *:description t* parameter. 

;; :   #+BEGIN: bom :total t :no-tag t :description t
;; :  | Component       | Quantity | Description  |
;; :  |-----------------+----------+--------------|
;; :  | CDs             |       50 | Not DVDs     |
;; :  | Headset         |        1 | N/A          |
;; :  | Keyboard        |        3 | Used to type |
;; :  | Mouse           |        2 | N/A          |
;; :  | USB flash drive |       23 | N/A          |
;; :   #+END:

;;    See how the CDs' description wasn't the empty field from the
;;    bom-details table.

;; ** Price
   
;;    You can add a price column in a BOM by adding the *:price t*
;;    parameter.

;; :   #+BEGIN: bom :total t :no-tag t :description t :price t
;; :  | Component       | Quantity | Price | Description  |
;; :  |-----------------+----------+-------+--------------|
;; :  | CDs             |       50 |       | Not DVDs     |
;; :  | Headset         |        1 |       | N/A          |
;; :  | Keyboard        |        3 |   120 | Used to type |
;; :  | Mouse           |        2 |       | N/A          |
;; :  | USB flash drive |       23 |       | N/A          |
;; :  |-----------------+----------+-------+--------------|
;; :  | TOTAL:          |          |   120 |              |
;; :      #+TBLFM: @>$3=vsum(@I..@>>)
;; :   #+END:
;;    The price is automatically multiplied by the quantity of each
;;    component. In addition, a total row is added at the table's bottom
;;    with a vertical sum formula.
   
;; ** Datasheet
   
;;    This is a special property and must be used only if you intend to
;;    export in a pdf document. See [[LaTeX mode and bom-datasheet]] for more details.
   
;; * List of BOM parameters
;;   Here is a list of all the parameters usable in a BOM dynamic block,
;;   as seen throughout this tutorial:

;;   - no-tag :: Remove the tags column
;;   - section :: Select this section (or more if there's a + sign)
;;   - part-match :: Select every section with at least the string
;;                   provided for the section parameter
;;   - remove :: Select every sections except the one(s) provided
;;   - descripton :: Add the description column
;;   - price :: Add the price column and a total row at the bottom of the
;;              table
;;   - col-name-*** :: Rename the associated column
;; * Advanced and elisp functions
;; ** Speed up updates
;;    Each BOM dynamic block scans the entire buffer individually. While
;;    it is necessary that each block be able to update itself, it
;;    becomes a waste when the command `org-update-all-dblocks' is
;;    used. (The components usually aren't changing from a dblock evaluation to
;;    another.)
   
;;    In order to speed up updates, there's a variable that can be used
;;    to stop each BOM dblock from doing a buffer-wide scan. To disable the
;;    scans, set `org-bom-update-enable' to nil.

;;    The author uses a function similar to this one to speed up updates:
;; #+BEGIN_SRC emacs-lisp :exports code
;; (defun reg-update-project (&optional latex-mode)
;;   "Update every table and dynamic block in the buffer. If latex-mode
;; is non-nil, various latex commands will be inserted."
;;   (interactive)
;;   (org-table-iterate-buffer-tables)
;;   (org-bom-total); manually update the BOM database
;;   (let ((org-bom-update-enable nil)
;; 	(org-bom-latex-mode latex-mode)
;; 	(org-bom-details (copy-tree org-bom-details)));so we don't overwrite
;;     (org-bom-check-for-details-table); manually update `org-bom-details'
;;     (org-update-all-dblocks))
;;   (message "Project updated"))
;; #+END_SRC
     
;; ** LaTeX mode and bom-datasheet
;;   This mode isn't fully integrated to org-mode and should be seen as a
;;   hack. It is however useful to the author, which is why it is
;;   explained here.

;;   Set the `org-bom-latex-mode' variable to non-nil in order to
;;   activate the latex-mode. If set, all BOM dynamic block will insert
;;   some latex commands.

;;   These commands targets:
;;   - Tags :: When there is more tags than `org-bom-latex-max-tags' per
;;             component, the remaining tags are put in a pdf comment.
;;   - Component name :: If a datasheet exists for the component, its
;;                       name will become a link to its datasheet.

		     
;;   If you ever activate the LaTeX mode, use the bom-datasheet dynamic
;;   block at the end of your document. The optional parameter
;;   *:description t* will add a summary of all the components used in
;;   this buffer with their description, just before the datasheets.

;; :  #+BEGIN: bom-datasheet
;; :  
;; :  #+LaTeX: \includepdf[pages=-,landscape=true,addtotoc={1, subsection, 1, CDs,CD.pdf}]{\DATASHEETPATH/CD.pdf}
;; :  
;; :  #+END:

;;   As you may have noticed, there's a LaTeX variable in this command:
;;         \DATASHEETPATH. In order to work, you must set this variable
;;         using:

;; :	#+LATEX_HEADER: \newcommand{\DATASHEETPATH}{Name-of-the-folder/}'

;; 	Name-of-the-folder is the folder where the datasheets' files
;;         are located. 
;-----------------------------------------------------
;------------------- End of tutorial -----------------
;-----------------------------------------------------
;       
;=====================================================
; The program begins here
;=====================================================

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
\":description\". It should also contain \":datasheet-pdf\" in order
to use the bom-datasheet dynamic block.") 

(defvar org-bom-update-enable t
  "Scan the buffer and update the BOM when a dynamic block is
refreshed. Should be set to nil for a buffer-wide dynamic block,
such as with `org-update-all-dblocks'. However, be sure to update
manually with `org-bom-total' in this case.") 

(defvar org-bom-latex-mode nil
  "If activated, every component's name will be replaced by a reference
to the datasheet and comments might be activated if necessary (large
number of tags). See `org-bom-latex-max-tags'.") 

(defvar org-bom-latex-max-tags 10
  "Define the maximum number before the tags start being hidden in a
PDF comment. Set to nil to disable.") 


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
  "Returns the DATABASE sorted alphabetically by component name"
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

(defun org-bom-prepare-linedata-for-database (&optional section-override)
  "Scan the buffer and add line-components to database. Search for an 
org-mode comment \"#+BOM:\". Everything before the keys (:section, :qty, :tag)
is considered to be the component's name, except the last whitespaces.
The same \"section\" priority is in this order: Given with the :section key, 
in a :SECTION: property, or the org heading."
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
\"description\", \"price\", and \"datasheet-pdf\". Adds the data to
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

		
(defun org-bom-get-table-field (&optional N)
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
  (save-excursion
    (set-buffer (find-file-noselect (or filename "list-export.txt"))) 
    (erase-buffer)
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
	    (insert-tab)))))
    (save-buffer)
    (kill-buffer)))

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
      (org-bom-total)); Scan the buffer and refresh the bill of materials
    (org-bom-insert-table params)
  (message "Bill of materials created")))

(defun org-bom-stringify (&optional argument)
  "If ARGUMENT is string, returns unchanged. If it's a symbol,
returns the symbol-name. If nil, return nil" 
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
	  (insert-col-quantity t) ; Always true, for now
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
				 insert-col-description))))
	
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




(defun org-bom-to-lisp-table (database &optional section tag price description)
  "Returns an orgtbl compliant table from an org-bom DATABASE.
See `org-bom-to-lisp-table-row' for more details."
  (let ((table '())) ;an empty list
  (dolist (current-component database)
    (push (org-bom-to-lisp-table-row current-component 
				     section
				     tag
				     price
				     description)
	  table))
  table))



(defun org-bom-to-lisp-table-row (component &optional section tag price description)
  "Returns an orgtbl compliant row for a given COMPONENT 
from the org-bom-database."
  (let ((list '())
	(tags (component-tag component))
	(name (component-name component))
	(quantity (component-quantity component)))
    (when section
      (push (component-section component) list))
    (when tag
      (push (org-bom-to-lisp-table-tags (component-tag component)) list))
    (when component
      (push (org-bom-to-lisp-table-name (component-name component)) list)
      (push (number-to-string (component-quantity component)) list))
    (when price
      (let ((current-price 
	      (plist-get (org-bom-get-current-component name) :price)))
	(push (if current-price 
		  (number-to-string (* quantity  
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
	 (plist-get (org-bom-get-current-component name) :datasheet-pdf)))
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



;;Horrible function from the time I was learning to code in elisp.
;;I haven't found the courage to re-write it yet.
(defun org-bom-insert-datasheet-table (params)
  "This is used to add used components datasheet (for LaTeX only). The
filename will be taken in the org-bom-details plist, with the
property :datasheet. A latex command such as \"#+LATEX_HEADER:
\newcommand{\DATASHEETPATH}{Name-of-the-folder/}\" shall be inserted
at the beginning of the org document, where Name-of-the-folder is
the folder where the datasheets files are. Note that the entire
filename must be in the plist; \"datasheet.pdf\". Set
\":description\" to enable a summary of components before the
datasheets. As for the BOM dynamic block, the columns names can be
changed with \":col-name-component\" and \":col-name-description\"."
  (save-excursion
    (save-restriction
      (widen)
      (let ((temp-database (org-bom-select-in-db org-bom-database 'component-name "" 'remove!))
	    (all-component-names nil)
	    (component-names-for-toc)
	    (all-component-datasheet nil)
	    (temp-filename nil)
	    (temp-name nil))
	(while temp-database
	  (add-to-list 'all-component-names (component-name (pop temp-database)))); Gather every component used	
	(setf all-component-names (sort all-component-names 'gnus-string<))
	(dolist (temp-component-name all-component-names )
	  (let ((current-datasheet (plist-get (org-bom-get-current-component temp-component-name) :datasheet-pdf)))
	    (if (> (length current-datasheet) 1)
		(or (member current-datasheet all-component-datasheet)
		    (progn (setq all-component-datasheet (cons current-datasheet all-component-datasheet))
			   (setq component-names-for-toc (cons temp-component-name component-names-for-toc)))))))
	(setf all-component-datasheet (nreverse all-component-datasheet))
	(setf component-names-for-toc (nreverse component-names-for-toc))
	(if (plist-get params :description)
	    (progn (let ((col-name-component (or (if (plist-get params :col-name-component)
						     (symbol-name (plist-get params :col-name-component))) "Component"))
			 (col-name-description (or (if (plist-get params :col-name-description)
						       (symbol-name (plist-get params :col-name-description))) "Description"))
			 (temp-all-component-names all-component-names)
			 (temp-all-component-filename all-component-datasheet))
		     (org-table-create (concat (int-to-string 2)"x" (int-to-string (+ 1 (length all-component-names))))) ;create a table
		     (org-table-goto-column 1)
		     (insert col-name-component)
		     (org-table-goto-column 2)
		     (insert col-name-description)
		     (while temp-all-component-names
		       (setf temp-name (pop temp-all-component-names))
		       (org-table-goto-line (+ (org-table-current-line) 1))
		       (org-table-goto-column 1)
		       (if (> (length (setf temp-filename (plist-get (org-bom-get-current-component temp-name) :datasheet-pdf))) 1)
			   (insert (concat "\\hyperref["temp-filename"]{"temp-name"}"))
			 (insert temp-name))
		       (org-table-goto-column 2)
		       (insert (or (plist-get (org-bom-get-current-component temp-name) :description) "N/A"))))
		   (org-table-align)
		   (end-of-line)
		   (newline)))
	(newline)
	(setf all-component-datasheet all-component-datasheet)
	(while all-component-datasheet ; for every datasheet
	  (if (> (length (setf temp-filename-and-component (pop all-component-datasheet))) 1) ; get the filename associated with the component's name
	      (progn(insert (concat "#+LaTeX: " "\\includepdf[pages=-,landscape=true,addtotoc={1, subsection, 1, " (pop component-names-for-toc) ","temp-filename-and-component"}]{\\DATASHEETPATH/" temp-filename-and-component "}"))
		    (newline))
	    (message (concat "#### " temp-name " doesn't have a datasheet... moving on."))))))))


(provide 'org-bom)

;========================================
; The program ends here
;========================================
