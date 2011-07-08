(defpackage :javascript
  (:use :cl :parenscript))

(in-package :javascript)

;;===== core
(defpsmacro define-site-variables ()
  `(defvar *site-address* "www.golfholidays.tv"))

(defpsmacro relative-url (url)
  `(+ "http://" *site-address* ,url))

;;===== domready
(defpsmacro domready (&body body)
  `((@ ($ document) ready) 
    (lambda () ,@body)))

;;===== callback / event
(defpsmacro defcallback (selector event-type callback)
  `((@ ,selector ,event-type) ,callback))

(defpsmacro defevent (selector event-type &body callback-body)
  `((@ ,selector ,event-type) (lambda () ,@callback-body)))

;;===== dataform
(defpsmacro defform (selector callback)
  `((@ ,selector ajax-form) ,callback))

(defpsmacro defsubmit (selector callback)
  `((@ ,selector submit)
    (lambda ()
      ((@ ($ this) ajax-submit)
       ,callback)
      (return false))))

(defpsmacro dom-dataform (form-element action method body)
  `(form (id ,form-element action ,action method ,method) append
	 ,body))

(defpsmacro dom-upload-dataform (form-element action method body)
  `(form (id ,form-element action ,action method ,method 
	     enctype "multipart/form-data") append
	 ,body))

(defpsmacro dom-select (id name selected lst)
  `(select (id ,id name ,name) append
	   (dolist (el ,lst)
	     (if (= ,selected el)
		 (option (value el selected "selected") append el)
		 (option (value el) append el)))))

;; dataform elements
(defpsmacro dom-standard-control-elements/add-remove ()
  `(div (id "datatable-standard-controls/add-remove") append
	(submit (name "add" value "Add") append)
	(submit (name "remove" value "Remove") append)))

(defpsmacro dom-standard-control-elements/remove ()
  `(div (id "datatable-standard-controls") append
	(submit (name "remove" value "Remove") append)))

(defpsmacro dom-datatable-control (tfoot-element control-elements)
  `(tfoot (id ,tfoot-element) append
	  (tr () append
	      (td (id "datatable-npages") append)
	      (td (id "datatable-control" colspan "2") append
		  ,control-elements))))

(defpsmacro dom-datatable (root-element new-root-element form-element table-element action
			   thead-element tbody-element tfoot)
  `((@ ($ ,root-element) replace-with)
    (div (id ,new-root-element) append
	 (form (id ,form-element action ,action  method "POST") append
	       (div (id "datatable-select") append
		    "Select "
		    (a (id "select-all" href "#select-all") append "All") " "
		    (a (id "select-none" href "#select-none") append "None"))
	       (table (id ,table-element class "data-table") append
		      (thead (id ,thead-element) append)
		      (tbody (id ,tbody-element) append)
		      ,tfoot)))))

(defpsmacro dom-standard-datatable/add-remove (root-element new-root-element form-element table-element action
					       thead-element tbody-element tfoot-element)
  `(dom-datatable ,root-element ,new-root-element ,form-element ,table-element ,action ,thead-element ,tbody-element
		  (dom-datatable-control ,tfoot-element (dom-standard-control-elements/add-remove))))

(defpsmacro dom-standard-datatable/remove (root-element new-root-element form-element table-element action
					   thead-element tbody-element tfoot-element)
  `(dom-datatable ,root-element ,new-root-element ,form-element ,table-element ,action ,thead-element ,tbody-element
		 (dom-datatable-control ,tfoot-element (dom-standard-control-elements/remove))))

;; create a new dom selector with the provided attributes
(defpsmacro make (selector &rest attributes)
  `((@ ,selector attr) (create ,@attributes)))

(defmacro defpstag (tag html)
  `(defpsmacro ,tag (property-list insert-body-function &body body)
     (loop for content in body
	with dom-tag = `(@ (make ($ ,,html) ,@property-list))
	do (setf dom-tag `((@ ,dom-tag ,insert-body-function) ,content))
	finally (return dom-tag))))

;;
(defpstag div "<div></div>")
(defpstag a "<a></a>")
(defpstag img "<img></img>")
(defpstag form "<form></form>")
(defpstag textbox "<input type=\"text\"></input>")
(defpstag fieldset "<fieldset></fieldset>")
(defpstag label "<label></label>")
(defpstag password "<input type=\"password\"></input>")
(defpstag hidden "<input type=\"hidden\"></input>")
(defpstag file "<input type=\"file\"></input>")
(defpstag textarea "<textarea></textarea>")
(defpstag script "<script></script>")
(defpstag slider "<div></div>")
(defpstag checkbox "<input type=\"checkbox\"></input>")
(defpstag submit "<input type=\"submit\"></input>")
(defpstag select "<select></select>")
(defpstag option "<option></option>")

;;
(defpstag ul "<ul></ul>")
(defpstag li "<li></li>")

;; table
(defpstag table "<table></table>")
(defpstag thead "<thead></thead>")
(defpstag tbody "<tbody></tbody>")
(defpstag tfoot "<tfoot></tfoot>")
(defpstag tr "<tr></tr>")
(defpstag td "<td></td>")

;; traversing

;; filtering
#|
(ps ((/. seq eq) index))
(ps ((/. seq filter) expr))
(ps ((/. seq is) expr))
(ps ((/. seq map) callback))
(ps ((/. seq not) expr))
(ps ((/. seq slice) start end))
|#

;; ajax
(defpsmacro get-json (url callback)
  `($.ajax
    (create url ,url
	    cache false
	    data-type "json"
	    success ,callback)))

(defpsmacro get-json/data (url data callback)
  `($.ajax
    (create type "post"
	    url ,url
	    data (+ "oid=" ,data)
	    data-type "json"
	    success ,callback)))

(defpsmacro simple-ajax-redirect (redirect-url query-string)
  `($.ajax
    (create type "get"
	    url (relative-url "")
	    data-type "json"
	    success
	    (lambda (data text-status)
	      (setf (@ window location href)
		    (+ ,redirect-url ,query-string)))
	    complete
	    (lambda (xml-http)
	      (setf (@ top location href) (+ ,redirect-url ,query-string))))))

(defpsmacro ajax-redirect (request-url redirect-url)
  `($.ajax
    (create type "post"
	    url ,request-url
	    data-type "json"
	    success
	    (lambda (data text-status)
	      (setf (@ window location href)
		    (+ ,redirect-url "?oid=" (@ data oid))))
	    complete
	    (lambda (xml-http)
	      (if (= (@ xml-http status) 301)
		  (setf (@ top location href) ,redirect-url))))))

(defpsmacro ajax-subredirect (request-url redirect-url)
  `($.ajax
    (create type "post"
	    url ,request-url
	    data-type "json"
	    success
	    (lambda (data text-status)
	      (setf (@ window location href)
		    (+ ,redirect-url "?oid=" ((@ $ get-url-var) "oid")
		       "&suboid=" (@ data oid))))
	    complete
	    (lambda (xml-http)
	      (if (= (@ xml-http status) 301)
		  (setf (@ top location href) ,redirect-url))))))

(defpsmacro ajax-subredirect2 (request-url redirect-url suboid)
  `($.ajax
    (create type "post"
	    url ,request-url
	    data-type "json"
	    success
	    (lambda (data text-status)
	      (setf (@ window location href)
		    (+ ,redirect-url "?oid=" ((@ $ get-url-var) "oid")
		       "&suboid=" ,suboid
		       "&suboid2=" (@ data oid))))
	    complete
	    (lambda (xml-http)
	      (if (= (@ xml-http status) 301)
		  (setf (@ top location href) ,redirect-url))))))

;; utilities
(defpsmacro each (items callback)
  `($.each ,items ,callback))

;;
(defpsmacro json-bind (root json-url &body body)
  `(get-json 
    ,json-url
    (lambda (data)
      (if (not (null (@ data items)))
	  (each (@ data items)
		(lambda (i item)
		  ((@ ,@body
		      append-to)
		   ,root)))
	  ((@ (td (colspan "2") append "No items have been added yet.") append-to)
	   ,root)))))

(defpsmacro json-bind/id (root json-url id &body body)
  `(get-json/data
    ,json-url
    ,id
    (lambda (data)
      (if (not (null (@ data items)))
	  (each (@ data items)
		(lambda (i item)
		  ((@ ,@body
		      append-to)
		   ,root)))
	  ((@ (td (colspan "2") append "No items have been added yet.") append-to)
	   ,root)))))

;; validation
(defpsmacro validate-required (value)
  `(> (length ,value) 0))

(defpsmacro validate-integer (value)
  `(let ((nr (new (*reg-exp "[0-9]+" "i"))))
     ((@ nr test) ,value)))

(defpsmacro validate-integer-range (value min max)
  `(and (validate-integer ,value)
	(>= ,value ,min)
	(<= ,value ,max)))

(defpsmacro validate-postcode (value)
  `(let ((r1 (new (*reg-exp "[a-z][a-z][0-9][0-9] [0-9][a-z][a-z]" "i")))
	 (r2 (new (*reg-exp "[a-z][a-z][0-9] [0-9][a-z][a-z]" "i")))
	 (r3 (new (*reg-exp "[a-z][a-z][0-9][0-9][0-9][a-z][a-z]" "i")))
	 (r4 (new (*reg-exp "[a-z][a-z][0-9][0-9][a-z][a-z]" "i"))))
     (or ((@ r1 test) ,value)
	 ((@ r2 test) ,value)
	 (= ,value ""))))

(defpsmacro validate-date (value)
  `(let ((r1 (new (*reg-exp "[0-3][0-9]/[0-1][0-9]/[0-9][0-9][0-9][0-9]"))))
     ((@ r1 test) ,value)))

(defpsmacro validate-e-mail (value)
  `(let ((r1 (new (*reg-exp "[a-zA-Z0-9_-]+@[a-zA-Z0-9_-].[a-zA-Z]"))))
     ((@ r1 test) ,value)))

(defpsmacro validate-day (value)
  `(validate-integer-range ,value 1 31))

(defpsmacro validate-month (value)
  `(validate-integer-range ,value 1 12))

(defpsmacro validate-year (value)
  `(validate-integer-range ,value 2000 5000))

;; tinymce
(defpsmacro initialize-tinymce (&body body)
    `((@ tiny-m-c-e init)
      (create ,@body)))





