;;; The HAL Library
;;; Author: Troy Bourdon
;;; License: GPL3
;;; Last Updated: 9/10/2013
;;;
;;; To use the library set the constant hal-workbench-dir in your .emacs file
;;; Example: (defconst hal-workbench-dir "/home/tstark/workspace")
;;;
;;; This library was born out of a need to do JEE development work in many, many projects in a terminal over an ssh session.
;;; Most JEE IDEs such as Eclipse, Netbeans, IntelliJ etc. support the workspace development model where the developer 
;;; checks out projects from version control into a workspace directory such as /home/user/workspace. In JEE development
;;; it is not uncommon to work in many projects at once.
;;;
;;; While terminal editors like Vim and Emacs are very powerful they fail to provide the project navigation features
;;; the standard JEE IDEs provide out of the box. 
;;;
;;; This library provides a set of functions to make project navigation easier in Emacs. There are no bold claims of
;;; superior usability of this library. It's a work in progress; suggestions and updates are welcome.
;;;
;;; At it's core, the HAL library provides a set of covenience wrapper functions around the find and grep commands. The functions
;;; allow a developer to quickly search their entire "workspace" for references to classes/types and easily navigate to them.
;;; The use of the find and grep utilities is a hack of sorts as the JVM is not used at all to find type references.
;;;
;;; In the future I hope to write functions which will not only interogate the developer's workspace but also the
;;; developer's .m2 local Maven repository.

(require 'cl)

(defconst hal-buffer-name "*HAL-OUTPUT*")
(defconst hal-transcript-name "*HAL-TRANSCRIPT*")
(defvar hal-transcript-list "")
(defvar hal-current-selected-import "")

(defun hal-find-file(search-term file-type)
  "Search for all files of type file-type that match the search-term and display results in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: \nsEnter File Type: ")
  (hal-display-command-results
   (hal-fetch-command-results (hal-build-find-file-command search-term (concat "*." file-type)))))

(defun hal-find-in-file(search-term file-type)
  "Find all files where the search-term exists in the file for the given file-type and display results in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: \nsEnter File Type: ")
  (hal-display-command-results
   (hal-fetch-command-results (hal-build-find-in-file-command search-term (concat "*." file-type)))))

(defun hal-find-file-in-dir(directory search-term file-type)
  "Find all files in a given directory for the given file-type that match the search-term and display results in the HAL-OUTPUT buffer."
  (interactive "DEnter Directory: \nsEnter Search Term: \nsEnter File Type: ")
  (hal-display-command-results
   (hal-fetch-command-results (hal-build-find-file-in-dir-command directory search-term (concat "*." file-type)))))

(defun hal-find-term-in-dir(directory search-term file-type)
  "Find all files where the search-term exists in the file for the given directory and file-type and display results in the HAL-OUTPUT buffer."
  (interactive "DEnter Directory: \nsEnter Search Term: \nsEnter File Type: ")
  (hal-display-command-results
   (hal-fetch-command-results (hal-build-find-term-in-dir-command directory search-term (concat "*." file-type)))))

(defun hal-display-command-results(command-results)
  "Display the command results in the HAL-OUTPUT buffer. The command results are of a continuous string type."
  (with-current-buffer (get-buffer-create hal-buffer-name)
	(goto-char (point-min))
	(insert command-results)
	(goto-char (point-min)))
  (pop-to-buffer hal-buffer-name))

(defun hal-execute-command(command)
  "Find all files returned from a properly formatted system command and display in the HAL-OUTPUT buffer."
  (setq hal-transcript-list (concat hal-transcript-list command "\n"))
  (hal-display-command-results (shell-command-to-string command)))

(defun hal-fetch-command-results(command)
  "Find all items returned from a properly formatted command and display in the HAL-OUTPUT buffer."
  (setq hal-transcript-list (concat hal-transcript-list command "\n"))
  (shell-command-to-string command))

(defun hal-execute-transcript-command(command)
  "Find all files returned from a properly formatted command and display in the HAL-OUTPUT buffer."
  (setq hal-transcript-list (concat hal-transcript-list command "\n"))
  (with-current-buffer (get-buffer-create hal-buffer-name)
	(goto-char (point-min))
	(insert (shell-command-to-string command))
	(goto-char (point-min)))
	(set-window-buffer (selected-window) hal-buffer-name))

(defun hal-transcript()
  "Show the HAL-TRANSCRIPT of previous commands for this session."
  (interactive)
  (with-current-buffer (get-buffer-create hal-transcript-name)
	(goto-char (point-min))
	(insert hal-transcript-list)
	(goto-char (point-min)))
	(pop-to-buffer hal-transcript-name))

(defun hal-build-find-file-command(search-term file-type)
  (concat "find " hal-workbench-dir " -type f -name '" file-type "'" " | grep " search-term " | grep -v target"))

(defun hal-build-find-file-in-dir-command(directory search-term file-type)
  (concat "find " directory " -type f -name '" file-type "'" " | grep " search-term " | grep -v target"))

(defun hal-build-find-term-in-dir-command(directory search-term file-type)
  (concat "grep -lrw " search-term " --include " file-type " " directory " | grep -v target"))

(defun hal-build-find-in-file-command(search-term file-type)
  (concat "grep -lrw " search-term " --include " file-type " " hal-workbench-dir " | grep -v target"))
  
(defun hal-find-groovy(search-term)
  "Search for all Groovy files that match the search-term and display in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: ")
  (hal-find-file search-term "groovy"))

(defun hal-find-java(search-term)
  "Search for all Java files that match the search-term and display in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: ")
  (hal-find-file search-term "java"))

(defun hal-find-java-paths(search-term)
  "Return a list of all Java file paths that match the search-term."
  (split-string (hal-fetch-command-results (hal-build-find-file-command search-term "*.java")) "\n"))

(defun hal-find-java-type()
  "Search for all Java files that match the word under point and display in the HAL-OUTPUT buffer."
  (interactive)
  (hal-find-file (thing-at-point 'word) "*.java"))

(defun hal-find-in-java(search-term)
  "Find all occurances of search-term in all Java files and display in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: ")
  (hal-find-in-file search-term "*.java"))

(defun hal-find-html(search-term)
  "Search for all HTML files that match the search-term and display in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: ")
  (hal-find-file search-term "html"))

(defun hal-find-xml(search-term)
  "Search for all XML files that match the search-term and display in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: ")
  (hal-find-file search-term "xml"))

(defun hal-find-props(search-term)
  "Search for all properties files that match the search-term and display in the HAL-OUTPUT buffer."
  (interactive "sEnter Search Term: ")
  (hal-find-file search-term "properties"))

(defun hal-find-java-imports(search-term)
  "Find the java file paths that match the search-term and convert them to import statements"
  (interactive "sEnter Search Term: ")
  (setq java-packages (hal-java-paths-to-packages (hal-find-java-paths search-term)))
  (setq command-results (loop for opackage in
							  (loop for ipackage in java-packages
									collect (concat ipackage "\n"))
							  concat opackage))
  (hal-display-command-results command-results))

(defun hal-java-paths-to-packages(java-paths)
  "Given a list of java paths create a new list of packages that map to the paths."
  (loop for path in java-paths
		collect (hal-convert-java-path-to-package path)))

(defun hal-convert-java-path-to-package(java-path)
  "Given a path to a java file, trim off the leading part of the path before /com and replace the /'s with .'s"
  (cond ((not (equal java-path "")) 
		 (hal-prepend-import-to-java-import-string
		  (hal-trim-java-from-import-string
		   (replace-regexp-in-string "/" "." (substring java-path (search "com" java-path))))))))
		
(defun hal-trim-java-from-import-string(java-path-string)
  "Given a dot delimeted path to a java path, trim off the trailing .java part"
  (replace-regexp-in-string ".java" "" java-path-string))

(defun hal-prepend-import-to-java-import-string(java-import-string)
  "Given a dot delimeted path to a java path, prepend the import"
  (concat "import " java-import-string ";"))

(defun hal-concat-string-list(the-list separator)
  "Iterate of the list and concatenate every element with the separator and the next element"
  (loop for item in the-list
		concat (item separator)))

(defun hal-open()
  "Open the file currently selected in the HAL-OUTPUT buffer"
  (interactive)
  (find-file-at-point)
  (kill-buffer hal-buffer-name)
  (delete-other-windows))

(defun hal-run()
  "Run the currently selected command in the HAL-TRANSCRIPT buffer."
  (interactive)
  (hal-execute-transcript-command (buffer-substring (line-beginning-position) (- (line-end-position) 1)))
  (kill-buffer hal-transcript-name))

(defun hal-bw-kill()
  "Kill the current buffer and window."
  (interactive)
  (kill-buffer-and-window))

(defun hal-b-kill()
  "Kill the current buffer."
  (interactive)
  (kill-buffer))

(defun dtb-w-kill()
  "Kill the current window."
  (interactive)
  (delete-window))

(defun hal-copy-import()
  "Copy the currently selected import line in the HAL-OUTPUT buffer"
  (interactive)
  (setq hal-current-selected-import (thing-at-point 'line))
  (message (concat (replace-regexp-in-string "\n" "" hal-current-selected-import) " copied to import buffer."))
  (kill-buffer-and-window))

(defun hal-yank-import()
  "Yank the currently selected import to the current line where point exists"
  (interactive)
  (insert hal-current-selected-import))

(provide 'hal)
