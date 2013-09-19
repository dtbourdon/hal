# Synopsis

A simple Emacs library to aid in Java/JEE development. The basic workflow is
to run a function in the library which returns a list of results in either the
HAL-OUTPUT buffer or the HAL-TRANSCRIPT buffer. From there a function can be
run on a selected item from the list. This will usually be opening a file but
in the case of the HAL-TRANSCRIPT buffer it would be running a previous command.

# Installation

Please, note that the current version of `hal.el` requires find and grep the cl lib.

## Manual

Drop 'hal.el' in your load path and add the following to your .emacs file

```lisp
(defconst hal-workbench-dir "/your/development/workspace")
(add-to-list 'load-path "~/.emacs.d/hal/")
(require 'hal)
```

# Usage

Command                                       | Description
----------------------------------------------|----------------------------------------------------------------------------------------------
<kbd>M-x hal-find-file</kbd>                  | Find all files for the given search string and file type
<kbd>M-x hal-find-in-file</kbd>               | Find all files in which the search string exists for the given file type
<kbd>M-x hal-find-file-in-dir</kbd>           | Find all files for the search string and file type in the given directory
<kbd>M-x hal-find-term-in-dir</kbd>           | Find all files that contain the search string for the given file type in the given directory
<kbd>M-x hal-find-java</kbd>                  | Convenience function that calls hal-find-file with file type of .java
<kbd>M-x hal-find-groovy</kbd>                | Convenience function that calls hal-find-file with file type of .groovy
<kbd>M-x hal-find-html</kbd>                  | Convenience function that calls hal-find-file with file type of .html
<kbd>M-x hal-find-xml</kbd>                   | Convenience function that calls hal-find-file with file type of .xml
<kbd>M-x hal-find-props</kbd>                 | Convenience function that calls hal-find-file with file type of .properties
<kbd>M-x hal-find-in-java</kbd>               | Convenience function that calls hal-find-in-file with file type of .java
<kbd>M-x hal-find-in-groovy</kbd>             | Convenience function that calls hal-find-in-file with file type of .groovy
<kbd>M-x hal-find-java-type</kbd>             | Find all java files which match the word under point
<kbd>M-x hal-find-groovy-type</kbd>		      | Find all groovy files which match the word under point
<kbd>M-x hal-find-java-imports</kbd>          | Find the java paths that match the search term and convert to import statemets 
<kbd>M-x hal-open</kbd>                       | Open the file currently selected in the HAL-OUTPUT buffer
<kbd>M-x hal-transcript</kbd>                 | Display the HAL-TRANSCRIPT buffer with the list of previous commands for this session
<kbd>M-x hal-run</kbd>                        | Run the currently selected command in the HAL-TRANSCRIPT buffer
<kbd>M-x hal-copy-import</kbd>                | Copy the currently selected import line in the HAL-OUTPUT buffer 
<kbd>M-x hal-yank-import</kbd>                | Yank the previously selected import to the line where point exists
<kbd>M-x hal-b-kill</kbd>                     | Kill the current buffer (eg HAL-OUTPUT or HAL-TRANSCRIPT)
<kbd>M-x hal-w-kill</kbd>                     | Kill the current window (eg HAL-OUTPUT or HAL-TRANSCRIPT)
<kbd>M-x hal-bw-kill</kbd>                    | Kill the current buffer and window (eg HAL-OUTPUT or HAL-TRANSCRIPT)


# Known issues


# Bugs & Improvements

Bug reports and suggestions for improvements are always
welcome. GitHub pull requests are even better! :-)

Cheers,<br/>
[dtbourdon](http://twitter.com/tbourdon)
