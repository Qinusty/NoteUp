=========================
My custom markup language
=========================

A simple markup language written for the purpose of simplistic note formatting.

How to
-------

TODO : Basic documentation of the available features...

Installing
-----------

.. code:: Bash

	git clone https://github.com/Qinusty/NotesMarkupConverter.git
	cd NotesMarkupConverter.git
	ghc --make main.hs
Using
------

.. code:: 

	cat inputFile.not | ./NoteParser > outputFile.rst
Requirements
~~~~~~~~~~~~~
* ghc - Haskell compiler
* Latex compiler (Pandoc compatible - texlive - pdftex)
* Pandoc - http://pandoc.org/


