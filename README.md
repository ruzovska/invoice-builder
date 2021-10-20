invoice-builder
===============

This program takes the files containing information about your invoice and creates a file in `tex` format which can be later converted to `pdf` format.

How to use this program
=======================

1. Clone this repository:

       git clone 'https://github.com/ruzovska/invoice-builder.git'
1. Navigate to the cloned repository:

       cd invoice-builder
1. Edit the `input` and `info-invoice` files to your liking but keep the structure intact.
1. Run the program:

       cabal run invoice-builder -- input info-invoice output.tex
1. A file named `output.tex` appeared in the directory. Convert it to `pdf` format using some conversion tool. I use `xelatex`:

       xelatex output.tex output.pdf