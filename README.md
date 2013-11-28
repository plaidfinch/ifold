Indenting Fold (`ifold`)
========================

Indenting fold (`ifold`) is a replacement for the UNIX `fold` command, written in Haskell. It indents every wrapped line by the same amount that the original line was indented. This is useful, e.g. for converting plain-text documents into other formats.

Example usage
-------------

The normal UNIX `fold` command looks like this:

```
> echo -e "\nThis is a test of the UNIX fold utility.\n\n\tNotice that indented paragraphs don't get indented as a whole." | fold -s -w 20

This is a test of 
the UNIX fold 
utility.

        Notice that 
indented paragraphs 
don't get indented 
as a whole.
```

Using `ifold` instead will produce the following:

```
> echo -e "\nThis is a test of the ifold utility.\n\n\tNotice that indented paragraphs will be indented as a whole." | ifold 20

This is a test of
the ifold utility.

        Notice that
        indented
        paragraphs
        will be
        indented as
        a whole.
```

This behavior matches the display behavior of many text editors operating in word-wrap mode. This means that using `ifold` to format your file will lead to it resembling its display in your text editor much more closely.

I've personally used `ifold` to format class notes which I take in plain text format and wanted to print. It was this use case which brought upon the creation of the utility, but I anticipate that there are other useful ways to use `ifold` as well.

Bug reports, suggestions, questions, comments — they are welcome!
