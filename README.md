# MD-to-HTML-Haskell

This  is a Markdown to Html converter. I follow only the Markdown guidelines
from the basic syntax portion of https://www.markdownguide.org/cheat-sheet/ . That means
the only elements covered in my converter are

Heading # H1, ## H2, ### H3
Bold **Bold Text**
Italic *Italicized text*
Blockquote > blockquote
Code `code`
Horizontal Rule ---
Link [title](https://www.example.com)
Image ![alt text](image.jpg)

There is functionality for both Ordered and Unordered Lists with 1. 2. 3. and - - - as options to
create the list, however they are both output the same as bullets. To use this converter, you
need to be in the current directory of the Converter.hs file. Compile it with ghc --make
Converter. Once it is compiled you run the executable by typing ./Converter. You’ll be
prompted to “Enter a file to convert”, input the name of the .md file you’d like converted to
.html in the same directory. If everything was done properly then you’ll have a converted.html
file saved in your current directory and that’s it!
