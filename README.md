# Calico :cat2:
Calico is a small utility written in Haskell which is `cat` but for colors.

It recognizes one (1) file format, GPL (also known as KPL in Krita).

Its output is highly configurable, and it is able to convert between RGB and HSL.

It also features a primitive command language for modifying the hue, saturation and luminosity values of all colors in the palette.

# Demonstration
[![asciicast](https://asciinema.org/a/TbvWQsfoiIa8yx9SXm3KxRTLz.svg)](https://asciinema.org/a/TbvWQsfoiIa8yx9SXm3KxRTLz)

On my machine at least, there are no gaps between the lines in the grid view.

# Motivations

`calico` was born out of a frustration with having to open a full image editor
in order to view the colors contained within a palette. It makes use of true-color terminal
escape codes to display the colors with accuracy on the command line.

# Further features
Feel free to request changes or features under the issues manager.
