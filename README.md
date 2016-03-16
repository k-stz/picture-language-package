# picture-language-package
Package to Implement SICP's Picture Language

Allows to draw pictures within a parallelogram in a desired pattern.

The parallelogram is transformable (computer graphics).



Efficiency

All the lines that are drawn are stored in a LIST, which is iterated through, creating
a fresh ffi-array, which is destroyed after feeding the data to the gpu, on each frame.
This can get slow fast for a significant number of lines.
This was a fast hack, which is based on code from another project.

If this is an issue, see the code in UPDATE-LINES-VAO. This function is updating the
VAO that has all the line drawing opengl state associated with it.

Lastly, a simpler, bottom-up, solution would be to just draw the lines without gl:CLEARing
the screen on each frame.