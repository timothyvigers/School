# Drawing a Graph (adopted from John Stasko’s CS4460 at Georgia Tech)

The purpose of this assignment is to give you an appreciation of just how challenging it is to lay out a graph (network) in the plane. Below is an adjacency matrix for an undirected graph. Your objective here is to come up with a positioning for all the nodes and edges such that an aesthetically pleasing graph drawing results. Please draw the graph using a standard technique: nodes are represented by some kind of glyph such as a circle, square, etc. with the node number inside. Edges are simply connections drawn between the nodes. Follow those basics, then you are free to embellish beyond that. Remember that you can derive additional attributes from the graph structure and map them on visual features.

Don’t use any graph drawing software for this assignment. You can either draw the graph by hand on paper and take a photo or you can use a drawing tool such as Powerpoint to place the nodes and edges in the plane.

You should turn in a pdf with two pages:

The first page should contain your graph drawing (you can use the entire side of an 8.5" X 11" piece of paper). Don’t put your name on this page. (2 points)

The second page should contain three paragraphs describing your design process and the method or algorithm you used to create the graph. (3 points)

This is just a short assignment, don't spend too much time on it. (It turns out that you could spend the rest of your life on it.) Create a reasonable layout and justify its design in your description. I will create an overview pictures of all the drawings and we will vote on the most liked design in class (voting results won’t influence grading).

      1  2   3   4  5   6   7  8   9   10
     ---------------------------------------

  1 | 0  0   1   0  0   1   1  0   0   0

  2 | 0  0   1   0  0   1   0  1   1   0

  3 | 1  1   0   0   0  0   0   0  0   1

  4 | 0  0   0   0  1   0   1  0   1   0

  5 | 0  0   0   1  0   0   0  1   0   0

  6 | 1  1   0   0  0   0   0  0   1   1

  7 | 1  0   0   1  0   0   0  1   0   0

  8 | 0  1   0   0  1   0   1  0   0   0

  9 | 0  1   0   1  0   1   0  0   0   0

 10 | 0  0   1   0  0   1   0  0   0   0