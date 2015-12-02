#A hyper-orthogonal data structure implementation

A ZZ structure (zzstruct) is an indexed collection of cells and their
neighbor lists. A cell is a generic container for arbitrary content,
formed by appending the tag "cell" to the list of a cell's index in
the zztruct and the content, which may be a deferentiable name.
Ex. In cell (cell 0 c0), "cell" is a literal symbol, "0" a literal
number, and "c0" a literal symbol, or name referencing other
content. A neighbor list is a list of ordered pairs identifying the
positive/upstream (left element), and negative/downstream (right
element) neighbors of a cell along each dimensional axis. All cells
are neighbors along the fundamental indexical axis, with positive and
negative ordering implicit, corresponding to the numerical order of
the cell indices. This fundamental index is used within the ordered
pairs for every axis of the neighbor list to identify cell
neighbors. The neighbor lists of all cells contain an entry for at
least this indexical axis, and for every axis inhabited by any cell,
thus maintaining length parity between neighbor lists. If a cell does
not have any neighbors in a dimension, it is its own upstream and
downstream neighbor. Therefore, the default minimal zzstruct is 
(((cell 0 0) (0 0))) - a zzstruct of one cell, which is a point along
the fundamental axis, and thus its own neighbor in both directions.

The "zz-" prefix is used to identify procedures as zzstruct specific.
