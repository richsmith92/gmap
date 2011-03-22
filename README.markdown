My google summer of code project - 'Efficient maps using generalised tries'.

Notice that there is a natural correspondence between the definition of a list and the definition of a traditional trie:

    data List a =
        Null
      | Cons a (a list)

    data Trie a b =
      Trie 
        (Maybe b) -- corresponds to Null
        (Map a (Trie a b)) -- correspons to Cons a (a list)

This can be generalised to define tries for any algebraic datatype. Sum types in the datatype become products in the trie. Product types in the datatype become maps in the trie. 

Building on previous work by Adrian Hey, GMap provides an extensive Map class and trie constructors for a range of datatypes. All the code is thoroughly tested including tests for various strictness/laziness contracts. 

[Hackage page](http://hackage.haskell.org/package/gmap)
