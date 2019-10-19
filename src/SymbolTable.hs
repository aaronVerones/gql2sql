module SymbolTable where
  -- A Symbol Table binds Identifiers to AST Elements to make AST navigation easier.
  data SymbolTable valueType = Empty
                             | Node [Char] valueType (SymbolTable valueType)

  -- Adds the key/value to the symbol table if it does not exist.
  -- Replaces the element if it does exist.
  addElement :: SymbolTable a -> [Char] -> a -> SymbolTable a
  addElement Empty key value = Node key value Empty
  addElement (Node key value next) newKey newValue =
    if (key == newKey) then (Node key newValue next)
    else (Node key value (addElement next newKey newValue))

  -- Removes the key/value from the symbol if it exists.
  removeElement :: SymbolTable a -> [Char] -> SymbolTable a
  removeElement Empty key = Empty
  removeElement (Node curKey curValue next) key =
    if (key == curKey) then next
    else (Node curKey curValue (removeElement next key))

  -- Returns the value with the given key in the SymbolTable.
  findElement :: SymbolTable a -> [Char] -> Maybe a
  findElement Empty key = Nothing
  findElement (Node curKey curValue next) key =
    if (key == curKey) then (Just curValue)
    else findElement next key
