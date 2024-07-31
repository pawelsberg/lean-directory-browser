namespace FilePath
  def parentPath (path: String) : String :=
    let parts := path.splitOn "/"
    let partsWithoutEmptyLast := if parts.getLast? == some "" then parts.dropLast else parts
    match partsWithoutEmptyLast with
    | [] => ""
    | parts => (parts.drop 1).dropLast.foldl (λ acc part => acc ++ "/" ++ part) ""
end FilePath

#eval FilePath.parentPath "/a/b/c" -- "/a/b"
#eval FilePath.parentPath "/a/b" -- "/a"
#eval FilePath.parentPath "/a/b/" -- "/a"
#eval FilePath.parentPath "/a" -- ""
#eval FilePath.parentPath "/a/" -- ""
#eval FilePath.parentPath "/" -- ""
