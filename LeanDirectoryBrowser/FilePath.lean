namespace FilePath
  def pathDelimiter : String := "\\"

  def parentPath (path: String) : String :=
    let parts := path.splitOn pathDelimiter
    let partsWithoutEmptyLast := if parts.getLast? == some "" then parts.dropLast else parts
    match partsWithoutEmptyLast with
    | [] => ""
    | parts => (parts.drop 1).dropLast.foldl (λ acc part => acc ++ pathDelimiter ++ part) parts.head!

end FilePath

#eval FilePath.parentPath "\\a\\b\\c" -- "\a\b"
#eval FilePath.parentPath "\\a\\b" -- "\a"
#eval FilePath.parentPath "\\a\\b\\" -- "\a"
#eval FilePath.parentPath "\\a" -- ""
#eval FilePath.parentPath "\\a\\" -- ""
#eval FilePath.parentPath "\\" -- ""
#eval FilePath.parentPath "C:\\Windows\\A" -- "C:\Windows"
#eval FilePath.parentPath "C:\\Windows\\A\\" -- "C:\Windows"
#eval FilePath.parentPath "C:\\" -- "C:"
