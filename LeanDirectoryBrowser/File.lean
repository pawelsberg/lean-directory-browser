import LeanDirectoryBrowser.FilePath

inductive File : Type
| directory
  (path : String)
  (children : List File) : File
| file
  (path : String)
  deriving Repr
namespace File
  -- check if equal
  def beq (file1: File) (file2: File): Bool :=
    let rec beqAux : File → File → Bool
    | File.directory path1 children1,
      File.directory path2 children2 =>
      path1 == path2
      && match children1, children2 with
         | [], [] => true
         | f1 :: fs1, f2 :: fs2 => beqAux f1 f2
            && beqAux (File.directory path1 fs1) (File.directory path2 fs2)
         | _, _ => false
    | File.file path1, File.file path2 => path1 == path2
    | _, _ => false
    beqAux file1 file2
  instance : BEq File := ⟨File.beq⟩

  def path : File → String
    | directory path _ => path
    | file path => path

  def parentPath : File → String
    | directory path _ => FilePath.parentPath path
    | file path => FilePath.parentPath path

  def filename (file : File) : String :=
    match file.path.splitOn "/" with
    | []        => ""
    | parts     => parts.getLastD ""

  -- find a directory in the subtree of the root directory
  def findDirectory (root: File) (path : String) : Option File :=
    let rec findDirectoryAux : File → String → Option File
    | File.directory current_path children, path =>
      if current_path == path then
        some (File.directory current_path children)
      else
        match children with
        | [] => none
        | f :: fs =>
          match findDirectoryAux f path with
          | some d => some d
          | none => findDirectoryAux (File.directory current_path fs) path
    | File.file _, _ => none
    findDirectoryAux root path

  def findPreviousFile : List File → String → String
    | [], currentFilePath => currentFilePath
    | f :: fs, currentFilePath =>
      match fs with
      | [] => currentFilePath
      | f' :: _ =>
        if currentFilePath == f'.path  then
            f.path
          else
            findPreviousFile fs currentFilePath

  def findNextFile : List File → String → String
    | [], currentFilePath => currentFilePath
    | f :: fs, currentFilePath =>
      if currentFilePath == f.path then
        match fs with
        | [] => f.path
        | f' :: _ => f'.path
      else
        findNextFile fs currentFilePath

  def indexOfFile : List File → String → Nat
    | [], _ => 0
    | f :: fs, currentFilePath =>
      if currentFilePath == f.path then
        0
      else
        1 + indexOfFile fs currentFilePath

end File
