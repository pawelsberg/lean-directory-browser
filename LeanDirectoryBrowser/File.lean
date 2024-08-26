import LeanDirectoryBrowser.FilePath

inductive File : Type
| directory
  (path : String)
  (children : Option (List File)) : File
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
         | none, none => true
         | some [], some [] => true
         | some (f1 :: fs1), some (f2 :: fs2) => beqAux f1 f2
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
    match file.path.splitOn FilePath.pathDelimiter with
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
        | none => none
        | some [] => none
        | some (f :: fs) =>
          match findDirectoryAux f path with
          | some d => some d
          | none => findDirectoryAux (File.directory current_path fs) path
    | File.file _, _ => none
    findDirectoryAux root path

def replaceDirectory (root: File) (path : String) (newDirectory : File) : File :=
  let rec replaceDirectoryAux : File → String → File → File
    | File.directory current_path children, path, newDirectory =>
      if current_path == path then
        newDirectory
      else
        match children with
        | none => File.directory current_path none
        | some childrenList =>
          let updatedChildren :=
            childrenList.map (λ child =>
              match child with
              | File.directory p _ =>
                if p == path then newDirectory
                else replaceDirectoryAux child path newDirectory
              | _ => child
            )
          File.directory current_path (some updatedChildren)
    | File.file _, _, _ => root
    decreasing_by sorry
  replaceDirectoryAux root path newDirectory

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

  def readChildren (path : String) : IO (List File) := do
    let dirEntries ← System.FilePath.readDir path
    let children := dirEntries.toList.map (λ dirEntry => do
      let isDir ← System.FilePath.isDir dirEntry.path
      let child :=
      match isDir with
        | true => File.directory dirEntry.path.toString none
        | false => File.file dirEntry.path.toString
      return child
      )
    children.foldr (fun ioFile acc =>
      ioFile >>= fun file =>
        acc >>= fun files =>
          pure (file :: files)) (pure [])

  def sortFiles (files: List File) : List File :=
    (files.toArray.insertionSort (fun f1 f2 => match f1, f2 with
      | File.directory _ _, File.file _ => true
      | File.file _, File.directory _ _ => false
      | File.directory path1 _, File.directory path2 _ => path1 < path2
      | File.file path1, File.file path2 => path1 < path2
     )).toList

end File

def exRoot := (File.directory "c:\\src" (some [
  (File.file "c:\\src\\file1"),
  (File.directory "c:\\src\\dir1" (some [
    (File.file "c:\\src\\dir1\\file2"),
    (File.directory "c:\\src\\dir1\\dir2" none)
  ])),
  (File.file "c:\\src\\file3")
]))

#eval exRoot
#eval exRoot.replaceDirectory "c:\\src\\dir1" (File.directory "c:\\src\\dir1" none)
