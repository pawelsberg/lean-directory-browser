import LeanDirectoryBrowser.FilePath

inductive File : Type
| directory
  (path : String)
  (children : Option (List File))
| file
  (path : String)
  deriving Repr, BEq

namespace File
  def path : File → String
    | directory path _ => path
    | file path => path

  def isDirectory : File → Prop
    | directory _ _ => True
    | file _ => False

  def isLoadedNonEmptyDirectory : File → Prop
    | directory _ (some children) =>
      match children with
      | [] => False
      | _ => True
    | _ => False

  def isLoadedEmptyDirectory : File → Prop
    | directory _ (some children) =>
      match children with
      | [] => True
      | _ => False
    | _ => False


  def parentPath (file: File): String :=
    FilePath.parentPath file.path

  def filename (file : File) : String :=
    (file.path.splitOn FilePath.pathDelimiter).getLastD ""

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

  def replaceFile (root: File) (path : String) (newFile : File) : File :=
    let rec replaceFileAux (file: File) (path: String) : File :=
      match file with
      | File.directory current_path children =>
        if current_path == path then
          newFile
        else
          match children with
          | none => file
          | some [] => file
          | some (f :: fs) =>
            let newChildren := replaceFileAux f path :: match (replaceFileAux (File.directory current_path fs) path) with
              | File.directory _ (some children) => children
              | _ => []
            File.directory current_path (some newChildren)
      | File.file current_path =>
        if current_path == path then
          newFile
        else
          file
    replaceFileAux root path

  def findPreviousFilePath : List File → String → String
    | [], currentFilePath => currentFilePath
    | f :: fs, currentFilePath =>
      match fs with
      | [] => currentFilePath
      | f' :: _ =>
        if currentFilePath == f'.path  then
            f.path
          else
            findPreviousFilePath fs currentFilePath

  def findNextFilePath : List File → String → String
    | [], currentFilePath => currentFilePath
    | f :: fs, currentFilePath =>
      if currentFilePath == f.path then
        match fs with
        | [] => f.path
        | f' :: _ => f'.path
      else
        findNextFilePath fs currentFilePath

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
#eval exRoot.replaceFile "c:\\src\\dir1" (File.directory "c:\\src\\dir1" none)
