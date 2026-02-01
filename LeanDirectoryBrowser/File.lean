import LeanDirectoryBrowser.FilePath

inductive File : Type
| directory (path : String) (children : Option (List File))
| file (path : String)
  deriving Repr

mutual
  private def beqFile : File → File → Bool
    | .file p, .file p' => p == p'
    | .directory p c, .directory p' c' => p == p' && beqOpt c c'
    | _, _ => false
  private def beqOpt : Option (List File) → Option (List File) → Bool
    | some xs, some ys => beqList xs ys
    | none, none => true
    | _, _ => false
  private def beqList : List File → List File → Bool
    | x::xs, y::ys => beqFile x y && beqList xs ys
    | [], [] => true
    | _, _ => false
end

instance : BEq File := ⟨beqFile⟩

mutual
  private theorem beqFile_eq : beqFile a b = true → a = b := by
    cases a <;> cases b <;> intro h <;> simp_all [beqFile]
    case directory.directory p c p' c' =>
      cases hp : (p == p') with
      | false => cases hc : beqOpt c c' <;> simp_all
      | true => cases hc : beqOpt c c' with
        | false => simp_all
        | true => simp_all [LawfulBEq.eq_of_beq hp, beqOpt_eq hc]
  private theorem beqOpt_eq : beqOpt c d = true → c = d := by
    cases c <;> cases d <;> intro h <;> simp_all [beqOpt]
    case some.some => simp_all [beqList_eq h]
  private theorem beqList_eq : beqList xs ys = true → xs = ys := by
    cases xs <;> cases ys <;> intro h <;> simp_all [beqList]
    case cons.cons x xs y ys =>
      cases hx : beqFile x y with
      | false => cases hxs : beqList xs ys <;> simp_all
      | true => cases hxs : beqList xs ys with
        | false => simp_all
        | true => simp_all [beqFile_eq hx, beqList_eq hxs]
end

mutual
  private theorem beqFile_rfl : beqFile a a = true := by cases a <;> simp [beqFile, beqOpt_rfl]
  private theorem beqOpt_rfl : beqOpt c c = true := by cases c <;> simp [beqOpt, beqList_rfl]
  private theorem beqList_rfl : beqList xs xs = true := by cases xs <;> simp [beqList, beqFile_rfl, beqList_rfl]
end

instance : LawfulBEq File where
  eq_of_beq h := beqFile_eq h
  rfl := beqFile_rfl

namespace File
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

  def path : File → String
    | directory path _ => path
    | file path => path
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
