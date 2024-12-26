import LeanDirectoryBrowser.File
import LeanDirectoryBrowser.Allegro
import LeanDirectoryBrowser.Domain.DisplayConstants
import LeanDirectoryBrowser.List

inductive ProgState
| start (rootDirectoryPath : String) -- loading root
| firstDirectoryLoaded (root : File) (hRootIsDir : File.isDirectory root) -- getting display width
| widthProvided (root : File) (hRootIsDir : File.isDirectory root) (displayWidth : Nat) -- getting display height
| heightProvided (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsDir : File.isDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) -- getting display column width
| emptyFolderBrowser (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsLoadedEmptyDir : File.isLoadedEmptyDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) (runPowerShell : Bool) -- reacting to keys
| folderBrowser (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsNonEmptyDirectory : File.isLoadedNonEmptyDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) (displayColumns : Nat) (displayColumnWidth : Nat) (selectedFilePath : String) (fileOnTopPath : String) (runPowerShell : Bool) -- reacting to keys
| changingDirectory (root : File) (hRootIsDir : File.isDirectory root) (currentDirectory : File) (hCurrentDirectoryIsDir : File.isDirectory currentDirectory) (displayWidth : Nat) (displayHeight : Nat) (displayRows : Nat) -- loading files when changing directory
| help (nextState : ProgState)
| error (nextState : ProgState) (errorMessage : String)
| exit -- exit program
deriving Repr, BEq

def isProgStateLoading (ps : ProgState) : Prop :=
  match ps with
  | ProgState.start _
  | ProgState.changingDirectory _ _ _ _ _ _ _ => true
  | _ => false
def isProgStateAnyFolderBrowser (ps : ProgState) : Prop :=
  match ps with
  | ProgState.emptyFolderBrowser _ _ _ _ _ _ _ _
  | ProgState.folderBrowser _ _ _ _ _ _ _ _ _ _ _ _ => true
  | _ => false

def isProgStateStart : ProgState → Prop
| ProgState.start _ => True
| _ => False
def isProgStateFirstDirectoryLoaded : ProgState → Prop
| ProgState.firstDirectoryLoaded _ _ => True
| _ => False
def isProgStateWidthProvided : ProgState → Prop
| ProgState.widthProvided _ _ _ => True
| _ => False
def isProgStateHeightProvided : ProgState → Prop
| ProgState.heightProvided _ _ _ _ _ _ _ => True
| _ => False
def isProgStateFolderBrowser : ProgState → Prop
| ProgState.folderBrowser _ _ _ _ _ _ _ _ _ _ _ _ => True
| _ => False
def isProgStateEmptyFolderBrowser : ProgState → Prop
| ProgState.emptyFolderBrowser _ _ _ _ _ _ _ _ => True
| _ => False
def isProgStateChangingDirectory : ProgState → Prop
  | ProgState.changingDirectory _ _ _ _ _ _ _ => True
  | _ => False
def isProgStateHelp : ProgState → Prop
| ProgState.help _ => True
| _ => False
def isProgStateError : ProgState → Prop
| ProgState.error _ _ => True
| _ => False
def isProgStateExit : ProgState → Prop
| ProgState.exit => True
| _ => False
