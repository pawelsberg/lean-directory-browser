import LeanDirectoryBrowser.ProgramState
import LeanDirectoryBrowser.Allegro


def main : IO Unit := do
  let (cpp : Al.CodeProxyProcess) ← IO.Process.spawn {
    cmd := "Al/CodeProxy.Console.exe",
    args := #[],
    stdin := IO.Process.Stdio.piped,
    stdout := IO.Process.Stdio.piped
    }
  cpp.init

  let children ← File.readChildren "c:\\Windows"
  let mut state := {example_program_state  with
    root := File.directory "c:\\Windows" (File.sortFiles children),
    currentDirectoryPath := "c:\\Windows",
    selectedFilePath := "c:\\Windows\\AppReadiness",
    fileOnTopPath := "c:\\Windows\\AppReadiness",
  }

  IO.println (repr children).pretty
  state.initFonts cpp
  cpp.run
  cpp.flush
  repeat do
    let alOutput ← cpp.getOutputLine
    IO.println alOutput
    let newState := ProgramState.process state alOutput
    if not newState.exitRequested then
      if not (newState.beq state) then do
        newState.draw state cpp
        IO.println "State change"
      else do
        IO.println "No state change"
      IO.sleep (ms:=1)
      state := newState
      continue
    else
      IO.println "Program ending..."
      newState.destroyFonts cpp
      cpp.run
      cpp.flush
      cpp.exit
      cpp.run
      cpp.flush
      break

  IO.println "Ended."
  cpp.waitForProcessExit >>= IO.print
