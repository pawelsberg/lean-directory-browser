import Lake
open Lake DSL

package «LeanDirectoryBrowser» where
  -- add package configuration options here

lean_lib «LeanDirectoryBrowser» where
  -- add library configuration options here

@[default_target]
lean_exe «leandirectorybrowser» where
  root := `Main
