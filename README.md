# lean-directory-browser
It is a windows folder explorer written in lean4 (using code-proxy).

## Running project
1. Build and copy code-proxy binaries to the Al subfolder of the output folder:
```.lake/build/bin/Al```
2. Build the project:
```lake build```
3. Run:
```
cd .lake/build/bin
.\leandirectorybrowser.exe
```

## Versioning
The versioning follows the following patterns:
- v(major).(minor) - for a new release

or

- v(major).(minor).(patch) - for a bug fix, refactoring or specification update only. This pattern is never used to release a new feature.

Major of 0 means that the library is in the development stage not ready for production.

Branches for these are:

```
release/(major)/(minor)/main 
```

or 

```
release/(major)/(minor)/(patch)/main
```	