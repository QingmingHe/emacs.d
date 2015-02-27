set objWSHShell = CreateObject("WScript.Shell")
set objFso = CreateObject("Scripting.FileSystemObject")

' command line arguments
sShortcut = objWSHShell.ExpandEnvironmentStrings(WScript.Arguments.Item(0))
sTargetPath = objWSHShell.ExpandEnvironmentStrings(WScript.Arguments.Item(1))
sWorkingDirectory = objFso.GetAbsolutePathName(sShortcut)

set objSC = objWSHShell.CreateShortcut(sShortcut)

objSC.TargetPath = sTargetPath
objSC.WorkingDirectory = sWorkingDirectory

objSC.Save