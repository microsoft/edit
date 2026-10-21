#ifndef AppVersion
  #define AppVersion "0.0.0"
#endif

[Setup]
AppId={{1717C176-3A2F-4E01-83C4-916424E34160}
AppName=Microsoft Edit
DefaultGroupName=Microsoft Edit
AppVersion={#AppVersion}
AppPublisher=Microsoft Corporation
AppPublisherURL=https://github.com/microsoft/edit
AppSupportURL=https://github.com/microsoft/edit
AppUpdatesURL=https://github.com/microsoft/edit
SetupMutex=microsoft-edit-setup
DefaultDirName={autopf}\Microsoft Edit
DisableDirPage=yes
DisableProgramGroupPage=yes
SetupIconFile=edit.ico
UninstallDisplayIcon={app}\msedit.exe
MinVersion=10.0
ArchitecturesAllowed={#ArchitecturesAllowed}
#if ArchitecturesAllowed != "x86"
ArchitecturesInstallIn64BitMode={#ArchitecturesAllowed}
#endif
PrivilegesRequired=admin
ChangesEnvironment=yes
SolidCompression=yes
WizardStyle=modern dynamic
OutputBaseFilename=edit

#ifdef SignedUninstallerDir
SignedUninstaller=yes
SignedUninstallerDir={#SignedUninstallerDir}
#endif

[Tasks]
Name: "path"; Description: "Add msedit to the system &PATH"; Flags: checkablealone
Name: "path\edit"; Description: "Also provide it as &edit, taking precedence over the edit.exe shipped with Windows"; Flags: dontinheritcheck

[Files]
Source: {#Source}; DestDir: "{app}"; DestName: "msedit.exe"; Flags: notimestamp ignoreversion

; Just in case, ensure that the install dir is in a clean state.
; This also ensures that edit is not currently being used. :)
[InstallDelete]
Type: filesandordirs; Name: "{app}"

[UninstallDelete]
Type: filesandordirs; Name: "{app}"

[Code]
function CreateHardLink(lpFileName, lpExistingFileName: String; lpSecurityAttributes: LongWord): Boolean;
external 'CreateHardLinkW@kernel32.dll stdcall';

const
    ENV_KEY = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';

var
    g_AppDirPath: String;
    g_MseditExePath: String;
    g_EditExePath: String;

procedure InitializeGlobals;
begin
    g_AppDirPath := ExpandConstant('{app}');
    g_MseditExePath := ExpandConstant('{app}\msedit.exe');
    g_EditExePath := ExpandConstant('{app}\edit.exe');
end;

procedure CreateHardlinks;
begin
    if not CreateHardLink(g_EditExePath, g_MseditExePath, 0) then
        RaiseException('Failed to create hardlink for edit.exe');
end;

// Install=False removes us from the PATH. Prepend=True places us ahead of
// System32 so that our edit.exe wins over the one shipped with Windows.
procedure ModifyPath(Install, Prepend: Boolean);
var
    PathsBefore, PathsAfter: TArrayOfString;
    PathsStringBefore, System32Path: String;
    I, Count, System32Index, AppPathIndex: Integer;
begin
    if not RegQueryStringValue(HKLM, ENV_KEY, 'Path', PathsStringBefore) then
        RaiseException('Failed to read system PATH');

    PathsBefore := StringSplit(PathsStringBefore, [';'], stExcludeEmpty);
    if GetArrayLength(PathsBefore) = 0 then
        RaiseException('Failed to parse system PATH');

    System32Index := -1;
    AppPathIndex := -1;
    if Install and Prepend then
    begin
        System32Path := ExpandConstant('{sys}');

        // Find the index of System32.
        System32Index := 0;
        for I := 0 to GetArrayLength(PathsBefore) - 1 do
        begin
            if PathStartsWith(PathsBefore[I], '%SystemRoot%\System32', True) or
            PathStartsWith(PathsBefore[I], System32Path, True) then
            begin
                System32Index := I;
                Break;
            end;
        end;

        // Find the index of our app path, if any. We want to retain the same
        // index between installations. Unless it was previously past System32.
        // We want it to be always before System32.
        AppPathIndex := 0;
        for I := 0 to System32Index - 1 do
        begin
            if PathStartsWith(PathsBefore[I], g_AppDirPath, True) then
            begin
                AppPathIndex := I;
                Break;
            end;
        end;
    end;

    // Remove any and all paths pointing to our app.
    // This doubles as an uninstall path.
    SetArrayLength(PathsAfter, GetArrayLength(PathsBefore) + 1);
    Count := 0;
    for I := 0 to GetArrayLength(PathsBefore) - 1 do
    begin
        if I = AppPathIndex then
        begin
            PathsAfter[Count] := g_AppDirPath;
            Count := Count + 1;
        end;
        if not PathStartsWith(PathsBefore[I], g_AppDirPath, True) then
        begin
            PathsAfter[Count] := PathsBefore[I];
            Count := Count + 1;
        end;
    end;

    // Otherwise we're happy to sit at the very end of the PATH.
    if Install and (not Prepend) then
    begin
        PathsAfter[Count] := g_AppDirPath;
        Count := Count + 1;
    end;

    SetArrayLength(PathsAfter, Count);

    if not RegWriteExpandStringValue(HKLM, ENV_KEY, 'Path', StringJoin(';', PathsAfter)) then
        RaiseException('Failed to write system PATH');
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
    Prepend: Boolean;
begin
    if CurStep = ssPostInstall then
    begin
        InitializeGlobals;

        Prepend := WizardIsTaskSelected('path\edit');
        if Prepend then
            CreateHardlinks;

        ModifyPath(WizardIsTaskSelected('path'), Prepend);
    end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
    if CurUninstallStep = usUninstall then
    begin
        InitializeGlobals;
        ModifyPath(False, False);
    end;
end;
