; OPOR — установщик плагина AutoCAD (ApplicationPlugins bundle).
;
; Логика:
;   * ставится в профиль пользователя (%APPDATA%), права администратора
;     и запрос UAC в обычном сценарии не нужны вообще;
;   * при запуске находит ВСЕ прежние копии OPOR во всех папках, которые
;     сканирует AutoCAD, и удаляет их — включая папки-бэкапы, которые
;     оставлял прежний установщик на PowerShell (OPOR.bundle.backup-*)
;     и брошенные промежуточные (OPOR.bundle.installing-*);
;   * если плагин уже установлен — предлагает выбор: обновить или удалить.

#define MyAppName "OPOR"
#define MyAppVersion "3.32.0"
#define MyAppPublisher "sayan group"
#define MyAppURL "https://t.me/loxopuzik"

[Setup]
AppId={{37B91766-2092-4B1F-A227-5D3F55A6EFA2}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
VersionInfoVersion={#MyAppVersion}
VersionInfoDescription=OPOR - расстановка регулируемых опор Level 3D/PRO
; Только профиль пользователя: AutoCAD сканирует эту папку наравне
; с машинными, поэтому UAC не нужен.
DefaultDirName={userappdata}\Autodesk\ApplicationPlugins\OPOR.bundle
DisableDirPage=yes
DefaultGroupName=OPOR
DisableProgramGroupPage=yes
UsePreviousAppDir=no
PrivilegesRequired=lowest
OutputDir=output
OutputBaseFilename=OPOR_Setup_{#MyAppVersion}
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
InfoAfterFile=after_install.txt
UninstallDisplayName={#MyAppName} {#MyAppVersion}
SetupLogging=yes

[Languages]
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Files]
Source: "payload\PackageContents.xml"; DestDir: "{app}"; Flags: ignoreversion
Source: "payload\Contents\OPOR_bootstrap.lsp"; DestDir: "{app}\Contents"; Flags: ignoreversion
Source: "payload\Contents\OPOR\*"; DestDir: "{app}\Contents\OPOR"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{group}\Удалить OPOR"; Filename: "{uninstallexe}"

[UninstallDelete]
Type: filesandordirs; Name: "{app}"

[Code]
const
  BUNDLE_SUBPATH = 'Autodesk\ApplicationPlugins';

var
  MaintPage: TInputOptionWizardPage;
  BlockedPaths: TStringList;   // что не удалось удалить своими правами
  RemovedCount: Integer;

procedure ExitProcess(uExitCode: UINT);
  external 'ExitProcess@kernel32.dll stdcall';

{ ------------------------------------------------------------------ }
{  Поиск запущенного AutoCAD                                          }
{ ------------------------------------------------------------------ }
function IsAutoCADRunning(): Boolean;
var
  ResultCode: Integer;
  TmpFile, Upper: String;
  Lines: TArrayOfString;
  I: Integer;
begin
  Result := False;
  TmpFile := ExpandConstant('{tmp}\opor_tasklist.txt');
  if Exec(ExpandConstant('{cmd}'),
     '/C tasklist /FI "IMAGENAME eq acad.exe" /FO CSV /NH > "' + TmpFile + '"' +
     ' & tasklist /FI "IMAGENAME eq acadlt.exe" /FO CSV /NH >> "' + TmpFile + '"',
     '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    if LoadStringsFromFile(TmpFile, Lines) then
    begin
      for I := 0 to GetArrayLength(Lines) - 1 do
      begin
        Upper := Uppercase(Lines[I]);
        if (Pos('ACAD.EXE', Upper) > 0) or (Pos('ACADLT.EXE', Upper) > 0) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
  if FileExists(TmpFile) then
    DeleteFile(TmpFile);
end;

{ ------------------------------------------------------------------ }
{  Все папки, где AutoCAD ищет плагины                                }
{ ------------------------------------------------------------------ }
// Куда ставим. Считаем сами, а НЕ через константу app: она
// инициализируется позже, и обращение к ней из кода страниц мастера
// даёт "attempt to expand the app constant before it was initialized".
// Значение совпадает с DefaultDirName.
function GetTargetBundleDir(): String;
begin
  Result := ExpandConstant('{userappdata}\' + BUNDLE_SUBPATH + '\OPOR.bundle');
end;

{ Inno Pascal не умеет вложенные процедуры, поэтому список собираем
  через TStringList — он же убирает дубликаты путей. }
function GetPluginRoots(): TStringList;
var
  P: String;
begin
  Result := TStringList.Create;

  P := ExpandConstant('{userappdata}\' + BUNDLE_SUBPATH);
  if Result.IndexOf(P) < 0 then Result.Add(P);

  P := ExpandConstant('{commonappdata}\' + BUNDLE_SUBPATH);
  if Result.IndexOf(P) < 0 then Result.Add(P);

  P := ExpandConstant('{commonpf32}\' + BUNDLE_SUBPATH);
  if Result.IndexOf(P) < 0 then Result.Add(P);

  if IsWin64 then
  begin
    P := ExpandConstant('{commonpf64}\' + BUNDLE_SUBPATH);
    if Result.IndexOf(P) < 0 then Result.Add(P);
  end;
end;

{ Собрать все OPOR.bundle* в корне (сам bundle, backup-*, installing-*) }
procedure CollectOporDirs(Root: String; List: TStringList);
var
  FindRec: TFindRec;
begin
  if not DirExists(Root) then Exit;
  if FindFirst(AddBackslash(Root) + 'OPOR.bundle*', FindRec) then
  begin
    try
      repeat
        if (FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        begin
          if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
            List.Add(AddBackslash(Root) + FindRec.Name);
        end;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
end;

{ Все прежние копии, кроме той папки, куда ставим сейчас }
function FindLegacyInstalls(): TStringList;
var
  Roots, All: TStringList;
  I, J: Integer;
  Target: String;
begin
  Result := TStringList.Create;
  All := TStringList.Create;
  Roots := GetPluginRoots();
  try
    Target := RemoveBackslash(GetTargetBundleDir());
    for I := 0 to Roots.Count - 1 do
      CollectOporDirs(Roots[I], All);
    for J := 0 to All.Count - 1 do
      if CompareText(RemoveBackslash(All[J]), Target) <> 0 then
        Result.Add(All[J]);
  finally
    All.Free;
    Roots.Free;
  end;
end;

{ Удалить перечисленное с повышением прав (одно окно UAC на всё сразу) }
function RemoveElevated(Paths: TStringList): Boolean;
var
  Args: String;
  I, ResultCode: Integer;
begin
  Args := '/C ';
  for I := 0 to Paths.Count - 1 do
    Args := Args + 'rmdir /s /q "' + RemoveBackslash(Paths[I]) + '" & ';
  Args := Args + 'exit /B 0';
  Result := ShellExec('runas', ExpandConstant('{cmd}'), Args, '',
                      SW_HIDE, ewWaitUntilTerminated, ResultCode);
end;

{ Основная очистка. Заполняет BlockedPaths тем, что не поддалось. }
procedure RemoveLegacyInstalls();
var
  Legacy: TStringList;
  I: Integer;
begin
  RemovedCount := 0;
  BlockedPaths.Clear;
  Legacy := FindLegacyInstalls();
  try
    for I := 0 to Legacy.Count - 1 do
    begin
      Log('OPOR: найдена прежняя копия -> ' + Legacy[I]);
      if DelTree(Legacy[I], True, True, True) then
      begin
        RemovedCount := RemovedCount + 1;
        Log('OPOR: удалена ' + Legacy[I]);
      end
      else
      begin
        BlockedPaths.Add(Legacy[I]);
        Log('OPOR: НЕ УДАЛОСЬ удалить (нужны права администратора) ' + Legacy[I]);
      end;
    end;
  finally
    Legacy.Free;
  end;

  { Машинные копии не удаляются без администратора. Молча оставлять их
    нельзя: AutoCAD поднимет старую версию и человек решит, что
    обновление не сработало. Поэтому спрашиваем один раз. }
  if BlockedPaths.Count > 0 then
  begin
    if not WizardSilent() then
    begin
      if MsgBox('Найдена ранее установленная версия OPOR, общая для всех ' +
         'пользователей компьютера:' + #13#10#13#10 + BlockedPaths.Text + #13#10 +
         'Её нужно удалить, иначе AutoCAD может продолжать запускать старую ' +
         'версию.' + #13#10#13#10 +
         'Удалить сейчас? Windows запросит подтверждение администратора.',
         mbConfirmation, MB_YESNO) = IDYES then
      begin
        if RemoveElevated(BlockedPaths) then
        begin
          { проверяем, что реально исчезло }
          for I := BlockedPaths.Count - 1 downto 0 do
            if not DirExists(BlockedPaths[I]) then
            begin
              RemovedCount := RemovedCount + 1;
              BlockedPaths.Delete(I);
            end;
        end;
      end;
    end;
  end;
end;

{ ------------------------------------------------------------------ }
{  Уже установлено? (ключ, который пишет сам Inno)                    }
{ ------------------------------------------------------------------ }
function GetUninstallString(): String;
var
  Key, S: String;
begin
  Key := 'Software\Microsoft\Windows\CurrentVersion\Uninstall\' +
         '{37B91766-2092-4B1F-A227-5D3F55A6EFA2}_is1';
  S := '';
  if not RegQueryStringValue(HKCU, Key, 'UninstallString', S) then
    RegQueryStringValue(HKLM, Key, 'UninstallString', S);
  Result := S;
end;

function IsAlreadyInstalled(): Boolean;
begin
  Result := (GetUninstallString() <> '') or
            FileExists(AddBackslash(GetTargetBundleDir()) + 'PackageContents.xml');
end;

function RunExistingUninstaller(): Boolean;
var
  S: String;
  ResultCode: Integer;
begin
  Result := False;
  S := RemoveQuotes(GetUninstallString());
  if S <> '' then
    Result := Exec(S, '/SILENT /NORESTART /SUPPRESSMSGBOXES', '',
                   SW_SHOW, ewWaitUntilTerminated, ResultCode);
end;

{ ------------------------------------------------------------------ }
{  Мастер                                                             }
{ ------------------------------------------------------------------ }
procedure InitializeWizard();
begin
  BlockedPaths := TStringList.Create;
  MaintPage := CreateInputOptionPage(wpWelcome,
    'OPOR уже установлен',
    'Выберите, что сделать.',
    'На компьютере уже установлен плагин OPOR. Выберите действие и нажмите «Далее».',
    True, False);
  MaintPage.Add('Переустановить / обновить — старая версия будет удалена');
  MaintPage.Add('Удалить OPOR с компьютера');
  MaintPage.SelectedValueIndex := 0;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;
  if PageID = MaintPage.ID then
    Result := not IsAlreadyInstalled();
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if CurPageID = MaintPage.ID then
  begin
    if MaintPage.SelectedValueIndex = 1 then
    begin
      if IsAutoCADRunning() then
      begin
        MsgBox('Закройте AutoCAD и повторите — иначе файлы плагина удалить нельзя.',
               mbError, MB_OK);
        Result := False;
        Exit;
      end;
      if RunExistingUninstaller() then
        MsgBox('OPOR удалён с компьютера.', mbInformation, MB_OK)
      else
        MsgBox('Не удалось запустить удаление. Удалите OPOR через ' +
               '«Параметры → Приложения».', mbError, MB_OK);
      ExitProcess(0);
    end;
  end;
end;

function InitializeSetup(): Boolean;
begin
  Result := True;

  // Диагностика в лог. Заодно это проверка, что обе функции безопасны
  // на самом раннем этапе: InitializeSetup выполняется ДО инициализации
  // константы app, и обращение к ней отсюда сразу дало бы
  // "attempt to expand the app constant before it was initialized".
  Log('OPOR: целевая папка = ' + GetTargetBundleDir());
  if IsAlreadyInstalled() then
    Log('OPOR: обнаружена ранее установленная версия')
  else
    Log('OPOR: ранее установленных версий не обнаружено');

  if IsAutoCADRunning() then
  begin
    { В тихом режиме диалога нет: при /SUPPRESSMSGBOXES MsgBox вернёт
      «Повторить» сам, и ожидание никогда не кончится. }
    if WizardSilent() then
    begin
      Log('OPOR: установка прервана — запущен AutoCAD.');
      Result := False;
      Exit;
    end;
    while IsAutoCADRunning() do
    begin
      if MsgBox('Обнаружен запущенный AutoCAD.' + #13#10 +
         'Закройте все окна AutoCAD и нажмите «Повторить».' + #13#10#13#10 +
         '«Отмена» прервёт установку.', mbError, MB_RETRYCANCEL) = IDCANCEL then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    RemoveLegacyInstalls();

  if CurStep = ssPostInstall then
  begin
    if BlockedPaths.Count > 0 then
      MsgBox('OPOR установлен, но осталась прежняя копия, которую не удалось ' +
        'удалить без прав администратора:' + #13#10#13#10 + BlockedPaths.Text + #13#10 +
        'Если AutoCAD будет запускать старую версию — удалите эту папку вручную.',
        mbInformation, MB_OK);
  end;
end;

procedure DeinitializeSetup();
begin
  if BlockedPaths <> nil then
    BlockedPaths.Free;
end;
