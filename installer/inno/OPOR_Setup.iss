; OPOR — установщик плагина AutoCAD (ApplicationPlugins bundle).
;
; Логика:
;   * ставится в профиль пользователя (%APPDATA%), права администратора
;     и запрос UAC в обычном сценарии не нужны вообще;
;   * находит установленные версии AutoCAD и даёт выбрать галочками, в
;     какие ставить. Bundle физически один, а ограничение по версиям
;     задаётся в PackageContents.xml через SeriesMin/SeriesMax: на каждый
;     выбранный релиз пишется свой блок <Components>. Поэтому файл
;     PackageContents.xml не копируется готовым, а генерируется здесь;
;   * находит и удаляет прежние копии OPOR во всех папках, которые
;     сканирует AutoCAD, включая мусор прежнего установщика на PowerShell
;     (OPOR.bundle.backup-*) и брошенные промежуточные (*.installing-*);
;   * если плагин уже установлен — предлагает обновить или удалить.

#define MyAppName "OPOR"
#define MyAppVersion "3.32.0"
#define MyAppPublisher "sayan group"
#define MyAppURL "https://sayangroup.ru/"

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
; PackageContents.xml здесь НЕТ — он генерируется в коде под выбранные версии.
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
  VerPage: TInputOptionWizardPage;
  BlockedPaths: TStringList;
  RelCodes: TStringList;          // R24.2, R25.1, ...
  RemovedCount: Integer;

procedure ExitProcess(uExitCode: UINT);
  external 'ExitProcess@kernel32.dll stdcall';

// Куда ставим. Считаем сами, а НЕ через константу app: она
// инициализируется позже, и обращение к ней из кода страниц мастера
// даёт "attempt to expand the app constant before it was initialized".
function GetTargetBundleDir(): String;
begin
  Result := ExpandConstant('{userappdata}\' + BUNDLE_SUBPATH + '\OPOR.bundle');
end;

{ ------------------------------------------------------------------ }
{  Запущенный AutoCAD                                                 }
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
{  Поиск установленных AutoCAD                                        }
{ ------------------------------------------------------------------ }
function HKLMRoot(): Integer;
begin
  // Установщик 32-битный, а AutoCAD пишет ключи в 64-битную ветку:
  // без HKLM64 мы смотрели бы в WOW6432Node и ничего не нашли.
  if IsWin64 then Result := HKLM64 else Result := HKLM;
end;

function HKCURoot(): Integer;
begin
  if IsWin64 then Result := HKCU64 else Result := HKCU;
end;

function ReleaseToName(Rel: String): String;
begin
  if Rel = 'R22.0' then Result := 'AutoCAD 2018'
  else if Rel = 'R23.0' then Result := 'AutoCAD 2019'
  else if Rel = 'R23.1' then Result := 'AutoCAD 2020'
  else if Rel = 'R24.0' then Result := 'AutoCAD 2021'
  else if Rel = 'R24.1' then Result := 'AutoCAD 2022'
  else if Rel = 'R24.2' then Result := 'AutoCAD 2023'
  else if Rel = 'R24.3' then Result := 'AutoCAD 2024'
  else if Rel = 'R25.0' then Result := 'AutoCAD 2025'
  else if Rel = 'R25.1' then Result := 'AutoCAD 2026'
  else Result := 'AutoCAD (' + Rel + ')';
end;

procedure ScanReleases(RootKey: Integer; List: TStringList);
var
  Names: TArrayOfString;
  I: Integer;
  Rel: String;
begin
  if RegGetSubkeyNames(RootKey, 'SOFTWARE\Autodesk\AutoCAD', Names) then
  begin
    for I := 0 to GetArrayLength(Names) - 1 do
    begin
      Rel := Trim(Names[I]);
      if (Length(Rel) > 1) and (Uppercase(Copy(Rel, 1, 1)) = 'R') then
        if List.IndexOf(Rel) < 0 then
          List.Add(Rel);
    end;
  end;
end;

procedure DetectAutoCAD();
begin
  RelCodes := TStringList.Create;
  ScanReleases(HKLMRoot(), RelCodes);
  ScanReleases(HKCURoot(), RelCodes);
  RelCodes.Sort;
end;

{ ------------------------------------------------------------------ }
{  Генерация PackageContents.xml под выбранные версии                 }
{ ------------------------------------------------------------------ }
function CommandsBlock(): String;
begin
  Result :=
    '      <Commands GroupName="OPOR_COMMANDS">' + #13#10 +
    '        <Command Local="OPOR" Global="OPOR" />' + #13#10 +
    '        <Command Local="XX" Global="XX" />' + #13#10 +
    '        <Command Local="OPORLOGS" Global="OPORLOGS" />' + #13#10 +
    '        <Command Local="OPORSLOPE" Global="OPORSLOPE" />' + #13#10 +
    '        <Command Local="OPORSLOPEWR" Global="OPORSLOPEWR" />' + #13#10 +
    '        <Command Local="OPORHEIGHTCHECK" Global="OPORHEIGHTCHECK" />' + #13#10 +
    '        <Command Local="OPORWRITELEVEL" Global="OPORWRITELEVEL" />' + #13#10 +
    '        <Command Local="OPORAUTOLEVEL" Global="OPORAUTOLEVEL" />' + #13#10 +
    '        <Command Local="OPORSLOPELEVELS" Global="OPORSLOPELEVELS" />' + #13#10 +
    '        <Command Local="OPORGEOLEVELS" Global="OPORGEOLEVELS" />' + #13#10 +
    '        <Command Local="OPORRING" Global="OPORRING" />' + #13#10 +
    '        <Command Local="OPORTIN" Global="OPORTIN" />' + #13#10 +
    '        <Command Local="OPORREGCHECK" Global="OPORREGCHECK" />' + #13#10 +
    '        <Command Local="OPORCHECK" Global="OPORCHECK" />' + #13#10 +
    '        <Command Local="OPORCLEAN" Global="OPORCLEAN" />' + #13#10 +
    '        <Command Local="OPORDUMP" Global="OPORDUMP" />' + #13#10 +
    '        <Command Local="OPORTABLES" Global="OPORTABLES" />' + #13#10 +
    '        <Command Local="OPOROLD" Global="OPOROLD" />' + #13#10 +
    '        <Command Local="OPORSHOW" Global="OPORSHOW" />' + #13#10 +
    '        <Command Local="OPORDEBUG" Global="OPORDEBUG" />' + #13#10 +
    '      </Commands>' + #13#10;
end;

function ComponentsBlock(Series, Note: String): String;
var
  Req: String;
begin
  // Platform и OS — ровно как в принятой рабочей версии манифеста,
  // чтобы не потерять вертикальные продукты (Civil 3D и т.п.).
  Req := '    <RuntimeRequirements OS="Win64" Platform="AutoCAD|AutoCAD*"' +
         ' SupportPath="./Contents/OPOR/"';
  if Series <> '' then
    Req := Req + ' SeriesMin="' + Series + '" SeriesMax="' + Series + '"';
  Req := Req + ' />' + #13#10;

  Result :=
    '  <Components Description="' + Note + '">' + #13#10 +
    Req +
    '    <ComponentEntry AppName="OPOR" AppDescription="OPOR AutoLISP bootstrap"' + #13#10 +
    '      ModuleName="./Contents/OPOR_bootstrap.lsp"' + #13#10 +
    '      PerDocument="True" LoadOnAutoCADStartup="False"' + #13#10 +
    '      LoadOnCommandInvocation="True">' + #13#10 +
    CommandsBlock() +
    '    </ComponentEntry>' + #13#10 +
    '  </Components>' + #13#10;
end;

procedure WritePackageContents();
var
  Xml: String;
  I, Picked: Integer;
begin
  Xml :=
    '<?xml version="1.0" encoding="UTF-8"?>' + #13#10 +
    '<ApplicationPackage' + #13#10 +
    '  SchemaVersion="1.0"' + #13#10 +
    '  AutodeskProduct="AutoCAD"' + #13#10 +
    '  ProductType="Application"' + #13#10 +
    '  ProductCode="{37B91766-2092-4B1F-A227-5D3F55A6EFA2}"' + #13#10 +
    '  UpgradeCode="{E56B3183-9FA1-4C48-880D-07201EE704B4}"' + #13#10 +
    '  AppVersion="{#MyAppVersion}"' + #13#10 +
    '  FriendlyVersion="{#MyAppVersion}"' + #13#10 +
    '  Name="OPOR"' + #13#10 +
    '  Description="Level 3D and Level PRO support layout for AutoCAD"' + #13#10 +
    '  Author="sayan group">' + #13#10 +
    '  <CompanyDetails Name="sayan group" Url="{#MyAppURL}" />' + #13#10 +
    '  <RuntimeRequirements OS="Win64" Platform="AutoCAD|AutoCAD*" />' + #13#10;

  Picked := 0;
  if (VerPage <> nil) and (RelCodes <> nil) then
  begin
    for I := 0 to RelCodes.Count - 1 do
    begin
      if VerPage.Values[I] then
      begin
        Xml := Xml + ComponentsBlock(RelCodes[I],
                 'OPOR for ' + ReleaseToName(RelCodes[I]));
        Picked := Picked + 1;
        Log('OPOR: включён релиз ' + RelCodes[I] + ' (' + ReleaseToName(RelCodes[I]) + ')');
      end
      else
        Log('OPOR: пропущен релиз ' + RelCodes[I]);
    end;
  end;

  // Ни одной версии не найдено (AutoCAD ещё не установлен) — пишем
  // блок без ограничений, чтобы плагин заработал после установки CAD.
  if Picked = 0 then
  begin
    Xml := Xml + ComponentsBlock('', 'OPOR AutoLISP components');
    Log('OPOR: версии не выбраны/не найдены — ограничение по релизам не задано');
  end;

  Xml := Xml + '</ApplicationPackage>' + #13#10;

  if SaveStringToFile(ExpandConstant('{app}\PackageContents.xml'), Xml, False) then
    Log('OPOR: PackageContents.xml записан, блоков: ' + IntToStr(Picked))
  else
    Log('OPOR: НЕ УДАЛОСЬ записать PackageContents.xml');
end;

{ ------------------------------------------------------------------ }
{  Удаление прежних копий                                             }
{ ------------------------------------------------------------------ }
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
          if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
            List.Add(AddBackslash(Root) + FindRec.Name);
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
end;

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

  if (BlockedPaths.Count > 0) and (not WizardSilent()) then
  begin
    if MsgBox('Найдена ранее установленная версия OPOR, общая для всех ' +
       'пользователей компьютера:' + #13#10#13#10 + BlockedPaths.Text + #13#10 +
       'Её нужно удалить, иначе AutoCAD может продолжать запускать старую ' +
       'версию.' + #13#10#13#10 +
       'Удалить сейчас? Windows запросит подтверждение администратора.',
       mbConfirmation, MB_YESNO) = IDYES then
    begin
      if RemoveElevated(BlockedPaths) then
        for I := BlockedPaths.Count - 1 downto 0 do
          if not DirExists(BlockedPaths[I]) then
          begin
            RemovedCount := RemovedCount + 1;
            BlockedPaths.Delete(I);
          end;
    end;
  end;
end;

{ ------------------------------------------------------------------ }
{  Уже установлено?                                                   }
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
var
  I: Integer;
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

  DetectAutoCAD();
  VerPage := CreateInputOptionPage(MaintPage.ID,
    'Версии AutoCAD',
    'В какие версии установить плагин?',
    'Найденные на компьютере версии перечислены ниже. Снимите галочки с тех, ' +
    'где плагин не нужен — в них он загружаться не будет.',
    False, False);
  for I := 0 to RelCodes.Count - 1 do
  begin
    VerPage.Add(ReleaseToName(RelCodes[I]) + '   (' + RelCodes[I] + ')');
    VerPage.Values[I] := True;          // по умолчанию отмечены все
  end;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;
  if PageID = MaintPage.ID then
    Result := not IsAlreadyInstalled();
  // Ни одной версии не нашли — спрашивать не о чем.
  if PageID = VerPage.ID then
    Result := (RelCodes = nil) or (RelCodes.Count = 0);
end;

function AnyVersionPicked(): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to RelCodes.Count - 1 do
    if VerPage.Values[I] then
    begin
      Result := True;
      Exit;
    end;
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

  if CurPageID = VerPage.ID then
  begin
    if not AnyVersionPicked() then
    begin
      MsgBox('Отметьте хотя бы одну версию AutoCAD, иначе ставить некуда.',
             mbError, MB_OK);
      Result := False;
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
    // В тихом режиме диалога нет: при /SUPPRESSMSGBOXES MsgBox вернёт
    // «Повторить» сам, и ожидание никогда не кончится.
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
    // Пишем ПОСЛЕ копирования файлов: папка {app} к этому моменту есть.
    WritePackageContents();

    if BlockedPaths.Count > 0 then
      MsgBox('OPOR установлен, но осталась прежняя копия, которую не удалось ' +
        'удалить без прав администратора:' + #13#10#13#10 + BlockedPaths.Text + #13#10 +
        'Если AutoCAD будет запускать старую версию — удалите эту папку вручную.',
        mbInformation, MB_OK);
  end;
end;

procedure DeinitializeSetup();
begin
  if BlockedPaths <> nil then BlockedPaths.Free;
  if RelCodes <> nil then RelCodes.Free;
end;
