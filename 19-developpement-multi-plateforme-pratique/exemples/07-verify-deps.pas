{ ============================================================================
  Section 19.7 : Gestion des dependances externes
  Description : Script de verification des dependances systeme
  Fichier source : 07-gestion-dependances-externes.md
  ============================================================================ }
program VerifyDeps;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs, Classes;  // DynLibs : LoadLibrary, UnloadLibrary, TLibHandle

type
  TDependency = record
    Name: string;
    {$IFDEF WINDOWS}
    FileName: string;
    {$ENDIF}
    {$IFDEF LINUX}
    FileName: string;
    {$ENDIF}
    {$IFDEF DARWIN}
    FileName: string;
    {$ENDIF}
    Required: Boolean;
  end;

const
  Dependencies: array[0..2] of TDependency = (
    (
      Name: 'SQLite';
      {$IFDEF WINDOWS} FileName: 'sqlite3.dll'; {$ENDIF}
      {$IFDEF LINUX} FileName: 'libsqlite3.so.0'; {$ENDIF}
      {$IFDEF DARWIN} FileName: 'libsqlite3.dylib'; {$ENDIF}
      Required: True
    ),
    (
      Name: 'OpenSSL';
      {$IFDEF WINDOWS} FileName: 'libssl-1_1.dll'; {$ENDIF}
      {$IFDEF LINUX} FileName: 'libssl.so.1.1'; {$ENDIF}
      {$IFDEF DARWIN} FileName: 'libssl.dylib'; {$ENDIF}
      Required: True
    ),
    (
      Name: 'libcurl';
      {$IFDEF WINDOWS} FileName: 'libcurl.dll'; {$ENDIF}
      {$IFDEF LINUX} FileName: 'libcurl.so.4'; {$ENDIF}
      {$IFDEF DARWIN} FileName: 'libcurl.dylib'; {$ENDIF}
      Required: False
    )
  );

function TestDependency(const Dep: TDependency): Boolean;  
var  
  Handle: TLibHandle;
begin
  Handle := LoadLibrary(Dep.FileName);
  Result := Handle <> 0;

  if Result then
  begin
    WriteLn('[OK] ', Dep.Name, ' trouve (', Dep.FileName, ')');
    UnloadLibrary(Handle);
  end
  else
  begin
    if Dep.Required then
      WriteLn('[ERREUR] ', Dep.Name, ' MANQUANT (', Dep.FileName, ')')
    else
      WriteLn('[WARN] ', Dep.Name, ' non trouve (optionnel)');
  end;
end;

var
  i: Integer;
  AllOK: Boolean;
begin
  WriteLn('=================================');
  WriteLn('Verification des dependances');
  WriteLn('=================================');
  WriteLn;

  AllOK := True;

  for i := 0 to High(Dependencies) do
  begin
    if not TestDependency(Dependencies[i]) then
    begin
      if Dependencies[i].Required then
        AllOK := False;
    end;
  end;

  WriteLn;
  WriteLn('=================================');
  if AllOK then
    WriteLn('Toutes les dependances requises sont presentes !')
  else
  begin
    WriteLn('ERREUR : Des dependances sont manquantes !');
    WriteLn('Consultez le fichier README pour les instructions d''installation.');
    Halt(1);
  end;
end.
