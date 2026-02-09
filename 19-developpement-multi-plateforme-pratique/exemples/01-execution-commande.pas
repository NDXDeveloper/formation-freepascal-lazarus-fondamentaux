{
  Section 19.1 - Différences fondamentales Windows/Linux
  Description : Exécution portable de commande système via TProcess
                (ls sous Linux, dir sous Windows)
  Fichier source : 01-differences-windows-linux.md
}
program ExecutionCommande;

{$mode objfpc}{$H+}

uses
  SysUtils, Process;

procedure ExecuterCommande(Commande: string);
var
  Proc: TProcess;
  Sortie: string;
  BytesLus: LongInt;
  Buffer: array[0..4095] of Byte;
begin
  Proc := TProcess.Create(nil);
  try
    {$IFDEF WINDOWS}
    Proc.Executable := 'cmd.exe';
    Proc.Parameters.Add('/c');
    {$ENDIF}

    {$IFDEF LINUX}
    Proc.Executable := '/bin/bash';
    Proc.Parameters.Add('-c');
    {$ENDIF}

    Proc.Parameters.Add(Commande);
    Proc.Options := [poUsePipes, poStderrToOutPut];
    Proc.Execute;

    { Lire la sortie du processus }
    Sortie := '';
    repeat
      BytesLus := Proc.Output.Read(Buffer, SizeOf(Buffer));
      if BytesLus > 0 then
        Sortie := Sortie + Copy(PChar(@Buffer[0]), 1, BytesLus);
    until BytesLus = 0;

    WriteLn(Sortie);
  finally
    Proc.Free;
  end;
end;

begin
  WriteLn('=== Exécution portable de commandes ===');
  WriteLn;

  {$IFDEF WINDOWS}
  WriteLn('Plateforme : Windows');
  WriteLn('Commande   : dir');
  WriteLn('---');
  ExecuterCommande('dir');
  {$ENDIF}

  {$IFDEF LINUX}
  WriteLn('Plateforme : Linux');
  WriteLn('Commande   : ls -la');
  WriteLn('---');
  ExecuterCommande('ls -la');
  {$ENDIF}
end.
