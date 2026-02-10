uses  
  SysUtils, Windows;

function ObtenirVersionOS: string;  
var  
  Info: TOSVersionInfo;
begin
  Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(Info) then
    Result := Format('Windows %d.%d', [Info.dwMajorVersion, Info.dwMinorVersion])
  else
    Result := 'Windows version inconnue';
end;

function ObtenirNomMachine: string;  
var  
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  if GetComputerName(@Buffer, Size) then
    Result := Buffer
  else
    Result := 'Inconnu';
end;
