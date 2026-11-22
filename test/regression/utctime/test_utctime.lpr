program TestUtcTime;
{$mode objfpc}{$H+}

uses
  {$IFDEF MSWINDOWS}Windows,{$ELSE}BaseUnix,{$ENDIF}
  SysUtils, DateUtils, RegressionUtils, GulpCommon;

function NativeTimeUtc(const AName: string): TDateTime;
{$IFDEF MSWINDOWS}
var
  handle: THandle;
  modified: TFileTime;
  utc: Windows.TSystemTime;
begin
  handle := CreateFileW(PWideChar(UnicodeString(AName)), FILE_READ_ATTRIBUTES,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  Check(handle <> INVALID_HANDLE_VALUE, 'open ' + AName);
  try
    Check(GetFileTime(handle, nil, nil, @modified), 'GetFileTime ' + AName);
  finally
    CloseHandle(handle);
  end;
  Check(FileTimeToSystemTime(modified, utc), 'FileTimeToSystemTime ' + AName);
  result := SystemTimeToDateTime(utc);
end;
{$ELSE}
var
  info: Stat;
begin
  Check(FpLStat(AName, info) = 0, 'lstat ' + AName);
  result := UnixToDateTime(info.st_mtime);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SetNativeTimeUtc(const AName: string; ATime: TDateTime);
var
  handle: THandle;
  modified: TFileTime;
  utc: Windows.TSystemTime;
begin
  DateTimeToSystemTime(ATime, utc);
  Check(SystemTimeToFileTime(utc, modified), 'SystemTimeToFileTime ' + AName);
  handle := CreateFileW(PWideChar(UnicodeString(AName)), FILE_WRITE_ATTRIBUTES,
    FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  Check(handle <> INVALID_HANDLE_VALUE, 'open ' + AName);
  try
    Check(Windows.SetFileTime(handle, nil, nil, @modified), 'SetFileTime ' + AName);
  finally
    CloseHandle(handle);
  end;
  Check(CompareDateTime(NativeTimeUtc(AName), ATime) = 0, 'native timestamp of ' + AName);
end;
{$ENDIF}

procedure CheckName(const AName: string; ATime: int64);
var
  utc: TDateTime;
begin
  utc := UnixToDateTime(ATime);
  Check(SetTimeUtc(AName, IncMilliSecond(utc, 987)) = 0, 'settimeutc ' + AName);
  Check(CompareDateTime(NativeTimeUtc(AName), utc) = 0, 'settimeutc writes whole UTC seconds for ' + AName);
  Check(CompareDateTime(GetTimeUtc(AName), utc) = 0, 'gettimeutc returns the UTC time of ' + AName);
end;

const
  Winter = 1579089600; // 2020-01-15 12:00:00 UTC
  Summer = 1593604801; // 2020-07-01 12:00:01 UTC
  {$IFDEF UNIX}
  LinkSummer = 1564653600; // 2019-08-01 10:00:00 UTC
  {$ENDIF}

procedure CheckRestored(const AName: string);
begin
  Check(CompareDateTime(NativeTimeUtc(AName),
    RecodeMilliSecond(NativeTimeUtc('../work/' + AName), 0)) = 0, 'restored time of ' + AName);
end;

begin
  EnterNewDir('work');
  WriteTextFile('winter', 'w');
  WriteTextFile('summer', 's');
  ForceDirectories('dir');
  {$IFDEF UNIX}
  Check(FpSymlink('summer', 'link') = 0, 'create link');
  CheckName('link', LinkSummer);
  {$ENDIF}
  CheckName('winter', Winter);
  CheckName('summer', Summer);
  CheckName('dir', Summer);
  {$IFDEF MSWINDOWS}
  SetNativeTimeUtc('summer', IncMilliSecond(UnixToDateTime(Summer), 987));
  Check(CompareDateTime(GetTimeUtc('summer'), UnixToDateTime(Summer)) = 0,
    'gettimeutc discards subsecond precision without rounding up');
  {$ENDIF}

  GulpOk(['-s', '../arc.gulp']);
  {$IFDEF MSWINDOWS}
  SetNativeTimeUtc('summer', IncMilliSecond(UnixToDateTime(Summer), 123));
  Check(Pos('Finished (0 ', GulpOk(['-s', '../arc.gulp'])) > 0,
    'a subsecond change alone does not create a new archive version');
  {$ENDIF}
  EnterNewDir('restored');
  GulpOk(['-r', '../arc.gulp']);
  CheckRestored('winter');
  CheckRestored('summer');
  CheckRestored('dir');
  {$IFDEF UNIX}
  CheckRestored('link');
  {$ENDIF}

  WriteLn('PASS: times are stored and restored in UTC');
end.
