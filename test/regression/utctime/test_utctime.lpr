program TestUtcTime;
{$mode objfpc}{$H+}

uses
  BaseUnix, SysUtils, DateUtils, RegressionUtils, GulpCommon;

function UnixTime(const AName: string): int64;
var
  info: Stat;
begin
  Check(FpLStat(AName, info) = 0, 'lstat ' + AName);
  result := info.st_mtime;
end;

procedure CheckName(const AName: string; ATime: int64);
begin
  Check(SetTimeUtc(AName, UnixToDateTime(ATime)) = 0, 'settimeutc ' + AName);
  Check(UnixTime(AName) = ATime, 'settimeutc sets the UTC time of ' + AName);
  Check(DateTimeToUnix(GetTimeUtc(AName)) = ATime, 'gettimeutc returns the UTC time of ' + AName);
end;

const
  Winter = 1579089600; // 2020-01-15 12:00:00 UTC
  Summer = 1593604800; // 2020-07-01 12:00:00 UTC
  LinkSummer = 1564653600; // 2019-08-01 10:00:00 UTC

var
  name: string;
begin
  EnterNewDir('work');
  WriteTextFile('winter', 'w');
  WriteTextFile('summer', 's');
  ForceDirectories('dir');
  Check(FpSymlink('summer', 'link') = 0, 'create link');
  CheckName('winter', Winter);
  CheckName('summer', Summer);
  CheckName('dir', Summer);
  CheckName('link', LinkSummer);

  GulpOk(['-s', '../arc.gulp']);
  EnterNewDir('restored');
  GulpOk(['-r', '../arc.gulp']);
  for name in ['winter', 'summer', 'dir', 'link'] do
    Check(UnixTime(name) = UnixTime('../work/' + name), 'restored time of ' + name);

  WriteLn('PASS: times are stored and restored in UTC');
end.
