program Linux101;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

begin
  try
    WriteLn('Hello from Linux Mint!');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
