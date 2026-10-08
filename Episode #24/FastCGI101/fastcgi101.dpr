program fastcgi101;
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Types,
  IPPeerServer,
  IPPeerAPI,
  Web.FastCGIApp,
  Web.WebReq,
  Web.WebBroker,
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  ServerConst1 in 'ServerConst1.pas';

{$R *.res}

function BindPort(APort: Integer): Boolean;
var
  LTestServer: IIPTestServer;
begin
  Result := True;
  try
    LTestServer := PeerFactory.CreatePeer('', IIPTestServer) as IIPTestServer;
    LTestServer.TestOpenPort(APort, nil);
  except
    Result := False;
  end;
end;

function CheckPort(APort: Integer): Integer;
begin
  if BindPort(APort) then
    Result := APort
  else
    Result := 0;
end;

procedure SetPort(const AServer: TFastCGIApplication; APort: String);
begin
  if not AServer.Active then
  begin
    APort := APort.Replace(cCommandSetPort, '').Trim;
    if CheckPort(APort.ToInteger) > 0 then
    begin
      AServer.DefaultPort := APort.ToInteger;
      Writeln(Format(sPortSet, [APort]));
    end
    else
      Writeln(Format(sPortInUse, [APort]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure SetLog(const AServer: TFastCGIApplication; ALog: String);
begin
  ALog := ALog.Replace(cCommandSetLog, '').Trim;
  if sametext(ALog, CLogDebug) then
    AServer.MinSeverity := TLogSeverity.lsDebug
  else if sametext(ALog, CLogInfo) then
    AServer.MinSeverity := TLogSeverity.lsInfo
  else if sametext(ALog, CLogWarning) then
    AServer.MinSeverity := TLogSeverity.lsWarning
  else if sametext(ALog, CLogError) then
    AServer.MinSeverity := TLogSeverity.lsError
  else
    AServer.MinSeverity := TLogSeverity.lsInfo;
  Write(cArrow);
end;

procedure StartServer(const AServer: TFastCGIApplication);
begin
  if not AServer.Active then
  begin
    if CheckPort(AServer.DefaultPort) > 0 then
    begin
      Writeln(Format(sStartingServer, [AServer.DefaultPort]));
      AServer.Active := True;
    end
    else
      Writeln(Format(sPortInUse, [AServer.DefaultPort.ToString]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer(const AServer: TFastCGIApplication);
begin
  if AServer.Active then
  begin
    Writeln(sStoppingServer);
    AServer.Active := False;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure WriteStatus(const AServer: TFastCGIApplication);
const
  CLogSeverity: array [TLogSeverity] of string =
    (cLogDebug, cLogInfo, cLogWarning, cLogError);
begin
  Writeln(sRTLVersion + (GetRTLVersion shr 8).ToString + '.' + (GetRTLVersion and $FF).ToString);
  Writeln(sActive + AServer.Active.ToString(TUseBoolStrs.True));
  Writeln(sPort + AServer.DefaultPort.ToString);
  Writeln(sLogging + CLogSeverity[AServer.MinSeverity]);
  Write(cArrow);
end;

procedure RunServer(APort: Integer);
var
  LServer: TFastCGIApplication;
  LResponse: string;
begin
  WriteCommands;
  LServer := TFastCGIApplication(Application);
  LServer.DefaultPort := APort;
  while True do
  begin
    Readln(LResponse);
    LResponse := LowerCase(LResponse);
    if LResponse.StartsWith(cCommandSetPort) then
      SetPort(LServer, LResponse)
    else if LResponse.StartsWith(cCommandSetLog) then
      SetLog(LServer, LResponse)
    else if sametext(LResponse, cCommandStart) then
      StartServer(LServer)
    else if sametext(LResponse, cCommandStatus) then
      WriteStatus(LServer)
    else if sametext(LResponse, cCommandStop) then
      StopServer(LServer)
    else if sametext(LResponse, cCommandHelp) then
      WriteCommands
    else if sametext(LResponse, cCommandExit) then
      if LServer.Active then
      begin
        StopServer(LServer);
        break
      end
      else
        break
    else
    begin
      Writeln(sInvalidCommand);
      Write(cArrow);
    end;
  end;
end;

begin
  try
    Application.WebModuleClass := WebModuleClass;
    Application.Initialize;
    RunServer(9000);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
