unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils,
  System.DateUtils, System.IOUtils,
  Web.HTTPApp, Web.Stencils,
  SseDemoSupport;

type
  TWebModule1 = class(TWebModule)
    WebStencilsProcessor1: TWebStencilsProcessor;
    procedure WebModule1HealthAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1EventsAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1StartJobAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    function IsoUtcNow: string;
    function ReadIntervalMs(ARequest: TWebRequest; const AName: string;
      ADefault: Integer): Integer;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

function TWebModule1.IsoUtcNow: string;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"Z"',
    TTimeZone.Local.ToUniversalTime(Now));
end;

function TWebModule1.ReadIntervalMs(ARequest: TWebRequest; const AName: string;
  ADefault: Integer): Integer;
begin
  Result := SseClampIntervalMs(ARequest.QueryFields.Values[AName], ADefault);
end;

procedure TWebModule1.WebModule1HealthAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Handled := True;
  Response.ContentType := 'application/json; charset=utf-8';
  Response.Content := Format('{"status":"ok","utc":"%s"}', [IsoUtcNow]);
end;

procedure TWebModule1.WebModule1StartJobAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Handled := True;
  Response.ContentType := 'application/json; charset=utf-8';
  SharedMockJob.Start;
  Response.Content := '{"status":"started"}';
end;

procedure TWebModule1.WebModule1EventsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Stream: TWebResponseStream;
  Seq: UInt64;
  HeartbeatMs: Integer;
  DashMs: Integer;
  LastHeartbeatTick: UInt64;
  LastDashTick: UInt64;
  NowTick: UInt64;
  LastEventID: string;
  LastProgressStartTick: UInt64;
  LastProgressPctSent: Integer;
  JobPct: Integer;
  JobStage: string;
  EmitProgress: Boolean;
begin
  Handled := True;
  HeartbeatMs := ReadIntervalMs(Request, 'hbMs', 1000);
  DashMs := ReadIntervalMs(Request, 'dashMs', 3000);
  LastEventID := Trim(Request.GetFieldByName('Last-Event-ID'));
  Seq := StrToUInt64Def(LastEventID, 0);
  LastHeartbeatTick := 0;
  LastDashTick := 0;
  LastProgressStartTick := 0;
  LastProgressPctSent := -1;

  Stream := TWebResponseStream.BeginEventsStream(Response, 15);
  try
    while Stream.Connected do
    begin
      NowTick := TThread.GetTickCount64;

      if (LastHeartbeatTick = 0) or ((NowTick - LastHeartbeatTick) >= UInt64(HeartbeatMs)) then
      begin
        Inc(Seq);
        Stream.WriteEvent('heartbeat');
        Stream.WriteID(Seq.ToString);
        Stream.WriteData(Format('{"seq":%s,"utc":"%s"}',
          [Seq.ToString, IsoUtcNow]));
        Stream.EndEvent;
        LastHeartbeatTick := NowTick;
      end;

      if (LastDashTick = 0) or ((NowTick - LastDashTick) >= UInt64(DashMs)) then
      begin
        Inc(Seq);
        Stream.WriteEvent('dashboard');
        Stream.WriteID(Seq.ToString);
        Stream.WriteData(SharedDashboard.DashboardJson(Seq.ToString, NowTick));
        Stream.EndEvent;
        LastDashTick := NowTick;
      end;

      SharedMockJob.TickForConnection(LastProgressStartTick, LastProgressPctSent,
        NowTick, JobPct, JobStage, EmitProgress);
      if EmitProgress then
      begin
        Inc(Seq);
        Stream.WriteEvent('progress');
        Stream.WriteID(Seq.ToString);
        Stream.WriteData(Format('{"seq":%s,"percent":%d,"stage":"%s"}',
          [Seq.ToString, JobPct, JobStage]));
        Stream.EndEvent;
      end;

      Sleep(100);
    end;
  except
    on E: Exception do
      ;
  end;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Handled := True;
  Response.ContentType := 'text/html; charset=utf-8';
  WebStencilsProcessor1.WebRequest := Request;
  WebStencilsProcessor1.InputFileName :=
    TPath.Combine(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\html'), 'index.html');
  Response.Content := WebStencilsProcessor1.Content;
end;

end.
