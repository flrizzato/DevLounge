unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Net.HttpClient, System.Net.HttpSse, System.SyncObjs,
  System.Threading, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Memo, FMX.ScrollBox,
  FMX.Memo.Types, FMX.Layouts;

type
  TForm1 = class(TForm)
    grpConnection: TGroupBox;
    lblBaseUrl: TLabel;
    edtBaseUrl: TEdit;
    lblHbMs: TLabel;
    edtHbMs: TEdit;
    lblDashMs: TLabel;
    edtDashMs: TEdit;
    btnConnect: TButton;
    btnReconnect: TButton;
    btnDisconnect: TButton;
    lblStatusCaption: TLabel;
    lblStatus: TLabel;
    lblTitle: TLabel;
    grpDashboard: TGroupBox;
    gridDashboard: TGridPanelLayout;
    rectKpiUsers: TRectangle;
    lblKpiUsersBig: TLabel;
    lblKpiUsersCap: TLabel;
    rectKpiSales: TRectangle;
    lblKpiSalesCap: TLabel;
    lblKpiSalesBig: TLabel;
    rectKpiAvg: TRectangle;
    lblKpiAvgCap: TLabel;
    lblKpiAvgBig: TLabel;
    grpJob: TGroupBox;
    btnStartJob: TButton;
    ProgressBar1: TProgressBar;
    lblJobStage: TLabel;
    grpEvents: TGroupBox;
    lblLastHeartbeatCaption: TLabel;
    lblLastHeartbeat: TLabel;
    lblLastDashCaption: TLabel;
    lblLastDash: TLabel;
    gridEvents: TGridPanelLayout;
    lytEventsTotal: TLayout;
    lblTotal: TLabel;
    lblTotalCaption: TLabel;
    lytEventsHeartbeat: TLayout;
    lytEventsDashboard: TLayout;
    lblHeartbeatCaption: TLabel;
    lblDashCaption: TLabel;
    lblHeartbeat: TLabel;
    lblDash: TLabel;
    grpLogs: TGroupBox;
    memLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnReconnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnStartJobClick(Sender: TObject);
  private
    FClient: THTTPClient;
    FSource: THTTPEventSource;
    FStatusLock: TCriticalSection;
    FTotalCount: Integer;
    FHeartbeatCount: Integer;
    FDashCount: Integer;
    FStreamURL: string;
    FQueuedStatus: string;
    FQueuedLog: string;
    FQueuedConnected: Boolean;
    FUiTotalCount: Integer;
    FUiHeartbeatCount: Integer;
    FUiDashCount: Integer;
    FUiLastHeartbeat: string;
    FUiLastDash: string;
    FUiKpiUsers: string;
    FUiKpiSales: string;
    FUiKpiAvg: string;
    FUiJobPercent: Integer;
    FUiJobStage: string;
    FIsShuttingDown: Boolean;
    procedure UiApplyStatus;
    procedure UiAppendLog;
    procedure UiApplyButtons;
    procedure UiResetCounters;
    procedure UiApplyHeartbeat;
    procedure UiApplyDash;
    procedure UiApplyProgress;
    procedure ParseDashboardKpis(const AJson: string);
    procedure UpdateStatus(const AValue: string);
    procedure AppendLog(const AValue: string);
    procedure ResetCounters;
    function NormalizeMs(const AValue: string; ADefault: Integer): Integer;
    function BuildStreamURL: string;
    function BaseUrl: string;
    procedure OpenSource;
    procedure CloseSource;
    procedure HandleSseMessages(ASender: THTTPEventSource);
    procedure HandleSseOpen(ASender: THTTPEventSource);
    procedure HandleSseReconnect(ASender: THTTPEventSource);
    procedure HandleSseClose(ASender: THTTPEventSource);
    procedure HandleSseError(ASender: THTTPEventSource; const AException: Exception;
      var AReconnect: Boolean);
    procedure SetButtonsForState(AConnected: Boolean);
    function StatusColorFor(const AStatus: string): TAlphaColor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

function TForm1.StatusColorFor(const AStatus: string): TAlphaColor;
var
  S: string;
begin
  S := LowerCase(Trim(AStatus));

  if Pos('reconnecting', S) > 0 then
    Exit(TAlphaColorRec.Orange)
  else if Pos('disconnected', S) > 0 then
    Exit(TAlphaColorRec.Red)
  else if Pos('connected', S) > 0 then
    Exit(TAlphaColorRec.Green)
  else if Pos('error', S) > 0 then
    Exit(TAlphaColorRec.Red);

  Result := TAlphaColorRec.Black;
end;

procedure TForm1.UiApplyStatus;
begin
  lblStatus.Text := FQueuedStatus;
  lblStatus.StyledSettings := lblStatus.StyledSettings - [TStyledSetting.FontColor];
  lblStatus.TextSettings.FontColor := StatusColorFor(FQueuedStatus);
end;

procedure TForm1.UiAppendLog;
begin
  memLog.Lines.Add(FQueuedLog);
  memLog.GoToTextEnd;
end;

procedure TForm1.UiApplyButtons;
begin
  btnConnect.Enabled := not FQueuedConnected;
  btnReconnect.Enabled := FQueuedConnected;
  btnDisconnect.Enabled := FQueuedConnected;
  btnStartJob.Enabled := FQueuedConnected;
end;

procedure TForm1.UiResetCounters;
begin
  lblTotal.Text := '0';
  lblHeartbeat.Text := '0';
  lblDash.Text := '0';
  lblLastHeartbeat.Text := '-';
  lblLastDash.Text := '-';
  lblKpiUsersBig.Text := '-';
  lblKpiSalesBig.Text := '-';
  lblKpiAvgBig.Text := '-';
  ProgressBar1.Value := 0;
  lblJobStage.Text := 'Idle';
  memLog.Lines.Clear;
end;

procedure TForm1.UiApplyHeartbeat;
begin
  lblHeartbeat.Text := FUiHeartbeatCount.ToString;
  lblTotal.Text := FUiTotalCount.ToString;
  lblLastHeartbeat.Text := FUiLastHeartbeat;
end;

procedure TForm1.UiApplyDash;
begin
  lblDash.Text := FUiDashCount.ToString;
  lblTotal.Text := FUiTotalCount.ToString;
  lblLastDash.Text := FUiLastDash;
  lblKpiUsersBig.Text := FUiKpiUsers;
  lblKpiSalesBig.Text := FUiKpiSales;
  lblKpiAvgBig.Text := FUiKpiAvg;
end;

procedure TForm1.UiApplyProgress;
begin
  ProgressBar1.Value := FUiJobPercent;
  lblJobStage.Text := FUiJobStage;
end;

procedure TForm1.ParseDashboardKpis(const AJson: string);
var
  JObj: TJSONObject;
  V: TJSONValue;
  Sales: Double;
  Avg: Double;
  FS: TFormatSettings;
begin
  FUiKpiUsers := '-';
  FUiKpiSales := '-';
  FUiKpiAvg := '-';

  JObj := TJSONObject.ParseJSONValue(AJson) as TJSONObject;
  if JObj = nil then
    Exit;
  try
    V := JObj.Values['activeUsers'];
    if Assigned(V) and (V is TJSONNumber) then
      FUiKpiUsers := TJSONNumber(V).AsInt.ToString;

    V := JObj.Values['salesTotal'];
    if Assigned(V) and (V is TJSONNumber) then
    begin
      Sales := TJSONNumber(V).AsDouble;
      FS := TFormatSettings.Invariant;
      FS.ThousandSeparator := ',';
      FS.DecimalSeparator := '.';
      FUiKpiSales := Format('$%s', [FormatFloat('#,##0.00', Sales, FS)]);
    end;

    V := JObj.Values['avgOrder'];
    if Assigned(V) and (V is TJSONNumber) then
    begin
      Avg := TJSONNumber(V).AsDouble;
      FUiKpiAvg := Format('$%.2f', [Avg]);
    end;
  finally
    JObj.Free;
  end;
end;

procedure TForm1.AppendLog(const AValue: string);
begin
  if FIsShuttingDown then
    Exit;
  FQueuedLog := AValue;
  TThread.Queue(nil, UiAppendLog);
end;

function TForm1.NormalizeMs(const AValue: string; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(Trim(AValue), ADefault);
  if Result < 250 then
    Result := 250
  else if Result > 60000 then
    Result := 60000;
end;

function TForm1.BaseUrl: string;
begin
  Result := Trim(edtBaseUrl.Text);
  if Result.EndsWith('/') then
    Result := Result.Substring(0, Result.Length - 1);
end;

function TForm1.BuildStreamURL: string;
var
  HbMs: Integer;
  DashMs: Integer;
begin
  HbMs := NormalizeMs(edtHbMs.Text, 1000);
  DashMs := NormalizeMs(edtDashMs.Text, 3000);
  edtHbMs.Text := HbMs.ToString;
  edtDashMs.Text := DashMs.ToString;

  Result := Format('%s/events?hbMs=%d&dashMs=%d', [BaseUrl, HbMs, DashMs]);
end;

procedure TForm1.UpdateStatus(const AValue: string);
begin
  if FIsShuttingDown then
    Exit;
  FQueuedStatus := AValue;
  TThread.Queue(nil, UiApplyStatus);
end;

procedure TForm1.SetButtonsForState(AConnected: Boolean);
begin
  if FIsShuttingDown then
    Exit;
  FQueuedConnected := AConnected;
  TThread.Queue(nil, UiApplyButtons);
end;

procedure TForm1.ResetCounters;
begin
  if FIsShuttingDown then
    Exit;

  FStatusLock.Enter;
  try
    FTotalCount := 0;
    FHeartbeatCount := 0;
    FDashCount := 0;
  finally
    FStatusLock.Leave;
  end;

  TThread.Queue(nil, UiResetCounters);
end;

procedure TForm1.HandleSseMessages(ASender: THTTPEventSource);
var
  Ev: THTTPEvent;
  EventName: string;
  EventData: string;
  EventID: string;
  JObj: TJSONObject;
begin
  if FIsShuttingDown then
    Exit;

  if not Assigned(ASender) then
    Exit;

  Ev := ASender.GetEvent;
  while Assigned(Ev) do
  begin
    try
      EventName := Ev.Event;
      EventData := StringReplace(Ev.Data.Text, sLineBreak, '\n', [rfReplaceAll]);
      EventID := Ev.ID;

      if SameText(EventName, 'heartbeat') then
      begin
        FStatusLock.Enter;
        try
          Inc(FHeartbeatCount);
          Inc(FTotalCount);
          FUiHeartbeatCount := FHeartbeatCount;
          FUiTotalCount := FTotalCount;
          FUiLastHeartbeat := EventData;
        finally
          FStatusLock.Leave;
        end;

        TThread.Queue(nil, UiApplyHeartbeat);
      end
      else if SameText(EventName, 'dashboard') then
      begin
        ParseDashboardKpis(EventData);
        FStatusLock.Enter;
        try
          Inc(FDashCount);
          Inc(FTotalCount);
          FUiDashCount := FDashCount;
          FUiTotalCount := FTotalCount;
          FUiLastDash := EventData;
        finally
          FStatusLock.Leave;
        end;

        TThread.Queue(nil, UiApplyDash);
      end
      else if SameText(EventName, 'progress') then
      begin
        FStatusLock.Enter;
        try
          Inc(FTotalCount);
          FUiTotalCount := FTotalCount;
        finally
          FStatusLock.Leave;
        end;

        // Parse percent and stage from JSON payload
        try
          JObj := TJSONObject.ParseJSONValue(EventData) as TJSONObject;
          try
            FUiJobPercent := JObj.GetValue<Integer>('percent', 0);
            FUiJobStage := JObj.GetValue<string>('stage', '');
          finally
            JObj.Free;
          end;
        except
          FUiJobPercent := 0;
          FUiJobStage := EventData;
        end;

        TThread.Queue(nil, UiApplyProgress);
      end;

      if EventName.IsEmpty then
        EventName := 'message';
      AppendLog(Format('[%s] id=%s data=%s', [EventName, EventID, EventData]));
    finally
      Ev.Free;
    end;

    Ev := ASender.GetEvent;
  end;
end;

procedure TForm1.HandleSseOpen(ASender: THTTPEventSource);
begin
  UpdateStatus('connected');
  SetButtonsForState(True);
  AppendLog('[state] connected');
end;

procedure TForm1.HandleSseReconnect(ASender: THTTPEventSource);
begin
  UpdateStatus('reconnecting...');
  AppendLog('[state] reconnecting...');
end;

procedure TForm1.HandleSseClose(ASender: THTTPEventSource);
begin
  UpdateStatus('disconnected');
  SetButtonsForState(False);
  AppendLog('[state] disconnected');
end;

procedure TForm1.HandleSseError(ASender: THTTPEventSource;
  const AException: Exception; var AReconnect: Boolean);
begin
  UpdateStatus('connection error');
  AppendLog('[error] ' + AException.Message);
  AReconnect := True;
end;

procedure TForm1.OpenSource;
begin
  CloseSource;

  FStreamURL := BuildStreamURL;
  UpdateStatus('connecting...');
  AppendLog('[open] ' + FStreamURL);

  FSource := THTTPEventSource.Create;
  FSource.Client := FClient;
  FSource.URL := FStreamURL;
  FSource.OnMessage := HandleSseMessages;
  FSource.OnOpen := HandleSseOpen;
  FSource.OnReconnect := HandleSseReconnect;
  FSource.OnClose := HandleSseClose;
  FSource.OnError := HandleSseError;
  FSource.Open;
end;

procedure TForm1.CloseSource;
begin
  if Assigned(FSource) then
  begin
    FSource.OnMessage := nil;
    FSource.OnOpen := nil;
    FSource.OnReconnect := nil;
    FSource.OnClose := nil;
    FSource.OnError := nil;
    try
      FSource.Close;
    except
      on E: Exception do
        AppendLog('[close-error] ' + E.Message);
    end;
    FreeAndNil(FSource);
  end;

  UpdateStatus('disconnected');
  SetButtonsForState(False);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIsShuttingDown := False;
  FClient := THTTPClient.Create;
  FStatusLock := TCriticalSection.Create;

  edtBaseUrl.Text := 'http://localhost:8080';
  edtHbMs.Text := '1000';
  edtDashMs.Text := '3000';
  FQueuedStatus := 'disconnected';
  UiApplyStatus;
  SetButtonsForState(False);
  ResetCounters;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FIsShuttingDown := True;
  CloseSource;
  FreeAndNil(FStatusLock);
  FreeAndNil(FClient);
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  try
    OpenSource;
  except
    on E: Exception do
    begin
      UpdateStatus('error');
      AppendLog('[connect-error] ' + E.Message);
      SetButtonsForState(False);
    end;
  end;
end;

procedure TForm1.btnReconnectClick(Sender: TObject);
begin
  try
    OpenSource;
  except
    on E: Exception do
    begin
      UpdateStatus('error');
      AppendLog('[reconnect-error] ' + E.Message);
      SetButtonsForState(False);
    end;
  end;
end;

procedure TForm1.btnDisconnectClick(Sender: TObject);
begin
  CloseSource;
end;

procedure TForm1.btnStartJobClick(Sender: TObject);
var
  Url: string;
begin
  Url := BaseUrl + '/startjob';
  AppendLog('[job] POST ' + Url);

  TTask.Run(
    procedure
    var
      Resp: IHTTPResponse;
    begin
      try
        Resp := FClient.Get(Url);
        AppendLog('[job] ' + Resp.ContentAsString);
      except
        on E: Exception do
          AppendLog('[job-error] ' + E.Message);
      end;
    end
  );
end;

end.
