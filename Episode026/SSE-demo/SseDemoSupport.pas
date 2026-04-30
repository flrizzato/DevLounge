unit SseDemoSupport;

interface

uses
  System.Classes, System.SysUtils, System.Math, System.SyncObjs;

const
  /// Simulation advances on this wall-clock cadence for all clients (ms).
  /// Each connection's `dashMs` only controls how often it receives snapshots.
  CDashboardSimulationStepMs = 3000;

type
  /// Global random-walk KPIs shared by all /events connections (thread-safe).
  TSharedDashboard = class
  private
    FLock: TCriticalSection;
    FActiveUsers: Integer;
    FSalesTotal: Double;
    FAvgOrder: Double;
    /// Anchor tick for simulation steps; 0 = not yet anchored to ANowTick on first use.
    FLastSimTick: UInt64;
    FInitialized: Boolean;

    procedure Step;
    procedure SeedIfNeeded;
  public
    constructor Create;
    destructor Destroy; override;

    /// Advances shared model when due, then returns JSON for this stream's `seq`.
    function DashboardJson(const ASeqText: string; const ANowTick: UInt64): string;
  end;

  /// Global mock job: /startjob and all /events streams read/update through this.
  TSharedMockJob = class
  private
    FLock: TCriticalSection;
    FActive: Boolean;
    FPercent: Integer;
    FStartTick: UInt64;
    FDurationMs: Integer;

    class function StageForPercent(APct: Integer): string; static;
  public
    constructor Create(ADurationMs: Integer);
    destructor Destroy; override;

    procedure Start;

    /// Per-stream tick: updates shared elapsed percent, applies per-connection
    /// emit guard so every client receives the final 100% frame even if another
    /// stream cleared FActive first.
    procedure TickForConnection(var ALastStartTick: UInt64;
      var ALastPctSent: Integer; const ANowTick: UInt64; out APct: Integer;
      out AStage: string; out AShouldEmit: Boolean);
  end;

/// Singleton used by WebBroker actions (created in this unit's initialization).
function SharedMockJob: TSharedMockJob;

function SharedDashboard: TSharedDashboard;

/// Clamps query-style interval to a sensible demo range (ms).
function SseClampIntervalMs(const ARaw: string; ADefault: Integer): Integer;

implementation

uses
  System.StrUtils;

function SseClampIntervalMs(const ARaw: string; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(Trim(ARaw), ADefault);
  if Result < 250 then
    Result := 250
  else if Result > 60000 then
    Result := 60000;
end;

{ TSharedDashboard }

constructor TSharedDashboard.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FLastSimTick := 0;
  FInitialized := False;
end;

destructor TSharedDashboard.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TSharedDashboard.SeedIfNeeded;
begin
  if FInitialized then
    Exit;
  FActiveUsers := 150 + Random(100);
  FSalesTotal := 10000 + Random(40000);
  FAvgOrder := 50 + Random(50);
  FInitialized := True;
end;

procedure TSharedDashboard.Step;
begin
  FActiveUsers := Max(10, FActiveUsers + Random(21) - 10);
  FSalesTotal := FSalesTotal + Random(500) + 10;
  FAvgOrder := Max(10, FAvgOrder + (Random(11) - 5));
end;

function TSharedDashboard.DashboardJson(const ASeqText: string;
  const ANowTick: UInt64): string;
begin
  FLock.Enter;
  try
    SeedIfNeeded;
    if FLastSimTick = 0 then
      FLastSimTick := ANowTick;

    while ANowTick - FLastSimTick >= CDashboardSimulationStepMs do
    begin
      Step;
      Inc(FLastSimTick, CDashboardSimulationStepMs);
    end;

    Result := Format('{"seq":%s,"activeUsers":%d,"salesTotal":%.2f,"avgOrder":%.2f}',
      [ASeqText, FActiveUsers, FSalesTotal, FAvgOrder]);
  finally
    FLock.Leave;
  end;
end;

{ TSharedMockJob }

constructor TSharedMockJob.Create(ADurationMs: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FActive := False;
  FPercent := 0;
  FStartTick := 0;
  FDurationMs := ADurationMs;
end;

destructor TSharedMockJob.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

class function TSharedMockJob.StageForPercent(APct: Integer): string;
begin
  if APct < 25 then
    Result := 'Initializing...'
  else if APct < 50 then
    Result := 'Processing data...'
  else if APct < 75 then
    Result := 'Analyzing results...'
  else if APct < 100 then
    Result := 'Finalizing...'
  else
    Result := 'Complete';
end;

procedure TSharedMockJob.Start;
begin
  FLock.Enter;
  try
    FActive := True;
    FPercent := 0;
    FStartTick := TThread.GetTickCount64;
  finally
    FLock.Leave;
  end;
end;

procedure TSharedMockJob.TickForConnection(var ALastStartTick: UInt64;
  var ALastPctSent: Integer; const ANowTick: UInt64; out APct: Integer;
  out AStage: string; out AShouldEmit: Boolean);
var
  IsActive: Boolean;
  Elapsed: UInt64;
begin
  AShouldEmit := False;
  AStage := '';
  FLock.Enter;
  try
    if FStartTick <> ALastStartTick then
    begin
      ALastStartTick := FStartTick;
      ALastPctSent := -1;
    end;

    if FActive then
    begin
      Elapsed := ANowTick - FStartTick;
      APct := Min(100, Trunc((Elapsed / FDurationMs) * 100));
      FPercent := APct;
      if APct >= 100 then
        FActive := False;
    end
    else
      APct := FPercent;

    IsActive := FActive;
  finally
    FLock.Leave;
  end;

  if (IsActive or (APct > 0)) and (APct > ALastPctSent) then
  begin
    AStage := StageForPercent(APct);
    AShouldEmit := True;
    ALastPctSent := APct;
  end;
end;

var
  GSharedMockJob: TSharedMockJob;
  GSharedDashboard: TSharedDashboard;

function SharedMockJob: TSharedMockJob;
begin
  Result := GSharedMockJob;
end;

function SharedDashboard: TSharedDashboard;
begin
  Result := GSharedDashboard;
end;

initialization
  Randomize;
  GSharedMockJob := TSharedMockJob.Create(8000);
  GSharedDashboard := TSharedDashboard.Create;

finalization
  GSharedDashboard.Free;
  GSharedMockJob.Free;

end.
