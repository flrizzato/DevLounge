# Delphi SSE Analysis (RAD Studio 13.1)

## Goal
Create a practical understanding of **Server-Sent Events (SSE)** in Delphi (RAD Studio 13.1), then use that understanding to design a basic foundation project:

- A **WebBroker SSE server** endpoint
- A **basic HTML client** page consuming streamed events
- A set of **showcases** demonstrating what SSE can do

This document focuses on internal behavior seen in source units, architectural guidance, and open implementation choices. It now also includes **starter code templates** to accelerate implementation in a new context window.

---

## 1) What SSE Is and Why It Fits Here

SSE is a unidirectional, long-lived HTTP stream from server to client.

- Client opens an HTTP `GET` request (typically with `Accept: text/event-stream`)
- Server keeps connection open and pushes text frames over time
- Browser `EventSource` and Delphi `THTTPEventSource` parse frames incrementally
- Reconnect is built in (with optional event resume via `Last-Event-ID`)

Best use cases:

- Live metrics / dashboard updates
- Notifications
- Job progress updates
- Log tailing
- Server heartbeat/status channels

When to prefer WebSocket instead:

- If client must also push frequent low-latency data to server over same channel
- If bidirectional framing/protocol needs are complex

---

## 2) RAD Studio 13.1: New SSE Surface Area

### Server side (WebBroker)
In the internet sources, SSE server support is centered around:

- `TWebResponseStream`
- `TWebResponse.BeginStream`
- `TWebResponse.BeginEventsStream`

Observed unit:
- `source\internet\Web.HTTPApp.pas`

Supported WebBroker platforms in this release notes context:

- Indy
- Apache
- IIS/ISAPI
- FastCGI

Adapter-specific stream behavior is implemented in platform units such as:

- `Web.ApacheHTTP.pas`
- `Web.FastCGIHTTP.pas`
- `Web.Win.IsapiHTTP.pas`

### Client side (RTL)
SSE client support is concentrated in:

- `System.Net.HttpSse` with `THTTPEvent` and `THTTPEventSource`

Observed unit:
- `source\rtl\net\System.Net.HttpSse.pas`

---

## 3) Server Internals (How WebBroker Streams SSE)

Based on the source analysis:

1. Action dispatch reaches your `TWebActionItem.OnAction`.
2. Calling `BeginEventsStream(...)` initializes a streaming response:
   - content type set to `text/event-stream; charset=utf-8`
   - headers such as `Cache-Control: no-cache, no-store`
   - buffering hint `X-Accel-Buffering: no`
3. Response is started/sent early (stream mode).
4. Stream methods write SSE payload chunks progressively.
5. Flush/send paths route through `TWebResponse.SendStream(...)` and platform `WriteClient(...)`.
6. Request pipeline avoids a second final full response send if already marked sent.

Implication for endpoint design:

- Endpoint is not a normal “build full body then return” pattern.
- It is a long-running handler loop with periodic writes + flush.
- It must detect disconnect and stop cleanly.

---

## 4) Client Internals (How Delphi Parses SSE)

From `System.Net.HttpSse` behavior:

- Uses long-lived GET with headers for event streaming.
- Processes incoming bytes line-by-line.
- Supports standard fields:
  - `data`
  - `event`
  - `id`
  - `retry`
- Empty line dispatches event.
- Maintains last event id and can reconnect.
- Reconnect timing can be influenced by stream `retry:` or HTTP conditions.
- Event callbacks run on worker-thread context.

Important practical note:

- UI apps must marshal callback processing to main thread when needed.

---

## 5) SSE Message Model (For API Design)

Typical SSE event frame conceptually contains:

- optional `id: ...`
- optional `event: ...`
- one or more `data: ...` lines
- blank line terminator

Recommendations:

- Use `event` names as stable categories (`heartbeat`, `metric`, `notification`, `progress`, etc.).
- Keep data payload JSON for easy browser parsing.
- Include timestamp and monotonic sequence in payload.
- Emit heartbeat periodically to keep intermediaries from timing out idle streams.

---

## 6) Architecture for the Basic Foundation

### Minimal components

1. **WebBroker app**
   - One standard HTTP endpoint for health (`/health`)
   - One SSE endpoint (`/events`)
2. **In-memory publisher**
   - Periodic timer or worker generating demo events
   - Optional topic/type routing
3. **Static HTML page**
   - Uses browser `EventSource` to connect to `/events`
   - Displays raw stream + typed visual panels

### Suggested flow

1. Browser opens `EventSource('/events')`
2. WebBroker action starts events stream
3. Server emits:
   - `open` / intro event
   - periodic `heartbeat`
   - rotating demo events
4. Browser updates DOM by event type
5. On disconnect, browser auto-reconnects; server supports resume semantics where feasible

---

## 7) Showcase Ideas (Keep Scope Open)

Use these as modular “tracks” you can implement independently:

1. **Clock + heartbeat**
   - Push current server time each second
   - Push heartbeat every N seconds
2. **Random telemetry**
   - CPU-like percentage, queue length, latency
   - Plot simple sparkline or text bars in HTML
3. **Job progress simulator**
   - Start a fake background job; stream progress states
   - Emit final completed/failed event
4. **Named event channels**
   - Multiple `event:` names with different payload schema
   - HTML binds each to dedicated panel
5. **Reconnect + resume demo**
   - Include incrementing `id`
   - Force disconnect; verify client resumes from last id behavior
6. **Multi-client broadcast**
   - Open multiple browser tabs; all receive same stream
   - Demonstrate simple fan-out model
7. **Backpressure/safety demonstration**
   - Slow client simulation
   - Show bounded buffering and disconnect policy decisions

---

## 8) Implementation Options (Choose by Simplicity vs Robustness)

### Option A: Single endpoint loop (simplest)
- Emit periodic events directly in request handler loop.
- Pros: fastest to understand.
- Cons: ties request thread lifetime to producer logic.

### Option B: Shared publisher + per-client stream writers (recommended foundation)
- Separate event generation from endpoint writers.
- Endpoint subscribes, pushes events, unsubscribes on disconnect.
- Pros: cleaner architecture for growth.
- Cons: requires thread-safe queues/lists.

### Option C: Topic-based broker
- Add subscriptions per topic/event type.
- Pros: scalable conceptual model.
- Cons: heavier for first tutorial.

Suggested default for learning: **Option B**.

---

## 9) Risks and Design Guardrails

1. **Threading**
   - Stream endpoints can be long-lived; avoid blocking shared resources.
2. **Disconnect handling**
   - Always stop loops promptly when disconnected.
3. **Proxy buffering/timeouts**
   - Validate behavior behind IIS/Apache/reverse proxies.
4. **Memory growth**
   - Avoid unbounded per-client queues.
5. **Payload size/frequency**
   - Keep events small and cadence reasonable.
6. **Error visibility**
   - Add logs/metrics for connect, disconnect, sent count, dropped count.

---

## 10) AI-Ready Build Plan (for Next Step)

Use this plan as prompt structure when asking AI to generate the basic sample.

### Phase 1: Server skeleton
- Create WebBroker project with clear routing.
- Add `/health` and `/events`.
- Add a basic SSE writer abstraction around `TWebResponseStream`.
- Add periodic heartbeat and server-time events.

### Phase 2: HTML client
- Static page with:
  - connection status indicator
  - event counter
  - raw event log view
  - typed cards for at least 2 named events
- Auto-reconnect UX state messaging.

### Phase 3: Showcase modules
- Add optional toggles for:
  - telemetry simulation
  - job progress simulation
  - named events
  - reconnect/resume diagnostics

### Phase 4: Robustness pass
- Implement graceful disconnect detection.
- Add bounded buffering strategy.
- Add diagnostics endpoint/logging.
- Validate behavior on chosen platform target (Indy/Apache/IIS/FastCGI).

### Phase 5: Documentation and extension points
- Document event schema.
- Explain how to add new event types.
- Explain how to move from in-memory source to real business events.

---

## 11) Starter Code Templates (Implementation Acceleration)

These snippets are intentionally minimal and may need adjustment for your exact project structure, unit names, and hosting platform. Use them as a baseline, not as drop-in final production code.

### 11.1 WebBroker SSE action skeleton (Delphi)

```pascal
uses
  System.SysUtils, System.Classes, System.JSON, Web.HTTPApp;

procedure TWebModule1.WebModule1EventsAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  S: TWebResponseStream;
  Seq: UInt64;
  Payload: string;
begin
  Handled := True;
  Seq := 0;

  // Starts text/event-stream response and sends headers immediately.
  S := TWebResponseStream.BeginEventsStream(Response, 15);
  try
    while S.Connected do
    begin
      Inc(Seq);
      Payload := Format('{"seq":%d,"utc":"%s"}',
        [Seq, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now)]);

      S.WriteEvent('heartbeat');
      S.WriteID(Seq.ToString);
      S.WriteData(Payload);
      S.EndEvent;   // Terminates one SSE frame and flushes chunk.

      Sleep(1000);
    end;
  except
    on E: Exception do
    begin
      // Optional: log disconnect/write failures.
    end;
  end;
end;
```

### 11.2 Browser client (`EventSource`) skeleton

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <title>SSE Demo</title>
</head>
<body>
  <h1>SSE Demo</h1>
  <div id="status">connecting...</div>
  <pre id="log"></pre>

  <script>
    const log = (m) => {
      const el = document.getElementById("log");
      el.textContent += m + "\n";
    };

    const es = new EventSource("/events");

    es.onopen = () => {
      document.getElementById("status").textContent = "connected";
      log("[open]");
    };

    es.onerror = () => {
      document.getElementById("status").textContent = "reconnecting...";
      log("[error] connection dropped");
    };

    es.onmessage = (e) => {
      // Fallback handler for unnamed events.
      log(`[message] id=${e.lastEventId} data=${e.data}`);
    };

    es.addEventListener("heartbeat", (e) => {
      log(`[heartbeat] id=${e.lastEventId} data=${e.data}`);
    });
  </script>
</body>
</html>
```

### 11.3 Optional Delphi SSE client skeleton (`System.Net.HttpSse`)

```pascal
uses
  System.SysUtils, System.Net.HttpClient, System.Net.HttpSse;

var
  Client: THTTPClient;
  Source: THTTPEventSource;

procedure HandleSseMessage(Sender: TObject);
var
  Ev: THTTPEvent;
begin
  // Called on worker thread context.
  while Source.GetEvent(Ev) do
  begin
    Writeln(Format('event=%s id=%s data=%s', [
      Ev.Event, Ev.ID, Ev.Data.Text.Replace(sLineBreak, '\n')
    ]));
  end;
end;

begin
  Client := THTTPClient.Create;
  Source := THTTPEventSource.Create(Client, 'http://localhost:8080/events', HandleSseMessage);
  try
    Source.Open;
    Readln; // keep app running
    Source.Close;
  finally
    Source.Free;
    Client.Free;
  end;
end.
```

### 11.4 Lightweight in-memory broadcaster sketch (Option B)

```pascal
type
  TSseSubscriber = class
  public
    Stream: TWebResponseStream;
  end;

  TSseHub = class
  private
    FLock: TObject;
    FSubs: TList<TSseSubscriber>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Subscribe(ASub: TSseSubscriber);
    procedure Unsubscribe(ASub: TSseSubscriber);
    procedure Broadcast(const AEventName, AData: string);
  end;

procedure TSseHub.Broadcast(const AEventName, AData: string);
var
  I: Integer;
  Sub: TSseSubscriber;
begin
  TMonitor.Enter(FLock);
  try
    for I := FSubs.Count - 1 downto 0 do
    begin
      Sub := FSubs[I];
      if (Sub.Stream = nil) or not Sub.Stream.Connected then
      begin
        FSubs.Delete(I);
        Continue;
      end;
      Sub.Stream.WriteEvent(AEventName);
      Sub.Stream.WriteData(AData);
      Sub.Stream.EndEvent;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;
```

### 11.5 Suggested first implementation order

1. Implement `11.1` with one `heartbeat` event every second.
2. Connect `11.2` and verify continuous updates + reconnect behavior.
3. Introduce `id` and test resume behavior.
4. Refactor to `11.4` broadcaster to support multi-client fan-out.
5. Add telemetry/progress showcases.

---

## 12) Suggested Prompt Template for AI (When You Are Ready)

Use this as a starting point:

1. “Build a minimal Delphi WebBroker SSE foundation with `/events` and `/health`, using RAD Studio 13.1 streaming APIs (`TWebResponseStream` / `BeginEventsStream`).”
2. “Also create a simple HTML page using browser `EventSource` that renders connection status, event count, and typed events.”
3. “Include modular showcases (heartbeat, clock, telemetry, progress). Keep architecture extensible and thread-safe.”
4. “Do not over-engineer; keep code readable and explain each piece.”
5. “Add notes for IIS/Apache/FastCGI runtime differences and practical deployment caveats.”

---

## 13) What Is Confirmed vs Unknown

### Confirmed from source
- Core APIs and classes exist in RAD Studio 13.1 units listed above.
- Streaming/reconnect/parsing mechanics are implemented in those units.

### Still to validate empirically
- Exact runtime behavior under your selected server target and proxy stack.
- Preferred production threading/lifecycle pattern for your specific app load profile.

If needed, next step can be a focused “design-only” refinement selecting one architecture option (A/B/C) and defining exact event schema before any code generation.

