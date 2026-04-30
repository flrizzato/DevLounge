# Delphi SSE Demo (Server-Sent Events)

This repository contains a technical showcase demonstrating how to implement and consume **Server-Sent Events (SSE)** in Delphi. It highlights how a single WebBroker server can simultaneously push real-time data to both Web browsers and native applications via SSE.

The demo consists of two main projects:
1. **SSEServer**: A console WebBroker server that hosts the web application and broadcasts SSE events.
2. **FMXSSEClient**: A native FireMonkey desktop application that consumes the same SSE stream.

## Features Demonstrated

- **Real-time Server-Sent Events (SSE)** using Delphi's `Web.WebBroker` and `TWebResponseStream`.
- **Background Job Progress**: Simulates a long-running process on the server pushing percent progress to the clients.
- **Throttling & Cadence**: Setting intervals via URL parameters (`hbMs` and `dashMs`) so clients control their update frequency.
- **Cross-Platform Clients**: Shows the same SSE technology integrated into a Web page via `EventSource` and natively into Delphi using `THTTPEventSource`.

---

## How to Run

### 1. Start the Server

1. Open `SSEDemo.groupproj` in Delphi.
2. Compile and Run the **SSEServer** project.
3. The server runs as a console application. By default, it will start HTTP services on port `8080`.
4. In the server console window, you can type `start` to ensure it's running (it starts automatically) or `help` to see commands.

### 2. Run the Clients

You have two ways to see the demo working (and you can run multiple at the same time!):

**Option A: The Web Version**
1. With the server running, open a web browser.
2. Navigate to `http://localhost:8080`.
3. You will see a web dashboard demonstrating the live Dashboard KPIs, background job controls, and an event log.

**Option B: The FireMonkey (FMX) Client**
1. In Delphi, compile and run the **FMXSSEClient** project.
2. Once the app opens, click **Connect**.
3. It will connect to the same `localhost:8080` server and display the identical live metrics natively.

### Pushing Data
* Try opening a web client and the FMX client side-by-side.
* Click the "Start Job" button on either client. You will see both clients update their progress bars in real-time as the server broadcasts the job's progress.
