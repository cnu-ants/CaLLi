import React, { useEffect, useMemo, useRef, useState } from "react";
import ReactFlow, { Background, Controls, Position } from "reactflow";
import type { Edge, Node, ReactFlowInstance, Viewport } from "reactflow";
import "reactflow/dist/style.css";
import dagre from "dagre";

type NodeJSON = { id: string; label?: string; instrs?: string[] };
type EdgeJSON = { id: string; source: string; target: string; kind?: "call" | "fallback" | "ret" | "intra" };
type GraphJSON = { nodes: NodeJSON[]; edges: EdgeJSON[] };

type WLMsg =
  | { type: "worklist"; bb: string; ctxt: string; current: string; worklist: string[] }
  | { type: "done" }
  | { type: "error"; msg: string };

type StateResp = {
  bb: string;
  contexts: { ctxt: string; is_bot: boolean; entries: { addr: string; value: string }[] }[];
};

type EnvResp = { items: { var: string; addr: string }[] };

const NODE_W = 420;

// UI scale (panels)
const UI_SCALE = 1.5;

// Graph scale (node content font)
const NODE_SCALE = 1.25;

const NODE_TITLE_FS = Math.round(15 * NODE_SCALE);
const NODE_BODY_FS = Math.round(13 * NODE_SCALE);
const NODE_LINE_H = Math.round(16 * NODE_SCALE);

// Bigger ctxt buttons
const CTX_BTN_FS = Math.round(12 * NODE_SCALE * 1.15);
const CTX_BTN_PAD_Y = Math.round(4 * NODE_SCALE * 1.2);
const CTX_BTN_PAD_X = Math.round(10 * NODE_SCALE * 1.2);

function edgeStyle(kind: "call" | "fallback" | "ret" | "intra") {
  if (kind === "call") return { strokeWidth: 2.5, stroke: "#2563eb" };
  if (kind === "fallback") return { strokeWidth: 2.5, stroke: "#6b7280", strokeDasharray: "6 6" };
  if (kind === "ret") return { strokeWidth: 2.5, stroke: "#16a34a" };
  return { strokeWidth: 2.5, stroke: "#111827" };
}

function computeNodeHeight(instrsLen: number) {
  const header = 44 * NODE_SCALE;
  const ctxArea = 52 * NODE_SCALE; // slightly taller for bigger buttons
  const padding = 28 * NODE_SCALE;
  const termExtra = 16 * NODE_SCALE;
  const lines = Math.max(1, instrsLen);
  return Math.min(1400, Math.round(header + ctxArea + padding + lines * NODE_LINE_H + termExtra));
}

function layout(nodes: Node[], edges: Edge[]) {
  const g = new dagre.graphlib.Graph();
  g.setDefaultEdgeLabel(() => ({}));
  g.setGraph({ rankdir: "TB", nodesep: 50, ranksep: 150 });

  for (const n of nodes) {
    const w = (n.width as number) || NODE_W;
    const h = (n.height as number) || 200;
    g.setNode(n.id, { width: w, height: h });
  }
  for (const e of edges) g.setEdge(e.source, e.target);

  dagre.layout(g);

  const laidOut = nodes.map((n) => {
    const p = g.node(n.id);
    const w = (n.width as number) || NODE_W;
    const h = (n.height as number) || 200;
    return {
      ...n,
      position: { x: p.x - w / 2, y: p.y - h / 2 },
      targetPosition: Position.Top,
      sourcePosition: Position.Bottom,
    };
  });

  return { nodes: laidOut, edges };
}

export default function ICFGViewer() {
  const [graph, setGraph] = useState<GraphJSON | null>(null);
  const [err, setErr] = useState<string | null>(null);

  const [rfInstance, setRfInstance] = useState<ReactFlowInstance | null>(null);

  const wsRef = useRef<WebSocket | null>(null);
  const [wsStatus, setWsStatus] = useState<"disconnected" | "connecting" | "connected">("connecting");

  const [wl, setWl] = useState<string[]>([]);
  const [currentBb, setCurrentBb] = useState<string>("");
  const [currentCtxt, setCurrentCtxt] = useState<string>("");

  const [ctxMap, setCtxMap] = useState<Record<string, StateResp["contexts"]>>({});

  const [selBb, setSelBb] = useState<string>(""); // selected node (blue)
  const [selCtxt, setSelCtxt] = useState<string>("");
  const [selEntries, setSelEntries] = useState<{ addr: string; value: string }[]>([]);
  const [selIsBot, setSelIsBot] = useState<boolean>(false);

  const [envItems, setEnvItems] = useState<{ var: string; addr: string }[]>([]);

  useEffect(() => {
    fetch("/icfg")
      .then((r) => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then((j: GraphJSON) => {
        setGraph(j);
        setErr(null);
      })
      .catch((e) => {
        setErr(String(e));
        setGraph(null);
      });
  }, []);

  const fetchStatesForBb = async (bb: string) => {
    const r = await fetch(`/state?bb=${encodeURIComponent(bb)}`);
    if (!r.ok) throw new Error(`HTTP ${r.status}`);
    const j = (await r.json()) as StateResp;
    setCtxMap((prev) => ({ ...prev, [bb]: j.contexts }));
    return j.contexts;
  };

  const fetchEnv = async () => {
    const r = await fetch("/env");
    if (!r.ok) throw new Error(`HTTP ${r.status}`);
    const j = (await r.json()) as EnvResp;
    setEnvItems(j.items || []);
  };

  const selectBestContext = (bb: string, contexts: StateResp["contexts"], preferredCtxt: string) => {
    setSelBb(bb);
    if (contexts.length === 0) {
      setSelCtxt("");
      setSelIsBot(false);
      setSelEntries([]);
      return;
    }
    const hit = contexts.find((c) => c.ctxt === preferredCtxt) ?? contexts[0];
    setSelCtxt(hit.ctxt);
    setSelIsBot(hit.is_bot);
    setSelEntries(hit.entries);
  };

  const centerNodeBetweenPanels = (bbId: string, nodes: Node[]) => {
    if (!rfInstance) return;
    const node = nodes.find((n) => n.id === bbId);
    if (!node) return;

    const w = (node.width as number) || NODE_W;
    const h = (node.height as number) || 200;
    const nodeCenterX = node.position.x + w / 2;
    const nodeCenterY = node.position.y + h / 2;

    const LEFT_W = Math.round(430 * UI_SCALE);
    const RIGHT_W = Math.round(820 * UI_SCALE);
    const M = 16;

    const screenW = window.innerWidth;
    const screenH = window.innerHeight;

    const desiredScreenX = (LEFT_W + M + (screenW - RIGHT_W - M)) / 2;
    const desiredScreenY = screenH / 2;

    const vp: Viewport =
      (rfInstance as any).getViewport ? (rfInstance as any).getViewport() : { x: 0, y: 0, zoom: 1 };
    const zoom = vp.zoom ?? 1;

    const nextViewport: Viewport = {
      x: desiredScreenX - nodeCenterX * zoom,
      y: desiredScreenY - nodeCenterY * zoom,
      zoom,
    };

    (rfInstance as any).setViewport(nextViewport, { duration: 250 });
  };

  useEffect(() => {
    const proto = window.location.protocol === "https:" ? "wss" : "ws";
    const wsUrl = `${proto}://${window.location.host}/ws`;

    setWsStatus("connecting");
    const ws = new WebSocket(wsUrl);
    wsRef.current = ws;

    ws.onopen = () => setWsStatus("connected");
    ws.onclose = () => setWsStatus("disconnected");
    ws.onerror = () => setWsStatus("disconnected");

    ws.onmessage = async (ev) => {
      try {
        const msg = JSON.parse(ev.data) as WLMsg;
        if (msg.type === "worklist") {
          setCurrentBb(msg.bb);
          setCurrentCtxt(msg.ctxt);
          setWl(msg.worklist);

          try {
            const contexts = await fetchStatesForBb(msg.bb);
            // keep selected bb as-is; but if nothing selected yet, default-select current bb
            setSelBb((prev) => (prev ? prev : msg.bb));
            selectBestContext(msg.bb, contexts, msg.ctxt);
          } catch {}

          try {
            await fetchEnv();
          } catch {}
        }
      } catch {}
    };

    return () => {
      ws.close();
      wsRef.current = null;
    };
  }, []);

  const sendCmd = (cmd: "play" | "pause" | "step") => {
    const ws = wsRef.current;
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    ws.send(cmd);
  };

  const onNodeClick = async (_: any, node: any) => {
    const bb = node.id as string;

    // Blue select + center
    setSelBb(bb);

    try {
      const contexts = ctxMap[bb] ?? (await fetchStatesForBb(bb));
      const preferred = bb === currentBb ? currentCtxt : (contexts[0]?.ctxt ?? "");
      selectBestContext(bb, contexts, preferred);
    } catch {
      setSelCtxt("");
      setSelEntries([]);
      setSelIsBot(false);
    }

    // center after selection (uses current layout nodes)
    // will be called again by effect if needed
    // (safe even if not found)
    // eslint-disable-next-line @typescript-eslint/no-use-before-define
  };

  const chooseContext = (bb: string, ctxt: string) => {
    const lst = ctxMap[bb] ?? [];
    const hit = lst.find((x) => x.ctxt === ctxt);
    if (!hit) return;
    setSelBb(bb);
    setSelCtxt(hit.ctxt);
    setSelIsBot(hit.is_bot);
    setSelEntries(hit.entries);
  };

  const rf = useMemo(() => {
    if (!graph) return { nodes: [] as Node[], edges: [] as Edge[] };

    const nodes: Node[] = (graph.nodes || []).map((n) => {
      const label = n.label ?? n.id;
      const instrs = Array.isArray(n.instrs) ? n.instrs : [];
      const isCurrent = currentBb !== "" && n.id === currentBb;
      const isSelected = selBb !== "" && n.id === selBb;

      const contexts = ctxMap[n.id] ?? [];
      const ctxButtons =
        contexts.length === 0 ? (
          <div style={{ fontSize: CTX_BTN_FS, color: "#6b7280", textAlign: "left" }}>(no contexts yet)</div>
        ) : (
          <div style={{ display: "flex", flexWrap: "wrap", gap: 8 }}>
            {contexts.slice(0, 12).map((c) => (
              <button
                key={c.ctxt}
                onClick={(ev) => {
                  ev.stopPropagation();
                  chooseContext(n.id, c.ctxt);
                }}
                style={{
                  fontSize: CTX_BTN_FS,
                  border:
                    c.ctxt === selCtxt && n.id === selBb
                      ? "3px solid #ef4444"
                      : "2px solid #d1d5db",
                  borderRadius: 8,
                  padding: `${CTX_BTN_PAD_Y}px ${CTX_BTN_PAD_X}px`,
                  background: "#fff",
                  cursor: "pointer",
                }}
                title={c.ctxt}
              >
                ctxt
              </button>
            ))}
            {contexts.length > 12 && <span style={{ fontSize: CTX_BTN_FS }}>+{contexts.length - 12}</span>}
          </div>
        );

      const h = computeNodeHeight(instrs.length);

      // Border priority: current(red) > selected(blue) > default
      const border = isCurrent ? "3px solid #ef4444" : isSelected ? "3px solid #2563eb" : "1px solid #111827";
      const shadow = isCurrent
        ? "0 0 0 3px rgba(239,68,68,0.15)"
        : isSelected
        ? "0 0 0 3px rgba(37,99,235,0.12)"
        : "none";

      return {
        id: n.id,
        data: {
          label: (
            <div style={{ fontFamily: "monospace", textAlign: "left" }}>
              <div
                style={{
                  fontSize: NODE_TITLE_FS,
                  fontWeight: 700,
                  marginBottom: 10,
                  wordBreak: "break-all",
                  textAlign: "left",
                }}
              >
                {label}
              </div>

              <div style={{ marginBottom: 12, textAlign: "left" }}>{ctxButtons}</div>

              <div
                style={{
                  fontSize: NODE_BODY_FS,
                  lineHeight: `${NODE_LINE_H}px`,
                  whiteSpace: "pre-wrap",
                  wordBreak: "break-word",
                  textAlign: "left",
                }}
              >
                {instrs.length > 0 ? instrs.join("\n") : "(no instruction data yet)"}
              </div>
            </div>
          ),
        },
        position: { x: 0, y: 0 },
        width: NODE_W,
        height: h,
        style: {
          border,
          borderRadius: 10,
          padding: Math.round(12 * NODE_SCALE),
          width: NODE_W,
          background: "#ffffff",
          boxShadow: shadow,
        },
      };
    });

    const edges: Edge[] = (graph.edges || []).map((e) => {
      const kind: "call" | "fallback" | "ret" | "intra" = e.kind ?? "intra";
      return { id: e.id, source: e.source, target: e.target, style: edgeStyle(kind) };
    });

    return layout(nodes, edges);
  }, [graph, currentBb, selBb, ctxMap, selCtxt]);

  // Center when CURRENT changes (red)
  useEffect(() => {
    if (!rfInstance || !currentBb) return;
    centerNodeBetweenPanels(currentBb, rf.nodes);
  }, [rfInstance, currentBb, rf.nodes]);

  // Center when SELECTED changes (blue)
  useEffect(() => {
    if (!rfInstance || !selBb) return;
    centerNodeBetweenPanels(selBb, rf.nodes);
  }, [rfInstance, selBb, rf.nodes]);

  useEffect(() => {
    if (rfInstance && rf.nodes.length > 0) rfInstance.fitView({ padding: 0.2 });
  }, [rfInstance, rf.nodes.length]);

  if (!graph && !err) return <div style={{ padding: 16, fontFamily: "monospace" }}>Loading /icfg...</div>;
  if (err)
    return (
      <div style={{ padding: 16, fontFamily: "monospace" }}>
        <div>Failed to load /icfg</div>
        <pre>{err}</pre>
      </div>
    );

  const panelFont = Math.round(13 * UI_SCALE);
  const panelSmall = Math.round(12 * UI_SCALE);

  const LEFT_PANEL_W = Math.round(430 * UI_SCALE);
  const RIGHT_PANEL_W = Math.round(820 * UI_SCALE);

  return (
    <div style={{ width: "100vw", height: "100vh" }}>
      {/* left control panel */}
      <div
        style={{
          position: "fixed",
          top: 8,
          left: 8,
          zIndex: 9999,
          background: "#fff",
          padding: Math.round(10 * UI_SCALE),
          border: "1px solid #ccc",
          borderRadius: 10,
          fontFamily: "monospace",
          fontSize: panelFont,
          width: LEFT_PANEL_W,
        }}
      >
        <div style={{ display: "flex", gap: 12, alignItems: "center" }}>
          <button onClick={() => sendCmd("play")} disabled={wsStatus !== "connected"} style={{ fontSize: panelFont }}>
            Play
          </button>
          <button onClick={() => sendCmd("pause")} disabled={wsStatus !== "connected"} style={{ fontSize: panelFont }}>
            Pause
          </button>
          <button onClick={() => sendCmd("step")} disabled={wsStatus !== "connected"} style={{ fontSize: panelFont }}>
            Step
          </button>
          <span style={{ marginLeft: 10 }}>ws={wsStatus}</span>
        </div>

        <div style={{ marginTop: 12 }}>
          <div style={{ fontWeight: 700 }}>Current</div>
          <div style={{ fontSize: panelSmall, whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
            {currentBb ? `${currentBb} / ${currentCtxt}` : "(none)"}
          </div>
          <div style={{ marginTop: 8, fontSize: panelSmall }}>
            Selected: {selBb ? selBb : "(none)"}
          </div>
        </div>

        <div style={{ marginTop: 12 }}>
          <div style={{ fontWeight: 700 }}>Worklist</div>
          <div
            style={{
              marginTop: 8,
              maxHeight: Math.round(240 * UI_SCALE),
              overflow: "auto",
              border: "1px solid #e5e7eb",
              borderRadius: 8,
              padding: Math.round(8 * UI_SCALE),
              fontSize: panelSmall,
              lineHeight: 1.25,
            }}
          >
            {wl.length === 0 ? (
              <div>(empty)</div>
            ) : (
              wl.map((x, i) => (
                <div key={i} style={{ padding: "4px 0", borderBottom: "1px solid #f3f4f6" }}>
                  {x}
                </div>
              ))
            )}
          </div>
        </div>
      </div>

      {/* right panel */}
      <div
        style={{
          position: "fixed",
          top: 8,
          right: 8,
          zIndex: 9999,
          background: "#fff",
          padding: Math.round(10 * UI_SCALE),
          border: "1px solid #ccc",
          borderRadius: 10,
          fontFamily: "monospace",
          fontSize: panelFont,
          width: RIGHT_PANEL_W,
          maxHeight: "95vh",
          overflow: "auto",
        }}
      >
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: Math.round(14 * UI_SCALE) }}>
          <div>
            <div style={{ fontWeight: 700 }}>ENV (var → addr)</div>
            <div style={{ marginTop: 10, fontSize: panelSmall }}>
              {envItems.length === 0 ? (
                <div>(empty)</div>
              ) : (
                <table style={{ width: "100%", borderCollapse: "collapse" }}>
                  <thead>
                    <tr>
                      <th style={{ textAlign: "left", borderBottom: "1px solid #e5e7eb", paddingBottom: 8 }}>var</th>
                      <th style={{ textAlign: "left", borderBottom: "1px solid #e5e7eb", paddingBottom: 8 }}>addr</th>
                    </tr>
                  </thead>
                  <tbody>
                    {envItems.map((it, i) => (
                      <tr key={i}>
                        <td style={{ verticalAlign: "top", padding: "8px 10px 8px 0", borderBottom: "1px solid #f3f4f6" }}>
                          {it.var}
                        </td>
                        <td style={{ verticalAlign: "top", padding: "8px 0", borderBottom: "1px solid #f3f4f6" }}>
                          {it.addr}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </div>
          </div>

          <div>
            <div style={{ fontWeight: 700 }}>Selected State</div>
            <div style={{ marginTop: 10, fontSize: panelSmall, wordBreak: "break-word" }}>
              bb: {selBb || "(none)"} <br />
              ctxt: {selCtxt || "(none)"} <br />
              bot: {selBb ? String(selIsBot) : "(n/a)"}
            </div>

            <div style={{ marginTop: 12, fontSize: panelSmall }}>
              {selEntries.length === 0 ? (
                <div>(no entries)</div>
              ) : (
                <table style={{ width: "100%", borderCollapse: "collapse" }}>
                  <thead>
                    <tr>
                      <th style={{ textAlign: "left", borderBottom: "1px solid #e5e7eb", paddingBottom: 8 }}>addr</th>
                      <th style={{ textAlign: "left", borderBottom: "1px solid #e5e7eb", paddingBottom: 8 }}>value</th>
                    </tr>
                  </thead>
                  <tbody>
                    {selEntries.map((e, i) => (
                      <tr key={i}>
                        <td style={{ verticalAlign: "top", padding: "8px 10px 8px 0", borderBottom: "1px solid #f3f4f6" }}>
                          {e.addr}
                        </td>
                        <td style={{ verticalAlign: "top", padding: "8px 0", borderBottom: "1px solid #f3f4f6" }}>
                          {e.value}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </div>
          </div>
        </div>
      </div>

      <ReactFlow nodes={rf.nodes} edges={rf.edges} onInit={setRfInstance} onNodeClick={onNodeClick}>
        <Background />
        <Controls />
      </ReactFlow>
    </div>
  );
}
