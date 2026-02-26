import React, { useEffect, useMemo, useRef, useState } from "react";
import ReactFlow, { Background, Controls, Position } from "reactflow";
import type { Edge, Node, ReactFlowInstance } from "reactflow";
import "reactflow/dist/style.css";
import dagre from "dagre";

type NodeJSON = { id: string; label?: string; instrs?: string[] };
type EdgeJSON = { id: string; source: string; target: string; kind?: "call" | "fallback" | "ret" | "intra" };
type GraphJSON = { nodes: NodeJSON[]; edges: EdgeJSON[] };

type WLMsg =
  | { type: "worklist"; bb: string; ctxt: string; current: string; worklist: string[] }
  | { type: "done" }
  | { type: "error"; msg: string };

type StateResp = { bb: string; contexts: { ctxt: string; mem: string }[] };

const NODE_W = 360;
const NODE_H = 190;

function layout(nodes: Node[], edges: Edge[]) {
  const g = new dagre.graphlib.Graph();
  g.setDefaultEdgeLabel(() => ({}));
  g.setGraph({ rankdir: "TB", nodesep: 40, ranksep: 140 });

  for (const n of nodes) g.setNode(n.id, { width: NODE_W, height: NODE_H });
  for (const e of edges) g.setEdge(e.source, e.target);

  dagre.layout(g);

  const laidOut = nodes.map((n) => {
    const p = g.node(n.id);
    return {
      ...n,
      position: { x: p.x - NODE_W / 2, y: p.y - NODE_H / 2 },
      targetPosition: Position.Top,
      sourcePosition: Position.Bottom,
    };
  });

  return { nodes: laidOut, edges };
}

function edgeStyle(kind: "call" | "fallback" | "ret" | "intra") {
  if (kind === "call") return { strokeWidth: 2.5, stroke: "#2563eb" };
  if (kind === "fallback") return { strokeWidth: 2.5, stroke: "#6b7280", strokeDasharray: "6 6" };
  if (kind === "ret") return { strokeWidth: 2.5, stroke: "#16a34a" };
  return { strokeWidth: 2.5, stroke: "#111827" };
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

  // bb -> contexts list
  const [ctxMap, setCtxMap] = useState<Record<string, { ctxt: string; mem: string }[]>>({});

  // right panel selection
  const [selBb, setSelBb] = useState<string>("");
  const [selCtxt, setSelCtxt] = useState<string>("");
  const [selMem, setSelMem] = useState<string>("");

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

  const selectBestContext = (bb: string, contexts: { ctxt: string; mem: string }[], preferredCtxt: string) => {
    if (contexts.length === 0) {
      setSelBb(bb);
      setSelCtxt("");
      setSelMem("(no state yet for this basic block)");
      return;
    }
    const hit = contexts.find((c) => c.ctxt === preferredCtxt) ?? contexts[0];
    setSelBb(bb);
    setSelCtxt(hit.ctxt);
    setSelMem(hit.mem);
  };

  // WebSocket: on every step, auto-load states for current bb and auto-select current ctxt
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

          // auto-fetch current bb states and select msg.ctxt by default
          try {
            const contexts = await fetchStatesForBb(msg.bb);
            selectBestContext(msg.bb, contexts, msg.ctxt);
          } catch (e) {
            // keep UI alive
          }
        }
      } catch {
        // ignore
      }
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
    try {
      const contexts = ctxMap[bb] ?? (await fetchStatesForBb(bb));
      // prefer currentCtxt if the clicked node is currentBb; otherwise pick first
      const preferred = bb === currentBb ? currentCtxt : (contexts[0]?.ctxt ?? "");
      selectBestContext(bb, contexts, preferred);
    } catch (e) {
      setSelBb(bb);
      setSelCtxt("");
      setSelMem(String(e));
    }
  };

  const chooseContext = (bb: string, ctxt: string) => {
    const lst = ctxMap[bb] ?? [];
    const hit = lst.find((x) => x.ctxt === ctxt);
    setSelBb(bb);
    setSelCtxt(ctxt);
    setSelMem(hit ? hit.mem : "(missing mem)");
  };

  const rf = useMemo(() => {
    if (!graph) return { nodes: [] as Node[], edges: [] as Edge[] };

    const nodes: Node[] = (graph.nodes || []).map((n) => {
      const label = n.label ?? n.id;
      const instrs = Array.isArray(n.instrs) ? n.instrs : [];
      const isCurrent = currentBb !== "" && n.id === currentBb;

      const contexts = ctxMap[n.id] ?? [];
      const ctxButtons =
        contexts.length === 0 ? (
          <div style={{ fontSize: 12, color: "#6b7280" }}>(no contexts yet)</div>
        ) : (
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {contexts.slice(0, 8).map((c) => (
              <button
                key={c.ctxt}
                onClick={(ev) => {
                  ev.stopPropagation();
                  chooseContext(n.id, c.ctxt);
                }}
                style={{
                  fontSize: 12,
                  border: c.ctxt === selCtxt && n.id === selBb ? "2px solid #ef4444" : "1px solid #d1d5db",
                  borderRadius: 6,
                  padding: "2px 6px",
                  background: "#fff",
                  cursor: "pointer",
                }}
                title={c.ctxt}
              >
                ctxt
              </button>
            ))}
            {contexts.length > 8 && <span style={{ fontSize: 12 }}>+{contexts.length - 8}</span>}
          </div>
        );

      return {
        id: n.id,
        data: {
          label: (
            <div style={{ fontFamily: "monospace" }}>
              <div style={{ fontSize: 15, fontWeight: 700, marginBottom: 10, wordBreak: "break-all" }}>{label}</div>

              <div style={{ marginBottom: 10 }}>{ctxButtons}</div>

              <pre
                style={{
                  margin: 0,
                  fontSize: 13,
                  lineHeight: 1.25,
                  whiteSpace: "pre-wrap",
                  wordBreak: "break-word",
                  maxHeight: 110,
                  overflow: "auto",
                  borderTop: "1px solid #e5e7eb",
                  paddingTop: 10,
                }}
              >
                {instrs.length > 0 ? instrs.join("\n") : "(no instruction data yet)"}
              </pre>
            </div>
          ),
        },
        position: { x: 0, y: 0 },
        width: NODE_W,
        height: NODE_H,
        style: {
          border: isCurrent ? "3px solid #ef4444" : "1px solid #111827",
          borderRadius: 10,
          padding: 12,
          width: NODE_W,
          background: "#ffffff",
          boxShadow: isCurrent ? "0 0 0 3px rgba(239,68,68,0.15)" : "none",
        },
      };
    });

    const edges: Edge[] = (graph.edges || []).map((e) => {
      const kind: "call" | "fallback" | "ret" | "intra" = e.kind ?? "intra";
      return { id: e.id, source: e.source, target: e.target, style: edgeStyle(kind) };
    });

    return layout(nodes, edges);
  }, [graph, currentBb, ctxMap, selBb, selCtxt, currentCtxt]);

  useEffect(() => {
    if (rfInstance && rf.nodes.length > 0) rfInstance.fitView({ padding: 0.2 });
  }, [rfInstance, rf.nodes.length]);

  if (!graph && !err) return <div style={{ padding: 16, fontFamily: "monospace", fontSize: 14 }}>Loading /icfg...</div>;
  if (err)
    return (
      <div style={{ padding: 16, fontFamily: "monospace", fontSize: 14 }}>
        <div>Failed to load /icfg</div>
        <pre>{err}</pre>
      </div>
    );

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
          padding: 10,
          border: "1px solid #ccc",
          borderRadius: 8,
          fontFamily: "monospace",
          fontSize: 13,
          width: 430,
        }}
      >
        <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
          <button onClick={() => sendCmd("play")} disabled={wsStatus !== "connected"}>
            Play
          </button>
          <button onClick={() => sendCmd("pause")} disabled={wsStatus !== "connected"}>
            Pause
          </button>
          <button onClick={() => sendCmd("step")} disabled={wsStatus !== "connected"}>
            Step
          </button>
          <span style={{ marginLeft: 8 }}>ws={wsStatus}</span>
        </div>

        <div style={{ marginTop: 10 }}>
          <div style={{ fontWeight: 700 }}>Current</div>
          <div style={{ fontSize: 12, whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
            {currentBb ? `${currentBb} / ${currentCtxt}` : "(none)"}
          </div>
        </div>

        <div style={{ marginTop: 10 }}>
          <div style={{ fontWeight: 700 }}>Worklist</div>
          <div
            style={{
              marginTop: 6,
              maxHeight: 240,
              overflow: "auto",
              border: "1px solid #e5e7eb",
              borderRadius: 6,
              padding: 8,
              fontSize: 12,
              lineHeight: 1.25,
            }}
          >
            {wl.length === 0 ? (
              <div>(empty)</div>
            ) : (
              wl.map((x, i) => (
                <div key={i} style={{ padding: "2px 0", borderBottom: "1px solid #f3f4f6" }}>
                  {x}
                </div>
              ))
            )}
          </div>
        </div>

        <div style={{ marginTop: 10, fontSize: 12 }}>
          <span style={{ color: "#2563eb" }}>call</span>{" "}
          <span style={{ color: "#6b7280" }}>fallback</span>{" "}
          <span style={{ color: "#16a34a" }}>ret</span>{" "}
          <span style={{ color: "#111827" }}>intra</span>
        </div>
      </div>

      {/* right memory panel */}
      <div
        style={{
          position: "fixed",
          top: 8,
          right: 8,
          zIndex: 9999,
          background: "#fff",
          padding: 10,
          border: "1px solid #ccc",
          borderRadius: 8,
          fontFamily: "monospace",
          fontSize: 13,
          width: 520,
          maxHeight: "95vh",
          overflow: "auto",
        }}
      >
        <div style={{ fontWeight: 700 }}>Selected State</div>
        <div style={{ marginTop: 6, fontSize: 12, wordBreak: "break-word" }}>
          bb: {selBb || "(none)"}
          <br />
          ctxt: {selCtxt || "(none)"}
        </div>
        <pre style={{ marginTop: 10, fontSize: 12, lineHeight: 1.25, whiteSpace: "pre-wrap" }}>
          {selMem || "(step or click a node)"}
        </pre>
      </div>

      <ReactFlow nodes={rf.nodes} edges={rf.edges} onInit={setRfInstance} onNodeClick={onNodeClick}>
        <Background />
        <Controls />
      </ReactFlow>
    </div>
  );
}
