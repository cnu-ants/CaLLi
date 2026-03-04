import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import ReactFlow, { Background, Controls, MiniMap, Position } from "reactflow";
import type { Edge, Node, ReactFlowInstance, Viewport } from "reactflow";
import "reactflow/dist/style.css";
import dagre from "dagre";

type NodeJSON = { id: string; label?: string; instrs?: string[] };
type EdgeJSON = { id: string; source: string; target: string; kind?: "call" | "fallback" | "ret" | "intra" };
type GraphJSON = { nodes: NodeJSON[]; edges: EdgeJSON[] };

type WorklistMsg = {
  type: "worklist";
  bb: string;
  ctxt: string;
  current: string;
  worklist: string[];
  ran?: number;
  reason?: string;
};

type WLMsg =
  | WorklistMsg
  | { type: "done"; ran?: number; reason?: string }
  | { type: "breakpoints"; bbs: string[] }
  | { type: "error"; msg: string };

type StateResp = {
  bb: string;
  contexts: { ctxt: string; is_bot: boolean; entries: { addr: string; value: string }[] }[];
};

type StatesResp = { items: StateResp[] };
type EnvResp = { items: { var: string; addr: string }[] };

type NodeData = { label: React.ReactNode; rawLabel: string };

const NODE_W = 420;

const UI_SCALE = 1.5;
const NODE_SCALE = 1.25;

const NODE_TITLE_FS = Math.round(15 * NODE_SCALE);
const NODE_BODY_FS = Math.round(13 * NODE_SCALE);
const NODE_LINE_H = Math.round(16 * NODE_SCALE);

const CTX_BTN_FS = Math.round(12 * NODE_SCALE * 1.15);
const CTX_BTN_PAD_Y = Math.round(4 * NODE_SCALE * 1.2);
const CTX_BTN_PAD_X = Math.round(10 * NODE_SCALE * 1.2);

const BP_LS_KEY = "calli_breakpoints_v1";

const INSTR_BOX_H = Math.round(300 * NODE_SCALE);
const NODE_HEADER_H = Math.round(54 * NODE_SCALE);
const NODE_CTX_H = Math.round(64 * NODE_SCALE);
const NODE_PAD_H = Math.round(34 * NODE_SCALE);
const NODE_SAFETY_H = Math.round(18 * NODE_SCALE);
const NODE_H = NODE_HEADER_H + NODE_CTX_H + NODE_PAD_H + INSTR_BOX_H + NODE_SAFETY_H;

function edgeStyle(kind: "call" | "fallback" | "ret" | "intra") {
  if (kind === "call") return { strokeWidth: 2.5, stroke: "#2563eb" };
  if (kind === "fallback") return { strokeWidth: 2.5, stroke: "#6b7280", strokeDasharray: "6 6" };
  if (kind === "ret") return { strokeWidth: 2.5, stroke: "#16a34a" };
  return { strokeWidth: 2.5, stroke: "#111827" };
}

function loadBpMap(): Record<string, boolean> {
  try {
    const s = localStorage.getItem(BP_LS_KEY);
    if (!s) return {};
    const j = JSON.parse(s) as Record<string, boolean>;
    return j && typeof j === "object" ? j : {};
  } catch {
    return {};
  }
}

function saveBpMap(m: Record<string, boolean>) {
  try {
    localStorage.setItem(BP_LS_KEY, JSON.stringify(m));
  } catch {}
}

type LayoutInfo = {
  pos: Record<string, { x: number; y: number }>;
  edges: { id: string; source: string; target: string; kind: "call" | "fallback" | "ret" | "intra" }[];
};

function computeLayout(graph: GraphJSON): LayoutInfo {
  const nodes = graph.nodes || [];
  const edges = (graph.edges || []).map((e) => ({
    id: e.id,
    source: e.source,
    target: e.target,
    kind: (e.kind ?? "intra") as "call" | "fallback" | "ret" | "intra",
  }));

  const nodeCount = nodes.length;

  const sizeFactor = nodeCount > 800 ? 1.15 : nodeCount > 400 ? 1.1 : nodeCount > 200 ? 1.05 : 1.0;

  const baseNodesep = Math.round(Math.max(40, Math.min(95, NODE_W * 0.18)));
  const baseRanksep = Math.round(Math.max(120, Math.min(260, NODE_H * 0.33)));

  const nodesep = Math.round(baseNodesep * sizeFactor);
  const ranksep = Math.round(baseRanksep * sizeFactor);

  const g = new dagre.graphlib.Graph();
  g.setDefaultEdgeLabel(() => ({}));

  g.setGraph({
    rankdir: "TB",
    nodesep,
    ranksep,
    marginx: 36,
    marginy: 36,
    ranker: "longest-path",
  });

  for (const n of nodes) {
    g.setNode(n.id, { width: NODE_W, height: NODE_H });
  }

  for (const e of edges) {
    g.setEdge(e.source, e.target, { minlen: 1 });
  }

  dagre.layout(g);

  const pos: Record<string, { x: number; y: number }> = {};
  for (const n of nodes) {
    const p = g.node(n.id);
    pos[n.id] = { x: p.x - NODE_W / 2, y: p.y - NODE_H / 2 };
  }

  return { pos, edges };
}

function normalizeQuery(s: string): string {
  return s.trim().toLowerCase();
}

function nodeSearchText(n: Node<NodeData>): string {
  const raw = n.data?.rawLabel ?? "";
  return `${n.id} ${raw}`.trim();
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

  const [selBb, setSelBb] = useState<string>("");
  const [selCtxt, setSelCtxt] = useState<string>("");
  const [selEntries, setSelEntries] = useState<{ addr: string; value: string }[]>([]);
  const [selIsBot, setSelIsBot] = useState<boolean>(false);

  const [envItems, setEnvItems] = useState<{ var: string; addr: string }[]>([]);

  const [bpMap, setBpMap] = useState<Record<string, boolean>>(() => loadBpMap());

  const [searchText, setSearchText] = useState<string>("");
  const [activeMatchIdx, setActiveMatchIdx] = useState<number>(0);

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

  const fetchAllStates = async () => {
    const r = await fetch("/states");
    if (!r.ok) throw new Error(`HTTP ${r.status}`);
    const j = (await r.json()) as StatesResp;
    const items = Array.isArray(j.items) ? j.items : [];
    setCtxMap((prev) => {
      const next = { ...prev };
      for (const it of items) next[it.bb] = it.contexts;
      return next;
    });
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
    const h = (node.height as number) || NODE_H;
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

  const sendWs = (obj: any) => {
    const ws = wsRef.current;
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    ws.send(JSON.stringify(obj));
  };

  const syncBpsToServer = (m: Record<string, boolean>) => {
    const bbs = Object.keys(m).filter((k) => m[k]);
    sendWs({ cmd: "bp_sync", bbs });
  };

  const setBreakpoint = (bb: string, enabled: boolean) => {
    setBpMap((prev) => {
      const next = { ...prev, [bb]: enabled };
      saveBpMap(next);
      sendWs({ cmd: "bp_set", bb, enabled });
      return next;
    });
  };

  useEffect(() => {
    const proto = window.location.protocol === "https:" ? "wss" : "ws";
    const wsUrl = `${proto}://${window.location.host}/ws`;

    setWsStatus("connecting");
    const ws = new WebSocket(wsUrl);
    wsRef.current = ws;

    ws.onopen = () => {
      setWsStatus("connected");
      syncBpsToServer(loadBpMap());
    };
    ws.onclose = () => setWsStatus("disconnected");
    ws.onerror = () => setWsStatus("disconnected");

    ws.onmessage = async (ev) => {
      try {
        const msg = JSON.parse(ev.data) as WLMsg;

        if (msg.type === "breakpoints") {
          const serverMap: Record<string, boolean> = {};
          for (const bb of msg.bbs || []) serverMap[bb] = true;
          saveBpMap(serverMap);
          setBpMap(serverMap);
          return;
        }

        if (msg.type === "done") {
          try {
            await fetchAllStates();
          } catch {}
          try {
            await fetchEnv();
          } catch {}
          setWl([]);
          return;
        }

        if (msg.type === "worklist") {
          setCurrentBb(msg.bb);
          setCurrentCtxt(msg.ctxt);
          setWl(msg.worklist);

          const ran = typeof msg.ran === "number" ? msg.ran : 0;

          try {
            if (ran > 1) await fetchAllStates();
          } catch {}

          try {
            const contexts = await fetchStatesForBb(msg.bb);
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
    sendWs({ cmd });
  };

  const onNodeClick = async (_: any, node: any) => {
    const bb = node.id as string;
    setSelBb(bb);

    try {
      const contexts = ctxMap[bb] ?? (await fetchStatesForBb(bb));
      const preferred = bb === currentBb ? currentCtxt : contexts[0]?.ctxt ?? "";
      selectBestContext(bb, contexts, preferred);
    } catch {
      setSelCtxt("");
      setSelEntries([]);
      setSelIsBot(false);
    }
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

  const layoutInfo = useMemo<LayoutInfo | null>(() => {
    if (!graph) return null;
    return computeLayout(graph);
  }, [graph]);

  const rf = useMemo(() => {
    if (!graph || !layoutInfo) return { nodes: [] as Node<NodeData>[], edges: [] as Edge[] };

    const nodes: Node<NodeData>[] = (graph.nodes || []).map((n) => {
      const labelText = n.label ?? n.id;
      const instrs = Array.isArray(n.instrs) ? n.instrs : [];
      const isCurrent = currentBb !== "" && n.id === currentBb;
      const isSelected = selBb !== "" && n.id === selBb;
      const isBp = !!bpMap[n.id];

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
                  border: c.ctxt === selCtxt && n.id === selBb ? "3px solid #ef4444" : "2px solid #d1d5db",
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

      const p = layoutInfo.pos[n.id] ?? { x: 0, y: 0 };

      const border = isCurrent
        ? "3px solid #ef4444"
        : isSelected
        ? "3px solid #2563eb"
        : isBp
        ? "3px solid #f59e0b"
        : "1px solid #111827";

      const shadow = isCurrent
        ? "0 0 0 3px rgba(239,68,68,0.15)"
        : isSelected
        ? "0 0 0 3px rgba(37,99,235,0.12)"
        : isBp
        ? "0 0 0 3px rgba(245,158,11,0.12)"
        : "none";

      return {
        id: n.id,
        data: {
          rawLabel: labelText,
          label: (
            <div style={{ fontFamily: "monospace", textAlign: "left" }}>
              <div
                style={{
                  display: "flex",
                  alignItems: "center",
                  justifyContent: "space-between",
                  gap: 10,
                  marginBottom: 10,
                }}
              >
                <div
                  style={{
                    fontSize: NODE_TITLE_FS,
                    fontWeight: 700,
                    wordBreak: "break-all",
                    textAlign: "left",
                    flex: 1,
                  }}
                >
                  {labelText}
                </div>

                <label
                  style={{
                    display: "flex",
                    alignItems: "center",
                    gap: 8,
                    fontSize: NODE_BODY_FS,
                    userSelect: "none",
                    whiteSpace: "nowrap",
                  }}
                  onClick={(ev) => ev.stopPropagation()}
                  title="Breakpoint"
                >
                  <input
                    type="checkbox"
                    checked={isBp}
                    onChange={(ev) => {
                      ev.stopPropagation();
                      setBreakpoint(n.id, ev.target.checked);
                    }}
                    onClick={(ev) => ev.stopPropagation()}
                  />
                  bp
                </label>
              </div>

              <div style={{ marginBottom: 12, textAlign: "left" }}>{ctxButtons}</div>

              <div
                style={{
                  fontSize: NODE_BODY_FS,
                  lineHeight: `${NODE_LINE_H}px`,
                  whiteSpace: "pre-wrap",
                  wordBreak: "break-word",
                  textAlign: "left",
                  maxHeight: INSTR_BOX_H,
                  overflowY: "auto",
                  border: "1px solid #e5e7eb",
                  borderRadius: 8,
                  padding: 10,
                }}
              >
                {instrs.length > 0 ? instrs.join("\n") : "(no instruction data yet)"}
              </div>
            </div>
          ),
        },
        position: p,
        width: NODE_W,
        height: NODE_H,
        style: {
          border,
          borderRadius: 10,
          padding: Math.round(12 * NODE_SCALE),
          width: NODE_W,
          background: "#ffffff",
          boxShadow: shadow,
        },
        targetPosition: Position.Top,
        sourcePosition: Position.Bottom,
      };
    });

    const edges: Edge[] = layoutInfo.edges.map((e) => ({
      id: e.id,
      source: e.source,
      target: e.target,
      style: edgeStyle(e.kind),
    }));

    return { nodes, edges };
  }, [graph, layoutInfo, currentBb, selBb, ctxMap, selCtxt, bpMap]);

  const matchedNodeIds = useMemo(() => {
    const q = normalizeQuery(searchText);
    if (!q) return [];
    return rf.nodes
      .filter((n) => nodeSearchText(n as Node<NodeData>).toLowerCase().includes(q))
      .map((n) => n.id);
  }, [searchText, rf.nodes]);

  useEffect(() => {
    setActiveMatchIdx(0);
  }, [searchText]);

  const activeMatchId =
    matchedNodeIds.length > 0 ? matchedNodeIds[Math.min(activeMatchIdx, matchedNodeIds.length - 1)] : null;

  const focusNodeById = useCallback(
    (id: string) => {
      if (!rfInstance) return;
      if (!id) return;
      centerNodeBetweenPanels(id, rf.nodes);
    },
    [rfInstance, rf.nodes]
  );

  const gotoActiveMatch = useCallback(() => {
    if (!activeMatchId) return;
    focusNodeById(activeMatchId);
  }, [activeMatchId, focusNodeById]);

  const gotoPrevMatch = useCallback(() => {
    if (matchedNodeIds.length === 0) return;
    setActiveMatchIdx((i) => {
      const ni = (i - 1 + matchedNodeIds.length) % matchedNodeIds.length;
      const id = matchedNodeIds[ni];
      if (id) focusNodeById(id);
      return ni;
    });
  }, [matchedNodeIds, focusNodeById]);

  const gotoNextMatch = useCallback(() => {
    if (matchedNodeIds.length === 0) return;
    setActiveMatchIdx((i) => {
      const ni = (i + 1) % matchedNodeIds.length;
      const id = matchedNodeIds[ni];
      if (id) focusNodeById(id);
      return ni;
    });
  }, [matchedNodeIds, focusNodeById]);

  const nodesForRender = useMemo(() => {
    const q = normalizeQuery(searchText);
    if (!q) return rf.nodes;

    const matchSet = new Set(matchedNodeIds);
    const active = activeMatchId;

    return rf.nodes.map((n) => {
      if (!matchSet.has(n.id)) return n;

      const isActive = active === n.id;
      const prevStyle = (n.style ?? {}) as React.CSSProperties;

      return {
        ...n,
        style: {
          ...prevStyle,
          outline: isActive ? "3px solid rgba(17,24,39,0.95)" : "2px solid rgba(17,24,39,0.6)",
          outlineOffset: 2,
        },
      };
    });
  }, [rf.nodes, searchText, matchedNodeIds, activeMatchId]);

  useEffect(() => {
    if (!rfInstance || !currentBb) return;
    centerNodeBetweenPanels(currentBb, rf.nodes);
  }, [rfInstance, currentBb, rf.nodes]);

  useEffect(() => {
    if (!rfInstance || !selBb) return;
    centerNodeBetweenPanels(selBb, rf.nodes);
  }, [rfInstance, selBb, rf.nodes]);

  useEffect(() => {
    if (!rfInstance) return;
    if (!graph) return;
    if (rf.nodes.length === 0) return;
    rfInstance.fitView({ padding: 0.12 });
  }, [rfInstance, !!graph]);

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

  const MINIMAP_W = 440;
  const MINIMAP_H = 320;
  const MINIMAP_LEFT = 0;
  const MINIMAP_BOTTOM = 0;

  const bpList = Object.keys(bpMap).filter((k) => bpMap[k]);

  const miniNodeColor = (n: any) => {
    const id = String(n.id ?? "");
    if (id && id === currentBb) return "#ef4444";
    if (id && id === selBb) return "#2563eb";
    if (id && bpMap[id]) return "#f59e0b";
    return "#9ca3af";
  };

  return (
    <div style={{ width: "100vw", height: "100vh" }}>
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
          <div style={{ marginTop: 8, fontSize: panelSmall }}>Selected: {selBb ? selBb : "(none)"}</div>
        </div>

        <div style={{ marginTop: 12 }}>
          <div style={{ fontWeight: 700 }}>Breakpoints</div>
          <div
            style={{
              marginTop: 8,
              maxHeight: Math.round(120 * UI_SCALE),
              overflow: "auto",
              border: "1px solid #e5e7eb",
              borderRadius: 8,
              padding: Math.round(8 * UI_SCALE),
              fontSize: panelSmall,
              lineHeight: 1.25,
            }}
          >
            {bpList.length === 0 ? (
              <div>(none)</div>
            ) : (
              bpList.map((x) => (
                <div key={x} style={{ display: "flex", justifyContent: "space-between", gap: 10, padding: "4px 0" }}>
                  <span style={{ wordBreak: "break-all" }}>{x}</span>
                  <button
                    style={{ fontSize: panelSmall }}
                    onClick={() => setBreakpoint(x, false)}
                    disabled={wsStatus !== "connected"}
                  >
                    x
                  </button>
                </div>
              ))
            )}
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

        <div style={{ marginTop: 12 }}>
          <div style={{ fontWeight: 700 }}>Search</div>

          <input
            value={searchText}
            onChange={(e) => setSearchText(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter") gotoActiveMatch();
              if (e.key === "ArrowUp") gotoPrevMatch();
              if (e.key === "ArrowDown") gotoNextMatch();
              if (e.key === "Escape") setSearchText("");
            }}
            placeholder="Search by block id / label"
            style={{
              width: "100%",
              boxSizing: "border-box",
              marginTop: 8,
              padding: "8px 10px",
              borderRadius: 8,
              border: "1px solid rgba(0,0,0,0.2)",
              fontSize: panelSmall,
              fontFamily: "monospace",
            }}
          />

          <div
            style={{
              display: "flex",
              alignItems: "center",
              justifyContent: "space-between",
              gap: 8,
              marginTop: 8,
            }}
          >
            <div style={{ fontSize: panelSmall, opacity: 0.85 }}>
              {matchedNodeIds.length === 0 ? "0 matches" : `${matchedNodeIds.length} matches`}
              {activeMatchId ? ` (selected: ${activeMatchIdx + 1}/${matchedNodeIds.length})` : ""}
            </div>

            <div style={{ display: "flex", gap: 6 }}>
              <button
                type="button"
                onClick={gotoPrevMatch}
                disabled={matchedNodeIds.length === 0}
                style={{ padding: "6px 10px", borderRadius: 8, fontSize: panelSmall }}
              >
                Prev
              </button>
              <button
                type="button"
                onClick={gotoNextMatch}
                disabled={matchedNodeIds.length === 0}
                style={{ padding: "6px 10px", borderRadius: 8, fontSize: panelSmall }}
              >
                Next
              </button>
            </div>
          </div>

          {activeMatchId && (
            <div style={{ marginTop: 8, fontSize: panelSmall, opacity: 0.85, wordBreak: "break-all" }}>
              Active: {activeMatchId}
            </div>
          )}
        </div>
      </div>

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

      <ReactFlow nodes={nodesForRender} edges={rf.edges} onInit={setRfInstance} onNodeClick={onNodeClick}>
        <Background />
        <Controls />

        <MiniMap
          style={{
            position: "fixed",
            left: MINIMAP_LEFT,
            bottom: MINIMAP_BOTTOM,
            width: MINIMAP_W,
            height: MINIMAP_H,
            background: "#ffffff",
            border: "1px solid #d1d5db",
            borderRadius: 10,
            zIndex: 9998,
          }}
          nodeColor={miniNodeColor}
          nodeStrokeColor="#111827"
          nodeBorderRadius={6}
          maskColor="rgba(0,0,0,0.12)"
          pannable
          zoomable
        />
      </ReactFlow>
    </div>
  );
}
