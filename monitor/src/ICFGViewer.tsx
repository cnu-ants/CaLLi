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

const DEBUG = true;

function ts(): string {
  const d = new Date();
  const hh = String(d.getHours()).padStart(2, "0");
  const mm = String(d.getMinutes()).padStart(2, "0");
  const ss = String(d.getSeconds()).padStart(2, "0");
  const ms = String(d.getMilliseconds()).padStart(3, "0");
  return `${hh}:${mm}:${ss}.${ms}`;
}

function dbg(...args: any[]) {
  if (!DEBUG) return;
  // eslint-disable-next-line no-console
  console.log(`[${ts()}]`, ...args);
}

async function fetchJson<T>(url: string): Promise<T> {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`HTTP ${r.status}`);
  return (await r.json()) as T;
}

const NODE_W = 420;

const UI_SCALE = 1.0;
const NODE_SCALE = 1.0;

const NODE_TITLE_FS = Math.round(15 * NODE_SCALE);
const NODE_BODY_FS = Math.round(13 * NODE_SCALE);
const NODE_LINE_H = Math.round(16 * NODE_SCALE);

const CTX_BTN_FS = Math.round(12 * NODE_SCALE * 1.15);
const CTX_BTN_PAD_Y = Math.round(4 * NODE_SCALE * 1.2);
const CTX_BTN_PAD_X = Math.round(10 * NODE_SCALE * 1.2);

const BP_LS_KEY = "calli_breakpoints_v1";
const UI_RIGHT_PANEL_W_KEY = "calli_ui_right_panel_w_v1";
const UI_ENV_COLLAPSED_KEY = "calli_ui_env_collapsed_v1";
const UI_STATE_COLLAPSED_KEY = "calli_ui_state_collapsed_v1";

const INSTR_BOX_H = Math.round(300 * NODE_SCALE);
const NODE_HEADER_H = Math.round(54 * NODE_SCALE);
const NODE_CTX_H = Math.round(64 * NODE_SCALE);
const NODE_PAD_H = Math.round(34 * NODE_SCALE);
const NODE_SAFETY_H = Math.round(18 * NODE_SCALE);
const NODE_H = NODE_HEADER_H + NODE_CTX_H + NODE_PAD_H + INSTR_BOX_H + NODE_SAFETY_H;

const MIN_ZOOM = 0.02;
const MAX_ZOOM = 2.0;

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

function loadBool(key: string, def: boolean): boolean {
  try {
    const s = localStorage.getItem(key);
    if (s === null) return def;
    return s === "1";
  } catch {
    return def;
  }
}

function saveBool(key: string, v: boolean) {
  try {
    localStorage.setItem(key, v ? "1" : "0");
  } catch {}
}

function loadNum(key: string, def: number): number {
  try {
    const s = localStorage.getItem(key);
    if (!s) return def;
    const n = Number(s);
    return Number.isFinite(n) ? n : def;
  } catch {
    return def;
  }
}

function saveNum(key: string, v: number) {
  try {
    localStorage.setItem(key, String(v));
  } catch {}
}

type LayoutInfo = {
  pos: Record<string, { x: number; y: number }>;
  edges: { id: string; source: string; target: string; kind: "call" | "fallback" | "ret" | "intra" }[];
};

function clamp(n: number, lo: number, hi: number) {
  return Math.max(lo, Math.min(hi, n));
}

function computeLayout(graph: GraphJSON): LayoutInfo {
  const nodes = graph.nodes || [];
  const edges = (graph.edges || []).map((e) => ({
    id: e.id,
    source: e.source,
    target: e.target,
    kind: (e.kind ?? "intra") as "call" | "fallback" | "ret" | "intra",
  }));

  const nodeCount = nodes.length;

  const tightFactor =
    nodeCount > 1200 ? 0.55 : nodeCount > 800 ? 0.62 : nodeCount > 400 ? 0.72 : nodeCount > 200 ? 0.82 : 0.9;

  const baseNodesep = Math.round(clamp(NODE_W * 0.06, 14, 44));
  const baseRanksep = Math.round(clamp(NODE_H * 0.06, 22, 96));

  const nodesep = Math.round(clamp(baseNodesep * tightFactor, 10, 60));
  const ranksep = Math.round(clamp(baseRanksep * tightFactor, 16, 140));

  const g = new dagre.graphlib.Graph();
  g.setDefaultEdgeLabel(() => ({}));

  g.setGraph({
    rankdir: "TB",
    nodesep,
    ranksep,
    marginx: 18,
    marginy: 18,
    ranker: "tight-tree",
  });

  for (const n of nodes) g.setNode(n.id, { width: NODE_W, height: NODE_H });
  for (const e of edges) g.setEdge(e.source, e.target, { minlen: 1 });

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

function mkKey(bb: string, ctxt: string): string {
  return `${bb}||${ctxt}`;
}

function entriesToMap(entries: { addr: string; value: string }[]): Record<string, string> {
  const m: Record<string, string> = {};
  for (const e of entries) m[e.addr] = e.value;
  return m;
}

export default function ICFGViewer() {
  const [graph, setGraph] = useState<GraphJSON | null>(null);
  const [err, setErr] = useState<string | null>(null);

  const [rfInstance, setRfInstance] = useState<ReactFlowInstance | null>(null);

  const wsRef = useRef<WebSocket | null>(null);
  const wsConnIdRef = useRef<number>(0);
  const [wsConnId, setWsConnId] = useState<number>(0);
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

  const [envCollapsed, setEnvCollapsed] = useState<boolean>(() => loadBool(UI_ENV_COLLAPSED_KEY, false));
  const [stateCollapsed, setStateCollapsed] = useState<boolean>(() => loadBool(UI_STATE_COLLAPSED_KEY, false));

  const defaultRightPanelW = Math.round(680 * UI_SCALE);
  const [rightPanelW, setRightPanelW] = useState<number>(() => {
    const stored = loadNum(UI_RIGHT_PANEL_W_KEY, defaultRightPanelW);
    const migrated = stored > 1000 ? Math.round(stored * 0.67) : stored;

    const minW = Math.round(380 * UI_SCALE);
    const maxW = Math.max(minW, Math.round(window.innerWidth * 0.65));
    return clamp(migrated, minW, maxW);
  });

  const [envQuery, setEnvQuery] = useState<string>("");
  const [stateAddrQuery, setStateAddrQuery] = useState<string>("");

  const [highlightAddr, setHighlightAddr] = useState<string>("");
  const highlightTimerRef = useRef<number | null>(null);
  const stateRowRefs = useRef<Record<string, HTMLTableRowElement | null>>({});

  const resizingRef = useRef<boolean>(false);
  const resizeStartXRef = useRef<number>(0);
  const resizeStartWRef = useRef<number>(0);

  const prevStateRef = useRef<Record<string, Record<string, string>>>({});
  const [changedAddrsByKey, setChangedAddrsByKey] = useState<Record<string, Record<string, boolean>>>({});

  const msgSerialRef = useRef<number>(0);
  const latestMsgSerialRef = useRef<number>(0);

  const restartEpochRef = useRef<number>(0);
  const [restartEpoch, setRestartEpoch] = useState<number>(0);
  const pendingRestartRef = useRef<boolean>(false);

  const [restartProbe, setRestartProbe] = useState<string>("(n/a)");
  const [droppedWhileRestart, setDroppedWhileRestart] = useState<number>(0);

  const cmdQueueRef = useRef<Array<"play" | "step">>([]);
  const inFlightRef = useRef<null | "play" | "step" | "restart">(null);

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

  const sendWs = useCallback((connId: number, obj: any) => {
    const ws = wsRef.current;
    if (!ws || ws.readyState !== WebSocket.OPEN) {
      dbg(`[ws#${connId}] send dropped (not open)`, obj);
      return false;
    }
    const s = JSON.stringify(obj);
    dbg(`[ws#${connId}] send`, s);
    ws.send(s);
    return true;
  }, []);

  const syncBpsToServer = useCallback(
    (connId: number) => {
      const m = loadBpMap();
      const bbs = Object.keys(m).filter((k) => m[k]);
      sendWs(connId, { cmd: "bp_sync", bbs });
    },
    [sendWs]
  );

  const resetUiForRestart = useCallback(() => {
    setWl([]);
    setCurrentBb("");
    setCurrentCtxt("");
    setCtxMap({});
    setSelBb("");
    setSelCtxt("");
    setSelEntries([]);
    setSelIsBot(false);
    setEnvItems([]);
    setSearchText("");
    setActiveMatchIdx(0);
    setEnvQuery("");
    setStateAddrQuery("");
    setHighlightAddr("");
    prevStateRef.current = {};
    setChangedAddrsByKey({});
  }, []);

  const updateDeltaForVisited = useCallback((bb: string, ctxt: string, contexts: StateResp["contexts"]) => {
  const hit = contexts.find((c) => c.ctxt === ctxt);
  if (!hit) return;

  const key = mkKey(bb, ctxt);
  const newMap = entriesToMap(hit.entries || []);

  const hasPrev = Object.prototype.hasOwnProperty.call(prevStateRef.current, key);

  // First time we ever see this (bb, ctxt): treat as baseline, show as normal (black)
  if (!hasPrev) {
    prevStateRef.current[key] = newMap;
    setChangedAddrsByKey((prev) => ({
      ...prev,
      [key]: {}, // no highlights on first snapshot
    }));
    return;
  }

  const oldMap = prevStateRef.current[key] ?? {};
  const changed: Record<string, boolean> = {};

  for (const addr of Object.keys(newMap)) {
    const nv = newMap[addr];
    const ov = oldMap[addr];
    if (ov !== nv) changed[addr] = true;
  }

  prevStateRef.current[key] = newMap;

  setChangedAddrsByKey((prev) => ({
    ...prev,
    [key]: changed,
  }));
}, []);

  const selectBestContext = useCallback((bb: string, contexts: StateResp["contexts"], preferredCtxt: string) => {
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
  }, []);

  async function fetchStatesForBb(bb: string, serial: number, connId: number): Promise<StateResp["contexts"] | null> {
    dbg(`[ws#${connId}] http GET /state`, bb, `serial=${serial} epoch=${restartEpochRef.current}`);
    try {
      const j = await fetchJson<StateResp>(`/state?bb=${encodeURIComponent(bb)}`);
      if (latestMsgSerialRef.current !== serial) {
        dbg(`[ws#${connId}] drop stale /state`, `serial=${serial} now=${latestMsgSerialRef.current}`);
        return null;
      }
      setCtxMap((prev) => ({ ...prev, [bb]: j.contexts }));
      return j.contexts;
    } catch (e) {
      dbg(`[ws#${connId}] /state failed`, String(e));
      return null;
    }
  }

  async function fetchAllStates(serial: number, connId: number): Promise<StateResp[] | null> {
    dbg(`[ws#${connId}] http GET /states`, `serial=${serial} epoch=${restartEpochRef.current}`);
    try {
      const j = await fetchJson<StatesResp>("/states");
      if (latestMsgSerialRef.current !== serial) {
        dbg(`[ws#${connId}] drop stale /states`, `serial=${serial} now=${latestMsgSerialRef.current}`);
        return null;
      }
      const items = Array.isArray(j.items) ? j.items : [];
      setCtxMap((prev) => {
        const next = { ...prev };
        for (const it of items) next[it.bb] = it.contexts;
        return next;
      });
      return items;
    } catch (e) {
      dbg(`[ws#${connId}] /states failed`, String(e));
      return null;
    }
  }

  async function fetchEnv(serial: number, connId: number): Promise<{ var: string; addr: string }[] | null> {
    dbg(`[ws#${connId}] http GET /env`, `serial=${serial} epoch=${restartEpochRef.current}`);
    try {
      const j = await fetchJson<EnvResp>("/env");
      if (latestMsgSerialRef.current !== serial) {
        dbg(`[ws#${connId}] drop stale /env`, `serial=${serial} now=${latestMsgSerialRef.current}`);
        return null;
      }
      setEnvItems(j.items || []);
      return j.items || [];
    } catch (e) {
      dbg(`[ws#${connId}] /env failed`, String(e));
      return null;
    }
  }

  async function runRestartProbe(serial: number, connId: number) {
    const items = await fetchAllStates(serial, connId);
    const env = await fetchEnv(serial, connId);
    if (!items || !env) return;

    const itemCount = items.length;
    const ctxCount = items.reduce((acc, it) => acc + (it.contexts?.length ?? 0), 0);

    const nonBotNonEmpty = items.flatMap((it) =>
      (it.contexts || [])
        .filter((c) => !c.is_bot && (c.entries?.length ?? 0) > 0)
        .map((c) => ({ bb: it.bb, ctxt: c.ctxt, n: c.entries.length }))
    );

    const top = nonBotNonEmpty
      .sort((a, b) => b.n - a.n)
      .slice(0, 8)
      .map((x) => `${x.bb}::${x.ctxt}(${x.n})`);

    const probe = `epoch=${restartEpochRef.current} items=${itemCount} ctx=${ctxCount} env=${env.length} nonBotNonEmpty=${nonBotNonEmpty.length} top=${JSON.stringify(
      top
    )}`;

    dbg(`[ws#${connId}] restart-probe`, probe);
    setRestartProbe(probe);
  }

  const pumpCmdQueue = useCallback(
    (connId: number) => {
      if (pendingRestartRef.current) return;
      const ws = wsRef.current;
      if (!ws || ws.readyState !== WebSocket.OPEN) return;
      if (inFlightRef.current !== null) return;

      const next = cmdQueueRef.current.shift();
      if (!next) return;

      inFlightRef.current = next;
      dbg(`[ws#${connId}] pump send`, next, `q=${cmdQueueRef.current.length} epoch=${restartEpochRef.current}`);
      sendWs(connId, { cmd: next });
    },
    [sendWs]
  );

  const sendCmd = useCallback(
    (cmd: "play" | "step" | "restart") => {
      const connId = wsConnIdRef.current;
      dbg(`[ws#${connId}] ui click`, cmd, `wsStatus=${wsStatus} epoch=${restartEpochRef.current}`);

      const ws = wsRef.current;
      if (!ws || ws.readyState !== WebSocket.OPEN) return;

      if (cmd === "restart") {
        cmdQueueRef.current = [];
        inFlightRef.current = "restart";

        pendingRestartRef.current = true;
        setDroppedWhileRestart(0);

        restartEpochRef.current += 1;
        setRestartEpoch(restartEpochRef.current);

        resetUiForRestart();

        setRestartProbe("(pending restart ack...)");

        sendWs(connId, { cmd: "restart" });
        return;
      }

      if (pendingRestartRef.current) {
        dbg(`[ws#${connId}] ui ignored (pending restart)`, cmd);
        return;
      }

      cmdQueueRef.current.push(cmd);
      pumpCmdQueue(connId);
    },
    [pumpCmdQueue, resetUiForRestart, sendWs, wsStatus]
  );

  const setBreakpoint = useCallback(
    (bb: string, enabled: boolean) => {
      setBpMap((prev) => {
        const next = { ...prev, [bb]: enabled };
        saveBpMap(next);

        const connId = wsConnIdRef.current;
        sendWs(connId, { cmd: "bp_set", bb, enabled });

        return next;
      });
    },
    [sendWs]
  );

  useEffect(() => {
    let stopped = false;
    let reconnectTimer: number | null = null;
    let backoffMs = 250;

    const connect = () => {
      if (stopped) return;

      const connId = (wsConnIdRef.current = wsConnIdRef.current + 1);
      setWsConnId(connId);

      const proto = window.location.protocol === "https:" ? "wss" : "ws";
      const wsUrl = `${proto}://${window.location.host}/ws`;

      dbg(`[ws#${connId}] connect url=${wsUrl}`);
      setWsStatus("connecting");

      const ws = new WebSocket(wsUrl);
      wsRef.current = ws;

      ws.onopen = () => {
        dbg(`[ws#${connId}] onopen readyState=${ws.readyState}`);
        setWsStatus("connected");
        backoffMs = 250;

        inFlightRef.current = null;
        cmdQueueRef.current = [];

        pendingRestartRef.current = false;
        setDroppedWhileRestart(0);
        setRestartProbe("(n/a)");

        syncBpsToServer(connId);
      };

      ws.onerror = (ev) => {
        dbg(`[ws#${connId}] onerror`, ev);
      };

      ws.onclose = (ev) => {
        dbg(`[ws#${connId}] onclose code=${ev.code} reason=${ev.reason} wasClean=${ev.wasClean}`);
        setWsStatus("disconnected");
        if (wsRef.current === ws) wsRef.current = null;

        inFlightRef.current = null;

        if (stopped) return;
        if (reconnectTimer !== null) return;

        const delay = backoffMs;
        backoffMs = Math.min(backoffMs * 2, 3000);

        dbg(`[ws#${connId}] schedule reconnect in ${delay}ms`);
        reconnectTimer = window.setTimeout(() => {
          reconnectTimer = null;
          connect();
        }, delay);
      };

      ws.onmessage = async (ev) => {
        const raw = typeof ev.data === "string" ? ev.data : "";
        dbg(`[ws#${connId}] onmessage len=${raw.length}`);

        let msg: WLMsg | null = null;
        try {
          msg = JSON.parse(raw) as WLMsg;
        } catch (e) {
          dbg(`[ws#${connId}] parse error`, String(e));
          return;
        }

        const mtype = (msg as any)?.type;
        dbg(`[ws#${connId}] msg.type=${mtype}`);

        if (mtype === "breakpoints") {
          const bmsg = msg as any as { type: "breakpoints"; bbs: string[] };
          const serverMap: Record<string, boolean> = {};
          for (const bb of bmsg.bbs || []) serverMap[bb] = true;
          saveBpMap(serverMap);
          setBpMap(serverMap);
          return;
        }

        const serial = (msgSerialRef.current += 1);
        latestMsgSerialRef.current = serial;

        const finishInflight = () => {
          if (inFlightRef.current !== null) {
            dbg(`[ws#${connId}] inflight done`, inFlightRef.current);
            inFlightRef.current = null;
          }
          pumpCmdQueue(connId);
        };

        if (mtype === "error") {
          const emsg = msg as any as { type: "error"; msg: string };
          dbg(`[ws#${connId}] server error`, emsg.msg);
          finishInflight();
          return;
        }

        if (mtype === "done") {
          const dmsg = msg as any as { type: "done"; ran?: number; reason?: string };
          dbg(`[ws#${connId}] done ran=${dmsg.ran ?? 0} reason=${dmsg.reason ?? ""} serial=${serial} epoch=${restartEpochRef.current}`);

          if (pendingRestartRef.current) {
            setDroppedWhileRestart((x) => x + 1);
            dbg(`[ws#${connId}] drop done (pending restart)`);
            finishInflight();
            return;
          }

          await fetchAllStates(serial, connId);
          await fetchEnv(serial, connId);
          if (latestMsgSerialRef.current === serial) setWl([]);

          finishInflight();
          return;
        }

        if (mtype === "worklist") {
          const wmsg = msg as WorklistMsg;
          dbg(
            `[ws#${connId}] worklist bb=${wmsg.bb} ctxt=${wmsg.ctxt} ran=${wmsg.ran ?? 0} reason=${wmsg.reason ?? ""} serial=${serial} epoch=${restartEpochRef.current}`
          );

          const isRestartAck = (wmsg.reason ?? "") === "restart";

          if (pendingRestartRef.current && !isRestartAck) {
            setDroppedWhileRestart((x) => x + 1);
            dbg(`[ws#${connId}] drop worklist (pending restart, not ack) reason=${wmsg.reason ?? ""}`);
            finishInflight();
            return;
          }

          setCurrentBb(wmsg.bb);
          setCurrentCtxt(wmsg.ctxt);
          setWl(wmsg.worklist);

          const contexts = await fetchStatesForBb(wmsg.bb, serial, connId);
          if (contexts) {
            updateDeltaForVisited(wmsg.bb, wmsg.ctxt, contexts);
            setSelBb((prev) => (prev ? prev : wmsg.bb));
            selectBestContext(wmsg.bb, contexts, wmsg.ctxt);
          }

          await fetchEnv(serial, connId);

          if (isRestartAck) {
            pendingRestartRef.current = false;
            dbg(`[ws#${connId}] restart ack received epoch=${restartEpochRef.current}`);

            syncBpsToServer(connId);
            await runRestartProbe(serial, connId);
          }

          finishInflight();
          return;
        }
      };
    };

    connect();

    return () => {
      stopped = true;
      if (reconnectTimer !== null) {
        window.clearTimeout(reconnectTimer);
        reconnectTimer = null;
      }
      const ws = wsRef.current;
      wsRef.current = null;
      try {
        ws?.close();
      } catch {}
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const onNodeClick = useCallback(
    async (_: any, node: any) => {
      const bb = node.id as string;
      setSelBb(bb);

      const serial = (msgSerialRef.current += 1);
      latestMsgSerialRef.current = serial;

      const connId = wsConnIdRef.current;

      try {
        const contexts = ctxMap[bb] ?? (await fetchStatesForBb(bb, serial, connId));
        if (!contexts) return;
        const preferred = bb === currentBb ? currentCtxt : contexts[0]?.ctxt ?? "";
        selectBestContext(bb, contexts, preferred);
      } catch {
        setSelCtxt("");
        setSelEntries([]);
        setSelIsBot(false);
      }
    },
    [ctxMap, currentBb, currentCtxt, selectBestContext]
  );

  const chooseContext = useCallback(
    (bb: string, ctxt: string) => {
      const lst = ctxMap[bb] ?? [];
      const hit = lst.find((x) => x.ctxt === ctxt);
      if (!hit) return;
      setSelBb(bb);
      setSelCtxt(hit.ctxt);
      setSelIsBot(hit.is_bot);
      setSelEntries(hit.entries);
    },
    [ctxMap]
  );

  const LEFT_PANEL_W = Math.round(430 * UI_SCALE);
  const M = 16;

  const centerNodeBetweenPanels = useCallback(
    (bbId: string, nodes: Node[]) => {
      if (!rfInstance) return;
      const node = nodes.find((n) => n.id === bbId);
      if (!node) return;

      const w = (node.width as number) || NODE_W;
      const h = (node.height as number) || NODE_H;
      const nodeCenterX = node.position.x + w / 2;
      const nodeCenterY = node.position.y + h / 2;

      const screenW = window.innerWidth;
      const screenH = window.innerHeight;

      const desiredScreenX = (LEFT_PANEL_W + M + (screenW - rightPanelW - M)) / 2;
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
    },
    [rfInstance, rightPanelW, LEFT_PANEL_W]
  );

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
              <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 10, marginBottom: 10 }}>
                <div style={{ fontSize: NODE_TITLE_FS, fontWeight: 700, wordBreak: "break-all", textAlign: "left", flex: 1 }}>
                  {labelText}
                </div>

                <label
                  style={{ display: "flex", alignItems: "center", gap: 8, fontSize: NODE_BODY_FS, userSelect: "none", whiteSpace: "nowrap" }}
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
  }, [graph, layoutInfo, currentBb, selBb, ctxMap, selCtxt, bpMap, chooseContext, setBreakpoint]);

  const matchedNodeIds = useMemo(() => {
    const q = normalizeQuery(searchText);
    if (!q) return [];
    return rf.nodes.filter((n) => nodeSearchText(n as Node<NodeData>).toLowerCase().includes(q)).map((n) => n.id);
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
    [rfInstance, rf.nodes, centerNodeBetweenPanels]
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
    if (!rfInstance) return;
    if (!graph) return;
    if (rf.nodes.length === 0) return;
    rfInstance.fitView({ padding: 0.12, minZoom: MIN_ZOOM, maxZoom: MAX_ZOOM });
  }, [rfInstance, !!graph, rf.nodes.length]);

  useEffect(() => {
    if (!rfInstance || !currentBb) return;
    centerNodeBetweenPanels(currentBb, rf.nodes);
  }, [rfInstance, currentBb, rf.nodes, centerNodeBetweenPanels]);

  useEffect(() => {
    if (!rfInstance || !selBb) return;
    centerNodeBetweenPanels(selBb, rf.nodes);
  }, [rfInstance, selBb, rf.nodes, centerNodeBetweenPanels]);

  const panelFont = Math.round(13 * UI_SCALE);
  const panelSmall = Math.round(12 * UI_SCALE);

  const MINIMAP_W = 360;
  const MINIMAP_H = 260;
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

  const filteredEnvItems = useMemo(() => {
    const q = normalizeQuery(envQuery);
    if (!q) return envItems;
    return envItems.filter((it) => it.var.toLowerCase().includes(q));
  }, [envItems, envQuery]);

  const filteredStateEntries = useMemo(() => {
    const q = normalizeQuery(stateAddrQuery);
    if (!q) return selEntries;
    return selEntries.filter((e) => e.addr.toLowerCase().includes(q));
  }, [selEntries, stateAddrQuery]);

  const focusStateAddr = useCallback(
    (addr: string) => {
      if (!addr) return;

      if (highlightTimerRef.current !== null) {
        window.clearTimeout(highlightTimerRef.current);
        highlightTimerRef.current = null;
      }

      setStateCollapsed(false);
      saveBool(UI_STATE_COLLAPSED_KEY, false);

      setStateAddrQuery(addr);
      setHighlightAddr(addr);

      window.setTimeout(() => {
        const row = stateRowRefs.current[addr];
        if (row) row.scrollIntoView({ behavior: "smooth", block: "center" });
      }, 60);

      highlightTimerRef.current = window.setTimeout(() => {
        setHighlightAddr("");
        highlightTimerRef.current = null;
      }, 2500);
    },
    [setStateCollapsed]
  );

  const toggleEnvCollapsed = () => {
    setEnvCollapsed((prev) => {
      const next = !prev;
      saveBool(UI_ENV_COLLAPSED_KEY, next);
      return next;
    });
  };

  const toggleStateCollapsed = () => {
    setStateCollapsed((prev) => {
      const next = !prev;
      saveBool(UI_STATE_COLLAPSED_KEY, next);
      return next;
    });
  };

  const startResizeRightPanel = (ev: React.MouseEvent) => {
    ev.preventDefault();
    ev.stopPropagation();
    resizingRef.current = true;
    resizeStartXRef.current = ev.clientX;
    resizeStartWRef.current = rightPanelW;
  };

  useEffect(() => {
    const onMove = (ev: MouseEvent) => {
      if (!resizingRef.current) return;
      const dx = resizeStartXRef.current - ev.clientX;
      const nextW = resizeStartWRef.current + dx;
      const minW = Math.round(420 * UI_SCALE);
      const maxW = Math.max(minW, window.innerWidth - LEFT_PANEL_W - M - 80);
      const clampedW = Math.max(minW, Math.min(maxW, nextW));
      setRightPanelW(clampedW);
    };

    const onUp = () => {
      if (!resizingRef.current) return;
      resizingRef.current = false;
      saveNum(UI_RIGHT_PANEL_W_KEY, rightPanelW);
    };

    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
    return () => {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
    };
  }, [rightPanelW, LEFT_PANEL_W]);

  if (!graph && !err) return <div style={{ padding: 16, fontFamily: "monospace" }}>Loading /icfg...</div>;
  if (err)
    return (
      <div style={{ padding: 16, fontFamily: "monospace" }}>
        <div>Failed to load /icfg</div>
        <pre>{err}</pre>
      </div>
    );

  const changedKey = selBb && selCtxt ? mkKey(selBb, selCtxt) : "";
  const changedSet = changedKey ? (changedAddrsByKey[changedKey] ?? {}) : {};

  const disableControls = wsStatus !== "connected" || pendingRestartRef.current;

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
          width: Math.round(430 * UI_SCALE),
        }}
      >
        <div style={{ display: "flex", gap: 12, alignItems: "center", flexWrap: "wrap" }}>
          <button onClick={() => sendCmd("play")} disabled={disableControls} style={{ fontSize: panelFont }}>
            Play
          </button>
          <button onClick={() => sendCmd("step")} disabled={disableControls} style={{ fontSize: panelFont }}>
            Step
          </button>
          <button onClick={() => sendCmd("restart")} disabled={wsStatus !== "connected"} style={{ fontSize: panelFont }}>
            Restart
          </button>
          <span style={{ marginLeft: 10 }}>
            ws#{wsConnId}={wsStatus} epoch={restartEpoch} pendingRestart={String(pendingRestartRef.current)} dropped={droppedWhileRestart}
          </span>
        </div>

        <div style={{ marginTop: 10, fontSize: panelSmall, opacity: 0.85, whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
          Restart probe: {restartProbe}
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
                  <button style={{ fontSize: panelSmall }} onClick={() => setBreakpoint(x, false)} disabled={wsStatus !== "connected"}>
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

          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 8, marginTop: 8 }}>
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
          width: rightPanelW,
          maxHeight: "95vh",
          overflow: "auto",
          boxSizing: "border-box",
        }}
      >
        <div
          onMouseDown={startResizeRightPanel}
          title="Resize"
          style={{
            position: "absolute",
            left: -6,
            top: 0,
            bottom: 0,
            width: 12,
            cursor: "col-resize",
            zIndex: 10000,
          }}
        />

        <div style={{ display: "flex", flexDirection: "column", gap: Math.round(12 * UI_SCALE) }}>
          <div style={{ border: "1px solid #e5e7eb", borderRadius: 10, padding: Math.round(10 * UI_SCALE) }}>
            <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 10 }}>
              <div style={{ fontWeight: 700 }}>ENV (var → addr)</div>
              <button type="button" onClick={toggleEnvCollapsed} style={{ fontSize: panelSmall }}>
                {envCollapsed ? "Expand" : "Collapse"}
              </button>
            </div>

            {!envCollapsed && (
              <>
                <input
                  value={envQuery}
                  onChange={(e) => setEnvQuery(e.target.value)}
                  placeholder="Filter by var"
                  style={{
                    width: "100%",
                    boxSizing: "border-box",
                    marginTop: 10,
                    padding: "8px 10px",
                    borderRadius: 8,
                    border: "1px solid rgba(0,0,0,0.2)",
                    fontSize: panelSmall,
                    fontFamily: "monospace",
                  }}
                />

                <div style={{ marginTop: 10, fontSize: panelSmall }}>
                  {filteredEnvItems.length === 0 ? (
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
                        {filteredEnvItems.map((it, i) => (
                          <tr key={`${it.var}:${it.addr}:${i}`}>
                            <td
                              style={{
                                verticalAlign: "top",
                                padding: "8px 10px 8px 0",
                                borderBottom: "1px solid #f3f4f6",
                                maxWidth: 260,
                                overflow: "hidden",
                                textOverflow: "ellipsis",
                                whiteSpace: "nowrap",
                              }}
                              title={it.var}
                            >
                              {it.var}
                            </td>
                            <td style={{ verticalAlign: "top", padding: "8px 0", borderBottom: "1px solid #f3f4f6" }}>
                              <button
                                type="button"
                                onClick={() => focusStateAddr(it.addr)}
                                style={{
                                  fontFamily: "monospace",
                                  fontSize: panelSmall,
                                  padding: 0,
                                  border: "none",
                                  background: "transparent",
                                  cursor: "pointer",
                                  color: "#2563eb",
                                  textAlign: "left",
                                  maxWidth: 360,
                                  overflow: "hidden",
                                  textOverflow: "ellipsis",
                                  whiteSpace: "nowrap",
                                }}
                                title={`Focus state addr: ${it.addr}`}
                              >
                                {it.addr}
                              </button>
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  )}
                </div>
              </>
            )}
          </div>

          <div style={{ border: "1px solid #e5e7eb", borderRadius: 10, padding: Math.round(10 * UI_SCALE) }}>
            <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 10 }}>
              <div style={{ fontWeight: 700 }}>Selected State</div>
              <button type="button" onClick={toggleStateCollapsed} style={{ fontSize: panelSmall }}>
                {stateCollapsed ? "Expand" : "Collapse"}
              </button>
            </div>

            {!stateCollapsed && (
              <>
                <div style={{ marginTop: 10, fontSize: panelSmall, wordBreak: "break-word" }}>
                  bb: {selBb || "(none)"} <br />
                  ctxt: {selCtxt || "(none)"} <br />
                  bot: {selBb ? String(selIsBot) : "(n/a)"}
                </div>

                <input
                  value={stateAddrQuery}
                  onChange={(e) => setStateAddrQuery(e.target.value)}
                  placeholder="Filter by addr"
                  style={{
                    width: "100%",
                    boxSizing: "border-box",
                    marginTop: 10,
                    padding: "8px 10px",
                    borderRadius: 8,
                    border: "1px solid rgba(0,0,0,0.2)",
                    fontSize: panelSmall,
                    fontFamily: "monospace",
                  }}
                />

                <div style={{ marginTop: 12, fontSize: panelSmall }}>
                  {filteredStateEntries.length === 0 ? (
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
                        {filteredStateEntries.map((e, i) => {
                          const isHL = highlightAddr !== "" && e.addr === highlightAddr;
                          const isChanged = !!changedSet[e.addr];

                          const changedStyle: React.CSSProperties = isChanged ? { color: "#dc2626", fontWeight: 700 } : {};

                          return (
                            <tr
                              key={`${e.addr}:${i}`}
                              ref={(el) => {
                                stateRowRefs.current[e.addr] = el;
                              }}
                              style={{
                                background: isHL ? "rgba(245,158,11,0.18)" : "transparent",
                                outline: isHL ? "2px solid rgba(245,158,11,0.55)" : "none",
                                outlineOffset: -2,
                              }}
                            >
                              <td
                                style={{
                                  verticalAlign: "top",
                                  padding: "8px 10px 8px 0",
                                  borderBottom: "1px solid #f3f4f6",
                                  maxWidth: 360,
                                  overflow: "hidden",
                                  textOverflow: "ellipsis",
                                  whiteSpace: "nowrap",
                                  ...changedStyle,
                                }}
                                title={e.addr}
                              >
                                {e.addr}
                              </td>
                              <td style={{ verticalAlign: "top", padding: "8px 0", borderBottom: "1px solid #f3f4f6" }}>
                                <div
                                  style={{
                                    maxWidth: 520,
                                    overflow: "hidden",
                                    textOverflow: "ellipsis",
                                    whiteSpace: "nowrap",
                                    ...changedStyle,
                                  }}
                                  title={e.value}
                                >
                                  {e.value}
                                </div>
                              </td>
                            </tr>
                          );
                        })}
                      </tbody>
                    </table>
                  )}
                </div>
              </>
            )}
          </div>
        </div>
      </div>

      <ReactFlow
        nodes={nodesForRender}
        edges={rf.edges}
        onInit={setRfInstance}
        onNodeClick={onNodeClick}
        minZoom={MIN_ZOOM}
        maxZoom={MAX_ZOOM}
      >
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
