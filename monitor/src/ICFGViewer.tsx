import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import ReactFlow, { Background, Controls, MiniMap, Position } from "reactflow";
import type { Edge, Node, ReactFlowInstance, Viewport } from "reactflow";
import "reactflow/dist/style.css";
import dagre from "dagre";

type NodeJSON = {
  id: string;
  label?: string;
  instrs?: string[];
  members?: string[];
};

type EdgeJSON = {
  id: string;
  source: string;
  target: string;
  kind?: "call" | "fallback" | "ret" | "intra";
};

type GraphJSON = { nodes: NodeJSON[]; edges: EdgeJSON[] };

type FunctionItem = {
  name: string;
  block_count: number;
  entry: string;
  exit: string;
};

type FunctionsResp = { items: FunctionItem[] };

type BlockIndexItem = { id: string; func: string };
type BlockIndexResp = { items: BlockIndexItem[] };

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

type LayoutInfo = {
  pos: Record<string, { x: number; y: number }>;
  edges: { id: string; source: string; target: string; kind: "call" | "fallback" | "ret" | "intra" }[];
};

type MainTab = "cfg" | "code" | "callgraph";
type CallGraphMode = "neighbors" | "scc" | "full";
type SearchHit = { id: string; func: string; scope: "cfg" | "global" | "callgraph" };

type CallGraphMetaResp = {
  focus: string;
  too_big: boolean;
  pred_count: number;
  succ_count: number;
  node_estimate: number;
  reason: string;
};

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
  console.log(`[${ts()}]`, ...args);
}

async function fetchJson<T>(url: string): Promise<T> {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`HTTP ${r.status}`);
  return (await r.json()) as T;
}

function clamp(n: number, lo: number, hi: number) {
  return Math.max(lo, Math.min(hi, n));
}

function normalizeQuery(s: string): string {
  return s.trim().toLowerCase();
}

function bbToFunc(bb: string): string {
  const i = bb.indexOf("#");
  return i >= 0 ? bb.slice(0, i) : bb;
}

function mkKey(bb: string, ctxt: string): string {
  return `${bb}||${ctxt}`;
}

function entriesToMap(entries: { addr: string; value: string }[]): Record<string, string> {
  const m: Record<string, string> = {};
  for (const e of entries) m[e.addr] = e.value;
  return m;
}

function nodeSearchText(n: Node<NodeData>): string {
  const raw = n.data?.rawLabel ?? "";
  return `${n.id} ${raw}`.trim();
}

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

function loadStr(key: string, def: string): string {
  try {
    const s = localStorage.getItem(key);
    return s === null ? def : s;
  } catch {
    return def;
  }
}

function saveStr(key: string, v: string) {
  try {
    localStorage.setItem(key, v);
  } catch {}
}

function computeLayout(
  graph: GraphJSON,
  {
    nodeW,
    nodeH,
    rankdir,
  }: {
    nodeW: number;
    nodeH: number;
    rankdir: "TB" | "LR";
  }
): LayoutInfo {
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

  const baseNodesep = Math.round(clamp(nodeW * 0.06, 14, 64));
  const baseRanksep = Math.round(clamp(nodeH * 0.1, 24, 120));

  const nodesep = Math.round(clamp(baseNodesep * tightFactor, 10, 80));
  const ranksep = Math.round(clamp(baseRanksep * tightFactor, 18, 160));

  const g = new dagre.graphlib.Graph();
  g.setDefaultEdgeLabel(() => ({}));

  g.setGraph({
    rankdir,
    nodesep,
    ranksep,
    marginx: 18,
    marginy: 18,
    ranker: rankdir === "TB" ? "tight-tree" : "network-simplex",
  });

  for (const n of nodes) g.setNode(n.id, { width: nodeW, height: nodeH });
  for (const e of edges) g.setEdge(e.source, e.target, { minlen: 1 });

  dagre.layout(g);

  const pos: Record<string, { x: number; y: number }> = {};
  for (const n of nodes) {
    const p = g.node(n.id);
    pos[n.id] = { x: p.x - nodeW / 2, y: p.y - nodeH / 2 };
  }

  return { pos, edges };
}

function callGraphApproveKey(func: string, mode: CallGraphMode): string {
  return `${mode}@@${func}`;
}

const NODE_W = 420;
const CALL_NODE_W = 280;

const UI_SCALE = 1.0;
const NODE_SCALE = 1.0;

const NODE_TITLE_FS = Math.round(15 * NODE_SCALE);
const NODE_BODY_FS = Math.round(13 * NODE_SCALE);
const NODE_LINE_H = Math.round(16 * NODE_SCALE);

const CTX_BTN_FS = Math.round(12 * NODE_SCALE * 1.15);
const CTX_BTN_PAD_Y = Math.round(4 * NODE_SCALE * 1.2);
const CTX_BTN_PAD_X = Math.round(10 * NODE_SCALE * 1.2);

const BP_LS_KEY = "calli_breakpoints_v1";

const UI_LEFT_PANEL_W_KEY = "calli_ui_left_panel_w_v1";
const UI_RIGHT_PANEL_W_KEY = "calli_ui_right_panel_w_v1";
const UI_ENV_COLLAPSED_KEY = "calli_ui_env_collapsed_v1";
const UI_STATE_COLLAPSED_KEY = "calli_ui_state_collapsed_v1";

const UI_MAIN_TAB_KEY = "calli_ui_main_tab_v5";
const UI_ACTIVE_FUNC_KEY = "calli_ui_active_func_v5";
const UI_FOLLOW_CURRENT_FUNC_KEY = "calli_ui_follow_current_func_v5";
const UI_CODE_FOLLOW_CURRENT_KEY = "calli_ui_code_follow_current_v5";
const UI_CALLGRAPH_MODE_KEY = "calli_ui_callgraph_mode_v5";

const INSTR_BOX_H = Math.round(300 * NODE_SCALE);
const NODE_HEADER_H = Math.round(54 * NODE_SCALE);
const NODE_CTX_H = Math.round(64 * NODE_SCALE);
const NODE_PAD_H = Math.round(34 * NODE_SCALE);
const NODE_SAFETY_H = Math.round(18 * NODE_SCALE);
const NODE_H = NODE_HEADER_H + NODE_CTX_H + NODE_PAD_H + INSTR_BOX_H + NODE_SAFETY_H;

const CALL_NODE_H = 84;

const MIN_ZOOM = 0.02;
const MAX_ZOOM = 2.0;

const LEFT_PANEL_LEFT = 8;
const RIGHT_PANEL_RIGHT = 8;
const PANEL_GAP = 16;

export default function ICFGViewer() {
  const [functions, setFunctions] = useState<FunctionItem[]>([]);
  const [blockIndex, setBlockIndex] = useState<BlockIndexItem[]>([]);

  const [callGraph, setCallGraph] = useState<GraphJSON | null>(null);
  const [cfg, setCfg] = useState<GraphJSON | null>(null);
  const [err, setErr] = useState<string | null>(null);

  const [mainTab, setMainTab] = useState<MainTab>(() => {
    const v = loadStr(UI_MAIN_TAB_KEY, "cfg");
    return v === "code" || v === "callgraph" ? v : "cfg";
  });

  const [activeFunc, setActiveFunc] = useState<string>(() => loadStr(UI_ACTIVE_FUNC_KEY, ""));
  const [followCurrentFunc, setFollowCurrentFunc] = useState<boolean>(() => loadBool(UI_FOLLOW_CURRENT_FUNC_KEY, true));
  const [followCurrentCode, setFollowCurrentCode] = useState<boolean>(() => loadBool(UI_CODE_FOLLOW_CURRENT_KEY, true));

  const [callGraphMode, setCallGraphMode] = useState<CallGraphMode>(() => {
    const v = loadStr(UI_CALLGRAPH_MODE_KEY, "neighbors");
    return v === "scc" || v === "full" ? v : "neighbors";
  });

  const [callGraphLoading, setCallGraphLoading] = useState<boolean>(false);
  const [callGraphConfirm, setCallGraphConfirm] = useState<null | {
    func: string;
    mode: CallGraphMode;
    pred_count: number;
    succ_count: number;
    node_estimate: number;
    reason: string;
  }>(null);
  const [callGraphApprovedKey, setCallGraphApprovedKey] = useState<string>("");

  const activeFuncRef = useRef(activeFunc);
  const followCurrentFuncRef = useRef(followCurrentFunc);
  const ctxMapRef = useRef<Record<string, StateResp["contexts"]>>({});

  useEffect(() => {
    activeFuncRef.current = activeFunc;
  }, [activeFunc]);

  useEffect(() => {
    followCurrentFuncRef.current = followCurrentFunc;
  }, [followCurrentFunc]);

  const [rfInstance, setRfInstance] = useState<ReactFlowInstance | null>(null);

  const cfgViewportRef = useRef<Viewport | null>(null);
  const callGraphViewportRef = useRef<Viewport | null>(null);
  const lastCfgCenteredFuncRef = useRef<string>("");

  const lastAutoCurrentKeyRef = useRef<string>("");
  const prevAutoCurrentFuncRef = useRef<string>("");

  const handleCfgInit = useCallback((inst: ReactFlowInstance) => {
    setRfInstance(inst);

    requestAnimationFrame(() => {
      const saved = cfgViewportRef.current;
      if (!saved) return;
      (inst as any).setViewport(saved, { duration: 0 });
    });
  }, []);

  const handleCallGraphInit = useCallback((inst: ReactFlowInstance) => {
    setRfInstance(inst);

    requestAnimationFrame(() => {
      const saved = callGraphViewportRef.current;
      if (!saved) return;
      (inst as any).setViewport(saved, { duration: 0 });
    });
  }, []);

  const handleCfgMoveEnd = useCallback((_event: any, viewport: Viewport) => {
    cfgViewportRef.current = viewport;
  }, []);

  const handleCallGraphMoveEnd = useCallback((_event: any, viewport: Viewport) => {
    callGraphViewportRef.current = viewport;
  }, []);

  const wsRef = useRef<WebSocket | null>(null);
  const wsConnIdRef = useRef<number>(0);
  const [wsStatus, setWsStatus] = useState<"disconnected" | "connecting" | "connected">("connecting");

  const [wl, setWl] = useState<string[]>([]);
  const [currentBb, setCurrentBb] = useState<string>("");
  const [currentCtxt, setCurrentCtxt] = useState<string>("");

  const [ctxMap, setCtxMap] = useState<Record<string, StateResp["contexts"]>>({});
  useEffect(() => {
    ctxMapRef.current = ctxMap;
  }, [ctxMap]);

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

  const defaultLeftPanelW = Math.round(430 * UI_SCALE);
  const [leftPanelW, setLeftPanelW] = useState<number>(() => {
    const stored = loadNum(UI_LEFT_PANEL_W_KEY, defaultLeftPanelW);
    const minW = Math.round(320 * UI_SCALE);
    const maxW = Math.max(minW, Math.round(window.innerWidth * 0.55));
    return clamp(stored, minW, maxW);
  });

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

  const resizingRightRef = useRef<boolean>(false);
  const rightResizeStartXRef = useRef<number>(0);
  const rightResizeStartWRef = useRef<number>(0);

  const resizingLeftRef = useRef<boolean>(false);
  const leftResizeStartXRef = useRef<number>(0);
  const leftResizeStartWRef = useRef<number>(0);

  const prevStateRef = useRef<Record<string, Record<string, string>>>({});
  const [changedAddrsByKey, setChangedAddrsByKey] = useState<Record<string, Record<string, boolean>>>({});

  const msgSerialRef = useRef<number>(0);
  const latestMsgSerialRef = useRef<number>(0);

  const restartEpochRef = useRef<number>(0);
  const pendingRestartRef = useRef<boolean>(false);

  const [restartProbe, setRestartProbe] = useState<string>("(n/a)");
  useEffect(() => {
    if (!DEBUG) return;
    if (!restartProbe || restartProbe === "(n/a)") return;
    console.log("[restart-probe]", restartProbe);
  }, [restartProbe]);

  const cmdQueueRef = useRef<Array<"play" | "step">>([]);
  const inFlightRef = useRef<null | "play" | "step" | "restart">(null);

  const codeBbRefs = useRef<Record<string, HTMLDivElement | null>>({});
  const pendingJumpRef = useRef<{ bb: string } | null>(null);
  const pendingSearchJumpRef = useRef<{ id: string; func: string } | null>(null);

  useEffect(() => {
    saveStr(UI_MAIN_TAB_KEY, mainTab);
  }, [mainTab]);

  useEffect(() => {
    saveStr(UI_ACTIVE_FUNC_KEY, activeFunc);
  }, [activeFunc]);

  useEffect(() => {
    saveBool(UI_FOLLOW_CURRENT_FUNC_KEY, followCurrentFunc);
  }, [followCurrentFunc]);

  useEffect(() => {
    saveBool(UI_CODE_FOLLOW_CURRENT_KEY, followCurrentCode);
  }, [followCurrentCode]);

  useEffect(() => {
    saveStr(UI_CALLGRAPH_MODE_KEY, callGraphMode);
  }, [callGraphMode]);

  useEffect(() => {
    let cancelled = false;

    Promise.all([fetchJson<FunctionsResp>("/functions"), fetchJson<BlockIndexResp>("/block_index")])
      .then(([fns, bi]) => {
        if (cancelled) return;
        setFunctions(fns.items || []);
        setBlockIndex(bi.items || []);
        setErr(null);
      })
      .catch((e) => {
        if (cancelled) return;
        setErr(String(e));
        setFunctions([]);
        setBlockIndex([]);
      });

    return () => {
      cancelled = true;
    };
  }, []);

  useEffect(() => {
    if (functions.length === 0) return;

    if (activeFunc && functions.some((f) => f.name === activeFunc)) return;

    const curFn = currentBb ? bbToFunc(currentBb) : "";
    if (curFn && functions.some((f) => f.name === curFn)) {
      setActiveFunc(curFn);
      return;
    }

    setActiveFunc(functions[0].name);
  }, [functions, activeFunc, currentBb]);

  useEffect(() => {
    if (!activeFunc) {
      setCfg(null);
      return;
    }

    let cancelled = false;

    fetchJson<GraphJSON>(`/cfg?func=${encodeURIComponent(activeFunc)}`)
      .then((j) => {
        if (cancelled) return;
        setCfg(j);
        setErr(null);
      })
      .catch((e) => {
        if (cancelled) return;
        setErr(String(e));
        setCfg(null);
      });

    return () => {
      cancelled = true;
    };
  }, [activeFunc]);

  useEffect(() => {
    if (mainTab !== "callgraph") return;

    let cancelled = false;
    const controller = new AbortController();

    const run = async () => {
      try {
        if (callGraphMode === "neighbors" && activeFunc) {
          const approveKey = callGraphApproveKey(activeFunc, callGraphMode);

          const meta = await fetchJson<CallGraphMetaResp>(
            `/callgraph_meta?mode=neighbors&func=${encodeURIComponent(activeFunc)}`
          );

          if (cancelled) return;

          if (meta.too_big && callGraphApprovedKey !== approveKey) {
            setCallGraph(null);
            setCallGraphLoading(false);
            setCallGraphConfirm({
              func: activeFunc,
              mode: callGraphMode,
              pred_count: meta.pred_count,
              succ_count: meta.succ_count,
              node_estimate: meta.node_estimate,
              reason: meta.reason,
            });
            return;
          }
        }

        setCallGraphConfirm(null);
        setCallGraphLoading(true);

        let url = `/callgraph?mode=${callGraphMode}`;
        if (callGraphMode === "neighbors" && activeFunc) {
          url += `&func=${encodeURIComponent(activeFunc)}`;
        }

        const r = await fetch(url, { signal: controller.signal });
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        const j = (await r.json()) as GraphJSON;

        if (cancelled) return;
        setCallGraph(j);
        setErr(null);
      } catch (e: any) {
        if (cancelled) return;
        if (e?.name === "AbortError") return;
        setErr(String(e));
        setCallGraph(null);
      } finally {
        if (!cancelled) setCallGraphLoading(false);
      }
    };

    run();

    return () => {
      cancelled = true;
      controller.abort();
    };
  }, [mainTab, callGraphMode, activeFunc, callGraphApprovedKey]);

  useEffect(() => {
    if (mainTab !== "callgraph") {
      setCallGraph(null);
      setCallGraphLoading(false);
      setCallGraphConfirm(null);
    }
  }, [mainTab]);

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
    lastCfgCenteredFuncRef.current = "";
    lastAutoCurrentKeyRef.current = "";
    prevAutoCurrentFuncRef.current = "";
  }, []);

  const updateDeltaForVisited = useCallback((bb: string, ctxt: string, contexts: StateResp["contexts"]) => {
    const hit = contexts.find((c) => c.ctxt === ctxt);
    if (!hit) return;

    const key = mkKey(bb, ctxt);
    const newMap = entriesToMap(hit.entries || []);

    const hasPrev = Object.prototype.hasOwnProperty.call(prevStateRef.current, key);

    if (!hasPrev) {
      prevStateRef.current[key] = newMap;
      setChangedAddrsByKey((prev) => ({
        ...prev,
        [key]: {},
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

  async function fetchFunctionStates(func: string, serial: number, connId: number): Promise<StateResp[] | null> {
    dbg(`[ws#${connId}] http GET /function_states`, func, `serial=${serial} epoch=${restartEpochRef.current}`);
    try {
      const j = await fetchJson<StatesResp>(`/function_states?func=${encodeURIComponent(func)}`);
      if (latestMsgSerialRef.current !== serial) {
        dbg(`[ws#${connId}] drop stale /function_states`, `serial=${serial} now=${latestMsgSerialRef.current}`);
        return null;
      }

      const items = Array.isArray(j.items) ? j.items : [];
      setCtxMap((prev) => {
        const next = { ...prev };
        for (const k of Object.keys(next)) {
          if (bbToFunc(k) === func) delete next[k];
        }
        for (const it of items) next[it.bb] = it.contexts;
        return next;
      });

      return items;
    } catch (e) {
      dbg(`[ws#${connId}] /function_states failed`, String(e));
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

  async function runRestartProbe(func: string, serial: number, connId: number) {
    const items = await fetchFunctionStates(func, serial, connId);
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

    const probe = `epoch=${restartEpochRef.current} func=${func} items=${itemCount} ctx=${ctxCount} env=${env.length} nonBotNonEmpty=${nonBotNonEmpty.length} top=${JSON.stringify(
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

        restartEpochRef.current += 1;

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
            dbg(`[ws#${connId}] drop done (pending restart)`);
            finishInflight();
            return;
          }

          const fn = currentBb ? bbToFunc(currentBb) : activeFuncRef.current;
          if (fn) await fetchFunctionStates(fn, serial, connId);
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
            dbg(`[ws#${connId}] drop worklist (pending restart, not ack) reason=${wmsg.reason ?? ""}`);
            finishInflight();
            return;
          }

          setCurrentBb(wmsg.bb);
          setCurrentCtxt(wmsg.ctxt);
          setWl(wmsg.worklist);

          const fn = bbToFunc(wmsg.bb);
          if (followCurrentFuncRef.current && fn !== activeFuncRef.current) {
            setActiveFunc(fn);
          }

          const funcItems = await fetchFunctionStates(fn, serial, connId);

          let contexts: StateResp["contexts"] | null =
            funcItems?.find((x) => x.bb === wmsg.bb)?.contexts ??
            ctxMapRef.current[wmsg.bb] ??
            null;

          if (!contexts) {
            contexts = await fetchStatesForBb(wmsg.bb, serial, connId);
          }

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
            await runRestartProbe(fn || activeFuncRef.current || "", serial, connId);
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
  }, [pumpCmdQueue, resetUiForRestart, selectBestContext, sendWs, syncBpsToServer, updateDeltaForVisited]);

  const onCfgNodeClick = useCallback(
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

  const onCallGraphNodeClick = useCallback((_evt: any, node: any) => {
    const fn = String(node.id ?? "");
    if (!fn) return;
    setActiveFunc(fn);
  }, []);

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

  const centerNodeBetweenPanels = useCallback(
    (
      scope: "cfg" | "callgraph",
      id: string,
      nodes: Node[],
      defaultW: number,
      defaultH: number,
      animated: boolean = true
    ) => {
      if (!rfInstance) return;

      const node = nodes.find((n) => n.id === id);
      if (!node) return;

      const w = (node.width as number) || defaultW;
      const h = (node.height as number) || defaultH;
      const nodeCenterX = node.position.x + w / 2;
      const nodeCenterY = node.position.y + h / 2;

      const screenW = window.innerWidth;
      const screenH = window.innerHeight;

      const leftBound = LEFT_PANEL_LEFT + leftPanelW + PANEL_GAP;
      const rightBound = screenW - RIGHT_PANEL_RIGHT - rightPanelW - PANEL_GAP;

      const desiredScreenX = (leftBound + rightBound) / 2;
      const desiredScreenY = screenH / 2;

      const vp: Viewport =
        (rfInstance as any).getViewport ? (rfInstance as any).getViewport() : { x: 0, y: 0, zoom: 1 };

      const zoom = vp.zoom ?? 1;

      const nextViewport: Viewport = {
        x: desiredScreenX - nodeCenterX * zoom,
        y: desiredScreenY - nodeCenterY * zoom,
        zoom,
      };

      if (scope === "cfg") cfgViewportRef.current = nextViewport;
      else callGraphViewportRef.current = nextViewport;

      (rfInstance as any).setViewport(nextViewport, { duration: animated ? 250 : 0 });
    },
    [rfInstance, rightPanelW, leftPanelW]
  );

  const cfgLayoutInfo = useMemo<LayoutInfo | null>(() => {
    if (!cfg) return null;
    return computeLayout(cfg, { nodeW: NODE_W, nodeH: NODE_H, rankdir: "TB" });
  }, [cfg]);

  const callGraphLayoutInfo = useMemo<LayoutInfo | null>(() => {
    if (!callGraph) return null;
    return computeLayout(callGraph, { nodeW: CALL_NODE_W, nodeH: CALL_NODE_H, rankdir: "LR" });
  }, [callGraph]);

  const functionMetaMap = useMemo(() => {
    const m: Record<string, FunctionItem> = {};
    for (const f of functions) m[f.name] = f;
    return m;
  }, [functions]);

  const currentFunc = currentBb ? bbToFunc(currentBb) : "";

  const cfgRf = useMemo(() => {
    if (!cfg || !cfgLayoutInfo) return { nodes: [] as Node<NodeData>[], edges: [] as Edge[] };

    const nodes: Node<NodeData>[] = (cfg.nodes || []).map((n) => {
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

      const p = cfgLayoutInfo.pos[n.id] ?? { x: 0, y: 0 };

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
          rawLabel: `${labelText}\n${instrs.join("\n")}`,
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

    const edges: Edge[] = cfgLayoutInfo.edges.map((e) => ({
      id: e.id,
      source: e.source,
      target: e.target,
      style: edgeStyle(e.kind),
    }));

    return { nodes, edges };
  }, [cfg, cfgLayoutInfo, currentBb, selBb, ctxMap, selCtxt, bpMap, chooseContext, setBreakpoint]);

  const callGraphRf = useMemo(() => {
    if (!callGraph || !callGraphLayoutInfo) return { nodes: [] as Node<NodeData>[], edges: [] as Edge[] };

    const nodes: Node<NodeData>[] = (callGraph.nodes || []).map((n) => {
      const labelText = n.label ?? n.id;
      const meta = functionMetaMap[n.id];
      const members = Array.isArray(n.members) ? n.members : [];

      const isCurrent = currentFunc !== "" && n.id === currentFunc;
      const isActive = activeFunc !== "" && n.id === activeFunc;

      const border = isCurrent ? "3px solid #ef4444" : isActive ? "3px solid #2563eb" : "1px solid #111827";
      const shadow = isCurrent
        ? "0 0 0 3px rgba(239,68,68,0.15)"
        : isActive
        ? "0 0 0 3px rgba(37,99,235,0.12)"
        : "none";

      return {
        id: n.id,
        data: {
          rawLabel: `${labelText} ${(members || []).join(" ")}`,
          label: (
            <div style={{ fontFamily: "monospace", textAlign: "left" }}>
              <div style={{ fontSize: 15, fontWeight: 800, wordBreak: "break-all" }}>{labelText}</div>
              <div style={{ marginTop: 6, fontSize: 12, opacity: 0.85 }}>
                {members.length > 0 ? `members: ${members.length}` : `blocks: ${meta?.block_count ?? "?"}`}
              </div>
              <div style={{ marginTop: 6, fontSize: 12, opacity: 0.85 }}>
                {isCurrent ? "current function" : isActive ? "active function" : ""}
              </div>
            </div>
          ),
        },
        position: callGraphLayoutInfo.pos[n.id] ?? { x: 0, y: 0 },
        width: CALL_NODE_W,
        height: CALL_NODE_H,
        style: {
          border,
          borderRadius: 10,
          padding: 12,
          width: CALL_NODE_W,
          background: "#ffffff",
          boxShadow: shadow,
        },
        targetPosition: Position.Left,
        sourcePosition: Position.Right,
      };
    });

    const edges: Edge[] = callGraphLayoutInfo.edges.map((e) => ({
      id: e.id,
      source: e.source,
      target: e.target,
      style: edgeStyle("call"),
    }));

    return { nodes, edges };
  }, [callGraph, callGraphLayoutInfo, functionMetaMap, currentFunc, activeFunc]);

  const searchHits = useMemo<SearchHit[]>(() => {
    const q = normalizeQuery(searchText);
    if (!q) return [];

    if (mainTab === "callgraph") {
      return callGraphRf.nodes
        .filter((n) => nodeSearchText(n as Node<NodeData>).toLowerCase().includes(q))
        .map((n) => ({ id: n.id, func: n.id, scope: "callgraph" as const }));
    }

    const localHits: SearchHit[] = cfgRf.nodes
      .filter((n) => nodeSearchText(n as Node<NodeData>).toLowerCase().includes(q))
      .map((n) => ({ id: n.id, func: bbToFunc(n.id), scope: "cfg" as const }));

    const globalHits: SearchHit[] = blockIndex
      .filter((it) => `${it.id} ${it.func}`.toLowerCase().includes(q))
      .map((it) => ({ id: it.id, func: it.func, scope: "global" as const }));

    const seen = new Set<string>();
    return [...localHits, ...globalHits].filter((h) => {
      const k = `${h.id}@@${h.func}`;
      if (seen.has(k)) return false;
      seen.add(k);
      return true;
    });
  }, [searchText, mainTab, callGraphRf.nodes, cfgRf.nodes, blockIndex]);

  useEffect(() => {
    setActiveMatchIdx(0);
  }, [searchText, mainTab, activeFunc]);

  const activeSearchHit = searchHits.length > 0 ? searchHits[Math.min(activeMatchIdx, searchHits.length - 1)] : null;

  const scrollToBbInCode = useCallback((bb: string) => {
    const el = codeBbRefs.current[bb];
    if (el) el.scrollIntoView({ behavior: "smooth", block: "center" });
  }, []);

  const focusVisibleId = useCallback(
    (id: string) => {
      if (!id) return;

      if (mainTab === "code") {
        scrollToBbInCode(id);
        return;
      }

      if (mainTab === "callgraph") {
        centerNodeBetweenPanels("callgraph", id, callGraphRf.nodes, CALL_NODE_W, CALL_NODE_H, true);
        return;
      }

      centerNodeBetweenPanels("cfg", id, cfgRf.nodes, NODE_W, NODE_H, true);
    },
    [mainTab, scrollToBbInCode, callGraphRf.nodes, cfgRf.nodes, centerNodeBetweenPanels]
  );

  const gotoSearchHit = useCallback(
    (hit: SearchHit) => {
      if (hit.scope === "callgraph") {
        setMainTab("callgraph");
        requestAnimationFrame(() => {
          focusVisibleId(hit.id);
        });
        return;
      }

      if (hit.func !== activeFunc) {
        pendingSearchJumpRef.current = { id: hit.id, func: hit.func };
        setActiveFunc(hit.func);
        setMainTab("cfg");
        return;
      }

      setMainTab("cfg");
      requestAnimationFrame(() => {
        focusVisibleId(hit.id);
      });
    },
    [activeFunc, focusVisibleId]
  );

  const gotoActiveMatch = useCallback(() => {
    if (!activeSearchHit) return;
    gotoSearchHit(activeSearchHit);
  }, [activeSearchHit, gotoSearchHit]);

  const gotoPrevMatch = useCallback(() => {
    if (searchHits.length === 0) return;
    setActiveMatchIdx((i) => {
      const ni = (i - 1 + searchHits.length) % searchHits.length;
      const hit = searchHits[ni];
      if (hit) gotoSearchHit(hit);
      return ni;
    });
  }, [searchHits, gotoSearchHit]);

  const gotoNextMatch = useCallback(() => {
    if (searchHits.length === 0) return;
    setActiveMatchIdx((i) => {
      const ni = (i + 1) % searchHits.length;
      const hit = searchHits[ni];
      if (hit) gotoSearchHit(hit);
      return ni;
    });
  }, [searchHits, gotoSearchHit]);

  useEffect(() => {
    const p = pendingSearchJumpRef.current;
    if (!p) return;
    if (p.func !== activeFunc) return;
    if (!(cfg?.nodes ?? []).some((n) => n.id === p.id)) return;

    pendingSearchJumpRef.current = null;
    setMainTab("cfg");
    requestAnimationFrame(() => {
      focusVisibleId(p.id);
    });
  }, [activeFunc, cfg, focusVisibleId]);

  const cfgNodesForRender = useMemo(() => {
    const q = normalizeQuery(searchText);
    if (!q || mainTab === "callgraph") return cfgRf.nodes;

    const matchSet = new Set(searchHits.filter((h) => h.scope !== "callgraph").map((h) => h.id));
    const activeId = activeSearchHit?.scope === "callgraph" ? null : activeSearchHit?.id ?? null;

    return cfgRf.nodes.map((n) => {
      if (!matchSet.has(n.id)) return n;

      const isActive = activeId === n.id;
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
  }, [cfgRf.nodes, searchText, mainTab, searchHits, activeSearchHit]);

  const callGraphNodesForRender = useMemo(() => {
    const q = normalizeQuery(searchText);
    if (!q || mainTab !== "callgraph") return callGraphRf.nodes;

    const matchSet = new Set(searchHits.filter((h) => h.scope === "callgraph").map((h) => h.id));
    const activeId = activeSearchHit?.scope === "callgraph" ? activeSearchHit.id : null;

    return callGraphRf.nodes.map((n) => {
      if (!matchSet.has(n.id)) return n;

      const isActive = activeId === n.id;
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
  }, [callGraphRf.nodes, searchText, mainTab, searchHits, activeSearchHit]);

  useEffect(() => {
    if (mainTab !== "cfg") return;
    if (!activeFunc) return;
    if (cfgRf.nodes.length === 0) return;
    if (lastCfgCenteredFuncRef.current === activeFunc) return;

    if (currentBb && bbToFunc(currentBb) === activeFunc) {
      lastCfgCenteredFuncRef.current = activeFunc;
      return;
    }

    if (selBb && bbToFunc(selBb) === activeFunc) {
      lastCfgCenteredFuncRef.current = activeFunc;
      return;
    }

    const targetId = functionMetaMap[activeFunc]?.entry || cfgRf.nodes[0]?.id;
    if (!targetId) return;
    if (!cfgRf.nodes.some((n) => n.id === targetId)) return;

    lastCfgCenteredFuncRef.current = activeFunc;
    centerNodeBetweenPanels("cfg", targetId, cfgRf.nodes, NODE_W, NODE_H, false);
  }, [mainTab, activeFunc, cfgRf.nodes, currentBb, selBb, functionMetaMap, centerNodeBetweenPanels]);

  useEffect(() => {
    if (!currentBb) return;
    if (!followCurrentCode) return;
    pendingJumpRef.current = { bb: currentBb };
  }, [currentBb, followCurrentCode]);

  useEffect(() => {
    if (mainTab !== "cfg") return;
    if (!currentBb) return;
    if (bbToFunc(currentBb) !== activeFunc) return;
    if (!cfgRf.nodes.some((n) => n.id === currentBb)) return;

    const currentKey = `${currentBb}@@${currentCtxt}`;
    if (lastAutoCurrentKeyRef.current === currentKey) return;

    const fn = bbToFunc(currentBb);
    const prevFn = prevAutoCurrentFuncRef.current;
    const crossFunctionStep = prevFn !== "" && prevFn !== fn;

    centerNodeBetweenPanels("cfg", currentBb, cfgRf.nodes, NODE_W, NODE_H, !crossFunctionStep);

    lastAutoCurrentKeyRef.current = currentKey;
    prevAutoCurrentFuncRef.current = fn;
  }, [mainTab, currentBb, currentCtxt, activeFunc, cfgRf.nodes, centerNodeBetweenPanels]);

  useEffect(() => {
    if (mainTab !== "callgraph") return;
    if (!activeFunc) return;
    centerNodeBetweenPanels("callgraph", activeFunc, callGraphRf.nodes, CALL_NODE_W, CALL_NODE_H, true);
  }, [mainTab, activeFunc, callGraphRf.nodes, centerNodeBetweenPanels]);

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

  const focusStateAddr = useCallback((addr: string) => {
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
  }, []);

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
    resizingRightRef.current = true;
    rightResizeStartXRef.current = ev.clientX;
    rightResizeStartWRef.current = rightPanelW;
  };

  const startResizeLeftPanel = (ev: React.MouseEvent) => {
    ev.preventDefault();
    ev.stopPropagation();
    resizingLeftRef.current = true;
    leftResizeStartXRef.current = ev.clientX;
    leftResizeStartWRef.current = leftPanelW;
  };

  useEffect(() => {
    const onMove = (ev: MouseEvent) => {
      if (resizingRightRef.current) {
        const dx = rightResizeStartXRef.current - ev.clientX;
        const nextW = rightResizeStartWRef.current + dx;

        const minW = Math.round(420 * UI_SCALE);
        const maxW = Math.max(minW, window.innerWidth - leftPanelW - LEFT_PANEL_LEFT - RIGHT_PANEL_RIGHT - PANEL_GAP * 2 - 80);

        const clampedW = Math.max(minW, Math.min(maxW, nextW));
        setRightPanelW(clampedW);
      }

      if (resizingLeftRef.current) {
        const dx = ev.clientX - leftResizeStartXRef.current;
        const nextW = leftResizeStartWRef.current + dx;

        const minW = Math.round(320 * UI_SCALE);
        const maxW = Math.max(minW, window.innerWidth - rightPanelW - LEFT_PANEL_LEFT - RIGHT_PANEL_RIGHT - PANEL_GAP * 2 - 80);

        const clampedW = Math.max(minW, Math.min(maxW, nextW));
        setLeftPanelW(clampedW);
      }
    };

    const onUp = () => {
      if (resizingRightRef.current) {
        resizingRightRef.current = false;
        saveNum(UI_RIGHT_PANEL_W_KEY, rightPanelW);
      }
      if (resizingLeftRef.current) {
        resizingLeftRef.current = false;
        saveNum(UI_LEFT_PANEL_W_KEY, leftPanelW);
      }
    };

    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
    return () => {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
    };
  }, [rightPanelW, leftPanelW]);

  const changedKey = selBb && selCtxt ? mkKey(selBb, selCtxt) : "";
  const changedSet = changedKey ? changedAddrsByKey[changedKey] ?? {} : {};

  const disableControls = wsStatus !== "connected" || pendingRestartRef.current;

  const activeFunctionMeta = activeFunc ? functionMetaMap[activeFunc] : undefined;

  const jumpToBb = useCallback((bb: string) => {
    if (!bb) return;

    const fn = bbToFunc(bb);
    pendingJumpRef.current = { bb };

    setActiveFunc(fn);
    setMainTab("code");
  }, []);

  useEffect(() => {
    const p = pendingJumpRef.current;
    if (!p) return;
    if (mainTab !== "code") return;
    if (bbToFunc(p.bb) !== activeFunc) return;

    pendingJumpRef.current = null;
    requestAnimationFrame(() => {
      scrollToBbInCode(p.bb);
    });
  }, [mainTab, activeFunc, scrollToBbInCode]);

  if (!cfg && !err && functions.length === 0) {
    return <div style={{ padding: 16, fontFamily: "monospace" }}>Loading viewer data...</div>;
  }

  if (err && functions.length === 0 && !cfg) {
    return (
      <div style={{ padding: 16, fontFamily: "monospace" }}>
        <div>Failed to load viewer data</div>
        <pre>{err}</pre>
      </div>
    );
  }

  const renderCodeView = () => {
    const padL = LEFT_PANEL_LEFT + leftPanelW + PANEL_GAP + 10;
    const padR = RIGHT_PANEL_RIGHT + rightPanelW + PANEL_GAP + 10;

    const bbs = (cfg?.nodes ?? []).slice();
    const pos = cfgLayoutInfo?.pos ?? {};

    bbs.sort((a, b) => {
      const pa = pos[a.id] ?? { x: 0, y: 0 };
      const pb = pos[b.id] ?? { x: 0, y: 0 };
      if (pa.y !== pb.y) return pa.y - pb.y;
      return pa.x - pb.x;
    });

    return (
      <div
        style={{
          position: "absolute",
          inset: 0,
          display: "flex",
          flexDirection: "column",
          paddingLeft: padL,
          paddingRight: padR,
          paddingTop: 4,
          paddingBottom: 8,
          boxSizing: "border-box",
          fontFamily: "monospace",
          background: "#ffffff",
        }}
      >
        <div
          style={{
            flex: "0 0 auto",
            background: "#ffffff",
            borderBottom: "1px solid #e5e7eb",
            paddingTop: 2,
            paddingBottom: 8,
            marginBottom: 8,
            zIndex: 5,
          }}
        >
          <div style={{ display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap" }}>
            <div style={{ fontWeight: 700 }}>Code</div>
            <div style={{ fontSize: panelSmall, opacity: 0.85 }}>function={activeFunc || "(none)"}</div>

            <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: panelSmall, userSelect: "none" }}>
              <input
                type="checkbox"
                checked={followCurrentCode}
                onChange={(e) => setFollowCurrentCode(e.target.checked)}
              />
              follow current block
            </label>

            <button
              type="button"
              onClick={() => jumpToBb(currentBb)}
              disabled={!currentBb}
              style={{ fontSize: panelSmall, padding: "6px 10px", borderRadius: 8 }}
            >
              Jump current
            </button>

            <button
              type="button"
              onClick={() => jumpToBb(selBb)}
              disabled={!selBb}
              style={{ fontSize: panelSmall, padding: "6px 10px", borderRadius: 8 }}
            >
              Jump selected
            </button>

            <button
              type="button"
              onClick={() => setMainTab("cfg")}
              style={{ fontSize: panelSmall, padding: "6px 10px", borderRadius: 8 }}
            >
              View CFG
            </button>

            <div style={{ fontSize: panelSmall, opacity: 0.75 }}>
              current={currentBb || "(none)"} selected={selBb || "(none)"}
            </div>
          </div>
        </div>

        <div
          style={{
            flex: 1,
            minHeight: 0,
            overflowY: "auto",
            paddingTop: 0,
            paddingBottom: 8,
          }}
        >
          {bbs.length === 0 ? (
            <div style={{ opacity: 0.75 }}>(no blocks for this function)</div>
          ) : (
            <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
              {bbs.map((n) => {
                const bb = n.id;
                const instrs = Array.isArray(n.instrs) ? n.instrs : [];
                const isCur = bb === currentBb && currentBb !== "";
                const isSel = bb === selBb && selBb !== "";
                const isBp = !!bpMap[bb];

                const headerBg = isCur ? "rgba(239,68,68,0.10)" : isSel ? "rgba(37,99,235,0.08)" : "rgba(0,0,0,0.02)";
                const headerBorder = isCur ? "2px solid #ef4444" : isSel ? "2px solid #2563eb" : "1px solid #e5e7eb";

                return (
                  <div
                    key={bb}
                    ref={(el) => {
                      codeBbRefs.current[bb] = el;
                    }}
                    style={{
                      border: headerBorder,
                      borderRadius: 10,
                      padding: 12,
                    }}
                    onClick={() => {
                      setSelBb(bb);
                      const serial = (msgSerialRef.current += 1);
                      latestMsgSerialRef.current = serial;
                      const connId = wsConnIdRef.current;
                      (async () => {
                        const contexts = ctxMap[bb] ?? (await fetchStatesForBb(bb, serial, connId));
                        if (!contexts) return;
                        const preferred = bb === currentBb ? currentCtxt : contexts[0]?.ctxt ?? "";
                        selectBestContext(bb, contexts, preferred);
                      })().catch(() => {});
                    }}
                  >
                    <div
                      style={{
                        display: "flex",
                        alignItems: "center",
                        justifyContent: "space-between",
                        gap: 10,
                        padding: "8px 10px",
                        borderRadius: 8,
                        background: headerBg,
                        marginBottom: 10,
                      }}
                    >
                      <div style={{ fontWeight: 800, wordBreak: "break-all" }}>{bb}</div>

                      <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
                        <div style={{ fontSize: panelSmall, opacity: 0.8 }}>{isCur ? "current" : isSel ? "selected" : ""}</div>

                        <label
                          style={{
                            display: "flex",
                            alignItems: "center",
                            gap: 8,
                            fontSize: panelSmall,
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
                              setBreakpoint(bb, ev.target.checked);
                            }}
                            onClick={(ev) => ev.stopPropagation()}
                          />
                          bp
                        </label>

                        <button
                          type="button"
                          onClick={(ev) => {
                            ev.stopPropagation();
                            setMainTab("cfg");
                            requestAnimationFrame(() => {
                              focusVisibleId(bb);
                            });
                          }}
                          style={{ fontSize: panelSmall, padding: "6px 10px", borderRadius: 8 }}
                        >
                          View CFG
                        </button>
                      </div>
                    </div>

                    <pre
                      style={{
                        margin: 0,
                        fontSize: 13,
                        lineHeight: "16px",
                        whiteSpace: "pre",
                        overflowX: "auto",
                        background: "#ffffff",
                      }}
                    >
                      {instrs.length > 0 ? instrs.join("\n") : "(no instruction data yet)"}
                    </pre>
                  </div>
                );
              })}
            </div>
          )}
        </div>
      </div>
    );
  };

  const startPanelResizeHandleStyle: React.CSSProperties = {
    position: "absolute",
    top: 0,
    bottom: 0,
    width: 12,
    cursor: "col-resize",
    zIndex: 10000,
  };

  return (
    <div style={{ width: "100vw", height: "100vh", position: "relative" }}>
      <div
        style={{
          position: "fixed",
          top: 8,
          left: LEFT_PANEL_LEFT,
          zIndex: 9999,
          background: "#fff",
          padding: Math.round(10 * UI_SCALE),
          border: "1px solid #ccc",
          borderRadius: 10,
          fontFamily: "monospace",
          fontSize: panelFont,
          width: leftPanelW,
          boxSizing: "border-box",
        }}
      >
        <div
          onMouseDown={startResizeLeftPanel}
          title="Resize"
          style={{
            ...startPanelResizeHandleStyle,
            right: -6,
          }}
        />

        <div style={{ display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap", marginBottom: 10 }}>
          <button
            type="button"
            onClick={() => setMainTab("cfg")}
            style={{
              fontSize: panelSmall,
              padding: "6px 10px",
              borderRadius: 8,
              border: mainTab === "cfg" ? "2px solid #111827" : "1px solid #d1d5db",
              background: "#fff",
              cursor: "pointer",
            }}
          >
            CFG
          </button>

          <button
            type="button"
            onClick={() => setMainTab("code")}
            style={{
              fontSize: panelSmall,
              padding: "6px 10px",
              borderRadius: 8,
              border: mainTab === "code" ? "2px solid #111827" : "1px solid #d1d5db",
              background: "#fff",
              cursor: "pointer",
            }}
          >
            CODE
          </button>

          <button
            type="button"
            onClick={() => setMainTab("callgraph")}
            style={{
              fontSize: panelSmall,
              padding: "6px 10px",
              borderRadius: 8,
              border: mainTab === "callgraph" ? "2px solid #111827" : "1px solid #d1d5db",
              background: "#fff",
              cursor: "pointer",
            }}
          >
            CALLGRAPH
          </button>

          <span style={{ marginLeft: 8, fontSize: panelSmall, opacity: 0.85 }}>ws={wsStatus}</span>
        </div>

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
        </div>

        <div style={{ marginTop: 12 }}>
          <div style={{ fontWeight: 700 }}>Function</div>

          <select
            value={activeFunc}
            onChange={(e) => setActiveFunc(e.target.value)}
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
          >
            {functions.length === 0 ? (
              <option value="">(no functions)</option>
            ) : (
              functions.map((f) => (
                <option key={f.name} value={f.name}>
                  {f.name} ({f.block_count})
                </option>
              ))
            )}
          </select>

          <label
            style={{
              display: "flex",
              alignItems: "center",
              gap: 8,
              fontSize: panelSmall,
              userSelect: "none",
              marginTop: 8,
            }}
          >
            <input
              type="checkbox"
              checked={followCurrentFunc}
              onChange={(e) => setFollowCurrentFunc(e.target.checked)}
            />
            follow current function
          </label>

          <div style={{ marginTop: 8, fontSize: panelSmall, whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
            active: {activeFunc || "(none)"}{"\n"}
            current func: {currentFunc || "(none)"}{"\n"}
            blocks in active cfg: {cfg?.nodes.length ?? 0}{"\n"}
            entry: {activeFunctionMeta?.entry ?? "(n/a)"}{"\n"}
            exit: {activeFunctionMeta?.exit ?? "(n/a)"}
          </div>
        </div>

        <div style={{ marginTop: 12 }}>
          <div style={{ fontWeight: 700 }}>Call graph mode</div>
          <select
            value={callGraphMode}
            onChange={(e) => setCallGraphMode(e.target.value as CallGraphMode)}
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
          >
            <option value="neighbors">neighbors</option>
            <option value="scc">scc</option>
            <option value="full">full</option>
          </select>
        </div>

        {mainTab === "callgraph" && callGraphLoading && (
          <div
            style={{
              marginTop: 10,
              border: "1px solid #e5e7eb",
              borderRadius: 8,
              padding: 10,
              fontSize: panelSmall,
              background: "#fafafa",
            }}
          >
            Loading call graph...
          </div>
        )}

        {mainTab === "callgraph" && callGraphConfirm && (
          <div
            style={{
              marginTop: 10,
              border: "1px solid #f59e0b",
              borderRadius: 8,
              padding: 10,
              fontSize: panelSmall,
              background: "rgba(245,158,11,0.08)",
            }}
          >
            <div style={{ fontWeight: 700, marginBottom: 8 }}>Call graph is too large</div>
            <div style={{ whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
              function: {callGraphConfirm.func}
              {"\n"}preds: {callGraphConfirm.pred_count}
              {"\n"}succs: {callGraphConfirm.succ_count}
              {"\n"}estimated nodes: {callGraphConfirm.node_estimate}
            </div>

            <div style={{ display: "flex", gap: 8, marginTop: 10 }}>
              <button
                type="button"
                onClick={() => {
                  setCallGraphApprovedKey(callGraphApproveKey(callGraphConfirm.func, callGraphConfirm.mode));
                  setCallGraphConfirm(null);
                }}
                style={{ fontSize: panelSmall, padding: "6px 10px", borderRadius: 8 }}
              >
                Load anyway
              </button>

              <button
                type="button"
                onClick={() => {
                  setCallGraphConfirm(null);
                  setMainTab("cfg");
                }}
                style={{ fontSize: panelSmall, padding: "6px 10px", borderRadius: 8 }}
              >
                Stay on CFG
              </button>
            </div>
          </div>
        )}

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
            placeholder={mainTab === "callgraph" ? "Search by function or SCC members" : "Search by block id / label / instr"}
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
              {searchHits.length === 0 ? "0 matches" : `${searchHits.length} matches`}
              {activeSearchHit ? ` (selected: ${activeMatchIdx + 1}/${searchHits.length})` : ""}
            </div>

            <div style={{ display: "flex", gap: 6 }}>
              <button
                type="button"
                onClick={gotoPrevMatch}
                disabled={searchHits.length === 0}
                style={{ padding: "6px 10px", borderRadius: 8, fontSize: panelSmall }}
              >
                Prev
              </button>
              <button
                type="button"
                onClick={gotoNextMatch}
                disabled={searchHits.length === 0}
                style={{ padding: "6px 10px", borderRadius: 8, fontSize: panelSmall }}
              >
                Next
              </button>
            </div>
          </div>

          {activeSearchHit && (
            <div style={{ marginTop: 8, fontSize: panelSmall, opacity: 0.85, wordBreak: "break-all" }}>
              Active: {activeSearchHit.id} / func={activeSearchHit.func} / scope={activeSearchHit.scope}
            </div>
          )}
        </div>
      </div>

      <div
        style={{
          position: "fixed",
          top: 8,
          right: RIGHT_PANEL_RIGHT,
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
            ...startPanelResizeHandleStyle,
            left: -6,
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

      {mainTab === "code" ? (
        renderCodeView()
      ) : mainTab === "callgraph" ? (
        <ReactFlow
          key={`callgraph-${callGraphMode}`}
          nodes={callGraphNodesForRender}
          edges={callGraphRf.edges}
          onInit={handleCallGraphInit}
          onMoveEnd={handleCallGraphMoveEnd}
          onNodeClick={onCallGraphNodeClick}
          minZoom={MIN_ZOOM}
          maxZoom={MAX_ZOOM}
        >
          <Background />
          <Controls />
        </ReactFlow>
      ) : (
        <ReactFlow
          key={"cfg"}
          nodes={cfgNodesForRender}
          edges={cfgRf.edges}
          onInit={handleCfgInit}
          onMoveEnd={handleCfgMoveEnd}
          onNodeClick={onCfgNodeClick}
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
      )}
    </div>
  );
}
