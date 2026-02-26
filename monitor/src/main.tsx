import React from "react";
import ReactDOM from "react-dom/client";
import "./index.css";
import ICFGViewer from "./ICFGViewer";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <div style={{ padding: 16, fontFamily: "monospace" }}>MAIN OK</div>
    <ICFGViewer />
  </React.StrictMode>
);
