import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [react()],
  server: {
    host: true,
    port: 5173,
    allowedHosts: ["lambda1.cnu.ac.kr"],
    proxy: {
  "/icfg": { target: "http://localhost:8080", changeOrigin: true },
  "/state": { target: "http://localhost:8080", changeOrigin: true },
  "/ws": { target: "ws://localhost:8080", ws: true },
}
  },
});
