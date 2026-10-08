// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopVoiceDebugEventSinkEntry} from '@electron/common/Types';
import {getDesktopDistributionPath} from '@electron/main/DesktopDistributionPath';
import {focusWindow} from '@electron/main/Window';
import {BrowserWindow, ipcMain} from 'electron';
import log from 'electron-log';

const VOICE_DEBUG_EVENT_SINK_WINDOW_TITLE = 'Fluxer | Voice Debug Event Sink';
export const VOICE_DEBUG_EVENT_SINK_POPOUT_KEY = 'fluxer-voice-debug-event-sink';
const VOICE_DEBUG_EVENT_SINK_WINDOW_WIDTH = 1000;
const VOICE_DEBUG_EVENT_SINK_WINDOW_HEIGHT = 700;
const VOICE_DEBUG_EVENT_SINK_WINDOW_MIN_WIDTH = 420;
const VOICE_DEBUG_EVENT_SINK_WINDOW_MIN_HEIGHT = 260;
const VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES = 1000;
const VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS = 262_144;
const VOICE_DEBUG_EVENT_SINK_FLUSH_INTERVAL_MS = 50;
const VOICE_DEBUG_EVENT_SINK_MAX_PENDING_ENTRIES = 1000;
const VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS = 1_048_576;

const VOICE_DEBUG_EVENT_SINK_HTML = String.raw`<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Voice Debug Event Sink</title>
</head>
<body>
<h1>Voice Debug</h1>
<section>
<h2>Stats</h2>
<div>
<button id="copy-stats-json" type="button">Copy stats JSON</button>
<button id="copy-stats-text" type="button">Copy stats text</button>
<button id="copy-stats-html" type="button">Copy stats HTML</button>
<button id="copy-bundle" type="button">Copy diagnostics bundle</button>
<button id="toggle-sticky" type="button" aria-pressed="true">Unpin window</button>
<span id="stats-status"></span>
</div>
<div id="stats"><p>No stats snapshot received.</p></div>
</section>
<hr>
<section>
<h2>Event log</h2>
<div>
<label><input id="autoscroll" type="checkbox" checked> Autoscroll</label>
<label><input id="wrap" type="checkbox"> Wrap</label>
<label><input id="pause" type="checkbox"> Pause render</label>
<input id="filter" type="search" placeholder="Filter log text or type">
<select id="type-filter"><option value="">All event types</option></select>
<button id="clear-filter" type="button">Clear filter</button>
<button id="copy-visible" type="button">Copy visible</button>
<button id="copy-all" type="button">Copy all</button>
<button id="clear-log" type="button">Clear view</button>
<span id="status"></span>
</div>
<pre id="log"></pre>
</section>
<script>
(() => {
"use strict";
const maxEntries = 1000;
const log = document.getElementById("log");
const status = document.getElementById("status");
const stats = document.getElementById("stats");
const statsStatus = document.getElementById("stats-status");
const autoscroll = document.getElementById("autoscroll");
const wrap = document.getElementById("wrap");
const pause = document.getElementById("pause");
const filter = document.getElementById("filter");
const typeFilter = document.getElementById("type-filter");
const clearFilter = document.getElementById("clear-filter");
const copyVisible = document.getElementById("copy-visible");
const copyAll = document.getElementById("copy-all");
const clearLog = document.getElementById("clear-log");
const copyStatsJson = document.getElementById("copy-stats-json");
const copyStatsText = document.getElementById("copy-stats-text");
const copyStatsHtml = document.getElementById("copy-stats-html");
const copyBundle = document.getElementById("copy-bundle");
const toggleSticky = document.getElementById("toggle-sticky");
const popoutKey = "fluxer-voice-debug-event-sink";
let entries = [];
let statsHtml = "";
let statsReceivedAt = "";
let isSticky = true;
function parseEntryType(line) {
	try {
		const parsed = JSON.parse(line);
		return parsed && typeof parsed.type === "string" ? parsed.type : "";
	} catch {}
	return "";
}
function sanitizeEntry(entry) {
	if (!entry || typeof entry !== "object") return null;
	if (!Number.isSafeInteger(entry.sequence) || entry.sequence < 1) return null;
	if (typeof entry.line !== "string") return null;
	return {sequence: entry.sequence, line: entry.line, type: parseEntryType(entry.line)};
}
function trimEntries() {
	if (entries.length > maxEntries) entries = entries.slice(entries.length - maxEntries);
}
function getVisibleEntries() {
	const text = filter.value.trim().toLowerCase();
	const type = typeFilter.value;
	return entries.filter((entry) => {
		if (type && entry.type !== type) return false;
		if (!text) return true;
		return entry.line.toLowerCase().includes(text) || entry.type.toLowerCase().includes(text);
	});
}
function updateTypeFilterOptions() {
	const selected = typeFilter.value;
	const types = Array.from(new Set(entries.map((entry) => entry.type).filter(Boolean))).sort();
	typeFilter.textContent = "";
	const allOption = document.createElement("option");
	allOption.value = "";
	allOption.textContent = "All event types";
	typeFilter.appendChild(allOption);
	for (const type of types) {
		const option = document.createElement("option");
		option.value = type;
		option.textContent = type;
		typeFilter.appendChild(option);
	}
	typeFilter.value = types.includes(selected) ? selected : "";
}
function formatStatus(visibleEntries) {
	const last = entries.length ? entries[entries.length - 1].sequence : 0;
	const paused = pause.checked ? " paused" : "";
	return visibleEntries.length + " visible / " + entries.length + " entries; last #" + last + paused;
}
function render() {
	const visibleEntries = getVisibleEntries();
	log.textContent = visibleEntries.map((entry) => entry.line).join("\n");
	status.textContent = formatStatus(visibleEntries);
	if (autoscroll.checked) window.scrollTo(0, document.body.scrollHeight);
}
function renderIfNotPaused() {
	updateTypeFilterOptions();
	if (pause.checked) {
		status.textContent = formatStatus(getVisibleEntries());
		return;
	}
	render();
}
async function copyText(text) {
	try {
		await navigator.clipboard.writeText(text);
		return true;
	} catch {}
	const textarea = document.createElement("textarea");
	textarea.value = text;
	document.body.appendChild(textarea);
	textarea.select();
	const copied = document.execCommand("copy");
	textarea.remove();
	return copied;
}
function setStatsHtml(nextStatsHtml) {
	statsHtml = typeof nextStatsHtml === "string" ? nextStatsHtml : "";
	statsReceivedAt = statsHtml ? new Date().toISOString() : "";
	stats.innerHTML = statsHtml || "<p>No stats snapshot received.</p>";
	statsStatus.textContent = statsHtml ? "stats snapshot loaded " + statsReceivedAt : "no stats snapshot";
}
function renderStickyButton() {
	toggleSticky.setAttribute("aria-pressed", isSticky ? "true" : "false");
	toggleSticky.textContent = isSticky ? "Unpin window" : "Pin window";
}
async function setSticky(nextSticky) {
	const desktop = window.electron;
	if (!desktop || typeof desktop.popoutSetAlwaysOnTop !== "function") return;
	const changed = await desktop.popoutSetAlwaysOnTop(popoutKey, nextSticky);
	if (!changed) return;
	isSticky = nextSticky;
	renderStickyButton();
}
function getStatsJsonText() {
	const json = document.getElementById("stats-json");
	return json ? json.textContent || "" : "";
}
function getBundleText() {
	return [
		"# voice debug stats JSON",
		getStatsJsonText(),
		"",
		"# visible event log",
		getVisibleEntries().map((entry) => entry.line).join("\n"),
	].join("\n");
}
function appendEntries(nextEntries) {
	if (!Array.isArray(nextEntries)) return;
	for (const entry of nextEntries.slice(-maxEntries)) {
		const sanitized = sanitizeEntry(entry);
		if (sanitized) entries.push(sanitized);
	}
	trimEntries();
	renderIfNotPaused();
}
window.__fluxerVoiceDebugEventSinkSetEntries = (nextEntries) => {
	entries = [];
	appendEntries(nextEntries);
};
window.__fluxerVoiceDebugEventSinkAppendEntries = appendEntries;
window.__fluxerVoiceDebugEventSinkSetStatsHtml = setStatsHtml;
wrap.addEventListener("change", () => {
	log.style.whiteSpace = wrap.checked ? "pre-wrap" : "pre";
});
pause.addEventListener("change", render);
filter.addEventListener("input", render);
typeFilter.addEventListener("change", render);
clearFilter.addEventListener("click", () => {
	filter.value = "";
	typeFilter.value = "";
	render();
});
clearLog.addEventListener("click", () => {
	entries = [];
	updateTypeFilterOptions();
	render();
});
copyVisible.addEventListener("click", async () => {
	const text = getVisibleEntries().map((entry) => entry.line).join("\n");
	const copied = await copyText(text);
	status.textContent = copied ? "copied " + getVisibleEntries().length + " visible entries" : "copy failed";
});
copyAll.addEventListener("click", async () => {
	const text = entries.map((entry) => entry.line).join("\n");
	const copied = await copyText(text);
	status.textContent = copied ? "copied " + entries.length + " entries" : "copy failed";
});
copyStatsJson.addEventListener("click", async () => {
	const copied = await copyText(getStatsJsonText());
	statsStatus.textContent = copied ? "copied stats JSON" : "copy failed";
});
copyStatsText.addEventListener("click", async () => {
	const copied = await copyText(stats.textContent || "");
	statsStatus.textContent = copied ? "copied stats text" : "copy failed";
});
copyStatsHtml.addEventListener("click", async () => {
	const copied = await copyText(statsHtml);
	statsStatus.textContent = copied ? "copied stats HTML" : "copy failed";
});
copyBundle.addEventListener("click", async () => {
	const copied = await copyText(getBundleText());
	statsStatus.textContent = copied ? "copied diagnostics bundle" : "copy failed";
});
toggleSticky.addEventListener("click", () => {
	void setSticky(!isSticky);
});
render();
renderStickyButton();
setStatsHtml("");
})();
</script>
</body>
</html>`;

const VOICE_DEBUG_EVENT_SINK_DATA_URL = `data:text/html;charset=utf-8,${encodeURIComponent(
	VOICE_DEBUG_EVENT_SINK_HTML,
)}`;

let eventSinkWindow: BrowserWindow | null = null;
let eventSinkEntries: Array<DesktopVoiceDebugEventSinkEntry> = [];
let eventSinkStatsHtml = '';
let pendingPopoutEntries: Array<DesktopVoiceDebugEventSinkEntry> = [];
let flushTimer: NodeJS.Timeout | null = null;

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function isAliveWindow(window: BrowserWindow | null): window is BrowserWindow {
	return Boolean(window && !window.isDestroyed());
}

function truncateLine(line: string): string {
	if (line.length <= VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS) return line;
	const omittedChars = line.length - VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS;
	return `${line.slice(0, VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS)}... [truncated ${omittedChars} chars]`;
}

function truncateStatsHtml(html: string): string {
	if (html.length <= VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS) return html;
	const omittedChars = html.length - VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS;
	return `${html.slice(0, VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS)}<p>[truncated ${omittedChars} chars]</p>`;
}

function normalizeEntry(value: unknown): DesktopVoiceDebugEventSinkEntry | null {
	if (!isRecord(value)) return null;
	const sequence = value.sequence;
	if (typeof sequence !== 'number') return null;
	if (!Number.isSafeInteger(sequence) || sequence < 1) return null;
	const line = value.line;
	if (typeof line !== 'string') return null;
	return {
		sequence,
		line: truncateLine(line),
	};
}

function normalizeEntries(value: unknown): Array<DesktopVoiceDebugEventSinkEntry> {
	if (!Array.isArray(value)) return [];
	const normalized: Array<DesktopVoiceDebugEventSinkEntry> = [];
	for (const entry of value.slice(-VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES)) {
		const normalizedEntry = normalizeEntry(entry);
		if (normalizedEntry) normalized.push(normalizedEntry);
	}
	return normalized;
}

function normalizeStatsHtml(value: unknown): string {
	if (typeof value !== 'string') return '';
	return truncateStatsHtml(value);
}

function replaceEventSinkEntries(entries: Array<DesktopVoiceDebugEventSinkEntry>): void {
	eventSinkEntries = entries.slice(-VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES);
	pendingPopoutEntries = [];
}

function appendEventSinkEntries(entries: Array<DesktopVoiceDebugEventSinkEntry>): void {
	if (entries.length === 0) return;
	eventSinkEntries.push(...entries);
	if (eventSinkEntries.length > VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES) {
		eventSinkEntries.splice(0, eventSinkEntries.length - VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES);
	}
}

function getEventSinkWindow(): BrowserWindow | null {
	if (isAliveWindow(eventSinkWindow)) return eventSinkWindow;
	eventSinkWindow = null;
	return null;
}

function escapeScriptValue(value: unknown): string {
	return (JSON.stringify(value) ?? 'null').replace(/</g, '\\u003c');
}

function runPopoutFunction(window: BrowserWindow, functionName: string, payload: unknown): Promise<unknown> {
	const encodedPayload = escapeScriptValue(payload);
	const source = `typeof window.${functionName} === "function" && window.${functionName}(${encodedPayload})`;
	return window.webContents.executeJavaScript(source, true);
}

function sendSnapshotToPopout(window: BrowserWindow): void {
	pendingPopoutEntries = [];
	void runPopoutFunction(window, '__fluxerVoiceDebugEventSinkSetEntries', eventSinkEntries).catch((error) => {
		log.warn('Failed to send voice debug event sink snapshot to popout', error);
	});
	void runPopoutFunction(window, '__fluxerVoiceDebugEventSinkSetStatsHtml', eventSinkStatsHtml).catch((error) => {
		log.warn('Failed to send voice debug stats snapshot to popout', error);
	});
}

function schedulePendingFlush(): void {
	if (flushTimer) return;
	flushTimer = setTimeout(() => {
		flushTimer = null;
		flushPendingEntriesToPopout();
	}, VOICE_DEBUG_EVENT_SINK_FLUSH_INTERVAL_MS);
}

function flushPendingEntriesToPopout(): void {
	const window = getEventSinkWindow();
	if (!window || pendingPopoutEntries.length === 0) return;
	if (window.webContents.isLoadingMainFrame()) {
		schedulePendingFlush();
		return;
	}
	const entries = pendingPopoutEntries.splice(0, pendingPopoutEntries.length);
	void runPopoutFunction(window, '__fluxerVoiceDebugEventSinkAppendEntries', entries).catch((error) => {
		log.warn('Failed to append voice debug event sink entries to popout', error);
	});
}

function queueEntriesForPopout(entries: Array<DesktopVoiceDebugEventSinkEntry>): void {
	const window = getEventSinkWindow();
	if (!window || entries.length === 0) return;
	pendingPopoutEntries.push(...entries);
	if (pendingPopoutEntries.length > VOICE_DEBUG_EVENT_SINK_MAX_PENDING_ENTRIES) {
		pendingPopoutEntries.splice(0, pendingPopoutEntries.length - VOICE_DEBUG_EVENT_SINK_MAX_PENDING_ENTRIES);
	}
	schedulePendingFlush();
}

function createEventSinkWindow(): BrowserWindow {
	const window = new BrowserWindow({
		width: VOICE_DEBUG_EVENT_SINK_WINDOW_WIDTH,
		height: VOICE_DEBUG_EVENT_SINK_WINDOW_HEIGHT,
		minWidth: VOICE_DEBUG_EVENT_SINK_WINDOW_MIN_WIDTH,
		minHeight: VOICE_DEBUG_EVENT_SINK_WINDOW_MIN_HEIGHT,
		show: true,
		title: VOICE_DEBUG_EVENT_SINK_WINDOW_TITLE,
		webPreferences: {
			preload: getDesktopDistributionPath('preload', 'index.cjs'),
			contextIsolation: true,
			nodeIntegration: false,
			sandbox: false,
			webSecurity: true,
			spellcheck: false,
		},
	});
	eventSinkWindow = window;
	window.setAlwaysOnTop(true);
	window.once('closed', () => {
		if (eventSinkWindow === window) eventSinkWindow = null;
	});
	window.webContents.on('did-finish-load', () => sendSnapshotToPopout(window));
	window.loadURL(VOICE_DEBUG_EVENT_SINK_DATA_URL).catch((error) => {
		log.warn('Failed to load voice debug event sink popout', error);
	});
	return window;
}

async function openVoiceDebugEventSinkPopout(rawEntries: unknown): Promise<void> {
	replaceEventSinkEntries(normalizeEntries(rawEntries));
	const window = getEventSinkWindow() ?? createEventSinkWindow();
	focusWindow(window);
	if (!window.webContents.isLoadingMainFrame()) sendSnapshotToPopout(window);
}

export function focusVoiceDebugEventSinkPopout(): boolean {
	const window = getEventSinkWindow();
	if (!window) {
		return false;
	}
	focusWindow(window);
	return true;
}

export function setVoiceDebugEventSinkAlwaysOnTop(flag: boolean): boolean {
	const window = getEventSinkWindow();
	if (!window) {
		return false;
	}
	window.setAlwaysOnTop(flag);
	return true;
}

function appendVoiceDebugEventSinkEntries(rawEntries: unknown): void {
	const entries = normalizeEntries(rawEntries);
	appendEventSinkEntries(entries);
	queueEntriesForPopout(entries);
}

function setVoiceDebugEventSinkStatsHtml(rawStatsHtml: unknown): void {
	eventSinkStatsHtml = normalizeStatsHtml(rawStatsHtml);
	const window = getEventSinkWindow();
	if (!window || window.webContents.isLoadingMainFrame()) return;
	void runPopoutFunction(window, '__fluxerVoiceDebugEventSinkSetStatsHtml', eventSinkStatsHtml).catch((error) => {
		log.warn('Failed to update voice debug stats in popout', error);
	});
}

export function registerVoiceDebugEventSinkPopoutIpcHandlers(): void {
	ipcMain.handle(
		'voice-debug-event-sink:open',
		(_event, entries: unknown): Promise<void> => openVoiceDebugEventSinkPopout(entries),
	);
	ipcMain.on('voice-debug-event-sink:append', (_event, entries: unknown): void => {
		appendVoiceDebugEventSinkEntries(entries);
	});
	ipcMain.on('voice-debug-event-sink:set-stats-html', (_event, statsHtml: unknown): void => {
		setVoiceDebugEventSinkStatsHtml(statsHtml);
	});
}
