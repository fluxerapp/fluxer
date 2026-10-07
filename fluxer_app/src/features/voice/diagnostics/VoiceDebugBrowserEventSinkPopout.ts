// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopVoiceDebugEventSinkEntry} from '@app/types/electron.d';

const VOICE_DEBUG_EVENT_SINK_WINDOW_NAME = 'fluxer-voice-debug-event-sink';
const VOICE_DEBUG_EVENT_SINK_WINDOW_FEATURES = 'popup,width=1000,height=700';
const VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL = 'fluxer.voice.debug.event_sink.v1';
const VOICE_DEBUG_EVENT_SINK_MESSAGE_TARGET_ORIGIN = '*';
const VOICE_DEBUG_EVENT_SINK_BROADCAST_CHANNEL_NAME = 'fluxer.voice.debug.event_sink.v1';
const VOICE_DEBUG_EVENT_SINK_INITIAL_STATE_PLACEHOLDER = '__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_STATE__';
const VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_HTML_PLACEHOLDER = '__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_HTML__';
const VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_STATUS_PLACEHOLDER =
	'__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_STATUS__';
const VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_TEXT_PLACEHOLDER = '__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_TEXT__';
const VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_STATUS_PLACEHOLDER = '__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_STATUS__';
const VOICE_DEBUG_EVENT_SINK_SCRIPT_URL_PLACEHOLDER = '__FLUXER_VOICE_DEBUG_EVENT_SINK_SCRIPT_URL__';
const VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES = 1000;
const VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS = 262_144;
const VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS = 1_048_576;
const VOICE_DEBUG_EVENT_SINK_EMPTY_STATS_HTML = '<p>No stats snapshot captured before popout opened.</p>';

type BrowserVoiceDebugEventSinkMessage =
	| {
			protocol: typeof VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL;
			type: 'setEntries';
			entries: Array<DesktopVoiceDebugEventSinkEntry>;
	  }
	| {
			protocol: typeof VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL;
			type: 'appendEntries';
			entries: Array<DesktopVoiceDebugEventSinkEntry>;
	  }
	| {
			protocol: typeof VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL;
			type: 'setStatsHtml';
			html: string;
	  };

interface BrowserVoiceDebugEventSinkReadyMessage {
	protocol: typeof VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL;
	type: 'ready';
}

interface BrowserVoiceDebugEventSinkInitialState {
	entries: Array<DesktopVoiceDebugEventSinkEntry>;
	statsHtml: string;
}

type BrowserVoiceDebugEventSinkWindow = Window;

const VOICE_DEBUG_EVENT_SINK_HTML = `<!doctype html>
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
<span id="stats-status">__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_STATUS__</span>
</div>
<div id="stats">__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_HTML__</div>
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
<span id="status">__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_STATUS__</span>
</div>
<pre id="log">__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_TEXT__</pre>
</section>
<script id="initial-state" type="application/json">__FLUXER_VOICE_DEBUG_EVENT_SINK_INITIAL_STATE__</script>
<script src="__FLUXER_VOICE_DEBUG_EVENT_SINK_SCRIPT_URL__"></script>
</body>
</html>`;

const VOICE_DEBUG_EVENT_SINK_SCRIPT = String.raw`(() => {
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
const messageProtocol = "fluxer.voice.debug.event_sink.v1";
const broadcastChannelName = "fluxer.voice.debug.event_sink.v1";
let entries = [];
let statsHtml = "";
let statsReceivedAt = "";
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
function restoreFocus(node) {
	try {
		if (node && typeof node.focus === "function") node.focus({preventScroll: true});
	} catch {}
}
function copyTextWithSelection(text) {
	const activeElement = document.activeElement;
	const textarea = document.createElement("textarea");
	textarea.value = text;
	textarea.setAttribute("readonly", "");
	textarea.style.position = "fixed";
	textarea.style.top = "0";
	textarea.style.left = "-9999px";
	textarea.style.opacity = "0";
	document.body.appendChild(textarea);
	try {
		textarea.focus({preventScroll: true});
		textarea.select();
		textarea.setSelectionRange(0, textarea.value.length);
		return document.execCommand("copy");
	} catch {
		return false;
	} finally {
		textarea.remove();
		restoreFocus(activeElement);
	}
}
function showManualCopyText(text) {
	let section = document.getElementById("manual-copy-section");
	let textarea = document.getElementById("manual-copy");
	if (!section || !textarea) {
		section = document.createElement("section");
		section.id = "manual-copy-section";
		const heading = document.createElement("h2");
		heading.textContent = "Manual copy";
		const hint = document.createElement("p");
		hint.textContent = "Automatic clipboard access was blocked. The text below is selected.";
		textarea = document.createElement("textarea");
		textarea.id = "manual-copy";
		textarea.setAttribute("readonly", "");
		textarea.style.boxSizing = "border-box";
		textarea.style.width = "100%";
		textarea.style.height = "180px";
		section.appendChild(heading);
		section.appendChild(hint);
		section.appendChild(textarea);
		document.body.insertBefore(section, document.body.firstChild);
	}
	textarea.value = text;
	textarea.focus({preventScroll: true});
	textarea.select();
	textarea.setSelectionRange(0, textarea.value.length);
	return true;
}
function formatCopyStatus(result, copiedText) {
	if (result === "copied") return copiedText;
	if (result === "manual") return "copy blocked; selected text for manual copy";
	return "copy failed";
}
async function copyText(text) {
	if (copyTextWithSelection(text)) return "copied";
	try {
		if (navigator.clipboard && typeof navigator.clipboard.writeText === "function") {
			await navigator.clipboard.writeText(text);
			return "copied";
		}
	} catch {}
	return showManualCopyText(text) ? "manual" : "failed";
}
function setStatsHtml(nextStatsHtml) {
	statsHtml = typeof nextStatsHtml === "string" ? nextStatsHtml : "";
	statsReceivedAt = statsHtml ? new Date().toISOString() : "";
	stats.innerHTML = statsHtml || "<p>No stats snapshot received.</p>";
	statsStatus.textContent = statsHtml ? "stats snapshot loaded " + statsReceivedAt : "no stats snapshot";
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
function readInitialState() {
	const node = document.getElementById("initial-state");
	if (!node || typeof node.textContent !== "string") return null;
	try {
		const parsed = JSON.parse(node.textContent);
		if (!parsed || typeof parsed !== "object") return null;
		return parsed;
	} catch {}
	return null;
}
function applyInitialState() {
	const initialState = readInitialState();
	entries = [];
	appendEntries(initialState && Array.isArray(initialState.entries) ? initialState.entries : []);
	setStatsHtml(initialState && typeof initialState.statsHtml === "string" ? initialState.statsHtml : "");
}
function handleMessage(message) {
	if (!message || typeof message !== "object") return;
	if (message.protocol !== messageProtocol) return;
	if (message.type === "setEntries") {
		entries = [];
		appendEntries(message.entries);
		return;
	}
	if (message.type === "appendEntries") {
		appendEntries(message.entries);
		return;
	}
	if (message.type === "setStatsHtml") {
		setStatsHtml(message.html);
	}
}
window.__fluxerVoiceDebugEventSinkSetEntries = (nextEntries) => {
	entries = [];
	appendEntries(nextEntries);
};
window.__fluxerVoiceDebugEventSinkAppendEntries = appendEntries;
window.__fluxerVoiceDebugEventSinkSetStatsHtml = setStatsHtml;
window.addEventListener("message", (event) => {
	handleMessage(event.data);
});
let broadcastChannel = null;
if (typeof BroadcastChannel === "function") {
	broadcastChannel = new BroadcastChannel(broadcastChannelName);
	broadcastChannel.addEventListener("message", (event) => {
		handleMessage(event.data);
	});
}
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
	const result = await copyText(text);
	status.textContent = formatCopyStatus(result, "copied " + getVisibleEntries().length + " visible entries");
});
copyAll.addEventListener("click", async () => {
	const text = entries.map((entry) => entry.line).join("\n");
	const result = await copyText(text);
	status.textContent = formatCopyStatus(result, "copied " + entries.length + " entries");
});
copyStatsJson.addEventListener("click", async () => {
	const result = await copyText(getStatsJsonText());
	statsStatus.textContent = formatCopyStatus(result, "copied stats JSON");
});
copyStatsText.addEventListener("click", async () => {
	const result = await copyText(stats.textContent || "");
	statsStatus.textContent = formatCopyStatus(result, "copied stats text");
});
copyStatsHtml.addEventListener("click", async () => {
	const result = await copyText(statsHtml);
	statsStatus.textContent = formatCopyStatus(result, "copied stats HTML");
});
copyBundle.addEventListener("click", async () => {
	const result = await copyText(getBundleText());
	statsStatus.textContent = formatCopyStatus(result, "copied diagnostics bundle");
});
applyInitialState();
if (broadcastChannel) {
	broadcastChannel.postMessage({protocol: messageProtocol, type: "ready"});
}
if (window.opener) {
	window.opener.postMessage({protocol: messageProtocol, type: "ready"}, "*");
}
})();
`;

let eventSinkWindow: BrowserVoiceDebugEventSinkWindow | null = null;
let eventSinkEntries: Array<DesktopVoiceDebugEventSinkEntry> = [];
let eventSinkStatsHtml = '';
let eventSinkBlobUrl: string | null = null;
let eventSinkScriptBlobUrl: string | null = null;
let eventSinkBroadcastChannel: BroadcastChannel | null = null;
let messageListenerInstalled = false;

function isOpenWindow(
	windowRef: BrowserVoiceDebugEventSinkWindow | null,
): windowRef is BrowserVoiceDebugEventSinkWindow {
	return Boolean(windowRef && !windowRef.closed);
}

function truncateLine(line: string): string {
	if (line.length <= VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS) return line;
	const omittedChars = line.length - VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS;
	return `${line.slice(0, VOICE_DEBUG_EVENT_SINK_MAX_LINE_CHARS)}... [truncated ${omittedChars} chars]`;
}

function truncateStatsHtml(html: string): string {
	if (html.length <= VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS) return html;
	const omittedChars = html.length - VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS;
	return `${html.slice(0, VOICE_DEBUG_EVENT_SINK_MAX_STATS_HTML_CHARS)}<!-- truncated ${omittedChars} chars -->`;
}

function normalizeEntry(value: unknown): DesktopVoiceDebugEventSinkEntry | null {
	if (value === null || typeof value !== 'object') return null;
	const entry = value as Partial<DesktopVoiceDebugEventSinkEntry>;
	const sequence = entry.sequence;
	const line = entry.line;
	if (typeof sequence !== 'number') return null;
	if (!Number.isSafeInteger(sequence)) return null;
	if (sequence < 1) return null;
	if (typeof line !== 'string') return null;
	return {sequence, line: truncateLine(line)};
}

function normalizeEntries(
	values: ReadonlyArray<DesktopVoiceDebugEventSinkEntry>,
): Array<DesktopVoiceDebugEventSinkEntry> {
	const normalized: Array<DesktopVoiceDebugEventSinkEntry> = [];
	for (const value of values.slice(-VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES)) {
		const entry = normalizeEntry(value);
		if (entry) normalized.push(entry);
	}
	return normalized;
}

function trimEntries(): void {
	if (eventSinkEntries.length <= VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES) return;
	eventSinkEntries = eventSinkEntries.slice(eventSinkEntries.length - VOICE_DEBUG_EVENT_SINK_MAX_ENTRIES);
}

function appendNormalizedEventSinkEntries(entries: ReadonlyArray<DesktopVoiceDebugEventSinkEntry>): void {
	for (const entry of entries) {
		eventSinkEntries.push(entry);
	}
	trimEntries();
}

function replaceEventSinkEntries(entries: ReadonlyArray<DesktopVoiceDebugEventSinkEntry>): void {
	eventSinkEntries = normalizeEntries(entries);
}

function serializeInitialState(initialState: BrowserVoiceDebugEventSinkInitialState): string {
	return JSON.stringify(initialState).replaceAll('<', '\\u003c');
}

function escapeHtml(value: string): string {
	return value
		.replaceAll('&', '&amp;')
		.replaceAll('<', '&lt;')
		.replaceAll('>', '&gt;')
		.replaceAll('"', '&quot;')
		.replaceAll("'", '&#39;');
}

function createInitialState(): BrowserVoiceDebugEventSinkInitialState {
	return {
		entries: eventSinkEntries,
		statsHtml: eventSinkStatsHtml,
	};
}

function formatInitialLogStatus(initialState: BrowserVoiceDebugEventSinkInitialState): string {
	const last = initialState.entries.length ? initialState.entries[initialState.entries.length - 1]?.sequence : 0;
	const emptyReason = initialState.entries.length === 0 ? '; no debug events captured before popout opened' : '';
	return `${initialState.entries.length} visible / ${initialState.entries.length} entries; last #${last}${emptyReason}`;
}

function getInitialStatsStatus(initialState: BrowserVoiceDebugEventSinkInitialState): string {
	return initialState.statsHtml
		? 'stats snapshot captured before popout opened'
		: 'no stats snapshot captured before popout opened';
}

function createPopoutHtml(scriptUrl: string): string {
	const initialState = createInitialState();
	const initialStatsHtml = initialState.statsHtml || VOICE_DEBUG_EVENT_SINK_EMPTY_STATS_HTML;
	const initialLogText = initialState.entries.map((entry) => entry.line).join('\n');
	return VOICE_DEBUG_EVENT_SINK_HTML.replace(VOICE_DEBUG_EVENT_SINK_INITIAL_STATE_PLACEHOLDER, () =>
		serializeInitialState(initialState),
	)
		.replace(VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_HTML_PLACEHOLDER, () => initialStatsHtml)
		.replace(VOICE_DEBUG_EVENT_SINK_INITIAL_STATS_STATUS_PLACEHOLDER, () =>
			escapeHtml(getInitialStatsStatus(initialState)),
		)
		.replace(VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_TEXT_PLACEHOLDER, () => escapeHtml(initialLogText))
		.replace(VOICE_DEBUG_EVENT_SINK_INITIAL_LOG_STATUS_PLACEHOLDER, () =>
			escapeHtml(formatInitialLogStatus(initialState)),
		)
		.replace(VOICE_DEBUG_EVENT_SINK_SCRIPT_URL_PLACEHOLDER, () => escapeHtml(scriptUrl));
}

function isReadyMessage(value: unknown): value is BrowserVoiceDebugEventSinkReadyMessage {
	if (value === null || typeof value !== 'object') return false;
	const message = value as Partial<BrowserVoiceDebugEventSinkReadyMessage>;
	if (message.protocol !== VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL) return false;
	return message.type === 'ready';
}

function handlePopoutMessage(event: MessageEvent<unknown>): void {
	if (!isOpenWindow(eventSinkWindow)) return;
	if (!isReadyMessage(event.data)) return;
	sendSnapshotToPopout();
}

function ensureMessageListener(): void {
	if (messageListenerInstalled) return;
	window.addEventListener('message', handlePopoutMessage);
	messageListenerInstalled = true;
}

function ensureBroadcastChannel(): BroadcastChannel | null {
	if (eventSinkBroadcastChannel) return eventSinkBroadcastChannel;
	if (typeof BroadcastChannel !== 'function') return null;
	eventSinkBroadcastChannel = new BroadcastChannel(VOICE_DEBUG_EVENT_SINK_BROADCAST_CHANNEL_NAME);
	eventSinkBroadcastChannel.addEventListener('message', handlePopoutMessage);
	return eventSinkBroadcastChannel;
}

function createPopoutUrls(): {scriptUrl: string; htmlUrl: string} {
	const scriptBlob = new Blob([VOICE_DEBUG_EVENT_SINK_SCRIPT], {type: 'text/javascript;charset=utf-8'});
	const scriptUrl = URL.createObjectURL(scriptBlob);
	const htmlBlob = new Blob([createPopoutHtml(scriptUrl)], {type: 'text/html;charset=utf-8'});
	return {scriptUrl, htmlUrl: URL.createObjectURL(htmlBlob)};
}

function revokePopoutUrl(): void {
	if (eventSinkBlobUrl) {
		URL.revokeObjectURL(eventSinkBlobUrl);
		eventSinkBlobUrl = null;
	}
	if (eventSinkScriptBlobUrl) {
		URL.revokeObjectURL(eventSinkScriptBlobUrl);
		eventSinkScriptBlobUrl = null;
	}
}

function postMessageToPopout(message: BrowserVoiceDebugEventSinkMessage): void {
	if (!isOpenWindow(eventSinkWindow)) return;
	const broadcastChannel = ensureBroadcastChannel();
	if (broadcastChannel) {
		broadcastChannel.postMessage(message);
		return;
	}
	eventSinkWindow.postMessage(message, VOICE_DEBUG_EVENT_SINK_MESSAGE_TARGET_ORIGIN);
}

function sendSnapshotToPopout(): void {
	if (!isOpenWindow(eventSinkWindow)) return;
	postMessageToPopout({
		protocol: VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL,
		type: 'setEntries',
		entries: eventSinkEntries,
	});
	postMessageToPopout({
		protocol: VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL,
		type: 'setStatsHtml',
		html: eventSinkStatsHtml,
	});
}

function createBrowserPopoutWindow(): BrowserVoiceDebugEventSinkWindow | null {
	if (!canOpenBrowserVoiceDebugEventSinkPopout()) return null;
	ensureMessageListener();
	ensureBroadcastChannel();
	revokePopoutUrl();
	const {scriptUrl, htmlUrl} = createPopoutUrls();
	const opened = window.open(htmlUrl, VOICE_DEBUG_EVENT_SINK_WINDOW_NAME, VOICE_DEBUG_EVENT_SINK_WINDOW_FEATURES);
	if (!opened) {
		URL.revokeObjectURL(htmlUrl);
		URL.revokeObjectURL(scriptUrl);
		return null;
	}
	eventSinkBlobUrl = htmlUrl;
	eventSinkScriptBlobUrl = scriptUrl;
	eventSinkWindow = opened;
	opened.focus();
	sendSnapshotToPopout();
	return opened;
}

function canCreatePopoutUrl(): boolean {
	if (typeof Blob === 'undefined') return false;
	if (typeof URL === 'undefined') return false;
	return typeof URL.createObjectURL === 'function';
}

export function canOpenBrowserVoiceDebugEventSinkPopout(): boolean {
	if (typeof window === 'undefined') return false;
	if (typeof window.open !== 'function') return false;
	return canCreatePopoutUrl();
}

export async function openBrowserVoiceDebugEventSinkPopout(
	entries: ReadonlyArray<DesktopVoiceDebugEventSinkEntry>,
): Promise<boolean> {
	replaceEventSinkEntries(entries);
	if (!isOpenWindow(eventSinkWindow)) {
		return createBrowserPopoutWindow() !== null;
	}
	eventSinkWindow.focus();
	sendSnapshotToPopout();
	return true;
}

export function appendBrowserVoiceDebugEventSinkEntries(entries: ReadonlyArray<DesktopVoiceDebugEventSinkEntry>): void {
	const normalizedEntries = normalizeEntries(entries);
	appendNormalizedEventSinkEntries(normalizedEntries);
	if (!isOpenWindow(eventSinkWindow)) return;
	postMessageToPopout({
		protocol: VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL,
		type: 'appendEntries',
		entries: normalizedEntries,
	});
}

export function setBrowserVoiceDebugEventSinkStatsHtml(html: string): void {
	eventSinkStatsHtml = truncateStatsHtml(html);
	if (!isOpenWindow(eventSinkWindow)) return;
	postMessageToPopout({
		protocol: VOICE_DEBUG_EVENT_SINK_MESSAGE_PROTOCOL,
		type: 'setStatsHtml',
		html: eventSinkStatsHtml,
	});
}
