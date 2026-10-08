// SPDX-License-Identifier: AGPL-3.0-or-later

const {ipcRenderer} = require('electron');

const SPLASH_STATE_CHANNEL = 'desktop-splash:state';
const SPLASH_REVEALED_CHANNEL = 'desktop-splash:revealed';
const SPLASH_READY_CHANNEL = 'desktop-splash:ready';
const SPLASH_RETRY_CHANNEL = 'desktop-splash:retry-now';
const SPLASH_QUIT_CHANNEL = 'desktop-splash:quit';
const SPLASH_OPEN_DOWNLOAD_CHANNEL = 'desktop-splash:open-download';
const SPLASH_NETWORK_ONLINE_CHANNEL = 'desktop-splash:network-online';
const SPLASH_OPEN_LOGS_CHANNEL = 'desktop-splash:open-logs';
const SPLASH_COPY_DIAGNOSTICS_CHANNEL = 'desktop-splash:copy-diagnostics';

const SPLASH_MOUNT_ID = 'splash-mount';
const SPLASH_MARK_SIZE = 88;
const SPLASH_MARK_SOURCE = './symbol.svg';
const COUNTDOWN_INTERVAL_MS = 1000;
const SPLASH_BRANDING_WAIT_MS = 400;
const SPLASH_FONT_FACES = Object.freeze(['400 12px "Fluxer Sans"', '500 16px "Fluxer Sans"']);
const NON_BREAKING_SPACE = '\u00a0';
const SPLASH_ACTION_LABEL_MAX_LENGTH = 48;
const SPLASH_MESSAGE_MAX_LENGTH = 160;
const SPLASH_VERSION_LABEL_MAX_LENGTH = 64;
const SPLASH_OPTION_LABEL_MAX_LENGTH = 48;
const SPLASH_OPTION_BUTTON_LABEL_MAX_LENGTH = 24;
const SPLASH_OPTION_MAX_COUNT = 8;
const SPLASH_OPTION_VALUE_PATTERN = /^[a-z0-9_]{1,32}$/;
const DIAGNOSTICS_REVEAL_MS = 30000;
const DIAGNOSTICS_COPIED_MS = 2000;
const BYTES_PER_MEGABYTE = 1000000;
const BYTES_PER_KILOBYTE = 1000;

const SplashLayout = Object.freeze({
	SPLASH: 'splash',
	MANUAL_UPDATE: 'manual-update',
});

const SplashStatus = Object.freeze({
	CHECKING_FOR_UPDATES: 'checking-for-updates',
	DOWNLOADING_UPDATES: 'downloading-updates',
	INSTALLING_UPDATES: 'installing-updates',
	VERIFYING: 'verifying',
	UPDATE_FAILURE: 'update-failure',
	DOWNLOAD_STALLED: 'download-stalled',
	SHELL_UPDATE_DOWNLOADING: 'shell-update-downloading',
	SHELL_UPDATE_RESTARTING: 'shell-update-restarting',
	BLOCKED_UPDATE_REQUIRED: 'blocked-update-required',
	BLOCKED_UPDATE_UNREACHABLE: 'blocked-update-unreachable',
	BLOCKED_SECURITY_UPDATE_REQUIRED: 'blocked-security-update-required',
	BLOCKED_SECURITY_UPDATE_MANAGED: 'blocked-security-update-managed',
	BLOCKED_SHELL_UPDATE: 'blocked-shell-update',
	BLOCKED_SHELL_UPDATE_MANAGED: 'blocked-shell-update-managed',
	BLOCKED_UNSUPPORTED_BUILD: 'blocked-unsupported-build',
	LAUNCHING: 'launching',
	UNREACHABLE_LAUNCH: 'unreachable-launch',
	WAITING_LOCAL_NETWORK: 'waiting-local-network',
});

const KNOWN_LAYOUTS = new Set(Object.values(SplashLayout));
const KNOWN_STATUSES = new Set(Object.values(SplashStatus));
const PROGRESS_STATUSES = new Set([
	SplashStatus.DOWNLOADING_UPDATES,
	SplashStatus.DOWNLOAD_STALLED,
	SplashStatus.INSTALLING_UPDATES,
	SplashStatus.SHELL_UPDATE_DOWNLOADING,
]);
const DIAGNOSTICS_STATUSES = new Set([
	SplashStatus.UPDATE_FAILURE,
	SplashStatus.DOWNLOAD_STALLED,
	SplashStatus.BLOCKED_UPDATE_REQUIRED,
	SplashStatus.BLOCKED_UPDATE_UNREACHABLE,
	SplashStatus.BLOCKED_SECURITY_UPDATE_REQUIRED,
	SplashStatus.BLOCKED_SHELL_UPDATE,
	SplashStatus.BLOCKED_UNSUPPORTED_BUILD,
	SplashStatus.UNREACHABLE_LAUNCH,
]);
const DIAGNOSTICS_HIDDEN_STATUSES = new Set([SplashStatus.LAUNCHING, SplashStatus.SHELL_UPDATE_RESTARTING]);
const ACTION_CHANNELS = new Map([
	['retry', SPLASH_RETRY_CHANNEL],
	['download', SPLASH_OPEN_DOWNLOAD_CHANNEL],
	['quit', SPLASH_QUIT_CHANNEL],
]);
const SPLASH_FALLBACK_ACTION = Object.freeze({kind: 'quit', label: 'Quit'});

let splashState = null;
let renderedLayout = null;
let renderedManualSignature = null;
let countdownTimer = null;
let selectedOption = null;
let splashStartedAt = Number.POSITIVE_INFINITY;
let diagnosticsRevealTimer = null;
let diagnosticsCopiedTimer = null;

function toCount(value) {
	if (typeof value !== 'number' || !Number.isFinite(value)) return null;
	return Math.max(0, Math.trunc(value));
}

function toText(value, maxLength) {
	if (typeof value !== 'string') return null;
	const trimmed = value.trim();
	if (trimmed.length === 0) return null;
	return trimmed.slice(0, maxLength);
}

function normalizeAction(value) {
	if (value == null || typeof value !== 'object') return null;
	if (typeof value.kind !== 'string' || !ACTION_CHANNELS.has(value.kind)) return null;
	const label = toText(value.label, SPLASH_ACTION_LABEL_MAX_LENGTH);
	if (label == null) return null;
	return {kind: value.kind, label};
}

function normalizeOption(value) {
	if (value == null || typeof value !== 'object') return null;
	if (typeof value.value !== 'string' || !SPLASH_OPTION_VALUE_PATTERN.test(value.value)) return null;
	if (typeof value.kind !== 'string' || !ACTION_CHANNELS.has(value.kind)) return null;
	const label = toText(value.label, SPLASH_OPTION_LABEL_MAX_LENGTH);
	if (label == null) return null;
	const buttonLabel = toText(value.buttonLabel, SPLASH_OPTION_BUTTON_LABEL_MAX_LENGTH);
	if (buttonLabel == null) return null;
	return {value: value.value, label, buttonLabel, kind: value.kind};
}

function normalizeOptions(value) {
	if (!Array.isArray(value)) return null;
	const accepted = [];
	for (const entry of value.slice(0, SPLASH_OPTION_MAX_COUNT)) {
		const option = normalizeOption(entry);
		if (option != null) {
			accepted.push(option);
		}
	}
	return accepted.length === 0 ? null : accepted;
}

function withManualUpdateAffordance(state) {
	if (state.layout !== SplashLayout.MANUAL_UPDATE) return state;
	if (state.options != null || state.action != null) return state;
	return {...state, action: SPLASH_FALLBACK_ACTION};
}

function normalizeState(payload) {
	if (payload == null || typeof payload !== 'object') {
		return {
			layout: SplashLayout.SPLASH,
			status: SplashStatus.CHECKING_FOR_UPDATES,
			requiredSecurityUpdate: false,
			current: null,
			total: null,
			progress: null,
			seconds: null,
			receivedBytes: null,
			totalBytes: null,
			bytesPerSecond: null,
			action: null,
			message: null,
			versionLabel: null,
			options: null,
		};
	}
	return withManualUpdateAffordance({
		layout: KNOWN_LAYOUTS.has(payload.layout) ? payload.layout : SplashLayout.SPLASH,
		status: KNOWN_STATUSES.has(payload.status) ? payload.status : SplashStatus.CHECKING_FOR_UPDATES,
		requiredSecurityUpdate: payload.requiredSecurityUpdate === true,
		current: toCount(payload.current),
		total: toCount(payload.total),
		progress: typeof payload.progress === 'number' && Number.isFinite(payload.progress) ? payload.progress : null,
		seconds: toCount(payload.seconds),
		receivedBytes: toCount(payload.receivedBytes),
		totalBytes: toCount(payload.totalBytes),
		bytesPerSecond: toCount(payload.bytesPerSecond),
		action: normalizeAction(payload.action),
		message: toText(payload.message, SPLASH_MESSAGE_MAX_LENGTH),
		versionLabel: toText(payload.versionLabel, SPLASH_VERSION_LABEL_MAX_LENGTH),
		options: normalizeOptions(payload.options),
	});
}

function getStatusText(state) {
	switch (state.status) {
		case SplashStatus.DOWNLOADING_UPDATES:
			return state.requiredSecurityUpdate
				? `Downloading required security update ${state.current ?? 0} of ${state.total ?? 0}…`
				: `Downloading update ${state.current ?? 0} of ${state.total ?? 0}…`;
		case SplashStatus.INSTALLING_UPDATES:
			return state.requiredSecurityUpdate
				? `Installing required security update ${state.current ?? 0} of ${state.total ?? 0}…`
				: `Installing update ${state.current ?? 0} of ${state.total ?? 0}…`;
		case SplashStatus.VERIFYING:
			return state.requiredSecurityUpdate ? 'Verifying required security update…' : 'Verifying files…';
		case SplashStatus.DOWNLOAD_STALLED:
			return `Download stalled. Retrying in ${state.seconds ?? 0} sec…`;
		case SplashStatus.UPDATE_FAILURE:
			return state.requiredSecurityUpdate
				? `Required security update failed. Retrying in ${state.seconds ?? 0} sec…`
				: `Update failed. Retrying in ${state.seconds ?? 0} sec…`;
		case SplashStatus.SHELL_UPDATE_DOWNLOADING:
			return state.requiredSecurityUpdate ? 'Downloading required security update…' : 'Updating Fluxer…';
		case SplashStatus.SHELL_UPDATE_RESTARTING:
			return state.requiredSecurityUpdate
				? 'Restarting to finish the required security update…'
				: 'Restarting to finish the update…';
		case SplashStatus.BLOCKED_UPDATE_REQUIRED:
			return state.requiredSecurityUpdate
				? 'A required security update is needed to continue'
				: 'Update required to continue';
		case SplashStatus.BLOCKED_UPDATE_UNREACHABLE:
			return "Can't reach the Fluxer update server";
		case SplashStatus.BLOCKED_SECURITY_UPDATE_REQUIRED:
			return 'A required security update is needed to continue';
		case SplashStatus.BLOCKED_SECURITY_UPDATE_MANAGED:
			return 'Install the required security update through your package manager';
		case SplashStatus.BLOCKED_SHELL_UPDATE:
			return state.requiredSecurityUpdate
				? 'A required security update is needed to continue'
				: 'A new version of Fluxer is required';
		case SplashStatus.BLOCKED_SHELL_UPDATE_MANAGED:
			return 'Update Fluxer through your package manager';
		case SplashStatus.BLOCKED_UNSUPPORTED_BUILD:
			return 'This installation is incomplete. Reinstall Fluxer';
		case SplashStatus.LAUNCHING:
			return 'Starting…';
		case SplashStatus.UNREACHABLE_LAUNCH:
			return 'Starting offline…';
		case SplashStatus.WAITING_LOCAL_NETWORK:
			return 'Waiting for local network permission…';
		default:
			return state.requiredSecurityUpdate ? 'Checking for required security update…' : 'Checking for updates…';
	}
}

function shouldShowProgress(state) {
	return state.progress !== null && PROGRESS_STATUSES.has(state.status);
}

function toProgressPercent(state) {
	if (state.receivedBytes !== null && state.totalBytes !== null && state.totalBytes > 0) {
		return Math.min(100, Math.max(0, (state.receivedBytes / state.totalBytes) * 100));
	}
	if (state.progress === null) return 0;
	return Math.min(100, Math.max(0, state.progress));
}

function formatMegabytes(bytes) {
	const megabytes = bytes / BYTES_PER_MEGABYTE;
	return megabytes >= 10 ? String(Math.round(megabytes)) : megabytes.toFixed(1);
}

function formatSpeed(bytesPerSecond) {
	if (bytesPerSecond >= BYTES_PER_MEGABYTE) {
		return `${(bytesPerSecond / BYTES_PER_MEGABYTE).toFixed(1)} MB/s`;
	}
	return `${Math.max(0, Math.round(bytesPerSecond / BYTES_PER_KILOBYTE))} KB/s`;
}

function getMetricsText(state) {
	if (state.status !== SplashStatus.DOWNLOADING_UPDATES && state.status !== SplashStatus.DOWNLOAD_STALLED) return '';
	if (state.receivedBytes === null || state.totalBytes === null || state.totalBytes === 0) return '';
	const amount = `${formatMegabytes(state.receivedBytes)} of ${formatMegabytes(state.totalBytes)} MB`;
	if (state.status !== SplashStatus.DOWNLOADING_UPDATES || state.bytesPerSecond === null) return amount;
	return `${amount}, ${formatSpeed(state.bytesPerSecond)}`;
}

function shouldShowDiagnostics(state) {
	if (state.layout !== SplashLayout.SPLASH) return false;
	if (DIAGNOSTICS_HIDDEN_STATUSES.has(state.status)) return false;
	if (DIAGNOSTICS_STATUSES.has(state.status)) return true;
	return Date.now() - splashStartedAt >= DIAGNOSTICS_REVEAL_MS;
}

function createNode(tagName, className) {
	const node = document.createElement(tagName);
	if (className != null) {
		node.className = className;
	}
	return node;
}

function createMark(className) {
	const mark = createNode('img', className);
	mark.setAttribute('src', SPLASH_MARK_SOURCE);
	mark.setAttribute('alt', '');
	mark.setAttribute('width', String(SPLASH_MARK_SIZE));
	mark.setAttribute('height', String(SPLASH_MARK_SIZE));
	return mark;
}

function createProgress(percent) {
	const progress = createNode('div', 'progress');
	const bar = createNode('div', 'progress-bar');
	const complete = createNode('div', 'complete');
	complete.style.width = `${percent}%`;
	bar.appendChild(complete);
	progress.appendChild(bar);
	return progress;
}

function createProgressPlaceholder() {
	const placeholder = createNode('div', 'progress-placeholder');
	placeholder.textContent = NON_BREAKING_SPACE;
	return placeholder;
}

function sendSplashAction(kind, value) {
	const channel = ACTION_CHANNELS.get(kind);
	if (channel == null) return;
	if (value === undefined) {
		ipcRenderer.send(channel);
		return;
	}
	ipcRenderer.send(channel, value);
}

function onSplashActionClick() {
	if (splashState == null || splashState.action == null) return;
	sendSplashAction(splashState.action.kind, undefined);
}

function buildSplashLayout(mount) {
	const splash = createNode('div');
	splash.id = 'splash';
	const inner = createNode('div', 'splash-inner');
	const media = createNode('div', 'splash-media');
	media.appendChild(createMark('splash-mark'));
	const text = createNode('div', 'splash-text');
	text.appendChild(createNode('span', 'splash-status'));
	text.appendChild(createNode('span', 'splash-detail'));
	text.appendChild(createProgressPlaceholder());
	text.appendChild(createNode('span', 'splash-metrics'));
	text.appendChild(createNode('div', 'splash-action-slot'));
	inner.appendChild(media);
	inner.appendChild(text);
	splash.appendChild(inner);
	splash.appendChild(buildDiagnosticsRow());
	mount.replaceChildren(splash);
}

function buildDiagnosticsRow() {
	const row = createNode('div', 'splash-diagnostics');
	const openLogs = createNode('button', 'splash-diagnostics-link');
	openLogs.setAttribute('data-action', 'open-logs');
	openLogs.textContent = 'Open logs';
	openLogs.addEventListener('click', () => {
		ipcRenderer.send(SPLASH_OPEN_LOGS_CHANNEL);
	});
	const copy = createNode('button', 'splash-diagnostics-link');
	copy.setAttribute('data-action', 'copy-diagnostics');
	copy.textContent = 'Copy diagnostics';
	copy.addEventListener('click', () => {
		ipcRenderer.send(SPLASH_COPY_DIAGNOSTICS_CHANNEL);
		copy.textContent = 'Copied';
		if (diagnosticsCopiedTimer != null) clearTimeout(diagnosticsCopiedTimer);
		diagnosticsCopiedTimer = setTimeout(() => {
			diagnosticsCopiedTimer = null;
			copy.textContent = 'Copy diagnostics';
		}, DIAGNOSTICS_COPIED_MS);
	});
	row.appendChild(openLogs);
	row.appendChild(copy);
	return row;
}

function reconcileDiagnostics(mount, state) {
	const row = mount.querySelector('.splash-diagnostics');
	if (row == null) return;
	if (shouldShowDiagnostics(state)) {
		row.classList.add('is-visible');
	} else {
		row.classList.remove('is-visible');
	}
}

function reconcileProgress(mount, state) {
	const existing = mount.querySelector('.progress, .progress-placeholder');
	if (existing == null) return;
	if (shouldShowProgress(state)) {
		const percent = toProgressPercent(state);
		const complete = existing.querySelector('.complete');
		if (complete != null) {
			complete.style.width = `${percent}%`;
			return;
		}
		existing.replaceWith(createProgress(percent));
		return;
	}
	if (existing.classList.contains('progress-placeholder')) return;
	existing.replaceWith(createProgressPlaceholder());
}

function reconcileAction(mount, state) {
	const slot = mount.querySelector('.splash-action-slot');
	if (slot == null) return;
	const existing = slot.querySelector('.splash-action');
	if (state.action == null) {
		if (existing != null) {
			slot.replaceChildren();
		}
		return;
	}
	if (existing == null) {
		const button = createNode('button', 'splash-action');
		button.textContent = state.action.label;
		button.addEventListener('click', onSplashActionClick);
		slot.appendChild(button);
		return;
	}
	existing.textContent = state.action.label;
}

function findSelectedOption() {
	if (splashState == null || splashState.options == null) return null;
	return splashState.options.find((option) => option.value === selectedOption) ?? null;
}

function manualUpdateSignature(state) {
	return JSON.stringify([state.action, state.options]);
}

function patchManualUpdateText(mount, state) {
	const message = mount.querySelector('.dl-update-message');
	if (message != null) {
		message.textContent = state.message ?? '';
	}
	const version = mount.querySelector('.dl-version-message');
	if (version != null) {
		version.textContent = state.versionLabel ?? '';
	}
}

function buildManualUpdateLayout(mount, state) {
	const splash = createNode('div');
	splash.id = 'splash';
	const inner = createNode('div', 'splash-inner-dl');
	const markBox = createNode('div', 'splash-mark-dl');
	markBox.appendChild(createMark(null));
	const message = createNode('div', 'dl-update-message');
	message.textContent = state.message ?? '';
	const version = createNode('div', 'dl-version-message');
	version.textContent = state.versionLabel ?? '';
	inner.appendChild(markBox);
	inner.appendChild(message);
	inner.appendChild(buildManualUpdateControl(state));
	inner.appendChild(version);
	splash.appendChild(inner);
	mount.replaceChildren(splash);
}

function buildManualUpdateControl(state) {
	return state.options == null ? buildManualUpdateAction(state) : buildManualUpdatePicker(state);
}

function buildManualUpdateAction(state) {
	const frame = createNode('div', 'dl-action-frame');
	const button = createNode('button', 'dl-button');
	button.id = 'dl-button';
	button.textContent = state.action.label;
	button.addEventListener('click', onSplashActionClick);
	frame.appendChild(button);
	return frame;
}

function buildManualUpdatePicker(state) {
	const frame = createNode('div', 'dl-select-frame');
	const selectBox = createNode('div', 'dl-select');
	const select = createNode('select');
	select.id = 'dl-select-input';
	for (const option of state.options) {
		const node = createNode('option');
		node.value = option.value;
		node.textContent = option.label;
		select.appendChild(node);
	}
	selectBox.appendChild(select);
	const button = createNode('button', 'dl-button');
	button.id = 'dl-button';
	if (findSelectedOption() == null) {
		selectedOption = state.options[0].value;
	}
	const initial = findSelectedOption();
	select.value = selectedOption;
	button.textContent = initial == null ? '' : initial.buttonLabel;
	select.addEventListener('change', () => {
		selectedOption = select.value;
		const option = findSelectedOption();
		button.textContent = option == null ? '' : option.buttonLabel;
	});
	button.addEventListener('click', () => {
		const option = findSelectedOption();
		if (option == null) return;
		sendSplashAction(option.kind, option.value);
	});
	frame.appendChild(selectBox);
	frame.appendChild(button);
	return frame;
}

function render() {
	const mount = document.getElementById(SPLASH_MOUNT_ID);
	if (mount == null) return;
	const state = splashState ?? normalizeState(null);
	if (state.layout === SplashLayout.MANUAL_UPDATE) {
		const signature = manualUpdateSignature(state);
		if (renderedLayout !== SplashLayout.MANUAL_UPDATE || renderedManualSignature !== signature) {
			renderedLayout = SplashLayout.MANUAL_UPDATE;
			renderedManualSignature = signature;
			buildManualUpdateLayout(mount, state);
			return;
		}
		patchManualUpdateText(mount, state);
		return;
	}
	if (renderedLayout !== SplashLayout.SPLASH) {
		renderedLayout = SplashLayout.SPLASH;
		renderedManualSignature = null;
		buildSplashLayout(mount);
	}
	const statusNode = mount.querySelector('.splash-status');
	if (statusNode != null) {
		statusNode.textContent = getStatusText(state);
	}
	const detailNode = mount.querySelector('.splash-detail');
	if (detailNode != null) {
		detailNode.textContent = state.message ?? '';
	}
	const metricsNode = mount.querySelector('.splash-metrics');
	if (metricsNode != null) {
		metricsNode.textContent = getMetricsText(state) || NON_BREAKING_SPACE;
	}
	reconcileProgress(mount, state);
	reconcileAction(mount, state);
	reconcileDiagnostics(mount, state);
}

function hasCountdown(state) {
	return state.layout === SplashLayout.SPLASH && state.seconds != null && state.seconds > 0;
}

function stopCountdown() {
	if (countdownTimer == null) return;
	clearInterval(countdownTimer);
	countdownTimer = null;
}

function tickCountdown() {
	if (splashState == null || !hasCountdown(splashState)) {
		stopCountdown();
		return;
	}
	splashState = {...splashState, seconds: splashState.seconds - 1};
	if (!hasCountdown(splashState)) {
		stopCountdown();
	}
	render();
}

function syncCountdown(state) {
	stopCountdown();
	if (!hasCountdown(state)) return;
	countdownTimer = setInterval(tickCountdown, COUNTDOWN_INTERVAL_MS);
}

ipcRenderer.on(SPLASH_STATE_CHANNEL, (_event, payload) => {
	splashState = normalizeState(payload);
	syncCountdown(splashState);
	render();
});

function waitForBranding(mount) {
	const pending = [];
	const mark = mount.querySelector('.splash-mark');
	if (mark != null && typeof mark.decode === 'function') {
		pending.push(mark.decode().catch(() => undefined));
	}
	const fonts = document.fonts;
	if (fonts != null && typeof fonts.load === 'function') {
		for (const face of SPLASH_FONT_FACES) {
			pending.push(fonts.load(face).catch(() => undefined));
		}
	}
	return Promise.race([
		Promise.all(pending),
		new Promise((resolve) => {
			setTimeout(resolve, SPLASH_BRANDING_WAIT_MS);
		}),
	]);
}

function afterNextPaint(callback) {
	window.requestAnimationFrame(() => {
		window.requestAnimationFrame(callback);
	});
}

function restartDiagnosticsReveal() {
	splashStartedAt = Date.now();
	if (diagnosticsRevealTimer != null) clearTimeout(diagnosticsRevealTimer);
	diagnosticsRevealTimer = setTimeout(() => {
		diagnosticsRevealTimer = null;
		render();
	}, DIAGNOSTICS_REVEAL_MS);
}

ipcRenderer.on(SPLASH_REVEALED_CHANNEL, () => {
	restartDiagnosticsReveal();
	render();
});

window.addEventListener('DOMContentLoaded', () => {
	if (!/(?:^|[?&])held=1(?:&|$)/.test(window.location?.search ?? '')) {
		restartDiagnosticsReveal();
	}
	render();
	const mount = document.getElementById(SPLASH_MOUNT_ID);
	const branded = mount == null ? Promise.resolve() : waitForBranding(mount);
	void branded.then(() => {
		afterNextPaint(() => {
			ipcRenderer.send(SPLASH_READY_CHANNEL);
		});
	});
});

window.addEventListener('online', () => {
	ipcRenderer.send(SPLASH_NETWORK_ONLINE_CHANNEL);
});

window.addEventListener('beforeunload', () => {
	stopCountdown();
	if (diagnosticsRevealTimer != null) clearTimeout(diagnosticsRevealTimer);
	if (diagnosticsCopiedTimer != null) clearTimeout(diagnosticsCopiedTimer);
});
