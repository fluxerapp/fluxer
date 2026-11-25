// SPDX-License-Identifier: AGPL-3.0-or-later

const {ipcRenderer} = require('electron');

const SPLASH_STATE_CHANNEL = 'desktop-splash:state';
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
const DIAGNOSTICS_REVEAL_MS = 30000;
const DIAGNOSTICS_COPIED_MS = 2000;
const BYTES_PER_MEGABYTE = 1000000;
const BYTES_PER_KILOBYTE = 1000;

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
	BLOCKED_SHELL_UPDATE: 'blocked-shell-update',
	BLOCKED_UNSUPPORTED_BUILD: 'blocked-unsupported-build',
	LAUNCHING: 'launching',
	UNREACHABLE_LAUNCH: 'unreachable-launch',
	WAITING_LOCAL_NETWORK: 'waiting-local-network',
});

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
const DEFAULT_STRINGS = Object.freeze({
	windowTitle: 'Fluxer Updater',
	checkingForUpdates: 'Checking for updates…',
	downloadingUpdates: 'Downloading update {current} of {total}…',
	installingUpdates: 'Installing update {current} of {total}…',
	verifying: 'Verifying files…',
	downloadStalled: 'Download stalled. Retrying in {seconds} sec…',
	updateFailed: 'Update failed. Retrying in {seconds} sec…',
	shellUpdateDownloading: 'Updating Fluxer…',
	shellUpdateRestarting: 'Restarting to finish the update…',
	blockedUpdateRequired: 'Update required to continue',
	blockedUpdateUnreachable: "Can't reach the Fluxer update server",
	blockedShellUpdate: 'A new version of Fluxer is required',
	blockedUnsupportedBuild: 'This installation is incomplete. Reinstall Fluxer',
	launching: 'Starting…',
	unreachableLaunch: 'Starting offline…',
	waitingLocalNetwork: 'Waiting for local network permission…',
	downloadAmount: '{received} of {total} MB',
	downloadAmountWithSpeed: '{received} of {total} MB, {speed}',
	speedMegabytes: '{value} MB/s',
	speedKilobytes: '{value} KB/s',
	openLogs: 'Open logs',
	copyDiagnostics: 'Copy diagnostics',
	copied: 'Copied',
});
const PLACEHOLDER_PATTERN = /\{(\w+)\}/g;
const DEFAULT_STATE = Object.freeze({
	status: SplashStatus.CHECKING_FOR_UPDATES,
	current: null,
	total: null,
	progress: null,
	seconds: null,
	receivedBytes: null,
	totalBytes: null,
	bytesPerSecond: null,
	action: null,
	message: null,
	locale: 'en-US',
	strings: DEFAULT_STRINGS,
});

let splashState = null;
let splashBuilt = false;
let countdownTimer = null;
let splashStartedAt = Number.POSITIVE_INFINITY;
let diagnosticsRevealTimer = null;
let diagnosticsCopiedTimer = null;
let diagnosticsCopied = false;

function getString(state, name) {
	const strings = state.strings ?? DEFAULT_STRINGS;
	const value = strings[name];
	return typeof value === 'string' && value.length > 0 ? value : DEFAULT_STRINGS[name];
}

function formatString(state, name, values) {
	return getString(state, name).replace(PLACEHOLDER_PATTERN, (match, key) =>
		Object.hasOwn(values, key) ? values[key] : match,
	);
}

function createNumberFormat(state, options) {
	try {
		return new Intl.NumberFormat(state.locale, options);
	} catch {
		return new Intl.NumberFormat(undefined, options);
	}
}

function formatInteger(state, value) {
	return createNumberFormat(state, {maximumFractionDigits: 0}).format(value ?? 0);
}

function getStatusText(state) {
	switch (state.status) {
		case SplashStatus.DOWNLOADING_UPDATES:
			return formatString(state, 'downloadingUpdates', {
				current: formatInteger(state, state.current),
				total: formatInteger(state, state.total),
			});
		case SplashStatus.INSTALLING_UPDATES:
			return formatString(state, 'installingUpdates', {
				current: formatInteger(state, state.current),
				total: formatInteger(state, state.total),
			});
		case SplashStatus.VERIFYING:
			return getString(state, 'verifying');
		case SplashStatus.DOWNLOAD_STALLED:
			return formatString(state, 'downloadStalled', {seconds: formatInteger(state, state.seconds)});
		case SplashStatus.UPDATE_FAILURE:
			return formatString(state, 'updateFailed', {seconds: formatInteger(state, state.seconds)});
		case SplashStatus.SHELL_UPDATE_DOWNLOADING:
			return getString(state, 'shellUpdateDownloading');
		case SplashStatus.SHELL_UPDATE_RESTARTING:
			return getString(state, 'shellUpdateRestarting');
		case SplashStatus.BLOCKED_UPDATE_REQUIRED:
			return getString(state, 'blockedUpdateRequired');
		case SplashStatus.BLOCKED_UPDATE_UNREACHABLE:
			return getString(state, 'blockedUpdateUnreachable');
		case SplashStatus.BLOCKED_SHELL_UPDATE:
			return getString(state, 'blockedShellUpdate');
		case SplashStatus.BLOCKED_UNSUPPORTED_BUILD:
			return getString(state, 'blockedUnsupportedBuild');
		case SplashStatus.LAUNCHING:
			return getString(state, 'launching');
		case SplashStatus.UNREACHABLE_LAUNCH:
			return getString(state, 'unreachableLaunch');
		case SplashStatus.WAITING_LOCAL_NETWORK:
			return getString(state, 'waitingLocalNetwork');
		default:
			return getString(state, 'checkingForUpdates');
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

function formatOneDecimal(state, value) {
	return createNumberFormat(state, {minimumFractionDigits: 1, maximumFractionDigits: 1}).format(value);
}

function formatMegabytes(state, bytes) {
	const megabytes = bytes / BYTES_PER_MEGABYTE;
	return megabytes >= 10 ? formatInteger(state, Math.round(megabytes)) : formatOneDecimal(state, megabytes);
}

function formatSpeed(state, bytesPerSecond) {
	if (bytesPerSecond >= BYTES_PER_MEGABYTE) {
		return formatString(state, 'speedMegabytes', {value: formatOneDecimal(state, bytesPerSecond / BYTES_PER_MEGABYTE)});
	}
	return formatString(state, 'speedKilobytes', {
		value: formatInteger(state, Math.max(0, Math.round(bytesPerSecond / BYTES_PER_KILOBYTE))),
	});
}

function getMetricsText(state) {
	if (state.status !== SplashStatus.DOWNLOADING_UPDATES && state.status !== SplashStatus.DOWNLOAD_STALLED) return '';
	if (state.receivedBytes === null || state.totalBytes === null || state.totalBytes === 0) return '';
	const received = formatMegabytes(state, state.receivedBytes);
	const total = formatMegabytes(state, state.totalBytes);
	if (state.status !== SplashStatus.DOWNLOADING_UPDATES || state.bytesPerSecond === null) {
		return formatString(state, 'downloadAmount', {received, total});
	}
	return formatString(state, 'downloadAmountWithSpeed', {
		received,
		total,
		speed: formatSpeed(state, state.bytesPerSecond),
	});
}

function shouldShowDiagnostics(state) {
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

function sendSplashAction(kind) {
	const channel = ACTION_CHANNELS.get(kind);
	if (channel == null) return;
	ipcRenderer.send(channel);
}

function onSplashActionClick() {
	if (splashState == null || splashState.action == null) return;
	sendSplashAction(splashState.action.kind);
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
	openLogs.textContent = getString(DEFAULT_STATE, 'openLogs');
	openLogs.addEventListener('click', () => {
		ipcRenderer.send(SPLASH_OPEN_LOGS_CHANNEL);
	});
	const copy = createNode('button', 'splash-diagnostics-link');
	copy.setAttribute('data-action', 'copy-diagnostics');
	copy.textContent = getString(DEFAULT_STATE, 'copyDiagnostics');
	copy.addEventListener('click', () => {
		ipcRenderer.send(SPLASH_COPY_DIAGNOSTICS_CHANNEL);
		diagnosticsCopied = true;
		render();
		if (diagnosticsCopiedTimer != null) clearTimeout(diagnosticsCopiedTimer);
		diagnosticsCopiedTimer = setTimeout(() => {
			diagnosticsCopiedTimer = null;
			diagnosticsCopied = false;
			render();
		}, DIAGNOSTICS_COPIED_MS);
	});
	row.appendChild(openLogs);
	row.appendChild(copy);
	return row;
}

function reconcileDiagnostics(mount, state) {
	const row = mount.querySelector('.splash-diagnostics');
	if (row == null) return;
	const openLogs = row.querySelector('[data-action="open-logs"]');
	if (openLogs != null) openLogs.textContent = getString(state, 'openLogs');
	const copy = row.querySelector('[data-action="copy-diagnostics"]');
	if (copy != null) copy.textContent = getString(state, diagnosticsCopied ? 'copied' : 'copyDiagnostics');
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

function render() {
	const mount = document.getElementById(SPLASH_MOUNT_ID);
	if (mount == null) return;
	const state = splashState ?? DEFAULT_STATE;
	document.documentElement.lang = typeof state.locale === 'string' ? state.locale : DEFAULT_STATE.locale;
	document.title = getString(state, 'windowTitle');
	if (!splashBuilt) {
		splashBuilt = true;
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
	return state.seconds != null && state.seconds > 0;
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
	splashState = payload ?? DEFAULT_STATE;
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

window.addEventListener('DOMContentLoaded', () => {
	restartDiagnosticsReveal();
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
