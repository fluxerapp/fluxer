// SPDX-License-Identifier: AGPL-3.0-or-later

const {EventEmitter} = require('node:events');
const {existsSync, readdirSync, readFileSync} = require('node:fs');
const {join, sep} = require('node:path');
const {createNativeLoadError, loadNativeBinding} = require('./loader-diagnostics.cjs');
const MODULE_NAME = '@fluxer/linux-audio-capture';
const SKIP_NATIVE_PROBE_ENV = 'FLUXER_LINUX_AUDIO_CAPTURE_SKIP_NATIVE_PROBE';

function resolveNativeRoot() {
	const asarSegment = `${sep}app.asar${sep}`;
	if (!__dirname.includes(asarSegment)) return __dirname;
	const unpackedDir = __dirname.replace(asarSegment, `${sep}app.asar.unpacked${sep}`);
	return existsSync(unpackedDir) ? unpackedDir : __dirname;
}

function nativeFileName() {
	if (process.platform !== 'linux') {
		throw new Error(`@fluxer/linux-audio-capture is only supported on Linux, got ${process.platform}`);
	}
	switch (process.arch) {
		case 'x64':
			return 'linux-audio-capture.linux-x64-gnu.node';
		case 'arm64':
			return 'linux-audio-capture.linux-arm64-gnu.node';
		default:
			throw new Error(`Unsupported Linux architecture: ${process.arch}`);
	}
}

let binding;

try {
	const nativeRoot = resolveNativeRoot();
	const nativePath = join(nativeRoot, nativeFileName());
	const loadedNative = loadNativeBinding({
		moduleName: MODULE_NAME,
		nativePath,
		nativeRoot,
		packageDir: __dirname,
		skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
	});
	if (loadedNative.loadError) {
		throw loadedNative.loadError;
	}
	binding = loadedNative.binding;
} catch (error) {
	throw createNativeLoadError({
		moduleName: MODULE_NAME,
		nativeRoot: resolveNativeRoot(),
		packageDir: __dirname,
		reason: 'native loader threw before binding load completed',
		cause: error,
		skipNativeProbeEnv: SKIP_NATIVE_PROBE_ENV,
	});
}

function normalizeRoutingRule(rule) {
	if (!rule || typeof rule !== 'object') return {};
	const normalizeList = (value) =>
		Array.isArray(value)
			? value
					.filter((entry) => entry && typeof entry === 'object' && !Array.isArray(entry))
					.map((entry) =>
						Object.fromEntries(
							Object.entries(entry)
								.filter(([, v]) => typeof v === 'string' || typeof v === 'number' || typeof v === 'boolean')
								.map(([k, v]) => [k, String(v)]),
						),
					)
			: undefined;
	return {
		include: normalizeList(rule.include),
		exclude: normalizeList(rule.exclude),
		workaround: normalizeList(rule.workaround),
		ignoreDevices: rule.ignoreDevices ?? rule.ignore_devices,
		onlySpeakers: rule.onlySpeakers ?? rule.only_speakers,
		onlyDefaultSpeakers: rule.onlyDefaultSpeakers ?? rule.only_default_speakers,
	};
}

function readProcParentMap() {
	const parents = new Map();
	let entries = [];
	try {
		entries = readdirSync('/proc', {withFileTypes: true});
	} catch {
		return parents;
	}
	for (const entry of entries) {
		if (!entry.isDirectory() || !/^\d+$/.test(entry.name)) continue;
		try {
			const stat = readFileSync(`/proc/${entry.name}/stat`, 'utf8');
			const end = stat.lastIndexOf(')');
			if (end < 0) continue;
			const fields = stat
				.slice(end + 1)
				.trim()
				.split(/\s+/);
			const parentPid = Number(fields[1]);
			if (Number.isSafeInteger(parentPid) && parentPid > 0) {
				parents.set(Number(entry.name), parentPid);
			}
		} catch {}
	}
	return parents;
}

function isDescendantPid(pid, rootPid, parents) {
	let current = pid;
	const seen = new Set();
	while (parents.has(current) && !seen.has(current)) {
		seen.add(current);
		const parent = parents.get(current);
		if (parent === rootPid) return true;
		current = parent;
	}
	return false;
}

function targetPidList(pid, includeProcessTree) {
	if (!includeProcessTree) return [pid];
	const parents = readProcParentMap();
	const pids = [pid];
	for (const candidate of parents.keys()) {
		if (candidate !== pid && isDescendantPid(candidate, pid, parents)) {
			pids.push(candidate);
		}
	}
	return pids;
}

function appendUniquePattern(patterns, pattern) {
	if (!pattern || typeof pattern !== 'object') return;
	const entries = Object.entries(pattern).filter(([, value]) => typeof value === 'string' && value.length > 0);
	if (entries.length === 0) return;
	const normalized = Object.fromEntries(entries);
	const key = JSON.stringify(Object.entries(normalized).sort(([a], [b]) => a.localeCompare(b)));
	if (
		patterns.some((existing) => JSON.stringify(Object.entries(existing).sort(([a], [b]) => a.localeCompare(b))) === key)
	) {
		return;
	}
	patterns.push(normalized);
}

function inventoryPatternsForTargetPids(pids) {
	if (!(binding && typeof binding.AudioBridge === 'function')) return [];
	const wanted = new Set(pids.map((pid) => String(pid)));
	const patterns = [];
	let bridge = null;
	try {
		bridge = new binding.AudioBridge();
		const inventory = bridge.inventory();
		if (!Array.isArray(inventory)) return patterns;
		for (const props of inventory) {
			if (!props || typeof props !== 'object') continue;
			if (props['media.class'] !== 'Stream/Output/Audio') continue;
			const processId = props['application.process.id'] || props['pipewire.sec.pid'];
			if (!wanted.has(String(processId || ''))) continue;
			appendUniquePattern(patterns, {'object.serial': String(props['object.serial'] || '')});
			appendUniquePattern(patterns, {'node.name': String(props['node.name'] || '')});
			appendUniquePattern(patterns, {'client.id': String(props['client.id'] || '')});
		}
	} catch {
	} finally {
		try {
			bridge?.release?.();
		} catch {}
	}
	return patterns;
}

function routingRuleFromTarget(target, options) {
	if (target && typeof target === 'object') {
		return normalizeRoutingRule(target.linuxRule || target);
	}
	const pid = Number(target);
	if (!Number.isSafeInteger(pid) || pid <= 0) {
		throw new TypeError('ProcessLoopback target pid must be a positive integer');
	}
	const targetPids = targetPidList(pid, Boolean(options?.includeProcessTree));
	const include = inventoryPatternsForTargetPids(targetPids);
	for (const candidate of targetPids) {
		appendUniquePattern(include, {'application.process.id': String(candidate)});
		appendUniquePattern(include, {'pipewire.sec.pid': String(candidate)});
	}
	return normalizeRoutingRule({
		include,
		ignoreDevices: options?.ignoreDevices ?? true,
	});
}

const LATE_SPAWN_REFRESH_INTERVAL_MS = 2_000;
const MAX_DRAIN_FRAMES_PER_TICK = 16;
const MAX_IDLE_DIRECT_CAPTURES = 2;
const MIX_TICK_PERIOD_MS = 20;

let idleDirectCaptures = [];

function acquireDirectAudioCapture() {
	if (typeof binding.DirectAudioCapture !== 'function') {
		throw new Error('DirectAudioCapture native export missing');
	}
	const pooled = idleDirectCaptures.pop();
	return pooled ?? new binding.DirectAudioCapture();
}

function releaseDirectAudioCapture(capture) {
	if (!capture || idleDirectCaptures.includes(capture)) return;
	if (idleDirectCaptures.length < MAX_IDLE_DIRECT_CAPTURES) {
		idleDirectCaptures.push(capture);
	}
}

function clearIdleDirectCapturePool() {
	idleDirectCaptures = [];
}

function patternsEqual(a, b) {
	if (a === b) return true;
	if (!Array.isArray(a) || !Array.isArray(b)) return false;
	if (a.length !== b.length) return false;
	const serialize = (entry) =>
		JSON.stringify(
			Object.entries(entry)
				.filter(([, v]) => typeof v === 'string')
				.sort(([x], [y]) => x.localeCompare(y)),
		);
	const aSet = a.map(serialize).sort();
	const bSet = b.map(serialize).sort();
	for (let i = 0; i < aSet.length; i++) {
		if (aSet[i] !== bSet[i]) return false;
	}
	return true;
}

function routingRulesEqual(a, b) {
	if (a === b) return true;
	if (!a || !b) return false;
	if (!patternsEqual(a.include, b.include)) return false;
	if (!patternsEqual(a.exclude, b.exclude)) return false;
	if (!patternsEqual(a.workaround, b.workaround)) return false;
	return (
		Boolean(a.ignoreDevices) === Boolean(b.ignoreDevices) &&
		Boolean(a.onlySpeakers) === Boolean(b.onlySpeakers) &&
		Boolean(a.onlyDefaultSpeakers) === Boolean(b.onlyDefaultSpeakers)
	);
}

class ProcessLoopback extends EventEmitter {
	constructor(target, options = {}) {
		super();
		this.capture = acquireDirectAudioCapture();
		if (typeof this.capture.setLifecycleCallback === 'function') {
			this.capture.setLifecycleCallback((type, message) => this.handleNativeLifecycle(type, message));
		}
		this.targetPid = null;
		this.includeProcessTree = false;
		if (!(target && typeof target === 'object')) {
			const pid = Number(target);
			if (Number.isSafeInteger(pid) && pid > 0) {
				this.targetPid = pid;
				this.includeProcessTree = Boolean(options?.includeProcessTree);
			}
		}
		this.rule = routingRuleFromTarget(target, options);
		this.options = options;
		this.timer = null;
		this.refreshTimer = null;
		this.closed = false;
		this.started = false;
	}

	handleNativeLifecycle(type, message) {
		if (type === 'error') {
			this.emit('error', new Error(message || 'Linux direct audio capture stopped'));
			if (!this.closed) void this.stop();
			return;
		}
		if (type === 'closed' || type === 'closed-clean') {
			if (!this.closed) void this.stop();
			return;
		}
		if (type === 'diagnostic') {
			this.emit('diagnostic', message || '');
		}
	}

	start() {
		if (this.closed) {
			throw new Error('ProcessLoopback already closed');
		}
		if (this.started) return;
		if (!this.capture.start(this.rule)) {
			throw new Error('failed to start Linux direct audio capture');
		}
		this.started = true;
		this.timer = setInterval(() => this.tick(), MIX_TICK_PERIOD_MS);
		this.timer.unref?.();
		if (this.targetPid !== null && this.includeProcessTree && typeof this.capture.setRule === 'function') {
			this.refreshTimer = setInterval(() => this.refreshRuleForLateChildren(), LATE_SPAWN_REFRESH_INTERVAL_MS);
			this.refreshTimer.unref?.();
		}
	}

	setRoutingRule(target, options = {}) {
		if (this.closed || !this.started) return false;
		if (typeof this.capture?.setRule !== 'function') return false;
		let nextRule;
		try {
			nextRule = routingRuleFromTarget(target, options);
		} catch {
			return false;
		}
		if (routingRulesEqual(nextRule, this.rule)) return true;
		let applied = false;
		try {
			applied = this.capture.setRule(nextRule) !== false;
		} catch {
			return false;
		}
		if (!applied) return false;
		this.rule = nextRule;
		this.options = options;
		return true;
	}

	refreshRuleForLateChildren() {
		if (this.closed || !this.started || this.targetPid === null) return;
		let nextRule;
		try {
			nextRule = routingRuleFromTarget(this.targetPid, {
				...(this.options ?? {}),
				includeProcessTree: this.includeProcessTree,
			});
		} catch {
			return;
		}
		if (patternsEqual(nextRule.include, this.rule.include) && patternsEqual(nextRule.exclude, this.rule.exclude)) {
			return;
		}
		this.rule = nextRule;
		try {
			this.capture.setRule(this.rule);
		} catch {}
	}

	tick() {
		if (this.closed || !this.started) return;
		try {
			this.drainCaptureFrames();
		} catch (error) {
			this.emit('error', error instanceof Error ? error : new Error(String(error)));
			void this.stop();
		}
	}

	drainCaptureFrames() {
		for (let i = 0; i < MAX_DRAIN_FRAMES_PER_TICK; i++) {
			const frame = this.capture.read();
			if (!frame || !(frame.samples instanceof ArrayBuffer) || frame.samples.byteLength === 0) return;
			this.emit('frame', {
				samples: new Float32Array(frame.samples),
				sampleRate: frame.sampleRate,
				channels: frame.channels,
				timestampUs: frame.timestampUs,
			});
		}
	}

	routingGraph() {
		return this.capture && typeof this.capture.routingGraph === 'function' ? this.capture.routingGraph() : null;
	}

	setScreenAudioSink(handle) {
		if (!this.capture || typeof this.capture.setScreenAudioSink !== 'function') return false;
		try {
			return this.capture.setScreenAudioSink(handle) !== false;
		} catch {
			return false;
		}
	}

	clearScreenAudioSink() {
		if (this.capture && typeof this.capture.clearScreenAudioSink === 'function') {
			this.capture.clearScreenAudioSink();
		}
	}

	async stop() {
		if (this.closed) return;
		this.closed = true;
		this.clearScreenAudioSink();
		if (this.timer) clearInterval(this.timer);
		this.timer = null;
		if (this.refreshTimer) clearInterval(this.refreshTimer);
		this.refreshTimer = null;
		const wasStarted = this.started;
		this.started = false;
		const capture = this.capture;
		this.capture = null;
		let stopped = false;
		try {
			capture?.stop();
			stopped = true;
		} finally {
			if (stopped && wasStarted) {
				releaseDirectAudioCapture(capture);
			}
			this.emit('closed');
		}
	}
}

module.exports = {
	AudioBridge: binding.AudioBridge,
	DirectAudioCapture: binding.DirectAudioCapture,
	AudioMixRuntimeHandle: binding.AudioMixRuntimeHandle,
	ProcessLoopback,
	pipeWireAvailable: binding.pipeWireAvailable,
	audioBackend: binding.audioBackend ?? (() => (binding.pipeWireAvailable?.() ? 'pipewire' : 'none')),
	__setBindingForTests(nextBinding) {
		binding = nextBinding;
		clearIdleDirectCapturePool();
	},
	__getIdleDirectCaptureCountForTests() {
		return idleDirectCaptures.length;
	},
};
