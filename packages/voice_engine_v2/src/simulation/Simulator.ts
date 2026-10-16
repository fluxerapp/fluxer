// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {
	type VoiceEngineV2EventLogEntry,
	VoiceEngineV2Runtime,
} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {
	type VoiceEngineV2FaultDecision,
	VoiceEngineV2FaultInjector,
	type VoiceEngineV2FaultPlan,
} from '@fluxer/voice_engine_v2/src/simulation/FaultInjector';
import {
	collectVoiceEngineV2SafetyViolations,
	type VoiceEngineV2SafetyViolation,
} from '@fluxer/voice_engine_v2/src/simulation/SafetyInvariants';
import {
	createSimulatorClock,
	type SimulatorDriverDeviceInventory,
	type SimulatorDriverFaultPolicy,
	VoiceEngineV2SimulatorDriver,
} from '@fluxer/voice_engine_v2/src/simulation/SimulatorPorts';
import type {VoiceEngineV2Workload} from '@fluxer/voice_engine_v2/src/simulation/Workload';
import {VoiceEngineV2TestImplementation} from '@fluxer/voice_engine_v2/src/testing/VoiceEngineV2TestImplementation';

export const SIMULATOR_TICK_MAX = 4096;
const SIMULATOR_TICK_NS_DEFAULT = 16_666_666;
const SIMULATOR_MICROTASK_FLUSH_MAX = 64;
const SIMULATOR_LIVENESS_RECOVERY_TICKS = 64;
export const STABLE_STRINGIFY_DEPTH_MAX = 128;
const STABLE_STRINGIFY_WORK_ITEMS_MAX = 1_048_576;

export type VoiceEngineV2SimulatorMode = 'safety' | 'liveness';

interface VoiceEngineV2SimulatorOptions {
	seed: number;
	workload: VoiceEngineV2Workload;
	faults: VoiceEngineV2FaultPlan;
	mode: VoiceEngineV2SimulatorMode;
	tickNs?: number;
	maxTicks?: number;
	inventory?: SimulatorDriverDeviceInventory;
}

export interface VoiceEngineV2SimulatorResult {
	snapshotHash: string;
	finalTick: number;
	violations: ReadonlyArray<VoiceEngineV2SafetyViolation>;
	eventLog: ReadonlyArray<VoiceEngineV2EventLogEntry>;
	livenessRecovered: boolean;
	partitionedPeers: ReadonlyArray<string>;
}

export class VoiceEngineV2Simulator {
	private readonly seed: number;
	private readonly workload: VoiceEngineV2Workload;
	private readonly faultPlan: VoiceEngineV2FaultPlan;
	private readonly mode: VoiceEngineV2SimulatorMode;
	private readonly tickNs: number;
	private readonly maxTicks: number;
	private readonly inventory: SimulatorDriverDeviceInventory;

	constructor(options: VoiceEngineV2SimulatorOptions) {
		assert.ok(options, 'simulator requires options');
		assert.ok(Number.isInteger(options.seed), 'simulator seed must be an integer');
		assert.ok(options.seed >= 0, 'simulator seed must be non-negative');
		assert.ok(options.workload, 'simulator requires a workload');
		assert.ok(options.faults, 'simulator requires a fault plan');
		assert.ok(options.mode === 'safety' || options.mode === 'liveness', 'simulator mode invalid');
		this.seed = options.seed;
		this.workload = options.workload;
		this.faultPlan = options.faults;
		this.mode = options.mode;
		this.tickNs = options.tickNs ?? SIMULATOR_TICK_NS_DEFAULT;
		this.maxTicks = options.maxTicks ?? SIMULATOR_TICK_MAX;
		this.inventory = options.inventory ?? defaultInventory();
		assert.ok(this.tickNs > 0, 'tickNs must be positive');
		assert.ok(this.maxTicks > 0, 'maxTicks must be positive');
		assert.ok(this.maxTicks <= SIMULATOR_TICK_MAX, 'maxTicks exceeds SIMULATOR_TICK_MAX cap');
	}

	async run(): Promise<VoiceEngineV2SimulatorResult> {
		assert.ok(this.maxTicks > 0, 'simulator run requires positive maxTicks');
		assert.ok(this.workload.steps.length <= this.maxTicks * 8, 'workload step density violates simulator budget');
		const clock = createSimulatorClock(0);
		const injector = new VoiceEngineV2FaultInjector(this.seed, this.faultPlan);
		const stepsByTick = bucketStepsByTick(this.workload, this.maxTicks);
		const policyState: PolicyState = {decision: blankDecision()};
		const driver = new VoiceEngineV2SimulatorDriver({
			policy: buildPolicy(policyState),
			inventory: this.inventory,
		});
		const implementation = new VoiceEngineV2TestImplementation(driver);
		const runtime = new VoiceEngineV2Runtime(implementation, {
			clock,
			eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
			verifyEventLogInvariantsOnDispatch: true,
		});
		const totalTicks = this.computeTotalTicks();
		let livenessBaselinePublishCount = 0;
		let livenessRecovered = this.mode === 'safety';
		let partitionStarted = false;
		const partitionedPeers = new Set<string>();
		for (let tick = 0; tick < totalTicks; tick++) {
			const decision = injector.decide(tick);
			policyState.decision = decision;
			for (const peer of decision.asymmetricallyPartitionedPeers) partitionedPeers.add(peer);
			if (decision.asymmetricallyPartitionedPeers.length > 0 && !partitionStarted) {
				partitionStarted = true;
				livenessBaselinePublishCount = countMicrophonePublishSucceeded(runtime.eventLog);
			}
			const steps = stepsByTick.get(tick) ?? [];
			for (const step of steps) runtime.dispatch(step.event);
			clock.advanceNs(this.tickNs);
			await flushMicrotasks(SIMULATOR_MICROTASK_FLUSH_MAX);
			if (this.mode === 'liveness' && partitionStarted && !livenessRecovered) {
				const current = countMicrophonePublishSucceeded(runtime.eventLog);
				if (current > livenessBaselinePublishCount) livenessRecovered = true;
			}
		}
		await flushMicrotasks(SIMULATOR_MICROTASK_FLUSH_MAX);
		const finalLog = runtime.eventLog;
		const violations = this.mode === 'safety' ? collectVoiceEngineV2SafetyViolations(finalLog) : [];
		const hash = hashSnapshot(runtime.snapshot);
		runtime.dispose();
		assertResultWellFormed(hash, finalLog);
		return {
			snapshotHash: hash,
			finalTick: totalTicks,
			violations,
			eventLog: finalLog,
			livenessRecovered,
			partitionedPeers: [...partitionedPeers],
		};
	}

	private computeTotalTicks(): number {
		const workloadTicks = Math.min(this.workload.tickCount, this.maxTicks);
		if (this.mode === 'safety') return Math.max(workloadTicks, 1);
		const total = Math.min(workloadTicks + SIMULATOR_LIVENESS_RECOVERY_TICKS, this.maxTicks);
		assert.ok(total >= workloadTicks, 'liveness tick budget must extend workload');
		return Math.max(total, 1);
	}
}

interface PolicyState {
	decision: VoiceEngineV2FaultDecision;
}

function bucketStepsByTick(
	workload: VoiceEngineV2Workload,
	maxTicks: number,
): Map<number, ReadonlyArray<{tick: number; event: VoiceEngineV2Event}>> {
	assert.ok(workload, 'workload required for bucketing');
	assert.ok(maxTicks > 0, 'maxTicks must be positive when bucketing');
	const buckets = new Map<number, Array<{tick: number; event: VoiceEngineV2Event}>>();
	for (const step of workload.steps) {
		if (step.tick >= maxTicks) continue;
		const bucket = buckets.get(step.tick) ?? [];
		bucket.push({tick: step.tick, event: step.event});
		buckets.set(step.tick, bucket);
	}
	return buckets;
}

function blankDecision(): VoiceEngineV2FaultDecision {
	return {
		connectShouldDrop: false,
		disconnectShouldDrop: false,
		microphonePublishShouldFail: false,
		cameraPublishShouldFail: false,
		screenPublishShouldFail: false,
		failingCaptureIds: [],
		deviceLossDeviceIds: [],
		gpuLost: false,
		asymmetricallyPartitionedPeers: [],
		networkPartitionActive: false,
	};
}

function buildPolicy(state: PolicyState): SimulatorDriverFaultPolicy {
	return {
		shouldDropConnect: () => state.decision.connectShouldDrop,
		shouldDropDisconnect: () => state.decision.disconnectShouldDrop,
		shouldFailMicrophonePublish: () => state.decision.microphonePublishShouldFail,
		shouldFailCameraPublish: () => state.decision.cameraPublishShouldFail,
		shouldFailScreenPublish: () => state.decision.screenPublishShouldFail,
		shouldFailNativeCaptureStart: (captureId) => state.decision.failingCaptureIds.includes(captureId),
		shouldEmitDeviceLoss: () => state.decision.deviceLossDeviceIds.length > 0,
	};
}

async function flushMicrotasks(maxIterations: number): Promise<void> {
	assert.ok(maxIterations > 0, 'flushMicrotasks requires a positive iteration cap');
	for (let i = 0; i < maxIterations; i++) {
		await Promise.resolve();
	}
}

function countMicrophonePublishSucceeded(entries: ReadonlyArray<VoiceEngineV2EventLogEntry>): number {
	assert.ok(Array.isArray(entries), 'event log entries required');
	let count = 0;
	for (const entry of entries) if (entry.event.type === 'microphone.publishSucceeded') count++;
	return count;
}

export function hashSnapshot(snapshot: unknown): string {
	assert.ok(snapshot !== undefined, 'cannot hash an undefined snapshot');
	const serialized = stableStringify(snapshot);
	assert.ok(typeof serialized === 'string', 'stableStringify must return a string');
	let hash = 0x811c9dc5;
	for (let i = 0; i < serialized.length; i++) {
		hash ^= serialized.charCodeAt(i);
		hash = Math.imul(hash, 0x01000193);
	}
	const value = (hash >>> 0).toString(16).padStart(8, '0');
	assert.ok(value.length === 8, 'hash must be eight hex characters');
	return value;
}

type StableStringifyWorkItem = {literal: string} | {value: unknown; depth: number};

export function stableStringify(value: unknown): string {
	const out: Array<string> = [];
	const stack: Array<StableStringifyWorkItem> = [{value, depth: 0}];
	let processedCount = 0;
	while (stack.length > 0) {
		processedCount += 1;
		assert.ok(processedCount <= STABLE_STRINGIFY_WORK_ITEMS_MAX, 'stableStringify work item budget exceeded');
		const item = stack.pop();
		assert.ok(item !== undefined, 'stableStringify stack pop must yield an item');
		if ('literal' in item) {
			out.push(item.literal);
		} else {
			appendStableStringifyValue(item.value, item.depth, out, stack);
		}
	}
	return out.join('');
}

function appendStableStringifyValue(
	value: unknown,
	depth: number,
	out: Array<string>,
	stack: Array<StableStringifyWorkItem>,
): void {
	assert.ok(depth >= 0, 'stableStringify depth must be non-negative');
	assert.ok(depth <= STABLE_STRINGIFY_DEPTH_MAX, 'stableStringify depth budget exceeded');
	if (value === null) {
		out.push('null');
		return;
	}
	if (typeof value === 'number') {
		out.push(Number.isFinite(value) ? String(value) : 'null');
		return;
	}
	if (typeof value === 'boolean') {
		out.push(value ? 'true' : 'false');
		return;
	}
	if (typeof value === 'string') {
		out.push(JSON.stringify(value));
		return;
	}
	if (Array.isArray(value)) {
		out.push('[');
		stack.push({literal: ']'});
		for (let i = value.length - 1; i >= 0; i -= 1) {
			stack.push({value: value[i], depth: depth + 1});
			if (i > 0) stack.push({literal: ','});
		}
		return;
	}
	if (typeof value === 'object') {
		const record = value as Record<string, unknown>;
		const keys = Object.keys(record).sort();
		out.push('{');
		stack.push({literal: '}'});
		for (let i = keys.length - 1; i >= 0; i -= 1) {
			const key = keys[i];
			assert.ok(key !== undefined, 'stableStringify sorted keys must not contain holes');
			stack.push({value: record[key], depth: depth + 1});
			stack.push({literal: `${JSON.stringify(key)}:`});
			if (i > 0) stack.push({literal: ','});
		}
		return;
	}
	out.push('null');
}

function assertResultWellFormed(hash: string, entries: ReadonlyArray<VoiceEngineV2EventLogEntry>): void {
	assert.ok(typeof hash === 'string', 'hash must be a string');
	assert.ok(hash.length > 0, 'hash must be non-empty');
	assert.ok(Array.isArray(entries), 'event log must be an array on return');
}

function defaultInventory(): SimulatorDriverDeviceInventory {
	return {
		audioInputs: ['default-mic'],
		audioOutputs: ['default-output'],
		cameras: ['default-camera'],
	};
}
