// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import {isElectronPlatform} from '@app/features/platform/types/Platform';
import {resetNativeHardwareEncoderCapabilities} from '@app/features/voice/utils/NativeHardwareEncoderCapabilities';
import {
	VOICE_ENGINE_V2_BRIDGE_METHODS,
	VOICE_ENGINE_V2_BRIDGE_VERSION,
	VOICE_ENGINE_V2_ENGINE_READY_EVENT_TYPE,
	type VoiceEngineV2BridgeApi,
	type VoiceEngineV2BridgeCapabilities,
	type VoiceEngineV2BridgeMethodName,
	type VoiceEngineV2BridgeReadiness,
} from '@fluxer/voice_engine_v2/bridge';

export const NATIVE_VOICE_ENGINE_BRIDGE_VERSION = VOICE_ENGINE_V2_BRIDGE_VERSION;
export const NATIVE_VOICE_ENGINE_READINESS_TIMEOUT_MS = 8000;

export type NativeVoiceEngineUpgradeBlockReason =
	| 'missing-bridge'
	| 'missing-bridge-version'
	| 'unsupported-bridge-version'
	| 'incomplete-bridge';

export type NativeVoiceEngineSelectionSnapshot =
	| {state: 'web'}
	| {state: 'uninitialized'}
	| {state: 'checking'}
	| {state: 'native'}
	| {state: 'blocked'; reason: NativeVoiceEngineUpgradeBlockReason}
	| {state: 'unsupported'};

const REQUIRED_NATIVE_VOICE_ENGINE_METHODS: ReadonlyArray<VoiceEngineV2BridgeMethodName> =
	VOICE_ENGINE_V2_BRIDGE_METHODS;

export class NativeVoiceEngineUpgradeRequiredError extends Error {
	readonly reason: NativeVoiceEngineUpgradeBlockReason;

	constructor(reason: NativeVoiceEngineUpgradeBlockReason) {
		super(`Native voice engine bridge is not available in this desktop app build: ${reason}`);
		this.name = 'NativeVoiceEngineUpgradeRequiredError';
		this.reason = reason;
	}
}

class NativeVoiceEngineInvariantError extends Error {
	constructor(message: string, cause?: unknown) {
		super(cause instanceof Error ? `${message}: ${cause.message}` : message);
		this.name = 'NativeVoiceEngineInvariantError';
	}
}

let nativeSupportState: 'unknown' | 'checking' | 'supported' | 'unsupported' = 'unknown';
let nativeSupportPromise: Promise<boolean> | null = null;
let nativePrewarmPromise: Promise<void> | null = null;
let nativeCapabilitiesSnapshot: VoiceEngineV2BridgeCapabilities | null = null;

const NATIVE_VOICE_ENGINE_FORCE_DISABLED: boolean = true;

function isNativeVoiceEngineRequired(): boolean {
	if (NATIVE_VOICE_ENGINE_FORCE_DISABLED) return false;
	return isElectronPlatform();
}

function getNativeVoiceEngineBridgeCandidate(): unknown {
	return window.electron?.voiceEngine ?? null;
}

function getNativeVoiceEngineBridgeUpgradeBlockReason(): NativeVoiceEngineUpgradeBlockReason | null {
	if (!isElectronPlatform()) return null;
	const bridge = getNativeVoiceEngineBridgeCandidate();
	if (!bridge || typeof bridge !== 'object') return 'missing-bridge';
	const record = bridge as Record<string, unknown>;
	const bridgeVersion = record.bridgeVersion;
	if (typeof bridgeVersion !== 'number') return 'missing-bridge-version';
	if (bridgeVersion !== NATIVE_VOICE_ENGINE_BRIDGE_VERSION) return 'unsupported-bridge-version';
	if (REQUIRED_NATIVE_VOICE_ENGINE_METHODS.some((method) => typeof record[method] !== 'function')) {
		return 'incomplete-bridge';
	}
	return null;
}

function getNativeVoiceEngineBridge(): VoiceEngineV2BridgeApi | null {
	if (!isElectronPlatform()) return null;
	if (getNativeVoiceEngineBridgeUpgradeBlockReason()) return null;
	return getNativeVoiceEngineBridgeCandidate() as VoiceEngineV2BridgeApi | null;
}

function shouldSelectNativeFromCachedState(): boolean {
	return isNativeVoiceEngineRequired();
}

function crashNativeVoiceEngineInvariant(message: string, cause?: unknown): never {
	nativeSupportState = 'unsupported';
	const error = new NativeVoiceEngineInvariantError(message, cause);
	throw RuntimeCrash.triggerFatalCrash(error);
}

function prewarmNativeVoiceEngineBridge(bridge: VoiceEngineV2BridgeApi): Promise<void> {
	if (!nativePrewarmPromise) {
		nativePrewarmPromise = bridge.prewarm().catch((error: unknown) => {
			nativePrewarmPromise = null;
			throw error;
		});
	}
	return nativePrewarmPromise;
}

export function isNativeVoiceEngineSelected(): boolean {
	if (!shouldSelectNativeFromCachedState()) return false;
	const bridge = getNativeVoiceEngineBridge();
	if (!bridge) return false;
	return nativeSupportState === 'supported' && shouldSelectNativeFromCachedState();
}

export function isNativeVoiceEngineSelectionPending(): boolean {
	if (!shouldSelectNativeFromCachedState()) return false;
	if (getNativeVoiceEngineBridgeUpgradeBlockReason()) return true;
	return nativeSupportState === 'checking' || nativeSupportState === 'unknown';
}

export function getNativeVoiceEngineUpgradeBlockReason(): NativeVoiceEngineUpgradeBlockReason | null {
	if (!shouldSelectNativeFromCachedState()) return null;
	return getNativeVoiceEngineBridgeUpgradeBlockReason();
}

export function getNativeVoiceEngineSelectionSnapshot(): NativeVoiceEngineSelectionSnapshot {
	if (!shouldSelectNativeFromCachedState()) return {state: 'web'};
	const upgradeBlockReason = getNativeVoiceEngineBridgeUpgradeBlockReason();
	if (upgradeBlockReason) return {state: 'blocked', reason: upgradeBlockReason};
	if (nativeSupportState === 'supported') return {state: 'native'};
	if (nativeSupportState === 'checking') return {state: 'checking'};
	if (nativeSupportState === 'unsupported') return {state: 'unsupported'};
	return {state: 'uninitialized'};
}

export function getNativeVoiceEngineCapabilitiesSnapshot(): VoiceEngineV2BridgeCapabilities | null {
	return nativeCapabilitiesSnapshot;
}

export async function refreshNativeVoiceEngineCapabilitiesSnapshot(): Promise<VoiceEngineV2BridgeCapabilities | null> {
	if (nativeCapabilitiesSnapshot) return nativeCapabilitiesSnapshot;
	const bridge = getNativeVoiceEngineBridge();
	if (!bridge) return null;
	const capabilities = await bridge.getCapabilities();
	nativeCapabilitiesSnapshot = capabilities;
	return capabilities;
}

function waitForNativeVoiceEngineReadyEvent(
	bridge: VoiceEngineV2BridgeApi,
	initialReason: string | undefined,
	timeoutMs: number,
): Promise<VoiceEngineV2BridgeReadiness> {
	return new Promise<VoiceEngineV2BridgeReadiness>((resolve) => {
		let settled = false;
		let dispose: (() => void) | null = null;
		let timeoutId: ReturnType<typeof setTimeout> | null = null;
		const settle = (readiness: VoiceEngineV2BridgeReadiness): void => {
			if (settled) return;
			settled = true;
			if (timeoutId !== null) clearTimeout(timeoutId);
			dispose?.();
			resolve(readiness);
		};
		dispose = bridge.onEvent((event) => {
			if (event.type === VOICE_ENGINE_V2_ENGINE_READY_EVENT_TYPE) {
				settle({ready: true});
			}
		});
		timeoutId = setTimeout(() => {
			settle({ready: false, reason: initialReason ?? 'readiness-timeout'});
		}, timeoutMs);
		void bridge
			.getVoiceEngineReadiness()
			.then((readiness) => {
				if (readiness.ready) settle(readiness);
			})
			.catch(() => {});
	});
}

export async function awaitNativeVoiceEngineReadiness(
	timeoutMs: number = NATIVE_VOICE_ENGINE_READINESS_TIMEOUT_MS,
): Promise<VoiceEngineV2BridgeReadiness> {
	const bridge = getNativeVoiceEngineBridge();
	if (!bridge) return {ready: false, reason: 'missing-bridge'};
	let initial: VoiceEngineV2BridgeReadiness;
	try {
		initial = await bridge.getVoiceEngineReadiness();
	} catch (error) {
		return {ready: false, reason: error instanceof Error ? error.message : 'readiness-query-failed'};
	}
	if (initial.ready) return initial;
	return waitForNativeVoiceEngineReadyEvent(bridge, initial.reason, timeoutMs);
}

export function isNativeVoiceEngineUpgradeRequiredError(
	error: unknown,
): error is NativeVoiceEngineUpgradeRequiredError {
	return error instanceof NativeVoiceEngineUpgradeRequiredError;
}

export async function initializeNativeVoiceEngineSelectionForStartup(): Promise<NativeVoiceEngineSelectionSnapshot> {
	if (!shouldSelectNativeFromCachedState()) return getNativeVoiceEngineSelectionSnapshot();
	const selected = await shouldUseNativeVoiceEngine();
	if (!selected) {
		throw new Error('Native voice engine startup selection did not select native in Electron');
	}
	return getNativeVoiceEngineSelectionSnapshot();
}

export async function shouldUseNativeVoiceEngine(): Promise<boolean> {
	if (!shouldSelectNativeFromCachedState()) return false;
	const upgradeBlockReason = getNativeVoiceEngineBridgeUpgradeBlockReason();
	if (upgradeBlockReason) {
		throw new NativeVoiceEngineUpgradeRequiredError(upgradeBlockReason);
	}
	const bridge = getNativeVoiceEngineBridge();
	if (!bridge) {
		throw new NativeVoiceEngineUpgradeRequiredError('missing-bridge');
	}
	if (nativeSupportState === 'supported') {
		return true;
	}
	if (nativeSupportState === 'unsupported') {
		crashNativeVoiceEngineInvariant('Native voice engine bridge is current but the native backend is unsupported');
	}
	if (nativeSupportPromise) return nativeSupportPromise;
	nativeSupportState = 'checking';
	nativeSupportPromise = bridge
		.isSupported()
		.then(async (supported) => {
			if (!supported) {
				crashNativeVoiceEngineInvariant('Native voice engine bridge is current but support probing returned false');
			}
			const capabilities = await bridge.getCapabilities();
			if (!capabilities.microphoneCapture) {
				crashNativeVoiceEngineInvariant(
					'Native voice engine bridge is current but the native backend cannot capture device microphone audio',
				);
			}
			nativeCapabilitiesSnapshot = capabilities;
			await prewarmNativeVoiceEngineBridge(bridge);
			const readiness = await bridge.getVoiceEngineReadiness();
			if (!readiness?.ready) {
				crashNativeVoiceEngineInvariant(
					`Native voice engine reported not-ready after a successful prewarm: ${readiness?.reason ?? 'unknown'}`,
				);
			}
			nativeSupportState = 'supported';
			return true;
		})
		.catch((error) => {
			if (error instanceof NativeVoiceEngineInvariantError) {
				throw error;
			}
			crashNativeVoiceEngineInvariant('Native voice engine support probing failed', error);
		})
		.finally(() => {
			nativeSupportPromise = null;
		});
	return nativeSupportPromise;
}

export function resetNativeVoiceEngineSelectionForTesting(): void {
	nativeSupportState = 'unknown';
	nativeSupportPromise = null;
	nativePrewarmPromise = null;
	nativeCapabilitiesSnapshot = null;
	resetNativeHardwareEncoderCapabilities();
}
