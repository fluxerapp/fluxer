// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';

export type GameCaptureInjectionMethod = 'auto' | 'remote-thread' | 'set-windows-hook';

export type CaptureStrategyName = 'game-hook' | 'wgc' | 'dxgi-duplication' | 'window-gdi';

export interface ScreenCaptureRect {
	x: number;
	y: number;
	width: number;
	height: number;
}

export interface ScreenCaptureOptions {
	sourceId: string;
	sourceKind: 'screen' | 'window' | 'game';
	width?: number;
	height?: number;
	frameRate?: number;
	hookDllPath?: string;
	hookDllPathX86?: string;
	injectionMethod?: GameCaptureInjectionMethod;
	captureId?: string;
	colorRange?: 'full' | 'limited';
	colorSpace?: 'rec709' | 'srgb';
	showCursorClicks?: boolean;
	captureRect?: ScreenCaptureRect;
	frameSinkHandle?: unknown;
	nativeFrameSinkRequired?: boolean;
}

export interface ScreenCaptureStartResult {
	width: number;
	height: number;
	frameRate: number;
	pixelFormat: 'nv12' | 'bgra';
}

export interface ScreenCaptureSourceDescriptor {
	kind: 'screen' | 'window' | 'game';
	id: string;
	name: string;
	width: number;
	height: number;
	targetPid?: number;
}

export interface AvailabilityInfo {
	available: boolean;
	backend: string;
	reason?: string;
}

export interface CaptureDiagnostics {
	state: number;
	apiType: number;
	transport: number;
	fallbackReason: number;
	captureFlags: number;
	width: number;
	height: number;
	dxgiFormat: number;
	frameCounter: number;
	droppedFrameCounter: number;
	lastPresentTimestampUs: number;
	lastError: number;
	requestedInjectionMethod: GameCaptureInjectionMethod;
	injectionMethod: 'remote-thread' | 'set-windows-hook';
	activeStrategy: CaptureStrategyName;
	lastFallbackReason: string;
	startOptions: ScreenCaptureStartOptionsDiagnostics;
	frameSinkAccepted: number;
	frameSinkCoalesced: number;
	frameSinkRejected: number;
	mediaFramesDroppedWithoutSink: number;
	cpuFallbackFramesDropped: number;
}

export interface ScreenCaptureStartOptionsDiagnostics {
	colorRange?: 'full' | 'limited';
	colorSpace?: 'rec709' | 'srgb';
	showCursorClicks?: boolean;
	captureRect?: ScreenCaptureRect;
	unsupportedOptions: Array<'showCursorClicks' | 'captureRect' | 'colorRange' | 'colorSpace'>;
}

export interface SharedTextureHandleInfo {
	handle: bigint;
	width: number;
	height: number;
	dxgiFormat: number;
	timestampUs: number;
}

export interface EncoderAttachDiagnostics {
	attached: boolean;
	width: number;
	height: number;
	capacity: number;
	framesSubmitted: number;
	framesDropped: number;
	ringFullEvents: number;
	failedBlits: number;
}

export interface FrameSinkDiagnostics {
	accepted: number;
	coalesced: number;
	rejected: number;
	mediaFramesDroppedWithoutSink: number;
	cpuFallbackFramesDropped: number;
}

export interface VulkanLayerRegistrationState {
	registered: boolean;
	manifestExists: boolean;
	dllExists: boolean;
	manifestPath: string | null;
}

export declare interface ScreenCapture {
	on(event: 'error', listener: (err: Error) => void): this;
	on(event: 'closed', listener: () => void): this;
	on(event: 'stalled', listener: (message?: string) => void): this;
	on(event: 'diagnostic', listener: (message?: string) => void): this;
	on(event: string | symbol, listener: (...args: Array<unknown>) => void): this;

	off(event: 'error', listener: (err: Error) => void): this;
	off(event: 'closed', listener: () => void): this;
	off(event: 'stalled', listener: (message?: string) => void): this;
	off(event: 'diagnostic', listener: (message?: string) => void): this;
	off(event: string | symbol, listener: (...args: Array<unknown>) => void): this;

	emit(event: 'error', err: Error): boolean;
	emit(event: 'closed'): boolean;
	emit(event: 'stalled', message?: string): boolean;
	emit(event: 'diagnostic', message?: string): boolean;
}

export declare class ScreenCapture extends EventEmitter {
	constructor(options?: ScreenCaptureOptions);
	start(): Promise<ScreenCaptureStartResult | undefined>;
	stop(): Promise<void>;
	getDiagnostics(): CaptureDiagnostics | null;
	getSharedTextureHandle(): SharedTextureHandleInfo | null;
	attachEncoder(width: number, height: number, frameRate?: number): void;
	detachEncoder(): void;
	isEncoderAttached(): boolean;
	encoderRingFullCount(): number;
	getEncoderAttachDiagnostics(): EncoderAttachDiagnostics | null;
	getFrameSinkDiagnostics(): FrameSinkDiagnostics;
}

export declare function isSupported(): boolean;
export declare function getAvailability(): AvailabilityInfo;
export declare function listSources(): Promise<Array<ScreenCaptureSourceDescriptor>>;
export declare function resolveGameHookPath(): string | null;
export declare function resolveGameHookPathX86(): string | null;
export declare function isGameCaptureHookAvailable(): boolean;
export declare function resolveVulkanLayerManifestPath(): string | null;
export declare function registerVulkanLayerManifest(): boolean;
export declare function unregisterVulkanLayerManifest(): boolean;
export declare function getVulkanLayerRegistrationState(): VulkanLayerRegistrationState;
export declare function parseFallbackRecommendation(message: string | undefined): CaptureStrategyName | 'none' | null;
export declare function elevateGpuSchedulingPriority(processId?: number, priorityClass?: 'high' | 'realtime'): boolean;
export declare function restoreGpuSchedulingPriority(processId?: number): boolean;
export declare function __setBindingForTests(binding: unknown): void;
export declare const loadError: Error | null;
