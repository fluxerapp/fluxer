// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2AppScreenShareExecutionAdapter} from '@app/features/voice/engine/v2/VoiceEngineV2AppScreenShareExecutionAdapter';
import {logger} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import LocalVoiceState from '@app/features/voice/state/LocalVoiceState';
import type {VoiceEngineV2ScreenOptions} from '@fluxer/voice_engine_v2';
import type {Room, ScreenShareCaptureOptions, TrackPublishOptions, VideoCodec} from 'livekit-client';

const SCREEN_SHARE_CONTROLLER_PUBLISH_WIDTH_DEFAULT = 1920;
const SCREEN_SHARE_CONTROLLER_PUBLISH_HEIGHT_DEFAULT = 1080;
const SCREEN_SHARE_CONTROLLER_PUBLISH_REQUESTS_CAP = 8;
const SCREEN_SHARE_CONTROLLER_STOP_REQUESTS_CAP = 8;

export const SCREEN_SHARE_PUBLISH_INACTIVE_ERROR_NAME = 'VoiceEngineV2AppScreenSharePublishInactiveError';
export const SCREEN_SHARE_ROUTING_SATURATED_ERROR_NAME = 'VoiceEngineV2AppScreenShareRoutingSaturatedError';

export type VoiceEngineV2AppScreenSharePlannedOperationsListener = (operationIds: ReadonlyArray<number>) => void;

export interface VoiceEngineV2AppScreenShareControllerGateway {
	isScreenCommandRoutable(): boolean;
	hasScreenPublication(): boolean;
	hasScreenDesired(): boolean;
	clearScreenDesired(): void;
	executingScreenOperationId(): number | null;
	isScreenOperationPending(operationId: number): boolean;
	publishScreen(
		options: VoiceEngineV2ScreenOptions,
		onPlanned: VoiceEngineV2AppScreenSharePlannedOperationsListener,
	): Promise<void>;
	unpublishScreen(onPlanned: VoiceEngineV2AppScreenSharePlannedOperationsListener): Promise<void>;
}

export type VoiceEngineV2AppScreenShareSetEnabledOptions = ScreenShareCaptureOptions & {
	sendUpdate?: boolean;
	playSound?: boolean;
	restartIfEnabled?: boolean;
	reason?: string;
};

export interface VoiceEngineV2AppScreenShareSetEnabledSelection {
	captureOptions: ScreenShareCaptureOptions;
	sendUpdate: boolean;
	playSound: boolean;
	restartIfEnabled: boolean;
	reason: string | null;
}

export function selectVoiceEngineV2AppScreenShareSetEnabledOptions(
	options?: VoiceEngineV2AppScreenShareSetEnabledOptions,
): VoiceEngineV2AppScreenShareSetEnabledSelection {
	const {
		sendUpdate = true,
		playSound = true,
		restartIfEnabled = false,
		reason = null,
		...captureOptions
	} = options || {};
	return {
		captureOptions,
		sendUpdate,
		playSound,
		restartIfEnabled,
		reason,
	};
}

interface PendingScreenSharePublishRequest {
	readonly captureId: string;
	readonly room: Room | null;
	readonly options: VoiceEngineV2AppScreenShareSetEnabledOptions | undefined;
	readonly publishOptions: TrackPublishOptions | undefined;
	operationIds: ReadonlyArray<number>;
	settledInactive: boolean;
	verbSettled: boolean;
	failure: unknown;
}

interface PendingScreenShareStopRequest {
	readonly room: Room | null;
	readonly sendUpdate: boolean;
	readonly playSound: boolean;
	operationIds: ReadonlyArray<number>;
	verbSettled: boolean;
	failure: unknown;
}

function createScreenShareCaptureId(): string {
	const cryptoPort = globalThis.crypto;
	if (!cryptoPort || typeof cryptoPort.randomUUID !== 'function') {
		throw new Error('Screen-share capture ID generation requires crypto.randomUUID');
	}
	const captureId = cryptoPort.randomUUID();
	assert.ok(captureId.length > 0, 'crypto.randomUUID must return a non-empty capture ID');
	return captureId;
}

function buildScreenSharePublishInactiveError(): Error {
	const error = new Error('Screen-share publish settled without an active share');
	error.name = SCREEN_SHARE_PUBLISH_INACTIVE_ERROR_NAME;
	return error;
}

function buildScreenShareRoutingSaturatedError(kind: 'publish' | 'stop'): Error {
	const error = new Error(`Screen-share controller routing is saturated with in-flight ${kind} requests`);
	error.name = SCREEN_SHARE_ROUTING_SATURATED_ERROR_NAME;
	return error;
}

function isScreenShareVideoCodec(value: unknown): value is VideoCodec {
	return value === 'av1' || value === 'h265' || value === 'h264' || value === 'vp9' || value === 'vp8';
}

function roundPublishDimension(value: number, fallback: number): number {
	assert.ok(fallback > 0, 'publish dimension fallback must be positive');
	if (!Number.isFinite(value) || value <= 0) return fallback;
	return Math.round(value);
}

export function buildControllerScreenPublishOptions(args: {
	captureId: string;
	options: VoiceEngineV2AppScreenShareSetEnabledOptions | undefined;
	publishOptions: TrackPublishOptions | undefined;
}): VoiceEngineV2ScreenOptions {
	assert.ok(args.captureId.length > 0, 'controller screen publish requires a captureId');
	const resolution = args.options?.resolution;
	const encoding = args.publishOptions?.screenShareEncoding;
	const maxFramerate = resolution?.frameRate ?? encoding?.maxFramerate;
	const codec = args.publishOptions?.videoCodec;
	return {
		captureId: args.captureId,
		width: roundPublishDimension(resolution?.width ?? Number.NaN, SCREEN_SHARE_CONTROLLER_PUBLISH_WIDTH_DEFAULT),
		height: roundPublishDimension(resolution?.height ?? Number.NaN, SCREEN_SHARE_CONTROLLER_PUBLISH_HEIGHT_DEFAULT),
		...(isScreenShareVideoCodec(codec) ? {codec} : {}),
		...(encoding?.maxBitrate !== undefined ? {maxBitrateBps: encoding.maxBitrate} : {}),
		...(maxFramerate !== undefined ? {maxFramerate} : {}),
	};
}

function buildFallbackSetEnabledOptions(
	options: VoiceEngineV2ScreenOptions,
): VoiceEngineV2AppScreenShareSetEnabledOptions {
	assert.ok(options.width > 0, 'fallback set-enabled options require a positive width');
	assert.ok(options.height > 0, 'fallback set-enabled options require a positive height');
	return {
		resolution: {
			width: options.width,
			height: options.height,
			...(options.maxFramerate !== undefined ? {frameRate: options.maxFramerate} : {}),
		},
	};
}

export class VoiceEngineV2AppScreenShareControllerRouting {
	private readonly adapter: VoiceEngineV2AppScreenShareExecutionAdapter;
	private gateway: VoiceEngineV2AppScreenShareControllerGateway | null = null;
	private readonly pendingPublishRequests = new Map<string, PendingScreenSharePublishRequest>();
	private readonly pendingStopRequestsByOperationId = new Map<number, PendingScreenShareStopRequest>();

	constructor(adapter: VoiceEngineV2AppScreenShareExecutionAdapter) {
		assert.ok(adapter, 'screen-share controller routing requires the adapter');
		this.adapter = adapter;
	}

	setGateway(gateway: VoiceEngineV2AppScreenShareControllerGateway | null): void {
		if (gateway !== null) {
			assert.equal(typeof gateway.isScreenCommandRoutable, 'function');
			assert.equal(typeof gateway.hasScreenPublication, 'function');
			assert.equal(typeof gateway.hasScreenDesired, 'function');
			assert.equal(typeof gateway.clearScreenDesired, 'function');
			assert.equal(typeof gateway.executingScreenOperationId, 'function');
			assert.equal(typeof gateway.isScreenOperationPending, 'function');
			assert.equal(typeof gateway.publishScreen, 'function');
			assert.equal(typeof gateway.unpublishScreen, 'function');
		}
		this.gateway = gateway;
	}

	isStopRoutable(): boolean {
		const gateway = this.gateway;
		if (gateway === null) return false;
		if (!gateway.isScreenCommandRoutable()) return false;
		if (gateway.hasScreenPublication()) return true;
		return gateway.hasScreenDesired();
	}

	get pendingPublishRequestCount(): number {
		return this.pendingPublishRequests.size;
	}

	get pendingStopRequestCount(): number {
		return this.pendingStopRequestsByOperationId.size;
	}

	async setEnabled(
		room: Room | null,
		enabled: boolean,
		options?: VoiceEngineV2AppScreenShareSetEnabledOptions,
		publishOptions?: TrackPublishOptions,
	): Promise<void> {
		assert.equal(typeof enabled, 'boolean');
		const gateway = this.gateway;
		if (gateway === null || !gateway.isScreenCommandRoutable()) {
			await this.adapter.executeScreenShareSetEnabledDirect(room, enabled, options, publishOptions);
			if (enabled) {
				await this.adapter.applyPendingScreenShareRequestsForRoom(room);
			} else {
				gateway?.clearScreenDesired();
			}
			return;
		}
		if (enabled) {
			await this.publishThroughController(gateway, room, options, publishOptions);
			await this.adapter.applyPendingScreenShareRequestsForRoom(room);
			return;
		}
		if (!gateway.hasScreenPublication()) {
			if (!gateway.hasScreenDesired()) {
				await this.adapter.executeScreenShareSetEnabledDirect(room, false, options, publishOptions);
				return;
			}
		}
		await this.unpublishThroughController(gateway, room, options);
	}

	private async publishThroughController(
		gateway: VoiceEngineV2AppScreenShareControllerGateway,
		room: Room | null,
		options: VoiceEngineV2AppScreenShareSetEnabledOptions | undefined,
		publishOptions: TrackPublishOptions | undefined,
	): Promise<void> {
		if (this.adapter.isScreenSharePending) {
			logger.debug('Already pending, ignoring routed screen-share publish request');
			return;
		}
		this.ensurePublishRequestCapacity();
		const request: PendingScreenSharePublishRequest = {
			captureId: createScreenShareCaptureId(),
			room,
			options,
			publishOptions,
			operationIds: [],
			settledInactive: false,
			verbSettled: false,
			failure: null,
		};
		this.pendingPublishRequests.set(request.captureId, request);
		assert.ok(
			this.pendingPublishRequests.size <= SCREEN_SHARE_CONTROLLER_PUBLISH_REQUESTS_CAP,
			'pending screen-share publish requests exceeded cap after registration',
		);
		try {
			await gateway.publishScreen(
				buildControllerScreenPublishOptions({
					captureId: request.captureId,
					options,
					publishOptions,
				}),
				(operationIds) => {
					request.operationIds = operationIds;
				},
			);
			if (request.settledInactive) return;
			if (request.failure !== null) throw request.failure;
		} catch (error) {
			if (request.settledInactive) {
				return;
			}
			throw request.failure ?? error;
		} finally {
			request.verbSettled = true;
			this.releasePublishRequestIfSettled(request);
		}
	}

	private ensurePublishRequestCapacity(): void {
		if (this.pendingPublishRequests.size < SCREEN_SHARE_CONTROLLER_PUBLISH_REQUESTS_CAP) return;
		this.sweepSettledPublishRequests();
		if (this.pendingPublishRequests.size < SCREEN_SHARE_CONTROLLER_PUBLISH_REQUESTS_CAP) return;
		this.evictOldestAbandonedPublishRequest();
		if (this.pendingPublishRequests.size < SCREEN_SHARE_CONTROLLER_PUBLISH_REQUESTS_CAP) return;
		throw buildScreenShareRoutingSaturatedError('publish');
	}

	private evictOldestAbandonedPublishRequest(): void {
		assert.ok(this.pendingPublishRequests.size > 0, 'publish request eviction requires resident requests');
		for (const [captureId, request] of this.pendingPublishRequests) {
			if (!request.verbSettled) continue;
			this.pendingPublishRequests.delete(captureId);
			logger.warn('Evicted abandoned screen-share publish request at cap', {
				captureId,
				operationIds: request.operationIds,
			});
			return;
		}
	}

	private async unpublishThroughController(
		gateway: VoiceEngineV2AppScreenShareControllerGateway,
		room: Room | null,
		options: VoiceEngineV2AppScreenShareSetEnabledOptions | undefined,
	): Promise<void> {
		this.ensureStopRequestCapacity();
		const request: PendingScreenShareStopRequest = {
			room,
			sendUpdate: options?.sendUpdate ?? true,
			playSound: options?.playSound ?? true,
			operationIds: [],
			verbSettled: false,
			failure: null,
		};
		try {
			await gateway.unpublishScreen((operationIds) => {
				request.operationIds = operationIds;
				for (const operationId of operationIds) {
					assert.ok(Number.isInteger(operationId), 'planned stop operationId must be an integer');
					this.ensureStopRequestCapacity();
					this.pendingStopRequestsByOperationId.set(operationId, request);
				}
				assert.ok(
					this.pendingStopRequestsByOperationId.size <= SCREEN_SHARE_CONTROLLER_STOP_REQUESTS_CAP,
					'pending screen-share stop requests exceeded cap after registration',
				);
			});
			if (request.failure !== null) throw request.failure;
		} catch (error) {
			throw request.failure ?? error;
		} finally {
			request.verbSettled = true;
			this.releaseStopRequestIfSettled(request);
		}
	}

	private ensureStopRequestCapacity(): void {
		if (this.pendingStopRequestsByOperationId.size < SCREEN_SHARE_CONTROLLER_STOP_REQUESTS_CAP) return;
		this.sweepSettledStopRequests();
		if (this.pendingStopRequestsByOperationId.size < SCREEN_SHARE_CONTROLLER_STOP_REQUESTS_CAP) return;
		this.evictOldestAbandonedStopRequest();
		if (this.pendingStopRequestsByOperationId.size < SCREEN_SHARE_CONTROLLER_STOP_REQUESTS_CAP) return;
		throw buildScreenShareRoutingSaturatedError('stop');
	}

	private evictOldestAbandonedStopRequest(): void {
		assert.ok(this.pendingStopRequestsByOperationId.size > 0, 'stop request eviction requires resident requests');
		let abandoned: PendingScreenShareStopRequest | null = null;
		for (const request of this.pendingStopRequestsByOperationId.values()) {
			if (!request.verbSettled) continue;
			abandoned = request;
			break;
		}
		if (abandoned === null) return;
		for (const operationId of Array.from(this.pendingStopRequestsByOperationId.keys())) {
			if (this.pendingStopRequestsByOperationId.get(operationId) !== abandoned) continue;
			this.pendingStopRequestsByOperationId.delete(operationId);
		}
		logger.warn('Evicted abandoned screen-share stop request at cap', {operationIds: abandoned.operationIds});
	}

	private hasPendingScreenOperation(operationIds: ReadonlyArray<number>): boolean {
		assert.ok(Array.isArray(operationIds), 'operationIds must be an array');
		const gateway = this.gateway;
		if (gateway === null) return false;
		for (const operationId of operationIds) {
			if (gateway.isScreenOperationPending(operationId)) return true;
		}
		return false;
	}

	private releasePublishRequestIfSettled(request: PendingScreenSharePublishRequest): void {
		assert.ok(request.captureId.length > 0, 'publish request release requires a captureId');
		if (!this.pendingPublishRequests.has(request.captureId)) return;
		if (this.hasPendingScreenOperation(request.operationIds)) return;
		this.pendingPublishRequests.delete(request.captureId);
		assert.ok(!this.pendingPublishRequests.has(request.captureId), 'publish request must be released');
	}

	private releaseStopRequestIfSettled(request: PendingScreenShareStopRequest): void {
		assert.ok(request !== null && typeof request === 'object', 'stop request release requires the request');
		for (const operationId of request.operationIds) {
			if (this.pendingStopRequestsByOperationId.get(operationId) !== request) continue;
			if (this.hasPendingScreenOperation([operationId])) continue;
			this.pendingStopRequestsByOperationId.delete(operationId);
		}
	}

	private sweepSettledPublishRequests(): void {
		assert.ok(this.pendingPublishRequests.size >= 0, 'publish request map must be readable');
		for (const [captureId, request] of this.pendingPublishRequests) {
			if (request.operationIds.length === 0) continue;
			if (this.hasPendingScreenOperation(request.operationIds)) continue;
			this.pendingPublishRequests.delete(captureId);
		}
	}

	private sweepSettledStopRequests(): void {
		assert.ok(this.pendingStopRequestsByOperationId.size >= 0, 'stop request map must be readable');
		for (const operationId of Array.from(this.pendingStopRequestsByOperationId.keys())) {
			if (this.hasPendingScreenOperation([operationId])) continue;
			this.pendingStopRequestsByOperationId.delete(operationId);
		}
	}

	private takePublishRequest(captureId: string): PendingScreenSharePublishRequest | null {
		assert.ok(captureId.length > 0, 'publish request lookup requires a captureId');
		const request = this.pendingPublishRequests.get(captureId);
		if (request === undefined) return null;
		this.pendingPublishRequests.delete(captureId);
		return request;
	}

	private executingScreenOperationId(): number | null {
		const gateway = this.gateway;
		if (gateway === null) return null;
		const operationId = gateway.executingScreenOperationId();
		assert.ok(operationId === null || Number.isInteger(operationId), 'executing operationId must be an integer');
		return operationId;
	}

	private takeStopRequest(operationId: number | null): PendingScreenShareStopRequest | null {
		assert.ok(operationId === null || Number.isInteger(operationId), 'stop request lookup requires an integer id');
		if (operationId === null) return null;
		const request = this.pendingStopRequestsByOperationId.get(operationId) ?? null;
		if (request !== null) this.pendingStopRequestsByOperationId.delete(operationId);
		assert.ok(!this.pendingStopRequestsByOperationId.has(operationId), 'taken stop request must leave the map');
		return request;
	}

	async publishViaLiveKitFlows(roomFromPort: Room | null, options: VoiceEngineV2ScreenOptions): Promise<void> {
		assert.ok(options.captureId.length > 0, 'LiveKit screen publish requires a captureId');
		const request = this.takePublishRequest(options.captureId);
		const room = request !== null ? request.room : roomFromPort;
		const enabledOptions = request !== null ? request.options : buildFallbackSetEnabledOptions(options);
		const publishOptions = request !== null ? request.publishOptions : undefined;
		try {
			await this.adapter.liveKitFlows.setEnabled(room, true, enabledOptions, publishOptions);
		} catch (error) {
			if (request !== null) request.failure = error;
			throw error;
		}
		if (!LocalVoiceState.getSelfStream()) {
			if (request !== null) request.settledInactive = true;
			throw buildScreenSharePublishInactiveError();
		}
	}

	async unpublishViaLiveKitFlows(roomFromPort: Room | null): Promise<void> {
		const request = this.takeStopRequest(this.executingScreenOperationId());
		const room = request !== null ? request.room : roomFromPort;
		try {
			await this.adapter.liveKitFlows.setEnabled(room, false, {
				sendUpdate: request?.sendUpdate ?? true,
				playSound: request?.playSound ?? true,
			});
		} catch (error) {
			if (request !== null) request.failure = error;
			throw error;
		}
	}
}
