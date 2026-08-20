// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	type CodecCapabilityReport,
	type CodecPreference,
	type CodecSupportInfo,
	getCodecCapabilityReport,
	resolveEffectiveScreenShareEncoderMode,
	type ScreenShareEncoderMode,
	selectNativeScreenCaptureScreenShareCodec,
	selectOptimalScreenShareCodec,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {loadGpuEncoderReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {loadNativeHardwareEncoderCapabilities} from '@app/features/voice/utils/NativeHardwareEncoderCapabilities';
import {loadOpenH264Status} from '@app/features/voice/utils/OpenH264Status';
import {
	getVideoDecoderExclusionsSync,
	loadVideoDecoderExclusions,
} from '@app/features/voice/utils/VideoDecoderCapabilities';
import type {Participant, Room, VideoCodec} from 'livekit-client';
import {RoomEvent} from 'livekit-client';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

const logger = new Logger('ScreenShareCodecNegotiation');
const PROTOCOL_TOPIC = 'fluxer.rtc.codec-negotiation.v1';
const SELECT_PROTOCOL_OP = 1;
const SESSION_UPDATE_OP = 14;
const TEXT_ENCODER = new TextEncoder();
const TEXT_DECODER = new TextDecoder();
const CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['av1', 'h265', 'h264', 'vp9', 'vp8'];
const SOFTWARE_CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['av1', 'vp9', 'h264', 'vp8', 'h265'];
const COMPATIBILITY_FALLBACK_CODEC_PREFERENCE: ReadonlyArray<VideoCodec> = ['vp9', 'vp8'];
const BASELINE_VIDEO_CODEC: VideoCodec = 'vp8';
const VIDEO_CODEC_NAMES: Record<VideoCodec, FluxerVideoCodecName> = {
	av1: 'AV1',
	h265: 'H265',
	h264: 'H264',
	vp9: 'VP9',
	vp8: 'VP8',
};
const NAME_TO_VIDEO_CODEC: Record<FluxerVideoCodecName, VideoCodec> = {
	AV1: 'av1',
	H265: 'h265',
	H264: 'h264',
	VP9: 'vp9',
	VP8: 'vp8',
};
const VIDEO_CODEC_PROTOCOL_TABLE: Record<
	VideoCodec,
	{
		payloadType: number;
		rtxPayloadType: number;
		priority: number;
	}
> = {
	av1: {payloadType: 101, rtxPayloadType: 102, priority: 5000},
	h265: {payloadType: 105, rtxPayloadType: 106, priority: 4000},
	h264: {payloadType: 103, rtxPayloadType: 104, priority: 3000},
	vp9: {payloadType: 109, rtxPayloadType: 110, priority: 2000},
	vp8: {payloadType: 107, rtxPayloadType: 108, priority: 1000},
};

type FluxerVideoCodecName = 'AV1' | 'H265' | 'H264' | 'VP9' | 'VP8';
type FluxerCodecName = 'opus' | FluxerVideoCodecName;
type FluxerCodecType = 'audio' | 'video';
export type NegotiationReason =
	| 'connected'
	| 'data'
	| 'participant-connected'
	| 'participant-disconnected'
	| 'reconnected'
	| 'manual';

export interface FluxerCodecAdvertisement {
	name: FluxerCodecName;
	type: FluxerCodecType;
	payload_type: number;
	rtx_payload_type?: number;
	priority: number;
	encode?: boolean;
	decode?: boolean;
}

export interface FluxerSelectProtocolMessage {
	op: typeof SELECT_PROTOCOL_OP;
	d: {
		protocol: 'livekit';
		data: {
			mode: 'livekit-sfu';
		};
		codecs: Array<FluxerCodecAdvertisement>;
		rtc_connection_id: string | null;
		experiments: Array<string>;
	};
}

export interface FluxerSessionUpdateMessage {
	op: typeof SESSION_UPDATE_OP;
	d: {
		video_codec: FluxerVideoCodecName;
		media_session_id: string;
		reason: NegotiationReason;
		codecs: Array<FluxerCodecAdvertisement>;
	};
}

export type FluxerCodecNegotiationMessage = FluxerSelectProtocolMessage | FluxerSessionUpdateMessage;

export interface CodecNegotiationSelection {
	codec: VideoCodec;
	reason: NegotiationReason;
	candidates: Array<VideoCodec>;
	unknownParticipants: number;
}

interface ScreenShareCodecNegotiationMachineContext {
	selectedCodec: VideoCodec | null;
	selection: CodecNegotiationSelection | null;
}

type ScreenShareCodecNegotiationMachineEvent =
	| {
			type: 'negotiation.evaluate';
			localCodecs: ReadonlyArray<FluxerCodecAdvertisement>;
			remoteCodecs: ReadonlyArray<ReadonlyArray<FluxerCodecAdvertisement>>;
			unknownParticipants: number;
			reason: NegotiationReason;
			codecPreference: ReadonlyArray<VideoCodec>;
	  }
	| {type: 'negotiation.reset'};

interface BindOptions {
	onSelectedCodecChanged?: (selection: CodecNegotiationSelection) => void | Promise<void>;
}

function createId(prefix: string): string {
	const cryptoObject = globalThis.crypto as Crypto | undefined;
	if (typeof cryptoObject?.randomUUID === 'function') return `${prefix}_${cryptoObject.randomUUID()}`;
	return `${prefix}_${Date.now().toString(36)}_${Math.random().toString(36).slice(2)}`;
}

function hasReceiverCapability(codec: VideoCodec): boolean | null {
	const receiver = (globalThis as Record<string, unknown>).RTCRtpReceiver as
		| {getCapabilities?: (kind: 'video') => RTCRtpCapabilities | null}
		| undefined;
	const caps = receiver?.getCapabilities?.('video');
	if (!caps) return null;
	const expectedMime = `video/${codec}`.toLowerCase();
	if (codec === 'av1') {
		return caps.codecs.some((entry) => {
			const mimeType = entry.mimeType.toLowerCase();
			return mimeType === expectedMime || mimeType === 'video/av1x';
		});
	}
	return caps.codecs.some((entry) => entry.mimeType.toLowerCase() === expectedMime);
}

function getLocalDecodeCapabilities(): Record<VideoCodec, boolean> {
	const exclusions = new Set(getVideoDecoderExclusionsSync() ?? []);
	const result: Record<VideoCodec, boolean> = {
		av1: false,
		h265: false,
		h264: true,
		vp9: false,
		vp8: true,
	};
	for (const codec of CODEC_PREFERENCE) {
		const advertised = hasReceiverCapability(codec);
		result[codec] = advertised === null ? result[codec] : advertised && !exclusions.has(codec);
	}
	return result;
}

export function getScreenShareCodecPreferenceOrder(
	preference: CodecPreference = VoiceSettings.getPreferredScreenShareCodec(),
): ReadonlyArray<VideoCodec> {
	const encoderMode = resolveEffectiveScreenShareEncoderMode(VoiceSettings.getScreenShareEncoderMode());
	const automaticOrder =
		encoderMode === 'software' ? SOFTWARE_CODEC_PREFERENCE : getHardwareFirstScreenShareCodecPreferenceOrder();
	if (preference !== 'auto') return [preference, ...automaticOrder.filter((codec) => codec !== preference)];
	return automaticOrder;
}

function getHardwareFirstScreenShareCodecPreferenceOrder(): ReadonlyArray<VideoCodec> {
	const report = getCodecCapabilityReport();
	const hardwareCodecs = CODEC_PREFERENCE.filter(
		(codec) => report[codec].supported && report[codec].hardwareAccelerated === 'hardware',
	);
	if (hardwareCodecs.length === 0) return SOFTWARE_CODEC_PREFERENCE;
	return [...hardwareCodecs, ...CODEC_PREFERENCE.filter((codec) => !hardwareCodecs.includes(codec))];
}

function getLocalEncodeCapabilities(): Record<VideoCodec, boolean> {
	const report = getCodecCapabilityReport();
	const encoderMode = resolveEffectiveScreenShareEncoderMode(VoiceSettings.getScreenShareEncoderMode());
	const pinned = VoiceSettings.getPreferredScreenShareCodec();
	const hardwareEncodeAvailable = CODEC_PREFERENCE.some(
		(codec) => report[codec].supported && report[codec].hardwareAccelerated === 'hardware',
	);
	const advertise = (codec: VideoCodec): boolean =>
		(pinned === codec && report[codec].supported) ||
		shouldAdvertiseVideoEncode(codec, report[codec], encoderMode, hardwareEncodeAvailable, report);
	return {
		av1: advertise('av1'),
		h265: advertise('h265'),
		h264: advertise('h264'),
		vp9: advertise('vp9'),
		vp8: report.vp8.supported,
	};
}

function shouldAdvertiseVideoEncode(
	codec: VideoCodec,
	info: CodecSupportInfo,
	encoderMode: ScreenShareEncoderMode,
	hardwareEncodeAvailable: boolean,
	report: CodecCapabilityReport,
): boolean {
	if (!info.supported) return false;
	if (encoderMode === 'software') {
		if (codec === 'h265') return false;
		return true;
	}
	if (hardwareEncodeAvailable) {
		if (codec === 'h264') return true;
		if (codec === 'vp8') return !report.h264.supported;
		return info.hardwareAccelerated === 'hardware';
	}
	if (codec === 'h265') return false;
	return true;
}

function buildVideoCodecAdvertisement(
	codec: VideoCodec,
	encodeCaps: Record<VideoCodec, boolean>,
	decodeCaps: Record<VideoCodec, boolean>,
): FluxerCodecAdvertisement {
	const table = VIDEO_CODEC_PROTOCOL_TABLE[codec];
	return {
		name: VIDEO_CODEC_NAMES[codec],
		type: 'video',
		payload_type: table.payloadType,
		rtx_payload_type: table.rtxPayloadType,
		priority: table.priority,
		encode: encodeCaps[codec],
		decode: decodeCaps[codec],
	};
}

export function buildLocalCodecAdvertisements(): Array<FluxerCodecAdvertisement> {
	const encodeCaps = getLocalEncodeCapabilities();
	const decodeCaps = getLocalDecodeCapabilities();
	return [
		{
			name: 'opus',
			type: 'audio',
			payload_type: 120,
			priority: 1000,
			encode: true,
			decode: true,
		},
		...CODEC_PREFERENCE.map((codec) => buildVideoCodecAdvertisement(codec, encodeCaps, decodeCaps)),
	];
}

function getDecodeSet(codecs: ReadonlyArray<FluxerCodecAdvertisement>): Set<VideoCodec> {
	const result = new Set<VideoCodec>();
	for (const codec of codecs) {
		if (codec.type !== 'video' || codec.decode !== true) continue;
		const mapped = NAME_TO_VIDEO_CODEC[codec.name as FluxerVideoCodecName];
		if (mapped) result.add(mapped);
	}
	return result;
}

function getEncodeSet(codecs: ReadonlyArray<FluxerCodecAdvertisement>): Set<VideoCodec> {
	const result = new Set<VideoCodec>();
	for (const codec of codecs) {
		if (codec.type !== 'video' || codec.encode !== true) continue;
		const mapped = NAME_TO_VIDEO_CODEC[codec.name as FluxerVideoCodecName];
		if (mapped) result.add(mapped);
	}
	return result;
}

function selectCompatibilityFallbackCodec(localEncode: ReadonlySet<VideoCodec>): VideoCodec {
	for (const codec of COMPATIBILITY_FALLBACK_CODEC_PREFERENCE) {
		if (localEncode.has(codec)) return codec;
	}
	return BASELINE_VIDEO_CODEC;
}

export function computeNegotiatedVideoCodec(
	localCodecs: ReadonlyArray<FluxerCodecAdvertisement>,
	remoteCodecs: ReadonlyArray<ReadonlyArray<FluxerCodecAdvertisement>>,
	unknownParticipants = 0,
	codecPreference: ReadonlyArray<VideoCodec> = CODEC_PREFERENCE,
): CodecNegotiationSelection {
	const localEncode = getEncodeSet(localCodecs);
	const candidates = codecPreference.filter((codec) => localEncode.has(codec));
	const constrainedCandidates = candidates.filter((codec) => {
		if (unknownParticipants > 0 && !COMPATIBILITY_FALLBACK_CODEC_PREFERENCE.includes(codec)) return false;
		for (const remote of remoteCodecs) {
			if (!getDecodeSet(remote).has(codec)) return false;
		}
		return true;
	});
	const codec = constrainedCandidates[0] ?? selectCompatibilityFallbackCodec(localEncode);
	return {
		codec,
		reason: 'manual',
		candidates: constrainedCandidates,
		unknownParticipants,
	};
}

function evaluateScreenShareCodecNegotiation(
	event: Extract<ScreenShareCodecNegotiationMachineEvent, {type: 'negotiation.evaluate'}>,
): CodecNegotiationSelection {
	return {
		...computeNegotiatedVideoCodec(
			event.localCodecs,
			event.remoteCodecs,
			event.unknownParticipants,
			event.codecPreference,
		),
		reason: event.reason,
	};
}

export const screenShareCodecNegotiationStateMachine = setup({
	types: {} as {
		context: ScreenShareCodecNegotiationMachineContext;
		events: ScreenShareCodecNegotiationMachineEvent;
	},
	actions: {
		evaluate: assign(({event}) => {
			if (event.type !== 'negotiation.evaluate') return {};
			const selection = evaluateScreenShareCodecNegotiation(event);
			return {
				selectedCodec: selection.codec,
				selection,
			};
		}),
		reset: assign(() => ({
			selectedCodec: null,
			selection: null,
		})),
	},
}).createMachine({
	id: 'screenShareCodecNegotiation',
	context: () => ({
		selectedCodec: null,
		selection: null,
	}),
	initial: 'ready',
	states: {
		ready: {
			on: {
				'negotiation.evaluate': {actions: 'evaluate'},
				'negotiation.reset': {actions: 'reset'},
			},
		},
	},
});

export type ScreenShareCodecNegotiationSnapshot = SnapshotFrom<typeof screenShareCodecNegotiationStateMachine>;

export function createScreenShareCodecNegotiationSnapshot(): ScreenShareCodecNegotiationSnapshot {
	return getInitialSnapshot(screenShareCodecNegotiationStateMachine);
}

export function transitionScreenShareCodecNegotiationSnapshot(
	snapshot: ScreenShareCodecNegotiationSnapshot,
	event: ScreenShareCodecNegotiationMachineEvent,
): ScreenShareCodecNegotiationSnapshot {
	return transition(screenShareCodecNegotiationStateMachine, snapshot, event)[0] as ScreenShareCodecNegotiationSnapshot;
}

function isObject(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function isBooleanOrUndefined(value: unknown): value is boolean | undefined {
	return value === undefined || typeof value === 'boolean';
}

function isNumberOrUndefined(value: unknown): value is number | undefined {
	return value === undefined || typeof value === 'number';
}

function isFluxerVideoCodecName(value: unknown): value is FluxerVideoCodecName {
	return value === 'AV1' || value === 'H265' || value === 'H264' || value === 'VP9' || value === 'VP8';
}

function isFluxerCodecName(value: unknown): value is FluxerCodecName {
	return value === 'opus' || isFluxerVideoCodecName(value);
}

function isFluxerCodecType(value: unknown): value is FluxerCodecType {
	return value === 'audio' || value === 'video';
}

function isCodecAdvertisement(value: unknown): value is FluxerCodecAdvertisement {
	if (!isObject(value)) return false;
	return (
		isFluxerCodecName(value.name) &&
		isFluxerCodecType(value.type) &&
		typeof value.payload_type === 'number' &&
		isNumberOrUndefined(value.rtx_payload_type) &&
		typeof value.priority === 'number' &&
		isBooleanOrUndefined(value.encode) &&
		isBooleanOrUndefined(value.decode)
	);
}

function isCodecAdvertisementList(value: unknown): value is Array<FluxerCodecAdvertisement> {
	return Array.isArray(value) && value.every(isCodecAdvertisement);
}

function isSelectProtocolMessage(value: unknown): value is FluxerSelectProtocolMessage {
	if (!isObject(value) || value.op !== SELECT_PROTOCOL_OP || !isObject(value.d)) return false;
	const data = value.d.data;
	return (
		value.d.protocol === 'livekit' &&
		isObject(data) &&
		data.mode === 'livekit-sfu' &&
		isCodecAdvertisementList(value.d.codecs) &&
		(typeof value.d.rtc_connection_id === 'string' || value.d.rtc_connection_id === null) &&
		Array.isArray(value.d.experiments) &&
		value.d.experiments.every((experiment) => typeof experiment === 'string')
	);
}

function isNegotiationReason(value: unknown): value is NegotiationReason {
	return (
		value === 'connected' ||
		value === 'data' ||
		value === 'participant-connected' ||
		value === 'participant-disconnected' ||
		value === 'reconnected' ||
		value === 'manual'
	);
}

function isSessionUpdateMessage(value: unknown): value is FluxerSessionUpdateMessage {
	if (!isObject(value) || value.op !== SESSION_UPDATE_OP || !isObject(value.d)) return false;
	return (
		isFluxerVideoCodecName(value.d.video_codec) &&
		typeof value.d.media_session_id === 'string' &&
		isNegotiationReason(value.d.reason) &&
		isCodecAdvertisementList(value.d.codecs)
	);
}

function parseMessage(payload: Uint8Array): FluxerCodecNegotiationMessage | null {
	try {
		const parsed = JSON.parse(TEXT_DECODER.decode(payload)) as unknown;
		if (isSelectProtocolMessage(parsed)) return parsed;
		if (isSessionUpdateMessage(parsed)) return parsed;
		return null;
	} catch {
		return null;
	}
}

class ScreenShareCodecNegotiation {
	private room: Room | null = null;
	private bindDisposer: (() => void) | null = null;
	private selectedCodec: VideoCodec | null = null;
	private localCodecs: Array<FluxerCodecAdvertisement> = [];
	private remoteCodecsByIdentity = new Map<string, Array<FluxerCodecAdvertisement>>();
	private bindOptions: BindOptions = {};
	private rtcConnectionId = createId('rtc');
	private mediaSessionId = createId('media');
	private negotiationSnapshot = createScreenShareCodecNegotiationSnapshot();

	getSelectedCodec(): VideoCodec | null {
		return this.selectedCodec;
	}

	selectScreenShareCodec(preference: CodecPreference = 'auto'): VideoCodec {
		const selected = this.selectedCodec;
		const selector = (codecPreference: CodecPreference): VideoCodec =>
			selectOptimalScreenShareCodec(codecPreference, VoiceSettings.getScreenShareEncoderMode());
		if (preference === 'auto')
			return selected && this.canUseSelectedCodecForCurrentParticipants(selected)
				? selected
				: this.selectLocalEncodeFallback(selector, preference);
		return this.selectLocalEncodeFallback(selector, preference);
	}

	selectNativeScreenShareCodec(preference: CodecPreference = 'auto'): VideoCodec {
		const selected = this.selectedCodec;
		if (preference === 'auto')
			return selected && this.canUseSelectedCodecForCurrentParticipants(selected)
				? selected
				: this.selectLocalEncodeFallback(selectNativeScreenCaptureScreenShareCodec, preference);
		return this.selectLocalEncodeFallback(selectNativeScreenCaptureScreenShareCodec, preference);
	}

	private canLocalEncode(codec: VideoCodec): boolean {
		if (this.localCodecs.length === 0) this.localCodecs = buildLocalCodecAdvertisements();
		return getEncodeSet(this.localCodecs).has(codec);
	}

	private canUseSelectedCodecForCurrentParticipants(codec: VideoCodec): boolean {
		if (!this.canLocalEncode(codec)) return false;
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs();
		if (unknownParticipants > 0) return false;
		return knownRemoteCodecs.every((remote) => getDecodeSet(remote).has(codec));
	}

	private selectLocalEncodeFallback(
		selector: (preference: CodecPreference) => VideoCodec,
		preference: CodecPreference,
	): VideoCodec {
		if (this.selectedCodec && this.canUseSelectedCodecForCurrentParticipants(this.selectedCodec)) {
			return this.selectedCodec;
		}
		if (this.localCodecs.length === 0) this.localCodecs = buildLocalCodecAdvertisements();
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs();
		const negotiated = computeNegotiatedVideoCodec(
			this.localCodecs,
			knownRemoteCodecs,
			unknownParticipants,
			getScreenShareCodecPreferenceOrder(preference),
		);
		return negotiated.codec ?? selector('auto');
	}

	private getRemoteCodecInputs(room: Room | null = this.room): {
		knownRemoteCodecs: Array<Array<FluxerCodecAdvertisement>>;
		unknownParticipants: number;
	} {
		const knownRemoteCodecs: Array<Array<FluxerCodecAdvertisement>> = [];
		let unknownParticipants = 0;
		for (const participant of room?.remoteParticipants.values() ?? []) {
			const codecs = this.remoteCodecsByIdentity.get(participant.identity);
			if (codecs) {
				knownRemoteCodecs.push(codecs);
			} else {
				unknownParticipants++;
			}
		}
		return {knownRemoteCodecs, unknownParticipants};
	}

	bind(room: Room, options: BindOptions = {}): () => void {
		this.dispose();
		this.room = room;
		this.bindOptions = options;
		const onDataReceived = (
			payload: Uint8Array,
			participant: Participant | undefined,
			_kind: unknown,
			topic?: string,
		): void => {
			if (topic !== PROTOCOL_TOPIC || !participant) return;
			this.handleDataMessage(room, participant, payload, options);
		};
		const onParticipantConnected = (): void => {
			void this.publishLocalCapabilities(room, 'participant-connected', options);
		};
		const onParticipantDisconnected = (participant: Participant): void => {
			this.remoteCodecsByIdentity.delete(participant.identity);
			void this.updateSelection(room, 'participant-disconnected', options);
		};
		const onReconnected = (): void => {
			void this.publishLocalCapabilities(room, 'reconnected', options);
		};
		room.on(RoomEvent.DataReceived, onDataReceived);
		room.on(RoomEvent.ParticipantConnected, onParticipantConnected);
		room.on(RoomEvent.ParticipantDisconnected, onParticipantDisconnected);
		room.on(RoomEvent.Reconnected, onReconnected);
		void this.publishLocalCapabilities(room, 'connected', options);
		this.bindDisposer = () => {
			room.off(RoomEvent.DataReceived, onDataReceived);
			room.off(RoomEvent.ParticipantConnected, onParticipantConnected);
			room.off(RoomEvent.ParticipantDisconnected, onParticipantDisconnected);
			room.off(RoomEvent.Reconnected, onReconnected);
		};
		return this.bindDisposer;
	}

	dispose(): void {
		this.bindDisposer?.();
		this.bindDisposer = null;
		this.room = null;
		this.selectedCodec = null;
		this.localCodecs = [];
		this.bindOptions = {};
		this.remoteCodecsByIdentity.clear();
		this.mediaSessionId = createId('media');
		this.negotiationSnapshot = createScreenShareCodecNegotiationSnapshot();
	}

	async publishLocalCapabilities(
		room: Room | null = this.room,
		reason: NegotiationReason = 'manual',
		options: BindOptions = this.bindOptions,
	): Promise<CodecNegotiationSelection | null> {
		if (typeof window === 'undefined') return null;
		if (!room?.localParticipant) return null;
		this.room = room;
		await Promise.allSettled([
			loadGpuEncoderReport(),
			loadNativeHardwareEncoderCapabilities(),
			loadVideoDecoderExclusions(),
			loadOpenH264Status(),
		]);
		this.localCodecs = buildLocalCodecAdvertisements();
		const message: FluxerSelectProtocolMessage = {
			op: SELECT_PROTOCOL_OP,
			d: {
				protocol: 'livekit',
				data: {
					mode: 'livekit-sfu',
				},
				codecs: this.localCodecs,
				rtc_connection_id: this.rtcConnectionId,
				experiments: ['fixed_keyframe_interval', 'maintain_framerate', 'opus_red', 'transport_cc', 'loss_based_bwe_v2'],
			},
		};
		await this.publishMessage(room, message);
		return await this.updateSelection(room, reason, options);
	}

	private handleDataMessage(room: Room, participant: Participant, payload: Uint8Array, options: BindOptions): void {
		const message = parseMessage(payload);
		if (!message) return;
		if (message.op === SELECT_PROTOCOL_OP) {
			this.remoteCodecsByIdentity.set(participant.identity, message.d.codecs);
			void this.updateSelection(room, 'data', options);
		} else if (message.op === SESSION_UPDATE_OP) {
			this.remoteCodecsByIdentity.set(participant.identity, message.d.codecs);
			logger.debug('Received remote codec session update', {
				participantIdentity: participant.identity,
				videoCodec: message.d.video_codec,
				mediaSessionId: message.d.media_session_id,
				reason: message.d.reason,
			});
			void this.updateSelection(room, 'data', options);
		}
	}

	private async updateSelection(
		room: Room,
		reason: NegotiationReason,
		options: BindOptions,
	): Promise<CodecNegotiationSelection | null> {
		if (!room.localParticipant) return null;
		if (this.localCodecs.length === 0) this.localCodecs = buildLocalCodecAdvertisements();
		if (reason === 'participant-disconnected' && room.remoteParticipants.size === 0 && this.selectedCodec) {
			logger.debug('Keeping active screen share codec after last viewer disconnected', {
				codec: this.selectedCodec,
			});
			return null;
		}
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs(room);
		const previousCodec = this.selectedCodec;
		this.negotiationSnapshot = transitionScreenShareCodecNegotiationSnapshot(this.negotiationSnapshot, {
			type: 'negotiation.evaluate',
			localCodecs: this.localCodecs,
			remoteCodecs: knownRemoteCodecs,
			unknownParticipants,
			reason,
			codecPreference: getScreenShareCodecPreferenceOrder(),
		});
		const selection = this.negotiationSnapshot.context.selection;
		if (!selection) return null;
		this.selectedCodec = selection.codec;
		if (selection.codec === previousCodec) return selection;
		this.mediaSessionId = createId('media');
		logger.info('Selected screen share codec from XState capability intersection', selection);
		await this.publishSessionUpdate(room, selection);
		await options.onSelectedCodecChanged?.(selection);
		return selection;
	}

	private async publishSessionUpdate(room: Room, selection: CodecNegotiationSelection): Promise<void> {
		const message: FluxerSessionUpdateMessage = {
			op: SESSION_UPDATE_OP,
			d: {
				video_codec: VIDEO_CODEC_NAMES[selection.codec],
				media_session_id: this.mediaSessionId,
				reason: selection.reason,
				codecs: this.localCodecs,
			},
		};
		await this.publishMessage(room, message);
	}

	private async publishMessage(room: Room, message: FluxerCodecNegotiationMessage): Promise<void> {
		try {
			await room.localParticipant.publishData(TEXT_ENCODER.encode(JSON.stringify(message)), {
				reliable: true,
				topic: PROTOCOL_TOPIC,
			});
		} catch (error) {
			logger.debug('Failed to publish codec negotiation message', {error, op: message.op});
		}
	}
}

export {CODEC_PREFERENCE, PROTOCOL_TOPIC as SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC};

export default new ScreenShareCodecNegotiation();
