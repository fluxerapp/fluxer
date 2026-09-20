// SPDX-License-Identifier: AGPL-3.0-or-later

import {getDesktopTroubleshootingSettings} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	getVoiceConnectionContextFromMediaEngine,
	getVoiceStateByConnectionIdFromMediaEngine,
} from '@app/features/voice/engine/VoiceMediaEngineBridge';
import {getStreamKeyForParticipantIdentity} from '@app/features/voice/engine/VoiceStreamWatchState';
import {
	getLocalScreenShareVideoPublications,
	isLiveLocalTrackPublication,
} from '@app/features/voice/engine/VoiceTrackPublicationUtils';
import ScreenShareDeliveryRollout from '@app/features/voice/state/ScreenShareDeliveryRollout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	buildScreenShareCodecProfile,
	type CodecPreference,
	isVideoCodecAllowedForPublish,
	type ScreenShareEncoderMode,
	selectNativeScreenCaptureScreenShareCodec,
	selectOptimalScreenShareCodec,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {loadGpuEncoderReport} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {loadNativeHardwareEncoderCapabilities} from '@app/features/voice/utils/NativeHardwareEncoderCapabilities';
import {
	buildScreenShareCodecAdvertisements,
	CODEC_PREFERENCE,
	type CodecNegotiationSelection,
	computeNegotiatedVideoCodec,
	countUnknownScreenShareParticipants,
	type FluxerCodecAdvertisement,
	type FluxerCodecName,
	type FluxerCodecType,
	type FluxerVideoCodecName,
	getDecodeSet,
	getEncodeSet,
	isScreenShareCodecUpgrade,
	type NegotiationReason,
	rankScreenShareCodecs,
	SCREEN_SHARE_CODEC_ADVERTISEMENT_GRACE_MS,
	SCREEN_SHARE_CODEC_CHANGE_SUPPRESSION_MS,
	VIDEO_CODEC_NAMES,
} from '@app/features/voice/utils/ScreenShareCodecSelection';
import {
	getScreenShareDecodeFailures,
	getVideoDecoderExclusionsSync,
	loadVideoDecoderExclusions,
} from '@app/features/voice/utils/VideoDecoderCapabilities';
import {parseVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import type {LocalTrackPublication, Participant, Room, VideoCodec} from 'livekit-client';
import {RoomEvent} from 'livekit-client';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

const logger = new Logger('ScreenShareCodecNegotiation');
const PROTOCOL_TOPIC = 'fluxer.rtc.codec-negotiation.v1';
const SELECT_PROTOCOL_OP = 1;
const SESSION_UPDATE_OP = 14;
const TEXT_ENCODER = new TextEncoder();
const TEXT_DECODER = new TextDecoder('utf-8', {fatal: true});
const NEGOTIATION_MESSAGE_BYTES_MAX = 16 * 1024;
const CODEC_ADVERTISEMENTS_MAX = 16;
const NEGOTIATION_IDENTIFIER_CHARS_MAX = 256;
const EXPERIMENTS_MAX = 16;
const EXPERIMENT_NAME_CHARS_MAX = 128;
const RTP_PAYLOAD_TYPE_MAX = 255;
const CODEC_PRIORITY_MAX = 65_535;

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
			publishedCodec: VideoCodec | null;
	  }
	| {type: 'negotiation.reset'};

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
	for (const codec of getScreenShareDecodeFailures()) {
		exclusions.add(codec);
	}
	const result: Record<VideoCodec, boolean> = {
		av1: false,
		h265: false,
		h264: true,
		vp9: false,
		vp8: true,
	};
	for (const codec of CODEC_PREFERENCE) {
		const advertised = hasReceiverCapability(codec);
		result[codec] = (advertised === null ? result[codec] : advertised) && !exclusions.has(codec);
	}
	return result;
}

export function getScreenShareCodecPreferenceOrder(
	preference: CodecPreference = VoiceSettings.getPreferredScreenShareCodec(),
): ReadonlyArray<VideoCodec> {
	return rankScreenShareCodecs({
		profile: buildScreenShareCodecProfile(),
		encoderModeSetting: VoiceSettings.getScreenShareEncoderMode(),
		pin: preference,
	}).order;
}

export function buildLocalCodecAdvertisements(
	order: ReadonlyArray<VideoCodec> = getScreenShareCodecPreferenceOrder(),
): Array<FluxerCodecAdvertisement> {
	return buildScreenShareCodecAdvertisements(order, getLocalDecodeCapabilities());
}

function evaluateScreenShareCodecNegotiation(
	event: Extract<ScreenShareCodecNegotiationMachineEvent, {type: 'negotiation.evaluate'}>,
): CodecNegotiationSelection {
	const negotiated = computeNegotiatedVideoCodec(
		event.localCodecs,
		event.remoteCodecs,
		event.unknownParticipants,
		event.codecPreference,
	);
	const publishedCodec = event.publishedCodec;
	const codec =
		publishedCodec !== null && isScreenShareCodecUpgrade(event.codecPreference, publishedCodec, negotiated.codec)
			? publishedCodec
			: negotiated.codec;
	return {
		...negotiated,
		codec,
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
	return initialTransition(screenShareCodecNegotiationStateMachine)[0];
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

function isBoundedInteger(value: unknown, maximum: number): value is number {
	return typeof value === 'number' && Number.isSafeInteger(value) && value >= 0 && value <= maximum;
}

function isBoundedIntegerOrUndefined(value: unknown, maximum: number): value is number | undefined {
	return value === undefined || isBoundedInteger(value, maximum);
}

function isBoundedString(value: unknown, maximumLength: number): value is string {
	return typeof value === 'string' && value.length > 0 && value.length <= maximumLength;
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
		((value.name === 'opus' && value.type === 'audio') || (value.name !== 'opus' && value.type === 'video')) &&
		isBoundedInteger(value.payload_type, RTP_PAYLOAD_TYPE_MAX) &&
		isBoundedIntegerOrUndefined(value.rtx_payload_type, RTP_PAYLOAD_TYPE_MAX) &&
		isBoundedInteger(value.priority, CODEC_PRIORITY_MAX) &&
		isBooleanOrUndefined(value.encode) &&
		isBooleanOrUndefined(value.decode)
	);
}

function isCodecAdvertisementList(value: unknown): value is Array<FluxerCodecAdvertisement> {
	return (
		Array.isArray(value) &&
		value.length > 0 &&
		value.length <= CODEC_ADVERTISEMENTS_MAX &&
		value.every(isCodecAdvertisement)
	);
}

function isSelectProtocolMessage(value: unknown): value is FluxerSelectProtocolMessage {
	if (!isObject(value) || value.op !== SELECT_PROTOCOL_OP || !isObject(value.d)) return false;
	const data = value.d.data;
	return (
		value.d.protocol === 'livekit' &&
		isObject(data) &&
		data.mode === 'livekit-sfu' &&
		isCodecAdvertisementList(value.d.codecs) &&
		(isBoundedString(value.d.rtc_connection_id, NEGOTIATION_IDENTIFIER_CHARS_MAX) ||
			value.d.rtc_connection_id === null) &&
		Array.isArray(value.d.experiments) &&
		value.d.experiments.length <= EXPERIMENTS_MAX &&
		value.d.experiments.every((experiment) => isBoundedString(experiment, EXPERIMENT_NAME_CHARS_MAX))
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
		isBoundedString(value.d.media_session_id, NEGOTIATION_IDENTIFIER_CHARS_MAX) &&
		isNegotiationReason(value.d.reason) &&
		isCodecAdvertisementList(value.d.codecs)
	);
}

function parseMessage(payload: Uint8Array): FluxerCodecNegotiationMessage | null {
	if (payload.byteLength === 0 || payload.byteLength > NEGOTIATION_MESSAGE_BYTES_MAX) return null;
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
	private remoteFirstSeenAt = new Map<string, number>();
	private answeredIdentities = new Set<string>();
	private graceTimers = new Map<string, ReturnType<typeof setTimeout>>();
	private rtcConnectionId = createId('rtc');
	private mediaSessionId = createId('media');
	private negotiationSnapshot = createScreenShareCodecNegotiationSnapshot();
	private bindingRevision = 0;
	private frozenCodecPreference: {
		preference: CodecPreference;
		encoderMode: ScreenShareEncoderMode;
		order: ReadonlyArray<VideoCodec>;
	} | null = null;
	private frozenDelivery: {trackSid: string; enabled: boolean} | null = null;
	private lastCodecChangeAt = 0;
	private suppressionTimer: ReturnType<typeof setTimeout> | null = null;
	private publishedTrackSid: string | null = null;
	private onSelectionChanged: ((room: Room, codec: VideoCodec, reason: NegotiationReason) => void) | null = null;

	getSelectedCodec(): VideoCodec | null {
		return this.selectedCodec;
	}

	getLocalCodecAdvertisements(): Array<FluxerCodecAdvertisement> {
		if (this.localCodecs.length === 0) return buildLocalCodecAdvertisements();
		return [...this.localCodecs];
	}

	getRemoteDecodeCodecsByIdentity(): Record<string, Array<VideoCodec>> {
		const result: Record<string, Array<VideoCodec>> = {};
		for (const [identity, codecs] of this.remoteCodecsByIdentity) {
			result[identity] = [...getDecodeSet(codecs)];
		}
		return result;
	}

	getRemoteDecodeInputs(): {knownDecode: Array<Set<VideoCodec>>; unknownParticipants: number} {
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs();
		return {knownDecode: knownRemoteCodecs.map((codecs) => getDecodeSet(codecs)), unknownParticipants};
	}

	setSelectionChangeListener(
		listener: ((room: Room, codec: VideoCodec, reason: NegotiationReason) => void) | null,
	): void {
		this.onSelectionChanged = listener;
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
		this.ensureLocalCodecAdvertisements();
		return getEncodeSet(this.localCodecs).has(codec);
	}

	private canUseSelectedCodecForCurrentParticipants(codec: VideoCodec): boolean {
		if (!isVideoCodecAllowedForPublish(codec)) return false;
		if (!this.canLocalEncode(codec)) return false;
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs();
		if (unknownParticipants > 0) return false;
		return knownRemoteCodecs.every((remote) => getDecodeSet(remote).has(codec));
	}

	private selectLocalEncodeFallback(
		selector: (preference: CodecPreference) => VideoCodec,
		preference: CodecPreference,
	): VideoCodec {
		if (
			preference === 'auto' &&
			this.selectedCodec &&
			this.canUseSelectedCodecForCurrentParticipants(this.selectedCodec)
		) {
			return this.selectedCodec;
		}
		this.ensureLocalCodecAdvertisements();
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs();
		const negotiated = computeNegotiatedVideoCodec(
			this.localCodecs,
			knownRemoteCodecs,
			unknownParticipants,
			this.resolveCodecPreferenceOrder(preference),
		);
		if (getEncodeSet(this.localCodecs).has(negotiated.codec)) return negotiated.codec;
		return selector(preference);
	}

	private ensureLocalCodecAdvertisements(): void {
		if (this.localCodecs.length > 0) return;
		this.localCodecs = buildLocalCodecAdvertisements(this.resolveCodecPreferenceOrder());
	}

	private resolveScreenShareDelivery(room: Room | null = this.room): boolean {
		const trackSid = this.getLocalScreenSharePublication(room)?.trackSid ?? null;
		if (trackSid === null) {
			this.frozenDelivery = null;
			return ScreenShareDeliveryRollout.enabled;
		}
		if (this.frozenDelivery?.trackSid === trackSid) return this.frozenDelivery.enabled;
		const enabled = ScreenShareDeliveryRollout.enabled;
		this.frozenDelivery = {trackSid, enabled};
		return enabled;
	}

	private resolveCodecPreferenceOrder(
		preference: CodecPreference = VoiceSettings.getPreferredScreenShareCodec(),
	): ReadonlyArray<VideoCodec> {
		if (!this.resolveScreenShareDelivery()) return getScreenShareCodecPreferenceOrder(preference);
		const encoderMode = VoiceSettings.getScreenShareEncoderMode();
		const frozen = this.frozenCodecPreference;
		if (
			frozen !== null &&
			frozen.preference === preference &&
			frozen.encoderMode === encoderMode &&
			this.getLocalScreenSharePublication() !== null
		) {
			return frozen.order;
		}
		const order = getScreenShareCodecPreferenceOrder(preference);
		this.frozenCodecPreference = {preference, encoderMode, order};
		return order;
	}

	private getLocalScreenSharePublication(room: Room | null = this.room): LocalTrackPublication | null {
		const participant = room?.localParticipant;
		if (!participant) return null;
		return getLocalScreenShareVideoPublications(participant).find(isLiveLocalTrackPublication) ?? null;
	}

	private observePublishedScreenShareCodec(room: Room | null = this.room): VideoCodec | null {
		const publication = this.getLocalScreenSharePublication(room);
		const trackSid = publication?.trackSid ?? null;
		if (trackSid !== this.publishedTrackSid) {
			this.publishedTrackSid = trackSid;
			if (trackSid !== null) this.armCodecChangeSuppression();
		}
		if (!publication) return null;
		return (publication as {options?: {videoCodec?: VideoCodec}}).options?.videoCodec ?? null;
	}

	private getScreenShareViewerIdentities(room: Room | null): ReadonlySet<string> | null {
		const localIdentity = room?.localParticipant?.identity;
		if (!localIdentity || this.getLocalScreenSharePublication(room) === null) return null;
		const context = getVoiceConnectionContextFromMediaEngine();
		if (!context) return null;
		const streamKey = getStreamKeyForParticipantIdentity(context.guildId, context.channelId, localIdentity);
		if (!streamKey) return null;
		const viewers = new Set<string>();
		for (const participant of room?.remoteParticipants.values() ?? []) {
			const {connectionId} = parseVoiceParticipantIdentity(participant.identity);
			const viewerStreamKeys = getVoiceStateByConnectionIdFromMediaEngine(connectionId)?.viewer_stream_keys;
			if (viewerStreamKeys === undefined || viewerStreamKeys.includes(streamKey)) {
				viewers.add(participant.identity);
			}
		}
		return viewers;
	}

	private getRemoteCodecInputs(room: Room | null = this.room): {
		knownRemoteCodecs: Array<Array<FluxerCodecAdvertisement>>;
		unknownParticipants: number;
	} {
		const now = Date.now();
		const viewerIdentities = this.resolveScreenShareDelivery(room) ? this.getScreenShareViewerIdentities(room) : null;
		const knownRemoteCodecs: Array<Array<FluxerCodecAdvertisement>> = [];
		const participants: Array<{identity: string; firstSeenAt: number}> = [];
		for (const participant of room?.remoteParticipants.values() ?? []) {
			const firstSeenAt = this.remoteFirstSeenAt.get(participant.identity) ?? now;
			this.remoteFirstSeenAt.set(participant.identity, firstSeenAt);
			if (viewerIdentities !== null && !viewerIdentities.has(participant.identity)) continue;
			participants.push({identity: participant.identity, firstSeenAt});
			const codecs = this.remoteCodecsByIdentity.get(participant.identity);
			if (codecs) knownRemoteCodecs.push(codecs);
		}
		return {
			knownRemoteCodecs,
			unknownParticipants: countUnknownScreenShareParticipants(
				participants,
				new Set(this.remoteCodecsByIdentity.keys()),
				now,
			),
		};
	}

	private armCodecChangeSuppression(): void {
		this.lastCodecChangeAt = Date.now();
	}

	private clearSuppressionTimer(): void {
		if (this.suppressionTimer === null) return;
		clearTimeout(this.suppressionTimer);
		this.suppressionTimer = null;
	}

	private scheduleSuppressedReevaluation(room: Room, reason: NegotiationReason, bindingRevision: number): void {
		if (this.suppressionTimer !== null) return;
		const remaining = Math.max(this.lastCodecChangeAt + SCREEN_SHARE_CODEC_CHANGE_SUPPRESSION_MS - Date.now(), 0);
		this.suppressionTimer = setTimeout(() => {
			this.suppressionTimer = null;
			void this.updateSelection(room, reason, bindingRevision);
		}, remaining);
	}

	private scheduleGraceReevaluations(room: Room, bindingRevision: number): void {
		const now = Date.now();
		for (const participant of room.remoteParticipants.values()) {
			const {identity} = participant;
			if (this.remoteCodecsByIdentity.has(identity) || this.graceTimers.has(identity)) continue;
			const remaining = (this.remoteFirstSeenAt.get(identity) ?? now) + SCREEN_SHARE_CODEC_ADVERTISEMENT_GRACE_MS - now;
			if (remaining <= 0) continue;
			this.graceTimers.set(
				identity,
				setTimeout(() => {
					this.graceTimers.delete(identity);
					void this.updateSelection(room, 'participant-connected', bindingRevision);
				}, remaining),
			);
		}
	}

	private clearGraceTimer(identity: string): void {
		const timer = this.graceTimers.get(identity);
		if (timer === undefined) return;
		clearTimeout(timer);
		this.graceTimers.delete(identity);
	}

	private forgetIdentity(identity: string): void {
		this.clearGraceTimer(identity);
		this.remoteCodecsByIdentity.delete(identity);
		this.remoteFirstSeenAt.delete(identity);
		this.answeredIdentities.delete(identity);
	}

	bind(room: Room): () => void {
		this.dispose();
		this.room = room;
		const bindingRevision = this.bindingRevision;
		const onDataReceived = (
			payload: Uint8Array,
			participant: Participant | undefined,
			_kind: unknown,
			topic?: string,
		): void => {
			if (topic !== PROTOCOL_TOPIC || !participant) return;
			this.handleDataMessage(room, participant, payload, bindingRevision);
		};
		const onParticipantConnected = (): void => {
			void this.publishBoundLocalCapabilities(room, 'participant-connected', bindingRevision);
		};
		const onParticipantDisconnected = (participant: Participant): void => {
			if (!this.isBindingCurrent(room, bindingRevision)) return;
			this.forgetIdentity(participant.identity);
			void this.updateSelection(room, 'participant-disconnected', bindingRevision);
		};
		const onReconnected = (): void => {
			void this.publishBoundLocalCapabilities(room, 'reconnected', bindingRevision);
		};
		room.on(RoomEvent.DataReceived, onDataReceived);
		room.on(RoomEvent.ParticipantConnected, onParticipantConnected);
		room.on(RoomEvent.ParticipantDisconnected, onParticipantDisconnected);
		room.on(RoomEvent.Reconnected, onReconnected);
		void this.publishBoundLocalCapabilities(room, 'connected', bindingRevision);
		this.bindDisposer = () => {
			room.off(RoomEvent.DataReceived, onDataReceived);
			room.off(RoomEvent.ParticipantConnected, onParticipantConnected);
			room.off(RoomEvent.ParticipantDisconnected, onParticipantDisconnected);
			room.off(RoomEvent.Reconnected, onReconnected);
		};
		return this.bindDisposer;
	}

	dispose(): void {
		this.bindingRevision += 1;
		this.bindDisposer?.();
		this.bindDisposer = null;
		this.room = null;
		this.selectedCodec = null;
		this.localCodecs = [];
		this.frozenCodecPreference = null;
		this.frozenDelivery = null;
		this.lastCodecChangeAt = 0;
		this.publishedTrackSid = null;
		this.clearSuppressionTimer();
		for (const timer of this.graceTimers.values()) clearTimeout(timer);
		this.graceTimers.clear();
		this.remoteCodecsByIdentity.clear();
		this.remoteFirstSeenAt.clear();
		this.answeredIdentities.clear();
		this.mediaSessionId = createId('media');
		this.negotiationSnapshot = createScreenShareCodecNegotiationSnapshot();
	}

	async publishLocalCapabilities(
		room: Room | null = this.room,
		reason: NegotiationReason = 'manual',
	): Promise<CodecNegotiationSelection | null> {
		if (!room) return null;
		return await this.publishBoundLocalCapabilities(room, reason, this.bindingRevision);
	}

	async refreshSelection(room: Room | null = this.room): Promise<CodecNegotiationSelection | null> {
		if (!room) return null;
		this.frozenCodecPreference = null;
		return await this.updateSelection(room, 'manual', this.bindingRevision);
	}

	private async publishBoundLocalCapabilities(
		room: Room,
		reason: NegotiationReason,
		bindingRevision: number,
	): Promise<CodecNegotiationSelection | null> {
		if (typeof window === 'undefined') return null;
		if (!room.localParticipant || !this.isBindingCurrent(room, bindingRevision)) return null;
		await Promise.allSettled([
			loadGpuEncoderReport(),
			loadNativeHardwareEncoderCapabilities(),
			loadVideoDecoderExclusions(),
			getDesktopTroubleshootingSettings(),
		]);
		if (!this.isBindingCurrent(room, bindingRevision)) return null;
		if (reason === 'manual') this.frozenCodecPreference = null;
		this.localCodecs = buildLocalCodecAdvertisements(this.resolveCodecPreferenceOrder());
		await this.publishSelectProtocol(room);
		if (!this.isBindingCurrent(room, bindingRevision)) return null;
		return await this.updateSelection(room, reason, bindingRevision);
	}

	private async publishSelectProtocol(room: Room): Promise<void> {
		this.ensureLocalCodecAdvertisements();
		const message: FluxerSelectProtocolMessage = {
			op: SELECT_PROTOCOL_OP,
			d: {
				protocol: 'livekit',
				data: {
					mode: 'livekit-sfu',
				},
				codecs: this.localCodecs,
				rtc_connection_id: this.rtcConnectionId,
				experiments: [],
			},
		};
		await this.publishMessage(room, message);
	}

	private handleDataMessage(room: Room, participant: Participant, payload: Uint8Array, bindingRevision: number): void {
		if (!this.isBindingCurrent(room, bindingRevision)) return;
		const message = parseMessage(payload);
		if (!message) return;
		this.remoteCodecsByIdentity.set(participant.identity, message.d.codecs);
		this.clearGraceTimer(participant.identity);
		if (message.op === SESSION_UPDATE_OP) {
			logger.debug('Received remote codec session update', {
				participantIdentity: participant.identity,
				videoCodec: message.d.video_codec,
				mediaSessionId: message.d.media_session_id,
				reason: message.d.reason,
			});
			void this.updateSelection(room, 'data', bindingRevision);
			return;
		}
		void this.answerSelectProtocol(room, participant.identity, bindingRevision);
	}

	private async answerSelectProtocol(room: Room, identity: string, bindingRevision: number): Promise<void> {
		if (!this.answeredIdentities.has(identity)) {
			this.answeredIdentities.add(identity);
			await this.publishSelectProtocol(room);
			if (!this.isBindingCurrent(room, bindingRevision)) return;
		}
		await this.updateSelection(room, 'data', bindingRevision);
	}

	private async updateSelection(
		room: Room,
		reason: NegotiationReason,
		bindingRevision: number,
	): Promise<CodecNegotiationSelection | null> {
		if (!room.localParticipant || !this.isBindingCurrent(room, bindingRevision)) return null;
		this.ensureLocalCodecAdvertisements();
		if (reason === 'participant-disconnected' && room.remoteParticipants.size === 0 && this.selectedCodec) {
			logger.debug('Keeping active screen share codec after last viewer disconnected', {
				codec: this.selectedCodec,
			});
			return null;
		}
		const {knownRemoteCodecs, unknownParticipants} = this.getRemoteCodecInputs(room);
		this.scheduleGraceReevaluations(room, bindingRevision);
		const previousCodec = this.selectedCodec;
		const delivery = this.resolveScreenShareDelivery(room);
		this.negotiationSnapshot = transitionScreenShareCodecNegotiationSnapshot(this.negotiationSnapshot, {
			type: 'negotiation.evaluate',
			localCodecs: this.localCodecs,
			remoteCodecs: knownRemoteCodecs,
			unknownParticipants,
			reason,
			codecPreference: this.resolveCodecPreferenceOrder(),
			publishedCodec: delivery ? this.observePublishedScreenShareCodec(room) : null,
		});
		const selection = this.negotiationSnapshot.context.selection;
		if (!selection) return null;
		if (selection.codec === previousCodec) {
			this.selectedCodec = selection.codec;
			return selection;
		}
		if (
			delivery &&
			previousCodec !== null &&
			Date.now() - this.lastCodecChangeAt < SCREEN_SHARE_CODEC_CHANGE_SUPPRESSION_MS
		) {
			logger.debug('Suppressed a screen share codec change inside the change window', {
				codec: selection.codec,
				previousCodec,
				reason,
			});
			this.scheduleSuppressedReevaluation(room, reason, bindingRevision);
			return selection;
		}
		this.selectedCodec = selection.codec;
		if (delivery) this.armCodecChangeSuppression();
		this.mediaSessionId = createId('media');
		logger.info('Selected screen share codec from XState capability intersection', selection);
		await this.publishSessionUpdate(room, selection);
		if (!this.isBindingCurrent(room, bindingRevision)) return null;
		this.onSelectionChanged?.(room, selection.codec, selection.reason);
		return selection;
	}

	private isBindingCurrent(room: Room, bindingRevision: number): boolean {
		return this.room === room && this.bindingRevision === bindingRevision;
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

export {PROTOCOL_TOPIC as SCREEN_SHARE_CODEC_NEGOTIATION_TOPIC};

export default new ScreenShareCodecNegotiation();
