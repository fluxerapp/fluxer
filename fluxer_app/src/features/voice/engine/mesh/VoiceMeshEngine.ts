// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {VoiceSignalSendResult} from '@app/features/gateway/transport/GatewaySocket';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	flushVoiceMeshConnectionReports,
	queueVoiceMeshConnectionReport,
	readVoiceMeshConnectionPath,
	UNKNOWN_VOICE_MESH_CONNECTION_PATH,
} from '@app/features/voice/engine/mesh/VoiceMeshConnectionReports';
import {VoiceMeshPeer} from '@app/features/voice/engine/mesh/VoiceMeshPeer';
import VoiceMeshPeers from '@app/features/voice/engine/mesh/VoiceMeshPeers';
import {
	VOICE_MESH_TRACK_KINDS,
	VOICE_MESH_TRACK_SOURCES,
	VoiceMeshSender,
} from '@app/features/voice/engine/mesh/VoiceMeshSender';
import {
	sendVoiceMeshSignal,
	subscribeVoiceMeshSignals,
	VOICE_MESH_SOURCES,
	VOICE_MESH_TOKEN,
	type VoiceMeshIncomingSignal,
	type VoiceMeshSignalData,
	type VoiceMeshSource,
	type VoiceMeshTrackInfo,
	voiceMeshTrackSid,
} from '@app/features/voice/engine/mesh/VoiceMeshSignal';
import {VoiceMeshSignalClient} from '@app/features/voice/engine/mesh/VoiceMeshSignalClient';
import type {NormalizedVoiceState} from '@app/features/voice/engine/VoiceGatewayStateMachine';
import {getSharedVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {
	getRemoteSpeakingThresholdRms,
	SPEAKING_REMOTE_RELEASE_MS,
} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import {computeTimeDomainRms} from '@app/features/voice/engine/v2/VoiceEngineV2AppRemoteSpeakingAdapter';
import voiceEngineV2AppVoiceStateAdapter from '@app/features/voice/engine/v2/VoiceEngineV2AppVoiceStateAdapter';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {ME} from '@fluxer/constants/src/AppConstants';
import {
	type AddTrackRequest,
	Codec,
	DisconnectReason,
	EngineEvent,
	type InternalRoomOptions,
	JoinResponse,
	type LocalTrack,
	LocalVideoTrack,
	ParticipantInfo,
	ParticipantInfo_State,
	ParticipantPermission,
	type PCTransportManager,
	PCTransportState,
	type Room,
	RTCEngine,
	type SignalOptions,
	SpeakerInfo,
	Track,
	TrackInfo,
	TrackInvalidError,
	type TrackPublishOptions,
} from 'livekit-client';
import {reaction} from 'mobx';

const logger = new Logger('VoiceMeshEngine');

const INBOX_LIMIT = 64;
const CONVERSION_GRACE_MS = 10_000;
const SPEAKING_TICK_MS = 50;
const SCREEN_SHARE_BUDGET_BPS = 12_000_000;
const CAMERA_BUDGET_BPS = 4_000_000;
const MESH_VIDEO_CODEC_MIME_TYPES = ['video/H264', 'video/VP8'];

export interface VoiceMeshTarget {
	iceServers: Array<Pick<RTCIceServer, 'urls'>>;
	guildId: string | null;
	channelId: string;
	connectionId: string;
	userId: string;
}

interface SpeakingAnalyser {
	track: MediaStreamTrack;
	source: MediaStreamAudioSourceNode;
	analyser: AnalyserNode;
	samples: Float32Array<ArrayBuffer>;
	timer: ReturnType<typeof setInterval>;
	lastLoudAt: number | null;
	active: boolean;
}

let activeEngine: VoiceMeshEngine | null = null;

function mergeStatsReports(reports: ReadonlyArray<readonly [string, RTCStatsReport]>): RTCStatsReport {
	const merged = new Map<string, Record<string, unknown>>();
	for (const [prefix, report] of reports) {
		report.forEach((stat: Record<string, unknown>) => {
			const entry: Record<string, unknown> = {...stat};
			for (const [key, value] of Object.entries(entry)) {
				if ((key === 'id' || key.endsWith('Id')) && typeof value === 'string') entry[key] = `${prefix}:${value}`;
			}
			merged.set(String(entry.id), entry);
		});
	}
	return merged as unknown as RTCStatsReport;
}

function applySendParameters(
	target: RTCRtpSendParameters,
	requested: RTCRtpSendParameters,
	bitrateCap: number | undefined,
): boolean {
	const encoding = target.encodings[0];
	if (!encoding) return false;
	const top = requested.encodings.findLast((candidate) => candidate.active !== false);
	const maxBitrate = bitrateCap === undefined ? top?.maxBitrate : Math.min(top?.maxBitrate ?? bitrateCap, bitrateCap);
	let changed = false;
	const assign = <K extends keyof RTCRtpEncodingParameters>(key: K, value: RTCRtpEncodingParameters[K] | undefined) => {
		if (value === undefined || encoding[key] === value) return;
		encoding[key] = value;
		changed = true;
	};
	assign('active', top !== undefined);
	assign('maxBitrate', maxBitrate);
	assign('maxFramerate', top?.maxFramerate);
	assign('scaleResolutionDownBy', top?.scaleResolutionDownBy);
	assign('priority', top?.priority);
	assign('networkPriority', top?.networkPriority);
	if (
		requested.degradationPreference !== undefined &&
		target.degradationPreference !== requested.degradationPreference
	) {
		target.degradationPreference = requested.degradationPreference;
		changed = true;
	}
	return changed;
}

function sameSids(a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean {
	return a.length === b.length && a.every((sid, index) => sid === b[index]);
}

export class VoiceMeshEngine extends RTCEngine {
	private readonly target: VoiceMeshTarget;
	private readonly signal: VoiceMeshSignalClient;
	private readonly senders: Readonly<Record<VoiceMeshSource, VoiceMeshSender>>;
	private readonly peers = new Map<string, VoiceMeshPeer>();
	private readonly inbox = new Map<string, Array<VoiceMeshIncomingSignal>>();
	private readonly reportedPeers = new Set<string>();
	private lifecycle: 'new' | 'joined' | 'closed' = 'new';
	private room: Room | null = null;
	private disposers: Array<() => void> = [];
	private version = Date.now();
	private tracks: {v: number; tracks: Array<VoiceMeshTrackInfo>} = {v: this.version, tracks: []};
	private conversionTimer: ReturnType<typeof setTimeout> | null = null;
	private conversionHeld = false;
	private speaking: SpeakingAnalyser | null = null;

	constructor(options: InternalRoomOptions, target: VoiceMeshTarget) {
		super(options);
		this.target = target;
		this.signal = new VoiceMeshSignalClient(() => this.syncLocalInterest());
		this.client = this.signal;
		this.senders = {
			microphone: new VoiceMeshSender(this, 'microphone'),
			camera: new VoiceMeshSender(this, 'camera'),
			screen_share: new VoiceMeshSender(this, 'screen_share'),
			screen_share_audio: new VoiceMeshSender(this, 'screen_share_audio'),
		};
		this.pcManager = this.createTransportManager();
	}

	override get isClosed(): boolean {
		return this.lifecycle !== 'joined';
	}

	override get isNewlyCreated(): boolean {
		return this.lifecycle === 'new';
	}

	override get pendingReconnect(): boolean {
		return false;
	}

	override get serverVersion(): string | undefined {
		return undefined;
	}

	get iceServers(): Array<Pick<RTCIceServer, 'urls'>> {
		return this.target.iceServers;
	}

	private get localParticipantSid(): string {
		return `PA_${this.target.connectionId}`;
	}

	override async join(_url: string, _token: string, opts: SignalOptions): ReturnType<RTCEngine['join']> {
		this.lifecycle = 'joined';
		this.signal.open(opts);
		const joinResponse = new JoinResponse({
			participant: new ParticipantInfo({
				sid: this.localParticipantSid,
				identity: buildVoiceParticipantIdentity(this.target.userId, this.target.connectionId),
				state: ParticipantInfo_State.ACTIVE,
				permission: new ParticipantPermission({canPublish: true, canSubscribe: true}),
			}),
			enabledPublishCodecs: MESH_VIDEO_CODEC_MIME_TYPES.map((mime) => new Codec({mime})),
		});
		this.emit(EngineEvent.SignalConnected, joinResponse);
		return {joinResponse, serverInfo: {version: VOICE_MESH_TOKEN}};
	}

	override async close(): Promise<void> {
		if (this.lifecycle === 'closed') return;
		this.lifecycle = 'closed';
		this.emit(EngineEvent.Closing);
		this.removeAllListeners();
		this.stopMesh();
		await this.signal.close();
	}

	start(room: Room): void {
		if (this.lifecycle !== 'joined' || this.room) return;
		activeEngine?.stopMesh();
		activeEngine = this;
		this.room = room;
		this.disposers = [
			subscribeVoiceMeshSignals(this.handleSignal),
			voiceEngineV2AppVoiceStateAdapter.subscribe(this.syncRoster),
			reaction(
				() => GatewayConnection.isConnected,
				(connected) => {
					if (connected) this.handleGatewayReconnected();
				},
			),
		];
		this.syncRoster();
		this.syncSpeaking();
	}

	override async createSender(
		track: LocalTrack,
		opts: TrackPublishOptions,
		encodings?: Array<RTCRtpEncodingParameters>,
	): Promise<RTCRtpSender> {
		const sender = this.senders[this.sourceOf(track.source)];
		if (track instanceof LocalVideoTrack) track.codec = opts.videoCodec;
		sender.attach(track.mediaStreamTrack, encodings);
		return sender as unknown as RTCRtpSender;
	}

	override async addTrack(req: AddTrackRequest): Promise<TrackInfo> {
		const src = this.sourceOf(Track.sourceFromProto(req.source));
		const sid = voiceMeshTrackSid(this.target.connectionId, src);
		this.senders[src].publish({sid, muted: req.muted, width: req.width, height: req.height});
		return new TrackInfo({
			sid,
			type: req.type,
			source: req.source,
			name: req.name,
			muted: req.muted,
			width: req.width,
			height: req.height,
			stream: req.stream,
		});
	}

	override removeTrack(sender: RTCRtpSender): boolean {
		if (sender instanceof VoiceMeshSender) sender.unpublish();
		return false;
	}

	override updateMuteStatus(trackSid: string, muted: boolean): void {
		Object.values(this.senders)
			.find((sender) => sender.sid === trackSid)
			?.setMuted(muted);
	}

	override negotiate(): Promise<void> {
		return Promise.resolve();
	}

	override waitForPCInitialConnection(): Promise<void> {
		return Promise.resolve();
	}

	override verifyTransport(): boolean {
		return !this.isClosed;
	}

	override sendDataPacket(): Promise<void> {
		return Promise.resolve();
	}

	syncSender(src: VoiceMeshSource): Promise<void> {
		this.refreshTracks();
		if (src === 'microphone') this.syncSpeaking();
		return Promise.all(Array.from(this.peers.values(), (peer) => peer.requestSenderSync(src))).then(() => undefined);
	}

	async collectSenderStats(src: VoiceMeshSource): Promise<RTCStatsReport> {
		const reports = await Promise.all(
			Array.from(this.peers.values()).flatMap((peer) => {
				const rtpSender = peer.rtpSender(src);
				if (!rtpSender) return [];
				return [rtpSender.getStats().then((report) => [peer.connectionId, report] as const)];
			}),
		);
		return mergeStatsReports(reports);
	}

	sendPeerSignal(peer: VoiceMeshPeer, data: VoiceMeshSignalData): VoiceSignalSendResult {
		if (!this.room) return VoiceSignalSendResult.Unsent;
		const {guildId, channelId} = this.target;
		return sendVoiceMeshSignal({guildId, channelId, to: peer.connectionId}, data);
	}

	handlePeerStatus(peer: VoiceMeshPeer): void {
		if (this.peers.get(peer.connectionId) !== peer) return;
		const {status} = peer;
		VoiceMeshPeers.setStatus(peer.connectionId, status);
		if (status === 'connecting') return;
		this.reportPeer(peer, status);
		if (status !== 'connected') return;
		for (const src of VOICE_MESH_SOURCES) void peer.requestSenderSync(src);
	}

	private reportPeer(peer: VoiceMeshPeer, outcome: 'connected' | 'failed'): void {
		if (this.reportedPeers.has(peer.connectionId)) return;
		this.reportedPeers.add(peer.connectionId);
		const report = {
			channel_id: this.target.channelId,
			guild_id: this.target.guildId,
			participant_count: this.peers.size + 1,
			outcome,
			setup_ms: peer.setupMs,
			ice_restarted: peer.iceRestarted,
		};
		const stats = peer.getStats();
		if (!stats) {
			queueVoiceMeshConnectionReport({...report, ...UNKNOWN_VOICE_MESH_CONNECTION_PATH});
			return;
		}
		void stats
			.then(readVoiceMeshConnectionPath, () => UNKNOWN_VOICE_MESH_CONNECTION_PATH)
			.then((path) => queueVoiceMeshConnectionReport({...report, ...path}));
	}

	handlePeerTransport(peer: VoiceMeshPeer): void {
		for (const src of VOICE_MESH_SOURCES) {
			void peer.requestSenderSync(src);
			this.syncPeerReceiver(peer, src);
		}
		if (peer.status === 'failed') return;
		peer.send({t: 'tracks', ...this.tracks});
		if (peer.localWant) peer.send({t: 'want', ...peer.localWant});
	}

	handlePeerTracks(peer: VoiceMeshPeer): void {
		if (this.peers.get(peer.connectionId) !== peer) return;
		this.emitPeerInfo(peer, ParticipantInfo_State.ACTIVE);
		this.syncPeerInterest(peer);
	}

	handlePeerWant(): void {
		this.syncAllSenders();
	}

	async syncPeerSender(peer: VoiceMeshPeer, src: VoiceMeshSource): Promise<void> {
		const rtpSender = peer.rtpSender(src);
		if (!rtpSender) return;
		const sender = this.senders[src];
		const track = sender.sid !== null && peer.remoteWant.has(sender.sid) ? sender.track : null;
		try {
			if (rtpSender.track !== track) await rtpSender.replaceTrack(track);
			if (peer.rtpSender(src) !== rtpSender) return;
			const parameters = rtpSender.getParameters();
			if (applySendParameters(parameters, sender.getParameters(), this.bitrateCap(src))) {
				await rtpSender.setParameters(parameters);
			}
		} catch (error) {
			logger.warn('Failed to sync mesh sender', {connectionId: peer.connectionId, src, error});
		}
	}

	private syncPeerReceiver(peer: VoiceMeshPeer, src: VoiceMeshSource): void {
		const participant = this.room?.remoteParticipants.get(peer.identity);
		const info = peer.remoteTracks.find((track) => track.src === src);
		if (!participant || !info) return;
		const publication = participant.getTrackPublicationBySid(info.sid);
		if (!publication) return;
		const receiver = peer.rtpReceiver(src);
		const serverMuted =
			src === 'microphone' &&
			voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(peer.connectionId)?.mute === true;
		if (receiver && receiver.track.readyState === 'live' && publication.isDesired && !serverMuted) {
			if (publication.track?.mediaStreamTrack !== receiver.track) {
				participant.addSubscribedMediaTrack(receiver.track, info.sid, new MediaStream([receiver.track]), receiver);
			}
			return;
		}
		if (!publication.track) return;
		publication.track.stop();
		publication.setTrack(undefined);
	}

	private readonly syncRoster = (): void => {
		if (!this.room) return;
		const {connectionId, channelId, guildId} = this.target;
		const own = voiceEngineV2AppVoiceStateAdapter.getVoiceStateByConnectionId(connectionId);
		const ownP2p = own?.channel_id === channelId ? own.p2p : undefined;
		this.syncConversionWatchdog(ownP2p === false && !this.conversionHeld);
		const roster = new Map<string, NormalizedVoiceState>();
		if (ownP2p === true) {
			const states = voiceEngineV2AppVoiceStateAdapter.getAllVoiceStatesInChannel(guildId ?? ME, channelId);
			for (const state of Object.values(states)) {
				if (state.p2p === true && state.connection_id !== connectionId) roster.set(state.connection_id, state);
			}
		}
		const before = this.peers.size;
		for (const peer of Array.from(this.peers.values())) {
			if (!roster.has(peer.connectionId)) this.removePeer(peer);
		}
		const kept = this.peers.size;
		for (const state of roster.values()) {
			if (!this.peers.has(state.connection_id)) this.addPeer(state);
		}
		if (kept !== before || this.peers.size !== kept) this.syncAllSenders();
		for (const peer of this.peers.values()) this.syncPeerReceiver(peer, 'microphone');
	};

	private addPeer(state: NormalizedVoiceState): void {
		const peer = new VoiceMeshPeer(this, state.connection_id, state.user_id, this.target.connectionId);
		this.peers.set(peer.connectionId, peer);
		VoiceMeshPeers.setStatus(peer.connectionId, peer.status);
		this.emitPeerInfo(peer, ParticipantInfo_State.ACTIVE);
		const pending = this.inbox.get(peer.connectionId) ?? [];
		this.inbox.delete(peer.connectionId);
		for (const signal of pending) {
			if (signal.userId === peer.userId) peer.handleSignal(signal.data);
		}
		peer.start();
	}

	private removePeer(peer: VoiceMeshPeer): void {
		peer.close();
		this.peers.delete(peer.connectionId);
		this.inbox.delete(peer.connectionId);
		VoiceMeshPeers.remove(peer.connectionId);
		this.emitPeerInfo(peer, ParticipantInfo_State.DISCONNECTED);
	}

	private emitPeerInfo(peer: VoiceMeshPeer, state: ParticipantInfo_State): void {
		peer.infoVersion += 1;
		const info = new ParticipantInfo({
			sid: peer.participantSid,
			identity: peer.identity,
			state,
			version: peer.infoVersion,
			permission: new ParticipantPermission({canPublish: true, canSubscribe: true}),
			tracks: peer.remoteTracks.map(
				(track) =>
					new TrackInfo({
						sid: track.sid,
						type: Track.kindToProto(VOICE_MESH_TRACK_KINDS[track.src]),
						source: Track.sourceToProto(VOICE_MESH_TRACK_SOURCES[track.src]),
						muted: track.m,
						width: track.w ?? 0,
						height: track.h ?? 0,
					}),
			),
		});
		this.emit(EngineEvent.ParticipantUpdate, [info]);
	}

	private readonly handleSignal = (signal: VoiceMeshIncomingSignal): void => {
		if (!this.room || signal.channelId !== this.target.channelId || signal.guildId !== this.target.guildId) return;
		const peer = this.peers.get(signal.from);
		if (peer) {
			if (signal.userId === peer.userId) peer.handleSignal(signal.data);
			return;
		}
		if (signal.from === this.target.connectionId) return;
		const pending = this.inbox.get(signal.from) ?? [];
		if (pending.length >= INBOX_LIMIT) return;
		pending.push(signal);
		this.inbox.set(signal.from, pending);
	};

	private handleGatewayReconnected(): void {
		for (const peer of this.peers.values()) {
			peer.resume();
			peer.send({t: 'tracks', ...this.tracks});
			if (peer.localWant) peer.send({t: 'want', ...peer.localWant});
		}
	}

	private syncLocalInterest(): void {
		if (!this.room) return;
		for (const peer of this.peers.values()) this.syncPeerInterest(peer);
	}

	private syncPeerInterest(peer: VoiceMeshPeer): void {
		const participant = this.room?.remoteParticipants.get(peer.identity);
		const sids = participant
			? Array.from(participant.trackPublications.values())
					.filter((publication) => publication.isDesired && publication.isEnabled)
					.map((publication) => publication.trackSid)
					.sort()
			: [];
		if (!sameSids(peer.localWant?.sids ?? [], sids)) {
			peer.localWant = {v: this.nextVersion(), sids};
			peer.send({t: 'want', ...peer.localWant});
		}
		for (const src of VOICE_MESH_SOURCES) this.syncPeerReceiver(peer, src);
	}

	private refreshTracks(): void {
		const tracks = VOICE_MESH_SOURCES.flatMap((src) => {
			const info = this.senders[src].info;
			return info ? [info] : [];
		});
		if (JSON.stringify(tracks) === JSON.stringify(this.tracks.tracks)) return;
		this.tracks = {v: this.nextVersion(), tracks};
		for (const peer of this.peers.values()) peer.send({t: 'tracks', ...this.tracks});
	}

	private syncAllSenders(): void {
		for (const peer of this.peers.values()) {
			for (const src of VOICE_MESH_SOURCES) void peer.requestSenderSync(src);
		}
	}

	private bitrateCap(src: VoiceMeshSource): number | undefined {
		switch (src) {
			case 'screen_share': {
				const sid = this.senders.screen_share.sid;
				let watchers = 0;
				for (const peer of this.peers.values()) {
					if (sid !== null && peer.remoteWant.has(sid)) watchers += 1;
				}
				return Math.floor(SCREEN_SHARE_BUDGET_BPS / Math.max(1, watchers));
			}
			case 'camera':
				return Math.floor(CAMERA_BUDGET_BPS / Math.max(1, this.peers.size));
			default:
				return undefined;
		}
	}

	holdConversion(held: boolean): void {
		this.conversionHeld = held;
		this.syncRoster();
	}

	private syncConversionWatchdog(converting: boolean): void {
		if (!converting) {
			this.clearConversionWatchdog();
			return;
		}
		if (this.conversionTimer) return;
		this.conversionTimer = setTimeout(() => {
			this.conversionTimer = null;
			logger.warn('Voice state left P2P mode without a standard call grant', {channelId: this.target.channelId});
			this.emit(EngineEvent.Disconnected, DisconnectReason.STATE_MISMATCH);
			void this.close();
		}, CONVERSION_GRACE_MS);
	}

	private clearConversionWatchdog(): void {
		if (!this.conversionTimer) return;
		clearTimeout(this.conversionTimer);
		this.conversionTimer = null;
	}

	private syncSpeaking(): void {
		const microphone = this.senders.microphone;
		const track = this.room && microphone.sid !== null ? microphone.track : null;
		if ((this.speaking?.track ?? null) === track) return;
		this.stopSpeaking();
		if (!track) return;
		const context = getSharedVoiceAudioContext();
		if (!context) return;
		const source = context.createMediaStreamSource(new MediaStream([track]));
		const analyser = context.createAnalyser();
		analyser.fftSize = 512;
		source.connect(analyser);
		const speaking: SpeakingAnalyser = {
			track,
			source,
			analyser,
			samples: new Float32Array(analyser.fftSize),
			timer: setInterval(() => this.tickSpeaking(speaking), SPEAKING_TICK_MS),
			lastLoudAt: null,
			active: false,
		};
		this.speaking = speaking;
	}

	private tickSpeaking(speaking: SpeakingAnalyser): void {
		speaking.analyser.getFloatTimeDomainData(speaking.samples);
		const level = computeTimeDomainRms(speaking.samples);
		const now = performance.now();
		if (level >= getRemoteSpeakingThresholdRms(VoiceSettings.getVadThreshold())) speaking.lastLoudAt = now;
		const active = speaking.lastLoudAt !== null && now - speaking.lastLoudAt < SPEAKING_REMOTE_RELEASE_MS;
		if (active === speaking.active) return;
		speaking.active = active;
		this.emitLocalSpeaking(level, active);
	}

	private stopSpeaking(): void {
		const speaking = this.speaking;
		if (!speaking) return;
		this.speaking = null;
		clearInterval(speaking.timer);
		speaking.source.disconnect();
		speaking.analyser.disconnect();
		if (speaking.active) this.emitLocalSpeaking(0, false);
	}

	private emitLocalSpeaking(level: number, active: boolean): void {
		this.emit(EngineEvent.SpeakersChanged, [new SpeakerInfo({sid: this.localParticipantSid, level, active})]);
	}

	private stopMesh(): void {
		if (!this.room) return;
		this.room = null;
		if (activeEngine === this) activeEngine = null;
		for (const dispose of this.disposers) dispose();
		this.disposers = [];
		this.clearConversionWatchdog();
		for (const peer of this.peers.values()) {
			peer.close();
			VoiceMeshPeers.remove(peer.connectionId);
		}
		this.peers.clear();
		this.inbox.clear();
		this.stopSpeaking();
		flushVoiceMeshConnectionReports();
	}

	private sourceOf(source: Track.Source): VoiceMeshSource {
		const src = VOICE_MESH_SOURCES.find((candidate) => VOICE_MESH_TRACK_SOURCES[candidate] === source);
		if (!src) throw new TrackInvalidError(`P2P calls cannot publish a ${source} track`);
		return src;
	}

	private nextVersion(): number {
		this.version += 1;
		return this.version;
	}

	private createTransportManager(): PCTransportManager {
		const isClosed = () => this.isClosed;
		return {
			publisher: {
				getStats: () => this.collectTransportStats(),
				getTransceivers: () => Array.from(this.peers.values(), (peer) => peer.rtpTransceivers).flat(),
				setTrackCodecBitrate: () => undefined,
				removeTrack: () => undefined,
			},
			mode: 'publisher-only',
			get currentState() {
				return isClosed() ? PCTransportState.CLOSED : PCTransportState.CONNECTED;
			},
		} as unknown as PCTransportManager;
	}

	private async collectTransportStats(): Promise<RTCStatsReport> {
		const reports = await Promise.all(
			Array.from(this.peers.values()).flatMap((peer) => {
				const stats = peer.getStats();
				return stats ? [stats.then((report) => [peer.connectionId, report] as const)] : [];
			}),
		);
		return mergeStatsReports(reports);
	}
}
