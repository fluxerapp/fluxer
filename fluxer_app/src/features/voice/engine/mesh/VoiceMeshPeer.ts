// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceSignalSendResult} from '@app/features/gateway/transport/GatewaySocket';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {VoiceMeshEngine} from '@app/features/voice/engine/mesh/VoiceMeshEngine';
import type {VoiceMeshPeerStatus} from '@app/features/voice/engine/mesh/VoiceMeshPeers';
import {VOICE_MESH_TRACK_KINDS} from '@app/features/voice/engine/mesh/VoiceMeshSender';
import {
	VOICE_MESH_SOURCES,
	type VoiceMeshSignalData,
	type VoiceMeshSource,
	type VoiceMeshTrackInfo,
	voiceMeshTrackSid,
} from '@app/features/voice/engine/mesh/VoiceMeshSignal';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import {selectPublisherCodecPreferences, Track} from 'livekit-client';

const logger = new Logger('VoiceMeshPeer');

const SETUP_DEADLINE_MS = 15_000;
const DISCONNECTED_GRACE_MS = 5_000;
const RESTART_DEADLINE_MS = 10_000;

type VoiceMeshNegotiationSignal = Exclude<VoiceMeshSignalData, {t: 'tracks'} | {t: 'want'}>;

function withMimeType(mimeType: string): (codec: RTCRtpCodec) => boolean {
	return (codec) => codec.mimeType.toLowerCase() === mimeType;
}

function codecPreferences(src: VoiceMeshSource): Array<RTCRtpCodec> {
	const kind = VOICE_MESH_TRACK_KINDS[src];
	const codecs = RTCRtpReceiver.getCapabilities(kind)?.codecs ?? [];
	if (kind === Track.Kind.Audio) {
		return [...codecs.filter(withMimeType('audio/red')), ...codecs.filter(withMimeType('audio/opus'))];
	}
	return [
		...selectPublisherCodecPreferences('h264', codecs).filter(withMimeType('video/h264')),
		...codecs.filter(withMimeType('video/vp8')),
		...codecs.filter(withMimeType('video/rtx')),
	];
}

export class VoiceMeshPeer {
	readonly participantSid: string;
	readonly identity: string;
	readonly isOfferer: boolean;
	status: VoiceMeshPeerStatus = 'connecting';
	remoteTracks: ReadonlyArray<VoiceMeshTrackInfo> = [];
	remoteWant: ReadonlySet<string> = new Set();
	localWant: {v: number; sids: Array<string>} | null = null;
	infoVersion = 0;
	setupMs: number | null = null;
	iceRestarted = false;
	private readonly createdAt = performance.now();
	private remoteTracksVersion = 0;
	private remoteWantVersion = 0;
	private pc: RTCPeerConnection | null = null;
	private transceivers: ReadonlyArray<RTCRtpTransceiver> = [];
	private generation = 0;
	private answered = false;
	private restarting = false;
	private closed = false;
	private queue: Promise<void> = Promise.resolve();
	private readonly pendingSenderSyncs = new Map<VoiceMeshSource, Promise<void>>();
	private setupTimer: ReturnType<typeof setTimeout> | null = null;
	private disconnectedTimer: ReturnType<typeof setTimeout> | null = null;
	private restartTimer: ReturnType<typeof setTimeout> | null = null;

	constructor(
		private readonly engine: VoiceMeshEngine,
		readonly connectionId: string,
		readonly userId: string,
		localConnectionId: string,
	) {
		this.participantSid = `PA_${connectionId}`;
		this.identity = buildVoiceParticipantIdentity(userId, connectionId);
		this.isOfferer = localConnectionId < connectionId;
	}

	get rtpTransceivers(): ReadonlyArray<RTCRtpTransceiver> {
		return this.transceivers;
	}

	rtpSender(src: VoiceMeshSource): RTCRtpSender | null {
		return this.transceivers[VOICE_MESH_SOURCES.indexOf(src)]?.sender ?? null;
	}

	rtpReceiver(src: VoiceMeshSource): RTCRtpReceiver | null {
		return this.transceivers[VOICE_MESH_SOURCES.indexOf(src)]?.receiver ?? null;
	}

	getStats(): Promise<RTCStatsReport> | null {
		return this.pc?.getStats() ?? null;
	}

	start(): void {
		this.armSetupDeadline();
		void this.enqueue(async () => {
			if (this.isOfferer) {
				if (!this.pc) await this.startGeneration();
				return;
			}
			if (this.generation === 0) this.send({t: 'need-offer', g: 0});
		});
	}

	resume(): void {
		if (this.closed || this.status === 'failed') return;
		if (this.status === 'connected') {
			if (this.restarting) void this.enqueue(() => this.recover());
			return;
		}
		if (this.isOfferer) {
			void this.enqueue(() => this.startGeneration());
			return;
		}
		this.armSetupDeadline();
		this.send({t: 'need-offer', g: 0});
	}

	handleSignal(data: VoiceMeshSignalData): void {
		if (this.closed || this.status === 'failed') return;
		switch (data.t) {
			case 'tracks':
				this.receiveTracks(data.v, data.tracks);
				return;
			case 'want':
				this.receiveWant(data.v, data.sids);
				return;
			default:
				void this.enqueue(() => this.negotiate(data));
		}
	}

	send(data: VoiceMeshSignalData): void {
		if (this.closed || this.status === 'failed') return;
		if (this.engine.sendPeerSignal(this, data) !== VoiceSignalSendResult.Oversized) return;
		logger.warn('Voice signal exceeds the gateway frame cap', {connectionId: this.connectionId, type: data.t});
		this.fail();
	}

	requestSenderSync(src: VoiceMeshSource): Promise<void> {
		const pending = this.pendingSenderSyncs.get(src);
		if (pending) return pending;
		const run = this.enqueue(async () => {
			this.pendingSenderSyncs.delete(src);
			await this.engine.syncPeerSender(this, src);
		});
		this.pendingSenderSyncs.set(src, run);
		return run;
	}

	close(): void {
		if (this.closed) return;
		this.closed = true;
		this.resetTransport();
		this.pendingSenderSyncs.clear();
	}

	private receiveTracks(v: number, tracks: ReadonlyArray<VoiceMeshTrackInfo>): void {
		if (v <= this.remoteTracksVersion) return;
		const sources = new Set<VoiceMeshSource>();
		for (const track of tracks) {
			if (sources.has(track.src) || track.sid !== voiceMeshTrackSid(this.connectionId, track.src)) return;
			sources.add(track.src);
		}
		this.remoteTracksVersion = v;
		this.remoteTracks = tracks;
		this.engine.handlePeerTracks(this);
	}

	private receiveWant(v: number, sids: ReadonlyArray<string>): void {
		if (v <= this.remoteWantVersion) return;
		this.remoteWantVersion = v;
		this.remoteWant = new Set(sids);
		this.engine.handlePeerWant();
	}

	private async negotiate(data: VoiceMeshNegotiationSignal): Promise<void> {
		if (this.status === 'failed') return;
		switch (data.t) {
			case 'offer':
				if (this.isOfferer || data.g < this.generation) return;
				if (data.g > this.generation) {
					await this.acceptGeneration(data.g, data.sdp);
					return;
				}
				await this.answerRestart(data.sdp);
				return;
			case 'answer': {
				const pc = this.pc;
				if (!this.isOfferer || !pc || data.g !== this.generation || pc.signalingState !== 'have-local-offer') return;
				await pc.setRemoteDescription({type: 'answer', sdp: data.sdp});
				if (this.isStale(pc, data.g)) return;
				this.answered = true;
				return;
			}
			case 'ice': {
				const pc = this.pc;
				if (!pc || data.g !== this.generation) return;
				try {
					await pc.addIceCandidate(data.c);
				} catch (error) {
					logger.debug('Dropping ICE candidate', {connectionId: this.connectionId, error});
				}
				return;
			}
			case 'need-offer':
				if (!this.isOfferer) return;
				if (data.restart) {
					if (data.g === this.generation) await this.recover();
					return;
				}
				if (data.g === this.generation && this.answered) return;
				await this.startGeneration();
		}
	}

	private async startGeneration(): Promise<void> {
		const g = this.generation === 0 ? Date.now() : this.generation + 1;
		this.beginGeneration(g);
		const pc = this.createTransport(g);
		this.transceivers = VOICE_MESH_SOURCES.map((src) => {
			const transceiver = pc.addTransceiver(VOICE_MESH_TRACK_KINDS[src], {direction: 'sendrecv'});
			transceiver.setCodecPreferences(codecPreferences(src));
			return transceiver;
		});
		this.engine.handlePeerTransport(this);
		await this.sendOffer(pc, g, false);
	}

	private async acceptGeneration(g: number, sdp: string): Promise<void> {
		this.beginGeneration(g);
		const pc = this.createTransport(g);
		await pc.setRemoteDescription({type: 'offer', sdp});
		if (this.isStale(pc, g)) return;
		const transceivers = pc.getTransceivers();
		const matchesLayout =
			transceivers.length === VOICE_MESH_SOURCES.length &&
			VOICE_MESH_SOURCES.every((src, index) => transceivers[index].receiver.track.kind === VOICE_MESH_TRACK_KINDS[src]);
		if (!matchesLayout) {
			logger.warn('Offer does not carry the mesh transceiver layout', {connectionId: this.connectionId});
			this.fail();
			return;
		}
		VOICE_MESH_SOURCES.forEach((src, index) => {
			transceivers[index].direction = 'sendrecv';
			transceivers[index].setCodecPreferences(codecPreferences(src));
		});
		this.transceivers = transceivers;
		this.engine.handlePeerTransport(this);
		await this.sendAnswer(pc, g);
	}

	private async answerRestart(sdp: string): Promise<void> {
		const pc = this.pc;
		if (!pc) return;
		const g = this.generation;
		this.iceRestarted = true;
		await pc.setRemoteDescription({type: 'offer', sdp});
		if (this.isStale(pc, g)) return;
		await this.sendAnswer(pc, g);
	}

	private async sendOffer(pc: RTCPeerConnection, g: number, iceRestart: boolean): Promise<void> {
		const offer = await pc.createOffer({iceRestart});
		if (this.isStale(pc, g)) return;
		await pc.setLocalDescription(offer);
		if (this.isStale(pc, g)) return;
		this.send({t: 'offer', g, sdp: offer.sdp ?? ''});
	}

	private async sendAnswer(pc: RTCPeerConnection, g: number): Promise<void> {
		const answer = await pc.createAnswer();
		if (this.isStale(pc, g)) return;
		await pc.setLocalDescription(answer);
		if (this.isStale(pc, g)) return;
		this.send({t: 'answer', g, sdp: answer.sdp ?? ''});
	}

	private async recover(): Promise<void> {
		const pc = this.pc;
		if (!pc || this.status === 'failed') return;
		if (this.restarting) {
			this.resendRestart(pc);
			return;
		}
		this.restarting = true;
		this.iceRestarted = true;
		this.restartTimer = setTimeout(() => {
			this.restartTimer = null;
			if (pc.connectionState !== 'connected') {
				this.fail();
				return;
			}
			this.restarting = false;
		}, RESTART_DEADLINE_MS);
		if (this.isOfferer) {
			await this.sendOffer(pc, this.generation, true);
			return;
		}
		this.send({t: 'need-offer', g: this.generation, restart: true});
	}

	private resendRestart(pc: RTCPeerConnection): void {
		if (!this.isOfferer) {
			this.send({t: 'need-offer', g: this.generation, restart: true});
			return;
		}
		const offer = pc.localDescription;
		if (pc.signalingState !== 'have-local-offer' || !offer) return;
		this.send({t: 'offer', g: this.generation, sdp: offer.sdp});
	}

	private beginGeneration(g: number): void {
		this.resetTransport();
		this.generation = g;
		this.answered = false;
		this.restarting = false;
		this.setStatus('connecting');
		this.armSetupDeadline();
	}

	private createTransport(g: number): RTCPeerConnection {
		const pc = new RTCPeerConnection({iceServers: this.engine.iceServers, bundlePolicy: 'max-bundle'});
		pc.onicecandidate = (event) => {
			if (this.pc !== pc) return;
			this.send({t: 'ice', g, c: event.candidate ? event.candidate.toJSON() : null});
		};
		pc.onconnectionstatechange = () => {
			if (this.pc !== pc) return;
			this.handleConnectionState(pc.connectionState);
		};
		this.pc = pc;
		return pc;
	}

	private handleConnectionState(state: RTCPeerConnectionState): void {
		switch (state) {
			case 'connected':
				this.clearTimers();
				this.restarting = false;
				this.setupMs ??= Math.round(performance.now() - this.createdAt);
				this.setStatus('connected');
				return;
			case 'disconnected':
				this.disconnectedTimer ??= setTimeout(() => {
					this.disconnectedTimer = null;
					void this.enqueue(() => this.recover());
				}, DISCONNECTED_GRACE_MS);
				return;
			case 'failed':
				if (this.restarting) {
					this.fail();
					return;
				}
				void this.enqueue(() => this.recover());
				return;
			default:
				return;
		}
	}

	private fail(): void {
		if (this.closed || this.status === 'failed') return;
		this.resetTransport();
		this.setStatus('failed');
		this.engine.handlePeerTransport(this);
	}

	private setStatus(status: VoiceMeshPeerStatus): void {
		if (this.status === status) return;
		this.status = status;
		this.engine.handlePeerStatus(this);
	}

	private armSetupDeadline(): void {
		if (this.setupTimer) clearTimeout(this.setupTimer);
		this.setupTimer = setTimeout(() => {
			this.setupTimer = null;
			this.fail();
		}, SETUP_DEADLINE_MS);
	}

	private clearTimers(): void {
		for (const timer of [this.setupTimer, this.disconnectedTimer, this.restartTimer]) {
			if (timer) clearTimeout(timer);
		}
		this.setupTimer = null;
		this.disconnectedTimer = null;
		this.restartTimer = null;
	}

	private resetTransport(): void {
		this.clearTimers();
		const pc = this.pc;
		this.pc = null;
		this.transceivers = [];
		if (!pc) return;
		pc.onicecandidate = null;
		pc.onconnectionstatechange = null;
		pc.close();
	}

	private isStale(pc: RTCPeerConnection, g: number): boolean {
		return this.closed || this.pc !== pc || this.generation !== g;
	}

	private enqueue(task: () => Promise<void> | void): Promise<void> {
		this.queue = this.queue
			.then(async () => {
				if (this.closed) return;
				await task();
			})
			.catch((error: unknown) => {
				if (this.closed) return;
				logger.warn('Mesh negotiation failed', {connectionId: this.connectionId, error});
				this.fail();
			});
		return this.queue;
	}
}
