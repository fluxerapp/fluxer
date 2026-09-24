// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {EventEmitter} from 'events';
import type {MediaAttributes, MediaDescription, SessionDescription} from 'sdp-transform';
import {parse, write} from 'sdp-transform';
import type TypedEmitter from 'typed-emitter';
import log, {getLogger, LoggerNames} from '../logger.ts';
import {debounce} from './debounce.ts';
import {NegotiationError, UnexpectedConnectionState} from './errors.ts';
import type {LoggerOptions} from './types.ts';
import {ddExtensionURI, isChromiumBased, isFireFox, isSafari, isSVCCodec} from './utils.ts';

export interface TrackBitrateInfo {
	cid?: string;
	transceiver?: RTCRtpTransceiver;
	codec: string;
	maxbr: number;
	isScreenShare?: boolean;
	stereo?: boolean;
}

const startBitrateMultiplier = 0.9;

const maxStartBitrateKbps = 1000;

const maxScreenShareStartBitrateKbps = 1500;

const minScreenShareStartBitrateKbps = 600;

const startBitrateParameter = 'x-google-start-bitrate';

const debounceInterval = 20;

const opusMaxAverageBitrateBps = 510000;

const opusPacketTimeMs = 10;

const requiredOpusFmtpParameters = {
	minptime: '10',
	useinbandfec: '1',
	usedtx: '0',
};

export function applyVideoStartBitrate(
	media: MediaDescription,
	cid: string,
	codec: string,
	maxbr: number,
	isScreenShare = false,
): number | undefined {
	if (!media.msid?.includes(cid)) {
		return undefined;
	}

	const codecPayloads = media.rtp
		.filter((rtp) => rtp.codec.toUpperCase() === codec.toUpperCase())
		.map((rtp) => rtp.payload);
	if (codecPayloads.length === 0 || codecPayloads[0] === 0) {
		return 0;
	}

	const calculatedStartBitrate = Math.round(maxbr * startBitrateMultiplier);
	let startBitrate = Math.min(calculatedStartBitrate, maxStartBitrateKbps);
	if (isScreenShare) {
		startBitrate = Math.max(
			minScreenShareStartBitrateKbps,
			Math.min(calculatedStartBitrate, maxScreenShareStartBitrateKbps),
		);
	}

	for (const payload of codecPayloads) {
		const fmtp = ensureFmtp(media, payload);
		fmtp.config = setFmtpParameter(fmtp.config, startBitrateParameter, String(startBitrate));
	}

	return codecPayloads[0];
}

export const PCEvents = {
	NegotiationStarted: 'negotiationStarted',
	NegotiationComplete: 'negotiationComplete',
	OfferAnswered: 'offerAnswered',
	RTPVideoPayloadTypes: 'rtpVideoPayloadTypes',
} as const;

export default class PCTransport extends (EventEmitter as new () => TypedEmitter<PCTransportEventCallbacks>) {
	private _pc: RTCPeerConnection | null;

	private get pc() {
		if (!this._pc) {
			this._pc = this.createPC();
		}
		return this._pc;
	}

	private config?: RTCConfiguration;

	private log = log;

	private iceLog = log;

	private loggerOptions: LoggerOptions;

	private ddExtID = 0;

	latestOfferId: number = 0;

	latestAcknowledgedOfferId: number = 0;

	private offerLock: Mutex;

	private pendingInitialOffer?: RTCSessionDescriptionInit;

	pendingCandidates: Array<RTCIceCandidateInit> = [];

	restartingIce: boolean = false;

	renegotiate: boolean = false;

	trackBitrates: Array<TrackBitrateInfo> = [];

	remoteStereoMids: Array<string> = [];

	remoteNackMids: Array<string> = [];

	excludedVideoDecoderMimeTypes: Set<string> = new Set();

	onOffer?: (offer: RTCSessionDescriptionInit, offerId: number) => void;

	onIceCandidate?: (candidate: RTCIceCandidate) => void;

	onIceCandidateError?: (ev: Event) => void;

	onConnectionStateChange?: (state: RTCPeerConnectionState) => void;

	onIceConnectionStateChange?: (state: RTCIceConnectionState) => void;

	onSignalingStatechange?: (state: RTCSignalingState) => void;

	onDataChannel?: (ev: RTCDataChannelEvent) => void;

	onTrack?: (ev: RTCTrackEvent) => void;

	constructor(config?: RTCConfiguration, loggerOptions: LoggerOptions = {}) {
		super();
		this.loggerOptions = loggerOptions;
		this.log = getLogger(loggerOptions.loggerName ?? LoggerNames.PCTransport, () => this.logContext);
		this.iceLog = getLogger(LoggerNames.ICE, () => this.logContext);
		this.config = config;
		this._pc = this.createPC();
		this.offerLock = new Mutex();
	}

	private createPC() {
		const pc = new RTCPeerConnection(this.config);

		pc.onicecandidate = (ev) => {
			if (!ev.candidate) return;
			this.iceLog.debug('local ICE candidate gathered', {candidate: ev.candidate.candidate});
			this.onIceCandidate?.(ev.candidate);
		};
		pc.onicecandidateerror = (ev) => {
			this.iceLog.debug('ICE candidate error', {event: ev});
			this.onIceCandidateError?.(ev);
		};

		pc.oniceconnectionstatechange = () => {
			this.iceLog.debug(`ICE connection state: ${pc.iceConnectionState}`);
			this.onIceConnectionStateChange?.(pc.iceConnectionState);
		};

		pc.onsignalingstatechange = () => {
			this.log.debug(`signaling state: ${pc.signalingState}`);
			this.onSignalingStatechange?.(pc.signalingState);
		};

		pc.onconnectionstatechange = () => {
			this.log.debug(`connection state: ${pc.connectionState}`);
			this.onConnectionStateChange?.(pc.connectionState);
		};
		pc.ondatachannel = (ev) => {
			this.log.debug('data channel opened by peer', {
				label: ev.channel.label,
				id: ev.channel.id,
			});
			this.onDataChannel?.(ev);
		};
		pc.ontrack = (ev) => {
			this.onTrack?.(ev);
		};
		return pc;
	}

	private get logContext() {
		return {
			...this.loggerOptions.loggerContextCb?.(),
		};
	}

	get isICEConnected(): boolean {
		return (
			this._pc !== null && (this.pc.iceConnectionState === 'connected' || this.pc.iceConnectionState === 'completed')
		);
	}

	async addIceCandidate(candidate: RTCIceCandidateInit): Promise<void> {
		if (this.pc.remoteDescription && !this.restartingIce) {
			return this.pc.addIceCandidate(candidate);
		}
		this.iceLog.debug('queuing remote ICE candidate until remote description applied', {
			pendingCount: this.pendingCandidates.length + 1,
		});
		this.pendingCandidates.push(candidate);
	}

	async setRemoteDescription(sd: RTCSessionDescriptionInit, offerId: number): Promise<boolean> {
		if (sd.type === 'answer' && this.latestOfferId > 0 && offerId > 0 && offerId !== this.latestOfferId) {
			this.log.warn('ignoring answer for old offer', {
				offerId,
				latestOfferId: this.latestOfferId,
			});
			return false;
		}
		let mungedSDP: string | undefined;
		if (sd.type === 'offer') {
			const {stereoMids, nackMids} = extractStereoAndNackAudioFromOffer(sd);
			this.remoteStereoMids = stereoMids;
			this.remoteNackMids = nackMids;
		} else if (sd.type === 'answer') {
			if (this.pendingInitialOffer && this._pc) {
				const initialOffer = this.pendingInitialOffer;
				this.pendingInitialOffer = undefined;
				const sdpParsed = parse(initialOffer.sdp ?? '');
				sdpParsed.media.forEach((media) => {
					ensureIPAddrMatchVersion(media);
				});
				this.log.debug('setting pending initial offer before processing answer');
				await this.setMungedSDP(initialOffer, write(sdpParsed));
			}
			const sdpParsed = parse(sd.sdp ?? '');
			sdpParsed.media.forEach((media) => {
				const mid = getMidString(media.mid!);
				if (media.type === 'audio') {
					const trackbr = this.trackBitrates.find(
						(br) => br.transceiver !== undefined && mid === br.transceiver.mid && br.codec.toLowerCase() === 'opus',
					);
					if (trackbr && getCodecPayload(media, trackbr.codec) !== 0) {
						ensureOpusFmtp(
							media,
							trackbr.maxbr > 0 ? trackbr.maxbr * 1000 : opusMaxAverageBitrateBps,
							trackbr.stereo === true,
						);
					} else {
						ensureOpusFmtp(media);
					}
				}
			});
			const placeholderMids = this.getPlaceholderMids();
			if (placeholderMids.size > 0) {
				conformBundledCodecFmtp(sdpParsed.media, (media) => placeholderMids.has(getMidString(media.mid!)));
			}
			mungedSDP = write(sdpParsed);
		}
		await this.setMungedSDP(sd, mungedSDP, true);

		if (this.pendingCandidates.length > 0) {
			this.iceLog.debug('flushing queued ICE candidates', {
				count: this.pendingCandidates.length,
			});
		}
		this.pendingCandidates.forEach((candidate) => {
			this.pc.addIceCandidate(candidate);
		});
		this.pendingCandidates = [];
		this.restartingIce = false;

		if (sd.type === 'answer') {
			this.latestAcknowledgedOfferId = offerId;
			this.emit(PCEvents.OfferAnswered, offerId);
		}

		if (this.renegotiate) {
			this.renegotiate = false;
			await this.createAndSendOffer();
		} else if (sd.type === 'answer') {
			this.emit(PCEvents.NegotiationComplete);
			if (sd.sdp) {
				const sdpParsed = parse(sd.sdp);
				sdpParsed.media.forEach((media) => {
					if (media.type === 'video') {
						this.emit(PCEvents.RTPVideoPayloadTypes, media.rtp);
					}
				});
			}
		}
		return true;
	}

	negotiate = debounce(async (onError?: (e: Error) => void) => {
		this.emit(PCEvents.NegotiationStarted);
		try {
			await this.createAndSendOffer();
		} catch (e) {
			if (onError) {
				onError(e as Error);
			} else {
				throw e;
			}
		}
	}, debounceInterval);

	async createInitialOffer() {
		const unlock = await this.offerLock.lock();
		try {
			if (this.pc.signalingState !== 'stable') {
				this.log.warn('signaling state is not stable, cannot create initial offer');
				return;
			}
			const offerId = this.latestOfferId + 1;
			this.latestOfferId = offerId;
			this.applyVideoDecoderCodecExclusions();
			const offer = await this.pc.createOffer();
			this.pendingInitialOffer = {sdp: offer.sdp, type: offer.type};
			const sdpParsed = parse(offer.sdp ?? '');
			sdpParsed.media.forEach((media) => {
				ensureIPAddrMatchVersion(media);
			});
			offer.sdp = write(sdpParsed);
			return {offer, offerId};
		} finally {
			unlock();
		}
	}

	async createAndSendOffer(options?: RTCOfferOptions) {
		const unlock = await this.offerLock.lock();

		try {
			if (this.onOffer === undefined) {
				return;
			}

			if (options?.iceRestart) {
				this.iceLog.debug('restarting ICE');
				this.restartingIce = true;
			}

			if (this._pc && (this._pc.signalingState === 'have-local-offer' || this.pendingInitialOffer)) {
				const currentSD = this._pc.remoteDescription;
				if (options?.iceRestart && currentSD) {
					await this._pc.setRemoteDescription(currentSD);
				} else if (options?.iceRestart) {
					throw new NegotiationError(
						'ICE restart requested without a remote description, peer connection must be recreated',
					);
				} else {
					this.renegotiate = true;
					this.log.debug('requesting renegotiation');
					return;
				}
			} else if (!this._pc || this._pc.signalingState === 'closed') {
				this.log.warn('could not createOffer with closed peer connection');
				return;
			}

			this.log.debug('starting to negotiate');
			const offerId = this.latestOfferId + 1;
			this.latestOfferId = offerId;
			this.applyVideoDecoderCodecExclusions();
			const offer = await this.pc.createOffer(options);
			this.log.debug('original offer', {sdp: offer.sdp});

			const sdpParsed = parse(offer.sdp ?? '');
			const stereoMids = collectStereoMids(this.trackBitrates, sdpParsed.media);
			sdpParsed.media.forEach((media) => {
				ensureIPAddrMatchVersion(media);
				if (media.type === 'audio') {
					ensureAudioNackAndStereo(media, stereoMids, []);
				} else if (media.type === 'video') {
					if (isChromiumBased() && videoSectionCanReceiveAV1(media)) {
						this.ddExtID = ensureVideoDDExtension(media, sdpParsed, this.ddExtID);
					}
					this.trackBitrates.some((trackbr): boolean => {
						if (!trackbr.cid) {
							return false;
						}

						const codecPayload = applyVideoStartBitrate(
							media,
							trackbr.cid,
							trackbr.codec,
							trackbr.maxbr,
							trackbr.isScreenShare,
						);
						if (codecPayload === undefined) {
							return false;
						}

						if (codecPayload > 0 && isSVCCodec(trackbr.codec) && !isSafari()) {
							this.ddExtID = ensureVideoDDExtension(media, sdpParsed, this.ddExtID);
						}

						return true;
					});
				}
			});
			const placeholderMids = this.getPlaceholderMids();
			if (placeholderMids.size > 0) {
				conformBundledCodecFmtp(sdpParsed.media, (media) => placeholderMids.has(getMidString(media.mid!)));
			}
			if (this.latestOfferId > offerId) {
				this.log.warn('latestOfferId mismatch', {
					latestOfferId: this.latestOfferId,
					offerId,
				});
				return;
			}
			await this.setMungedSDP(offer, write(sdpParsed));
			this.onOffer(offer, this.latestOfferId);
		} finally {
			unlock();
		}
	}

	async createAndSetAnswer(): Promise<RTCSessionDescriptionInit> {
		this.applyVideoDecoderCodecExclusions();
		const answer = await this.pc.createAnswer();
		const sdpParsed = parse(answer.sdp ?? '');
		sdpParsed.media.forEach((media) => {
			ensureIPAddrMatchVersion(media);
			if (media.type === 'audio') {
				ensureAudioNackAndStereo(media, this.remoteStereoMids, this.remoteNackMids);
			}
		});
		await this.setMungedSDP(answer, write(sdpParsed));
		return answer;
	}

	private applyVideoDecoderCodecExclusions() {
		if (this.excludedVideoDecoderMimeTypes.size === 0) return;
		if (isFireFox() || isSafari()) return;
		if (typeof RTCRtpReceiver === 'undefined' || typeof RTCRtpReceiver.getCapabilities !== 'function') return;
		const receiverCaps = RTCRtpReceiver.getCapabilities?.('video');
		if (!receiverCaps) return;
		const allowed = receiverCaps.codecs.filter(
			(c) => !this.excludedVideoDecoderMimeTypes.has(c.mimeType.toLowerCase()),
		);
		if (allowed.length === receiverCaps.codecs.length) return;
		for (const transceiver of this.getTransceivers()) {
			if (transceiver.receiver.track?.kind !== 'video') continue;
			if ((transceiver as {stopped?: boolean}).stopped) continue;
			if (transceiver.direction !== 'recvonly') continue;
			if (typeof transceiver.setCodecPreferences !== 'function') continue;
			try {
				transceiver.setCodecPreferences(allowed);
			} catch (e) {
				this.log.warn('failed to set subscriber codec preferences', {error: e});
			}
		}
	}

	private getPlaceholderMids(): Set<string> {
		return placeholderMidsFromTransceivers(this._pc?.getTransceivers() ?? []);
	}

	createDataChannel(label: string, dataChannelDict: RTCDataChannelInit) {
		return this.pc.createDataChannel(label, dataChannelDict);
	}

	addTransceiver(mediaStreamTrack: MediaStreamTrack, transceiverInit: RTCRtpTransceiverInit) {
		return this.pc.addTransceiver(mediaStreamTrack, transceiverInit);
	}

	addTransceiverOfKind(kind: 'audio' | 'video', transceiverInit: RTCRtpTransceiverInit) {
		return this.pc.addTransceiver(kind, transceiverInit);
	}

	addTrack(track: MediaStreamTrack) {
		if (!this._pc) {
			throw new UnexpectedConnectionState('PC closed, cannot add track');
		}
		return this._pc.addTrack(track);
	}

	setTrackCodecBitrate(info: TrackBitrateInfo) {
		const existing = this.trackBitrates.findIndex(
			(trackbr) =>
				(info.cid !== undefined && trackbr.cid === info.cid) ||
				(info.transceiver !== undefined && trackbr.transceiver === info.transceiver),
		);
		if (existing === -1) {
			this.trackBitrates.push(info);
			return;
		}
		this.trackBitrates[existing] = info;
	}

	setConfiguration(rtcConfig: RTCConfiguration) {
		if (!this._pc) {
			throw new UnexpectedConnectionState('PC closed, cannot configure');
		}
		return this._pc?.setConfiguration(rtcConfig);
	}

	canRemoveTrack(): boolean {
		return !!this._pc?.removeTrack;
	}

	removeTrack(sender: RTCRtpSender) {
		return this._pc?.removeTrack(sender);
	}

	getConnectionState() {
		return this._pc?.connectionState ?? 'closed';
	}

	getICEConnectionState() {
		return this._pc?.iceConnectionState ?? 'closed';
	}

	getSignallingState() {
		return this._pc?.signalingState ?? 'closed';
	}

	getTransceivers() {
		return this._pc?.getTransceivers() ?? [];
	}

	getSenders() {
		return this._pc?.getSenders() ?? [];
	}

	getLocalDescription() {
		return this._pc?.localDescription;
	}

	getRemoteDescription() {
		return this.pc?.remoteDescription;
	}

	getStats() {
		return this._pc?.getStats();
	}

	getMaxMessageSize() {
		return this._pc?.sctp?.maxMessageSize;
	}

	async getConnectedAddress(): Promise<string | undefined> {
		if (!this._pc) {
			return;
		}
		let selectedCandidatePairId = '';
		const candidatePairs = new Map<string, RTCIceCandidatePairStats>();
		const candidates = new Map<string, string>();
		const stats: RTCStatsReport = await this._pc.getStats();
		stats.forEach((v) => {
			switch (v.type) {
				case 'transport':
					selectedCandidatePairId = v.selectedCandidatePairId;
					break;
				case 'candidate-pair':
					if (selectedCandidatePairId === '' && v.selected) {
						selectedCandidatePairId = v.id;
					}
					candidatePairs.set(v.id, v);
					break;
				case 'remote-candidate':
					candidates.set(v.id, `${v.address}:${v.port}`);
					break;
				default:
			}
		});

		if (selectedCandidatePairId === '') {
			return undefined;
		}
		const selectedID = candidatePairs.get(selectedCandidatePairId)?.remoteCandidateId;
		if (selectedID === undefined) {
			return undefined;
		}
		return candidates.get(selectedID);
	}

	close = () => {
		if (!this._pc) {
			return;
		}
		this.log.debug('closing peer connection');
		this.pendingInitialOffer = undefined;
		this._pc.close();
		this._pc.onconnectionstatechange = null;
		this._pc.oniceconnectionstatechange = null;
		this._pc.onicegatheringstatechange = null;
		this._pc.ondatachannel = null;
		this._pc.onnegotiationneeded = null;
		this._pc.onsignalingstatechange = null;
		this._pc.onicecandidate = null;
		this._pc.ondatachannel = null;
		this._pc.ontrack = null;
		this._pc.onconnectionstatechange = null;
		this._pc.oniceconnectionstatechange = null;
		this._pc = null;
	};

	private async setMungedSDP(sd: RTCSessionDescriptionInit, munged?: string, remote?: boolean) {
		const originalSdp = sd.sdp;
		if (munged) {
			sd.sdp = munged;
			try {
				this.log.debug(`setting munged ${remote ? 'remote' : 'local'} description`);
				if (remote) {
					await this.pc.setRemoteDescription(sd);
				} else {
					await this.pc.setLocalDescription(sd);
				}
				return;
			} catch (e) {
				this.log.warn(`not able to set ${sd.type}, falling back to unmodified sdp`, {
					error: e,
					mungedSdp: munged,
					originalSdp,
				});
				sd.sdp = originalSdp;
			}
		}

		try {
			if (remote) {
				await this._pc?.setRemoteDescription(sd);
			} else {
				await this._pc?.setLocalDescription(sd);
			}
		} catch (e) {
			let msg = 'unknown error';
			if (e instanceof Error) {
				msg = e.message;
			} else if (typeof e === 'string') {
				msg = e;
			}

			const fields: {
				error: string;
				sdp?: string;
				mungedSdp?: string;
				remoteSdp?: RTCSessionDescription | null;
			} = {
				error: msg,
				sdp: sd.sdp,
			};
			if (munged && munged !== originalSdp) {
				fields.mungedSdp = munged;
			}
			if (!remote && this.pc.remoteDescription) {
				fields.remoteSdp = this.pc.remoteDescription;
			}
			this.log.error(`unable to set ${sd.type}`, {fields});
			throw new NegotiationError(msg);
		}
	}
}

export function ensureVideoDDExtension(
	media: {
		type: string;
		port: number;
		protocol: string;
		payloads?: string | undefined;
	} & MediaDescription,
	sdp: SessionDescription,
	ddExtID: number,
): number {
	const id = ddExtensionIDFor(sdp, ddExtID);
	if (id === undefined) {
		return ddExtID;
	}

	if (!media.ext?.some((ext) => ext.uri === ddExtensionURI)) {
		media.ext ??= [];
		media.ext.push({
			value: id,
			uri: ddExtensionURI,
		});
	}
	return id;
}

export function videoSectionCanReceiveAV1(media: MediaDescription): boolean {
	if (media.direction !== 'recvonly' && media.direction !== 'sendrecv') return false;
	return media.rtp.some((rtp) => rtp.codec.toLowerCase() === 'av1');
}

function ddExtensionIDFor(sdp: SessionDescription, cachedID: number): number | undefined {
	const mapped = mappedExtensionID(sdp, ddExtensionURI);
	if (mapped !== undefined) {
		return usedForOtherURI(sdp, mapped, ddExtensionURI) ? undefined : mapped;
	}
	if (cachedID !== 0 && !usedForOtherURI(sdp, cachedID, ddExtensionURI)) {
		return cachedID;
	}
	return unusedExtensionID(sdp);
}

function mappedExtensionID(sdp: SessionDescription, uri: string): number | undefined {
	for (const media of sdp.media) {
		const ext = media.ext?.find((candidate) => candidate.uri === uri);
		if (ext) {
			return ext.value;
		}
	}
	return undefined;
}

function usedForOtherURI(sdp: SessionDescription, id: number, uri: string): boolean {
	return sdp.media.some((media) => media.ext?.some((ext) => ext.value === id && ext.uri !== uri));
}

function unusedExtensionID(sdp: SessionDescription): number {
	let maxID = 0;
	sdp.media.forEach((media) => {
		media.ext?.forEach((ext) => {
			if (ext.value > maxID) {
				maxID = ext.value;
			}
		});
	});
	return maxID + 1 === 15 ? 16 : maxID + 1;
}

export function fmtpConfigHasParam(config: string, param: string): boolean {
	return config.split(';').some((entry) => entry.trim() === param);
}

function getCodecPayload(media: MediaDescription, codec: string): number {
	let payload = 0;
	media.rtp.some((rtp): boolean => {
		if (rtp.codec.toLowerCase() === codec.toLowerCase()) {
			payload = rtp.payload;
			return true;
		}
		return false;
	});
	return payload;
}

function ensureFmtp(media: MediaDescription, payload: number): {payload: number; config: string} {
	if (!media.fmtp) {
		media.fmtp = [];
	}
	let found = media.fmtp.find((fmtp) => fmtp.payload === payload);
	if (!found) {
		found = {payload, config: ''};
		media.fmtp.push(found);
	}
	return found;
}

function setFmtpParameter(config: string, key: string, value: string): string {
	const prefix = `${key}=`;
	const parts = config
		.split(';')
		.map((part) => part.trim())
		.filter((part) => part.length > 0 && !part.startsWith(prefix) && part !== key);
	parts.push(`${key}=${value}`);
	return parts.join(';');
}

function ensureAudioRedFmtp(media: MediaDescription, opusPayload: number): void {
	const redPayload = getCodecPayload(media, 'red');
	if (redPayload <= 0 || opusPayload <= 0) return;
	const fmtp = ensureFmtp(media, redPayload);
	if (fmtp.config.trim().length === 0) {
		fmtp.config = `${opusPayload}/${opusPayload}`;
	}
}

export function ensureOpusFmtp(
	media: MediaDescription,
	maxAverageBitrateBps: number = opusMaxAverageBitrateBps,
	stereo = false,
): number {
	const opusPayload = getCodecPayload(media, 'opus');
	if (opusPayload <= 0) return 0;
	media.ptime = opusPacketTimeMs;
	const fmtp = ensureFmtp(media, opusPayload);
	let config = fmtp.config;
	for (const [key, value] of Object.entries(requiredOpusFmtpParameters)) {
		config = setFmtpParameter(config, key, value);
	}
	if (stereo) {
		config = setFmtpParameter(config, 'stereo', '1');
		config = setFmtpParameter(config, 'sprop-stereo', '1');
	}
	if (maxAverageBitrateBps > 0) {
		config = setFmtpParameter(config, 'maxaveragebitrate', String(maxAverageBitrateBps));
	}
	fmtp.config = config;
	ensureAudioRedFmtp(media, opusPayload);
	return opusPayload;
}

export function ensureAudioNackAndStereo(
	media: {
		type: string;
		port: number;
		protocol: string;
		payloads?: string | undefined;
	} & MediaDescription,
	stereoMids: Array<string>,
	nackMids: Array<string>,
) {
	const mid = getMidString(media.mid!);
	const opusPayload = ensureOpusFmtp(media, opusMaxAverageBitrateBps, stereoMids.includes(mid));
	if (opusPayload > 0) {
		if (!media.rtcpFb) {
			media.rtcpFb = [];
		}

		if (nackMids.includes(mid) && !media.rtcpFb.some((fb) => fb.payload === opusPayload && fb.type === 'nack')) {
			media.rtcpFb.push({
				payload: opusPayload,
				type: 'nack',
			});
		}
	}
}

export function collectStereoMids(
	trackBitrates: Array<TrackBitrateInfo>,
	media: Array<MediaDescription>,
): Array<string> {
	const stereoMids: Array<string> = [];
	for (const trackbr of trackBitrates) {
		if (trackbr.stereo !== true || !trackbr.transceiver) {
			continue;
		}
		if (trackbr.transceiver.mid) {
			stereoMids.push(getMidString(trackbr.transceiver.mid));
			continue;
		}
		const trackId = trackbr.transceiver.sender.track?.id;
		if (trackId === undefined) {
			continue;
		}
		for (const m of media) {
			if (m.type === 'audio' && m.mid !== undefined && m.msid?.includes(trackId)) {
				stereoMids.push(getMidString(m.mid));
			}
		}
	}
	return stereoMids;
}

export function placeholderMidsFromTransceivers(transceivers: ReadonlyArray<RTCRtpTransceiver>): Set<string> {
	const mids = new Set<string>();
	for (const transceiver of transceivers) {
		if (transceiver.currentDirection === 'stopped') {
			continue;
		}
		if (transceiver.mid && !transceiver.sender.track) {
			mids.add(transceiver.mid);
		}
	}
	return mids;
}

export function conformBundledCodecFmtp(
	media: Array<MediaDescription>,
	isPlaceholder: (media: MediaDescription) => boolean,
) {
	const canonicalByPayload = new Map<number, string>();
	const fromRealSection = new Set<number>();
	for (const m of media) {
		const placeholder = isPlaceholder(m);
		for (const fmtp of m.fmtp ?? []) {
			if (!placeholder) {
				canonicalByPayload.set(fmtp.payload, fmtp.config);
				fromRealSection.add(fmtp.payload);
			} else if (!canonicalByPayload.has(fmtp.payload)) {
				canonicalByPayload.set(fmtp.payload, fmtp.config);
			}
		}
	}
	if (canonicalByPayload.size === 0) {
		return;
	}

	for (const m of media) {
		if (!isPlaceholder(m)) {
			continue;
		}
		for (const fmtp of m.fmtp ?? []) {
			const config = canonicalByPayload.get(fmtp.payload);
			if (config !== undefined && fmtp.config !== config) {
				fmtp.config = config;
			}
		}
	}
}

export function extractStereoAndNackAudioFromOffer(offer: RTCSessionDescriptionInit): {
	stereoMids: Array<string>;
	nackMids: Array<string>;
} {
	const stereoMids: Array<string> = [];
	const nackMids: Array<string> = [];
	const sdpParsed = parse(offer.sdp ?? '');
	let opusPayload = 0;
	sdpParsed.media.forEach((media) => {
		const mid = getMidString(media.mid!);
		if (media.type === 'audio') {
			media.rtp.some((rtp): boolean => {
				if (rtp.codec.toLowerCase() === 'opus') {
					opusPayload = rtp.payload;
					return true;
				}
				return false;
			});

			if (media.rtcpFb?.some((fb) => fb.payload === opusPayload && fb.type === 'nack')) {
				nackMids.push(mid);
			}

			media.fmtp.some((fmtp): boolean => {
				if (fmtp.payload === opusPayload) {
					if (fmtpConfigHasParam(fmtp.config, 'sprop-stereo=1')) {
						stereoMids.push(mid);
					}
					return true;
				}
				return false;
			});
		}
	});
	return {stereoMids, nackMids};
}

function ensureIPAddrMatchVersion(media: MediaDescription) {
	if (media.connection) {
		const isV6 = media.connection.ip.indexOf(':') >= 0;
		if ((media.connection.version === 4 && isV6) || (media.connection.version === 6 && !isV6)) {
			media.connection.ip = '0.0.0.0';
			media.connection.version = 4;
		}
	}
}

function getMidString(mid: string | number) {
	return typeof mid === 'number' ? mid.toFixed(0) : mid;
}

type PCTransportEventCallbacks = {
	negotiationStarted: () => void;
	negotiationComplete: () => void;
	offerAnswered: (offerId: number) => void;
	rtpVideoPayloadTypes: (attributes: MediaAttributes['rtp']) => void;
};
