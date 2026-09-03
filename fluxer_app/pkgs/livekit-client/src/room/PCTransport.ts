// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {EventEmitter} from 'events';
import type {MediaDescription, SessionDescription} from 'sdp-transform';
import {parse, write} from 'sdp-transform';
import {debounce} from 'ts-debounce';
import log, {getLogger, LoggerNames} from '../logger.ts';
import {NegotiationError, UnexpectedConnectionState} from './errors.ts';
import type {LoggerOptions} from './types.ts';
import {ddExtensionURI, isFireFox, isSafari, isSVCCodec} from './utils.ts';

interface TrackBitrateInfo {
	cid?: string;
	transceiver?: RTCRtpTransceiver;
	codec: string;
	maxbr: number;
}

const startBitrateFraction = 0.7;
const opusMaxAverageBitrateBps = 510000;
const opusPacketTimeMs = 10;
const requiredOpusFmtpParameters = {
	minptime: '10',
	useinbandfec: '1',
	usedtx: '0',
	stereo: '1',
	'sprop-stereo': '1',
};
const debounceInterval = 20;
export const PCEvents = {
	NegotiationStarted: 'negotiationStarted',
	NegotiationComplete: 'negotiationComplete',
	RTPVideoPayloadTypes: 'rtpVideoPayloadTypes',
} as const;

export function appendStartBitrateToFmtp(media: MediaDescription, codecPayload: number, startBitrate: number): void {
	if (startBitrate <= 0) {
		return;
	}
	for (const fmtp of media.fmtp) {
		if (fmtp.payload !== codecPayload) {
			continue;
		}
		if (!fmtp.config.includes('x-google-start-bitrate')) {
			fmtp.config += `;x-google-start-bitrate=${startBitrate}`;
		}
		return;
	}
}

export default class PCTransport extends EventEmitter {
	private _pc: RTCPeerConnection | null;

	private get pc() {
		if (!this._pc) {
			this._pc = this.createPC();
		}
		return this._pc;
	}

	private config?: RTCConfiguration;
	private log = log;
	private loggerOptions: LoggerOptions;
	private ddExtID = 0;
	private latestOfferId: number = 0;
	private offerLock: Mutex;
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
		this.log = getLogger(loggerOptions.loggerName ?? LoggerNames.PCTransport);
		this.loggerOptions = loggerOptions;
		this.config = config;
		this._pc = this.createPC();
		this.offerLock = new Mutex();
	}

	private createPC() {
		const pc = new RTCPeerConnection(this.config);
		pc.onicecandidate = (ev) => {
			if (!ev.candidate) return;
			this.onIceCandidate?.(ev.candidate);
		};
		pc.onicecandidateerror = (ev) => {
			this.onIceCandidateError?.(ev);
		};
		pc.oniceconnectionstatechange = () => {
			this.onIceConnectionStateChange?.(pc.iceConnectionState);
		};
		pc.onsignalingstatechange = () => {
			this.onSignalingStatechange?.(pc.signalingState);
		};
		pc.onconnectionstatechange = () => {
			this.onConnectionStateChange?.(pc.connectionState);
		};
		pc.ondatachannel = (ev) => {
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
		this.pendingCandidates.push(candidate);
	}

	async setRemoteDescription(sd: RTCSessionDescriptionInit, offerId: number): Promise<boolean> {
		if (sd.type === 'answer' && this.latestOfferId > 0 && offerId > 0 && offerId !== this.latestOfferId) {
			this.log.warn('ignoring answer for old offer', {
				...this.logContext,
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
			const sdpParsed = parse(sd.sdp ?? '');
			sdpParsed.media.forEach((media) => {
				const mid = getMidString(media.mid!);
				if (media.type === 'audio') {
					ensureOpusFmtp(media);
					this.trackBitrates.some((trackbr): boolean => {
						if (!trackbr.transceiver || mid !== trackbr.transceiver.mid) {
							return false;
						}
						const codecPayload = getCodecPayload(media, trackbr.codec);
						if (codecPayload === 0) {
							return true;
						}
						if (trackbr.codec.toLowerCase() === 'opus') {
							ensureOpusFmtp(media, trackbr.maxbr > 0 ? trackbr.maxbr * 1000 : opusMaxAverageBitrateBps);
						}
						return true;
					});
				}
			});
			mungedSDP = write(sdpParsed);
		}
		await this.setMungedSDP(sd, mungedSDP, true);
		this.pendingCandidates.forEach((candidate) => {
			this.pc.addIceCandidate(candidate);
		});
		this.pendingCandidates = [];
		this.restartingIce = false;
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

	async createAndSendOffer(options?: RTCOfferOptions) {
		const unlock = await this.offerLock.lock();
		try {
			if (this.onOffer === undefined) {
				return;
			}
			if (options?.iceRestart) {
				this.log.debug('restarting ICE', this.logContext);
				this.restartingIce = true;
			}
			if (this._pc && this._pc.signalingState === 'have-local-offer') {
				const currentSD = this._pc.remoteDescription;
				if (options?.iceRestart && currentSD) {
					await this._pc.setRemoteDescription(currentSD);
				} else {
					this.renegotiate = true;
					this.log.debug('requesting renegotiation', {...this.logContext});
					return;
				}
			} else if (!this._pc || this._pc.signalingState === 'closed') {
				this.log.warn('could not createOffer with closed peer connection', this.logContext);
				return;
			}
			this.log.debug('starting to negotiate', this.logContext);
			const offerId = this.latestOfferId + 1;
			this.latestOfferId = offerId;
			this.applyVideoDecoderCodecExclusions();
			const offer = await this.pc.createOffer(options);
			this.log.debug('original offer', {sdp: offer.sdp, ...this.logContext});
			const sdpParsed = parse(offer.sdp ?? '');
			sdpParsed.media.forEach((media) => {
				ensureIPAddrMatchVersion(media);
				if (media.type === 'audio') {
					ensureAudioNackAndStereo(media, ['all'], []);
				} else if (media.type === 'video') {
					this.trackBitrates.some((trackbr): boolean => {
						if (!media.msid || !trackbr.cid || !media.msid.includes(trackbr.cid)) {
							return false;
						}
						let codecPayload = 0;
						media.rtp.some((rtp): boolean => {
							if (rtp.codec.toUpperCase() === trackbr.codec.toUpperCase()) {
								codecPayload = rtp.payload;
								return true;
							}
							return false;
						});
						if (codecPayload === 0) {
							return true;
						}
						if (isSVCCodec(trackbr.codec) && !isSafari()) {
							this.ensureVideoDDExtensionForSVC(media, sdpParsed);
						}
						if (trackbr.maxbr <= 0) {
							return true;
						}
						appendStartBitrateToFmtp(media, codecPayload, Math.round(trackbr.maxbr * startBitrateFraction));
						return true;
					});
				}
			});
			if (this.latestOfferId > offerId) {
				this.log.warn('latestOfferId mismatch', {
					...this.logContext,
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
			if (transceiver.direction !== 'recvonly' && transceiver.direction !== 'sendrecv') continue;
			if (typeof transceiver.setCodecPreferences !== 'function') continue;
			try {
				transceiver.setCodecPreferences(allowed);
			} catch (e) {
				this.log.warn('failed to set subscriber codec preferences', {...this.logContext, error: e});
			}
		}
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
		return this.pc.getStats();
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
		if (munged) {
			const originalSdp = sd.sdp;
			sd.sdp = munged;
			try {
				this.log.debug(`setting munged ${remote ? 'remote' : 'local'} description`, this.logContext);
				if (remote) {
					await this.pc.setRemoteDescription(sd);
				} else {
					await this.pc.setLocalDescription(sd);
				}
				return;
			} catch (e) {
				this.log.warn(`not able to set ${sd.type}, falling back to unmodified sdp`, {
					...this.logContext,
					error: e,
					sdp: munged,
				});
				sd.sdp = originalSdp;
			}
		}
		try {
			if (remote) {
				await this.pc.setRemoteDescription(sd);
			} else {
				await this.pc.setLocalDescription(sd);
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
				remoteSdp?: RTCSessionDescription | null;
			} = {
				error: msg,
				sdp: sd.sdp,
			};
			if (!remote && this.pc.remoteDescription) {
				fields.remoteSdp = this.pc.remoteDescription;
			}
			this.log.error(`unable to set ${sd.type}`, {...this.logContext, fields});
			throw new NegotiationError(msg);
		}
	}

	private ensureVideoDDExtensionForSVC(
		media: {
			type: string;
			port: number;
			protocol: string;
			payloads?: string | undefined;
		} & MediaDescription,
		sdp: SessionDescription,
	) {
		const ddFound = media.ext?.some((ext): boolean => {
			if (ext.uri === ddExtensionURI) {
				return true;
			}
			return false;
		});
		if (!ddFound) {
			if (this.ddExtID === 0) {
				let maxID = 0;
				sdp.media.forEach((m) => {
					if (m.type !== 'video') {
						return;
					}
					m.ext?.forEach((ext) => {
						if (ext.value > maxID) {
							maxID = ext.value;
						}
					});
				});
				this.ddExtID = maxID + 1;
			}
			media.ext?.push({
				value: this.ddExtID,
				uri: ddExtensionURI,
			});
		}
	}
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

function ensureOpusFmtp(media: MediaDescription, maxAverageBitrateBps: number = opusMaxAverageBitrateBps): number {
	const opusPayload = getCodecPayload(media, 'opus');
	if (opusPayload <= 0) return 0;
	media.ptime = opusPacketTimeMs;
	const fmtp = ensureFmtp(media, opusPayload);
	let config = fmtp.config;
	for (const [key, value] of Object.entries(requiredOpusFmtpParameters)) {
		config = setFmtpParameter(config, key, value);
	}
	if (maxAverageBitrateBps > 0) {
		config = setFmtpParameter(config, 'maxaveragebitrate', String(maxAverageBitrateBps));
	}
	fmtp.config = config;
	ensureAudioRedFmtp(media, opusPayload);
	return opusPayload;
}

function ensureAudioNackAndStereo(
	media: {
		type: string;
		port: number;
		protocol: string;
		payloads?: string | undefined;
	} & MediaDescription,
	_stereoMids: Array<string>,
	nackMids: Array<string>,
) {
	const mid = getMidString(media.mid!);
	const opusPayload = ensureOpusFmtp(media);
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

function extractStereoAndNackAudioFromOffer(offer: RTCSessionDescriptionInit): {
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
				if (rtp.codec === 'opus') {
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
					if (fmtp.config.includes('sprop-stereo=1')) {
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
