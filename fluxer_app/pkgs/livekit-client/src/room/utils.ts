// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {
	type ChatMessage as ChatMessageModel,
	ClientInfo,
	ClientInfo_SDK,
	DisconnectReason,
	type Transcription as TranscriptionModel,
} from '@livekit/protocol';
import type {BrowserDetails} from '../utils/browserParser.ts';
import {getBrowser} from '../utils/browserParser.ts';
import TypedPromise from '../utils/TypedPromise.ts';
import {protocolVersion, version} from '../version.ts';
import {type ConnectionError, ConnectionErrorReason} from './errors.ts';
import type LocalParticipant from './participant/LocalParticipant.ts';
import type Participant from './participant/Participant.ts';
import type RemoteParticipant from './participant/RemoteParticipant.ts';
import CriticalTimers from './timers.ts';
import type LocalAudioTrack from './track/LocalAudioTrack.ts';
import type LocalTrack from './track/LocalTrack.ts';
import type LocalVideoTrack from './track/LocalVideoTrack.ts';
import {type AudioCodec, audioCodecs, type VideoCodec, videoCodecs} from './track/options.ts';
import type RemoteAudioTrack from './track/RemoteAudioTrack.ts';
import type RemoteTrack from './track/RemoteTrack.ts';
import type RemoteTrackPublication from './track/RemoteTrackPublication.ts';
import type RemoteVideoTrack from './track/RemoteVideoTrack.ts';
import {Track} from './track/Track.ts';
import type {TrackPublication} from './track/TrackPublication.ts';
import {getNewAudioContext} from './track/utils.ts';
import type {ChatMessage, LiveKitReactNativeInfo, TranscriptionSegment} from './types.ts';

const separator = '|';
export const ddExtensionURI = 'https://aomediacodec.github.io/av1-rtp-spec/#dependency-descriptor-rtp-header-extension';
const preferredPublishVideoCodecs: ReadonlyArray<VideoCodec> = ['h264', 'vp9', 'vp8', 'av1', 'h265'];

export function unpackStreamId(packed: string): Array<string> {
	const parts = packed.split(separator);
	if (parts.length > 1) {
		return [parts[0], packed.substr(parts[0].length + 1)];
	}
	return [packed, ''];
}

export function sleep(duration: number): TypedPromise<void, never> {
	return new TypedPromise<void, never>((resolve) => CriticalTimers.setTimeout(resolve, duration));
}

export function supportsTransceiver() {
	return 'addTransceiver' in RTCPeerConnection.prototype;
}

export function supportsAddTrack() {
	return 'addTrack' in RTCPeerConnection.prototype;
}

export function supportsAdaptiveStream() {
	return typeof ResizeObserver !== 'undefined' && typeof IntersectionObserver !== 'undefined';
}

export function supportsDynacast() {
	return supportsTransceiver();
}

export function supportsAV1(): boolean {
	if (isSafari() || isFireFox()) {
		return false;
	}
	return hasSenderMimeType('video/av1') || hasSenderMimeType('video/av1x');
}

export function supportsH265(): boolean {
	if (isFireFox()) {
		return false;
	}
	return hasSenderMimeType('video/h265');
}

export function supportsVP9(): boolean {
	if (isFireFox()) {
		return false;
	}
	if (isSafari()) {
		const browser = getBrowser();
		if (browser?.version && compareVersions(browser.version, '16') < 0) {
			return false;
		}
		if (browser?.os === 'iOS' && browser?.osVersion && compareVersions(browser.osVersion, '16') < 0) {
			return false;
		}
	}
	return hasSenderMimeType('video/vp9');
}

export function supportsVideoCodec(codec: VideoCodec): boolean {
	switch (codec) {
		case 'av1':
			return supportsAV1();
		case 'h265':
			return supportsH265();
		case 'vp9':
			return supportsVP9();
		case 'h264':
			return hasSenderMimeType('video/h264') || !hasSenderCapabilitiesApi();
		case 'vp8':
			return hasSenderMimeType('video/vp8') || !hasSenderCapabilitiesApi();
	}
}

export function selectPreferredVideoCodec(
	candidates: ReadonlyArray<VideoCodec> = preferredPublishVideoCodecs,
): VideoCodec {
	return candidates.find((codec) => supportsVideoCodec(codec)) ?? candidates[0] ?? 'vp8';
}

function hasSenderCapabilitiesApi(): boolean {
	return typeof RTCRtpSender !== 'undefined' && 'getCapabilities' in RTCRtpSender;
}

function hasSenderMimeType(mimeType: string): boolean {
	if (!hasSenderCapabilitiesApi()) {
		return false;
	}
	const capabilities = RTCRtpSender.getCapabilities('video');
	if (!capabilities) {
		return false;
	}
	const lower = mimeType.toLowerCase();
	return capabilities.codecs.some((codec) => codec.mimeType.toLowerCase() === lower);
}

export function isSVCCodec(codec?: string): boolean {
	return codec === 'av1' || codec === 'vp9';
}

export function supportsSetSinkId(elm?: HTMLMediaElement): boolean {
	if (!document || isSafariBased()) {
		return false;
	}
	if (!elm) {
		elm = document.createElement('audio');
	}
	return 'setSinkId' in elm;
}

export function supportsAudioOutputSelection(): boolean {
	return supportsSetSinkId();
}

export function isBrowserSupported() {
	if (typeof RTCPeerConnection === 'undefined') {
		return false;
	}
	return supportsTransceiver() || supportsAddTrack();
}

export function isFireFox(): boolean {
	return getBrowser()?.name === 'Firefox';
}

export function isChromiumBased(): boolean {
	const browser = getBrowser();
	return !!browser && browser.name === 'Chrome' && browser.os !== 'iOS';
}

export function isSafari(): boolean {
	return getBrowser()?.name === 'Safari';
}

export function isSafariBased(): boolean {
	const b = getBrowser();
	return b?.name === 'Safari' || b?.os === 'iOS';
}

export function isSafari17Based(): boolean {
	const b = getBrowser();
	return (
		(b?.name === 'Safari' && b.version.startsWith('17.')) ||
		(b?.os === 'iOS' && !!b?.osVersion && compareVersions(b.osVersion, '17') >= 0)
	);
}

export function isSafariSvcApi(browser?: BrowserDetails): boolean {
	if (!browser) {
		browser = getBrowser();
	}
	return (
		(browser?.name === 'Safari' && compareVersions(browser.version, '18.3') > 0) ||
		(browser?.os === 'iOS' && !!browser?.osVersion && compareVersions(browser.osVersion, '18.3') > 0)
	);
}

export function isMobile(): boolean {
	if (!isWeb()) return false;
	const userAgentData = (navigator as Navigator & {userAgentData?: {mobile?: boolean}}).userAgentData;
	return userAgentData?.mobile ?? /Tablet|iPad|Mobile|Android|BlackBerry/.test(navigator.userAgent);
}

export function isE2EESimulcastSupported() {
	const browser = getBrowser();
	const supportedSafariVersion = '17.2';
	if (browser) {
		if (browser.name !== 'Safari' && browser.os !== 'iOS') {
			return true;
		} else if (
			browser.os === 'iOS' &&
			browser.osVersion &&
			compareVersions(browser.osVersion, supportedSafariVersion) >= 0
		) {
			return true;
		} else if (browser.name === 'Safari' && compareVersions(browser.version, supportedSafariVersion) >= 0) {
			return true;
		} else {
			return false;
		}
	}
	return false;
}

export function isWeb(): boolean {
	return typeof document !== 'undefined';
}

export function isReactNative(): boolean {
	return navigator.product === 'ReactNative';
}

export function isCloud(serverUrl: URL) {
	return serverUrl.hostname.endsWith('.livekit.cloud') || serverUrl.hostname.endsWith('.livekit.run');
}

export function extractProjectFromUrl(serverUrl: URL): string | null {
	if (!isCloud(serverUrl)) {
		return null;
	}
	return serverUrl.hostname.split('.')[0];
}

function getLKReactNativeInfo(): LiveKitReactNativeInfo | undefined {
	const g = globalThis as Record<string, unknown>;
	if (g.LiveKitReactNativeGlobal) {
		return g.LiveKitReactNativeGlobal as LiveKitReactNativeInfo;
	}

	return undefined;
}

export function getReactNativeOs(): string | undefined {
	if (!isReactNative()) {
		return undefined;
	}

	const info = getLKReactNativeInfo();
	if (info) {
		return info.platform;
	}

	return undefined;
}

export function getDevicePixelRatio(): number {
	if (isWeb()) {
		return window.devicePixelRatio;
	}

	if (isReactNative()) {
		const info = getLKReactNativeInfo();
		if (info) {
			return info.devicePixelRatio;
		}
	}

	return 1;
}

export function compareVersions(v1: string, v2: string): number {
	const parts1 = v1.split('.');
	const parts2 = v2.split('.');
	const k = Math.min(parts1.length, parts2.length);
	for (let i = 0; i < k; ++i) {
		const p1 = parseInt(parts1[i], 10);
		const p2 = parseInt(parts2[i], 10);
		if (p1 > p2) return 1;
		if (p1 < p2) return -1;
		if (i === k - 1 && p1 === p2) return 0;
	}
	if (v1 === '' && v2 !== '') {
		return -1;
	} else if (v2 === '') {
		return 1;
	}
	return parts1.length === parts2.length ? 0 : parts1.length < parts2.length ? -1 : 1;
}

function roDispatchCallback(entries: Array<ResizeObserverEntry>) {
	for (const entry of entries) {
		(entry.target as ObservableMediaElement).handleResize(entry);
	}
}

function ioDispatchCallback(entries: Array<IntersectionObserverEntry>) {
	for (const entry of entries) {
		(entry.target as ObservableMediaElement).handleVisibilityChanged(entry);
	}
}

let resizeObserver: ResizeObserver | null = null;
export const getResizeObserver = () => {
	if (!resizeObserver) resizeObserver = new ResizeObserver(roDispatchCallback);
	return resizeObserver;
};

let intersectionObserver: IntersectionObserver | null = null;
export const getIntersectionObserver = () => {
	if (!intersectionObserver) {
		intersectionObserver = new IntersectionObserver(ioDispatchCallback, {
			root: null,
			rootMargin: '0px',
		});
	}
	return intersectionObserver;
};

export interface ObservableMediaElement extends HTMLMediaElement {
	handleResize: (entry: ResizeObserverEntry) => void;
	handleVisibilityChanged: (entry: IntersectionObserverEntry) => void;
}

export function getClientInfo(): ClientInfo {
	const info = new ClientInfo({
		sdk: ClientInfo_SDK.JS,
		protocol: protocolVersion,
		version,
	});

	if (isReactNative()) {
		info.os = getReactNativeOs() ?? '';
	}
	return info;
}

let emptyVideoStreamTrack: MediaStreamTrack | undefined;

export function getEmptyVideoStreamTrack() {
	if (!emptyVideoStreamTrack) {
		emptyVideoStreamTrack = createDummyVideoStreamTrack();
	}
	return emptyVideoStreamTrack.clone();
}

export function createDummyVideoStreamTrack(
	width: number = 16,
	height: number = 16,
	enabled: boolean = false,
	paintContent: boolean = false,
) {
	const canvas = document.createElement('canvas');
	canvas.width = width;
	canvas.height = height;
	const ctx = canvas.getContext('2d');
	ctx?.fillRect(0, 0, canvas.width, canvas.height);
	if (paintContent && ctx) {
		ctx.beginPath();
		ctx.arc(width / 2, height / 2, 50, 0, Math.PI * 2, true);
		ctx.closePath();
		ctx.fillStyle = 'grey';
		ctx.fill();
	}
	const dummyStream = canvas.captureStream();
	const [dummyTrack] = dummyStream.getTracks();
	if (!dummyTrack) {
		throw Error('Could not get empty media stream video track');
	}
	dummyTrack.enabled = enabled;

	return dummyTrack;
}

let emptyAudioStreamTrack: MediaStreamTrack | undefined;

export function getEmptyAudioStreamTrack() {
	if (!emptyAudioStreamTrack) {
		const ctx = new AudioContext();
		const oscillator = ctx.createOscillator();
		const gain = ctx.createGain();
		gain.gain.setValueAtTime(0, 0);
		const dst = ctx.createMediaStreamDestination();
		oscillator.connect(gain);
		gain.connect(dst);
		oscillator.start();
		[emptyAudioStreamTrack] = dst.stream.getAudioTracks();
		if (!emptyAudioStreamTrack) {
			throw Error('Could not get empty media stream audio track');
		}
		emptyAudioStreamTrack.enabled = false;
	}
	return emptyAudioStreamTrack.clone();
}

export class Future<T, E extends Error> {
	promise: Promise<T>;

	resolve?: (arg: T) => void;

	reject?: (e: E) => void;

	onFinally?: () => void;

	get isResolved(): boolean {
		return this._isResolved;
	}

	private _isResolved: boolean = false;

	constructor(futureBase?: (resolve: (arg: T) => void, reject: (e: E) => void) => void, onFinally?: () => void) {
		this.onFinally = onFinally;
		this.promise = new Promise<T>((resolve, reject) => {
			this.resolve = resolve;
			this.reject = reject;
			if (futureBase) {
				try {
					void Promise.resolve(futureBase(resolve, reject)).catch(reject);
				} catch (error) {
					reject(error as E);
				}
			}
		}).finally(() => {
			this._isResolved = true;
			this.onFinally?.();
		});
	}
}

export type AudioAnalyserOptions = {
	cloneTrack?: boolean;
	fftSize?: number;
	smoothingTimeConstant?: number;
	minDecibels?: number;
	maxDecibels?: number;
};

export function createAudioAnalyser(track: LocalAudioTrack | RemoteAudioTrack, options?: AudioAnalyserOptions) {
	const opts = {
		cloneTrack: false,
		fftSize: 2048,
		smoothingTimeConstant: 0.8,
		minDecibels: -100,
		maxDecibels: -80,
		...options,
	};
	const audioContext = getNewAudioContext();

	if (!audioContext) {
		throw new Error('Audio Context not supported on this browser');
	}

	const streamTrack = opts.cloneTrack ? track.mediaStreamTrack.clone() : track.mediaStreamTrack;
	const mediaStreamSource = audioContext.createMediaStreamSource(new MediaStream([streamTrack]));
	const analyser = audioContext.createAnalyser();
	analyser.minDecibels = opts.minDecibels;
	analyser.maxDecibels = opts.maxDecibels;
	analyser.fftSize = opts.fftSize;
	analyser.smoothingTimeConstant = opts.smoothingTimeConstant;

	mediaStreamSource.connect(analyser);
	const dataArray = new Uint8Array(analyser.frequencyBinCount);

	const calculateVolume = () => {
		analyser.getByteFrequencyData(dataArray);
		let sum = 0;
		for (const amplitude of dataArray) {
			sum += (amplitude / 255) ** 2;
		}
		const volume = Math.sqrt(sum / dataArray.length);
		return volume;
	};

	const cleanup = async () => {
		await audioContext.close();
		if (opts.cloneTrack) {
			streamTrack.stop();
		}
	};

	return {calculateVolume, analyser, cleanup};
}

export function isAudioCodec(maybeCodec: string): maybeCodec is AudioCodec {
	return audioCodecs.includes(maybeCodec as AudioCodec);
}

export function isVideoCodec(maybeCodec: string): maybeCodec is VideoCodec {
	return videoCodecs.includes(maybeCodec as VideoCodec);
}

export function unwrapConstraint(constraint: ConstrainDOMString): string;
export function unwrapConstraint(constraint: ConstrainULong): number;
export function unwrapConstraint(constraint: ConstrainDOMString | ConstrainULong): string | number {
	if (typeof constraint === 'string' || typeof constraint === 'number') {
		return constraint;
	}

	if (Array.isArray(constraint)) {
		return constraint[0];
	}
	if (constraint.exact !== undefined) {
		if (Array.isArray(constraint.exact)) {
			return constraint.exact[0];
		}
		return constraint.exact;
	}
	if (constraint.ideal !== undefined) {
		if (Array.isArray(constraint.ideal)) {
			return constraint.ideal[0];
		}
		return constraint.ideal;
	}
	throw Error('could not unwrap constraint');
}

export function toWebsocketUrl(url: string): string {
	if (url.startsWith('http')) {
		return url.replace(/^(http)/, 'ws');
	}
	return url;
}

export function toHttpUrl(url: string): string {
	if (url.startsWith('ws')) {
		return url.replace(/^(ws)/, 'http');
	}
	return url;
}

export function extractTranscriptionSegments(
	transcription: TranscriptionModel,
	firstReceivedTimesMap: Map<string, number>,
): Array<TranscriptionSegment> {
	return transcription.segments.map(({id, text, language, startTime, endTime, final}) => {
		const firstReceivedTime = firstReceivedTimesMap.get(id) ?? Date.now();
		const lastReceivedTime = Date.now();
		if (final) {
			firstReceivedTimesMap.delete(id);
		} else {
			firstReceivedTimesMap.set(id, firstReceivedTime);
		}
		return {
			id,
			text,
			startTime: Number.parseInt(startTime.toString(), 10),
			endTime: Number.parseInt(endTime.toString(), 10),
			final,
			language,
			firstReceivedTime,
			lastReceivedTime,
		};
	});
}

export function extractChatMessage(msg: ChatMessageModel): ChatMessage {
	const {id, timestamp, message, editTimestamp} = msg;
	return {
		id,
		timestamp: Number.parseInt(timestamp.toString(), 10),
		editTimestamp: editTimestamp ? Number.parseInt(editTimestamp.toString(), 10) : undefined,
		message,
	};
}

export function getDisconnectReasonFromConnectionError(e: ConnectionError) {
	switch (e.reason) {
		case ConnectionErrorReason.LeaveRequest:
			return e.context as DisconnectReason;
		case ConnectionErrorReason.Cancelled:
			return DisconnectReason.CLIENT_INITIATED;
		case ConnectionErrorReason.NotAllowed:
			return DisconnectReason.USER_REJECTED;
		case ConnectionErrorReason.ServerUnreachable:
			return DisconnectReason.JOIN_FAILURE;
		default:
			return DisconnectReason.UNKNOWN_REASON;
	}
}

export function bigIntToNumber<T extends bigint | undefined>(value: T): T extends bigint ? number : undefined {
	return (value !== undefined ? Number(value) : undefined) as T extends bigint ? number : undefined;
}

export function numberToBigInt<T extends number | undefined>(value: T): T extends number ? bigint : undefined {
	return (value !== undefined ? BigInt(value) : undefined) as T extends number ? bigint : undefined;
}

export function isLocalTrack(track: Track | MediaStreamTrack | undefined): track is LocalTrack {
	return !!track && !(track instanceof MediaStreamTrack) && track.isLocal;
}

export function isAudioTrack(track: Track | undefined): track is LocalAudioTrack | RemoteAudioTrack {
	return !!track && track.kind === Track.Kind.Audio;
}

export function isVideoTrack(track: Track | undefined): track is LocalVideoTrack | RemoteVideoTrack {
	return !!track && track.kind === Track.Kind.Video;
}

export function isLocalVideoTrack(track: Track | MediaStreamTrack | undefined): track is LocalVideoTrack {
	return isLocalTrack(track) && isVideoTrack(track);
}

export function isLocalAudioTrack(track: Track | MediaStreamTrack | undefined): track is LocalAudioTrack {
	return isLocalTrack(track) && isAudioTrack(track);
}

export function isRemoteTrack(track: Track | undefined): track is RemoteTrack {
	return !!track && !track.isLocal;
}

export function isRemotePub(pub: TrackPublication | undefined): pub is RemoteTrackPublication {
	return !!pub && !pub.isLocal;
}

export function isRemoteVideoTrack(track: Track | undefined): track is RemoteVideoTrack {
	return isRemoteTrack(track) && isVideoTrack(track);
}

export function isLocalParticipant(p: Participant): p is LocalParticipant {
	return p.isLocal;
}

export function isRemoteParticipant(p: Participant): p is RemoteParticipant {
	return !p.isLocal;
}

export function splitUtf8(s: string, n: number): Array<Uint8Array> {
	if (n < 4) {
		throw new Error('n must be at least 4 due to utf8 encoding rules');
	}
	const result: Array<Uint8Array> = [];
	let encoded = new TextEncoder().encode(s);
	while (encoded.length > n) {
		let k = n;
		while (k > 0) {
			const byte = encoded[k];
			if (byte !== undefined && (byte & 0xc0) !== 0x80) {
				break;
			}
			k--;
		}
		result.push(encoded.slice(0, k));
		encoded = encoded.slice(k);
	}
	if (encoded.length > 0) {
		result.push(encoded);
	}
	return result;
}

export function extractMaxAgeFromRequestHeaders(headers: Headers): number | undefined {
	const cacheControl = headers.get('Cache-Control');
	if (cacheControl) {
		const maxAge = cacheControl.match(/(?:^|[,\s])max-age=(\d+)/)?.[1];
		if (maxAge) {
			return parseInt(maxAge, 10);
		}
	}
	return undefined;
}
