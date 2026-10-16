// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
/// <reference path="../../type-polyfills/document-pip.d.ts" />
import type {FrameMetadata} from '../../frameMetadata/types.ts';
import {debounce} from '../debounce.ts';
import {TrackEvent} from '../events.ts';
import type {VideoReceiverStats} from '../stats.ts';
import {computeBitrate} from '../stats.ts';
import CriticalTimers from '../timers.ts';
import type {LoggerOptions} from '../types.ts';
import type {ObservableMediaElement} from '../utils.ts';
import {getDevicePixelRatio, getIntersectionObserver, getResizeObserver, isWeb} from '../utils.ts';
import type {FrameMetadataExtractor} from './FrameMetadataExtractor.ts';
import RemoteTrack from './RemoteTrack.ts';
import {attachToElement, detachTrack, Track} from './Track.ts';
import type {AdaptiveStreamSettings} from './types.ts';

const REACTION_DELAY = 100;

type CodecStats = RTCStats & {
	mimeType: string;
};

function hasMimeType(stats: RTCStats): stats is CodecStats {
	return 'mimeType' in stats && typeof stats.mimeType === 'string';
}

export default class RemoteVideoTrack extends RemoteTrack<Track.Kind.Video> {
	private prevStats?: VideoReceiverStats;

	private elementInfos: Array<ElementInfo> = [];

	private adaptiveStreamSettings?: AdaptiveStreamSettings;

	private lastVisible?: boolean;

	private lastDimensions?: Track.Dimensions;

	frameMetadataExtractor?: FrameMetadataExtractor;

	constructor(
		mediaTrack: MediaStreamTrack,
		sid: string,
		receiver: RTCRtpReceiver,
		adaptiveStreamSettings?: AdaptiveStreamSettings,
		loggerOptions?: LoggerOptions,
	) {
		super(mediaTrack, sid, Track.Kind.Video, receiver, loggerOptions);
		this.adaptiveStreamSettings = adaptiveStreamSettings;
	}

	get isAdaptiveStream(): boolean {
		return this.adaptiveStreamSettings !== undefined;
	}

	lookupFrameMetadata({rtpTimestamp}: {rtpTimestamp: number}): FrameMetadata | undefined {
		return this.frameMetadataExtractor?.lookupMetadata(rtpTimestamp);
	}

	override setStreamState(value: Track.StreamState) {
		super.setStreamState(value);
		this.log.debug('setStreamState', value);
		if (this.isAdaptiveStream && value === Track.StreamState.Active) {
			this.updateVisibility();
		}
	}

	override get mediaStreamTrack() {
		return this._mediaStreamTrack;
	}

	override setMuted(muted: boolean) {
		super.setMuted(muted);

		this.attachedElements.forEach((element) => {
			if (muted) {
				detachTrack(this._mediaStreamTrack, element);
			} else {
				attachToElement(this._mediaStreamTrack, element);
			}
		});
	}

	override attach(): HTMLMediaElement;
	override attach(element: HTMLMediaElement): HTMLMediaElement;
	override attach(element?: HTMLMediaElement): HTMLMediaElement {
		if (!element) {
			element = super.attach();
		} else {
			super.attach(element);
		}

		if (this.adaptiveStreamSettings && this.elementInfos.find((info) => info.element === element) === undefined) {
			const elementInfo = new HTMLElementInfo(element);
			this.observeElementInfo(elementInfo);
		}
		return element;
	}

	observeElementInfo(elementInfo: ElementInfo) {
		if (this.adaptiveStreamSettings && this.elementInfos.find((info) => info === elementInfo) === undefined) {
			elementInfo.handleResize = () => {
				this.debouncedHandleResize();
			};
			elementInfo.handleVisibilityChanged = () => {
				this.updateVisibility();
			};
			this.elementInfos.push(elementInfo);
			elementInfo.observe();
			this.debouncedHandleResize();
			this.updateVisibility();
		} else {
			this.log.warn('visibility resize observer not triggered', this.logContext);
		}
	}

	stopObservingElementInfo(elementInfo: ElementInfo) {
		if (!this.isAdaptiveStream) {
			this.log.warn('stopObservingElementInfo ignored', this.logContext);
			return;
		}
		const stopElementInfos = this.elementInfos.filter((info) => info === elementInfo);
		for (const info of stopElementInfos) {
			info.stopObserving();
		}
		this.elementInfos = this.elementInfos.filter((info) => info !== elementInfo);
		this.updateVisibility();
		this.debouncedHandleResize();
	}
	override detach(): Array<HTMLMediaElement>;
	override detach(element: HTMLMediaElement): HTMLMediaElement;
	override detach(element?: HTMLMediaElement): HTMLMediaElement | Array<HTMLMediaElement> {
		if (element) {
			this.stopObservingElement(element);
			return super.detach(element);
		}
		const detachedElements = super.detach();

		for (const e of detachedElements) {
			this.stopObservingElement(e);
		}

		return detachedElements;
	}

	getDecoderImplementation(): string | undefined {
		return this.prevStats?.decoderImplementation;
	}

	protected monitorReceiver = async () => {
		if (!this.receiver) {
			this._currentBitrate = 0;
			return;
		}
		if (this.isAdaptiveStream && this.lastVisible === false) {
			this._currentBitrate = 0;
			this.prevStats = undefined;
			return;
		}
		const stats = await this.getReceiverStats();

		if (stats && this.prevStats && this.receiver) {
			this._currentBitrate = computeBitrate(stats, this.prevStats);
		}

		this.prevStats = stats;
	};

	async getReceiverStats(): Promise<VideoReceiverStats | undefined> {
		if (!this.receiver?.getStats) {
			return;
		}

		const stats = await this.receiver.getStats();
		let receiverStats: VideoReceiverStats | undefined;
		let codecID = '';
		const codecs = new Map<string, CodecStats>();
		stats.forEach((v) => {
			if (v.type === 'inbound-rtp') {
				codecID = v.codecId;
				receiverStats = {
					type: 'video',
					streamId: v.id,
					framesDecoded: v.framesDecoded,
					framesDropped: v.framesDropped,
					framesReceived: v.framesReceived,
					packetsReceived: v.packetsReceived,
					packetsLost: v.packetsLost,
					frameWidth: v.frameWidth,
					frameHeight: v.frameHeight,
					pliCount: v.pliCount,
					firCount: v.firCount,
					nackCount: v.nackCount,
					jitter: v.jitter,
					timestamp: v.timestamp,
					bytesReceived: v.bytesReceived,
					decoderImplementation: v.decoderImplementation,
					powerEfficientDecoder: v.powerEfficientDecoder,
				};
			} else if (v.type === 'codec' && hasMimeType(v)) {
				codecs.set(v.id, v);
			}
		});
		const codecStats = codecs.get(codecID);
		if (receiverStats && codecStats) {
			receiverStats.mimeType = codecStats.mimeType;
		}
		return receiverStats;
	}

	private stopObservingElement(element: HTMLMediaElement) {
		const stopElementInfos = this.elementInfos.filter((info) => info.element === element);
		for (const info of stopElementInfos) {
			this.stopObservingElementInfo(info);
		}
	}

	protected override async handleAppVisibilityChanged() {
		await super.handleAppVisibilityChanged();
		if (!this.isAdaptiveStream) return;
		this.updateVisibility();
	}

	private readonly debouncedHandleResize = debounce(() => {
		this.updateDimensions();
	}, REACTION_DELAY);

	private updateVisibility(forceEmit?: boolean) {
		const lastVisibilityChange = this.elementInfos.reduce(
			(prev, info) => Math.max(prev, info.visibilityChangedAt || 0),
			0,
		);

		const backgroundPause = (this.adaptiveStreamSettings?.pauseVideoInBackground ?? true) ? this.isInBackground : false;
		const isPiPMode = this.elementInfos.some((info) => info.pictureInPicture);
		const isVisible = (this.elementInfos.some((info) => info.visible) && !backgroundPause) || isPiPMode;

		if (this.lastVisible === isVisible && !forceEmit) {
			return;
		}

		if (!isVisible && Date.now() - lastVisibilityChange < REACTION_DELAY) {
			CriticalTimers.setTimeout(() => {
				this.updateVisibility();
			}, REACTION_DELAY);
			return;
		}

		this.lastVisible = isVisible;
		this.emit(TrackEvent.VisibilityChanged, isVisible, this);
	}

	private updateDimensions() {
		let maxWidth = 0;
		let maxHeight = 0;
		const pixelDensity = this.getPixelDensity();
		for (const info of this.elementInfos) {
			const currentElementWidth = info.width() * pixelDensity;
			const currentElementHeight = info.height() * pixelDensity;
			if (currentElementWidth + currentElementHeight > maxWidth + maxHeight) {
				maxWidth = currentElementWidth;
				maxHeight = currentElementHeight;
			}
		}

		if (this.lastDimensions?.width === maxWidth && this.lastDimensions?.height === maxHeight) {
			return;
		}

		this.lastDimensions = {
			width: maxWidth,
			height: maxHeight,
		};

		this.emit(TrackEvent.VideoDimensionsChanged, this.lastDimensions, this);
	}

	private getPixelDensity(): number {
		const pixelDensity = this.adaptiveStreamSettings?.pixelDensity;
		if (pixelDensity === 'screen') {
			return getDevicePixelRatio();
		} else if (!pixelDensity) {
			const devicePixelRatio = getDevicePixelRatio();
			if (devicePixelRatio > 2) {
				return 2;
			} else {
				return 1;
			}
		}
		return pixelDensity;
	}
}

export interface ElementInfo {
	element: object;
	width(): number;
	height(): number;
	visible: boolean;
	pictureInPicture: boolean;
	visibilityChangedAt: number | undefined;

	handleResize?: () => void;
	handleVisibilityChanged?: () => void;
	observe(): void;
	stopObserving(): void;
}

class HTMLElementInfo implements ElementInfo {
	element: HTMLMediaElement;

	get visible(): boolean {
		return this.isPiP || this.isIntersecting;
	}

	get pictureInPicture(): boolean {
		return this.isPiP;
	}

	visibilityChangedAt: number | undefined;

	handleResize?: () => void;

	handleVisibilityChanged?: () => void;

	private isPiP: boolean;

	private isIntersecting: boolean;

	constructor(element: HTMLMediaElement, visible?: boolean) {
		this.element = element;
		this.isIntersecting = visible ?? isElementInViewport(element);
		this.isPiP = isWeb() && isElementInPiP(element);
		this.visibilityChangedAt = 0;
	}

	width(): number {
		return this.element.clientWidth;
	}

	height(): number {
		return this.element.clientHeight;
	}

	observe() {
		this.isIntersecting = isElementInViewport(this.element);
		this.isPiP = isElementInPiP(this.element);

		(this.element as ObservableMediaElement).handleResize = () => {
			this.handleResize?.();
		};
		(this.element as ObservableMediaElement).handleVisibilityChanged = this.onVisibilityChanged;

		getIntersectionObserver().observe(this.element);
		getResizeObserver().observe(this.element);
		(this.element as HTMLVideoElement).addEventListener('enterpictureinpicture', this.onEnterPiP);
		(this.element as HTMLVideoElement).addEventListener('leavepictureinpicture', this.onLeavePiP);
		window.documentPictureInPicture?.addEventListener('enter', this.onEnterPiP);
		window.documentPictureInPicture?.window?.addEventListener('pagehide', this.onLeavePiP);
	}

	private onVisibilityChanged = (entry: IntersectionObserverEntry) => {
		const {target, isIntersecting} = entry;
		if (target === this.element) {
			this.isIntersecting = isIntersecting;
			this.isPiP = isElementInPiP(this.element);
			this.visibilityChangedAt = Date.now();
			this.handleVisibilityChanged?.();
		}
	};

	private onEnterPiP = () => {
		window.documentPictureInPicture?.window?.addEventListener('pagehide', this.onLeavePiP);
		queueMicrotask(() => {
			requestAnimationFrame(() => {
				this.isPiP = isElementInPiP(this.element);
				this.handleVisibilityChanged?.();
			});
		});
	};

	private onLeavePiP = () => {
		this.isPiP = isElementInPiP(this.element);
		this.handleVisibilityChanged?.();
	};

	stopObserving() {
		getIntersectionObserver()?.unobserve(this.element);
		getResizeObserver()?.unobserve(this.element);
		(this.element as HTMLVideoElement).removeEventListener('enterpictureinpicture', this.onEnterPiP);
		(this.element as HTMLVideoElement).removeEventListener('leavepictureinpicture', this.onLeavePiP);
		window.documentPictureInPicture?.removeEventListener('enter', this.onEnterPiP);
		window.documentPictureInPicture?.window?.removeEventListener('pagehide', this.onLeavePiP);
	}
}

function isElementInPiP(el: HTMLElement) {
	if (document.pictureInPictureElement === el) return true;
	if (window.documentPictureInPicture?.window) return isElementInViewport(el, window.documentPictureInPicture?.window);
	return false;
}

function isElementInViewport(el: HTMLElement, win?: Window) {
	const viewportWindow = win || window;
	let top = el.offsetTop;
	let left = el.offsetLeft;
	const width = el.offsetWidth;
	const height = el.offsetHeight;
	const {hidden} = el;
	const {display} = getComputedStyle(el);

	while (el.offsetParent) {
		el = el.offsetParent as HTMLElement;
		top += el.offsetTop;
		left += el.offsetLeft;
	}

	return (
		top < viewportWindow.pageYOffset + viewportWindow.innerHeight &&
		left < viewportWindow.pageXOffset + viewportWindow.innerWidth &&
		top + height > viewportWindow.pageYOffset &&
		left + width > viewportWindow.pageXOffset &&
		!hidden &&
		display !== 'none'
	);
}
