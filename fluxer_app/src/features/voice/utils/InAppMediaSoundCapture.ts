// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getSoundCaptureAudioContext,
	getSoundCaptureMasterGainNode,
	isSoundCaptureActive,
	onSoundCaptureActivated,
} from '@app/features/notification/utils/SoundUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('InAppMediaSoundCapture');

interface RoutedElement {
	connected: boolean;
	source: MediaElementAudioSourceNode;
	gain: GainNode;
	volumeListener: () => void;
}

const routedElements: WeakMap<HTMLMediaElement, RoutedElement> = new WeakMap();
const pendingElements: Set<WeakRef<HTMLMediaElement>> = new Set();

function syncGainFromElement(el: HTMLMediaElement, gain: GainNode): void {
	const volume = el.muted ? 0 : Math.max(0, Math.min(1, el.volume));
	try {
		gain.gain.value = volume;
	} catch {}
}

function attachToCaptureGraph(element: HTMLMediaElement): void {
	const existing = routedElements.get(element);
	if (existing) {
		if (existing.connected) return;
		try {
			existing.gain.connect(getSoundCaptureMasterGainNode());
		} catch (error) {
			logger.debug('Failed to reconnect media element to sound capture graph', {error});
			return;
		}
		element.addEventListener('volumechange', existing.volumeListener);
		syncGainFromElement(element, existing.gain);
		existing.connected = true;
		return;
	}
	let ctx: AudioContext;
	let master: GainNode;
	try {
		ctx = getSoundCaptureAudioContext();
		master = getSoundCaptureMasterGainNode();
	} catch (error) {
		logger.debug('Sound capture context unavailable; leaving media element on native playback', {error});
		return;
	}
	let source: MediaElementAudioSourceNode;
	let gain: GainNode;
	try {
		source = ctx.createMediaElementSource(element);
		gain = ctx.createGain();
		source.connect(gain);
		gain.connect(master);
	} catch (error) {
		logger.debug('Failed to route media element through sound capture graph', {error});
		return;
	}
	syncGainFromElement(element, gain);
	const volumeListener = (): void => syncGainFromElement(element, gain);
	element.addEventListener('volumechange', volumeListener);
	routedElements.set(element, {source, gain, volumeListener, connected: true});
}

function detachFromCaptureGraph(element: HTMLMediaElement): void {
	const entry = routedElements.get(element);
	if (!entry || !entry.connected) return;
	entry.connected = false;
	element.removeEventListener('volumechange', entry.volumeListener);
	try {
		entry.gain.disconnect();
	} catch {}
}

function flushPendingElements(): void {
	for (const ref of pendingElements) {
		const element = ref.deref();
		pendingElements.delete(ref);
		if (element) {
			attachToCaptureGraph(element);
		}
	}
}

onSoundCaptureActivated(flushPendingElements);

export function routeMediaElementForSoundCapture(element: HTMLMediaElement): () => void {
	if (isSoundCaptureActive()) {
		attachToCaptureGraph(element);
		return () => detachFromCaptureGraph(element);
	}
	const ref = new WeakRef(element);
	pendingElements.add(ref);
	return () => {
		pendingElements.delete(ref);
		detachFromCaptureGraph(element);
	};
}
