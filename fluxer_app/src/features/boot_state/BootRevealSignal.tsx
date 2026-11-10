// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	bootFrameNeedsHistory,
	notifyFirstContentPainted,
	whenBootFrameInputSettled,
} from '@app/features/boot_state/BootDomFrame';
import {retainFrameImages} from '@app/features/boot_state/BootFrameImages';
import {replayFrameInput} from '@app/features/boot_state/BootFrameInput';
import {releaseBootReveal} from '@app/features/boot_state/BootReveal';
import {BOOT_FRAME_ELEMENT_ID} from '@app/features/boot_state/BootStateBrowserFormat';
import {
	frameLayoutsAllowWheel,
	measureFrameLayout,
	takeRestoredFrameLayout,
} from '@app/features/boot_state/BootViewport';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {
	BOOT_FRAME_INPUT_HELD_ATTRIBUTE,
	BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE,
	BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE,
} from '@fluxer/desktop_ipc/src/BootStateContract';
import {reaction} from 'mobx';
import {useLayoutEffect} from 'react';

const VISIBLE_IMAGE_DECODE_BUDGET_MS = 150;

const logger = new Logger('BootRevealSignal');

function nextFrame(signal: AbortSignal): Promise<void> {
	if (signal.aborted) return Promise.resolve();
	return new Promise((resolve) => {
		const frame = requestAnimationFrame(() => {
			signal.removeEventListener('abort', abort);
			resolve();
		});
		const abort = () => {
			cancelAnimationFrame(frame);
			resolve();
		};
		signal.addEventListener('abort', abort, {once: true});
	});
}

function intersectsViewport(rect: DOMRect): boolean {
	return (
		rect.width > 0 &&
		rect.height > 0 &&
		rect.bottom > 0 &&
		rect.right > 0 &&
		rect.top < window.innerHeight &&
		rect.left < window.innerWidth
	);
}

function visibleImages(): ReadonlyArray<HTMLImageElement> {
	return Array.from(document.images).filter(
		(image) => image.getAttribute('src') != null && intersectsViewport(image.getBoundingClientRect()),
	);
}

async function decodeVisibleImages(signal: AbortSignal, deadline: number): Promise<void> {
	if (signal.aborted) return;
	const frame = document.getElementById(BOOT_FRAME_ELEMENT_ID);
	const live = document.getElementById('root');
	if (frame !== null && live !== null && !StreamerMode.enabled) retainFrameImages(frame, live, signal);
	const warmed = new Set(
		Array.from(frame?.querySelectorAll('img') ?? [], (image) =>
			image.complete && image.naturalWidth > 0 ? image.currentSrc : '',
		),
	);
	const pending = visibleImages().filter(
		(image) => !frame?.contains(image) && !(image.complete && warmed.has(image.currentSrc)),
	);

	const remaining = deadline - performance.now();
	if (pending.length === 0 || remaining <= 0) return;
	await new Promise<void>((resolve) => {
		const done = () => {
			clearTimeout(timer);
			signal.removeEventListener('abort', done);
			resolve();
		};
		const timer = setTimeout(done, remaining);
		signal.addEventListener('abort', done, {once: true});
		void Promise.all(pending.map((image) => image.decode().catch(() => undefined))).then(done);
	});
}

async function settleRedaction(signal: AbortSignal): Promise<void> {
	const enabledAtCommit = StreamerMode.enabled;
	await StreamerMode.whenRedactionSettled();
	if (StreamerMode.enabled !== enabledAtCommit) {
		await nextFrame(signal);
		await nextFrame(signal);
	}
}

async function checkHeldInputLayout(signal: AbortSignal): Promise<void> {
	const root = document.documentElement;
	if (!root.hasAttribute(BOOT_FRAME_INPUT_HELD_ATTRIBUTE)) return;
	if (isDesktopLocalAppDocument() || bootFrameNeedsHistory()) {
		Messages.prependAllDeferredOlderMessages();
		await nextFrame(signal);
	}
	const frameLayout = takeRestoredFrameLayout();
	const currentLayout = measureFrameLayout();
	root.removeAttribute(BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE);
	if (frameLayout === null || frameLayout !== currentLayout) {
		logger.info('The live layout differs from the boot frame, so clicks held under it are dropped', {
			frameLayoutKnown: frameLayout !== null,
		});
		root.setAttribute(BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE, '');
		if (isDesktopLocalAppDocument() && frameLayout !== null && frameLayoutsAllowWheel(frameLayout, currentLayout)) {
			root.setAttribute(BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE, '');
		}
	}
}

export function BootRevealSignal(): null {
	useLayoutEffect(() => {
		const controller = new AbortController();
		const {signal} = controller;
		let restoreVisibility: (() => void) | null = null;
		let redacting = false;
		const dispose = reaction(
			() => StreamerMode.enabled,
			(enabled) => {
				if (!enabled || redacting) return;
				redacting = true;
				const live = document.getElementById('root');
				if (live !== null) {
					const visibility = live.style.visibility;
					live.style.visibility = 'hidden';
					restoreVisibility = () => {
						live.style.visibility = visibility;
					};
				}
				notifyFirstContentPainted('other');
				void settleRedaction(signal)
					.then(() => nextFrame(signal))
					.then(() => nextFrame(signal))
					.then(() => {
						if (!signal.aborted) {
							restoreVisibility?.();
							releaseBootReveal();
						}
					});
			},
		);
		const imageDeadline = performance.now() + VISIBLE_IMAGE_DECODE_BUDGET_MS;
		void Promise.all([decodeVisibleImages(signal, imageDeadline), nextFrame(signal), settleRedaction(signal)]).then(
			async () => {
				if (signal.aborted) return;
				await checkHeldInputLayout(signal);
				await decodeVisibleImages(signal, imageDeadline);
				await whenBootFrameInputSettled();
				if (signal.aborted || redacting) return;
				notifyFirstContentPainted('restored', replayFrameInput);
				releaseBootReveal();
			},
		);
		return () => {
			controller.abort();
			restoreVisibility?.();
			dispose();
		};
	}, []);
	return null;
}
