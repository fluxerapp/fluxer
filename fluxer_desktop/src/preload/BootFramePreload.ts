// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BOOT_FRAME_INPUT_HELD_ATTRIBUTE,
	BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE,
	BOOT_FRAME_MAX_REPLAYED_INPUTS,
	BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH,
	BOOT_FRAME_TEXT_TARGET_SELECTOR,
	BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE,
	DESKTOP_BOOT_STATE_CHANNELS,
	type DesktopBootFrame,
	type DesktopBootFrameInput,
	type DesktopBootFrameModifier,
} from '@fluxer/desktop_ipc/src/BootStateContract';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';

const BOOT_FRAME_ELEMENT_ID = 'fluxer-boot-frame';
const BOOT_FRAME_MAX_HOLD_MS = 10_000;
const SYSTEM_DARK_QUERY = '(prefers-color-scheme: dark)';
const BOOT_FRAME_STYLE =
	'position:fixed;inset:0;width:100vw;height:100vh;margin:0;border:0;z-index:2147483647;pointer-events:none;user-select:none';
const TEXT_SINK_STYLE =
	'position:fixed;left:0;bottom:0;width:1px;height:1px;margin:0;padding:0;border:0;opacity:0;resize:none;overflow:hidden;pointer-events:none;z-index:2147483646';
const IME_PROCESS_KEY_CODE = 229;

const NAMED_KEY_CODES: Readonly<Record<string, string>> = Object.freeze({
	Enter: 'Enter',
	Backspace: 'Backspace',
	Tab: 'Tab',
	Escape: 'Escape',
	Delete: 'Delete',
	Home: 'Home',
	End: 'End',
	PageUp: 'PageUp',
	PageDown: 'PageDown',
	ArrowUp: 'Up',
	ArrowDown: 'Down',
	ArrowLeft: 'Left',
	ArrowRight: 'Right',
	Shift: 'Shift',
	Control: 'Control',
	Alt: 'Alt',
	Meta: 'Meta',
});
const MOUSE_BUTTONS = ['left', 'middle', 'right'] as const;
const RECORDED_EVENTS = ['keydown', 'keyup', 'mousedown', 'mouseup', 'wheel'] as const;
const SWALLOWED_EVENTS = ['keypress', 'click', 'dblclick', 'auxclick', 'contextmenu'] as const;

interface BootFrameRenderer {
	invoke(channel: string): Promise<unknown>;
	send(channel: string, ...args: Array<unknown>): void;
}

type FrameScheduler = (callback: () => void) => unknown;

function readFrame(value: unknown): DesktopBootFrame | null {
	if (!isRecord(value)) {
		return null;
	}
	const {jpeg, pixelWidth, pixelHeight, viewportWidth, viewportHeight, systemDark} = value;
	if (
		!(jpeg instanceof Uint8Array) ||
		typeof pixelWidth !== 'number' ||
		typeof pixelHeight !== 'number' ||
		typeof viewportWidth !== 'number' ||
		typeof viewportHeight !== 'number' ||
		(systemDark !== null && typeof systemDark !== 'boolean')
	) {
		return null;
	}
	return {jpeg, pixelWidth, pixelHeight, viewportWidth, viewportHeight, systemDark};
}

function matchesAppearance(frame: DesktopBootFrame): boolean {
	return frame.systemDark === null || window.matchMedia(SYSTEM_DARK_QUERY).matches === frame.systemDark;
}

function matchesViewport(frame: DesktopBootFrame): boolean {
	return (
		Math.abs(window.innerWidth - frame.viewportWidth) <= 1 && Math.abs(window.innerHeight - frame.viewportHeight) <= 1
	);
}

function modifiersOf(event: KeyboardEvent | MouseEvent): Array<DesktopBootFrameModifier> {
	const modifiers: Array<DesktopBootFrameModifier> = [];
	if (event.shiftKey) modifiers.push('shift');
	if (event.ctrlKey) modifiers.push('control');
	if (event.altKey) modifiers.push('alt');
	if (event.metaKey) modifiers.push('meta');
	if (event instanceof KeyboardEvent && event.repeat) modifiers.push('isautorepeat');
	return modifiers;
}

function keyCodeOf(event: KeyboardEvent): string | null {
	if (event.key === ' ') {
		return 'Space';
	}
	if (/^F([1-9]|1[0-9]|2[0-4])$/u.test(event.key)) {
		return event.key;
	}
	if ([...event.key].length === 1) {
		return event.key;
	}
	return NAMED_KEY_CODES[event.key] ?? null;
}

function readInput(event: Event): DesktopBootFrameInput | null {
	if (event instanceof KeyboardEvent) {
		const keyCode = keyCodeOf(event);
		if (keyCode === null || event.isComposing) {
			return null;
		}
		const printable = [...event.key].length === 1 && !event.ctrlKey && !event.metaKey;
		return {
			kind: 'key',
			type: event.type === 'keydown' ? 'keyDown' : 'keyUp',
			keyCode,
			text: printable && event.type === 'keydown' ? event.key : null,
			modifiers: modifiersOf(event),
		};
	}
	if (event instanceof WheelEvent) {
		const scale = event.deltaMode === WheelEvent.DOM_DELTA_LINE ? 40 : 1;
		return {
			kind: 'wheel',
			x: event.clientX,
			y: event.clientY,
			deltaX: event.deltaX * scale,
			deltaY: event.deltaY * scale,
			modifiers: modifiersOf(event),
		};
	}
	if (event instanceof MouseEvent) {
		const button = MOUSE_BUTTONS[event.button];
		if (button === undefined) {
			return null;
		}
		return {
			kind: 'mouse',
			type: event.type === 'mousedown' ? 'mouseDown' : 'mouseUp',
			x: event.clientX,
			y: event.clientY,
			button,
			clickCount: Math.max(1, Math.min(3, event.detail)),
			modifiers: modifiersOf(event),
		};
	}
	return null;
}

function isImeKeyEvent(event: Event): boolean {
	return event instanceof KeyboardEvent && (event.isComposing || event.keyCode === IME_PROCESS_KEY_CODE);
}

function createTextSink(): HTMLTextAreaElement {
	const sink = document.createElement('textarea');
	sink.tabIndex = -1;
	sink.setAttribute('aria-hidden', 'true');
	sink.setAttribute('autocomplete', 'off');
	sink.setAttribute('style', TEXT_SINK_STYLE);
	return sink;
}

function holdInputWhileFrameShown(root: HTMLElement): () => ReadonlyArray<DesktopBootFrameInput> {
	const held: Array<DesktopBootFrameInput> = [];
	const hold = (input: DesktopBootFrameInput): void => {
		if (held.length < BOOT_FRAME_MAX_REPLAYED_INPUTS) {
			held.push(input);
			document.documentElement.setAttribute(BOOT_FRAME_INPUT_HELD_ATTRIBUTE, '');
		}
	};
	const record = (event: Event): void => {
		if (isImeKeyEvent(event)) {
			return;
		}
		event.preventDefault();
		event.stopImmediatePropagation();
		const input = readInput(event);
		if (input !== null) {
			hold(input);
		}
	};
	const sink = createTextSink();
	const takeSinkText = (event: Event): void => {
		if (event instanceof InputEvent && event.isComposing) {
			return;
		}
		const text = sink.value.slice(0, BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH);
		sink.value = '';
		if (text.length > 0) {
			hold({kind: 'text', text, modifiers: []});
		}
	};
	sink.addEventListener('input', takeSinkText);
	sink.addEventListener('compositionend', takeSinkText);
	root.appendChild(sink);
	sink.focus({preventScroll: true});
	const swallow = (event: Event): void => {
		event.preventDefault();
		event.stopImmediatePropagation();
	};
	const options = {capture: true, passive: false} as const;
	for (const type of RECORDED_EVENTS) window.addEventListener(type, record, options);
	for (const type of SWALLOWED_EVENTS) window.addEventListener(type, swallow, options);
	return () => {
		for (const type of RECORDED_EVENTS) window.removeEventListener(type, record, options);
		for (const type of SWALLOWED_EVENTS) window.removeEventListener(type, swallow, options);
		sink.removeEventListener('input', takeSinkText);
		sink.removeEventListener('compositionend', takeSinkText);
		sink.remove();
		document.documentElement.removeAttribute(BOOT_FRAME_INPUT_HELD_ATTRIBUTE);
		return held;
	};
}

function whenDocumentElement(attach: (root: HTMLElement) => void): void {
	if (document.documentElement !== null) {
		attach(document.documentElement);
		return;
	}
	const observer = new MutationObserver(() => {
		if (document.documentElement !== null) {
			observer.disconnect();
			attach(document.documentElement);
		}
	});
	observer.observe(document, {childList: true});
}

function createFrameImage(frame: DesktopBootFrame): HTMLImageElement {
	const image = document.createElement('img');
	image.id = BOOT_FRAME_ELEMENT_ID;
	image.alt = '';
	image.draggable = false;
	image.setAttribute('aria-hidden', 'true');
	image.setAttribute('style', BOOT_FRAME_STYLE);
	image.src = URL.createObjectURL(new Blob([new Uint8Array(frame.jpeg)], {type: 'image/jpeg'}));
	return image;
}

function removeFrameImage(image: HTMLImageElement): void {
	image.remove();
	URL.revokeObjectURL(image.src);
}

function isPositionFree(input: DesktopBootFrameInput): boolean {
	return input.kind === 'key' || input.kind === 'text';
}

function takeReplayableInput(held: ReadonlyArray<DesktopBootFrameInput>): ReadonlyArray<DesktopBootFrameInput> {
	const root = document.documentElement;
	const layoutMoved = root.hasAttribute(BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE);
	const wheelLayoutStable = root.hasAttribute(BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE);
	root.removeAttribute(BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE);
	root.removeAttribute(BOOT_FRAME_WHEEL_LAYOUT_STABLE_ATTRIBUTE);
	if (!layoutMoved) return held;
	let hasPriorAction = false;
	return held.filter((input) => {
		if (input.kind === 'wheel') return wheelLayoutStable && !hasPriorAction;
		hasPriorAction = true;
		return isPositionFree(input);
	});
}

export type BootFrameRelease = (view: 'restored' | 'other') => void;

export function installBootFrame(renderer: BootFrameRenderer, scheduleFrame: FrameScheduler): BootFrameRelease {
	let released = false;
	let image: HTMLImageElement | null = null;
	let releaseInput: (() => ReadonlyArray<DesktopBootFrameInput>) | null = null;
	const pressedButtons = new Set<number>();
	let releasePending = false;
	let gestureTimer: ReturnType<typeof setTimeout> | null = null;
	const observeMouse = (event: MouseEvent): void => {
		if (gestureTimer !== null) clearTimeout(gestureTimer);
		gestureTimer = null;
		if (event.type === 'mousedown') pressedButtons.add(event.button);
		else pressedButtons.delete(event.button);
		if (releasePending && pressedButtons.size === 0) {
			gestureTimer = setTimeout(() => {
				gestureTimer = null;
				release('restored');
			}, 0);
		}
	};
	const release: BootFrameRelease = (view) => {
		if (released) {
			return;
		}
		if (view === 'restored' && pressedButtons.size > 0) {
			releasePending = true;
			return;
		}
		released = true;
		if (gestureTimer !== null) clearTimeout(gestureTimer);
		window.removeEventListener('mousedown', observeMouse, true);
		window.removeEventListener('mouseup', observeMouse, true);
		if (view === 'other') renderer.send(DESKTOP_BOOT_STATE_CHANNELS.frameDropped);
		clearTimeout(holdTimer);
		if (image !== null) {
			removeFrameImage(image);
		}
		const held = takeReplayableInput(releaseInput?.() ?? []);
		releaseInput = null;
		if (view === 'restored' && held.length > 0) {
			if (held.some((input) => input.kind === 'text')) {
				document.querySelector<HTMLElement>(BOOT_FRAME_TEXT_TARGET_SELECTOR)?.focus({preventScroll: true});
			}
			renderer.send(DESKTOP_BOOT_STATE_CHANNELS.replayInput, held);
		}
	};
	const dropFrame = (): void => release('other');
	const holdTimer = setTimeout(dropFrame, BOOT_FRAME_MAX_HOLD_MS);
	const attach = (created: HTMLImageElement): void => {
		whenDocumentElement((root) => {
			if (released) {
				return;
			}
			root.appendChild(created);
			window.addEventListener('mousedown', observeMouse, true);
			window.addEventListener('mouseup', observeMouse, true);
			releaseInput = holdInputWhileFrameShown(root);
			scheduleFrame(() => {
				scheduleFrame(() => {
					if (!released) renderer.send(DESKTOP_BOOT_STATE_CHANNELS.framePainted);
				});
			});
		});
	};
	void renderer
		.invoke(DESKTOP_BOOT_STATE_CHANNELS.takeFrame)
		.then(readFrame)
		.then((frame) => {
			if (released || frame === null || !matchesViewport(frame) || !matchesAppearance(frame)) {
				dropFrame();
				return;
			}
			const created = createFrameImage(frame);
			image = created;
			return created.decode().then(() => attach(created));
		})
		.catch(dropFrame);
	return release;
}
