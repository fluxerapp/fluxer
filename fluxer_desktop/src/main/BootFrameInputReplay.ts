// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BOOT_FRAME_MAX_REPLAYED_INPUTS,
	BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH,
	type DesktopBootFrameInput,
	type DesktopBootFrameModifier,
} from '@fluxer/desktop_ipc/src/BootStateContract';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';

const MODIFIERS: ReadonlySet<string> = new Set<DesktopBootFrameModifier>([
	'shift',
	'control',
	'alt',
	'meta',
	'isautorepeat',
]);
const MOUSE_BUTTONS: ReadonlySet<string> = new Set(['left', 'middle', 'right']);
const MAX_KEY_CODE_LENGTH = 16;
const MAX_COORDINATE = 0xffff;
const MAX_WHEEL_DELTA = 10_000;

function readModifiers(value: unknown): Array<DesktopBootFrameModifier> | null {
	if (!Array.isArray(value) || value.length > MODIFIERS.size) {
		return null;
	}
	const modifiers: Array<DesktopBootFrameModifier> = [];
	for (const entry of value) {
		if (typeof entry !== 'string' || !MODIFIERS.has(entry)) {
			return null;
		}
		modifiers.push(entry as DesktopBootFrameModifier);
	}
	return modifiers;
}

function isBounded(value: unknown, limit: number): value is number {
	return typeof value === 'number' && Number.isFinite(value) && Math.abs(value) <= limit;
}

function readInput(value: unknown): DesktopBootFrameInput | null {
	if (!isRecord(value)) {
		return null;
	}
	const modifiers = readModifiers(value.modifiers);
	if (modifiers === null) {
		return null;
	}
	switch (value.kind) {
		case 'key': {
			const {type, keyCode, text} = value;
			if (
				(type !== 'keyDown' && type !== 'keyUp') ||
				typeof keyCode !== 'string' ||
				keyCode.length === 0 ||
				keyCode.length > MAX_KEY_CODE_LENGTH ||
				(text !== null && (typeof text !== 'string' || [...text].length !== 1))
			) {
				return null;
			}
			return {kind: 'key', type, keyCode, text, modifiers};
		}
		case 'mouse': {
			const {type, x, y, button, clickCount} = value;
			if (
				(type !== 'mouseDown' && type !== 'mouseUp') ||
				!isBounded(x, MAX_COORDINATE) ||
				!isBounded(y, MAX_COORDINATE) ||
				typeof button !== 'string' ||
				!MOUSE_BUTTONS.has(button) ||
				typeof clickCount !== 'number' ||
				!Number.isInteger(clickCount) ||
				clickCount < 1 ||
				clickCount > 3
			) {
				return null;
			}
			return {kind: 'mouse', type, x, y, button: button as 'left' | 'middle' | 'right', clickCount, modifiers};
		}
		case 'wheel': {
			const {x, y, deltaX, deltaY} = value;
			if (
				!isBounded(x, MAX_COORDINATE) ||
				!isBounded(y, MAX_COORDINATE) ||
				!isBounded(deltaX, MAX_WHEEL_DELTA) ||
				!isBounded(deltaY, MAX_WHEEL_DELTA)
			) {
				return null;
			}
			return {kind: 'wheel', x, y, deltaX, deltaY, modifiers};
		}
		case 'text': {
			const {text} = value;
			if (typeof text !== 'string' || text.length === 0 || text.length > BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH) {
				return null;
			}
			return {kind: 'text', text, modifiers};
		}
		default:
			return null;
	}
}

export function readBootFrameInputs(value: unknown): Array<DesktopBootFrameInput> | null {
	if (!Array.isArray(value) || value.length === 0 || value.length > BOOT_FRAME_MAX_REPLAYED_INPUTS) {
		return null;
	}
	const inputs: Array<DesktopBootFrameInput> = [];
	for (const entry of value) {
		const input = readInput(entry);
		if (input === null) {
			return null;
		}
		inputs.push(input);
	}
	return inputs;
}

type ReplayedInputEvent = Electron.KeyboardInputEvent | Electron.MouseInputEvent | Electron.MouseWheelInputEvent;

function toElectronInputEvents(input: Exclude<DesktopBootFrameInput, {kind: 'text'}>): Array<ReplayedInputEvent> {
	const modifiers = [...input.modifiers];
	switch (input.kind) {
		case 'key': {
			const events: Array<Electron.KeyboardInputEvent> = [{type: input.type, keyCode: input.keyCode, modifiers}];
			if (input.text !== null) {
				events.push({type: 'char', keyCode: input.text, modifiers});
			}
			return events;
		}
		case 'mouse': {
			const x = Math.round(input.x);
			const y = Math.round(input.y);
			const event: Electron.MouseInputEvent = {
				type: input.type,
				x,
				y,
				button: input.button,
				clickCount: input.clickCount,
				modifiers,
			};
			return input.type === 'mouseDown' ? [{type: 'mouseMove', x, y, modifiers}, event] : [event];
		}
		case 'wheel':
			return [
				{
					type: 'mouseWheel',
					x: Math.round(input.x),
					y: Math.round(input.y),
					deltaX: -input.deltaX,
					deltaY: -input.deltaY,
					canScroll: true,
					modifiers,
				} satisfies Electron.MouseWheelInputEvent,
			];
	}
}

export function replayBootFrameInputs(
	contents: Electron.WebContents,
	inputs: ReadonlyArray<DesktopBootFrameInput>,
): void {
	for (const input of inputs) {
		if (contents.isDestroyed()) {
			return;
		}
		if (input.kind === 'text') {
			void contents.insertText(input.text);
			continue;
		}
		for (const event of toElectronInputEvents(input)) {
			if (contents.isDestroyed()) {
				return;
			}
			contents.sendInputEvent(event);
		}
	}
}
