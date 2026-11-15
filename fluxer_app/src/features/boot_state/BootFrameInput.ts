// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {$selectComposerRange} from '@app/features/lexical/composer/composerOffsets';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {KEY_ENTER_COMMAND} from 'lexical';
import {flushSync} from 'react-dom';

const logger = new Logger('BootFrameInput');

interface FrameTarget {
	readonly flx: string | null;
	readonly index: number;
	readonly identity: readonly [string, string] | null;
	readonly innerIndex: number;
	readonly composer: boolean;
}

interface Modifiers {
	readonly shiftKey: boolean;
	readonly ctrlKey: boolean;
	readonly altKey: boolean;
	readonly metaKey: boolean;
}

interface ComposerInput {
	readonly kind: 'composer';
	readonly text: string;
	readonly anchor: number | null;
	readonly focus: number | null;
	readonly focused: boolean;
}

type FrameInput =
	| ComposerInput
	| ({readonly kind: 'send'} & Modifiers)
	| ({
			readonly kind: 'click';
			readonly type: string;
			readonly target: FrameTarget;
			readonly x: number;
			readonly y: number;
			readonly button: number;
			readonly detail: number;
	  } & Modifiers)
	| ({
			readonly kind: 'key';
			readonly type: string;
			readonly key: string;
			readonly code: string;
			readonly keyCode: number;
			readonly location: number;
			readonly repeat: boolean;
			readonly target: FrameTarget;
	  } & Modifiers)
	| {
			readonly kind: 'scroll';
			readonly target: FrameTarget;
			readonly top: number;
			readonly left: number;
	  };

export interface HeldFrameInput {
	readonly inputs: ReadonlyArray<FrameInput>;
	readonly overflow: number;
}

export interface FrameComposer {
	resolve(): ComposerHandle | null;
	adopt(text: string): void;
}

function byFlx(owner: Element, flx: string, index: number): HTMLElement | null {
	return owner.querySelectorAll<HTMLElement>(`[data-flx="${CSS.escape(flx)}"]`)[index] ?? null;
}

function resolveTarget(target: FrameTarget, composer: ComposerHandle | null): HTMLElement | null {
	if (target.composer) {
		return composer === null ? null : composer.getEditor().getRootElement();
	}
	if (target.identity !== null) {
		const [name, value] = target.identity;
		const owner = document.querySelector<HTMLElement>(`#root [${CSS.escape(name)}="${CSS.escape(value)}"]`);
		if (owner === null || target.flx === null || owner.getAttribute('data-flx') === target.flx) {
			return owner;
		}
		return byFlx(owner, target.flx, target.innerIndex);
	}
	const root = document.getElementById('root');
	return root === null || target.flx === null ? null : byFlx(root, target.flx, target.index);
}

function applyComposer(input: ComposerInput, registration: FrameComposer | null): void {
	const handle = registration?.resolve();
	if (handle == null) {
		if (input.text.length > 0 || input.focused) logger.warn('The held composer input has no live composer');
		return;
	}
	flushSync(() => {
		registration?.adopt(input.text);
		if (handle.getDisplayValue() !== input.text) handle.hydrate(input.text, []);
	});
	if (input.focused) {
		handle.focus();
		const anchor = input.anchor === null ? input.text.length : input.anchor;
		const focus = input.focus === null ? anchor : input.focus;
		handle.getEditor().update(() => $selectComposerRange(anchor, focus), {discrete: true});
	}
}

function replayClick(input: Extract<FrameInput, {kind: 'click'}>, target: HTMLElement): void {
	const buttons = [1, 4, 2][input.button] ?? 0;
	const init = {...input, bubbles: true, cancelable: true, composed: true, clientX: input.x, clientY: input.y};
	if (input.type === 'dblclick') {
		target.dispatchEvent(new MouseEvent('dblclick', init));
		return;
	}
	const pointer = {pointerId: 1, isPrimary: true, pointerType: 'mouse'};
	target.dispatchEvent(new PointerEvent('pointerdown', {...init, ...pointer, buttons}));
	const allowFocus = target.dispatchEvent(new MouseEvent('mousedown', {...init, buttons}));
	if (allowFocus)
		target.closest<HTMLElement>('button, a[href], input, textarea, [tabindex]')?.focus({preventScroll: true});
	target.dispatchEvent(new PointerEvent('pointerup', {...init, ...pointer, buttons: 0}));
	target.dispatchEvent(new MouseEvent('mouseup', {...init, buttons: 0}));
	target.dispatchEvent(new MouseEvent(input.type, {...init, buttons: 0}));
}

function keyboardEvent(input: Extract<FrameInput, {kind: 'key'}>): KeyboardEvent {
	const event = new KeyboardEvent(input.type, {...input, bubbles: true, cancelable: true, composed: true});
	Object.defineProperty(event, 'which', {value: input.keyCode});
	return event;
}

function moveTabFocus(backward: boolean): void {
	const elements = Array.from(
		document.querySelectorAll<HTMLElement>(
			'#root button, #root a[href], #root input, #root textarea, #root [tabindex], #root [contenteditable="true"]',
		),
	).filter(
		(element) =>
			element.tabIndex >= 0 && !element.matches(':disabled, [inert] *') && element.getClientRects().length > 0,
	);
	const index = elements.indexOf(document.activeElement as HTMLElement);
	const next = index + (backward ? -1 : 1);
	elements[(next + elements.length) % elements.length]?.focus({preventScroll: true});
}

export function replayFrameInput(
	input: HeldFrameInput,
	resolveComposer: () => FrameComposer | null,
	layoutMoved: boolean,
): void {
	if (input.overflow > 0) logger.warn('The boot frame input log is at its limit', {dropped: input.overflow});
	let submittedText: string | null = null;
	let submittedComposer: ComposerInput | null = null;
	let appliedComposer: ComposerInput | null = null;
	let pendingComposer: ComposerInput | null = null;
	for (const entry of input.inputs) {
		try {
			if (entry.kind === 'scroll') {
				const target = resolveTarget(entry.target, resolveComposer()?.resolve() ?? null);
				if (target !== null) {
					target.scrollTop += entry.top;
					target.scrollLeft += entry.left;
				}
				continue;
			}
			if (entry.kind === 'composer') {
				pendingComposer = entry;
				continue;
			}
			if (pendingComposer !== null && pendingComposer !== appliedComposer && pendingComposer.text !== submittedText) {
				applyComposer(pendingComposer, resolveComposer());
				appliedComposer = pendingComposer;
			}
			const handle = resolveComposer()?.resolve() ?? null;
			if (entry.kind === 'send') {
				if (pendingComposer !== null && pendingComposer === submittedComposer) continue;
				if (handle === null || pendingComposer === null) throw new Error('The held send has no live composer state');
				if (pendingComposer !== appliedComposer || handle.getDisplayValue() !== pendingComposer.text) {
					applyComposer(pendingComposer, resolveComposer());
					appliedComposer = pendingComposer;
				}
				submittedText = handle.getDisplayValue();
				submittedComposer = pendingComposer;
				flushSync(() =>
					handle
						.getEditor()
						.dispatchCommand(
							KEY_ENTER_COMMAND,
							new KeyboardEvent('keydown', {...entry, key: 'Enter', code: 'Enter', cancelable: true}),
						),
				);
				continue;
			}
			let target = resolveTarget(entry.target, handle);
			if (entry.kind === 'key') {
				const active = document.activeElement;
				const recipient = active instanceof HTMLElement && active !== document.body ? active : target;
				flushSync(() => {
					const allowed = (recipient === null ? window : recipient).dispatchEvent(keyboardEvent(entry));
					if (allowed && entry.type === 'keydown' && entry.key === 'Tab') moveTabFocus(entry.shiftKey);
					if (allowed && recipient?.matches('button, a[href]') && entry.key === 'Enter' && entry.type === 'keydown')
						recipient.click();
					if (allowed && recipient?.matches('button') && entry.key === ' ' && entry.type === 'keyup') recipient.click();
				});
				continue;
			}
			if (target === null && !layoutMoved && entry.target.identity === null && entry.target.flx === null) {
				const candidate = document.elementFromPoint(entry.x, entry.y);
				if (candidate instanceof HTMLElement && candidate.closest('#root')) target = candidate;
			}
			if (target === null) {
				logger.warn('A held input target is gone', {kind: entry.kind, target: entry.target});
				continue;
			}
			const recipient = target;
			flushSync(() => replayClick(entry, recipient));
		} catch (error) {
			logger.warn('Failed to replay a held input', {kind: entry.kind, error});
		}
	}
	if (pendingComposer !== null && pendingComposer !== appliedComposer && pendingComposer.text !== submittedText) {
		try {
			applyComposer(pendingComposer, resolveComposer());
		} catch (error) {
			logger.warn('Failed to restore the final held composer state', error);
		}
	}
}
