// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	TextareaContextMenu,
	type TextareaContextMenuEditFlags,
} from '@app/features/channel/components/textarea/TextareaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {getElectronAPI, isElectron} from '@app/features/ui/utils/NativeUtils';
import type React from 'react';
import type {AbstractView} from 'react';
import {useEffect, useMemo} from 'react';

interface ContextMenuCoordinates {
	clientX: number;
	clientY: number;
	screenX: number;
	screenY: number;
}

interface RecentContextMenuTarget {
	element: HTMLElement;
	coordinates: ContextMenuCoordinates;
	timestamp: number;
}

function toAbstractView(view: Window | null): AbstractView | null {
	if (view === null) return null;
	return view;
}

const DISALLOWED_INPUT_TYPES = new Set([
	'button',
	'checkbox',
	'color',
	'date',
	'datetime-local',
	'file',
	'hidden',
	'image',
	'radio',
	'range',
	'reset',
	'submit',
	'time',
	'week',
	'password',
]);
const CONTEXT_TARGET_MAX_AGE_MS = 5000;
const createSyntheticEvent = (
	event: MouseEvent,
	targetElement?: HTMLElement | null,
	currentTargetElement?: HTMLElement | null,
): React.MouseEvent<HTMLElement> => {
	const view = toAbstractView(event.view) ?? window;
	return {
		preventDefault: () => {},
		stopPropagation: () => {},
		pageX: event.pageX,
		pageY: event.pageY,
		clientX: event.clientX,
		clientY: event.clientY,
		screenX: event.screenX,
		screenY: event.screenY,
		movementX: 0,
		movementY: 0,
		button: event.button,
		buttons: event.buttons,
		altKey: event.altKey,
		ctrlKey: event.ctrlKey,
		metaKey: event.metaKey,
		shiftKey: event.shiftKey,
		detail: event.detail,
		target: (targetElement ?? (event.target as HTMLElement | null)) as HTMLElement,
		currentTarget: (currentTargetElement ?? (event.currentTarget as HTMLElement | null)) as HTMLElement,
		nativeEvent: event,
		bubbles: event.bubbles,
		cancelable: event.cancelable,
		defaultPrevented: event.defaultPrevented,
		eventPhase: event.eventPhase,
		isTrusted: event.isTrusted,
		timeStamp: event.timeStamp,
		type: 'contextmenu',
		getModifierState: (key: string) => event.getModifierState(key),
		isDefaultPrevented: () => event.defaultPrevented,
		isPropagationStopped: () => false,
		persist: () => {},
		view,
		relatedTarget: null,
	} satisfies React.MouseEvent<HTMLElement>;
};
const getEditableTarget = (node: Element | null): HTMLElement | null => {
	if (!node) return null;
	if (node instanceof HTMLTextAreaElement) {
		return node;
	}
	if (node instanceof HTMLInputElement) {
		const inputType = (node.type ?? 'text').toLowerCase();
		if (!DISALLOWED_INPUT_TYPES.has(inputType)) {
			return node;
		}
	}
	if ((node as HTMLElement).isContentEditable) {
		return node as HTMLElement;
	}
	const textarea = node.closest('textarea') as HTMLTextAreaElement | null;
	if (textarea) {
		return textarea;
	}
	const input = node.closest('input') as HTMLInputElement | null;
	if (input && !DISALLOWED_INPUT_TYPES.has((input['type'] ?? 'text').toLowerCase())) {
		return input;
	}
	return null;
};
const openTextareaContextMenu = (
	event: React.MouseEvent,
	menuProps?: Partial<React.ComponentProps<typeof TextareaContextMenu>>,
) => {
	ContextMenuCommands.openFromEvent(event, ({onClose}) => (
		<TextareaContextMenu
			onClose={onClose}
			data-flx="ui.use-text-input-context-menu.open-textarea-context-menu.textarea-context-menu"
			{...menuProps}
		/>
	));
};
const getCandidateCoordinates = (params: {x: number; y: number}): Array<ContextMenuCoordinates> => {
	const screenOffsetCandidate = {
		clientX: params['x'] - window.screenX,
		clientY: params['y'] - window.screenY,
		screenX: params['x'],
		screenY: params['y'],
	} satisfies ContextMenuCoordinates;
	const rawCandidate = {
		clientX: params['x'],
		clientY: params['y'],
		screenX: params['x'],
		screenY: params['y'],
	} satisfies ContextMenuCoordinates;
	const unique = new Map<string, ContextMenuCoordinates>();
	for (const candidate of [screenOffsetCandidate, rawCandidate]) {
		const key = `${candidate.clientX}:${candidate.clientY}`;
		unique.set(key, candidate);
	}
	return [...unique.values()];
};
const getRecentContextMenuTarget = (
	recentTarget: RecentContextMenuTarget | null,
): {editable: HTMLElement; coordinates: ContextMenuCoordinates} | null => {
	if (!recentTarget) return null;
	if (!recentTarget.element.isConnected) return null;
	if (Date.now() - recentTarget.timestamp > CONTEXT_TARGET_MAX_AGE_MS) return null;
	return {
		editable: recentTarget.element,
		coordinates: recentTarget.coordinates,
	};
};
const getEditableTargetFromCoordinates = (
	candidates: Array<ContextMenuCoordinates>,
): {editable: HTMLElement; coordinates: ContextMenuCoordinates} | null => {
	for (const coordinates of candidates) {
		const targetNode = document.elementFromPoint(coordinates.clientX, coordinates.clientY);
		const editable = getEditableTarget(targetNode);
		if (editable) {
			return {editable, coordinates};
		}
	}
	return null;
};
export const useTextInputContextMenu = () => {
	const nativeShim = useMemo(() => isElectron(), []);
	useEffect(() => {
		if (!nativeShim) return;
		const electronAPI = getElectronAPI();
		if (!electronAPI || !electronAPI.onTextareaContextMenu) return;
		let recentTarget: RecentContextMenuTarget | null = null;
		const onContextMenuCapture = (event: MouseEvent) => {
			const editable = getEditableTarget(event.target as Element | null);
			if (!editable) return;
			recentTarget = {
				element: editable,
				coordinates: {
					clientX: event.clientX,
					clientY: event.clientY,
					screenX: event.screenX,
					screenY: event.screenY,
				},
				timestamp: Date.now(),
			};
		};
		window.addEventListener('contextmenu', onContextMenuCapture, true);
		const unsubscribe = electronAPI.onTextareaContextMenu((params) => {
			const candidates = getCandidateCoordinates(params);
			let resolvedTarget = getRecentContextMenuTarget(recentTarget);
			if (!resolvedTarget) {
				resolvedTarget = getEditableTargetFromCoordinates(candidates);
			}
			if (!resolvedTarget) {
				const activeEditable = getEditableTarget(document.activeElement);
				if (!activeEditable) return;
				const fallbackCoordinates = candidates[0];
				if (!fallbackCoordinates) return;
				resolvedTarget = {
					editable: activeEditable,
					coordinates: fallbackCoordinates,
				};
			}
			const nativeEvent = new MouseEvent('contextmenu', {
				clientX: resolvedTarget.coordinates.clientX,
				clientY: resolvedTarget.coordinates.clientY,
				screenX: resolvedTarget.coordinates.screenX,
				screenY: resolvedTarget.coordinates.screenY,
				bubbles: true,
				cancelable: true,
			});
			openTextareaContextMenu(createSyntheticEvent(nativeEvent, resolvedTarget.editable, resolvedTarget.editable), {
				misspelledWord: params.misspelledWord ?? undefined,
				suggestions: params.suggestions,
				editFlags: params.editFlags as TextareaContextMenuEditFlags | undefined,
				targetElement: resolvedTarget.editable,
				showSendButtonToggle: resolvedTarget.editable.closest('flx-channel-textarea') != null,
			});
		});
		return () => {
			unsubscribe();
			window.removeEventListener('contextmenu', onContextMenuCapture, true);
		};
	}, [nativeShim]);
};
