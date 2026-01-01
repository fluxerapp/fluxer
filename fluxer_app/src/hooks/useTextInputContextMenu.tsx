/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {TextareaContextMenu} from '~/components/channel/textarea/TextareaContextMenu';
import {getElectronAPI, isElectron} from '~/utils/NativeUtils';

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

const createSyntheticEvent = (
	event: MouseEvent,
	targetElement?: HTMLElement | null,
	currentTargetElement?: HTMLElement | null,
): React.MouseEvent<HTMLElement> => {
	const view = (event.view ?? null) as unknown as React.MouseEvent<HTMLElement>['view'];
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
	if (input && !DISALLOWED_INPUT_TYPES.has((input.type ?? 'text').toLowerCase())) {
		return input;
	}

	return null;
};

const openTextareaContextMenu = (
	event: React.MouseEvent,
	menuProps?: Partial<React.ComponentProps<typeof TextareaContextMenu>>,
) => {
	ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
		<TextareaContextMenu onClose={onClose} {...menuProps} />
	));
};

export const useTextInputContextMenu = () => {
	const nativeShim = React.useMemo(() => isElectron(), []);

	React.useEffect(() => {
		if (!nativeShim) return;

		const electronAPI = getElectronAPI();
		if (!electronAPI || !electronAPI.onTextareaContextMenu) return;

		return electronAPI.onTextareaContextMenu((params) => {
			const targetNode = document.elementFromPoint(params.x, params.y);
			const editable = getEditableTarget(targetNode);
			if (!editable) return;

			const nativeEvent = new MouseEvent('contextmenu', {
				clientX: params.x,
				clientY: params.y,
				screenX: params.x,
				screenY: params.y,
				bubbles: true,
				cancelable: true,
			});

			openTextareaContextMenu(createSyntheticEvent(nativeEvent, editable, editable), {
				misspelledWord: params.misspelledWord,
				suggestions: params.suggestions,
				editFlags: params.editFlags,
			});
		});
	}, [nativeShim]);
};
