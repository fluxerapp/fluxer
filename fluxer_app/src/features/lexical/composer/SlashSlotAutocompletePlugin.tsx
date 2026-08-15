// SPDX-License-Identifier: AGPL-3.0-or-later

import {Autocomplete} from '@app/features/channel/components/Autocomplete';
import {
	type AutocompleteOption,
	type AutocompleteType,
	getAutocompleteOptionId,
} from '@app/features/channel/components/AutocompleteTypes';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {useLexicalComposerContext} from '@lexical/react/LexicalComposerContext';
import {mergeRegister} from '@lexical/utils';
import {
	COMMAND_PRIORITY_LOW,
	KEY_ARROW_DOWN_COMMAND,
	KEY_ARROW_UP_COMMAND,
	KEY_ENTER_COMMAND,
	KEY_TAB_COMMAND,
} from 'lexical';
import type React from 'react';
import {useEffect, useLayoutEffect, useRef, useState} from 'react';

const SLASH_SLOT_AUTOCOMPLETE_MAIN_AXIS_OFFSET = 8;

function hasModifier(event: KeyboardEvent | null): boolean {
	return event != null && (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey);
}

export interface SlashSlotAutocompletePluginProps {
	options: Array<AutocompleteOption>;
	type: AutocompleteType;
	query: string;
	referenceElement: HTMLElement | null;
	listboxId: string;
	enabled: boolean;
	onSelect: (option: AutocompleteOption) => void;
	activeRef: React.MutableRefObject<boolean>;
}

export const SlashSlotAutocompletePlugin = ({
	options,
	type,
	query,
	referenceElement,
	listboxId,
	enabled,
	onSelect,
	activeRef,
}: SlashSlotAutocompletePluginProps) => {
	const [editor] = useLexicalComposerContext();
	const [selectedIndex, setSelectedIndex] = useState(0);
	const visible = enabled && options.length > 0;

	const optionsRef = useRef(options);
	optionsRef.current = options;
	const selectedIndexRef = useRef(selectedIndex);
	selectedIndexRef.current = selectedIndex;
	const onSelectRef = useRef(onSelect);
	onSelectRef.current = onSelect;
	const visibleRef = useRef(visible);
	visibleRef.current = visible;

	useEffect(() => {
		setSelectedIndex(0);
	}, [options]);

	useLayoutEffect(() => {
		if (!enabled) {
			return;
		}
		activeRef.current = visible;
		return () => {
			activeRef.current = false;
		};
	}, [enabled, visible, activeRef]);

	useEffect(() => {
		if (!enabled) {
			return;
		}
		const root = editor.getRootElement();
		if (root == null) {
			return;
		}
		const clearRootAria = (): void => {
			const target = editor.getRootElement();
			if (target == null) {
				return;
			}
			target.removeAttribute('aria-activedescendant');
			if (target.getAttribute('aria-controls') === listboxId) {
				target.removeAttribute('aria-controls');
			}
		};
		if (visible) {
			root.setAttribute('aria-controls', listboxId);
			root.setAttribute('aria-activedescendant', getAutocompleteOptionId(listboxId, selectedIndex));
		} else {
			clearRootAria();
		}
		return clearRootAria;
	}, [editor, enabled, listboxId, selectedIndex, visible]);

	useEffect(() => {
		const move = (delta: number): boolean => {
			const count = optionsRef.current.length;
			if (count === 0) {
				return false;
			}
			setSelectedIndex((index) => (((index + delta) % count) + count) % count);
			return true;
		};
		const choose = (): boolean => {
			const currentOptions = optionsRef.current;
			const index = selectedIndexRef.current;
			if (index < 0 || index >= currentOptions.length) {
				return false;
			}
			onSelectRef.current(currentOptions[index]!);
			return true;
		};
		return mergeRegister(
			editor.registerCommand(
				KEY_ARROW_DOWN_COMMAND,
				(event) => {
					if (!visibleRef.current || hasModifier(event)) {
						return false;
					}
					if (event != null) {
						event.preventDefault();
					}
					return move(1);
				},
				COMMAND_PRIORITY_LOW,
			),
			editor.registerCommand(
				KEY_ARROW_UP_COMMAND,
				(event) => {
					if (!visibleRef.current || hasModifier(event)) {
						return false;
					}
					if (event != null) {
						event.preventDefault();
					}
					return move(-1);
				},
				COMMAND_PRIORITY_LOW,
			),
			editor.registerCommand(
				KEY_ENTER_COMMAND,
				(event) => {
					if (!visibleRef.current || hasModifier(event) || (event != null && isIMEComposing(event))) {
						return false;
					}
					if (event != null) {
						event.preventDefault();
					}
					return choose();
				},
				COMMAND_PRIORITY_LOW,
			),
			editor.registerCommand(
				KEY_TAB_COMMAND,
				(event) => {
					if (!visibleRef.current || hasModifier(event) || isIMEComposing(event)) {
						return false;
					}
					event.preventDefault();
					return choose();
				},
				COMMAND_PRIORITY_LOW,
			),
		);
	}, [editor]);

	if (!visible) {
		return null;
	}
	return (
		<Autocomplete
			type={type}
			options={options}
			selectedIndex={selectedIndex}
			setSelectedIndex={setSelectedIndex}
			onSelect={onSelect}
			referenceElement={referenceElement}
			query={query}
			attached
			listboxId={listboxId}
			mainAxisOffset={SLASH_SLOT_AUTOCOMPLETE_MAIN_AXIS_OFFSET}
		/>
	);
};
