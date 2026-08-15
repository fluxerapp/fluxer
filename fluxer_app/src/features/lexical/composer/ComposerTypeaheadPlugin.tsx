// SPDX-License-Identifier: AGPL-3.0-or-later

import {Autocomplete} from '@app/features/channel/components/Autocomplete';
import {
	type AutocompleteOption,
	type AutocompleteType,
	getAutocompleteOptionId,
} from '@app/features/channel/components/AutocompleteTypes';
import {$getTextUpToCursor} from '@app/features/lexical/composer/composerOffsets';
import {buildComposerMenuMatch} from '@app/features/lexical/composer/composerTypeaheadTrigger';
import {$getActiveOptionalContext, $getActiveSlotAutocompleteContext} from '@app/features/lexical/composer/slashSlots';
import {useLexicalComposerContext} from '@lexical/react/LexicalComposerContext';
import {
	LexicalTypeaheadMenuPlugin,
	MenuOption,
	type MenuRenderFn,
	type TriggerFn,
} from '@lexical/react/LexicalTypeaheadMenuPlugin';
import {$getRoot, COMMAND_PRIORITY_LOW, HISTORY_MERGE_TAG, type LexicalEditor, type TextNode} from 'lexical';
import type React from 'react';
import {useCallback, useLayoutEffect, useMemo, useRef} from 'react';

class ComposerAutocompleteMenuOption extends MenuOption {
	readonly option: AutocompleteOption;

	constructor(option: AutocompleteOption, key: string) {
		super(key);
		this.option = option;
	}
}

export interface ComposerTypeaheadPluginProps {
	options: Array<AutocompleteOption>;
	type: AutocompleteType;
	query: string;
	referenceElement: HTMLElement | null;
	listboxId: string;
	enabled: boolean;
	onSelect: (option: AutocompleteOption) => void;
	activeRef: React.MutableRefObject<boolean>;
}

export const ComposerTypeaheadPlugin = ({
	options,
	type,
	query,
	referenceElement,
	listboxId,
	enabled,
	onSelect,
	activeRef,
}: ComposerTypeaheadPluginProps) => {
	const [editor] = useLexicalComposerContext();
	const menuOptions = useMemo(
		() => options.map((option, index) => new ComposerAutocompleteMenuOption(option, `composer-typeahead-${index}`)),
		[options],
	);

	const optionsAvailableRef = useRef(false);
	optionsAvailableRef.current = enabled && options.length > 0;

	const prevAvailableRef = useRef(false);
	useLayoutEffect(() => {
		const available = optionsAvailableRef.current;
		if (available !== prevAvailableRef.current) {
			editor.update(
				() => {
					$getRoot().markDirty();
				},
				{tag: HISTORY_MERGE_TAG},
			);
		}
		prevAvailableRef.current = available;
	});

	const triggerFn = useCallback<TriggerFn>((text) => {
		if (!optionsAvailableRef.current) {
			return null;
		}
		if ($getActiveSlotAutocompleteContext() != null || $getActiveOptionalContext() != null) {
			return null;
		}
		const fullText = $getTextUpToCursor();
		return buildComposerMenuMatch(fullText, text);
	}, []);

	const onSelectOption = useCallback(
		(
			selectedOption: ComposerAutocompleteMenuOption,
			_textNodeContainingQuery: TextNode | null,
			closeMenu: () => void,
		) => {
			const {option} = selectedOption;
			closeMenu();
			onSelect(option);
		},
		[onSelect],
	);

	const onClose = useCallback(() => {
		activeRef.current = false;
	}, [activeRef]);

	const menuRenderFn = useCallback<MenuRenderFn<ComposerAutocompleteMenuOption>>(
		(anchorElementRef, {selectedIndex, selectOptionAndCleanUp, setHighlightedIndex}) => {
			const hasOptions = enabled && options.length > 0;
			activeRef.current = hasOptions;
			if (!hasOptions) {
				return null;
			}
			const resolvedSelectedIndex = selectedIndex == null ? 0 : selectedIndex;
			return (
				<>
					<ComposerTypeaheadAccessibilityBridge
						editor={editor}
						anchorElementRef={anchorElementRef}
						listboxId={listboxId}
						selectedIndex={resolvedSelectedIndex}
					/>
					<Autocomplete
						type={type}
						options={options}
						selectedIndex={resolvedSelectedIndex}
						setSelectedIndex={(next) => {
							const resolved = typeof next === 'function' ? next(resolvedSelectedIndex) : next;
							setHighlightedIndex(resolved);
						}}
						onSelect={(option) => {
							const index = options.indexOf(option);
							const menuOption = index >= 0 ? menuOptions[index] : undefined;
							if (menuOption) {
								selectOptionAndCleanUp(menuOption);
							}
						}}
						referenceElement={referenceElement}
						query={query}
						attached
						listboxId={listboxId}
					/>
				</>
			);
		},
		[activeRef, editor, enabled, listboxId, menuOptions, options, query, referenceElement, type],
	);

	return (
		<LexicalTypeaheadMenuPlugin<ComposerAutocompleteMenuOption>
			options={menuOptions}
			triggerFn={triggerFn}
			onQueryChange={NOOP_QUERY_CHANGE}
			onSelectOption={onSelectOption}
			onClose={onClose}
			menuRenderFn={menuRenderFn}
			commandPriority={COMMAND_PRIORITY_LOW}
			ignoreEntityBoundary
		/>
	);
};

function ComposerTypeaheadAccessibilityBridge({
	editor,
	anchorElementRef,
	listboxId,
	selectedIndex,
}: {
	editor: LexicalEditor;
	anchorElementRef: React.RefObject<HTMLElement | null>;
	listboxId: string;
	selectedIndex: number;
}) {
	const selectedIndexRef = useRef(selectedIndex);
	selectedIndexRef.current = selectedIndex;

	useLayoutEffect(() => {
		const root = editor.getRootElement();
		const anchor = anchorElementRef.current;
		if (root == null || anchor == null) {
			return;
		}
		const synchronize = () => {
			const lexicalSelectedIndex = getLexicalSelectedIndex(root.getAttribute('aria-activedescendant'));
			setAttributeIfChanged(root, 'aria-controls', listboxId);
			setAttributeIfChanged(
				root,
				'aria-activedescendant',
				getAutocompleteOptionId(
					listboxId,
					lexicalSelectedIndex == null ? selectedIndexRef.current : lexicalSelectedIndex,
				),
			);
			removeAttributeIfPresent(anchor, 'id');
			removeAttributeIfPresent(anchor, 'role');
			removeAttributeIfPresent(anchor, 'aria-label');
			setAttributeIfChanged(anchor, 'aria-hidden', 'true');
		};

		synchronize();
		const defaultView = root.ownerDocument.defaultView;
		const MutationObserverConstructor = defaultView == null ? undefined : defaultView.MutationObserver;
		const observer = MutationObserverConstructor == null ? null : new MutationObserverConstructor(synchronize);
		if (observer != null) {
			observer.observe(root, {
				attributes: true,
				attributeFilter: ['aria-controls', 'aria-activedescendant'],
			});
			observer.observe(anchor, {
				attributes: true,
				attributeFilter: ['id', 'role', 'aria-label', 'aria-hidden'],
			});
		}
		return () => {
			if (observer != null) {
				observer.disconnect();
			}
			removeAttributeIfValue(root, 'aria-controls', listboxId);
			removeAttributeIfValue(
				root,
				'aria-activedescendant',
				getAutocompleteOptionId(listboxId, selectedIndexRef.current),
			);
			removeAttributeIfValue(anchor, 'aria-hidden', 'true');
		};
	}, [anchorElementRef, editor, listboxId]);

	useLayoutEffect(() => {
		const root = editor.getRootElement();
		if (root != null) {
			setAttributeIfChanged(root, 'aria-activedescendant', getAutocompleteOptionId(listboxId, selectedIndex));
		}
	}, [editor, listboxId, selectedIndex]);

	return null;
}

function setAttributeIfChanged(element: HTMLElement, name: string, value: string): void {
	if (element.getAttribute(name) !== value) {
		element.setAttribute(name, value);
	}
}

function removeAttributeIfPresent(element: HTMLElement, name: string): void {
	if (element.hasAttribute(name)) {
		element.removeAttribute(name);
	}
}

function removeAttributeIfValue(element: HTMLElement, name: string, value: string): void {
	if (element.getAttribute(name) === value) {
		element.removeAttribute(name);
	}
}

function getLexicalSelectedIndex(activeDescendant: string | null): number | null {
	const match = /^typeahead-item-(\d+)$/.exec(activeDescendant == null ? '' : activeDescendant);
	return match == null ? null : Number(match[1]);
}

const NOOP_QUERY_CHANGE = (_matchingString: string | null): void => {};
