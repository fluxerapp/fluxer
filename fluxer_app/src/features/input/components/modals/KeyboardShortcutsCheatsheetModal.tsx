// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useTextOverflow} from '@app/features/app/hooks/useTextOverflow';
import styles from '@app/features/input/components/modals/KeyboardShortcutsCheatsheetModal.module.css';
import Keybind, {type KeybindConfig, type KeybindSection} from '@app/features/input/state/InputKeybind';
import {formatKeyComboParts} from '@app/features/input/utils/KeybindUtils';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	chipsForDefaultEntry,
	DEFAULT_KEYBIND_SECTIONS,
	sortBySectionDisplayOrder,
} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useRef} from 'react';

const KEYBOARD_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Keyboard shortcuts',
	comment: 'Modal title for the keyboard shortcuts cheatsheet.',
});
const DEFAULT_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Default shortcuts',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});
const MESSAGES_DESCRIPTOR = msg({
	message: 'Messages',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});
const NAVIGATION_DESCRIPTOR = msg({
	message: 'Navigation',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});
const DRAG_AND_DROP_DESCRIPTOR = msg({
	message: 'Drag and drop',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});
const CHAT_DESCRIPTOR = msg({
	message: 'Chat',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});
const VOICE_AND_VIDEO_DESCRIPTOR = msg({
	message: 'Voice and video',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});
const MISCELLANEOUS_DESCRIPTOR = msg({
	message: 'Miscellaneous',
	comment: 'Short heading in the keyboard shortcuts cheatsheet.',
});

interface CheatsheetShortcut {
	action: string;
	label: string;
	chipGroups: Array<Array<string>>;
}

function getShortcutChipGroups(i18n: I18n, entry: KeybindConfig, hasCustomBinding: boolean): Array<Array<string>> {
	if (!hasCustomBinding) {
		const chips = chipsForDefaultEntry(i18n, entry);
		return chips.length > 0 ? [chips] : [];
	}
	return Keybind.getActiveCombosForAction(entry.action)
		.map((combo) => formatKeyComboParts(i18n, combo))
		.filter((chips) => chips.length > 0);
}

const ShortcutLabel: React.FC<{label: string}> = ({label}) => {
	const labelRef = useRef<HTMLSpanElement>(null);
	const isLabelOverflowing = useTextOverflow(labelRef);
	return (
		<Tooltip text={isLabelOverflowing ? label : ''} data-flx="input.keyboard-shortcuts-cheatsheet-modal.tooltip">
			<span ref={labelRef} className={styles.shortcutLabel} data-flx="input.keyboard-shortcuts-cheatsheet-modal.label">
				{label}
			</span>
		</Tooltip>
	);
};

const ShortcutChipGroups: React.FC<{groups: Array<Array<string>>}> = ({groups}) => (
	<div className={styles.chipGroups} data-flx="input.keyboard-shortcuts-cheatsheet-modal.chip-groups">
		{groups.map((group, groupIndex) => (
			<div
				className={styles.chipGroup}
				key={`${groupIndex}-${group.join('-')}`}
				data-flx="input.keyboard-shortcuts-cheatsheet-modal.chip-group"
			>
				{group.map((chip, chipIndex) => (
					<kbd
						className={styles.chip}
						key={`${chipIndex}-${chip}`}
						data-flx="input.keyboard-shortcuts-cheatsheet-modal.chip"
					>
						{chip}
					</kbd>
				))}
			</div>
		))}
	</div>
);

export const KeyboardShortcutsCheatsheetModal = observer(() => {
	const {i18n} = useLingui();
	const defaults = Keybind.getDefaults();
	const customKeybinds = Keybind.getCustomKeybinds();
	const customActions = useMemo(() => {
		const result = new Set<string>();
		for (const entry of customKeybinds) {
			if (entry.action) result.add(entry.action);
		}
		return result;
	}, [customKeybinds]);
	const sectionLabels: Record<KeybindSection, string> = {
		defaults: i18n._(DEFAULT_SHORTCUTS_DESCRIPTOR),
		messages: i18n._(MESSAGES_DESCRIPTOR),
		navigation: i18n._(NAVIGATION_DESCRIPTOR),
		drag_and_drop: i18n._(DRAG_AND_DROP_DESCRIPTOR),
		chat: i18n._(CHAT_DESCRIPTOR),
		voice_and_video: i18n._(VOICE_AND_VIDEO_DESCRIPTOR),
		misc: i18n._(MISCELLANEOUS_DESCRIPTOR),
	};
	const sections = useMemo(() => {
		const result: Record<KeybindSection, Array<CheatsheetShortcut>> = {
			defaults: [],
			messages: [],
			navigation: [],
			drag_and_drop: [],
			chat: [],
			voice_and_video: [],
			misc: [],
		};
		for (const sectionId of DEFAULT_KEYBIND_SECTIONS) {
			const entries = defaults.filter((entry) => entry.section === sectionId && !entry.hideFromDefaults);
			for (const entry of sortBySectionDisplayOrder(sectionId, entries)) {
				const chipGroups = getShortcutChipGroups(i18n, entry, customActions.has(entry.action));
				if (chipGroups.length === 0) continue;
				result[sectionId].push({
					action: entry.action,
					label: entry.label,
					chipGroups,
				});
			}
		}
		return result;
	}, [customActions, defaults, i18n, i18n.locale]);
	return (
		<Modal.Root size="xlarge" data-flx="input.keyboard-shortcuts-cheatsheet-modal.modal-root">
			<Modal.ScreenReaderLabel
				text={i18n._(KEYBOARD_SHORTCUTS_DESCRIPTOR)}
				data-flx="input.keyboard-shortcuts-cheatsheet-modal.modal-screen-reader-label"
			/>
			<Modal.Content
				padding="none"
				className={styles.content}
				data-flx="input.keyboard-shortcuts-cheatsheet-modal.modal-content"
			>
				<div className={styles.sections} data-flx="input.keyboard-shortcuts-cheatsheet-modal.sections">
					{DEFAULT_KEYBIND_SECTIONS.map((sectionId) => {
						const entries = sections[sectionId];
						if (entries.length === 0) return null;
						return (
							<section
								className={styles.section}
								key={sectionId}
								data-flx="input.keyboard-shortcuts-cheatsheet-modal.section"
							>
								<h4 className={styles.sectionTitle} data-flx="input.keyboard-shortcuts-cheatsheet-modal.section-title">
									{sectionLabels[sectionId]}
								</h4>
								<div className={styles.shortcutRows} data-flx="input.keyboard-shortcuts-cheatsheet-modal.shortcut-rows">
									{entries.map((entry) => (
										<div
											className={styles.shortcutRow}
											key={entry.action}
											data-flx="input.keyboard-shortcuts-cheatsheet-modal.shortcut-row"
										>
											<ShortcutLabel
												label={entry.label}
												data-flx="input.keyboard-shortcuts-cheatsheet-modal.shortcut-label"
											/>
											<ShortcutChipGroups
												groups={entry.chipGroups}
												data-flx="input.keyboard-shortcuts-cheatsheet-modal.shortcut-chip-groups"
											/>
										</div>
									))}
								</div>
							</section>
						);
					})}
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
