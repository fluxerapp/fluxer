// SPDX-License-Identifier: AGPL-3.0-or-later

import Keybind, {
	type KeybindCommand,
	type KeybindConfig,
	type KeybindSection,
} from '@app/features/input/state/InputKeybind';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {DefaultShortcutRow} from '@app/features/user/components/modals/tabs/keybinds_tab/DefaultShortcutRow';
import {
	DEFAULT_KEYBIND_SECTIONS,
	entryMatchesQuery,
	normalizeQuery,
	partitionMergedShortcutRows,
	sortBySectionDisplayOrder,
} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const DEFAULT_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Default shortcuts',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const MESSAGES_DESCRIPTOR = msg({
	message: 'Messages',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const NAVIGATION_DESCRIPTOR = msg({
	message: 'Navigation',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const DRAG_AND_DROP_DESCRIPTOR = msg({
	message: 'Drag and drop',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const CHAT_DESCRIPTOR = msg({
	message: 'Chat',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const VOICE_AND_VIDEO_DESCRIPTOR = msg({
	message: 'Voice and video',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
const MISCELLANEOUS_DESCRIPTOR = msg({
	message: 'Miscellaneous',
	comment: 'Short label in the keybinds tab. Keep it concise.',
});
export const DefaultKeybindsList: React.FC<{searchQuery: string}> = observer(({searchQuery}) => {
	const {i18n} = useLingui();
	const defaults = Keybind.getDefaults();
	const customKeybinds = Keybind.getCustomKeybinds();
	const overriddenActions = useMemo(() => {
		const set = new Set<KeybindCommand>();
		for (const entry of customKeybinds) {
			if (entry.action) set.add(entry.action);
		}
		return set;
	}, [customKeybinds]);
	const normalized = normalizeQuery(searchQuery);
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
		const map: Record<KeybindSection, Array<KeybindConfig>> = {
			defaults: [],
			messages: [],
			navigation: [],
			drag_and_drop: [],
			chat: [],
			voice_and_video: [],
			misc: [],
		};
		for (const entry of defaults) {
			if (entry.hideFromDefaults) continue;
			if (!entryMatchesQuery(i18n, entry, normalized)) continue;
			map[entry.section].push(entry);
		}
		return map;
	}, [defaults, normalized, i18n, i18n.locale]);
	const hasAny = DEFAULT_KEYBIND_SECTIONS.some((sectionId) => sections[sectionId].length > 0);
	if (!hasAny && normalized) {
		return (
			<div className={styles.defaultsList} data-flx="user.keybinds-tab.default-keybinds-list.defaults-list">
				<div className={styles.emptyState} data-flx="user.keybinds-tab.default-keybinds-list.empty-state">
					<Trans>No shortcuts match that search.</Trans>
				</div>
			</div>
		);
	}
	return (
		<div className={styles.defaultsList} data-flx="user.keybinds-tab.default-keybinds-list.defaults-list--2">
			{DEFAULT_KEYBIND_SECTIONS.map((sectionId) => {
				const entries = sections[sectionId];
				if (entries.length === 0) return null;
				const orderedEntries = sortBySectionDisplayOrder(sectionId, entries);
				const rows = partitionMergedShortcutRows(orderedEntries);
				return (
					<div
						className={styles.defaultsSection}
						key={sectionId}
						data-flx="user.keybinds-tab.default-keybinds-list.defaults-section"
					>
						<div
							className={styles.defaultsSectionTitle}
							data-flx="user.keybinds-tab.default-keybinds-list.defaults-section-title"
						>
							{sectionLabels[sectionId]}
						</div>
						<div
							className={styles.defaultsSectionRows}
							data-flx="user.keybinds-tab.default-keybinds-list.defaults-section-rows"
						>
							{rows.map((row, idx) => (
								<DefaultShortcutRow
									key={Array.isArray(row) ? `${row[0].action}-${row[1].action}-${idx}` : `${row.action}-${idx}`}
									row={row}
									overriddenActions={overriddenActions}
									data-flx="user.keybinds-tab.default-keybinds-list.default-shortcut-row"
								/>
							))}
						</div>
					</div>
				);
			})}
		</div>
	);
});
