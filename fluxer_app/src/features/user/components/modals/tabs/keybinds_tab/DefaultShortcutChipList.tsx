// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {OVERRIDDEN_CHIP_STYLE} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

export const DefaultShortcutChipList: React.FC<{chips: Array<string>; overridden?: boolean; disabled?: boolean}> = ({
	chips,
	overridden,
	disabled,
}) => (
	<div className={styles.defaultChips} data-flx="user.keybinds-tab.default-shortcut-chip-list.default-chips">
		{chips.length === 0 ? (
			<span
				className={styles.defaultChipsEmpty}
				data-flx="user.keybinds-tab.default-shortcut-chip-list.default-chips-empty"
			>
				<Trans>Unassigned</Trans>
			</span>
		) : (
			chips.map((chip, idx) => (
				<span
					key={`${idx}-${chip}`}
					className={styles.defaultChip}
					style={overridden || disabled ? OVERRIDDEN_CHIP_STYLE : undefined}
					data-flx="user.keybinds-tab.default-shortcut-chip-list.default-chip"
				>
					{chip}
				</span>
			))
		)}
		{overridden ? (
			<span
				className={styles.defaultChipsEmpty}
				data-flx="user.keybinds-tab.default-shortcut-chip-list.default-chips-empty--2"
			>
				<Trans>Overridden</Trans>
			</span>
		) : disabled ? (
			<span
				className={styles.defaultChipsEmpty}
				data-flx="user.keybinds-tab.default-shortcut-chip-list.default-chips-empty--3"
			>
				<Trans comment="Status next to a built-in shortcut the user turned off in the keybinds tab.">Disabled</Trans>
			</span>
		) : null}
	</div>
);
