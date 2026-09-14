// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {OVERRIDDEN_CHIP_STYLE} from '@app/features/user/components/modals/tabs/keybinds_tab/shared';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

export const DefaultShortcutChipList: React.FC<{chips: Array<string>; overridden?: boolean}> = ({
	chips,
	overridden,
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
					style={overridden ? OVERRIDDEN_CHIP_STYLE : undefined}
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
		) : null}
	</div>
);
