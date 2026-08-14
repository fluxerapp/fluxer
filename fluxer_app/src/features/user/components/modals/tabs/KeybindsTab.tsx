// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Input} from '@app/features/ui/components/form/FormInput';
import {LinuxInputAccessSection} from '@app/features/user/components/modals/tabs/components/LinuxInputAccessSection';
import styles from '@app/features/user/components/modals/tabs/KeybindsTab.module.css';
import {CustomKeybindsList} from '@app/features/user/components/modals/tabs/keybinds_tab/CustomKeybindsList';
import {DefaultKeybindsList} from '@app/features/user/components/modals/tabs/keybinds_tab/DefaultKeybindsList';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef, useState} from 'react';

const SEARCH_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Search shortcuts…',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const SEARCH_SHORTCUTS_2_DESCRIPTOR = msg({
	message: 'Search shortcuts',
	comment: 'Button or menu action label in the keybinds tab. Keep it concise.',
});
const KeybindsTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement>(null);
	const [searchQuery, setSearchQuery] = useState('');
	return (
		<div
			ref={containerRef}
			className={clsx(styles.container, styles.containerCompact)}
			data-flx="user.keybinds-tab.container"
		>
			<div className={styles.searchRow} data-flx="user.keybinds-tab.search-row">
				<Input
					placeholder={i18n._(SEARCH_SHORTCUTS_DESCRIPTOR)}
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="user.keybinds-tab.magnifying-glass-icon"
						/>
					}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					aria-label={i18n._(SEARCH_SHORTCUTS_2_DESCRIPTOR)}
					data-flx="user.keybinds-tab.input.set-search-query"
				/>
			</div>
			<CustomKeybindsList searchQuery={searchQuery} data-flx="user.keybinds-tab.custom-keybinds-list" />
			<LinuxInputAccessSection data-flx="user.keybinds-tab.linux-input-access-section" />
			<DefaultKeybindsList searchQuery={searchQuery} data-flx="user.keybinds-tab.default-keybinds-list" />
		</div>
	);
});

export default KeybindsTab;
