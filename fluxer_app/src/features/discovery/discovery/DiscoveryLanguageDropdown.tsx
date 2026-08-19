// SPDX-License-Identifier: AGPL-3.0-or-later

import Discovery from '@app/features/discovery/state/Discovery';
import {ContextMenuCloseProvider} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {getSortedDiscoveryLanguages} from '@app/features/user/utils/LocaleUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const ALL_LANGUAGES_DESCRIPTOR = msg({
	message: 'All languages',
	comment: 'Option in the discovery language filter that clears the filter. Keep it concise.',
});

interface Props {
	onClose: () => void;
}

export const DiscoveryLanguageDropdown: React.FC<Props> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const languages = useMemo(() => getSortedDiscoveryLanguages(), [i18n.locale]);
	const selectLanguage = useCallback(
		(language: string | null) => {
			if (Discovery.language !== language) {
				void Discovery.search({language, offset: 0});
			}
			onClose();
		},
		[onClose],
	);
	return (
		<ContextMenuCloseProvider
			value={onClose}
			data-flx="discovery.discovery.discovery-language-dropdown.context-menu-close-provider"
		>
			<MenuGroup data-flx="discovery.discovery.discovery-language-dropdown.menu-group">
				<MenuItemRadio
					label={i18n._(ALL_LANGUAGES_DESCRIPTOR)}
					selected={Discovery.language == null}
					onSelect={() => selectLanguage(null)}
					data-flx="discovery.discovery.discovery-language-dropdown.menu-item-radio.all"
				/>
				{languages.map((language) => (
					<MenuItemRadio
						key={language.code}
						label={language.label}
						selected={Discovery.language === language.code}
						onSelect={() => selectLanguage(language.code)}
						data-flx="discovery.discovery.discovery-language-dropdown.menu-item-radio.language"
					/>
				))}
			</MenuGroup>
		</ContextMenuCloseProvider>
	);
});
