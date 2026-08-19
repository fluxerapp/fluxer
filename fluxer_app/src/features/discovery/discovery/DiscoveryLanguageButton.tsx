// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {DiscoveryLanguageDropdown} from '@app/features/discovery/discovery/DiscoveryLanguageDropdown';
import Discovery from '@app/features/discovery/state/Discovery';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import {getDiscoveryLanguageLabel} from '@app/features/user/utils/LocaleUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TranslateIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const FILTER_BY_LANGUAGE_DESCRIPTOR = msg({
	message: 'Filter by language',
	comment: 'Label for the discovery language filter button. Keep it concise.',
});
const FILTERING_BY_LANGUAGE_DESCRIPTOR = msg({
	message: 'Filtering by {language}',
	comment:
		'Label for the discovery language filter button once a language is chosen. Preserve {language}; it is inserted by code.',
});

export const DiscoveryLanguageButton = observer(() => {
	const {i18n} = useLingui();
	const {isOpen, withTracking} = useContextMenuTrigger();
	const activeLanguage = Discovery.language;
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromElementBottomRight(
				event,
				({onClose}) => (
					<DiscoveryLanguageDropdown
						onClose={onClose}
						data-flx="discovery.discovery.discovery-language-button.handle-click.discovery-language-dropdown"
					/>
				),
				withTracking(),
			);
		},
		[withTracking],
	);
	return (
		<ChannelHeaderIcon
			icon={TranslateIcon}
			iconWeight="bold"
			label={
				activeLanguage == null
					? i18n._(FILTER_BY_LANGUAGE_DESCRIPTOR)
					: i18n._(FILTERING_BY_LANGUAGE_DESCRIPTOR, {language: getDiscoveryLanguageLabel(activeLanguage)})
			}
			isSelected={isOpen || activeLanguage != null}
			aria-haspopup="menu"
			aria-expanded={isOpen}
			onClick={handleClick}
			data-flx="discovery.discovery.discovery-language-button.channel-header-icon.click"
		/>
	);
});
