// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {APP_ZOOM_LEVEL_DESCRIPTOR, THEME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ActiveNowTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/ActiveNowTab';
import {
	HdrTabContent,
	shouldShowHdrSettings,
} from '@app/features/user/components/modals/tabs/appearance_tab/AppearanceTabHdrTab';
import {ChannelListTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/ChannelListTab';
import {InterfaceTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/InterfaceTab';
import {MessagesTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/MessagesTab';
import {AppZoomLevelTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/ScalingTab';
import {StreamerModeTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/StreamerModeTab';
import {ThemeTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeTabContent';
import styles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {shouldShowAppZoomLevel} from '@app/features/user/components/settings_utils/AppZoomLevelUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const HIGH_DYNAMIC_RANGE_DESCRIPTOR = msg({
	message: 'High dynamic range',
	comment: 'Short label in the inline. Keep it concise.',
});
const MESSAGES_DESCRIPTOR = msg({
	message: 'Messages',
	comment: 'Short label in the inline. Keep it concise.',
});
const INTERFACE_DESCRIPTOR = msg({
	message: 'Interface',
	comment: 'Short label in the inline. Keep it concise.',
});
const CHANNEL_LIST_DESCRIPTOR = msg({
	message: 'Channel list',
	comment: 'Short label in the inline. Keep it concise.',
});
const ACTIVE_NOW_DESCRIPTOR = msg({
	message: 'Active now',
	comment: 'Short label in the inline. Keep it concise.',
});
const STREAMER_MODE_DESCRIPTOR = msg({
	message: 'Streaming privacy',
	comment: 'Short label in the inline. Keep it concise.',
});
export const AppearanceInlineContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const showHdrSettings = shouldShowHdrSettings();
	const showZoomLevel = shouldShowAppZoomLevel();
	return (
		<div className={styles.container} data-flx="user.appearance-tab.inline.appearance-inline-content.container">
			<SettingsSection
				id="theme"
				title={i18n._(THEME_DESCRIPTOR)}
				data-flx="user.appearance-tab.inline.appearance-inline-content.theme"
			>
				<ThemeTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.theme-tab-content" />
			</SettingsSection>
			{showHdrSettings ? (
				<SettingsSection
					id="hdr"
					title={i18n._(HIGH_DYNAMIC_RANGE_DESCRIPTOR)}
					data-flx="user.appearance-tab.inline.appearance-inline-content.hdr"
				>
					<HdrTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.hdr-tab-content" />
				</SettingsSection>
			) : null}
			{showZoomLevel ? (
				<SettingsSection
					id="app-zoom-level"
					title={i18n._(APP_ZOOM_LEVEL_DESCRIPTOR)}
					data-flx="user.appearance-tab.inline.appearance-inline-content.app-zoom-level"
				>
					<AppZoomLevelTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.app-zoom-level-tab-content" />
				</SettingsSection>
			) : null}
			<SettingsSection
				id="messages"
				title={i18n._(MESSAGES_DESCRIPTOR)}
				data-flx="user.appearance-tab.inline.appearance-inline-content.messages"
			>
				<MessagesTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.messages-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="interface"
				title={i18n._(INTERFACE_DESCRIPTOR)}
				data-flx="user.appearance-tab.inline.appearance-inline-content.interface"
			>
				<InterfaceTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.interface-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="channel-list"
				title={i18n._(CHANNEL_LIST_DESCRIPTOR)}
				data-flx="user.appearance-tab.inline.appearance-inline-content.channel-list"
			>
				<ChannelListTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.channel-list-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="active-now"
				tabType="appearance"
				title={i18n._(ACTIVE_NOW_DESCRIPTOR)}
				data-flx="user.appearance-tab.inline.appearance-inline-content.active-now"
			>
				<ActiveNowTabContent data-flx="user.appearance-tab.inline.appearance-inline-content.active-now-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="streamer-mode"
				title={i18n._(STREAMER_MODE_DESCRIPTOR)}
				data-flx="user.appearance-tab.inline.appearance-inline-content.streamer-mode"
			>
				<StreamerModeTabContent data-flx="user.appearance-tab.appearance-tab-inline.appearance-inline-content.streamer-mode-tab-content" />
			</SettingsSection>
		</div>
	);
});
