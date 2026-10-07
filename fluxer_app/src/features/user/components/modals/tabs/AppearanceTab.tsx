// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {APP_ZOOM_LEVEL_DESCRIPTOR, THEME_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {ActiveNowTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/ActiveNowTab';
import {
	HdrTabContent,
	shouldShowHdrSettings,
} from '@app/features/user/components/modals/tabs/appearance_tab/AppearanceTabHdrTab';
import {ChannelListTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/ChannelListTab';
import {InterfaceTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/InterfaceTab';
import {
	AppearanceTabPreview,
	MessagesTabContent,
} from '@app/features/user/components/modals/tabs/appearance_tab/MessagesTab';
import {
	AppZoomLevelResetAction,
	AppZoomLevelTabContent,
	canResetAppZoomLevel,
} from '@app/features/user/components/modals/tabs/appearance_tab/ScalingTab';
import {StreamerModeTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/StreamerModeTab';
import {ThemeTabContent} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeTabContent';
import {shouldShowAppZoomLevel} from '@app/features/user/components/settings_utils/AppZoomLevelUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const HIGH_DYNAMIC_RANGE_DESCRIPTOR = msg({
	message: 'High dynamic range',
	comment: 'Short label in the appearance tab. Keep it concise.',
});
const MESSAGES_DESCRIPTOR = msg({
	message: 'Messages',
	comment: 'Short label in the appearance tab. Keep it concise.',
});
const INTERFACE_DESCRIPTOR = msg({
	message: 'Interface',
	comment: 'Short label in the appearance tab. Keep it concise.',
});
const CHANNEL_LIST_DESCRIPTOR = msg({
	message: 'Channel list',
	comment: 'Short label in the appearance tab. Keep it concise.',
});
const ACTIVE_NOW_DESCRIPTOR = msg({
	message: 'Active now',
	comment: 'Short label in the appearance tab. Keep it concise.',
});
const STREAMER_MODE_DESCRIPTOR = msg({
	message: 'Streaming privacy',
	comment: 'Short label in the appearance tab. Keep it concise.',
});
export const AppearanceTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const showZoomLevel = shouldShowAppZoomLevel();
	const showHdrSettings = shouldShowHdrSettings();
	return (
		<SettingsTabContainer data-flx="user.appearance-tab.settings-tab-container">
			{!MobileLayout.enabled && <AppearanceTabPreview data-flx="user.appearance-tab.appearance-tab-preview" />}
			<SettingsTabContent data-flx="user.appearance-tab.settings-tab-content">
				<SettingsSection id="theme" title={i18n._(THEME_DESCRIPTOR)} data-flx="user.appearance-tab.theme">
					<ThemeTabContent data-flx="user.appearance-tab.theme-tab-content" />
				</SettingsSection>
				{showHdrSettings ? (
					<SettingsSection id="hdr" title={i18n._(HIGH_DYNAMIC_RANGE_DESCRIPTOR)} data-flx="user.appearance-tab.hdr">
						<HdrTabContent data-flx="user.appearance-tab.hdr-tab-content" />
					</SettingsSection>
				) : null}
				{showZoomLevel ? (
					<SettingsSection
						id="app-zoom-level"
						title={i18n._(APP_ZOOM_LEVEL_DESCRIPTOR)}
						actions={
							canResetAppZoomLevel() ? (
								<AppZoomLevelResetAction data-flx="user.appearance-tab.app-zoom-level-reset-action" />
							) : null
						}
						data-flx="user.appearance-tab.app-zoom-level"
					>
						<AppZoomLevelTabContent data-flx="user.appearance-tab.app-zoom-level-tab-content" />
					</SettingsSection>
				) : null}
				<SettingsSection id="messages" title={i18n._(MESSAGES_DESCRIPTOR)} data-flx="user.appearance-tab.messages">
					<MessagesTabContent data-flx="user.appearance-tab.messages-tab-content" />
				</SettingsSection>
				<SettingsSection id="interface" title={i18n._(INTERFACE_DESCRIPTOR)} data-flx="user.appearance-tab.interface">
					<InterfaceTabContent data-flx="user.appearance-tab.interface-tab-content" />
				</SettingsSection>
				<SettingsSection
					id="channel-list"
					title={i18n._(CHANNEL_LIST_DESCRIPTOR)}
					data-flx="user.appearance-tab.channel-list"
				>
					<ChannelListTabContent data-flx="user.appearance-tab.channel-list-tab-content" />
				</SettingsSection>
				<SettingsSection
					id="active-now"
					tabType="appearance"
					title={i18n._(ACTIVE_NOW_DESCRIPTOR)}
					data-flx="user.appearance-tab.active-now"
				>
					<ActiveNowTabContent data-flx="user.appearance-tab.active-now-tab-content" />
				</SettingsSection>
				<SettingsSection
					id="streamer-mode"
					title={i18n._(STREAMER_MODE_DESCRIPTOR)}
					data-flx="user.appearance-tab.streamer-mode"
				>
					<StreamerModeTabContent data-flx="user.appearance-tab.streamer-mode-tab-content" />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
