// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {InputTabContent} from '@app/features/user/components/modals/tabs/chat_settings_tab/ChatSettingsTabInputTab';
import {DisplayTabContent} from '@app/features/user/components/modals/tabs/chat_settings_tab/DisplayTab';
import inlineStyles from '@app/features/user/components/modals/tabs/TabInline.module.css';
import {
	DISPLAY_2_DESCRIPTOR as DISPLAY_DESCRIPTOR,
	INPUT_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const ChatSettingsSections = observer(() => {
	const {i18n} = useLingui();
	return (
		<>
			<SettingsSection id="display" title={i18n._(DISPLAY_DESCRIPTOR)} data-flx="user.chat-settings-tab.display">
				<DisplayTabContent data-flx="user.chat-settings-tab.display-tab-content" />
			</SettingsSection>

			<SettingsSection id="input" title={i18n._(INPUT_DESCRIPTOR)} data-flx="user.chat-settings-tab.input">
				<InputTabContent data-flx="user.chat-settings-tab.input-tab-content" />
			</SettingsSection>
		</>
	);
});
const ChatSettingsTab: React.FC = observer(() => {
	return (
		<SettingsTabContainer data-flx="user.chat-settings-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.chat-settings-tab.settings-tab-content">
				<ChatSettingsSections data-flx="user.chat-settings-tab.chat-settings-sections" />
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default ChatSettingsTab;

export const ChatSettingsInlineContent = observer(() => {
	return (
		<div className={inlineStyles.container} data-flx="user.chat-settings-tab.chat-settings-inline-content.div">
			<ChatSettingsSections data-flx="user.chat-settings-tab.chat-settings-inline-content.chat-settings-sections" />
		</div>
	);
});
