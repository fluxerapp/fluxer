// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import styles from '@app/features/user/components/modals/tabs/chat_settings_tab/ChatSettingsTabInline.module.css';
import {InputTabContent} from '@app/features/user/components/modals/tabs/chat_settings_tab/ChatSettingsTabInputTab';
import {DisplayTabContent} from '@app/features/user/components/modals/tabs/chat_settings_tab/DisplayTab';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DISPLAY_DESCRIPTOR = msg({
	message: 'Display',
	comment: 'Short label in the inline. Keep it concise.',
});
const INPUT_DESCRIPTOR = msg({
	message: 'Input',
	comment: 'Short label in the inline. Keep it concise.',
});
export const ChatSettingsInlineContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<div className={styles.container} data-flx="user.chat-settings-tab.inline.chat-settings-inline-content.container">
			<SettingsSection
				id="display"
				title={i18n._(DISPLAY_DESCRIPTOR)}
				data-flx="user.chat-settings-tab.inline.chat-settings-inline-content.display"
			>
				<DisplayTabContent data-flx="user.chat-settings-tab.inline.chat-settings-inline-content.display-tab-content" />
			</SettingsSection>
			<SettingsSection
				id="input"
				title={i18n._(INPUT_DESCRIPTOR)}
				data-flx="user.chat-settings-tab.inline.chat-settings-inline-content.input"
			>
				<InputTabContent data-flx="user.chat-settings-tab.inline.chat-settings-inline-content.input-tab-content" />
			</SettingsSection>
		</div>
	);
});
